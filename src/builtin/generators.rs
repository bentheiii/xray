use crate::builtin::core::{eval, get_func_with_type, unpack_dyn_types, unpack_native, xerr};
use crate::builtin::optional::{XOptional, XOptionalType};
use crate::builtin::sequence::{XSequence, XSequenceType};

use crate::native_types::{NativeType, XNativeValue};
use crate::root_runtime_scope::EvaluatedValue;
use crate::runtime_scope::RuntimeScope;
use crate::runtime_violation::RuntimeViolation;

use crate::xtype::{XFuncSpec, X_BOOL, X_INT, X_STRING};
use crate::xvalue::{ManagedXError, ManagedXValue, XFunction, XFunctionFactoryOutput, XValue};
use crate::XType::XCallable;
use crate::{
    forward_err, manage_native, to_native, to_primitive, xraise, xraise_opt, CompilationError,
    RTCell, RootCompilationScope, XCallableSpec, XStaticFunction, XType,
};
use derivative::Derivative;

use either::Either;

use num_traits::{One, Signed, ToPrimitive, Zero};
use rc::Rc;

use std::fmt::Debug;
use std::io::Write;
use std::mem::size_of;

use crate::builtin::mapping::XMapping;
use crate::builtin::set::XSet;
use crate::util::fenced_string::FencedString;
use crate::util::lazy_bigint::LazyBigint;
use std::borrow::Cow;
use std::sync::Arc;
use std::{iter, rc};

use crate::util::multieither::{
    either_a, either_b, either_c, either_d, either_e, either_f, either_g, either_h, either_i,
    either_j, either_k, either_l, either_m_last,
};
use crate::xexpr::XExpr;

#[derive(Debug, Clone)]
pub(crate) struct XGeneratorType;

impl XGeneratorType {
    pub(crate) fn xtype(t: Arc<XType>) -> Arc<XType> {
        Arc::new(XType::XNative(Box::new(Self {}), vec![t]))
    }
}

impl NativeType for XGeneratorType {
    fn generic_names(&self) -> Vec<String> {
        vec!["T".to_string()]
    }
    fn name(&self) -> &str {
        "Generator"
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub(crate) enum XGenerator<W> {
    Aggregate {
        inner: Rc<ManagedXValue<W>>,
        initial_state: Rc<ManagedXValue<W>>,
        func: Rc<ManagedXValue<W>>,
    },
    FromSequence(Rc<ManagedXValue<W>>),
    SuccessorsUntil(Rc<ManagedXValue<W>>, Rc<ManagedXValue<W>>),
    Map(Rc<ManagedXValue<W>>, Rc<ManagedXValue<W>>),
    Filter(Rc<ManagedXValue<W>>, Rc<ManagedXValue<W>>),
    Zip(Vec<Rc<ManagedXValue<W>>>),
    Chain(Vec<Rc<ManagedXValue<W>>>),
    Slice(Rc<ManagedXValue<W>>, usize, Option<usize>),
    Repeat(Rc<ManagedXValue<W>>),
    TakeWhile(Rc<ManagedXValue<W>>, Rc<ManagedXValue<W>>),
    SkipUntil(Rc<ManagedXValue<W>>, Rc<ManagedXValue<W>>),
    FromSet(Rc<ManagedXValue<W>>),
    FromMapping(Rc<ManagedXValue<W>>),
}

impl<W: 'static> XNativeValue for XGenerator<W> {
    fn dyn_size(&self) -> usize {
        match self {
            Self::Zip(arr) | Self::Chain(arr) => arr.len() * size_of::<Rc<ManagedXValue<W>>>(),
            _ => 0,
        }
    }
}

impl<W: Write + 'static> XGenerator<W> {
    fn _iter<'a>(
        &'a self,
        ns: &'a RuntimeScope<W>,
        rt: RTCell<W>,
    ) -> impl Iterator<Item = Result<EvaluatedValue<W>, RuntimeViolation>> + 'a {
        type InnerIter<'a, W> =
            dyn Iterator<Item = Result<EvaluatedValue<W>, RuntimeViolation>> + 'a;
        type BIter<'a, W> = Box<InnerIter<'a, W>>;

        match self {
            Self::Aggregate {
                inner: gen,
                initial_state,
                func,
            } => either_a({
                let inner: BIter<_> = Box::new(to_native!(gen, Self)._iter(ns, rt.clone()));
                let fun = to_primitive!(func, Function);
                iter::once(Ok(Ok(initial_state.clone()))).chain(inner.scan(
                    Ok(initial_state.clone()),
                    move |state, x| {
                        let Ok(x) = x else { return Some(x); };
                        let res = match ns.eval_func_with_values(
                            fun,
                            vec![state.clone(), x],
                            rt.clone(),
                            false,
                        ) {
                            Ok(g) => g.unwrap_value(),
                            Err(violation) => return Some(Err(violation)),
                        };
                        *state = res.clone();
                        Some(Ok(res))
                    },
                ))
            }),
            Self::FromSequence(seq) => either_b(to_native!(seq, XSequence<W>).iter(ns, rt)),
            Self::SuccessorsUntil(initial_state, func) => either_c({
                let fun = to_primitive!(func, Function);
                iter::successors(Some(Ok(Ok(initial_state.clone()))), move |prev| {
                    let Ok(prev) = prev else { return Some(prev.clone()); };
                    match ns.eval_func_with_values(fun, vec![prev.clone()], rt.clone(), false) {
                        Ok(g) => {
                            let g = g.unwrap_value();
                            let Ok(g) = g else { return Some(Ok(g)); };
                            let as_opt = to_native!(g, XOptional<W>);
                            as_opt.value.as_ref().map(|v| Ok(Ok(v.clone())))
                        }
                        Err(violation) => Some(Err(violation)),
                    }
                })
            }),
            Self::Map(gen, func) => either_d({
                let inner: BIter<_> = Box::new(to_native!(gen, Self)._iter(ns, rt.clone()));
                let f = to_primitive!(func, Function);
                inner.map(move |v| {
                    v.and_then(|v| {
                        Ok(ns
                            .eval_func_with_values(f, vec![v], rt.clone(), false)?
                            .unwrap_value())
                    })
                })
            }),
            Self::Zip(arr) => either_e({
                let mut iters = arr
                    .iter()
                    .map(|gen| {
                        let ret: BIter<_> = Box::new(to_native!(gen, Self)._iter(ns, rt.clone()));
                        ret
                    })
                    .collect::<Vec<BIter<_>>>();
                iter::from_fn(move || {
                    iters
                        .iter_mut()
                        .map(|i| i.next())
                        .collect::<Option<Result<Result<Vec<_>, _>, _>>>()
                        .map(|items| {
                            ManagedXValue::new(
                                XValue::StructInstance(forward_err!(items?)),
                                rt.clone(),
                            )
                            .map(Ok)
                        })
                })
            }),
            Self::Chain(arr) => either_f({
                arr.iter().flat_map(move |gen| {
                    to_native!(gen, Self)
                        ._iter(ns, rt.clone())
                        .collect::<Vec<_>>()
                })
            }),
            Self::Slice(gen, start, end) => either_g({
                let inner: BIter<_> = Box::new(to_native!(gen, Self)._iter(ns, rt));
                if let Some(end) = end {
                    Either::Left(inner.skip(*start).take(*end))
                } else {
                    Either::Right(inner.skip(*start))
                }
            }),
            Self::Filter(gen, func) => either_h({
                let inner: BIter<_> = Box::new(to_native!(gen, Self)._iter(ns, rt.clone()));
                let f = to_primitive!(func, Function);
                inner.filter_map(move |i| {
                    let Ok(value) = i else { return Some(i); };
                    let guard =
                        match ns.eval_func_with_values(f, vec![value.clone()], rt.clone(), false) {
                            Ok(g) => g.unwrap_value(),
                            Err(violation) => return Some(Err(violation)),
                        };
                    let Ok(guard) = guard else { return Some(Ok(guard)); };
                    to_primitive!(guard, Bool).then(|| Ok(value))
                })
            }),
            Self::Repeat(gen) => either_i({
                let gen = to_native!(gen, Self);
                iter::repeat_with(move || {
                    let inner: BIter<_> = Box::new(gen._iter(ns, rt.clone()));
                    inner
                })
                .flatten()
            }),
            Self::TakeWhile(gen, func) => either_j({
                let inner: BIter<_> = Box::new(to_native!(gen, Self)._iter(ns, rt.clone()));
                let f = to_primitive!(func, Function);
                inner.map_while(move |i| {
                    let Ok(value) = i else { return Some(i); };
                    let guard =
                        match ns.eval_func_with_values(f, vec![value.clone()], rt.clone(), false) {
                            Ok(g) => g.unwrap_value(),
                            Err(violation) => return Some(Err(violation)),
                        };
                    let Ok(guard) = guard else { return Some(Ok(guard)); };
                    to_primitive!(guard, Bool).then(|| Ok(value))
                })
            }),
            Self::SkipUntil(gen, func) => either_k({
                let inner: BIter<_> = Box::new(to_native!(gen, Self)._iter(ns, rt.clone()));
                let f = to_primitive!(func, Function);
                let mut found_first = false;
                inner.filter_map(move |i| {
                    if found_first {
                        return Some(i);
                    }
                    let Ok(value) = i else { return Some(i); };
                    let guard =
                        match ns.eval_func_with_values(f, vec![value.clone()], rt.clone(), false) {
                            Ok(g) => g.unwrap_value(),
                            Err(violation) => return Some(Err(violation)),
                        };
                    let Ok(guard) = guard else { return Some(Ok(guard)); };
                    found_first = *to_primitive!(guard, Bool);
                    found_first.then_some(Ok(value))
                })
            }),
            Self::FromSet(set) => either_l(to_native!(set, XSet<W>).iter().map(|e| Ok(Ok(e)))),
            Self::FromMapping(mapping) => {
                either_m_last(to_native!(mapping, XMapping<W>).iter().map(move |(k, v)| {
                    Ok(Ok(ManagedXValue::new(
                        XValue::StructInstance(vec![k, v]),
                        rt.clone(),
                    )?))
                }))
            }
        }
    }

    pub(super) fn iter<'a>(
        &'a self,
        ns: &'a RuntimeScope<W>,
        rt: RTCell<W>,
    ) -> impl Iterator<Item = Result<EvaluatedValue<W>, RuntimeViolation>> + 'a {
        self._iter(ns, rt.clone())
            .zip(rt.borrow().limits.search_iter())
            .map(|(v, search)| {
                search?;
                v
            })
    }

    fn chain<'a>(base0: &'a Rc<ManagedXValue<W>>, base1: &'a Rc<ManagedXValue<W>>) -> Self {
        let gen0 = to_native!(base0, Self);
        let gen1 = to_native!(base1, Self);
        let parts = match (gen0, gen1) {
            (Self::Chain(parts0), Self::Chain(parts1)) => {
                parts0.iter().chain(parts1.iter()).cloned().collect()
            }

            (Self::Chain(parts0), _) => parts0.iter().chain(iter::once(base1)).cloned().collect(),

            (_, Self::Chain(parts1)) => iter::once(base0).chain(parts1.iter()).cloned().collect(),

            (_, _) => {
                vec![base0.clone(), base1.clone()]
            }
        };
        Self::Chain(parts)
    }

    fn slice(
        base: &Rc<ManagedXValue<W>>,
        start: usize,
        end: Option<usize>,
    ) -> Result<Self, &Rc<ManagedXValue<W>>> {
        if start == 0 && end.is_none() {
            return Err(base);
        }
        let gen = to_native!(base, Self);
        Ok(match gen {
            Self::Slice(inner, inner_start, inner_end) => Self::Slice(
                inner.clone(),
                inner_start + start,
                inner_end
                    .iter()
                    .chain(end.map(|e| e + inner_start).iter())
                    .min()
                    .cloned(),
            ),
            _ => Self::Slice(base.clone(), start, end),
        })
    }
}

pub(crate) fn add_generator_type<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], _) = scope.generics_from_names(["T"]);
    scope.add_native_type("Generator", XGeneratorType::xtype(t))
}

pub(crate) fn add_generator_successors_until<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t.clone());

    scope.add_func(
        "successors_until",
        XFuncSpec::new(
            &[
                &t,
                &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![t.clone()],
                    return_type: XOptionalType::xtype(t.clone()),
                })),
            ],
            t_gen,
        )
        .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);

            Ok(manage_native!(XGenerator::SuccessorsUntil(a0, a1), rt))
        }),
    )
}

pub(crate) fn add_generator_add<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t);

    scope.add_func(
        "add",
        XFuncSpec::new(&[&t_gen, &t_gen], t_gen.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);

            Ok(manage_native!(XGenerator::chain(&a0, &a1), rt))
        }),
    )
}

pub(crate) fn add_generator_nth<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t.clone());

    scope.add_func(
        "nth",
        XFuncSpec::new(
            &[
                &t_gen,
                &X_INT,
                &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![t.clone()],
                    return_type: X_BOOL.clone(),
                })),
            ],
            XOptionalType::xtype(t),
        )
        .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a2 = xraise!(eval(&args[2], ns, &rt)?);
            let gen0 = to_native!(a0, XGenerator<W>);
            let mut matches_left = to_primitive!(a1, Int).clone();
            if matches_left.is_negative() {
                return xerr(ManagedXError::new(
                    "cannot get negative nth of a generator",
                    rt,
                )?);
            }
            let f = to_primitive!(a2, Function);
            for value in gen0.iter(ns, rt.clone()) {
                let value = value?;
                if *to_primitive!(
                    xraise!(ns
                        .eval_func_with_values(f, vec![value.clone()], rt.clone(), false)?
                        .unwrap_value()),
                    Bool
                ) {
                    if matches_left.is_zero() {
                        return Ok(manage_native!(
                            XOptional {
                                value: Some(xraise!(value))
                            },
                            rt
                        ));
                    }
                    matches_left = matches_left - One::one();
                }
            }
            Ok(manage_native!(XOptional::<W> { value: None }, rt))
        }),
    )
}

pub(crate) fn add_generator_get<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t.clone());

    scope.add_func(
        "get",
        XFuncSpec::new(&[&t_gen, &X_INT], t).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let gen0 = to_native!(a0, XGenerator<W>);
            let mut idx = to_primitive!(a1, Int).clone();
            if idx.is_negative() {
                return xerr(ManagedXError::new(
                    "cannot get negative index of a generator",
                    rt,
                )?);
            }
            for value in gen0.iter(ns, rt.clone()) {
                let value = value?;
                if idx.is_zero() {
                    return Ok(value.into());
                }
                idx = idx - One::one();
            }
            xerr(ManagedXError::new("index out of bounds", rt)?)
        }),
    )
}

pub(crate) fn add_generator_filter<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t.clone());

    scope.add_func(
        "filter",
        XFuncSpec::new(
            &[
                &t_gen.clone(),
                &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![t],
                    return_type: X_BOOL.clone(),
                })),
            ],
            t_gen,
        )
        .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);

            Ok(manage_native!(XGenerator::Filter(a0, a1), rt))
        }),
    )
}

pub(crate) fn add_generator_map<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t0, t1], params) = scope.generics_from_names(["T0", "T1"]);
    let t0_gen = XGeneratorType::xtype(t0.clone());
    let t1_gen = XGeneratorType::xtype(t1.clone());

    scope.add_func(
        "map",
        XFuncSpec::new(
            &[
                &t0_gen,
                &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![t0],
                    return_type: t1,
                })),
            ],
            t1_gen,
        )
        .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);

            Ok(manage_native!(XGenerator::Map(a0, a1), rt))
        }),
    )
}

pub(crate) fn add_generator_aggregate<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t0, t1], params) = scope.generics_from_names(["T0", "T1"]);
    let t0_gen = XGeneratorType::xtype(t0.clone());
    let t1_gen = XGeneratorType::xtype(t1.clone());

    scope.add_func(
        "aggregate",
        XFuncSpec::new(
            &[
                &t0_gen,
                &t1,
                &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![t1.clone(), t0],
                    return_type: t1.clone(),
                })),
            ],
            t1_gen,
        )
        .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a2 = xraise!(eval(&args[2], ns, &rt)?);

            Ok(manage_native!(
                XGenerator::Aggregate {
                    inner: a0,
                    initial_state: a1,
                    func: a2
                },
                rt
            ))
        }),
    )
}

pub(crate) fn add_generator_skip<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t);

    scope.add_func(
        "skip",
        XFuncSpec::new(&[&t_gen.clone(), &X_INT, ], t_gen).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let Some(start) = to_primitive!(a1, Int).to_usize() else { return xerr(ManagedXError::new("index too large", rt)?); };

            Ok(match XGenerator::slice(&a0, start, None) {
                Ok(slice) => manage_native!(slice, rt),
                Err(existing) => existing.clone()
            }.into())
        }),
    )
}

pub(crate) fn add_generator_take<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t);

    scope.add_func(
        "take",
        XFuncSpec::new(&[&t_gen.clone(), &X_INT, ], t_gen).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let Some(end) = to_primitive!(a1, Int).to_usize() else { return xerr(ManagedXError::new("index too large", rt)?); };

            Ok(match XGenerator::slice(&a0, 0, Some(end)) {
                Ok(slice) => manage_native!(slice, rt),
                Err(existing) => existing.clone()
            }.into())
        }),
    )
}

pub(crate) fn add_generator_skip_until<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t.clone());

    scope.add_func(
        "skip_until",
        XFuncSpec::new(
            &[
                &t_gen.clone(),
                &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![t],
                    return_type: X_BOOL.clone(),
                })),
            ],
            t_gen,
        )
        .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);

            Ok(manage_native!(XGenerator::SkipUntil(a0, a1), rt))
        }),
    )
}

pub(crate) fn add_generator_take_while<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t.clone());

    scope.add_func(
        "take_while",
        XFuncSpec::new(
            &[
                &t_gen.clone(),
                &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![t],
                    return_type: X_BOOL.clone(),
                })),
            ],
            t_gen,
        )
        .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);

            Ok(manage_native!(XGenerator::TakeWhile(a0, a1), rt))
        }),
    )
}

pub(crate) fn add_generator_repeat<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t);

    scope.add_func(
        "repeat",
        XFuncSpec::new(&[&t_gen.clone()], t_gen).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);

            Ok(manage_native!(XGenerator::Repeat(a0), rt))
        }),
    )
}

pub(crate) fn add_generator_to_array<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t.clone());

    scope.add_func(
        "to_array",
        XFuncSpec::new(&[&t_gen], XSequenceType::xtype(t)).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let gen0 = to_native!(a0, XGenerator<W>);
            let mut ret = Vec::new();
            for value in gen0.iter(ns, rt.clone()) {
                let value = xraise!(value?);
                ret.push(value);
                rt.borrow().can_afford(&ret)?;
            }
            Ok(manage_native!(XSequence::Array(ret), rt))
        }),
    )
}

pub(crate) fn add_generator_len<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t);

    scope.add_func(
        "len",
        XFuncSpec::new(&[&t_gen], X_INT.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let gen0 = to_native!(a0, XGenerator<W>);
            let mut ret = 0usize;
            for value in gen0.iter(ns, rt.clone()) {
                xraise!(value?);
                ret += 1;
            }
            Ok(ManagedXValue::new(XValue::Int(LazyBigint::from(ret)), rt)?.into())
        }),
    )
}

pub(crate) fn add_generator_last<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XGeneratorType::xtype(t.clone());

    scope.add_func(
        "last",
        XFuncSpec::new(&[&t_arr], t).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let gen0 = to_native!(a0, XGenerator<W>);
            let mut ret = None;
            for value in gen0.iter(ns, rt.clone()) {
                let value = xraise!(value?);
                ret = Some(value);
            }
            if let Some(ret) = ret {
                Ok(ret.into())
            } else {
                xerr(ManagedXError::new("generator is empty", rt)?)
            }
        }),
    )
}

pub(crate) fn add_generator_join<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let t_arr = XGeneratorType::xtype(X_STRING.clone());

    scope.add_func(
        "join",
        XFuncSpec::new_with_optional(&[&t_arr], &[&X_STRING], X_STRING.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise_opt!(args.get(1).map(|e| eval(e, ns, &rt)).transpose()?);
            let seq0 = to_native!(a0, XGenerator<W>);
            let delimiter = match a1 {
                None => Cow::Owned(FencedString::from_str("")),
                Some(ref a) => Cow::Borrowed(to_primitive!(a, String).as_ref()),
            };
            let mut ret = FencedString::default();
            let mut first = true;

            for x in seq0.iter(ns, rt.clone()) {
                let item = xraise!(x?);
                if !first {
                    ret.push(delimiter.as_ref())
                }

                let f = to_primitive!(item, String);
                ret.push(f.as_ref());
                rt.borrow().can_allocate_by(|| Some(ret.size()))?;
                first = false;
            }
            ret.shrink_to_fit();

            Ok(ManagedXValue::new(XValue::String(Box::new(ret)), rt)?.into())
        }),
    )
}

pub(crate) fn add_generator_dyn_mean<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let f_symbol = scope.identifier("add");
    let div_symbol = scope.identifier("div");
    let enumerate_symbol = scope.identifier("enumerate");
    let last_symbol = scope.identifier("last");
    let agg_symbol = scope.identifier("aggregate");

    scope.add_dyn_func(
        "mean",
        "generator",
        move |_params, types, ns, bind| {
            if bind.is_some() {
                return Err("this dyn func has no bind".to_string());
            }

            let [t0] = unpack_dyn_types(types)?;
            let [inner0] = unpack_native(t0, "Generator")? else { unreachable!() };

            let (inner_f, f_t) =
                get_func_with_type(ns, f_symbol, &[inner0.clone(), inner0.clone()], Some(inner0))?;
            let (inner_agg, agg_t) =
                get_func_with_type(ns, agg_symbol, &[t0.clone(), f_t.xtype()], None)?;
            let (inner_enumerate, enumerate_t) =
                get_func_with_type(ns, enumerate_symbol, &[agg_t.rtype(), X_INT.clone()], None)?;
            let (inner_last, _) = get_func_with_type(ns, last_symbol, &[enumerate_t.rtype()], Some(&Arc::new(XType::Tuple(vec![X_INT.clone(), inner0.clone()]))))?;
            let (inner_div, div_t) = get_func_with_type(ns, div_symbol, &[inner0.clone(), X_INT.clone()], None)?;

            Ok(XFunctionFactoryOutput::from_delayed_native(
                XFuncSpec::new(&[t0], div_t.rtype()),
                move |ns, rt| {
                    let inner_f = forward_err!(ns.eval(&inner_f, rt.clone(), false)?.unwrap_value());
                    let inner_agg = forward_err!(ns.eval(&inner_agg, rt.clone(), false)?.unwrap_value());
                    let inner_enumerate = forward_err!(ns.eval(&inner_enumerate, rt.clone(), false)?.unwrap_value());
                    let inner_last = forward_err!(ns.eval(&inner_last, rt.clone(), false)?.unwrap_value());
                    let inner_div = forward_err!(ns.eval(&inner_div, rt.clone(), false)?.unwrap_value());
                    let one = ManagedXValue::new(XValue::Int(LazyBigint::from(1)), rt)?;
                    Ok(Ok(
                        move |args: &[XExpr<W>], ns: &RuntimeScope<'_, W>, _tca, rt: RTCell<_>| {
                            let a0 = xraise!(eval(&args[0], ns, &rt)?);
                            let XValue::Function(inner_agg) = &inner_agg.value else { unreachable!() };
                            let XValue::Function(inner_enumerate) = &inner_enumerate.value else { unreachable!() };
                            let XValue::Function(inner_last) = &inner_last.value else { unreachable!() };
                            let XValue::Function(inner_div) = &inner_div.value else { unreachable!() };
                            let aggregated = xraise!(ns.eval_func_with_values(inner_agg, vec![
                        Ok(a0),
                        Ok(inner_f.clone()),
                    ], rt.clone(), false)?.unwrap_value());
                            let enumerated = xraise!(ns.eval_func_with_values(inner_enumerate, vec![
                        Ok(aggregated),
                        Ok(one.clone()),
                    ], rt.clone(), false)?.unwrap_value());
                            let last = xraise!(ns.eval_func_with_values(inner_last, vec![
                        Ok(enumerated),
                    ], rt.clone(), false)?.unwrap_value());
                            let XValue::StructInstance(vals) = &last.value else { unreachable!() };
                            ns.eval_func_with_values(inner_div, vec![
                                Ok(vals[1].clone()),
                                Ok(vals[0].clone()),
                            ], rt, false)
                        },
                    ))
                },
            ))
        },
    )
}

pub(crate) fn add_generator_dyn_geo_mean<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let f_symbol = scope.identifier("mul");
    let div_symbol = scope.identifier("div");
    let pow_symbol = scope.identifier("pow");
    let enumerate_symbol = scope.identifier("enumerate");
    let last_symbol = scope.identifier("last");
    let agg_symbol = scope.identifier("aggregate");

    scope.add_dyn_func(
        "geo_mean",
        "generator",
        move |_params, types, ns, bind| {
            if bind.is_some() {
                return Err("this dyn func has no bind".to_string());
            }

            let [t0] = unpack_dyn_types(types)?;
            let [inner0] = unpack_native(t0, "Generator")? else { unreachable!() };

            let (inner_f, f_t) =
                get_func_with_type(ns, f_symbol, &[inner0.clone(), inner0.clone()], Some(inner0))?;
            let (inner_agg, agg_t) =
                get_func_with_type(ns, agg_symbol, &[t0.clone(), f_t.xtype()], None)?;
            let (inner_enumerate, enumerate_t) =
                get_func_with_type(ns, enumerate_symbol, &[agg_t.rtype(), X_INT.clone()], None)?;
            let (inner_last, _) = get_func_with_type(ns, last_symbol, &[enumerate_t.rtype()], Some(&Arc::new(XType::Tuple(vec![X_INT.clone(), inner0.clone()]))))?;
            let (inner_div, div_t) = get_func_with_type(ns, div_symbol, &[X_INT.clone(), X_INT.clone()], None)?;
            let (inner_pow, pow_t) = get_func_with_type(ns, pow_symbol, &[inner0.clone(), div_t.rtype()], None)?;

            Ok(XFunctionFactoryOutput::from_delayed_native(
                XFuncSpec::new(&[t0], pow_t.rtype()),
                move |ns, rt| {
                    let inner_f = forward_err!(ns.eval(&inner_f, rt.clone(), false)?.unwrap_value());
                    let inner_agg = forward_err!(ns.eval(&inner_agg, rt.clone(), false)?.unwrap_value());
                    let inner_enumerate = forward_err!(ns.eval(&inner_enumerate, rt.clone(), false)?.unwrap_value());
                    let inner_last = forward_err!(ns.eval(&inner_last, rt.clone(), false)?.unwrap_value());
                    let inner_div = forward_err!(ns.eval(&inner_div, rt.clone(), false)?.unwrap_value());
                    let inner_pow = forward_err!(ns.eval(&inner_pow, rt.clone(), false)?.unwrap_value());
                    let one = ManagedXValue::new(XValue::Int(LazyBigint::from(1)), rt)?;
                    Ok(Ok(
                        move |args: &[XExpr<W>], ns: &RuntimeScope<'_, W>, _tca, rt: RTCell<_>| {
                            let a0 = xraise!(eval(&args[0], ns, &rt)?);
                            let XValue::Function(inner_agg) = &inner_agg.value else { unreachable!() };
                            let XValue::Function(inner_enumerate) = &inner_enumerate.value else { unreachable!() };
                            let XValue::Function(inner_last) = &inner_last.value else { unreachable!() };
                            let XValue::Function(inner_div) = &inner_div.value else { unreachable!() };
                            let XValue::Function(inner_pow) = &inner_pow.value else { unreachable!() };
                            let aggregated = xraise!(ns.eval_func_with_values(inner_agg, vec![
                                Ok(a0),
                                Ok(inner_f.clone()),
                            ], rt.clone(), false)?.unwrap_value());
                            let enumerated = xraise!(ns.eval_func_with_values(inner_enumerate, vec![
                                Ok(aggregated),
                                Ok(one.clone()),
                            ], rt.clone(), false)?.unwrap_value());
                            let last = xraise!(ns.eval_func_with_values(inner_last, vec![
                                Ok(enumerated),
                            ], rt.clone(), false)?.unwrap_value());
                            let XValue::StructInstance(vals) = &last.value else { unreachable!() };
                            let exponent = xraise!(ns.eval_func_with_values(inner_div, vec![
                                Ok(one.clone()),
                                Ok(vals[0].clone())
                            ], rt.clone(), false)?.unwrap_value());
                            ns.eval_func_with_values(inner_pow, vec![
                                Ok(vals[1].clone()),
                                Ok(exponent),
                            ], rt, false)
                        },
                    ))
                },
            ))
        },
    )
}

pub(crate) fn add_generator_dyn_zip<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_dyn_func("zip", "generators", move |_params, types, _ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let mut inner_types = vec![];
        let mut is_seq = vec![];
        let mut any_gen = false;
        // todo try zip without args
        for t in types.unwrap() {
            if let Ok(as_gen) = unpack_native(t, "Generator") {
                inner_types.push(as_gen[0].clone());
                is_seq.push(false);
                any_gen = true;
            } else {
                let [inner] = unpack_native(t, "Sequence")? else { unreachable!() };
                inner_types.push(inner.clone());
                is_seq.push(true);
            }
        }

        if !any_gen {
            return Err("this function requires at least one generator".to_string());
        }

        let arg_types = types.unwrap().iter().collect::<Vec<_>>();

        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(
                &arg_types,
                XGeneratorType::xtype(Arc::new(XType::Tuple(inner_types))),
            ),
            move |args, ns, _tca, rt| {
                let mut gens = vec![];
                for (a, is_seq) in args.iter().zip(is_seq.iter()) {
                    let mut a = xraise!(eval(a, ns, &rt)?);
                    if *is_seq {
                        a = manage_native!(XGenerator::FromSequence(a), rt.clone());
                    }
                    gens.push(a);
                    rt.as_ref().borrow().can_afford(&gens)?
                }
                Ok(manage_native!(XGenerator::<W>::Zip(gens), rt))
            },
        ))
    })
}

pub(crate) fn add_generator_dyn_unzip<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_dyn_func("unzip", "generators", move |_params, types, _ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let [t0] = unpack_dyn_types(types)?;
        let [inner0] = unpack_native(t0, "Generator")? else { unreachable!() };
        let XType::Tuple(inner_types) = inner0.as_ref() else { return Err(format!("expected sequence of tuples, got {t0:?}")); };
        let t_len = inner_types.len();

        let ret_type = Arc::new(XType::Tuple(inner_types.iter().map(|t| XGeneratorType::xtype(t.clone())).collect()));


        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(&[t0], ret_type),
            move |args, ns, _tca, rt| {
                let a = xraise!(eval(&args[0], ns, &rt)?);

                let mut items = vec![];
                rt.as_ref().borrow().can_allocate(t_len * 2 + 1)?;
                for i in 0..t_len {
                    let func = ManagedXValue::new(XValue::Function(XFunction::Native(Rc::new(
                        move |args, ns, _tca, rt| {
                            let a0 = xraise!(eval(&args[0], ns, &rt)?);
                            let t0 = to_primitive!(a0, StructInstance);
                            Ok(t0[i].clone().into())
                        }
                    ))), rt.clone())?;
                    let item = manage_native!(XGenerator::Map(a.clone(), func), rt.clone());
                    items.push(item);
                }

                Ok(ManagedXValue::new(
                    XValue::StructInstance(items), rt,
                )?.into())
            },
        ))
    })
}
