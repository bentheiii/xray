use crate::builtin::core::{eval, get_func_with_type, unpack_dyn_types, unpack_native, xerr};
use crate::builtin::optional::{XOptional, XOptionalType};
use crate::builtin::sequence::{XSequence, XSequenceType};

use crate::native_types::{NativeType, XNativeValue};
use crate::runtime_scope::RuntimeScope;

use crate::xtype::{XFuncSpec, X_BOOL, X_INT, X_STRING};
use crate::xvalue::{
    ManagedXError, ManagedXValue, XFunction, XFunctionFactoryOutput, XResult, XValue,
};
use crate::{
    delegate, forward_err, manage_native, to_native, to_primitive, xraise, xraise_opt,
    CompilationError, RTCell, RootCompilationScope, XCallableSpec, XStaticFunction, XType,
};
use derivative::Derivative;

use either::Either;

use num_traits::{One, Signed, ToPrimitive, Zero};
use rc::Rc;

use std::collections::VecDeque;
use std::fmt::Debug;

use std::mem::{size_of, take};

use crate::builtin::mapping::XMapping;
use crate::builtin::set::XSet;
use crate::util::fenced_string::FencedString;
use crate::util::lazy_bigint::LazyBigint;
use std::borrow::Cow;
use std::sync::Arc;
use std::{iter, rc};

use crate::util::multieither::{
    either_a, either_b, either_c, either_d, either_e, either_f, either_g, either_h, either_i,
    either_j, either_k, either_l, either_m, either_n, either_o, either_p, either_q_last,
};
use crate::xexpr::{TailedEvalResult, XExpr};

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
pub(crate) enum XGenerator<W, R, T> {
    Aggregate {
        inner: Rc<ManagedXValue<W, R, T>>,
        initial_state: Rc<ManagedXValue<W, R, T>>,
        func: Rc<ManagedXValue<W, R, T>>,
    },
    FromSequence(Rc<ManagedXValue<W, R, T>>),
    SuccessorsUntil(Rc<ManagedXValue<W, R, T>>, Rc<ManagedXValue<W, R, T>>),
    Map(Rc<ManagedXValue<W, R, T>>, Rc<ManagedXValue<W, R, T>>),
    Filter(Rc<ManagedXValue<W, R, T>>, Rc<ManagedXValue<W, R, T>>),
    Zip(Vec<Rc<ManagedXValue<W, R, T>>>),
    Chain(Vec<Rc<ManagedXValue<W, R, T>>>),
    Slice(Rc<ManagedXValue<W, R, T>>, usize, Option<usize>),
    Repeat(Rc<ManagedXValue<W, R, T>>),
    TakeWhile(Rc<ManagedXValue<W, R, T>>, Rc<ManagedXValue<W, R, T>>),
    SkipUntil(Rc<ManagedXValue<W, R, T>>, Rc<ManagedXValue<W, R, T>>),
    FromSet(Rc<ManagedXValue<W, R, T>>),
    FromMapping(Rc<ManagedXValue<W, R, T>>),
    WithCount {
        inner: Rc<ManagedXValue<W, R, T>>,
        eq_func: Rc<ManagedXValue<W, R, T>>,
        hash_func: Rc<ManagedXValue<W, R, T>>,
    },
    Group {
        inner: Rc<ManagedXValue<W, R, T>>,
        eq_func: Rc<ManagedXValue<W, R, T>>,
    },
    Windows {
        inner: Rc<ManagedXValue<W, R, T>>,
        size: usize,
    },
    Product(Vec<Rc<ManagedXValue<W, R, T>>>),
}

impl<W: 'static, R: 'static, T: 'static> XNativeValue for XGenerator<W, R, T> {
    fn dyn_size(&self) -> usize {
        match self {
            Self::Zip(arr) | Self::Chain(arr) => {
                arr.len() * size_of::<Rc<ManagedXValue<W, R, T>>>()
            }
            _ => 0,
        }
    }
}

impl<W: 'static, R: 'static, T: 'static> XGenerator<W, R, T> {
    fn _iter<'a>(
        &'a self,
        ns: &'a RuntimeScope<W, R, T>,
        rt: RTCell<W, R, T>,
    ) -> impl Iterator<Item = XResult<Rc<ManagedXValue<W, R, T>>, W, R, T>> + 'a {
        type InnerIter<'a, W, R, T> =
            dyn Iterator<Item = XResult<Rc<ManagedXValue<W, R, T>>, W, R, T>> + 'a;
        type BIter<'a, W, R, T> = Box<InnerIter<'a, W, R, T>>;

        match self {
            Self::Aggregate {
                inner: gen,
                initial_state,
                func,
            } => either_a({
                let inner: BIter<_, _, _> = Box::new(to_native!(gen, Self)._iter(ns, rt.clone()));
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
            Self::FromSequence(seq) => either_b(to_native!(seq, XSequence<W, R, T>).iter(ns, rt)),
            Self::SuccessorsUntil(initial_state, func) => either_c({
                let fun = to_primitive!(func, Function);
                iter::successors(Some(Ok(Ok(initial_state.clone()))), move |prev| {
                    let Ok(prev) = prev else { return Some(prev.clone()); };
                    match ns.eval_func_with_values(fun, vec![prev.clone()], rt.clone(), false) {
                        Ok(g) => {
                            let g = g.unwrap_value();
                            let Ok(g) = g else { return Some(Ok(g)); };
                            let as_opt = to_native!(g, XOptional<W, R, T>);
                            as_opt.value.as_ref().map(|v| Ok(Ok(v.clone())))
                        }
                        Err(violation) => Some(Err(violation)),
                    }
                })
            }),
            Self::Map(gen, func) => either_d({
                let inner: BIter<_, _, _> = Box::new(to_native!(gen, Self)._iter(ns, rt.clone()));
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
                        let ret: BIter<_, _, _> =
                            Box::new(to_native!(gen, Self)._iter(ns, rt.clone()));
                        ret
                    })
                    .collect::<Vec<BIter<_, _, _>>>();
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
                let inner: BIter<_, _, _> = Box::new(to_native!(gen, Self)._iter(ns, rt));
                if let Some(end) = end {
                    Either::Left(inner.skip(*start).take(*end))
                } else {
                    Either::Right(inner.skip(*start))
                }
            }),
            Self::Filter(gen, func) => either_h({
                let inner: BIter<_, _, _> = Box::new(to_native!(gen, Self)._iter(ns, rt.clone()));
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
                    let inner: BIter<_, _, _> = Box::new(gen._iter(ns, rt.clone()));
                    inner
                })
                .flatten()
            }),
            Self::TakeWhile(gen, func) => either_j({
                let inner: BIter<_, _, _> = Box::new(to_native!(gen, Self)._iter(ns, rt.clone()));
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
                let inner: BIter<_, _, _> = Box::new(to_native!(gen, Self)._iter(ns, rt.clone()));
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
            Self::FromSet(set) => {
                either_l(to_native!(set, XSet<W, R, T>).iter().map(|e| Ok(Ok(e))))
            }
            Self::FromMapping(mapping) => either_m(
                to_native!(mapping, XMapping<W, R, T>)
                    .iter()
                    .map(move |(k, v)| {
                        Ok(Ok(ManagedXValue::new(
                            XValue::StructInstance(vec![k, v]),
                            rt.clone(),
                        )?))
                    }),
            ),
            Self::WithCount {
                inner,
                hash_func,
                eq_func,
            } => either_n({
                let inner: BIter<_, _, _> = Box::new(to_native!(inner, Self)._iter(ns, rt.clone()));

                let mut counter =
                    XMapping::new(hash_func.clone(), eq_func.clone(), Default::default(), 0);
                inner.map(move |i| {
                    let i = forward_err!(i?);
                    rt.can_allocate(counter.dyn_size())?;
                    let v = forward_err!(counter.put(&i, || 1usize, |v| v + 1, ns, rt.clone())?);
                    let v = ManagedXValue::new(XValue::Int(LazyBigint::from(*v)), rt.clone())?;
                    let tup = ManagedXValue::new(XValue::StructInstance(vec![i, v]), rt.clone())?;
                    Ok(Ok(tup))
                })
            }),
            Self::Group { inner, eq_func } => either_o({
                let inner: BIter<_, _, _> = Box::new(to_native!(inner, Self)._iter(ns, rt.clone()));
                let eq_f = to_primitive!(eq_func, Function);
                let mut current_group: Vec<Rc<ManagedXValue<W, R, T>>> = Vec::new();

                inner
                    .map(Some)
                    .chain(iter::once(None))
                    .zip(rt.limits.search_iter())
                    .filter_map(move |(i, s)| {
                        if let Err(violation) = s {
                            return Some(Err(violation));
                        }

                        if let Some(i) = i {
                            if let Ok(Ok(i)) = i {
                                if let Some(cgk) = current_group.first() {
                                    let eq = ns
                                        .eval_func_with_values(
                                            eq_f,
                                            vec![Ok(cgk.clone()), Ok(i.clone())],
                                            rt.clone(),
                                            false,
                                        )
                                        .map(TailedEvalResult::unwrap_value);
                                    if let Ok(Ok(eq)) = eq {
                                        let &eq = to_primitive!(eq, Bool);
                                        if eq {
                                            current_group.push(i);
                                            None
                                        } else {
                                            let group = take(&mut current_group);
                                            let seq = ManagedXValue::new(
                                                XValue::Native(Box::new(XSequence::array(group))),
                                                rt.clone(),
                                            );
                                            current_group.push(i);
                                            Some(seq.map(Ok))
                                        }
                                    } else {
                                        Some(eq)
                                    }
                                } else {
                                    current_group.push(i);
                                    None
                                }
                            } else {
                                Some(i)
                            }
                        } else if !current_group.is_empty() {
                            // we're at the end of the generator, so we need to return the last group
                            let group = take(&mut current_group);
                            let seq = ManagedXValue::new(
                                XValue::Native(Box::new(XSequence::array(group))),
                                rt.clone(),
                            );
                            Some(seq.map(Ok))
                        } else {
                            None
                        }
                    })
            }),
            Self::Windows { inner, size } => either_p({
                let inner: BIter<_, _, _> = Box::new(to_native!(inner, Self)._iter(ns, rt.clone()));
                let mut memory = VecDeque::with_capacity(*size);
                inner
                    .zip(rt.limits.search_iter())
                    .filter_map(move |(i, s)| {
                        if let Err(violation) = s {
                            return Some(Err(violation));
                        }

                        if let Ok(Ok(i)) = i {
                            memory.push_back(i);
                            if memory.len() == *size {
                                let seq = ManagedXValue::new(
                                    XValue::Native(Box::new(XSequence::array(
                                        memory.iter().cloned().collect(),
                                    ))),
                                    rt.clone(),
                                );
                                memory.pop_front();
                                Some(seq.map(Ok))
                            } else {
                                None
                            }
                        } else {
                            Some(i)
                        }
                    })
            }),
            Self::Product(inners) => either_q_last({
                let inners = inners
                    .iter()
                    .map(|gen| to_native!(gen, Self))
                    .collect::<Vec<_>>();
                let mut iters = inners
                    .iter()
                    .map(|gen| {
                        let ret: BIter<_, _, _> = Box::new(gen._iter(ns, rt.clone()));
                        ret
                    })
                    .collect::<Vec<BIter<_, _, _>>>();
                let mut current: Option<Vec<Rc<ManagedXValue<W, R, T>>>> = None;
                let rt2 = rt.clone();
                iter::from_fn(move || {
                    if let Some(ref mut current) = current {
                        let mut found = false;
                        for idx_to_bump in (0..current.len()).rev() {
                            let next_value = iters[idx_to_bump].next();
                            match next_value {
                                None => {
                                    iters[idx_to_bump] =
                                        Box::new(inners[idx_to_bump]._iter(ns, rt.clone()));
                                    let next_value = iters[idx_to_bump].next().unwrap();
                                    // we can be sure the iterator is not empty because it wan't empty before
                                    match next_value {
                                        Ok(Ok(next_value)) => {
                                            current[idx_to_bump] = next_value;
                                        }
                                        Err(other) => return Some(Err(other)),
                                        Ok(Err(other)) => return Some(Ok(Err(other))),
                                    }
                                    continue;
                                }
                                Some(Ok(Ok(next_value))) => {
                                    current[idx_to_bump] = next_value;
                                    found = true;
                                    break;
                                }
                                Some(Err(other)) => return Some(Err(other)),
                                Some(Ok(Err(other))) => return Some(Ok(Err(other))),
                            }
                        }
                        if found {
                            Some(Ok(Ok(current.clone())))
                        } else {
                            None
                        }
                    } else {
                        let firsts = iters
                            .iter_mut()
                            .map(|i| i.next())
                            .collect::<Option<XResult<Vec<_>, _, _, _>>>();
                        match firsts {
                            Some(Ok(Ok(firsts))) => {
                                current = Some(firsts);
                                Some(Ok(Ok(current.clone().unwrap())))
                            }
                            Some(Err(other)) => Some(Err(other)),
                            Some(Ok(Err(other))) => Some(Ok(Err(other))),
                            None => None,
                        }
                    }
                })
                .map(move |v| {
                    let v = forward_err!(v?);
                    ManagedXValue::new(XValue::StructInstance(v), rt2.clone()).map(Ok)
                })
            }),
        }
    }

    pub(super) fn iter<'a>(
        &'a self,
        ns: &'a RuntimeScope<W, R, T>,
        rt: RTCell<W, R, T>,
    ) -> impl Iterator<Item = XResult<Rc<ManagedXValue<W, R, T>>, W, R, T>> + 'a {
        self._iter(ns, rt.clone())
            .zip(rt.limits.search_iter())
            .map(|(v, search)| {
                search?;
                v
            })
    }

    fn chain<'a>(
        base0: &'a Rc<ManagedXValue<W, R, T>>,
        base1: &'a Rc<ManagedXValue<W, R, T>>,
    ) -> Self {
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
        base: &Rc<ManagedXValue<W, R, T>>,
        start: usize,
        end: Option<usize>,
    ) -> Result<Self, &Rc<ManagedXValue<W, R, T>>> {
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

pub(crate) fn add_generator_type<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], _) = scope.generics_from_names(["T"]);
    scope.add_native_type("Generator", XGeneratorType::xtype(t))
}

pub(crate) fn add_generator_successors_until<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t.clone());

    scope.add_func(
        "successors_until",
        XFuncSpec::new(
            &[
                &t,
                &Arc::new(XType::XCallable(XCallableSpec {
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

pub(crate) fn add_generator_group<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t.clone());

    scope.add_func(
        "group",
        XFuncSpec::new(
            &[
                &t_gen,
                &Arc::new(XType::XCallable(XCallableSpec {
                    param_types: vec![t.clone(), t.clone()],
                    return_type: X_BOOL.clone(),
                })),
            ],
            XGeneratorType::xtype(XSequenceType::xtype(t)),
        )
        .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            Ok(manage_native!(
                XGenerator::Group {
                    inner: a0,
                    eq_func: a1
                },
                rt
            ))
        }),
    )
}

pub(crate) fn add_generator_windows<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t.clone());

    scope.add_func(
        "windows",
        XFuncSpec::new(
            &[&t_gen, &X_INT],
            XGeneratorType::xtype(XSequenceType::xtype(t)),
        )
        .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let Some(size) = to_primitive!(a1, Int).to_usize() else {return xerr(ManagedXError::new(
                "window size out of bounds",
                rt,
            )?)};
            Ok(manage_native!(XGenerator::Windows { inner: a0, size }, rt))
        }),
    )
}

pub(crate) fn add_generator_add<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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

pub(crate) fn add_generator_nth<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t.clone());

    scope.add_func(
        "nth",
        XFuncSpec::new(
            &[
                &t_gen,
                &X_INT,
                &Arc::new(XType::XCallable(XCallableSpec {
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
            let gen0 = to_native!(a0, XGenerator<W, R, T>);
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
            Ok(manage_native!(XOptional::<W, R, T> { value: None }, rt))
        }),
    )
}

pub(crate) fn add_generator_get<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t.clone());

    scope.add_func(
        "get",
        XFuncSpec::new(&[&t_gen, &X_INT], t).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let gen0 = to_native!(a0, XGenerator<W, R, T>);
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

pub(crate) fn add_generator_filter<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t.clone());

    scope.add_func(
        "filter",
        XFuncSpec::new(
            &[
                &t_gen.clone(),
                &Arc::new(XType::XCallable(XCallableSpec {
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

pub(crate) fn add_generator_with_count<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t.clone());

    scope.add_func(
        "with_count",
        XFuncSpec::new(
            &[
                &t_gen,
                &Arc::new(XType::XCallable(XCallableSpec {
                    param_types: vec![t.clone()],
                    return_type: X_INT.clone(),
                })),
                &Arc::new(XType::XCallable(XCallableSpec {
                    param_types: vec![t.clone(), t.clone()],
                    return_type: X_BOOL.clone(),
                })),
            ],
            XGeneratorType::xtype(Arc::new(XType::Tuple(vec![t, X_INT.clone()]))),
        )
        .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a2 = xraise!(eval(&args[2], ns, &rt)?);

            Ok(manage_native!(
                XGenerator::WithCount {
                    inner: a0,
                    hash_func: a1,
                    eq_func: a2,
                },
                rt
            ))
        }),
    )
}

pub(crate) fn add_generator_map<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t0, t1], params) = scope.generics_from_names(["T0", "T1"]);
    let t0_gen = XGeneratorType::xtype(t0.clone());
    let t1_gen = XGeneratorType::xtype(t1.clone());

    scope.add_func(
        "map",
        XFuncSpec::new(
            &[
                &t0_gen,
                &Arc::new(XType::XCallable(XCallableSpec {
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

pub(crate) fn add_generator_aggregate<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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
                &Arc::new(XType::XCallable(XCallableSpec {
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

pub(crate) fn add_generator_skip<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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

pub(crate) fn add_generator_take<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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

pub(crate) fn add_generator_skip_until<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t.clone());

    scope.add_func(
        "skip_until",
        XFuncSpec::new(
            &[
                &t_gen.clone(),
                &Arc::new(XType::XCallable(XCallableSpec {
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

pub(crate) fn add_generator_take_while<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t.clone());

    scope.add_func(
        "take_while",
        XFuncSpec::new(
            &[
                &t_gen.clone(),
                &Arc::new(XType::XCallable(XCallableSpec {
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

pub(crate) fn add_generator_repeat<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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

pub(crate) fn add_generator_to_array<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t.clone());

    scope.add_func(
        "to_array",
        XFuncSpec::new(&[&t_gen], XSequenceType::xtype(t)).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let gen0 = to_native!(a0, XGenerator<W, R, T>);
            let mut ret = Vec::new();
            for value in gen0.iter(ns, rt.clone()) {
                let value = xraise!(value?);
                ret.push(value);
                rt.can_afford(&ret)?;
            }
            Ok(manage_native!(XSequence::Array(ret), rt))
        }),
    )
}

pub(crate) fn add_generator_len<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t);

    scope.add_func(
        "len",
        XFuncSpec::new(&[&t_gen], X_INT.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let gen0 = to_native!(a0, XGenerator<W, R, T>);
            let mut ret = 0usize;
            for value in gen0.iter(ns, rt.clone()) {
                xraise!(value?);
                ret += 1;
            }
            Ok(ManagedXValue::new(XValue::Int(LazyBigint::from(ret)), rt)?.into())
        }),
    )
}

pub(crate) fn add_generator_last<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XGeneratorType::xtype(t.clone());

    scope.add_func(
        "last",
        XFuncSpec::new(&[&t_arr], t).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let gen0 = to_native!(a0, XGenerator<W, R, T>);
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

pub(crate) fn add_generator_join<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let t_arr = XGeneratorType::xtype(X_STRING.clone());

    scope.add_func(
        "join",
        XFuncSpec::new_with_optional(&[&t_arr], &[&X_STRING], X_STRING.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise_opt!(args.get(1).map(|e| eval(e, ns, &rt)).transpose()?);
            let seq0 = to_native!(a0, XGenerator<W, R, T>);
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
                rt.can_allocate(ret.size())?;
                first = false;
            }
            ret.shrink_to_fit();

            Ok(ManagedXValue::new(XValue::String(Box::new(ret)), rt)?.into())
        }),
    )
}

fn add_delegate_hash_eq<W, R, T>(
    name: &'static str,
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let hash_symbol = scope.identifier("hash");
    let eq_symbol = scope.identifier("eq");
    let cb_symbol = scope.identifier(name);

    scope.add_dyn_func(name, "sequences", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let [t0] = unpack_dyn_types(types)?;
        let [inner0] = unpack_native(t0, "Generator")? else { unreachable!() };

        let (inner_hash, hash_t) = get_func_with_type(ns, hash_symbol, &[inner0.clone()], None)?;
        let (inner_eq, eq_t) =
            get_func_with_type(ns, eq_symbol, &[inner0.clone(), inner0.clone()], None)?;
        let (cb, cb_t) = get_func_with_type(
            ns,
            cb_symbol,
            &[t0.clone(), hash_t.xtype(), eq_t.xtype()],
            None,
        )?;

        Ok(XFunctionFactoryOutput::from_delayed_native(
            XFuncSpec::new(&[t0], cb_t.rtype()),
            delegate!(
                with [inner_hash, inner_eq, cb],
                args [0->a0],
                cb(a0, inner_hash, inner_eq)
            ),
        ))
    })
}

pub(crate) fn add_generator_dyn_with_count<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    add_delegate_hash_eq("with_count", scope)
}

pub(crate) fn add_generator_dyn_distinct<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    add_delegate_hash_eq("distinct", scope)
}

pub(crate) fn add_generator_dyn_mean<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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
                        move |args: &[XExpr<W, R, T>], ns: &RuntimeScope<'_, W, R, T>, _tca, rt: RTCell<_, _, _>| {
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

pub(crate) fn add_generator_dyn_geo_mean<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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
                        move |args: &[XExpr<W, R, T>], ns: &RuntimeScope<'_, W, R, T>, _tca, rt: RTCell<_, _, _>| {
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

pub(crate) fn add_generator_dyn_zip<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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
                    rt.can_afford(&gens)?
                }
                Ok(manage_native!(XGenerator::<W, R, T>::Zip(gens), rt))
            },
        ))
    })
}

pub(crate) fn add_generator_dyn_unzip<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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
                rt.can_allocate((t_len * 2 + 1)*size_of::<usize>())?;
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

pub(crate) fn add_generator_dyn_product<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_dyn_func("product", "generators", move |_params, types, _ns, bind| {
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
                    rt.can_afford(&gens)?
                }
                Ok(manage_native!(XGenerator::<W, R, T>::Product(gens), rt))
            },
        ))
    })
}
