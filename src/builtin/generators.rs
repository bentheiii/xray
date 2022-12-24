use crate::builtin::core::{eval, get_func, search, unpack_native, xerr};
use crate::builtin::optional::{XOptional, XOptionalType};
use crate::builtin::stack::{XStack, XStackType};
use crate::native_types::{NativeType, XNativeValue};
use crate::root_runtime_scope::EvaluatedValue;
use crate::runtime_scope::RuntimeScope;
use crate::runtime_violation::RuntimeViolation;
use crate::util::trysort::try_sort;
use crate::xtype::{XFuncSpec, X_BOOL, X_INT};
use crate::xvalue::{ManagedXError, ManagedXValue, XFunction, XFunctionFactoryOutput, XValue};
use crate::XType::XCallable;
use crate::{
    forward_err, manage_native, to_native, to_primitive, unpack_types, xraise, xraise_opt,
    CompilationError, RTCell, RootCompilationScope, XCallableSpec, XStaticFunction, XType,
};
use derivative::Derivative;
use either::Either;
use num_traits::{One, Signed, ToPrimitive, Zero};
use rc::Rc;
use std::borrow::Cow;
use std::fmt::Debug;
use std::io::Write;
use std::mem::size_of;
use std::ops::Neg;
use std::{iter, rc};
use std::sync::Arc;
use dyn_clone::DynClone;
use itertools::multizip;
use crate::builtin::sequence::{XSequence, XSequenceType};

use crate::util::lazy_bigint::LazyBigint;
use crate::util::multieither::{either_1, either_2, either_3, either_4, either_5, either_6};
use crate::util::try_extend::TryExtend;

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
pub(crate) enum XGenerator<W: Write + 'static> {
    Aggregate{inner: Rc<ManagedXValue<W>>, initial_state: Rc<ManagedXValue<W>>, func: Rc<ManagedXValue<W>>},
    FromSequence(Rc<ManagedXValue<W>>),
    Successors(Rc<ManagedXValue<W>>, Rc<ManagedXValue<W>>),
    Map(Rc<ManagedXValue<W>>, Rc<ManagedXValue<W>>),
    Filter(Rc<ManagedXValue<W>>, Rc<ManagedXValue<W>>),
    Zip(Vec<Rc<ManagedXValue<W>>>),
    Chain(Vec<Rc<ManagedXValue<W>>>),
    Slice(Rc<ManagedXValue<W>>, usize, Option<usize>),
}

impl<W: Write + 'static> XNativeValue for XGenerator<W> {
    fn dyn_size(&self) -> usize {
        match self {
            Self::Zip(arr) | Self::Chain(arr) => arr.len() * size_of::<Rc<ManagedXValue<W>>>(),
            _ => 0
        }
    }
}

impl<W: Write + 'static> XGenerator<W> {
    fn _iter<'a>(&'a self, ns: &'a RuntimeScope<W>, rt: RTCell<W>) -> impl Iterator<Item=Result<EvaluatedValue<W>, RuntimeViolation>> + 'a {
        type InnerIter<'a, W> = dyn Iterator<Item=Result<EvaluatedValue<W>, RuntimeViolation>> + 'a;
        type BIter<'a, W> = Box<InnerIter<'a, W>>;

        match self {
            Self::Iterable(b, ..) => either_1(b.clone()),
            Self::Map(gen, func) => either_2({
                let inner: BIter<_> = Box::new(to_native!(gen, Self)._iter(ns, rt.clone()));
                let f = to_primitive!(func, Function);
                inner.map(move |v| {
                    v.and_then(
                        |v| Ok(ns.eval_func_with_values(f, vec![v], rt.clone(), false)?.unwrap_value())
                    )
                })
            }),
            Self::Zip(arr) => either_3({
                let mut iters = arr.iter().map(|gen| {
                    let ret: BIter<_> = Box::new(to_native!(gen, Self)._iter(ns, rt.clone()));
                    ret
                }).collect::<Vec<BIter<_>>>();
                iter::from_fn(move ||{
                    iters.iter_mut().map(|i| i.next()).collect::<Option<Result<Result<Vec<_>,_>,_>>>().map(|items| ManagedXValue::new(XValue::StructInstance(forward_err!(items?)), rt.clone()).map(Ok))
                })
            }),
            Self::Chain(arr) => either_4({
                arr.iter().flat_map(move |gen|{
                    to_native!(gen, Self)._iter(ns, rt.clone()).collect::<Vec<_>>()
                })
            }),
            Self::Slice(gen, start, end) => either_5({
                let inner: BIter<_> =  Box::new(to_native!(gen, Self)._iter(ns, rt));
                if let Some(end) = end{
                    Either::Left(inner.skip(*start).take(*end))
                } else {
                    Either::Right(inner.skip(*start))
                }
            }),
            Self::Filter(gen, func) => either_6({
                let inner: BIter<_> = Box::new(to_native!(gen, Self)._iter(ns, rt.clone()));
                let f = to_primitive!(func, Function);
                inner.filter_map(move |i| {
                    let Ok(value) = i else {return Some(i)};
                    let guard = match ns.eval_func_with_values(f, vec![value.clone()], rt.clone(), false){
                        Ok(g) => g.unwrap_value(),
                        Err(violation) => return Some(Err(violation))
                    };
                    let Ok(guard) = guard else {return Some(Ok(guard))};
                    to_primitive!(guard, Bool).then(|| Ok(value))
                })
            })
        }
    }

    fn iter<'a>(&'a self, ns: &'a RuntimeScope<W>, rt: RTCell<W>) -> impl Iterator<Item=Result<EvaluatedValue<W>, RuntimeViolation>> + 'a{
        self._iter(ns, rt.clone()).zip(rt.borrow().limits.search_iter()).map(|(v,search)| {
            search?;
            v
        })
    }

    fn chain<'a>(base0: &'a Rc<ManagedXValue<W>>, base1: &'a Rc<ManagedXValue<W>>,)->Self{
        let gen0 = to_native!(base0, Self);
        let gen1 = to_native!(base1, Self);
        let parts = match (gen0, gen1) {
            (Self::Chain(parts0), Self::Chain(parts1)) => {
                parts0.iter().chain(parts1.iter()).cloned().collect()
            }

            (Self::Chain(parts0), _) => {
                parts0.iter().chain(iter::once(base1)).cloned().collect()
            }

            (_, Self::Chain(parts1)) => {
                iter::once(base0).chain(parts1.iter()).cloned().collect()
            }

            (_, _) => {
                vec![base0.clone(), base1.clone()]
            }
        };
        Self::Chain(parts)
    }

    fn slice(base: &Rc<ManagedXValue<W>>, start: usize, end: Option<usize>) ->Result<Self, &Rc<ManagedXValue<W>>> {
        if start == 0 && end.is_none(){
            return Err(base)
        }
        let gen = to_native!(base, Self);
        Ok(match gen{
            Self::Slice(inner, inner_start, inner_end)=> Self::Slice(inner.clone(), inner_start+start, inner_end.iter().chain(end.map(|e| e+inner_start).iter()).min().cloned()),
            _ => Self::Slice(base.clone(), start, end)
        })
    }
}

pub(crate) fn add_generator_type<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], _) = scope.generics_from_names(["T"]);
    scope.add_native_type("Generator", XGeneratorType::xtype(t))
}

pub(crate) fn add_generator_successors<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t.clone());

    scope.add_func(
        "successors",
        XFuncSpec::new(&[&t, &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![t.clone()],
                    return_type: t.clone(),
                }))], t_gen.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let f = to_primitive!(a1, Function).clone();
            let rt_ = rt.clone();

            let iter = iter::successors(Some(Ok(Ok(a0.clone()))), move |v|{
                // a good assumption here is that the previous value is not a violation (or else we wouldn't be called)
                let Ok(v) = v else {unreachable!()};
                Some(ns.eval_func_with_values(&f, vec![v.clone()], rt.clone(), false).map(|s| s.unwrap_value()))
            });

            Ok(manage_native!(XGenerator::iterator(iter), rt_))
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
        XFuncSpec::new(&[&t_gen, &X_INT, &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![t.clone()],
                    return_type: X_BOOL.clone(),
                })),], XOptionalType::xtype(t)).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a2 = xraise!(eval(&args[2], ns, &rt)?);
            let gen0 = to_native!(a0, XGenerator<W>);
            let mut matches_left = to_primitive!(a1, Int).clone();
            if matches_left.is_negative(){
                return xerr(ManagedXError::new("cannot get negative nth of a generator", rt)?)
            }
            let f = to_primitive!(a2, Function);
            for value in gen0.iter(ns, rt.clone()){
                let value = value?;
                if *to_primitive!(
                    xraise!(ns
                        .eval_func_with_values(f, vec![value.clone()], rt.clone(), false)?
                        .unwrap_value()),
                    Bool
                ) {
                    if matches_left.is_zero() {
                        return Ok(manage_native!(XOptional { value: Some(xraise!(value)) }, rt));
                    }
                    matches_left = matches_left - One::one();
                }
            }
            Ok(manage_native!(XOptional::<W> { value: None }, rt))
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
        XFuncSpec::new(&[&t_gen.clone(), &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![t.clone()],
                    return_type: X_BOOL.clone(),
                })),], t_gen).generic(params),
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
        XFuncSpec::new(&[&t0_gen, &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![t0],
                    return_type: t1,
                })),], t1_gen).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);

            Ok(manage_native!(XGenerator::Map(a0, a1), rt))
        }),
    )
}

pub(crate) fn add_generator_skip<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t.clone());

    scope.add_func(
        "skip",
        XFuncSpec::new(&[&t_gen.clone(), &X_INT,], t_gen).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let Some(start) = to_primitive!(a1, Int).to_usize() else {return xerr(ManagedXError::new("index too large", rt)?)};

            Ok(manage_native!(XGenerator::Slice(a0, start, None), rt))
        }),
    )
}

pub(crate) fn add_generator_take<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_gen = XGeneratorType::xtype(t.clone());

    scope.add_func(
        "take",
        XFuncSpec::new(&[&t_gen.clone(), &X_INT,], t_gen).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let Some(end) = to_primitive!(a1, Int).to_usize() else {return xerr(ManagedXError::new("index too large", rt)?)};

            Ok(manage_native!(XGenerator::Slice(a0, 0, Some(end)), rt))
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
        XFuncSpec::new(&[&t_gen,], XSequenceType::xtype(t)).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let gen0 = to_native!(a0, XGenerator<W>);
            let mut ret = Vec::new();
            for value in gen0.iter(ns, rt.clone()){
                let value = xraise!(value?);
                ret.push(value);
                rt.borrow().can_afford(&ret)?;
            }
            Ok(manage_native!(XSequence::Array(ret), rt))
        }),
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
        // todo try zip without args
        for t in types.unwrap() {
            let [inner] = unpack_native(t, "Generator")? else { unreachable!() };
            inner_types.push(inner.clone())
        }

        let arg_types = types.unwrap().iter().collect::<Vec<_>>();

        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(
                &arg_types,
                XGeneratorType::xtype(Arc::new(XType::Tuple(inner_types))),
            ),
            move |args, ns, _tca, rt| {
                let mut seqs = vec![];
                for a in args {
                    let a = xraise!(eval(a, ns, &rt)?);
                    seqs.push(a);
                    rt.as_ref().borrow().can_afford(&seqs)?
                }
                Ok(manage_native!(XGenerator::<W>::Zip(seqs), rt))
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

        let (t0, ) = unpack_types!(types, 0);
        let [inner0] = unpack_native(t0, "Generators")? else { unreachable!() };
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