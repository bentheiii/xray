use crate::xtype::{XFuncSpec, X_BOOL, X_INT, X_STRING};
use crate::xvalue::{ManagedXError, ManagedXValue, XFunctionFactoryOutput, XValue};
use crate::{forward_err, to_primitive, xraise, CompilationError, RootCompilationScope, XType};
use std::collections::hash_map::DefaultHasher;
use std::hash::Hasher;

use crate::builtin::core::{eval, get_func, unpack_dyn_types, xerr};
use crate::runtime_scope::RuntimeScope;
use crate::util::fenced_string::FencedString;
use crate::util::lazy_bigint::LazyBigint;
use crate::xexpr::{XExpr, XStaticFunction};
use num_traits::{ToPrimitive, Zero};
use std::io::Write;
use std::sync::Arc;

pub(crate) fn add_tuple_empty_and<W, R>(
    scope: &mut RootCompilationScope<W, R>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);

    scope.add_func(
        "and",
        XFuncSpec::new(&[&Arc::new(XType::Tuple(Vec::new())), &t.clone()], t).generic(params),
        XStaticFunction::from_native(|args, ns, tca, rt| {
            let _ = xraise!(eval(&args[0], ns, &rt)?);
            ns.eval(&args[1], rt, tca)
        }),
    )
}

pub(crate) fn add_tuple_dyn_eq<W, R>(
    scope: &mut RootCompilationScope<W, R>,
) -> Result<(), CompilationError> {
    let eq_symbol = scope.identifier("eq");

    scope.add_dyn_func("eq", "tuples", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let [t0, t1] = unpack_dyn_types(types)?;
        let XType::Tuple(subtypes0) = t0.as_ref() else { return Err("argument 1 is not a tuple".to_string()); };
        let XType::Tuple(subtypes1) = t1.as_ref() else { return Err("argument 2 is not a tuple".to_string()); };
        if subtypes0.len() != subtypes1.len() {
            return Err("tuples are not of equal length".to_string());
        }

        let inner_funcs: Vec<_> = subtypes0.iter().zip(subtypes1.iter()).map(|(s0, s1)| get_func(ns, eq_symbol, &[s0.clone(), s1.clone()], &X_BOOL)).collect::<Result<_, _>>()?;

        Ok(XFunctionFactoryOutput::from_delayed_native(
            XFuncSpec::new(&[t0, t1], X_BOOL.clone()),
            move |ns, rt| {
                let inner_values = {
                    let mut inner_values = Vec::with_capacity(inner_funcs.len());
                    for inner in inner_funcs.iter() {
                        inner_values.push(
                            forward_err!(ns.eval(inner, rt.clone(), false)?.unwrap_value())
                        )
                    }
                    inner_values
                };
                Ok(Ok(move |args: &[XExpr<W, R>], ns: &RuntimeScope<'_, W, R>, _tca, rt| {
                    let a0 = xraise!(eval(&args[0], ns, &rt)?);
                    let a1 = xraise!(eval(&args[1], ns, &rt)?);
                    let t0 = to_primitive!(a0, StructInstance);
                    let t1 = to_primitive!(a1, StructInstance);
                    let mut ret = true;
                    for ((i0, i1), func) in t0.iter().zip(t1.iter()).zip(inner_values.iter()) {
                        let func = to_primitive!(func, Function);
                        let eq = xraise!(ns.eval_func_with_values(func, vec![Ok(i0.clone()), Ok(i1.clone())], rt.clone(), false)?.unwrap_value());
                        let is_eq = to_primitive!(eq, Bool);
                        if !*is_eq {
                            ret = false;
                            break;
                        }
                    }
                    Ok(ManagedXValue::new(XValue::Bool(ret), rt)?.into())
                }))
            },
        ))
    })
}

pub(crate) fn add_tuple_dyn_cmp<W, R>(
    scope: &mut RootCompilationScope<W, R>,
) -> Result<(), CompilationError> {
    let eq_symbol = scope.identifier("cmp");

    scope.add_dyn_func("cmp", "tuples", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let [t0, t1] = unpack_dyn_types(types)?;
        let XType::Tuple(subtypes0) = t0.as_ref() else { return Err("argument 1 is not a tuple".to_string()); };
        let XType::Tuple(subtypes1) = t1.as_ref() else { return Err("argument 2 is not a tuple".to_string()); };

        if subtypes0.len() != subtypes1.len() {
            return Err("tuples are not of equal length".to_string());
        }

        let inner_funcs: Vec<_> = subtypes0.iter().zip(subtypes1.iter()).map(|(s0, s1)| get_func(ns, eq_symbol, &[s0.clone(), s1.clone()], &X_INT)).collect::<Result<_, _>>()?;

        Ok(XFunctionFactoryOutput::from_delayed_native(
            XFuncSpec::new(&[t0, t1], X_INT.clone()),
            move |ns, rt| {
                let inner_values = {
                    let mut inner_values = Vec::with_capacity(inner_funcs.len());
                    for inner in inner_funcs.iter() {
                        inner_values.push(
                            forward_err!(ns.eval(inner, rt.clone(), false)?.unwrap_value())
                        )
                    }
                    inner_values
                };
                Ok(Ok(move |args: &[XExpr<W, R>], ns: &RuntimeScope<'_, W, R>, _tca, rt| {
                    let a0 = xraise!(eval(&args[0], ns, &rt)?);
                    let a1 = xraise!(eval(&args[1], ns, &rt)?);
                    let t0 = to_primitive!(a0, StructInstance);
                    let t1 = to_primitive!(a1, StructInstance);
                    for ((i0, i1), func) in t0.iter().zip(t1.iter()).zip(inner_values.iter()) {
                        let func = to_primitive!(func, Function);
                        let cmp = xraise!(ns.eval_func_with_values(func, vec![Ok(i0.clone()), Ok(i1.clone())], rt.clone(), false)?.unwrap_value());
                        let v = to_primitive!(cmp, Int);
                        if !v.is_zero() {
                            return Ok(cmp.into())
                        }
                    }
                    Ok(ManagedXValue::new(XValue::Int(LazyBigint::zero()), rt)?.into())
                }))
            },
        ))
    })
}

pub(crate) fn add_tuple_dyn_hash<W, R>(
    scope: &mut RootCompilationScope<W, R>,
) -> Result<(), CompilationError> {
    let eq_symbol = scope.identifier("hash");

    scope.add_dyn_func("hash", "tuples", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let [t0] = unpack_dyn_types(types)?;
        let XType::Tuple(subtypes0) = t0.as_ref() else { return Err("argument 1 is not a tuple".to_string()); };

        let inner_funcs: Vec<_> = subtypes0.iter().map(|s0| get_func(ns, eq_symbol, &[s0.clone()], &X_INT)).collect::<Result<_, _>>()?;

        Ok(XFunctionFactoryOutput::from_delayed_native(
            XFuncSpec::new(&[t0], X_INT.clone()),
            move |ns, rt| {
                let inner_values = {
                    let mut inner_values = Vec::with_capacity(inner_funcs.len());
                    for inner in inner_funcs.iter() {
                        inner_values.push(
                            forward_err!(ns.eval(inner, rt.clone(), false)?.unwrap_value())
                        )
                    }
                    inner_values
                };
                Ok(Ok(move |args: &[XExpr<W, R>], ns: &RuntimeScope<'_, W, R>, _tca, rt| {
                    let a0 = xraise!(eval(&args[0], ns, &rt)?);
                    let t0 = to_primitive!(a0, StructInstance);
                    let mut hasher = DefaultHasher::new();
                    for (i0, func) in t0.iter().zip(inner_values.iter()) {
                        let func = to_primitive!(func, Function);
                        let hash = xraise!(ns.eval_func_with_values(func, vec![Ok(i0.clone())], rt.clone(), false)?.unwrap_value());
                        let Some(f) = to_primitive!(hash, Int).to_u64() else { return xerr(ManagedXError::new("hash out of bounds", rt)?); };
                        hasher.write_u64(f);
                    }
                    let ret = hasher.finish();

                    Ok(ManagedXValue::new(XValue::Int(LazyBigint::from(ret)), rt)?.into())
                }))
            },
        ))
    })
}

pub(crate) fn add_tuple_dyn_to_str<W, R>(
    scope: &mut RootCompilationScope<W, R>,
) -> Result<(), CompilationError> {
    let eq_symbol = scope.identifier("to_str");

    scope.add_dyn_func("to_str", "tuples", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let [t0] = unpack_dyn_types(types)?;
        let XType::Tuple(subtypes0) = t0.as_ref() else { return Err("argument 1 is not a tuple".to_string()); };

        let inner_funcs: Vec<_> = subtypes0.iter().map(|s0| get_func(ns, eq_symbol, &[s0.clone()], &X_STRING)).collect::<Result<_, _>>()?;

        Ok(XFunctionFactoryOutput::from_delayed_native(
            XFuncSpec::new(&[t0], X_STRING.clone()),
            move |ns, rt| {
                let inner_values = {
                    let mut inner_values = Vec::with_capacity(inner_funcs.len());
                    for inner in inner_funcs.iter() {
                        inner_values.push(
                            forward_err!(ns.eval(inner, rt.clone(), false)?.unwrap_value())
                        )
                    }
                    inner_values
                };
                Ok(Ok(move |args: &[XExpr<W, R>], ns: &RuntimeScope<'_, W, R>, _tca, rt| {
                    let a0 = xraise!(eval(&args[0], ns, &rt)?);
                    let t0 = to_primitive!(a0, StructInstance);
                    let mut ret = FencedString::from_str("(");
                    let mut first = true;
                    for (i0, func) in t0.iter().zip(inner_values.iter()) {
                        if first{
                            first = false;
                        } else {
                            ret.push_ascii(", ");
                        }
                        let func = to_primitive!(func, Function);
                        let str = xraise!(ns.eval_func_with_values(func, vec![Ok(i0.clone())], rt.clone(), false)?.unwrap_value());
                        let f = to_primitive!(str, String).as_ref();
                        ret.push(f);
                    }
                    ret.push_ascii(")");
                    ret.shrink_to_fit();

                    Ok(ManagedXValue::new(XValue::String(Box::new(ret)), rt)?.into())
                }))
            },
        ))
    })
}
