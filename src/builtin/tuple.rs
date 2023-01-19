use crate::xtype::{XFuncSpec, X_BOOL};
use crate::xvalue::{ManagedXValue, XFunctionFactoryOutput, XValue};
use crate::{
    forward_err, to_primitive, unpack_types, xraise, CompilationError, RootCompilationScope, XType,
};

use crate::builtin::core::{eval, get_func};
use crate::runtime_scope::RuntimeScope;
use crate::xexpr::{XExpr, XStaticFunction};
use std::io::Write;
use std::sync::Arc;

pub(crate) fn add_tuple_empty_and<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
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

pub(crate) fn add_tuple_dyn_eq<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let eq_symbol = scope.identifier("eq");

    scope.add_dyn_func("eq", "tuples", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let (t0, t1) = unpack_types!(types, 0, 1);
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
                Ok(Ok(move |args: &[XExpr<W>], ns: &RuntimeScope<'_, W>, _tca, rt| {
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
