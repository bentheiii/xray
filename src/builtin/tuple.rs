use crate::xtype::{XFuncSpec, X_BOOL};
use crate::xvalue::{ManagedXValue, XFunctionFactoryOutput, XValue};
use crate::{to_primitive, unpack_types, xraise, CompilationError, RootCompilationScope, XType};

use crate::builtin::core::{eval, eval_resolved_func, get_func};
use std::io::Write;
use std::sync::Arc;
use crate::xexpr::XStaticFunction;

pub(crate) fn add_empty_tup_and<W: Write + 'static>(
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

pub(crate) fn add_tuple_eq<W: Write + 'static>(
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

        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(&[t0, t1], X_BOOL.clone()),
            move |args, ns, _tca, rt| {
                let a0 = xraise!(eval(&args[0], ns, &rt)?);
                let a1 = xraise!(eval(&args[1], ns, &rt)?);
                let t0 = to_primitive!(a0, StructInstance);
                let t1 = to_primitive!(a1, StructInstance);
                let mut ret = true;
                for ((i0, i1), func) in t0.iter().zip(t1.iter()).zip(inner_funcs.iter()) {
                    let eq = xraise!(eval_resolved_func(func, ns, rt.clone(), vec![Ok(i0.clone()), Ok(i1.clone())])?);
                    let is_eq = to_primitive!(eq, Bool);
                    if !*is_eq {
                        ret = false;
                        break;
                    }
                }
                Ok(ManagedXValue::new(XValue::Bool(ret), rt)?.into())
            },
        ))
    })
}
