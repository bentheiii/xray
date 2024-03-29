use crate::builtin::core::{eval, xcmp};
use crate::builtin::optional::{XOptional, XOptionalType};
use crate::xtype::{XFuncSpec, X_BOOL, X_INT, X_STRING};
use crate::xvalue::{ManagedXError, ManagedXValue, XValue};
use crate::{
    add_binfunc, manage_native, to_primitive, ufunc, xraise, CompilationError,
    RootCompilationScope, XStaticFunction,
};
use num_traits::{One, Zero};
use rc::Rc;

use crate::util::fenced_string::FencedString;
use crate::xexpr::XExpr;

use std::rc;

pub(crate) fn add_bool_type<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_native_type("bool", X_BOOL.clone())
}

add_binfunc!(add_bool_eq, eq, X_BOOL, Bool, X_BOOL, |a, b, _| Ok(Ok(
    XValue::Bool(a == b)
)));

pub(crate) fn add_bool_assert<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "assert",
        XFuncSpec::new(&[&X_BOOL], X_BOOL.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            if let XValue::Bool(true) = a0.value {
                Ok(a0.into())
            } else {
                // we fetch the original expression to maybe make the error message better
                // todo improve
                let msg = match &args[0] {
                    XExpr::Call(_func, inner_args) => {
                        let inner_arg_values = inner_args
                            .iter()
                            .map(|e| ns.eval(e, rt.clone(), false).map(|a| a.unwrap_value()))
                            .collect::<Result<Vec<_>, _>>()?;
                        format!("function call with arguments: {inner_arg_values:?} is untrue")
                    }
                    _ => "assertion is untrue".to_string(),
                };
                Ok(Err(ManagedXError::new(msg, rt)?).into())
            }
        }),
    )
}

pub(crate) fn add_bool_not<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "not",
        XFuncSpec::new(&[&X_BOOL], X_BOOL.clone()),
        ufunc!(Bool, |a: &bool, _rt| { Ok(Ok(XValue::Bool(!a))) }),
    )
}

pub(crate) fn add_bool_and<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "and",
        XFuncSpec::new(&[&X_BOOL, &X_BOOL], X_BOOL.clone()),
        XStaticFunction::from_native(|args, ns, tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            if *to_primitive!(a0, Bool) {
                return ns.eval(&args[1], rt, tca);
            }
            Ok(a0.into())
        }),
    )
}

pub(crate) fn add_bool_or<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "or",
        XFuncSpec::new(&[&X_BOOL, &X_BOOL], X_BOOL.clone()),
        XStaticFunction::from_native(|args, ns, tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            if !*to_primitive!(a0, Bool) {
                return ns.eval(&args[1], rt, tca);
            }
            Ok(a0.into())
        }),
    )
}

pub(crate) fn add_bool_then<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);

    scope.add_func(
        "then",
        XFuncSpec::new(&[&X_BOOL, &t], XOptionalType::xtype(t.clone())).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            Ok(manage_native!(
                XOptional {
                    value: if *to_primitive!(a0, Bool) {
                        let a1 = xraise!(eval(&args[1], ns, &rt)?);
                        Some(a1)
                    } else {
                        None
                    }
                },
                rt
            ))
        }),
    )
}

pub(crate) fn add_bool_hash<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "hash",
        XFuncSpec::new(&[&X_BOOL], X_INT.clone()),
        ufunc!(Bool, |a: &bool, _rt| {
            Ok(Ok(XValue::Int(if *a { One::one() } else { Zero::zero() })))
        }),
    )
}

pub(crate) fn add_bool_to_str<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "to_str",
        XFuncSpec::new(&[&X_BOOL], X_STRING.clone()),
        ufunc!(Bool, |a: &bool, _rt| {
            Ok(Ok(XValue::String(Box::new(FencedString::from_string(
                if *a { "true" } else { "false" }.to_string(),
            )))))
        }),
    )
}

add_binfunc!(add_bool_cmp, cmp, X_BOOL, Bool, X_INT, |a, b, _| Ok(Ok(
    xcmp(a, b)
)));
