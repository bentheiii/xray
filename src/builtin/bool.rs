use crate::builtin::core::{eval, xcmp};
use crate::builtin::optional::{XOptional, XOptionalType};
use crate::xtype::{XFuncSpec, X_BOOL, X_INT, X_STRING};
use crate::xvalue::{ManagedXValue, XValue};
use crate::{add_binop, add_ufunc, add_ufunc_ref, manage_native, to_primitive, CompilationError, RootCompilationScope, XStaticFunction, xraise};
use num_traits::{One, Zero};
use rc::Rc;

use crate::xexpr::XExpr;
use std::io::Write;
use std::rc;

pub(crate) fn add_bool_type<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    scope.add_native_type("bool", X_BOOL.clone())
}

add_binop!(add_bool_eq, eq, X_BOOL, Bool, X_BOOL, |a, b| Ok(
    XValue::Bool(a == b)
));

pub(crate) fn add_bool_assert<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    scope.add_func(
        "assert",
            XFuncSpec::new(&[&X_BOOL], X_BOOL.clone()),
        XStaticFunction::from_native(
            |args, ns, _tca, rt| {
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
                                .map(|e| ns.eval(e, rt.clone(), false).map(|a|a.unwrap_value()))
                                .collect::<Result<Vec<_>, _>>()?;
                            format!("function call with arguments: {inner_arg_values:?} is untrue")
                        }
                        _ => "assertion is untrue".to_string(),
                    };
                    Ok(Err(msg).into())
                }
            },
        ),
    )
}

add_ufunc!(add_bool_not, not, X_BOOL, Bool, X_BOOL, |a: &bool| {
    Ok(XValue::Bool(!a))
});

pub(crate) fn add_bool_and<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    scope.add_func(
        "and",
            XFuncSpec::new(&[&X_BOOL, &X_BOOL], X_BOOL.clone()),
        XStaticFunction::from_native(
            |args, ns, tca, rt| {
                let a0 = xraise!(eval(&args[0], ns, &rt)?);
                if *to_primitive!(a0, Bool) {
                    return ns.eval(&args[1], rt, tca);
                }
                Ok(a0.into())
            },
        ),
    )
}

pub(crate) fn add_bool_or<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    scope.add_func(
        "or",
            XFuncSpec::new(&[&X_BOOL, &X_BOOL], X_BOOL.clone()),
        XStaticFunction::from_native(
            |args, ns, tca, rt| {
                let a0 = xraise!(eval(&args[0], ns, &rt)?);
                if !*to_primitive!(a0, Bool) {
                    return ns.eval(&args[1], rt, tca);
                }
                Ok(a0.into())
            },
        ),
    )
}

pub(crate) fn add_bool_then<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    let ([t], params) = scope.generics_from_names(["T"]);

    scope.add_func(
        "then",
            XFuncSpec::new(&[&X_BOOL, &t], XOptionalType::xtype(t.clone())).generic(params),
        XStaticFunction::from_native(
            |args, ns, _tca, rt| {
                let a0 = xraise!(eval(&args[0], ns, &rt)?);
                Ok(manage_native!(
                    XOptional {
                        value: if *to_primitive!(a0, Bool) {
                            let a1 = eval(&args[1], ns, &rt)?;
                            Some(a1)
                        } else {
                            None
                        }
                    },
                    rt
                ))
            },
        ),
    )
}

add_ufunc!(add_bool_hash, hash, X_BOOL, Bool, X_INT, |a: &bool| {
    Ok(XValue::Int(if *a { One::one() } else { Zero::zero() }))
});

add_ufunc!(
    add_bool_to_str,
    to_str,
    X_BOOL,
    Bool,
    X_STRING,
    |a: &bool| {
        Ok(XValue::String(
            if *a { "true" } else { "false" }.to_string(),
        ))
    }
);

add_binop!(add_bool_cmp, cmp, X_BOOL, Bool, X_INT, |a, b| Ok(xcmp(
    a, b
)));
