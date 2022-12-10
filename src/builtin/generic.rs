use crate::xexpr::XExpr;
use crate::xtype::{XFuncSpec, X_BOOL, X_INT, X_STRING, X_UNKNOWN};
use crate::xvalue::{ManagedXValue, XFunctionFactoryOutput, XValue};
use crate::{
    to_primitive, ufunc, unpack_types, xraise, xraise_opt, CompilationError, RootCompilationScope,
    XStaticFunction, XType,
};
use rc::Rc;

use crate::builtin::core::{eval, eval_resolved_func, get_func};
use crate::runtime_violation::RuntimeViolation;
use num_traits::Signed;
use std::io::Write;
use std::rc;

pub(crate) fn add_if<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    scope.add_func(
        "if",
        XFuncSpec::new(&[&X_BOOL, &t, &t], t.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            ns.eval(&args[if *to_primitive!(a0, Bool) { 1 } else { 2 }], rt, tca)
        }),
    )
}

pub(crate) fn add_error<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "error",
        XFuncSpec::new(&[&X_STRING], X_UNKNOWN.clone()),
        ufunc!(String, |a: &String| Err(a.clone())),
    )
}

pub(crate) fn add_debug<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    scope.add_func(
        "debug",
        XFuncSpec::new_with_optional(&[&t], &[&X_STRING], t.clone()).generic(params),
        XStaticFunction::from_native(|args: &[XExpr<W>], ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise_opt!(args.get(1).map(|e| eval(e, ns, &rt)).transpose()?);
            let b = to_primitive!(a1, String, "".to_string());
            writeln!(rt.borrow_mut().stdout, "{b}{a0:?}")
                .map_err(RuntimeViolation::OutputFailure)?;
            Ok(a0.into())
        }),
    )
}

pub(crate) fn add_is_error<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    scope.add_func(
        "is_error",
        XFuncSpec::new(&[&t], X_BOOL.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = eval(&args[0], ns, &rt)?;
            Ok(ManagedXValue::new(XValue::Bool(a0.is_err()), rt)?.into())
        }),
    )
}

pub(crate) fn add_if_error<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    scope.add_func(
        "if_error",
        XFuncSpec::new(&[&t, &t], t.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, tca, rt| {
            let a0 = eval(&args[0], ns, &rt)?;
            if a0.is_ok() {
                Ok(a0.into())
            } else {
                ns.eval(&args[1], rt, tca)
            }
        }),
    )
}

pub(crate) fn add_ne<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let eq_symbol = scope.identifier("eq");

    scope.add_dyn_func("ne", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }
        let (t0, t1) = unpack_types!(types, 0, 1);

        let eq_expr = get_func(ns, eq_symbol, &[t0.clone(), t1.clone()], &X_BOOL)?;

        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(&[t0, t1], X_BOOL.clone()),
            move |args, ns, _tca, rt| {
                let a0 = eval(&args[0], ns, &rt)?;
                let a1 = eval(&args[1], ns, &rt)?;
                let eq = xraise!(eval_resolved_func(&eq_expr, ns, rt.clone(), vec![a0, a1])?);
                let is_eq = to_primitive!(eq, Bool);
                Ok(ManagedXValue::new(XValue::Bool(!*is_eq), rt)?.into())
            },
        ))
    })
}

pub(crate) fn add_display<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let to_str_symbol = scope.identifier("to_str");

    scope.add_dyn_func("display", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let (t, t1) = unpack_types!(types, 0 | 1);
        if let Some(t1) = t1 {
            if let XType::String = t1.as_ref() {
            } else {
                return Err(format!("argument 2 must be a string, got {t1:?}"));
            }
        }

        let inner_to_str = get_func(ns, to_str_symbol, &[t.clone()], &X_STRING)?;

        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new_with_optional(&[&t.clone()], &[&X_STRING], t.clone()),
            move |args, ns, _tca, rt| {
                let a0 = eval(&args[0], ns, &rt)?;
                let a1 = xraise_opt!(args.get(1).map(|e| eval(e, ns, &rt)).transpose()?);
                let b = to_primitive!(a1, String, "".to_string());
                let string = xraise!(eval_resolved_func(
                    &inner_to_str,
                    ns,
                    rt.clone(),
                    vec![a0.clone()]
                )?);
                let str_slice = to_primitive!(string, String);
                writeln!(rt.borrow_mut().stdout, "{b}{str_slice}")
                    .map_err(RuntimeViolation::OutputFailure)?;
                Ok(a0.into())
            },
        ))
    })
}

pub(crate) fn add_cmp_lt<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let cmp_symbol = scope.identifier("cmp");

    scope.add_dyn_func("lt", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }
        let (t0, t1) = unpack_types!(types, 0, 1);

        let inner_func = get_func(ns, cmp_symbol, &[t0.clone(), t1.clone()], &X_INT)?;

        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(&[t0, t1], X_BOOL.clone()),
            move |args, ns, _tca, rt| {
                let a0 = eval(&args[0], ns, &rt)?;
                let a1 = eval(&args[1], ns, &rt)?;
                let cmp = xraise!(eval_resolved_func(
                    &inner_func,
                    ns,
                    rt.clone(),
                    vec![a0, a1]
                )?);
                Ok(
                    ManagedXValue::new(XValue::Bool(to_primitive!(cmp, Int).is_negative()), rt)?
                        .into(),
                )
            },
        ))
    })
}

pub(crate) fn add_cmp_gt<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let cmp_symbol = scope.identifier("cmp");

    scope.add_dyn_func("gt", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let (t0, t1) = unpack_types!(types, 0, 1);

        let inner_func = get_func(ns, cmp_symbol, &[t0.clone(), t1.clone()], &X_INT)?;

        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(&[t0, t1], X_BOOL.clone()),
            move |args, ns, _tca, rt| {
                let a0 = eval(&args[0], ns, &rt)?;
                let a1 = eval(&args[1], ns, &rt)?;
                let cmp = xraise!(eval_resolved_func(
                    &inner_func,
                    ns,
                    rt.clone(),
                    vec![a0, a1]
                )?);
                Ok(
                    ManagedXValue::new(XValue::Bool(to_primitive!(cmp, Int).is_positive()), rt)?
                        .into(),
                )
            },
        ))
    })
}

pub(crate) fn add_cmp_ge<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let cmp_symbol = scope.identifier("cmp");

    scope.add_dyn_func("ge", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }
        let (t0, t1) = unpack_types!(types, 0, 1);

        let inner_func = get_func(ns, cmp_symbol, &[t0.clone(), t1.clone()], &X_INT)?;

        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(&[t0, t1], X_BOOL.clone()),
            move |args, ns, _tca, rt| {
                let a0 = eval(&args[0], ns, &rt)?;
                let a1 = eval(&args[1], ns, &rt)?;
                let cmp = xraise!(eval_resolved_func(
                    &inner_func,
                    ns,
                    rt.clone(),
                    vec![a0, a1]
                )?);
                Ok(
                    ManagedXValue::new(XValue::Bool(!to_primitive!(cmp, Int).is_negative()), rt)?
                        .into(),
                )
            },
        ))
    })
}

pub(crate) fn add_cmp_le<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let cmp_symbol = scope.identifier("cmp");

    scope.add_dyn_func("le", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let (t0, t1) = unpack_types!(types, 0, 1);

        let inner_func = get_func(ns, cmp_symbol, &[t0.clone(), t1.clone()], &X_INT)?;

        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(&[t0, t1], X_BOOL.clone()),
            move |args, ns, _tca, rt| {
                let a0 = eval(&args[0], ns, &rt)?;
                let a1 = eval(&args[1], ns, &rt)?;
                let cmp = xraise!(eval_resolved_func(
                    &inner_func,
                    ns,
                    rt.clone(),
                    vec![a0, a1]
                )?);
                Ok(
                    ManagedXValue::new(XValue::Bool(!to_primitive!(cmp, Int).is_positive()), rt)?
                        .into(),
                )
            },
        ))
    })
}

pub(crate) fn add_cast<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_dyn_func("cast", move |_params, _types, _ns, bind| {
        let bind_len = bind.map_or(0, |a| a.len());
        if bind_len != 1 {
            return Err(format!(
                "this dyn func requires exactly 1 bind, got {bind_len}"
            ));
        }
        let t_out = &bind.unwrap()[0];

        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(&[t_out], t_out.clone()),
            move |args, ns, tca, rt| ns.eval(&args[0], rt, tca),
        ))
    })
}
