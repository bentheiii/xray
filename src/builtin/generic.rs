use crate::xexpr::XExpr;
use crate::xtype::{XFuncSpec, X_BOOL, X_INT, X_STRING, X_UNKNOWN};
use crate::xvalue::{ManagedXValue, XFunctionFactoryOutput, XValue};
use crate::{
    add_ufunc, add_ufunc_ref, meval, to_primitive, unpack_types, CompilationError,
    RootCompilationScope, XStaticFunction, XType,
};
use rc::Rc;

use crate::builtin::core::{eval, eval_if_present, eval_resolved_func, eval_result, get_func};
use num_traits::Signed;
use std::io::Write;
use std::rc;

pub(crate) fn add_if<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    let ([t], params) = scope.generics_from_names(["T"]);
    scope.add_func(
        "if",
            XFuncSpec::new(&[&X_BOOL, &t, &t], t.clone()).generic(params),
        XStaticFunction::from_native(
            |args, ns, tca, rt| {
                let [a0,] = eval(args, ns, &rt,[0])?;
                ns.eval(&args[if *to_primitive!(a0, Bool) { 1 } else { 2 }], rt, tca)
            },
        ),
    )
}

add_ufunc!(
    add_error,
    error,
    X_STRING,
    String,
    X_UNKNOWN,
    |a: &String| Err(a.clone())
);

pub(crate) fn add_cast<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    let ([t], params) = scope.generics_from_names(["T"]);
    scope.add_func(
        "cast",
            XFuncSpec::new(&[&t], t.clone()).generic(params),
        XStaticFunction::from_native(
            |args, ns, tca, rt| ns.eval(&args[0], rt, tca),
        ),
    )
}

pub(crate) fn add_debug<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    let ([t], params) = scope.generics_from_names(["T"]);
    scope.add_func(
        "debug",
            XFuncSpec::new_with_optional(&[&t], &[&X_STRING], t.clone()).generic(params),
        XStaticFunction::from_native(
            |args: &[XExpr<W>], ns, _tca, rt| {
                let [a0,] = eval(args, ns, &rt,[0])?;
                let [a1,] = eval_if_present(args, ns, &rt, [1])?;
                let b = to_primitive!(a1, String, "".to_string());
                writeln!(rt.borrow_mut().stdout, "{b}{a0:?}")
                    .map_err(|e| format!("failed writing to output: {e:?}"))?;
                Ok(a0.into())
            },
        ),
    )
}

pub(crate) fn add_is_error<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    let ([t], params) = scope.generics_from_names(["T"]);
    scope.add_func(
        "is_error",
            XFuncSpec::new(&[&t], X_BOOL.clone()).generic(params),
        XStaticFunction::from_native(
            |args, ns, _tca, rt| {
                let [a0] = eval_result(args, ns, &rt, [0])?;
                Ok(ManagedXValue::new(XValue::Bool(a0.is_err()), rt)?.into())
            },
        ),
    )
}

pub(crate) fn add_if_error<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    let ([t], params) = scope.generics_from_names(["T"]);
    scope.add_func(
        "if_error",
            XFuncSpec::new(&[&t, &t], t.clone()).generic(params),
        XStaticFunction::from_native(
            |args, ns, tca, rt| {
                let a0 = ns.eval(&args[0], rt.clone(), false);
                a0.or_else(|_e| ns.eval(&args[1], rt.clone(), tca))
            },
        ),
    )
}

pub(crate) fn add_ne<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
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
                let [a0, a1] = eval_result(args, ns, &rt,[0, 1])?;
                let eq = eval_resolved_func(&eq_expr, ns, rt.clone(), vec![a0, a1])??;
                let is_eq = to_primitive!(eq, Bool);
                Ok(ManagedXValue::new(XValue::Bool(!*is_eq), rt)?.into())
            },
        ))
    })
}

pub(crate) fn add_display<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    let to_str_symbol = scope.identifier("to_str");

    scope.add_dyn_func("display", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let (t, t1) = unpack_types!(types, 0 | 1);
        if let Some(t1) = t1 {
            if let XType::String = t1.as_ref() {
            } else {
                return Err(format!("argument 2 must be a string, got {:?}", t1));
            }
        }

        let inner_to_str = get_func(ns, to_str_symbol, &[t.clone()], &X_STRING)?;

        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new_with_optional(&[&t.clone()], &[&X_STRING], t.clone()),
            move |args, ns, _tca, rt| {
                let [a0,] = eval_result(args, ns, &rt,[0])?;
                let [a1,] = eval_if_present(args, ns, &rt, [1])?;
                let b = to_primitive!(a1, String, "".to_string());
                let string = eval_resolved_func(&inner_to_str, ns, rt.clone(), vec![a0.clone()])??;
                let str_slice = to_primitive!(string, String);
                writeln!(rt.borrow_mut().stdout, "{b}{str_slice}")
                    .map_err(|e| format!("failed writing to output: {e:?}"))?;
                a0.map(|i| i.into())
            },
        ))
    })
}

pub(crate) fn add_cmp_lt<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
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
                let [a0, a1] = eval_result(args, ns, &rt,[0, 1])?;
                let cmp = eval_resolved_func(&inner_func, ns, rt.clone(), vec![a0, a1])??;
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
) -> Result<(), CompilationError<W>> {
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
                let [a0, a1] = eval_result(args, ns, &rt,[0, 1])?;
                let cmp = eval_resolved_func(&inner_func, ns, rt.clone(), vec![a0, a1])??;
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
) -> Result<(), CompilationError<W>> {
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
                let [a0, a1] = eval_result(args, ns, &rt,[0, 1])?;
                let cmp = eval_resolved_func(&inner_func, ns, rt.clone(), vec![a0, a1])??;
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
) -> Result<(), CompilationError<W>> {
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
                let [a0, a1] = eval_result(args, ns, &rt,[0, 1])?;
                let cmp = eval_resolved_func(&inner_func, ns, rt.clone(), vec![a0, a1])??;
                Ok(
                    ManagedXValue::new(XValue::Bool(!to_primitive!(cmp, Int).is_positive()), rt)?
                        .into(),
                )
            },
        ))
    })
}
