use crate::xexpr::XExpr;
use crate::xtype::{Bind, XFuncSpec, X_BOOL, X_INT, X_STRING, X_UNKNOWN};
use crate::xvalue::{ManagedXValue, XFunction, XFunctionFactoryOutput, XValue};
use crate::{
    forward_err, manage_native, to_primitive, ufunc, unpack_types, xraise, xraise_opt,
    CompilationError, RootCompilationScope, XStaticFunction, XType,
};
use rc::Rc;

use crate::builtin::builtin_permissions;
use crate::builtin::core::{eval, get_func};
use crate::builtin::optional::{XOptional, XOptionalType};
use crate::runtime::RTCell;
use crate::runtime_scope::RuntimeScope;
use crate::runtime_violation::RuntimeViolation;
use crate::util::fenced_string::FencedString;
use num_traits::Signed;
use std::io::Write;
use std::sync::Arc;
use std::{iter, rc};

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
        ufunc!(String, |a: &FencedString, rt: RTCell<W>| {
            rt.borrow().can_allocate(a.bytes())?;
            Ok(Err(a.to_string()))
        }),
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
            rt.borrow()
                .limits
                .check_permission(&builtin_permissions::PRINT_DEBUG)?;
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise_opt!(args.get(1).map(|e| eval(e, ns, &rt)).transpose()?);
            if let Some(a1) = a1 {
                let b = to_primitive!(a1, String).as_str();
                writeln!(rt.borrow_mut().stdout, "{b}{a0:?}")
            } else {
                writeln!(rt.borrow_mut().stdout, "{a0:?}")
            }
            .map_err(|e| RuntimeViolation::OutputFailure(Rc::new(e)))?;
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
        XFuncSpec::new_with_optional(&[&t], &[&X_STRING], X_BOOL.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = eval(&args[0], ns, &rt)?;
            let a1 = xraise_opt!(args.get(1).map(|e| eval(e, ns, &rt)).transpose()?);
            if let Some(a1) = a1 {
                let s1 = to_primitive!(a1, String);

                Ok(ManagedXValue::new(
                    XValue::Bool(match &a0 {
                        Ok(_) => false,
                        Err(e) if e.error.contains(s1.as_str()) => true,
                        _ => return Ok(a0.into()),
                    }),
                    rt,
                )?
                .into())
            } else {
                Ok(ManagedXValue::new(XValue::Bool(a0.is_err()), rt)?.into())
            }
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

pub(crate) fn add_if_error_specific<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    scope.add_func(
        "if_error",
        XFuncSpec::new(&[&t, &X_STRING, &t], t.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, tca, rt| {
            let a0 = eval(&args[0], ns, &rt)?;
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let s1 = to_primitive!(a1, String);
            if let Err(ref err) = a0 {
                if err.error.contains(s1.as_str()) {
                    return ns.eval(&args[2], rt, tca);
                }
            }
            Ok(a0.into())
        }),
    )
}

pub(crate) fn add_get_error<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "get_error",
        XFuncSpec::new(&[&X_UNKNOWN], XOptionalType::xtype(X_STRING.clone())),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = eval(&args[0], ns, &rt)?;
            Ok(manage_native!(
                XOptional {
                    value: if let Err(e) = a0 {
                        Some(ManagedXValue::new(
                            XValue::String(Box::new(FencedString::from_str(&e.error))),
                            rt.clone(),
                        )?)
                    } else {
                        None
                    }
                },
                rt
            ))
        }),
    )
}

pub(crate) fn add_ne<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let eq_symbol = scope.identifier("eq");

    scope.add_dyn_func("ne", "eq-inverse", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }
        let (t0, t1) = unpack_types!(types, 0, 1);

        let eq_expr = get_func(ns, eq_symbol, &[t0.clone(), t1.clone()], &X_BOOL)?;

        Ok(XFunctionFactoryOutput::from_delayed_native(
            XFuncSpec::new(&[t0, t1], X_BOOL.clone()),
            move |ns, rt| {
                let inner_value = forward_err!(ns.eval(&eq_expr, rt, false)?.unwrap_value());
                Ok(Ok(
                    move |args: &[XExpr<W>], ns: &RuntimeScope<'_, W>, _tca, rt| {
                        let a0 = eval(&args[0], ns, &rt)?;
                        let a1 = eval(&args[1], ns, &rt)?;
                        let func = to_primitive!(inner_value, Function);
                        let eq = xraise!(ns
                            .eval_func_with_values(func, vec![a0, a1], rt.clone(), false)?
                            .unwrap_value());
                        let is_eq = to_primitive!(eq, Bool);
                        Ok(ManagedXValue::new(XValue::Bool(!*is_eq), rt)?.into())
                    },
                ))
            },
        ))
    })
}

pub(crate) fn add_display<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let to_str_symbol = scope.identifier("to_str");

    scope.add_dyn_func(
        "display",
        "print-to_str",
        move |_params, types, ns, bind| {
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

            Ok(XFunctionFactoryOutput::from_delayed_native(
                XFuncSpec::new_with_optional(&[&t.clone()], &[&X_STRING], t.clone()),
                move |ns, rt| {
                    let inner_value =
                        forward_err!(ns.eval(&inner_to_str, rt, false)?.unwrap_value());
                    Ok(Ok(
                        move |args: &[XExpr<W>], ns: &RuntimeScope<'_, W>, _tca, rt: RTCell<W>| {
                            rt.borrow()
                                .limits
                                .check_permission(&builtin_permissions::PRINT)?;
                            let a0 = eval(&args[0], ns, &rt)?;
                            let a1 =
                                xraise_opt!(args.get(1).map(|e| eval(e, ns, &rt)).transpose()?);
                            let func = to_primitive!(inner_value, Function);
                            let string = xraise!(ns
                                .eval_func_with_values(func, vec![a0.clone()], rt.clone(), false)?
                                .unwrap_value());
                            let str_slice = to_primitive!(string, String);
                            if let Some(a1) = a1 {
                                let b = to_primitive!(a1, String).as_str();
                                writeln!(rt.borrow_mut().stdout, "{b}{str_slice}")
                            } else {
                                writeln!(rt.borrow_mut().stdout, "{str_slice}")
                            }
                            .map_err(|e| RuntimeViolation::OutputFailure(Rc::new(e)))?;
                            Ok(a0.into())
                        },
                    ))
                },
            ))
        },
    )
}

pub(crate) fn add_cmp_lt<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let cmp_symbol = scope.identifier("cmp");

    scope.add_dyn_func("lt", "use-cmp", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }
        let (t0, t1) = unpack_types!(types, 0, 1);

        let inner_func = get_func(ns, cmp_symbol, &[t0.clone(), t1.clone()], &X_INT)?;

        Ok(XFunctionFactoryOutput::from_delayed_native(
            XFuncSpec::new(&[t0, t1], X_BOOL.clone()),
            move |ns, rt| {
                let inner_value = forward_err!(ns.eval(&inner_func, rt, false)?.unwrap_value());
                Ok(Ok(
                    move |args: &[XExpr<W>], ns: &RuntimeScope<'_, W>, _tca, rt| {
                        let a0 = eval(&args[0], ns, &rt)?;
                        let a1 = eval(&args[1], ns, &rt)?;
                        let func = to_primitive!(inner_value, Function);
                        let cmp = xraise!(ns
                            .eval_func_with_values(func, vec![a0, a1], rt.clone(), false)?
                            .unwrap_value());
                        Ok(ManagedXValue::new(
                            XValue::Bool(to_primitive!(cmp, Int).is_negative()),
                            rt,
                        )?
                        .into())
                    },
                ))
            },
        ))
    })
}

pub(crate) fn add_cmp_gt<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let cmp_symbol = scope.identifier("cmp");

    scope.add_dyn_func("gt", "use-cmp", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let (t0, t1) = unpack_types!(types, 0, 1);

        let inner_func = get_func(ns, cmp_symbol, &[t0.clone(), t1.clone()], &X_INT)?;

        Ok(XFunctionFactoryOutput::from_delayed_native(
            XFuncSpec::new(&[t0, t1], X_BOOL.clone()),
            move |ns, rt| {
                let inner_value = forward_err!(ns.eval(&inner_func, rt, false)?.unwrap_value());
                Ok(Ok(
                    move |args: &[XExpr<W>], ns: &RuntimeScope<'_, W>, _tca, rt| {
                        let a0 = eval(&args[0], ns, &rt)?;
                        let a1 = eval(&args[1], ns, &rt)?;
                        let func = to_primitive!(inner_value, Function);
                        let cmp = xraise!(ns
                            .eval_func_with_values(func, vec![a0, a1], rt.clone(), false)?
                            .unwrap_value());
                        Ok(ManagedXValue::new(
                            XValue::Bool(to_primitive!(cmp, Int).is_positive()),
                            rt,
                        )?
                        .into())
                    },
                ))
            },
        ))
    })
}

pub(crate) fn add_cmp_ge<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let cmp_symbol = scope.identifier("cmp");

    scope.add_dyn_func("ge", "use-cmp", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }
        let (t0, t1) = unpack_types!(types, 0, 1);

        let inner_func = get_func(ns, cmp_symbol, &[t0.clone(), t1.clone()], &X_INT)?;

        Ok(XFunctionFactoryOutput::from_delayed_native(
            XFuncSpec::new(&[t0, t1], X_BOOL.clone()),
            move |ns, rt| {
                let inner_value = forward_err!(ns.eval(&inner_func, rt, false)?.unwrap_value());
                Ok(Ok(
                    move |args: &[XExpr<W>], ns: &RuntimeScope<'_, W>, _tca, rt| {
                        let a0 = eval(&args[0], ns, &rt)?;
                        let a1 = eval(&args[1], ns, &rt)?;
                        let func = to_primitive!(inner_value, Function);
                        let cmp = xraise!(ns
                            .eval_func_with_values(func, vec![a0, a1], rt.clone(), false)?
                            .unwrap_value());
                        Ok(ManagedXValue::new(
                            XValue::Bool(!to_primitive!(cmp, Int).is_negative()),
                            rt,
                        )?
                        .into())
                    },
                ))
            },
        ))
    })
}

pub(crate) fn add_cmp_le<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let cmp_symbol = scope.identifier("cmp");

    scope.add_dyn_func("le", "use-cmp", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let (t0, t1) = unpack_types!(types, 0, 1);

        let inner_func = get_func(ns, cmp_symbol, &[t0.clone(), t1.clone()], &X_INT)?;

        Ok(XFunctionFactoryOutput::from_delayed_native(
            XFuncSpec::new(&[t0, t1], X_BOOL.clone()),
            move |ns, rt| {
                let inner_value = forward_err!(ns.eval(&inner_func, rt, false)?.unwrap_value());
                Ok(Ok(
                    move |args: &[XExpr<W>], ns: &RuntimeScope<'_, W>, _tca, rt| {
                        let a0 = eval(&args[0], ns, &rt)?;
                        let a1 = eval(&args[1], ns, &rt)?;
                        let func = to_primitive!(inner_value, Function);
                        let cmp = xraise!(ns
                            .eval_func_with_values(func, vec![a0, a1], rt.clone(), false)?
                            .unwrap_value());
                        Ok(ManagedXValue::new(
                            XValue::Bool(!to_primitive!(cmp, Int).is_positive()),
                            rt,
                        )?
                        .into())
                    },
                ))
            },
        ))
    })
}

pub(crate) fn add_cast<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_dyn_func("cast", "", move |_params, _types, _ns, bind| {
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

pub(crate) fn add_partial<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_dyn_func("partial", "currying", move |_params, types, _ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let (t0, ) = unpack_types!(types, 0);
        let XType::XFunc(spec) = t0.as_ref() else { return Err("first argument must be a function".to_string()); };
        let mut binding = Bind::new();
        let star_types = types.unwrap().iter().skip(1).collect::<Vec<_>>();
        if star_types.len() > spec.params.len() {
            return Err("function has less parameters than supplied".to_string());
        }
        for (param_spec, t) in spec.params.iter().zip(star_types.iter()) {
            if let Some(b) = param_spec.type_.bind_in_assignment(t).and_then(|b| binding.mix(&b)) {
                binding = b;
            } else {
                return Err("arguments to function must match".to_string());
            }
        }
        if !binding.is_empty() {
            return Err("cannot curry generic parameters".to_string());
        }

        let new_spec = spec.skip_args(star_types.len());

        let arg_types = iter::once(t0).chain(star_types).collect::<Vec<_>>();

        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(&arg_types, Arc::new(XType::XFunc(new_spec))),
            move |args, ns, _tca, rt| {
                let a0 = xraise!(eval(&args[0], ns, &rt)?);

                let curried = xraise!(args.iter().skip(1).map(|e| eval(e, ns, &rt)).collect::<Result<Result<Vec<_>,_>,_>>()?);
                let curried = curried.into_iter().map(|v| XExpr::Dummy(Ok(v))).collect::<Vec<_>>();

                let f = XValue::Function(XFunction::Native(Rc::new(move |inner_args, inner_ns, tca, rt| {
                    let new_args: Vec<_> = curried.iter().chain(inner_args).cloned().collect();
                    let f0 = to_primitive!(a0, Function).clone();
                    inner_ns.eval_func_with_expressions(&f0, &new_args, rt, tca)
                })));
                Ok(
                    ManagedXValue::new(f, rt)?
                        .into(),
                )
            },
        ))
    })
}
