use crate::builtin::core::{eval, get_func, unpack_dyn_types, unpack_native, xerr};
use crate::native_types::{NativeType, XNativeValue};
use crate::runtime_scope::RuntimeScope;
use crate::util::fenced_string::FencedString;
use crate::util::lazy_bigint::LazyBigint;
use crate::xexpr::XExpr;
use crate::xtype::{XFuncSpec, X_BOOL, X_INT, X_STRING, X_UNKNOWN};
use crate::xvalue::{ManagedXError, ManagedXValue, XFunctionFactoryOutput, XValue};
use crate::XType::XCallable;
use crate::{
    forward_err, manage_native, to_native, to_primitive, xraise, xraise_opt,
    CompilationError, RootCompilationScope, XCallableSpec, XStaticFunction, XType,
};
use derivative::Derivative;
use num_traits::Zero;
use std::fmt::Debug;
use std::io::Write;
use std::rc::Rc;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub(crate) struct XOptionalType {}

impl XOptionalType {
    pub(crate) fn xtype(t: Arc<XType>) -> Arc<XType> {
        Arc::new(XType::XNative(Box::new(Self {}), vec![t]))
    }
}

impl NativeType for XOptionalType {
    fn generic_names(&self) -> Vec<String> {
        vec!["T".to_string()]
    }
    fn name(&self) -> &str {
        "Optional"
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub(crate) struct XOptional<W> {
    pub(crate) value: Option<Rc<ManagedXValue<W>>>,
}

impl<W: 'static> XNativeValue for XOptional<W> {
    fn dyn_size(&self) -> usize {
        0
    }
}

pub(crate) fn add_optional_type<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], _) = scope.generics_from_names(["T"]);
    scope.add_native_type("Optional", XOptionalType::xtype(t))
}

pub(crate) fn add_optional_none<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "none",
        XFuncSpec::new(&[], XOptionalType::xtype(X_UNKNOWN.clone())),
        XStaticFunction::from_native(|_args, _ns, _tca, rt| {
            Ok(manage_native!(XOptional::<W> { value: None }, rt))
        }),
    )
}

pub(crate) fn add_optional_some<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    scope.add_func(
        "some",
        XFuncSpec::new(&[&t], XOptionalType::xtype(t.clone())).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            Ok(manage_native!(XOptional { value: Some(a0) }, rt))
        }),
    )
}

pub(crate) fn add_optional_map<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t_in, t_out], params) = scope.generics_from_names(["T_IN", "T_OUT"]);
    scope.add_func(
        "map",
        XFuncSpec::new(
            &[
                &XOptionalType::xtype(t_in.clone()),
                &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![t_in],
                    return_type: t_out.clone(),
                })),
            ],
            XOptionalType::xtype(t_out),
        )
        .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let opt0 = &to_native!(a0, XOptional<W>).value;
            Ok(match opt0 {
                None => a0.into(),
                Some(v) => {
                    let a1 = xraise!(eval(&args[1], ns, &rt)?);
                    let f1 = to_primitive!(a1, Function);
                    manage_native!(
                        XOptional {
                            value: Some(xraise!(ns
                                .eval_func_with_values(f1, vec![Ok(v.clone())], rt.clone(), false)?
                                .unwrap_value()))
                        },
                        rt
                    )
                }
            })
        }),
    )
}

pub(crate) fn add_optional_map_or<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    scope.add_func(
        "map_or",
        XFuncSpec::new(
            &[
                &XOptionalType::xtype(t.clone()),
                &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![t.clone()],
                    return_type: t.clone(),
                })),
                &t,
            ],
            t.clone(),
        )
        .generic(params),
        XStaticFunction::from_native(|args, ns, tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let opt0 = &to_native!(a0, XOptional<W>).value;
            match opt0 {
                None => Ok(ns.eval(&args[2], rt, tca)?),
                Some(v) => {
                    let a1 = xraise!(eval(&args[1], ns, &rt)?);
                    let f1 = to_primitive!(a1, Function);
                    Ok(xraise!(ns
                        .eval_func_with_values(f1, vec![Ok(v.clone())], rt, false)?
                        .unwrap_value())
                    .into())
                }
            }
        }),
    )
}

pub(crate) fn add_optional_or_unwrap<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let opt_t = XOptionalType::xtype(t.clone());
    scope.add_func(
        "or",
        XFuncSpec::new(&[&opt_t, &t], t.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let opt0 = &to_native!(a0, XOptional<W>).value;
            Ok(match opt0 {
                None => ns.eval(&args[1], rt, tca)?,
                Some(v) => v.clone().into(),
            })
        }),
    )
}

pub(crate) fn add_optional_or<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let opt_t = XOptionalType::xtype(t);
    scope.add_func(
        "or",
        XFuncSpec::new(&[&opt_t, &opt_t], opt_t.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let opt0 = &to_native!(a0, XOptional<W>).value;
            Ok(match opt0 {
                None => ns.eval(&args[1], rt, tca)?,
                Some(_) => a0.clone().into(),
            })
        }),
    )
}

pub(crate) fn add_optional_and<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let opt_t = XOptionalType::xtype(t);
    scope.add_func(
        "and",
        XFuncSpec::new(&[&opt_t, &opt_t], opt_t.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let opt0 = &to_native!(a0, XOptional<W>).value;
            Ok(match opt0 {
                Some(_) => ns.eval(&args[1], rt, tca)?,
                None => a0.clone().into(),
            })
        }),
    )
}

pub(crate) fn add_optional_has_value<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let opt_t = XOptionalType::xtype(t);
    scope.add_func(
        "has_value",
        XFuncSpec::new(&[&opt_t], X_BOOL.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let opt0 = &to_native!(a0, XOptional<W>).value;
            Ok(ManagedXValue::new(XValue::Bool(opt0.is_some()), rt)?.into())
        }),
    )
}

pub(crate) fn add_optional_value<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let opt_t = XOptionalType::xtype(t.clone());
    scope.add_func(
        "value",
        XFuncSpec::new_with_optional(&[&opt_t], &[&X_STRING], t).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise_opt!(args.get(1).map(|e| eval(e, ns, &rt)).transpose()?);
            let err_msg = match a1 {
                None => "optional has no value",
                Some(ref a) => to_primitive!(a, String).as_str(),
            };
            let Some(ref opt0) = to_native!(a0, XOptional<W>).value else { return xerr(ManagedXError::new(err_msg, rt)?); };
            Ok(opt0.clone().into())
        }),
    )
}

pub(crate) fn add_optional_dyn_eq<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let eq_symbol = scope.identifier("eq");

    scope.add_dyn_func("eq", "options", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let [a0, a1] = unpack_dyn_types(types)?;
        let [t0] = unpack_native(a0, "Optional")? else { unreachable!() };
        let [t1] = unpack_native(a1, "Optional")? else { unreachable!() };
        let inner_eq = get_func(ns, eq_symbol, &[t0.clone(), t1.clone()], &X_BOOL)?;

        Ok(XFunctionFactoryOutput::from_delayed_native(
            XFuncSpec::new(
                &[
                    &XOptionalType::xtype(t0.clone()),
                    &XOptionalType::xtype(t1.clone()),
                ],
                X_BOOL.clone(),
            ),
            move |ns, rt| {
                let inner_value = forward_err!(ns.eval(&inner_eq, rt, false)?.unwrap_value());
                Ok(Ok(
                    move |args: &[XExpr<W>], ns: &RuntimeScope<'_, W>, _tca, rt| {
                        let a0 = xraise!(eval(&args[0], ns, &rt)?);
                        let a1 = xraise!(eval(&args[1], ns, &rt)?);
                        let opt0 = &to_native!(a0, XOptional<W>).value;
                        let opt1 = &to_native!(a1, XOptional<W>).value;
                        if opt0.is_some() && opt1.is_some() {
                            let v0 = opt0.clone().unwrap();
                            let v1 = opt1.clone().unwrap();
                            let func = to_primitive!(inner_value, Function);
                            let eq = xraise!(ns
                                .eval_func_with_values(func, vec![Ok(v0), Ok(v1)], rt, false)?
                                .unwrap_value());
                            Ok(eq.into())
                        } else {
                            Ok(ManagedXValue::new(
                                XValue::Bool(opt0.is_some() == opt1.is_some()),
                                rt,
                            )?
                            .into())
                        }
                    },
                ))
            },
        ))
    })
}

pub(crate) fn add_optional_dyn_hash<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let symbol = scope.identifier("hash");

    scope.add_dyn_func("hash", "options", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let [a0] = unpack_dyn_types(types)?;
        let [t0] = unpack_native(a0, "Optional")? else { unreachable!() };
        let inner = get_func(ns, symbol, &[t0.clone()], &X_INT)?;

        Ok(XFunctionFactoryOutput::from_delayed_native(
            XFuncSpec::new(&[&XOptionalType::xtype(t0.clone())], X_INT.clone()),
            move |ns, rt| {
                let inner_value = forward_err!(ns.eval(&inner, rt, false)?.unwrap_value());
                Ok(Ok(
                    move |args: &[XExpr<W>], ns: &RuntimeScope<'_, W>, _tca, rt| {
                        let a0 = xraise!(eval(&args[0], ns, &rt)?);
                        let opt0 = &to_native!(a0, XOptional<W>).value;
                        if let Some(v) = opt0 {
                            let func = to_primitive!(inner_value, Function);
                            let hash = xraise!(ns
                                .eval_func_with_values(func, vec![Ok(v.clone())], rt, false)?
                                .unwrap_value());
                            Ok(hash.into())
                        } else {
                            Ok(ManagedXValue::new(XValue::Int(LazyBigint::zero()), rt)?.into())
                        }
                    },
                ))
            },
        ))
    })
}

pub(crate) fn add_optional_dyn_to_string<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let symbol = scope.identifier("to_str");

    scope.add_dyn_func("to_str", "options", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let [a0] = unpack_dyn_types(types)?;
        let [t0] = unpack_native(a0, "Optional")? else { unreachable!() };
        let inner = get_func(ns, symbol, &[t0.clone()], &X_STRING)?;

        Ok(XFunctionFactoryOutput::from_delayed_native(
            XFuncSpec::new(&[&XOptionalType::xtype(t0.clone())], X_STRING.clone()),
            move |ns, rt| {
                let inner_value = forward_err!(ns.eval(&inner, rt, false)?.unwrap_value());
                Ok(Ok(
                    move |args: &[XExpr<W>], ns: &RuntimeScope<'_, W>, _tca, rt| {
                        let a0 = xraise!(eval(&args[0], ns, &rt)?);
                        let opt0 = &to_native!(a0, XOptional<W>).value;
                        if let Some(v) = opt0 {
                            let func = to_primitive!(inner_value, Function);
                            let inner_v = xraise!(ns
                                .eval_func_with_values(func, vec![Ok(v.clone())], rt, false)?
                                .unwrap_value());
                            Ok(inner_v.into())
                        } else {
                            Ok(ManagedXValue::new(
                                XValue::String(Box::new(FencedString::from_str("None"))),
                                rt,
                            )?
                            .into())
                        }
                    },
                ))
            },
        ))
    })
}
