use crate::builtin::core::{eval, eval_resolved_func, get_func, unpack_native, xerr};
use crate::native_types::{NativeType, XNativeValue};
use crate::xtype::{XFuncSpec, X_BOOL, X_UNKNOWN};
use crate::xvalue::{ManagedXError, ManagedXValue, XFunctionFactoryOutput, XValue};
use crate::XType::XCallable;
use crate::{
    manage_native, to_native, to_primitive, unpack_types, xraise, CompilationError,
    RootCompilationScope, XCallableSpec, XStaticFunction, XType,
};
use derivative::Derivative;
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

pub(crate) fn add_optional_null<W: Write + 'static>(
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
        XFuncSpec::new(&[&opt_t], t).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let Some(opt0) = to_native!(a0, XOptional<W>).value.clone() else { return xerr(ManagedXError::new("optional has no value", rt)?)};
            Ok(opt0.into())
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

        let (a0, a1) = unpack_types!(types, 0, 1);
        let [t0] = unpack_native(a0, "Optional")? else {unreachable!()};
        let [t1] = unpack_native(a1, "Optional")? else {unreachable!()};
        let inner_eq = get_func(ns, eq_symbol, &[t0.clone(), t1.clone()], &X_BOOL)?;

        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(
                &[
                    &XOptionalType::xtype(t0.clone()),
                    &XOptionalType::xtype(t1.clone()),
                ],
                X_BOOL.clone(),
            ),
            move |args, ns, _tca, rt| {
                let a0 = xraise!(eval(&args[0], ns, &rt)?);
                let a1 = xraise!(eval(&args[1], ns, &rt)?);
                let opt0 = &to_native!(a0, XOptional<W>).value;
                let opt1 = &to_native!(a1, XOptional<W>).value;
                if opt0.is_some() && opt1.is_some() {
                    let v0 = opt0.clone().unwrap();
                    let v1 = opt1.clone().unwrap();
                    let eq = eval_resolved_func(&inner_eq, ns, rt, vec![Ok(v0), Ok(v1)])?;
                    Ok(eq.into())
                } else {
                    Ok(
                        ManagedXValue::new(XValue::Bool(opt0.is_some() == opt1.is_some()), rt)?
                            .into(),
                    )
                }
            },
        ))
    })
}
