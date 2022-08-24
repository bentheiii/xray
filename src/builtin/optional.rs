use crate::native_types::{NativeType, XNativeValue};
use crate::xexpr::XExpr;
use crate::xtype::{XFuncParamSpec, XFuncSpec, X_BOOL, X_UNKNOWN};
use crate::xvalue::{ManagedXValue, XValue};
use crate::XType::XCallable;
use crate::{
    eval, manage_native, to_native, to_primitive, CompilationError, Identifier,
    RootCompilationScope, XCallableSpec, XCompilationScope, XStaticFunction, XType,
};
use rc::Rc;
use std::rc;
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

#[derive(Debug)]
pub(crate) struct XOptional {
    pub(crate) value: Option<Rc<ManagedXValue>>,
}

impl XNativeValue for XOptional {
    fn size(&self) -> usize {
        1
    }
}

pub(crate) fn add_optional_type(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t], _) = scope.generics_from_names(["T"]);
    scope.add_native_type("Optional", XOptionalType::xtype(t))
}

pub(crate) fn add_optional_null(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    scope.add_func(
        "null",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: None,
                params: vec![],
                ret: XOptionalType::xtype(X_UNKNOWN.clone()),
            },
            |_args, _ns, _tca, rt| Ok(manage_native!(XOptional { value: None }, rt)),
        ),
    )
}

pub(crate) fn add_optional_some(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    scope.add_func(
        "some",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![XFuncParamSpec {
                    type_: t.clone(),
                    required: true,
                }],
                ret: XOptionalType::xtype(t),
            },
            |args, ns, _tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                Ok(manage_native!(XOptional { value: Some(a0) }, rt))
            },
        ),
    )
}

pub(crate) fn add_optional_map(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t_in, t_out], params) = scope.generics_from_names(["T_IN", "T_OUT"]);
    scope.add_func(
        "map",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: XOptionalType::xtype(t_in.clone()),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: Arc::new(XCallable(XCallableSpec {
                            param_types: vec![t_in],
                            return_type: t_out.clone(),
                        })),
                        required: true,
                    },
                ],
                ret: XOptionalType::xtype(t_out),
            },
            |args, ns, _tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                let opt0 = &to_native!(a0, XOptional).value;
                Ok(match opt0 {
                    None => a0.into(),
                    Some(v) => {
                        let (a1,) = eval!(args, ns, rt, 1);
                        let f1 = to_primitive!(a1, Function);
                        manage_native!(
                            XOptional {
                                value: Some(f1.eval_values(&[v.clone()], ns, rt.clone())?)
                            },
                            rt
                        )
                    }
                })
            },
        ),
    )
}

pub(crate) fn add_optional_map_or(
    scope: &mut RootCompilationScope,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    scope.add_func(
        "map_or",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: XOptionalType::xtype(t.clone()),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: Arc::new(XCallable(XCallableSpec {
                            param_types: vec![t.clone()],
                            return_type: t.clone(),
                        })),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: t.clone(),
                        required: true,
                    },
                ],
                ret: t,
            },
            |args, ns, tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                let opt0 = &to_native!(a0, XOptional).value;
                match opt0 {
                    None => Ok(args[2].eval(ns, tca, rt)?),
                    Some(v) => {
                        let (a1,) = eval!(args, ns, rt, 1);
                        let f1 = to_primitive!(a1, Function);
                        Ok(f1.eval_values(&[v.clone()], ns, rt)?.into())
                    }
                }
            },
        ),
    )
}

pub(crate) fn add_optional_or_unwrap(
    scope: &mut RootCompilationScope,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let opt_t = XOptionalType::xtype(t.clone());
    scope.add_func(
        "or",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: opt_t,
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: t.clone(),
                        required: true,
                    },
                ],
                ret: t,
            },
            |args, ns, tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                let opt0 = &to_native!(a0, XOptional).value;
                Ok(match opt0 {
                    None => args[1].eval(ns, tca, rt)?,
                    Some(v) => v.clone().into(),
                })
            },
        ),
    )
}

pub(crate) fn add_optional_or(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let opt_t = XOptionalType::xtype(t);
    scope.add_func(
        "or",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: opt_t.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: opt_t.clone(),
                        required: true,
                    },
                ],
                ret: opt_t,
            },
            |args, ns, tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                let opt0 = &to_native!(a0, XOptional).value;
                Ok(match opt0 {
                    None => args[1].eval(ns, tca, rt)?,
                    Some(_) => a0.clone().into(),
                })
            },
        ),
    )
}

pub(crate) fn add_optional_and(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let opt_t = XOptionalType::xtype(t);
    scope.add_func(
        "and",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: opt_t.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: opt_t.clone(),
                        required: true,
                    },
                ],
                ret: opt_t,
            },
            |args, ns, tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                let opt0 = &to_native!(a0, XOptional).value;
                Ok(match opt0 {
                    Some(_) => args[1].eval(ns, tca, rt)?,
                    None => a0.clone().into(),
                })
            },
        ),
    )
}

pub(crate) fn add_optional_has_value(
    scope: &mut RootCompilationScope,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let opt_t = XOptionalType::xtype(t);
    scope.add_func(
        "has_value",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![XFuncParamSpec {
                    type_: opt_t,
                    required: true,
                }],
                ret: X_BOOL.clone(),
            },
            |args, ns, _tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                let opt0 = &to_native!(a0, XOptional).value;
                Ok(ManagedXValue::new(XValue::Bool(opt0.is_some()), rt)?.into())
            },
        ),
    )
}

pub(crate) fn add_optional_value(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let opt_t = XOptionalType::xtype(t.clone());
    scope.add_func(
        "value",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![XFuncParamSpec {
                    type_: opt_t,
                    required: true,
                }],
                ret: t,
            },
            |args, ns, _tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                let opt0 = to_native!(a0, XOptional).value.clone();
                Ok(opt0.unwrap().into())
            },
        ),
    )
}

pub(crate) fn add_optional_eq(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let eq_symbol = scope.identifier("eq");

    fn static_from_eq(t0: Arc<XType>, t1: Arc<XType>, eq_expr: XExpr) -> Rc<XStaticFunction> {
        Rc::new(XStaticFunction::from_native(
            XFuncSpec {
                generic_params: None,
                params: vec![
                    XFuncParamSpec {
                        type_: XOptionalType::xtype(t0),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: XOptionalType::xtype(t1),
                        required: true,
                    },
                ],
                ret: X_BOOL.clone(),
            },
            move |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let opt0 = &to_native!(a0, XOptional).value;
                let opt1 = &to_native!(a1, XOptional).value;
                if opt0.is_some() && opt1.is_some() {
                    let v0 = opt0.clone().unwrap();
                    let v1 = opt1.clone().unwrap();
                    let inner_equal_value = eq_expr.eval(ns, false, rt.clone())?.unwrap_value();
                    let inner_eq_func = to_primitive!(inner_equal_value, Function);
                    let eq = inner_eq_func.eval_values(&[v0, v1], ns, rt)?;
                    Ok(eq.into())
                } else {
                    Ok(
                        ManagedXValue::new(XValue::Bool(opt0.is_some() == opt1.is_some()), rt)?
                            .into(),
                    )
                }
            },
        ))
    }

    fn from_types(
        types: &[Arc<XType>],
        scope: &XCompilationScope,
        eq_symbol: Identifier,
    ) -> Result<Rc<XStaticFunction>, String> {
        if types.len() != 2 {
            return Err(format!("Expected 2 types, got {}", types.len()));
        }
        let a0 = types[0].clone();
        let t0 = match &a0.as_ref() {
            XType::XNative(nt0, bind) if nt0.name() == "Optional" => bind[0].clone(),
            _ => return Err(format!("Expected optional type, got {:?}", a0)), // todo improve
        };
        let a1 = types[1].clone();
        let t1 = match &a1.as_ref() {
            XType::XNative(nt1, bind) if nt1.name() == "Optional" => bind[0].clone(),
            _ => return Err(format!("Expected optional type, got {:?}", a1)), // todo improve
        };

        let inner_eq = scope.resolve_overload(eq_symbol, &[t0.clone(), t1.clone()])?; // todo ensure that the function returns a bool

        Ok(static_from_eq(t0, t1, inner_eq))
    }

    scope.add_dyn_func("eq", move |_params, types, ns| {
        from_types(types, ns, eq_symbol)
    })
}
