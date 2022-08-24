use crate::xexpr::XExpr;
use crate::xtype::{XFuncParamSpec, XFuncSpec, X_BOOL, X_STRING, X_UNKNOWN};
use crate::xvalue::{ManagedXValue, XValue};
use crate::{
    add_ufunc, add_ufunc_ref, eval, meval, to_primitive, CompilationError, Identifier,
    RootCompilationScope, XCompilationScope, XStaticFunction, XType,
};
use rc::Rc;
use std::rc;
use std::sync::Arc;

pub(crate) fn add_if(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    scope.add_func(
        "if",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: X_BOOL.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: t.clone(),
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
                args[match to_primitive!(a0, Bool) {
                    true => 1,
                    false => 2,
                }]
                .eval(ns, tca, rt)
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

pub(crate) fn add_cast(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    scope.add_func(
        "cast",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![XFuncParamSpec {
                    type_: t.clone(),
                    required: true,
                }],
                ret: t,
            },
            |args, ns, tca, rt| args[0].eval(ns, tca, rt),
        ),
    )
}

pub(crate) fn add_debug(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    scope.add_func(
        "debug",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: t.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: X_STRING.clone(),
                        required: false,
                    },
                ],
                ret: t,
            },
            |args, ns, _tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                let (a1,) = meval!(args, ns, rt, 1);
                let b = to_primitive!(a1, String, "".to_string());
                println!("{}{:?}", b, a0);
                Ok(a0.into())
            },
        ),
    )
}

pub(crate) fn add_is_error(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    scope.add_func(
        "is_error",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![XFuncParamSpec {
                    type_: t,
                    required: true,
                }],
                ret: X_BOOL.clone(),
            },
            |args, ns, _tca, rt| {
                let a0 = args[0].eval(ns, false, rt.clone());
                Ok(ManagedXValue::new(XValue::Bool(a0.is_err()), rt)?.into())
            },
        ),
    )
}

pub(crate) fn add_if_error(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    scope.add_func(
        "if_error",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: t.clone(),
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
                let a0 = args[0].eval(ns, false, rt.clone());
                a0.or_else(|_e| args[1].eval(ns, tca, rt.clone()))
            },
        ),
    )
}

pub(crate) fn add_ne(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let eq_symbol = scope.identifier("eq");

    fn static_from_eq(t0: Arc<XType>, t1: Arc<XType>, eq_expr: XExpr) -> Rc<XStaticFunction> {
        Rc::new(XStaticFunction::from_native(
            XFuncSpec {
                generic_params: None,
                params: vec![
                    XFuncParamSpec {
                        type_: t0,
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: t1,
                        required: true,
                    },
                ],
                ret: X_BOOL.clone(),
            },
            move |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let inner_equal_value = eq_expr.eval(ns, false, rt.clone())?.unwrap_value();
                let inner_eq_func = to_primitive!(inner_equal_value, Function);
                let eq = inner_eq_func.eval_values(&[a0, a1], ns, rt.clone())?;
                let is_eq = to_primitive!(eq, Bool);
                Ok(ManagedXValue::new(XValue::Bool(!*is_eq), rt)?.into())
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
        let t0 = types[0].clone();
        let t1 = types[1].clone();

        let inner_eq = scope.resolve_overload(eq_symbol, types)?; // todo ensure that the function returns a bool

        Ok(static_from_eq(t0, t1, inner_eq))
    }

    scope.add_dyn_func("ne", move |_params, types, ns| {
        from_types(types, ns, eq_symbol)
    })
}

pub(crate) fn add_display(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let to_str_symbol = scope.identifier("to_str");

    fn static_from_to_str(t: Arc<XType>, to_str_expr: XExpr) -> Rc<XStaticFunction> {
        Rc::new(XStaticFunction::from_native(
            XFuncSpec {
                generic_params: None,
                params: vec![
                    XFuncParamSpec {
                        type_: t.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: X_STRING.clone(),
                        required: false,
                    },
                ],
                ret: t,
            },
            move |args, ns, _tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                let (a1,) = meval!(args, ns, rt, 1);
                let b = to_primitive!(a1, String, "".to_string());
                let to_str_value = to_str_expr.eval(ns, false, rt.clone())?.unwrap_value();
                let to_str_func = to_primitive!(to_str_value, Function);
                let string = to_str_func.eval_values(&[a0.clone()], ns, rt.clone())?;
                let str_slice = to_primitive!(string, String);
                println!("{b}{str_slice}");
                Ok(a0.into())
            },
        ))
    }

    fn from_types(
        types: &[Arc<XType>],
        scope: &XCompilationScope,
        to_str_symbol: Identifier,
    ) -> Result<Rc<XStaticFunction>, String> {
        if types.len() < 1 || types.len() > 2  {
            return Err(format!("Expected 1 or 2 type, got {}", types.len()));
        }
        let t0 = types[0].clone();
        if let Some(t1) = types.get(1){
            if let XType::String = t1.as_ref(){}
            else {
                return Err(format!("argument 2 must be a string, got {:?}", t1))
            }
        }

        let inner_to_str = scope.resolve_overload(to_str_symbol, &[t0.clone()])?; // todo ensure that the function returns a bool

        Ok(static_from_to_str(t0, inner_to_str))
    }

    scope.add_dyn_func("display", move |_params, types, ns| {
        from_types(types, ns, to_str_symbol)
    })
}