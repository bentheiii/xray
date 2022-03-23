use std::rc;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use crate::{add_binop, add_ufunc, add_ufunc_ref, Bind, XCompilationScope, XStaticFunction, XType};
use crate::xtype::{X_BOOL, X_INT, X_RATIONAL, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec};
use crate::xvalue::{XValue};
use rc::Rc;
use std::sync::Arc;

pub fn add_bool_type(scope: &mut XCompilationScope) -> Result<(), String> {
    scope.add_native_type("bool", X_BOOL.clone())
}

add_binop!(add_bool_eq, pow, X_BOOL, XValue::Bool, X_BOOL, |a,b|
    Ok(XValue::Bool(a == b).into())
);

add_ufunc!(add_bool_display, display, X_BOOL, XValue::Bool, X_STRING, |a:&bool| {
    println!("{}", a);
    Ok(XValue::String(a.to_string()).into())
});

add_ufunc_ref!(add_assert, display, X_BOOL, X_BOOL, |a:Rc<XValue>| {
    if let XValue::Bool(true) = a.as_ref(){
        Ok(a.into())
    }else{
        Err("assertion is untrue".to_string())
    }
});

add_ufunc!(add_bool_not, not, X_BOOL, XValue::Bool, X_BOOL, |a:&bool| {
    Ok(XValue::Bool(!a).into())
});

pub fn add_and(scope: &mut XCompilationScope) -> Result<(), String> {
    scope.add_func(
        "and", XStaticFunction::Native(XFuncSpec {
            generic_params: None,
            params: vec![
                XFuncParamSpec {
                    type_: X_BOOL.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: X_BOOL.clone(),
                    required: true,
                },
            ],
            ret: X_BOOL.clone(),
        }, |args, ns, tca| {
            let lhs = args[0].eval(&ns, false)?.unwrap_value();
            if let XValue::Bool(true) = lhs.as_ref() {
                return Ok(args[1].eval(&ns, tca)?);
            }
            Ok(lhs.into())
        }))?;
    Ok(())
}

pub fn add_or(scope: &mut XCompilationScope) -> Result<(), String> {
    scope.add_func(
        "or", XStaticFunction::Native(XFuncSpec {
            generic_params: None,
            params: vec![
                XFuncParamSpec {
                    type_: X_BOOL.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: X_BOOL.clone(),
                    required: true,
                },
            ],
            ret: X_BOOL.clone(),
        }, |args, ns, tca| {
            let lhs = args[0].eval(&ns, false)?.unwrap_value();
            if let XValue::Bool(false) = lhs.as_ref() {
                return Ok(args[1].eval(&ns, tca)?);
            }
            Ok(lhs.into())
        }))?;
    Ok(())
}

