use std::rc;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use crate::{add_binop, add_ufunc, add_ufunc_ref, Bind, eval, to_primitive, XCompilationScope, XStaticFunction, XType};
use crate::xtype::{X_BOOL, X_INT, X_RATIONAL, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec};
use crate::xvalue::{XValue, ManagedXValue};
use rc::Rc;
use std::sync::Arc;
use string_interner::StringInterner;

pub fn add_bool_type(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    scope.add_native_type(interner.get_or_intern_static("bool"), X_BOOL.clone())
}

add_binop!(add_bool_eq, pow, X_BOOL, Bool, X_BOOL, |a,b|
    Ok(XValue::Bool(a == b))
);

add_ufunc!(add_bool_display, display, X_BOOL, Bool, X_STRING, |a:&bool| {
    println!("{}", a);
    Ok(XValue::String(a.to_string()).into())
});

add_ufunc_ref!(add_assert, display, X_BOOL, X_BOOL, |a:Rc<ManagedXValue>, rt| {
    if let XValue::Bool(true) = a.value{
        Ok(a.into())
    }else{
        Err("assertion is untrue".to_string())
    }
});

add_ufunc!(add_bool_not, not, X_BOOL, Bool, X_BOOL, |a:&bool| {
    Ok(XValue::Bool(!a).into())
});

pub fn add_and(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    scope.add_func_intern(
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
        }, |args, ns, tca, rt| {
            let (a0,) = eval!(args, ns, rt, 0);
            if *to_primitive!(a0, Bool) {
                return Ok(args[1].eval(&ns, tca, rt)?);
            }
            Ok(a0.into())
        }), interner)?;
    Ok(())
}

pub fn add_or(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    scope.add_func_intern(
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
        }, |args, ns, tca, rt| {
            let (a0,) = eval!(args, ns, rt, 0);
            if !*to_primitive!(a0, Bool) {
                return Ok(args[1].eval(&ns, tca, rt)?);
            }
            Ok(a0.into())
        }), interner)?;
    Ok(())
}

