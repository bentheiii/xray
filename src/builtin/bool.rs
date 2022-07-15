use std::rc;
use num::{BigInt, BigRational, Signed, ToPrimitive, One, Zero};
use crate::{add_binop, add_ufunc, add_ufunc_ref, Bind, CompilationError, eval, manage_native, to_primitive, XCompilationScope, XOptional, XOptionalType, XStaticFunction, XType};
use crate::xtype::{X_BOOL, X_INT, X_RATIONAL, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec};
use crate::xvalue::{XValue, ManagedXValue};
use rc::Rc;
use std::sync::Arc;
use string_interner::StringInterner;
use crate::builtin::core::xcmp;

pub fn add_bool_type(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    scope.add_native_type(interner.get_or_intern_static("bool"), X_BOOL.clone())
}

add_binop!(add_bool_eq, eq, X_BOOL, Bool, X_BOOL, |a,b|
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

pub fn add_and(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    scope.add_func_intern(
        "and", XStaticFunction::from_native(XFuncSpec {
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
            let (a0, ) = eval!(args, ns, rt, 0);
            if *to_primitive!(a0, Bool) {
                return Ok(args[1].eval(&ns, tca, rt)?);
            }
            Ok(a0.into())
        }), interner)?;
    Ok(())
}

pub fn add_or(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    scope.add_func_intern(
        "or", XStaticFunction::from_native(XFuncSpec {
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
            let (a0, ) = eval!(args, ns, rt, 0);
            if !*to_primitive!(a0, Bool) {
                return Ok(args[1].eval(&ns, tca, rt)?);
            }
            Ok(a0.into())
        }), interner)?;
    Ok(())
}

pub fn add_bool_then(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let t = XType::generic_from_name("T", interner);

    scope.add_func_intern(
        "then", XStaticFunction::from_native(XFuncSpec {
            generic_params: None,
            params: vec![
                XFuncParamSpec {
                    type_: X_BOOL.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: t.clone(),
                    required: true,
                },
            ],
            ret: XOptionalType::xtype(t.clone()),
        }, |args, ns, tca, rt| {
            let (a0, ) = eval!(args, ns, rt, 0);
            Ok(manage_native!(XOptional{value: if *to_primitive!(a0, Bool) {
                        let (a1,) = eval!(args, ns, rt, 1);
                        Some(a1)
            } else {
                        None
            }}, rt))
        }), interner)?;
    Ok(())
}

add_ufunc!(add_bool_hash, hash, X_BOOL, Bool, X_INT, |a:&bool| {
    Ok(XValue::Int(if *a {One::one()} else {Zero::zero()}))
});

add_binop!(add_bool_cmp, cmp, X_BOOL, Bool, X_INT, |a,b|
    Ok(xcmp(a,b))
);