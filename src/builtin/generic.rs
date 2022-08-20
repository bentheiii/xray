use std::rc;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use crate::{add_binop, add_ufunc, add_ufunc_ref, to_primitive, Bind, XCompilationScope, XStaticFunction, XType, eval, intern, CompilationError, meval};
use crate::xtype::{X_BOOL, X_INT, X_FLOAT, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec};
use crate::xvalue::{XValue, ManagedXValue};
use rc::Rc;
use std::sync::Arc;
use mopa::Any;
use string_interner::StringInterner;

pub fn add_if(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let t = XType::generic_from_name("T", interner);
    scope.add_func(
        interner.get_or_intern_static("if"), XStaticFunction::from_native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
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
        }, |args, ns, tca, rt| {
            let (a0, ) = eval!(args, ns, rt, 0);
            args[match to_primitive!(a0, Bool) {
                true => 1,
                false => 2,
            }].eval(&ns, tca, rt)
        }))?;
    Ok(())
}

add_ufunc!(add_error, error, X_STRING, String, X_UNKNOWN, |a: &String| Err(a.clone()));

pub fn add_cast(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let t = XType::generic_from_name("T", interner);
    scope.add_func_intern(
        "cast", XStaticFunction::from_native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: t.clone(),
                    required: true,
                },
            ],
            ret: t,
        }, |args, ns, tca, rt| {
            args[0].eval(&ns, tca, rt)
        }), interner)?;
    Ok(())
}

pub fn add_debug(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let t = XType::generic_from_name("T", interner);
    scope.add_func_intern(
        "debug", XStaticFunction::from_native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
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
        }, |args, ns, tca, rt| {
            let (a0, ) = eval!(args, ns, rt, 0);
            let (a1,) = meval!(args, ns, rt, 1);
            let b = to_primitive!(a1, String, "".to_string());
            println!("{}{:?}", b, a0);
            Ok(a0.into())
        }), interner)?;
    Ok(())
}