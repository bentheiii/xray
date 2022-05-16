use std::rc;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use crate::{add_binop, add_ufunc, add_ufunc_ref, to_primitive, Bind, XCompilationScope, XStaticFunction, XType, eval, intern};
use crate::xtype::{X_BOOL, X_INT, X_RATIONAL, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec};
use crate::xvalue::{XValue};
use rc::Rc;
use std::sync::Arc;
use mopa::Any;
use string_interner::StringInterner;

pub fn add_if(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    scope.add_func(
        interner.get_or_intern_static("if"), XStaticFunction::Native(XFuncSpec {
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
        }, |args, ns, tca| {
            let (a0,) = eval!(args, ns, 0);
            args[match to_primitive!(a0, Bool) {
                true => 1,
                false => 2,
            }].eval(&ns, tca)
        }))?;
    Ok(())
}

add_ufunc!(add_error, error, X_STRING, String, X_UNKNOWN, |a: &String| Err(a.clone()));

pub fn add_cast(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    scope.add_func_intern(
        "cast", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: t.clone(),
                    required: true,
                },
            ],
            ret: t,
        }, |args, ns, tca| {
            args[0].eval(&ns, tca)
        }), interner)?;
    Ok(())
}