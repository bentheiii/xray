use std::rc;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use crate::{add_binop, add_ufunc, add_ufunc_ref, Bind, XCompilationScope, XStaticFunction, XType};
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
            generic_params: Some(vec!["T"].iter().map(|s| interner.get_or_intern_static(s)).collect()),
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
            let cond = args[0].eval(&ns, false)?.unwrap_value();
            match cond.as_ref() {
                XValue::Bool(true) => args[1].eval(&ns, tca),
                XValue::Bool(false) => args[2].eval(&ns, tca),
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}

add_ufunc!(add_error, error, X_STRING, XValue::String, X_UNKNOWN, |a: &String| Err(a.clone()));

pub fn add_cast(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    scope.add_func_intern(
        "cast", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["T"].iter().map(|s| interner.get_or_intern_static(s)).collect()),
            params: vec![
                XFuncParamSpec {
                    type_: t.clone(),
                    required: true,
                },
            ],
            ret: t,
        }, |args, ns, tca| {
            args[0].eval(&ns, true)
        }), interner)?;
    Ok(())
}