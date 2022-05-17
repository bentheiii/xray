use std::rc;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use crate::{add_binop, add_ufunc, add_ufunc_ref, Bind, eval, intern, manage_native, to_native, to_primitive, XArray, XArrayType, XCallableSpec, XCompilationScope, XSet, XSetType, XStaticFunction, XType};
use crate::xtype::{X_BOOL, X_INT, X_RATIONAL, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec};
use crate::xvalue::{ManagedXValue, XValue};
use rc::Rc;
use std::collections::{HashMap, HashSet};
use std::mem::size_of;
use std::sync::Arc;
use crate::native_types::{NativeType, XNativeValue};
use derivative::Derivative;
use string_interner::StringInterner;
use crate::XType::XCallable;

#[derive(Debug, Clone)]
pub struct XOptionalType {}

impl XOptionalType {
    pub fn xtype(t: Arc<XType>) -> Arc<XType> {
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

#[derive(Debug, Eq, PartialEq, Hash)]
struct XOptional {
    pub value: Option<Rc<ManagedXValue>>,
}

impl XNativeValue for XOptional {
    fn size(&self) -> usize {
        1
    }
}

pub fn add_optional_type(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    scope.add_native_type_intern("Optional", XOptionalType::xtype(XType::generic_from_name("T", interner)), interner)
}

pub fn add_optional_null(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    scope.add_func_intern(
        "null", XStaticFunction::Native(XFuncSpec {
            generic_params: None,
            params: vec![],
            ret: XOptionalType::xtype(X_UNKNOWN.clone()),
        }, |_args, _ns, _tca, rt| {
            Ok(manage_native!(XOptional { value: None }, rt))
        }), interner)?;
    Ok(())
}

pub fn add_optional_some(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    scope.add_func_intern(
        "some", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![XFuncParamSpec {
                type_: t.clone(),
                required: true,
            }],
            ret: XOptionalType::xtype(t),
        }, |args, ns, _tca, rt| {
            let (a0, ) = eval!(args, ns, rt, 0);
            Ok(manage_native!(XOptional { value: Some(a0) }, rt))
        }), interner)?;
    Ok(())
}

pub fn add_optional_map(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    scope.add_func_intern(
        "map", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
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
                }],
            ret: XOptionalType::xtype(t),
        }, |args, ns, _tca, rt| {
            let (a0, ) = eval!(args, ns, rt, 0);
            let opt0 = &to_native!(a0, XOptional).value;
            Ok(match opt0 {
                None => a0.into(),
                Some(v) => {
                    let (a1, ) = eval!(args, ns, rt, 1);
                    let f1 = to_primitive!(a1, Function);
                    manage_native!(XOptional {
                        value: Some(f1.eval_values(vec![v.clone()], &ns, rt.clone())?)
                    }, rt)
                }
            })
        }), interner)?;
    Ok(())
}

pub fn add_optional_map_or(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    scope.add_func_intern(
        "map_or", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
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
        }, |args, ns, tca, rt| {
            let (a0, ) = eval!(args, ns, rt, 0);
            let opt0 = &to_native!(a0, XOptional).value;
            match opt0 {
                None => Ok(args[2].eval(&ns, tca, rt)?),
                Some(v) => {
                    let (a1, ) = eval!(args, ns, rt, 1);
                    let f1 = to_primitive!(a1, Function);
                    Ok(f1.eval_values(vec![v.clone()], &ns, rt)?.into())
                }
            }
        }), interner)?;
    Ok(())
}

pub fn add_optional_or_unwrap(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    let opt_t = XOptionalType::xtype(t.clone());
    scope.add_func_intern(
        "or", XStaticFunction::Native(
            XFuncSpec {
                generic_params: Some(intern!(interner, "T")),
                params: vec![
                    XFuncParamSpec {
                        type_: opt_t.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: t.clone(),
                        required: true,
                    }],
                ret: t.clone(),
            },
            |args, ns, tca, rt| {
                let (a0, ) = eval!(args, ns, rt, 0);
                let opt0 = &to_native!(a0, XOptional).value;
                Ok(match opt0 {
                    None => args[1].eval(&ns, tca, rt)?,
                    Some(v) => v.clone().into()
                })
            },
        ), interner)?;
    Ok(())
}

pub fn add_optional_or(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    let opt_t = XOptionalType::xtype(t.clone());
    scope.add_func_intern(
        "or", XStaticFunction::Native(
            XFuncSpec {
                generic_params: Some(intern!(interner, "T")),
                params: vec![
                    XFuncParamSpec {
                        type_: opt_t.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: opt_t.clone(),
                        required: true,
                    }],
                ret: opt_t.clone(),
            },
            |args, ns, tca, rt| {
                let (a0, ) = eval!(args, ns, rt, 0);
                let opt0 = &to_native!(a0, XOptional).value;
                Ok(match opt0 {
                    None => args[1].eval(&ns, tca, rt)?,
                    Some(_) => a0.clone().into()
                })
            },
        ), interner)?;
    Ok(())
}

pub fn add_optional_and(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    let opt_t = XOptionalType::xtype(t.clone());
    scope.add_func_intern(
        "and", XStaticFunction::Native(
            XFuncSpec {
                generic_params: Some(intern!(interner, "T")),
                params: vec![
                    XFuncParamSpec {
                        type_: opt_t.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: opt_t.clone(),
                        required: true,
                    }],
                ret: opt_t,
            },
            |args, ns, tca, rt| {
                let (a0, ) = eval!(args, ns, rt, 0);
                let opt0 = &to_native!(a0, XOptional).value;
                Ok(match opt0 {
                    Some(_) => args[1].eval(&ns, tca, rt)?,
                    None => a0.clone().into()
                })
            },
        ), interner)?;
    Ok(())
}

pub fn add_optional_has_value(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    let opt_t = XOptionalType::xtype(t.clone());
    scope.add_func_intern(
        "has_value", XStaticFunction::Native(
            XFuncSpec {
                generic_params: Some(intern!(interner, "T")),
                params: vec![
                    XFuncParamSpec {
                        type_: opt_t.clone(),
                        required: true,
                    },
                ],
                ret: X_BOOL.clone(),
            },
            |args, ns, _tca, rt| {
                let (a0, ) = eval!(args, ns, rt, 0);
                let opt0 = &to_native!(a0, XOptional).value;
                Ok(ManagedXValue::new(XValue::Bool(opt0.is_some()), rt)?.into())
            },
        ), interner)?;
    Ok(())
}

pub fn add_optional_value(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    let opt_t = XOptionalType::xtype(t.clone());
    scope.add_func_intern(
        "value", XStaticFunction::Native(
            XFuncSpec {
                generic_params: Some(intern!(interner, "T")),
                params: vec![
                    XFuncParamSpec {
                        type_: opt_t.clone(),
                        required: true,
                    },
                ],
                ret: t.clone(),
            },
            |args, ns, _tca, rt| {
                let (a0, ) = eval!(args, ns, rt, 0);
                let opt0 = to_native!(a0, XOptional).value.clone();
                Ok(opt0.unwrap().clone().into())
            },
        ), interner)?;
    Ok(())
}