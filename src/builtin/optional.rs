use std::rc;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use crate::{add_binop, add_ufunc, add_ufunc_ref, Bind, XArray, XArrayType, XCallableSpec, XCompilationScope, XSet, XSetType, XStaticFunction, XType};
use crate::xtype::{X_BOOL, X_INT, X_RATIONAL, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec};
use crate::xvalue::{XValue};
use rc::Rc;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use crate::native_types::{NativeType, XNativeValue};
use derivative::Derivative;
use crate::XType::XCallable;

#[derive(Debug, Clone)]
pub struct XOptionalType {}

impl XOptionalType {
    pub fn xtype(t: Arc<XType>) -> Arc<XType> {
        Arc::new(XType::XNative(Box::new(Self {}),
                                HashMap::from([('T'.to_string(), t)])))
    }
}

impl NativeType for XOptionalType {
    fn generic_names(&self) -> Vec<String> {
        vec!["T".to_string()]
    }
    fn name(&self) -> String {
        "Optional".to_string()
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
struct XOptional {
    pub value: Option<Rc<XValue>>,
}

impl XNativeValue for XOptional {}

pub fn add_optional_type(scope: &mut XCompilationScope) -> Result<(), String> {
    scope.add_native_type("Optional", XOptionalType::xtype(XType::XGeneric("T".to_string()).into()))
}

pub fn add_optional_null(scope: &mut XCompilationScope) -> Result<(), String> {
    scope.add_func(
        "null", XStaticFunction::Native(XFuncSpec {
            generic_params: None,
            params: vec![],
            ret: XOptionalType::xtype(X_UNKNOWN.clone()),
        }, |_args, _ns, _tca| {
            Ok(XValue::Native(Box::new(XOptional { value: None })).into())
        }))?;
    Ok(())
}

pub fn add_optional_some(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    scope.add_func(
        "some", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["T".to_string()]),
            params: vec![XFuncParamSpec {
                type_: t.clone(),
                required: true,
            }],
            ret: XOptionalType::xtype(t),
        }, |args, ns, _tca| {
            let value = args[0].eval(&ns, false)?.unwrap_value();
            Ok(XValue::Native(Box::new(XOptional { value: Some(value) })).into())
        }))?;
    Ok(())
}

pub fn add_optional_map(scope: &mut XCompilationScope) -> Result<(), String> {
    let in_t = Arc::new(XType::XGeneric("K".to_string()));
    let out_t = Arc::new(XType::XGeneric("V".to_string()));
    scope.add_func(
        "map", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["K".to_string(), "V".to_string()]),
            params: vec![
                XFuncParamSpec {
                    type_: XOptionalType::xtype(in_t.clone()),
                    required: true,
                },
                XFuncParamSpec {
                    type_: Arc::new(XCallable(XCallableSpec {
                        param_types: vec![in_t.clone()],
                        return_type: out_t.clone(),
                    })),
                    required: true,
                }],
            ret: XOptionalType::xtype(out_t),
        }, |args, ns, _tca| {
            let value = args[0].eval(&ns, false)?.unwrap_value();
            match value.as_ref() {
                XValue::Native(b) => {
                    let opt = &b.as_ref()._as_any().downcast_ref::<XOptional>().unwrap().value;
                    match opt {
                        None => Ok(value.into()),
                        Some(v) => {
                            let func = args[1].eval(&ns, false)?.unwrap_value();
                            match func.as_ref() {
                                XValue::Function(f) => Ok(XValue::Native(Box::new(XOptional {
                                    value: Some(f.eval_values(vec![v.clone()], &ns)?),
                                })).into()),
                                _ => unreachable!(),
                            }
                        }
                    }
                }
                _ => unreachable!()
            }
        }))?;
    Ok(())
}

pub fn add_optional_map_or(scope: &mut XCompilationScope) -> Result<(), String> {
    let in_t = Arc::new(XType::XGeneric("K".to_string()));
    let out_t = Arc::new(XType::XGeneric("V".to_string()));
    scope.add_func(
        "map_or", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["K".to_string(), "V".to_string()]),
            params: vec![
                XFuncParamSpec {
                    type_: XOptionalType::xtype(in_t.clone()),
                    required: true,
                },
                XFuncParamSpec {
                    type_: Arc::new(XCallable(XCallableSpec {
                        param_types: vec![in_t.clone()],
                        return_type: out_t.clone(),
                    })),
                    required: true,
                },
                XFuncParamSpec {
                    type_: out_t.clone(),
                    required: true,
                },
            ],
            ret: out_t.clone(),
        }, |args, ns, _tca| {
            let value = args[0].eval(&ns, false)?.unwrap_value();
            match value.as_ref() {
                XValue::Native(b) => {
                    let opt = &b.as_ref()._as_any().downcast_ref::<XOptional>().unwrap().value;
                    match opt {
                        None => Ok(args[2].eval(&ns, true)?),
                        Some(v) => {
                            let func = args[1].eval(&ns, false)?.unwrap_value();
                            match func.as_ref() {
                                XValue::Function(f) => Ok(f.eval_values(vec![v.clone()], &ns)?.into()),
                                _ => unreachable!(),
                            }
                        }
                    }
                }
                _ => unreachable!()
            }
        }))?;
    Ok(())
}

pub fn add_optional_or_unwrap(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let opt_t = XOptionalType::xtype(t.clone());
    scope.add_func(
        "or", XStaticFunction::Native(
            XFuncSpec {
                generic_params: Some(vec!["T".to_string()]),
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
            |args, ns, _tca| {
                let value = args[0].eval(&ns, false)?.unwrap_value();
                match value.as_ref() {
                    XValue::Native(b) => {
                        let opt = &b.as_ref()._as_any().downcast_ref::<XOptional>().unwrap().value;
                        match opt {
                            None => Ok(args[1].eval(&ns, true)?),
                            Some(v) => Ok(v.clone().into()),
                        }
                    }
                    _ => unreachable!()
                }
            },
        ))?;
    Ok(())
}

pub fn add_optional_or(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let opt_t = XOptionalType::xtype(t.clone());
    scope.add_func(
        "or", XStaticFunction::Native(
            XFuncSpec {
                generic_params: Some(vec!["T".to_string()]),
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
            |args, ns, _tca| {
                let value = args[0].eval(&ns, false)?.unwrap_value();
                match value.as_ref() {
                    XValue::Native(b) => {
                        let opt = &b.as_ref()._as_any().downcast_ref::<XOptional>().unwrap().value;
                        match opt {
                            None => Ok(args[1].eval(&ns, true)?),
                            Some(_) => Ok(value.clone().into()),
                        }
                    }
                    _ => unreachable!()
                }
            },
        ))?;
    Ok(())
}

pub fn add_optional_and(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let opt_t = XOptionalType::xtype(t.clone());
    scope.add_func(
        "and", XStaticFunction::Native(
            XFuncSpec {
                generic_params: Some(vec!["T".to_string()]),
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
            |args, ns, _tca| {
                let value = args[0].eval(&ns, false)?.unwrap_value();
                match value.as_ref() {
                    XValue::Native(b) => {
                        let opt = &b.as_ref()._as_any().downcast_ref::<XOptional>().unwrap().value;
                        match opt {
                            None => Ok(value.clone().into()),
                            Some(_) => Ok(args[1].eval(&ns, true)?),
                        }
                    }
                    _ => unreachable!()
                }
            },
        ))?;
    Ok(())
}

pub fn add_optional_has_value(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let opt_t = XOptionalType::xtype(t.clone());
    scope.add_func(
        "has_value", XStaticFunction::Native(
            XFuncSpec {
                generic_params: Some(vec!["T".to_string()]),
                params: vec![
                    XFuncParamSpec {
                        type_: opt_t.clone(),
                        required: true,
                    },
                ],
                ret: X_BOOL.clone(),
            },
            |args, ns, _tca| {
                let value = args[0].eval(&ns, false)?.unwrap_value();
                match value.as_ref() {
                    XValue::Native(b) => {
                        let opt = &b.as_ref()._as_any().downcast_ref::<XOptional>().unwrap().value;
                        Ok(XValue::Bool(opt.is_some()).into())
                    }
                    _ => unreachable!()
                }
            },
        ))?;
    Ok(())
}

pub fn add_optional_value(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let opt_t = XOptionalType::xtype(t.clone());
    scope.add_func(
        "value", XStaticFunction::Native(
            XFuncSpec {
                generic_params: Some(vec!["T".to_string()]),
                params: vec![
                    XFuncParamSpec {
                        type_: opt_t.clone(),
                        required: true,
                    },
                ],
                ret: t.clone(),
            },
            |args, ns, _tca| {
                let value = args[0].eval(&ns, false)?.unwrap_value();
                match value.as_ref() {
                    XValue::Native(b) => {
                        let opt = &b.as_ref()._as_any().downcast_ref::<XOptional>().unwrap().value;
                        match opt {
                            None => Err(format!("Optional has no value")),
                            Some(v) => Ok(v.clone().into()),
                        }
                    }
                    _ => unreachable!()
                }
            },
        ))?;
    Ok(())
}