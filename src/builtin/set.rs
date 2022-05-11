use std::rc;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use crate::{add_binop, add_ufunc, add_ufunc_ref, Bind, XCompilationScope, XStaticFunction, XType};
use crate::xtype::{X_BOOL, X_INT, X_RATIONAL, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec};
use crate::xvalue::{XValue};
use rc::Rc;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use crate::native_types::{NativeType, XNativeValue};
use derivative::Derivative;
use crate::builtin::stack::{XStack, XStackType};

#[derive(Debug, Clone)]
pub struct XSetType {}

impl XSetType {
    pub fn xtype(t: Arc<XType>) -> Arc<XType> {
        Arc::new(XType::XNative(Box::new(Self {}),
                                HashMap::from([('T'.to_string(), t)])))
    }
}

impl NativeType for XSetType {
    fn generic_names(&self) -> Vec<String> {
        vec!["T".to_string()]
    }
    fn name(&self) -> String { "set".to_string() }
}

#[derive(Debug, Eq, PartialEq, Derivative)]
#[derivative(Hash)]
pub struct XSet {
    #[derivative(Hash = "ignore")]
    pub value: HashSet<Rc<XValue>>,
}

impl XSet {
    pub fn new(value: HashSet<Rc<XValue>>) -> Self {
        Self { value }
    }
}

impl XNativeValue for XSet {}

pub fn add_set_type(scope: &mut XCompilationScope) -> Result<(), String> {
    scope.add_native_type("Set", XSetType::xtype(XType::XGeneric("T".to_string()).into()))
}

pub fn add_set_bitor(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let set_t = XSetType::xtype(t.clone());

    scope.add_func(
        "bit_or", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["T".to_string()]),
            params: vec![
                XFuncParamSpec {
                    type_: set_t.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: set_t.clone(),
                    required: true,
                },
            ],
            ret: set_t.clone(),
        }, |args, ns, _tca| {
            let s1 = args[0].eval(&ns, false)?.unwrap_value();
            let s2 = args[1].eval(&ns, false)?.unwrap_value();
            match (s1.as_ref(), s2.as_ref()) {
                (XValue::Native(b1), XValue::Native(b2)) => {
                    let s1 = &b1.as_ref()._as_any().downcast_ref::<XSet>().unwrap().value;
                    let s2 = &b2.as_ref()._as_any().downcast_ref::<XSet>().unwrap().value;

                    Ok(XValue::Native(Box::new(XSet::new(s1.union(s2).cloned().collect()))).into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}

pub fn add_set_bitand(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let set_t = XSetType::xtype(t.clone());

    scope.add_func(
        "bit_and", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["T".to_string()]),
            params: vec![
                XFuncParamSpec {
                    type_: set_t.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: set_t.clone(),
                    required: true,
                },
            ],
            ret: set_t.clone(),
        }, |args, ns, _tca| {
            let s1 = args[0].eval(&ns, false)?.unwrap_value();
            let s2 = args[1].eval(&ns, false)?.unwrap_value();
            match (s1.as_ref(), s2.as_ref()) {
                (XValue::Native(b1), XValue::Native(b2)) => {
                    let s1 = &b1.as_ref()._as_any().downcast_ref::<XSet>().unwrap().value;
                    let s2 = &b2.as_ref()._as_any().downcast_ref::<XSet>().unwrap().value;

                    Ok(XValue::Native(Box::new(XSet::new(s1.intersection(s2).cloned().collect()))).into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}

pub fn add_set_bitxor(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let set_t = XSetType::xtype(t);

    scope.add_func(
        "bit_xor", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["T".to_string()]),
            params: vec![
                XFuncParamSpec {
                    type_: set_t.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: set_t.clone(),
                    required: true,
                },
            ],
            ret: set_t.clone(),
        }, |args, ns, _tca| {
            let s1 = args[0].eval(&ns, false)?.unwrap_value();
            let s2 = args[1].eval(&ns, false)?.unwrap_value();
            match (s1.as_ref(), s2.as_ref()) {
                (XValue::Native(b1), XValue::Native(b2)) => {
                    let s1 = &b1.as_ref()._as_any().downcast_ref::<XSet>().unwrap().value;
                    let s2 = &b2.as_ref()._as_any().downcast_ref::<XSet>().unwrap().value;

                    Ok(XValue::Native(Box::new(XSet::new(s1.symmetric_difference(s2).cloned().collect()))).into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}

pub fn add_set_sub(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let set_t = XSetType::xtype(t.clone());

    scope.add_func(
        "bit_sub", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["T".to_string()]),
            params: vec![
                XFuncParamSpec {
                    type_: set_t.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: set_t.clone(),
                    required: true,
                },
            ],
            ret: set_t.clone(),
        }, |args, ns, _tca| {
            let s1 = args[0].eval(&ns, false)?.unwrap_value();
            let s2 = args[1].eval(&ns, false)?.unwrap_value();
            match (s1.as_ref(), s2.as_ref()) {
                (XValue::Native(b1), XValue::Native(b2)) => {
                    let s1 = &b1.as_ref()._as_any().downcast_ref::<XSet>().unwrap().value;
                    let s2 = &b2.as_ref()._as_any().downcast_ref::<XSet>().unwrap().value;

                    Ok(XValue::Native(Box::new(XSet::new(s1.difference(s2).cloned().collect()))).into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}

pub fn add_set_to_stack(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));

    scope.add_func(
        "to_stack", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["T".to_string()]),
            params: vec![
                XFuncParamSpec {
                    type_: XSetType::xtype(t.clone()),
                    required: true,
                },
            ],
            ret: XStackType::xtype(t.clone()),
        }, |args, ns, _tca| {
            let arr = args[0].eval(&ns, false)?.unwrap_value();
            match arr.as_ref() {
                XValue::Native(b) => {
                    let arr = &b.as_ref()._as_any().downcast_ref::<XSet>().unwrap().value;
                    let mut ret = XStack::new();
                    for x in arr {
                        ret = ret.push(x.clone());
                    }
                    Ok(XValue::Native(Box::new(ret)).into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}