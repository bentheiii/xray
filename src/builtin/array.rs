use std::rc;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use crate::{add_binop, add_ufunc, add_ufunc_ref, Bind, XCompilationScope, XStaticFunction, XType};
use crate::xtype::{X_BOOL, X_INT, X_RATIONAL, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec};
use crate::xvalue::{XValue};
use rc::Rc;
use std::collections::HashMap;
use std::sync::Arc;
use crate::builtin::stack::{XStack, XStackType};
use crate::native_types::{NativeType, XNativeValue};

#[derive(Debug, Clone)]
pub struct XArrayType {}

impl XArrayType {
    pub fn xtype(t: Arc<XType>) -> Arc<XType> {
        Arc::new(XType::XNative(Box::new(Self {}),
                                HashMap::from([('T'.to_string(), t)])))
    }
}

impl NativeType for XArrayType {
    fn generic_names(&self) -> Vec<String> {
        vec!["T".to_string()]
    }
    fn name(&self) -> String { "Array".to_string() }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct XArray {
    pub value: Vec<Rc<XValue>>,
}

impl XArray {
    pub fn new(value: Vec<Rc<XValue>>) -> Self {
        Self { value }
    }
}

impl XNativeValue for XArray {}

fn value_to_idx(arr: &Vec<Rc<XValue>>, i: &BigInt) -> Result<usize, String> {
    let i = if i.is_negative() {
        i + arr.len()
    } else {
        i.clone()
    };
    if i.is_negative() {
        return Err("index too low".to_string());
    }
    let idx = i.to_usize().ok_or("index too large")?;
    if idx >= arr.len() {
        return Err("index out of bounds".to_string());
    }
    Ok(idx)
}

pub fn add_array_type(scope: &mut XCompilationScope) -> Result<(), String> {
    scope.add_native_type("Array", XArrayType::xtype(XType::XGeneric("T".to_string()).into()))
}

pub fn add_array_get(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));

    scope.add_func(
        "get", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["T".to_string()]),
            params: vec![
                XFuncParamSpec {
                    type_: XArrayType::xtype(t.clone()),
                    required: true,
                },
                XFuncParamSpec {
                    type_: X_INT.clone(),
                    required: true,
                },
            ],
            ret: t,
        }, |args, ns, _tca| {
            let arr = args[0].eval(&ns, false)?.unwrap_value();
            let idx = args[1].eval(&ns, false)?.unwrap_value();
            match (arr.as_ref(), idx.as_ref()) {
                (XValue::Native(b), XValue::Int(idx)) => {
                    let arr = &b.as_ref()._as_any().downcast_ref::<XArray>().unwrap().value;
                    let idx = value_to_idx(&arr, idx)?;
                    Ok(arr[idx].clone().into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}

pub fn add_array_len(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));

    scope.add_func(
        "len", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["T".to_string()]),
            params: vec![
                XFuncParamSpec {
                    type_: XArrayType::xtype(t.clone()),
                    required: true,
                },
            ],
            ret: X_INT.clone(),
        }, |args, ns, _tca| {
            let arr = args[0].eval(&ns, false)?.unwrap_value();
            match arr.as_ref() {
                XValue::Native(b) => {
                    let arr = &b.as_ref()._as_any().downcast_ref::<XArray>().unwrap().value;
                    Ok(XValue::Int(arr.len().into()).into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}

pub fn add_array_add(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let t_arr = XArrayType::xtype(t.clone());

    scope.add_func(
        "add", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["T".to_string()]),
            params: vec![
                XFuncParamSpec {
                    type_: t_arr.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: t_arr.clone(),
                    required: true,
                },
            ],
            ret: t_arr.clone(),
        }, |args, ns, _tca| {
            let v0 = args[0].eval(&ns, false)?.unwrap_value();
            let v1 = args[1].eval(&ns, false)?.unwrap_value();
            match (v0.as_ref(), v1.as_ref()) {
                (XValue::Native(b0), XValue::Native(b1)) => {
                    let arr0 = &b0.as_ref()._as_any().downcast_ref::<XArray>().unwrap().value;
                    let arr1 = &b1.as_ref()._as_any().downcast_ref::<XArray>().unwrap().value;
                    if arr0.is_empty() {
                        return Ok(v1.clone().into());
                    }
                    if arr1.is_empty() {
                        return Ok(v0.clone().into());
                    }
                    let mut arr = arr0.clone();
                    arr.append(&mut arr1.clone());
                    Ok(XValue::Native(Box::new(XArray::new(arr))).into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}

pub fn add_array_push(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let t_arr = XArrayType::xtype(t.clone());

    scope.add_func(
        "push", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["T".to_string()]),
            params: vec![
                XFuncParamSpec {
                    type_: t_arr.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: t.clone(),
                    required: true,
                },
            ],
            ret: t_arr.clone(),
        }, |args, ns, _tca| {
            let v0 = args[0].eval(&ns, false)?.unwrap_value();
            let el = args[1].eval(&ns, false)?.unwrap_value();
            match v0.as_ref() {
                XValue::Native(b0) => {
                    let arr = &b0.as_ref()._as_any().downcast_ref::<XArray>().unwrap().value;
                    let mut arr = arr.clone();
                    arr.push(el.clone());
                    Ok(XValue::Native(Box::new(XArray::new(arr))).into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}

pub fn add_array_rpush(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let t_arr = XArrayType::xtype(t.clone());

    scope.add_func(
        "rpush", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["T".to_string()]),
            params: vec![
                XFuncParamSpec {
                    type_: t.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: t_arr.clone(),
                    required: true,
                },
            ],
            ret: t_arr.clone(),
        }, |args, ns, _tca| {
            let el = args[0].eval(&ns, false)?.unwrap_value();
            let v0 = args[1].eval(&ns, false)?.unwrap_value();
            match v0.as_ref() {
                XValue::Native(b0) => {
                    let arr_ = &b0.as_ref()._as_any().downcast_ref::<XArray>().unwrap().value;
                    let mut arr = vec![el.clone()];
                    arr.extend(arr_.clone());
                    Ok(XValue::Native(Box::new(XArray::new(arr))).into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}

pub fn add_array_insert(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let t_arr = XArrayType::xtype(t.clone());

    scope.add_func(
        "insert", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["T".to_string()]),
            params: vec![
                XFuncParamSpec {
                    type_: t_arr.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: X_INT.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: t,
                    required: true,
                },
            ],
            ret: t_arr.clone(),
        }, |args, ns, _tca| {
            let arr = args[0].eval(&ns, false)?.unwrap_value();
            let idx = args[1].eval(&ns, false)?.unwrap_value();
            let el = args[2].eval(&ns, false)?.unwrap_value();
            match (arr.as_ref(), idx.as_ref()) {
                (XValue::Native(b), XValue::Int(idx)) => {
                    let arr = &b.as_ref()._as_any().downcast_ref::<XArray>().unwrap().value;
                    let idx = value_to_idx(arr, idx)?;
                    let mut ret: Vec<Rc<_>> = vec![];
                    ret.extend(arr.iter().take(idx - 1).map(|x| x.clone()));
                    ret.push(el.clone());
                    ret.extend(arr.iter().skip(idx - 1).map(|x| x.clone()));
                    Ok(XValue::Native(Box::new(XArray::new(ret))).into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}

pub fn add_array_pop(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let t_arr = XArrayType::xtype(t.clone());

    scope.add_func(
        "pop", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["T".to_string()]),
            params: vec![
                XFuncParamSpec {
                    type_: t_arr.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: X_INT.clone(),
                    required: true,
                },
            ],
            ret: t_arr.clone(),
        }, |args, ns, _tca| {
            let arr = args[0].eval(&ns, false)?.unwrap_value();
            let idx = args[1].eval(&ns, false)?.unwrap_value();
            match (arr.as_ref(), idx.as_ref()) {
                (XValue::Native(b), XValue::Int(idx)) => {
                    let arr = &b.as_ref()._as_any().downcast_ref::<XArray>().unwrap().value;
                    let idx = value_to_idx(arr, idx)?;
                    let mut ret: Vec<Rc<_>> = vec![];
                    ret.extend(arr.iter().take(idx).map(|x| x.clone()));
                    ret.extend(arr.iter().skip(idx + 1).map(|x| x.clone()));
                    Ok(XValue::Native(Box::new(XArray::new(ret))).into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}

pub fn add_array_set(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let t_arr = XArrayType::xtype(t.clone());

    scope.add_func(
        "set", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["T".to_string()]),
            params: vec![
                XFuncParamSpec {
                    type_: t_arr.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: X_INT.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: t,
                    required: true,
                },
            ],
            ret: t_arr.clone(),
        }, |args, ns, _tca| {
            let arr = args[0].eval(&ns, false)?.unwrap_value();
            let idx = args[1].eval(&ns, false)?.unwrap_value();
            let el = args[2].eval(&ns, false)?.unwrap_value();
            match (arr.as_ref(), idx.as_ref()) {
                (XValue::Native(b), XValue::Int(idx)) => {
                    let mut idx = idx.clone();
                    let arr = &b.as_ref()._as_any().downcast_ref::<XArray>().unwrap().value;
                    let idx = value_to_idx(arr, &idx)?;
                    let mut ret = vec![];
                    ret.extend(arr.iter().take(idx - 1).map(|x| x.clone()));
                    ret.push(el.clone());
                    ret.extend(arr.iter().skip(idx).map(|x| x.clone()));
                    Ok(XValue::Native(Box::new(XArray::new(ret))).into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}

pub fn add_array_swap(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let t_arr = XArrayType::xtype(t.clone());

    scope.add_func(
        "swap", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["T".to_string()]),
            params: vec![
                XFuncParamSpec {
                    type_: t_arr.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: X_INT.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: X_INT.clone(),
                    required: true,
                },
            ],
            ret: t_arr.clone(),
        }, |args, ns, _tca| {
            let arr_val = args[0].eval(&ns, false)?.unwrap_value();
            let idx0 = args[1].eval(&ns, false)?.unwrap_value();
            let idx1 = args[2].eval(&ns, false)?.unwrap_value();
            match (arr_val.as_ref(), idx0.as_ref(), idx1.as_ref()) {
                (XValue::Native(b), XValue::Int(idx0), XValue::Int(idx1)) => {
                    let arr = &b.as_ref()._as_any().downcast_ref::<XArray>().unwrap().value;
                    let mut idx0 = value_to_idx(arr, idx0)?;
                    let mut idx1 = value_to_idx(arr, idx1)?;
                    if idx0 == idx1 {
                        return Ok(arr_val.clone().into());
                    }
                    if idx0 > idx1 {
                        std::mem::swap(&mut idx0, &mut idx1);
                    }
                    let mut ret = vec![];
                    ret.extend(arr.iter().take(idx0).map(|x| x.clone()));
                    ret.push(arr[idx1].clone());
                    ret.extend(arr.iter().skip(idx0 + 1).take(idx1 - idx0 - 1).map(|x| x.clone()));
                    ret.push(arr[idx0].clone());
                    ret.extend(arr.iter().skip(idx1 + 1).map(|x| x.clone()));
                    Ok(XValue::Native(Box::new(XArray::new(ret))).into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}

pub fn add_array_to_stack(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));

    scope.add_func(
        "to_stack", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["T".to_string()]),
            params: vec![
                XFuncParamSpec {
                    type_: XArrayType::xtype(t.clone()),
                    required: true,
                },
            ],
            ret: XStackType::xtype(t.clone()),
        }, |args, ns, _tca| {
            let arr = args[0].eval(&ns, false)?.unwrap_value();
            match arr.as_ref() {
                XValue::Native(b) => {
                    let arr = &b.as_ref()._as_any().downcast_ref::<XArray>().unwrap().value;
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