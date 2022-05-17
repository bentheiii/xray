use std::rc;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use crate::{add_binop, add_ufunc, add_ufunc_ref, Bind, eval, intern, to_native, to_primitive, XCallableSpec, XCompilationScope, XStaticFunction, XType};
use crate::xtype::{X_BOOL, X_INT, X_RATIONAL, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec};
use crate::xvalue::{XValue};
use rc::Rc;
use std::borrow::Cow;
use std::collections::HashMap;
use std::sync::Arc;
use string_interner::StringInterner;
use crate::builtin::stack::{XStack, XStackType};
use crate::native_types::{NativeType, XNativeValue};
use crate::XType::XCallable;

#[derive(Debug, Clone)]
pub struct XArrayType {}

impl XArrayType {
    pub fn xtype(t: Arc<XType>) -> Arc<XType> {
        Arc::new(XType::XNative(Box::new(Self {}), vec![t.clone()]))
    }
}

impl NativeType for XArrayType {
    fn generic_names(&self) -> Vec<String> {
        vec!["T".to_string()]
    }
    fn name(&self) -> &str { "Array" }
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
    let mut i = Cow::Borrowed(i);
    if i.is_negative() {
        i = Cow::Owned(i.as_ref() + arr.len());
        if i.is_negative() {
            return Err("index too low".to_string());
        }
    };
    let idx = i.to_usize().ok_or("index too large")?;
    if idx >= arr.len() {
        return Err("index out of bounds".to_string());
    }
    Ok(idx)
}

pub fn add_array_type(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    scope.add_native_type_intern("Array", XArrayType::xtype(XType::generic_from_name("T", interner)), interner)
}

pub fn add_array_get(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);

    scope.add_func_intern(
        "get", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
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
            let (a0, a1) = eval!(args, ns, 0,1);
            let arr = &to_native!(a0, XArray).value;
            let idx = to_primitive!(a1, Int);
            let idx = value_to_idx(&arr, idx)?;
            Ok(arr[idx].clone().into())
        }), interner)?;
    Ok(())
}

pub fn add_array_len(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);

    scope.add_func_intern(
        "len", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: XArrayType::xtype(t.clone()),
                    required: true,
                },
            ],
            ret: X_INT.clone(),
        }, |args, ns, _tca| {
            let (a0,) = eval!(args, ns, 0);
            let arr = &to_native!(a0, XArray).value;
            Ok(XValue::Int(arr.len().into()).into())
        }), interner)?;
    Ok(())
}

pub fn add_array_add(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    let t_arr = XArrayType::xtype(t.clone());

    scope.add_func_intern(
        "add", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
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
        }, |args, ns, tca| {
            let (a0,) = eval!(args, ns, 0);
            let vec0 = &to_native!(a0, XArray).value;
            if vec0.is_empty() {
                return Ok(args[1].eval(&ns, tca)?);
            }
            let (a1,) = eval!(args, ns, 1);
            let vec1 = &to_native!(a1, XArray).value;
            if vec1.is_empty() {
                return Ok(a0.clone().into());
            }
            let mut arr = vec0.clone();
            arr.extend(vec1.iter().cloned());
            Ok(XValue::Native(Box::new(XArray::new(arr))).into())
        }), interner)?;
    Ok(())
}

pub fn add_array_push(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    let t_arr = XArrayType::xtype(t.clone());

    scope.add_func_intern(
        "push", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
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
            let (a0, a1) = eval!(args, ns, 0, 1);
            let vec0 = &to_native!(a0, XArray).value;
            let mut arr = vec0.clone();
            arr.push(a1.clone());
            Ok(XValue::Native(Box::new(XArray::new(arr))).into())
        }), interner)?;
    Ok(())
}

pub fn add_array_rpush(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    let t_arr = XArrayType::xtype(t.clone());

    scope.add_func_intern(
        "rpush", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
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
            let (a0, a1) = eval!(args, ns, 0, 1);
            let vec0 = &to_native!(a0, XArray).value;
            let mut arr = vec![a1.clone()];
            arr.extend(vec0.iter().cloned());
            Ok(XValue::Native(Box::new(XArray::new(arr))).into())
        }), interner)?;
    Ok(())
}

pub fn add_array_insert(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    let t_arr = XArrayType::xtype(t.clone());

    scope.add_func_intern(
        "insert", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
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
            let (a0, a1, a2) = eval!(args, ns, 0,1,2);
            let arr = &to_native!(a0, XArray).value;
            let idx = to_primitive!(a1, Int);
            let idx = value_to_idx(arr, idx)?;
            let mut ret: Vec<Rc<_>> = vec![];
            ret.extend(arr.iter().take(idx - 1).map(|x| x.clone()));
            ret.push(a2.clone());
            ret.extend(arr.iter().skip(idx - 1).map(|x| x.clone()));
            Ok(XValue::Native(Box::new(XArray::new(ret))).into())
        }), interner)?;
    Ok(())
}

pub fn add_array_pop(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    let t_arr = XArrayType::xtype(t.clone());

    scope.add_func_intern(
        "pop", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
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
            let (a0, a1) = eval!(args, ns, 0,1);
            let arr = &to_native!(a0, XArray).value;
            let idx = to_primitive!(a1, Int);
            let idx = value_to_idx(&arr, idx)?;
            let mut ret: Vec<Rc<_>> = vec![];
            ret.extend(arr.iter().take(idx).cloned());
            ret.extend(arr.iter().skip(idx + 1).cloned());
            Ok(XValue::Native(Box::new(XArray::new(ret))).into())
        }), interner)?;
    Ok(())
}

pub fn add_array_set(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    let t_arr = XArrayType::xtype(t.clone());

    scope.add_func_intern(
        "set", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
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
            let (a0, a1, a2) = eval!(args, ns, 0,1,2);
            let arr = &to_native!(a0, XArray).value;
            let idx = to_primitive!(a1, Int);
            let idx = value_to_idx(arr, idx)?;
            let mut ret: Vec<Rc<_>> = vec![];
            ret.extend(arr.iter().take(idx - 1).map(|x| x.clone()));
            ret.push(a2.clone());
            ret.extend(arr.iter().skip(idx).map(|x| x.clone()));
            Ok(XValue::Native(Box::new(XArray::new(ret))).into())
        }), interner)?;
    Ok(())
}

pub fn add_array_swap(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    let t_arr = XArrayType::xtype(t.clone());

    scope.add_func_intern(
        "swap", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
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
            let (a0, a1, a2) = eval!(args, ns, 0,1,2);
            let arr = &to_native!(a0, XArray).value;
            let idx1 = to_primitive!(a1, Int);
            let idx2 = to_primitive!(a2, Int);
            let mut idx1 = value_to_idx(arr, idx1)?;
            let mut idx2 = value_to_idx(arr, idx2)?;
            if idx1 == idx2 {
                return Ok(a0.clone().into());
            }
            if idx1 > idx2 {
                (idx1, idx2) = (idx2, idx1);
            }
            let mut ret = vec![];
            ret.extend(arr.iter().take(idx1).cloned());
            ret.push(arr[idx2].clone());
            ret.extend(arr.iter().skip(idx1 + 1).take(idx1 - idx2 - 1).cloned());
            ret.push(arr[idx1].clone());
            ret.extend(arr.iter().skip(idx2 + 1).cloned());
            Ok(XValue::Native(Box::new(XArray::new(ret))).into())
        }), interner)?;
    Ok(())
}

pub fn add_array_to_stack(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);

    scope.add_func_intern(
        "to_stack", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: XArrayType::xtype(t.clone()),
                    required: true,
                },
            ],
            ret: XStackType::xtype(t.clone()),
        }, |args, ns, _tca| {
            let (a0,) = eval!(args, ns, 0);
            let arr = &to_native!(a0, XArray).value;
            let mut ret = XStack::new();
            for x in arr {
                ret = ret.push(x.clone());
            }
            Ok(XValue::Native(Box::new(ret)).into())
        }), interner)?;
    Ok(())
}

pub fn add_array_map(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let input_t = XType::generic_from_name("T_IN", interner);
    let output_t = XType::generic_from_name("T_OUT", interner);

    scope.add_func_intern(
        "map", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T_IN", "T_OUT")),
            params: vec![
                XFuncParamSpec {
                    type_: XArrayType::xtype(input_t.clone()),
                    required: true,
                },
                XFuncParamSpec {
                    type_: Arc::new(XCallable(XCallableSpec {
                        param_types: vec![input_t.clone()],
                        return_type: output_t.clone(),
                    })),
                    required: true,
                },
            ],
            ret: XArrayType::xtype(output_t.clone()),
        }, |args, ns, _tca| {
            let (a0, a1) = eval!(args, ns, 0,1);
            let arr = &to_native!(a0, XArray).value;
            let f = to_primitive!(a1, Function);
            let ret = arr.iter().map(|x| {
                f.eval_values(vec![x.clone()], &ns)
            }).collect::<Result<_, _>>()?;
            Ok(XValue::Native(Box::new(XArray::new(ret))).into())
        }), interner)?;
    Ok(())
}