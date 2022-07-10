use std::rc;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use crate::{add_binop, add_ufunc, add_ufunc_ref, Bind, CompilationError, eval, Identifier, intern, manage_native, to_native, to_primitive, XCallableSpec, XCompilationScope, XStaticFunction, XType};
use crate::xtype::{X_BOOL, X_INT, X_RATIONAL, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec};
use crate::xvalue::{ManagedXValue, XValue};
use rc::Rc;
use std::any::{Any, TypeId};
use std::borrow::Cow;
use std::collections::HashMap;
use std::mem::size_of;
use std::ops::Deref;
use std::sync::Arc;
use string_interner::StringInterner;
use crate::builtin::stack::{XStack, XStackType};
use crate::native_types::{NativeType, RuntimeEquatable, XNativeValue};
use crate::XType::XCallable;

use crate::xexpr::XExpr;


#[derive(Debug, Clone)]
pub struct XArrayType;

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
    pub value: Vec<Rc<ManagedXValue>>,
}

impl XArray {
    pub fn new(value: Vec<Rc<ManagedXValue>>) -> Self {
        Self { value }
    }
}

impl XNativeValue for XArray {
    fn size(&self) -> usize {
        self.value.len() * size_of::<usize>()
    }
}

fn value_to_idx(arr: &Vec<Rc<ManagedXValue>>, i: &BigInt) -> Result<usize, String> {
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

pub fn add_array_type(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    scope.add_native_type_intern("Array", XArrayType::xtype(XType::generic_from_name("T", interner)), interner)
}

pub fn add_array_get(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let t = XType::generic_from_name("T", interner);

    scope.add_func_intern(
        "get", XStaticFunction::from_native(XFuncSpec {
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
        }, |args, ns, _tca, rt| {
            let (a0, a1) = eval!(args, ns, rt, 0,1);
            let arr = &to_native!(a0, XArray).value;
            let idx = to_primitive!(a1, Int);
            let idx = value_to_idx(&arr, idx)?;
            Ok(arr[idx].clone().into())
        }), interner)?;
    Ok(())
}

pub fn add_array_len(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let t = XType::generic_from_name("T", interner);

    scope.add_func_intern(
        "len", XStaticFunction::from_native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: XArrayType::xtype(t.clone()),
                    required: true,
                },
            ],
            ret: X_INT.clone(),
        }, |args, ns, _tca, rt| {
            let (a0, ) = eval!(args, ns, rt, 0);
            let arr = &to_native!(a0, XArray).value;
            Ok(ManagedXValue::new(XValue::Int(arr.len().into()), rt)?.into())
        }), interner)?;
    Ok(())
}

pub fn add_array_add(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let t = XType::generic_from_name("T", interner);
    let t_arr = XArrayType::xtype(t.clone());

    scope.add_func_intern(
        "add", XStaticFunction::from_native(XFuncSpec {
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
        }, |args, ns, tca, rt| {
            let (a0, ) = eval!(args, ns, rt, 0);
            let vec0 = &to_native!(a0, XArray).value;
            if vec0.is_empty() {
                return Ok(args[1].eval(&ns, tca, rt)?);
            }
            let (a1, ) = eval!(args, ns, rt, 1);
            let vec1 = &to_native!(a1, XArray).value;
            if vec1.is_empty() {
                return Ok(a0.clone().into());
            }
            let mut arr = vec0.clone();
            arr.extend(vec1.iter().cloned());
            Ok(manage_native!(XArray::new(arr), rt))
        }), interner)?;
    Ok(())
}

pub fn add_array_push(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let t = XType::generic_from_name("T", interner);
    let t_arr = XArrayType::xtype(t.clone());

    scope.add_func_intern(
        "push", XStaticFunction::from_native(XFuncSpec {
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
        }, |args, ns, _tca, rt| {
            let (a0, a1) = eval!(args, ns, rt, 0, 1);
            let vec0 = &to_native!(a0, XArray).value;
            let mut arr = vec0.clone();
            arr.push(a1.clone());
            Ok(manage_native!(XArray::new(arr), rt))
        }), interner)?;
    Ok(())
}

pub fn add_array_rpush(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let t = XType::generic_from_name("T", interner);
    let t_arr = XArrayType::xtype(t.clone());

    scope.add_func_intern(
        "rpush", XStaticFunction::from_native(XFuncSpec {
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
        }, |args, ns, _tca, rt| {
            let (a0, a1) = eval!(args, ns, rt, 0, 1);
            let vec0 = &to_native!(a0, XArray).value;
            let mut arr = vec![a1.clone()];
            arr.extend(vec0.iter().cloned());
            Ok(manage_native!(XArray::new(arr), rt))
        }), interner)?;
    Ok(())
}

pub fn add_array_insert(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let t = XType::generic_from_name("T", interner);
    let t_arr = XArrayType::xtype(t.clone());

    scope.add_func_intern(
        "insert", XStaticFunction::from_native(XFuncSpec {
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
        }, |args, ns, _tca, rt| {
            let (a0, a1, a2) = eval!(args, ns, rt, 0,1,2);
            let arr = &to_native!(a0, XArray).value;
            let idx = to_primitive!(a1, Int);
            let idx = value_to_idx(arr, idx)?;
            let mut ret: Vec<Rc<_>> = vec![];
            ret.extend(arr.iter().take(idx - 1).map(|x| x.clone()));
            ret.push(a2.clone());
            ret.extend(arr.iter().skip(idx - 1).map(|x| x.clone()));
            Ok(manage_native!(XArray::new(ret), rt))
        }), interner)?;
    Ok(())
}

pub fn add_array_pop(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let t = XType::generic_from_name("T", interner);
    let t_arr = XArrayType::xtype(t.clone());

    scope.add_func_intern(
        "pop", XStaticFunction::from_native(XFuncSpec {
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
        }, |args, ns, _tca, rt| {
            let (a0, a1) = eval!(args, ns, rt, 0,1);
            let arr = &to_native!(a0, XArray).value;
            let idx = to_primitive!(a1, Int);
            let idx = value_to_idx(&arr, idx)?;
            let mut ret: Vec<Rc<_>> = vec![];
            ret.extend(arr.iter().take(idx).cloned());
            ret.extend(arr.iter().skip(idx + 1).cloned());
            Ok(manage_native!(XArray::new(ret), rt))
        }), interner)?;
    Ok(())
}

pub fn add_array_set(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let t = XType::generic_from_name("T", interner);
    let t_arr = XArrayType::xtype(t.clone());

    scope.add_func_intern(
        "set", XStaticFunction::from_native(XFuncSpec {
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
        }, |args, ns, _tca, rt| {
            let (a0, a1, a2) = eval!(args, ns, rt, 0,1,2);
            let arr = &to_native!(a0, XArray).value;
            let idx = to_primitive!(a1, Int);
            let idx = value_to_idx(arr, idx)?;
            let mut ret: Vec<Rc<_>> = vec![];
            ret.extend(arr.iter().take(idx - 1).map(|x| x.clone()));
            ret.push(a2.clone());
            ret.extend(arr.iter().skip(idx).map(|x| x.clone()));
            Ok(manage_native!(XArray::new(ret), rt))
        }), interner)?;
    Ok(())
}

pub fn add_array_swap(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let t = XType::generic_from_name("T", interner);
    let t_arr = XArrayType::xtype(t.clone());

    scope.add_func_intern(
        "swap", XStaticFunction::from_native(XFuncSpec {
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
        }, |args, ns, _tca, rt| {
            let (a0, a1, a2) = eval!(args, ns, rt, 0,1,2);
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
            Ok(manage_native!(XArray::new(ret), rt))
        }), interner)?;
    Ok(())
}

pub fn add_array_to_stack(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let t = XType::generic_from_name("T", interner);

    scope.add_func_intern(
        "to_stack", XStaticFunction::from_native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: XArrayType::xtype(t.clone()),
                    required: true,
                },
            ],
            ret: XStackType::xtype(t.clone()),
        }, |args, ns, _tca, rt| {
            let (a0, ) = eval!(args, ns, rt, 0);
            let arr = &to_native!(a0, XArray).value;
            let mut ret = XStack::new();
            for x in arr {
                ret = ret.push(x.clone());
            }
            Ok(manage_native!(ret, rt))
        }), interner)?;
    Ok(())
}

pub fn add_array_map(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let input_t = XType::generic_from_name("T_IN", interner);
    let output_t = XType::generic_from_name("T_OUT", interner);

    scope.add_func_intern(
        "map", XStaticFunction::from_native(XFuncSpec {
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
        }, |args, ns, _tca, rt| {
            let (a0, a1) = eval!(args, ns, rt, 0,1);
            let arr = &to_native!(a0, XArray).value;
            let f = to_primitive!(a1, Function);
            let ret = arr.iter().map(|x| {
                f.eval_values(vec![x.clone()], &ns, rt.clone())
            }).collect::<Result<_, _>>()?;
            Ok(manage_native!(XArray::new(ret), rt))
        }), interner)?;
    Ok(())
}

pub fn add_array_eq(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let eq_symbol = interner.get_or_intern_static("eq");

    fn static_from_eq(t0: Arc<XType>, t1: Arc<XType>, eq_expr: XExpr)->Rc<XStaticFunction>{
        Rc::new(XStaticFunction::from_native(XFuncSpec {
            generic_params: None,
            params: vec![
                XFuncParamSpec {
                    type_: XArrayType::xtype(t0.clone()),
                    required: true,
                },
                XFuncParamSpec {
                    type_: XArrayType::xtype(t1.clone()),
                    required: true,
                },
            ],
            ret: X_BOOL.clone(),
        }, move |args, ns, _tca, rt| {
            let (a0, a1) = eval!(args, ns, rt, 0,1);
            let arr0 = &to_native!(a0, XArray).value;
            let arr1 = &to_native!(a1, XArray).value;
            if arr0.len() != arr1.len() {
                return Ok(ManagedXValue::new(XValue::Bool(false), rt)?.into());
            }
            let mut ret = true;
            for (x, y) in arr0.iter().zip(arr1.iter()) {
                let inner_equal_value = eq_expr.eval(ns, false, rt.clone())?.unwrap_value();
                let inner_eq_func = to_primitive!(inner_equal_value, Function);
                let eq = inner_eq_func.eval_values(vec![x.clone(), y.clone()], &ns, rt.clone())?;
                let is_eq = to_primitive!(eq, Bool);
                if !*is_eq {
                    ret = false;
                    break;
                }
            }
            return Ok(ManagedXValue::new(XValue::Bool(ret), rt)?.into());
        }))
    }

    fn from_types(types: &Vec<Arc<XType>>, scope: &XCompilationScope, eq_symbol: Identifier) -> Result<Rc<XStaticFunction>, String> {
        if types.len() != 2 {
            return Err(format!("Expected 2 types, got {}", types.len()));
        }
        let a0 = types[0].clone();
        let t0 = match &a0.as_ref() {
            XType::XNative(nt0, bind) if nt0.name() == "Array" => bind[0].clone(),
            _ => return Err(format!("Expected array type, got {:?}", a0)),  // todo improve
        };
        let a1 = types[1].clone();
        let t1 = match &a1.as_ref() {
            XType::XNative(nt1, bind) if nt1.name() == "Array" => bind[0].clone(),
            _ => return Err(format!("Expected array type, got {:?}", a1)),  // todo improve
        };

        let inner_eq = scope.resolve_overload(eq_symbol, vec![t0.clone(), t1.clone()])?;  // todo ensure that the function returns a bool

        Ok(static_from_eq(t0, t1, inner_eq))
    }

    scope.add_dyn_func(eq_symbol, move |_params, types, ns| {
        from_types(types, ns, eq_symbol)
    })
}