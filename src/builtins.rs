use std::rc;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use crate::{Bind, XCompilationScope, XStaticFunction, XType};
use crate::xtype::{X_BOOL, X_INT, X_RATIONAL, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec};
use crate::xvalue::{XValue};
use rc::Rc;
use std::sync::Arc;
use crate::native_types::{XSequence, XSequenceType};

macro_rules! add_binop {
    ($fn_name:ident, $name:ident, $operand_type: ident, $operand_variant:path, $return_type:ident, $func:expr) => {
        pub fn $fn_name(scope: &mut XCompilationScope<'_>) -> Result<(), String> {
            scope.add_func(
                stringify!($name), XStaticFunction::Native(XFuncSpec {
                    generic_params: None,
                    params: vec![
                        XFuncParamSpec {
                            type_: $operand_type.clone(),
                            required: true,
                        },
                        XFuncParamSpec {
                            type_: $operand_type.clone(),
                            required: true,
                        },
                    ],
                    ret: $return_type.clone(),
                }, |args, ns, _tca| {
                    let a = args[0].eval(&ns,false)?.unwrap_value();
                    let b = args[1].eval(&ns,false)?.unwrap_value();
                    match (a.as_ref(), b.as_ref()) {
                        ($operand_variant(a), $operand_variant(b)) => $func(a,b),
                        (other_a, other_b) => {
                            println!("entered function with unexpected args {:?} and {:?}", other_a, other_b);
                            unreachable!()
                        },
                    }
                }))?;
            Ok(())
        }
    };
}

macro_rules! add_ufunc_ref {
    ($fn_name:ident, $name:ident, $operand_type: ident, $return_type:ident, $func:expr) => {
        pub fn $fn_name(scope: &mut XCompilationScope<'_>) -> Result<(), String> {
            scope.add_func(
                stringify!($name), XStaticFunction::Native(XFuncSpec {
                    generic_params: None,
                    params: vec![
                        XFuncParamSpec {
                            type_: $operand_type.clone(),
                            required: true,
                        },
                    ],
                    ret: $return_type.clone(),
                }, |args, ns, _tca| {
                    let a = args[0].eval(&ns,false)?.unwrap_value();
                    $func(a)
                }))?;
            Ok(())
        }
    };
}

macro_rules! add_ufunc {
    ($fn_name:ident, $name:ident, $operand_type: ident, $operand_variant:path, $return_type:ident, $func:expr) => {
        add_ufunc_ref!($fn_name, $name, $operand_type, $return_type, |a: Rc<XValue>| {
            if let $operand_variant(a) = a.as_ref() {
                $func(a)
            } else {
                unreachable!()
            }
        });
    };
}

// region ints
pub fn add_int_type(scope: &mut XCompilationScope) -> Result<(), String> {
    scope.add_native_type("int", X_INT.clone())
}

macro_rules! add_int_binop {
    ($fn_name:ident, $name:ident, $func:expr) => {add_binop!($fn_name, $name, X_INT, XValue::Int, X_INT, $func);};
}

add_int_binop!(add_int_add, add, |a,b| Ok(XValue::Int(a + b).into()));
add_int_binop!(add_int_sub, sub, |a,b| Ok(XValue::Int(a - b).into()));
add_int_binop!(add_int_mul, mul, |a,b| Ok(XValue::Int(a * b).into()));
add_int_binop!(add_int_mod, mod, |a: &BigInt, b : &BigInt| {
        if b.is_zero() {
            Err(String::from("Modulo by zero"))
        } else {
            Ok(XValue::Int(a % b).into())
        }
    });
add_int_binop!(add_int_bit_or, bit_or, |a,b| Ok(XValue::Int(a | b).into()));
add_int_binop!(add_int_bit_and, bit_and, |a,b| Ok(XValue::Int(a & b).into()));
add_int_binop!(add_int_bit_xor, bit_xor, |a,b| Ok(XValue::Int(a ^ b).into()));
add_binop!(add_int_div, div, X_INT, XValue::Int, X_RATIONAL, |a: &BigInt, b : &BigInt| {
        if b.is_zero() {
            Err(String::from("Division by zero"))
        } else {
            Ok(XValue::Rational(BigRational::new(a.clone(), b.clone())).into())
        }
    }
);
add_binop!(add_int_pow, pow, X_INT, XValue::Int, X_RATIONAL, |a: &BigInt,b: &BigInt|
    if !b.is_positive() && a.is_zero() {
        Err(String::from("cannot raise zero to a non-positive power"))
    } else {
        match b.to_i32() {
            Some(b) => Ok(XValue::Rational(BigRational::from(a.clone()).pow(b)).into()),
            None => Err(String::from("exponent too high"))
        }
    }
);
add_binop!(add_int_lt, lt, X_INT, XValue::Int, X_BOOL, |a,b|
    Ok(XValue::Bool(a < b).into())
);
add_binop!(add_int_gt, gt, X_INT, XValue::Int, X_BOOL, |a,b|
    Ok(XValue::Bool(a > b).into())
);
add_binop!(add_int_eq, eq, X_INT, XValue::Int, X_BOOL, |a,b|
    Ok(XValue::Bool(a == b).into())
);
add_binop!(add_int_ne, ne, X_INT, XValue::Int, X_BOOL, |a,b|
    Ok(XValue::Bool(a != b).into())
);
add_binop!(add_int_le, le, X_INT, XValue::Int, X_BOOL, |a,b|
    Ok(XValue::Bool(a <= b).into())
);
add_binop!(add_int_ge, ge, X_INT, XValue::Int, X_BOOL, |a,b|
    Ok(XValue::Bool(a >= b).into())
);

add_ufunc!(add_int_neg, neg, X_INT, XValue::Int, X_INT, |a:&BigInt| {
    Ok(XValue::Int(-a).into())
});

add_ufunc!(add_int_to_str, to_str, X_INT, XValue::Int, X_STRING, |a:&BigInt| Ok(XValue::String(a.to_string()).into()));

add_ufunc!(add_int_display, display, X_INT, XValue::Int, X_STRING, |a:&BigInt| {
    println!("{}", a);
    Ok(XValue::String(a.to_string()).into())
});
// endregion

// region strings
pub fn add_str_type(scope: &mut XCompilationScope) -> Result<(), String> {
    scope.add_native_type("str", X_STRING.clone())
}

add_binop!(add_str_eq, pow, X_STRING, XValue::String, X_BOOL, |a,b|
    Ok(XValue::Bool(a == b).into())
);

add_ufunc!(add_panic, panic, X_STRING, XValue::String, X_UNKNOWN, |a: &String| Err(a.clone()));
add_ufunc_ref!(add_str_display, display, X_STRING, X_STRING, |a: Rc<XValue>| {
    if let XValue::String(s) = a.as_ref(){
        println!("{}", s);
        Ok(a.into())
    }else{
        unreachable!();
    }
});
// endregion

// region bools
pub fn add_bool_type(scope: &mut XCompilationScope) -> Result<(), String> {
    scope.add_native_type("bool", X_BOOL.clone())
}

add_binop!(add_bool_eq, pow, X_BOOL, XValue::Bool, X_BOOL, |a,b|
    Ok(XValue::Bool(a == b).into())
);

add_ufunc!(add_bool_display, display, X_BOOL, XValue::Bool, X_STRING, |a:&bool| {
    println!("{}", a);
    Ok(XValue::String(a.to_string()).into())
});

add_ufunc_ref!(add_assert, display, X_BOOL, X_BOOL, |a:Rc<XValue>| {
    if let XValue::Bool(true) = a.as_ref(){
        Ok(a.into())
    }else{
        Err("assertion is untrue".to_string())
    }
});

add_ufunc!(add_bool_not, not, X_BOOL, XValue::Bool, X_BOOL, |a:&bool| {
    Ok(XValue::Bool(!a).into())
});

pub fn add_and(scope: &mut XCompilationScope) -> Result<(), String> {
    scope.add_func(
        "and", XStaticFunction::Native(XFuncSpec {
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
        }, |args, ns, tca| {
            let lhs = args[0].eval(&ns, false)?.unwrap_value();
            if let XValue::Bool(true) = lhs.as_ref() {
                return Ok(args[1].eval(&ns, tca)?);
            }
            Ok(lhs.into())
        }))?;
    Ok(())
}

pub fn add_or(scope: &mut XCompilationScope) -> Result<(), String> {
    scope.add_func(
        "or", XStaticFunction::Native(XFuncSpec {
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
        }, |args, ns, tca| {
            let lhs = args[0].eval(&ns, false)?.unwrap_value();
            if let XValue::Bool(false) = lhs.as_ref() {
                return Ok(args[1].eval(&ns, tca)?);
            }
            Ok(lhs.into())
        }))?;
    Ok(())
}
// endregion

// region generic
pub fn add_if(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    scope.add_func(
        "if", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["T".to_string()]),
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
// endregion

// region rational
pub fn add_rational_type(scope: &mut XCompilationScope) -> Result<(), String> {
    scope.add_native_type("rational", X_RATIONAL.clone())
}

macro_rules! add_rational_binop {
    ($fn_name:ident, $name:ident, $func:expr) => {add_binop!($fn_name, $name, X_RATIONAL, XValue::Rational, X_RATIONAL, $func);};
}

add_rational_binop!(add_rational_add, add, |a,b| Ok(XValue::Rational(a + b).into()));
add_rational_binop!(add_rational_sub, sub, |a,b| Ok(XValue::Rational(a - b).into()));
add_rational_binop!(add_rational_mul, mul, |a,b| Ok(XValue::Rational(a * b).into()));
add_rational_binop!(add_rational_mod, mod, |a: &BigRational,b: &BigRational| {
    if b.is_zero() {
        Err("modulo by zero".to_string())
    }
    else {
        Ok(XValue::Rational(a % b).into())
    }
});
add_rational_binop!(add_rational_div, div, |a: &BigRational,b: &BigRational| {
    if b.is_zero() {
        Err("division by zero".to_string())
    }
    else {
        Ok(XValue::Rational(a / b).into())
    }
});

add_ufunc!(add_rational_floor, floor, X_RATIONAL, XValue::Rational, X_INT, |a:&BigRational| Ok(XValue::Int(a.floor().numer().clone()).into()));
add_ufunc!(add_rational_ceil, ceil, X_RATIONAL, XValue::Rational, X_INT, |a:&BigRational| Ok(XValue::Int(a.ceil().numer().clone()).into()));
add_ufunc!(add_rational_trunc, trunc, X_RATIONAL, XValue::Rational, X_INT, |a:&BigRational| Ok(XValue::Int(a.trunc().numer().clone()).into()));

add_ufunc!(add_rational_to_str, to_str, X_RATIONAL, XValue::Rational, X_RATIONAL, |a:&BigRational| {
    Ok(XValue::String(a.to_f64().ok_or("rational cannot be converted to float")?.to_string()).into())
});
// endregion


//region arrays
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
    scope.add_native_type("Array", XSequenceType::xtype(XType::XGeneric("T".to_string()).into()))
}

pub fn add_array_get(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));

    scope.add_func(
        "get", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["T".to_string()]),
            params: vec![
                XFuncParamSpec {
                    type_: XSequenceType::xtype(t.clone()),
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
                    let arr = &b.as_ref()._as_any().downcast_ref::<XSequence>().unwrap().value;
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
                    type_: XSequenceType::xtype(t.clone()),
                    required: true,
                },
            ],
            ret: X_INT.clone(),
        }, |args, ns, _tca| {
            let arr = args[0].eval(&ns, false)?.unwrap_value();
            match arr.as_ref() {
                XValue::Native(b) => {
                    let arr = &b.as_ref()._as_any().downcast_ref::<XSequence>().unwrap().value;
                    Ok(XValue::Int(arr.len().into()).into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}

pub fn add_array_add(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let t_arr = XSequenceType::xtype(t.clone());

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
                    let arr0 = &b0.as_ref()._as_any().downcast_ref::<XSequence>().unwrap().value;
                    let arr1 = &b1.as_ref()._as_any().downcast_ref::<XSequence>().unwrap().value;
                    if arr0.is_empty() {
                        return Ok(v1.clone().into());
                    }
                    if arr1.is_empty() {
                        return Ok(v0.clone().into());
                    }
                    let mut arr = arr0.clone();
                    arr.append(&mut arr1.clone());
                    Ok(XValue::Native(Box::new(XSequence::new(arr))).into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}

pub fn add_array_push(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let t_arr = XSequenceType::xtype(t.clone());

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
                    let arr = &b0.as_ref()._as_any().downcast_ref::<XSequence>().unwrap().value;
                    let mut arr = arr.clone();
                    arr.push(el.clone());
                    Ok(XValue::Native(Box::new(XSequence::new(arr))).into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}

pub fn add_array_rpush(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let t_arr = XSequenceType::xtype(t.clone());

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
                    let arr_ = &b0.as_ref()._as_any().downcast_ref::<XSequence>().unwrap().value;
                    let mut arr = vec![el.clone()];
                    arr.extend(arr_.clone());
                    Ok(XValue::Native(Box::new(XSequence::new(arr))).into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}

pub fn add_array_insert(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let t_arr = XSequenceType::xtype(t.clone());

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
                    let arr = &b.as_ref()._as_any().downcast_ref::<XSequence>().unwrap().value;
                    let idx = value_to_idx(arr, idx)?;
                    let mut ret: Vec<Rc<_>> = vec![];
                    ret.extend(arr.iter().take(idx - 1).map(|x| x.clone()));
                    ret.push(el.clone());
                    ret.extend(arr.iter().skip(idx - 1).map(|x| x.clone()));
                    Ok(XValue::Native(Box::new(XSequence::new(ret))).into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}

pub fn add_array_pop(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let t_arr = XSequenceType::xtype(t.clone());

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
                    let arr = &b.as_ref()._as_any().downcast_ref::<XSequence>().unwrap().value;
                    let idx = value_to_idx(arr, idx)?;
                    let mut ret: Vec<Rc<_>> = vec![];
                    ret.extend(arr.iter().take(idx).map(|x| x.clone()));
                    ret.extend(arr.iter().skip(idx+1).map(|x| x.clone()));
                    Ok(XValue::Native(Box::new(XSequence::new(ret))).into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}

pub fn add_array_set(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let t_arr = XSequenceType::xtype(t.clone());

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
                    let arr = &b.as_ref()._as_any().downcast_ref::<XSequence>().unwrap().value;
                    let idx = value_to_idx(arr, &idx)?;
                    let mut ret = vec![];
                    ret.extend(arr.iter().take(idx - 1).map(|x| x.clone()));
                    ret.push(el.clone());
                    ret.extend(arr.iter().skip(idx).map(|x| x.clone()));
                    Ok(XValue::Native(Box::new(XSequence::new(ret))).into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}

pub fn add_array_swap(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let t_arr = XSequenceType::xtype(t.clone());

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
                    let arr = &b.as_ref()._as_any().downcast_ref::<XSequence>().unwrap().value;
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
                    ret.extend(arr.iter().skip(idx0+1).take(idx1-idx0-1).map(|x| x.clone()));
                    ret.push(arr[idx0].clone());
                    ret.extend(arr.iter().skip(idx1+1).map(|x| x.clone()));
                    Ok(XValue::Native(Box::new(XSequence::new(ret))).into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}
//endregion
// region general
// endregion