use std::rc;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use crate::{add_binop, add_ufunc, add_ufunc_ref, Bind, XCompilationScope, XStaticFunction, XType};
use crate::xtype::{X_BOOL, X_INT, X_RATIONAL, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec};
use crate::xvalue::{XValue};
use rc::Rc;
use std::sync::Arc;

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