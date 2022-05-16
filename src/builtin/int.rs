use std::rc;
use num::{BigInt, BigRational, Integer, Signed, ToPrimitive, Zero};
use crate::{add_binop, add_ufunc, add_ufunc_ref, to_primitive, Bind, XArray, XArrayType, XCompilationScope, XStaticFunction, XType};
use crate::xtype::{X_BOOL, X_INT, X_RATIONAL, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec};
use crate::xvalue::{XValue};
use rc::Rc;
use std::sync::Arc;
use string_interner::StringInterner;

pub fn add_int_type(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    scope.add_native_type(interner.get_or_intern_static("int"), X_INT.clone())
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

add_ufunc!(add_int_neg, neg, X_INT, Int, X_INT, |a:&BigInt| {
    Ok(XValue::Int(-a).into())
});

add_ufunc!(add_int_to_str, to_str, X_INT, Int, X_STRING, |a:&BigInt| Ok(XValue::String(a.to_string()).into()));

add_ufunc!(add_int_display, display, X_INT, Int, X_STRING, |a:&BigInt| {
    println!("{}", a);
    Ok(XValue::String(a.to_string()).into())
});

pub fn add_int_digits(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {

    scope.add_func(
        interner.get_or_intern_static("digits"), XStaticFunction::Native(XFuncSpec {
            generic_params: None,
            params: vec![
                XFuncParamSpec {
                    type_: X_INT.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: X_INT.clone(),
                    required: false,
                },
            ],
            ret: XArrayType::xtype(X_INT.clone()),
        }, |args, ns, _tca| {
            let n = args[0].eval(&ns, false)?.unwrap_value();
            let b_evaled = args.get(1).map(|v|v.eval(&ns, false)).transpose()?.map(|v|v.unwrap_value().clone());
            let b;
            if b_evaled.is_some() {
                b = match b_evaled.unwrap().as_ref() {
                    XValue::Int(i) => i.clone(),
                    _ => unreachable!()
                };
            } else {
                b = BigInt::from(10 as i64);
            }
            match n.as_ref() {
                XValue::Int(n) => {
                    let mut digits = Vec::new();
                    let mut n = n.clone();
                    while !n.is_zero() {
                        digits.push(n.mod_floor(&b));
                        n = n.div_floor(&b);
                    }
                    Ok(XValue::Native(Box::new(XArray::new(digits.into_iter().map(|v| Rc::new(XValue::Int(v))).collect()))).into())
                },
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}