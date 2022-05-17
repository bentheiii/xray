use std::rc;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use crate::{add_binop, add_ufunc, add_ufunc_ref, to_primitive, eval, Bind, XCompilationScope, XStaticFunction, XType};
use crate::xtype::{X_BOOL, X_INT, X_RATIONAL, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec};
use crate::xvalue::{XValue, ManagedXValue};
use rc::Rc;
use std::sync::Arc;
use string_interner::StringInterner;
use crate::XType::Int;

pub fn add_rational_type(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    scope.add_native_type(interner.get_or_intern_static("rational"), X_RATIONAL.clone())
}

macro_rules! add_rational_binop {
    ($fn_name:ident, $name:ident, $func:expr) => {add_binop!($fn_name, $name, X_RATIONAL, Rational, X_RATIONAL, $func);};
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

add_ufunc!(add_rational_floor, floor, X_RATIONAL, Rational, X_INT, |a:&BigRational| Ok(XValue::Int(a.floor().numer().clone()).into()));
add_ufunc!(add_rational_ceil, ceil, X_RATIONAL, Rational, X_INT, |a:&BigRational| Ok(XValue::Int(a.ceil().numer().clone()).into()));
add_ufunc!(add_rational_trunc, trunc, X_RATIONAL, Rational, X_INT, |a:&BigRational| Ok(XValue::Int(a.trunc().numer().clone()).into()));

add_ufunc!(add_rational_to_str, to_str, X_RATIONAL, Rational, X_RATIONAL, |a:&BigRational| {
    Ok(XValue::String(a.to_f64().ok_or("rational cannot be converted to float")?.to_string()).into())
});