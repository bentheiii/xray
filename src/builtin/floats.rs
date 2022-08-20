use crate::builtin::core::xcmp;
use crate::xtype::{XFuncParamSpec, XFuncSpec, X_BOOL, X_FLOAT, X_INT, X_STRING, X_UNKNOWN};
use crate::xvalue::{ManagedXValue, XValue};
use crate::XType::Int;
use crate::{
    add_binop, add_ufunc, add_ufunc_ref, eval, to_primitive, Bind, CompilationError,
    XCompilationScope, XStaticFunction, XType,
};
use num::bigint::Sign;
use num::{BigInt, BigUint, Float, FromPrimitive, Signed, ToPrimitive, Zero};
use rc::Rc;
use std::rc;
use std::sync::Arc;
use string_interner::StringInterner;

pub fn add_float_type(
    scope: &mut XCompilationScope,
    interner: &mut StringInterner,
) -> Result<(), CompilationError> {
    scope.add_native_type(interner.get_or_intern_static("float"), X_FLOAT.clone())
}

macro_rules! add_float_binop {
    ($fn_name:ident, $name:ident, $func:expr) => {
        add_binop!($fn_name, $name, X_FLOAT, Float, X_FLOAT, $func);
    };
}

add_float_binop!(add_float_add, add, |a, b| Ok(XValue::Float(a + b).into()));
add_float_binop!(add_float_sub, sub, |a, b| Ok(XValue::Float(a - b).into()));
add_float_binop!(add_float_mul, mul, |a, b| Ok(XValue::Float(a * b).into()));
add_float_binop!(add_float_mod, mod, |a: &f64, b: &f64| {
    if b.is_zero() {
        Err("modulo by zero".to_string())
    } else {
        Ok(XValue::Float(a % b).into())
    }
});
add_float_binop!(add_float_div, div, |a: &f64, b: &f64| {
    if b.is_zero() {
        Err("division by zero".to_string())
    } else {
        Ok(XValue::Float(a / b).into())
    }
});
add_binop!(
    add_float_eq,
    eq,
    X_FLOAT,
    Float,
    X_BOOL,
    |a: &f64, b: &f64| { Ok(XValue::Bool(a == b).into()) }
);

fn whole_float_to_bigint(f: f64) -> BigInt {
    let (mantissa, exponent, sign) = f.integer_decode();
    let bigint_sign = if sign == 1 { Sign::Plus } else { Sign::Minus };

    let mut numer: BigUint = FromPrimitive::from_u64(mantissa).unwrap();
    if exponent < 0 {
        numer >>= (-exponent) as usize;
    } else {
        numer <<= exponent as usize;
    }
    BigInt::from_biguint(bigint_sign, numer)
}

add_ufunc!(add_float_floor, floor, X_FLOAT, Float, X_INT, |a: &f64| Ok(
    XValue::Int(whole_float_to_bigint(a.floor())).into()
));
add_ufunc!(add_float_ceil, ceil, X_FLOAT, Float, X_INT, |a: &f64| Ok(
    XValue::Int(whole_float_to_bigint(a.ceil())).into()
));
add_ufunc!(add_float_trunc, trunc, X_FLOAT, Float, X_INT, |a: &f64| Ok(
    XValue::Int(whole_float_to_bigint(a.trunc())).into()
));

add_ufunc!(
    add_float_to_str,
    to_str,
    X_FLOAT,
    Float,
    X_FLOAT,
    |a: &f64| {
        Ok(XValue::String(
            a.to_f64()
                .ok_or("rational cannot be converted to float")?
                .to_string(),
        )
        .into())
    }
);

add_binop!(add_float_cmp, cmp, X_FLOAT, Float, X_INT, |a, b| Ok(xcmp(
    a, b
)));
