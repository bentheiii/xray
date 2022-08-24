use crate::builtin::core::xcmp;
use crate::xtype::{XFuncParamSpec, XFuncSpec, X_BOOL, X_FLOAT, X_INT, X_STRING};
use crate::xvalue::{ManagedXValue, XValue};
use crate::{
    add_binop, add_ufunc, add_ufunc_ref, eval, to_primitive, CompilationError,
    RootCompilationScope, XStaticFunction,
};

use crate::util::lazy_bigint::LazyBigint;
use num_traits::{FromPrimitive, Zero};
use rc::Rc;

use std::io::Write;
use std::rc;

pub(crate) fn add_float_type<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    scope.add_native_type("float", X_FLOAT.clone())
}

macro_rules! add_float_binop {
    ($fn_name:ident, $name:ident, $func:expr) => {
        add_binop!($fn_name, $name, X_FLOAT, Float, X_FLOAT, $func);
    };
}

add_float_binop!(add_float_add, add, |a, b| Ok(XValue::Float(a + b)));
add_float_binop!(add_float_sub, sub, |a, b| Ok(XValue::Float(a - b)));
add_float_binop!(add_float_mul, mul, |a, b| Ok(XValue::Float(a * b)));
add_float_binop!(add_float_mod, mod, |a: &f64, b: &f64| {
    if b.is_zero() {
        Err("modulo by zero".to_string())
    } else {
        Ok(XValue::Float(a % b))
    }
});
add_float_binop!(add_float_div, div, |a: &f64, b: &f64| {
    if b.is_zero() {
        Err("division by zero".to_string())
    } else {
        Ok(XValue::Float(a / b))
    }
});
add_binop!(
    add_float_eq,
    eq,
    X_FLOAT,
    Float,
    X_BOOL,
    |a: &f64, b: &f64| { Ok(XValue::Bool(a == b)) }
);

add_ufunc!(add_float_floor, floor, X_FLOAT, Float, X_INT, |a: &f64| Ok(
    XValue::Int(LazyBigint::from_f64(a.floor()).unwrap())
));
add_ufunc!(add_float_ceil, ceil, X_FLOAT, Float, X_INT, |a: &f64| Ok(
    XValue::Int(LazyBigint::from_f64(a.ceil()).unwrap())
));
add_ufunc!(add_float_trunc, trunc, X_FLOAT, Float, X_INT, |a: &f64| Ok(
    XValue::Int(LazyBigint::from_f64(a.trunc()).unwrap())
));
add_ufunc!(add_float_neg, neg, X_FLOAT, Float, X_FLOAT, |a: &f64| Ok(
    XValue::Float(-a)
));

add_ufunc!(
    add_float_to_str,
    to_str,
    X_FLOAT,
    Float,
    X_STRING,
    |a: &f64| { Ok(XValue::String(a.to_string(),)) }
);

add_binop!(add_float_cmp, cmp, X_FLOAT, Float, X_INT, |a, b| Ok(xcmp(
    a, b
)));
