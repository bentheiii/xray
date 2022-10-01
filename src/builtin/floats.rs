use crate::builtin::core::{eval, eval_if_present, xcmp};
use crate::xtype::{XFuncSpec, X_BOOL, X_FLOAT, X_INT, X_STRING};
use crate::xvalue::{ManagedXValue, XValue};
use crate::{
    add_binop, add_ufunc, add_ufunc_ref, meval, to_primitive, CompilationError,
    RootCompilationScope, XStaticFunction,
};

use crate::util::lazy_bigint::LazyBigint;
use num_traits::{FromPrimitive, Zero};
use rc::Rc;
use std::cmp::max_by;

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
        Ok(XValue::Float(((a % b) + b) % b))
    }
});
add_float_binop!(add_float_div, div, |a: &f64, b: &f64| {
    if b.is_zero() {
        Err("division by zero".to_string())
    } else {
        Ok(XValue::Float(a / b))
    }
});
add_float_binop!(add_float_pow, pow, |a: &f64, b: &f64| {
    if (*a <= 0.0 && *b <= 0.0) || (*a < 0.0 && *b < 1.0) {
        Err("undefined exponential".to_string())
    } else {
        Ok(XValue::Float(a.powf(*b)))
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
pub(crate) fn add_float_is_close<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    scope.add_func(
        "is_close",
            XFuncSpec::new_with_optional(
                &[&X_FLOAT, &X_FLOAT],
                &[&X_FLOAT, &X_FLOAT],
                X_BOOL.clone(),
            ),
        XStaticFunction::from_native(
            |args, ns, _tca, rt| {
                let [a0, a1] = eval(args, ns, &rt,[0, 1])?;
                let [a2, a3] = eval_if_present(args, ns, &rt, [2, 3])?;
                let rel_tol = a2.map_or(1e-9, |a2| *to_primitive!(a2, Float));
                let abs_tol = a3.map_or(1e-9, |a2| *to_primitive!(a2, Float));
                let f0 = to_primitive!(a0, Float);
                let f1 = to_primitive!(a1, Float);
                let tol = max_by(
                    rel_tol * max_by(f0.abs(), f1.abs(), |a, b| a.partial_cmp(b).unwrap()),
                    abs_tol,
                    |a, b| a.partial_cmp(b).unwrap(),
                );
                let ret = (f1 - f0).abs() <= tol;
                Ok(ManagedXValue::new(XValue::Bool(ret), rt)?.into())
            },
        ),
    )
}

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
    add_float_sqrt,
    sqrt,
    X_FLOAT,
    Float,
    X_FLOAT,
    |a: &f64| if *a < 0.0 {
        Err("undefined exponential".to_string())
    } else {
        Ok(XValue::Float(a.sqrt()))
    }
);

add_ufunc!(
    add_float_to_str,
    to_str,
    X_FLOAT,
    Float,
    X_STRING,
    |a: &f64| {
        Ok(XValue::String(format!(
            "{:?}",
            if *a == -0.0 { 0.0 } else { *a }
        )))
    }
);

add_binop!(add_float_cmp, cmp, X_FLOAT, Float, X_INT, |a, b| Ok(xcmp(
    a, b
)));
