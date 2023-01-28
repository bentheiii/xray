use crate::builtin::core::{eval, xcmp, xerr};
use crate::xtype::{XFuncSpec, X_BOOL, X_FLOAT, X_INT, X_STRING};
use crate::xvalue::{ManagedXError, ManagedXValue, XValue};
use crate::{
    add_binfunc, to_primitive, ufunc, xraise, xraise_opt, CompilationError, RootCompilationScope,
    XStaticFunction,
};

use crate::util::lazy_bigint::LazyBigint;
use num_traits::{FromPrimitive, Zero};
use rc::Rc;
use std::cmp::max_by;
use std::f64::consts::PI;

use crate::util::fenced_string::FencedString;
use std::io::Write;
use std::rc;

pub(crate) fn add_float_type<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_native_type("float", X_FLOAT.clone())
}

macro_rules! add_float_binop {
    ($fn_name:ident, $name:ident, $func:expr) => {
        add_binfunc!($fn_name, $name, X_FLOAT, Float, X_FLOAT, $func);
    };
}

add_float_binop!(add_float_add, add, |a, b, _| Ok(Ok(XValue::Float(a + b))));
add_float_binop!(add_float_sub, sub, |a, b, _| Ok(Ok(XValue::Float(a - b))));
add_float_binop!(add_float_mul, mul, |a, b, _| Ok(Ok(XValue::Float(a * b))));
pub(crate) fn add_float_mod<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "mod",
        XFuncSpec::new(&[&X_FLOAT, &X_FLOAT], X_FLOAT.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a = to_primitive!(a0, Float);
            let b = to_primitive!(a1, Float);
            if b.is_zero() {
                xerr(ManagedXError::new("modulo by zero", rt)?)
            } else {
                Ok(ManagedXValue::new(XValue::Float(((a % b) + b) % b), rt)?.into())
            }
        }),
    )
}

pub(crate) fn add_float_div<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "div",
        XFuncSpec::new(&[&X_FLOAT, &X_FLOAT], X_FLOAT.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a = to_primitive!(a0, Float);
            let b = to_primitive!(a1, Float);
            if b.is_zero() {
                xerr(ManagedXError::new("division by zero", rt)?)
            } else {
                Ok(ManagedXValue::new(XValue::Float(a / b), rt)?.into())
            }
        }),
    )
}

pub(crate) fn add_float_pow<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "pow",
        XFuncSpec::new(&[&X_FLOAT, &X_FLOAT], X_FLOAT.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a = to_primitive!(a0, Float);
            let b = to_primitive!(a1, Float);
            if (*a <= 0.0 && *b <= 0.0) || (*a < 0.0 && *b < 1.0) {
                xerr(ManagedXError::new("undefined exponenitial", rt)?)
            } else {
                Ok(ManagedXValue::new(XValue::Float(a.powf(*b)), rt)?.into())
            }
        }),
    )
}
add_binfunc!(
    add_float_eq,
    eq,
    X_FLOAT,
    Float,
    X_BOOL,
    |a: &f64, b: &f64, _| { Ok(Ok(XValue::Bool(a == b))) }
);
pub(crate) fn add_float_is_close<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "is_close",
        XFuncSpec::new_with_optional(&[&X_FLOAT, &X_FLOAT], &[&X_FLOAT, &X_FLOAT], X_BOOL.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a2 = xraise_opt!(args.get(2).map(|e| eval(e, ns, &rt)).transpose()?);
            let a3 = xraise_opt!(args.get(3).map(|e| eval(e, ns, &rt)).transpose()?);
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
        }),
    )
}

pub(crate) fn add_float_floor<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "floor",
        XFuncSpec::new(&[&X_FLOAT], X_INT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(Ok(XValue::Int(
            LazyBigint::from_f64(a.floor()).unwrap()
        )))),
    )
}

pub(crate) fn add_float_ceil<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "ceil",
        XFuncSpec::new(&[&X_FLOAT], X_INT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(Ok(XValue::Int(
            LazyBigint::from_f64(a.ceil()).unwrap()
        )))),
    )
}

pub(crate) fn add_float_trunc<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "trunc",
        XFuncSpec::new(&[&X_FLOAT], X_INT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(Ok(XValue::Int(
            LazyBigint::from_f64(a.trunc()).unwrap()
        )))),
    )
}

pub(crate) fn add_float_neg<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "neg",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(Ok(XValue::Float(-a)))),
    )
}

pub(crate) fn add_float_sqrt<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "sqrt",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(if *a < 0.0 {
            Err("cannot find square root of negative number".to_string())
        } else {
            Ok(XValue::Float(a.sqrt()))
        })),
    )
}

pub(crate) fn add_float_cbrt<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "cbrt",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(Ok(XValue::Float(a.cbrt())))),
    )
}

pub(crate) fn add_float_acos<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "acos",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(if *a > 1.0 || *a < -1.0 {
            Err("acos argument must be within -1 and 1".to_string())
        } else {
            Ok(XValue::Float(a.acos()))
        })),
    )
}

pub(crate) fn add_float_acosh<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "acosh",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(if *a < 1.0 {
            Err("acosh argument must be above 1".to_string())
        } else {
            Ok(XValue::Float(a.acosh()))
        })),
    )
}

pub(crate) fn add_float_acot<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "acot",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(Ok(XValue::Float(
            PI / 2.0 - a.atan()
        )))),
    )
}

pub(crate) fn add_float_acoth<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "acoth",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(if *a < 1.0 && *a > -1.0 {
            Err("acoth argument must be above 1 or lower than -1".to_string())
        } else {
            Ok(XValue::Float(0.5 * ((*a + 1.0) / (*a - 1.0)).ln()))
        })),
    )
}

pub(crate) fn add_float_asin<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "asin",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(if *a > 1.0 || *a < -1.0 {
            Err("asin argument must be within -1 and 1".to_string())
        } else {
            Ok(XValue::Float(a.asin()))
        })),
    )
}

pub(crate) fn add_float_asinh<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "asinh",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(Ok(XValue::Float(a.asinh())))),
    )
}

pub(crate) fn add_float_atan<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "atan",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(Ok(XValue::Float(a.atan())))),
    )
}

pub(crate) fn add_float_atan2<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "atan",
        XFuncSpec::new(&[&X_FLOAT, &X_FLOAT], X_FLOAT.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a = to_primitive!(a0, Float);
            let b = to_primitive!(a1, Float);
            Ok(ManagedXValue::new(XValue::Float(a.atan2(*b)), rt)?.into())
        }),
    )
}

pub(crate) fn add_float_atanh<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "atanh",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(if *a >= 1.0 || *a <= -1.0 {
            Err("atanh argument must be within -1 and 1".to_string())
        } else {
            Ok(XValue::Float(a.atanh()))
        })),
    )
}

pub(crate) fn add_float_to_str<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "to_str",
        XFuncSpec::new(&[&X_FLOAT], X_STRING.clone()),
        ufunc!(Float, |a: &f64, _rt| {
            Ok(Ok(XValue::String(Box::new(FencedString::from_string(
                format!("{:?}", if *a == -0.0 { 0.0 } else { *a }),
            )))))
        }),
    )
}

add_binfunc!(add_float_cmp, cmp, X_FLOAT, Float, X_INT, |a, b, _| Ok(Ok(
    xcmp(a, b)
)));
