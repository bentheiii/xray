use crate::builtin::core::{eval, xcmp, xerr};
use crate::util::str_parts::StrParts;
use crate::util::xformatter::{group_string, XFormatting};
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

use crate::util::fenced_string::FencedString;
use statrs::function::erf::{erf, erfc};
use statrs::function::gamma::{gamma, ln_gamma};

use std::rc;

pub(crate) fn add_float_type<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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
pub(crate) fn add_float_mod<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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

pub(crate) fn add_float_div<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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

pub(crate) fn add_float_pow<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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
pub(crate) fn add_float_is_close<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "is_close",
        XFuncSpec::new_with_optional(&[&X_FLOAT, &X_FLOAT], &[&X_FLOAT, &X_FLOAT], X_BOOL.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a2 = xraise_opt!(args.get(2).map(|e| eval(e, ns, &rt)).transpose()?);
            let a3 = xraise_opt!(args.get(3).map(|e| eval(e, ns, &rt)).transpose()?);
            let rel_tol = a2.map_or(1e-6, |a| *to_primitive!(a, Float));
            let abs_tol = a3.map_or(1e-6, |a| *to_primitive!(a, Float));
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

pub(crate) fn add_float_floor<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "floor",
        XFuncSpec::new(&[&X_FLOAT], X_INT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(Ok(XValue::Int(
            LazyBigint::from_f64(a.floor()).unwrap()
        )))),
    )
}

pub(crate) fn add_float_ceil<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "ceil",
        XFuncSpec::new(&[&X_FLOAT], X_INT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(Ok(XValue::Int(
            LazyBigint::from_f64(a.ceil()).unwrap()
        )))),
    )
}

pub(crate) fn add_float_trunc<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "trunc",
        XFuncSpec::new(&[&X_FLOAT], X_INT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(Ok(XValue::Int(
            LazyBigint::from_f64(a.trunc()).unwrap()
        )))),
    )
}

pub(crate) fn add_float_neg<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "neg",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(Ok(XValue::Float(-a)))),
    )
}

pub(crate) fn add_float_sqrt<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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

pub(crate) fn add_float_cbrt<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "cbrt",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(Ok(XValue::Float(a.cbrt())))),
    )
}

pub(crate) fn add_float_acos<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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

pub(crate) fn add_float_acosh<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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

pub(crate) fn add_float_asin<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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

pub(crate) fn add_float_asinh<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "asinh",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(Ok(XValue::Float(a.asinh())))),
    )
}

pub(crate) fn add_float_atan<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "atan",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(Ok(XValue::Float(a.atan())))),
    )
}

pub(crate) fn add_float_atan2<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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

pub(crate) fn add_float_atanh<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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

pub(crate) fn add_float_cos<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "cos",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(Ok(XValue::Float(a.cos())))),
    )
}

pub(crate) fn add_float_cosh<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "cosh",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(Ok(XValue::Float(a.cosh())))),
    )
}

pub(crate) fn add_float_sin<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "sin",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(Ok(XValue::Float(a.sin())))),
    )
}

pub(crate) fn add_float_ln<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "ln",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(Ok(XValue::Float(a.ln())))),
    )
}

pub(crate) fn add_float_erf<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "erf",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(Ok(XValue::Float(erf(*a))))),
    )
}

pub(crate) fn add_float_erfc<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "erfc",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(Ok(XValue::Float(erfc(*a))))),
    )
}

pub(crate) fn add_float_gamma<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "gamma",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| {
            Ok(if a <= &0.0 && *a % 1.0 == 0.0 {
                Err("cannot get gamma of non-positive whole".to_string())
            } else {
                Ok(XValue::Float(gamma(*a)))
            })
        }),
    )
}

pub(crate) fn add_float_tan<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "tan",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(Ok(XValue::Float(a.tan())))),
    )
}

pub(crate) fn add_float_tanh<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "tanh",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| Ok(Ok(XValue::Float(a.tanh())))),
    )
}

pub(crate) fn add_float_gammaln<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "gammaln",
        XFuncSpec::new(&[&X_FLOAT], X_FLOAT.clone()),
        ufunc!(Float, |a: &f64, _rt| {
            Ok(if a <= &0.0 && *a % 2.0 >= 1.0 {
                Err("invalid value".to_string())
            } else {
                Ok(XValue::Float(ln_gamma(*a)))
            })
        }),
    )
}

pub(crate) fn add_float_to_str<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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

pub(crate) fn add_float_format<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    fn get_body<'a>(
        mag: f64,
        ty: Option<&'a str>,
        precision: usize,
        grouping: Option<&'a str>,
    ) -> Result<StrParts<'a>, String> {
        Ok(match ty {
            Some("e") => StrParts::from(format!("{mag:.precision$e}")),
            Some("E") => StrParts::from(format!("{mag:.precision$E}")),
            Some("%") => {
                let mut i = get_body(100.0 * mag, Some("f"), precision, grouping)?;
                i.extend(["%"]);
                i
            }
            None | Some("f") => {
                let b = format!("{mag:.precision$}");
                let (whole, part) = if let Some((w, p)) = b.split_once('.') {
                    (w.to_string(), Some(p.to_string()))
                } else {
                    (b, None)
                };
                let mut ret = if let Some(grouping) = grouping {
                    group_string(whole, grouping)
                } else {
                    StrParts::from(whole)
                };
                if let Some(part) = part {
                    ret.extend(["."]);
                    ret.extend([part])
                }
                ret
            }
            Some(other) => {
                return Err(format!("unrecognized float type: {other}"));
            }
        })
    }

    scope.add_func(
        "format",
        XFuncSpec::new(&[&X_FLOAT, &X_STRING], X_STRING.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let f0 = to_primitive!(a0, Float);
            let s1 = to_primitive!(a1, String);
            let Some(specs) = XFormatting::from_str(s1.as_str()) else {return xerr(ManagedXError::new("invalid format spec", rt)?);};

            rt.borrow().can_allocate_by(|| Some(specs.min_width()))?;

            let mag = f0.abs();
            if specs.ty.alternative{
                return xerr(ManagedXError::new("no alt type available for float formatting", rt)?);
            }
            let body = match get_body(
                mag,
                specs.ty.type_,
                specs.precision.unwrap_or(6),
                specs.grouping,
            ) {
                Ok(body)=>body,
                Err(err)=> {return xerr(ManagedXError::new(err, rt)?);}
            };
            let sign_parts = specs.sign(*f0 < 0.0);

            let (prefix, infix, postfix) = specs
                .fill_specs
                .map(|f| f.fillers(body.len() + sign_parts.len()))
                .unwrap_or_default();

            let ret = XValue::String(Box::new(FencedString::from_string(format!("{prefix}{sign_parts}{infix}{body}{postfix}"))));
            Ok(ManagedXValue::new(ret, rt)?.into())
        }),
    )
}
