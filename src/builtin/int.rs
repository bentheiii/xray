use crate::builtin::core::{eval, ufunc_ref, xcmp};
use crate::builtin::sequence::{XSequence, XSequenceType};
use crate::xtype::{XFuncSpec, X_BOOL, X_FLOAT, X_INT, X_STRING};
use crate::xvalue::{ManagedXValue, XValue};
use crate::{add_binfunc, manage_native, to_primitive, xraise, xraise_opt, CompilationError, XStaticFunction, ufunc};

use num_traits::{Pow, Signed, ToPrimitive, Zero};

use rc::Rc;

use std::io::Write;
use std::ops::Neg;
use std::rc;
use std::convert::TryFrom;

use crate::root_compilation_scope::RootCompilationScope;
use crate::util::lazy_bigint::LazyBigint;

pub(crate) fn add_int_type<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    scope.add_native_type("int", X_INT.clone())
}

macro_rules! add_int_binop {
    ($fn_name:ident, $name:ident, $func:expr) => {
        add_binfunc!($fn_name, $name, X_INT, Int, X_INT, $func);
    };
}

add_int_binop!(add_int_add, add, |a: &LazyBigint, b: &LazyBigint| Ok(
    XValue::Int(a.clone() + b.clone())
));
add_int_binop!(add_int_sub, sub, |a: &LazyBigint, b: &LazyBigint| Ok(
    XValue::Int(a.clone() - b.clone())
));
add_int_binop!(add_int_mul, mul, |a: &LazyBigint, b: &LazyBigint| Ok(
    XValue::Int(a.clone() * b.clone())
));
add_int_binop!(add_int_mod, mod, |a: &LazyBigint, b: &LazyBigint| {
    if b.is_zero() {
        Err(String::from("Modulo by zero"))
    } else {
        Ok(XValue::Int(a.clone() % b.clone()))
    }
});
add_int_binop!(add_int_bit_or, bit_or, |a: &LazyBigint, b: &LazyBigint| Ok(
    XValue::Int(a.clone() | b.clone())
));
add_int_binop!(
    add_int_bit_and,
    bit_and,
    |a: &LazyBigint, b: &LazyBigint| Ok(XValue::Int(a.clone() & b.clone()))
);
add_binfunc!(
    add_int_div,
    div,
    X_INT,
    Int,
    X_FLOAT,
    |a: &LazyBigint, b: &LazyBigint| {
        if b.is_zero() {
            Err(String::from("Division by zero"))
        } else {
            Ok(XValue::Float(a.clone().true_div(b.clone())))
        }
    }
);
add_binfunc!(
    add_int_pow,
    pow,
    X_INT,
    Int,
    X_INT,
    |a: &LazyBigint, b: &LazyBigint| if b.is_negative() {
        Err(String::from("cannot raise integer to a negative power"))
    } else if b.is_zero() && a.is_zero() {
        Err(String::from("cannot raise zero to a zero power"))
    } else {
        Ok(XValue::Int(a.clone().pow(b.clone())))
    }
);
add_binfunc!(add_int_lt, lt, X_INT, Int, X_BOOL, |a, b| Ok(XValue::Bool(
    a < b
)));
add_binfunc!(add_int_gt, gt, X_INT, Int, X_BOOL, |a, b| Ok(XValue::Bool(
    a > b
)));
add_binfunc!(add_int_eq, eq, X_INT, Int, X_BOOL, |a, b| Ok(XValue::Bool(
    a == b
)));
add_binfunc!(add_int_ne, ne, X_INT, Int, X_BOOL, |a, b| Ok(XValue::Bool(
    a != b
)));
add_binfunc!(add_int_le, le, X_INT, Int, X_BOOL, |a, b| Ok(XValue::Bool(
    a <= b
)));
add_binfunc!(add_int_ge, ge, X_INT, Int, X_BOOL, |a, b| Ok(XValue::Bool(
    a >= b
)));

pub(crate) fn add_int_neg<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    scope.add_func(
        "neg",
        XFuncSpec::new(&[&X_INT], X_INT.clone()),
        ufunc!(Int, |a: &LazyBigint| Ok(XValue::Int(a.clone().neg()))),
    )
}

pub(crate) fn add_int_to_str<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    scope.add_func(
        "to_str",
        XFuncSpec::new(&[&X_INT], X_STRING.clone()),
        ufunc!(Int, |a: &LazyBigint| Ok(XValue::String(a.to_string()))),
    )
}

pub(crate) fn add_int_digits<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    scope.add_func(
        "digits",
        XFuncSpec::new_with_optional(&[&X_INT], &[&X_INT], XSequenceType::xtype(X_INT.clone())),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let n = to_primitive!(a0, Int);
            let a1 = xraise_opt!(args.get(1).map(|e| eval(e, ns, &rt)).transpose()?);
            let b = to_primitive!(a1, Int, LazyBigint::from(10));
            let mut digits = Vec::new();
            let mut n = n.clone();
            while !n.is_zero() {
                digits.push(n.clone() % b.clone().into_owned());
                n = n / b.clone().into_owned();
            }
            Ok(manage_native!(
                XSequence::array(
                    digits
                        .into_iter()
                        .map(|v| ManagedXValue::new(XValue::Int(v), rt.clone()))
                        .collect::<Result<Vec<_>, _>>()?
                        .into_iter()
                        .map(Ok)
                        .collect()
                ),
                rt
            ))
        }),
    )
}

pub(crate) fn add_int_hash<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    scope.add_func(
        "hash",
        XFuncSpec::new(&[&X_INT], X_INT.clone()),
        ufunc_ref(|a: Rc<ManagedXValue<W>>, rt| {
            let v = to_primitive!(a, Int);
            if v.to_u64().is_some() {
                // the number is already within the bounds
                return Ok(a.into());
            }
            Ok(ManagedXValue::new(XValue::Int(v.first_u64_digit()), rt)?.into())
        }),
    )
}

add_binfunc!(add_int_cmp, cmp, X_INT, Int, X_INT, |a, b| Ok(xcmp(a, b)));



pub(crate) fn add_int_chr<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    scope.add_func(
        "chr",
        XFuncSpec::new(&[&X_INT], X_STRING.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], &ns, &rt)?);
            let s = to_primitive!(a0, Int);
            let Some(ord) = s.to_u32() else {xraise!(Err("number too large"))};
            let Ok(chr) = char::try_from(ord) else {xraise!(Err("value is not a unicode char"))};
            Ok(ManagedXValue::new(XValue::String(chr.into()), rt)?.into())
        }),
    )
}