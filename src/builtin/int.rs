use crate::builtin::core::xcmp;
use crate::builtin::sequence::{XSequence, XSequenceType};
use crate::xtype::{XFuncParamSpec, XFuncSpec, X_BOOL, X_FLOAT, X_INT, X_STRING};
use crate::xvalue::{ManagedXValue, XValue};
use crate::{
    add_binop, add_ufunc, add_ufunc_ref, eval, manage_native, meval, to_primitive,
    CompilationError, XStaticFunction,
};

use num_traits::{Inv, Pow, Signed, ToPrimitive, Zero};

use rc::Rc;
use std::ops::Neg;
use std::rc;

use crate::compilation_scope::RootCompilationScope;
use crate::util::lazy_bigint::LazyBigint;

pub(crate) fn add_int_type(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    scope.add_native_type("int", X_INT.clone())
}

macro_rules! add_int_binop {
    ($fn_name:ident, $name:ident, $func:expr) => {
        add_binop!($fn_name, $name, X_INT, Int, X_INT, $func);
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
add_int_binop!(
    add_int_bit_xor,
    bit_xor,
    |a: &LazyBigint, b: &LazyBigint| Ok(XValue::Int(a.clone() ^ b.clone()))
);
add_binop!(
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
add_binop!(
    add_int_pow,
    pow,
    X_INT,
    Int,
    X_FLOAT,
    |a: &LazyBigint, b: &LazyBigint| if !b.is_positive() && a.is_zero() {
        Err(String::from("cannot raise zero to a non-positive power"))
    } else if b.is_negative() {
        Ok(XValue::Float(a.clone().pow(b.clone().neg()).inv()))
    } else {
        a.clone().pow(b.clone()).to_f64().map_or_else(
            || Err("number is too large".to_string()),
            |f| Ok(XValue::Float(f)),
        )
    }
);
add_binop!(add_int_lt, lt, X_INT, Int, X_BOOL, |a, b| Ok(XValue::Bool(
    a < b
)));
add_binop!(add_int_gt, gt, X_INT, Int, X_BOOL, |a, b| Ok(XValue::Bool(
    a > b
)));
add_binop!(add_int_eq, eq, X_INT, Int, X_BOOL, |a, b| Ok(XValue::Bool(
    a == b
)));
add_binop!(add_int_ne, ne, X_INT, Int, X_BOOL, |a, b| Ok(XValue::Bool(
    a != b
)));
add_binop!(add_int_le, le, X_INT, Int, X_BOOL, |a, b| Ok(XValue::Bool(
    a <= b
)));
add_binop!(add_int_ge, ge, X_INT, Int, X_BOOL, |a, b| Ok(XValue::Bool(
    a >= b
)));

add_ufunc!(add_int_neg, neg, X_INT, Int, X_INT, |a: &LazyBigint| {
    Ok(XValue::Int(a.clone().neg()))
});

add_ufunc!(
    add_int_to_str,
    to_str,
    X_INT,
    Int,
    X_STRING,
    |a: &LazyBigint| Ok(XValue::String(a.to_string()))
);

pub(crate) fn add_int_digits(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    scope.add_func(
        "digits",
        XStaticFunction::from_native(
            XFuncSpec {
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
                ret: XSequenceType::xtype(X_INT.clone()),
            },
            |args, ns, _tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                let n = to_primitive!(a0, Int);
                let (a1,) = meval!(args, ns, rt, 1);
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
                            .collect::<Result<_, _>>()?
                    ),
                    rt
                ))
            },
        ),
    )
}

pub(crate) fn add_int_hash(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    scope.add_func(
        "hash",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: None,
                params: vec![XFuncParamSpec {
                    type_: X_INT.clone(),
                    required: true,
                }],
                ret: X_INT.clone(),
            },
            |args, ns, _tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                let v0 = to_primitive!(a0, Int);
                if v0.to_u64().is_some() {
                    // the number is already within the bounds
                    return Ok(a0.into());
                }
                Ok(ManagedXValue::new(XValue::Int(v0.first_u64_digit()), rt)?.into())
            },
        ),
    )
}

add_binop!(add_int_cmp, cmp, X_INT, Int, X_INT, |a, b| Ok(xcmp(a, b)));
