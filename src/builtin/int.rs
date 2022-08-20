use crate::builtin::core::xcmp;
use crate::builtin::sequence::{XSequence, XSequenceType};
use crate::xtype::{XFuncParamSpec, XFuncSpec, X_BOOL, X_FLOAT, X_INT, X_STRING, X_UNKNOWN};
use crate::xvalue::{ManagedXValue, XValue};
use crate::{
    add_binop, add_ufunc, add_ufunc_ref, eval, manage_native, meval, to_primitive, Bind,
    CompilationError, RTCell, XCompilationScope, XStaticFunction, XType,
};
use num::{BigInt, BigRational, Integer, Signed, ToPrimitive, Zero};
use rc::Rc;
use std::rc;
use std::sync::Arc;
use string_interner::StringInterner;

pub fn add_int_type(
    scope: &mut XCompilationScope,
    interner: &mut StringInterner,
) -> Result<(), CompilationError> {
    scope.add_native_type(interner.get_or_intern_static("int"), X_INT.clone())
}

macro_rules! add_int_binop {
    ($fn_name:ident, $name:ident, $func:expr) => {
        add_binop!($fn_name, $name, X_INT, Int, X_INT, $func);
    };
}

add_int_binop!(add_int_add, add, |a, b| Ok(XValue::Int(a + b)));
add_int_binop!(add_int_sub, sub, |a, b| Ok(XValue::Int(a - b)));
add_int_binop!(add_int_mul, mul, |a, b| Ok(XValue::Int(a * b)));
add_int_binop!(add_int_mod, mod, |a: &BigInt, b: &BigInt| {
    if b.is_zero() {
        Err(String::from("Modulo by zero"))
    } else {
        Ok(XValue::Int(a % b).into())
    }
});
add_int_binop!(add_int_bit_or, bit_or, |a, b| Ok(XValue::Int(a | b).into()));
add_int_binop!(add_int_bit_and, bit_and, |a, b| Ok(
    XValue::Int(a & b).into()
));
add_int_binop!(add_int_bit_xor, bit_xor, |a, b| Ok(
    XValue::Int(a ^ b).into()
));
add_binop!(
    add_int_div,
    div,
    X_INT,
    Int,
    X_FLOAT,
    |a: &BigInt, b: &BigInt| {
        if b.is_zero() {
            Err(String::from("Division by zero"))
        } else {
            // todo quicker shortcut for small ints
            Ok(XValue::Float(BigRational::new(a.clone(), b.clone()).to_f64().unwrap()).into())
        }
    }
);
add_binop!(
    add_int_pow,
    pow,
    X_INT,
    Int,
    X_FLOAT,
    |a: &BigInt, b: &BigInt| if !b.is_positive() && a.is_zero() {
        Err(String::from("cannot raise zero to a non-positive power"))
    } else {
        match b.to_i32() {
            Some(b) => {
                Ok(XValue::Float(BigRational::from(a.clone()).pow(b).to_f64().unwrap()).into())
            }
            None => Err(String::from("exponent too high")),
        }
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

add_ufunc!(add_int_neg, neg, X_INT, Int, X_INT, |a: &BigInt| {
    Ok(XValue::Int(-a))
});

add_ufunc!(
    add_int_to_str,
    to_str,
    X_INT,
    Int,
    X_STRING,
    |a: &BigInt| Ok(XValue::String(a.to_string()).into())
);

add_ufunc!(
    add_int_display,
    display,
    X_INT,
    Int,
    X_STRING,
    |a: &BigInt| {
        println!("{}", a);
        Ok(XValue::String(a.to_string()).into())
    }
);

pub fn add_int_digits(
    scope: &mut XCompilationScope,
    interner: &mut StringInterner,
) -> Result<(), CompilationError> {
    scope.add_func(
        interner.get_or_intern_static("digits"),
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
                let b = to_primitive!(a1, Int, BigInt::from(10 as i64));
                let mut digits = Vec::new();
                let mut n = n.clone();
                while !n.is_zero() {
                    digits.push(n.mod_floor(&b));
                    n = n.div_floor(&b);
                }
                Ok(manage_native!(
                    XSequence::array(
                        digits
                            .into_iter()
                            .map(|v| ManagedXValue::new(XValue::Int(v), rt.clone()))
                            .collect::<Result<_, _>>()?
                    ),
                    rt.clone()
                ))
            },
        ),
    )?;
    Ok(())
}

pub fn add_int_hash(
    scope: &mut XCompilationScope,
    interner: &mut StringInterner,
) -> Result<(), CompilationError> {
    scope.add_func_intern(
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
            |args, ns, tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                let v0 = to_primitive!(a0, Int);
                if !v0.is_negative() && v0.bits() <= 64 {
                    // the number is already within the bounds
                    return Ok(a0.into());
                }
                Ok(ManagedXValue::new(
                    XValue::Int(BigInt::from(v0.iter_u64_digits().next().unwrap_or(0))),
                    rt,
                )?
                .into())
            },
        ),
        interner,
    )?;
    Ok(())
}

add_binop!(add_int_cmp, cmp, X_INT, Int, X_INT, |a, b| Ok(xcmp(a, b)));
