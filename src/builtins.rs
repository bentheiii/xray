use std::rc;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use crate::{XCompilationScope, XStaticFunction, XType};
use crate::xexpr::XExpr;
use crate::xtype::{X_BOOL, X_INT, X_RATIONAL, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec};
use crate::XType::XUnknown;
use crate::xvalue::{XValue};
use rc::Rc;

macro_rules! add_binop {
    ($fn_name:ident, $name:ident, $operand_type: ident, $operand_variant:path, $return_type:ident, $func:expr) => {
        pub fn $fn_name(scope: &mut XCompilationScope<'_, '_>) -> Result<(), String> {
            scope.add_func(
                stringify!($name), XStaticFunction::Native(XFuncSpec {
                    generic_params: None,
                    params: vec![
                        XFuncParamSpec {
                            type_: Box::new($operand_type),
                            required: true,
                        },
                        XFuncParamSpec {
                            type_: Box::new($operand_type),
                            required: true,
                        },
                    ],
                    ret: Box::new($return_type),
                }, |args, ns, tca| {
                    let a = args[0].eval(&ns,false)?.unwrap_value();
                    let b = args[1].eval(&ns,false)?.unwrap_value();
                    match (a.as_ref(), b.as_ref()) {
                        ($operand_variant(a), $operand_variant(b)) => $func(a,b),
                        _ => unreachable!(),
                    }
                }))?;
            Ok(())
        }
    };
}

macro_rules! add_ufunc_ref {
    ($fn_name:ident, $name:ident, $operand_type: ident, $return_type:ident, $func:expr) => {
        pub fn $fn_name<'s>(scope: &mut XCompilationScope<'_, 's>) -> Result<(), String> {
            scope.add_func(
                stringify!($name), XStaticFunction::Native(XFuncSpec {
                    generic_params: None,
                    params: vec![
                        XFuncParamSpec {
                            type_: Box::new($operand_type),
                            required: true,
                        },
                    ],
                    ret: Box::new($return_type),
                }, |args, ns, tca| {
                    let a = args[0].eval(&ns,false)?.unwrap_value();
                    $func(a)
                }))?;
            Ok(())
        }
    };
}

macro_rules! add_ufunc {
    ($fn_name:ident, $name:ident, $operand_type: ident, $operand_variant:path, $return_type:ident, $func:expr) => {
        add_ufunc_ref!($fn_name, $name, $operand_type, $return_type, |a: Rc<XValue<'_>>| {
            if let $operand_variant(a) = a.as_ref() {
                $func(a)
            } else {
                unreachable!()
            }
        });
    };
}

// region ints
pub fn add_int_type(scope: &mut XCompilationScope) -> Result<(), String> {
    scope.add_native_type("int", X_INT)
}

macro_rules! add_int_binop {
    ($fn_name:ident, $name:ident, $func:expr) => {add_binop!($fn_name, $name, X_INT, XValue::Int, X_INT, $func);};
}

add_int_binop!(add_int_add, add, |a,b| Ok(XValue::Int(a + b).into()));
add_int_binop!(add_int_sub, sub, |a,b| Ok(XValue::Int(a - b).into()));
add_int_binop!(add_int_mul, mul, |a,b| Ok(XValue::Int(a * b).into()));
add_int_binop!(add_int_mod, mod, |a,b| Ok(XValue::Int(a % b).into()));
add_int_binop!(add_int_bit_or, bit_or, |a,b| Ok(XValue::Int(a | b).into()));
add_int_binop!(add_int_bit_and, bit_and, |a,b| Ok(XValue::Int(a & b).into()));
add_int_binop!(add_int_bit_xor, bit_xor, |a,b| Ok(XValue::Int(a ^ b).into()));
add_binop!(add_int_div, div, X_INT, XValue::Int, X_RATIONAL, |a: &BigInt, b : &BigInt| {
        let denom = b.clone();
        if denom == Zero::zero() {
            Err(String::from("Division by zero"))
        } else {
            Ok(XValue::Rational(BigRational::new(a.clone(), denom)).into())
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

add_ufunc!(add_int_to_str, to_str, X_INT, XValue::Int, X_STRING, |a:&BigInt| Ok(XValue::String(a.to_string()).into()));

add_ufunc!(add_int_display, display, X_INT, XValue::Int, X_STRING, |a:&BigInt| {
    println!("{}", a);
    Ok(XValue::String(a.to_string()).into())
});
// endregion

// region strings
pub fn add_str_type(scope: &mut XCompilationScope) -> Result<(), String> {
    scope.add_native_type("str", X_STRING)
}

add_binop!(add_str_eq, pow, X_STRING, XValue::String, X_BOOL, |a,b|
    Ok(XValue::Bool(a == b).into())
);

add_ufunc!(add_panic, panic, X_STRING, XValue::String, XUnknown, |a: &String| Err(a.clone()));
add_ufunc_ref!(add_str_display, display, X_STRING, X_STRING, |a: Rc<XValue<'s>>| {
    if let XValue::String(s) = a.as_ref(){
        println!("{}", s);
        Ok(a.into())
    }else{
        unreachable!();
    }
});
// endregion

// region bools
pub fn add_bool_type(scope: &mut XCompilationScope) -> Result<(), String> {
    scope.add_native_type("bool", X_BOOL)
}

add_binop!(add_bool_eq, pow, X_BOOL, XValue::Bool, X_BOOL, |a,b|
    Ok(XValue::Bool(a == b).into())
);

add_ufunc!(add_bool_display, display, X_BOOL, XValue::Bool, X_STRING, |a:&bool| {
    println!("{}", a);
    Ok(XValue::String(a.to_string()).into())
});

add_ufunc_ref!(add_assert, display, X_BOOL, X_BOOL, |a:Rc<XValue<'s>>| {
    if let XValue::Bool(true) = a.as_ref(){
        Ok(a.into())
    }else{
        Err("assertion is untrue".to_string())
    }
});

pub fn add_and(scope: &mut XCompilationScope) -> Result<(), String> {
    scope.add_func(
        "and", XStaticFunction::Native(XFuncSpec {
            generic_params: None,
            params: vec![
                XFuncParamSpec {
                    type_: Box::new(X_BOOL),
                    required: true,
                },
                XFuncParamSpec {
                    type_: Box::new(X_BOOL),
                    required: true,
                },
            ],
            ret: Box::new(X_BOOL),
        }, |args, ns, tca| {
            let lhs = args[0].eval(&ns, false)?.unwrap_value();
            if let XValue::Bool(true) = lhs.as_ref() {
                return Ok(args[1].eval(&ns, true)?);
            }
            Ok(lhs.into())
        }))?;
    Ok(())
}

pub fn add_or(scope: &mut XCompilationScope) -> Result<(), String> {
    scope.add_func(
        "or", XStaticFunction::Native(XFuncSpec {
            generic_params: None,
            params: vec![
                XFuncParamSpec {
                    type_: Box::new(X_BOOL),
                    required: true,
                },
                XFuncParamSpec {
                    type_: Box::new(X_BOOL),
                    required: true,
                },
            ],
            ret: Box::new(X_BOOL),
        }, |args, ns, tca| {
            let lhs = args[0].eval(&ns, false)?.unwrap_value();
            if let XValue::Bool(false) = lhs.as_ref() {
                return Ok(args[1].eval(&ns, true)?);
            }
            Ok(lhs.into())
        }))?;
    Ok(())
}
// endregion

// region generic
pub fn add_if(scope: &mut XCompilationScope) -> Result<(), String> {
    scope.add_func(
        "if", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["T".to_string()]),
            params: vec![
                XFuncParamSpec {
                    type_: Box::new(X_BOOL),
                    required: true,
                },
                XFuncParamSpec {
                    type_: Box::new(XType::XGeneric("T".to_string())),
                    required: true,
                },
                XFuncParamSpec {
                    type_: Box::new(XType::XGeneric("T".to_string())),
                    required: true,
                },
            ],
            ret: Box::new(XType::XGeneric("T".to_string())),
        }, |args, ns, tca| {
            let cond = args[0].eval(&ns, false)?.unwrap_value();
            match cond.as_ref() {
                XValue::Bool(true) => args[1].eval(&ns, true),
                XValue::Bool(false) => args[2].eval(&ns, true),
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}
// endregion


// region general
// endregion