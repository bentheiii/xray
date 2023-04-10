use crate::xvalue::{ManagedXError, ManagedXValue, XValue};
use num_traits::{One, Zero};

use crate::compilation_scope::CompilationScope;
use crate::root_runtime_scope::{EvaluatedValue, RuntimeResult};
use crate::runtime_scope::RuntimeScope;
use crate::util::lazy_bigint::LazyBigint;
use crate::xexpr::{TailedEvalResult, XExpr};
use crate::xtype::CallbackType;
use crate::{Identifier, RTCell, XStaticFunction, XType};
use itertools::Itertools;
use std::ops::Neg;
use std::rc::Rc;
use std::sync::Arc;

#[macro_export]
macro_rules! xraise {
    ($e: expr) => {{
        match $e {
            Ok(__i) => __i,
            Err(__e) => return Ok($crate::xexpr::TailedEvalResult::Value(Err(__e.into()))),
        }
    }};
}

pub fn xerr<W, R, T>(err: Rc<ManagedXError<W, R, T>>) -> RuntimeResult<TailedEvalResult<W, R, T>> {
    Ok(TailedEvalResult::Value(Err(err)))
}

#[macro_export]
macro_rules! add_binfunc {
    ($fn_name:ident, $name:ident, $operand_type: expr, $operand_variant:ident, $return_type:expr, $func:expr) => {
        pub(crate) fn $fn_name<W, R, T>(
            scope: &mut RootCompilationScope<W, R, T>,
        ) -> Result<(), $crate::CompilationError> {
            scope.add_func(
                stringify!($name),
                XFuncSpec::new(&[&$operand_type, &$operand_type], $return_type.clone()),
                XStaticFunction::from_native(|args, ns, _tca, rt| {
                    let a0 = $crate::xraise!(eval(&args[0], ns, &rt)?);
                    let a1 = $crate::xraise!(eval(&args[1], ns, &rt)?);
                    let v0 = to_primitive!(a0, $operand_variant);
                    let v1 = to_primitive!(a1, $operand_variant);
                    let result: Result<_, String> = $func(v0, v1, &rt)?;
                    let result = match result {
                        Ok(v) => Ok(v),
                        Err(s) => Err($crate::xvalue::ManagedXError::new(s, rt.clone())?),
                    };
                    Ok(ManagedXValue::from_result(result, rt)?.into())
                }),
            )
        }
    };
}

pub fn ufunc_ref<W: 'static, R: 'static, T: 'static, F>(func: F) -> XStaticFunction<W, R, T>
where
    F: Fn(Rc<ManagedXValue<W, R, T>>, RTCell<W, R, T>) -> RuntimeResult<TailedEvalResult<W, R, T>>
        + 'static,
{
    XStaticFunction::from_native(move |args, ns, _tca, rt| {
        let a0 = xraise!(eval(&args[0], ns, &rt)?);
        func(a0, rt)
    })
}

#[macro_export]
macro_rules! ufunc {
    ($operand_variant:ident, $func:expr) => {{
        $crate::builtin::core::ufunc_ref(
            |a: Rc<ManagedXValue<W, R, T>>, rt: $crate::runtime::RTCell<W, R, T>| {
                let result: Result<_, String> =
                    $func(to_primitive!(a, $operand_variant), rt.clone())?;
                let result = match result {
                    Ok(v) => Ok(v),
                    Err(s) => Err($crate::xvalue::ManagedXError::new(s, rt.clone())?),
                };
                Ok(ManagedXValue::from_result(result, rt)?.into())
            },
        )
    }};
}

#[macro_export]
macro_rules! to_native {
    ($x: expr, $t: ty) => {
        match &$x.value {
            XValue::Native(__b) => __b.as_ref()._as_any().downcast_ref::<$t>().unwrap(),
            _ => panic!("to_native: expected native value, got {:?}", $x.value),
        }
    };
}

#[macro_export]
macro_rules! to_primitive {
    ($x: expr, $v: ident) => {
        match &$x.value {
            $crate::xvalue::XValue::$v(__b) => __b,
            other => panic!(
                "error when converting primitive, expected {}, got {:?}",
                stringify!($v),
                other
            ),
        }
    };
    ($x: expr, $v: ident, $d: expr) => {
        if let Some(__x) = &$x {
            std::borrow::Cow::Borrowed(to_primitive!(__x, $v))
        } else {
            std::borrow::Cow::Owned($d)
        }
    };
}

pub(super) fn eval<W: 'static, R: 'static, T: 'static>(
    expr: &XExpr<W, R, T>,
    ns: &RuntimeScope<W, R, T>,
    rt: &RTCell<W, R, T>,
) -> RuntimeResult<EvaluatedValue<W, R, T>> {
    ns.eval(expr, rt.clone(), false).map(|i| i.unwrap_value())
}

#[macro_export]
macro_rules! xraise_opt {
    ($e: expr) => {{
        match $e {
            None => None,
            Some(__i) => Some(xraise!(__i)),
        }
    }};
}

#[macro_export]
macro_rules! manage_native {
    ($native: expr, $rt: expr) => {
        ManagedXValue::new(XValue::Native(Box::new($native)), $rt)?.into()
    };
}

pub(super) fn xcmp<I: PartialOrd, W, R, T>(rhs: I, lhs: I) -> XValue<W, R, T> {
    XValue::Int(if rhs < lhs {
        LazyBigint::one().neg()
    } else if rhs > lhs {
        LazyBigint::one()
    } else {
        LazyBigint::zero()
    })
}

pub(crate) fn unpack_dyn_types<const N: usize>(
    dyn_types: Option<&[Arc<XType>]>,
) -> Result<[&Arc<XType>; N], String> {
    if let Some(dyn_types) = dyn_types {
        if dyn_types.len() != N {
            Err(format!("expected {N} arguments, got {}", dyn_types.len()))
        } else {
            Ok(dyn_types.iter().collect_vec().try_into().unwrap())
        }
    } else {
        Err("this binding requires specialization".to_string())
    }
}

#[allow(clippy::type_complexity)]
pub(crate) fn unpack_dyn_types_with_optional<const N: usize, const P: usize>(
    dyn_types: Option<&[Arc<XType>]>,
) -> Result<([&Arc<XType>; N], [Option<&Arc<XType>>; P]), String> {
    if let Some(dyn_types) = dyn_types {
        if dyn_types.len() < N || dyn_types.len() > (N + P) {
            Err(format!(
                "expected between {N} and {P} arguments, got {}",
                dyn_types.len()
            ))
        } else {
            Ok((
                (dyn_types.iter().take(N).collect_vec().try_into().unwrap()),
                (dyn_types
                    .iter()
                    .skip(N)
                    .map(Some)
                    .pad_using(P, |_| None)
                    .collect_vec()
                    .try_into()
                    .unwrap()),
            ))
        }
    } else {
        Err("this binding requires specialization".to_string())
    }
}

pub(crate) fn unpack_dyn_types_at_least<const N: usize>(
    dyn_types: Option<&[Arc<XType>]>,
) -> Result<[&Arc<XType>; N], String> {
    if let Some(dyn_types) = dyn_types {
        if dyn_types.len() < N {
            Err(format!(
                "expected at least {N} arguments, got {}",
                dyn_types.len()
            ))
        } else {
            Ok(dyn_types.iter().take(N).collect_vec().try_into().unwrap())
        }
    } else {
        Err("this binding requires specialization".to_string())
    }
}

pub(crate) fn unpack_native<'a>(
    t: &'a Arc<XType>,
    name: &'static str,
) -> Result<&'a [Arc<XType>], String> {
    unpack_natives(t, &[name])
}

pub(crate) fn unpack_natives<'a>(
    t: &'a Arc<XType>,
    names: &[&'static str],
) -> Result<&'a [Arc<XType>], String> {
    match t.as_ref() {
        XType::XNative(nt0, bind) if names.iter().any(|n| *n == nt0.name()) => Ok(&bind[..]),
        _ => Err(format!("Expected one of {names:?} type, got {t:?}")),
    }
}

pub(super) fn get_func<W, R, T>(
    scope: &mut CompilationScope<W, R, T>,
    symbol: Identifier,
    arguments: &[Arc<XType>],
    expected_return_type: &Arc<XType>,
) -> Result<XExpr<W, R, T>, String> {
    get_func_with_type(scope, symbol, arguments, Some(expected_return_type)).map(|x| x.0)
}

pub(super) fn get_func_with_type<W, R, T>(
    scope: &mut CompilationScope<W, R, T>,
    symbol: Identifier,
    arguments: &[Arc<XType>],
    expected_return_type: Option<&Arc<XType>>,
) -> Result<(XExpr<W, R, T>, CallbackType), String> {
    let ret = scope
        .get_func(&symbol, arguments)
        .map_err(|e| format!("{e:?}"))?;
    let ret_xtype = scope.type_of(&ret).unwrap();
    let cb = CallbackType::new(ret_xtype, arguments);
    if let Some(ert) = expected_return_type {
        if &cb.rtype() != ert {
            return Err(format!(
                "expected {symbol:?}{{{arguments:?}}} to return {ert:?}, got {:?} instead",
                cb.rtype()
            ));
        }
    }
    Ok((ret, cb))
}

#[macro_export]
macro_rules! parse_hash {
    ($v: expr, $rt: expr) => {{
        xraise!(to_primitive!(xraise!($v.unwrap_value()), Int)
            .to_u64()
            .ok_or($crate::xvalue::ManagedXError::new(
                "hash is out of bounds",
                $rt
            )?))
    }};
}

pub(crate) fn search<W, R, T, I: IntoIterator>(
    other: I,
    rt: RTCell<W, R, T>,
) -> impl Iterator<Item = (I::Item, RuntimeResult<()>)> {
    let s = rt.borrow().limits.search_iter();
    other.into_iter().zip(s)
}

#[macro_export]
macro_rules! delegate {
    (
        with[$($func: ident),*],
        args[$($idx:literal -> $arg: ident),*],
        $call:ident($($param: ident),*)
    ) => {
        move |ns, rt| {
            $(
                let $func = forward_err!(ns.eval(&$func, rt.clone(), false)?.unwrap_value());
            )*
            Ok(Ok(
                #[allow(unused_variables)]
                move |args: &[XExpr<W, R, T>], ns: &RuntimeScope<'_, W, R, T>, _tca, rt: RTCell<_, _, _>| {
                    $(
                        let $arg = xraise!(eval(&args[$idx], ns, &rt)?);
                    )*
                    $(
                        let $func = $func.clone();
                    )*
                    let XValue::Function($call) = &$call.value else {unreachable!()};
                    ns.eval_func_with_values(
                        $call,
                        vec![
                            $(
                            Ok($param)
                            ),*
                        ],
                        rt,
                        false,
                    )
                },
            ))
        }
    };
}
