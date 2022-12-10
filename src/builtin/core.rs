use crate::xvalue::{ManagedXValue, XValue};
use num_traits::{One, Zero};

use std::io::Write;

use crate::compilation_scope::CompilationScope;
use crate::evaluation_scope::EvaluatedValue;
use crate::runtime_violation::RuntimeViolation;
use crate::runtime_scope::RuntimeScope;
use crate::util::lazy_bigint::LazyBigint;
use crate::xexpr::{TailedEvalResult, XExpr};
use crate::{forward_err, Identifier, RTCell, XStaticFunction, XType};
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

#[macro_export]
macro_rules! add_binfunc {
    ($fn_name:ident, $name:ident, $operand_type: ident, $operand_variant:ident, $return_type:ident, $func:expr) => {
        pub(crate) fn $fn_name<W: Write + 'static>(
            scope: &mut RootCompilationScope<W>,
        ) -> Result<(), $crate::CompilationError> {
            scope.add_func(
                stringify!($name),
                XFuncSpec::new(&[&$operand_type, &$operand_type], $return_type.clone()),
                XStaticFunction::from_native(|args, ns, _tca, rt| {
                    let a0 = $crate::xraise!(eval(&args[0], ns, &rt)?);
                    let a1 = $crate::xraise!(eval(&args[1], ns, &rt)?);
                    let v0 = to_primitive!(a0, $operand_variant);
                    let v1 = to_primitive!(a1, $operand_variant);
                    let result: Result<_, String> = $func(v0, v1);
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

pub fn ufunc_ref<W: Write + 'static, F>(func: F) -> XStaticFunction<W>
where
    F: Fn(Rc<ManagedXValue<W>>, RTCell<W>) -> Result<TailedEvalResult<W>, RuntimeViolation> + 'static,
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
            |a: Rc<ManagedXValue<W>>, rt: $crate::runtime::RTCell<W>| {
                let result: Result<_, String> = $func(to_primitive!(a, $operand_variant));
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
            XValue::$v(__b) => __b,
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

pub(super) fn eval<W: Write + 'static>(
    expr: &XExpr<W>,
    ns: &RuntimeScope<W>,
    rt: &RTCell<W>,
) -> Result<EvaluatedValue<W>, RuntimeViolation> {
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

pub(super) fn xcmp<W: Write + 'static, T: PartialOrd>(rhs: T, lhs: T) -> XValue<W> {
    XValue::Int(if rhs < lhs {
        LazyBigint::one().neg()
    } else if rhs > lhs {
        LazyBigint::one()
    } else {
        LazyBigint::zero()
    })
}

#[macro_export]
macro_rules! count {
    () => (0usize);
    ( $x:tt $($xs:tt)* ) => (1usize + $crate::count!($($xs)*));
}

#[macro_export]
macro_rules! unpack_types {
    ($types: expr, $($required: literal),* $(| $($optional: literal),*)?) => {{
        const __MIN_SIZE: usize = $crate::count!($($required),*);
        const __MAX_SIZE: usize = __MIN_SIZE + $crate::count!($($($optional),*)?);
        let __types = $types.ok_or_else(|| "this binding requires specialization".to_string())?;
        (
            $(
                __types.get($required).ok_or_else(|| {
                    let expected = if (__MIN_SIZE == __MAX_SIZE) {format!("{}", __MIN_SIZE)} else {format!("between {} and {}", __MIN_SIZE, __MAX_SIZE)};
                    format!("expected {} arguments, got {}", expected, $required)
                })?
            ),*
            $(,$(__types.get($optional)),*)?,
        )
    }};
}

#[macro_export]
macro_rules! unpack_native {
    ($t: expr, $name: literal, $($bind_idx: literal),*) => {{
        match $t.as_ref(){
            XType::XNative(nt0, bind) if nt0.name() == $name => (
                $(
                bind[$bind_idx].clone()
                ),*,
            ),
            _ => return Err(format!("Expected {} type, got {:?}", $name, $t))
        }
    }};
}

pub(super) fn get_func<W: Write + 'static>(
    scope: &mut CompilationScope<W>,
    symbol: Identifier,
    arguments: &[Arc<XType>],
    expected_return_type: &Arc<XType>,
) -> Result<XExpr<W>, String> {
    let ret = scope
        .get_func(&symbol, arguments)
        .map_err(|e| format!("{e:?}"))?;
    let ret_xtype = scope.type_of(&ret).unwrap();
    let func_spec = if let XType::XFunc(spec) = ret_xtype.as_ref() {
        spec
    } else {
        return Err(format!("expected {symbol:?} function, got {ret_xtype:?}"));
    };
    if &func_spec.ret != expected_return_type {
        return Err(format!("expected {symbol:?}::<{arguments:?}> to return {expected_return_type:?}, got {:?} instead", func_spec.ret));
    }
    Ok(ret)
}

pub(super) fn eval_resolved_func<W: Write + 'static>(
    expr: &XExpr<W>,
    ns: &RuntimeScope<W>,
    rt: RTCell<W>,
    args: Vec<EvaluatedValue<W>>,
) -> Result<EvaluatedValue<W>, RuntimeViolation> {
    let managed_func = forward_err!(ns.eval(expr, rt.clone(), false)?.unwrap_value());
    let func = to_primitive!(managed_func, Function);
    ns.eval_func_with_values(func, args, rt, false)
        .map(|v| v.unwrap_value())
}
