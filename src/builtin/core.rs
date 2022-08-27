use crate::xvalue::{ManagedXValue, XValue};
use num_traits::{One, Zero};

use std::io::Write;

use crate::util::lazy_bigint::LazyBigint;
use crate::xexpr::XExpr;
use crate::{Identifier, RTCell, XCompilationScope, XEvaluationScope, XType};
use std::ops::Neg;
use std::rc::Rc;
use std::sync::Arc;

#[macro_export]
macro_rules! add_binop {
    ($fn_name:ident, $name:ident, $operand_type: ident, $operand_variant:ident, $return_type:ident, $func:expr) => {
        pub(crate) fn $fn_name<W: Write + 'static>(
            scope: &mut RootCompilationScope<W>,
        ) -> Result<(), $crate::CompilationError<W>> {
            scope.add_func(
                stringify!($name),
                XStaticFunction::from_native(
                    XFuncSpec::new(&[&$operand_type, &$operand_type], $return_type.clone()),
                    |args, ns, _tca, rt| {
                        let (a0, a1) = eval!(args, ns, rt, 0, 1);
                        let v0 = to_primitive!(a0, $operand_variant);
                        let v1 = to_primitive!(a1, $operand_variant);
                        let result: Result<_, String> = $func(v0, v1);
                        Ok(ManagedXValue::new(result?, rt.clone())?.into())
                    },
                ),
            )
        }
    };
}

#[macro_export]
macro_rules! add_ufunc_ref {
    ($fn_name:ident, $name:ident, $operand_type: ident, $return_type:ident, $func:expr) => {
        pub(crate) fn $fn_name<W: Write + 'static>(
            scope: &mut RootCompilationScope<W>,
        ) -> Result<(), $crate::CompilationError<W>> {
            scope.add_func(
                stringify!($name),
                XStaticFunction::from_native(
                    XFuncSpec::new(&[&$operand_type], $return_type.clone()),
                    |args, ns, _tca, rt| {
                        let (a0,) = eval!(args, ns, rt, 0);
                        $func(a0, rt)
                    },
                ),
            )
        }
    };
}

#[macro_export]
macro_rules! add_ufunc {
    ($fn_name:ident, $name:ident, $operand_type: ident, $operand_variant:ident, $return_type:ident, $func:expr) => {
        add_ufunc_ref!(
            $fn_name,
            $name,
            $operand_type,
            $return_type,
            |a: Rc<ManagedXValue<W>>, rt: $crate::runtime::RTCell<W>| {
                let result: Result<_, String> = $func(to_primitive!(a, $operand_variant));
                Ok(ManagedXValue::new(result?, rt.clone())?.into())
            }
        );
    };
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
            _ => unreachable!(),
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

#[macro_export]
macro_rules! intern {
    ($interner: expr, $($name:expr),*) => {
        vec![$(
            $interner.get_or_intern_static($name),
        )*]
    };
}

#[macro_export]
macro_rules! eval {
    ($args: expr, $ns: expr, $rt: expr, $($idx:expr),*) => {
        ($(
            $args[$idx].eval(&$ns, false, $rt.clone())?.unwrap_value(),
        )*)
    };
}

#[macro_export]
macro_rules! meval {
    ($args: expr, $ns: expr, $rt: expr, $($idx:expr),*) => {
        ($(
            $args.get($idx).map(|e| e.eval(&$ns, false, $rt.clone())).transpose()?.map(|e| e.unwrap_value().clone()),
        )*)
    };
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
        (
            $(
                $types.get($required).ok_or_else(|| {
                    let expected = if (__MIN_SIZE == __MAX_SIZE) {format!("{}", __MIN_SIZE)} else {format!("between {} and {}", __MIN_SIZE, __MAX_SIZE)};
                    format!("expected {} arguments, got {}", expected, $required)
                })?
            ),*
            $(,$($types.get($optional)),*)?
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
    scope: &XCompilationScope<W>,
    symbol: Identifier,
    arguments: &[Arc<XType>],
    expected_return_type: &Arc<XType>,
) -> Result<XExpr<W>, String> {
    let ret = scope.resolve_overload(symbol, arguments)?;
    let ret_xtype = ret.xtype().unwrap();
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
    ns: &XEvaluationScope<W>,
    rt: RTCell<W>,
    args: &[Rc<ManagedXValue<W>>],
) -> Result<Rc<ManagedXValue<W>>, String> {
    let managed_func = expr.eval(ns, false, rt.clone())?.unwrap_value();
    to_primitive!(managed_func, Function).eval_values(args, ns, rt)
}
