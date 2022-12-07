use std::convert::TryInto;
use crate::xvalue::{ManagedXValue, XValue};
use num_traits::{One, Zero};

use std::io::Write;

use crate::util::lazy_bigint::LazyBigint;
use crate::xexpr::XExpr;
use crate::{Identifier, RTCell, XType};
use std::ops::Neg;
use std::rc::Rc;
use std::sync::Arc;
use itertools::Itertools;
use crate::compilation_scopes::CompilationScope;
use crate::evaluation_scope::EvaluatedVariable;
use crate::runtime_scope::RuntimeScope;

#[macro_export]
macro_rules! add_binop {
    ($fn_name:ident, $name:ident, $operand_type: ident, $operand_variant:ident, $return_type:ident, $func:expr) => {
        pub(crate) fn $fn_name<W: Write + 'static>(
            scope: &mut RootCompilationScope<W>,
        ) -> Result<(), $crate::CompilationError<W>> {
            scope.add_func(
                stringify!($name),
                XFuncSpec::new(&[&$operand_type, &$operand_type], $return_type.clone()),
                XStaticFunction::from_native(
                    |args, ns, _tca, rt| {
                        let [a0, a1] = eval(args, ns, &rt,[0, 1])?;
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
                XFuncSpec::new(&[&$operand_type], $return_type.clone()),
                XStaticFunction::from_native(
                    |args, ns, _tca, rt| {
                        let [a0,] = eval(args, ns, &rt,[0])?;
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
            other => panic!("error when converting primitive, expected {}, got {:?}", stringify!($v), other),
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

// todo make this a one-to-one func
pub(super) fn eval<W: Write + 'static, const M: usize>(args: &[XExpr<W>], ns: &RuntimeScope<W>, rt: &RTCell<W>, indices: [usize; M]) -> Result<[Rc<ManagedXValue<W>>; M], String> {
    Ok(indices.iter().map(|&i| ns.eval(&args[i], rt.clone(), false).map(|i| i.unwrap_value()))
        .collect::<Result<Result<Vec<_>, _>, _>>()??
        .try_into().unwrap())
}

pub(super) fn eval_if_present<W: Write + 'static, const M: usize>(args: &[XExpr<W>], ns: &RuntimeScope<W>, rt: &RTCell<W>, indices: [usize; M]) -> Result<[Option<Rc<ManagedXValue<W>>>; M], String> {
    Ok(indices.iter().map(|&i| args.get(i).map(
        |a| ns.eval(a, rt.clone(), false).map(|i| i.unwrap_value())
    ).transpose())
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .map(|i| i.transpose())
        .collect::<Result<Vec<_>, _>>()?
        .try_into().unwrap())
}

pub(super) fn eval_result<W: Write + 'static, const M: usize>(args: &[XExpr<W>], ns: &RuntimeScope<W>, rt: &RTCell<W>, indices: [usize; M]) -> Result<[EvaluatedVariable<W>; M], String> {
    Ok(indices.iter().map(|&i| ns.eval(&args[i], rt.clone(), false).map(|i| i.unwrap_value()))
        .collect::<Result<Vec<_>, _>>()?
        .try_into().unwrap())
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
    let ret = scope.get_func(&symbol, arguments).map_err(|e| format!("{e:?}"))?; // todo improve error here
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
    args: Vec<EvaluatedVariable<W>>,
) -> Result<EvaluatedVariable<W>, String> {
    let managed_func = ns.eval(expr, rt.clone(), false)?.unwrap_value()?;
    let func = to_primitive!(managed_func, Function);
    ns.eval_func_with_values(func, args, rt, false).map(|v| v.unwrap_value())
}
