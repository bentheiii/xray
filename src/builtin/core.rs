use std::fmt::Debug;
use std::io::Write;
use crate::xvalue::XValue;
use num_traits::{One, Zero};

use crate::util::lazy_bigint::LazyBigint;
use std::ops::Neg;

#[macro_export]
macro_rules! add_binop {
    ($fn_name:ident, $name:ident, $operand_type: ident, $operand_variant:ident, $return_type:ident, $func:expr) => {
        pub(crate) fn $fn_name<W: Write + Debug + 'static>(
            scope: &mut RootCompilationScope<W>,
        ) -> Result<(), $crate::CompilationError<W>> {
            scope.add_func(
                stringify!($name),
                XStaticFunction::from_native(
                    XFuncSpec {
                        generic_params: None,
                        params: vec![
                            XFuncParamSpec {
                                type_: $operand_type.clone(),
                                required: true,
                            },
                            XFuncParamSpec {
                                type_: $operand_type.clone(),
                                required: true,
                            },
                        ],
                        ret: $return_type.clone(),
                    },
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
        pub(crate) fn $fn_name<W: Write + Debug + 'static>(
            scope: &mut RootCompilationScope<W>,
        ) -> Result<(), $crate::CompilationError<W>> {
            scope.add_func(
                stringify!($name),
                XStaticFunction::from_native(
                    XFuncSpec {
                        generic_params: None,
                        params: vec![XFuncParamSpec {
                            type_: $operand_type.clone(),
                            required: true,
                        }],
                        ret: $return_type.clone(),
                    },
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

pub(super) fn xcmp<W: Write + Debug + 'static, T: PartialOrd>(rhs: T, lhs: T) -> XValue<W> {
    XValue::Int(if rhs < lhs {
        LazyBigint::one().neg()
    } else if rhs > lhs {
        LazyBigint::one()
    } else {
        LazyBigint::zero()
    })
}
