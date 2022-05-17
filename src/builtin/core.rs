#[macro_export]
macro_rules! add_binop {
    ($fn_name:ident, $name:ident, $operand_type: ident, $operand_variant:ident, $return_type:ident, $func:expr) => {
        pub fn $fn_name(scope: &mut XCompilationScope<'_>, interner: &mut StringInterner) -> Result<(), String> {
            scope.add_func(
                interner.get_or_intern_static(stringify!($name)), XStaticFunction::Native(XFuncSpec {
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
                }, |args, ns, _tca| {
                    let (a0,a1) = eval!(args, ns, 0, 1);
                    let v0 = to_primitive!(a0, $operand_variant);
                    let v1 = to_primitive!(a1, $operand_variant);
                    $func(v0, v1)
                }))?;
            Ok(())
        }
    };
}

#[macro_export]
macro_rules! add_ufunc_ref {
    ($fn_name:ident, $name:ident, $operand_type: ident, $return_type:ident, $func:expr) => {
        pub fn $fn_name(scope: &mut XCompilationScope<'_>, interner: &mut StringInterner) -> Result<(), String> {
            scope.add_func(
                interner.get_or_intern_static(stringify!($name)), XStaticFunction::Native(XFuncSpec {
                    generic_params: None,
                    params: vec![
                        XFuncParamSpec {
                            type_: $operand_type.clone(),
                            required: true,
                        },
                    ],
                    ret: $return_type.clone(),
                }, |args, ns, _tca| {
                    let (a0,) = eval!(args, ns, 0);
                    $func(a0)
                }))?;
            Ok(())
        }
    };
}

#[macro_export]
macro_rules! add_ufunc {
    ($fn_name:ident, $name:ident, $operand_type: ident, $operand_variant:ident, $return_type:ident, $func:expr) => {
        add_ufunc_ref!($fn_name, $name, $operand_type, $return_type, |a: Rc<XValue>| $func(to_primitive!(a, $operand_variant)));
    };
}

#[macro_export]
macro_rules! to_native {
    ($x: expr, $t: ty) => {
        match $x.as_ref() {
            XValue::Native(__b) => {
                __b.as_ref()._as_any().downcast_ref::<$t>().unwrap()
            },
            _ => unreachable!(),
        }
    };
}

#[macro_export]
macro_rules! to_primitive {
    ($x: expr, $v: ident) => {
        match $x.as_ref() {
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
    ($args: expr, $ns: expr, $($idx:expr),*) => {
        ($(
            $args[$idx].eval(&$ns, false)?.unwrap_value(),
        )*)
    };
}

#[macro_export]
macro_rules! meval {
    ($args: expr, $ns: expr, $($idx:expr),*) => {
        ($(
            $args.get($idx).map(|e| e.eval(&$ns, false)).transpose()?.map(|e| e.unwrap_value().clone()),
        )*)
    };
}