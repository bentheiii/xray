#[macro_export]
macro_rules! add_binop {
    ($fn_name:ident, $name:ident, $operand_type: ident, $operand_variant:path, $return_type:ident, $func:expr) => {
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
                    let a = args[0].eval(&ns,false)?.unwrap_value();
                    let b = args[1].eval(&ns,false)?.unwrap_value();
                    match (a.as_ref(), b.as_ref()) {
                        ($operand_variant(a), $operand_variant(b)) => $func(a,b),
                        (other_a, other_b) => {
                            println!("entered function with unexpected args {:?} and {:?}", other_a, other_b);
                            unreachable!()
                        },
                    }
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
                    let a = args[0].eval(&ns,false)?.unwrap_value();
                    $func(a)
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