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
    ($fn_name:ident, $name:ident, $operand_type: ident, $operand_variant:path, $return_type:ident, $func:expr) => {
        add_ufunc_ref!($fn_name, $name, $operand_type, $return_type, |a: Rc<XValue>| {
            if let $operand_variant(a) = a.as_ref() {
                $func(a)
            } else {
                unreachable!()
            }
        });
    };
}