use crate::builtin::core::xcmp;
use crate::builtin::optional::{XOptional, XOptionalType};
use crate::xtype::{XFuncParamSpec, XFuncSpec, X_BOOL, X_INT, X_STRING};
use crate::xvalue::{ManagedXValue, XValue};
use crate::{
    add_binop, add_ufunc, add_ufunc_ref, eval, manage_native, to_primitive, CompilationError,
    RootCompilationScope, XStaticFunction,
};
use num_traits::{One, Zero};
use rc::Rc;
use std::rc;

pub fn add_bool_type(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    scope.add_native_type("bool", X_BOOL.clone())
}

add_binop!(add_bool_eq, eq, X_BOOL, Bool, X_BOOL, |a, b| Ok(
    XValue::Bool(a == b)
));

add_ufunc!(
    add_bool_display,
    display,
    X_BOOL,
    Bool,
    X_STRING,
    |a: &bool| {
        println!("{}", a);
        Ok(XValue::String(a.to_string()))
    }
);

add_ufunc_ref!(
    add_assert,
    assert,
    X_BOOL,
    X_BOOL,
    |a: Rc<ManagedXValue>, _rt| {
        if let XValue::Bool(true) = a.value {
            Ok(a.into())
        } else {
            Err("assertion is untrue".to_string())
        }
    }
);

add_ufunc!(add_bool_not, not, X_BOOL, Bool, X_BOOL, |a: &bool| {
    Ok(XValue::Bool(!a))
});

pub fn add_and(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    scope.add_func(
        "and",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: None,
                params: vec![
                    XFuncParamSpec {
                        type_: X_BOOL.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: X_BOOL.clone(),
                        required: true,
                    },
                ],
                ret: X_BOOL.clone(),
            },
            |args, ns, tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                if *to_primitive!(a0, Bool) {
                    return args[1].eval(ns, tca, rt);
                }
                Ok(a0.into())
            },
        ),
    )
}

pub fn add_or(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    scope.add_func(
        "or",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: None,
                params: vec![
                    XFuncParamSpec {
                        type_: X_BOOL.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: X_BOOL.clone(),
                        required: true,
                    },
                ],
                ret: X_BOOL.clone(),
            },
            |args, ns, tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                if !*to_primitive!(a0, Bool) {
                    return args[1].eval(ns, tca, rt);
                }
                Ok(a0.into())
            },
        ),
    )
}

pub fn add_bool_then(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);

    scope.add_func(
        "then",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: X_BOOL.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: t.clone(),
                        required: true,
                    },
                ],
                ret: XOptionalType::xtype(t),
            },
            |args, ns, _tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                Ok(manage_native!(
                    XOptional {
                        value: if *to_primitive!(a0, Bool) {
                            let (a1,) = eval!(args, ns, rt, 1);
                            Some(a1)
                        } else {
                            None
                        }
                    },
                    rt
                ))
            },
        ),
    )
}

add_ufunc!(add_bool_hash, hash, X_BOOL, Bool, X_INT, |a: &bool| {
    Ok(XValue::Int(if *a { One::one() } else { Zero::zero() }))
});

add_binop!(add_bool_cmp, cmp, X_BOOL, Bool, X_INT, |a, b| Ok(xcmp(
    a, b
)));
