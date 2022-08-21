use crate::builtin::core::xcmp;
use crate::xtype::{XFuncParamSpec, XFuncSpec, X_BOOL, X_INT, X_STRING};
use crate::xvalue::{ManagedXValue, XValue};
use crate::{add_binop, add_ufunc, add_ufunc_ref, eval, to_primitive, CompilationError, XStaticFunction, RootCompilationScope};
use num_bigint::{BigInt};
use rc::Rc;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::rc;


pub fn add_str_type(
    scope: &mut RootCompilationScope,
) -> Result<(), CompilationError> {
    scope.add_native_type("str", X_STRING.clone())
}

add_binop!(add_str_eq, eq, X_STRING, String, X_BOOL, |a, b| Ok(
    XValue::Bool(a == b)
));

add_binop!(add_str_add, add, X_STRING, String, X_STRING, |a, b| {
    let mut ret = String::new();
    ret.push_str(a);
    ret.push_str(b);
    Ok(XValue::String(ret))
});

add_ufunc_ref!(
    add_str_display,
    display,
    X_STRING,
    X_STRING,
    |a: Rc<ManagedXValue>, _rt| {
        if let XValue::String(s) = &a.value {
            println!("{}", s);
            Ok(a.into())
        } else {
            unreachable!();
        }
    }
);

add_ufunc!(add_str_hash, hash, X_STRING, String, X_INT, |a: &String| {
    Ok(XValue::Int({
        let mut s = DefaultHasher::new();
        a.hash(&mut s);
        let hash = s.finish();
        BigInt::from(hash)
    }))
});

add_binop!(add_str_cmp, cmp, X_STRING, String, X_INT, |a, b| Ok(xcmp(
    a, b
)));
