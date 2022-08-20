use crate::builtin::core::xcmp;
use crate::xtype::{XFuncParamSpec, XFuncSpec, X_BOOL, X_FLOAT, X_INT, X_STRING, X_UNKNOWN};
use crate::xvalue::{ManagedXValue, XValue};
use crate::{
    add_binop, add_ufunc, add_ufunc_ref, eval, to_primitive, Bind, CompilationError,
    XCompilationScope, XStaticFunction, XType,
};
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use rc::Rc;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::rc;
use std::sync::Arc;
use string_interner::StringInterner;

pub fn add_str_type(
    scope: &mut XCompilationScope,
    interner: &mut StringInterner,
) -> Result<(), CompilationError> {
    scope.add_native_type(interner.get_or_intern_static("str"), X_STRING.clone())
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
    |a: Rc<ManagedXValue>, rt| {
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
