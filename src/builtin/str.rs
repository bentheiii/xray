use std::rc;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use crate::{add_binop, add_ufunc, add_ufunc_ref, eval, to_primitive, Bind, XCompilationScope, XStaticFunction, XType, CompilationError};
use crate::xtype::{X_BOOL, X_INT, X_RATIONAL, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec};
use crate::xvalue::{XValue, ManagedXValue};
use rc::Rc;
use std::sync::Arc;
use string_interner::StringInterner;

pub fn add_str_type(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    scope.add_native_type(interner.get_or_intern_static("str"), X_STRING.clone())
}

add_binop!(add_str_eq, eq, X_STRING, String, X_BOOL, |a,b|
    Ok(XValue::Bool(a == b))
);

add_binop!(add_str_add, add, X_STRING, String, X_STRING, |a,b| {
    let mut ret = String::new();
    ret.push_str(a);
    ret.push_str(b);
    Ok(XValue::String(ret))
    }
);

add_ufunc_ref!(add_str_display, display, X_STRING, X_STRING, |a: Rc<ManagedXValue>, rt| {
    if let XValue::String(s) = &a.value{
        println!("{}", s);
        Ok(a.into())
    }else{
        unreachable!();
    }
});