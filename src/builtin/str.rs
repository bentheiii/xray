use std::rc;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use crate::{add_binop, add_ufunc, add_ufunc_ref, Bind, XCompilationScope, XStaticFunction, XType};
use crate::xtype::{X_BOOL, X_INT, X_RATIONAL, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec};
use crate::xvalue::{XValue};
use rc::Rc;
use std::sync::Arc;
use string_interner::StringInterner;

pub fn add_str_type(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    scope.add_native_type(interner.get_or_intern_static("str"), X_STRING.clone())
}

add_binop!(add_str_eq, pow, X_STRING, XValue::String, X_BOOL, |a,b|
    Ok(XValue::Bool(a == b).into())
);

add_ufunc_ref!(add_str_display, display, X_STRING, X_STRING, |a: Rc<XValue>| {
    if let XValue::String(s) = a.as_ref(){
        println!("{}", s);
        Ok(a.into())
    }else{
        unreachable!();
    }
});