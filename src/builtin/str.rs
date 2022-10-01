use crate::builtin::core::{xcmp, eval};
use crate::xtype::{XFuncSpec, X_BOOL, X_INT, X_STRING};
use crate::xvalue::{ManagedXValue, XValue};
use crate::{
    add_binop, add_ufunc, add_ufunc_ref, to_primitive, CompilationError,
    RootCompilationScope, XStaticFunction,
};

use crate::util::lazy_bigint::LazyBigint;
use rc::Rc;
use std::collections::hash_map::DefaultHasher;

use std::hash::{Hash, Hasher};
use std::io::Write;
use std::rc;

pub(crate) fn add_str_type<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
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

add_ufunc!(add_str_hash, hash, X_STRING, String, X_INT, |a: &String| {
    Ok(XValue::Int({
        let mut s = DefaultHasher::new();
        a.hash(&mut s);
        let hash = s.finish();
        LazyBigint::from(hash)
    }))
});

pub(crate) fn add_str_to_str<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    scope.add_func(
        "to_str",
            XFuncSpec::new(&[&X_STRING], X_STRING.clone()),
        XStaticFunction::from_native(
            |args, ns, tca, rt| ns.eval(&args[0], rt, tca),
        ),
    )
}

add_binop!(add_str_cmp, cmp, X_STRING, String, X_INT, |a, b| Ok(xcmp(
    a, b
)));
