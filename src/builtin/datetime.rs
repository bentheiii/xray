use crate::builtin::builtin_permissions;
use crate::compile_err::CompilationError;
use crate::root_compilation_scope::RootCompilationScope;
use crate::xexpr::XStaticFunction;
use crate::xtype::{XFuncSpec, X_FLOAT};
use crate::xvalue::{ManagedXValue, XValue};
use std::time::SystemTime;

pub(crate) fn add_datetime_now<W, R>(
    scope: &mut RootCompilationScope<W, R>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "__std_unix_now",
        XFuncSpec::new(&[], X_FLOAT.clone()),
        XStaticFunction::from_native(|_args, _ns, _tca, rt| {
            rt.borrow()
                .limits
                .check_permission(&builtin_permissions::NOW)?;
            let utc = SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_secs_f64();
            Ok(ManagedXValue::new(XValue::Float(utc), rt)?.into())
        }),
    )
}