use crate::builtin::builtin_permissions;
use crate::compile_err::CompilationError;
use crate::root_compilation_scope::RootCompilationScope;
use crate::runtime::RTCell;
use crate::time_provider::TimeProvider;
use crate::xexpr::XStaticFunction;
use crate::xraise;
use crate::xtype::{XFuncSpec, X_FLOAT};
use crate::xvalue::{ManagedXValue, XValue};

pub(crate) fn add_datetime_now<W, R, T: TimeProvider>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "__std_unix_now",
        XFuncSpec::new(&[], X_FLOAT.clone()),
        XStaticFunction::from_native(|_args, _ns, _tca, rt: RTCell<_, _, T>| {
            rt.limits.check_permission(&builtin_permissions::NOW)?;
            let utc = rt.time_provider.unix_now();
            let utc = xraise!(XValue::float(utc, &rt)?);
            Ok(ManagedXValue::new(utc, rt)?.into())
        }),
    )
}
