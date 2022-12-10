use crate::xtype::{X_BOOL, X_UNKNOWN};
use crate::{unpack_types, xraise, CompilationError, RootCompilationScope, XFuncSpec};

use crate::xvalue::{ManagedXError, XFunctionFactoryOutput};
use std::io::Write;

pub(crate) fn add_unknown_eq<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_dyn_func("eq", move |_params, types, _ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let (a0, a1) = unpack_types!(types, 0, 1);
        if a0 != &X_UNKNOWN.clone() {
            return Err(format!("expected exactly unknown, got {a0:?}"));
        }
        if a1 != &X_UNKNOWN.clone() {
            return Err(format!("expected exactly unknown, got {a1:?}"));
        }

        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(&[&X_UNKNOWN, &X_UNKNOWN], X_BOOL.clone()),
            move |_args, _ns, _tca, rt| xraise!(Err(ManagedXError::new("unknown eq applied",rt)?)),
        ))
    })
}
