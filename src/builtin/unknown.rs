use crate::xtype::{X_BOOL, X_STRING, X_UNKNOWN};
use crate::{unpack_types, CompilationError, RootCompilationScope, XFuncSpec};

use crate::builtin::core::xerr;
use crate::xvalue::{ManagedXError, XFunctionFactoryOutput};
use std::io::Write;

pub(crate) fn add_unknown_eq<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_dyn_func("eq", "unknown", move |_params, types, _ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let (a0, a1) = unpack_types!(types, 0, 1);

        if a0 != &X_UNKNOWN.clone() && a1 != &X_UNKNOWN.clone() {
            return Err(format!(
                "expected at least one unknown, got {a0:?} and {a1:?}"
            ));
        }

        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(&[&X_UNKNOWN, &X_UNKNOWN], X_BOOL.clone()),
            move |_args, _ns, _tca, rt| xerr(ManagedXError::new("unknown eq applied", rt)?),
        ))
    })
}

pub(crate) fn add_unknown_to_str<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_dyn_func("to_str", "unknown", move |_params, types, _ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let (a0,) = unpack_types!(types, 0);

        if a0 != &X_UNKNOWN.clone() {
            return Err(format!("expected at unknown, got {a0:?}"));
        }

        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(&[&X_UNKNOWN], X_STRING.clone()),
            move |_args, _ns, _tca, rt| xerr(ManagedXError::new("unknown to_str applied", rt)?),
        ))
    })
}

pub(crate) fn add_unknown_hash<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_dyn_func("hash", "unknown", move |_params, types, _ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let (a0,) = unpack_types!(types, 0);

        if a0 != &X_UNKNOWN.clone() {
            return Err(format!("expected at unknown, got {a0:?}"));
        }

        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(&[&X_UNKNOWN], X_STRING.clone()),
            move |_args, _ns, _tca, rt| xerr(ManagedXError::new("unknown hash applied", rt)?),
        ))
    })
}
