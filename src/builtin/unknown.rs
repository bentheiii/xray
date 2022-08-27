use crate::xtype::{X_BOOL, X_UNKNOWN};
use crate::{unpack_types, CompilationError, RootCompilationScope, XFuncSpec, XStaticFunction};

use std::io::Write;
use std::rc::Rc;

pub(crate) fn add_unknown_eq<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    scope.add_dyn_func("eq", move |_params, types, _ns| {
        let (a0, a1) = unpack_types!(types, 0, 1);
        if a0 != &X_UNKNOWN.clone() {
            return Err(format!("expected exactly unknown, got {a0:?}"));
        }
        if a1 != &X_UNKNOWN.clone() {
            return Err(format!("expected exactly unknown, got {a1:?}"));
        }

        Ok(Rc::new(XStaticFunction::from_native_short_circut(
            XFuncSpec::new(&[&X_UNKNOWN, &X_UNKNOWN], X_BOOL.clone()),
            move |_args, _ns, _tca, _rt| Err("unknown eq applied".to_string()),
        )))
    })
}
