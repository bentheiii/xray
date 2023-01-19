use std::io::Write;
use std::sync::Arc;
use crate::compile_err::CompilationError;
use crate::root_compilation_scope::RootCompilationScope;
use crate::{unpack_types};
use crate::xtype::{CompoundKind, XFuncSpec, XType};
use crate::xvalue::XFunctionFactoryOutput;

pub(crate) fn add_struct_members<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_dyn_func("members", "structs", move |_params, types, _ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let (t0, ) = unpack_types!(types, 0);
        let XType::Compound(CompoundKind::Struct, spec, bind) = t0.as_ref() else { return Err("argument 1 is not a struct".to_string()); };
        let ret_type = Arc::new(XType::Tuple(spec.fields.iter().map(|t| t.type_.resolve_bind(bind, Some(t0))).collect()));


        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(&[t0], ret_type),
            move |args, ns, tca, rt| {
                ns.eval(&args[0], rt, tca)
            },
        ))
    })
}