use crate::builtin::core::unpack_dyn_types;
use crate::builtin::optional::{XOptional, XOptionalType};
use crate::compile_err::CompilationError;
use crate::root_compilation_scope::RootCompilationScope;
use crate::xtype::{CompoundKind, XFuncSpec, XType};
use crate::xvalue::{ManagedXValue, XFunctionFactoryOutput, XValue};
use crate::{manage_native, to_primitive, xraise};
use std::io::Write;
use std::iter;
use std::rc::Rc;
use std::sync::Arc;

pub(crate) fn add_union_members<W: Write + 'static, R>(
    scope: &mut RootCompilationScope<W, R>,
) -> Result<(), CompilationError> {
    scope.add_dyn_func("members", "unions", move |_params, types, _ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let [t0] = unpack_dyn_types(types)?;
        let XType::Compound(CompoundKind::Union, spec, bind) = t0.as_ref() else { return Err("argument 1 is not a union".to_string()); };
        let n_fields = spec.fields.len();
        let ret_type = Arc::new(XType::Tuple(spec.fields.iter().map(|t| XOptionalType::xtype(t.type_.resolve_bind(bind, Some(t0)))).collect()));



        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(&[t0], ret_type),
            move |args, ns, _tca, rt| {
                let a0 = xraise!(ns.eval(&args[0], rt.clone(), false)?.unwrap_value());
                let (tag,u0) = to_primitive!(a0, UnionInstance).clone();
                let xnone: Rc<ManagedXValue<W, R>> = manage_native!(XOptional::<W, R> {value: None}, rt.clone());
                let not_none = manage_native!(XOptional {value: Some(u0)}, rt.clone());
                let members = iter::repeat(xnone.clone()).take(tag).chain(iter::once(not_none)).chain(iter::repeat(xnone)).take(n_fields).collect();
                let ret = ManagedXValue::new(XValue::StructInstance(members), rt)?;
                Ok(ret.into())
            },
        ))
    })
}
