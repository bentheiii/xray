use crate::xtype::{X_BOOL, X_UNKNOWN};
use crate::{
    eval, CompilationError, XCompilationScope, XFuncParamSpec, XFuncSpec, XStaticFunction, XType,
};
use std::fmt::format;
use std::rc::Rc;
use string_interner::StringInterner;

pub fn add_unknown_eq(
    scope: &mut XCompilationScope,
    interner: &mut StringInterner,
) -> Result<(), CompilationError> {
    scope.add_dyn_func(
        interner.get_or_intern_static("eq"),
        move |_params, types, ns| {
            if types.len() != 2 {
                return Err(format!("Expected 2 types, got {}", types.len()));
            }
            let a0 = types[0].clone();
            if a0 != X_UNKNOWN.clone() {
                return Err(format!("expected exactly unknown, got {a0:?}"));
            }
            let a1 = types[1].clone();
            if a1 != X_UNKNOWN.clone() {
                return Err(format!("expected exactly unknown, got {a1:?}"));
            }

            Ok(Rc::new(XStaticFunction::from_native_short_circut(
                XFuncSpec {
                    generic_params: None,
                    params: vec![
                        XFuncParamSpec {
                            type_: X_UNKNOWN.clone(),
                            required: true,
                        },
                        XFuncParamSpec {
                            type_: X_UNKNOWN.clone(),
                            required: true,
                        },
                    ],
                    ret: X_BOOL.clone(),
                },
                move |args, ns, _tca, rt| Err("unknown eq applied".to_string()),
            )))
        },
    )
}
