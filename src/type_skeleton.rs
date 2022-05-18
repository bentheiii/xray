use std::sync::{Arc, Weak};
use crate::{Bind, TRef, XStructFieldSpec, XStructSpec, XType};
use crate::mref::MRef;
use crate::xscope::Identifier;

pub enum XTypeSkeleton {
    Known(TRef, Vec<XTypeSkeleton>),
    Tail(Bind),
}

type FnFromSpec = fn(MRef<XStructSpec>, Bind) -> XType;

impl XTypeSkeleton {
    fn from_known(base: TRef, gen_args: Vec<XTypeSkeleton>) -> Result<XTypeSkeleton, String> {
        if base.gen_param_count() != gen_args.len() {
            return Err(format!("type {:?} Expected {} generic arguments, got {}", base, base.gen_param_count(), gen_args.len()));
        }
        Ok(XTypeSkeleton::Known(base, gen_args))
    }

    fn build(self, weak_tref: &TRef, weak_spec: &MRef<XStructSpec>, fn_from_spec: FnFromSpec) -> TRef {
        match self {
            XTypeSkeleton::Known(tref, bind) => {
                let bind = bind.iter().map(|b| b.build(weak_tref, weak_spec, fn_from_spec)).collect();
                tref.bind(bind).unwrap()
            }
            XTypeSkeleton::Tail(bind) => {
                if bind.is_empty() {
                    weak_tref.clone()
                } else {
                    TRef::from(fn_from_spec(weak_spec.clone(), bind))
                }
            }
        }
    }
}

pub type FieldsSkeleton = Vec<(String, XTypeSkeleton)>;

fn build(fskel: FieldsSkeleton, name: Identifier, generic_names: Vec<Identifier>, fn_from_spec: FnFromSpec) -> TRef {
    let strong_tref = Arc::new_cyclic(|tref| {
        let weak_tref = TRef::Weak(tref.clone());
        let strong_spec = Arc::new_cyclic(|spec| {
            let weak_spec = MRef::Weak(spec.clone());
            XStructSpec::new(name, generic_names,
                             fskel.iter().map(|(name, skel)| {
                                 XStructFieldSpec {
                                     name: name.clone(),
                                     type_: skel.build(&weak_tref, &weak_spec, fn_from_spec),
                                 }
                             }).collect())
        });
        let spec = MRef::Strong(strong_spec);
        fn_from_spec(spec, Bind::new())
    });
    TRef::Strong(strong_tref)
}