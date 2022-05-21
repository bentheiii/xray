use std::collections::HashMap;
use std::sync::{Arc, Weak};
use crate::{Bind, TRef, XCallableSpec, XStructFieldSpec, XStructSpec, XType};
use crate::mref::MRef;
use crate::xscope::Identifier;

pub enum XTypeSkeleton {
    Known(TRef, Vec<XTypeSkeleton>),
    Signature(Vec<XTypeSkeleton>, Box<XTypeSkeleton>),
    Tail(HashMap<Identifier, XTypeSkeleton>),
}

pub type FnFromSpec = fn(MRef<XStructSpec>, Bind) -> XType;

impl XTypeSkeleton {
    pub fn from_known(base: TRef, gen_args: Vec<XTypeSkeleton>) -> Result<XTypeSkeleton, String> {
        if base.gen_param_count() != gen_args.len() {
            return Err(format!("type {:?} Expected {} generic arguments, got {}", base, base.gen_param_count(), gen_args.len()));
        }
        Ok(XTypeSkeleton::Known(base, gen_args))
    }

    fn build(&self, weak_tref: &TRef, weak_spec: &MRef<XStructSpec>, fn_from_spec: FnFromSpec) -> TRef {
        match self {
            XTypeSkeleton::Known(tref, bind) => {
                let bind = bind.iter().map(|b| b.build(weak_tref, weak_spec, fn_from_spec)).collect();
                tref.bind(bind).unwrap()
            }
            XTypeSkeleton::Tail(bind) => {
                if bind.is_empty() {
                    weak_tref.clone()
                } else {
                    let bind = Bind::from_iter(bind.iter().map(|(id, b)| (id.clone(), b.build(weak_tref, weak_spec, fn_from_spec))));
                    TRef::from(fn_from_spec(weak_spec.clone(), bind))
                }
            }
            XTypeSkeleton::Signature(gen_args, ret) => {
                let gen_args = gen_args.iter().map(|g| g.build(weak_tref, weak_spec, fn_from_spec)).collect();
                let ret = ret.build(weak_tref, weak_spec, fn_from_spec);
                TRef::from(XType::XCallable(XCallableSpec {
                    param_types: gen_args,
                    return_type: ret,
                }))
            }
        }
    }
}

pub type FieldsSkeleton = Vec<(String, XTypeSkeleton)>;

pub fn build_from_skeleton(fskel: FieldsSkeleton, name: Identifier, generic_names: Vec<Identifier>, fn_from_spec: FnFromSpec) -> MRef<XStructSpec> {
    println!("!!! A.0 {:?}", generic_names);
    let strong_spec = Arc::new_cyclic(|spec| {
        let weak_spec = MRef::Weak(spec.clone());
        let strong_tref = Arc::new_cyclic(|tref| {
            fn_from_spec(weak_spec.clone(), Bind::new())
        });
        let tref = TRef::Strong(strong_tref);
        XStructSpec::new(name, generic_names,
                         fskel.iter().map(|(name, skel)| {
                             XStructFieldSpec {
                                 name: name.clone(),
                                 type_: skel.build(&tref, &weak_spec, fn_from_spec),
                             }
                         }).collect())
    });
    println!("!!! A.1");
    MRef::Strong(strong_spec)
}