use std::any::Any;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug};
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::sync::Arc;
use dyn_clone::DynClone;
use derivative::Derivative;
use crate::XType;
use crate::xvalue::XValue;

pub trait NativeType: Send + Sync + Debug + DynClone {
    fn generic_names(&self) -> Vec<String>;
    fn name(&self) -> &str;
}

dyn_clone::clone_trait_object!(NativeType);

impl PartialEq for dyn NativeType {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name()
    }
}

impl Eq for dyn NativeType {}

pub trait RuntimeEquatable: Any {
    fn _as_any(&self) -> &dyn Any;
}

impl<S: 'static> RuntimeEquatable for S {
    fn _as_any(&self) -> &dyn Any {
        self
    }
}

pub trait XNativeValue: Debug + RuntimeEquatable {
    fn size(&self) -> usize;
}

