use dyn_clone::DynClone;
use std::any::Any;
use std::fmt::Debug;
use std::mem::size_of;

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
    fn static_size(&self) -> usize;
}

impl<S: 'static> RuntimeEquatable for S {
    fn _as_any(&self) -> &dyn Any {
        self
    }
    fn static_size(&self) -> usize {
        size_of::<Self>()
    }
}

pub trait XNativeValue: Debug + RuntimeEquatable {
    fn dyn_size(&self) -> usize;

    fn full_size(&self)->usize{
        self.static_size() + self.dyn_size()
    }
}