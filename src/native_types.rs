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
    fn name(&self) -> String;
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
    fn _equals(&self, _: &dyn RuntimeEquatable) -> bool;
}

impl<S: 'static + PartialEq> RuntimeEquatable for S {
    fn _as_any(&self) -> &dyn Any {
        self
    }

    fn _equals(&self, other: &dyn RuntimeEquatable) -> bool {
        // Do a type-safe casting. If the types are different,
        // return false, otherwise test the values for equality.
        other
            ._as_any()
            .downcast_ref::<S>()
            .map_or(false, |a| self == a)
    }
}

pub trait XNativeValue: Debug + RuntimeEquatable {}

impl PartialEq for dyn XNativeValue {
    fn eq(&self, that: &dyn XNativeValue) -> bool {
        self._equals(that)
    }
}

impl PartialEq<&Self> for Box<dyn XNativeValue> {
    fn eq(&self, that: &&Self) -> bool {
        self._equals(that.as_ref())
    }
}

impl Eq for dyn XNativeValue {}

impl Hash for dyn XNativeValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // todo improve?
        0.hash(state);
    }
}

#[derive(Debug, Clone)]
pub struct XSequenceType {}

impl XSequenceType {
    pub fn xtype(t: Arc<XType>) -> Arc<XType> {
        Arc::new(XType::XNative(Box::new(Self {}),
                                HashMap::from([('T'.to_string(), t)])))
    }
}

impl NativeType for XSequenceType {
    fn generic_names(&self) -> Vec<String> {
        vec!["T".to_string()]
    }
    fn name(&self) -> String { "Sequence".to_string() }
}

#[derive(Debug, Clone)]
pub struct XSetType {}

impl XSetType {
    pub fn xtype(t: Arc<XType>) -> Arc<XType> {
        Arc::new(XType::XNative(Box::new(Self {}),
                                HashMap::from([('T'.to_string(), t)])))
    }
}

impl NativeType for XSetType {
    fn generic_names(&self) -> Vec<String> {
        vec!["T".to_string()]
    }
    fn name(&self) -> String { "Set".to_string() }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct XSequence {
    pub value: Vec<Rc<XValue>>,
}

impl XSequence {
    pub fn new(value: Vec<Rc<XValue>>) -> Self {
        Self { value }
    }
}

impl XNativeValue for XSequence {}

#[derive(Debug, Eq, PartialEq, Derivative)]
#[derivative(Hash)]
pub struct XSet {
    #[derivative(Hash = "ignore")]
    pub value: HashSet<Rc<XValue>>,
}

impl XSet {
    pub fn new(value: Vec<Rc<XValue>>) -> Self {
        Self { value: value.into_iter().collect() }
    }
}

impl XNativeValue for XSet {}