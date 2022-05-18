use std::fmt::{Debug, Display, Formatter};
use std::sync::{Arc, Weak};

pub enum MRef<T> {
    Weak(Weak<T>),
    Strong(Arc<T>),
}

impl<T> Clone for MRef<T> {
    fn clone(&self) -> Self {
        match self {
            MRef::Weak(weak) => MRef::Weak(weak.clone()),
            MRef::Strong(strong) => MRef::Strong(strong.clone()),
        }
    }
}

impl<T: Debug> Debug for MRef<T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Debug::fmt(self.to_arc().as_ref(), f)
    }
}

impl<T: Display> Display for MRef<T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Display::fmt(self.to_arc().as_ref(), f)
    }
}

impl<T: PartialEq> PartialEq for MRef<T> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (MRef::Weak(a), MRef::Weak(b)) => a.upgrade().unwrap().as_ref() == b.upgrade().unwrap().as_ref(),
            (MRef::Strong(a), MRef::Strong(b)) => a.as_ref() == b.as_ref(),
            (MRef::Strong(a), MRef::Weak(b)) => a.as_ref() == b.upgrade().unwrap().as_ref(),
            (MRef::Weak(_), MRef::Strong(_)) => other == self,
            _ => false,
        }
    }
}

impl<T: PartialEq> Eq for MRef<T> {}

impl<T> From<T> for MRef<T> {
    fn from(t: T) -> Self<> {
        Self::Strong(Arc::new(t))
    }
}

impl<T> MRef<T>{
    pub fn to_arc(&self) -> Arc<T> {
        match self {
            Self::Strong(t) => t.clone(),
            Self::Weak(t) => t.upgrade().unwrap(),
        }
    }
}