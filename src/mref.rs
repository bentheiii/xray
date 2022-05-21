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
        match self {
            MRef::Weak(..) => write!(f, "Weak(...)"),
            MRef::Strong(strong) => Debug::fmt(strong, f),
        }
    }
}

impl<T: Display> Display for MRef<T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            MRef::Weak(..) => write!(f, "Weak(...)"),
            MRef::Strong(strong) => Display::fmt(strong, f),
        }
    }
}

impl<T: PartialEq+Debug> PartialEq for MRef<T> {
    fn eq(&self, other: &Self) -> bool {
        println!("!!! EQ.0 {:?} {:?}", self, other);
        match (self, other) {
            (MRef::Weak(a), MRef::Weak(b)) => a.ptr_eq(b),
            (MRef::Strong(a), MRef::Strong(b)) => a.as_ref() == b.as_ref(),
            (MRef::Strong(a), MRef::Weak(b)) => a.as_ref() == b.upgrade().unwrap().as_ref(),
            (MRef::Weak(_), MRef::Strong(_)) => other == self,
            _ => false,
        }
    }
}

impl<T: PartialEq+Debug> Eq for MRef<T> {}

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