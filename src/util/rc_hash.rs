use std::hash::{Hash, Hasher};
use std::rc::Rc;

pub(crate) struct RcHash<T>(pub Rc<T>);

impl<T> PartialEq for RcHash<T> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<T> Eq for RcHash<T> {}

impl<T> Hash for RcHash<T> {
    fn hash<H>(&self, hasher: &mut H)
    where
        H: Hasher,
    {
        hasher.write_usize(Rc::as_ptr(&self.0) as usize);
    }
}
