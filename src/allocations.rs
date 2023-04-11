use crate::{units::AllocatedMemory, xvalue::XValue};

pub(crate) trait Allocateable {
    fn byte_size(&self) -> AllocatedMemory;
}

impl<W, R, T> Allocateable for XValue<W, R, T> {
    fn byte_size(&self) -> AllocatedMemory {
        self.size().into()
    }
}

impl Allocateable for String {
    fn byte_size(&self) -> AllocatedMemory {
        self.len().into()
    }
}
