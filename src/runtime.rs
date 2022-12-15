use crate::runtime_violation::RuntimeViolation;
use crate::util::lazy_bigint::LazyBigint;
use std::cell::RefCell;
use std::fmt::Debug;
use std::io::Write;
use std::mem::size_of;
use std::rc::Rc;

#[derive(Debug, Default)]
pub struct RuntimeLimits {
    pub size_limit: Option<usize>,
    pub depth_limit: Option<usize>,
    pub recursion_limit: Option<usize>,
}

impl RuntimeLimits {
    pub fn to_runtime<W: Write + 'static>(self, output: W) -> RTCell<W> {
        Rc::new(RefCell::new(Runtime {
            limits: self,
            size: 0,
            stdout: output,
        }))
    }
}

pub struct Runtime<W: Write + 'static> {
    pub(crate) limits: RuntimeLimits,
    pub(crate) size: usize, // this will be zero if the runtime has no size limit
    pub stdout: W,
}

pub type RTCell<W> = Rc<RefCell<Runtime<W>>>;

impl<W: Write + 'static> Runtime<W> {
    pub fn can_allocate(&self, new_size: usize) -> Result<(), RuntimeViolation> {
        self.can_allocate_by(|| Some(new_size))
    }

    pub fn can_allocate_by(&self, f: impl Fn() -> Option<usize>) -> Result<(), RuntimeViolation> {
        if let Some(size_limit) = self.limits.size_limit {
            if f().map_or(true, |size| {
                self.size + size * size_of::<usize>() >= size_limit
            }) {
                return Err(RuntimeViolation::AllocationLimitReached);
            }
        }
        Ok(())
    }

    pub fn can_afford(&self, x: &impl ProspectiveSize) -> Result<(), RuntimeViolation> {
        self.can_allocate_by(|| Some(x.prospective_size()))
    }

    pub fn vec<T>(&self, capacity: usize) -> Result<Vec<T>, RuntimeViolation> {
        self.can_allocate(capacity)
            .map(|_| Vec::with_capacity(capacity))
    }
}

pub trait ProspectiveSize {
    fn prospective_size(&self) -> usize;
}

impl<T> ProspectiveSize for Vec<T> {
    fn prospective_size(&self) -> usize {
        self.len()
    }
}

impl ProspectiveSize for LazyBigint {
    fn prospective_size(&self) -> usize {
        (self.bits() / 8) as usize
    }
}
