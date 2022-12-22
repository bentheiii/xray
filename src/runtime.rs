use crate::runtime_violation::RuntimeViolation;
use crate::util::lazy_bigint::LazyBigint;
use either::Either;
use std::cell::RefCell;
use std::fmt::Debug;
use std::io::Write;
use std::iter;
use std::mem::size_of;
use std::rc::Rc;

#[derive(Debug, Default)]
pub struct RuntimeLimits {
    pub size_limit: Option<usize>,
    pub depth_limit: Option<usize>,
    pub recursion_limit: Option<usize>,
    pub ud_call_limit: Option<usize>,
    // note that all the standard functions already take upa bout 5K bytes
    pub maximum_search: Option<usize>,
}

impl RuntimeLimits {
    pub fn to_runtime<W: Write + 'static>(self, output: W) -> RTCell<W> {
        Rc::new(RefCell::new(Runtime {
            limits: self,
            size: 0,
            stdout: output,
            ud_calls: 0,
        }))
    }

    pub fn search_iter(&self) -> impl Iterator<Item = Result<(), RuntimeViolation>> + 'static {
        self.maximum_search.map_or_else(
            || Either::Left(iter::repeat_with(|| Ok(()))),
            |maximum_search| {
                Either::Right(
                    iter::repeat_with(|| Ok(()))
                        .take(maximum_search)
                        .chain(iter::once(Err(RuntimeViolation::MaximumSearch))),
                )
            },
        )
    }
}

pub struct Runtime<W: Write + 'static> {
    pub limits: RuntimeLimits,
    pub(crate) size: usize, // this will be zero if the runtime has no size limit
    pub(crate) ud_calls: usize, // this will be zero if the runtime has no us_call limit
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
