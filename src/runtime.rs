use crate::permissions::{Permission, PermissionSet};
use crate::runtime_violation::RuntimeViolation;
use crate::util::lazy_bigint::LazyBigint;
use either::Either;
use std::cell::RefCell;
use std::fmt::Debug;
use std::iter;
use std::mem::size_of;
use std::rc::Rc;
use std::time::{Duration, Instant};
use rand::SeedableRng;

#[derive(Debug, Default)]
pub struct RuntimeLimits {
    // note that all the standard functions already take up about 20K bytes
    pub size_limit: Option<usize>,
    pub depth_limit: Option<usize>,
    pub recursion_limit: Option<usize>,
    pub ud_call_limit: Option<usize>,
    pub maximum_search: Option<usize>,
    pub time_limit: Option<Duration>,
    pub permissions: PermissionSet,
}

impl RuntimeLimits {
    pub fn to_runtime<W, R>(self, output: W) -> RTCell<W, R> {
        Rc::new(RefCell::new(Runtime {
            size: 0,
            stdout: output,
            ud_calls: 0,
            timeout: self.time_limit.map(|ti| Instant::now() + ti),
            rng: None,
            limits: self,
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

    pub fn check_permission(&self, permission: &Permission) -> Result<(), RuntimeViolation> {
        if self.permissions[permission] {
            Ok(())
        } else {
            Err(RuntimeViolation::PermissionError(permission.0))
        }
    }
}

pub struct Runtime<W, R> {
    pub limits: RuntimeLimits,
    pub(crate) size: usize, // this will be zero if the runtime has no size limit
    pub(crate) ud_calls: usize, // this will be zero if the runtime has no us_call limit
    pub stdout: W,
    pub rng: Option<R>,
    pub(crate) timeout: Option<Instant>,
}

pub type RTCell<W, R> = Rc<RefCell<Runtime<W, R>>>;

impl<W, R> Runtime<W, R> {
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

    pub fn size_left(&self) -> usize {
        if let Some(size_limit) = self.limits.size_limit {
            size_limit - self.size
        } else {
            usize::MAX
        }
    }

    pub fn reset_ud_calls(&mut self) {
        self.ud_calls = 0
    }

    pub fn reset_timeout(&mut self) {
        self.timeout = self.limits.time_limit.map(|tl| Instant::now() + tl)
    }

    pub fn check_timeout(&self) -> Result<(), RuntimeViolation> {
        self.timeout
            .map_or(true, |timeout| timeout > Instant::now())
            .then_some(())
            .ok_or(RuntimeViolation::Timeout)
    }
}

impl<W, R: SeedableRng> Runtime<W, R>{
    pub fn get_rng(&mut self)->&mut R{
        self.rng.get_or_insert_with(|| R::from_entropy())
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
