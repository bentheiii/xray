use crate::allocations::Allocateable;
use crate::permissions::{Permission, PermissionSet};
use crate::root_runtime_scope::RuntimeResult;
use crate::runtime_violation::RuntimeViolation;
use crate::time_provider::SystemTimeProvider;
use crate::units::AllocatedMemory;
use crate::util::lazy_bigint::LazyBigint;
use either::Either;
use num_traits::Zero;
use rand::rngs::StdRng;
use rand::SeedableRng;
use std::cell::RefCell;
use std::fmt::Debug;
use std::io::Stdout;
use std::iter;
use std::marker::PhantomData;
use std::mem::size_of;
use std::rc::Rc;
use std::time::{Duration, Instant};

const VERBOSE_ALLOC: bool = false;

#[derive(Debug, Default)]
pub struct RuntimeLimits {
    // note that all the standard functions already take up about 40K bytes overhead
    pub size_limit: Option<usize>,
    pub depth_limit: Option<usize>,
    pub recursion_limit: Option<usize>,
    pub ud_call_limit: Option<usize>,
    pub maximum_search: Option<usize>,
    pub time_limit: Option<Duration>,
    pub permissions: PermissionSet,
}

impl RuntimeLimits {
    pub fn to_runtime<W, R, T>(self, output: W, time_provider: T) -> RTCell<W, R, T> {
        Rc::new(Runtime {
            stats: RefCell::new(RuntimeStats::new(output, self.time_limit)),
            time_provider,
            limits: self,
        })
    }

    pub fn search_iter(&self) -> impl Iterator<Item = RuntimeResult<()>> + 'static {
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

    pub fn check_permission(&self, permission: &Permission) -> RuntimeResult<()> {
        if self.permissions.get(permission) {
            Ok(())
        } else {
            Err(RuntimeViolation::PermissionError(permission.id))
        }
    }
}

#[derive(Debug)]
pub struct RuntimeStats<W, R, T> {
    pub(crate) size: AllocatedMemory, // this will be zero if the runtime has no size limit
    pub(crate) ud_calls: usize,       // this will be zero if the runtime has no us_call limit
    pub(crate) timeout: Option<Instant>,
    pub(crate) rng: Option<R>,
    pub stdout: W,

    _t: PhantomData<T>,
}

impl<W, R, T> RuntimeStats<W, R, T> {
    fn new(stdout: W, time_limit: Option<Duration>) -> Self {
        let mut ret = Self {
            size: Zero::zero(),
            ud_calls: 0,
            timeout: None,
            rng: None,
            stdout,
            _t: PhantomData,
        };
        ret.reset_timeout(time_limit);
        ret
    }

    fn reset_timeout(&mut self, time_limit: Option<Duration>) {
        self.timeout = time_limit.map(|ti| Instant::now() + ti)
    }
}

impl<W, R: SeedableRng, T> RuntimeStats<W, R, T> {
    pub(crate) fn get_rng(&mut self) -> &mut R {
        self.rng.get_or_insert_with(|| R::from_entropy())
    }
}

pub struct Runtime<W, R, T> {
    pub limits: RuntimeLimits,
    pub stats: RefCell<RuntimeStats<W, R, T>>,
    pub time_provider: T,
}

pub type RTCell<W = Stdout, R = StdRng, T = SystemTimeProvider> = Rc<Runtime<W, R, T>>;

impl<W, R, T> Runtime<W, R, T> {
    pub fn can_allocate(&self, new_size: usize) -> RuntimeResult<()> {
        self.can_allocate_by(|| Some(new_size))
    }

    pub fn can_allocate_by(&self, f: impl Fn() -> Option<usize>) -> RuntimeResult<()> {
        if let Some(size_limit) = self.limits.size_limit {
            if let Some(size) = f() {
                let stat = self.stats.borrow();
                if usize::from(stat.size) + size > size_limit {
                    return Err(RuntimeViolation::AllocationLimitReached);
                }
            }
        }
        Ok(())
    }

    pub fn can_afford(&self, x: &impl ProspectiveSize) -> RuntimeResult<()> {
        self.can_allocate_by(|| Some(x.prospective_size()))
    }

    pub(crate) fn size_left(&self) -> usize {
        if let Some(size_limit) = self.limits.size_limit {
            size_limit - usize::from(self.stats.borrow().size)
        } else {
            usize::MAX
        }
    }

    pub fn reset_ud_calls(&self) {
        self.stats.borrow_mut().ud_calls = 0
    }

    pub fn reset_timeout(&self) {
        self.stats
            .borrow_mut()
            .reset_timeout(self.limits.time_limit)
    }

    pub fn check_timeout(&self) -> RuntimeResult<()> {
        self.stats
            .borrow()
            .timeout
            .map_or(true, |timeout| timeout > Instant::now())
            .then_some(())
            .ok_or(RuntimeViolation::Timeout)
    }

    pub(crate) fn increment_call_limit(&self) -> RuntimeResult<()> {
        if let Some(ud_limit) = self.limits.ud_call_limit {
            let mut stats = self.stats.borrow_mut();
            stats.ud_calls += 1;
            if stats.ud_calls >= ud_limit {
                return Err(RuntimeViolation::MaximumUDCall);
            }
        }
        Ok(())
    }

    pub fn reset_call_limit(&self) {
        self.stats.borrow_mut().ud_calls = 0
    }

    pub(crate) fn allocate<A: Allocateable + Debug>(
        &self,
        value: &A,
    ) -> RuntimeResult<AllocatedMemory> {
        if let Some(max_size) = self.limits.size_limit {
            let size = value.byte_size();
            let mut stats = self.stats.borrow_mut();
            stats.size += size;
            if VERBOSE_ALLOC {
                println!(
                    "Allocated {size} bytes (total {}) for {value:?}",
                    stats.size
                );
            }
            if usize::from(stats.size) > max_size {
                Err(RuntimeViolation::AllocationLimitReached)
            } else {
                Ok(size)
            }
        } else {
            Ok(0.into())
        }
    }

    pub(crate) fn deallocate(&self, size: AllocatedMemory) {
        if !size.is_zero() {
            self.stats.borrow_mut().size -= size
        }
    }
}

pub trait ProspectiveSize {
    fn prospective_size(&self) -> usize;
}

impl<T> ProspectiveSize for Vec<T> {
    fn prospective_size(&self) -> usize {
        self.len() * size_of::<usize>()
    }
}

impl ProspectiveSize for LazyBigint {
    fn prospective_size(&self) -> usize {
        (self.bits() / 8) as usize
    }
}
