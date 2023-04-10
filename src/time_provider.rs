use std::time::SystemTime;

use either::{for_both, Either};

pub trait TimeProvider {
    fn unix_now(&self) -> f64;
}

pub struct SystemTimeProvider;

impl TimeProvider for SystemTimeProvider {
    fn unix_now(&self) -> f64 {
        SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_secs_f64()
    }
}

#[cfg(feature = "either")]
impl<T0: TimeProvider, T1: TimeProvider> TimeProvider for Either<T0, T1> {
    fn unix_now(&self) -> f64 {
        for_both!(self, i=>i.unix_now())
    }
}
