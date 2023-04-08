use either::Either;
use xray::time_provider::{TimeProvider, SystemTimeProvider};

struct ConstTimeProvider(f64);

impl TimeProvider for ConstTimeProvider{
    fn unix_now(&self)->f64 {
        self.0
    }
}

pub fn mk_time_provider(v: Option<f64>)->impl TimeProvider{
    if let Some(v) = v{
        Either::Left(ConstTimeProvider(v))
    } else {
        Either::Right(SystemTimeProvider)
    }
}