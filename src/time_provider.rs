use std::time::SystemTime;

pub trait TimeProvider{
    fn unix_now(&self)->f64;
}

pub struct SystemTimeProvider;

impl TimeProvider for SystemTimeProvider{
    fn unix_now(&self)->f64 {
        SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_secs_f64()
    }
}