use std::fmt::Display;

use derive_more::{Add, AddAssign, From, Into, Sub, SubAssign};
use num_traits::Zero;

#[derive(
    Eq,
    PartialEq,
    Debug,
    Ord,
    PartialOrd,
    Copy,
    Clone,
    Hash,
    Add,
    Sub,
    From,
    Into,
    AddAssign,
    SubAssign,
)]
pub(crate) struct ScopeDepth(pub(crate) usize);

#[derive(
    Eq,
    PartialEq,
    Debug,
    Ord,
    PartialOrd,
    Copy,
    Clone,
    Hash,
    Add,
    Sub,
    From,
    Into,
    AddAssign,
    SubAssign,
)]
pub(crate) struct StackDepth(pub(crate) usize);

#[derive(
    Eq,
    PartialEq,
    Debug,
    Ord,
    PartialOrd,
    Copy,
    Clone,
    Hash,
    Add,
    Sub,
    From,
    Into,
    AddAssign,
    SubAssign,
)]
pub(crate) struct AllocatedMemory(pub(crate) usize);

impl Display for AllocatedMemory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} bytes", self.0)
    }
}

macro_rules! arith {
    ($t:ty) => {
        impl Zero for $t {
            fn is_zero(&self) -> bool {
                self.0.is_zero()
            }

            fn zero() -> Self {
                Self(Zero::zero())
            }
        }
    };
}

arith!(ScopeDepth);
arith!(StackDepth);
arith!(AllocatedMemory);
