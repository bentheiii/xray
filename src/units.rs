use std::ops::{Add, Sub};
use num_traits::Zero;

#[derive(Eq, PartialEq, Debug, Ord, PartialOrd, Copy, Clone)]
pub(crate) struct ScopeDepth(pub usize);

#[derive(Eq, PartialEq, Debug, Ord, PartialOrd, Copy, Clone)]
pub(crate) struct StackDepth(pub usize);

macro_rules! arith {
    ($t:ty) => {
        impl Add<Self> for $t{
            type Output = Self;

            fn add(self, rhs: Self) -> Self::Output {
                Self(self.0 + rhs.0)
            }
        }

        impl Add<usize> for $t{
            type Output = Self;

            fn add(self, rhs: usize) -> Self::Output {
                Self(self.0 + rhs)
            }
        }

        impl Sub<Self> for $t{
            type Output = Self;

            fn sub(self, rhs: Self) -> Self::Output {
                Self(self.0 - rhs.0)
            }
        }

        impl Sub<usize> for $t{
            type Output = Self;

            fn sub(self, rhs: usize) -> Self::Output {
                Self(self.0 - rhs)
            }
        }

        impl Zero for $t{
            fn is_zero(&self) -> bool {
                self.0.is_zero()
            }

            fn zero() -> Self {
                Self(Zero::zero())
            }
        }
    }
}


arith!(ScopeDepth);
arith!(StackDepth);