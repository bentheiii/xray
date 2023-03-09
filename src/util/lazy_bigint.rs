use either::Either;
use num_bigint::{BigInt, BigUint, ParseBigIntError};
use num_rational::BigRational;
use num_traits::{FromPrimitive, Inv, Num, One, Pow, Signed, ToPrimitive, Zero};
use std::cmp::Ordering;
use std::convert::{TryFrom, TryInto};
use std::fmt::{Debug, Display, Formatter};
use std::iter;
use std::mem::size_of;
use std::num::{IntErrorKind, ParseIntError};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Rem, Sub};

type SmallInt = i64;

#[derive(Eq, PartialEq, Clone)]
pub enum LazyBigint {
    Short(SmallInt),
    Long(BigInt),
}

impl LazyBigint {
    pub(crate) fn true_div(self, rhs: Self) -> f64 {
        match (self, rhs) {
            (Self::Short(s1), Self::Short(s2)) => s1 as f64 / s2 as f64,
            (Self::Short(s), Self::Long(b)) => {
                BigRational::new(BigInt::from(s), b).to_f64().unwrap()
            }
            (Self::Long(b), Self::Short(s)) => {
                BigRational::new(b, BigInt::from(s)).to_f64().unwrap()
            }
            (Self::Long(b0), Self::Long(b1)) => BigRational::new(b0, b1).to_f64().unwrap(),
        }
    }

    pub(crate) fn first_u64_digit(&self) -> Self {
        Self::from(match self {
            Self::Short(s) => *s as u64,
            Self::Long(b) => b.iter_u64_digits().next().unwrap(),
        })
    }

    // todo remove this function
    pub(crate) fn bits(&self) -> u64 {
        match self {
            Self::Short(_) => 64,
            Self::Long(b) => b.bits(),
        }
    }

    pub(crate) fn additional_size(&self) -> usize {
        match self {
            Self::Short(_) => 0,
            Self::Long(b) => size_of::<BigInt>() + b.iter_u64_digits().count() * 8,
        }
    }

    pub(crate) fn from_str_radix(
        s: &str,
        radix: u32,
    ) -> Result<Self, Either<ParseIntError, ParseBigIntError>> {
        match i128::from_str_radix(s, radix) {
            // shortcut, if std will work we use that
            Ok(v) => Ok(LazyBigint::from(v)),
            Err(e) => match e.kind() {
                IntErrorKind::PosOverflow | IntErrorKind::NegOverflow => Ok(LazyBigint::from(
                    BigInt::from_str_radix(s, radix).map_err(Either::Right)?,
                )),
                _ => Err(Either::Left(e)),
            },
        }
    }

    pub(crate) fn range(&'_ self) -> impl IntoIterator<Item = Self> + '_ {
        iter::successors(Some(Self::zero()), |p| Some(p + &Self::one())).take_while(|i| i < self)
    }
    
    pub(crate) fn sign(&self)->i8{
        if self.is_positive() {
            1
        } else if self.is_negative() {
            -1
        } else {
            0
        }
    }
}

impl Inv for LazyBigint {
    type Output = f64;
    fn inv(self) -> Self::Output {
        match self {
            Self::Short(s) => (s as f64).inv(),
            Self::Long(b) => BigRational::from(b).inv().to_f64().unwrap(),
        }
    }
}

impl<T: TryInto<SmallInt> + Into<BigInt> + Clone> From<T> for LazyBigint {
    fn from(t: T) -> Self {
        t.clone()
            .try_into()
            .ok()
            .map_or_else(|| Self::Long(t.into()), Self::Short)
    }
}

impl Debug for LazyBigint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Short(short) => write!(f, "{short:?}"),
            Self::Long(long) => write!(f, "{long:?}"),
        }
    }
}

impl One for LazyBigint {
    fn one() -> Self {
        Self::Short(1)
    }

    fn is_one(&self) -> bool
    where
        Self: PartialEq,
    {
        matches!(self, Self::Short(1))
    }
}

impl Zero for LazyBigint {
    fn zero() -> Self {
        Self::Short(0)
    }

    fn is_zero(&self) -> bool
    where
        Self: PartialEq,
    {
        matches!(self, Self::Short(0))
    }
}

impl Neg for LazyBigint {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Self::Short(SmallInt::MIN) => Self::Long(assert_is_long(-BigInt::from(SmallInt::MIN))),
            Self::Short(s) => Self::Short(-s),
            Self::Long(b) => Self::from(-b),
        }
    }
}

fn assert_is_long(bi: BigInt) -> BigInt {
    debug_assert!(<&BigInt as TryInto<SmallInt>>::try_into(&bi).is_err());
    bi
}

impl Mul for LazyBigint {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        &self * &rhs
    }
}

impl Mul for &LazyBigint {
    type Output = LazyBigint;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LazyBigint::Short(0), _) | (_, LazyBigint::Short(0)) => LazyBigint::zero(),
            (LazyBigint::Short(s1), LazyBigint::Short(s2)) => s1.checked_mul(*s2).map_or_else(
                || LazyBigint::Long(assert_is_long(BigInt::from(*s1) * s2)),
                LazyBigint::Short,
            ),
            (LazyBigint::Short(s), LazyBigint::Long(b))
            | (LazyBigint::Long(b), LazyBigint::Short(s)) => {
                LazyBigint::Long(assert_is_long(b * s))
            }
            (LazyBigint::Long(b0), LazyBigint::Long(b1)) => {
                LazyBigint::Long(assert_is_long(b0 * b1))
            }
        }
    }
}

impl Add for LazyBigint {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        &self + &rhs
    }
}

impl Add for &LazyBigint {
    type Output = LazyBigint;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LazyBigint::Short(0), a) | (a, LazyBigint::Short(0)) => a.clone(),
            (LazyBigint::Short(s1), LazyBigint::Short(s2)) => s1.checked_add(*s2).map_or_else(
                || LazyBigint::Long(assert_is_long(BigInt::from(*s1) + s2)),
                LazyBigint::Short,
            ),
            (LazyBigint::Short(s), LazyBigint::Long(b))
            | (LazyBigint::Long(b), LazyBigint::Short(s)) => LazyBigint::from(b + s),
            (LazyBigint::Long(b0), LazyBigint::Long(b1)) => LazyBigint::from(b0 + b1),
        }
    }
}

impl Add<usize> for LazyBigint {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        self + Self::from(rhs)
    }
}

impl Add<usize> for &LazyBigint {
    type Output = LazyBigint;

    fn add(self, rhs: usize) -> Self::Output {
        self + &LazyBigint::from(rhs)
    }
}

impl Sub for LazyBigint {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        &self - &rhs
    }
}

impl Sub for &LazyBigint {
    type Output = LazyBigint;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (a, LazyBigint::Short(0)) => a.clone(),
            (LazyBigint::Short(s1), LazyBigint::Short(s2)) => s1.checked_sub(*s2).map_or_else(
                || LazyBigint::Long(assert_is_long(BigInt::from(*s1) - s2)),
                LazyBigint::Short,
            ),
            (LazyBigint::Short(s), LazyBigint::Long(b))
            | (LazyBigint::Long(b), LazyBigint::Short(s)) => LazyBigint::from(b - s),
            (LazyBigint::Long(b0), LazyBigint::Long(b1)) => LazyBigint::from(b0 - b1),
        }
    }
}

impl Rem for LazyBigint {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (_, Self::Short(0)) => panic!("modulo by 0"),
            (Self::Short(0), _) => Self::zero(),
            (_, Self::Short(-1 | 1)) => Self::zero(),
            (Self::Short(s1), Self::Short(s2)) => Self::Short(s1 % s2),
            (Self::Long(b), Self::Short(s)) => Self::from(b % s),
            (Self::Short(s), Self::Long(b)) => Self::from(s % b),
            (Self::Long(b0), Self::Long(b1)) => Self::from(b0 % b1),
        }
    }
}

impl Div for LazyBigint {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Short(s1), Self::Short(s2)) => Self::Short(s1 / s2),
            (Self::Short(s), Self::Long(b)) => Self::from(s / b),
            (Self::Long(b), Self::Short(s)) => Self::from(b / s),
            (Self::Long(b0), Self::Long(b1)) => Self::from(b0 / b1),
        }
    }
}

impl BitAnd for LazyBigint {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Short(s1), Self::Short(s2)) => Self::Short(s1 & s2),
            (Self::Short(s), Self::Long(b)) | (Self::Long(b), Self::Short(s)) => {
                Self::from(b & BigInt::from(s))
            }
            (Self::Long(b0), Self::Long(b1)) => Self::from(b0 & b1),
        }
    }
}

impl BitOr for LazyBigint {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Short(s1), Self::Short(s2)) => Self::Short(s1 | s2),
            (Self::Short(s), Self::Long(b)) | (Self::Long(b), Self::Short(s)) => {
                Self::from(b | BigInt::from(s))
            }
            (Self::Long(b0), Self::Long(b1)) => Self::from(b0 | b1),
        }
    }
}

impl BitXor for LazyBigint {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Short(s1), Self::Short(s2)) => Self::Short(s1 ^ s2),
            (Self::Short(s), Self::Long(b)) | (Self::Long(b), Self::Short(s)) => {
                Self::from(b ^ BigInt::from(s))
            }
            (Self::Long(b0), Self::Long(b1)) => Self::from(b0 ^ b1),
        }
    }
}

impl FromPrimitive for LazyBigint {
    fn from_isize(n: isize) -> Option<Self> {
        Some(Self::from(n))
    }

    fn from_i64(n: i64) -> Option<Self> {
        Some(Self::from(n))
    }

    fn from_u64(n: u64) -> Option<Self> {
        Some(Self::from(n))
    }

    fn from_f64(n: f64) -> Option<Self> {
        n.fract().is_zero().then(|| match SmallInt::from_f64(n) {
            None => Self::Long(assert_is_long(BigInt::from_f64(n).unwrap())),
            Some(s) => Self::Short(s),
        })
    }
}

impl Pow<Self> for LazyBigint {
    type Output = Self;

    fn pow(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Short(s1), Self::Short(s2)) => {
                s1.checked_pow(s2.try_into().unwrap()).map_or_else(
                    || Self::Long(BigInt::from(s1).pow(BigUint::try_from(s2).unwrap())),
                    Self::Short,
                )
            }
            (Self::Short(s), Self::Long(b)) => {
                Self::Long(BigInt::from(s).pow(BigUint::try_from(b).unwrap()))
            }
            (Self::Long(b), Self::Short(s)) => Self::Long(b.pow(BigUint::try_from(s).unwrap())),
            (Self::Long(b0), Self::Long(b1)) => Self::Long(b0.pow(BigUint::try_from(b1).unwrap())),
        }
    }
}

impl Num for LazyBigint {
    type FromStrRadixErr = <BigInt as Num>::FromStrRadixErr;

    fn from_str_radix(str: &str, radix: u32) -> Result<Self, Self::FromStrRadixErr> {
        BigInt::from_str_radix(str, radix).map(Self::from)
    }
}

impl Signed for LazyBigint {
    fn abs(&self) -> Self {
        match self {
            Self::Short(SmallInt::MIN) => Self::Long(-BigInt::from(SmallInt::MIN)),
            Self::Short(a) => Self::Short(a.abs()),
            Self::Long(a) => Self::Long(assert_is_long(a.abs())),
        }
    }

    fn abs_sub(&self, other: &Self) -> Self {
        (self.clone() - other.clone()).abs()
    }

    fn signum(&self) -> Self {
        match self {
            Self::Short(a) => Self::Short(a.signum()),
            Self::Long(a) => Self::Short(a.signum().try_into().unwrap()),
        }
    }

    fn is_positive(&self) -> bool {
        match self {
            Self::Short(a) => a.is_positive(),
            Self::Long(a) => a.is_positive(),
        }
    }

    fn is_negative(&self) -> bool {
        match self {
            Self::Short(a) => a.is_negative(),
            Self::Long(a) => a.is_negative(),
        }
    }
}

impl ToPrimitive for LazyBigint {
    fn to_i64(&self) -> Option<i64> {
        match self {
            Self::Short(s) => s.to_i64(),
            Self::Long(b) => b.to_i64(),
        }
    }

    fn to_u64(&self) -> Option<u64> {
        match self {
            Self::Short(s) => s.to_u64(),
            Self::Long(b) => b.to_u64(),
        }
    }

    fn to_f64(&self) -> Option<f64> {
        match self {
            Self::Short(s) => s.to_f64(),
            Self::Long(b) => b.to_f64(),
        }
    }
}

impl PartialOrd for LazyBigint {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for LazyBigint {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::Short(s0), Self::Short(s1)) => s0.cmp(s1),
            (Self::Long(b0), Self::Long(b1)) => b0.cmp(b1),
            (Self::Short(_), Self::Long(b)) => {
                if b.is_positive() {
                    Ordering::Less
                } else {
                    Ordering::Greater
                }
            }
            (Self::Long(b), Self::Short(_)) => {
                if b.is_positive() {
                    Ordering::Greater
                } else {
                    Ordering::Less
                }
            }
        }
    }
}

impl Display for LazyBigint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Short(s) => write!(f, "{s}"),
            Self::Long(b) => write!(f, "{b}"),
        }
    }
}
