use crate::builtin::core::{eval, xerr};
use crate::native_types::{NativeType, XNativeValue};
use crate::util::lazy_bigint::LazyBigint;
use crate::xtype::{XFuncSpec, X_FLOAT, X_INT};
use crate::xvalue::{ManagedXError, ManagedXValue, XResult, XValue};
use crate::{
    manage_native, to_native, to_primitive, xraise, CompilationError, RootCompilationScope,
    XStaticFunction, XType,
};
use num_traits::{Bounded, Float, Num, Signed, ToPrimitive};
use statrs::distribution::{
    Binomial, Discrete, DiscreteCDF, DiscreteUniform, Hypergeometric, NegativeBinomial, Poisson,
};
use statrs::statistics::{DiscreteDistribution, Distribution, Max, Min};
use std::fmt::Debug;

use crate::builtin::builtin_permissions;
use crate::builtin::sequence::{XSequence, XSequenceType};
use crate::util::means::Means;
use itertools::Itertools;
use num_traits::FromPrimitive;
use rand::distributions::{Distribution as _, Standard};
use rand::{Rng, RngCore, SeedableRng};
use std::sync::Arc;

fn inverse_cdf<K: Bounded + Clone + Num + Debug, T: Float>(s: &impl DiscreteCDF<K, T>, p: T) -> K {
    if p == T::zero() {
        return s.min();
    };
    if p == T::one() {
        return s.max();
    };
    let two = K::one() + K::one();
    let mut high = two.clone();
    let mut low = K::min_value();
    while s.cdf(high.clone()) < p {
        high = high.clone() + high.clone();
    }
    while high != low {
        let mid = (high.clone() + low.clone()) / two.clone();
        if s.cdf(mid.clone()) >= p {
            high = mid;
        } else if low == mid {
            return mid;
        } else {
            low = mid;
        }
    }
    high
}

#[derive(Debug, Clone)]
pub(crate) struct XDiscreteDistributionType;

impl NativeType for XDiscreteDistributionType {
    fn generic_names(&self) -> Vec<String> {
        vec![]
    }
    fn name(&self) -> &str {
        "DiscreteDistribution"
    }
}

lazy_static! {
    static ref X_DISCDIST: Arc<XType> =
        Arc::new(XType::XNative(Box::new(XDiscreteDistributionType), vec![]));
}

#[derive(Debug)]
pub(crate) enum XDiscreteDistribution {
    Binomial(Binomial),
    Custom(Vec<(LazyBigint, f64)>),
    Hypergeometric(Hypergeometric),
    NegativeBinomial(NegativeBinomial),
    Poisson(Poisson),
    Uniform(DiscreteUniform),
}

fn big_int_cdf<'a, K: Bounded + Clone + Num, T: Float>(
    c: &impl DiscreteCDF<K, T>,
    x: &'a LazyBigint,
) -> T
where
    K: TryFrom<&'a LazyBigint>,
{
    let Ok(x) = x.try_into() else {
        return if x.is_negative() { T::zero() } else { T::one() };
    };
    c.cdf(x)
}

fn big_int_pdf<'a, K: Bounded + Clone + Num, T: Float>(
    c: &impl Discrete<K, T>,
    x: &'a LazyBigint,
) -> T
where
    K: TryFrom<&'a LazyBigint>,
{
    let Ok(x) = x.try_into() else {
        return T::zero();
    };
    c.pmf(x)
}

impl XDiscreteDistribution {
    fn cdf(&self, x: &LazyBigint) -> f64 {
        match self {
            Self::Binomial(i) => big_int_cdf(i, x),
            Self::Custom(items) => {
                let idx = items.partition_point(|(i, _)| i <= x);
                if idx == 0 {
                    0.0
                } else {
                    items[idx - 1].1
                }
            }
            Self::Hypergeometric(i) => big_int_cdf(i, x),
            Self::NegativeBinomial(i) => big_int_cdf(i, x),
            Self::Poisson(i) => big_int_cdf(i, x),
            Self::Uniform(i) => big_int_cdf(i, x),
        }
    }

    fn pmf(&self, x: &LazyBigint) -> f64 {
        match self {
            Self::Binomial(i) => big_int_pdf(i, x),
            Self::Custom(items) => match items.binary_search_by(|(i, _)| i.cmp(x)) {
                Ok(0) | Err(..) => 0.0,
                Ok(idx) => items[idx].1 - items[idx - 1].1,
            },
            Self::Hypergeometric(i) => big_int_pdf(i, x),
            Self::NegativeBinomial(i) => big_int_pdf(i, x),
            Self::Poisson(i) => big_int_pdf(i, x),
            Self::Uniform(i) => big_int_pdf(i, x),
        }
    }

    fn quantile(&self, x: f64) -> LazyBigint {
        match self {
            Self::Binomial(i) => inverse_cdf(i, x).into(),
            Self::Custom(items) => {
                let idx = items.partition_point(|(_, p)| p <= &x);
                items[idx].0.clone()
            }
            Self::Hypergeometric(i) => inverse_cdf(i, x).into(),
            Self::NegativeBinomial(i) => {
                if i.r() == 1.0 {
                    let p = i.p();
                    (((1.0 - x) / (1.0 - p)).ln() / (1.0 - p).ln())
                        .floor()
                        .to_i64()
                        .unwrap()
                        .into()
                } else {
                    inverse_cdf(i, x).into()
                }
            }
            Self::Poisson(i) => inverse_cdf(i, x).into(),
            Self::Uniform(i) => (x * ((i.max() - i.min() + 1) as f64) + (i.min() - 1) as f64)
                .floor()
                .to_i64()
                .unwrap()
                .into(),
        }
    }

    fn sample(&self, n: usize, rng: &mut impl RngCore) -> Vec<LazyBigint> {
        match self {
            Self::Binomial(i) => i
                .sample_iter(rng)
                .map(|p| LazyBigint::from_f64(p).unwrap())
                .take(n)
                .collect(),
            Self::Custom(items) => rng
                .sample_iter(Standard)
                .map(|x: f64| {
                    let idx = items.partition_point(|(_, p)| p <= &x);
                    items[idx].0.clone()
                })
                .take(n)
                .collect(),
            Self::Hypergeometric(i) => i
                .sample_iter(rng)
                .map(|p| LazyBigint::from_f64(p).unwrap())
                .take(n)
                .collect(),
            Self::NegativeBinomial(i) => i.sample_iter(rng).map(LazyBigint::from).take(n).collect(),
            Self::Poisson(i) => i
                .sample_iter(rng)
                .map(|p| LazyBigint::from_f64(p).unwrap())
                .take(n)
                .collect(),
            Self::Uniform(i) => i
                .sample_iter(rng)
                .map(|p| LazyBigint::from_f64(p).unwrap())
                .take(n)
                .collect(),
        }
    }

    fn skewness(&self) -> Option<f64> {
        match self {
            Self::Binomial(i) => i.skewness(),
            Self::Custom(items) => {
                let mut means = Means::<3>::default();
                let mut prev_v = 0.0;
                for (k, v) in items {
                    if !means.insert(k, *v - prev_v) {
                        return None;
                    }
                    prev_v = *v;
                }
                let [first, second, third] = means.0;
                let std_sq = second - first * first;
                let std_dev = std_sq.sqrt();
                Some((third - 3.0 * first * std_sq - first.powi(3)) / (std_dev.powi(3)))
            }
            Self::Hypergeometric(i) => i.skewness(),
            Self::NegativeBinomial(i) => i.skewness(),
            Self::Poisson(i) => i.skewness(),
            Self::Uniform(i) => i.skewness(),
        }
    }

    fn mean(&self) -> Option<f64> {
        match self {
            Self::Binomial(i) => i.mean(),
            Self::Custom(items) => {
                let mut means = Means::<1>::default();
                let mut prev_v = 0.0;
                for (k, v) in items {
                    if !means.insert(k, *v - prev_v) {
                        return None;
                    }
                    prev_v = *v;
                }
                let [first] = means.0;
                Some(first)
            }
            Self::Hypergeometric(i) => i.mean(),
            Self::NegativeBinomial(i) => i.mean(),
            Self::Poisson(i) => i.mean(),
            Self::Uniform(i) => i.mean(),
        }
    }

    fn variance(&self) -> Option<f64> {
        match self {
            Self::Binomial(i) => i.variance(),
            Self::Custom(items) => {
                let mut means = Means::<2>::default();
                let mut prev_v = 0.0;
                for (k, v) in items {
                    if !means.insert(k, *v - prev_v) {
                        return None;
                    }
                    prev_v = *v;
                }
                let [first, second] = means.0;
                Some(second - first * first)
            }
            Self::Hypergeometric(i) => i.variance(),
            Self::NegativeBinomial(i) => i.variance(),
            Self::Poisson(i) => i.variance(),
            Self::Uniform(i) => i.variance(),
        }
    }
}

impl XNativeValue for XDiscreteDistribution {
    fn dyn_size(&self) -> usize {
        0
    }
}

pub(crate) fn add_discrete_distribution_type<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_native_type("DiscreteDistribution", X_DISCDIST.clone())
}

pub(crate) fn add_discdist_binomial<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "binomial_distribution",
        XFuncSpec::new(&[&X_INT, &X_FLOAT], X_DISCDIST.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let Some(i0) = to_primitive!(a0, Int).to_u64() else {
                return xerr(ManagedXError::new("n out of bounds", rt)?);
            };
            let f1 = to_primitive!(a1, Float);
            let ret = match Binomial::new(*f1, i0) {
                Ok(ret) => ret,
                Err(e) => {
                    return xerr(ManagedXError::new(format!("{e:?}"), rt)?);
                }
            };
            Ok(manage_native!(XDiscreteDistribution::Binomial(ret), rt))
        }),
    )
}

pub(crate) fn add_discdist_custom<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "custom_distribution",
        XFuncSpec::new(&[&XSequenceType::xtype(Arc::new(XType::Tuple(vec![X_INT.clone(), X_FLOAT.clone()]))), ], X_DISCDIST.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let s0 = to_native!(a0, XSequence::<W, R, T>);
            let Some(len) = s0.len() else { return xerr(ManagedXError::new("sequence is infinite", rt)?); };
            if len == 0 {
                return xerr(ManagedXError::new("sequence is empty", rt)?);
            }
            rt.borrow().can_allocate_by(|| Some(len))?;
            let arr = xraise!(s0.diter(ns, rt.clone()).unwrap().collect::<XResult<Vec<_>,_, _, _>>()?);
            let mut items = arr.iter().map(|item| {
                let tup = to_primitive!(item, StructInstance);
                let val = to_primitive!(tup[0], Int).clone();
                let prob = *to_primitive!(tup[1], Float);
                (val, prob)
            }).sorted_unstable_by(|(k0, _), (k1, _)| k0.cmp(k1))
                .group_by(|p| p.0.clone())
                .into_iter()
                .scan(0.0, |acc, (val, p)| {
                    *acc += p.map(|(_,p)|p).sum::<f64>();
                    Some((val, *acc))
                }).collect::<Vec<_>>();
            let first_p = items.first().unwrap().1;
            if first_p < 0.0 {
                return xerr(ManagedXError::new("probabilities cannot be negative", rt)?);
            }
            let last_p = items.last().unwrap().1;
            if last_p == 0.0{
                return xerr(ManagedXError::new("probabilities cannot all be zero", rt)?);
            }
            if last_p != 1.0{
                items.iter_mut().for_each(|(_, p)| *p /= last_p)
            }
            Ok(manage_native!(XDiscreteDistribution::Custom(items), rt))
        }),
    )
}

pub(crate) fn add_discdist_hypergeometric<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "hypergeometric_distribution",
        XFuncSpec::new(&[&X_INT, &X_INT, &X_INT], X_DISCDIST.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a2 = xraise!(eval(&args[2], ns, &rt)?);
            let Some(i0) = to_primitive!(a0, Int).to_u64() else {
                return xerr(ManagedXError::new("N out of bounds", rt)?);
            };
            let Some(i1) = to_primitive!(a1, Int).to_u64() else {
                return xerr(ManagedXError::new("K out of bounds", rt)?);
            };
            let Some(i2) = to_primitive!(a2, Int).to_u64() else {
                return xerr(ManagedXError::new("n out of bounds", rt)?);
            };
            let ret = match Hypergeometric::new(i0, i1, i2) {
                Ok(ret) => ret,
                Err(e) => {
                    return xerr(ManagedXError::new(format!("{e:?}"), rt)?);
                }
            };
            Ok(manage_native!(
                XDiscreteDistribution::Hypergeometric(ret),
                rt
            ))
        }),
    )
}

pub(crate) fn add_discdist_negative_binomial<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "negative_binomial_distribution",
        XFuncSpec::new(&[&X_FLOAT, &X_FLOAT], X_DISCDIST.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let i0 = to_primitive!(a0, Float);
            let i1 = to_primitive!(a1, Float);
            let ret = match NegativeBinomial::new(*i0, *i1) {
                Ok(ret) => ret,
                Err(e) => {
                    return xerr(ManagedXError::new(format!("{e:?}"), rt)?);
                }
            };
            Ok(manage_native!(
                XDiscreteDistribution::NegativeBinomial(ret),
                rt
            ))
        }),
    )
}

pub(crate) fn add_discdist_poisson<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "poisson_distribution",
        XFuncSpec::new(&[&X_FLOAT], X_DISCDIST.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let i0 = to_primitive!(a0, Float);
            let ret = match Poisson::new(*i0) {
                Ok(ret) => ret,
                Err(e) => {
                    return xerr(ManagedXError::new(format!("{e:?}"), rt)?);
                }
            };
            Ok(manage_native!(XDiscreteDistribution::Poisson(ret), rt))
        }),
    )
}

pub(crate) fn add_discdist_uniform<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "uniform_distribution",
        XFuncSpec::new(&[&X_INT, &X_INT], X_DISCDIST.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let Some(i0) = to_primitive!(a0, Int).to_i64() else {
                return xerr(ManagedXError::new("N out of bounds", rt)?);
            };
            let Some(i1) = to_primitive!(a1, Int).to_i64() else {
                return xerr(ManagedXError::new("K out of bounds", rt)?);
            };
            let ret = match DiscreteUniform::new(i0, i1) {
                Ok(ret) => ret,
                Err(e) => {
                    return xerr(ManagedXError::new(format!("{e:?}"), rt)?);
                }
            };
            Ok(manage_native!(XDiscreteDistribution::Uniform(ret), rt))
        }),
    )
}

pub(crate) fn add_discdist_cdf<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "cdf",
        XFuncSpec::new(&[&X_DISCDIST, &X_INT], X_FLOAT.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let d0 = to_native!(a0, XDiscreteDistribution);
            let i1 = to_primitive!(a1, Int);
            Ok(ManagedXValue::new(XValue::Float(d0.cdf(i1)), rt)?.into())
        }),
    )
}

pub(crate) fn add_discdist_pmf<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "pmf",
        XFuncSpec::new(&[&X_DISCDIST, &X_INT], X_FLOAT.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let d0 = to_native!(a0, XDiscreteDistribution);
            let f1 = to_primitive!(a1, Int);
            Ok(ManagedXValue::new(XValue::Float(d0.pmf(f1)), rt)?.into())
        }),
    )
}

pub(crate) fn add_discdist_quantile<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "quantile",
        XFuncSpec::new(&[&X_DISCDIST, &X_FLOAT], X_INT.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let d0 = to_native!(a0, XDiscreteDistribution);
            let f1 = to_primitive!(a1, Float);
            if *f1 > 1.0 || *f1 < 0.0 {
                return xerr(ManagedXError::new("quantile must be between 0 and 1", rt)?);
            }
            Ok(ManagedXValue::new(XValue::Int(d0.quantile(*f1)), rt)?.into())
        }),
    )
}

pub(crate) fn add_discdist_skewness<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "skewness",
        XFuncSpec::new(&[&X_DISCDIST], X_FLOAT.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let d0 = to_native!(a0, XDiscreteDistribution);
            let ret = d0.skewness();
            match ret {
                None => xerr(ManagedXError::new("distribution has no skew", rt)?),
                Some(ret) => Ok(ManagedXValue::new(XValue::Float(ret), rt)?.into()),
            }
        }),
    )
}

pub(crate) fn add_discdist_mean<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "mean",
        XFuncSpec::new(&[&X_DISCDIST], X_FLOAT.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let d0 = to_native!(a0, XDiscreteDistribution);
            let ret = d0.mean();
            match ret {
                None => xerr(ManagedXError::new("distribution has no mean", rt)?),
                Some(ret) => Ok(ManagedXValue::new(XValue::Float(ret), rt)?.into()),
            }
        }),
    )
}

pub(crate) fn add_discdist_variance<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "variance",
        XFuncSpec::new(&[&X_DISCDIST], X_FLOAT.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let d0 = to_native!(a0, XDiscreteDistribution);
            let ret = d0.variance();
            match ret {
                None => xerr(ManagedXError::new("distribution has no variance", rt)?),
                Some(ret) => Ok(ManagedXValue::new(XValue::Float(ret), rt)?.into()),
            }
        }),
    )
}

pub(crate) fn add_discdist_sample<W, R: SeedableRng + RngCore, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "sample",
        XFuncSpec::new(&[&X_DISCDIST, &X_INT], XSequenceType::xtype(X_INT.clone())),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let d0 = to_native!(a0, XDiscreteDistribution);
            let Some(i1) = to_primitive!(a1, Int).to_usize() else { return xerr(ManagedXError::new("count out of bounds", rt)?); };
            rt.borrow()
                .limits
                .check_permission(&builtin_permissions::RANDOM)?;
            rt.borrow().can_allocate(i1)?;
            let nums = d0.sample(i1, rt.borrow_mut().get_rng());
            let nums = nums.into_iter().map(|v| ManagedXValue::new(XValue::Int(v), rt.clone())).collect::<Result<Vec<_>, _>>()?;
            let ret = XSequence::array(nums);
            Ok(manage_native!(ret, rt))
        }),
    )
}
