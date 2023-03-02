use crate::builtin::core::{eval, xerr};
use crate::native_types::{NativeType, XNativeValue};
use crate::util::lazy_bigint::LazyBigint;
use crate::xtype::{XFuncSpec, X_FLOAT, X_INT};
use crate::xvalue::{ManagedXError, ManagedXValue, XValue};
use crate::{
    manage_native, to_native, to_primitive, xraise, CompilationError, RootCompilationScope,
    XStaticFunction, XType,
};
use num_traits::{Bounded, Float, Num, Signed, ToPrimitive};
use statrs::distribution::{Binomial, Discrete, DiscreteCDF, DiscreteUniform, Hypergeometric};
use statrs::statistics::{Max, Min};
use std::fmt::Debug;
use std::io::Write;
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
    Hypergeometric(Hypergeometric),
    Uniform(DiscreteUniform),
}

impl XDiscreteDistribution {
    fn cdf(&self, x: &LazyBigint) -> f64 {
        match self {
            Self::Binomial(i) => x
                .to_u64()
                .map_or_else(|| if x.is_negative() { 0.0 } else { 1.0 }, |x| i.cdf(x)),
            Self::Hypergeometric(i) => x
                .to_u64()
                .map_or_else(|| if x.is_negative() { 0.0 } else { 1.0 }, |x| i.cdf(x)),
            Self::Uniform(i) => x
                .to_i64()
                .map_or_else(|| if x.is_negative() { 0.0 } else { 1.0 }, |x| i.cdf(x)),
        }
    }

    fn pmf(&self, x: &LazyBigint) -> f64 {
        match self {
            Self::Binomial(i) => x.to_u64().map_or(0.0, |x| i.pmf(x)),
            Self::Hypergeometric(i) => x.to_u64().map_or(0.0, |x| i.pmf(x)),
            Self::Uniform(i) => x.to_i64().map_or(0.0, |x| i.pmf(x)),
        }
    }

    fn quantile(&self, x: f64) -> LazyBigint {
        match self {
            Self::Binomial(i) => inverse_cdf(i, x).into(),
            Self::Hypergeometric(i) => inverse_cdf(i, x).into(),
            Self::Uniform(i) => (x * ((i.max() - i.min() + 1) as f64) + (i.min() - 1) as f64)
                .floor()
                .to_i64()
                .unwrap()
                .into(),
        }
    }
}

impl XNativeValue for XDiscreteDistribution {
    fn dyn_size(&self) -> usize {
        0
    }
}

pub(crate) fn add_discrete_distribution_type<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_native_type("DiscreteDistribution", X_DISCDIST.clone())
}

pub(crate) fn add_discdist_binomial<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
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

pub(crate) fn add_discdist_hypergeometric<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
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

pub(crate) fn add_discdist_cdf<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
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

pub(crate) fn add_discdist_pmf<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
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

pub(crate) fn add_discdist_quantile<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
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
