use crate::builtin::core::{eval, xerr};
use crate::native_types::{NativeType, XNativeValue};
use crate::xtype::{XFuncSpec, X_FLOAT};
use crate::xvalue::{ManagedXError, ManagedXValue, XValue};
use crate::{
    manage_native, to_native, to_primitive, xraise, CompilationError, RootCompilationScope,
    XStaticFunction, XType,
};
use num_traits::Float;
use statrs::distribution::{Beta, Continuous, ContinuousCDF, Exp, FisherSnedecor, Gamma, LogNormal, Normal, Uniform};
use statrs::function::erf::erf_inv;
use statrs::statistics::{Max, Min};
use std::fmt::Debug;
use std::io::Write;
use std::sync::Arc;

/// copy-paste of the inverse cdf algorithm from statrs, but with more precision
fn deep_inverse_cdf<K: Float, T: Float, S: ContinuousCDF<K, T>>(s: &S, p: T) -> K {
    if p == T::zero() {
        return s.min();
    };
    if p == T::one() {
        return s.max();
    };
    let two = K::one() + K::one();
    let mut high = two;
    let mut low = -high;
    while s.cdf(low) > p {
        low = low + low;
    }
    while s.cdf(high) < p {
        high = high + high;
    }
    let mut i = 32;
    while i != 0 {
        let mid = (high + low) / two;
        if s.cdf(mid) >= p {
            high = mid;
        } else {
            low = mid;
        }
        i -= 1;
    }
    (high + low) / two
}

#[derive(Debug, Clone)]
pub(crate) struct XContinuousDistributionType;

impl NativeType for XContinuousDistributionType {
    fn generic_names(&self) -> Vec<String> {
        vec![]
    }
    fn name(&self) -> &str {
        "ContinuousDistribution"
    }
}

lazy_static! {
    static ref X_CONTDIST: Arc<XType> = Arc::new(XType::XNative(
        Box::new(XContinuousDistributionType),
        vec![]
    ));
}

#[derive(Debug)]
pub(crate) enum XContinuousDistribution {
    Beta(Beta),
    Exponential(Exp),
    FisherSnedecor(FisherSnedecor),
    Gamma(Gamma),
    LogNormal(LogNormal, f64, f64),
    Normal(Normal),
    Uniform(Uniform),
}

impl XContinuousDistribution {
    fn cdf(&self, x: f64) -> f64 {
        match self {
            Self::Beta(i) => i.cdf(x),
            Self::Exponential(i) => i.cdf(x),
            Self::FisherSnedecor(i) => i.cdf(x),
            Self::Gamma(i) => i.cdf(x),
            Self::LogNormal(i, ..) => i.cdf(x),
            Self::Normal(i) => i.cdf(x),
            Self::Uniform(i) => i.cdf(x),
        }
    }

    fn pdf(&self, x: f64) -> f64 {
        match self {
            Self::Beta(i) => i.pdf(x),
            Self::Exponential(i) => i.pdf(x),
            Self::FisherSnedecor(i) => i.pdf(x),
            Self::Gamma(i) => i.pdf(x),
            Self::LogNormal(i, ..) => i.pdf(x),
            Self::Normal(i) => i.pdf(x),
            Self::Uniform(i) => i.pdf(x),
        }
    }

    fn quantile(&self, x: f64) -> f64 {
        match self {
            Self::Beta(i) => deep_inverse_cdf(i, x),
            Self::Exponential(i) => -(1.0 - x).ln() / i.rate(),
            Self::FisherSnedecor(i) => deep_inverse_cdf(i, x),
            Self::Gamma(i) => deep_inverse_cdf(i, x),
            Self::LogNormal(_, location, scale) => {
                (location + (2.0 * scale * scale).sqrt() * erf_inv(2.0 * x - 1.0)).exp()
            }
            Self::Normal(i) => i.inverse_cdf(x),
            Self::Uniform(i) => x * (i.max() - i.min()) + i.min(),
        }
    }
}

impl XNativeValue for XContinuousDistribution {
    fn dyn_size(&self) -> usize {
        0
    }
}

pub(crate) fn add_continuous_distribution_type<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_native_type("ContinuousDistribution", X_CONTDIST.clone())
}

pub(crate) fn add_contdist_beta<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "beta_distribution",
        XFuncSpec::new(&[&X_FLOAT, &X_FLOAT], X_CONTDIST.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let f0 = to_primitive!(a0, Float);
            let f1 = to_primitive!(a1, Float);
            let ret = match Beta::new(*f0, *f1) {
                Ok(ret) => ret,
                Err(e) => {
                    return xerr(ManagedXError::new(format!("{e:?}"), rt)?);
                }
            };
            Ok(manage_native!(XContinuousDistribution::Beta(ret), rt))
        }),
    )
}

pub(crate) fn add_contdist_lognormal<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "lognormal_distribution",
        XFuncSpec::new(&[&X_FLOAT, &X_FLOAT], X_CONTDIST.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let f0 = to_primitive!(a0, Float);
            let f1 = to_primitive!(a1, Float);
            let ret = match LogNormal::new(*f0, *f1) {
                Ok(ret) => ret,
                Err(e) => {
                    return xerr(ManagedXError::new(format!("{e:?}"), rt)?);
                }
            };
            Ok(manage_native!(
                XContinuousDistribution::LogNormal(ret, *f0, *f1),
                rt
            ))
        }),
    )
}

pub(crate) fn add_contdist_exp<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "exp_distribution",
        XFuncSpec::new(&[&X_FLOAT], X_CONTDIST.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let f0 = to_primitive!(a0, Float);
            let ret = match Exp::new(*f0) {
                Ok(ret) => ret,
                Err(e) => {
                    return xerr(ManagedXError::new(format!("{e:?}"), rt)?);
                }
            };
            Ok(manage_native!(
                XContinuousDistribution::Exponential(ret),
                rt
            ))
        }),
    )
}

pub(crate) fn add_contdist_fs<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "fisher_snedecor_distribution",
        XFuncSpec::new(&[&X_FLOAT, &X_FLOAT], X_CONTDIST.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let f0 = to_primitive!(a0, Float);
            let f1 = to_primitive!(a1, Float);
            let ret = match FisherSnedecor::new(*f0, *f1) {
                Ok(ret) => ret,
                Err(e) => {
                    return xerr(ManagedXError::new(format!("{e:?}"), rt)?);
                }
            };
            Ok(manage_native!(
                XContinuousDistribution::FisherSnedecor(ret),
                rt
            ))
        }),
    )
}

pub(crate) fn add_contdist_normal<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "normal_distribution",
        XFuncSpec::new(&[&X_FLOAT, &X_FLOAT], X_CONTDIST.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let f0 = to_primitive!(a0, Float);
            let f1 = to_primitive!(a1, Float);
            let ret = match Normal::new(*f0, *f1) {
                Ok(ret) => ret,
                Err(e) => {
                    return xerr(ManagedXError::new(format!("{e:?}"), rt)?);
                }
            };
            Ok(manage_native!(
                XContinuousDistribution::Normal(ret),
                rt
            ))
        }),
    )
}

pub(crate) fn add_contdist_cdf<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "cdf",
        XFuncSpec::new(&[&X_CONTDIST, &X_FLOAT], X_FLOAT.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let d0 = to_native!(a0, XContinuousDistribution);
            let f1 = to_primitive!(a1, Float);
            Ok(ManagedXValue::new(XValue::Float(d0.cdf(*f1)), rt)?.into())
        }),
    )
}

pub(crate) fn add_contdist_pdf<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "pdf",
        XFuncSpec::new(&[&X_CONTDIST, &X_FLOAT], X_FLOAT.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let d0 = to_native!(a0, XContinuousDistribution);
            let f1 = to_primitive!(a1, Float);
            Ok(ManagedXValue::new(XValue::Float(d0.pdf(*f1)), rt)?.into())
        }),
    )
}

pub(crate) fn add_contdist_quantile<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "quantile",
        XFuncSpec::new(&[&X_CONTDIST, &X_FLOAT], X_FLOAT.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let d0 = to_native!(a0, XContinuousDistribution);
            let f1 = to_primitive!(a1, Float);
            if *f1 > 1.0 || *f1 < 0.0 {
                return xerr(ManagedXError::new("quantile must be between 0 and 1", rt)?);
            }
            let ret = d0.quantile(*f1);
            if !ret.is_finite() {
                return xerr(ManagedXError::new("value out of bounds", rt)?);
            }
            Ok(ManagedXValue::new(XValue::Float(ret), rt)?.into())
        }),
    )
}

pub(crate) fn add_contdist_gamma<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "gamma_distribution",
        XFuncSpec::new(&[&X_FLOAT, &X_FLOAT], X_CONTDIST.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let f0 = to_primitive!(a0, Float);
            let f1 = to_primitive!(a1, Float);
            let ret = match Gamma::new(*f0, *f1) {
                Ok(ret) => ret,
                Err(e) => {
                    return xerr(ManagedXError::new(format!("{e:?}"), rt)?);
                }
            };
            Ok(manage_native!(XContinuousDistribution::Gamma(ret), rt))
        }),
    )
}
