use crate::builtin::core::{eval, xerr};
use crate::native_types::{NativeType, XNativeValue};
use crate::xtype::{XFuncSpec, X_FLOAT, X_INT};
use crate::xvalue::{ManagedXError, ManagedXValue, XValue};
use crate::{
    manage_native, to_native, to_primitive, xraise, xraise_opt, CompilationError,
    RootCompilationScope, XStaticFunction, XType,
};
use num_traits::{Float, ToPrimitive};
use statrs::distribution::{
    Beta, Continuous, ContinuousCDF, Exp, FisherSnedecor, Gamma, LogNormal, Normal, StudentsT,
    Triangular, Uniform, Weibull,
};
use statrs::function::erf::erf_inv;
use statrs::statistics::{Distribution, Max, Min, Mode};
use std::fmt::Debug;
use std::mem::size_of;

use crate::builtin::builtin_permissions;
use crate::builtin::sequence::{XSequence, XSequenceType};
use rand::distributions::Distribution as _;
use rand::{RngCore, SeedableRng};
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
    StudentsT(StudentsT),
    Triangular(Triangular),
    Uniform(Uniform),
    Weibull(Weibull),
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
            Self::StudentsT(i) => i.cdf(x),
            Self::Triangular(i) => i.cdf(x),
            Self::Uniform(i) => i.cdf(x),
            Self::Weibull(i) => i.cdf(x),
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
            Self::Triangular(i) => i.pdf(x),
            Self::StudentsT(i) => i.pdf(x),
            Self::Uniform(i) => i.pdf(x),
            Self::Weibull(i) => i.pdf(x),
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
            Self::StudentsT(i) => i.inverse_cdf(x),
            Self::Triangular(i) => {
                if x <= 0.0 {
                    return i.min();
                }
                let cdf_at_peak = (i.mode().unwrap() - i.min()) / (i.max() - i.min());
                if x <= cdf_at_peak {
                    // we are betwen a and c
                    return i.min()
                        + (x * (i.max() - i.min()) * (i.mode().unwrap() - i.min())).sqrt();
                }
                if x >= 1.0 {
                    return i.max();
                }
                // we are between c and b
                i.max() - ((1.0 - x) * (i.max() - i.min()) * (i.max() - i.mode().unwrap())).sqrt()
            }
            Self::Uniform(i) => x * (i.max() - i.min()) + i.min(),
            Self::Weibull(i) => deep_inverse_cdf(i, x),
        }
    }

    fn sample(&self, n: usize, rng: &mut impl RngCore) -> Vec<f64> {
        match self {
            Self::Beta(i) => i.sample_iter(rng).take(n).collect(),
            Self::Exponential(i) => i.sample_iter(rng).take(n).collect(),
            Self::FisherSnedecor(i) => i.sample_iter(rng).take(n).collect(),
            Self::Gamma(i) => i.sample_iter(rng).take(n).collect(),
            Self::LogNormal(i, ..) => i.sample_iter(rng).take(n).collect(),
            Self::Normal(i) => i.sample_iter(rng).take(n).collect(),
            Self::StudentsT(i) => i.sample_iter(rng).take(n).collect(),
            Self::Triangular(i) => i.sample_iter(rng).take(n).collect(),
            Self::Uniform(i) => i.sample_iter(rng).take(n).collect(),
            Self::Weibull(i) => i.sample_iter(rng).take(n).collect(),
        }
    }

    fn skewness(&self) -> Option<f64> {
        match self {
            Self::Beta(i) => i.skewness(),
            Self::Exponential(i) => i.skewness(),
            Self::FisherSnedecor(i) => i.skewness(),
            Self::Gamma(i) => i.skewness(),
            Self::LogNormal(i, ..) => i.skewness(),
            Self::Normal(i) => i.skewness(),
            Self::StudentsT(i) => i.skewness(),
            Self::Triangular(i) => i.skewness(),
            Self::Uniform(i) => i.skewness(),
            Self::Weibull(i) => i.skewness(),
        }
    }

    fn mean(&self) -> Option<f64> {
        match self {
            Self::Beta(i) => i.mean(),
            Self::Exponential(i) => i.mean(),
            Self::FisherSnedecor(i) => i.mean(),
            Self::Gamma(i) => i.mean(),
            Self::LogNormal(i, ..) => i.mean(),
            Self::Normal(i) => i.mean(),
            Self::StudentsT(i) => i.mean(),
            Self::Triangular(i) => i.mean(),
            Self::Uniform(i) => i.mean(),
            Self::Weibull(i) => i.mean(),
        }
    }

    fn variance(&self) -> Option<f64> {
        match self {
            Self::Beta(i) => i.variance(),
            Self::Exponential(i) => i.variance(),
            Self::FisherSnedecor(i) => i.variance(),
            Self::Gamma(i) => i.variance(),
            Self::LogNormal(i, ..) => i.variance(),
            Self::Normal(i) => i.variance(),
            Self::StudentsT(i) => i.variance(),
            Self::Triangular(i) => i.variance(),
            Self::Uniform(i) => i.variance(),
            Self::Weibull(i) => i.variance(),
        }
    }
}

impl XNativeValue for XContinuousDistribution {
    fn dyn_size(&self) -> usize {
        0
    }
}

pub(crate) fn add_continuous_distribution_type<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_native_type("ContinuousDistribution", X_CONTDIST.clone())
}

pub(crate) fn add_contdist_beta<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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

pub(crate) fn add_contdist_lognormal<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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

pub(crate) fn add_contdist_exp<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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

pub(crate) fn add_contdist_fs<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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

pub(crate) fn add_contdist_normal<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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
            Ok(manage_native!(XContinuousDistribution::Normal(ret), rt))
        }),
    )
}

pub(crate) fn add_contdist_weibull<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "weibull_distribution",
        XFuncSpec::new(&[&X_FLOAT, &X_FLOAT], X_CONTDIST.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let f0 = to_primitive!(a0, Float);
            let f1 = to_primitive!(a1, Float);
            let ret = match Weibull::new(*f0, *f1) {
                Ok(ret) => ret,
                Err(e) => {
                    return xerr(ManagedXError::new(format!("{e:?}"), rt)?);
                }
            };
            Ok(manage_native!(XContinuousDistribution::Weibull(ret), rt))
        }),
    )
}

pub(crate) fn add_contdist_gamma<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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

pub(crate) fn add_contdist_rectangular<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "rectangular_distribution",
        XFuncSpec::new(&[&X_FLOAT, &X_FLOAT], X_CONTDIST.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let f0 = to_primitive!(a0, Float);
            let f1 = to_primitive!(a1, Float);
            let ret = match Uniform::new(*f0, *f1) {
                Ok(ret) => ret,
                Err(e) => {
                    return xerr(ManagedXError::new(format!("{e:?}"), rt)?);
                }
            };
            Ok(manage_native!(XContinuousDistribution::Uniform(ret), rt))
        }),
    )
}

pub(crate) fn add_contdist_students_t<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "students_t_distribution",
        XFuncSpec::new_with_optional(&[&X_FLOAT], &[&X_FLOAT, &X_FLOAT], X_CONTDIST.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise_opt!(args.get(1).map(|e| eval(e, ns, &rt)).transpose()?);
            let a2 = xraise_opt!(args.get(2).map(|e| eval(e, ns, &rt)).transpose()?);
            let f0 = to_primitive!(a0, Float);
            let f1 = a1.map_or(0.0, |a| *to_primitive!(a, Float));
            let f2 = a2.map_or(1.0, |a| *to_primitive!(a, Float));
            let ret = match StudentsT::new(f1, f2, *f0) {
                Ok(ret) => ret,
                Err(e) => {
                    return xerr(ManagedXError::new(format!("{e:?}"), rt)?);
                }
            };
            Ok(manage_native!(XContinuousDistribution::StudentsT(ret), rt))
        }),
    )
}

pub(crate) fn add_contdist_triangle<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "triangular_distribution",
        XFuncSpec::new_with_optional(&[&X_FLOAT, &X_FLOAT], &[&X_FLOAT], X_CONTDIST.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a2 = xraise_opt!(args.get(2).map(|e| eval(e, ns, &rt)).transpose()?);
            let f0 = to_primitive!(a0, Float);
            let f1 = to_primitive!(a1, Float);
            let f2 = a2.map_or((f0 + f1) / 2.0, |a| *to_primitive!(a, Float));
            let ret = match Triangular::new(*f0, *f1, f2) {
                Ok(ret) => ret,
                Err(e) => {
                    return xerr(ManagedXError::new(format!("{e:?}"), rt)?);
                }
            };
            Ok(manage_native!(XContinuousDistribution::Triangular(ret), rt))
        }),
    )
}

pub(crate) fn add_contdist_cdf<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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

pub(crate) fn add_contdist_pdf<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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

pub(crate) fn add_contdist_quantile<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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

pub(crate) fn add_contdist_skewness<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "skewness",
        XFuncSpec::new(&[&X_CONTDIST], X_FLOAT.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let d0 = to_native!(a0, XContinuousDistribution);
            let ret = d0.skewness();
            match ret {
                None => xerr(ManagedXError::new("distribution has no skew", rt)?),
                Some(ret) => Ok(ManagedXValue::new(XValue::Float(ret), rt)?.into()),
            }
        }),
    )
}

pub(crate) fn add_contdist_mean<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "mean",
        XFuncSpec::new(&[&X_CONTDIST], X_FLOAT.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let d0 = to_native!(a0, XContinuousDistribution);
            let ret = d0.mean();
            match ret {
                None => xerr(ManagedXError::new("distribution has no mean", rt)?),
                Some(ret) => Ok(ManagedXValue::new(XValue::Float(ret), rt)?.into()),
            }
        }),
    )
}

pub(crate) fn add_contdist_variance<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "variance",
        XFuncSpec::new(&[&X_CONTDIST], X_FLOAT.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let d0 = to_native!(a0, XContinuousDistribution);
            let ret = d0.variance();
            match ret {
                None => xerr(ManagedXError::new("distribution has no variance", rt)?),
                Some(ret) => Ok(ManagedXValue::new(XValue::Float(ret), rt)?.into()),
            }
        }),
    )
}

pub(crate) fn add_contdist_sample<W, R: SeedableRng + RngCore, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "sample",
        XFuncSpec::new(&[&X_CONTDIST, &X_INT], XSequenceType::xtype(X_FLOAT.clone())),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let d0 = to_native!(a0, XContinuousDistribution);
            let Some(i1) = to_primitive!(a1, Int).to_usize() else { return xerr(ManagedXError::new("count out of bounds", rt)?); };
            rt.limits.check_permission(&builtin_permissions::RANDOM)?;
            rt.can_allocate(i1*size_of::<usize>())?;
            let nums = d0.sample(i1, rt.stats.borrow_mut().get_rng());
            let nums = nums.into_iter().map(|v| ManagedXValue::new(XValue::Float(v), rt.clone())).collect::<Result<Vec<_>, _>>()?;
            let ret = XSequence::array(nums);
            Ok(manage_native!(ret, rt))
        }),
    )
}
