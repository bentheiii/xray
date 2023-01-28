use crate::builtin::core::{eval, xerr};
use crate::native_types::{NativeType, XNativeValue};
use crate::xtype::{XFuncSpec, X_FLOAT};
use crate::xvalue::{ManagedXError, ManagedXValue, XValue};
use crate::{
    manage_native, to_native, to_primitive, xraise, CompilationError, RootCompilationScope,
    XStaticFunction, XType,
};
use num_traits::Float;
use statrs::distribution::{Beta, Continuous, ContinuousCDF, Uniform};
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
    Uniform(Uniform),
}

impl XContinuousDistribution {
    fn cdf(&self, x: f64) -> f64 {
        match self {
            Self::Beta(i) => i.cdf(x),
            Self::Uniform(i) => i.cdf(x),
        }
    }

    fn pdf(&self, x: f64) -> f64 {
        match self {
            Self::Beta(i) => i.pdf(x),
            Self::Uniform(i) => i.pdf(x),
        }
    }

    fn quantile(&self, x: f64) -> f64 {
        match self {
            Self::Beta(i) => deep_inverse_cdf(i, x),
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
        "beta",
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
            Ok(ManagedXValue::new(XValue::Float(d0.quantile(*f1)), rt)?.into())
        }),
    )
}
