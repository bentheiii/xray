use crate::builtin::core::{eval, xerr};
use crate::native_types::{NativeType, XNativeValue};
use crate::xtype::{XFuncSpec, X_INT, X_STRING};
use crate::xvalue::{ManagedXError, ManagedXValue, XValue};

use crate::{
    manage_native, to_native, to_primitive, xraise, xraise_opt, CompilationError,
    RootCompilationScope, XStaticFunction, XType,
};

use std::fmt::Debug;

use num_traits::ToPrimitive;
use regex::{Regex, RegexBuilder};
use std::sync::Arc;

use crate::builtin::builtin_permissions;
use crate::builtin::optional::{XOptional, XOptionalType};
use crate::builtin::sequence::{XSequence, XSequenceType};

use crate::util::lazy_bigint::LazyBigint;

// note that this entire module is on hold until we can make regexes somewhat safe.

#[derive(Debug, Clone)]
pub(crate) struct XRegexType {}

impl NativeType for XRegexType {
    fn generic_names(&self) -> Vec<String> {
        vec![]
    }
    fn name(&self) -> &str {
        "Regex"
    }
}

lazy_static! {
    static ref X_REGEX: Arc<XType> = Arc::new(XType::XNative(Box::new(XRegexType {}), vec![]));
    static ref X_MATCH: Arc<XType> = XOptionalType::xtype(XSequenceType::xtype(
        XOptionalType::xtype(Arc::new(XType::Tuple(vec![X_INT.clone(), X_INT.clone()])))
    ));
}

impl XNativeValue for Regex {
    fn dyn_size(&self) -> usize {
        // It's really hard to tell how much data a regex pattern is taking up, so we're gonna
        // assume it's twice the length of the pattern
        2 * self.as_str().len()
    }
}

pub(crate) fn add_regex_type<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_native_type("Regex", X_REGEX.clone())
}

pub(crate) fn add_regex_new<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "regex",
        XFuncSpec::new(&[&X_STRING], X_REGEX.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            rt.limits.check_permission(&builtin_permissions::REGEX)?;
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let s0 = to_primitive!(a0, String);
            let mut builder = RegexBuilder::new(s0.as_str());
            if rt.limits.size_limit.is_some() {
                builder.size_limit(rt.size_left());
            }

            let regex = match builder.build() {
                Ok(regex) => regex,
                Err(err) => {
                    return xerr(ManagedXError::new(
                        format!("error compiling regex: {err}"),
                        rt,
                    )?)
                }
            };
            Ok(manage_native!(regex, rt))
        }),
    )
}

pub(crate) fn add_regex_match<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "match",
        XFuncSpec::new_with_optional(&[&X_REGEX, &X_STRING], &[&X_INT], X_MATCH.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a2 = xraise_opt!(args.get(2).map(|e| eval(e, ns, &rt)).transpose()?);
            let r0 = to_native!(a0, Regex);
            let s1 = to_primitive!(a1, String);
            let i2 = match a2 {
                None => 0,
                Some(a2) => match to_primitive!(a2, Int).to_usize() {
                    Some(i2) => i2,
                    None => return xerr(ManagedXError::new("start index out of range", rt)?),
                },
            };
            let Some(cap) = r0.captures(s1.substr(i2, None)) else {
                return Ok(manage_native!(XOptional::<W, R, T> {value: None}, rt))
            };
            let mut pairs = Vec::with_capacity(r0.captures_len());
            for sub_cap in cap.iter() {
                pairs.push(match sub_cap {
                    None => manage_native!(XOptional::<W, R, T> { value: None }, rt.clone()),
                    Some(m) => {
                        let start = ManagedXValue::new(
                            XValue::Int(LazyBigint::from(m.start() + i2)),
                            rt.clone(),
                        )?;
                        let end = ManagedXValue::new(
                            XValue::Int(LazyBigint::from(m.end() + i2)),
                            rt.clone(),
                        )?;
                        let t = ManagedXValue::new(
                            XValue::StructInstance(vec![start, end]),
                            rt.clone(),
                        )?;
                        manage_native!(XOptional::<W, R, T> { value: Some(t) }, rt.clone())
                    }
                })
            }

            let arr = manage_native!(XSequence::array(pairs), rt.clone());
            Ok(manage_native!(
                XOptional::<W, R, T> { value: Some(arr) },
                rt
            ))
        }),
    )
}
