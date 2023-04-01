use crate::builtin::core::{eval, xcmp, xerr};
use crate::util::xformatter::{Alignment, FillSpecs, XFormatting};
use crate::xtype::{XFuncSpec, X_BOOL, X_INT, X_STRING};
use crate::xvalue::{ManagedXError, ManagedXValue, XValue};

use crate::util::lazy_bigint::LazyBigint;
use rc::Rc;
use std::borrow::Cow;
use std::cmp::max;
use std::collections::hash_map::DefaultHasher;

use crate::builtin::optional::{XOptional, XOptionalType};
use crate::builtin::sequence::{XSequence, XSequenceType};
use crate::compile_err::CompilationError;
use crate::root_compilation_scope::RootCompilationScope;
use crate::runtime::RTCell;
use crate::util::fenced_string::FencedString;
use crate::xexpr::XStaticFunction;
use crate::{add_binfunc, manage_native, to_primitive, ufunc, xraise, xraise_opt};
use itertools::Itertools;
use num_traits::{FromPrimitive, Signed, ToPrimitive};
use std::hash::{Hash, Hasher};

use std::rc;

pub(crate) fn add_str_type<W, R>(
    scope: &mut RootCompilationScope<W, R>,
) -> Result<(), CompilationError> {
    scope.add_native_type("str", X_STRING.clone())
}

add_binfunc!(add_str_eq, eq, X_STRING, String, X_BOOL, |a, b, _rt| Ok(
    Ok(XValue::Bool(a == b))
));

add_binfunc!(
    add_str_add,
    add,
    X_STRING,
    String,
    X_STRING,
    |a: &FencedString, b: &FencedString, rt: &RTCell<W, R>| {
        rt.borrow().can_allocate(a.bytes() + b.bytes())?;
        Ok(Ok(XValue::String(Box::new(a + b))))
    }
);

pub(crate) fn add_str_hash<W, R>(
    scope: &mut RootCompilationScope<W, R>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "hash",
        XFuncSpec::new(&[&X_STRING], X_INT.clone()),
        ufunc!(String, |a: &FencedString, _rt| {
            Ok(Ok(XValue::Int({
                let mut s = DefaultHasher::new();
                a.hash(&mut s);
                let hash = s.finish();
                LazyBigint::from(hash)
            })))
        }),
    )
}

pub(crate) fn add_str_len<W, R>(
    scope: &mut RootCompilationScope<W, R>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "len",
        XFuncSpec::new(&[&X_STRING], X_INT.clone()),
        ufunc!(String, |a: &FencedString, _rt| {
            Ok(Ok(XValue::Int(LazyBigint::from(a.len()))))
        }),
    )
}

pub(crate) fn add_str_to_str<W, R>(
    scope: &mut RootCompilationScope<W, R>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "to_str",
        XFuncSpec::new(&[&X_STRING], X_STRING.clone()),
        XStaticFunction::from_native(|args, ns, tca, rt| ns.eval(&args[0], rt, tca)),
    )
}

pub(crate) fn add_str_chars<W, R>(
    scope: &mut RootCompilationScope<W, R>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "chars",
        XFuncSpec::new(&[&X_STRING], XSequenceType::xtype(X_STRING.clone())),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let s = to_primitive!(a0, String);
            rt.borrow().can_allocate_by(|| Some(s.bytes()))?;
            let chars = s
                .iter()
                .map(|c| {
                    ManagedXValue::new(
                        XValue::String(Box::new(FencedString::from_string(c.to_string()))),
                        rt.clone(),
                    )
                })
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .collect();
            Ok(manage_native!(XSequence::array(chars), rt))
        }),
    )
}

pub(crate) fn add_str_get<W, R>(
    scope: &mut RootCompilationScope<W, R>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "get",
        XFuncSpec::new(&[&X_STRING, &X_INT], X_STRING.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let s = to_primitive!(a0, String);
            let i = to_primitive!(a1, Int);
            let Some(i) = if i.is_negative() { Cow::Owned(i + s.len()) } else { Cow::Borrowed(i) }.to_usize() else { xraise!(Err(ManagedXError::new("index too large",rt)?)) };
            Ok(ManagedXValue::new(XValue::String(Box::new(s.substring(i, Some(i + 1)))), rt)?.into())
        }),
    )
}

pub(crate) fn add_str_find<W, R>(
    scope: &mut RootCompilationScope<W, R>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "find",
        XFuncSpec::new_with_optional(
            &[&X_STRING, &X_STRING],
            &[&X_INT],
            XOptionalType::xtype(X_INT.clone()),
        ),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a2 = xraise_opt!(args.get(2).map(|e| eval(e, ns, &rt)).transpose()?);
            let string = to_primitive!(a0, String);
            let needle = to_primitive!(a1, String);
            if needle.is_empty() {
                return xerr(ManagedXError::new("needle cannot be empty", rt)?);
            }
            let start_ind = match a2 {
                None => 0usize,
                Some(a2) => match to_primitive!(a2, Int).to_usize() {
                    None => return xerr(ManagedXError::new("index out of bounds", rt)?),
                    Some(i) => i,
                },
            };
            let haystack = string.substr(start_ind, None);
            let found_idx = haystack
                .find(needle.as_str())
                .map(|i| ManagedXValue::new(XValue::Int((i + start_ind).into()), rt.clone()))
                .transpose()?;
            Ok(manage_native!(XOptional { value: found_idx }, rt))
        }),
    )
}

pub(crate) fn add_str_rfind<W, R>(
    scope: &mut RootCompilationScope<W, R>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "rfind",
        XFuncSpec::new_with_optional(
            &[&X_STRING, &X_STRING],
            &[&X_INT],
            XOptionalType::xtype(X_INT.clone()),
        ),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a2 = xraise_opt!(args.get(2).map(|e| eval(e, ns, &rt)).transpose()?);
            let string = to_primitive!(a0, String);
            let needle = to_primitive!(a1, String);
            if needle.is_empty() {
                return xerr(ManagedXError::new("needle cannot be empty", rt)?);
            }
            let end_ind = match a2 {
                None => None,
                Some(a2) => Some(match to_primitive!(a2, Int).to_usize() {
                    None => return xerr(ManagedXError::new("index out of bounds", rt)?),
                    Some(i) => i,
                }),
            };
            let haystack = string.substr(0, end_ind);
            let found_idx = haystack
                .rfind(needle.as_str())
                .map(|i| ManagedXValue::new(XValue::Int(i.into()), rt.clone()))
                .transpose()?;
            Ok(manage_native!(XOptional { value: found_idx }, rt))
        }),
    )
}

pub(crate) fn add_str_substring<W, R>(
    scope: &mut RootCompilationScope<W, R>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "substring",
        XFuncSpec::new(&[&X_STRING, &X_INT, &X_INT], X_STRING.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a2 = xraise!(eval(&args[2], ns, &rt)?);
            let string = to_primitive!(a0, String);
            let Some(start) = to_primitive!(a1, Int).to_usize() else { return xerr(ManagedXError::new("index out of bounds", rt)?); };
            let raw_end = to_primitive!(a2, Int);
            let raw_end = if raw_end.is_negative() { Cow::Owned(raw_end + string.len()) } else { Cow::Borrowed(raw_end) };
            let Some(end) = raw_end.to_usize() else { return xerr(ManagedXError::new("index out of bounds", rt)?); };
            if end < start { return xerr(ManagedXError::new("index out of bounds", rt)?); }

            let ret = string.substring(start, Some(end));
            Ok(ManagedXValue::new(XValue::String(Box::new(ret)), rt)?.into())
        }),
    )
}

pub(crate) fn add_str_code_point<W, R>(
    scope: &mut RootCompilationScope<W, R>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "code_point",
        XFuncSpec::new(&[&X_STRING], X_INT.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let s = to_primitive!(a0, String);
            let Ok(chr) = s.iter().exactly_one() else { xraise!(Err(ManagedXError::new("cannot get code_point a string without exactly one char",rt)?)) };
            Ok(ManagedXValue::new(XValue::Int(LazyBigint::from_u64(chr.into()).unwrap()), rt)?.into())
        }),
    )
}

pub(crate) fn add_str_lower<W, R>(
    scope: &mut RootCompilationScope<W, R>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "lower",
        XFuncSpec::new(&[&X_STRING], X_STRING.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let s = to_primitive!(a0, String);
            let ret = if let Some(r) = s.to_lowercase() {
                ManagedXValue::new(XValue::String(Box::new(r)), rt)?
            } else {
                a0.clone()
            };
            Ok(ret.into())
        }),
    )
}

pub(crate) fn add_str_upper<W, R>(
    scope: &mut RootCompilationScope<W, R>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "upper",
        XFuncSpec::new(&[&X_STRING], X_STRING.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let s = to_primitive!(a0, String);
            let ret = if let Some(r) = s.to_uppercase() {
                ManagedXValue::new(XValue::String(Box::new(r)), rt)?
            } else {
                a0.clone()
            };
            Ok(ret.into())
        }),
    )
}

pub(crate) fn add_str_to_int<W, R>(
    scope: &mut RootCompilationScope<W, R>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "to_int",
        XFuncSpec::new_with_optional(&[&X_STRING], &[&X_INT], X_INT.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise_opt!(args.get(1).map(|e| eval(e, ns, &rt)).transpose()?);
            let s0 = to_primitive!(a0, String);
            let i1 = match a1 {
                Some(ref a1) => Cow::Borrowed(to_primitive!(a1, Int)),
                None => Cow::Owned(LazyBigint::from(10)),
            };
            if i1.as_ref() <= &LazyBigint::from(1) {
                return xerr(ManagedXError::new("base must be larger than 1", rt)?);
            }
            if i1.as_ref() > &LazyBigint::from(36) {
                return xerr(ManagedXError::new("base must be lower than 36", rt)?);
            }
            let radix = i1.to_u32().unwrap();
            let ret = match LazyBigint::from_str_radix(s0.as_str(), radix) {
                Ok(lz) => lz,
                Err(e) => return xerr(ManagedXError::new(format!("{e}"), rt)?),
            };
            Ok(ManagedXValue::new(XValue::Int(ret), rt)?.into())
        }),
    )
}

add_binfunc!(add_str_cmp, cmp, X_STRING, String, X_INT, |a, b, _rt| Ok(
    Ok(xcmp(a, b))
));

pub(crate) fn add_str_format<W, R>(
    scope: &mut RootCompilationScope<W, R>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "format",
        XFuncSpec::new(&[&X_STRING, &X_STRING], X_STRING.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let s0 = to_primitive!(a0, String);
            let s1 = to_primitive!(a1, String);
            let Some(specs) = XFormatting::from_str(s1.as_str()) else {return xerr(ManagedXError::new("invalid format spec", rt)?);};

            if specs.precision.is_some(){
                return xerr(ManagedXError::new("str cannot be formatted with precision", rt)?);
            }
            if specs.ty.type_.is_some() || specs.ty.alternative{
                return xerr(ManagedXError::new("str cannot be formatted with type", rt)?);
            }
            if specs.grouping.is_some(){
                return xerr(ManagedXError::new("str cannot be formatted with group", rt)?);
            }
            if specs.sign_mode.is_some(){
                return xerr(ManagedXError::new("str cannot be formatted with sign", rt)?);
            }
            if specs.fill_specs.is_none(){
                return Ok(a0.into());
            }
            if let Some(FillSpecs{alignment: Some(Alignment::RightWithSign), ..}) = specs.fill_specs{
                return xerr(ManagedXError::new("str cannot be formatted with sign-sensitivity", rt)?);
            }

            rt.borrow().can_allocate_by(|| {
                Some(max(
                    s0.len(),
                    specs.min_width(),
                ))
            })?;

            let (prefix, infix, postfix) = specs
                .fill_specs
                .map(|f| f.fillers(s0.len()))
                .unwrap_or_default();
            assert!(infix.is_empty());
            let ret = XValue::String(Box::new(FencedString::from_string(format!(
                "{prefix}{s0}{postfix}"
            ))));
            Ok(ManagedXValue::new(ret, rt)?.into())
        }),
    )
}
