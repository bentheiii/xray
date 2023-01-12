use crate::builtin::core::{eval, xcmp, xerr};
use crate::xtype::{XFuncSpec, X_BOOL, X_INT, X_STRING};
use crate::xvalue::{ManagedXError, ManagedXValue, XValue};

use crate::util::lazy_bigint::LazyBigint;
use rc::Rc;
use std::borrow::Cow;
use std::collections::hash_map::DefaultHasher;

use crate::builtin::optional::{XOptional, XOptionalType};
use crate::builtin::sequence::{XSequence, XSequenceType};
use crate::compile_err::CompilationError;
use crate::root_compilation_scope::RootCompilationScope;
use crate::runtime::RTCell;
use crate::xexpr::XStaticFunction;
use crate::{add_binfunc, manage_native, to_primitive, ufunc, xraise, xraise_opt};
use itertools::Itertools;
use num_traits::{FromPrimitive, Signed, ToPrimitive};
use std::hash::{Hash, Hasher};
use std::io::Write;
use std::ops::Neg;
use std::rc;

pub(crate) fn add_str_type<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
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
    |a: &String, b: &String, rt: &RTCell<W>| {
        rt.borrow().can_allocate(a.len() + b.len())?;
        let mut ret = String::with_capacity(a.len() + b.len());
        ret.push_str(a);
        ret.push_str(b);
        Ok(Ok(XValue::String(ret)))
    }
);

pub(crate) fn add_str_hash<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "hash",
        XFuncSpec::new(&[&X_STRING], X_INT.clone()),
        ufunc!(String, |a: &String, _rt| {
            Ok(Ok(XValue::Int({
                let mut s = DefaultHasher::new();
                a.hash(&mut s);
                let hash = s.finish();
                LazyBigint::from(hash)
            })))
        }),
    )
}

pub(crate) fn add_str_len<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "len",
        XFuncSpec::new(&[&X_STRING], X_INT.clone()),
        ufunc!(String, |a: &String, _rt| {
            Ok(Ok(XValue::Int(LazyBigint::from(a.chars().count()))))
        }),
    )
}

pub(crate) fn add_str_to_str<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "to_str",
        XFuncSpec::new(&[&X_STRING], X_STRING.clone()),
        XStaticFunction::from_native(|args, ns, tca, rt| ns.eval(&args[0], rt, tca)),
    )
}

pub(crate) fn add_str_chars<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "chars",
        XFuncSpec::new(&[&X_STRING], XSequenceType::xtype(X_STRING.clone())),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let s = to_primitive!(a0, String);
            rt.borrow().can_allocate_by(|| {
                Some(if s.len() < 128 {
                    // as a shortcut, we won't check the lengths of all strings under a certain bound
                    0
                } else {
                    s.chars().count()
                })
            })?;
            let chars = s
                .chars()
                .map(|c| ManagedXValue::new(XValue::String(c.to_string()), rt.clone()))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .collect();
            Ok(manage_native!(XSequence::array(chars), rt))
        }),
    )
}

pub(crate) fn add_str_get<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "get",
        XFuncSpec::new(&[&X_STRING, &X_INT], X_STRING.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let s = to_primitive!(a0, String);
            let i = to_primitive!(a1, Int);
            let Some(char) = (if i.is_negative(){
                let Some(i) = i.clone().neg().to_usize() else { xraise!(Err(ManagedXError::new("index too large",rt)?)) };
                s.chars().nth_back(i-1)
            } else {
                let Some(i) = (i).to_usize() else { xraise!(Err(ManagedXError::new("index too large",rt)?)) };
                s.chars().nth(i)
            }) else { xraise!(Err(ManagedXError::new("index out of bounds",rt)?))};
            Ok(ManagedXValue::new(XValue::String(char.to_string()), rt)?.into())
        }),
    )
}

pub(crate) fn add_str_find<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
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
            let found_idx = string.as_str()[start_ind..]
                .find(needle)
                .map(|i| ManagedXValue::new(XValue::Int((i + start_ind).into()), rt.clone()))
                .transpose()?;
            Ok(manage_native!(XOptional { value: found_idx }, rt))
        }),
    )
}

pub(crate) fn add_str_substring<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "substring",
        XFuncSpec::new(&[&X_STRING, &X_INT, &X_INT], X_STRING.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a2 = xraise!(eval(&args[2], ns, &rt)?);
            let string = to_primitive!(a0, String);
            let Some(start) = to_primitive!(a1, Int).to_usize() else {return xerr(ManagedXError::new("index out of bounds", rt)?)};
            let raw_end = to_primitive!(a2, Int);
            let raw_end = if raw_end.is_negative() {Cow::Owned(raw_end+string.chars().count())} else {Cow::Borrowed(raw_end)};
            let Some(end) = raw_end.to_usize() else {return xerr(ManagedXError::new("index out of bounds", rt)?)};
            let ret = string.chars().take(end).skip(start).collect();
            Ok(ManagedXValue::new(XValue::String(ret), rt)?.into())
        }),
    )
}

pub(crate) fn add_str_ord<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "ord",
        XFuncSpec::new(&[&X_STRING], X_INT.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let s = to_primitive!(a0, String);
            let Ok(chr) = s.chars().exactly_one() else { xraise!(Err(ManagedXError::new("cannot ord a string without exactly one char",rt)?))};
            Ok(ManagedXValue::new(XValue::Int(LazyBigint::from_u64(chr.into()).unwrap()), rt)?.into())
        }),
    )
}

add_binfunc!(add_str_cmp, cmp, X_STRING, String, X_INT, |a, b, _rt| Ok(
    Ok(xcmp(a, b))
));
