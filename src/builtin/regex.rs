use crate::builtin::core::{eval, xerr};
use crate::native_types::{NativeType, XNativeValue};
use crate::root_runtime_scope::RuntimeResult;
use crate::runtime::RTCell;
use crate::util::fenced_string::FencedString;
use crate::xtype::{XFuncSpec, X_INT, X_STRING};
use crate::xvalue::{ManagedXError, ManagedXValue, XResult, XValue};

use crate::{
    manage_native, to_native, to_primitive, xraise, CompilationError, RootCompilationScope,
    XStaticFunction, XType,
};

use std::fmt::Debug;

use num_traits::ToPrimitive;
use regex_automata::nfa::thompson::pikevm::PikeVM;
use regex_automata::{
    hybrid::dfa::{Cache, DFA},
    Input,
};
use regex_automata::{Anchored, PatternID};
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

#[derive(Debug)]
struct Regex {
    dfa: DFA,
    pike: PikeVM,
}

impl XNativeValue for Regex {
    fn dyn_size(&self) -> usize {
        self.dfa.memory_usage() + self.pike.get_nfa().memory_usage()
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

            let dfa = match DFA::new(s0.as_str()) {
                Ok(regex) => regex,
                Err(err) => {
                    return xerr(ManagedXError::new(
                        format!("error compiling regex: {err}"),
                        rt,
                    )?)
                }
            };
            let pike = match PikeVM::new(s0.as_str()) {
                Ok(pike) => pike,
                Err(err) => {
                    return xerr(ManagedXError::new(
                        format!("error compiling regex: {err}"),
                        rt,
                    )?)
                }
            };
            let re = Regex { dfa, pike };
            Ok(manage_native!(re, rt))
        }),
    )
}

fn match_at<W, R, T>(
    dfa: &DFA,
    inp: &Input,
    cache: &mut Cache,
    rt: RTCell<W, R, T>,
    search_iter: &mut impl Iterator<Item = RuntimeResult<()>>,
) -> XResult<Option<usize>, W, R, T> {
    let mut state = match dfa.start_state_forward(cache, inp) {
        Ok(state) => state,
        Err(e) => {
            return Ok(Err(ManagedXError::new(
                format!("error creating initial state: {}", e),
                rt,
            )?))
        }
    };
    if state.is_dead() {
        return Ok(Ok(None));
    }
    if state.is_quit() {
        return Ok(Err(ManagedXError::new("dfa quit during search", rt)?));
    }
    let mut longest = if state.is_match() { Some(0) } else { None };
    for (i, &b) in inp
        .haystack()
        .iter()
        .take(inp.end())
        .skip(inp.start())
        .enumerate()
    {
        search_iter.next().unwrap()?;
        state = match dfa.next_state(cache, state, b) {
            Ok(state) => state,
            Err(e) => {
                return Ok(Err(ManagedXError::new(
                    format!("error creating next state: {}", e),
                    rt,
                )?))
            }
        };
        rt.can_allocate(cache.memory_usage())?;
        if state.is_dead() {
            return Ok(Ok(longest));
        }
        if state.is_quit() {
            return Ok(Err(ManagedXError::new("dfa quit during search", rt)?));
        }
        if state.is_match() {
            longest = Some(i);
        }
    }
    state = match dfa.next_eoi_state(cache, state) {
        Ok(state) => state,
        Err(e) => {
            return Ok(Err(ManagedXError::new(
                format!("error creating next state: {}", e),
                rt,
            )?))
        }
    };
    if state.is_quit() {
        return Ok(Err(ManagedXError::new("dfa quit during search", rt)?));
    }
    if state.is_match() {
        longest = Some(inp.haystack().len() - inp.start());
    }
    return Ok(Ok(longest));
}

pub(crate) fn add_regex_priv_match<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "__std_match",
        XFuncSpec::new(&[&X_REGEX, &X_STRING, &X_INT, &X_INT], X_MATCH.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a2 = xraise!(eval(&args[2], ns, &rt)?);
            // NOTE: this is the last possible index of the start of the match, so for example: the pattern ".*" with haystack "0123456", start 2, end 3 will match "23456"
            let a3 = xraise!(eval(&args[3], ns, &rt)?);
            let r0 = to_native!(a0, Regex);
            let s1 = to_primitive!(a1, String);
            let i2 = match to_primitive!(a2, Int).to_usize() {
                Some(i2) => i2,
                None => return xerr(ManagedXError::new("start index out of range", rt)?),
            };
            let i3 = match to_primitive!(a3, Int).to_usize() {
                Some(i3) => i3,
                None => return xerr(ManagedXError::new("end index out of range", rt)?),
            };
            let mut cache = r0.dfa.create_cache();
            let mut search_iter = rt.limits.search_iter();
            let base_inp = Input::new(s1.as_str()).anchored(Anchored::Yes);
            let (offset, len) = 'o_l: {
                for offset in i2..=i3 {
                    let inp = base_inp.clone().range(offset..);
                    if let Some(i) = xraise!(match_at(
                        &r0.dfa,
                        &inp,
                        &mut cache,
                        rt.clone(),
                        &mut search_iter
                    )?) {
                        break 'o_l (offset, i);
                    }
                }
                return Ok(manage_native!(XOptional::<W, R, T> { value: None }, rt));
            };
            let inp = base_inp.range(offset..offset + len);
            let mut captures = r0.pike.create_captures();
            let mut cache = r0.pike.create_cache();
            r0.pike.captures(&mut cache, inp, &mut captures);
            if !captures.is_match() {
                return xerr(ManagedXError::new("dfa found match but pike did not", rt)?);
            }
            if captures.get_match().unwrap().len() != len {
                return xerr(ManagedXError::new(
                    "pike match is smaller than dfa match",
                    rt,
                )?);
            }
            let mut pairs = Vec::with_capacity(captures.group_len());
            for sub_cap in captures.iter() {
                pairs.push(match sub_cap {
                    None => manage_native!(XOptional::<W, R, T> { value: None }, rt.clone()),
                    Some(m) => {
                        let start =
                            ManagedXValue::new(XValue::Int(LazyBigint::from(m.start)), rt.clone())?;
                        let end =
                            ManagedXValue::new(XValue::Int(LazyBigint::from(m.end)), rt.clone())?;
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

pub(crate) fn add_regex_prov_group_names<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "__std_group_names",
        XFuncSpec::new(
            &[&X_REGEX],
            XSequenceType::xtype(XOptionalType::xtype(X_STRING.clone())),
        ),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let r0 = to_native!(a0, Regex);
            let base: Vec<Option<&str>> = r0
                .pike
                .get_nfa()
                .group_info()
                .pattern_names(PatternID::ZERO)
                .collect();
            let mut out = Vec::with_capacity(base.len());
            for g_name in base {
                out.push(match g_name {
                    None => manage_native!(XOptional::<W, R, T> { value: None }, rt.clone()),
                    Some(g_name) => {
                        let g_name = ManagedXValue::new(
                            XValue::String(Box::new(FencedString::from_str(g_name))),
                            rt.clone(),
                        )?;
                        manage_native!(
                            XOptional::<W, R, T> {
                                value: Some(g_name)
                            },
                            rt.clone()
                        )
                    }
                })
            }
            Ok(manage_native!(XSequence::array(out), rt.clone()))
        }),
    )
}
