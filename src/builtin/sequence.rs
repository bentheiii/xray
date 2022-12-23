use crate::builtin::core::{eval, get_func, search, unpack_native, xerr};
use crate::builtin::optional::{XOptional, XOptionalType};
use crate::builtin::stack::{XStack, XStackType};
use crate::native_types::{NativeType, XNativeValue};
use crate::root_runtime_scope::EvaluatedValue;
use crate::runtime_scope::RuntimeScope;
use crate::runtime_violation::RuntimeViolation;
use crate::util::trysort::try_sort;
use crate::xtype::{XFuncSpec, X_BOOL, X_INT};
use crate::xvalue::{ManagedXError, ManagedXValue, XFunction, XFunctionFactoryOutput, XValue};
use crate::XType::XCallable;
use crate::{
    forward_err, manage_native, to_native, to_primitive, unpack_types, xraise, xraise_opt,
    CompilationError, RTCell, RootCompilationScope, XCallableSpec, XStaticFunction, XType,
};
use derivative::Derivative;
use either::Either;
use num_traits::{One, Signed, ToPrimitive, Zero};
use rc::Rc;
use std::borrow::Cow;
use std::fmt::Debug;
use std::io::Write;
use std::mem::size_of;
use std::ops::Neg;
use std::{iter, rc};
use std::sync::Arc;

use crate::util::lazy_bigint::LazyBigint;
use crate::util::try_extend::TryExtend;

#[derive(Debug, Clone)]
pub(crate) struct XSequenceType;

impl XSequenceType {
    pub(crate) fn xtype(t: Arc<XType>) -> Arc<XType> {
        Arc::new(XType::XNative(Box::new(Self {}), vec![t]))
    }
}

impl NativeType for XSequenceType {
    fn generic_names(&self) -> Vec<String> {
        vec!["T".to_string()]
    }
    fn name(&self) -> &str {
        "Sequence"
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub enum XSequence<W: Write + 'static> {
    Empty,
    Array(Vec<Rc<ManagedXValue<W>>>),
    Range(i64, i64, i64),
    Map(Rc<ManagedXValue<W>>, Rc<ManagedXValue<W>>),
    // todo store the length?
    Zip(Vec<Rc<ManagedXValue<W>>>),
    // we are guaranteed that
    // a. there are at least two parts,
    // b. all parts except (possibly) the last are finite
    // c. there is exactly one more part than midpoints
    // d. none of the parts are empty
    Chain { parts: Vec<Rc<ManagedXValue<W>>>, midpoint_lengths: Vec<usize> },
    // end is always at most the length of the sequence, start is always lower than end
    // end = None indicates an infinite sequence
    Slice(Rc<ManagedXValue<W>>, usize, Option<usize>),
    Count,
}

impl<W: Write + 'static> XSequence<W> {
    pub(crate) fn array(value: Vec<Rc<ManagedXValue<W>>>) -> Self {
        if value.is_empty() {
            Self::Empty
        } else {
            Self::Array(value)
        }
    }

    pub(super) fn len(&self) -> Option<usize> {
        Some(match self {
            Self::Empty => 0,
            Self::Array(arr) => arr.len(),
            Self::Range(start, end, step) => {
                if step.is_positive() && start < end {
                    (1 + (end - 1 - start) / step) as usize
                } else {
                    assert!(step.is_negative() && start > end);
                    (1 + (start - 1 - end) / -step) as usize
                }
            }
            Self::Map(seq, ..) => to_native!(seq, Self).len()?,
            Self::Zip(sequences) => sequences
                .iter()
                .map(|seq| to_native!(seq, Self))
                .filter_map(|seq| seq.len())
                .min()?,
            Self::Slice(_, start, end) => (*end)? - start,
            Self::Count => None?,
            Self::Chain { parts, midpoint_lengths } =>
                to_native!(parts.last().unwrap(), Self).len()? + midpoint_lengths.last().unwrap()
        })
    }

    pub(super) fn get(
        &self,
        idx: usize,
        ns: &RuntimeScope<W>,
        rt: RTCell<W>,
    ) -> Result<EvaluatedValue<W>, RuntimeViolation> {
        match self {
            Self::Empty => unreachable!(),
            Self::Array(arr) => Ok(Ok(arr[idx].clone())),
            Self::Range(start, _, step) => {
                let v = LazyBigint::from(*start) + LazyBigint::from(idx) * LazyBigint::from(*step);
                ManagedXValue::new(XValue::Int(v), rt).map(Ok)
            }
            Self::Map(seq, func) => {
                let original = to_native!(seq, Self).get(idx, ns, rt.clone())?;
                let f = to_primitive!(func, Function);
                ns.eval_func_with_values(f, vec![original], rt, false)
                    .map(|e| Ok(e.unwrap_value()))?
            }
            Self::Zip(sequences) => {
                let items = forward_err!(sequences
                    .iter()
                    .map(|seq| to_native!(seq, Self).get(idx, ns, rt.clone()))
                    .collect::<Result<Result<Vec<_>, _>, _>>()?);
                ManagedXValue::new(XValue::StructInstance(items), rt).map(Ok)
            }
            Self::Slice(seq, start, ..) => to_native!(seq, Self).get(idx + start, ns, rt),
            Self::Count => ManagedXValue::new(XValue::Int(idx.into()), rt).map(Ok),
            Self::Chain { parts, midpoint_lengths } => {
                let part_idx = midpoint_lengths.partition_point(|x| *x <= idx);
                let internal_idx = idx - if part_idx == 0 {
                    0
                } else {
                    midpoint_lengths[part_idx - 1]
                };
                to_native!(parts[part_idx], Self).get(internal_idx, ns, rt)
            }
        }
    }

    pub(super) fn diter<'a>(
        &'a self,
        ns: &'a RuntimeScope<W>,
        rt: RTCell<W>,
    ) -> Option<impl DoubleEndedIterator<Item=Result<EvaluatedValue<W>, RuntimeViolation>> + 'a>
    {

        Some(
            match self{
                XSequence::Array(arr) => Either::Left(arr.iter().cloned().map(|i| Ok(Ok(i)))),
                _ => Either::Right((0..self.len()?).map(move |idx| self.get(idx, ns, rt.clone())))
            }
        )
    }

    pub(super) fn iter<'a>(
        &'a self,
        ns: &'a RuntimeScope<W>,
        rt: RTCell<W>,
    ) -> impl Iterator<Item=Result<EvaluatedValue<W>, RuntimeViolation>> + 'a {
        self.diter(ns, rt.clone()).map_or_else(
            || Either::Left((0..).map(move |idx| self.get(idx, ns, rt.clone()))),
            Either::Right,
        )
    }

    pub(crate) fn slice(
        base: &Rc<ManagedXValue<W>>,
        start: usize,
        mut end: Option<usize>,
    ) -> Option<Self> // todo return an error with a reference instead?
    {
        let self_ = to_native!(base, XSequence<W>);
        let len = self_.len();
        if end.map_or(true, |end| len.map_or(false, |len| end >= len)) {
            if start == 0 {
                return None;
            }
            end = len;
        }
        if end.map_or(false, |end| start >= end) || len.map_or(false, |len| start >= len) {
            return Some(Self::Empty);
        }
        Some(match self_ {
            Self::Slice(origin, old_start, ..) => Self::Slice(
                origin.clone(),
                old_start + start,
                end.map(|end| old_start + end),
            ),
            _ => Self::Slice(base.clone(), start, end),
        })
    }

    pub(crate) fn chain<'a>(
        base0: &'a Rc<ManagedXValue<W>>,
        base1: &'a Rc<ManagedXValue<W>>,
    ) -> Result<Result<Self, &'a Rc<ManagedXValue<W>>>, &'static str> {
        let seq0 = to_native!(base0, XSequence<W>);
        let seq1 = to_native!(base1, XSequence<W>);
        if seq0.is_empty(){
            return if seq1.is_empty(){
                Ok(Ok(XSequence::Empty))
            } else {
                Ok(Err(base1))
            }
        } else if seq1.is_empty(){
            return Ok(Err(base0))
        }
        let Some(len0) = seq0.len() else {return Err("first sequence is infinite")};
        let (parts, midpoint_lengths) = match (seq0, seq1) {
            (XSequence::Chain {parts: parts0, midpoint_lengths:mid_lengths0}, XSequence::Chain {parts:parts1, midpoint_lengths:mid_lengths1}) => {
                let parts = parts0.iter().chain(parts1).cloned().collect();
                let midpoint_lengths = mid_lengths0.iter().cloned().chain(iter::once(len0)).chain(mid_lengths1.iter().map(|len| len + len0)).collect();
                (parts, midpoint_lengths)
            }

            (XSequence::Chain {parts: parts0, midpoint_lengths:mid_lengths0}, _) => {
                let parts = parts0.iter().chain(iter::once(base1)).cloned().collect();
                let midpoint_lengths = mid_lengths0.iter().cloned().chain(iter::once(len0)).collect();
                (parts, midpoint_lengths)
            }

            (_, XSequence::Chain {parts: parts1, midpoint_lengths:mid_lengths1}) => {
                let parts = iter::once(base0).chain(parts1.iter()).cloned().collect();
                let midpoint_lengths = iter::once(len0).chain(mid_lengths1.iter().map(|len| len + len0)).collect();
                (parts, midpoint_lengths)
            }

            (_, _) => {
                let parts = vec![base0.clone(), base1.clone()];
                let midpoint_lengths = vec![len0];
                (parts, midpoint_lengths)
            }
        };
        Ok(Ok(XSequence::Chain {parts, midpoint_lengths}))
    }

    pub(super) fn is_empty(&self) -> bool {
        matches!(self, Self::Empty)
    }

    fn sorted(
        &self,
        cmp_func: &XFunction<W>,
        ns: &RuntimeScope<W>,
        rt: RTCell<W>,
    ) -> Result<Result<Option<Self>, Rc<ManagedXError<W>>>, RuntimeViolation> {
        let arr = forward_err!(self
            .iter(ns, rt.clone())
            .collect::<Result<Result<Vec<_>, _>, _>>()?);
        // first we check if the seq is already sorted
        let mut is_sorted = true;
        for w in arr.windows(2) {
            if to_primitive!(
                match ns
                    .eval_func_with_values(
                        cmp_func,
                        vec![Ok(w[0].clone()), Ok(w[1].clone())],
                        rt.clone(),
                        false
                    )?
                    .unwrap_value()
                {
                    Ok(i) => i,
                    Err(e) => return Ok(Err(e)),
                },
                Int
            )
                .is_positive()
            {
                is_sorted = false;
                break;
            }
        }
        if is_sorted {
            Ok(Ok(None))
        } else {
            let mut ret = arr;
            forward_err!(try_sort(
                &mut ret,
                |a, b| -> Result<Result<_, Rc<ManagedXError<W>>>, RuntimeViolation> {
                    Ok(Ok(to_primitive!(
                        forward_err!(ns
                            .eval_func_with_values(
                                cmp_func,
                                vec![Ok(a.clone()), Ok(b.clone())],
                                rt.clone(),
                                false
                            )?
                            .unwrap_value()),
                        Int
                    )
                    .is_negative()))
                }
            )?);
            Ok(Ok(Some(XSequence::array(ret))))
        }
    }

    fn value_to_idx(
        &self,
        i: &LazyBigint,
        rt: RTCell<W>,
    ) -> Result<Result<usize, Rc<ManagedXError<W>>>, RuntimeViolation> {
        let mut i = Cow::Borrowed(i);
        let len = self.len();
        if i.is_negative() {
            let Some(len) = len else {
                return Ok(Err(ManagedXError::new("cannot get negative index of infinite sequence", rt)?));
            };
            i = Cow::Owned(i.into_owned() + LazyBigint::from(len));
            if i.is_negative() {
                return Ok(Err(ManagedXError::new("index too low", rt)?));
            }
        };
        let Some(idx) = i.to_usize() else { return Ok(Err(ManagedXError::new("index out of bounds", rt)?)); };
        if len.map_or(false, |len| idx >= len) {
            return Ok(Err(ManagedXError::new("index out of bounds", rt)?));
        }
        Ok(Ok(idx))
    }
}

impl<W: Write + 'static> XNativeValue for XSequence<W> {
    fn size(&self) -> usize {
        match self {
            Self::Empty => size_of::<usize>(),
            Self::Array(arr) => arr.len() * size_of::<usize>(),
            Self::Zip(arr) => arr.len() * size_of::<usize>(),
            Self::Chain{parts, ..} => (parts.len()*2 - 1) * size_of::<usize>(),
            Self::Range(..) => 3 * size_of::<i64>(),
            Self::Map(..) => 2 * size_of::<usize>(),
            Self::Slice(..) => 3 * size_of::<usize>(),
            Self::Count => size_of::<usize>(),
        }
    }
}

pub(crate) fn add_sequence_type<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], _) = scope.generics_from_names(["T"]);
    scope.add_native_type("Sequence", XSequenceType::xtype(t))
}

pub(crate) fn add_sequence_get<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    scope.add_func(
        "get",
        XFuncSpec::new(&[&XSequenceType::xtype(t.clone()), &X_INT], t).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let arr = &to_native!(a0, XSequence<W>);
            let idx = to_primitive!(a1, Int);
            let idx = xraise!(arr.value_to_idx(idx, rt.clone())?);
            Ok(arr.get(idx, ns, rt)?.into())
        }),
    )
}

pub(crate) fn add_sequence_len<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);

    scope.add_func(
        "len",
        XFuncSpec::new(&[&XSequenceType::xtype(t)], X_INT.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let arr = &to_native!(a0, XSequence<W>);
            let Some(len) = arr.len() else { return xerr(ManagedXError::new("sequence is infinite", rt)?); };
            Ok(ManagedXValue::new(XValue::Int(len.into()), rt)?.into())
        }),
    )
}

pub(crate) fn add_sequence_add<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t);

    scope.add_func(
        "add",
        XFuncSpec::new(&[&t_arr, &t_arr], t_arr.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);

            match XSequence::chain(&a0, &a1){
                Err(s) => xerr(ManagedXError::new(s, rt)?),
                Ok(Err(v)) => Ok(v.clone().into()),
                Ok(Ok(s)) => Ok(manage_native!(s, rt))
            }
        }),
    )
}

pub(crate) fn add_sequence_add_stack<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t.clone());
    let t_stack = XStackType::xtype(t);

    scope.add_func(
        "add",
        XFuncSpec::new(&[&t_arr, &t_stack], t_arr.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let seq0 = to_native!(a0, XSequence<W>);
            let stk1 = to_native!(a1, XStack<W>);
            if stk1.length == 0 {
                Ok(a0.clone().into())
            } else {
                let Some(len0) = seq0.len() else { return xerr(ManagedXError::new("sequence is infinite", rt)?); };
                rt.as_ref().borrow().can_allocate(len0 + stk1.length)?;
                let mut arr = xraise!(seq0
                    .iter(ns, rt.clone())
                    .collect::<Result<Result<Vec<_>, _>, _>>()?);
                arr.reserve_exact(stk1.length);
                for v in stk1.iter() {
                    arr.push(v.clone());
                }
                Ok(manage_native!(XSequence::array(arr), rt))
            }
        }),
    )
}

pub(crate) fn add_sequence_addrev_stack<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t.clone());
    let t_stack = XStackType::xtype(t);

    scope.add_func(
        "add_rev",
        XFuncSpec::new(&[&t_arr, &t_stack], t_arr.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let seq0 = to_native!(a0, XSequence<W>);
            let stk1 = to_native!(a1, XStack<W>);
            if stk1.length == 0 {
                Ok(a0.clone().into())
            } else {
                let Some(len0) = seq0.len() else { return xerr(ManagedXError::new("sequence is infinite", rt)?); };
                rt.as_ref().borrow().can_allocate(len0 + stk1.length)?;
                let mut arr = xraise!(seq0
                    .iter(ns, rt.clone())
                    .collect::<Result<Result<Vec<_>, _>, _>>()?);
                let original_len = arr.len();
                arr.reserve_exact(stk1.length);
                for v in stk1.iter() {
                    arr.push(v.clone());
                }
                arr[original_len..].reverse();
                Ok(manage_native!(XSequence::array(arr), rt))
            }
        }),
    )
}

pub(crate) fn add_sequence_push<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t.clone());

    scope.add_func(
        "push",
        XFuncSpec::new(&[&t_arr, &t], t_arr.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let seq0 = to_native!(a0, XSequence<W>);
            let Some(len0) = seq0.len() else { return xerr(ManagedXError::new("sequence is infinite", rt)?); };
            rt.as_ref().borrow().can_allocate(len0 + 1)?;
            let mut arr = xraise!(seq0
                .iter(ns, rt.clone())
                .collect::<Result<Result<Vec<_>, _>, _>>()?);
            arr.push(a1);
            Ok(manage_native!(XSequence::array(arr), rt))
        }),
    )
}

pub(crate) fn add_sequence_rpush<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t.clone());

    scope.add_func(
        "rpush",
        XFuncSpec::new(&[&t_arr, &t], t_arr.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let seq0 = to_native!(a0, XSequence<W>);
            let Some(len0) = seq0.len() else { return xerr(ManagedXError::new("sequence is infinite", rt)?); };
            rt.as_ref().borrow().can_allocate(len0 + 1)?;
            let mut arr = vec![a1];
            xraise!(arr.try_extend(seq0.iter(ns, rt.clone()))?);
            Ok(manage_native!(XSequence::array(arr), rt))
        }),
    )
}

pub(crate) fn add_sequence_insert<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t.clone());

    scope.add_func(
        "insert",
        XFuncSpec::new(&[&t_arr, &X_INT, &t], t_arr.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a2 = xraise!(eval(&args[2], ns, &rt)?);
            let seq0 = to_native!(a0, XSequence<W>);
            let Some(len0) = seq0.len() else { return xerr(ManagedXError::new("sequence is infinite", rt)?); };
            rt.as_ref().borrow().can_allocate(len0 + 1)?;
            let idx = to_primitive!(a1, Int);
            let idx = xraise!(seq0.value_to_idx(idx, rt.clone())?);
            let mut ret = xraise!(seq0
                .iter(ns, rt.clone())
                .take(idx)
                .collect::<Result<Result<Vec<_>, _>, _>>()?);
            ret.push(a2);
            xraise!(ret.try_extend(seq0.iter(ns, rt.clone()).skip(idx))?);
            Ok(manage_native!(XSequence::array(ret), rt))
        }),
    )
}

pub(crate) fn add_sequence_pop<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t);

    scope.add_func(
        "pop",
        XFuncSpec::new(&[&t_arr, &X_INT], t_arr.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let seq0 = to_native!(a0, XSequence<W>);
            let Some(len0) = seq0.len() else { return xerr(ManagedXError::new("sequence is infinite", rt)?); };
            rt.as_ref().borrow().can_allocate(len0 - 1)?;
            let idx = to_primitive!(a1, Int);
            let idx = xraise!(seq0.value_to_idx(idx, rt.clone())?);
            if len0 == 1 {
                return Ok(manage_native!(XSequence::<W>::Empty, rt));
            }
            let mut ret = xraise!(seq0
                .iter(ns, rt.clone())
                .take(idx)
                .collect::<Result<Result<Vec<_>, _>, _>>()?);
            xraise!(ret.try_extend(seq0.iter(ns, rt.clone()).skip(idx + 1))?);
            Ok(manage_native!(XSequence::array(ret), rt))
        }),
    )
}

pub(crate) fn add_sequence_set<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t.clone());

    scope.add_func(
        "set",
        XFuncSpec::new(&[&t_arr, &X_INT, &t], t_arr.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a2 = xraise!(eval(&args[2], ns, &rt)?);
            let seq0 = to_native!(a0, XSequence<W>);
            let Some(len0) = seq0.len() else { return xerr(ManagedXError::new("sequence is infinite", rt)?); };
            rt.as_ref().borrow().can_allocate(len0)?;
            let idx = to_primitive!(a1, Int);
            let idx = xraise!(seq0.value_to_idx(idx, rt.clone())?);
            let mut ret = xraise!(seq0
                .iter(ns, rt.clone())
                .take(idx)
                .collect::<Result<Result<Vec<_>, _>, _>>()?);
            ret.push(a2);
            xraise!(ret.try_extend(seq0.iter(ns, rt.clone()).skip(idx + 1),)?);
            Ok(manage_native!(XSequence::array(ret), rt))
        }),
    )
}

pub(crate) fn add_sequence_swap<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t);

    scope.add_func(
        "swap",
        XFuncSpec::new(&[&t_arr, &X_INT, &X_INT], t_arr.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a2 = xraise!(eval(&args[2], ns, &rt)?);
            let seq0 = to_native!(a0, XSequence<W>);
            let Some(len0) = seq0.len() else { return xerr(ManagedXError::new("sequence is infinite", rt)?); };
            rt.as_ref().borrow().can_allocate(len0)?;
            let idx1 = to_primitive!(a1, Int);
            let idx2 = to_primitive!(a2, Int);
            let mut idx1 = xraise!(seq0.value_to_idx(idx1, rt.clone())?);
            let mut idx2 = xraise!(seq0.value_to_idx(idx2, rt.clone())?);
            if idx1 == idx2 {
                return Ok(a0.clone().into());
            }
            if idx1 > idx2 {
                (idx1, idx2) = (idx2, idx1);
            }
            let mut ret = xraise!(seq0
                .iter(ns, rt.clone())
                .take(idx1)
                .collect::<Result<Result<Vec<_>, _>, _>>()?);
            ret.push(xraise!(seq0.get(idx2, ns, rt.clone())?));
            xraise!(ret.try_extend(seq0.iter(ns, rt.clone()).take(idx2).skip(idx1 + 1))?);
            ret.push(xraise!(seq0.get(idx1, ns, rt.clone())?));
            xraise!(ret.try_extend(seq0.iter(ns, rt.clone()).skip(idx2 + 1))?);
            Ok(manage_native!(XSequence::array(ret), rt))
        }),
    )
}

pub(crate) fn add_sequence_to_stack<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);

    scope.add_func(
        "to_stack",
        XFuncSpec::new(&[&XSequenceType::xtype(t.clone())], XStackType::xtype(t)).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let seq0 = to_native!(a0, XSequence<W>);
            let Some(len0) = seq0.len() else { return xerr(ManagedXError::new("sequence is infinite", rt)?); };
            rt.as_ref().borrow().can_allocate(len0)?;
            let mut ret = XStack::new();
            for x in seq0.iter(ns, rt.clone()) {
                ret = ret.push(xraise!(x?));
            }
            Ok(manage_native!(ret, rt))
        }),
    )
}

pub(crate) fn add_sequence_map<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([input_t, output_t], params) = scope.generics_from_names(["T_IN", "T_OUT"]);

    scope.add_func(
        "map",
        XFuncSpec::new(
            &[
                &XSequenceType::xtype(input_t.clone()),
                &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![input_t],
                    return_type: output_t.clone(),
                })),
            ],
            XSequenceType::xtype(output_t),
        )
            .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            Ok(manage_native!(XSequence::Map(a0, a1), rt))
        }),
    )
}

pub(crate) fn add_sequence_to_array<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);

    scope.add_func(
        "to_array",
        XFuncSpec::new(&[&XSequenceType::xtype(t.clone())], XSequenceType::xtype(t))
            .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let seq0 = to_native!(a0, XSequence<W>);
            if let XSequence::Array(..) = seq0 {
                return Ok(a0.into());
            }
            let Some(len0) = seq0.len() else { return xerr(ManagedXError::new("sequence is infinite", rt)?); };
            rt.as_ref().borrow().can_allocate(len0)?;
            let ret = xraise!(seq0
                .iter(ns, rt.clone())
                .collect::<Result<Result<Vec<_>, _>, _>>()?);
            Ok(manage_native!(XSequence::array(ret), rt))
        }),
    )
}

pub(crate) fn add_sequence_sort<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t.clone());

    scope.add_func(
        "sort",
        XFuncSpec::new(
            &[
                &t_arr,
                &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![t.clone(), t],
                    return_type: X_INT.clone(),
                })),
            ],
            t_arr.clone(),
        )
            .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let seq0 = to_native!(a0, XSequence<W>);
            let Some(len0) = seq0.len() else { return xerr(ManagedXError::new("sequence is infinite", rt)?); };
            rt.as_ref().borrow().can_allocate(len0)?;
            let f = to_primitive!(a1, Function);
            xraise!(seq0.sorted(f, ns, rt.clone())?)
                .map_or_else(|| Ok(a0.clone().into()), |s| Ok(manage_native!(s, rt)))
        }),
    )
}

pub(crate) fn add_sequence_reduce3<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t, s], params) = scope.generics_from_names(["T", "S"]);
    let t_arr = XSequenceType::xtype(t.clone());

    scope.add_func(
        "reduce",
        XFuncSpec::new(
            &[
                &t_arr,
                &s,
                &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![s.clone(), t],
                    return_type: s.clone(),
                })),
            ],
            s.clone(),
        )
            .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a2 = xraise!(eval(&args[2], ns, &rt)?);
            let a1 = eval(&args[1], ns, &rt)?;
            let seq0 = to_native!(a0, XSequence<W>);
            if seq0.len().is_none() {
                return xerr(ManagedXError::new("sequence is infinite", rt)?);
            }
            let f = to_primitive!(a2, Function);
            let mut ret = a1;
            let arr = seq0.iter(ns, rt.clone());
            for i in arr {
                ret = ns
                    .eval_func_with_values(f, vec![ret, i?], rt.clone(), false)
                    .map(|i| i.unwrap_value())?;
            }
            Ok(ret.into())
        }),
    )
}

pub(crate) fn add_sequence_reduce2<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t.clone());

    scope.add_func(
        "reduce",
        XFuncSpec::new(
            &[
                &t_arr,
                &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![t.clone(), t.clone()],
                    return_type: t.clone(),
                })),
            ],
            t,
        )
            .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let seq0 = to_native!(a0, XSequence<W>);
            if seq0.is_empty() {
                return xerr(ManagedXError::new("sequence is empty", rt)?);
            }
            if seq0.len().is_none() {
                return xerr(ManagedXError::new("sequence is infinite", rt)?);
            }
            let f = to_primitive!(a1, Function);
            let mut ret = seq0.get(0, ns, rt.clone())?;
            let arr = seq0.iter(ns, rt.clone()).skip(1);
            for i in arr {
                ret = ns
                    .eval_func_with_values(f, vec![ret, i?], rt.clone(), false)
                    .map(|i| i.unwrap_value())?;
            }
            Ok(ret.into())
        }),
    )
}

pub(crate) fn add_sequence_range<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let t_arr = XSequenceType::xtype(X_INT.clone());

    scope.add_func(
        "range",
        XFuncSpec::new_with_optional(&[&X_INT], &[&X_INT, &X_INT], t_arr),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let (start, end, step);
            if args.len() == 1 {
                let a0 = xraise!(eval(&args[0], ns, &rt)?);
                let Some(end0) = to_primitive!(a0, Int).to_i64() else { return xerr(ManagedXError::new("end out of bounds", rt)?); };
                end = end0;
                start = 0i64;
                step = 1i64;
            } else {
                let a0 = xraise!(eval(&args[0], ns, &rt)?);
                let a1 = xraise!(eval(&args[1], ns, &rt)?);
                let a2 = xraise_opt!(args.get(2).map(|e| eval(e, ns, &rt)).transpose()?);
                let Some(start0) = to_primitive!(a0, Int).to_i64() else { return xerr(ManagedXError::new("start out of bounds", rt)?); };
                let Some(end0) = to_primitive!(a1, Int).to_i64() else { return xerr(ManagedXError::new("end out of bounds", rt)?); };
                let Some(step0) = a2.map_or(Some(1i64), |a2| { to_primitive!(a2, Int).to_i64() }) else { return xerr(ManagedXError::new("step out of bounds", rt)?); };
                start = start0;
                end = end0;
                step = step0;
            }
            if step.is_zero() {
                xerr(ManagedXError::new("invalid range, step size cannot be zero", rt)?)
            } else if (step.is_positive() && start >= end) || (step.is_negative() && start <= end) {
                Ok(manage_native!(XSequence::<W>::Empty, rt))
            } else {
                Ok(manage_native!(XSequence::<W>::Range(start, end, step), rt))
            }
        }),
    )
}

pub(crate) fn add_sequence_filter<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t.clone());

    scope.add_func(
        "filter",
        XFuncSpec::new(
            &[
                &t_arr,
                &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![t],
                    return_type: X_BOOL.clone(),
                })),
            ],
            t_arr.clone(),
        )
            .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let seq0 = to_native!(a0, XSequence<W>);
            if seq0.len().is_none() {
                return xerr(ManagedXError::new("sequence is infinite", rt)?);
            }
            let f = to_primitive!(a1, Function);
            // first we check if the seq already fully_matches
            let mut first_drop_idx = None; // if this is not none, it is the first index we need to drop
            let mut items = seq0.iter(ns, rt.clone());
            let mut ret = Vec::new();
            for (i, item) in items.by_ref().enumerate() {
                let item = xraise!(item?);
                let res = xraise!(ns
                    .eval_func_with_values(f, vec![Ok(item.clone())], rt.clone(), false)?
                    .unwrap_value());
                if !*to_primitive!(res, Bool) {
                    first_drop_idx = Some(i);
                    break;
                }
                ret.push(item);
                rt.as_ref().borrow().can_afford(&ret)?;
            }
            if first_drop_idx.is_some() {
                for item in items {
                    let item = xraise!(item?);
                    let res = xraise!(ns
                        .eval_func_with_values(f, vec![Ok(item.clone())], rt.clone(), false)?
                        .unwrap_value());
                    if *to_primitive!(res, Bool) {
                        ret.push(item);
                        rt.as_ref().borrow().can_afford(&ret)?;
                    }
                }
                Ok(manage_native!(XSequence::array(ret), rt))
            } else {
                // no indices need to drop, we can just return the sequence
                Ok(a0.clone().into())
            }
        }),
    )
}

pub(crate) fn add_sequence_nth<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t.clone());

    scope.add_func(
        "nth",
        XFuncSpec::new(
            &[
                &t_arr,
                &X_INT,
                &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![t.clone()],
                    return_type: X_BOOL.clone(),
                })),
            ],
            XOptionalType::xtype(t),
        )
            .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a2 = xraise!(eval(&args[2], ns, &rt)?);
            let seq0 = to_native!(a0, XSequence<W>);
            let mut matches_left = to_primitive!(a1, Int).clone();
            let arr = if matches_left.is_negative() {
                let Some(original_iter) = seq0.diter(ns, rt.clone()) else { return xerr(ManagedXError::new("negative match index cannot be used with infinite sequence", rt)?); };
                matches_left = matches_left.neg() - One::one();
                Either::Left(original_iter.rev())
            } else {
                Either::Right(seq0.iter(ns, rt.clone()))
            };
            let f = to_primitive!(a2, Function);
            for (item, search_lim) in search(arr, rt.clone()) {
                search_lim?;
                let item = xraise!(item?);
                if *to_primitive!(
                    xraise!(ns
                        .eval_func_with_values(f, vec![Ok(item.clone())], rt.clone(), false)?
                        .unwrap_value()),
                    Bool
                ) {
                    if matches_left.is_zero() {
                        return Ok(manage_native!(XOptional { value: Some(item) }, rt));
                    }
                    matches_left = matches_left - One::one();
                }
            }
            Ok(manage_native!(XOptional::<W> { value: None }, rt))
        }),
    )
}

pub(crate) fn add_sequence_take_while<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t.clone());

    scope.add_func(
        "take_while",
        XFuncSpec::new(
            &[
                &t_arr,
                &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![t],
                    return_type: X_BOOL.clone(),
                })),
            ],
            t_arr.clone(),
        )
            .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let seq = to_native!(a0, XSequence<W>);
            let arr = seq.iter(ns, rt.clone());
            let f = to_primitive!(a1, Function);
            let mut end_idx = seq.len();
            for ((i, item), search) in search(arr.enumerate(), rt.clone()) {
                search?;
                let item = item?;
                if !*to_primitive!(
                    xraise!(ns
                        .eval_func_with_values(f, vec![item.clone()], rt.clone(), false)?
                        .unwrap_value()),
                    Bool
                ) {
                    end_idx = Some(i);
                    break;
                }
            }
            Ok(match XSequence::slice(&a0, 0, end_idx) {
                None => a0.into(),
                Some(ret) => manage_native!(ret, rt),
            })
        }),
    )
}

pub(crate) fn add_sequence_skip_until<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t.clone());

    scope.add_func(
        "skip_until",
        XFuncSpec::new(
            &[
                &t_arr,
                &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![t],
                    return_type: X_BOOL.clone(),
                })),
            ],
            t_arr.clone(),
        )
            .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let seq = to_native!(a0, XSequence<W>);
            let arr = seq.iter(ns, rt.clone());
            let f = to_primitive!(a1, Function);
            // if the sequence is infinite, then we need a match anyway
            let mut start_idx = seq.len().unwrap_or(0);
            for ((i, item), search) in search(arr.enumerate(), rt.clone()) {
                search?;
                let item = item?;
                if *to_primitive!(
                    xraise!(ns
                        .eval_func_with_values(f, vec![item.clone()], rt.clone(), false)?
                        .unwrap_value()),
                    Bool
                ) {
                    start_idx = i;
                    break;
                }
            }
            Ok(match XSequence::slice(&a0, start_idx, None) {
                None => a0.into(),
                Some(ret) => manage_native!(ret, rt),
            })
        }),
    )
}

pub(crate) fn add_sequence_take<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t);

    scope.add_func(
        "take",
        XFuncSpec::new(&[&t_arr, &X_INT], t_arr.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let Some(end_idx) = to_primitive!(a1, Int).to_usize() else { return xerr(ManagedXError::new("index too large", rt)?); };
            Ok(match XSequence::slice(&a0, 0, Some(end_idx)) {
                None => a0.into(),
                Some(ret) => manage_native!(ret, rt)
            })
        }),
    )
}

pub(crate) fn add_sequence_skip<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t);

    scope.add_func(
        "skip",
        XFuncSpec::new(&[&t_arr, &X_INT], t_arr.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let Some(start_idx) = to_primitive!(a1, Int).to_usize() else { return xerr(ManagedXError::new("index too large", rt)?); };
            Ok(match XSequence::slice(&a0, start_idx, None) {
                None => a0.into(),
                Some(ret) => manage_native!(ret, rt)
            })
        }),
    )
}

pub(crate) fn add_sequence_count<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "count",
        XFuncSpec::new(&[], XSequenceType::xtype(X_INT.clone())),
        XStaticFunction::from_native(|_args, _ns, _tca, rt| {
            Ok(manage_native!(XSequence::<W>::Count, rt))
        }),
    )
}

pub(crate) fn add_sequence_dyn_eq<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let eq_symbol = scope.identifier("eq");

    scope.add_dyn_func("eq", "sequences", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let (a0, a1) = unpack_types!(types, 0, 1);
        let [t0] = unpack_native(a0, "Sequence")? else { unreachable!() };
        let [t1] = unpack_native(a1, "Sequence")? else { unreachable!() };

        let inner_eq = get_func(ns, eq_symbol, &[t0.clone(), t1.clone()], &X_BOOL)?;

        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(
                &[
                    &XSequenceType::xtype(t0.clone()),
                    &XSequenceType::xtype(t1.clone()),
                ],
                X_BOOL.clone(),
            ),
            move |args, ns, _tca, rt| {
                let a0 = xraise!(eval(&args[0], ns, &rt)?);
                let a1 = xraise!(eval(&args[1], ns, &rt)?);
                let seq0 = to_native!(a0, XSequence<W>);
                let seq1 = to_native!(a1, XSequence<W>);
                if seq0.len() != seq1.len() {
                    return Ok(ManagedXValue::new(XValue::Bool(false), rt)?.into());
                }
                let arr0 = seq0.iter(ns, rt.clone());
                let arr1 = seq1.iter(ns, rt.clone());
                let mut ret = true;
                let inner_equal_value =
                    xraise!(ns.eval(&inner_eq, rt.clone(), false)?.unwrap_value());
                let inner_eq_func = to_primitive!(inner_equal_value, Function);

                for ((x, y), search) in search(arr0.zip(arr1), rt.clone()) {
                    search?;
                    let eq = xraise!(ns
                        .eval_func_with_values(inner_eq_func, vec![x?, y?], rt.clone(), false)?
                        .unwrap_value());
                    let is_eq = to_primitive!(eq, Bool);
                    if !*is_eq {
                        ret = false;
                        break;
                    }
                }
                Ok(ManagedXValue::new(XValue::Bool(ret), rt)?.into())
            },
        ))
    })
}

pub(crate) fn add_sequence_dyn_sort<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let eq_symbol = scope.identifier("cmp");

    scope.add_dyn_func("sort", "sequences", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let (a0, ) = unpack_types!(types, 0);
        let [t0] = unpack_native(a0, "Sequence")? else { unreachable!() };

        let inner_eq = get_func(ns, eq_symbol, &[t0.clone(), t0.clone()], &X_INT)?;

        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(&[a0], a0.clone()),
            move |args, ns, _tca, rt| {
                let a0 = xraise!(eval(&args[0], ns, &rt)?);
                let seq0 = to_native!(a0, XSequence<W>);
                let Some(len0) = seq0.len() else { return xerr(ManagedXError::new("sequence is infinite", rt)?); };
                rt.as_ref().borrow().can_allocate(len0)?;
                let f_evaled = xraise!(ns.eval(&inner_eq, rt.clone(), false)?.unwrap_value());
                let f = to_primitive!(f_evaled, Function);
                xraise!(seq0.sorted(f, ns, rt.clone())?)
                    .map_or_else(|| Ok(a0.clone().into()), |s| Ok(manage_native!(s, rt)))
            },
        ))
    })
}

pub(crate) fn add_sequence_dyn_zip<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_dyn_func("zip", "sequences", move |_params, types, _ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let mut inner_types = vec![];
        // todo try zip without args
        for t in types.unwrap() {
            let [inner] = unpack_native(t, "Sequence")? else { unreachable!() };
            inner_types.push(inner.clone())
        }

        let arg_types = types.unwrap().iter().collect::<Vec<_>>();

        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(
                &arg_types,
                XSequenceType::xtype(Arc::new(XType::Tuple(inner_types))),
            ),
            move |args, ns, _tca, rt| {
                let mut seqs = vec![];
                for a in args {
                    let a = xraise!(eval(a, ns, &rt)?);
                    let seq = to_native!(a, XSequence<W>);
                    if seq.is_empty() {
                        return Ok(manage_native!(XSequence::<W>::Empty, rt));
                    }
                    seqs.push(a);
                    rt.as_ref().borrow().can_afford(&seqs)?
                }
                Ok(manage_native!(XSequence::<W>::Zip(seqs), rt))
            },
        ))
    })
}

pub(crate) fn add_sequence_dyn_unzip<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_dyn_func("unzip", "sequences", move |_params, types, _ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let (t0, ) = unpack_types!(types, 0);
        let [inner0] = unpack_native(t0, "Sequence")? else { unreachable!() };
        let XType::Tuple(inner_types) = inner0.as_ref() else { return Err(format!("expected sequence of tuples, got {t0:?}")); };
        let t_len = inner_types.len();

        let ret_type = Arc::new(XType::Tuple(inner_types.iter().map(|t| XSequenceType::xtype(t.clone())).collect()));


        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(&[t0], ret_type),
            move |args, ns, _tca, rt| {
                let a = xraise!(eval(&args[0], ns, &rt)?);

                let mut items = vec![];
                rt.as_ref().borrow().can_allocate(t_len * 2 + 1)?;
                for i in 0..t_len {
                    let func = ManagedXValue::new(XValue::Function(XFunction::Native(Rc::new(
                        move |args, ns, _tca, rt| {
                            let a0 = xraise!(eval(&args[0], ns, &rt)?);
                            let t0 = to_primitive!(a0, StructInstance);
                            Ok(t0[i].clone().into())
                        }
                    ))), rt.clone())?;
                    let item = manage_native!(XSequence::Map(a.clone(), func), rt.clone());
                    items.push(item);
                }

                Ok(ManagedXValue::new(
                    XValue::StructInstance(items), rt,
                )?.into())
            },
        ))
    })
}
