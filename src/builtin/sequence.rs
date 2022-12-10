use crate::builtin::core::{eval, get_func};
use crate::builtin::optional::{XOptional, XOptionalType};
use crate::builtin::stack::{XStack, XStackType};
use crate::evaluation_scope::EvaluatedValue;
use crate::native_types::{NativeType, XNativeValue};
use crate::runtime_err::RuntimeError;
use crate::runtime_scope::RuntimeScope;
use crate::util::trysort::try_sort;
use crate::xtype::{XFuncSpec, X_BOOL, X_INT};
use crate::xvalue::{ManagedXError, ManagedXValue, XFunction, XFunctionFactoryOutput, XValue};
use crate::XType::XCallable;
use crate::{
    forward_err, manage_native, to_native, to_primitive, unpack_native, unpack_types, xraise,
    xraise_opt, CompilationError, RTCell, RootCompilationScope, XCallableSpec, XStaticFunction,
    XType,
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
use std::rc;
use std::sync::Arc;

use crate::util::lazy_bigint::LazyBigint;

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
    Array(Vec<EvaluatedValue<W>>),
    // never empty
    Range(i64, i64, i64),
    Map(Rc<ManagedXValue<W>>, Rc<ManagedXValue<W>>),
    Zip(Vec<Rc<ManagedXValue<W>>>), // never empty //todo shortcut zip creation so that if any of the items are empty, Empty is returned instead
                                    // todo slice
}

impl<W: Write + 'static> XSequence<W> {
    pub(crate) fn array(value: Vec<EvaluatedValue<W>>) -> Self {
        if value.is_empty() {
            Self::Empty
        } else {
            Self::Array(value)
        }
    }

    pub(super) fn len(&self) -> usize {
        match self {
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
            Self::Map(seq, ..) => to_native!(seq, Self).len(),
            Self::Zip(sequences) => sequences
                .iter()
                .map(|seq| to_native!(seq, Self).len())
                .min()
                .unwrap(),
        }
    }

    pub(super) fn get(
        &self,
        idx: usize,
        ns: &RuntimeScope<W>,
        rt: RTCell<W>,
    ) -> Result<EvaluatedValue<W>, RuntimeError> {
        match self {
            Self::Empty => unreachable!(),
            Self::Array(arr) => Ok(arr[idx].clone()),
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
                let items = sequences
                    .iter()
                    .map(|seq| to_native!(seq, Self).get(idx, ns, rt.clone()))
                    .collect::<Result<Vec<_>, _>>()?;
                ManagedXValue::new(XValue::StructInstance(items), rt).map(Ok)
            }
        }
    }

    pub(super) fn slice<'a>(
        &'a self,
        ns: &'a RuntimeScope<W>,
        rt: RTCell<W>,
    ) -> impl DoubleEndedIterator<Item = Result<EvaluatedValue<W>, RuntimeError>> + 'a {
        (0..self.len()).map(move |idx| self.get(idx, ns, rt.clone()))
    }

    pub(super) fn is_empty(&self) -> bool {
        matches!(self, Self::Empty)
    }

    fn sorted(
        &self,
        cmp_func: &XFunction<W>,
        ns: &RuntimeScope<W>,
        rt: RTCell<W>,
    ) -> Result<Result<Option<Self>, Rc<ManagedXError<W>>>, RuntimeError> {
        let arr = self.slice(ns, rt.clone()).collect::<Result<Vec<_>, _>>()?;
        // first we check if the seq is already sorted
        let mut is_sorted = true;
        for w in arr.windows(2) {
            if to_primitive!(
                match ns
                    .eval_func_with_values(
                        cmp_func,
                        vec![w[0].clone(), w[1].clone()],
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
                |a, b| -> Result<Result<_, Rc<ManagedXError<W>>>, RuntimeError> {
                    Ok(Ok(to_primitive!(
                        forward_err!(ns
                            .eval_func_with_values(
                                cmp_func,
                                vec![a.clone(), b.clone()],
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
}

impl<W: Write + 'static> XNativeValue for XSequence<W> {
    fn size(&self) -> usize {
        match self {
            Self::Empty => size_of::<usize>(),
            Self::Array(arr) => arr.len() * size_of::<usize>(),
            Self::Zip(arr) => arr.len() * size_of::<usize>(),
            Self::Range(..) => 3 * size_of::<usize>(),
            Self::Map(..) => 2 * size_of::<usize>(),
        }
    }
}

fn value_to_idx<W: Write + 'static>(arr: &XSequence<W>, i: &LazyBigint, rt: RTCell<W>) -> Result<Result<usize, Rc<ManagedXError<W>>>, RuntimeError> {
    // todo why is this not a method?
    let mut i = Cow::Borrowed(i);
    if i.is_negative() {
        i = Cow::Owned(i.into_owned() + LazyBigint::from(arr.len()));
        if i.is_negative() {
            return Ok(Err(ManagedXError::new("index too low",rt)?));
        }
    };
    let Some(idx) = i.to_usize() else { return Ok(Err(ManagedXError::new("index out of bounds",rt)?)) };
    if idx >= arr.len() {
        return Ok(Err(ManagedXError::new("index out of bounds",rt)?));
    }
    Ok(Ok(idx))
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
            let idx = xraise!(value_to_idx(arr, idx, rt.clone())?);
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
            Ok(ManagedXValue::new(XValue::Int(arr.len().into()), rt)?.into())
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
        XStaticFunction::from_native(|args, ns, tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let seq0 = to_native!(a0, XSequence<W>);
            if seq0.is_empty() {
                return ns.eval(&args[1], rt, tca);
            }
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let seq1 = to_native!(a1, XSequence<W>);
            if seq1.is_empty() {
                return Ok(a0.clone().into());
            }
            let mut arr = seq0.slice(ns, rt.clone()).collect::<Result<Vec<_>, _>>()?;
            arr.extend(
                // todo ideally we shouldn't collect here
                seq1.slice(ns, rt.clone()).collect::<Result<Vec<_>, _>>()?,
            );
            Ok(manage_native!(XSequence::array(arr), rt))
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
                let mut arr = seq0.slice(ns, rt.clone()).collect::<Result<Vec<_>, _>>()?;
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
                let mut arr = seq0.slice(ns, rt.clone()).collect::<Result<Vec<_>, _>>()?;
                let original_len = arr.len();
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
            let a1 = eval(&args[1], ns, &rt)?;
            let seq0 = to_native!(a0, XSequence<W>);
            let mut arr = seq0.slice(ns, rt.clone()).collect::<Result<Vec<_>, _>>()?;
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
            let a1 = eval(&args[1], ns, &rt)?;
            let seq0 = to_native!(a0, XSequence<W>);
            let mut arr = vec![a1];
            arr.extend(
                // todo ideally we shouldn't collect here
                seq0.slice(ns, rt.clone()).collect::<Result<Vec<_>, _>>()?,
            );
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
            let a2 = eval(&args[2], ns, &rt)?;
            let seq = to_native!(a0, XSequence<W>);
            let idx = to_primitive!(a1, Int);
            let idx = xraise!(value_to_idx(seq, idx, rt.clone())?);
            let mut ret = seq
                .slice(ns, rt.clone())
                .take(idx)
                .collect::<Result<Vec<_>, _>>()?;
            ret.push(a2);
            ret.extend(
                // todo remove collect
                seq.slice(ns, rt.clone())
                    .skip(idx)
                    .collect::<Result<Vec<_>, _>>()?,
            );
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
            let seq = to_native!(a0, XSequence<W>);
            let idx = to_primitive!(a1, Int);
            let idx = xraise!(value_to_idx(seq, idx, rt.clone())?);
            if seq.len() == 1 {
                return Ok(manage_native!(XSequence::<W>::Empty, rt));
            }
            let mut ret = seq
                .slice(ns, rt.clone())
                .take(idx)
                .collect::<Result<Vec<_>, _>>()?;
            ret.extend(
                // todo avoid collecting
                seq.slice(ns, rt.clone())
                    .skip(idx + 1)
                    .collect::<Result<Vec<_>, _>>()?,
            );
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
            let a2 = eval(&args[2], ns, &rt)?;
            let seq = to_native!(a0, XSequence<W>);
            let idx = to_primitive!(a1, Int);
            let idx = xraise!(value_to_idx(seq, idx, rt.clone())?);
            let mut ret: Vec<_> = seq
                .slice(ns, rt.clone())
                .take(idx)
                .collect::<Result<_, _>>()?;
            ret.push(a2);
            ret.extend(
                // todo avoid collecting
                seq.slice(ns, rt.clone())
                    .skip(idx + 1)
                    .collect::<Result<Vec<_>, _>>()?,
            );
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
            let seq = to_native!(a0, XSequence<W>);
            let idx1 = to_primitive!(a1, Int);
            let idx2 = to_primitive!(a2, Int);
            let mut idx1 = xraise!(value_to_idx(seq, idx1, rt.clone())?);
            let mut idx2 = xraise!(value_to_idx(seq, idx2, rt.clone())?);
            if idx1 == idx2 {
                return Ok(a0.clone().into());
            }
            if idx1 > idx2 {
                (idx1, idx2) = (idx2, idx1);
            }
            let mut ret = seq
                .slice(ns, rt.clone())
                .take(idx1)
                .collect::<Result<Vec<_>, _>>()?;
            ret.push(seq.get(idx2, ns, rt.clone())?);
            ret.extend(
                // todo avoid collecting
                seq.slice(ns, rt.clone())
                    .take(idx2)
                    .skip(idx1 + 1)
                    .collect::<Result<Vec<_>, _>>()?,
            );
            ret.push(seq.get(idx1, ns, rt.clone())?);
            ret.extend(
                // todo avoid collecting
                seq.slice(ns, rt.clone())
                    .skip(idx2 + 1)
                    .collect::<Result<Vec<_>, _>>()?,
            );
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
            let arr = to_native!(a0, XSequence<W>);
            let mut ret = XStack::new();
            for x in arr.slice(ns, rt.clone()) {
                ret = ret.push(x?);
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
            let seq = to_native!(a0, XSequence<W>);
            let f = to_primitive!(a1, Function);
            xraise!(seq.sorted(f, ns, rt.clone())?)
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
            let seq = to_native!(a0, XSequence<W>);
            let f = to_primitive!(a2, Function);
            let mut ret = a1;
            let arr = seq.slice(ns, rt.clone());
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
            let seq = to_native!(a0, XSequence<W>);
            if seq.is_empty() {
                return xraise!(Err(ManagedXError::new("sequence is empty",rt)?));
            }
            let f = to_primitive!(a1, Function);
            let mut ret = seq.get(0, ns, rt.clone())?;
            let arr = seq.slice(ns, rt.clone()).skip(1);
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
                let Some(end0) = to_primitive!(a0, Int).to_i64() else { xraise!(Err(ManagedXError::new("end out of bounds", rt)?)) };
                end = end0;
                start = 0i64;
                step = 1i64;
            } else {
                let a0 = xraise!(eval(&args[0], ns, &rt)?);
                let a1 = xraise!(eval(&args[1], ns, &rt)?);
                let a2 = xraise_opt!(args.get(2).map(|e| eval(e, ns, &rt)).transpose()?);
                let Some(start0) = to_primitive!(a0, Int).to_i64() else { xraise!(Err(ManagedXError::new("start out of bounds", rt)?)) };
                let Some(end0) = to_primitive!(a1, Int).to_i64() else { xraise!(Err(ManagedXError::new("end out of bounds", rt)?)) };
                let Some(step0) = a2.map_or(Some(1i64), |a2| {to_primitive!(a2, Int).to_i64()}) else {xraise!(Err(ManagedXError::new("step out of bounds", rt)?))};
                start = start0;
                end = end0;
                step = step0;
            }
            if step.is_zero() {
                xraise!(Err(ManagedXError::new("invalid range, step size cannot be zero", rt)?))
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
            let seq = to_native!(a0, XSequence<W>);
            let f = to_primitive!(a1, Function);
            // first we check if the seq already fully_matches
            let mut first_drop_idx = None; // if this is not none, it is the first index we need to drop
            let mut items = seq.slice(ns, rt.clone());
            let mut ret = Vec::new();
            for (i, item) in items.by_ref().enumerate() {
                let item = item?;
                let res = xraise!(ns
                    .eval_func_with_values(f, vec![item.clone()], rt.clone(), false)?
                    .unwrap_value());
                if !*to_primitive!(res, Bool) {
                    first_drop_idx = Some(i);
                    break;
                }
                ret.push(item);
            }
            if first_drop_idx.is_some() {
                for item in items {
                    let item = item?;
                    let res = xraise!(ns
                        .eval_func_with_values(f, vec![item.clone()], rt.clone(), false)?
                        .unwrap_value());
                    if *to_primitive!(res, Bool) {
                        ret.push(item);
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
            let seq = to_native!(a0, XSequence<W>);
            let original_arr = seq.slice(ns, rt.clone());
            let mut matches_left = to_primitive!(a1, Int).clone();
            if matches_left.is_zero() {
                return xraise!(Err(ManagedXError::new("match_count must be non-zero",rt)?));
            }
            let arr = if matches_left.is_negative() {
                matches_left = matches_left.neg();
                Either::Left(original_arr.rev())
            } else {
                Either::Right(original_arr)
            };
            let f = to_primitive!(a2, Function);
            for item in arr {
                let item = item?;
                if *to_primitive!(
                    xraise!(ns
                        .eval_func_with_values(f, vec![item.clone()], rt.clone(), false)?
                        .unwrap_value()),
                    Bool
                ) {
                    matches_left = matches_left - One::one();
                    if matches_left.is_zero() {
                        return Ok(manage_native!(XOptional { value: Some(item) }, rt));
                    }
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
            let arr = seq.slice(ns, rt.clone());
            let f = to_primitive!(a1, Function);
            let mut end_idx = seq.len();
            let mut ret = vec![];
            for (i, item) in arr.enumerate() {
                let item = item?;
                if !*to_primitive!(
                    xraise!(ns
                        .eval_func_with_values(f, vec![item.clone()], rt.clone(), false)?
                        .unwrap_value()),
                    Bool
                ) {
                    end_idx = i;
                    break;
                }
                ret.push(item);
            }
            if end_idx == seq.len() {
                Ok(a0.clone().into())
            } else {
                Ok(manage_native!(XSequence::array(ret), rt))
            }
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
            let mut arr = seq.slice(ns, rt.clone());
            let f = to_primitive!(a1, Function);
            let mut start_idx = seq.len();
            let mut ret = vec![];
            for (i, item) in arr.by_ref().enumerate() {
                let item = item?;
                if *to_primitive!(
                    xraise!(ns
                        .eval_func_with_values(f, vec![item.clone()], rt.clone(), false)?
                        .unwrap_value()),
                    Bool
                ) {
                    start_idx = i;
                    ret.push(item);
                    break;
                }
            }
            if start_idx == 0 {
                Ok(a0.clone().into())
            } else {
                ret.extend(
                    // todo avoid collecting
                    arr.collect::<Result<Vec<_>, _>>()?,
                );
                Ok(manage_native!(XSequence::array(ret), rt))
            }
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
            let seq = to_native!(a0, XSequence<W>);
            let end_idx = to_primitive!(a1, Int);
            if end_idx.to_usize().map_or(true, |s| s >= seq.len()) {
                Ok(a0.clone().into())
            } else {
                let arr = seq
                    .slice(ns, rt.clone())
                    .take(end_idx.to_usize().unwrap())
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(manage_native!(XSequence::array(arr), rt))
            }
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
            let seq = to_native!(a0, XSequence<W>);
            let start_idx = to_primitive!(a1, Int);
            if start_idx.is_zero() {
                Ok(a0.clone().into())
            } else if start_idx.to_usize().map_or(true, |s| s > seq.len()) {
                Ok(manage_native!(XSequence::<W>::Empty, rt))
            } else {
                let arr = seq
                    .slice(ns, rt.clone())
                    .skip(start_idx.to_usize().unwrap())
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(manage_native!(XSequence::array(arr), rt))
            }
        }),
    )
}

pub(crate) fn add_sequence_eq<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let eq_symbol = scope.identifier("eq");

    scope.add_dyn_func("eq", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let (a0, a1) = unpack_types!(types, 0, 1);
        let (t0,) = unpack_native!(a0, "Sequence", 0);
        let (t1,) = unpack_native!(a1, "Sequence", 0);

        let inner_eq = get_func(ns, eq_symbol, &[t0.clone(), t1.clone()], &X_BOOL)?;

        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(
                &[&XSequenceType::xtype(t0), &XSequenceType::xtype(t1)],
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
                let arr0 = seq0.slice(ns, rt.clone());
                let arr1 = seq1.slice(ns, rt.clone());
                let mut ret = true;
                let inner_equal_value =
                    xraise!(ns.eval(&inner_eq, rt.clone(), false)?.unwrap_value());
                let inner_eq_func = to_primitive!(inner_equal_value, Function);

                for (x, y) in arr0.into_iter().zip(arr1.into_iter()) {
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

    scope.add_dyn_func("sort", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let (a0,) = unpack_types!(types, 0);
        let (t0,) = unpack_native!(a0, "Sequence", 0);

        let inner_eq = get_func(ns, eq_symbol, &[t0.clone(), t0], &X_INT)?;

        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(&[a0], a0.clone()),
            move |args, ns, _tca, rt| {
                let a0 = xraise!(eval(&args[0], ns, &rt)?);
                let seq = to_native!(a0, XSequence<W>);
                let f_evaled = xraise!(ns.eval(&inner_eq, rt.clone(), false)?.unwrap_value());
                let f = to_primitive!(f_evaled, Function);
                xraise!(seq.sorted(f, ns, rt.clone())?)
                    .map_or_else(|| Ok(a0.clone().into()), |s| Ok(manage_native!(s, rt)))
            },
        ))
    })
}
