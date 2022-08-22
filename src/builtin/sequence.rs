use crate::builtin::optional::{XOptional, XOptionalType};
use crate::builtin::stack::{XStack, XStackType};
use crate::native_types::{NativeType, XNativeValue};
use crate::util::trysort::try_sort;
use crate::xtype::{XFuncParamSpec, XFuncSpec, X_BOOL, X_INT};
use crate::xvalue::{ManagedXValue, XValue};
use crate::XType::XCallable;
use crate::{
    eval, manage_native, meval, to_native, to_primitive, CompilationError, Identifier, RTCell,
    RootCompilationScope, XCallableSpec, XCompilationScope, XEvaluationScope, XStaticFunction,
    XType,
};
use num_traits::{One, Signed, ToPrimitive, Zero};
use rc::Rc;
use std::borrow::Cow;
use std::mem::size_of;
use std::ops::Neg;
use std::rc;
use std::sync::Arc;

use crate::util::lazy_bigint::LazyBigint;

use crate::xexpr::XExpr;

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

#[derive(Debug)]
pub enum XSequence {
    Empty,
    Array(Vec<Rc<ManagedXValue>>),
    // never empty
    Range(i64, i64, i64),
    Map(Rc<ManagedXValue>, Rc<ManagedXValue>),
    Zip(Vec<Rc<ManagedXValue>>), // never empty //todo shortcut zip creation so that if any of the items are empty, Empty is returned instead
}

impl XSequence {
    pub(crate) fn array(value: Vec<Rc<ManagedXValue>>) -> Self {
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
        ns: &XEvaluationScope,
        rt: RTCell,
    ) -> Result<Rc<ManagedXValue>, String> {
        match self {
            Self::Empty => unreachable!(),
            Self::Array(arr) => Ok(arr[idx].clone()),
            Self::Range(start, _, step) => {
                let v = LazyBigint::from(*start) + LazyBigint::from(idx) * LazyBigint::from(*step);
                Ok(ManagedXValue::new(XValue::Int(v), rt)?)
            }
            Self::Map(seq, func) => {
                let original = to_native!(seq, Self).get(idx, ns, rt.clone())?;
                let f = to_primitive!(func, Function);
                f.eval_values(&[original], ns, rt)
            }
            Self::Zip(sequences) => {
                let items = sequences
                    .iter()
                    .map(|seq| to_native!(seq, Self).get(idx, ns, rt.clone()))
                    .collect::<Result<_, _>>()?;
                Ok(ManagedXValue::new(XValue::StructInstance(items), rt)?)
            }
        }
    }

    pub(super) fn slice(
        &self,
        start_idx: usize,
        end_idx: usize,
        ns: &XEvaluationScope,
        rt: RTCell,
    ) -> Result<Vec<Rc<ManagedXValue>>, String> {
        // todo make this return an iterator
        match self {
            Self::Empty => Ok(vec![]),
            Self::Array(arr) => Ok(arr[start_idx..end_idx].to_vec()),
            Self::Range(start, _, step) => {
                let step = LazyBigint::from(*step);
                let mut current =
                    LazyBigint::from(*start) + LazyBigint::from(start_idx) * step.clone();
                let mut ret = vec![ManagedXValue::new(
                    XValue::Int(current.clone()),
                    rt.clone(),
                )?];
                for _ in start_idx + 1..end_idx {
                    current = current + step.clone();
                    ret.push(ManagedXValue::new(
                        XValue::Int(current.clone()),
                        rt.clone(),
                    )?);
                }
                Ok(ret)
            }
            Self::Map(seq, func) => {
                let seq = to_native!(seq, Self);
                let func = to_primitive!(func, Function);
                let ret = (start_idx..end_idx)
                    .map(|idx| {
                        let original = seq.get(idx, ns, rt.clone())?;
                        func.eval_values(&[original], ns, rt.clone())
                    })
                    .collect::<Result<_, _>>()?;
                Ok(ret)
            }
            Self::Zip(sequences) => {
                let sequences = sequences
                    .iter()
                    .map(|seq| to_native!(seq, Self))
                    .collect::<Vec<_>>();
                let ret = (start_idx..end_idx)
                    .map(|idx| {
                        let items = sequences
                            .iter()
                            .map(|seq| seq.get(idx, ns, rt.clone()))
                            .collect::<Result<_, _>>()?;
                        ManagedXValue::new(XValue::StructInstance(items), rt.clone())
                    })
                    .collect::<Result<_, _>>()?;
                Ok(ret)
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, Self::Empty)
    }
}

impl XNativeValue for XSequence {
    fn size(&self) -> usize {
        match self {
            Self::Empty => size_of::<usize>(),
            Self::Array(arr) | Self::Zip(arr) => arr.len() * size_of::<usize>(),
            Self::Range(..) => 3 * size_of::<usize>(),
            Self::Map(..) => 2 * size_of::<usize>(),
        }
    }
}

fn value_to_idx(arr: &XSequence, i: &LazyBigint) -> Result<usize, String> {
    let mut i = Cow::Borrowed(i);
    if i.is_negative() {
        i = Cow::Owned(i.into_owned() + LazyBigint::from(arr.len()));
        if i.is_negative() {
            return Err("index too low".to_string());
        }
    };
    let idx = i.to_usize().ok_or("index too large")?;
    if idx >= arr.len() {
        return Err("index out of bounds".to_string());
    }
    Ok(idx)
}

pub(crate) fn add_sequence_type(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t], _) = scope.generics_from_names(["T"]);
    scope.add_native_type("Sequence", XSequenceType::xtype(t))
}

pub(crate) fn add_sequence_get(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);

    scope.add_func(
        "get",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: XSequenceType::xtype(t.clone()),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: X_INT.clone(),
                        required: true,
                    },
                ],
                ret: t,
            },
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let arr = &to_native!(a0, XSequence);
                let idx = to_primitive!(a1, Int);
                let idx = value_to_idx(arr, idx)?;
                Ok(arr.get(idx, ns, rt)?.into())
            },
        ),
    )
}

pub(crate) fn add_sequence_len(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);

    scope.add_func(
        "len",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![XFuncParamSpec {
                    type_: XSequenceType::xtype(t),
                    required: true,
                }],
                ret: X_INT.clone(),
            },
            |args, ns, _tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                let arr = &to_native!(a0, XSequence);
                Ok(ManagedXValue::new(XValue::Int(arr.len().into()), rt)?.into())
            },
        ),
    )
}

pub(crate) fn add_sequence_add(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t);

    scope.add_func(
        "add",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: t_arr.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: t_arr.clone(),
                        required: true,
                    },
                ],
                ret: t_arr,
            },
            |args, ns, tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                let seq0 = to_native!(a0, XSequence);
                if seq0.is_empty() {
                    return args[1].eval(ns, tca, rt);
                }
                let (a1,) = eval!(args, ns, rt, 1);
                let seq1 = to_native!(a1, XSequence);
                if seq1.is_empty() {
                    return Ok(a0.clone().into());
                }
                let mut arr = seq0.slice(0, seq0.len(), ns, rt.clone())?;
                arr.extend(seq1.slice(0, seq1.len(), ns, rt.clone())?);
                Ok(manage_native!(XSequence::array(arr), rt))
            },
        ),
    )
}

pub(crate) fn add_sequence_add_stack(
    scope: &mut RootCompilationScope,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t.clone());
    let t_stack = XStackType::xtype(t);

    scope.add_func(
        "add",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: t_arr.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: t_stack,
                        required: true,
                    },
                ],
                ret: t_arr,
            },
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let seq0 = to_native!(a0, XSequence);
                let stk1 = to_native!(a1, XStack);
                if stk1.length == 0 {
                    Ok(a0.clone().into())
                } else {
                    let mut arr = seq0.slice(0, seq0.len(), ns, rt.clone())?;
                    for v in stk1.iter() {
                        arr.push(v.clone());
                    }
                    Ok(manage_native!(XSequence::array(arr), rt))
                }
            },
        ),
    )
}

pub(crate) fn add_sequence_addrev_stack(
    scope: &mut RootCompilationScope,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t.clone());
    let t_stack = XStackType::xtype(t);

    scope.add_func(
        "add_rev",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: t_arr.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: t_stack,
                        required: true,
                    },
                ],
                ret: t_arr,
            },
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let seq0 = to_native!(a0, XSequence);
                let stk1 = to_native!(a1, XStack);
                if stk1.length == 0 {
                    Ok(a0.clone().into())
                } else {
                    let mut arr = seq0.slice(0, seq0.len(), ns, rt.clone())?;
                    let original_len = arr.len();
                    for v in stk1.iter() {
                        arr.push(v.clone());
                    }
                    arr[original_len..].reverse();
                    Ok(manage_native!(XSequence::array(arr), rt))
                }
            },
        ),
    )
}

pub(crate) fn add_sequence_push(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t.clone());

    scope.add_func(
        "push",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: t_arr.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: t,
                        required: true,
                    },
                ],
                ret: t_arr,
            },
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let seq0 = to_native!(a0, XSequence);
                let mut arr = seq0.slice(0, seq0.len(), ns, rt.clone())?;
                arr.push(a1);
                Ok(manage_native!(XSequence::array(arr), rt))
            },
        ),
    )
}

pub(crate) fn add_sequence_rpush(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t.clone());

    scope.add_func(
        "rpush",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: t_arr.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: t,
                        required: true,
                    },
                ],
                ret: t_arr,
            },
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let seq0 = to_native!(a0, XSequence);
                let mut arr = vec![a1];
                arr.extend(seq0.slice(0, seq0.len(), ns, rt.clone())?);
                Ok(manage_native!(XSequence::array(arr), rt))
            },
        ),
    )
}

pub(crate) fn add_sequence_insert(
    scope: &mut RootCompilationScope,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t.clone());

    scope.add_func(
        "insert",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: t_arr.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: X_INT.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: t,
                        required: true,
                    },
                ],
                ret: t_arr,
            },
            |args, ns, _tca, rt| {
                let (a0, a1, a2) = eval!(args, ns, rt, 0, 1, 2);
                let seq = to_native!(a0, XSequence);
                let idx = to_primitive!(a1, Int);
                let idx = value_to_idx(seq, idx)?;
                let mut ret: Vec<Rc<_>> = vec![];
                ret.extend(seq.slice(0, idx, ns, rt.clone())?);
                ret.push(a2);
                ret.extend(seq.slice(idx, seq.len(), ns, rt.clone())?);
                Ok(manage_native!(XSequence::array(ret), rt))
            },
        ),
    )
}

pub(crate) fn add_sequence_pop(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t);

    scope.add_func(
        "pop",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: t_arr.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: X_INT.clone(),
                        required: true,
                    },
                ],
                ret: t_arr,
            },
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let seq = to_native!(a0, XSequence);
                let idx = to_primitive!(a1, Int);
                let idx = value_to_idx(seq, idx)?;
                if seq.len() == 1 {
                    return Ok(manage_native!(XSequence::Empty, rt));
                }
                let mut ret: Vec<Rc<_>> = vec![];
                ret.extend(seq.slice(0, idx, ns, rt.clone())?);
                ret.extend(seq.slice(idx + 1, seq.len(), ns, rt.clone())?);
                Ok(manage_native!(XSequence::array(ret), rt))
            },
        ),
    )
}

pub(crate) fn add_sequence_set(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t.clone());

    scope.add_func(
        "set",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: t_arr.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: X_INT.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: t,
                        required: true,
                    },
                ],
                ret: t_arr,
            },
            |args, ns, _tca, rt| {
                let (a0, a1, a2) = eval!(args, ns, rt, 0, 1, 2);
                let seq = to_native!(a0, XSequence);
                let idx = to_primitive!(a1, Int);
                let idx = value_to_idx(seq, idx)?;
                let mut ret: Vec<Rc<_>> = vec![];
                ret.extend(seq.slice(0, idx, ns, rt.clone())?);
                ret.push(a2);
                ret.extend(seq.slice(idx + 1, seq.len(), ns, rt.clone())?);
                Ok(manage_native!(XSequence::array(ret), rt))
            },
        ),
    )
}

pub(crate) fn add_sequence_swap(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t);

    scope.add_func(
        "swap",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: t_arr.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: X_INT.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: X_INT.clone(),
                        required: true,
                    },
                ],
                ret: t_arr,
            },
            |args, ns, _tca, rt| {
                let (a0, a1, a2) = eval!(args, ns, rt, 0, 1, 2);
                let seq = to_native!(a0, XSequence);
                let idx1 = to_primitive!(a1, Int);
                let idx2 = to_primitive!(a2, Int);
                let mut idx1 = value_to_idx(seq, idx1)?;
                let mut idx2 = value_to_idx(seq, idx2)?;
                if idx1 == idx2 {
                    return Ok(a0.clone().into());
                }
                if idx1 > idx2 {
                    (idx1, idx2) = (idx2, idx1);
                }
                let mut ret = vec![];
                ret.extend(seq.slice(0, idx1, ns, rt.clone())?);
                ret.push(seq.get(idx2, ns, rt.clone())?);
                ret.extend(seq.slice(idx1 + 1, idx2, ns, rt.clone())?);
                ret.push(seq.get(idx1, ns, rt.clone())?);
                ret.extend(seq.slice(idx2 + 1, seq.len(), ns, rt.clone())?);
                Ok(manage_native!(XSequence::array(ret), rt))
            },
        ),
    )
}

pub(crate) fn add_sequence_to_stack(
    scope: &mut RootCompilationScope,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);

    scope.add_func(
        "to_stack",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![XFuncParamSpec {
                    type_: XSequenceType::xtype(t.clone()),
                    required: true,
                }],
                ret: XStackType::xtype(t),
            },
            |args, ns, _tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                let arr = to_native!(a0, XSequence);
                let mut ret = XStack::new();
                for x in arr.slice(0, arr.len(), ns, rt.clone())? {
                    ret = ret.push(x);
                }
                Ok(manage_native!(ret, rt))
            },
        ),
    )
}

pub(crate) fn add_sequence_map(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([input_t, output_t], params) = scope.generics_from_names(["T_IN", "T_OUT"]);

    scope.add_func(
        "map",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: XSequenceType::xtype(input_t.clone()),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: Arc::new(XCallable(XCallableSpec {
                            param_types: vec![input_t],
                            return_type: output_t.clone(),
                        })),
                        required: true,
                    },
                ],
                ret: XSequenceType::xtype(output_t),
            },
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                Ok(manage_native!(XSequence::Map(a0, a1), rt))
            },
        ),
    )
}

pub(crate) fn add_sequence_sort(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t.clone());

    scope.add_func(
        "sort",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: t_arr.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: Arc::new(XCallable(XCallableSpec {
                            param_types: vec![t.clone(), t],
                            return_type: X_INT.clone(),
                        })),
                        required: true,
                    },
                ],
                ret: t_arr,
            },
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let seq = to_native!(a0, XSequence);
                let arr = seq.slice(0, seq.len(), ns, rt.clone())?;
                let f = to_primitive!(a1, Function);
                // first we check if the seq is already sorted
                let mut is_sorted = true;
                for w in arr.windows(2) {
                    if to_primitive!(
                        f.eval_values(&[w[0].clone(), w[1].clone()], ns, rt.clone())?,
                        Int
                    )
                    .is_positive()
                    {
                        is_sorted = false;
                        break;
                    }
                }
                if is_sorted {
                    Ok(a0.clone().into())
                } else {
                    let mut ret = arr;
                    try_sort(&mut ret, |a, b| -> Result<_, String> {
                        Ok(to_primitive!(
                            f.eval_values(&[a.clone(), b.clone()], ns, rt.clone())?,
                            Int
                        )
                        .is_negative())
                    })?;
                    Ok(manage_native!(XSequence::array(ret), rt))
                }
            },
        ),
    )
}

pub(crate) fn add_sequence_reduce3(
    scope: &mut RootCompilationScope,
) -> Result<(), CompilationError> {
    let ([t, s], params) = scope.generics_from_names(["T", "S"]);
    let t_arr = XSequenceType::xtype(t.clone());

    scope.add_func(
        "reduce",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: t_arr,
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: s.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: Arc::new(XCallable(XCallableSpec {
                            param_types: vec![s.clone(), t],
                            return_type: s.clone(),
                        })),
                        required: true,
                    },
                ],
                ret: s,
            },
            |args, ns, _tca, rt| {
                let (a0, a1, a2) = eval!(args, ns, rt, 0, 1, 2);
                let seq = to_native!(a0, XSequence);
                let arr = seq.slice(0, seq.len(), ns, rt.clone())?;
                let f = to_primitive!(a2, Function);
                let mut ret = a1;
                for i in arr {
                    ret = f.eval_values(&[ret, i], ns, rt.clone())?;
                }
                Ok(ret.into())
            },
        ),
    )
}

pub(crate) fn add_sequence_reduce2(
    scope: &mut RootCompilationScope,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t.clone());

    scope.add_func(
        "reduce",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: t_arr,
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: Arc::new(XCallable(XCallableSpec {
                            param_types: vec![t.clone(), t.clone()],
                            return_type: t.clone(),
                        })),
                        required: true,
                    },
                ],
                ret: t,
            },
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let seq = to_native!(a0, XSequence);
                if seq.is_empty() {
                    return Err("sequence is empty".to_string());
                }
                let arr = seq.slice(1, seq.len(), ns, rt.clone())?;
                let f = to_primitive!(a1, Function);
                let mut ret = seq.get(0, ns, rt.clone())?;
                for i in arr {
                    ret = f.eval_values(&[ret, i], ns, rt.clone())?;
                }
                Ok(ret.into())
            },
        ),
    )
}

pub(crate) fn add_sequence_range(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let t_arr = XSequenceType::xtype(X_INT.clone());

    scope.add_func(
        "range",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: None,
                params: vec![
                    XFuncParamSpec {
                        type_: X_INT.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: X_INT.clone(),
                        required: false,
                    },
                    XFuncParamSpec {
                        type_: X_INT.clone(),
                        required: false,
                    },
                ],
                ret: t_arr,
            },
            |args, ns, _tca, rt| {
                let (start, end, step);
                if args.len() == 1 {
                    let (a0,) = eval!(args, ns, rt, 0);
                    end = to_primitive!(a0, Int).to_i64().ok_or("end out of bounds")?;
                    start = 0i64;
                    step = 1i64;
                } else {
                    let (a0, a1) = eval!(args, ns, rt, 0, 1);
                    let (a2,) = meval!(args, ns, rt, 2);
                    start = to_primitive!(a0, Int)
                        .to_i64()
                        .ok_or("start out of bounds")?;
                    end = to_primitive!(a1, Int).to_i64().ok_or("end out of bounds")?;
                    step = a2.map_or(Ok(1i64), |a2| {
                        to_primitive!(a2, Int).to_i64().ok_or("step out of bounds")
                    })?;
                }
                if step.is_zero() {
                    Err("invalid range, step size cannot be zero".to_string())
                } else if (step.is_positive() && start >= end)
                    || (step.is_negative() && start <= end)
                {
                    Ok(manage_native!(XSequence::Empty, rt))
                } else {
                    Ok(manage_native!(XSequence::Range(start, end, step), rt))
                }
            },
        ),
    )
}

pub(crate) fn add_sequence_filter(
    scope: &mut RootCompilationScope,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t.clone());

    scope.add_func(
        "filter",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: t_arr.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: Arc::new(XCallable(XCallableSpec {
                            param_types: vec![t],
                            return_type: X_BOOL.clone(),
                        })),
                        required: true,
                    },
                ],
                ret: t_arr,
            },
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let seq = to_native!(a0, XSequence);
                let arr = seq.slice(0, seq.len(), ns, rt.clone())?;
                let f = to_primitive!(a1, Function);
                // first we check if the seq already fully_matches
                let mut first_drop_idx = None; // if this is not none, it is the first index we need to drop
                for (i, item) in arr.iter().enumerate() {
                    if !*to_primitive!(f.eval_values(&[item.clone()], ns, rt.clone())?, Bool) {
                        first_drop_idx = Some(i);
                        break;
                    }
                }
                if let Some(first_drop_idx) = first_drop_idx {
                    let mut ret = arr[..first_drop_idx].to_vec();

                    for item in arr.iter().skip(first_drop_idx + 1) {
                        if *to_primitive!(f.eval_values(&[item.clone()], ns, rt.clone())?, Bool) {
                            ret.push(item.clone());
                        }
                    }
                    Ok(manage_native!(XSequence::array(ret), rt))
                } else {
                    // no indices need to drop, we can just return the sequence
                    Ok(a0.clone().into())
                }
            },
        ),
    )
}

pub(crate) fn add_sequence_nth(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t.clone());

    scope.add_func(
        "nth",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: t_arr,
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: X_INT.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: Arc::new(XCallable(XCallableSpec {
                            param_types: vec![t.clone()],
                            return_type: X_BOOL.clone(),
                        })),
                        required: true,
                    },
                ],
                ret: XOptionalType::xtype(t),
            },
            |args, ns, _tca, rt| {
                let (a0, a1, a2) = eval!(args, ns, rt, 0, 1, 2);
                let seq = to_native!(a0, XSequence);
                let mut arr = seq.slice(0, seq.len(), ns, rt.clone())?;
                let mut matches_left = to_primitive!(a1, Int).clone();
                if matches_left.is_zero() {
                    return Err("match_count must be non-zero".to_string());
                }
                if matches_left.is_negative() {
                    arr.reverse(); // todo improve
                    matches_left = matches_left.neg()
                }
                let f = to_primitive!(a2, Function);
                for item in arr {
                    if *to_primitive!(f.eval_values(&[item.clone()], ns, rt.clone())?, Bool) {
                        matches_left = matches_left - One::one();
                        if matches_left.is_zero() {
                            return Ok(manage_native!(XOptional { value: Some(item) }, rt));
                        }
                    }
                }
                Ok(manage_native!(XOptional { value: None }, rt))
            },
        ),
    )
}

pub(crate) fn add_sequence_take_while(
    scope: &mut RootCompilationScope,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t.clone());

    scope.add_func(
        "take_while",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: t_arr.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: Arc::new(XCallable(XCallableSpec {
                            param_types: vec![t],
                            return_type: X_BOOL.clone(),
                        })),
                        required: true,
                    },
                ],
                ret: t_arr,
            },
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let seq = to_native!(a0, XSequence);
                let mut arr = seq.slice(0, seq.len(), ns, rt.clone())?;
                let f = to_primitive!(a1, Function);
                let mut end_idx = arr.len();
                for (i, item) in arr.iter().enumerate() {
                    if !*to_primitive!(f.eval_values(&[item.clone()], ns, rt.clone())?, Bool) {
                        end_idx = i;
                        break;
                    }
                }
                if end_idx == arr.len() {
                    Ok(a0.clone().into())
                } else {
                    arr.drain(end_idx..);
                    Ok(manage_native!(XSequence::array(arr), rt))
                }
            },
        ),
    )
}

pub(crate) fn add_sequence_skip_until(
    scope: &mut RootCompilationScope,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t.clone());

    scope.add_func(
        "skip_until",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: t_arr.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: Arc::new(XCallable(XCallableSpec {
                            param_types: vec![t],
                            return_type: X_BOOL.clone(),
                        })),
                        required: true,
                    },
                ],
                ret: t_arr,
            },
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let seq = to_native!(a0, XSequence);
                let mut arr = seq.slice(0, seq.len(), ns, rt.clone())?;
                let f = to_primitive!(a1, Function);
                let mut start_idx = arr.len();
                for (i, item) in arr.iter().enumerate() {
                    if *to_primitive!(f.eval_values(&[item.clone()], ns, rt.clone())?, Bool) {
                        start_idx = i;
                        break;
                    }
                }
                if start_idx == 0 {
                    Ok(a0.clone().into())
                } else {
                    arr = arr.drain(start_idx..).collect();
                    Ok(manage_native!(XSequence::array(arr), rt))
                }
            },
        ),
    )
}

pub(crate) fn add_sequence_take(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t);

    scope.add_func(
        "take",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: t_arr.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: X_INT.clone(),
                        required: true,
                    },
                ],
                ret: t_arr,
            },
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let seq = to_native!(a0, XSequence);
                let end_idx = to_primitive!(a1, Int);
                if end_idx.to_usize().map_or(true, |s| s >= seq.len()) {
                    Ok(a0.clone().into())
                } else {
                    let arr = seq.slice(0, end_idx.to_usize().unwrap(), ns, rt.clone())?;
                    Ok(manage_native!(XSequence::array(arr), rt))
                }
            },
        ),
    )
}

pub(crate) fn add_sequence_skip(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_arr = XSequenceType::xtype(t);

    scope.add_func(
        "skip",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: t_arr.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: X_INT.clone(),
                        required: true,
                    },
                ],
                ret: t_arr,
            },
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let seq = to_native!(a0, XSequence);
                let start_idx = to_primitive!(a1, Int);
                if start_idx.is_zero() {
                    Ok(a0.clone().into())
                } else if start_idx.to_usize().map_or(true, |s| s > seq.len()) {
                    Ok(manage_native!(XSequence::Empty, rt))
                } else {
                    let arr =
                        seq.slice(start_idx.to_usize().unwrap(), seq.len(), ns, rt.clone())?;
                    Ok(manage_native!(XSequence::array(arr), rt))
                }
            },
        ),
    )
}

pub(crate) fn add_sequence_eq(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let eq_symbol = scope.get_identifier("eq");

    fn static_from_eq(t0: Arc<XType>, t1: Arc<XType>, eq_expr: XExpr) -> Rc<XStaticFunction> {
        Rc::new(XStaticFunction::from_native(
            XFuncSpec {
                generic_params: None,
                params: vec![
                    XFuncParamSpec {
                        type_: XSequenceType::xtype(t0),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: XSequenceType::xtype(t1),
                        required: true,
                    },
                ],
                ret: X_BOOL.clone(),
            },
            move |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let seq0 = to_native!(a0, XSequence);
                let seq1 = to_native!(a1, XSequence);
                if seq0.len() != seq1.len() {
                    return Ok(ManagedXValue::new(XValue::Bool(false), rt)?.into());
                }
                let arr0 = seq0.slice(0, seq0.len(), ns, rt.clone())?;
                let arr1 = seq1.slice(0, seq1.len(), ns, rt.clone())?;
                let mut ret = true;
                let inner_equal_value = eq_expr.eval(ns, false, rt.clone())?.unwrap_value();
                let inner_eq_func = to_primitive!(inner_equal_value, Function);
                for (x, y) in arr0.into_iter().zip(arr1.into_iter()) {
                    let eq = inner_eq_func.eval_values(&[x, y], ns, rt.clone())?;
                    let is_eq = to_primitive!(eq, Bool);
                    if !*is_eq {
                        ret = false;
                        break;
                    }
                }
                Ok(ManagedXValue::new(XValue::Bool(ret), rt)?.into())
            },
        ))
    }

    fn from_types(
        types: &[Arc<XType>],
        scope: &XCompilationScope,
        eq_symbol: Identifier,
    ) -> Result<Rc<XStaticFunction>, String> {
        if types.len() != 2 {
            return Err(format!("Expected 2 types, got {}", types.len()));
        }
        let a0 = types[0].clone();
        let t0 = match &a0.as_ref() {
            XType::XNative(nt0, bind) if nt0.name() == "Sequence" => bind[0].clone(),
            _ => return Err(format!("Expected sequence type, got {:?}", a0)), // todo improve
        };
        let a1 = types[1].clone();
        let t1 = match &a1.as_ref() {
            XType::XNative(nt1, bind) if nt1.name() == "Sequence" => bind[0].clone(),
            _ => return Err(format!("Expected sequence type, got {:?}", a1)), // todo improve
        };

        let inner_eq = scope.resolve_overload(eq_symbol, &[t0.clone(), t1.clone()])?; // todo ensure that the function returns a bool

        Ok(static_from_eq(t0, t1, inner_eq))
    }

    scope.add_dyn_func("eq", move |_params, types, ns| {
        from_types(types, ns, eq_symbol)
    })
}
