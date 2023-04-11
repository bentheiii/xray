use crate::builtin::core::{eval, get_func, unpack_dyn_types, unpack_native, xerr};
use crate::builtin::sequence::{XSequence, XSequenceType};
use crate::native_types::{NativeType, XNativeValue};
use crate::runtime_scope::RuntimeScope;
use crate::util::lazy_bigint::LazyBigint;
use crate::xexpr::XExpr;
use crate::xtype::{XFuncSpec, X_BOOL, X_INT, X_UNKNOWN};
use crate::xvalue::{ManagedXError, ManagedXValue, XFunctionFactoryOutput, XValue};
use crate::{
    forward_err, manage_native, to_native, to_primitive, xraise, CompilationError,
    RootCompilationScope, XStaticFunction, XType,
};
use derivative::Derivative;
use num_traits::ToPrimitive;
use rc::Rc;
use std::collections::hash_map::DefaultHasher;
use std::fmt::Debug;
use std::hash::Hasher;

use std::iter::from_fn;
use std::mem::size_of;
use std::rc;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub(super) struct XStackType {}

impl XStackType {
    pub(super) fn xtype(t: Arc<XType>) -> Arc<XType> {
        Arc::new(XType::XNative(Box::new(Self {}), vec![t]))
    }
}

impl NativeType for XStackType {
    fn generic_names(&self) -> Vec<String> {
        vec!["T".to_string()]
    }
    fn name(&self) -> &str {
        "Stack"
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
struct StackNode<W, R, T> {
    value: Rc<ManagedXValue<W, R, T>>,
    next: Option<Rc<StackNode<W, R, T>>>,
}

impl<W, R, T> StackNode<W, R, T> {
    fn first(value: Rc<ManagedXValue<W, R, T>>) -> Rc<Self> {
        Rc::new(Self { value, next: None })
    }
    fn new(value: Rc<ManagedXValue<W, R, T>>, next: Rc<Self>) -> Rc<Self> {
        Rc::new(Self {
            value,
            next: Some(next),
        })
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub(super) struct XStack<W, R, T> {
    head: Option<Rc<StackNode<W, R, T>>>,
    pub(super) length: usize,
}

impl<W, R, T> Default for XStack<W, R, T> {
    fn default() -> Self {
        Self {
            head: None,
            length: 0,
        }
    }
}

impl<W, R, T> XStack<W, R, T> {
    pub(super) fn new() -> Self {
        Self::default()
    }

    pub(super) fn push(&self, value: Rc<ManagedXValue<W, R, T>>) -> Self {
        let node = match self.head {
            None => StackNode::first(value),
            Some(ref head) => StackNode::new(value, head.clone()),
        };
        Self {
            head: Some(node),
            length: self.length + 1,
        }
    }

    fn to_vec<const REV: bool>(&self) -> Vec<Rc<ManagedXValue<W, R, T>>> {
        let mut vec = Vec::with_capacity(self.length);
        let mut node = &self.head;
        while let Some(ref n) = node {
            vec.push(n.value.clone());
            node = &n.next;
        }
        if !REV {
            vec.reverse();
        }
        vec
    }

    pub(super) fn iter(&self) -> impl Iterator<Item = Rc<ManagedXValue<W, R, T>>> + '_ {
        let mut node = &self.head;
        from_fn(move || match &node {
            None => None,
            Some(n) => {
                let ret = n.value.clone();
                node = &n.next;
                Some(ret)
            }
        })
    }
}

impl<W: 'static, R: 'static, T: 'static> XNativeValue for XStack<W, R, T> {
    fn dyn_size(&self) -> usize {
        let mut managed_count = 0;
        let mut node = &self.head;
        while let Some(ref n) = node {
            if Rc::strong_count(n) > 1 {
                // if the strong count is higher than 1, someone else already counted everything
                // after this towards the limit
                break;
            }
            managed_count += 1;
            node = &n.next;
        }
        if node.is_none() {
            managed_count += 1;
        }
        (managed_count + 1) * size_of::<Rc<ManagedXValue<W, R, T>>>()
    }
}

pub(crate) fn add_stack_type<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], _) = scope.generics_from_names(["T"]);
    scope.add_native_type("Stack", XStackType::xtype(t))
}

pub(crate) fn add_stack_new<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "stack",
        XFuncSpec::new(&[], XStackType::xtype(X_UNKNOWN.clone())),
        XStaticFunction::from_native(|_args, _ns, _tca, rt| {
            Ok(manage_native!(XStack::<W, R, T>::new(), rt))
        }),
    )
}

pub(crate) fn add_stack_push<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func(
        "push",
        XFuncSpec::new(&[&t_stk, &t], t_stk.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let stk0 = to_native!(a0, XStack<W, R, T>);
            Ok(manage_native!(stk0.push(a1), rt))
        }),
    )
}

pub(crate) fn add_stack_to_array<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func(
        "to_array",
        XFuncSpec::new(&[&t_stk], XSequenceType::xtype(t)).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let stk0 = to_native!(a0, XStack<W, R, T>);
            rt.can_allocate(stk0.length* size_of::<usize>())?;
            Ok(manage_native!(XSequence::array(stk0.to_vec::<false>()), rt))
        }),
    )
}

pub(crate) fn add_stack_to_array_reversed<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func(
        "to_array_reversed",
        XFuncSpec::new(&[&t_stk], XSequenceType::xtype(t)).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let stk0 = to_native!(a0, XStack<W, R, T>);
            rt.can_allocate(stk0.length* size_of::<usize>())?;
            Ok(manage_native!(XSequence::array(stk0.to_vec::<true>()), rt))
        }),
    )
}

pub(crate) fn add_stack_len<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_stk = XStackType::xtype(t);

    scope.add_func(
        "len",
        XFuncSpec::new(&[&t_stk], X_INT.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let stk0 = to_native!(a0, XStack<W, R, T>);
            Ok(ManagedXValue::new(XValue::Int(stk0.length.into()), rt)?.into())
        }),
    )
}

pub(crate) fn add_stack_head<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func(
        "head",
        XFuncSpec::new(&[&t_stk], t).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let stk0 = to_native!(a0, XStack<W, R, T>);
            match &stk0.head {
                Some(v) => Ok(v.value.clone().into()),
                None => Ok(Err(ManagedXError::new("stack is empty", rt)?).into()),
            }
        }),
    )
}

pub(crate) fn add_stack_tail<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_stk = XStackType::xtype(t);

    scope.add_func(
        "tail",
        XFuncSpec::new(&[&t_stk], t_stk.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let stk0 = to_native!(a0, XStack<W, R, T>);
            match &stk0.head {
                Some(v) => Ok(manage_native!(
                    XStack {
                        head: v.next.clone(),
                        length: stk0.length - 1,
                    },
                    rt
                )),
                None => xerr(ManagedXError::new("stack is empty", rt)?),
            }
        }),
    )
}

pub(crate) fn add_stack_dyn_eq<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let eq_symbol = scope.identifier("eq");

    scope.add_dyn_func("eq", "stacks", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let [a0, a1] = unpack_dyn_types(types)?;
        let [t0] = unpack_native(a0, "Stack")? else { unreachable!() };
        let [t1] = unpack_native(a1, "Stack")? else { unreachable!() };
        let inner_eq = get_func(ns, eq_symbol, &[t0.clone(), t1.clone()], &X_BOOL)?;

        Ok(XFunctionFactoryOutput::from_delayed_native(
            XFuncSpec::new(
                &[
                    &XStackType::xtype(t0.clone()),
                    &XStackType::xtype(t1.clone()),
                ],
                X_BOOL.clone(),
            ),
            move |ns, rt| {
                let inner_value = forward_err!(ns.eval(&inner_eq, rt, false)?.unwrap_value());
                Ok(Ok(
                    move |args: &[XExpr<W, R, T>], ns: &RuntimeScope<'_, W, R, T>, _tca, rt| {
                        let a0 = xraise!(eval(&args[0], ns, &rt)?);
                        let a1 = xraise!(eval(&args[1], ns, &rt)?);
                        let s0 = &to_native!(a0, XStack<W, R, T>);
                        let s1 = &to_native!(a1, XStack<W, R, T>);
                        if s0.length != s1.length {
                            return Ok(ManagedXValue::new(XValue::Bool(false), rt)?.into());
                        }
                        let func = to_primitive!(inner_value, Function);
                        for (i0, i1) in s0.iter().zip(s1.iter()) {
                            let eq = *to_primitive!(
                                xraise!(ns
                                    .eval_func_with_values(
                                        func,
                                        vec![Ok(i0), Ok(i1)],
                                        rt.clone(),
                                        false
                                    )?
                                    .unwrap_value()),
                                Bool
                            );
                            if !eq {
                                return Ok(ManagedXValue::new(XValue::Bool(false), rt)?.into());
                            }
                        }
                        Ok(ManagedXValue::new(XValue::Bool(true), rt)?.into())
                    },
                ))
            },
        ))
    })
}

pub(crate) fn add_stack_dyn_hash<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let symbol = scope.identifier("hash");

    scope.add_dyn_func("hash", "stacks", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let [a0] = unpack_dyn_types(types)?;
        let [t0] = unpack_native(a0, "Stack")? else { unreachable!() };

        let inner = get_func(ns, symbol, &[t0.clone()], &X_INT)?;


        Ok(XFunctionFactoryOutput::from_delayed_native(
            XFuncSpec::new(&[&XStackType::xtype(t0.clone())], X_INT.clone()),
            move |ns, rt| {
                let inner_value = forward_err!(ns.eval(&inner, rt, false)?.unwrap_value());


                Ok(Ok(move |args: &[XExpr<W, R, T>], ns: &RuntimeScope<'_, W, R, T>, _tca, rt| {
                    let a0 = xraise!(eval(&args[0], ns, &rt)?);
                    let seq0 = to_native!(a0, XStack<W, R, T>);
                    let mut hasher = DefaultHasher::new();
                    let inner_func = to_primitive!(inner_value, Function);

                    for x in seq0.iter() {
                        let hash = xraise!(ns
                        .eval_func_with_values(inner_func, vec![Ok(x)], rt.clone(), false)?
                        .unwrap_value());
                        let Some(f) = to_primitive!(hash, Int).to_u64() else { return xerr(ManagedXError::new("hash out of bounds", rt)?); };
                        hasher.write_u64(f);
                    }
                    let ret = hasher.finish();

                    Ok(ManagedXValue::new(XValue::Int(LazyBigint::from(ret)), rt)?.into())
                }))
            },
        ))
    })
}
