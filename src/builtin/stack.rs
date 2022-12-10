use crate::builtin::core::eval;
use crate::builtin::sequence::{XSequence, XSequenceType};
use crate::evaluation_scope::EvaluatedValue;
use crate::native_types::{NativeType, XNativeValue};
use crate::xtype::{XFuncSpec, X_INT, X_UNKNOWN};
use crate::xvalue::{ManagedXError, ManagedXValue, XValue};
use crate::{
    manage_native, to_native, xraise, CompilationError, RootCompilationScope, XStaticFunction,
    XType,
};
use derivative::Derivative;
use rc::Rc;
use std::fmt::Debug;
use std::io::Write;
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
struct StackNode<W: Write + 'static> {
    value: EvaluatedValue<W>,
    next: Option<Rc<StackNode<W>>>,
}

impl<W: Write + 'static> StackNode<W> {
    fn first(value: EvaluatedValue<W>) -> Rc<Self> {
        Rc::new(Self { value, next: None })
    }
    fn new(value: EvaluatedValue<W>, next: Rc<Self>) -> Rc<Self> {
        Rc::new(Self {
            value,
            next: Some(next),
        })
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub(super) struct XStack<W: Write + 'static> {
    head: Option<Rc<StackNode<W>>>,
    pub(super) length: usize,
}

impl<W: Write + 'static> Default for XStack<W> {
    fn default() -> Self {
        Self {
            head: None,
            length: 0,
        }
    }
}

impl<W: Write + 'static> XStack<W> {
    pub(super) fn new() -> Self {
        Self::default()
    }

    pub(super) fn push(&self, value: EvaluatedValue<W>) -> Self {
        let node = match self.head {
            None => StackNode::first(value),
            Some(ref head) => StackNode::new(value, head.clone()),
        };
        Self {
            head: Some(node),
            length: self.length + 1,
        }
    }

    fn to_vec<const REV: bool>(&self) -> Vec<EvaluatedValue<W>> {
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

    pub(super) fn iter(&self) -> impl Iterator<Item = EvaluatedValue<W>> + '_ {
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

impl<W: Write + 'static> XNativeValue for XStack<W> {
    fn size(&self) -> usize {
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
        (managed_count + 1) * size_of::<usize>()
    }
}

pub(crate) fn add_stack_type<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], _) = scope.generics_from_names(["T"]);
    scope.add_native_type("Stack", XStackType::xtype(t))
}

pub(crate) fn add_stack_new<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "stack",
        XFuncSpec::new(&[], XStackType::xtype(X_UNKNOWN.clone())),
        XStaticFunction::from_native(|_args, _ns, _tca, rt| {
            Ok(manage_native!(XStack::<W>::new(), rt))
        }),
    )
}

pub(crate) fn add_stack_push<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func(
        "push",
        XFuncSpec::new(&[&t_stk, &t], t_stk.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = eval(&args[1], ns, &rt)?;
            let stk0 = to_native!(a0, XStack<W>);
            Ok(manage_native!(stk0.push(a1), rt))
        }),
    )
}

pub(crate) fn add_stack_to_array<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func(
        "to_array",
        XFuncSpec::new(&[&t_stk], XSequenceType::xtype(t)).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let stk0 = to_native!(a0, XStack<W>);
            Ok(manage_native!(XSequence::array(stk0.to_vec::<false>()), rt))
        }),
    )
}

pub(crate) fn add_stack_to_array_reversed<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func(
        "to_array_reversed",
        XFuncSpec::new(&[&t_stk], XSequenceType::xtype(t)).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let stk0 = to_native!(a0, XStack<W>);
            Ok(manage_native!(XSequence::array(stk0.to_vec::<true>()), rt))
        }),
    )
}

pub(crate) fn add_stack_len<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_stk = XStackType::xtype(t);

    scope.add_func(
        "len",
        XFuncSpec::new(&[&t_stk], X_INT.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let stk0 = to_native!(a0, XStack<W>);
            Ok(ManagedXValue::new(XValue::Int(stk0.length.into()), rt)?.into())
        }),
    )
}

pub(crate) fn add_stack_head<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func(
        "head",
        XFuncSpec::new(&[&t_stk], t).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let stk0 = to_native!(a0, XStack<W>);
            match &stk0.head {
                Some(v) => Ok(v.value.clone().into()),
                None => Ok(Err(ManagedXError::new("stack is empty", rt)?).into()),
            }
        }),
    )
}

pub(crate) fn add_stack_tail<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_stk = XStackType::xtype(t);

    scope.add_func(
        "tail",
        XFuncSpec::new(&[&t_stk], t_stk.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let stk0 = to_native!(a0, XStack<W>);
            match &stk0.head {
                Some(v) => Ok(manage_native!(
                    XStack {
                        head: v.next.clone(),
                        length: stk0.length - 1,
                    },
                    rt
                )),
                None => xraise!(Err(ManagedXError::new("stack is empty", rt)?)),
            }
        }),
    )
}
