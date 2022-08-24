use crate::builtin::sequence::{XSequence, XSequenceType};
use crate::native_types::{NativeType, XNativeValue};
use crate::xtype::{XFuncParamSpec, XFuncSpec, X_INT, X_UNKNOWN};
use crate::xvalue::{ManagedXValue, XValue};
use crate::{
    eval, manage_native, to_native, CompilationError, RootCompilationScope, XStaticFunction, XType,
};
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

#[derive(Debug)]
struct StackNode<W: Write + Debug + 'static> {
    value: Rc<ManagedXValue<W>>,
    next: Option<Rc<StackNode<W>>>,
}

impl<W: Write + Debug + 'static> StackNode<W> {
    fn first(value: Rc<ManagedXValue<W>>) -> Rc<Self> {
        Rc::new(Self { value, next: None })
    }
    fn new(value: Rc<ManagedXValue<W>>, next: Rc<Self>) -> Rc<Self> {
        Rc::new(Self {
            value,
            next: Some(next),
        })
    }
}

#[derive(Debug)]
pub(super) struct XStack<W: Write + Debug + 'static> {
    head: Option<Rc<StackNode<W>>>,
    pub(super) length: usize,
}

impl<W: Write + Debug + 'static> Default for XStack<W>{
    fn default() -> Self {
        Self{
            head: None,
            length: 0
        }
    }
}

impl<W: Write + Debug + 'static> XStack<W> {
    pub(super) fn new() -> Self {
        Self::default()
    }

    pub(super) fn push(&self, value: Rc<ManagedXValue<W>>) -> Self {
        let node = match self.head {
            None => StackNode::first(value),
            Some(ref head) => StackNode::new(value, head.clone()),
        };
        Self {
            head: Some(node),
            length: self.length + 1,
        }
    }

    fn to_vec<const REV: bool>(&self) -> Vec<Rc<ManagedXValue<W>>> {
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

    pub(super) fn iter(&self) -> impl Iterator<Item = Rc<ManagedXValue<W>>> + '_ {
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

impl<W: Write + Debug + 'static> XNativeValue for XStack<W> {
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

pub(crate) fn add_stack_type<W: Write + Debug + 'static>(scope: &mut RootCompilationScope<W>) -> Result<(), CompilationError<W>> {
    let ([t], _) = scope.generics_from_names(["T"]);
    scope.add_native_type("Stack", XStackType::xtype(t))
}

pub(crate) fn add_stack_new<W: Write + Debug + 'static>(scope: &mut RootCompilationScope<W>) -> Result<(), CompilationError<W>> {
    scope.add_func(
        "stack",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: None,
                params: vec![],
                ret: XStackType::xtype(X_UNKNOWN.clone()),
            },
            |_args, _ns, _tca, rt| Ok(manage_native!(XStack::<W>::new(), rt)),
        ),
    )
}

pub(crate) fn add_stack_push<W: Write + Debug + 'static>(scope: &mut RootCompilationScope<W>) -> Result<(), CompilationError<W>> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func(
        "push",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: t_stk.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: t,
                        required: true,
                    },
                ],
                ret: t_stk,
            },
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let stk0 = to_native!(a0, XStack<W>);
                Ok(manage_native!(stk0.push(a1), rt))
            },
        ),
    )
}

pub(crate) fn add_stack_to_array<W: Write + Debug + 'static>(scope: &mut RootCompilationScope<W>) -> Result<(), CompilationError<W>> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func(
        "to_array",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![XFuncParamSpec {
                    type_: t_stk,
                    required: true,
                }],
                ret: XSequenceType::xtype(t),
            },
            |args, ns, _tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                let stk0 = to_native!(a0, XStack<W>);
                Ok(manage_native!(XSequence::array(stk0.to_vec::<false>()), rt))
            },
        ),
    )
}

pub(crate) fn add_stack_to_array_reversed<W: Write + Debug + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func(
        "to_array_reversed",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![XFuncParamSpec {
                    type_: t_stk,
                    required: true,
                }],
                ret: XSequenceType::xtype(t),
            },
            |args, ns, _tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                let stk0 = to_native!(a0, XStack<W>);
                Ok(manage_native!(XSequence::array(stk0.to_vec::<true>()), rt))
            },
        ),
    )
}

pub(crate) fn add_stack_len<W: Write + Debug + 'static>(scope: &mut RootCompilationScope<W>) -> Result<(), CompilationError<W>> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_stk = XStackType::xtype(t);

    scope.add_func(
        "len",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![XFuncParamSpec {
                    type_: t_stk,
                    required: true,
                }],
                ret: X_INT.clone(),
            },
            |args, ns, _tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                let stk0 = to_native!(a0, XStack<W>);
                Ok(ManagedXValue::new(XValue::Int(stk0.length.into()), rt)?.into())
            },
        ),
    )
}

pub(crate) fn add_stack_head<W: Write + Debug + 'static>(scope: &mut RootCompilationScope<W>) -> Result<(), CompilationError<W>> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func(
        "head",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![XFuncParamSpec {
                    type_: t_stk,
                    required: true,
                }],
                ret: t,
            },
            |args, ns, _tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                let stk0 = to_native!(a0, XStack<W>);
                match &stk0.head {
                    Some(v) => Ok(v.value.clone().into()),
                    None => Err("stack is empty".to_string()),
                }
            },
        ),
    )
}

pub(crate) fn add_stack_tail<W: Write + Debug + 'static>(scope: &mut RootCompilationScope<W>) -> Result<(), CompilationError<W>> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let t_stk = XStackType::xtype(t);

    scope.add_func(
        "tail",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![XFuncParamSpec {
                    type_: t_stk.clone(),
                    required: true,
                }],
                ret: t_stk,
            },
            |args, ns, _tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                let stk0 = to_native!(a0, XStack<W>);
                match &stk0.head {
                    Some(v) => Ok(manage_native!(
                        XStack {
                            head: v.next.clone(),
                            length: stk0.length - 1,
                        },
                        rt
                    )),
                    None => Err("stack is empty".to_string()),
                }
            },
        ),
    )
}
