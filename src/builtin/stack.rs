use std::rc;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use crate::{add_binop, add_ufunc, add_ufunc_ref, Bind, CompilationError, eval, intern, manage_native, to_native, XArray, XArrayType, XCompilationScope, XStaticFunction, XType};
use crate::xtype::{X_BOOL, X_INT, X_RATIONAL, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec};
use crate::xvalue::{ManagedXValue, XValue};
use rc::Rc;
use std::any::Any;
use std::collections::{HashMap, HashSet};
use std::iter::from_fn;
use std::mem::size_of;
use std::sync::Arc;
use crate::native_types::{NativeType, XNativeValue};
use derivative::Derivative;
use string_interner::StringInterner;
use crate::CompilationError::PairNotType;

#[derive(Debug, Clone)]
pub struct XStackType {}

impl XStackType {
    pub fn xtype(t: Arc<XType>) -> Arc<XType> {
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
pub struct StackNode {
    value: Rc<ManagedXValue>,
    next: Option<Rc<StackNode>>,
}

impl StackNode {
    fn first(value: Rc<ManagedXValue>) -> Rc<StackNode> {
        Rc::new(StackNode { value, next: None })
    }
    fn new(value: Rc<ManagedXValue>, next: Rc<StackNode>) -> Rc<StackNode> {
        Rc::new(StackNode { value, next: Some(next) })
    }
}

#[derive(Debug)]
pub struct XStack {
    pub head: Option<Rc<StackNode>>,
    pub length: usize,
}

impl XStack {
    pub fn new() -> Self {
        XStack {
            head: None,
            length: 0,
        }
    }

    pub fn push(&self, value: Rc<ManagedXValue>) -> Self {
        let node = match self.head {
            None => StackNode::first(value),
            Some(ref head) => StackNode::new(value, head.clone())
        };
        XStack {
            head: Some(node),
            length: self.length + 1,
        }
    }

    fn to_vec<const REV: bool>(&self) -> Vec<Rc<ManagedXValue>> {
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

    pub fn iter(&self) -> impl Iterator<Item=Rc<ManagedXValue>> + '_ {
        let mut node = &self.head;
        return from_fn(move || {
            match &node {
                None => None,
                Some(n) => {
                    let ret = n.value.clone();
                    node = &n.next;
                    Some(ret)
                }
            }
        })
    }
}

impl XNativeValue for XStack {
    fn size(&self) -> usize {
        let mut managed_count = 0;
        let mut node = &self.head;
        while let Some(ref n) = node {
            if Rc::strong_count(n) > 1 {
                // if the strong count is higher than 0, someone else already counted everything
                // after this towards a limit
                break;
            }
            managed_count += 1;
            node = &n.next;
        }
        if node.is_none(){
            managed_count+=1;
        }
        (managed_count + 1) * size_of::<usize>()
    }
}

pub fn add_stack_type(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    scope.add_native_type_intern("Stack", XStackType::xtype(XType::generic_from_name("T", interner)), interner)
}

pub fn add_stack_new(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    scope.add_func_intern(
        "stack", XStaticFunction::from_native(XFuncSpec {
            generic_params: None,
            params: vec![],
            ret: XStackType::xtype(X_UNKNOWN.clone()),
        }, |args, ns, _tca, rt| {
            Ok(manage_native!(XStack::new(), rt))
        }), interner)?;
    Ok(())
}

pub fn add_stack_push(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let t = XType::generic_from_name("T", interner);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func_intern(
        "push", XStaticFunction::from_native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: t_stk.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: t.clone(),
                    required: true,
                },
            ],
            ret: t_stk.clone(),
        }, |args, ns, _tca, rt| {
            let (a0, a1) = eval!(args, ns, rt, 0, 1);
            let stk0 = to_native!(a0, XStack);
            Ok(manage_native!(stk0.push(a1), rt))
        }), interner)?;
    Ok(())
}

pub fn add_stack_to_array(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let t = XType::generic_from_name("T", interner);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func_intern(
        "to_array", XStaticFunction::from_native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: t_stk.clone(),
                    required: true,
                },
            ],
            ret: XArrayType::xtype(t.clone()),
        }, |args, ns, _tca, rt| {
            let (a0, ) = eval!(args, ns, rt, 0);
            let stk0 = to_native!(a0, XStack);
            Ok(manage_native!(XArray::new(stk0.to_vec::<false>()), rt))
        }), interner)?;
    Ok(())
}

pub fn add_stack_to_array_reversed(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let t = XType::generic_from_name("T", interner);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func_intern(
        "to_array_reversed", XStaticFunction::from_native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: t_stk.clone(),
                    required: true,
                },
            ],
            ret: XArrayType::xtype(t.clone()),
        }, |args, ns, _tca, rt| {
            let (a0, ) = eval!(args, ns, rt, 0);
            let stk0 = to_native!(a0, XStack);
            Ok(manage_native!(XArray::new(stk0.to_vec::<true>()), rt))
        }), interner)?;
    Ok(())
}

pub fn add_stack_len(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let t = XType::generic_from_name("T", interner);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func_intern(
        "len", XStaticFunction::from_native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: t_stk.clone(),
                    required: true,
                },
            ],
            ret: X_INT.clone(),
        }, |args, ns, _tca, rt| {
            let (a0, ) = eval!(args, ns, rt, 0);
            let stk0 = to_native!(a0, XStack);
            Ok(ManagedXValue::new(XValue::Int(stk0.length.into()), rt)?.into())
        }), interner)?;
    Ok(())
}

pub fn add_stack_head(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let t = XType::generic_from_name("T", interner);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func_intern(
        "head", XStaticFunction::from_native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: t_stk.clone(),
                    required: true,
                },
            ],
            ret: t.clone(),
        }, |args, ns, _tca, rt| {
            let (a0, ) = eval!(args, ns, rt, 0);
            let stk0 = to_native!(a0, XStack);
            match &stk0.head {
                Some(v) => Ok(v.value.clone().into()),
                None => Err("stack is empty".to_string()),
            }
        }), interner)?;
    Ok(())
}

pub fn add_stack_tail(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let t = XType::generic_from_name("T", interner);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func_intern(
        "tail", XStaticFunction::from_native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: t_stk.clone(),
                    required: true,
                },
            ],
            ret: t_stk.clone(),
        }, |args, ns, _tca, rt| {
            let (a0, ) = eval!(args, ns, rt, 0);
            let stk0 = to_native!(a0, XStack);
            match &stk0.head {
                Some(v) => Ok(manage_native!(XStack {
                    head: v.next.clone(),
                    length: stk0.length - 1,
                }, rt)),
                None => Err("stack is empty".to_string()),
            }
        }), interner)?;
    Ok(())
}