use std::rc;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use crate::{add_binop, add_ufunc, add_ufunc_ref, Bind, eval, intern, to_native, XArray, XArrayType, XCompilationScope, XSet, XSetType, XStaticFunction, XType};
use crate::xtype::{X_BOOL, X_INT, X_RATIONAL, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec};
use crate::xvalue::{XValue};
use rc::Rc;
use std::collections::{HashMap, HashSet};
use std::mem::size_of;
use std::sync::Arc;
use crate::native_types::{NativeType, XNativeValue};
use derivative::Derivative;
use string_interner::StringInterner;

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

#[derive(Debug, Eq, PartialEq, Derivative)]
#[derivative(Hash)]
pub struct StackNode {
    value: Rc<XValue>,
    #[derivative(Hash = "ignore")]
    next: Option<Rc<StackNode>>,
}

impl StackNode {
    fn first(value: Rc<XValue>) -> Rc<StackNode> {
        Rc::new(StackNode { value, next: None })
    }
    fn new(value: Rc<XValue>, next: Rc<StackNode>) -> Rc<StackNode> {
        Rc::new(StackNode { value, next: Some(next) })
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
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

    pub fn push(&self, value: Rc<XValue>) -> Self {
        let node = match self.head {
            None => StackNode::first(value),
            Some(ref head) => StackNode::new(value, head.clone())
        };
        XStack {
            head: Some(node),
            length: self.length + 1,
        }
    }

    fn to_vec<const REV: bool>(&self) -> Vec<Rc<XValue>> {
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

    fn to_set(&self) -> HashSet<Rc<XValue>> {
        let mut ret = HashSet::with_capacity(self.length);
        let mut node = &self.head;
        while let Some(ref n) = node {
            ret.insert(n.value.clone());
            node = &n.next;
        }
        ret
    }
}

impl XNativeValue for XStack {
    fn size(&self) -> usize {
        self.length * size_of::<usize>()
    }
}

pub fn add_stack_type(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    scope.add_native_type_intern("Stack", XStackType::xtype(XType::generic_from_name("T", interner)), interner)
}

pub fn add_stack_new(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    scope.add_func_intern(
        "stack", XStaticFunction::Native(XFuncSpec {
            generic_params: None,
            params: vec![],
            ret: XStackType::xtype(X_UNKNOWN.clone()),
        }, |args, ns, _tca| {
            Ok(XValue::Native(Box::new(XStack::new())).into())
        }), interner)?;
    Ok(())
}

pub fn add_stack_push(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func_intern(
        "push", XStaticFunction::Native(XFuncSpec {
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
        }, |args, ns, _tca| {
            let (a0, a1) = eval!(args, ns, 0, 1);
            let stk0 = to_native!(a0, XStack);
            Ok(XValue::Native(Box::new(stk0.push(a1))).into())
        }), interner)?;
    Ok(())
}

pub fn add_stack_to_array(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func_intern(
        "to_array", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: t_stk.clone(),
                    required: true,
                },
            ],
            ret: XArrayType::xtype(t.clone()),
        }, |args, ns, _tca| {
            let (a0, ) = eval!(args, ns, 0);
            let stk0 = to_native!(a0, XStack);
            Ok(XValue::Native(Box::new(XArray::new(stk0.to_vec::<false>()))).into())
        }), interner)?;
    Ok(())
}

pub fn add_stack_to_array_reversed(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func_intern(
        "to_array_reversed", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: t_stk.clone(),
                    required: true,
                },
            ],
            ret: XArrayType::xtype(t.clone()),
        }, |args, ns, _tca| {
            let (a0, ) = eval!(args, ns, 0);
            let stk0 = to_native!(a0, XStack);
            Ok(XValue::Native(Box::new(XArray::new(stk0.to_vec::<true>()))).into())
        }), interner)?;
    Ok(())
}

pub fn add_stack_to_set(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func_intern(
        "to_set", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: t_stk.clone(),
                    required: true,
                },
            ],
            ret: XSetType::xtype(t.clone()),
        }, |args, ns, _tca| {
            let (a0, ) = eval!(args, ns, 0);
            let stk0 = to_native!(a0, XStack);
            Ok(XValue::Native(Box::new(XSet::new(stk0.to_set()))).into())
        }), interner)?;
    Ok(())
}

pub fn add_stack_len(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func_intern(
        "len", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: t_stk.clone(),
                    required: true,
                },
            ],
            ret: X_INT.clone(),
        }, |args, ns, _tca| {
            let (a0, ) = eval!(args, ns, 0);
            let stk0 = to_native!(a0, XStack);
            Ok(XValue::Int(stk0.length.into()).into()).into()
        }), interner)?;
    Ok(())
}

pub fn add_stack_head(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func_intern(
        "head", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: t_stk.clone(),
                    required: true,
                },
            ],
            ret: t.clone(),
        }, |args, ns, _tca| {
            let (a0, ) = eval!(args, ns, 0);
            let stk0 = to_native!(a0, XStack);
            match &stk0.head {
                Some(v) => Ok(v.value.clone().into()),
                None => Err("stack is empty".to_string()),
            }
        }), interner)?;
    Ok(())
}

pub fn add_stack_tail(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func_intern(
        "tail", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: t_stk.clone(),
                    required: true,
                },
            ],
            ret: t_stk.clone(),
        }, |args, ns, _tca| {
            let (a0, ) = eval!(args, ns, 0);
            let stk0 = to_native!(a0, XStack);
            match &stk0.head {
                Some(v) => Ok(XValue::Native(Box::new(XStack {
                    head: v.next.clone(),
                    length: stk0.length - 1,
                })).into()),
                None => Err("stack is empty".to_string()),
            }
        }), interner)?;
    Ok(())
}