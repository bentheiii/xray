use std::rc;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use crate::{add_binop, add_ufunc, add_ufunc_ref, Bind, XArray, XArrayType, XCompilationScope, XSet, XSetType, XStaticFunction, XType};
use crate::xtype::{X_BOOL, X_INT, X_RATIONAL, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec};
use crate::xvalue::{XValue};
use rc::Rc;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use crate::native_types::{NativeType, XNativeValue};
use derivative::Derivative;

#[derive(Debug, Clone)]
struct XStackType {}

impl XStackType {
    pub fn xtype(t: Arc<XType>) -> Arc<XType> {
        Arc::new(XType::XNative(Box::new(Self {}),
                                HashMap::from([('T'.to_string(), t)])))
    }
}

impl NativeType for XStackType {
    fn generic_names(&self) -> Vec<String> {
        vec!["T".to_string()]
    }
    fn name(&self) -> String {
        "Stack".to_string()
    }
}

#[derive(Debug, Eq, PartialEq, Derivative)]
#[derivative(Hash)]
struct StackNode {
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
struct XStack {
    head: Option<Rc<StackNode>>,
    length: usize,
}

impl XStack {
    fn new() -> XStack {
        XStack {
            head: None,
            length: 0,
        }
    }

    fn push(&self, value: Rc<XValue>) -> Self {
        match self.head {
            None => {
                let node = StackNode::first(value);
                XStack {
                    head: Some(node.clone()),
                    length: 1,
                }
            }
            Some(ref head) => {
                let node = StackNode::new(value, head.clone());
                XStack {
                    head: Some(node),
                    length: self.length + 1,
                }
            }
        }
    }

    fn to_vec(&self) -> Vec<Rc<XValue>> {
        let mut vec = Vec::new();
        let mut node = &self.head;
        while let Some(ref n) = node {
            vec.push(n.value.clone());
            node = &n.next;
        }
        vec.reverse();
        vec
    }

    fn to_set(&self) -> HashSet<Rc<XValue>> {
        let mut ret = HashSet::new();
        let mut node = &self.head;
        while let Some(ref n) = node {
            ret.insert(n.value.clone());
            node = &n.next;
        }
        ret
    }
}

impl XNativeValue for XStack {}

pub fn add_stack_type(scope: &mut XCompilationScope) -> Result<(), String> {
    scope.add_native_type("Stack", XStackType::xtype(XType::XGeneric("T".to_string()).into()))
}

pub fn add_stack_new(scope: &mut XCompilationScope) -> Result<(), String> {
    scope.add_func(
        "stack", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["T".to_string()]),
            params: vec![],
            ret: XStackType::xtype(X_UNKNOWN.clone()),
        }, |args, ns, _tca| {
            Ok(XValue::Native(Box::new(XStack::new())).into())
        }))?;
    Ok(())
}

pub fn add_stack_push(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func(
        "push", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["T".to_string()]),
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
            let v0 = args[0].eval(&ns, false)?.unwrap_value();
            let el = args[1].eval(&ns, false)?.unwrap_value();
            match v0.as_ref() {
                XValue::Native(b0) => {
                    let stk = b0.as_ref()._as_any().downcast_ref::<XStack>().unwrap();
                    Ok(XValue::Native(Box::new(stk.push(el))).into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}

pub fn add_stack_to_array(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func(
        "to_array", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["T".to_string()]),
            params: vec![
                XFuncParamSpec {
                    type_: t_stk.clone(),
                    required: true,
                },
            ],
            ret: XArrayType::xtype(t.clone()),
        }, |args, ns, _tca| {
            let v0 = args[0].eval(&ns, false)?.unwrap_value();
            match v0.as_ref() {
                XValue::Native(b0) => {
                    let stk = b0.as_ref()._as_any().downcast_ref::<XStack>().unwrap();
                    Ok(XValue::Native(Box::new(XArray::new(stk.to_vec()))).into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}

pub fn add_stack_to_set(scope: &mut XCompilationScope) -> Result<(), String> {
    let t = Arc::new(XType::XGeneric("T".to_string()));
    let t_stk = XStackType::xtype(t.clone());

    scope.add_func(
        "to_set", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(vec!["T".to_string()]),
            params: vec![
                XFuncParamSpec {
                    type_: t_stk.clone(),
                    required: true,
                },
            ],
            ret: XSetType::xtype(t.clone()),
        }, |args, ns, _tca| {
            let v0 = args[0].eval(&ns, false)?.unwrap_value();
            match v0.as_ref() {
                XValue::Native(b0) => {
                    let stk = b0.as_ref()._as_any().downcast_ref::<XStack>().unwrap();
                    Ok(XValue::Native(Box::new(XSet::new(stk.to_set()))).into())
                }
                _ => unreachable!(),
            }
        }))?;
    Ok(())
}