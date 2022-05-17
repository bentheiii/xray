use std::rc;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use crate::{add_binop, add_ufunc, add_ufunc_ref, Bind, eval, intern, to_native, XCompilationScope, XStaticFunction, XType};
use crate::xtype::{X_BOOL, X_INT, X_RATIONAL, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec};
use crate::xvalue::{XValue};
use rc::Rc;
use std::collections::{HashMap, HashSet};
use std::mem::size_of;
use std::sync::Arc;
use crate::native_types::{NativeType, XNativeValue};
use derivative::Derivative;
use itertools::interleave;
use string_interner::StringInterner;
use crate::builtin::stack::{XStack, XStackType};

#[derive(Debug, Clone)]
pub struct XSetType {}

impl XSetType {
    pub fn xtype(t: Arc<XType>) -> Arc<XType> {
        Arc::new(XType::XNative(Box::new(Self {}),
                                vec![t.clone()]))
    }
}

impl NativeType for XSetType {
    fn generic_names(&self) -> Vec<String> {
        vec!["T".to_string()]
    }
    fn name(&self) -> &str { "Set" }
}

#[derive(Debug, Eq, PartialEq, Derivative)]
#[derivative(Hash)]
pub struct XSet {
    #[derivative(Hash = "ignore")]
    pub value: HashSet<Rc<XValue>>,
}

impl XSet {
    pub fn new(value: HashSet<Rc<XValue>>) -> Self {
        Self { value }
    }
}

impl XNativeValue for XSet {
    fn size(&self) -> usize {
        self.value.len() * size_of::<usize>()
    }
}

pub fn add_set_type(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    scope.add_native_type_intern("Set", XSetType::xtype(XType::generic_from_name("T", interner)), interner)
}

pub fn add_set_bitor(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    let set_t = XSetType::xtype(t.clone());

    scope.add_func_intern(
        "bit_or", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: set_t.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: set_t.clone(),
                    required: true,
                },
            ],
            ret: set_t.clone(),
        }, |args, ns, tca| {
            let (a0, ) = eval!(args, ns, 0);
            let set0 = &to_native!(a0, XSet).value;
            if set0.is_empty() {
                return Ok(args[1].eval(&ns, tca)?);
            }
            let (a1, ) = eval!(args, ns, 1);
            let set1 = &to_native!(a1, XSet).value;
            if set1.is_empty() {
                return Ok(a0.into());
            }
            Ok(XValue::Native(Box::new(XSet::new(set0.union(set1).cloned().collect()))).into())
        }), interner)?;
    Ok(())
}

pub fn add_set_bitand(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    let set_t = XSetType::xtype(t.clone());

    scope.add_func_intern(
        "bit_and", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: set_t.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: set_t.clone(),
                    required: true,
                },
            ],
            ret: set_t.clone(),
        }, |args, ns, _tca| {
            let (a0, ) = eval!(args, ns, 0);
            let set0 = &to_native!(a0, XSet).value;
            if set0.is_empty() {
                return Ok(a0.into());
            }
            let (a1, ) = eval!(args, ns, 1);
            let set1 = &to_native!(a1, XSet).value;
            if set1.is_empty() {
                return Ok(a1.into());
            }
            Ok(XValue::Native(Box::new(XSet::new(set0.intersection(set1).cloned().collect()))).into())
        }), interner)?;
    Ok(())
}

pub fn add_set_bitxor(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    let set_t = XSetType::xtype(t);

    scope.add_func_intern(
        "bit_xor", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: set_t.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: set_t.clone(),
                    required: true,
                },
            ],
            ret: set_t.clone(),
        }, |args, ns, tca| {
            let (a0, ) = eval!(args, ns, 0);
            let set0 = &to_native!(a0, XSet).value;
            if set0.is_empty() {
                return Ok(args[1].eval(&ns, tca)?);
            }
            let (a1, ) = eval!(args, ns, 1);
            let set1 = &to_native!(a1, XSet).value;
            if set1.is_empty() {
                return Ok(a0.into());
            }
            Ok(XValue::Native(Box::new(XSet::new(set0.symmetric_difference(set1).cloned().collect()))).into())
        }), interner)?;
    Ok(())
}

pub fn add_set_sub(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);
    let set_t = XSetType::xtype(t.clone());

    scope.add_func_intern(
        "sub", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: set_t.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: set_t.clone(),
                    required: true,
                },
            ],
            ret: set_t.clone(),
        }, |args, ns, _tca| {
            let (a0, ) = eval!(args, ns, 0);
            let set0 = &to_native!(a0, XSet).value;
            if set0.is_empty() {
                return Ok(a0.into());
            }
            let (a1, ) = eval!(args, ns, 1);
            let set1 = &to_native!(a1, XSet).value;
            if set1.is_empty() {
                return Ok(a0.into());
            }
            Ok(XValue::Native(Box::new(XSet::new(set0.difference(set1).cloned().collect()))).into())
        }), interner)?;
    Ok(())
}

pub fn add_set_to_stack(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), String> {
    let t = XType::generic_from_name("T", interner);

    scope.add_func_intern(
        "to_stack", XStaticFunction::Native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: XSetType::xtype(t.clone()),
                    required: true,
                },
            ],
            ret: XStackType::xtype(t.clone()),
        }, |args, ns, _tca| {
            let (a0, ) = eval!(args, ns, 0);
            let set0 = &to_native!(a0, XSet).value;
            let mut ret = XStack::new();
            for x in set0 {
                ret = ret.push(x.clone());
            }
            Ok(XValue::Native(Box::new(ret)).into())
        }), interner)?;
    Ok(())
}