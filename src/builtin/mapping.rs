use std::rc;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use crate::{add_binop, add_ufunc, add_ufunc_ref, Bind, CompilationError, eval, Identifier, intern, manage_native, RTCell, to_native, to_primitive, XSequence, XSequenceType, XCallableSpec, XCompilationScope, XEvaluationScope, XOptional, XOptionalType, XStaticFunction, XType};
use crate::xtype::{X_BOOL, X_INT, X_RATIONAL, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec};
use crate::xvalue::{ManagedXValue, XValue};
use rc::Rc;
use std::any::{Any, TypeId};
use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::iter::{Once, once};
use std::mem::size_of;
use std::ops::Deref;
use std::sync::Arc;
use string_interner::StringInterner;
use crate::builtin::stack::{XStack, XStackType};
use crate::native_types::{NativeType, RuntimeEquatable, XNativeValue};
use crate::XType::XCallable;

use crate::xexpr::{TailedEvalResult, XExpr};

#[derive(Debug, Clone)]
pub struct XMappingType;

impl XMappingType {
    pub fn xtype(k: Arc<XType>, v: Arc<XType>) -> Arc<XType> {
        Arc::new(XType::XNative(Box::new(Self {}), vec![k.clone(), v.clone()]))
    }
}

impl NativeType for XMappingType {
    fn generic_names(&self) -> Vec<String> {
        vec!["K".to_string(), "V".to_string()]
    }
    fn name(&self) -> &str { "Mapping" }
}

#[derive(Debug)]
pub struct XMapping {
    pub inner: HashMap<u64, Vec<(Rc<ManagedXValue>, Rc<ManagedXValue>)>>,
    pub hash_func: Rc<ManagedXValue>,
    pub eq_func: Rc<ManagedXValue>,
}

impl XMapping {
    fn new(hash_func: Rc<ManagedXValue>, eq_func: Rc<ManagedXValue>, dict: HashMap<u64, Vec<(Rc<ManagedXValue>, Rc<ManagedXValue>)>>) -> Self {
        Self {
            inner: dict,
            hash_func,
            eq_func,
        }
    }

    fn with_update(&self, items: impl Iterator<Item=(Rc<ManagedXValue>, Rc<ManagedXValue>)>, ns: &XEvaluationScope, rt: RTCell) -> Result<TailedEvalResult, String> {
        let hash_func = to_primitive!(self.hash_func, Function);
        let mut eq_func = None;
        let mut new_dict = self.inner.clone();
        for (k, v) in items {
            let hash_key = to_primitive!(hash_func.eval_values(vec![k.clone()], &ns, rt.clone())?, Int).to_u64().ok_or("hash is out of bounds")?;

            let spot = new_dict.entry(hash_key);
            match spot {
                Entry::Vacant(spot) => {
                    spot.insert(vec![(k, v)]);
                }
                Entry::Occupied(mut spot) => {
                    if eq_func.is_none() {
                        eq_func = Some(to_primitive!(self.eq_func, Function));
                    }
                    let mut found = false;
                    for (i, (existing_k, existing_v)) in spot.get().iter().enumerate() {
                        if *to_primitive!(eq_func.unwrap().eval_values(vec![existing_k.clone(), k.clone()], &ns, rt.clone())?, Bool) {
                            spot.get_mut()[i].1 = v.clone();
                            found = true;
                            break;
                        }
                    }
                    if !found {
                        spot.get_mut().push((k, v));
                    }
                }
            }
        }

        Ok(manage_native!(XMapping::new(self.hash_func.clone(), self.eq_func.clone(), new_dict), rt))
    }
}

impl XNativeValue for XMapping {
    fn size(&self) -> usize {
        (self.inner.len() * 2 + 2) * size_of::<usize>()
    }
}

pub fn add_mapping_type(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    scope.add_native_type_intern("Mapping", XMappingType::xtype(XType::generic_from_name("K", interner), XType::generic_from_name("V", interner)), interner)
}

pub fn add_mapping_new(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let k = XType::generic_from_name("K", interner);

    scope.add_func_intern(
        "mapping", XStaticFunction::from_native(XFuncSpec {
            generic_params: Some(intern!(interner, "K")),
            params: vec![
                XFuncParamSpec {
                    type_: Arc::new(XCallable(XCallableSpec {
                        param_types: vec![k.clone()],
                        return_type: X_INT.clone(),
                    })),
                    required: true,
                },
                XFuncParamSpec {
                    type_: Arc::new(XCallable(XCallableSpec {
                        param_types: vec![k.clone(), k.clone()],
                        return_type: X_BOOL.clone(),
                    })),
                    required: true,
                },
            ],
            ret: XMappingType::xtype(k.clone(), X_UNKNOWN.clone()),
        }, |args, ns, _tca, rt| {
            let (hash_func, eq_func) = eval!(args, ns, rt, 0, 1);
            Ok(manage_native!(XMapping::new(hash_func, eq_func, Default::default()), rt))
        }), interner)?;
    Ok(())
}

pub fn add_mapping_set(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let k = XType::generic_from_name("K", interner);
    let v = XType::generic_from_name("V", interner);
    let mp = XMappingType::xtype(k.clone(), v.clone());

    scope.add_func_intern(
        "set", XStaticFunction::from_native(XFuncSpec {
            generic_params: Some(intern!(interner, "K", "V")),
            params: vec![
                XFuncParamSpec {
                    type_: mp.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: k.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: v.clone(),
                    required: true,
                },
            ],
            ret: mp.clone(),
        }, |args, ns, _tca, rt| {
            let (a0, a1, a2) = eval!(args, ns, rt, 0, 1, 2);
            let mapping = to_native!(a0, XMapping);
            mapping.with_update(once((a1, a2)), ns, rt)
        }), interner)?;
    Ok(())
}

pub fn add_mapping_update(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let k = XType::generic_from_name("K", interner);
    let v = XType::generic_from_name("V", interner);
    let mp = XMappingType::xtype(k.clone(), v.clone());

    scope.add_func_intern(
        "update", XStaticFunction::from_native(XFuncSpec {
            generic_params: Some(intern!(interner, "K", "V")),
            params: vec![
                XFuncParamSpec {
                    type_: mp.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: XSequenceType::xtype(Arc::new(XType::Tuple(vec![k.clone(), v.clone()]))),
                    required: true,
                },
            ],
            ret: mp.clone(),
        }, |args, ns, _tca, rt| {
            let (a0, a1) = eval!(args, ns, rt, 0, 1);
            let mapping = to_native!(a0, XMapping);
            let seq = to_native!(a1, XSequence);
            let arr = seq.slice(0, seq.len(), ns, rt.clone())?;
            let items = arr.iter().map(|t| {
                let tup = to_primitive!(t, StructInstance);
                (tup[0].clone(), tup[1].clone())
            });
            mapping.with_update(items, ns, rt)
        }), interner)?;
    Ok(())
}

pub fn add_mapping_get(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let k = XType::generic_from_name("K", interner);
    let v = XType::generic_from_name("V", interner);
    let mp = XMappingType::xtype(k.clone(), v.clone());

    scope.add_func_intern(
        "get", XStaticFunction::from_native(XFuncSpec {
            generic_params: Some(intern!(interner, "K", "V")),
            params: vec![
                XFuncParamSpec {
                    type_: mp.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: k.clone(),
                    required: true,
                },
            ],
            ret: XOptionalType::xtype(v.clone()),
        }, |args, ns, _tca, rt| {
            let (a0, a1) = eval!(args, ns, rt, 0, 1);
            let mapping = to_native!(a0, XMapping);
            let hash_func = to_primitive!(mapping.hash_func, Function);
            let hash_key = to_primitive!(hash_func.eval_values(vec![a1.clone()], &ns, rt.clone())?, Int).to_u64().ok_or("hash is out of bounds")?;
            let spot = mapping.inner.get(&hash_key);
            match spot {
                None => {
                    Ok(manage_native!(XOptional { value: None }, rt.clone()))
                }
                Some(candidates) => {
                    let eq_func = to_primitive!(mapping.eq_func, Function);
                    for (k, v) in candidates.iter() {
                        if *to_primitive!(eq_func.eval_values(vec![a1.clone(), k.clone()], &ns, rt.clone())?, Bool) {
                            return Ok(manage_native!(XOptional { value: Some(v.clone()) }, rt.clone()));
                        }
                    }
                    Ok(manage_native!(XOptional { value: None }, rt.clone()))
                }
            }
        }), interner)?;
    Ok(())
}

pub fn add_mapping_len(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let k = XType::generic_from_name("K", interner);
    let v = XType::generic_from_name("V", interner);
    let mp = XMappingType::xtype(k.clone(), v.clone());

    scope.add_func_intern(
        "len", XStaticFunction::from_native(XFuncSpec {
            generic_params: Some(intern!(interner, "K", "V")),
            params: vec![
                XFuncParamSpec {
                    type_: mp.clone(),
                    required: true,
                },
            ],
            ret: X_INT.clone(),
        }, |args, ns, _tca, rt| {
            let (a0, ) = eval!(args, ns, rt, 0);
            let mapping = to_native!(a0, XMapping);
            let len: usize = mapping.inner.values().map(|v| v.len()).sum();
            Ok(ManagedXValue::new(XValue::Int(len.into()), rt)?.into())
        }), interner)?;
    Ok(())
}

pub fn add_mapping_entries(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let k = XType::generic_from_name("K", interner);
    let v = XType::generic_from_name("V", interner);
    let mp = XMappingType::xtype(k.clone(), v.clone());

    scope.add_func_intern(
        "entries", XStaticFunction::from_native(XFuncSpec {
            generic_params: Some(intern!(interner, "K", "V")),
            params: vec![
                XFuncParamSpec {
                    type_: mp.clone(),
                    required: true,
                },
            ],
            ret: XSequenceType::xtype(Arc::new(XType::Tuple(vec![k.clone(), v.clone()]))),
        }, |args, ns, _tca, rt| {
            let (a0, ) = eval!(args, ns, rt, 0);
            let mapping = to_native!(a0, XMapping);
            let entries = mapping.inner.values()
                .flat_map(|lst|
                    lst.iter().map(|(k, v)|
                        ManagedXValue::new(XValue::StructInstance(vec![k.clone(), v.clone()]), rt.clone())))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(manage_native!(XSequence::array(entries), rt))
        }), interner)?;
    Ok(())
}

pub fn add_mapping_contains(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let k = XType::generic_from_name("K", interner);
    let v = XType::generic_from_name("V", interner);
    let mp = XMappingType::xtype(k.clone(), v.clone());

    scope.add_func_intern(
        "contains", XStaticFunction::from_native(XFuncSpec {
            generic_params: Some(intern!(interner, "K", "V")),
            params: vec![
                XFuncParamSpec {
                    type_: mp.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: k.clone(),
                    required: true,
                },
            ],
            ret: X_BOOL.clone(),
        }, |args, ns, _tca, rt| {
            let (a0, a1) = eval!(args, ns, rt, 0, 1);
            let mapping = to_native!(a0, XMapping);
            let hash_func = to_primitive!(mapping.hash_func, Function);
            let hash_key = to_primitive!(hash_func.eval_values(vec![a1.clone()], &ns, rt.clone())?, Int).to_u64().ok_or("hash is out of bounds")?;
            let spot = mapping.inner.get(&hash_key);
            let mut ret = false;
            if let Some(candidates) = spot{
                    let eq_func = to_primitive!(mapping.eq_func, Function);
                    for (k, _) in candidates.iter() {
                        if *to_primitive!(eq_func.eval_values(vec![a1.clone(), k.clone()], &ns, rt.clone())?, Bool) {
                            ret = true;
                            break
                        }
                    }

            }
            return Ok(ManagedXValue::new(XValue::Bool(ret), rt)?.into());
        }), interner)?;
    Ok(())
}