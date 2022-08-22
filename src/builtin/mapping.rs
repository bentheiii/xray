use crate::builtin::optional::{XOptional, XOptionalType};
use crate::builtin::sequence::{XSequence, XSequenceType};
use crate::native_types::{NativeType, XNativeValue};
use crate::xtype::{XFuncParamSpec, XFuncSpec, X_BOOL, X_INT, X_UNKNOWN};
use crate::xvalue::{ManagedXValue, XValue};
use crate::XType::XCallable;
use crate::{
    eval, manage_native, to_native, to_primitive, CompilationError, RTCell, RootCompilationScope,
    XCallableSpec, XEvaluationScope, XStaticFunction, XType,
};
use num_traits::ToPrimitive;
use rc::Rc;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::iter::once;
use std::mem::size_of;
use std::rc;
use std::sync::Arc;

use crate::xexpr::TailedEvalResult;

#[derive(Debug, Clone)]
pub struct XMappingType;

impl XMappingType {
    pub fn xtype(k: Arc<XType>, v: Arc<XType>) -> Arc<XType> {
        Arc::new(XType::XNative(Box::new(Self {}), vec![k, v]))
    }
}

impl NativeType for XMappingType {
    fn generic_names(&self) -> Vec<String> {
        vec!["K".to_string(), "V".to_string()]
    }
    fn name(&self) -> &str {
        "Mapping"
    }
}

type MappingBucket = Vec<(Rc<ManagedXValue>, Rc<ManagedXValue>)>;

#[derive(Debug)]
pub struct XMapping {
    pub inner: HashMap<u64, MappingBucket>,
    pub hash_func: Rc<ManagedXValue>,
    pub eq_func: Rc<ManagedXValue>,
}

impl XMapping {
    fn new(
        hash_func: Rc<ManagedXValue>,
        eq_func: Rc<ManagedXValue>,
        dict: HashMap<u64, MappingBucket>,
    ) -> Self {
        Self {
            inner: dict,
            hash_func,
            eq_func,
        }
    }

    fn with_update(
        &self,
        items: impl Iterator<Item = (Rc<ManagedXValue>, Rc<ManagedXValue>)>,
        ns: &XEvaluationScope,
        rt: RTCell,
    ) -> Result<TailedEvalResult, String> {
        let hash_func = to_primitive!(self.hash_func, Function);
        let mut eq_func = None;
        let mut new_dict = self.inner.clone();
        for (k, v) in items {
            let hash_key = to_primitive!(hash_func.eval_values(&[k.clone()], ns, rt.clone())?, Int)
                .to_u64()
                .ok_or("hash is out of bounds")?;

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
                    for (i, (existing_k, _existing_v)) in spot.get().iter().enumerate() {
                        if *to_primitive!(
                            eq_func.unwrap().eval_values(
                                &[existing_k.clone(), k.clone()],
                                ns,
                                rt.clone()
                            )?,
                            Bool
                        ) {
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

        Ok(manage_native!(
            Self::new(self.hash_func.clone(), self.eq_func.clone(), new_dict),
            rt
        ))
    }
}

impl XNativeValue for XMapping {
    fn size(&self) -> usize {
        (self.inner.len() * 2 + 2) * size_of::<usize>()
    }
}

pub(crate) fn add_mapping_type(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([k, v], _) = scope.generics_from_names(["K", "V"]);
    scope.add_native_type("Mapping", XMappingType::xtype(k, v))
}

pub(crate) fn add_mapping_new(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([k], params) = scope.generics_from_names(["K"]);

    scope.add_func(
        "mapping",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
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
                ret: XMappingType::xtype(k, X_UNKNOWN.clone()),
            },
            |args, ns, _tca, rt| {
                let (hash_func, eq_func) = eval!(args, ns, rt, 0, 1);
                Ok(manage_native!(
                    XMapping::new(hash_func, eq_func, Default::default()),
                    rt
                ))
            },
        ),
    )
}

pub(crate) fn add_mapping_set(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v.clone());

    scope.add_func(
        "set",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: mp.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: k,
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: v,
                        required: true,
                    },
                ],
                ret: mp,
            },
            |args, ns, _tca, rt| {
                let (a0, a1, a2) = eval!(args, ns, rt, 0, 1, 2);
                let mapping = to_native!(a0, XMapping);
                mapping.with_update(once((a1, a2)), ns, rt)
            },
        ),
    )
}

pub(crate) fn add_mapping_update(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v.clone());

    scope.add_func(
        "update",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: mp.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: XSequenceType::xtype(Arc::new(XType::Tuple(vec![k, v]))),
                        required: true,
                    },
                ],
                ret: mp,
            },
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let mapping = to_native!(a0, XMapping);
                let seq = to_native!(a1, XSequence);
                let arr = seq.slice(0, seq.len(), ns, rt.clone())?;
                let items = arr.iter().map(|t| {
                    let tup = to_primitive!(t, StructInstance);
                    (tup[0].clone(), tup[1].clone())
                });
                mapping.with_update(items, ns, rt)
            },
        ),
    )
}

pub(crate) fn add_mapping_get(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v.clone());

    scope.add_func(
        "get",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: mp,
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: k,
                        required: true,
                    },
                ],
                ret: XOptionalType::xtype(v),
            },
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let mapping = to_native!(a0, XMapping);
                let hash_func = to_primitive!(mapping.hash_func, Function);
                let hash_key =
                    to_primitive!(hash_func.eval_values(&[a1.clone()], ns, rt.clone())?, Int)
                        .to_u64()
                        .ok_or("hash is out of bounds")?;
                let spot = mapping.inner.get(&hash_key);
                match spot {
                    None => Ok(manage_native!(XOptional { value: None }, rt)),
                    Some(candidates) => {
                        let eq_func = to_primitive!(mapping.eq_func, Function);
                        for (k, v) in candidates.iter() {
                            if *to_primitive!(
                                eq_func.eval_values(&[a1.clone(), k.clone()], ns, rt.clone())?,
                                Bool
                            ) {
                                return Ok(manage_native!(
                                    XOptional {
                                        value: Some(v.clone())
                                    },
                                    rt
                                ));
                            }
                        }
                        Ok(manage_native!(XOptional { value: None }, rt))
                    }
                }
            },
        ),
    )
}

pub(crate) fn add_mapping_len(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k, v);

    scope.add_func(
        "len",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![XFuncParamSpec {
                    type_: mp,
                    required: true,
                }],
                ret: X_INT.clone(),
            },
            |args, ns, _tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                let mapping = to_native!(a0, XMapping);
                let len: usize = mapping.inner.values().map(|v| v.len()).sum();
                Ok(ManagedXValue::new(XValue::Int(len.into()), rt)?.into())
            },
        ),
    )
}

pub(crate) fn add_mapping_entries(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v.clone());

    scope.add_func(
        "entries",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![XFuncParamSpec {
                    type_: mp,
                    required: true,
                }],
                ret: XSequenceType::xtype(Arc::new(XType::Tuple(vec![k, v]))),
            },
            |args, ns, _tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                let mapping = to_native!(a0, XMapping);
                let entries = mapping
                    .inner
                    .values()
                    .flat_map(|lst| {
                        lst.iter().map(|(k, v)| {
                            ManagedXValue::new(
                                XValue::StructInstance(vec![k.clone(), v.clone()]),
                                rt.clone(),
                            )
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(manage_native!(XSequence::array(entries), rt))
            },
        ),
    )
}

pub(crate) fn add_mapping_contains(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v);

    scope.add_func(
        "contains",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: mp,
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: k,
                        required: true,
                    },
                ],
                ret: X_BOOL.clone(),
            },
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let mapping = to_native!(a0, XMapping);
                let hash_func = to_primitive!(mapping.hash_func, Function);
                let hash_key =
                    to_primitive!(hash_func.eval_values(&[a1.clone()], ns, rt.clone())?, Int)
                        .to_u64()
                        .ok_or("hash is out of bounds")?;
                let spot = mapping.inner.get(&hash_key);
                let mut ret = false;
                if let Some(candidates) = spot {
                    let eq_func = to_primitive!(mapping.eq_func, Function);
                    for (k, _) in candidates.iter() {
                        if *to_primitive!(
                            eq_func.eval_values(&[a1.clone(), k.clone()], ns, rt.clone())?,
                            Bool
                        ) {
                            ret = true;
                            break;
                        }
                    }
                }
                Ok(ManagedXValue::new(XValue::Bool(ret), rt)?.into())
            },
        ),
    )
}

pub(crate) fn add_mapping_pop(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v);

    scope.add_func(
        "pop",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: mp.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: k,
                        required: true,
                    },
                ],
                ret: mp,
            },
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let mapping = to_native!(a0, XMapping);
                let hash_func = to_primitive!(mapping.hash_func, Function);
                let hash_key =
                    to_primitive!(hash_func.eval_values(&[a1.clone()], ns, rt.clone())?, Int)
                        .to_u64()
                        .ok_or("hash is out of bounds")?;
                let spot = mapping.inner.get(&hash_key);
                let mut new_spot = None;
                if let Some(candidates) = spot {
                    let eq_func = to_primitive!(mapping.eq_func, Function);
                    for (i, (k, _)) in candidates.iter().enumerate() {
                        if *to_primitive!(
                            eq_func.eval_values(&[a1.clone(), k.clone()], ns, rt.clone())?,
                            Bool
                        ) {
                            new_spot = Some(
                                candidates[..i]
                                    .iter()
                                    .cloned()
                                    .chain(candidates[i + 1..].iter().cloned())
                                    .collect(),
                            );
                            break;
                        }
                    }
                }
                match new_spot {
                    None => Err("Key not found in mapping".to_string()),
                    Some(new_spot) => {
                        let mut new_dict = HashMap::from([(hash_key, new_spot)]);
                        for (k, v) in &mapping.inner {
                            if *k != hash_key {
                                new_dict.insert(*k, v.clone());
                            }
                        }
                        Ok(manage_native!(
                            XMapping::new(
                                mapping.hash_func.clone(),
                                mapping.eq_func.clone(),
                                new_dict
                            ),
                            rt
                        ))
                    }
                }
            },
        ),
    )
}

pub(crate) fn add_mapping_discard(scope: &mut RootCompilationScope) -> Result<(), CompilationError> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v);

    scope.add_func(
        "discard",
        XStaticFunction::from_native(
            XFuncSpec {
                generic_params: Some(params),
                params: vec![
                    XFuncParamSpec {
                        type_: mp.clone(),
                        required: true,
                    },
                    XFuncParamSpec {
                        type_: k,
                        required: true,
                    },
                ],
                ret: mp,
            },
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let mapping = to_native!(a0, XMapping);
                let hash_func = to_primitive!(mapping.hash_func, Function);
                let hash_key =
                    to_primitive!(hash_func.eval_values(&[a1.clone()], ns, rt.clone())?, Int)
                        .to_u64()
                        .ok_or("hash is out of bounds")?;
                let spot = mapping.inner.get(&hash_key);
                let mut new_spot = None;
                if let Some(candidates) = spot {
                    let eq_func = to_primitive!(mapping.eq_func, Function);
                    for (i, (k, _)) in candidates.iter().enumerate() {
                        if *to_primitive!(
                            eq_func.eval_values(&[a1.clone(), k.clone()], ns, rt.clone())?,
                            Bool
                        ) {
                            new_spot = Some(
                                candidates[..i]
                                    .iter()
                                    .cloned()
                                    .chain(candidates[i + 1..].iter().cloned())
                                    .collect(),
                            );
                            break;
                        }
                    }
                }
                match new_spot {
                    None => Ok(a0.clone().into()),
                    Some(new_spot) => {
                        let mut new_dict = HashMap::from([(hash_key, new_spot)]);
                        for (k, v) in &mapping.inner {
                            if *k != hash_key {
                                new_dict.insert(*k, v.clone());
                            }
                        }
                        Ok(manage_native!(
                            XMapping::new(
                                mapping.hash_func.clone(),
                                mapping.eq_func.clone(),
                                new_dict
                            ),
                            rt
                        ))
                    }
                }
            },
        ),
    )
}
