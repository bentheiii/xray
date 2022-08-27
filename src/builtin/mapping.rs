use crate::builtin::optional::{XOptional, XOptionalType};
use crate::builtin::sequence::{XSequence, XSequenceType};
use crate::native_types::{NativeType, XNativeValue};
use crate::xtype::{XFuncSpec, X_BOOL, X_INT, X_UNKNOWN};
use crate::xvalue::{ManagedXValue, XValue};
use crate::XType::XCallable;
use crate::{eval, manage_native, to_native, to_primitive, CompilationError, RTCell, RootCompilationScope, XCallableSpec, XEvaluationScope, XStaticFunction, XType, unpack_types};
use derivative::Derivative;
use num_traits::ToPrimitive;
use rc::Rc;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Debug;
use std::io::Write;
use std::iter::once;
use std::mem::size_of;
use std::rc;
use std::sync::Arc;
use crate::builtin::core::get_func;

use crate::xexpr::TailedEvalResult;

#[derive(Debug, Clone)]
struct XMappingType;

impl XMappingType {
    fn xtype(k: Arc<XType>, v: Arc<XType>) -> Arc<XType> {
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

type MappingBucket<W> = Vec<(Rc<ManagedXValue<W>>, Rc<ManagedXValue<W>>)>;

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
struct XMapping<W: Write + 'static> {
    inner: HashMap<u64, MappingBucket<W>>,
    hash_func: Rc<ManagedXValue<W>>,
    eq_func: Rc<ManagedXValue<W>>,
}

impl<W: Write + 'static> XMapping<W> {
    fn new(
        hash_func: Rc<ManagedXValue<W>>,
        eq_func: Rc<ManagedXValue<W>>,
        dict: HashMap<u64, MappingBucket<W>>,
    ) -> Self {
        Self {
            inner: dict,
            hash_func,
            eq_func,
        }
    }

    fn with_update(
        &self,
        items: impl Iterator<Item = (Rc<ManagedXValue<W>>, Rc<ManagedXValue<W>>)>,
        ns: &XEvaluationScope<W>,
        rt: RTCell<W>,
    ) -> Result<TailedEvalResult<W>, String> {
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

impl<W: Write + 'static> XNativeValue for XMapping<W> {
    fn size(&self) -> usize {
        (self.inner.len() * 2 + 2) * size_of::<usize>()
    }
}

pub(crate) fn add_mapping_type<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    let ([k, v], _) = scope.generics_from_names(["K", "V"]);
    scope.add_native_type("Mapping", XMappingType::xtype(k, v))
}

pub(crate) fn add_mapping_new<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    let ([k], params) = scope.generics_from_names(["K"]);

    scope.add_func(
        "mapping",
        XStaticFunction::from_native(
            XFuncSpec::new(
                &[
                    &Arc::new(XCallable(XCallableSpec {
                        param_types: vec![k.clone()],
                        return_type: X_INT.clone(),
                    })),
                    &Arc::new(XCallable(XCallableSpec {
                        param_types: vec![k.clone(), k.clone()],
                        return_type: X_BOOL.clone(),
                    })),
                ],
                XMappingType::xtype(k, X_UNKNOWN.clone()),
            )
            .generic(params),
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

pub(crate) fn add_mapping_set<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v.clone());

    scope.add_func(
        "set",
        XStaticFunction::from_native(
            XFuncSpec::new(&[&mp, &k, &v], mp.clone()).generic(params),
            |args, ns, _tca, rt| {
                let (a0, a1, a2) = eval!(args, ns, rt, 0, 1, 2);
                let mapping = to_native!(a0, XMapping<W>);
                mapping.with_update(once((a1, a2)), ns, rt)
            },
        ),
    )
}

pub(crate) fn add_mapping_update<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v.clone());

    scope.add_func(
        "update",
        XStaticFunction::from_native(
            XFuncSpec::new(
                &[
                    &mp,
                    &XSequenceType::xtype(Arc::new(XType::Tuple(vec![k, v]))),
                ],
                mp.clone(),
            )
            .generic(params),
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let mapping = to_native!(a0, XMapping<W>);
                let seq = to_native!(a1, XSequence<W>);
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

pub(crate) fn add_mapping_lookup<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v.clone());

    scope.add_func(
        "lookup",
        XStaticFunction::from_native(
            XFuncSpec::new(&[&mp, &k], XOptionalType::xtype(v)).generic(params),
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let mapping = to_native!(a0, XMapping<W>);
                let hash_func = to_primitive!(mapping.hash_func, Function);
                let hash_key =
                    to_primitive!(hash_func.eval_values(&[a1.clone()], ns, rt.clone())?, Int)
                        .to_u64()
                        .ok_or("hash is out of bounds")?;
                let spot = mapping.inner.get(&hash_key);
                match spot {
                    None => Ok(manage_native!(XOptional::<W> { value: None }, rt)),
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
                        Ok(manage_native!(XOptional::<W> { value: None }, rt))
                    }
                }
            },
        ),
    )
}

pub(crate) fn add_mapping_get<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v.clone());

    scope.add_func(
        "get",
        XStaticFunction::from_native(
            XFuncSpec::new(&[&mp, &k], v).generic(params),
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let mapping = to_native!(a0, XMapping<W>);
                let hash_func = to_primitive!(mapping.hash_func, Function);
                let hash_key =
                    to_primitive!(hash_func.eval_values(&[a1.clone()], ns, rt.clone())?, Int)
                        .to_u64()
                        .ok_or("hash is out of bounds")?;
                let spot = mapping.inner.get(&hash_key);
                match spot {
                    None => Err("key not found".to_string()),
                    Some(candidates) => {
                        let eq_func = to_primitive!(mapping.eq_func, Function);
                        for (k, v) in candidates.iter() {
                            if *to_primitive!(
                                eq_func.eval_values(&[a1.clone(), k.clone()], ns, rt.clone())?,
                                Bool
                            ) {
                                return Ok(v.clone().into());
                            }
                        }
                        Err("key not found".to_string())
                    }
                }
            },
        ),
    )
}

pub(crate) fn add_mapping_len<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k, v);

    scope.add_func(
        "len",
        XStaticFunction::from_native(
            XFuncSpec::new(&[&mp], X_INT.clone()).generic(params),
            |args, ns, _tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                let mapping = to_native!(a0, XMapping<W>);
                let len: usize = mapping.inner.values().map(|v| v.len()).sum();
                Ok(ManagedXValue::new(XValue::Int(len.into()), rt)?.into())
            },
        ),
    )
}

pub(crate) fn add_mapping_entries<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v.clone());

    scope.add_func(
        "entries",
        XStaticFunction::from_native(
            XFuncSpec::new(
                &[&mp],
                XSequenceType::xtype(Arc::new(XType::Tuple(vec![k, v]))),
            )
            .generic(params),
            |args, ns, _tca, rt| {
                let (a0,) = eval!(args, ns, rt, 0);
                let mapping = to_native!(a0, XMapping<W>);
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

pub(crate) fn add_mapping_contains<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v);

    scope.add_func(
        "contains",
        XStaticFunction::from_native(
            XFuncSpec::new(&[&mp, &k], X_BOOL.clone()).generic(params),
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let mapping = to_native!(a0, XMapping<W>);
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

pub(crate) fn add_mapping_pop<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v);

    scope.add_func(
        "pop",
        XStaticFunction::from_native(
            XFuncSpec::new(&[&mp, &k], mp.clone()).generic(params),
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let mapping = to_native!(a0, XMapping<W>);
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

pub(crate) fn add_mapping_discard<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v);

    scope.add_func(
        "discard",
        XStaticFunction::from_native(
            XFuncSpec::new(&[&mp, &k], mp.clone()).generic(params),
            |args, ns, _tca, rt| {
                let (a0, a1) = eval!(args, ns, rt, 0, 1);
                let mapping = to_native!(a0, XMapping<W>);
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

pub(crate) fn add_mapping_new_dyn<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError<W>> {
    let eq_symbol = scope.identifier("eq");
    let hash_symbol =  scope.identifier("hash");

    scope.add_dyn_func("mapping", move |_params, _types, ns, bind| {
        let a0 = unpack_types!(bind, 0);

        let inner_eq = get_func(ns, eq_symbol, &[a0.clone(), a0.clone()], &X_BOOL)?;
        let inner_hash = get_func(ns, hash_symbol, &[a0.clone()], &X_INT)?;

        Ok(Rc::new(XStaticFunction::from_native(
            XFuncSpec::new(
                &[],
                XMappingType::xtype(a0.clone(), X_UNKNOWN.clone()),
            ),
            move |_args, ns, _tca, rt| {
                let inner_equal_value = inner_eq.eval(ns, false, rt.clone())?.unwrap_value();
                let inner_hash_value = inner_hash.eval(ns, false, rt.clone())?.unwrap_value();
                Ok(manage_native!(
                    XMapping::new(inner_hash_value, inner_equal_value, Default::default()),
                    rt
                ))
            },
        )))
    })
}