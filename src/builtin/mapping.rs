use crate::builtin::core::{eval, get_func};
use crate::builtin::optional::{XOptional, XOptionalType};
use crate::builtin::sequence::{XSequence, XSequenceType};
use crate::native_types::{NativeType, XNativeValue};
use crate::xtype::{XFuncSpec, X_BOOL, X_INT, X_UNKNOWN};
use crate::xvalue::{ManagedXValue, XFunctionFactoryOutput, XValue};
use crate::XType::XCallable;
use crate::{manage_native, to_native, to_primitive, unpack_types, CompilationError, RTCell, RootCompilationScope, XCallableSpec, XStaticFunction, XType, xraise};
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
use crate::evaluation_scope::EvaluatedVariable;
use crate::runtime_err::RuntimeError;
use crate::runtime_scope::RuntimeScope;

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

type MappingBucket<W> = Vec<(EvaluatedVariable<W>, EvaluatedVariable<W>)>;

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
struct XMapping<W: Write + 'static> {
    inner: HashMap<u64, MappingBucket<W>>,
    hash_func: Rc<ManagedXValue<W>>,
    eq_func: Rc<ManagedXValue<W>>,
}

macro_rules! parse_hash {
    ($v: expr) => {{
        xraise!(to_primitive!(xraise!($v.unwrap_value()), Int)
                .to_u64()
                .ok_or("hash is out of bounds"))
    }};
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
        items: impl Iterator<Item = (EvaluatedVariable<W>, EvaluatedVariable<W>)>,
        ns: &RuntimeScope<W>,
        rt: RTCell<W>,
    ) -> Result<TailedEvalResult<W>, RuntimeError> {
        let hash_func = to_primitive!(self.hash_func, Function);
        let mut eq_func = None;
        let mut new_dict = self.inner.clone();
        for (k, v) in items {
            let hash_key = parse_hash!(ns.eval_func_with_values(hash_func, vec![k.clone()], rt.clone(), false)?);

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
                            xraise!(ns.eval_func_with_values(eq_func.unwrap(), vec![existing_k.clone(), k.clone()], rt.clone(), false)?.unwrap_value()),
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
        XStaticFunction::from_native(
            |args, ns, _tca, rt| {
                let hash_func = xraise!(eval(&args[0], ns, &rt)?);
                let eq_func = xraise!(eval(&args[1], ns, &rt)?);
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
            XFuncSpec::new(&[&mp, &k, &v], mp.clone()).generic(params),
        XStaticFunction::from_native(
            |args, ns, _tca, rt| {
                let a0 = xraise!(eval(&args[0], ns, &rt)?);
                let a1 = eval(&args[1], ns, &rt)?;
                let a2 = eval(&args[2], ns, &rt)?;
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
            XFuncSpec::new(
                &[
                    &mp,
                    &XSequenceType::xtype(Arc::new(XType::Tuple(vec![k, v]))),
                ],
                mp.clone(),
            )
            .generic(params),
        XStaticFunction::from_native(
            |args, ns, _tca, rt| {
                let a0 = xraise!(eval(&args[0], ns, &rt)?);
                let a1 = xraise!(eval(&args[1], ns, &rt)?);
                let mapping = to_native!(a0, XMapping<W>);
                let seq = to_native!(a1, XSequence<W>);
                let arr = xraise!(seq.slice(ns, rt.clone()).collect::<Result<Result<Vec<_>,_>, _>>()?);
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
            XFuncSpec::new(&[&mp, &k], XOptionalType::xtype(v)).generic(params),
        XStaticFunction::from_native(
            |args, ns, _tca, rt| {
                let a0 = xraise!(eval(&args[0], ns, &rt)?);
                let a1 = eval(&args[1], ns, &rt)?;
                let mapping = to_native!(a0, XMapping<W>);
                let hash_func = to_primitive!(mapping.hash_func, Function);
                let hash_key = parse_hash!(ns.eval_func_with_values(hash_func, vec![a1.clone()], rt.clone(), false)?);
                let spot = mapping.inner.get(&hash_key);
                match spot {
                    None => Ok(manage_native!(XOptional::<W> { value: None }, rt)),
                    Some(candidates) => {
                        let eq_func = to_primitive!(mapping.eq_func, Function);
                        for (k, v) in candidates.iter() {
                            if *to_primitive!(
                                xraise!(ns.eval_func_with_values(eq_func, vec![a1.clone(), k.clone()], rt.clone(), false)?.unwrap_value()),
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
            XFuncSpec::new(&[&mp, &k], v).generic(params),
        XStaticFunction::from_native(
            |args, ns, _tca, rt| {
                let a0 = xraise!(eval(&args[0], ns, &rt)?);
                let a1 = eval(&args[1], ns, &rt)?;
                let mapping = to_native!(a0, XMapping<W>);
                let hash_func = to_primitive!(mapping.hash_func, Function);
                let hash_key = parse_hash!(ns.eval_func_with_values(hash_func, vec![a1.clone()], rt.clone(), false)?);
                let spot = mapping.inner.get(&hash_key);
                match spot {
                    None => xraise!(Err("key not found".to_string())),
                    Some(candidates) => {
                        let eq_func = to_primitive!(mapping.eq_func, Function);
                        for (k, v) in candidates.iter() {
                            if *to_primitive!(
                                xraise!(ns.eval_func_with_values(eq_func, vec![a1.clone(), k.clone()], rt.clone(), false)?.unwrap_value()),
                                Bool
                            ) {
                                return Ok(v.clone().into());
                            }
                        }
                        xraise!(Err("key not found".to_string()))
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
            XFuncSpec::new(&[&mp], X_INT.clone()).generic(params),
        XStaticFunction::from_native(
            |args, ns, _tca, rt| {
                let a0 = xraise!(eval(&args[0], ns, &rt)?);
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
            XFuncSpec::new(
                &[&mp],
                XSequenceType::xtype(Arc::new(XType::Tuple(vec![k, v]))),
            )
            .generic(params),
        XStaticFunction::from_native(
            |args, ns, _tca, rt| {
                let a0 = xraise!(eval(&args[0], ns, &rt)?);
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
                Ok(manage_native!(XSequence::array(entries.into_iter().map(Ok).collect()), rt))
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
            XFuncSpec::new(&[&mp, &k], X_BOOL.clone()).generic(params),
        XStaticFunction::from_native(
            |args, ns, _tca, rt| {
                let a0 = xraise!(eval(&args[0], ns, &rt)?);
                let a1 = eval(&args[1], ns, &rt)?;
                let mapping = to_native!(a0, XMapping<W>);
                let hash_func = to_primitive!(mapping.hash_func, Function);
                let hash_key = parse_hash!(ns.eval_func_with_values(hash_func, vec![a1.clone()], rt.clone(), false)?);
                let spot = mapping.inner.get(&hash_key);
                let mut ret = false;
                if let Some(candidates) = spot {
                    let eq_func = to_primitive!(mapping.eq_func, Function);
                    for (k, _) in candidates.iter() {
                        if *to_primitive!(
                            xraise!(ns.eval_func_with_values(eq_func, vec![a1.clone(), k.clone()], rt.clone(), false)?.unwrap_value()),
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
            XFuncSpec::new(&[&mp, &k], mp.clone()).generic(params),
        XStaticFunction::from_native(
            |args, ns, _tca, rt| {
                let a0 = xraise!(eval(&args[0], ns, &rt)?);
                let a1 = eval(&args[1], ns, &rt)?;
                let mapping = to_native!(a0, XMapping<W>);
                let hash_func = to_primitive!(mapping.hash_func, Function);
                let hash_key = parse_hash!(ns.eval_func_with_values(hash_func, vec![a1.clone()], rt.clone(), false)?);
                let spot = mapping.inner.get(&hash_key);
                let mut new_spot = None;
                if let Some(candidates) = spot {
                    let eq_func = to_primitive!(mapping.eq_func, Function);
                    for (i, (k, _)) in candidates.iter().enumerate() {
                        if *to_primitive!(
                                xraise!(ns.eval_func_with_values(eq_func, vec![a1.clone(), k.clone()], rt.clone(), false)?.unwrap_value()),
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
                    None => xraise!(Err("Key not found in mapping".to_string())),
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
            XFuncSpec::new(&[&mp, &k], mp.clone()).generic(params),
        XStaticFunction::from_native(
            |args, ns, _tca, rt| {
                let a0 = xraise!(eval(&args[0], ns, &rt)?);
                let a1 = eval(&args[1], ns, &rt)?;
                let mapping = to_native!(a0, XMapping<W>);
                let hash_func = to_primitive!(mapping.hash_func, Function);
                let hash_key = parse_hash!(ns.eval_func_with_values(hash_func, vec![a1.clone()], rt.clone(), false)?);
                let spot = mapping.inner.get(&hash_key);
                let mut new_spot = None;
                if let Some(candidates) = spot {
                    let eq_func = to_primitive!(mapping.eq_func, Function);
                    for (i, (k, _)) in candidates.iter().enumerate() {
                        if *to_primitive!(
                                xraise!(ns.eval_func_with_values(eq_func, vec![a1.clone(), k.clone()], rt.clone(), false)?.unwrap_value()),
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
    let hash_symbol = scope.identifier("hash");

    scope.add_dyn_func("mapping", move |_params, _types, ns, bind| {
        let (a0,) = unpack_types!(bind, 0);

        let inner_eq = get_func(ns, eq_symbol, &[a0.clone(), a0.clone()], &X_BOOL)?;
        let inner_hash = get_func(ns, hash_symbol, &[a0.clone()], &X_INT)?;

        Ok(XFunctionFactoryOutput::from_native(
            XFuncSpec::new(&[], XMappingType::xtype(a0.clone(), X_UNKNOWN.clone())),
            move |_args, ns, _tca, rt| {

                let inner_equal_value = xraise!(ns.eval(&inner_eq, rt.clone(), false)?.unwrap_value());
                let inner_hash_value = xraise!(ns.eval(&inner_hash, rt.clone(), false)?.unwrap_value());
                Ok(manage_native!(
                    XMapping::new(inner_hash_value, inner_equal_value, Default::default()),
                    rt
                ))
            },
        ))
    })
}
