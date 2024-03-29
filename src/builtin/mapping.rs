use crate::builtin::core::{
    eval, get_func, get_func_with_type, unpack_dyn_types, unpack_native, xerr,
};
use crate::builtin::generators::{XGenerator, XGeneratorType};
use crate::builtin::optional::{XOptional, XOptionalType};
use crate::native_types::{NativeType, XNativeValue};
use crate::root_runtime_scope::RuntimeResult;
use crate::runtime_scope::RuntimeScope;
use crate::runtime_violation::RuntimeViolation;
use crate::util::lazy_bigint::LazyBigint;
use crate::xtype::{XFuncSpec, X_BOOL, X_INT, X_UNKNOWN};
use crate::xvalue::{ManagedXError, ManagedXValue, XFunctionFactoryOutput, XResult, XValue};
use crate::XType::XCallable;
use crate::{
    delegate, forward_err, manage_native, to_native, to_primitive, xraise, CompilationError,
    RTCell, RootCompilationScope, XCallableSpec, XStaticFunction, XType,
};
use derivative::Derivative;
use num_traits::ToPrimitive;
use rc::Rc;
use std::collections::HashMap;
use std::fmt::Debug;

use std::iter::once;
use std::mem::size_of;
use std::rc;
use std::sync::Arc;

use crate::xexpr::{TailedEvalResult, XExpr};

#[derive(Debug, Clone)]
pub(crate) struct XMappingType;

impl XMappingType {
    pub(crate) fn xtype(k: Arc<XType>, v: Arc<XType>) -> Arc<XType> {
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

type MappingBucket<W, R, T, V> = Vec<(Rc<ManagedXValue<W, R, T>>, V)>;

#[derive(Derivative)]
#[derivative(Debug(bound = "V: Debug"), Clone(bound = "V:Clone"))]
pub(super) struct XMapping<W, R, T, V = Rc<ManagedXValue<W, R, T>>> {
    inner: HashMap<u64, MappingBucket<W, R, T, V>>,
    len: usize,
    hash_func: Rc<ManagedXValue<W, R, T>>,
    eq_func: Rc<ManagedXValue<W, R, T>>,
}

#[derive(Debug)]
enum KeyLocation {
    Missing(u64),
    Vacant(u64),
    Found((u64, usize)),
}

impl KeyLocation {
    fn found(self) -> Option<(u64, usize)> {
        if let Self::Found(p) = self {
            Some(p)
        } else {
            None
        }
    }
}

impl<W: 'static, R: 'static, T: 'static, V: Debug + 'static> XMapping<W, R, T, V> {
    pub(super) fn new(
        hash_func: Rc<ManagedXValue<W, R, T>>,
        eq_func: Rc<ManagedXValue<W, R, T>>,
        dict: HashMap<u64, MappingBucket<W, R, T, V>>,
        len: usize,
    ) -> Self {
        Self {
            inner: dict,
            len,
            hash_func,
            eq_func,
        }
    }

    fn with_update(
        &self,
        items: impl Iterator<
            Item = Result<
                Result<(Rc<ManagedXValue<W, R, T>>, V), Rc<ManagedXError<W, R, T>>>,
                RuntimeViolation,
            >,
        >,
        ns: &RuntimeScope<W, R, T>,
        rt: RTCell<W, R, T>,
    ) -> RuntimeResult<TailedEvalResult<W, R, T>>
    where
        V: Clone,
    {
        let mut ret = self.clone();
        for item in items {
            let (k, v) = xraise!(item?);
            xraise!(ret.put(&k, || v.clone(), |_| v.clone(), ns, rt.clone())?);
        }

        Ok(manage_native!(ret, rt))
    }

    fn locate(
        &self,
        key: &Rc<ManagedXValue<W, R, T>>,
        ns: &RuntimeScope<W, R, T>,
        rt: RTCell<W, R, T>,
    ) -> XResult<KeyLocation, W, R, T> {
        let hash_func = to_primitive!(self.hash_func, Function);
        let raw_hash = forward_err!(ns
            .eval_func_with_values(hash_func, vec![Ok(key.clone())], rt.clone(), false)?
            .unwrap_value());
        let hash_key = forward_err!(to_primitive!(raw_hash, Int)
            .to_u64()
            .ok_or(ManagedXError::new("hash is out of bounds", rt.clone())?));
        let Some(bucket) = self.inner.get(&hash_key) else { return Ok(Ok(KeyLocation::Vacant(hash_key))); };
        let eq_func = Some(to_primitive!(self.eq_func, Function));
        for (i, (k, _)) in bucket.iter().enumerate() {
            if *to_primitive!(
                forward_err!(ns
                    .eval_func_with_values(
                        eq_func.unwrap(),
                        vec![Ok(key.clone()), Ok(k.clone())],
                        rt.clone(),
                        false
                    )?
                    .unwrap_value()),
                Bool
            ) {
                return Ok(Ok(KeyLocation::Found((hash_key, i))));
            }
        }
        Ok(Ok(KeyLocation::Missing(hash_key)))
    }

    fn get(&self, coordinates: (u64, usize)) -> &V {
        &self.inner[&coordinates.0][coordinates.1].1
    }

    fn try_put_located(
        &mut self,
        k: &Rc<ManagedXValue<W, R, T>>,
        loc: KeyLocation,
        on_empty: impl FnOnce() -> XResult<V, W, R, T>,
        on_found: impl FnOnce(&V) -> XResult<V, W, R, T>,
    ) -> XResult<&V, W, R, T> {
        Ok(Ok(match loc {
            KeyLocation::Found((hash_key, idx)) => {
                let prev_v = &self.inner.get(&hash_key).unwrap()[idx].1;
                let v = forward_err!(on_found(prev_v)?);
                let spot = &mut self.inner.get_mut(&hash_key).unwrap()[idx];
                spot.1 = v;
                &spot.1
            }
            KeyLocation::Missing(hash_key) => {
                let v = forward_err!(on_empty()?);
                let bucket = self.inner.get_mut(&hash_key).unwrap();
                bucket.push((k.clone(), v));
                self.len += 1;
                &bucket.last().unwrap().1
            }
            KeyLocation::Vacant(hash_key) => {
                let v = forward_err!(on_empty()?);
                let bucket = self.inner.entry(hash_key).or_insert(vec![(k.clone(), v)]);
                self.len += 1;
                &bucket.last().unwrap().1
            }
        }))
    }

    fn put_located(
        &mut self,
        k: &Rc<ManagedXValue<W, R, T>>,
        loc: KeyLocation,
        on_empty: impl FnOnce() -> V,
        on_found: impl FnOnce(&V) -> V,
    ) -> XResult<&V, W, R, T> {
        self.try_put_located(k, loc, || Ok(Ok(on_empty())), |v| Ok(Ok(on_found(v))))
    }

    pub(super) fn put(
        &mut self,
        k: &Rc<ManagedXValue<W, R, T>>,
        on_empty: impl FnOnce() -> V,
        on_found: impl FnOnce(&V) -> V,
        ns: &RuntimeScope<W, R, T>,
        rt: RTCell<W, R, T>,
    ) -> XResult<&V, W, R, T> {
        let loc = forward_err!(self.locate(k, ns, rt)?);
        self.put_located(k, loc, on_empty, on_found)
    }

    pub(super) fn try_put(
        &mut self,
        k: &Rc<ManagedXValue<W, R, T>>,
        on_empty: impl FnOnce() -> XResult<V, W, R, T>,
        on_found: impl FnOnce(&V) -> XResult<V, W, R, T>,
        ns: &RuntimeScope<W, R, T>,
        rt: RTCell<W, R, T>,
    ) -> XResult<&V, W, R, T> {
        let loc = forward_err!(self.locate(k, ns, rt)?);
        self.try_put_located(k, loc, on_empty, on_found)
    }

    pub(super) fn iter(&self) -> impl Iterator<Item = (Rc<ManagedXValue<W, R, T>>, V)> + '_
    where
        V: Clone,
    {
        self.inner.iter().flat_map(|(_, b)| b.iter()).cloned()
    }
}

impl<W: 'static, R: 'static, T: 'static, V: Debug + 'static> XNativeValue for XMapping<W, R, T, V> {
    fn dyn_size(&self) -> usize {
        (self.inner.len() * size_of::<MappingBucket<W, R, T, V>>())
            + (self.len * size_of::<Rc<ManagedXValue<W, R, T>>>())
            + (self.len * size_of::<V>())
    }
}

pub(crate) fn add_mapping_type<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([k, v], _) = scope.generics_from_names(["K", "V"]);
    scope.add_native_type("Mapping", XMappingType::xtype(k, v))
}

pub(crate) fn add_mapping_new<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
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
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let hash_func = xraise!(eval(&args[0], ns, &rt)?);
            let eq_func = xraise!(eval(&args[1], ns, &rt)?);
            Ok(manage_native!(
                XMapping::<W, R, T>::new(hash_func, eq_func, Default::default(), 0),
                rt
            ))
        }),
    )
}

pub(crate) fn add_mapping_clear<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["K"]);
    let mt = XMappingType::xtype(t, X_UNKNOWN.clone());

    scope.add_func(
        "clear",
        XFuncSpec::new(&[&mt], mt.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let map0 = to_native!(a0, XMapping<W, R, T>);
            if map0.len == 0 {
                return Ok(a0.clone().into());
            }
            Ok(manage_native!(
                XMapping::<_, _, _, Rc<ManagedXValue<W, R, T>>>::new(
                    map0.hash_func.clone(),
                    map0.eq_func.clone(),
                    Default::default(),
                    0,
                ),
                rt
            ))
        }),
    )
}

pub(crate) fn add_mapping_set<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v.clone());

    scope.add_func(
        "set",
        XFuncSpec::new(&[&mp, &k, &v], mp.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a2 = xraise!(eval(&args[2], ns, &rt)?);
            let mapping = to_native!(a0, XMapping<W, R, T>);
            rt.can_allocate(mapping.len * 2 * size_of::<usize>())?;
            mapping.with_update(once(Ok(Ok((a1, a2)))), ns, rt)
        }),
    )
}

pub(crate) fn add_mapping_set_default<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v.clone());

    scope.add_func(
        "set_default",
        XFuncSpec::new(&[&mp, &k, &v], mp.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let mapping = to_native!(a0, XMapping<W, R, T>);
            let loc = xraise!(mapping.locate(&a1, ns, rt.clone())?);
            if let KeyLocation::Found(..) = loc {
                return Ok(a0.into());
            }
            rt.can_allocate(mapping.len * 2 * size_of::<usize>())?;
            let a2 = xraise!(eval(&args[2], ns, &rt)?);
            let mut ret = mapping.clone();
            xraise!(ret.put_located(&a1, loc, || a2, |_| unreachable!())?);
            Ok(manage_native!(ret, rt))
        }),
    )
}

pub(crate) fn add_mapping_update<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v.clone());

    scope.add_func(
        "update",
        XFuncSpec::new(
            &[
                &mp,
                &XGeneratorType::xtype(Arc::new(XType::Tuple(vec![k, v]))),
            ],
            mp.clone(),
        )
        .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let mapping = to_native!(a0, XMapping<W, R, T>);
            let gen0 = to_native!(a1, XGenerator<W, R, T>);
            let items = gen0.iter(ns, rt.clone()).map(|t| {
                let t = match t? {
                    Ok(t) => t,
                    Err(e) => return Ok(Err(e)),
                };
                let tup = to_primitive!(t, StructInstance);
                Ok(Ok((tup[0].clone(), tup[1].clone())))
            });
            mapping.with_update(items, ns, rt)
        }),
    )
}

pub(crate) fn add_mapping_update_from_keys<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v.clone());

    scope.add_func(
        "update_from_keys",
        XFuncSpec::new(
            &[
                &mp,
                &XGeneratorType::xtype(k.clone()),
                &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![k.clone()],
                    return_type: v.clone(),
                })),
                &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![k, v.clone()],
                    return_type: v,
                })),
            ],
            mp.clone(),
        )
        .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let a2 = xraise!(eval(&args[2], ns, &rt)?);
            let a3 = xraise!(eval(&args[3], ns, &rt)?);
            let mapping = to_native!(a0, XMapping<W, R, T>);
            let gen0 = to_native!(a1, XGenerator<W, R, T>);
            let on_empty = to_primitive!(a2, Function);
            let on_occupied = to_primitive!(a3, Function);

            let mut ret = XMapping::new(
                mapping.hash_func.clone(),
                mapping.eq_func.clone(),
                mapping.inner.clone(),
                mapping.len,
            );
            for item in gen0.iter(ns, rt.clone()) {
                let item = xraise!(item?);
                xraise!(ret.try_put(
                    &item,
                    || {
                        ns.eval_func_with_values(
                            on_empty,
                            vec![Ok(item.clone())],
                            rt.clone(),
                            false,
                        )
                        .map(|v| v.unwrap_value())
                    },
                    |v| {
                        ns.eval_func_with_values(
                            on_occupied,
                            vec![Ok(item.clone()), Ok(v.clone())],
                            rt.clone(),
                            false,
                        )
                        .map(|v| v.unwrap_value())
                    },
                    ns,
                    rt.clone()
                )?);
            }

            Ok(manage_native!(ret, rt))
        }),
    )
}

pub(crate) fn add_mapping_lookup<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v.clone());

    scope.add_func(
        "lookup",
        XFuncSpec::new(&[&mp, &k], XOptionalType::xtype(v)).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let mapping = to_native!(a0, XMapping<W, R, T>);
            let val = xraise!(mapping.locate(&a1, ns, rt.clone())?)
                .found()
                .map(|c| mapping.get(c));
            Ok(manage_native!(
                XOptional::<W, R, T> {
                    value: val.cloned()
                },
                rt
            ))
        }),
    )
}

pub(crate) fn add_mapping_get3<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v.clone());

    scope.add_func(
        "get",
        XFuncSpec::new(&[&mp, &k, &v], v.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let mapping = to_native!(a0, XMapping<W, R, T>);
            let val = match xraise!(mapping.locate(&a1, ns, rt.clone())?).found() {
                Some(f) => mapping.get(f).clone(),
                None => xraise!(eval(&args[2], ns, &rt)?),
            };
            Ok(Ok(val).into())
        }),
    )
}

pub(crate) fn add_mapping_len<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k, v);

    scope.add_func(
        "len",
        XFuncSpec::new(&[&mp], X_INT.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let mapping = to_native!(a0, XMapping<W, R, T>);
            Ok(ManagedXValue::new(XValue::Int(mapping.len.into()), rt)?.into())
        }),
    )
}

pub(crate) fn add_mapping_to_generator<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v.clone());

    scope.add_func(
        "to_generator",
        XFuncSpec::new(
            &[&mp],
            XGeneratorType::xtype(Arc::new(XType::Tuple(vec![k, v]))),
        )
        .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let gen = XGenerator::FromMapping(a0);
            Ok(manage_native!(gen, rt))
        }),
    )
}

pub(crate) fn add_mapping_pop<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v);

    scope.add_func(
        "pop",
        XFuncSpec::new(&[&mp, &k], mp.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let mapping = to_native!(a0, XMapping<W, R, T>);
            if mapping.len == 0 {
                return xerr(ManagedXError::new("key not found", rt)?);
            }
            let KeyLocation::Found((hash_key, idx)) = xraise!(mapping.locate(&a1, ns, rt.clone())?) else {
                return xerr(ManagedXError::new("key not found", rt)?);
            };
            rt.can_allocate((mapping.len - 1) * 2 * size_of::<usize>())?;
            let mut new_dict = HashMap::from_iter(mapping.inner.iter().filter(|(k, _)| k != &&hash_key).map(|(k, b)| (*k, b.clone())));
            let old_bucket = &mapping.inner[&hash_key];
            new_dict.insert(hash_key, old_bucket.iter().take(idx).chain(old_bucket.iter().skip(idx + 1)).cloned().collect());
            Ok(manage_native!(
                        XMapping::new(mapping.hash_func.clone(), mapping.eq_func.clone(), new_dict, mapping.len-1),
                        rt
                    ))
        }),
    )
}

pub(crate) fn add_mapping_discard<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v);

    scope.add_func(
        "discard",
        XFuncSpec::new(&[&mp, &k], mp.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let mapping = to_native!(a0, XMapping<W, R, T>);
            if mapping.len == 0 {
                return Ok(a0.clone().into());
            }
            let KeyLocation::Found((hash_key, idx)) = xraise!(mapping.locate(&a1, ns, rt.clone())?) else {
                return Ok(a0.clone().into());
            };
            rt.can_allocate((mapping.len - 1) * 2* size_of::<usize>())?;
            let mut new_dict = HashMap::from_iter(mapping.inner.iter().filter(|(k, _)| k != &&hash_key).map(|(k, b)| (*k, b.clone())));
            let old_bucket = &mapping.inner[&hash_key];
            new_dict.insert(hash_key, old_bucket.iter().take(idx).chain(old_bucket.iter().skip(idx + 1)).cloned().collect());
            Ok(manage_native!(
                        XMapping::new(mapping.hash_func.clone(), mapping.eq_func.clone(), new_dict, mapping.len-1),
                        rt
                    ))
        }),
    )
}

pub(crate) fn add_mapping_dyn_hash<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let symbol = scope.identifier("hash");

    scope.add_dyn_func("hash", "mappings", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let [a0] = unpack_dyn_types(types)?;
        let [k0, v0] = unpack_native(a0, "Mapping")? else { unreachable!() };

        let inner = get_func(ns, symbol, &[v0.clone()], &X_INT)?;


        Ok(XFunctionFactoryOutput::from_delayed_native(
            XFuncSpec::new(&[&XMappingType::xtype(k0.clone(), v0.clone())], X_INT.clone()),
            move |ns, rt| {
                let inner_value = forward_err!(ns.eval(&inner, rt, false)?.unwrap_value());


                Ok(Ok(move |args: &[XExpr<W, R, T>], ns: &RuntimeScope<'_, W, R, T>, _tca, rt| {
                    let a0 = xraise!(eval(&args[0], ns, &rt)?);
                    let m0 = to_native!(a0, XMapping<W, R, T>);
                    let inner_value_hash_func = to_primitive!(inner_value, Function);
                    // note that since order is important to the default hasher, we'll just xor them together
                    let mut ret = 0u64;
                    for (hash, bucket) in m0.inner.iter() {
                        let v = hash.wrapping_add(bucket.len() as u64);
                        ret ^= v;
                        for (_, value) in bucket.iter() {
                            let hash = xraise!(ns
                                .eval_func_with_values(inner_value_hash_func, vec![Ok(value.clone())], rt.clone(), false)?
                                .unwrap_value());
                            let Some(f) = to_primitive!(hash, Int).to_u64() else { return xerr(ManagedXError::new("hash out of bounds", rt)?); };
                            ret ^= f;
                        }
                    }

                    Ok(ManagedXValue::new(XValue::Int(LazyBigint::from(ret)), rt)?.into())
                }))
            },
        ))
    })
}

pub(crate) fn add_mapping_dyn_eq<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let symbol = scope.identifier("eq");

    scope.add_dyn_func("eq", "mappings", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let [a0, a1] = unpack_dyn_types(types)?;
        let [k0, v0] = unpack_native(a0, "Mapping")? else { unreachable!() };
        let [k1, v1] = unpack_native(a1, "Mapping")? else { unreachable!() };

        if k0 != k1 {
            return Err("the two mappings must have the same key type".to_string());
        }

        let inner = get_func(ns, symbol, &[v0.clone(), v1.clone()], &X_BOOL)?;


        Ok(XFunctionFactoryOutput::from_delayed_native(
            XFuncSpec::new(&[a0, a1], X_BOOL.clone()),
            move |ns, rt| {
                let inner_value = forward_err!(ns.eval(&inner, rt, false)?.unwrap_value());


                Ok(Ok(move |args: &[XExpr<W, R, T>], ns: &RuntimeScope<'_, W, R, T>, _tca, rt| {
                    let a0 = xraise!(eval(&args[0], ns, &rt)?);
                    let a1 = xraise!(eval(&args[1], ns, &rt)?);
                    let m0 = to_native!(a0, XMapping<W, R, T>);
                    let m1 = to_native!(a1, XMapping<W, R, T>);
                    if m0.len != m1.len {
                        return Ok(ManagedXValue::new(XValue::Bool(false), rt)?.into());
                    }
                    let inner_func = to_primitive!(inner_value, Function);

                    for (key, value) in m0.iter() {
                        let KeyLocation::Found(location) = xraise!(m1.locate(&key, ns, rt.clone())?) else {
                            return Ok(ManagedXValue::new(XValue::Bool(false), rt)?.into());
                        };
                        let other_v = m1.get(location);
                        let eq = xraise!(ns
                                .eval_func_with_values(
                                    inner_func,
                                    vec![Ok(value), Ok(other_v.clone())],
                                    rt.clone(),
                                    false
                                )?
                                .unwrap_value());
                        let is_eq = to_primitive!(eq, Bool);
                        if !*is_eq {
                            return Ok(ManagedXValue::new(XValue::Bool(false), rt)?.into());
                        }
                    }

                    Ok(ManagedXValue::new(XValue::Bool(true), rt)?.into())
                }))
            },
        ))
    })
}

pub(crate) fn add_mapping_dyn_new<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let eq_symbol = scope.identifier("eq");
    let hash_symbol = scope.identifier("hash");
    let cb_symbol = scope.identifier("mapping");

    scope.add_dyn_func(
        "mapping",
        "default-funcs",
        move |_params, _types, ns, bind| {
            let [a0] = unpack_dyn_types(bind)?;

            let (inner_hash, hash_t) =
                get_func_with_type(ns, hash_symbol, &[a0.clone()], Some(&X_INT))?;
            let (inner_eq, eq_t) =
                get_func_with_type(ns, eq_symbol, &[a0.clone(), a0.clone()], Some(&X_BOOL))?;
            let (cb, cb_t) =
                get_func_with_type(ns, cb_symbol, &[hash_t.xtype(), eq_t.xtype()], None)?;

            Ok(XFunctionFactoryOutput::from_delayed_native(
                XFuncSpec::new(&[], cb_t.rtype()),
                delegate!(
                    with [inner_hash, inner_eq, cb],
                    args [],
                    cb(inner_hash, inner_eq)
                ),
            ))
        },
    )
}
