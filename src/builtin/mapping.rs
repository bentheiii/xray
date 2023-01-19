use crate::builtin::core::{eval, get_func, unpack_native, xerr};
use crate::builtin::generators::{XGenerator, XGeneratorType};
use crate::builtin::optional::{XOptional, XOptionalType};
use crate::native_types::{NativeType, XNativeValue};
use crate::runtime_scope::RuntimeScope;
use crate::runtime_violation::RuntimeViolation;
use crate::util::lazy_bigint::LazyBigint;
use crate::xtype::{XFuncSpec, X_BOOL, X_INT, X_UNKNOWN};
use crate::xvalue::{ManagedXError, ManagedXValue, XFunctionFactoryOutput, XValue};
use crate::XType::XCallable;
use crate::{
    forward_err, manage_native, to_native, to_primitive, unpack_types, xraise, CompilationError,
    RTCell, RootCompilationScope, XCallableSpec, XStaticFunction, XType,
};
use derivative::Derivative;
use num_traits::ToPrimitive;
use rc::Rc;
use std::collections::HashMap;
use std::fmt::Debug;
use std::io::Write;
use std::iter::once;
use std::mem::size_of;
use std::rc;
use std::sync::Arc;

use crate::xexpr::{TailedEvalResult, XExpr};

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
pub(super) struct XMapping<W> {
    inner: HashMap<u64, MappingBucket<W>>,
    len: usize,
    hash_func: Rc<ManagedXValue<W>>,
    eq_func: Rc<ManagedXValue<W>>,
}

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

impl<W: Write + 'static> XMapping<W> {
    fn new(
        hash_func: Rc<ManagedXValue<W>>,
        eq_func: Rc<ManagedXValue<W>>,
        dict: HashMap<u64, MappingBucket<W>>,
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
                Result<(Rc<ManagedXValue<W>>, Rc<ManagedXValue<W>>), Rc<ManagedXError<W>>>,
                RuntimeViolation,
            >,
        >,
        ns: &RuntimeScope<W>,
        rt: RTCell<W>,
    ) -> Result<TailedEvalResult<W>, RuntimeViolation> {
        let mut ret = Self::new(
            self.hash_func.clone(),
            self.eq_func.clone(),
            self.inner.clone(),
            self.len,
        );
        for item in items {
            let (k, v) = xraise!(item?);
            let loc = xraise!(ret.locate(&k, ns, rt.clone())?);
            match loc {
                KeyLocation::Found((hash_key, idx)) => {
                    ret.inner.get_mut(&hash_key).unwrap()[idx].1 = v
                }
                KeyLocation::Missing(hash_key) => {
                    ret.inner.get_mut(&hash_key).unwrap().push((k, v));
                    ret.len += 1;
                }
                KeyLocation::Vacant(hash_key) => {
                    ret.inner.insert(hash_key, vec![(k, v)]);
                    ret.len += 1;
                }
            }
        }

        Ok(manage_native!(ret, rt))
    }

    fn locate(
        &self,
        key: &Rc<ManagedXValue<W>>,
        ns: &RuntimeScope<W>,
        rt: RTCell<W>,
    ) -> Result<Result<KeyLocation, Rc<ManagedXError<W>>>, RuntimeViolation> {
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

    fn get(&self, coordinates: (u64, usize)) -> &Rc<ManagedXValue<W>> {
        &self.inner[&coordinates.0][coordinates.1].1
    }

    pub(super) fn iter(
        &self,
    ) -> impl Iterator<Item = (Rc<ManagedXValue<W>>, Rc<ManagedXValue<W>>)> + '_ {
        self.inner.iter().flat_map(|(_, b)| b.iter()).cloned()
    }
}

impl<W: 'static> XNativeValue for XMapping<W> {
    fn dyn_size(&self) -> usize {
        (self.len * 2 + self.inner.len() + 2) * size_of::<Rc<ManagedXValue<W>>>()
    }
}

pub(crate) fn add_mapping_type<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([k, v], _) = scope.generics_from_names(["K", "V"]);
    scope.add_native_type("Mapping", XMappingType::xtype(k, v))
}

pub(crate) fn add_mapping_new<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
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
                XMapping::new(hash_func, eq_func, Default::default(), 0),
                rt
            ))
        }),
    )
}

pub(crate) fn add_mapping_set<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
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
            let mapping = to_native!(a0, XMapping<W>);
            rt.borrow().can_allocate(mapping.len * 2)?;
            mapping.with_update(once(Ok(Ok((a1, a2)))), ns, rt)
        }),
    )
}

pub(crate) fn add_mapping_update<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
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
            let mapping = to_native!(a0, XMapping<W>);
            let gen0 = to_native!(a1, XGenerator<W>);
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

pub(crate) fn add_mapping_lookup<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v.clone());

    scope.add_func(
        "lookup",
        XFuncSpec::new(&[&mp, &k], XOptionalType::xtype(v)).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let mapping = to_native!(a0, XMapping<W>);
            let val = xraise!(mapping.locate(&a1, ns, rt.clone())?)
                .found()
                .map(|c| mapping.get(c));
            Ok(manage_native!(
                XOptional::<W> {
                    value: val.cloned()
                },
                rt
            ))
        }),
    )
}

pub(crate) fn add_mapping_len<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k, v);

    scope.add_func(
        "len",
        XFuncSpec::new(&[&mp], X_INT.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let mapping = to_native!(a0, XMapping<W>);
            Ok(ManagedXValue::new(XValue::Int(mapping.len.into()), rt)?.into())
        }),
    )
}

pub(crate) fn add_mapping_to_generator<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
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

pub(crate) fn add_mapping_pop<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v);

    scope.add_func(
        "pop",
        XFuncSpec::new(&[&mp, &k], mp.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let mapping = to_native!(a0, XMapping<W>);
            if mapping.len == 0 {
                return xerr(ManagedXError::new("key not found", rt)?);
            }
            let KeyLocation::Found((hash_key, idx)) = xraise!(mapping.locate(&a1, ns, rt.clone())?) else {
                return xerr(ManagedXError::new("key not found", rt)?);
            };
            rt.borrow().can_allocate((mapping.len - 1) * 2)?;
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

pub(crate) fn add_mapping_discard<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([k, v], params) = scope.generics_from_names(["K", "V"]);
    let mp = XMappingType::xtype(k.clone(), v);

    scope.add_func(
        "discard",
        XFuncSpec::new(&[&mp, &k], mp.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let mapping = to_native!(a0, XMapping<W>);
            if mapping.len == 0 {
                return Ok(a0.clone().into());
            }
            let KeyLocation::Found((hash_key, idx)) = xraise!(mapping.locate(&a1, ns, rt.clone())?) else {
                return Ok(a0.clone().into());
            };
            rt.borrow().can_allocate((mapping.len - 1) * 2)?;
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

pub(crate) fn add_mapping_dyn_hash<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let symbol = scope.identifier("hash");

    scope.add_dyn_func("hash", "mappings", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let (a0, ) = unpack_types!(types, 0);
        let [k0, v0] = unpack_native(a0, "Mapping")? else { unreachable!() };

        let inner = get_func(ns, symbol, &[v0.clone()], &X_INT)?;


        Ok(XFunctionFactoryOutput::from_delayed_native(
            XFuncSpec::new(&[&XMappingType::xtype(k0.clone(), v0.clone())], X_INT.clone()),
            move |ns, rt| {
                let inner_value = forward_err!(ns.eval(&inner, rt, false)?.unwrap_value());


                Ok(Ok(move |args: &[XExpr<W>], ns: &RuntimeScope<'_, W>, _tca, rt| {
                    let a0 = xraise!(eval(&args[0], ns, &rt)?);
                    let m0 = to_native!(a0, XMapping<W>);
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

pub(crate) fn add_mapping_dyn_eq<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let symbol = scope.identifier("eq");

    scope.add_dyn_func("eq", "mappings", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let (a0, a1) = unpack_types!(types, 0, 1);
        let [k0, v0] = unpack_native(a0, "Mapping")? else { unreachable!() };
        let [k1, v1] = unpack_native(a1, "Mapping")? else { unreachable!() };

        if k0 != k1{
            return Err("the two mappings must have the same key type".to_string())
        }

        let inner = get_func(ns, symbol, &[v0.clone(), v1.clone()], &X_BOOL)?;


        Ok(XFunctionFactoryOutput::from_delayed_native(
            XFuncSpec::new(&[a0, a1], X_BOOL.clone()),
            move |ns, rt| {
                let inner_value = forward_err!(ns.eval(&inner, rt, false)?.unwrap_value());


                Ok(Ok(move |args: &[XExpr<W>], ns: &RuntimeScope<'_, W>, _tca, rt| {
                    let a0 = xraise!(eval(&args[0], ns, &rt)?);
                    let a1 = xraise!(eval(&args[1], ns, &rt)?);
                    let m0 = to_native!(a0, XMapping<W>);
                    let m1 = to_native!(a1, XMapping<W>);
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

pub(crate) fn add_mapping_dyn_new<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let eq_symbol = scope.identifier("eq");
    let hash_symbol = scope.identifier("hash");

    scope.add_dyn_func(
        "mapping",
        "default-funcs",
        move |_params, _types, ns, bind| {
            let (a0,) = unpack_types!(bind, 0);

            let inner_eq = get_func(ns, eq_symbol, &[a0.clone(), a0.clone()], &X_BOOL)?;
            let inner_hash = get_func(ns, hash_symbol, &[a0.clone()], &X_INT)?;

            Ok(XFunctionFactoryOutput::from_delayed_native(
                XFuncSpec::new(&[], XMappingType::xtype(a0.clone(), X_UNKNOWN.clone())),
                move |ns, rt| {
                    let inner_equal_value =
                        forward_err!(ns.eval(&inner_eq, rt.clone(), false)?.unwrap_value());
                    let inner_hash_value =
                        forward_err!(ns.eval(&inner_hash, rt, false)?.unwrap_value());
                    Ok(Ok(
                        move |_args: &[XExpr<W>], _ns: &RuntimeScope<'_, W>, _tca, rt| {
                            Ok(manage_native!(
                                XMapping::new(
                                    inner_hash_value.clone(),
                                    inner_equal_value.clone(),
                                    Default::default(),
                                    0
                                ),
                                rt
                            ))
                        },
                    ))
                },
            ))
        },
    )
}
