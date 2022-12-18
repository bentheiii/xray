use crate::builtin::core::{eval, get_func, xerr};
use crate::builtin::sequence::{XSequence, XSequenceType};
use crate::native_types::{NativeType, XNativeValue};
use crate::runtime_scope::RuntimeScope;
use crate::runtime_violation::RuntimeViolation;
use crate::xtype::{XFuncSpec, X_BOOL, X_INT};
use crate::xvalue::{ManagedXError, ManagedXValue, XFunction, XFunctionFactoryOutput, XValue};
use crate::XType::XCallable;
use crate::{forward_err, manage_native, to_native, to_primitive, unpack_types, xraise, CompilationError, RTCell, RootCompilationScope, XCallableSpec, XStaticFunction, XType, parse_hash};
use derivative::Derivative;
use num_traits::ToPrimitive;
use rc::Rc;
use std::cmp::{max};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Debug;
use std::io::Write;
use std::iter::once;
use std::mem::size_of;
use std::rc;
use std::sync::Arc;

use crate::xexpr::TailedEvalResult;

#[derive(Debug, Clone)]
struct XSetType;

impl XSetType {
    fn xtype(k: Arc<XType>) -> Arc<XType> {
        Arc::new(XType::XNative(Box::new(Self {}), vec![k]))
    }
}

impl NativeType for XSetType {
    fn generic_names(&self) -> Vec<String> {
        vec!["T".to_string()]
    }
    fn name(&self) -> &str {
        "Set"
    }
}

type SetBucket<W> = Vec<Rc<ManagedXValue<W>>>;

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
struct XSet<W: Write + 'static> {
    inner: HashMap<u64, SetBucket<W>>,
    len: usize,
    hash_func: Rc<ManagedXValue<W>>,
    eq_func: Rc<ManagedXValue<W>>,
}

impl<W: Write + 'static> XSet<W> {
    fn new(
        hash_func: Rc<ManagedXValue<W>>,
        eq_func: Rc<ManagedXValue<W>>,
        table: HashMap<u64, SetBucket<W>>,
        len: usize,
    ) -> Self {
        Self {
            inner: table,
            len,
            hash_func,
            eq_func,
        }
    }

    fn with_update(
        &self,
        items: impl Iterator<Item=Rc<ManagedXValue<W>>>,
        ns: &RuntimeScope<W>,
        rt: RTCell<W>,
    ) -> Result<TailedEvalResult<W>, RuntimeViolation> {
        let hash_func = to_primitive!(self.hash_func, Function);
        let mut eq_func = None;
        let mut new_table = self.inner.clone();
        let mut new_len = self.len;
        for k in items {
            let hash_key = parse_hash!(
                ns.eval_func_with_values(hash_func, vec![Ok(k.clone())], rt.clone(), false)?,
                rt.clone()
            );

            let spot = new_table.entry(hash_key);
            match spot {
                Entry::Vacant(spot) => {
                    new_len += 1;
                    spot.insert(vec![k]);
                }
                Entry::Occupied(mut spot) => {
                    if eq_func.is_none() {
                        eq_func = Some(to_primitive!(self.eq_func, Function));
                    }
                    let mut found = false;
                    for existing_k in spot.get().iter() {
                        if *to_primitive!(
                            xraise!(ns
                                .eval_func_with_values(
                                    eq_func.unwrap(),
                                    vec![Ok(existing_k.clone()), Ok(k.clone())],
                                    rt.clone(),
                                    false
                                )?
                                .unwrap_value()),
                            Bool
                        ) {
                            found = true;
                            break;
                        }
                    }
                    if !found {
                        new_len += 1;
                        spot.get_mut().push(k);
                    }
                }
            }
        }

        Ok(manage_native!(
            Self::new(
                self.hash_func.clone(),
                self.eq_func.clone(),
                new_table,
                new_len
            ),
            rt
        ))
    }
}

impl<W: Write + 'static> XNativeValue for XSet<W> {
    fn size(&self) -> usize {
        (self.len + self.inner.len() + 2) * size_of::<usize>()
    }
}

pub(crate) fn add_set_type<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], _) = scope.generics_from_names(["T"]);
    scope.add_native_type("Set", XSetType::xtype(t))
}

pub(crate) fn add_set_new<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);

    scope.add_func(
        "set",
        XFuncSpec::new(
            &[
                &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![t.clone()],
                    return_type: X_INT.clone(),
                })),
                &Arc::new(XCallable(XCallableSpec {
                    param_types: vec![t.clone(), t.clone()],
                    return_type: X_BOOL.clone(),
                })),
            ],
            XSetType::xtype(t),
        )
            .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let hash_func = xraise!(eval(&args[0], ns, &rt)?);
            let eq_func = xraise!(eval(&args[1], ns, &rt)?);
            Ok(manage_native!(
                XSet::new(hash_func, eq_func, Default::default(), 0),
                rt
            ))
        }),
    )
}

pub(crate) fn add_set_update<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let st = XSetType::xtype(t.clone());

    scope.add_func(
        "update",
        XFuncSpec::new(
            &[
                &st,
                &XSequenceType::xtype(t),
            ],
            st.clone(),
        )
            .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let set = to_native!(a0, XSet<W>);
            let seq = to_native!(a1, XSequence<W>);
            rt.borrow()
                .can_allocate(max(set.len, seq.len()))?;
            let arr = xraise!(seq
                .iter(ns, rt.clone())
                .collect::<Result<Result<Vec<_>, _>, _>>()?);
            set.with_update(arr.into_iter(), ns, rt)
        }),
    )
}

pub(crate) fn add_set_add<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let st = XSetType::xtype(t.clone());

    scope.add_func(
        "add",
        XFuncSpec::new(&[&st, &t], st.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let mapping = to_native!(a0, XSet<W>);
            rt.borrow().can_allocate(mapping.len * 2)?;
            mapping.with_update(once(a1), ns, rt)
        }),
    )
}

pub(crate) fn add_set_contains<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let st = XSetType::xtype(t.clone());

    scope.add_func(
        "contains",
        XFuncSpec::new(
            &[
                &st,
                &t,
            ],
            X_BOOL.clone(),
        )
            .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let set = to_native!(a0, XSet<W>);

            let hash_func = to_primitive!(set.hash_func, Function);
            let hash_key = parse_hash!(
                ns.eval_func_with_values(hash_func, vec![Ok(a1.clone())], rt.clone(), false)?,
                rt.clone()
            );

            let spot = set.inner.get(&hash_key);

            match spot {
                None => Ok(ManagedXValue::new(XValue::Bool(false), rt)?.into()),
                Some(candidates) => {
                    let eq_func = to_primitive!(set.eq_func, Function);
                    for k in candidates.iter() {
                        if *to_primitive!(
                            xraise!(ns
                                .eval_func_with_values(
                                    eq_func,
                                    vec![Ok(a1.clone()), Ok(k.clone())],
                                    rt.clone(),
                                    false
                                )?
                                .unwrap_value()),
                            Bool
                        ) {
                            return Ok(ManagedXValue::new(XValue::Bool(true), rt)?.into());
                        }
                    }
                    Ok(ManagedXValue::new(XValue::Bool(false), rt)?.into())
                }
            }
        }),
    )
}

pub(crate) fn add_set_len<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let st = XSetType::xtype(t.clone());

    scope.add_func(
        "len",
        XFuncSpec::new(
            &[
                &st,
            ],
            X_INT.clone(),
        )
            .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let set = to_native!(a0, XSet<W>);
            Ok(ManagedXValue::new(XValue::Int(set.len.into()), rt)?.into())
        }),
    )
}

pub(crate) fn add_set_to_array<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let st = XSetType::xtype(t.clone());

    scope.add_func(
        "to_array",
        XFuncSpec::new(
            &[
                &st,
            ],
            XSequenceType::xtype(t),
        )
            .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let set = to_native!(a0, XSet<W>);
            rt.borrow().can_allocate(set.len)?;
            let items = set.inner.values().flat_map(
                |lst| lst.clone()
            ).collect::<Vec<_>>();
            Ok(manage_native!(
                XSequence::array(items),
                rt
            ))
        }),
    )
}

#[allow(clippy::type_complexity)]
fn bucket_without<W: Write + 'static>(
    old_bucket: &SetBucket<W>,
    eq_func: &XFunction<W>,
    key: &Rc<ManagedXValue<W>>,
    ns: &RuntimeScope<W>,
    rt: &RTCell<W>,
) -> Result<Result<Option<SetBucket<W>>, Rc<ManagedXError<W>>>, RuntimeViolation> {
    for (i, k) in old_bucket.iter().enumerate() {
        if *to_primitive!(
            forward_err!(ns
                .eval_func_with_values(
                    eq_func,
                    vec![Ok(key.clone()), Ok(k.clone())],
                    rt.clone(),
                    false
                )?
                .unwrap_value()),
            Bool
        ) {
            return Ok(Ok(Some(
                old_bucket[..i]
                    .iter()
                    .cloned()
                    .chain(old_bucket[i + 1..].iter().cloned())
                    .collect(),
            )));
        }
    }
    Ok(Ok(None))
}

pub(crate) fn add_set_remove<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let st = XSetType::xtype(t.clone());

    scope.add_func(
        "remove",
        XFuncSpec::new(&[&st, &t], st.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let set = to_native!(a0, XSet<W>);
            if set.len == 0 {
                return xerr(ManagedXError::new("item not found", rt)?)
            }
            rt.borrow().can_allocate(set.len - 1)?;
            let hash_func = to_primitive!(set.hash_func, Function);
            let hash_key = parse_hash!(
                ns.eval_func_with_values(hash_func, vec![Ok(a1.clone())], rt.clone(), false)?,
                rt.clone()
            );
            let spot = set.inner.get(&hash_key);
            match spot {
                None => xerr(ManagedXError::new("item not found", rt)?),
                Some(candidates) => {
                    let eq_func = to_primitive!(set.eq_func, Function);
                    let Some(new_spot) = xraise!(bucket_without(candidates, eq_func, &a1, ns, &rt)?) else { return xerr(ManagedXError::new("item not found", rt)?) };
                    let mut new_table = HashMap::from([(hash_key, new_spot)]);
                    for (k, v) in &set.inner {
                        if *k != hash_key {
                            new_table.insert(*k, v.clone());
                        }
                    }
                    Ok(manage_native!(
                        XSet::new(set.hash_func.clone(), set.eq_func.clone(), new_table, set.len-1),
                        rt
                    ))
                }
            }
        }),
    )
}

pub(crate) fn add_set_discard<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let st = XSetType::xtype(t.clone());

    scope.add_func(
        "discard",
        XFuncSpec::new(&[&st, &t], st.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let set = to_native!(a0, XSet<W>);
            if set.len == 0 {
                return Ok(a0.clone().into());
            }
            rt.borrow().can_allocate(set.len - 1)?;
            let hash_func = to_primitive!(set.hash_func, Function);
            let hash_key = parse_hash!(
                ns.eval_func_with_values(hash_func, vec![Ok(a1.clone())], rt.clone(), false)?,
                rt.clone()
            );
            let spot = set.inner.get(&hash_key);
            match spot {
                None => Ok(a0.clone().into()),
                Some(candidates) => {
                    let eq_func = to_primitive!(set.eq_func, Function);
                    let Some(new_spot) = xraise!(bucket_without(candidates, eq_func, &a1, ns, &rt)?) else { return Ok(a0.clone().into()); };
                    let mut new_table = HashMap::from([(hash_key, new_spot)]);
                    for (k, v) in &set.inner {
                        if *k != hash_key {
                            new_table.insert(*k, v.clone());
                        }
                    }
                    Ok(manage_native!(
                        XSet::new(set.hash_func.clone(), set.eq_func.clone(), new_table, set.len-1),
                        rt
                    ))
                }
            }
        }),
    )
}

pub(crate) fn add_set_bit_and<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let st = XSetType::xtype(t.clone());

    scope.add_func(
        "bit_and",
        XFuncSpec::new(
            &[
                &st,
                &st
            ],
            st.clone(),
        )
            .generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let set0 = to_native!(a0, XSet<W>);
            let set1 = to_native!(a1, XSet<W>);

            let hash_func = to_primitive!(set0.hash_func, Function);
            let eq_func = to_primitive!(set0.eq_func, Function);


            let mut new_values = Vec::new();

            for k1 in set1.inner.values().flat_map(|lst| lst.clone()) {
                let hash_key = parse_hash!(
                ns.eval_func_with_values(hash_func, vec![Ok(k1.clone())], rt.clone(), false)?,
                rt.clone()
            );
                let spot = set0.inner.get(&hash_key);
                let found_in_set0 = match spot {
                    None => false,
                    Some(candidates) => {
                        let mut found = false;
                        for k0 in candidates.iter() {
                            if *to_primitive!(
                            xraise!(ns
                                .eval_func_with_values(
                                    eq_func,
                                    vec![Ok(k0.clone()), Ok(k1.clone())],
                                    rt.clone(),
                                    false
                                )?
                                .unwrap_value()),
                            Bool
                        ) {
                                found = true;
                            }
                        }
                        found
                    }
                };
                if found_in_set0 {
                    new_values.push(k1);
                    rt.borrow()
                        .can_afford(&new_values)?;
                }
            }

            Ok(XSet::new(set0.hash_func.clone(), set0.eq_func.clone(), Default::default(), 0).with_update(new_values.into_iter(), &ns, rt)?)
        }),
    )
}

pub(crate) fn add_set_new_dyn<W: Write + 'static>(
    scope: &mut RootCompilationScope<W>,
) -> Result<(), CompilationError> {
    let eq_symbol = scope.identifier("eq");
    let hash_symbol = scope.identifier("hash");

    scope.add_dyn_func(
        "set",
        "default-funcs",
        move |_params, _types, ns, bind| {
            let (a0, ) = unpack_types!(bind, 0);

            let inner_eq = get_func(ns, eq_symbol, &[a0.clone(), a0.clone()], &X_BOOL)?;
            let inner_hash = get_func(ns, hash_symbol, &[a0.clone()], &X_INT)?;

            Ok(XFunctionFactoryOutput::from_native(
                XFuncSpec::new(&[], XSetType::xtype(a0.clone())),
                move |_args, ns, _tca, rt| {
                    let inner_equal_value =
                        xraise!(ns.eval(&inner_eq, rt.clone(), false)?.unwrap_value());
                    let inner_hash_value =
                        xraise!(ns.eval(&inner_hash, rt.clone(), false)?.unwrap_value());
                    Ok(manage_native!(
                        XSet::new(inner_hash_value, inner_equal_value, Default::default(), 0),
                        rt
                    ))
                },
            ))
        },
    )
}