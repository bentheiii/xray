use crate::builtin::core::{eval, get_func_with_type, unpack_dyn_types, xerr};
use crate::builtin::generators::{XGenerator, XGeneratorType};
use crate::native_types::{NativeType, XNativeValue};

use crate::root_runtime_scope::RuntimeResult;
use crate::runtime_scope::RuntimeScope;
use crate::util::lazy_bigint::LazyBigint;
use crate::xtype::{XFuncSpec, X_BOOL, X_INT};
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

type SetBucket<W, R, T> = Vec<Rc<ManagedXValue<W, R, T>>>;

enum KeyLocation {
    Missing(u64),
    Vacant(u64),
    Found((u64, usize)),
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub(crate) struct XSet<W, R, T> {
    inner: HashMap<u64, SetBucket<W, R, T>>,
    len: usize,
    hash_func: Rc<ManagedXValue<W, R, T>>,
    eq_func: Rc<ManagedXValue<W, R, T>>,
}

impl<W: 'static, R: 'static, T: 'static> XSet<W, R, T> {
    fn new(
        hash_func: Rc<ManagedXValue<W, R, T>>,
        eq_func: Rc<ManagedXValue<W, R, T>>,
        table: HashMap<u64, SetBucket<W, R, T>>,
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
        items: impl Iterator<Item = XResult<Rc<ManagedXValue<W, R, T>>, W, R, T>>,
        ns: &RuntimeScope<W, R, T>,
        rt: RTCell<W, R, T>,
    ) -> RuntimeResult<TailedEvalResult<W, R, T>> {
        let mut ret = Self::new(
            self.hash_func.clone(),
            self.eq_func.clone(),
            self.inner.clone(),
            self.len,
        );
        for k in items {
            let k = xraise!(k?);
            let loc = xraise!(ret.locate(&k, ns, rt.clone())?);
            match loc {
                KeyLocation::Found(_) => {}
                KeyLocation::Missing(hash_key) => {
                    ret.inner.get_mut(&hash_key).unwrap().push(k);
                    ret.len += 1;
                }
                KeyLocation::Vacant(hash_key) => {
                    ret.inner.insert(hash_key, vec![k]);
                    ret.len += 1;
                }
            }
        }

        Ok(manage_native!(ret, rt))
    }

    fn locate(
        &self,
        element: &Rc<ManagedXValue<W, R, T>>,
        ns: &RuntimeScope<W, R, T>,
        rt: RTCell<W, R, T>,
    ) -> XResult<KeyLocation, W, R, T> {
        let hash_func = to_primitive!(self.hash_func, Function);
        let raw_hash = forward_err!(ns
            .eval_func_with_values(hash_func, vec![Ok(element.clone())], rt.clone(), false)?
            .unwrap_value());
        let hash_key = forward_err!(to_primitive!(raw_hash, Int)
            .to_u64()
            .ok_or(ManagedXError::new("hash is out of bounds", rt.clone())?));
        let Some(bucket) = self.inner.get(&hash_key) else { return Ok(Ok(KeyLocation::Vacant(hash_key))); };
        let eq_func = Some(to_primitive!(self.eq_func, Function));
        for (i, k) in bucket.iter().enumerate() {
            if *to_primitive!(
                forward_err!(ns
                    .eval_func_with_values(
                        eq_func.unwrap(),
                        vec![Ok(element.clone()), Ok(k.clone())],
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

    pub(super) fn iter(&self) -> impl Iterator<Item = Rc<ManagedXValue<W, R, T>>> + '_ {
        self.inner.iter().flat_map(|(_, b)| b.iter()).cloned()
    }
}

impl<W: 'static, R: 'static, T: 'static> XNativeValue for XSet<W, R, T> {
    fn dyn_size(&self) -> usize {
        (self.len + self.inner.len() + 2) * size_of::<Rc<ManagedXValue<W, R, T>>>()
    }
}

pub(crate) fn add_set_type<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], _) = scope.generics_from_names(["T"]);
    scope.add_native_type("Set", XSetType::xtype(t))
}

pub(crate) fn add_set_new<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
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

pub(crate) fn add_set_update<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let st = XSetType::xtype(t.clone());

    scope.add_func(
        "update",
        XFuncSpec::new(&[&st, &XGeneratorType::xtype(t)], st.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let set = to_native!(a0, XSet<W, R, T>);
            let gen0 = to_native!(a1, XGenerator<W, R, T>);
            set.with_update(gen0.iter(ns, rt.clone()), ns, rt)
        }),
    )
}

pub(crate) fn add_set_add<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let st = XSetType::xtype(t.clone());

    scope.add_func(
        "add",
        XFuncSpec::new(&[&st, &t], st.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let mapping = to_native!(a0, XSet<W, R, T>);
            rt.can_allocate(mapping.len * 2 * size_of::<usize>())?;
            mapping.with_update(once(Ok(Ok(a1))), ns, rt)
        }),
    )
}

pub(crate) fn add_set_contains<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let st = XSetType::xtype(t.clone());

    scope.add_func(
        "contains",
        XFuncSpec::new(&[&st, &t], X_BOOL.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let set = to_native!(a0, XSet<W, R, T>);
            let found = matches!(
                xraise!(set.locate(&a1, ns, rt.clone())?),
                KeyLocation::Found(_)
            );
            Ok(ManagedXValue::new(XValue::Bool(found), rt)?.into())
        }),
    )
}

pub(crate) fn add_set_len<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let st = XSetType::xtype(t);

    scope.add_func(
        "len",
        XFuncSpec::new(&[&st], X_INT.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let set = to_native!(a0, XSet<W, R, T>);
            Ok(ManagedXValue::new(XValue::Int(set.len.into()), rt)?.into())
        }),
    )
}

pub(crate) fn add_set_to_generator<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let st = XSetType::xtype(t.clone());

    scope.add_func(
        "to_generator",
        XFuncSpec::new(&[&st], XGeneratorType::xtype(t)).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            Ok(manage_native!(XGenerator::FromSet(a0), rt))
        }),
    )
}

pub(crate) fn add_set_remove<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let st = XSetType::xtype(t.clone());

    scope.add_func(
        "remove",
        XFuncSpec::new(&[&st, &t], st.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let set = to_native!(a0, XSet<W, R, T>);
            if set.len == 0 {
                return xerr(ManagedXError::new("item not found", rt)?);
            }
            let KeyLocation::Found((hash_key, idx)) = xraise!(set.locate(&a1, ns, rt.clone())?) else {
                return xerr(ManagedXError::new("item not found", rt)?);
            };
            rt.can_allocate((set.len - 1)* size_of::<usize>())?;
            let mut new_dict = HashMap::from_iter(set.inner.iter().filter(|(k, _)| k != &&hash_key).map(|(k, b)| (*k, b.clone())));
            let old_bucket = &set.inner[&hash_key];
            new_dict.insert(hash_key, old_bucket.iter().take(idx).chain(old_bucket.iter().skip(idx + 1)).cloned().collect());
            Ok(manage_native!(
                XSet::new(set.hash_func.clone(), set.eq_func.clone(), new_dict, set.len-1),
                rt
            ))
        }),
    )
}

pub(crate) fn add_set_discard<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let st = XSetType::xtype(t.clone());

    scope.add_func(
        "discard",
        XFuncSpec::new(&[&st, &t], st.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);
            let set = to_native!(a0, XSet<W, R, T>);
            if set.len == 0 {
                return Ok(a0.clone().into());
            }
            let KeyLocation::Found((hash_key, idx)) = xraise!(set.locate(&a1, ns, rt.clone())?) else {
                return Ok(a0.clone().into());
            };
            rt.can_allocate((set.len - 1)* size_of::<usize>())?;
            let mut new_dict = HashMap::from_iter(set.inner.iter().filter(|(k, _)| k != &&hash_key).map(|(k, b)| (*k, b.clone())));
            let old_bucket = &set.inner[&hash_key];
            new_dict.insert(hash_key, old_bucket.iter().take(idx).chain(old_bucket.iter().skip(idx + 1)).cloned().collect());
            Ok(manage_native!(
                XSet::new(set.hash_func.clone(), set.eq_func.clone(), new_dict, set.len-1),
                rt
            ))
        }),
    )
}

pub(crate) fn add_set_clear<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let st = XSetType::xtype(t);

    scope.add_func(
        "clear",
        XFuncSpec::new(&[&st], st.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let set0 = to_native!(a0, XSet<W, R, T>);
            if set0.len == 0 {
                return Ok(a0.clone().into());
            }
            Ok(manage_native!(
                XSet::new(
                    set0.hash_func.clone(),
                    set0.eq_func.clone(),
                    Default::default(),
                    0,
                ),
                rt
            ))
        }),
    )
}

pub(crate) fn add_set_hash<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let ([t], params) = scope.generics_from_names(["T"]);
    let st = XSetType::xtype(t);

    scope.add_func(
        "hash",
        XFuncSpec::new(&[&st], X_INT.clone()).generic(params),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let set0 = to_native!(a0, XSet<W, R, T>);
            // note that since order is important to the default hasher, we'll just xor them together
            let mut ret = 0u64;
            for (hash, bucket) in set0.inner.iter() {
                let v = hash.wrapping_add(bucket.len() as u64);
                ret ^= v;
            }
            Ok(ManagedXValue::new(XValue::Int(LazyBigint::from(ret)), rt)?.into())
        }),
    )
}

pub(crate) fn add_set_dyn_new<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let eq_symbol = scope.identifier("eq");
    let hash_symbol = scope.identifier("hash");
    let cb_symbol = scope.identifier("set");

    scope.add_dyn_func("set", "default-funcs", move |_params, _types, ns, bind| {
        let [a0] = unpack_dyn_types(bind)?;

        let (inner_hash, hash_t) =
            get_func_with_type(ns, hash_symbol, &[a0.clone()], Some(&X_INT))?;
        let (inner_eq, eq_t) =
            get_func_with_type(ns, eq_symbol, &[a0.clone(), a0.clone()], Some(&X_BOOL))?;
        let (cb, cb_t) = get_func_with_type(ns, cb_symbol, &[hash_t.xtype(), eq_t.xtype()], None)?;

        Ok(XFunctionFactoryOutput::from_delayed_native(
            XFuncSpec::new(&[], cb_t.rtype()),
            delegate!(
                with [inner_hash, inner_eq, cb],
                args [],
                cb(inner_hash, inner_eq)
            ),
        ))
    })
}
