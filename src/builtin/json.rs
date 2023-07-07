use crate::builtin::core::{eval, xerr};
use crate::builtin::sequence::{XSequence, XSequenceType};
use crate::compilation_scope::CompilationItem;
use crate::runtime_scope::RuntimeScope;
use crate::xexpr::XExpr;
use crate::xtype::{
    Bind, CompoundKind, XCompoundFieldSpec, XCompoundSpec, XFuncSpec, XType, X_BOOL, X_FLOAT, X_STRING, X_UNKNOWN, XCallableSpec, X_INT,
};
use crate::xvalue::{ManagedXError, ManagedXValue, XFunctionFactoryOutput, XResult, XValue};
use crate::{
    forward_err, manage_native, to_native, to_primitive, xraise, CompilationError, XStaticFunction,
};

use std::rc::Rc;
use std::sync::Arc;

use crate::root_compilation_scope::RootCompilationScope;
use crate::runtime::RTCell;
use crate::util::fenced_string::FencedString;
use serde::ser::Serializer as _;
use serde_json::Serializer;

use super::core::{get_func_with_type, unpack_dyn_types, unpack_native};
use super::mapping::{XMappingType, XMapping};

pub(crate) fn add_json_type<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let cspec = Arc::new(XCompoundSpec::new(
        scope.identifier("JSON"),
        vec![],
        vec![
            XCompoundFieldSpec::new(scope.identifier("number"), X_FLOAT.clone()), // 0
            XCompoundFieldSpec::new(scope.identifier("string"), X_STRING.clone()), // 1
            XCompoundFieldSpec::new(scope.identifier("bool"), X_BOOL.clone()),    // 2
            XCompoundFieldSpec::new(scope.identifier("null"), Arc::new(XType::Tuple(vec![]))), // 3
            XCompoundFieldSpec::new(
                scope.identifier("array"),
                XSequenceType::xtype(Arc::new(XType::XTail(vec![]))),
            ), // 4
            XCompoundFieldSpec::new(
                scope.identifier("object"),
                XMappingType::xtype(X_STRING.clone(), Arc::new(XType::XTail(vec![]))),
            ), // 5
        ],
    ));
    let t = Arc::new(XType::Compound(CompoundKind::Union, cspec, Bind::default()));
    scope.add_native_type("JSON", t)
}

fn json_type<W, R, T>(
    scope: &RootCompilationScope<W, R, T>,
) -> Result<Arc<XType>, CompilationError> {
    if let Some(CompilationItem::Type(t)) =
        scope.scope.get_item(&scope.get_identifier("JSON").unwrap())
    {
        Ok(t)
    } else {
        unreachable!()
    }
}

fn value_to_json<W: 'static,R: 'static,T: 'static>(v: serde_json::Value, ns: &RuntimeScope<'_, W, R, T>, rt: RTCell<W, R, T>, str_hash: &Rc<ManagedXValue<W, R,T>>, str_eq: &Rc<ManagedXValue<W, R,T>>)->XResult<Rc<ManagedXValue<W,R,T>>, W,R,T>{
    let (tag, inner) = match v {
        serde_json::Value::Number(n) => (0, XValue::Float(n.as_f64().unwrap())),
        serde_json::Value::String(s) => (1, XValue::String(Box::new(FencedString::from_string(s)))),
        serde_json::Value::Bool(b) => (2, XValue::Bool(b)),
        serde_json::Value::Null => (3, XValue::StructInstance(vec![])),
        serde_json::Value::Array(a) => {
            let mut seq = Vec::with_capacity(a.len());
            for v in a {
                seq.push(forward_err!(value_to_json(v, ns, rt.clone(), str_hash, str_eq)?));
            }
            let seq = XSequence::array(seq);
            (4, XValue::Native(Box::new(seq)))
        }
        serde_json::Value::Object(ob) => {
            let mut fields = XMapping::new(str_hash.clone(), str_eq.clone(), Default::default(), 0);
            for (key, value) in ob {
                let key = ManagedXValue::new(XValue::String(Box::new(FencedString::from_string(key))), rt.clone())?;
                let value = forward_err!(value_to_json(value, ns, rt.clone(), str_hash, str_eq)?);
                forward_err!(fields.put(&key, || value, |_| unreachable!(), ns, rt.clone())?);
            }
            (5, XValue::Native(Box::new(fields)))
        }
    };
    let outer = ManagedXValue::new(inner, rt.clone())?;
    return Ok(Ok(ManagedXValue::new(XValue::UnionInstance((tag, outer)), rt)?));
}

pub(crate) fn add_json_priv_json_deserialzie<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let j_type = json_type(scope)?;
    scope.add_func(
        "__std_json_deserialize",
        XFuncSpec::new(
            &[&X_STRING, &Arc::new(XType::XCallable(XCallableSpec{
                param_types: vec![X_STRING.clone()],
                return_type: X_INT.clone(),
            })),
            &Arc::new(
                XType::XCallable(XCallableSpec{
                    param_types: vec![X_STRING.clone(), X_STRING.clone()],
                    return_type: X_BOOL.clone(),
                })
            )
            ],
            j_type
        ),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let a1 = xraise!(eval(&args[1], ns, &rt)?);  // hash
            let a2 = xraise!(eval(&args[2], ns, &rt)?);  // eq
            let string = to_primitive!(a0, String);
            let v = match serde_json::from_str::<serde_json::Value>(string.as_str()){
                Ok(v) => v,
                Err(e) => {return xerr(ManagedXError::new(format!("error deserializing json {e}"), rt)?)}
            };
            Ok(value_to_json(v, ns, rt, &a1, &a2)?.into())
        }),
    )
}

pub(crate) fn add_json_dyn_json_array<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let inner_symbol = scope.identifier("json");
    let j_type = json_type(scope)?;

    scope.add_dyn_func("json", "sequences", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let [a0] = unpack_dyn_types(types)?;
        let [t0] = unpack_native(a0, "Sequence")? else { unreachable!() };

        let (inner_f, _f_t) = get_func_with_type(ns, inner_symbol, &[t0.clone()], Some(&j_type))?;

        Ok(XFunctionFactoryOutput::from_delayed_native(
            XFuncSpec::new(&[a0], j_type.clone()),
            move |ns, rt|{
                let inner_f = forward_err!(ns.eval(&inner_f, rt, false)?.unwrap_value());
                Ok(Ok(
                    move |args: &[XExpr<W, R, T>], ns: &RuntimeScope<'_, W, R, T>, _tca, rt: RTCell<_, _, _>| {
                        let a0 = xraise!(eval(&args[0], ns, &rt)?);
                        let s0 = to_native!(a0, XSequence<W,R,T>);
                        let XValue::Function(inner_f) = &inner_f.clone().value else { unreachable!() };
                        let rt_copy = rt.clone();
                        let v = xraise!(s0.iter(ns, rt.clone()).map(move |item|->XResult<_,_,_,_> {
                            let item = item?;
                            Ok(ns.eval_func_with_values(inner_f, vec![item], rt_copy.clone(), false)?.unwrap_value())
                        }).collect::<XResult<Vec<_>,_,_,_>>()?);
                        let s = manage_native!(XSequence::array(v), rt.clone());

                        Ok(
                            ManagedXValue::new(XValue::UnionInstance((4, s)), rt)?.into()
                        )
                    }
                ))
            }
        ))
    })
}

pub(crate) fn add_json_priv_str_ser<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    scope.add_func(
        "__std_json_serialize_str",
        XFuncSpec::new(&[&X_STRING], X_STRING.clone()),
        XStaticFunction::from_native(|args, ns, _tca, rt| {
            let a0 = xraise!(eval(&args[0], ns, &rt)?);
            let string = to_primitive!(a0, String);

            let mut ret: Vec<u8> = Vec::new();
            let mut ser = Serializer::new(&mut ret);
            ser.serialize_str(string.as_str()).unwrap();
            
            // todo can we use from_utf8_unchecked here?
            let Ok(str) = String::from_utf8(ret) else {return xerr(ManagedXError::new("non-utf8 string", rt)?);};
            let ret = FencedString::from_string(str);
            Ok(ManagedXValue::new(XValue::String(Box::new(ret)), rt)?.into())
        }),
    )
}

pub(crate) fn add_json_dyn_json_object<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let inner_symbol = scope.identifier("json");
    let map_values_symbol = scope.identifier("map_values");

    let j_type = json_type(scope)?;

    scope.add_dyn_func("json", "objects", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let [a0] = unpack_dyn_types(types)?;
        let [t0, t1] = unpack_native(a0, "Mapping")? else { unreachable!() };

        if t0.as_ref() != X_STRING.as_ref() && t0.as_ref() != X_UNKNOWN.as_ref() {
            return Err("json expects a mapping with string keys".to_string());
        }

        // todo test with empty mapping
        let (inner_f, f_t) = get_func_with_type(ns, inner_symbol, &[t1.clone()], None)?;
        let (map_values_f, _map_values_t) = get_func_with_type(ns, map_values_symbol, &[a0.clone(), f_t.xtype()], Some(&XMappingType::xtype(X_STRING.clone(), j_type.clone())))?;

        Ok(XFunctionFactoryOutput::from_delayed_native(
            XFuncSpec::new(&[a0], j_type.clone()),
            move |ns, rt|{
                let inner_f = forward_err!(ns.eval(&inner_f, rt.clone(), false)?.unwrap_value());
                let map_values_f = forward_err!(ns.eval(&map_values_f, rt, false)?.unwrap_value());

                Ok(Ok(
                    move |args: &[XExpr<W, R, T>], ns: &RuntimeScope<'_, W, R, T>, _tca, rt: RTCell<_, _, _>| {
                        let a0 = xraise!(eval(&args[0], ns, &rt)?);
                        let XValue::Function(map_values_f) = &map_values_f.clone().value else { unreachable!() };

                        let m = xraise!(ns.eval_func_with_values(map_values_f, vec![Ok(a0), Ok(inner_f.clone())], rt.clone(), false)?.unwrap_value());

                        Ok(
                            ManagedXValue::new(XValue::UnionInstance((5, m)), rt)?.into()
                        )
                    }
                ))
            }
        ))
    })
}
