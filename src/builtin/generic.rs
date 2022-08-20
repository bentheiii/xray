use std::rc;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use crate::{add_binop, add_ufunc, add_ufunc_ref, to_primitive, Bind, XCompilationScope, XStaticFunction, XType, eval, intern, CompilationError, meval, Identifier};
use crate::xtype::{X_BOOL, X_INT, X_FLOAT, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec};
use crate::xvalue::{XValue, ManagedXValue};
use rc::Rc;
use std::sync::Arc;
use mopa::Any;
use string_interner::StringInterner;
use crate::xexpr::XExpr;

pub fn add_if(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let t = XType::generic_from_name("T", interner);
    scope.add_func(
        interner.get_or_intern_static("if"), XStaticFunction::from_native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: X_BOOL.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: t.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: t.clone(),
                    required: true,
                },
            ],
            ret: t,
        }, |args, ns, tca, rt| {
            let (a0, ) = eval!(args, ns, rt, 0);
            args[match to_primitive!(a0, Bool) {
                true => 1,
                false => 2,
            }].eval(&ns, tca, rt)
        }))?;
    Ok(())
}

add_ufunc!(add_error, error, X_STRING, String, X_UNKNOWN, |a: &String| Err(a.clone()));

pub fn add_cast(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let t = XType::generic_from_name("T", interner);
    scope.add_func_intern(
        "cast", XStaticFunction::from_native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: t.clone(),
                    required: true,
                },
            ],
            ret: t,
        }, |args, ns, tca, rt| {
            args[0].eval(&ns, tca, rt)
        }), interner)?;
    Ok(())
}

pub fn add_debug(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError> {
    let t = XType::generic_from_name("T", interner);
    scope.add_func_intern(
        "debug", XStaticFunction::from_native(XFuncSpec {
            generic_params: Some(intern!(interner, "T")),
            params: vec![
                XFuncParamSpec {
                    type_: t.clone(),
                    required: true,
                },
                XFuncParamSpec {
                    type_: X_STRING.clone(),
                    required: false,
                },
            ],
            ret: t,
        }, |args, ns, tca, rt| {
            let (a0, ) = eval!(args, ns, rt, 0);
            let (a1,) = meval!(args, ns, rt, 1);
            let b = to_primitive!(a1, String, "".to_string());
            println!("{}{:?}", b, a0);
            Ok(a0.into())
        }), interner)?;
    Ok(())
}

pub fn add_ne(scope: &mut XCompilationScope, interner: &mut StringInterner) -> Result<(), CompilationError>{
    let eq_symbol = interner.get_or_intern_static("eq");

    fn static_from_eq(t0: Arc<XType>, t1: Arc<XType>, eq_expr: XExpr) -> Rc<XStaticFunction> {
        Rc::new(XStaticFunction::from_native(XFuncSpec {
            generic_params: None,
            params: vec![
                XFuncParamSpec {
                    type_: t0,
                    required: true,
                },
                XFuncParamSpec {
                    type_: t1,
                    required: true,
                },
            ],
            ret: X_BOOL.clone(),
        }, move |args, ns, _tca, rt| {
            let (a0, a1) = eval!(args, ns, rt, 0,1);
            let inner_equal_value = eq_expr.eval(ns, false, rt.clone())?.unwrap_value();
            let inner_eq_func = to_primitive!(inner_equal_value, Function);
            let eq = inner_eq_func.eval_values(&vec![a0, a1], &ns, rt.clone())?;
            let is_eq = to_primitive!(eq, Bool);
            return Ok(ManagedXValue::new(XValue::Bool(!*is_eq), rt)?.into());
        }))
    }

    fn from_types(types: &[Arc<XType>], scope: &XCompilationScope, eq_symbol: Identifier) -> Result<Rc<XStaticFunction>, String> {
        if types.len() != 2 {
            return Err(format!("Expected 2 types, got {}", types.len()));
        }
        let t0 = types[0].clone();
        let t1 = types[1].clone();

        let inner_eq = scope.resolve_overload(eq_symbol, types)?;  // todo ensure that the function returns a bool

        Ok(static_from_eq(t0, t1, inner_eq))
    }

    scope.add_dyn_func(interner.get_or_intern_static("ne"), move |_params, types, ns| {
        from_types(types, ns, eq_symbol)
    })
}