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
use crate::xtype::{XFuncSpec, X_BOOL, X_INT, X_UNKNOWN, XCompoundSpec, CompoundKind, Bind, XCompoundFieldSpec};
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

use super::core::{unpack_compounds, unpack_compound};
use super::sequence::XSequenceType;

pub(crate) fn add_matrix_type<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let t_symbol = scope.identifier("T");
    let ([t], params) = scope.generics_from_names(["T"]);
    let cspec = Arc::new(XCompoundSpec::new(
        scope.identifier("Matrix"),
        params,
        vec![
            XCompoundFieldSpec::new(scope.identifier("_cols"), X_INT.clone()), // 0
            XCompoundFieldSpec::new(scope.identifier("_data"), XSequenceType::xtype(t.clone())), // 1
        ],
    ));
    let bind = Bind::from([(t_symbol, t)]);
    let t = Arc::new(XType::Compound(CompoundKind::Struct, cspec, bind));
    scope.add_native_type("Matrix", t)
}

pub(crate) fn add_matrix_dyn_add<W, R, T>(
    scope: &mut RootCompilationScope<W, R, T>,
) -> Result<(), CompilationError> {
    let gen_symbol = scope.identifier("Matrix");

    let cb_symbol = scope.identifier("add");

    scope.add_dyn_func("add", "matrices", move |_params, types, ns, bind| {
        if bind.is_some() {
            return Err("this dyn func has no bind".to_string());
        }

        let [a0, a1] = unpack_dyn_types(types)?;
        let [t0] = unpack_compound(a0, gen_symbol)?;
        let [t1] = unpack_compound(a1, gen_symbol)?;


        let (inner_add, add_t) =
            get_func_with_type(ns, cb_symbol, &[t0.clone(), t1.clone()], None)?;
        let (cb, cb_t) = get_func_with_type(
            ns,
            cb_symbol,
            &[a0.clone(), a1.clone(), add_t.xtype()],
            None,
        )?;

        Ok(XFunctionFactoryOutput::from_delayed_native(
            XFuncSpec::new(&[a0, a1], cb_t.rtype()),
            delegate!(
                with [inner_add, cb],
                args [0->a0, 1->a1],
                cb(a0, a1, inner_add)
            ),
        ))
    })
}