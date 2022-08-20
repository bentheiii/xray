use crate::builtin::optional::{XOptional, XOptionalType};
use crate::builtin::sequence::{XSequence, XSequenceType};
use crate::compilation_scope::{XCompilationScopeItem, XFunctionFactory};
use crate::evaluation_scope::XEvaluationScope;
use crate::runtime::{RTCell};
use crate::xtype::{
    common_type, Bind, CompoundKind, XCompoundSpec, XFuncParamSpec, XType, X_BOOL, X_FLOAT, X_INT,
    X_STRING,
};
use crate::xvalue::{ManagedXValue, NativeCallable, XFunction, XValue};
use crate::{
    manage_native, CompilationError, Declaration, Identifier,
    XCompilationScope, XFuncSpec,
};
use num::BigInt;
use std::collections::{HashSet};
use std::fmt::{Debug, Error, Formatter};
use std::rc::Rc;
use std::sync::Arc;
use string_interner::{DefaultSymbol, StringInterner};

#[derive(Debug, Clone)]
pub enum XStaticExpr {
    LiteralBool(bool),
    LiteralInt(i64),
    LiteralFloat(f64),
    LiteralString(String),
    Array(Vec<XStaticExpr>),
    Tuple(Vec<XStaticExpr>),
    Call(Box<XStaticExpr>, Vec<XStaticExpr>),
    Member(Box<XStaticExpr>, String),
    Ident(DefaultSymbol),
    SpecializedIdent(DefaultSymbol, Vec<Arc<XType>>),
    Lambda(Vec<XExplicitArgSpec>, Box<XStaticExpr>),
}

pub struct CompilationResult {
    pub expr: XExpr,
    pub closure_vars: Vec<DefaultSymbol>,
}

impl From<XExpr> for CompilationResult {
    fn from(expr: XExpr) -> Self {
        CompilationResult {
            expr,
            closure_vars: vec![],
        }
    }
}

impl CompilationResult {
    pub fn new(expr: XExpr, closure_vars: Vec<DefaultSymbol>) -> Self {
        CompilationResult {
            expr,
            closure_vars,
        }
    }
    fn join(results: Vec<Self>) -> (Vec<XExpr>, Vec<DefaultSymbol>) {
        let mut exprs = vec![];
        let mut closure_vars = vec![];
        for result in results {
            exprs.push(result.expr);
            closure_vars.extend(result.closure_vars.iter().map(|s| s.clone()));
        }
        (exprs, closure_vars)
    }
    fn from_multi(
        other: (Vec<XExpr>, Vec<DefaultSymbol>),
        f: impl FnOnce(Vec<XExpr>) -> XExpr,
    ) -> Self {
        Self::new(f(other.0), other.1)
    }
}

pub fn resolve_overload<'p>(
    overloads: Vec<Rc<XFunctionFactory>>,
    args: Option<&[XExpr]>,
    arg_types: &[Arc<XType>],
    name: DefaultSymbol,
    namespace: &'p XCompilationScope<'p>,
) -> Result<XExpr, CompilationError> {
    let mut exact_matches = vec![];
    let mut generic_matches = vec![];
    let mut dynamic_failures = vec![];
    let is_unknown = arg_types.iter().any(|t| t.is_unknown());
    // if the bindings are unknown, then we prefer generic solutions over exact solutions
    for overload in overloads {
        let overload = match overload.as_ref() {
            XFunctionFactory::Static(overload) => overload.clone(),
            XFunctionFactory::Dynamic(dyn_func) => match dyn_func(args, arg_types, namespace) {
                Ok(overload) => overload,
                Err(err) => {
                    dynamic_failures.push(err);
                    continue;
                }
            },
        };
        let b = overload.bind(arg_types);
        if let Some(bind) = b {
            let item = XExpr::KnownOverload(overload.clone(), bind);
            if overload.short_circut_overloads() {
                return Ok(item);
            }
            if overload.is_generic() ^ is_unknown {
                &mut generic_matches
            } else {
                &mut exact_matches
            }
            .push(item);
        }
    }
    if exact_matches.len() == 1 {
        return Ok(exact_matches.swap_remove(0));
    }
    if exact_matches.len() > 1 {
        return Err(CompilationError::AmbiguousOverload {
            name,
            is_generic: false,
            items: exact_matches,
            param_types: arg_types.iter().cloned().collect(),
        });
    }
    if generic_matches.len() == 1 {
        return Ok(generic_matches.swap_remove(0));
    }
    if generic_matches.len() > 1 {
        return Err(CompilationError::AmbiguousOverload {
            name,
            is_generic: true,
            items: generic_matches,
            param_types: arg_types.iter().cloned().collect(),
        });
    }
    Err(CompilationError::NoOverload {
        name,
        param_types: arg_types.iter().cloned().collect(),
        dynamic_failures,
    })
}

impl XStaticExpr {
    pub fn new_call(
        name: &'static str,
        args: Vec<XStaticExpr>,
        interner: &mut StringInterner,
    ) -> XStaticExpr {
        XStaticExpr::Call(
            Box::new(XStaticExpr::Ident(interner.get_or_intern_static(name))),
            args,
        )
    }

    pub fn new_call_sym(name: Identifier, args: Vec<XStaticExpr>) -> XStaticExpr {
        XStaticExpr::Call(Box::new(XStaticExpr::Ident(name)), args)
    }

    pub fn compile<'p>(
        &self,
        namespace: &'p XCompilationScope<'p>,
    ) -> Result<CompilationResult, CompilationError> {
        fn compile_many<'p>(
            exprs: &Vec<XStaticExpr>,
            namespace: &'p XCompilationScope<'p>,
        ) -> Result<(Vec<XExpr>, Vec<DefaultSymbol>), CompilationError> {
            let mut ret = vec![];
            for item in exprs {
                let item = item.compile(namespace)?;
                ret.push(item);
            }
            Ok(CompilationResult::join(ret))
        }

        match self {
            XStaticExpr::LiteralBool(v) => Ok(XExpr::LiteralBool(*v).into()),
            XStaticExpr::LiteralInt(v) => Ok(XExpr::LiteralInt(*v).into()),
            XStaticExpr::LiteralFloat(v) => Ok(XExpr::LiteralFloat(v.clone()).into()),
            XStaticExpr::LiteralString(v) => Ok(XExpr::LiteralString(v.clone()).into()),
            XStaticExpr::Array(items) => Ok(CompilationResult::from_multi(
                compile_many(items, namespace)?,
                XExpr::Array,
            )),
            XStaticExpr::Call(func, args) => {
                let (compiled_args, mut cvars) = compile_many(args, namespace)?;
                match func.as_ref() {
                    XStaticExpr::Member(obj, member_name) => {
                        //special case: member access can be a variant constructor
                        if let XStaticExpr::Ident(name) = obj.as_ref() {
                            if let Some(XCompilationScopeItem::Compound(
                                CompoundKind::Union,
                                spec,
                            )) = namespace.get(*name)
                            {
                                return if let Some(&index) = spec.indices.get(member_name) {
                                    if compiled_args.len() != 1 {
                                        return Err(CompilationError::VariantConstructorOneArg);
                                    }
                                    let com_type = Arc::new(XType::Compound(
                                        CompoundKind::Union,
                                        spec.clone(),
                                        Bind::new(),
                                    ));
                                    let var_type = spec.fields[index]
                                        .type_
                                        .resolve_bind(&Bind::new(), Some(&com_type));
                                    if let Some(bind) =
                                        var_type.bind_in_assignment(&compiled_args[0].xtype()?)
                                    {
                                        return Ok(CompilationResult::new(
                                            XExpr::Variant(
                                                spec,
                                                bind,
                                                index,
                                                Box::new(compiled_args[0].clone()),
                                            ),
                                            cvars,
                                        ));
                                    } else {
                                        Err(CompilationError::VariantConstructorTypeArgMismatch {
                                            union_name: spec.name.clone(),
                                            variant_name: member_name.clone(),
                                            expected_type: spec.fields[index].type_.clone(),
                                            actual_type: compiled_args[0].xtype()?,
                                        })
                                    }
                                } else {
                                    Err(CompilationError::MemberNotFound {
                                        spec,
                                        name: member_name.clone(),
                                    })
                                };
                            }
                        }
                    }
                    XStaticExpr::Ident(name) => {
                        match namespace.get(*name) {
                            Some(XCompilationScopeItem::Overload(overloads)) => {
                                let arg_types = compiled_args
                                    .iter()
                                    .map(|x| x.xtype())
                                    .collect::<Result<Vec<_>, _>>()?;
                                return Ok(CompilationResult::new(
                                    XExpr::Call(
                                        Box::new(resolve_overload(
                                            overloads,
                                            Some(&compiled_args),
                                            &arg_types,
                                            *name,
                                            namespace,
                                        )?),
                                        compiled_args,
                                    ),
                                    cvars,
                                ));
                            }
                            Some(XCompilationScopeItem::Compound(CompoundKind::Struct, spec)) => {
                                // todo support direct specialization?
                                let arg_types = compiled_args
                                    .iter()
                                    .map(|x| x.xtype())
                                    .collect::<Result<Vec<_>, _>>()?;
                                if arg_types.len() != spec.fields.len() {
                                    return Err(CompilationError::StructParamsLengthMismatch {
                                        struct_name: spec.name.clone(),
                                        expected_count: spec.fields.len(),
                                        actual_count: arg_types.len(),
                                    });
                                }
                                return if let Some(bind) = spec.bind(&arg_types) {
                                    Ok(CompilationResult::new(
                                        XExpr::Construct(spec.clone(), bind, compiled_args),
                                        cvars,
                                    ))
                                } else {
                                    Err(CompilationError::StructFieldTypeMismatch {
                                        struct_name: spec.name.clone(),
                                        expected_types: spec
                                            .fields
                                            .iter()
                                            .map(|x| x.type_.clone())
                                            .collect(),
                                        actual_types: arg_types,
                                    })
                                };
                            }
                            _ => {}
                        }
                    }
                    XStaticExpr::SpecializedIdent(name, arg_types) => {
                        let actual_arg_types = compiled_args
                            .iter()
                            .map(|x| x.xtype())
                            .collect::<Result<Vec<_>, _>>()?;
                        let mut bind = Bind::new();
                        for (idx, (arg_type, actual_type)) in
                            arg_types.iter().zip(actual_arg_types.iter()).enumerate()
                        {
                            bind = arg_type
                                .bind_in_assignment(actual_type)
                                .and_then(|b| bind.mix(&b))
                                .ok_or_else(|| {
                                    CompilationError::SpecializedFunctionTypeMismatch {
                                        name: name.clone(),
                                        idx,
                                        actual_type: actual_type.clone(),
                                        expected_type: arg_type.clone(),
                                    }
                                })?;
                        }
                        return match namespace.get(*name) {
                            Some(XCompilationScopeItem::Overload(overloads)) => {
                                Ok(CompilationResult::new(
                                    XExpr::Call(
                                        Box::new(resolve_overload(
                                            overloads,
                                            Some(&compiled_args),
                                            arg_types,
                                            *name,
                                            namespace,
                                        )?),
                                        compiled_args,
                                    ),
                                    cvars,
                                ))
                            }
                            Some(item) => Err(CompilationError::NonFunctionSpecialization {
                                name: name.clone(),
                                item,
                            }),
                            None => Err(CompilationError::FunctionNotFound { name: name.clone() }),
                        };
                    }
                    _ => {}
                }
                let CompilationResult {
                    expr: func_compiled,
                    closure_vars: func_closure_vars,
                } = func.compile(namespace)?;
                cvars.extend(func_closure_vars.into_iter());
                Ok(CompilationResult::new(
                    XExpr::Call(Box::new(func_compiled), compiled_args),
                    cvars,
                ))
            }
            XStaticExpr::Member(obj, member_name) => {
                let obj_compiled = obj.compile(namespace)?;
                let obj_type = obj_compiled.expr.xtype()?;
                match obj_type.as_ref() {
                    XType::Compound(_, spec, _) => {
                        if let Some(&index) = spec.indices.get(member_name) {
                            Ok(CompilationResult::new(
                                XExpr::Member(Box::new(obj_compiled.expr), index),
                                obj_compiled.closure_vars,
                            ))
                        } else {
                            Err(CompilationError::MemberNotFound {
                                spec: spec.clone(),
                                name: member_name.clone(),
                            })
                        }
                    }
                    XType::Tuple(types) => {
                        let idx = member_name
                            .strip_prefix("item")
                            .map(|s| s.parse::<usize>().unwrap())
                            .ok_or_else(|| CompilationError::NonItemTupleAccess {
                                member: member_name.clone(),
                            })?;
                        if idx > types.len() {
                            Err(CompilationError::TupleIndexOutOfBounds {
                                tuple_type: obj_type.clone(),
                                index: idx,
                                max: types.len(),
                            })
                        } else {
                            Ok(CompilationResult::new(
                                XExpr::Member(Box::new(obj_compiled.expr), idx),
                                obj_compiled.closure_vars,
                            ))
                        }
                    }
                    _ => Err(CompilationError::NonCompoundMemberAccess {
                        xtype: obj_compiled.expr.xtype()?,
                    }),
                }
            }
            XStaticExpr::Ident(name) => match namespace.get_with_depth(*name) {
                None => Err(CompilationError::ValueNotFound { name: name.clone() }),
                Some((
                    XCompilationScopeItem::Compound(..) | XCompilationScopeItem::NativeType(..),
                    _,
                )) => Err(CompilationError::TypeAsVariable { name: name.clone() }),
                Some((item, depth)) => {
                    let cvars = if depth != 0 && depth != namespace.height {
                        vec![name.clone()]
                    } else {
                        vec![]
                    };
                    match &item {
                        XCompilationScopeItem::Value(t) => Ok(CompilationResult::new(
                            XExpr::Ident(*name, Box::new(IdentItem::Value(t.clone()))),
                            cvars,
                        )),
                        XCompilationScopeItem::Overload(overloads) => {
                            if overloads.len() == 1 {
                                let overload = &overloads[0];
                                match overload.as_ref() {
                                    XFunctionFactory::Static(overload) => {
                                        if overload.is_generic() {
                                            Err(CompilationError::GenericFunctionAsVariable {
                                                name: name.clone(),
                                            })
                                        } else {
                                            Ok(CompilationResult::new(
                                                XExpr::Ident(
                                                    *name,
                                                    Box::new(IdentItem::Function(overload.clone())),
                                                ),
                                                cvars,
                                            ))
                                        }
                                    }
                                    XFunctionFactory::Dynamic(_) => {
                                        Err(CompilationError::DynamicFunctionAsVariable {
                                            name: name.clone(),
                                        })
                                    }
                                }
                            } else {
                                Err(CompilationError::OverloadedFunctionAsVariable {
                                    name: name.clone(),
                                })
                            }
                        }
                        _ => unreachable!(),
                    }
                }
            },
            XStaticExpr::SpecializedIdent(name, arg_types) => {
                match namespace.get_with_depth(*name) {
                    Some((XCompilationScopeItem::Overload(overloads), depth)) => {
                        let cvars = if depth != 0 && depth != namespace.height {
                            vec![name.clone()]
                        } else {
                            vec![]
                        };
                        Ok(CompilationResult::new(
                            resolve_overload(overloads, None, arg_types, *name, namespace)?,
                            cvars,
                        ))
                    }
                    None => Err(CompilationError::FunctionNotFound { name: name.clone() }),
                    Some((item, _)) => Err(CompilationError::NonFunctionSpecialization {
                        name: name.clone(),
                        item,
                    }),
                }
            }
            XStaticExpr::Lambda(args, body) => {
                let mut subscope = XCompilationScope::from_parent_lambda(namespace);
                for param in args {
                    subscope.add_param(param.name, param.type_.clone())?;
                }
                let body_result = body.compile(&subscope)?;
                let body_type = body_result.expr.xtype()?;
                return Ok(CompilationResult::from(XExpr::Lambda(Rc::new(
                    XStaticFunction::UserFunction(UfData::new(
                        XExplicitFuncSpec {
                            generic_params: None,
                            args: args.clone(),
                            ret: body_type,
                        },
                        vec![],
                        Box::new(body_result.expr),
                        body_result.closure_vars.iter().cloned().collect(),
                    )),
                ))));
            }
            XStaticExpr::Tuple(items) => Ok(CompilationResult::from_multi(
                compile_many(items, namespace)?,
                XExpr::Tuple,
            )),
        }
    }
}

#[derive(Debug, Clone)]
pub enum XExpr {
    LiteralBool(bool),
    LiteralInt(i64),
    LiteralFloat(f64),
    LiteralString(String),
    Array(Vec<XExpr>),
    Tuple(Vec<XExpr>),
    Call(Box<XExpr>, Vec<XExpr>),
    Construct(Arc<XCompoundSpec>, Bind, Vec<XExpr>),
    Variant(Arc<XCompoundSpec>, Bind, usize, Box<XExpr>),
    Member(Box<XExpr>, usize),
    KnownOverload(Rc<XStaticFunction>, Bind),
    Ident(DefaultSymbol, Box<IdentItem>),
    Lambda(Rc<XStaticFunction>),
    // this dummy exists for calling native functions with arguments that were already
    // evaluated
    Dummy(Rc<ManagedXValue>),
}

#[derive(Clone)] // todo why clone?
pub enum XStaticFunction {
    // identical to a native, with the exception that will short-circut overload resolution
    ShortCircutNative(XFuncSpec, NativeCallable),
    Native(XFuncSpec, NativeCallable),
    UserFunction(UfData),
    Recourse(Rc<XFuncSpec>, usize),
}

#[derive(Clone, Debug)] // todo better debug
pub struct UfData {
    pub spec: XExplicitFuncSpec,
    pub output: Box<XExpr>,
    pub cvars: HashSet<DefaultSymbol>,

    pub param_names: Vec<DefaultSymbol>,
    pub defaults: Vec<Rc<ManagedXValue>>,
    pub variable_declarations: Vec<(DefaultSymbol, XExpr)>,
}

impl UfData {
    pub fn new(
        spec: XExplicitFuncSpec,
        declarations: Vec<Declaration>,
        output: Box<XExpr>,
        cvars: HashSet<DefaultSymbol>,
    ) -> UfData {
        UfData {
            param_names: (&spec.args).iter().map(|p| p.name).collect(),
            defaults: (&spec.args)
                .iter()
                .skip_while(|p| p.default.is_none())
                .map(|p| p.default.clone().unwrap())
                .collect(),
            spec,
            output,
            cvars,
            variable_declarations: declarations
                .into_iter()
                .filter_map(|decl| {
                    if let Declaration::Value(name, expr) = decl {
                        Some((name.clone(), expr.clone()))
                    } else {
                        None
                    }
                })
                .collect(),
        }
    }
}

impl XStaticFunction {
    pub fn to_function(self: Rc<Self>, closure: &XEvaluationScope<'_>) -> XFunction {
        match self.as_ref() {
            XStaticFunction::Native(_, native) | XStaticFunction::ShortCircutNative(_, native) => {
                XFunction::Native(native.clone())
            }
            XStaticFunction::UserFunction(uf) => {
                let closure = uf
                    .cvars
                    .iter()
                    .map(|&name| (name, closure.get(name).unwrap().clone()))
                    .collect();
                XFunction::UserFunction(self.clone(), closure)
            }
            XStaticFunction::Recourse(_, depth) => XFunction::Recourse(*depth),
        }
    }

    pub fn from_native(
        spec: XFuncSpec,
        native: impl Fn(&[XExpr], &XEvaluationScope<'_>, bool, RTCell) -> Result<TailedEvalResult, String>
            + 'static,
    ) -> XStaticFunction {
        XStaticFunction::Native(spec, Rc::new(native))
    }

    pub fn from_native_short_circut(
        spec: XFuncSpec,
        native: impl Fn(&[XExpr], &XEvaluationScope<'_>, bool, RTCell) -> Result<TailedEvalResult, String>
            + 'static,
    ) -> XStaticFunction {
        XStaticFunction::ShortCircutNative(spec, Rc::new(native))
    }

    pub fn short_circut_overloads(&self) -> bool {
        if let Self::ShortCircutNative(..) = self {
            true
        } else {
            false
        }
    }
}

impl Debug for XStaticFunction {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            XStaticFunction::Native(spec, _) => {
                write!(f, "Native({:?})", spec)
            }
            XStaticFunction::ShortCircutNative(spec, _) => {
                write!(f, "Native({:?})", spec)
            }
            XStaticFunction::UserFunction(spec, ..) => {
                write!(f, "UserFunction({:?})", spec)
            }
            XStaticFunction::Recourse(spec, ..) => {
                write!(f, "Recourse({:?})", spec)
            }
        }
    }
}

impl PartialEq for XStaticFunction {
    fn eq(&self, _: &Self) -> bool {
        return false;
    }
}

impl Eq for XStaticFunction {}

#[derive(Debug, Clone)]
pub struct XExplicitFuncSpec {
    pub generic_params: Option<Vec<DefaultSymbol>>,
    pub args: Vec<XExplicitArgSpec>,
    pub ret: Arc<XType>,
}

#[derive(Debug, Clone)]
pub struct XExplicitArgSpec {
    pub name: DefaultSymbol,
    pub type_: Arc<XType>,
    pub default: Option<Rc<ManagedXValue>>,
}

impl XExplicitFuncSpec {
    pub fn to_spec(&self) -> XFuncSpec {
        XFuncSpec {
            generic_params: self.generic_params.clone(),
            params: self
                .args
                .iter()
                .map(|x| XFuncParamSpec {
                    type_: x.type_.clone(),
                    required: x.default.is_none(),
                })
                .collect(),
            ret: self.ret.clone(),
        }
    }
}

impl XStaticFunction {
    pub fn bind(&self, args: &[Arc<XType>]) -> Option<Bind> {
        match self {
            XStaticFunction::Native(spec, _) | XStaticFunction::ShortCircutNative(spec, _) => {
                spec.bind(args)
            }
            XStaticFunction::Recourse(spec, ..) => spec.bind(args),
            XStaticFunction::UserFunction(ud, ..) => ud.spec.to_spec().bind(args),
        }
    }
    pub fn rtype(&self, bind: &Bind) -> Arc<XType> {
        match self {
            XStaticFunction::Native(spec, _) | XStaticFunction::ShortCircutNative(spec, _) => {
                spec.rtype(bind)
            }
            XStaticFunction::Recourse(spec, ..) => spec.rtype(bind),
            XStaticFunction::UserFunction(ud, ..) => ud.spec.to_spec().rtype(bind),
        }
    }
    pub fn is_generic(&self) -> bool {
        match self {
            XStaticFunction::Native(spec, _) | XStaticFunction::ShortCircutNative(spec, _) => {
                spec.generic_params.is_some()
            }
            XStaticFunction::Recourse(spec, ..) => spec.generic_params.is_some(),
            XStaticFunction::UserFunction(ud, ..) => ud.spec.generic_params.is_some(),
        }
    }
    pub fn xtype(&self, bind: &Bind) -> Arc<XType> {
        match self {
            XStaticFunction::Native(spec, _) | XStaticFunction::ShortCircutNative(spec, _) => {
                spec.xtype(bind)
            }
            XStaticFunction::Recourse(spec, ..) => spec.xtype(bind),
            XStaticFunction::UserFunction(ud, ..) => ud.spec.to_spec().xtype(bind),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum IdentItem {
    Value(Arc<XType>),
    Function(Rc<XStaticFunction>),
}

#[derive(Debug)]
pub enum TailedEvalResult {
    Value(Rc<ManagedXValue>),
    TailCall(Vec<Rc<ManagedXValue>>),
}

impl TailedEvalResult {
    pub fn unwrap_value(self) -> Rc<ManagedXValue> {
        match self {
            TailedEvalResult::Value(v) => v,
            TailedEvalResult::TailCall(_) => {
                panic!("TailedEvalResult::unwrap_value called on a tail call")
            }
        }
    }
}

impl From<Rc<ManagedXValue>> for TailedEvalResult {
    fn from(v: Rc<ManagedXValue>) -> Self {
        TailedEvalResult::Value(v)
    }
}

impl XExpr {
    pub fn xtype(&self) -> Result<Arc<XType>, CompilationError> {
        match self {
            XExpr::LiteralBool(_) => Ok(X_BOOL.clone()),
            XExpr::LiteralInt(_) => Ok(X_INT.clone()),
            XExpr::LiteralFloat(_) => Ok(X_FLOAT.clone()),
            XExpr::LiteralString(_) => Ok(X_STRING.clone()),
            XExpr::Array(exprs) => {
                let element_type = common_type(exprs.iter().map(|x| x.xtype()))?;
                Ok(XType::XNative(Box::new(XSequenceType {}), vec![element_type]).into())
            }
            XExpr::Call(func, _) => {
                if let XExpr::KnownOverload(func, bind) = func.as_ref() {
                    return Ok(func.rtype(bind));
                }
                let func_type = func.xtype()?;
                if let XType::XCallable(spec) = func_type.as_ref() {
                    return Ok(spec.return_type.clone());
                }
                if let XType::XFunc(func) = func_type.as_ref() {
                    return Ok(func.rtype(&Bind::new()));
                }
                Err(CompilationError::NotAFunction {
                    type_: func_type.clone(),
                })
            }
            XExpr::Construct(spec, binding, ..) => Ok(Arc::new(XType::Compound(
                CompoundKind::Struct,
                spec.clone(),
                binding.clone(),
            ))),
            XExpr::Variant(spec, binding, ..) => Ok(Arc::new(XType::Compound(
                CompoundKind::Union,
                spec.clone(),
                binding.clone(),
            ))),
            XExpr::Member(obj, idx) => {
                let obj_type = obj.xtype()?;
                match obj_type.as_ref() {
                    XType::Compound(CompoundKind::Struct, spec, bind) => Ok(spec.fields[*idx]
                        .type_
                        .clone()
                        .resolve_bind(&bind, Some(&obj_type))),
                    XType::Compound(CompoundKind::Union, spec, bind) => {
                        let t = spec.fields[*idx]
                            .type_
                            .clone()
                            .resolve_bind(&bind, Some(&obj_type));
                        Ok(XOptionalType::xtype(t))
                    }
                    XType::Tuple(fields) => Ok(fields[*idx].clone()),
                    _ => Err(CompilationError::NotACompound {
                        type_: obj_type.clone(),
                    }),
                }
            }
            XExpr::KnownOverload(func, bind) => Ok(func.xtype(bind)),
            XExpr::Lambda(func) => Ok(func.xtype(&Bind::new())),
            XExpr::Ident(_, item) => match item.as_ref() {
                IdentItem::Value(xtype) => Ok(xtype.clone()),
                IdentItem::Function(func) => Ok(func.xtype(&Bind::new())),
            },
            XExpr::Tuple(items) => {
                let types = items
                    .iter()
                    .map(|x| x.xtype())
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Arc::new(XType::Tuple(types)))
            }
            XExpr::Dummy(_) => unreachable!(),
        }
    }

    pub fn eval<'p>(
        &self,
        namespace: &XEvaluationScope<'p>,
        tail_available: bool,
        runtime: RTCell,
    ) -> Result<TailedEvalResult, String> {
        match &self {
            XExpr::LiteralBool(b) => Ok(ManagedXValue::new(XValue::Bool(*b), runtime)?.into()),
            XExpr::LiteralInt(i) => {
                Ok(ManagedXValue::new(XValue::Int(BigInt::from(*i)), runtime)?.into())
            }
            XExpr::LiteralFloat(r) => Ok(ManagedXValue::new(XValue::Float(*r), runtime)?.into()),
            XExpr::LiteralString(s) => {
                Ok(ManagedXValue::new(XValue::String(s.clone()), runtime)?.into())
            }
            XExpr::Array(exprs) => {
                let seq = if exprs.is_empty() {
                    XSequence::Empty
                } else {
                    XSequence::array(
                        exprs
                            .iter()
                            .map(|x| {
                                x.eval(namespace, false, runtime.clone())
                                    .map(|r| r.unwrap_value())
                            })
                            .collect::<Result<Vec<_>, _>>()?,
                    )
                };
                Ok(ManagedXValue::new(XValue::Native(Box::new(seq)), runtime)?.into())
            }
            XExpr::Call(func, args) => {
                let callable = func
                    .eval(namespace, false, runtime.clone())?
                    .unwrap_value()
                    .clone();
                let ret;
                if let XValue::Function(xfunc) = &callable.value {
                    ret = xfunc.eval(args, namespace, tail_available, runtime)?;
                } else {
                    return Err(format!("Expected function, got {:?}", callable));
                }
                return Ok(ret);
            }
            XExpr::Construct(_, _, args) | XExpr::Tuple(args) => {
                let items = args
                    .iter()
                    .map(|x| {
                        x.eval(namespace, false, runtime.clone())
                            .map(|r| r.unwrap_value())
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(ManagedXValue::new(XValue::StructInstance(items), runtime)?.into())
            }
            XExpr::Member(obj, idx) => {
                let obj = obj.eval(namespace, false, runtime.clone())?.unwrap_value();
                match &obj.value {
                    XValue::StructInstance(items) => Ok(items[*idx].clone().into()),
                    XValue::UnionInstance(variant, item) => Ok(if variant == idx {
                        manage_native!(
                            XOptional {
                                value: Some(item.clone())
                            },
                            runtime.clone()
                        )
                    } else {
                        manage_native!(XOptional { value: None }, runtime.clone())
                    }),
                    _ => Err(format!("Expected struct, got {:?}", obj)),
                }
            }
            XExpr::Variant(.., idx, expr) => {
                let obj = expr.eval(namespace, false, runtime.clone())?.unwrap_value();
                Ok(ManagedXValue::new(XValue::UnionInstance(*idx, obj), runtime)?.into())
            }
            XExpr::KnownOverload(func, ..) | XExpr::Lambda(func) => Ok(ManagedXValue::new(
                XValue::Function(func.clone().to_function(namespace)),
                runtime,
            )?
            .into()),
            XExpr::Ident(name, item) => {
                if let IdentItem::Function(func) = item.as_ref() {
                    Ok(ManagedXValue::new(
                        XValue::Function(func.clone().to_function(namespace)),
                        runtime,
                    )?
                    .into())
                } else {
                    Ok(namespace
                        .get(*name)
                        .ok_or_else(|| {
                            format!("Undefined identifier during evaluation: {:?}", name)
                        })?
                        .clone()
                        .into())
                }
            }
            XExpr::Dummy(val) => Ok(val.clone().into()),
        }
    }
}
