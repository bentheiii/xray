use crate::builtin::optional::{XOptional, XOptionalType};
use crate::builtin::sequence::{XSequence, XSequenceType};
use crate::compilation_scope::{XCompilationScopeItem, XFunctionFactory};
use crate::evaluation_scope::XEvaluationScope;
use crate::runtime::RTCell;
use crate::xtype::{
    common_type, Bind, CompoundKind, XCompoundSpec, XFuncParamSpec, XType, X_BOOL, X_FLOAT, X_INT,
    X_STRING,
};
use crate::xvalue::{ManagedXValue, NativeCallable, XFunction, XValue};
use crate::{
    manage_native, CompilationError, Declaration, Identifier, XCompilationScope, XFuncSpec,
};

use crate::util::lazy_bigint::LazyBigint;
use crate::util::rc_hash::RcHash;
use std::collections::HashSet;
use std::fmt::{Debug, Error, Formatter};
use std::io::Write;
use std::rc::Rc;
use std::sync::Arc;
use string_interner::{DefaultSymbol, StringInterner};
use derivative::Derivative;

#[derive(Debug)]
pub(crate) enum XStaticExpr<W: Write + Debug + 'static> {
    LiteralBool(bool),
    LiteralInt(i64),
    LiteralFloat(f64),
    LiteralString(String),
    Array(Vec<XStaticExpr<W>>),
    Tuple(Vec<XStaticExpr<W>>),
    Call(Box<XStaticExpr<W>>, Vec<XStaticExpr<W>>),
    Member(Box<XStaticExpr<W>>, String),
    Ident(DefaultSymbol),
    SpecializedIdent(DefaultSymbol, Vec<Arc<XType>>),
    Lambda(Vec<XExplicitArgSpec<W>>, Box<XStaticExpr<W>>),
}

pub(crate) struct CompilationResult<W: Write + Debug + 'static> {
    pub(crate) expr: XExpr<W>,
    pub(crate) closure_vars: Vec<DefaultSymbol>,
}

impl<W: Write + Debug + 'static> From<XExpr<W>> for CompilationResult<W> {
    fn from(expr: XExpr<W>) -> Self {
        Self {
            expr,
            closure_vars: vec![],
        }
    }
}

impl<W: Write + Debug + 'static> CompilationResult<W> {
    fn new(expr: XExpr<W>, closure_vars: Vec<DefaultSymbol>) -> Self {
        Self { expr, closure_vars }
    }
    fn join(results: impl IntoIterator<Item = Self>) -> (Vec<XExpr<W>>, Vec<DefaultSymbol>) {
        let mut exprs = vec![];
        let mut closure_vars = vec![];
        for result in results {
            exprs.push(result.expr);
            closure_vars.extend(result.closure_vars.iter().copied());
        }
        (exprs, closure_vars)
    }
    fn from_multi(
        other: (Vec<XExpr<W>>, Vec<DefaultSymbol>),
        f: impl FnOnce(Vec<XExpr<W>>) -> XExpr<W>,
    ) -> Self {
        Self::new(f(other.0), other.1)
    }
}

pub(crate) fn resolve_overload<'p, W: Write + Debug + 'static>(
    overloads: &[XFunctionFactory<W>],
    args: Option<&[XExpr<W>]>,
    arg_types: &[Arc<XType>],
    name: DefaultSymbol,
    namespace: &'p XCompilationScope<'p, W>,
) -> Result<XExpr<W>, CompilationError<W>> {
    let mut exact_matches = vec![];
    let mut generic_matches = vec![];
    let mut dynamic_failures = vec![];
    let is_unknown = arg_types.iter().any(|t| t.is_unknown());
    // if the bindings are unknown, then we prefer generic solutions over exact solutions
    for overload in overloads {
        let overload = match overload {
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
            if overload.short_circuit_overloads() {
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
            param_types: arg_types.to_vec(),
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
            param_types: arg_types.to_vec(),
        });
    }
    Err(CompilationError::NoOverload {
        name,
        param_types: arg_types.to_vec(),
        dynamic_failures,
    })
}

impl<W: Write + Debug + 'static> XStaticExpr<W> {
    pub(crate) fn new_call(
        name: &'static str,
        args: Vec<Self>,
        interner: &mut StringInterner,
    ) -> Self {
        Self::Call(
            Box::new(Self::Ident(interner.get_or_intern_static(name))),
            args,
        )
    }

    pub(crate) fn new_call_sym(name: Identifier, args: Vec<Self>) -> Self {
        Self::Call(Box::new(Self::Ident(name)), args)
    }

    pub(crate) fn compile<'p>(
        self,
        namespace: &'p XCompilationScope<'p, W>,
    ) -> Result<CompilationResult<W>, CompilationError<W>> {
        fn compile_many<'p, W: Write + Debug + 'static>(
            exprs: impl IntoIterator<Item = XStaticExpr<W>>,
            namespace: &'p XCompilationScope<'p, W>,
        ) -> Result<(Vec<XExpr<W>>, Vec<DefaultSymbol>), CompilationError<W>> {
            let mut ret = vec![];
            for item in exprs {
                let item = item.compile(namespace)?;
                ret.push(item);
            }
            Ok(CompilationResult::join(ret))
        }

        match self {
            Self::LiteralBool(v) => Ok(XExpr::LiteralBool(v).into()),
            Self::LiteralInt(v) => Ok(XExpr::LiteralInt(v).into()),
            Self::LiteralFloat(v) => Ok(XExpr::LiteralFloat(v).into()),
            Self::LiteralString(v) => Ok(XExpr::LiteralString(v).into()),
            Self::Array(items) => Ok(CompilationResult::from_multi(
                compile_many(items, namespace)?,
                XExpr::Array,
            )),
            Self::Call(func, args) => {
                let (compiled_args, mut cvars) = compile_many(args, namespace)?;
                match func.as_ref() {
                    Self::Member(obj, member_name) => {
                        //special case: member access can be a variant constructor
                        if let Self::Ident(name) = obj.as_ref() {
                            if let Some(XCompilationScopeItem::Compound(
                                CompoundKind::Union,
                                spec,
                            )) = namespace.get(*name)
                            {
                                return if let Some(&index) = spec.indices.get(member_name) {
                                    if compiled_args.len() != 1 {
                                        return Err(CompilationError::VariantConstructorOneArg);
                                    }
                                    let compiled_arg = compiled_args.into_iter().next().unwrap();
                                    let com_type = Arc::new(XType::Compound(
                                        CompoundKind::Union,
                                        spec.clone(),
                                        Bind::new(),
                                    ));
                                    let var_type = spec.fields[index]
                                        .type_
                                        .resolve_bind(&Bind::new(), Some(&com_type));
                                    if let Some(bind) =
                                        var_type.bind_in_assignment(&compiled_arg.xtype()?)
                                    {
                                        return Ok(CompilationResult::new(
                                            XExpr::Variant(
                                                spec,
                                                bind,
                                                index,
                                                Box::new(compiled_arg),
                                            ),
                                            cvars,
                                        ));
                                    } else {
                                        Err(CompilationError::VariantConstructorTypeArgMismatch {
                                            union_name: spec.name,
                                            variant_name: member_name.clone(),
                                            expected_type: spec.fields[index].type_.clone(),
                                            actual_type: compiled_arg.xtype()?,
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
                    Self::Ident(name) => {
                        match namespace.get(*name) {
                            Some(XCompilationScopeItem::Overload(overloads)) => {
                                let arg_types = compiled_args
                                    .iter()
                                    .map(|x| x.xtype())
                                    .collect::<Result<Vec<_>, _>>()?;
                                return Ok(CompilationResult::new(
                                    XExpr::Call(
                                        Box::new(resolve_overload(
                                            &overloads,
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
                                        struct_name: spec.name,
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
                                        struct_name: spec.name,
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
                    Self::SpecializedIdent(name, arg_types) => {
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
                                        name: *name,
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
                                            &overloads,
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
                                name: *name,
                                item,
                            }),
                            None => Err(CompilationError::FunctionNotFound { name: *name }),
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
            Self::Member(obj, member_name) => {
                let obj_compiled = obj.compile(namespace)?;
                let obj_type = obj_compiled.expr.xtype()?;
                match obj_type.as_ref() {
                    XType::Compound(_, spec, _) => {
                        if let Some(&index) = spec.indices.get(&member_name) {
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
            Self::Ident(name) => match namespace.get_with_depth(name) {
                None => Err(CompilationError::ValueNotFound { name }),
                Some((
                    XCompilationScopeItem::Compound(..) | XCompilationScopeItem::NativeType(..),
                    _,
                )) => Err(CompilationError::TypeAsVariable { name }),
                Some((item, depth)) => {
                    let cvars = if depth != 0 && depth != namespace.height {
                        vec![name]
                    } else {
                        vec![]
                    };
                    match &item {
                        XCompilationScopeItem::Value(t) => Ok(CompilationResult::new(
                            XExpr::Ident(name, Box::new(IdentItem::Value(t.clone()))),
                            cvars,
                        )),
                        XCompilationScopeItem::Overload(overloads) => {
                            if overloads.len() == 1 {
                                // todo fix this if (turn into pattern)
                                let overload = &overloads[0];
                                match overload {
                                    XFunctionFactory::Static(overload) => {
                                        if overload.is_generic() {
                                            Err(CompilationError::GenericFunctionAsVariable {
                                                name,
                                            })
                                        } else {
                                            Ok(CompilationResult::new(
                                                XExpr::Ident(
                                                    name,
                                                    Box::new(IdentItem::Function(overload.clone())),
                                                ),
                                                cvars,
                                            ))
                                        }
                                    }
                                    XFunctionFactory::Dynamic(_) => {
                                        Err(CompilationError::DynamicFunctionAsVariable { name })
                                    }
                                }
                            } else {
                                Err(CompilationError::OverloadedFunctionAsVariable { name })
                            }
                        }
                        _ => unreachable!(),
                    }
                }
            },
            Self::SpecializedIdent(name, arg_types) => match namespace.get_with_depth(name) {
                Some((XCompilationScopeItem::Overload(overloads), depth)) => {
                    let cvars = if depth != 0 && depth != namespace.height {
                        vec![name]
                    } else {
                        vec![]
                    };
                    Ok(CompilationResult::new(
                        resolve_overload(&overloads, None, &arg_types, name, namespace)?,
                        cvars,
                    ))
                }
                None => Err(CompilationError::FunctionNotFound { name }),
                Some((item, _)) => Err(CompilationError::NonFunctionSpecialization { name, item }),
            },
            Self::Lambda(args, body) => {
                let mut subscope = XCompilationScope::from_parent_lambda(namespace);
                for param in &args {
                    subscope.add_param(param.name, param.type_.clone())?;
                }
                let body_result = body.compile(&subscope)?;
                let body_type = body_result.expr.xtype()?;
                return Ok(CompilationResult::from(XExpr::Lambda(Rc::new(
                    XStaticFunction::UserFunction(UfData::new(
                        XExplicitFuncSpec {
                            generic_params: None,
                            args,
                            ret: body_type,
                        },
                        vec![],
                        Box::new(body_result.expr),
                        body_result.closure_vars.iter().cloned().collect(),
                    )),
                ))));
            }
            Self::Tuple(items) => Ok(CompilationResult::from_multi(
                compile_many(items, namespace)?,
                XExpr::Tuple,
            )),
        }
    }
}

#[derive(Derivative)]
#[derivative(Clone(bound=""), Debug(bound=""))]
pub enum XExpr<W: Write + Debug + 'static> {
    LiteralBool(bool),
    LiteralInt(i64),
    LiteralFloat(f64),
    LiteralString(String),
    Array(Vec<XExpr<W>>),
    Tuple(Vec<XExpr<W>>),
    Call(Box<XExpr<W>>, Vec<XExpr<W>>),
    Construct(Arc<XCompoundSpec>, Bind, Vec<XExpr<W>>),
    Variant(Arc<XCompoundSpec>, Bind, usize, Box<XExpr<W>>),
    Member(Box<XExpr<W>>, usize),
    KnownOverload(Rc<XStaticFunction<W>>, Bind),
    Ident(DefaultSymbol, Box<IdentItem<W>>),
    Lambda(Rc<XStaticFunction<W>>),
    // this dummy exists for calling native functions with arguments that were already
    // evaluated
    Dummy(Rc<ManagedXValue<W>>),
}

pub enum XStaticFunction<W: Write + Debug + 'static> {
    // identical to a native, with the exception that will short-circut overload resolution
    ShortCircutNative(XFuncSpec, NativeCallable<W>),
    Native(XFuncSpec, NativeCallable<W>),
    UserFunction(UfData<W>),
    Recourse(Rc<XFuncSpec>, usize),
}

#[derive(Debug)] // todo better debug
pub struct UfData<W: Write + Debug + 'static> {
    pub spec: XExplicitFuncSpec<W>,
    pub output: Box<XExpr<W>>,
    pub cvars: HashSet<DefaultSymbol>,

    pub param_names: Vec<DefaultSymbol>,
    pub defaults: Vec<Rc<ManagedXValue<W>>>,
    pub declarations: Vec<Declaration<W>>,
}

impl<W: Write + Debug + 'static> UfData<W> {
    pub(crate) fn new(
        spec: XExplicitFuncSpec<W>,
        declarations: Vec<Declaration<W>>,
        output: Box<XExpr<W>>,
        cvars: HashSet<DefaultSymbol>,
    ) -> Self {
        Self {
            param_names: spec.args.iter().map(|p| p.name).collect(),
            defaults: spec
                .args
                .iter()
                .skip_while(|p| p.default.is_none())
                .map(|p| p.default.clone().unwrap())
                .collect(),
            spec,
            output,
            cvars,
            declarations,
        }
    }
}

impl<W: Write + Debug + 'static> XStaticFunction<W> {
    pub(crate) fn to_function(self: &Rc<Self>, closure: &XEvaluationScope<'_, W>) -> XFunction<W> {
        match self.as_ref() {
            Self::Native(_, native) | Self::ShortCircutNative(_, native) => {
                XFunction::Native(native.clone())
            }
            Self::UserFunction(..) => {
                let key = RcHash(self.clone());
                closure.get_ud_func(key).clone()
            }
            Self::Recourse(_, depth) => XFunction::Recourse(*depth),
        }
    }

    pub(crate) fn from_native(
        spec: XFuncSpec,
        native: impl Fn(&[XExpr<W>], &XEvaluationScope<'_, W>, bool, RTCell<W>) -> Result<TailedEvalResult<W>, String>
            + 'static,
    ) -> Self {
        Self::Native(spec, Rc::new(native))
    }

    pub(crate) fn from_native_short_circut(
        spec: XFuncSpec,
        native: impl Fn(&[XExpr<W>], &XEvaluationScope<'_, W>, bool, RTCell<W>) -> Result<TailedEvalResult<W>, String>
            + 'static,
    ) -> Self {
        Self::ShortCircutNative(spec, Rc::new(native))
    }

    fn short_circuit_overloads(&self) -> bool {
        matches!(self, Self::ShortCircutNative(..))
    }
}

impl<W: Write + Debug + 'static> Debug for XStaticFunction<W> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Self::Native(spec, _) => {
                write!(f, "Native({:?})", spec)
            }
            Self::ShortCircutNative(spec, _) => {
                write!(f, "Native({:?})", spec)
            }
            Self::UserFunction(spec, ..) => {
                write!(f, "UserFunction({:?})", spec)
            }
            Self::Recourse(spec, ..) => {
                write!(f, "Recourse({:?})", spec)
            }
        }
    }
}

impl<W: Write + Debug + 'static> PartialEq for XStaticFunction<W> {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl<W: Write + Debug + 'static> Eq for XStaticFunction<W> {}

#[derive(Debug)]
pub struct XExplicitFuncSpec<W: Write + Debug + 'static> {
    pub generic_params: Option<Vec<DefaultSymbol>>,
    pub args: Vec<XExplicitArgSpec<W>>,
    pub ret: Arc<XType>,
}

#[derive(Debug)]
pub struct XExplicitArgSpec<W: Write + Debug + 'static> {
    pub(crate) name: DefaultSymbol,
    pub(crate) type_: Arc<XType>,
    pub(crate) default: Option<Rc<ManagedXValue<W>>>,
}

impl<W: Write + Debug + 'static> XExplicitFuncSpec<W> {
    pub(crate) fn to_spec(&self) -> XFuncSpec {
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

impl<W: Write + Debug + 'static> XStaticFunction<W> {
    fn bind(&self, args: &[Arc<XType>]) -> Option<Bind> {
        match self {
            Self::Native(spec, _) | Self::ShortCircutNative(spec, _) => spec.bind(args),
            Self::Recourse(spec, ..) => spec.bind(args),
            Self::UserFunction(ud, ..) => ud.spec.to_spec().bind(args),
        }
    }
    fn rtype(&self, bind: &Bind) -> Arc<XType> {
        match self {
            Self::Native(spec, _) | Self::ShortCircutNative(spec, _) => spec.rtype(bind),
            Self::Recourse(spec, ..) => spec.rtype(bind),
            Self::UserFunction(ud, ..) => ud.spec.to_spec().rtype(bind),
        }
    }
    fn is_generic(&self) -> bool {
        match self {
            Self::Native(spec, _) | Self::ShortCircutNative(spec, _) => {
                spec.generic_params.is_some()
            }
            Self::Recourse(spec, ..) => spec.generic_params.is_some(),
            Self::UserFunction(ud, ..) => ud.spec.generic_params.is_some(),
        }
    }
    fn xtype(&self, bind: &Bind) -> Arc<XType> {
        match self {
            Self::Native(spec, _) | Self::ShortCircutNative(spec, _) => spec.xtype(bind),
            Self::Recourse(spec, ..) => spec.xtype(bind),
            Self::UserFunction(ud, ..) => ud.spec.to_spec().xtype(bind),
        }
    }
}

#[derive(Derivative)]
#[derivative(Clone(bound=""), Debug(bound=""))]
pub enum IdentItem<W: Write + Debug + 'static> {
    Value(Arc<XType>),
    Function(Rc<XStaticFunction<W>>),
}

#[derive(Debug)]
pub enum TailedEvalResult<W: Write + Debug + 'static> {
    Value(Rc<ManagedXValue<W>>),
    TailCall(Vec<Rc<ManagedXValue<W>>>),
}

impl<W: Write + Debug + 'static> TailedEvalResult<W> {
    pub(crate) fn unwrap_value(self) -> Rc<ManagedXValue<W>> {
        match self {
            Self::Value(v) => v,
            Self::TailCall(_) => {
                panic!("TailedEvalResult::unwrap_value called on a tail call")
            }
        }
    }
}

impl<W: Write + Debug + 'static> From<Rc<ManagedXValue<W>>> for TailedEvalResult<W> {
    fn from(v: Rc<ManagedXValue<W>>) -> Self {
        Self::Value(v)
    }
}

impl<W: Write + Debug + 'static> XExpr<W> {
    pub(crate) fn xtype(&self) -> Result<Arc<XType>, CompilationError<W>> {
        match self {
            Self::LiteralBool(_) => Ok(X_BOOL.clone()),
            Self::LiteralInt(_) => Ok(X_INT.clone()),
            Self::LiteralFloat(_) => Ok(X_FLOAT.clone()),
            Self::LiteralString(_) => Ok(X_STRING.clone()),
            Self::Array(exprs) => {
                let element_type = common_type(exprs.iter().map(|x| x.xtype()))?;
                Ok(XType::XNative(Box::new(XSequenceType {}), vec![element_type]).into())
            }
            Self::Call(func, _) => {
                if let Self::KnownOverload(func, bind) = func.as_ref() {
                    return Ok(func.rtype(bind));
                }
                let func_type = func.xtype()?;
                if let XType::XCallable(spec) = func_type.as_ref() {
                    return Ok(spec.return_type.clone());
                }
                if let XType::XFunc(func) = func_type.as_ref() {
                    return Ok(func.rtype(&Bind::new()));
                }
                Err(CompilationError::NotAFunction { type_: func_type })
            }
            Self::Construct(spec, binding, ..) => Ok(Arc::new(XType::Compound(
                CompoundKind::Struct,
                spec.clone(),
                binding.clone(),
            ))),
            Self::Variant(spec, binding, ..) => Ok(Arc::new(XType::Compound(
                CompoundKind::Union,
                spec.clone(),
                binding.clone(),
            ))),
            Self::Member(obj, idx) => {
                let obj_type = obj.xtype()?;
                match obj_type.as_ref() {
                    XType::Compound(CompoundKind::Struct, spec, bind) => Ok(spec.fields[*idx]
                        .type_
                        .clone()
                        .resolve_bind(bind, Some(&obj_type))),
                    XType::Compound(CompoundKind::Union, spec, bind) => {
                        let t = spec.fields[*idx]
                            .type_
                            .clone()
                            .resolve_bind(bind, Some(&obj_type));
                        Ok(XOptionalType::xtype(t))
                    }
                    XType::Tuple(fields) => Ok(fields[*idx].clone()),
                    _ => Err(CompilationError::NotACompound {
                        type_: obj_type.clone(),
                    }),
                }
            }
            Self::KnownOverload(func, bind) => Ok(func.xtype(bind)),
            Self::Lambda(func) => Ok(func.xtype(&Bind::new())),
            Self::Ident(_, item) => match item.as_ref() {
                IdentItem::Value(xtype) => Ok(xtype.clone()),
                IdentItem::Function(func) => Ok(func.xtype(&Bind::new())),
            },
            Self::Tuple(items) => {
                let types = items
                    .iter()
                    .map(|x| x.xtype())
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Arc::new(XType::Tuple(types)))
            }
            Self::Dummy(_) => unreachable!(),
        }
    }

    pub(crate) fn eval<'p>(
        &self,
        namespace: &XEvaluationScope<'p, W>,
        tail_available: bool,
        runtime: RTCell<W>,
    ) -> Result<TailedEvalResult<W>, String> {
        match &self {
            Self::LiteralBool(b) => Ok(ManagedXValue::new(XValue::Bool(*b), runtime)?.into()),
            Self::LiteralInt(i) => {
                Ok(ManagedXValue::new(XValue::Int(LazyBigint::from(*i)), runtime)?.into())
            }
            Self::LiteralFloat(r) => Ok(ManagedXValue::new(XValue::Float(*r), runtime)?.into()),
            Self::LiteralString(s) => {
                Ok(ManagedXValue::new(XValue::String(s.clone()), runtime)?.into())
            }
            Self::Array(exprs) => {
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
            Self::Call(func, args) => {
                let callable = func.eval(namespace, false, runtime.clone())?.unwrap_value();
                let ret;
                if let XValue::Function(xfunc) = &callable.value {
                    ret = xfunc.eval(args, namespace, tail_available, runtime)?;
                } else {
                    return Err(format!("Expected function, got {:?}", callable));
                }
                Ok(ret)
            }
            Self::Construct(_, _, args) | Self::Tuple(args) => {
                let items = args
                    .iter()
                    .map(|x| {
                        x.eval(namespace, false, runtime.clone())
                            .map(|r| r.unwrap_value())
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(ManagedXValue::new(XValue::StructInstance(items), runtime)?.into())
            }
            Self::Member(obj, idx) => {
                let obj = obj.eval(namespace, false, runtime.clone())?.unwrap_value();
                match &obj.value {
                    XValue::StructInstance(items) => Ok(items[*idx].clone().into()),
                    XValue::UnionInstance(variant, item) => Ok(if variant == idx {
                        manage_native!(
                            XOptional {
                                value: Some(item.clone())
                            },
                            runtime
                        )
                    } else {
                        manage_native!(XOptional::<W> { value: None }, runtime)
                    }),
                    _ => Err(format!("Expected struct, got {:?}", obj)),
                }
            }
            Self::Variant(.., idx, expr) => {
                let obj = expr.eval(namespace, false, runtime.clone())?.unwrap_value();
                Ok(ManagedXValue::new(XValue::UnionInstance(*idx, obj), runtime)?.into())
            }
            Self::KnownOverload(func, ..) => Ok(ManagedXValue::new(
                XValue::Function(func.clone().to_function(namespace)),
                runtime,
            )?
            .into()),
            Self::Lambda(func) => Ok(ManagedXValue::new(
                XValue::Function(namespace.lock_closure(func)),
                runtime,
            )?
            .into()),
            Self::Ident(name, item) => {
                if let IdentItem::Function(func) = item.as_ref() {
                    Ok(ManagedXValue::new(
                        XValue::Function(func.clone().to_function(namespace)),
                        runtime,
                    )?
                    .into())
                } else {
                    namespace
                        .get_value(*name)
                        .ok_or_else(|| {
                            format!("Undefined identifier during evaluation: {:?}", name)
                        })?
                        .map(
                            |v| v.into()
                        )
                }
            }
            Self::Dummy(val) => Ok(val.clone().into()),
        }
    }
}
