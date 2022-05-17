use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Error, Formatter};
use std::mem::take;
use std::rc::Rc;
use std::sync::Arc;
use num::{BigInt, BigRational};
use crate::builtin::array::{XArray, XArrayType};
use crate::builtin::set::{XSet, XSetType};
use crate::XFuncSpec;
use crate::xscope::{Declaration, Identifier, XCompilationScope, XCompilationScopeItem, XEvaluationScope};
use crate::xtype::{Bind, common_type, X_BOOL, X_INT, X_RATIONAL, X_STRING, XFuncParamSpec, XStructSpec, XType};
use crate::xvalue::{ManagedXValue, NativeCallable, XFunction, XValue};
use derivative::Derivative;
use itertools::Itertools;
use string_interner::{DefaultSymbol, StringInterner};
use crate::runtime::{RTCell, Runtime};

#[derive(Debug, Clone, Derivative)]
#[derivative(Hash)]
pub enum XStaticExpr {
    LiteralBool(bool),
    LiteralInt(i64),
    LiteralRational(BigRational),
    LiteralString(String),
    Array(Vec<XStaticExpr>),
    Set(Vec<XStaticExpr>),
    Call(Box<XStaticExpr>, Vec<XStaticExpr>),
    Member(Box<XStaticExpr>, String),
    Ident(DefaultSymbol),
    SpecializedIdent(DefaultSymbol,
                     #[derivative(Hash = "ignore")]
                     Vec<Arc<XType>>),
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
            expr: expr,
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
    fn map(self, f: impl FnOnce(XExpr) -> XExpr) -> Self {
        Self::new(f(self.expr), self.closure_vars)
    }
    fn from_multi(other: (Vec<XExpr>, Vec<DefaultSymbol>), f: impl FnOnce(Vec<XExpr>) -> XExpr) -> Self {
        Self::new(f(other.0), other.1)
    }
}


impl XStaticExpr {
    pub fn new_call(name: &'static str, args: Vec<XStaticExpr>, interner: &mut StringInterner) -> XStaticExpr {
        XStaticExpr::Call(Box::new(XStaticExpr::Ident(interner.get_or_intern_static(name))), args)
    }

    pub fn new_call_sym(name: Identifier, args: Vec<XStaticExpr>) -> XStaticExpr {
        XStaticExpr::Call(Box::new(XStaticExpr::Ident(name)), args)
    }

    pub fn compile<'p>(&self, namespace: &'p XCompilationScope<'p>) -> Result<CompilationResult, String> {
        fn resolve_overload(overloads: Vec<Rc<XStaticFunction>>, arg_types: &Vec<Arc<XType>>, name: DefaultSymbol) -> Result<XExpr, String> {
            let mut exact_matches = vec![];
            let mut generic_matches = vec![];
            let is_unknown = arg_types.iter().any(|t| t.is_unknown());
            // if the bindings are unknown, then we prefer generic solutions over exact solutions
            for overload in overloads {
                let b = overload.bind(arg_types);
                if let Some(bind) = b {
                    if overload.is_generic() ^ is_unknown {
                        &mut generic_matches
                    } else {
                        &mut exact_matches
                    }.push(XExpr::KnownOverload(overload, bind));
                }
            }
            if exact_matches.len() == 1 {
                return Ok(exact_matches.swap_remove(0));
            }
            if exact_matches.len() > 1 {
                // todo fix
                return Err(format!("ambiguous call to overloaded function {:?}", name));
            }
            if generic_matches.len() == 1 {
                return Ok(generic_matches.swap_remove(0));
            }
            if generic_matches.len() > 1 {
                // todo fix
                return Err(format!("ambiguous generic call to overloaded function {:?}", name));
            }
            Err(format!("no overload found for {:?} with param types: {:?}", name, arg_types))
        }

        fn compile_many<'p>(exprs: &Vec<XStaticExpr>, namespace: &'p XCompilationScope<'p>) -> Result<(Vec<XExpr>, Vec<DefaultSymbol>), String> {
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
            XStaticExpr::LiteralRational(v) => Ok(XExpr::LiteralRational(v.clone()).into()),
            XStaticExpr::LiteralString(v) => Ok(XExpr::LiteralString(v.clone()).into()),
            XStaticExpr::Array(items) => Ok(CompilationResult::from_multi(compile_many(items, namespace)?, XExpr::Array)),
            XStaticExpr::Set(items) => Ok(CompilationResult::from_multi(compile_many(items, namespace)?, XExpr::Set)),
            XStaticExpr::Call(func, args) => {
                let (compiled_args, mut cvars) = compile_many(args, namespace)?;
                match func.as_ref() {
                    XStaticExpr::Ident(name) => {
                        match namespace.get(*name) {
                            Some(XCompilationScopeItem::Overload(overloads)) => {
                                let arg_types = compiled_args.iter().map(|x| x.xtype()).collect::<Result<Vec<_>, _>>()?;
                                return Ok(CompilationResult::new(
                                    XExpr::Call(Box::new(resolve_overload(overloads, &arg_types, *name)?), compiled_args),
                                    cvars,
                                ));
                            }
                            Some(XCompilationScopeItem::Struct(spec)) => {
                                let arg_types = compiled_args.iter().map(|x| x.xtype()).collect::<Result<Vec<_>, _>>()?;
                                if arg_types.len() != spec.fields.len() {
                                    // todo
                                    return Err(format!("Struct {:?} takes {} arguments, but {} were given", spec.name, spec.fields.len(), arg_types.len()));
                                }
                                return if let Some(bind) = spec.bind(&arg_types) {
                                    Ok(CompilationResult::new(XExpr::Construct(spec.clone(), bind, compiled_args), cvars))
                                } else {
                                    // todo
                                    Err(format!("Struct {:?} does not take arguments {:?}", spec.name, arg_types))
                                };
                            }
                            _ => {}
                        }
                    }
                    XStaticExpr::SpecializedIdent(name, arg_types) => {
                        let actual_arg_types = compiled_args.iter().map(|x| x.xtype()).collect::<Result<Vec<_>, _>>()?;
                        let mut bind = Bind::new();
                        for (arg_type, actual_type) in arg_types.iter().zip(actual_arg_types.iter()) {
                            bind = arg_type.bind_in_assignment(actual_type)
                                .and_then(|b| bind.mix(&b))
                                .ok_or_else(|| format!("Specialized function {:?} takes argument of type {}, but {} was given", name, arg_type, actual_type))?;
                        }
                        return match namespace.get(*name) {
                            Some(XCompilationScopeItem::Overload(overloads)) => {
                                Ok(CompilationResult::new(
                                    XExpr::Call(Box::new(resolve_overload(overloads, arg_types, *name)?), compiled_args),
                                    cvars,
                                ))
                            }
                            Some(_) => {
                                // todo
                                Err(format!("Non-function {:?} cannot be specialized", name))
                            }
                            None => {
                                // todo
                                Err(format!("No such function {:?}", name))
                            }
                        };
                    }
                    _ => {}
                }
                let CompilationResult { expr: func_compiled, closure_vars: func_closure_vars } = func.compile(namespace)?;
                cvars.extend(func_closure_vars.into_iter());
                Ok(CompilationResult::new(XExpr::Call(Box::new(func_compiled), compiled_args), cvars))
            }
            XStaticExpr::Member(obj, member_name) => {
                let obj_compiled = obj.compile(namespace)?;
                match obj_compiled.expr.xtype()?.as_ref() {
                    XType::XStruct(spec, _) => {
                        if let Some(&index) = spec.indices.get(member_name) {
                            Ok(CompilationResult::new(XExpr::Member(Box::new(obj_compiled.expr), index), obj_compiled.closure_vars))
                        } else {
                            Err(format!("No member named {} in struct {:?}", member_name, spec))
                        }
                    }
                    _ => Err(format!("Expected struct type, got {:?}", obj_compiled.expr.xtype()))
                }
            }
            XStaticExpr::Ident(name) => {
                match namespace.get_with_depth(*name) {
                    None => Err(format!("Undefined identifier: {:?}", name)),
                    Some((XCompilationScopeItem::Struct(_), _)) => Err(format!("Struct {:?} cannot be used as a variable", name)),
                    Some((item, depth)) => {
                        let cvars = if depth != 0 && depth != namespace.height {
                            vec![name.clone()]
                        } else { vec![] };
                        match &item {
                            XCompilationScopeItem::Value(t) => {
                                Ok(CompilationResult::new(XExpr::Ident(*name, Box::new(IdentItem::Value(t.clone()))), cvars))
                            }
                            XCompilationScopeItem::Overload(overloads) => {
                                if overloads.len() == 1 {
                                    let overload = &overloads[0];
                                    if overload.is_generic() {
                                        Err(format!("Cannot use generic overload {:?} as a variable", name))
                                    } else {
                                        Ok(CompilationResult::new(XExpr::Ident(*name, Box::new(IdentItem::Function(overload.clone()))), cvars))
                                    }
                                } else {
                                    Err(format!("Ambiguous identifier: {:?}", name))
                                }
                            }
                            _ => unreachable!()
                        }
                    }
                }
            }
            XStaticExpr::SpecializedIdent(name, arg_types) => {
                match namespace.get_with_depth(*name) {
                    Some((XCompilationScopeItem::Overload(overloads), depth)) => {
                        let cvars = if depth != 0 && depth != namespace.height {
                            vec![name.clone()]
                        } else { vec![] };
                        Ok(CompilationResult::new(resolve_overload(overloads, arg_types, *name)?, cvars))
                    }
                    None => Err(format!("Undefined identifier: {:?}", name)),
                    Some(_) => Err(format!("Non-function {:?} cannot be specialized", name)),
                }
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum XExpr {
    LiteralBool(bool),
    LiteralInt(i64),
    LiteralRational(BigRational),
    LiteralString(String),
    Array(Vec<XExpr>),
    Set(Vec<XExpr>),
    /*
    Map(Vec<(XExpr, XExpr)>),
     */
    Call(Box<XExpr>, Vec<XExpr>),
    Construct(Arc<XStructSpec>, Bind, Vec<XExpr>),
    Member(Box<XExpr>, usize),
    KnownOverload(Rc<XStaticFunction>, Bind),
    Ident(DefaultSymbol, Box<IdentItem>),
    // this dummy exists for calling native functions with arguments that were already
    // evaluated
    Dummy(Rc<ManagedXValue>),
}

#[derive(Clone)]  // todo why clone?
pub enum XStaticFunction {
    Native(XFuncSpec, NativeCallable),
    UserFunction(UfData),
    Recourse(Rc<XFuncSpec>),
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
    pub fn new(spec: XExplicitFuncSpec, declarations: Vec<Declaration>, output: Box<XExpr>, cvars: HashSet<DefaultSymbol>) -> UfData {
        UfData {
            param_names: (&spec.args).iter().map(|p| p.name).collect(),
            defaults: (&spec.args).iter().skip_while(|p| p.default.is_none()).map(|p| p.default.clone().unwrap()).collect(),
            spec,
            output,
            cvars,
            variable_declarations: declarations.into_iter().filter_map(|decl| {
                if let Declaration::Value(name, expr) = decl {
                    Some((name.clone(), expr.clone()))
                } else {
                    None
                }
            }).collect(),
        }
    }
}

impl XStaticFunction {
    pub fn to_function(self: Rc<Self>, closure: &XEvaluationScope<'_>) -> XFunction {
        match self.as_ref() {
            XStaticFunction::Native(_, native) => XFunction::Native(*native),
            XStaticFunction::UserFunction(uf) => {
                let closure = uf.cvars.iter().map(|&name| (name, closure.get(name).unwrap().clone())).collect();
                XFunction::UserFunction(self.clone(), closure)
            }
            XStaticFunction::Recourse(_) => XFunction::Recourse(),
        }
    }
}

impl Debug for XStaticFunction {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            XStaticFunction::Native(spec, _) => {
                write!(f, "Native({:?})", spec)
            }
            XStaticFunction::UserFunction(spec, ..) => {
                write!(f, "UserFunction({:?})", spec)
            }
            XStaticFunction::Recourse(spec) => {
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

#[derive(Debug, Clone, Eq, PartialEq, Derivative)]
#[derivative(Hash)]
pub struct XExplicitFuncSpec {
    pub generic_params: Option<Vec<DefaultSymbol>>,
    pub args: Vec<XExplicitArgSpec>,
    #[derivative(Hash = "ignore")]
    pub ret: Arc<XType>,
}

#[derive(Debug, Clone, Eq, PartialEq, Derivative)]
#[derivative(Hash)]
pub struct XExplicitArgSpec {
    pub name: DefaultSymbol,
    #[derivative(Hash = "ignore")]
    pub type_: Arc<XType>,
    pub default: Option<Rc<ManagedXValue>>,
}

impl XExplicitFuncSpec {
    pub fn to_spec(&self) -> XFuncSpec {
        XFuncSpec {
            generic_params: self.generic_params.clone(),
            params: self.args.iter().map(|x| {
                XFuncParamSpec {
                    type_: x.type_.clone(),
                    required: x.default.is_none(),
                }
            }).collect(),
            ret: self.ret.clone(),
        }
    }
}

impl XStaticFunction {
    pub fn bind(&self, args: &Vec<Arc<XType>>) -> Option<Bind> {
        match self {
            XStaticFunction::Native(spec, _) => spec.bind(args),
            XStaticFunction::Recourse(spec) => spec.bind(args),
            XStaticFunction::UserFunction(ud, ..) => ud.spec.to_spec().bind(args),
        }
    }
    pub fn rtype(&self, bind: &Bind) -> Arc<XType> {
        match self {
            XStaticFunction::Native(spec, _) => spec.rtype(bind),
            XStaticFunction::Recourse(spec) => spec.rtype(bind),
            XStaticFunction::UserFunction(ud, ..) => ud.spec.to_spec().rtype(bind),
        }
    }
    pub fn is_generic(&self) -> bool {
        match self {
            XStaticFunction::Native(spec, _) => spec.generic_params.is_some(),
            XStaticFunction::Recourse(spec) => spec.generic_params.is_some(),
            XStaticFunction::UserFunction(ud, ..) => ud.spec.generic_params.is_some(),
        }
    }
    pub fn xtype(&self, bind: &Bind) -> Arc<XType> {
        match self {
            XStaticFunction::Native(spec, _) => spec.xtype(bind),
            XStaticFunction::Recourse(spec) => spec.xtype(bind),
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
            TailedEvalResult::TailCall(_) => panic!("TailedEvalResult::unwrap_value called on a tail call")
        }
    }
}

impl From<Rc<ManagedXValue>> for TailedEvalResult {
    fn from(v: Rc<ManagedXValue>) -> Self {
        TailedEvalResult::Value(v)
    }
}

impl XExpr {
    pub fn xtype(&self) -> Result<Arc<XType>, String> {
        match self {
            XExpr::LiteralBool(_) => Ok(X_BOOL.clone()),
            XExpr::LiteralInt(_) => Ok(X_INT.clone()),
            XExpr::LiteralRational(_) => Ok(X_RATIONAL.clone()),
            XExpr::LiteralString(_) => Ok(X_STRING.clone()),
            XExpr::Array(exprs) => {
                let element_type = common_type(exprs.iter().map(|x| x.xtype()))?;
                Ok(XType::XNative(Box::new(XArrayType {}), vec![element_type]).into())
            }
            XExpr::Set(exprs) => {
                let element_type = common_type(exprs.iter().map(|x| x.xtype()))?;
                Ok(XType::XNative(Box::new(XSetType {}), vec![element_type]).into())
            }
            XExpr::Call(func, _) => {
                if let XExpr::KnownOverload(func, bind) = func.as_ref() {
                    return Ok(func.rtype(bind));
                }
                if let XType::XCallable(spec) = func.xtype()?.as_ref() {
                    return Ok(spec.return_type.clone());
                }
                if let XType::XFunc(func) = func.xtype()?.as_ref() {
                    return Ok(func.rtype(&Bind::new()));
                }
                Err(format!("Expected function type, got {:?}", func.xtype()?))
            }
            XExpr::Construct(spec, binding, ..) => Ok(Arc::new(XType::XStruct(spec.clone(), binding.clone()))),
            XExpr::Member(obj, idx) => {
                if let XType::XStruct(spec, bind) = obj.xtype()?.as_ref() {
                    Ok(spec.fields[*idx].type_.clone().resolve_bind(&bind))
                } else {
                    Err(format!("Expected struct type, got {:?}", obj.xtype()?))
                }
            }
            XExpr::KnownOverload(func, bind) => Ok(func.xtype(bind)),
            XExpr::Ident(_, item) => {
                match item.as_ref() {
                    IdentItem::Value(xtype) => Ok(xtype.clone()),
                    IdentItem::Function(func) => Ok(func.xtype(&Bind::new())),
                }
            }
            XExpr::Dummy(_) => unreachable!(),
        }
    }

    pub fn eval<'p>(&self, namespace: &XEvaluationScope<'p>, tail_available: bool, runtime: RTCell) -> Result<TailedEvalResult, String> {
        match &self {
            XExpr::LiteralBool(b) => Ok(ManagedXValue::new(XValue::Bool(*b),runtime)?.into()),
            XExpr::LiteralInt(i) => Ok(ManagedXValue::new(XValue::Int(BigInt::from(*i)),runtime)?.into()),
            XExpr::LiteralRational(r) => Ok(ManagedXValue::new(XValue::Rational(r.clone()), runtime)?.into()),
            XExpr::LiteralString(s) => Ok(ManagedXValue::new(XValue::String(s.clone()), runtime)?.into()),
            XExpr::Array(exprs) => {
                Ok(ManagedXValue::new(XValue::Native(Box::new(XArray::new(
                    exprs.iter().map(|x| x.eval(namespace, false, runtime.clone()).map(|r| r.unwrap_value())).collect::<Result<Vec<_>, _>>()?)))
                , runtime)?.into())
            }
            XExpr::Set(exprs) => {
                Ok(ManagedXValue::new(XValue::Native(Box::new(XSet::new(
                    exprs.iter().map(|x| x.eval(namespace, false, runtime.clone()).map(|r| r.unwrap_value())).collect::<Result<HashSet<_>, _>>()?)))
                , runtime)?.into())
            }
            XExpr::Call(func, args) => {
                let callable = func.eval(namespace, false, runtime.clone())?.unwrap_value().clone();
                let ret;
                if let XValue::Function(xfunc) = &callable.value {
                    ret = xfunc.eval(args, namespace, tail_available, runtime)?;
                } else {
                    return Err(format!("Expected function, got {:?}", callable));
                }
                return Ok(ret);
            }
            XExpr::Construct(_, _, args) => {
                let items = args.iter().map(|x| x.eval(namespace, false, runtime.clone()).map(|r| r.unwrap_value())).collect::<Result<Vec<_>, _>>()?;
                Ok(ManagedXValue::new(XValue::StructInstance(items), runtime)?.into())
            }
            XExpr::Member(obj, idx) => {
                let obj = obj.eval(namespace, false, runtime)?.unwrap_value();
                if let XValue::StructInstance(items) = &obj.value {
                    Ok(items[*idx].clone().into())
                } else {
                    Err(format!("Expected struct, got {:?}", obj))
                }
            }
            XExpr::KnownOverload(func, _) => {
                Ok(ManagedXValue::new(XValue::Function(func.clone().to_function(namespace)), runtime)?.into())
            }
            XExpr::Ident(name, item) => {
                if let IdentItem::Function(func) = item.as_ref() {
                    Ok(ManagedXValue::new(XValue::Function(func.clone().to_function(namespace)), runtime)?.into())
                } else {
                    Ok(namespace.get(*name).ok_or_else(|| format!("Undefined identifier during evaluation: {:?}", name))?.clone().into())
                }
            }
            XExpr::Dummy(val) => Ok(val.clone().into()),
        }
    }
}