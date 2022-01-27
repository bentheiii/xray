use std::borrow::{Borrow, BorrowMut};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Error, Formatter};
use std::rc::Rc;
use num::{BigInt, BigRational};
use crate::xexpr::XExpr::{LiteralBool, LiteralInt};
use crate::XFuncSpec;
use crate::xscope::{Declaration, XCompilationScope, XCompilationScopeItem, XEvaluationScope};
use crate::xtype::{common_type, X_BOOL, X_INT, X_RATIONAL, X_STRING, XFuncParamSpec, XStructSpec, XType};
use crate::xvalue::{NativeCallable, XFunction, XValue};

#[derive(Hash, Debug, Clone)]
pub enum XStaticExpr {
    LiteralBool(bool),
    LiteralInt(i64),
    LiteralRational(BigRational),
    LiteralString(String),
    /*Array(Vec<XStaticExpr>),
    Set(Vec<XStaticExpr>),
    Map(Vec<(XStaticExpr, XStaticExpr)>),*/
    Call(Box<XStaticExpr>, Vec<XStaticExpr>),
    Member(Box<XStaticExpr>, String),
    Ident(String),
}

impl XStaticExpr {
    pub fn new_call(name: &str, args: Vec<XStaticExpr>) -> XStaticExpr {
        XStaticExpr::Call(Box::new(XStaticExpr::Ident(name.to_string())), args)
    }

    pub fn compile<'p, 's: 'p>(&self, namespace: &'p XCompilationScope<'p, 's>) -> Result<XExpr<'s>, String> {
        match self {
            XStaticExpr::LiteralBool(v) => Ok(XExpr::LiteralBool(*v)),
            XStaticExpr::LiteralInt(v) => Ok(XExpr::LiteralInt(*v)),
            XStaticExpr::LiteralRational(v) => Ok(XExpr::LiteralRational(v.clone())),
            XStaticExpr::LiteralString(v) => Ok(XExpr::LiteralString(v.clone())),
            /*XStaticExpr::Array(items) => Ok(XExpr::Array(items.iter().map(|x| x.compile(namespace)).collect::<Result<Vec<_>, _>>()?)),
            XStaticExpr::Set(items) => Ok(XExpr::Set(items.iter().map(|x| x.compile(namespace)).collect::<Result<Vec<_>, _>>()?)),
            XStaticExpr::Map(items) => Ok(XExpr::Map(
                items.iter().map(|(x, y)| Ok((x.compile(namespace)?, y.compile(namespace)?))).collect::<Result<Vec<_>, String>>()?,
            )),*/
            XStaticExpr::Call(func, args) => {
                let compiled_args = args.iter().map(|x| x.compile(namespace)).collect::<Result<Vec<_>, _>>()?;
                if let XStaticExpr::Ident(name) = func.as_ref() {
                    match namespace.get(name) {
                        Some(XCompilationScopeItem::Overload(overloads)) => {
                            let arg_types = compiled_args.iter().map(|x| x.xtype()).collect::<Result<Vec<_>, _>>()?;
                            let mut called = None;
                            for overload in overloads {
                                if let Some(bind) = overload.bind(arg_types.clone()) {
                                    let is_generic = overload.is_generic();
                                    called = Some(XExpr::KnownOverload(overload, bind));
                                    if !is_generic {
                                        break;
                                    }
                                }
                            }
                            return match called {
                                Some(x) => Ok(XExpr::Call(Box::new(x), compiled_args)),
                                None => Err(format!("No overload for {} with arguments {:?}", name, arg_types))
                            };
                        }
                        Some(XCompilationScopeItem::Struct(spec)) => {
                            let arg_types = compiled_args.iter().map(|x| x.xtype()).collect::<Result<Vec<_>, _>>()?;
                            if arg_types.len() != spec.fields.len() {
                                return Err(format!("Struct {} takes {} arguments, but {} were given", spec.name, spec.fields.len(), arg_types.len()));
                            }
                            return if let Some(bind) = spec.bind(arg_types.clone()) {
                                Ok(XExpr::Construct(spec.clone(), bind, compiled_args))
                            }
                            else {
                                Err(format!("Struct {} does not take arguments {:?}", spec.name, arg_types))
                            };
                        }
                        _ => {}
                    }
                }
                let func_compiled = func.compile(namespace)?;
                Ok(XExpr::Call(Box::new(func_compiled), compiled_args))
            }
            XStaticExpr::Member(obj, member_name) => {
                let obj_compiled = obj.compile(namespace)?;
                match obj_compiled.xtype()? {
                    XType::XStruct(spec, _) => {
                        if let Some(&index) = spec.indices.get(member_name) {
                            Ok(XExpr::Member(Box::new(obj_compiled), index))
                        } else {
                            Err(format!("No member named {} in struct {:?}", member_name, spec))
                        }
                    }
                    _ => Err(format!("Expected struct type, got {:?}", obj_compiled.xtype()))
                }
            }
            XStaticExpr::Ident(name) => {
                match namespace.get(name) {
                    None => Err(format!("Undefined identifier: {}", name)),
                    Some(XCompilationScopeItem::Struct(_)) => Err(format!("Struct {} cannot be used as a variable", name)),
                    Some(item) => {
                        match &item {
                            XCompilationScopeItem::Value(t) => Ok(XExpr::Ident(name.to_string(), Box::new(IdentItem::Value(t.clone())))),
                            XCompilationScopeItem::Overload(overloads) => {
                                if overloads.len() == 1 {
                                    let overload = &overloads[0];
                                    if overload.is_generic() {
                                        Err(format!("Cannot use generic overload {} as a variable", name))
                                    } else {
                                        Ok(XExpr::Ident(name.to_string(), Box::new(IdentItem::Function(overload.clone().into()))))
                                    }
                                } else {
                                    Err(format!("Ambiguous identifier: {}", name))
                                }
                            }
                            _ => unreachable!()
                        }
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum XExpr<'c> {
    LiteralBool(bool),
    LiteralInt(i64),
    LiteralRational(BigRational),
    LiteralString(String),
    /*
    Array(Vec<XExpr>),
    Set(Vec<XExpr>),
    Map(Vec<(XExpr, XExpr)>),
     */
    Call(Box<XExpr<'c>>, Vec<XExpr<'c>>),
    Construct(XStructSpec, HashMap<String, XType>, Vec<XExpr<'c>>),
    Member(Box<XExpr<'c>>, usize),
    KnownOverload(XStaticFunction<'c>, HashMap<String, XType>),
    Ident(String, Box<IdentItem<'c>>),
}

#[derive(Clone)]
pub enum XStaticFunction<'c> {
    Native(XFuncSpec, NativeCallable<'c>),
    UserFunction(XExplicitFuncSpec<'c>, Vec<Declaration<'c>>, Box<XExpr<'c>>),
    Recourse(XFuncSpec),
}

impl Debug for XStaticFunction<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            XStaticFunction::Native(spec, _) => {
                write!(f, "Native({:?})", spec)
            }
            XStaticFunction::UserFunction(spec, _, _) => {
                write!(f, "UserFunction({:?})", spec)
            }
            XStaticFunction::Recourse(spec) => {
                write!(f, "Recourse({:?})", spec)
            }
        }
    }
}

impl PartialEq for XStaticFunction<'_> {
    fn eq(&self, other: &Self) -> bool {
        return false
    }
}

impl Eq for XStaticFunction<'_> {}

#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct XExplicitFuncSpec<'c> {
    pub generic_params: Option<Vec<String>>,
    pub args: Vec<XExplicitArgSpec<'c>>,
    pub ret: XType,
}

#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct XExplicitArgSpec<'c> {
    pub name: String,
    pub type_: XType,
    pub default: Option<Rc<XValue<'c>>>,
}

impl XExplicitFuncSpec<'_> {
    pub fn to_spec(&self) -> XFuncSpec {
        XFuncSpec {
            generic_params: self.generic_params.clone(),
            params: self.args.iter().map(|x| {
                XFuncParamSpec {
                    type_: Box::new(x.type_.clone()),
                    required: x.default.is_none(),
                }
            }).collect(),
            ret: Box::new(self.ret.clone()),
        }
    }
}

impl<'c> XStaticFunction<'c> {
    pub fn bind(&self, args: Vec<XType>) -> Option<HashMap<String, XType>> {
        match self {
            XStaticFunction::Native(spec, _) | XStaticFunction::Recourse(spec) => spec.bind(args),
            XStaticFunction::UserFunction(spec, _, _) => spec.to_spec().bind(args),
        }
    }
    pub fn rtype(&self, bind: &HashMap<String, XType>) -> XType {
        match self {
            XStaticFunction::Native(spec, _) | XStaticFunction::Recourse(spec) => spec.rtype(bind),
            XStaticFunction::UserFunction(spec, _, _) => spec.to_spec().rtype(bind),
        }
    }
    pub fn is_generic(&self) -> bool {
        match self {
            XStaticFunction::Native(spec, _) | XStaticFunction::Recourse(spec) => spec.generic_params.is_some(),
            XStaticFunction::UserFunction(spec, _, _) => spec.generic_params.is_some(),
        }
    }
    pub fn xtype(&self, bind: &HashMap<String, XType>) -> XType {
        match self {
            XStaticFunction::Native(spec, _) | XStaticFunction::Recourse(spec) => spec.xtype(bind),
            XStaticFunction::UserFunction(spec, _, _) => spec.to_spec().xtype(bind),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum IdentItem<'c> {
    Value(XType),
    Function(XStaticFunction<'c>),
}

#[derive(Debug)]
pub enum TailedEvalResult<'c> {
    Value(Rc<XValue<'c>>),
    TailCall(Vec<Rc<XValue<'c>>>),
}

impl<'c> TailedEvalResult<'c> {
    pub fn unwrap_value(&self) -> Rc<XValue<'c>> {
        match self {
            TailedEvalResult::Value(v) => v.clone(),
            TailedEvalResult::TailCall(args) => panic!("TailedEvalResult::unwrap_value called on a tail call")
        }
    }
}
impl<'c> From<Rc<XValue<'c>>> for TailedEvalResult<'c> {
    fn from(v: Rc<XValue<'c>>) -> Self {
        TailedEvalResult::Value(v)
    }
}

impl<'c> From<XValue<'c>> for TailedEvalResult<'c> {
    fn from(v: XValue<'c>) -> Self {
        TailedEvalResult::Value(Rc::new(v))
    }
}

impl<'c> XExpr<'c> {
    pub fn xtype(&self) -> Result<XType, String> {
        match self {
            XExpr::LiteralBool(_) => Ok(XType::Bool),
            XExpr::LiteralInt(_) => Ok(XType::Int),
            XExpr::LiteralRational(_) => Ok(XType::Rational),
            XExpr::LiteralString(_) => Ok(XType::String),
            /*
            XExpr::Array(exprs) => {
                Ok(XType::XSeq(Box::new(common_type(exprs.iter().map(|x| x.xtype()))?)))
            }
            XExpr::Set(exprs) => {
                Ok(XType::XSet(Box::new(common_type(exprs.iter().map(|x| x.xtype()))?)))
            }
            XExpr::Map(exprs) => {
                Ok(XType::XMap(Box::new(common_type(exprs.iter().map(|x| x.0.xtype()))?),
                               Box::new(common_type(exprs.iter().map(|x| x.1.xtype()))?)))
            }
             */
            XExpr::Call(func, _) => {
                if let XExpr::KnownOverload(func, bind) = func.as_ref() {
                    return Ok(func.rtype(bind));
                }
                if let XType::XFunc(func) = func.xtype()? {
                    return Ok(func.rtype(&HashMap::new()));
                }
                Err(format!("Expected function type, got {:?}", func.xtype()?))
            }
            XExpr::Construct(spec, binding, _) => Ok(XType::XStruct(spec.clone(), binding.clone())),
            XExpr::Member(obj, idx) => {
                if let XType::XStruct(spec, bind) = obj.xtype()? {
                    Ok(spec.fields[*idx].type_.as_ref().resolve_bind(&bind))
                } else {
                    Err(format!("Expected struct type, got {:?}", obj.xtype()?))
                }
            }
            XExpr::KnownOverload(func, bind) => Ok(func.xtype(bind)),
            XExpr::Ident(_, item) => {
                match item.as_ref() {
                    IdentItem::Value(xtype) => Ok(xtype.clone()),
                    IdentItem::Function(func) => Ok(func.rtype(&HashMap::new())),
                }
            }
        }
    }

    pub fn eval<'p>(&self, namespace: &XEvaluationScope<'p, 'c>, tail_available: bool) -> Result<TailedEvalResult<'c>, String> where 'c : 'p{
        match &self {
            XExpr::LiteralBool(b) => Ok(XValue::Bool(*b).into()),
            XExpr::LiteralInt(i) => Ok(XValue::Int(BigInt::from(*i)).into()),
            XExpr::LiteralRational(r) => Ok(XValue::Rational(r.clone()).into()),
            XExpr::LiteralString(s) => Ok(XValue::String(s.clone()).into()),
            /*
            XExpr::Array(exprs) => {
                Ok(XValue::Sequence(exprs.iter().map(|x| x.eval(namespace)).collect::<Result<Vec<_>, _>>()?))
            }
            XExpr::Set(exprs) => {
                Ok(XValue::Set(XHashSet(
                    exprs.iter().map(|x| x.eval(namespace)).collect::<Result<HashSet<_>, _>>()?
                )))
            }
            XExpr::Map(exprs) => {
                Ok(XValue::Map(XHashMap(
                    exprs.iter().map(|(k,v)| Ok((k.eval(namespace)?, v.eval(namespace)?))).collect::<Result<HashMap<_,_>, String>>()?
                )))
            }*/
            XExpr::Call(func, args) => {
                let callable = func.eval(namespace, false)?.unwrap_value().clone();
                let ret;
                if let XValue::Function(xfunc) = callable.as_ref() {
                    ret = xfunc.eval(args, namespace, tail_available)?;
                }
                else {
                    return Err(format!("Expected function, got {:?}", callable))
                }
                return Ok(ret);
            }
            XExpr::Construct(_, _, args) => {
                let items = args.iter().map(|x| x.eval(namespace, false).map(|r|r.unwrap_value())).collect::<Result<Vec<_>, _>>()?;
                Ok(XValue::StructInstance(items).into())
            }
            XExpr::Member(obj, idx) => {
                let obj = obj.eval(namespace, false)?.unwrap_value();
                if let XValue::StructInstance(items) = obj.as_ref() {
                    Ok(items[*idx].clone().into())
                } else {
                    Err(format!("Expected struct, got {:?}", obj))
                }
            }
            XExpr::KnownOverload(func, _) => {
                Ok(XValue::Function(func.clone().into()).into())
            }
            XExpr::Ident(name, item) => {
                if let IdentItem::Function(func) = item.as_ref() {
                    Ok(XValue::Function(func.clone().into()).into())
                } else {
                    Ok(namespace.get(&name).ok_or_else(|| format!("Undefined identifier: {}", name))?.clone().into())
                }
            }
        }
    }
}