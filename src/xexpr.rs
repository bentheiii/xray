use std::borrow::BorrowMut;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use num::{BigInt, BigRational};
use crate::xexpr::XExpr::{LiteralBool, LiteralInt};
use crate::XFuncSpec;
use crate::xscope::{Declaration, XCompilationScope, XCompilationScopeItem, XEvaluationScope};
use crate::xtype::{common_type, X_BOOL, X_INT, X_RATIONAL, X_STRING, XStructSpec, XType};
use crate::xvalue::{XFunction, XHashMap, XHashSet, XValue};

#[derive(Hash, Debug, Clone)]
pub enum XStaticExpr {
    LiteralBool(bool),
    LiteralInt(i64),
    LiteralRational(BigRational),
    LiteralString(String),
    Array(Vec<XStaticExpr>),
    Set(Vec<XStaticExpr>),
    Map(Vec<(XStaticExpr, XStaticExpr)>),
    Call(Box<XStaticExpr>, Vec<XStaticExpr>),
    Member(Box<XStaticExpr>, String),
    Ident(String),
}

impl XStaticExpr {
    pub fn new_call(name: &str, args: Vec<XStaticExpr>) -> XStaticExpr {
        XStaticExpr::Call(Box::new(XStaticExpr::Ident(name.to_string())), args)
    }

    pub fn compile(&self, namespace: &XCompilationScope) -> Result<XExpr, String> {
        match self {
            XStaticExpr::LiteralBool(v) => Ok(XExpr::LiteralBool(*v)),
            XStaticExpr::LiteralInt(v) => Ok(XExpr::LiteralInt(*v)),
            XStaticExpr::LiteralRational(v) => Ok(XExpr::LiteralRational(v.clone())),
            XStaticExpr::LiteralString(v) => Ok(XExpr::LiteralString(v.clone())),
            XStaticExpr::Array(items) => Ok(XExpr::Array(items.iter().map(|x| x.compile(namespace)).collect::<Result<Vec<_>, _>>()?)),
            XStaticExpr::Set(items) => Ok(XExpr::Set(items.iter().map(|x| x.compile(namespace)).collect::<Result<Vec<_>, _>>()?)),
            XStaticExpr::Map(items) => Ok(XExpr::Map(
                items.iter().map(|(x, y)| Ok((x.compile(namespace)?, y.compile(namespace)?))).collect::<Result<Vec<_>, String>>()?,
            )),
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
                            let mut arg_types = compiled_args.iter().map(|x| x.xtype()).collect::<Result<Vec<_>, _>>()?;
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
                                        Ok(XExpr::Ident(name.to_string(), Box::new(IdentItem::Function(overload.clone()))))
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
pub enum XExpr {
    LiteralBool(bool),
    LiteralInt(i64),
    LiteralRational(BigRational),
    LiteralString(String),
    Array(Vec<XExpr>),
    Set(Vec<XExpr>),
    Map(Vec<(XExpr, XExpr)>),
    Call(Box<XExpr>, Vec<XExpr>),
    Construct(XStructSpec, HashMap<String, XType>, Vec<XExpr>),
    Member(Box<XExpr>, usize),
    KnownOverload(XFunction, HashMap<String, XType>),
    Ident(String, Box<IdentItem>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum IdentItem {
    Value(XType),
    Function(XFunction),
}

impl XExpr {
    pub fn xtype(&self) -> Result<XType, String> {
        match self {
            XExpr::LiteralBool(_) => Ok(XType::Bool),
            XExpr::LiteralInt(_) => Ok(XType::Int),
            XExpr::LiteralRational(_) => Ok(XType::Rational),
            XExpr::LiteralString(_) => Ok(XType::String),
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

    pub fn eval(&self, namespace: &XEvaluationScope<'_>) -> Result<XValue, String> {
        match self {
            XExpr::LiteralBool(b) => Ok(XValue::Bool(*b)),
            XExpr::LiteralInt(i) => Ok(XValue::Int(BigInt::from(*i))),
            XExpr::LiteralRational(r) => Ok(XValue::Rational(r.clone())),
            XExpr::LiteralString(s) => Ok(XValue::String(s.clone())),
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
            }
            XExpr::Call(func, args) => {
                let callable = func.eval(namespace)?;
                if let XValue::Function(xfunc) = callable {
                    xfunc.eval(args, namespace)
                }
                else {
                    Err(format!("Expected function, got {:?}", callable))
                }
            }
            XExpr::Construct(_, _, args) => {
                let items = args.iter().map(|x| x.eval(namespace)).collect::<Result<Vec<_>, _>>()?;
                Ok(XValue::StructInstance(items))
            }
            XExpr::Member(obj, idx) => {
                let obj = obj.eval(namespace)?;
                if let XValue::StructInstance(items) = obj {
                    Ok(items[*idx].clone())
                } else {
                    Err(format!("Expected struct, got {:?}", obj))
                }
            }
            XExpr::KnownOverload(func, _) => {
                Ok(XValue::Function(func.clone()))
            }
            XExpr::Ident(name, item) => {
                if let IdentItem::Function(func) = item.as_ref() {
                    Ok(XValue::Function(func.clone()))
                } else {
                    Ok(namespace.get(name).ok_or_else(|| format!("Undefined identifier: {}", name))?.clone())
                }
            }
        }
    }
}