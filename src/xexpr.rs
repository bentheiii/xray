use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Error, Formatter};
use std::rc::Rc;
use std::sync::Arc;
use num::{BigInt, BigRational};
use crate::builtin::array::{XArray, XArrayType};
use crate::builtin::set::{XSet, XSetType};
use crate::XFuncSpec;
use crate::xscope::{Declaration, XCompilationScope, XCompilationScopeItem, XEvaluationScope};
use crate::xtype::{Bind, common_type, X_BOOL, X_INT, X_RATIONAL, X_STRING, XFuncParamSpec, XStructSpec, XType};
use crate::xvalue::{NativeCallable, XFunction, XValue};
use derivative::Derivative;

#[derive(Debug, Clone, Derivative)]
#[derivative(Hash)]
pub enum XStaticExpr {
    LiteralBool(bool),
    LiteralInt(i64),
    LiteralRational(BigRational),
    LiteralString(String),
    Array(Vec<XStaticExpr>),
    Set(Vec<XStaticExpr>),
    /*
    Map(Vec<(XStaticExpr, XStaticExpr)>),
    */
    Call(Box<XStaticExpr>, Vec<XStaticExpr>),
    Member(Box<XStaticExpr>, String),
    Ident(String),
    SpecializedIdent(String,
                     #[derivative(Hash = "ignore")]
                     Vec<Arc<XType>>),
}

impl XStaticExpr {
    pub fn new_call(name: &str, args: Vec<XStaticExpr>) -> XStaticExpr {
        XStaticExpr::Call(Box::new(XStaticExpr::Ident(name.to_string())), args)
    }

    pub fn compile<'p, 's: 'p>(&self, namespace: &'p XCompilationScope<'p>) -> Result<XExpr, String> {
        fn resolve_overload(overloads: Vec<XStaticFunction>, arg_types: &Vec<Arc<XType>>) -> Option<XExpr> {
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
            called
        }

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
            XStaticExpr::Array(items) => Ok(XExpr::Array(items.iter().map(|x| x.compile(namespace)).collect::<Result<Vec<_>, _>>()?)),
            XStaticExpr::Set(items) => Ok(XExpr::Set(items.iter().map(|x| x.compile(namespace)).collect::<Result<Vec<_>, _>>()?)),
            XStaticExpr::Call(func, args) => {
                let compiled_args = args.iter().map(|x| x.compile(namespace)).collect::<Result<Vec<_>, _>>()?;
                match func.as_ref() {
                    XStaticExpr::Ident(name) => {
                        match namespace.get(name) {
                            Some(XCompilationScopeItem::Overload(overloads)) => {
                                let arg_types = compiled_args.iter().map(|x| x.xtype()).collect::<Result<Vec<_>, _>>()?;
                                let called = resolve_overload(overloads, &arg_types);
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
                                } else {
                                    Err(format!("Struct {} does not take arguments {:?}", spec.name, arg_types))
                                };
                            }
                            _ => {}
                        }
                    }
                    XStaticExpr::SpecializedIdent(name, arg_types) =>{
                        let actual_arg_types = compiled_args.iter().map(|x| x.xtype()).collect::<Result<Vec<_>, _>>()?;
                        if arg_types != &actual_arg_types {
                            return Err(format!("Specialized function {} takes arguments {:?}, but {:?} were given", name, arg_types, actual_arg_types));
                        }
                        return match namespace.get(name) {
                            Some(XCompilationScopeItem::Overload(overloads)) => {
                                let called = resolve_overload(overloads, arg_types);
                                match called {
                                    Some(x) => Ok(XExpr::Call(Box::new(x), compiled_args)),
                                    None => Err(format!("No overload for {} with arguments {:?}", name, arg_types))
                                }
                            }
                            Some(_) => {
                                Err(format!("Non-function {} cannot be specialized", name))
                            }
                            None => {
                                Err(format!("No such function {}", name))
                            }
                        }
                    }
                    _ => {}
                }
                let func_compiled = func.compile(namespace)?;
                Ok(XExpr::Call(Box::new(func_compiled), compiled_args))
            }
            XStaticExpr::Member(obj, member_name) => {
                let obj_compiled = obj.compile(namespace)?;
                match obj_compiled.xtype()?.as_ref() {
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
            XStaticExpr::SpecializedIdent(name, arg_types) => {
                match namespace.get(name) {
                    Some(XCompilationScopeItem::Overload(overloads)) => {
                        let called = resolve_overload(overloads, arg_types);
                        match called {
                            Some(overload) => Ok(overload),
                            None => Err(format!("No overload found for {} with types {:?}", name, arg_types))
                        }
                    }
                    None => Err(format!("Undefined identifier: {}", name)),
                    Some(_) => Err(format!("Non-function {} cannot be specialized", name)),
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
    Construct(XStructSpec, Bind, Vec<XExpr>),
    Member(Box<XExpr>, usize),
    KnownOverload(XStaticFunction, Bind),
    Ident(String, Box<IdentItem>),
    // this dummy exists for calling native functions with arguments that were already
    // evaluated
    Dummy(Rc<XValue>),
}

#[derive(Clone)]
pub enum XStaticFunction {
    Native(XFuncSpec, NativeCallable),
    UserFunction(XExplicitFuncSpec, Vec<Declaration>, Box<XExpr>),
    Recourse(XFuncSpec),
}

impl XStaticFunction {
    pub fn to_function(self, closure: HashMap<String, Rc<XValue>>) -> XFunction {
        match self {
            XStaticFunction::Native(_, native) => XFunction::Native(native),
            XStaticFunction::UserFunction(specs, declarations, output) => XFunction::UserFunction(specs.args.iter().map(|p| p.name.clone()).collect(),
                                                                                                  declarations.iter().filter_map(|decl|{
                                                                                                      if let Declaration::Value(name, expr) = decl {
                                                                                                          Some((name.clone(), expr.clone()))
                                                                                                      } else {
                                                                                                          None
                                                                                                      }
                                                                                                  }).collect(),
                                                                                                  output, closure),
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
            XStaticFunction::UserFunction(spec, _, _) => {
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
    pub generic_params: Option<Vec<String>>,
    pub args: Vec<XExplicitArgSpec>,
    #[derivative(Hash = "ignore")]
    pub ret: Arc<XType>,
}

#[derive(Debug, Clone, Eq, PartialEq, Derivative)]
#[derivative(Hash)]
pub struct XExplicitArgSpec {
    pub name: String,
    #[derivative(Hash = "ignore")]
    pub type_: Arc<XType>,
    pub default: Option<Rc<XValue>>,
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
    pub fn bind(&self, args: Vec<Arc<XType>>) -> Option<Bind> {
        match self {
            XStaticFunction::Native(spec, _) | XStaticFunction::Recourse(spec) => spec.bind(args),
            XStaticFunction::UserFunction(spec, _, _) => spec.to_spec().bind(args),
        }
    }
    pub fn rtype(&self, bind: &Bind) -> Arc<XType> {
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
    pub fn xtype(&self, bind: &Bind) -> Arc<XType> {
        match self {
            XStaticFunction::Native(spec, _) | XStaticFunction::Recourse(spec) => spec.xtype(bind),
            XStaticFunction::UserFunction(spec, _, _) => spec.to_spec().xtype(bind),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum IdentItem {
    Value(Arc<XType>),
    Function(XStaticFunction),
}

#[derive(Debug)]
pub enum TailedEvalResult {
    Value(Rc<XValue>),
    TailCall(Vec<Rc<XValue>>),
}

impl TailedEvalResult {
    pub fn unwrap_value(&self) -> Rc<XValue> {
        match self {
            TailedEvalResult::Value(v) => v.clone(),
            TailedEvalResult::TailCall(_) => panic!("TailedEvalResult::unwrap_value called on a tail call")
        }
    }
}

impl From<Rc<XValue>> for TailedEvalResult {
    fn from(v: Rc<XValue>) -> Self {
        TailedEvalResult::Value(v)
    }
}

impl From<XValue> for TailedEvalResult {
    fn from(v: XValue) -> Self {
        TailedEvalResult::Value(Rc::new(v))
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
                Ok(XType::XNative(Box::new(XArrayType {}), [("T".to_string(), element_type)].into()).into())
            }
            XExpr::Set(exprs) => {
                let element_type = common_type(exprs.iter().map(|x| x.xtype()))?;
                Ok(XType::XNative(Box::new(XSetType {}), [("T".to_string(), element_type)].into()).into())
            }
            XExpr::Call(func, _) => {
                if let XExpr::KnownOverload(func, bind) = func.as_ref() {
                    return Ok(func.rtype(bind));
                }
                if let XType::XCallable(spec) = func.xtype()?.as_ref() {
                    return Ok(spec.return_type.clone());
                }
                if let XType::XFunc(func) = func.xtype()?.as_ref() {
                    return Ok(func.rtype(&HashMap::new()));
                }
                Err(format!("Expected function type, got {:?}", func.xtype()?))
            }
            XExpr::Construct(spec, binding, _) => Ok(Arc::new(XType::XStruct(spec.clone(), binding.clone()))),
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
                    IdentItem::Function(func) => Ok(func.xtype(&HashMap::new())),
                }
            }
            XExpr::Dummy(_) => unreachable!(),
        }
    }

    pub fn eval<'p>(&self, namespace: &XEvaluationScope<'p>, tail_available: bool) -> Result<TailedEvalResult, String> {
        match &self {
            XExpr::LiteralBool(b) => Ok(XValue::Bool(*b).into()),
            XExpr::LiteralInt(i) => Ok(XValue::Int(BigInt::from(*i)).into()),
            XExpr::LiteralRational(r) => Ok(XValue::Rational(r.clone()).into()),
            XExpr::LiteralString(s) => Ok(XValue::String(s.clone()).into()),
            XExpr::Array(exprs) => {
                Ok(XValue::Native(Box::new(XArray::new(
                    exprs.iter().map(|x| x.eval(namespace, false).map(|r| r.unwrap_value())).collect::<Result<Vec<_>, _>>()?))).into()
                )
            }
            XExpr::Set(exprs) => {
                Ok(XValue::Native(Box::new(XSet::new(
                    exprs.iter().map(|x| x.eval(namespace, false).map(|r| r.unwrap_value())).collect::<Result<HashSet<_>, _>>()?))).into()
                )
            }
            XExpr::Call(func, args) => {
                let callable = func.eval(namespace, false)?.unwrap_value().clone();
                let ret;
                if let XValue::Function(xfunc) = callable.as_ref() {
                    ret = xfunc.eval(args, namespace, tail_available)?;
                } else {
                    return Err(format!("Expected function, got {:?}", callable));
                }
                return Ok(ret);
            }
            XExpr::Construct(_, _, args) => {
                let items = args.iter().map(|x| x.eval(namespace, false).map(|r| r.unwrap_value())).collect::<Result<Vec<_>, _>>()?;
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
                Ok(XValue::Function(func.clone().to_function(namespace.to_closure())).into())
            }
            XExpr::Ident(name, item) => {
                if let IdentItem::Function(func) = item.as_ref() {
                    Ok(XValue::Function(func.clone().to_function(namespace.to_closure())).into())
                } else {
                    Ok(namespace.get(&name).ok_or_else(|| format!("Undefined identifier: {}", name))?.clone().into())
                }
            }
            XExpr::Dummy(val) => Ok(val.clone().into()),
        }
    }
}