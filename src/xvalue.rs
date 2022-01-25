use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::collections::hash_map::DefaultHasher;
use std::fmt::{Debug, Error, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use num::{BigInt, BigRational};
use crate::xexpr::{XExpr, XStaticExpr};
use crate::xscope::{Declaration, XEvaluationScope};
use crate::xtype::{common_type, X_BOOL, X_INT, X_RATIONAL, X_STRING, X_UNKNOWN, XFuncParamSpec, XFuncSpec, XType};

#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum XValue {
    Int(BigInt),
    Rational(BigRational),
    String(String),
    Bool(bool),
    Sequence(Vec<XValue>),
    Function(XFunction),
    Set(XHashSet),
    Map(XHashMap),
    StructInstance(Vec<XValue>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct XHashSet(pub HashSet<XValue>);

impl Hash for XHashSet {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let mut p_hash: u64 = 0;  // todo improve?
        for element in &self.0 {
            let mut p_hasher = DefaultHasher::new();
            element.hash(&mut p_hasher);
            p_hash ^= p_hasher.finish();
        }
        p_hash.hash(state);
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct XHashMap(pub HashMap<XValue, XValue>);

impl Hash for XHashMap {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let mut p_hash: u64 = 0;  // todo improve?
        for pair in &self.0 {
            let mut p_hasher = DefaultHasher::new();
            pair.hash(&mut p_hasher);
            p_hash ^= p_hasher.finish();
        }
        p_hash.hash(state);
    }
}

#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct XExplicitFuncSpec {
    pub generic_params: Option<Vec<String>>,
    pub args: Vec<XExplicitArgSpec>,
    pub ret: XType,
}

#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub struct XExplicitArgSpec {
    pub name: String,
    pub type_: XType,
    pub default: Option<XValue>,
}

impl XExplicitFuncSpec {
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

#[derive(Clone)]
pub enum XFunction {
    Native(XFuncSpec, fn(Vec<XExpr>, &XEvaluationScope) -> Result<XValue, String>),
    UserFunction(XExplicitFuncSpec, Vec<Declaration>, Box<XExpr>),
    Recourse(XFuncSpec),
}

impl XFunction {
    pub fn bind(&self, args: Vec<XType>) -> Option<HashMap<String, XType>> {
        match self {
            XFunction::Native(spec, _) | XFunction::Recourse(spec) => spec.bind(args),
            XFunction::UserFunction(spec, _, _) => spec.to_spec().bind(args),
        }
    }
    pub fn rtype(&self, bind: &HashMap<String, XType>) -> XType {
        match self {
            XFunction::Native(spec, _) | XFunction::Recourse(spec)=> spec.rtype(bind),
            XFunction::UserFunction(spec, _, _) => spec.to_spec().rtype(bind),
        }
    }
    pub fn is_generic(&self) -> bool {
        match self {
            XFunction::Native(spec, _)| XFunction::Recourse(spec) => spec.generic_params.is_some(),
            XFunction::UserFunction(spec, _, _) => spec.generic_params.is_some(),
        }
    }
    pub fn xtype(&self, bind: &HashMap<String, XType>) -> XType {
        match self {
            XFunction::Native(spec, _) | XFunction::Recourse(spec)=> spec.xtype(bind),
            XFunction::UserFunction(spec, _, _) => spec.to_spec().xtype(bind),
        }
    }

    pub fn eval<'s>(&self, args: &Vec<XExpr>, parent_scope: &XEvaluationScope<'s>) -> Result<XValue, String> {
        match self {
            XFunction::Native(_, native) => {
                native(args.clone(), parent_scope)
            }
            XFunction::UserFunction(specs, declarations, output) => {
                let arguments = args.iter().map(|x| x.eval(parent_scope)).collect::<Result<Vec<_>, _>>()?;
                let mut scope = XEvaluationScope::from_parent(parent_scope.clone(), &self);
                for (spec, arg) in specs.args.iter().zip(arguments.iter()) {
                    scope.add(&spec.name, arg.clone());
                }
                for declaration in declarations {
                    if let Declaration::Value(name, expr) = declaration {
                        scope.add(&name, expr.eval(&scope)?);
                    }
                }
                output.eval(&scope)
            }
            XFunction::Recourse(_) => parent_scope.recourse.unwrap().eval(args, parent_scope),
        }
    }
}

impl Debug for XFunction {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            XFunction::Native(spec, _) => {
                write!(f, "Native({:?})", spec)
            }
            XFunction::UserFunction(spec, _, _) => {
                write!(f, "UserFunction({:?})", spec)
            }
            XFunction::Recourse(spec) => {
                write!(f, "Recourse({:?})", spec)
            }
        }
    }
}

impl PartialEq for XFunction {
    fn eq(&self, other: &Self) -> bool {
        return false
    }
}

impl Eq for XFunction {}

impl Hash for XFunction {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            XFunction::Native(spec, _) => {
                spec.hash(state)
            }
            XFunction::UserFunction(spec, _, _) => {
                spec.hash(state)
            }
            XFunction::Recourse(spec) => {
                spec.hash(state)
            }
        }
    }
}