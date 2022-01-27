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
pub enum XValue<'c> {
    Int(BigInt),
    Rational(BigRational),
    String(String),
    Bool(bool),
    Function(XFunction<'c>),
    /*Sequence(Vec<XValue>),
    Set(XHashSet),
    Map(XHashMap),*/
    StructInstance(Vec<XValue<'c>>),
}

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
    pub default: Option<XValue<'c>>,
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

#[derive(Clone)]
pub enum XFunction<'c> {
    Native(XFuncSpec, fn(&Vec<XExpr<'c>>, &XEvaluationScope<'_, 'c>) -> Result<XValue<'c>, String>),
    UserFunction(XExplicitFuncSpec<'c>, Vec<Declaration<'c>>, Box<XExpr<'c>>),
    Recourse(XFuncSpec),
}

impl<'c> XFunction<'c> {
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

    pub fn eval<'p>(&'p self, args: &Vec<XExpr<'c>>, parent_scope: &XEvaluationScope<'p, 'c>) -> Result<XValue<'c>, String> {
        match self {
            XFunction::Native(_, native) => {
                native(args, parent_scope)
            }
            XFunction::UserFunction(specs, declarations, output) => {
                let arguments = args.iter().map(|x| x.eval(parent_scope)).collect::<Result<Vec<_>, _>>()?;
                let mut scope = XEvaluationScope::from_parent(parent_scope, &self);
                for (spec, arg) in specs.args.iter().zip(arguments.iter()) {
                    scope.add(&spec.name, arg.clone());
                }
                for declaration in declarations {
                    if let Declaration::Value(name, expr) = declaration {
                        scope.add(&name, expr.eval(&scope)?);
                    }
                }
                output.eval(&scope).clone()
            }
            XFunction::Recourse(_) => parent_scope.recourse.unwrap().eval(args, parent_scope),
        }
    }
}

impl Debug for XFunction<'_> {
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

impl PartialEq for XFunction<'_> {
    fn eq(&self, other: &Self) -> bool {
        return false
    }
}

impl Eq for XFunction<'_> {}

impl Hash for XFunction<'_> {
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