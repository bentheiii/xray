use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::collections::hash_map::DefaultHasher;
use std::fmt::{Debug, Error, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use num::{BigInt, BigRational};
use crate::xexpr::{TailedEvalResult, XExplicitFuncSpec, XExpr, XStaticExpr, XStaticFunction};
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
    StructInstance(Vec<Rc<XValue<'c>>>),
}

pub type NativeCallable<'c> = fn(&Vec<XExpr<'c>>, &XEvaluationScope<'_, 'c>, bool) -> Result<TailedEvalResult<'c>, String>;

#[derive(Clone)]
pub enum XFunction<'c> {
    Native(NativeCallable<'c>),
    UserFunction(Vec<String>, Vec<Declaration<'c>>, Box<XExpr<'c>>),
    Recourse(),
}

impl<'c> XFunction<'c> {
    pub fn eval<'p>(&'p self, args: &Vec<XExpr<'c>>, parent_scope: &XEvaluationScope<'p, 'c>, tail_available: bool) -> Result<TailedEvalResult<'c>, String> {
        match self {
            XFunction::Native(native) => {
                native(args, parent_scope, tail_available)
            }
            XFunction::UserFunction(params, declarations, output) => {
                let mut arguments = args.iter().map(|x| x.eval(parent_scope, false).map(|r| r.unwrap_value())).collect::<Result<Vec<_>, _>>()?;
                loop {
                    let mut scope = XEvaluationScope::from_parent(parent_scope, &self);
                    for (name, arg) in params.iter().zip(arguments.iter()) {
                        scope.add(name, arg.clone());
                    }
                    for declaration in declarations {
                        if let Declaration::Value(name, expr) = declaration {
                            scope.add(&name, expr.eval(&scope, false)?.unwrap_value());
                        }
                    }
                    match output.eval(&scope, true)?{
                        TailedEvalResult::Value(value) => return Ok(value.into()),
                        TailedEvalResult::TailCall(new_args) => {
                            arguments = new_args;
                        }
                    }
                }
            }
            XFunction::Recourse() => {
                if tail_available{
                    let arguments = args.iter().map(|x| x.eval(parent_scope, false).map(|r| r.unwrap_value())).collect::<Result<Vec<_>, _>>()?;
                    return Ok(TailedEvalResult::TailCall(arguments));
                }
                parent_scope.recourse.unwrap().eval(args, parent_scope, tail_available)
            },
        }
    }
}

impl<'c> From<XStaticFunction<'c>> for XFunction<'c>{
    fn from(stat: XStaticFunction<'c>) -> Self {
        match stat {
            XStaticFunction::Native(_, native) => XFunction::Native(native),
            XStaticFunction::UserFunction(specs, declarations, output) => XFunction::UserFunction(specs.args.iter().map(|p| p.name.clone()).collect(), declarations, output),
            XStaticFunction::Recourse(_) => XFunction::Recourse(),
        }
    }
}

impl Debug for XFunction<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            XFunction::Native(_) => {
                write!(f, "Native()")
            }
            XFunction::UserFunction(params, _, _) => {
                write!(f, "UserFunction({:?})", params)
            }
            XFunction::Recourse() => {
                write!(f, "Recourse()")
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
            XFunction::Native(_) => {
                0.hash(state)
            }
            XFunction::UserFunction(args, _, _) => {
                args.hash(state)
            }
            XFunction::Recourse() => {
                2.hash(state)
            }
        }
    }
}