use std::fmt::{Debug, Error, Formatter};
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use num::{BigInt, BigRational};
use crate::native_types::XNativeValue;
use crate::xexpr::{TailedEvalResult, XExpr, XStaticFunction};
use crate::xscope::{Declaration, XEvaluationScope};

#[derive(Hash, Debug, Eq, PartialEq)]
pub enum XValue {
    Int(BigInt),
    Rational(BigRational),
    String(String),
    Bool(bool),
    Function(XFunction),
    /*Sequence(Vec<XValue>),
    Set(XHashSet),
    Map(XHashMap),*/
    StructInstance(Vec<Rc<XValue>>),
    Native(Box<dyn XNativeValue>),
}

pub type NativeCallable = fn(&Vec<XExpr>, &XEvaluationScope<'_>, bool) -> Result<TailedEvalResult, String>;

#[derive(Clone)]
pub enum XFunction {
    Native(NativeCallable),
    UserFunction(Vec<String>, Vec<(String, XExpr)>, Box<XExpr>),
    Recourse(),
}

impl XFunction {
    pub fn eval<'p>(&'p self, args: &Vec<XExpr>, parent_scope: &XEvaluationScope<'p>, tail_available: bool) -> Result<TailedEvalResult, String> {
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
                    for (name, expr) in declarations {
                        scope.add(&name, expr.eval(&scope, false)?.unwrap_value());
                    }
                    match output.eval(&scope, true)? {
                        TailedEvalResult::Value(value) => return Ok(value.into()),
                        TailedEvalResult::TailCall(new_args) => {
                            arguments = new_args;
                        }
                    }
                }
            }
            XFunction::Recourse() => {
                if tail_available {
                    let arguments = args.iter().map(|x| x.eval(parent_scope, false).map(|r| r.unwrap_value())).collect::<Result<Vec<_>, _>>()?;
                    return Ok(TailedEvalResult::TailCall(arguments));
                }
                parent_scope.recourse.unwrap().eval(args, parent_scope, tail_available)
            }
        }
    }
}

impl<'c> From<XStaticFunction> for XFunction {
    fn from(stat: XStaticFunction) -> Self {
        match stat {
            XStaticFunction::Native(_, native) => XFunction::Native(native),
            XStaticFunction::UserFunction(specs, declarations, output) => XFunction::UserFunction(specs.args.iter().map(|p| p.name.clone()).collect(),
                                                                                                  declarations.iter().filter_map(|decl|{
                                                                                                      if let Declaration::Value(name, expr) = decl {
                                                                                                          Some((name.clone(), expr.clone()))
                                                                                                      } else {
                                                                                                          None
                                                                                                      }
                                                                                                  }).collect(),
                                                                                                  output),
            XStaticFunction::Recourse(_) => XFunction::Recourse(),
        }
    }
}

impl Debug for XFunction {
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

impl PartialEq for XFunction {
    fn eq(&self, _: &Self) -> bool {
        return false;
    }
}

impl Eq for XFunction {}

impl Hash for XFunction {
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