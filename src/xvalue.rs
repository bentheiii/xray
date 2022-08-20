use crate::evaluation_scope::XEvaluationScope;
use crate::native_types::XNativeValue;
use crate::runtime::{RTCell};
use crate::xexpr::{TailedEvalResult, XExpr, XStaticFunction};
use crate::{Identifier, XCompilationScope, XType};
use num::BigInt;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::{Debug, Error, Formatter};
use std::mem::size_of;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::Arc;

#[derive(Debug)]
pub enum XValue {
    Int(BigInt),
    Float(f64),
    String(String),
    Bool(bool),
    Function(XFunction),
    StructInstance(Vec<Rc<ManagedXValue>>),
    UnionInstance(usize, Rc<ManagedXValue>),
    Native(Box<dyn XNativeValue>),
}

pub type NativeCallable =
    Rc<dyn Fn(&[XExpr], &XEvaluationScope<'_>, bool, RTCell) -> Result<TailedEvalResult, String>>;
pub type DynBind = Rc<
    dyn Fn(
        Option<&[XExpr]>,
        &[Arc<XType>],
        &XCompilationScope<'_>,
    ) -> Result<Rc<XStaticFunction>, String>,
>; // todo make this a compilation error?

#[derive(Clone)]
pub enum XFunction {
    Native(NativeCallable),
    UserFunction(Rc<XStaticFunction>, HashMap<Identifier, Rc<ManagedXValue>>),
    Recourse(usize),
}

impl XFunction {
    pub fn eval<'p>(
        &'p self,
        args: &Vec<XExpr>,
        parent_scope: &XEvaluationScope<'p>,
        tail_available: bool,
        runtime: RTCell,
    ) -> Result<TailedEvalResult, String> {
        match self {
            XFunction::Native(native) => native(args, parent_scope, tail_available, runtime),
            XFunction::UserFunction(..) => {
                let arguments = args
                    .iter()
                    .map(|x| {
                        x.eval(parent_scope, false, runtime.clone())
                            .map(|r| r.unwrap_value())
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                self.eval_values(&arguments, parent_scope, runtime)
                    .map(|r| r.into())
            }
            XFunction::Recourse(depth) => {
                if tail_available && *depth == 0 {
                    let arguments = args
                        .iter()
                        .map(|x| {
                            x.eval(parent_scope, false, runtime.clone())
                                .map(|r| r.unwrap_value())
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    return Ok(TailedEvalResult::TailCall(arguments));
                }
                parent_scope.ancestor(*depth).recourse.unwrap().eval(
                    args,
                    parent_scope,
                    tail_available,
                    runtime,
                )
            }
        }
    }

    pub fn eval_values<'p>(
        &'p self,
        args: &[Rc<ManagedXValue>],
        parent_scope: &XEvaluationScope<'p>,
        runtime: RTCell,
    ) -> Result<Rc<ManagedXValue>, String> {
        match self {
            XFunction::Native(native) => {
                // we need to wrap all the values with dummy expressions, so that native functions can handle them
                let args = args
                    .iter()
                    .map(|x| XExpr::Dummy(x.clone()))
                    .collect::<Vec<_>>();
                native(&args, parent_scope, false, runtime).map(|r| r.unwrap_value())
            }
            XFunction::UserFunction(func, closure) => {
                let mut args = Cow::Borrowed(args);
                let uf = match func.as_ref() {
                    XStaticFunction::UserFunction(uf) => uf,
                    _ => unreachable!(),
                };
                let mut recursion_depth = 0_usize;
                loop {
                    let closure_scope = if !closure.is_empty() {
                        let mut scope =
                            XEvaluationScope::from_parent(parent_scope, self, runtime.clone())?;
                        for (&name, value) in closure {
                            scope.add(name, value.clone());
                        }
                        Some(Box::new(scope))
                    } else {
                        None
                    };
                    let mut scope = XEvaluationScope::from_parent(
                        match closure_scope {
                            Some(ref scope) => scope,
                            None => parent_scope,
                        },
                        self,
                        runtime.clone(),
                    )?;
                    // explicit params
                    for (&name, arg) in uf.param_names.iter().zip(args.iter()) {
                        scope.add(name, arg.clone());
                    }
                    //default params
                    // we only want the defaults that haven't been specified
                    for (value, &name) in uf
                        .defaults
                        .iter()
                        .rev()
                        .zip(uf.param_names.iter().skip(args.len()).rev())
                        .rev()
                    {
                        scope.add(name, value.clone());
                    }
                    for (name, expr) in &uf.variable_declarations {
                        scope.add(
                            *name,
                            expr.eval(&scope, false, runtime.clone())?.unwrap_value(),
                        );
                    }
                    match uf.output.eval(&scope, true, runtime.clone())? {
                        TailedEvalResult::Value(value) => return Ok(value),
                        TailedEvalResult::TailCall(new_args) => {
                            args = Cow::Owned(new_args);
                            recursion_depth += 1;
                            if let Some(recursion_limit) = runtime.borrow().limits.recursion_limit {
                                if recursion_depth > recursion_limit {
                                    return Err(format!(
                                        "Recursion limit of {} exceeded",
                                        recursion_limit
                                    ));
                                }
                            }
                        }
                    }
                }
            }
            XFunction::Recourse(depth) => parent_scope
                .ancestor(*depth)
                .recourse
                .unwrap()
                .eval_values(args, parent_scope, runtime),
        }
    }
}

impl Debug for XFunction {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            XFunction::Native(_) => {
                write!(f, "Native()")
            }
            XFunction::UserFunction(params, ..) => {
                write!(f, "UserFunction({:?})", params)
            }
            XFunction::Recourse(..) => {
                write!(f, "Recourse()")
            }
        }
    }
}

impl PartialEq for XFunction {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl Eq for XFunction {}

impl XValue {
    pub fn size(&self) -> usize {
        match self {
            XValue::Int(i) => (i.bits() / 8) as usize,
            XValue::Float(_) => 64 / 8,
            XValue::String(s) => s.len(),
            XValue::Bool(_) => 1,
            XValue::Function(XFunction::Native(_)) => size_of::<usize>(),
            XValue::Function(XFunction::UserFunction(_, closure)) => {
                size_of::<usize>() + closure.len() * size_of::<usize>()
            }
            XValue::Function(XFunction::Recourse(..)) => size_of::<usize>(),
            XValue::StructInstance(items) => items.len() * size_of::<usize>(),
            XValue::UnionInstance(_, item) => item.size() + size_of::<usize>(),
            XValue::Native(n) => size_of::<usize>() + n.size(),
        }
    }
}

pub struct ManagedXValue {
    pub runtime: RTCell,
    size: usize, // this will be zero if the runtime has no size limit
    pub value: XValue,
}

impl Debug for ManagedXValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "ManagedXValue({:?})", self.value)
    }
}

impl Drop for ManagedXValue {
    fn drop(&mut self) {
        self.runtime.borrow_mut().size -= self.size;
    }
}

impl ManagedXValue {
    pub fn new(value: XValue, runtime: RTCell) -> Result<Rc<ManagedXValue>, String> {
        let size;
        {
            let size_limit = runtime.borrow().limits.size_limit;
            if let Some(max_size) = size_limit {
                size = value.size();
                runtime.borrow_mut().size += size;
                if runtime.borrow().size > max_size {
                    return Err(format!(
                        "Size limit exceeded: {} bytes",
                        runtime.borrow().size
                    ));
                }
            } else {
                size = 0;
            }
        }
        Ok(Rc::new(ManagedXValue {
            runtime,
            size,
            value,
        }))
    }
}

impl Deref for ManagedXValue {
    type Target = XValue;
    fn deref(&self) -> &XValue {
        &self.value
    }
}
