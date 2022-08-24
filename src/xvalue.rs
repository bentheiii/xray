use crate::evaluation_scope::{EvaluatedVariable, XEvaluationScope};
use crate::native_types::XNativeValue;
use crate::runtime::RTCell;
use crate::xexpr::{TailedEvalResult, XExpr, XStaticFunction};
use crate::{Identifier, XCompilationScope, XType};

use crate::util::lazy_bigint::LazyBigint;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::{Debug, Error, Formatter};
use std::mem::size_of;
use std::rc::Rc;
use std::sync::Arc;

#[derive(Debug)]
pub enum XValue {
    Int(LazyBigint),
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
    UserFunction(
        Rc<XStaticFunction>,
        Rc<HashMap<Identifier, EvaluatedVariable>>,
    ),
    Recourse(usize),
}

impl XFunction {
    pub(crate) fn eval<'p>(
        &'p self,
        args: &[XExpr],
        parent_scope: &XEvaluationScope<'p>,
        tail_available: bool,
        runtime: RTCell,
    ) -> Result<TailedEvalResult, String> {
        match self {
            Self::Native(native) => native(args, parent_scope, tail_available, runtime),
            Self::UserFunction(..) => {
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
            Self::Recourse(depth) => {
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

    pub(crate) fn eval_values<'p>(
        &'p self,
        args: &[Rc<ManagedXValue>],
        parent_scope: &XEvaluationScope<'p>,
        runtime: RTCell,
    ) -> Result<Rc<ManagedXValue>, String> {
        match self {
            Self::Native(native) => {
                // we need to wrap all the values with dummy expressions, so that native functions can handle them
                let args = args
                    .iter()
                    .map(|x| XExpr::Dummy(x.clone()))
                    .collect::<Vec<_>>();
                native(&args, parent_scope, false, runtime).map(|r| r.unwrap_value())
            }
            Self::UserFunction(func, closure) => {
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
                        for (&name, value) in closure.as_ref() {
                            scope.add_value(name, value.clone());
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
                        scope.add_value(name, Ok(arg.clone()));
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
                        scope.add_value(name, Ok(value.clone()));
                    }

                    for decl in &uf.declarations {
                        scope.add_from_declaration(decl, runtime.clone())?
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
            Self::Recourse(depth) => parent_scope.ancestor(*depth).recourse.unwrap().eval_values(
                args,
                parent_scope,
                runtime,
            ),
        }
    }
}

impl Debug for XFunction {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Self::Native(_) => {
                write!(f, "Native()")
            }
            Self::UserFunction(params, ..) => {
                write!(f, "UserFunction({:?})", params)
            }
            Self::Recourse(..) => {
                write!(f, "Recourse()")
            }
        }
    }
}

impl XValue {
    pub(crate) fn size(&self) -> usize {
        match self {
            Self::Int(i) => size_of::<LazyBigint>() + (i.bits() / 8_u64) as usize,
            Self::Float(_) => 64 / 8,
            Self::String(s) => s.len(),
            Self::Bool(_) => 1,
            Self::Function(XFunction::Native(_)) => size_of::<usize>(),
            Self::Function(XFunction::UserFunction(_, closure)) => {
                size_of::<usize>() + closure.len() * size_of::<usize>()
            }
            Self::Function(XFunction::Recourse(..)) => size_of::<usize>(),
            Self::StructInstance(items) => items.len() * size_of::<usize>(),
            Self::UnionInstance(_, item) => item.size + size_of::<usize>(),
            Self::Native(n) => size_of::<usize>() + n.size(),
        }
    }
}

pub struct ManagedXValue {
    runtime: RTCell,
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
    pub(crate) fn new(value: XValue, runtime: RTCell) -> Result<Rc<Self>, String> {
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
        Ok(Rc::new(Self {
            runtime,
            size,
            value,
        }))
    }
}
