use crate::evaluation_scope::{EvaluatedVariable};
use crate::native_types::XNativeValue;
use crate::runtime::RTCell;
use crate::xexpr::{TailedEvalResult, XExpr, XStaticFunction};
use crate::{Identifier, XFuncSpec, XType};

use crate::util::lazy_bigint::LazyBigint;
use derivative::Derivative;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::{Debug, Error, Formatter};
use std::io::Write;
use std::mem::size_of;
use std::rc::Rc;
use std::sync::Arc;
use crate::compilation_scopes::CompilationScope;
use crate::runtime_scope::{EvaluationCell, RuntimeScopeTemplate, RuntimeScope};

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub enum XValue<W: Write + 'static> {
    Int(LazyBigint),
    Float(f64),
    String(String),
    Bool(bool),
    Function(XFunction<W>),
    StructInstance(Vec<EvaluatedVariable<W>>),
    UnionInstance(usize, EvaluatedVariable<W>),
    Native(Box<dyn XNativeValue>),
}

// todo do these still have to be RCs? and pub? and dyn?
pub type NativeCallable<W> = Rc<
    dyn Fn(
        &[XExpr<W>],
        &RuntimeScope<'_, W>,
        bool,
        RTCell<W>,
    ) -> Result<TailedEvalResult<W>, String>,
>;
pub type DynBind<W> = Rc<
    dyn Fn(
        Option<&[XExpr<W>]>,
        Option<&[Arc<XType>]>,
        &mut CompilationScope<'_, W>,
        Option<&[Arc<XType>]>,
    ) -> Result<XFunctionFactoryOutput<W>, String>,
>; // todo make this a compilation error?

pub struct XFunctionFactoryOutput<W: Write + 'static> {
    pub(crate) spec: XFuncSpec,
    pub(crate) func: XStaticFunction<W>,
}

impl<W: Write + 'static> XFunctionFactoryOutput<W>{
    pub(crate) fn from_native(spec: XFuncSpec, callable: impl Fn(
        &[XExpr<W>],
        &RuntimeScope<'_, W>,
        bool,
        RTCell<W>,
    ) -> Result<TailedEvalResult<W>, String> + 'static)->Self{
        Self{
            spec,
            func: XStaticFunction::from_native(callable)
        }
    }
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub enum XFunction<W: Write + 'static> {
    Native(NativeCallable<W>),
    UserFunction {
        template: Rc<RuntimeScopeTemplate<W>>,
        // todo both the function and the template store these and I don't know why
        defaults: Vec<XExpr<W>>,
        output: Box<XExpr<W>>,
    },
}

impl<W: Write + 'static> Debug for XFunction<W> {
    // todo is this needed?
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Self::Native(..) => {
                write!(f, "Native(..)")
            }
            Self::UserFunction{template, ..} => {
                write!(f, "UserFunction({})", template.name.as_deref().unwrap_or(".."))
            }
        }
    }
}

pub(crate) fn size_of_value<W: Write + 'static>(v: &EvaluatedVariable<W>)->usize{
    match v {
        Err(e) => 0, // todo manage errors (and store their size)
        Ok(v) => v.size
    }
}

impl<W: Write + 'static> XValue<W> {
    pub(crate) fn size(&self) -> usize {
        match self {
            Self::Int(i) => size_of::<LazyBigint>() + (i.bits() / 8_u64) as usize,
            Self::Float(_) => 64 / 8,
            Self::String(s) => s.len(),
            Self::Bool(_) => 1,
            Self::Function(XFunction::Native(_)) => size_of::<usize>(),
            Self::Function(XFunction::UserFunction{template, ..}) => {
                size_of::<usize>() + template.cells.len() * size_of::<usize>()
            }
            Self::StructInstance(items) => items.len() * size_of::<usize>(),
            Self::UnionInstance(_, item) => size_of_value(item) + size_of::<usize>(),
            Self::Native(n) => size_of::<usize>() + n.size(),
        }
    }
}

pub struct ManagedXValue<W: Write + 'static> {
    runtime: RTCell<W>,
    size: usize,
    // this will be zero if the runtime has no size limit
    pub value: XValue<W>, // todo we need to manage errors too
}

impl<W: Write + 'static> Debug for ManagedXValue<W> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "ManagedXValue({:?})", self.value)
    }
}

impl<W: Write + 'static> Drop for ManagedXValue<W> {
    fn drop(&mut self) {
        self.runtime.borrow_mut().size -= self.size;
    }
}

impl<W: Write + 'static> ManagedXValue<W> {
    pub(crate) fn new(value: XValue<W>, runtime: RTCell<W>) -> Result<Rc<Self>, String> {
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
