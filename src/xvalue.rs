use crate::native_types::XNativeValue;
use crate::root_runtime_scope::EvaluatedValue;
use crate::runtime::RTCell;
use crate::xexpr::{TailedEvalResult, XExpr, XStaticFunction};
use crate::{XFuncSpec, XType};

use crate::util::lazy_bigint::LazyBigint;
use derivative::Derivative;

use crate::compilation_scope::CompilationScope;
use crate::runtime_scope::{RuntimeScope, RuntimeScopeTemplate};
use crate::runtime_violation::RuntimeViolation;
use crate::util::fenced_string::FencedString;
use std::fmt::{Debug, Error, Formatter};
use std::io::Write;
use std::mem::size_of;
use std::rc::Rc;
use std::sync::Arc;

const VERBOSE_ALLOC: bool = false;

pub(crate) type UnionInstance<W> = (usize, Rc<ManagedXValue<W>>);

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub enum XValue<W> {
    Int(LazyBigint),
    Float(f64),
    String(Box<FencedString>),
    Bool(bool),
    Function(XFunction<W>),
    StructInstance(Vec<Rc<ManagedXValue<W>>>),
    UnionInstance(UnionInstance<W>),
    Native(Box<dyn XNativeValue>),
}

pub(crate) type NativeCallback<W> = dyn Fn(
    &[XExpr<W>],
    &RuntimeScope<'_, W>,
    bool,
    RTCell<W>,
) -> Result<TailedEvalResult<W>, RuntimeViolation>;

type DynBindCallback<W> = dyn Fn(
    Option<&[XExpr<W>]>,
    Option<&[Arc<XType>]>,
    &mut CompilationScope<'_, W>,
    Option<&[Arc<XType>]>,
) -> Result<XFunctionFactoryOutput<W>, String>;
pub(crate) type DynEvalCallback<W> = Rc<
    dyn Fn(
        &RuntimeScope<W>,
        RTCell<W>,
    ) -> Result<Result<XStaticFunction<W>, Rc<ManagedXError<W>>>, RuntimeViolation>,
>;

pub type NativeCallable<W> = Rc<NativeCallback<W>>;
pub type DynBind<W> = Rc<DynBindCallback<W>>;

pub struct XFunctionFactoryOutput<W> {
    pub(crate) spec: XFuncSpec,
    pub(crate) func: DynEvalCallback<W>,
}

impl<W: Write + 'static> XFunctionFactoryOutput<W> {
    pub(crate) fn from_native(
        spec: XFuncSpec,
        callable: impl Fn(
                &[XExpr<W>],
                &RuntimeScope<'_, W>,
                bool,
                RTCell<W>,
            ) -> Result<TailedEvalResult<W>, RuntimeViolation>
            + Clone
            + 'static,
    ) -> Self {
        Self {
            spec,
            func: Rc::new(move |_, _| Ok(Ok(XStaticFunction::from_native(callable.clone())))),
        }
    }

    pub(crate) fn from_delayed_native<F>(
        spec: XFuncSpec,
        callable: impl Fn(
                &RuntimeScope<W>,
                RTCell<W>,
            ) -> Result<Result<F, Rc<ManagedXError<W>>>, RuntimeViolation>
            + 'static,
    ) -> Self
    where
        F: Fn(
                &[XExpr<W>],
                &RuntimeScope<'_, W>,
                bool,
                RTCell<W>,
            ) -> Result<TailedEvalResult<W>, RuntimeViolation>
            + 'static,
    {
        Self {
            spec,
            func: Rc::new(move |ns, rt| {
                Ok(Ok(XStaticFunction::from_native(match callable(ns, rt)? {
                    Ok(v) => v,
                    Err(e) => return Ok(Err(e)),
                })))
            }),
        }
    }
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub enum XFunction<W> {
    Native(NativeCallable<W>),
    UserFunction {
        template: Rc<RuntimeScopeTemplate<W>>,
        output: Box<XExpr<W>>,
    },
}

impl<W> Debug for XFunction<W> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Self::Native(..) => {
                write!(f, "Native(..)")
            }
            Self::UserFunction { .. } => {
                write!(
                    f,
                    "UserFunction(..)",
                )
            }
        }
    }
}

impl<W: Write + 'static> XValue<W> {
    pub(crate) fn size(&self) -> usize {
        let base = size_of::<Self>();
        base + match self {
            Self::Int(i) => i.additional_size(),
            Self::String(s) => s.size(),
            Self::Function(XFunction::UserFunction { template, .. }) => {
                size_of::<usize>() + template.cells.len() * size_of::<usize>()
            }
            Self::StructInstance(items) => items.len() * size_of::<usize>(),
            Self::Native(n) => size_of::<usize>() + n.full_size(),
            _ => 0,
        }
    }
}

pub struct ManagedXValue<W> {
    runtime: RTCell<W>,
    /// this will be zero if the runtime has no size limit
    size: usize,
    pub value: XValue<W>,
}

impl<W> Debug for ManagedXValue<W> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "ManagedXValue({:?})", self.value)
    }
}

impl<W> Drop for ManagedXValue<W> {
    fn drop(&mut self) {
        self.runtime.borrow_mut().size -= self.size;
    }
}

impl<W: Write + 'static> ManagedXValue<W> {
    pub(crate) fn new(value: XValue<W>, runtime: RTCell<W>) -> Result<Rc<Self>, RuntimeViolation> {
        let size;
        {
            let size_limit = runtime.borrow().limits.size_limit;
            if let Some(max_size) = size_limit {
                size = value.size();
                runtime.borrow_mut().size += size;
                if VERBOSE_ALLOC {
                    println!(
                        "Allocated {size} bytes (total {}) for {value:?}",
                        runtime.borrow().size
                    );
                }
                if runtime.borrow().size > max_size {
                    return Err(RuntimeViolation::AllocationLimitReached);
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

    pub(crate) fn from_result(
        value: Result<XValue<W>, Rc<ManagedXError<W>>>,
        runtime: RTCell<W>,
    ) -> Result<EvaluatedValue<W>, RuntimeViolation> {
        match value {
            Ok(value) => Self::new(value, runtime).map(Ok),
            Err(e) => Ok(Err(e)),
        }
    }
}

pub struct ManagedXError<W> {
    runtime: RTCell<W>,
    /// this will be zero if the runtime has no size limit
    size: usize,
    pub error: String,
}

impl<W> Debug for ManagedXError<W> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "ManagedXError({:?})", self.error)
    }
}

impl<W> Drop for ManagedXError<W> {
    fn drop(&mut self) {
        self.runtime.borrow_mut().size -= self.size;
    }
}

impl<W: Write + 'static> ManagedXError<W> {
    pub(crate) fn new<T: Into<String>>(
        error: T,
        runtime: RTCell<W>,
    ) -> Result<Rc<Self>, RuntimeViolation> {
        let size;
        let error = error.into();
        {
            let size_limit = runtime.borrow().limits.size_limit;
            if let Some(max_size) = size_limit {
                size = error.len();
                runtime.borrow_mut().size += size;
                if runtime.borrow().size > max_size {
                    return Err(RuntimeViolation::AllocationLimitReached);
                }
            } else {
                size = 0;
            }
        }
        Ok(Rc::new(Self {
            runtime,
            size,
            error,
        }))
    }
}

pub type XResult<T, W> = Result<Result<T, Rc<ManagedXError<W>>>, RuntimeViolation>;
