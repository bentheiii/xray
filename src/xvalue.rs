use crate::native_types::XNativeValue;

use crate::root_runtime_scope::RuntimeResult;
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
use std::mem::size_of;
use std::rc::Rc;
use std::sync::Arc;

const VERBOSE_ALLOC: bool = false;

pub(crate) type UnionInstance<W, R, T> = (usize, Rc<ManagedXValue<W, R, T>>);

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub enum XValue<W, R, T> {
    Int(LazyBigint),
    Float(f64),
    String(Box<FencedString>),
    Bool(bool),
    Function(XFunction<W, R, T>),
    StructInstance(Vec<Rc<ManagedXValue<W, R, T>>>),
    UnionInstance(UnionInstance<W, R, T>),
    Native(Box<dyn XNativeValue>),
}

pub(crate) type NativeCallback<W, R, T> = dyn Fn(
    &[XExpr<W, R, T>],
    &RuntimeScope<'_, W, R, T>,
    bool,
    RTCell<W, R, T>,
) -> RuntimeResult<TailedEvalResult<W, R, T>>;

type DynBindCallback<W, R, T> = dyn Fn(
    Option<&[XExpr<W, R, T>]>,
    Option<&[Arc<XType>]>,
    &mut CompilationScope<'_, W, R, T>,
    Option<&[Arc<XType>]>,
) -> Result<XFunctionFactoryOutput<W, R, T>, String>;
pub(crate) type DynEvalCallback<W, R, T> =
    Rc<dyn Fn(&RuntimeScope<W, R, T>, RTCell<W, R, T>) -> XResult<XStaticFunction<W, R, T>, W, R, T>>;

pub type NativeCallable<W, R, T> = Rc<NativeCallback<W, R, T>>;
pub type DynBind<W, R, T> = Rc<DynBindCallback<W, R, T>>;

pub struct XFunctionFactoryOutput<W, R, T> {
    pub(crate) spec: XFuncSpec,
    pub(crate) func: DynEvalCallback<W, R, T>,
}

impl<W: 'static, R: 'static, T: 'static> XFunctionFactoryOutput<W, R, T> {
    pub(crate) fn from_native(
        spec: XFuncSpec,
        callable: impl Fn(
                &[XExpr<W, R, T>],
                &RuntimeScope<'_, W, R, T>,
                bool,
                RTCell<W, R, T>,
            ) -> RuntimeResult<TailedEvalResult<W, R, T>>
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
        callable: impl Fn(&RuntimeScope<W, R, T>, RTCell<W, R, T>) -> XResult<F, W, R, T> + 'static,
    ) -> Self
    where
        F: Fn(
                &[XExpr<W, R, T>],
                &RuntimeScope<'_, W, R, T>,
                bool,
                RTCell<W, R, T>,
            ) -> RuntimeResult<TailedEvalResult<W, R, T>>
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
pub enum XFunction<W, R, T> {
    Native(NativeCallable<W, R, T>),
    UserFunction {
        template: Rc<RuntimeScopeTemplate<W, R, T>>,
        output: Box<XExpr<W, R, T>>,
    },
}

impl<W, R, T> Debug for XFunction<W, R, T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Self::Native(..) => {
                write!(f, "Native(..)")
            }
            Self::UserFunction { .. } => {
                write!(f, "UserFunction(..)",)
            }
        }
    }
}

impl<W, R, T> XValue<W, R, T> {
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

pub struct ManagedXValue<W, R, T> {
    runtime: RTCell<W, R, T>,
    /// this will be zero if the runtime has no size limit
    size: usize,
    pub value: XValue<W, R, T>,
}

impl<W, R, T> Debug for ManagedXValue<W, R, T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "ManagedXValue({:?})", self.value)
    }
}

impl<W, R, T> Drop for ManagedXValue<W, R, T> {
    fn drop(&mut self) {
        self.runtime.borrow_mut().size -= self.size;
    }
}

impl<W, R, T> ManagedXValue<W, R, T> {
    pub(crate) fn new(
        value: XValue<W, R, T>,
        runtime: RTCell<W, R, T>,
    ) -> RuntimeResult<Rc<Self>> {
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
        value: Result<XValue<W, R, T>, Rc<ManagedXError<W, R, T>>>,
        runtime: RTCell<W, R, T>,
    ) -> XResult<Rc<ManagedXValue<W, R, T>>, W, R, T> {
        match value {
            Ok(value) => Self::new(value, runtime).map(Ok),
            Err(e) => Ok(Err(e)),
        }
    }
}

pub struct ManagedXError<W, R, T> {
    runtime: RTCell<W, R, T>,
    /// this will be zero if the runtime has no size limit
    size: usize,
    pub error: String,
}

impl<W, R, T> Debug for ManagedXError<W, R, T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "ManagedXError({:?})", self.error)
    }
}

impl<W, R, T> Drop for ManagedXError<W, R, T> {
    fn drop(&mut self) {
        self.runtime.borrow_mut().size -= self.size;
    }
}

impl<W, R, T> ManagedXError<W, R, T> {
    pub(crate) fn new<E: Into<String>>(
        error: E,
        runtime: RTCell<W, R, T>,
    ) -> RuntimeResult<Rc<Self>> {
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

pub type XResult<I, W, R, T> = RuntimeResult<Result<I, Rc<ManagedXError<W, R, T>>>>;
