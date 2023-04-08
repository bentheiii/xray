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

pub(crate) type UnionInstance<W, R> = (usize, Rc<ManagedXValue<W, R>>);

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub enum XValue<W, R> {
    Int(LazyBigint),
    Float(f64),
    String(Box<FencedString>),
    Bool(bool),
    Function(XFunction<W, R>),
    StructInstance(Vec<Rc<ManagedXValue<W, R>>>),
    UnionInstance(UnionInstance<W, R>),
    Native(Box<dyn XNativeValue>),
}

pub(crate) type NativeCallback<W, R> = dyn Fn(
    &[XExpr<W, R>],
    &RuntimeScope<'_, W, R>,
    bool,
    RTCell<W, R>,
) -> RuntimeResult<TailedEvalResult<W, R>>;

type DynBindCallback<W, R> = dyn Fn(
    Option<&[XExpr<W, R>]>,
    Option<&[Arc<XType>]>,
    &mut CompilationScope<'_, W, R>,
    Option<&[Arc<XType>]>,
) -> Result<XFunctionFactoryOutput<W, R>, String>;
pub(crate) type DynEvalCallback<W, R> =
    Rc<dyn Fn(&RuntimeScope<W, R>, RTCell<W, R>) -> XResult<XStaticFunction<W, R>, W, R>>;

pub type NativeCallable<W, R> = Rc<NativeCallback<W, R>>;
pub type DynBind<W, R> = Rc<DynBindCallback<W, R>>;

pub struct XFunctionFactoryOutput<W, R> {
    pub(crate) spec: XFuncSpec,
    pub(crate) func: DynEvalCallback<W, R>,
}

impl<W: 'static, R: 'static> XFunctionFactoryOutput<W, R> {
    pub(crate) fn from_native(
        spec: XFuncSpec,
        callable: impl Fn(
                &[XExpr<W, R>],
                &RuntimeScope<'_, W, R>,
                bool,
                RTCell<W, R>,
            ) -> RuntimeResult<TailedEvalResult<W, R>>
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
        callable: impl Fn(&RuntimeScope<W, R>, RTCell<W, R>) -> XResult<F, W, R> + 'static,
    ) -> Self
    where
        F: Fn(
                &[XExpr<W, R>],
                &RuntimeScope<'_, W, R>,
                bool,
                RTCell<W, R>,
            ) -> RuntimeResult<TailedEvalResult<W, R>>
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
pub enum XFunction<W, R> {
    Native(NativeCallable<W, R>),
    UserFunction {
        template: Rc<RuntimeScopeTemplate<W, R>>,
        output: Box<XExpr<W, R>>,
    },
}

impl<W, R> Debug for XFunction<W, R> {
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

impl<W, R> XValue<W, R> {
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

pub struct ManagedXValue<W, R> {
    runtime: RTCell<W, R>,
    /// this will be zero if the runtime has no size limit
    size: usize,
    pub value: XValue<W, R>,
}

impl<W, R> Debug for ManagedXValue<W, R> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "ManagedXValue({:?})", self.value)
    }
}

impl<W, R> Drop for ManagedXValue<W, R> {
    fn drop(&mut self) {
        self.runtime.borrow_mut().size -= self.size;
    }
}

impl<W, R> ManagedXValue<W, R> {
    pub(crate) fn new(
        value: XValue<W, R>,
        runtime: RTCell<W, R>,
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
        value: Result<XValue<W, R>, Rc<ManagedXError<W, R>>>,
        runtime: RTCell<W, R>,
    ) -> XResult<Rc<ManagedXValue<W, R>>, W, R> {
        match value {
            Ok(value) => Self::new(value, runtime).map(Ok),
            Err(e) => Ok(Err(e)),
        }
    }
}

pub struct ManagedXError<W, R> {
    runtime: RTCell<W, R>,
    /// this will be zero if the runtime has no size limit
    size: usize,
    pub error: String,
}

impl<W, R> Debug for ManagedXError<W, R> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "ManagedXError({:?})", self.error)
    }
}

impl<W, R> Drop for ManagedXError<W, R> {
    fn drop(&mut self) {
        self.runtime.borrow_mut().size -= self.size;
    }
}

impl<W, R> ManagedXError<W, R> {
    pub(crate) fn new<T: Into<String>>(
        error: T,
        runtime: RTCell<W, R>,
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

pub type XResult<T, W, R> = RuntimeResult<Result<T, Rc<ManagedXError<W, R>>>>;
