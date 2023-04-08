use crate::root_runtime_scope::{EvaluatedValue, RuntimeResult};
use crate::runtime::RTCell;
use crate::xtype::{Bind, XCompoundSpec, XFuncSpec, XType};
use crate::xvalue::{ManagedXValue, NativeCallable, XFunction};
use crate::{Declaration, Identifier};
use std::borrow::Borrow;
use std::collections::HashSet;

use derivative::Derivative;

use crate::compilation_scope::{CellSpec, ForwardRefRequirement};
use crate::runtime_scope::{RuntimeScope, RuntimeScopeTemplate};
use std::fmt::{Debug, Error, Formatter};
use std::rc::Rc;
use std::sync::Arc;

use crate::root_compilation_scope::Interner;

#[derive(Debug)]
pub(crate) enum OverloadSpecialization {
    ParamTypes(Vec<Arc<XType>>),
    Binding(Vec<Arc<XType>>),
}

#[derive(Debug)]
pub(crate) enum OverloadSpecializationBorrowed<'a> {
    ParamTypes(&'a [Arc<XType>]),
    Binding(&'a [Arc<XType>]),
}

impl<'a> OverloadSpecialization {
    pub(crate) fn borrow(&'a self) -> OverloadSpecializationBorrowed<'a> {
        match self {
            Self::ParamTypes(v) => OverloadSpecializationBorrowed::ParamTypes(v.borrow()),
            Self::Binding(v) => OverloadSpecializationBorrowed::Binding(v.borrow()),
        }
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub(crate) enum XStaticExpr<W, R> {
    LiteralBool(bool),
    LiteralInt(i128),
    LiteralFloat(f64),
    LiteralString(String),
    Array(Vec<XStaticExpr<W, R>>),
    Tuple(Vec<XStaticExpr<W, R>>),
    Call(Box<XStaticExpr<W, R>>, Vec<XStaticExpr<W, R>>),
    Member(Box<XStaticExpr<W, R>>, Identifier),
    MemberValue(Box<XStaticExpr<W, R>>, Identifier),
    MemberOptValue(Box<XStaticExpr<W, R>>, Identifier),
    Ident(Identifier),
    SpecializedIdent(Identifier, OverloadSpecialization),
    Lambda(XFuncSpec, Box<XStaticFunction<W, R>>),
}

impl<W, R> XStaticExpr<W, R> {
    pub(crate) fn new_call(name: &'static str, args: Vec<Self>, interner: &mut Interner) -> Self {
        Self::Call(
            Box::new(Self::Ident(interner.get_or_intern_static(name))),
            args,
        )
    }

    pub(crate) fn new_call_sym(name: Identifier, args: Vec<Self>) -> Self {
        Self::Call(Box::new(Self::Ident(name)), args)
    }
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = ""))]
pub enum XExpr<W, R> {
    LiteralBool(bool),
    LiteralInt(i128),
    LiteralFloat(f64),
    LiteralString(String),
    Array(Vec<XExpr<W, R>>),
    Tuple(Vec<XExpr<W, R>>),
    Call(Box<XExpr<W, R>>, Vec<XExpr<W, R>>),
    Construct(Arc<XCompoundSpec>, Bind, Vec<XExpr<W, R>>),
    Variant(Arc<XCompoundSpec>, Bind, usize, Box<XExpr<W, R>>),
    Member(Box<XExpr<W, R>>, usize),
    MemberValue(Box<XExpr<W, R>>, usize),
    MemberOptValue(Box<XExpr<W, R>>, usize),
    Value(usize),
    // this dummy exists for calling native functions with arguments that were already
    // evaluated
    Dummy(EvaluatedValue<W, R>),
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub enum XStaticFunction<W, R> {
    Native(NativeCallable<W, R>),
    UserFunction(Rc<StaticUserFunction<W, R>>),
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct StaticUserFunction<W, R> {
    pub(crate) param_len: usize,
    pub(crate) defaults: Vec<XExpr<W, R>>,
    pub(crate) cell_specs: Vec<CellSpec>,
    pub(crate) declarations: Vec<Declaration<W, R>>,
    pub(crate) output: Box<XExpr<W, R>>,
    pub(crate) id: usize,
    pub(crate) parent_id: usize,
    pub(crate) forward_requirements: HashSet<ForwardRefRequirement>,
}

impl<W: 'static, R: 'static> XStaticFunction<W, R> {
    pub(crate) fn to_function(
        &self,
        closure: &RuntimeScope<'_, W, R>,
        rt: RTCell<W, R>,
    ) -> RuntimeResult<XFunction<W, R>> {
        Ok(match self {
            Self::Native(native) => XFunction::Native(native.clone()),
            Self::UserFunction(uf) => XFunction::UserFunction {
                template: RuntimeScopeTemplate::from_specs(
                    uf.id,
                    uf.param_len,
                    &uf.cell_specs,
                    Some(closure),
                    Some(uf.parent_id),
                    uf.declarations.clone(),
                    rt,
                    uf.defaults.clone(),
                    Some(uf.output.clone()),
                )?,
                output: uf.output.clone(),
            },
        })
    }

    pub(crate) fn from_native(
        f: impl Fn(
                &[XExpr<W, R>],
                &RuntimeScope<'_, W, R>,
                bool,
                RTCell<W, R>,
            ) -> RuntimeResult<TailedEvalResult<W, R>>
            + 'static,
    ) -> Self {
        Self::Native(Rc::new(f))
    }
}

impl<W, R> Debug for XStaticFunction<W, R> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Self::Native(..) => {
                write!(f, "Native(..)")
            }
            Self::UserFunction(..) => {
                write!(f, "UserFunction(..)",)
            }
        }
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct XExplicitStaticArgSpec<W, R> {
    pub(crate) name: Identifier,
    pub(crate) type_: Arc<XType>,
    pub(crate) default: Option<XStaticExpr<W, R>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub enum TailedEvalResult<W, R> {
    Value(EvaluatedValue<W, R>),
    TailCall(Vec<EvaluatedValue<W, R>>),
}

impl<W, R> TailedEvalResult<W, R> {
    pub fn unwrap_value(self) -> EvaluatedValue<W, R> {
        match self {
            Self::Value(v) => v,
            Self::TailCall(_) => {
                panic!("TailedEvalResult::unwrap_value called on a tail call")
            }
        }
    }
}

impl<W, R> From<Rc<ManagedXValue<W, R>>> for TailedEvalResult<W, R> {
    fn from(v: Rc<ManagedXValue<W, R>>) -> Self {
        Self::Value(Ok(v))
    }
}

impl<W, R> From<EvaluatedValue<W, R>> for TailedEvalResult<W, R> {
    fn from(v: EvaluatedValue<W, R>) -> Self {
        Self::Value(v)
    }
}
