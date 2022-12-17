use crate::evaluation_scope::EvaluatedValue;
use crate::runtime::RTCell;
use crate::xtype::{Bind, XCompoundSpec, XType};
use crate::xvalue::{ManagedXValue, NativeCallable, XFunction};
use crate::{Declaration, Identifier};

use derivative::Derivative;

use crate::compilation_scope::CellSpec;
use crate::runtime_scope::{RuntimeScope, RuntimeScopeTemplate};
use crate::runtime_violation::RuntimeViolation;
use std::fmt::{Debug, Error, Formatter};
use std::io::Write;
use std::rc::Rc;
use std::sync::Arc;
use string_interner::{DefaultSymbol, StringInterner};

#[derive(Debug)]
pub(crate) enum XStaticExpr {
    LiteralBool(bool),
    LiteralInt(i64),
    LiteralFloat(f64),
    LiteralString(String),
    Array(Vec<XStaticExpr>),
    Tuple(Vec<XStaticExpr>),
    Call(Box<XStaticExpr>, Vec<XStaticExpr>),
    // note that member names have to be strings because we need to identify "item" prefix
    // todo we can overcome this limitation with middleware interner that treats "item" strings
    //  differently
    Member(Box<XStaticExpr>, String),
    MemberValue(Box<XStaticExpr>, String),
    MemberOptValue(Box<XStaticExpr>, String),
    Ident(Identifier),
    // todo we always specialize with turbofish or bind, but never both, enforce with enum
    SpecializedIdent(Identifier, Option<Vec<Arc<XType>>>, Option<Vec<Arc<XType>>>),
    Lambda(Vec<XExplicitStaticArgSpec>, Box<XStaticExpr>),
}

impl XStaticExpr {
    pub(crate) fn new_call(
        name: &'static str,
        args: Vec<Self>,
        interner: &mut StringInterner,
    ) -> Self {
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
pub enum XExpr<W: Write + 'static> {
    LiteralBool(bool),
    LiteralInt(i64),
    LiteralFloat(f64),
    LiteralString(String),
    Array(Vec<XExpr<W>>),
    Tuple(Vec<XExpr<W>>),
    Call(Box<XExpr<W>>, Vec<XExpr<W>>),
    Construct(Arc<XCompoundSpec>, Bind, Vec<XExpr<W>>),
    Variant(Arc<XCompoundSpec>, Bind, usize, Box<XExpr<W>>),
    Member(Box<XExpr<W>>, usize),
    MemberValue(Box<XExpr<W>>, usize),
    MemberOptValue(Box<XExpr<W>>, usize),
    Value(usize),
    // this dummy exists for calling native functions with arguments that were already
    // evaluated
    Dummy(EvaluatedValue<W>),
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub enum XStaticFunction<W: Write + 'static> {
    Native(NativeCallable<W>),
    UserFunction(Rc<StaticUserFunction<W>>),
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct StaticUserFunction<W: Write + 'static> {
    pub(crate) name: Option<String>,
    pub(crate) param_len: usize,
    pub(crate) defaults: Vec<XExpr<W>>,
    pub(crate) cell_specs: Vec<CellSpec<W>>,
    pub(crate) declarations: Vec<Declaration<W>>,
    pub(crate) output: Box<XExpr<W>>,
    pub(crate) id: usize,
    pub(crate) parent_id: usize,
}

impl<W: Write + 'static> XStaticFunction<W> {
    pub(crate) fn to_function(
        &self,
        closure: &RuntimeScope<'_, W>,
        rt: RTCell<W>,
    ) -> Result<XFunction<W>, RuntimeViolation> {
        Ok(match self {
            Self::Native(native) => XFunction::Native(native.clone()),
            Self::UserFunction(uf) => XFunction::UserFunction {
                template: RuntimeScopeTemplate::from_specs(
                    uf.id,
                    uf.name.clone(),
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
                &[XExpr<W>],
                &RuntimeScope<'_, W>,
                bool,
                RTCell<W>,
            ) -> Result<TailedEvalResult<W>, RuntimeViolation>
            + 'static,
    ) -> Self {
        Self::Native(Rc::new(f))
    }
}

impl<W: Write + 'static> Debug for XStaticFunction<W> {
    // todo is this needed?
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Self::Native(..) => {
                write!(f, "Native(..)")
            }
            Self::UserFunction(template, ..) => {
                write!(
                    f,
                    "UserFunction({})",
                    template.name.as_deref().unwrap_or("..")
                )
            }
        }
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct XExplicitStaticArgSpec {
    pub(crate) name: DefaultSymbol,
    pub(crate) type_: Arc<XType>,
    pub(crate) default: Option<XStaticExpr>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub enum TailedEvalResult<W: Write + 'static> {
    Value(EvaluatedValue<W>),
    TailCall(Vec<EvaluatedValue<W>>),
}

impl<W: Write + 'static> TailedEvalResult<W> {
    pub fn unwrap_value(self) -> EvaluatedValue<W> {
        match self {
            Self::Value(v) => v,
            Self::TailCall(_) => {
                panic!("TailedEvalResult::unwrap_value called on a tail call")
            }
        }
    }
}

impl<W: Write + 'static> From<Rc<ManagedXValue<W>>> for TailedEvalResult<W> {
    fn from(v: Rc<ManagedXValue<W>>) -> Self {
        Self::Value(Ok(v))
    }
}

impl<W: Write + 'static> From<EvaluatedValue<W>> for TailedEvalResult<W> {
    fn from(v: EvaluatedValue<W>) -> Self {
        Self::Value(v)
    }
}
