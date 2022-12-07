use crate::builtin::optional::{XOptional, XOptionalType};
use crate::builtin::sequence::{XSequence, XSequenceType};
use crate::_compilation_scope::{XCompilationScopeItem, XFunctionFactory};
use crate::evaluation_scope::{EvaluatedVariable};
use crate::runtime::RTCell;
use crate::xtype::{
    common_type, Bind, CompoundKind, XCompoundSpec, XFuncParamSpec, XType, X_BOOL, X_FLOAT, X_INT,
    X_STRING,
};
use crate::xvalue::{ManagedXValue, NativeCallable, XFunction, XValue};
use crate::{
    manage_native, CompilationError, Declaration, Identifier, XFuncSpec,
};

use crate::util::lazy_bigint::LazyBigint;
use crate::util::rc_hash::RcHash;
use derivative::Derivative;
use itertools::Itertools;
use std::collections::HashSet;
use std::fmt::{Debug, Error, Formatter};
use std::io::Write;
use std::rc::Rc;
use std::sync::Arc;
use string_interner::{DefaultSymbol, StringInterner};
use crate::compilation_scopes::{CellSpec, CompilationScope};
use crate::runtime_scope::{RuntimeScope, RuntimeScopeTemplate};

#[derive(Debug)]
pub(crate) enum XStaticExpr {
    LiteralBool(bool),
    LiteralInt(i64),
    LiteralFloat(f64),
    LiteralString(String),
    Array(Vec<XStaticExpr>),
    Tuple(Vec<XStaticExpr>),
    Call(Box<XStaticExpr>, Vec<XStaticExpr>),
    Member(Box<XStaticExpr>, String),
    Ident(Identifier),
    SpecializedIdent(Identifier, Option<Vec<Arc<XType>>>, Option<Vec<Arc<XType>>>),
    Lambda(Vec<XExplicitStaticArgSpec>, Box<XStaticExpr>),
}

pub(crate) struct CompilationResult<W: Write + 'static> {
    pub(crate) expr: XExpr<W>,
    pub(crate) closure_vars: Vec<Identifier>,
}

impl<W: Write + 'static> From<XExpr<W>> for CompilationResult<W> {
    fn from(expr: XExpr<W>) -> Self {
        Self {
            expr,
            closure_vars: vec![],
        }
    }
}

impl<W: Write + 'static> CompilationResult<W> {
    fn new(expr: XExpr<W>, closure_vars: Vec<DefaultSymbol>) -> Self {
        Self { expr, closure_vars }
    }
    fn join(results: impl IntoIterator<Item=Self>) -> JoinedCompilationResult<W> {
        let mut exprs = vec![];
        let mut closure_vars = vec![];
        for result in results {
            exprs.push(result.expr);
            closure_vars.extend(result.closure_vars.iter().copied());
        }
        (exprs, closure_vars)
    }
    fn from_multi(
        other: JoinedCompilationResult<W>,
        f: impl FnOnce(Vec<XExpr<W>>) -> XExpr<W>,
    ) -> Self {
        Self::new(f(other.0), other.1)
    }
}

type JoinedCompilationResult<W> = (Vec<XExpr<W>>, Vec<DefaultSymbol>);

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
    // todo fund some way to divorce lambda and spec
    Lambda(Rc<StaticUserFunction<W>>, XFuncSpec),
    Value(usize),
    // this dummy exists for calling native functions with arguments that were already
    // evaluated
    Dummy(EvaluatedVariable<W>),
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
    pub(crate) output: Box<XExpr<W>>, // todo does this have to be a box?
    pub(crate) scope_depth: usize,
}

impl<W: Write + 'static> XStaticFunction<W> {
    pub(crate) fn to_function(&self, closure: &RuntimeScope<'_, W>, rt: RTCell<W>) -> Result<XFunction<W>, String> {
        Ok(match self {
            Self::Native(native) => {
                XFunction::Native(native.clone())
            }
            Self::UserFunction(uf) => {
                XFunction::UserFunction {
                    template: RuntimeScopeTemplate::from_specs(uf.name.clone(), &uf.cell_specs, Some(closure), uf.declarations.clone(), rt, uf.defaults.clone(), Some(uf.output.clone()), uf.scope_depth)?,
                    defaults: uf.defaults.clone(),
                    output: uf.output.clone(),
                }
            }
        })
    }

    pub(crate) fn from_native(f: impl Fn(
        &[XExpr<W>],
        &RuntimeScope<'_, W>,
        bool,
        RTCell<W>,
    ) -> Result<TailedEvalResult<W>, String> + 'static) -> Self {
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
                write!(f, "UserFunction({})", template.name.as_deref().unwrap_or(".."))
            }
        }
    }
}

impl<W: Write + 'static> PartialEq for XStaticFunction<W> {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl<W: Write + 'static> Eq for XStaticFunction<W> {}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct XExplicitStaticFuncSpec {
    pub generic_params: Option<Vec<DefaultSymbol>>,
    pub args: Vec<XExplicitStaticArgSpec>,
    pub ret: Arc<XType>,
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
pub struct XExplicitFuncSpec<W: Write + 'static> {
    pub generic_params: Option<Vec<DefaultSymbol>>,
    pub args: Vec<XExplicitArgSpec<W>>,
    pub ret: Arc<XType>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct XExplicitArgSpec<W: Write + 'static> {
    pub(crate) name: DefaultSymbol,
    pub(crate) type_: Arc<XType>,
    pub(crate) default: Option<XExpr<W>>,
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = ""))]
pub enum IdentItem<W: Write + 'static> {
    Value(Arc<XType>),
    Function(Rc<XStaticFunction<W>>),
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub enum TailedEvalResult<W: Write + 'static> {
    Value(EvaluatedVariable<W>),
    TailCall(Vec<EvaluatedVariable<W>>),
}

impl<W: Write + 'static> TailedEvalResult<W> {
    pub fn unwrap_value(self) -> EvaluatedVariable<W> {
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

impl<W: Write + 'static> From<EvaluatedVariable<W>> for TailedEvalResult<W> {
    fn from(v: EvaluatedVariable<W>) -> Self {
        Self::Value(v)
    }
}