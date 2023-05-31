use crate::parser::Rule;
use crate::xexpr::{XExpr, XStaticFunction};
use crate::xtype::{XFuncSpec, XType};
use crate::xvalue::{DynEvalCallback, XFunctionFactoryOutput};
use crate::{CompilationError, Identifier, XRayParser};
use std::cell::RefCell;

use std::collections::HashSet;
use std::convert::TryInto;

use std::ops::{Deref, DerefMut};
use std::sync::Arc;
use string_interner::{DefaultBackend, DefaultSymbol, StringInterner};

use crate::compile_err::ResolvedTracedCompilationError;
use crate::pest::Parser;

use derivative::Derivative;

use crate::compilation_scope::CompilationScope;
use crate::util::special_prefix_interner::SpecialPrefixBackend;

/// these will always point to variable cells
#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = ""))]
pub enum Declaration<W, R, T> {
    Parameter {
        cell_idx: usize,
        argument_idx: usize,
    },
    Value {
        cell_idx: usize,
        expr: XExpr<W, R, T>,
    },
    FactoryFunction {
        cell_idx: usize,
        #[derivative(Debug = "ignore")]
        cb: DynEvalCallback<W, R, T>,
    },
    Function {
        cell_idx: usize,
        func: XStaticFunction<W, R, T>,
    },
}

pub(crate) type Interner = StringInterner<SpecialPrefixBackend<DefaultBackend<DefaultSymbol>>>;

pub struct RootCompilationScope<W: 'static, R: 'static, T: 'static> {
    pub(crate) scope: CompilationScope<'static, W, R, T>,
    pub(crate) interner: RefCell<Interner>,
}

impl<W, R, T> RootCompilationScope<W, R, T> {
    pub fn new() -> Self {
        Self {
            scope: CompilationScope::root(),
            interner: RefCell::new(Interner::new()),
        }
    }

    pub fn add_native_type(
        &mut self,
        name: &'static str,
        type_: Arc<XType>,
    ) -> Result<(), CompilationError> {
        let id = self.identifier(name);
        self.scope.add_type(id, type_)
    }

    pub fn add_func(
        &mut self,
        name: &'static str,
        spec: XFuncSpec,
        func: XStaticFunction<W, R, T>,
    ) -> Result<(), CompilationError> {
        let id = self.identifier(name);
        self.scope.add_static_func(id, spec, func)
    }

    pub fn add_dyn_func(
        &mut self,
        name: &'static str,
        desc: &'static str,
        func: impl Fn(
                Option<&[XExpr<W, R, T>]>,
                Option<&[Arc<XType>]>,
                &mut CompilationScope<'_, W, R, T>,
                Option<&[Arc<XType>]>,
            ) -> Result<XFunctionFactoryOutput<W, R, T>, String>
            + 'static,
    ) -> Result<(), CompilationError> {
        let id = self.identifier(name);
        self.scope.add_dynamic_func(id, desc, func).map(|_| ())
    }

    pub(crate) fn generics_from_names<const N: usize>(
        &mut self,
        names: [&'static str; N],
    ) -> ([Arc<XType>; N], Vec<Identifier>) {
        let (v0, v1) = names
            .iter()
            .map(|name| {
                let ident = self.identifier(name);
                (XType::XGeneric(ident).into(), ident)
            })
            .unzip::<_, _, Vec<_>, Vec<_>>();
        (v0.try_into().unwrap(), v1)
    }

    pub fn identifier(&mut self, name: &'static str) -> Identifier {
        self.interner.borrow_mut().get_or_intern_static(name)
    }

    pub fn get_identifier(&self, name: &str) -> Option<Identifier> {
        self.interner.borrow().get(name)
    }

    pub fn feed_file(&mut self, input: &str) -> Result<(), Box<ResolvedTracedCompilationError>> {
        let body = XRayParser::parse(Rule::header, input)
            .map(|mut p| p.next().unwrap())
            .map_err(|s| ResolvedTracedCompilationError::Syntax(Box::new(s)))?;
        let r = self.scope.feed(
            body,
            &HashSet::new(),
            self.interner.borrow_mut().deref_mut(),
        );
        r.map_err(|e| Box::new(e.resolve_with_input(&self.interner.borrow_mut(), input)))
    }

    pub fn describe_type(&self, t: impl Deref<Target = XType>) -> String {
        t.to_string_with_interner(&self.interner.borrow())
    }
}

impl<W, R, T> Default for RootCompilationScope<W, R, T> {
    fn default() -> Self {
        Self::new()
    }
}
