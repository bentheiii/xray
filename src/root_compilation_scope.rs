use crate::parser::Rule;
use crate::xexpr::{XExpr, XStaticFunction};
use crate::xtype::{XFuncSpec, XType};
use crate::xvalue::XFunctionFactoryOutput;
use crate::{CompilationError, Identifier, XRayParser};

use std::collections::HashSet;
use std::convert::TryInto;

use std::io::Write;

use std::ops::Deref;

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
pub enum Declaration<W> {
    Parameter {
        cell_idx: usize,
        argument_idx: usize,
    },
    Value {
        cell_idx: usize,
        expr: XExpr<W>,
    },
    Function {
        cell_idx: usize,
        func: XStaticFunction<W>,
    },
}

pub(crate) type Interner = StringInterner<SpecialPrefixBackend<DefaultBackend<DefaultSymbol>>>;

pub struct RootCompilationScope<W: 'static> {
    pub(crate) scope: CompilationScope<'static, W>,
    interner: Interner,
}

impl<W: Write + 'static> RootCompilationScope<W> {
    pub fn new() -> Self {
        Self {
            scope: CompilationScope::root(),
            interner: Interner::new(),
        }
    }

    pub fn add_native_type(
        &mut self,
        name: &'static str,
        type_: Arc<XType>,
    ) -> Result<(), CompilationError> {
        self.scope
            .add_type(self.interner.get_or_intern_static(name), type_)
    }

    pub fn add_func(
        &mut self,
        name: &'static str,
        spec: XFuncSpec,
        func: XStaticFunction<W>,
    ) -> Result<(), CompilationError> {
        self.scope
            .add_static_func(self.interner.get_or_intern_static(name), spec, func)
    }

    pub fn add_dyn_func(
        &mut self,
        name: &'static str,
        desc: &'static str,
        func: impl Fn(
                Option<&[XExpr<W>]>,
                Option<&[Arc<XType>]>,
                &mut CompilationScope<'_, W>,
                Option<&[Arc<XType>]>,
            ) -> Result<XFunctionFactoryOutput<W>, String>
            + 'static,
    ) -> Result<(), CompilationError> {
        self.scope
            .add_dynamic_func(self.interner.get_or_intern_static(name), desc, func)
            .map(|_| ())
    }

    pub(crate) fn generics_from_names<const N: usize>(
        &mut self,
        names: [&'static str; N],
    ) -> ([Arc<XType>; N], Vec<Identifier>) {
        let (v0, v1) = names
            .iter()
            .map(|name| {
                let ident = self.interner.get_or_intern_static(name);
                (XType::XGeneric(ident).into(), ident)
            })
            .unzip::<_, _, Vec<_>, Vec<_>>();
        (v0.try_into().unwrap(), v1)
    }

    pub fn identifier(&mut self, name: &'static str) -> Identifier {
        self.interner.get_or_intern_static(name)
    }

    pub fn get_identifier(&self, name: &str) -> Option<Identifier> {
        self.interner.get(name)
    }

    pub fn feed_file(&mut self, input: &str) -> Result<(), Box<ResolvedTracedCompilationError>> {
        let body = XRayParser::parse(Rule::header, input)
            .map(|mut p| p.next().unwrap())
            .map_err(|s| ResolvedTracedCompilationError::Syntax(Box::new(s)))?;
        self.scope
            .feed(body, &HashSet::new(), &mut self.interner)
            .map_err(|e| Box::new(e.resolve_with_input(&self.interner, input)))
    }

    pub fn describe_type(&self, t: impl Deref<Target = XType>) -> String {
        t.to_string_with_interner(&self.interner)
    }
}

impl<W: Write + 'static> Default for RootCompilationScope<W> {
    fn default() -> Self {
        Self::new()
    }
}
