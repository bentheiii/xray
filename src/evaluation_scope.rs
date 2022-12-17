use crate::runtime::RTCell;
use crate::xexpr::TailedEvalResult;
use crate::xvalue::{ManagedXError, ManagedXValue, XFunction, XValue};

use std::io::Write;
use std::rc::Rc;

use crate::compilation_scope::{CellSpec, Overload};
use crate::runtime_scope::{EvaluationCell, RuntimeScope, RuntimeScopeTemplate};
use crate::runtime_violation::RuntimeViolation;
use crate::RootCompilationScope;

pub type EvaluatedValue<W> = Result<Rc<ManagedXValue<W>>, Rc<ManagedXError<W>>>;

#[derive(Debug)]
pub enum GetValueError {
    NotFound,
    NonValueCell,
}

#[derive(Debug)]
pub enum GetUniqueFunctionError {
    NotFound,
    OverloadedFunction,
    NonValueCell,
    FactoryFunction,
}

pub struct RootEvaluationScope<'c, W: Write + 'static> {
    scope: Rc<RuntimeScope<'static, W>>,
    compilation_scope: &'c RootCompilationScope<W>,
    runtime: RTCell<W>,
}

impl<'c, W: Write + 'static> RootEvaluationScope<'c, W> {
    pub fn from_compilation_scope(
        comp_scope: &'c RootCompilationScope<W>,
        runtime: RTCell<W>,
    ) -> Result<Self, RuntimeViolation> {
        let cell_specs = comp_scope
            .scope
            .cells
            .iter()
            .map(|c| CellSpec::from(c.clone()))
            .collect::<Vec<_>>();
        let template = RuntimeScopeTemplate::from_specs(
            comp_scope.scope.id,
            None,
            0,
            &cell_specs,
            None,
            None,
            comp_scope.scope.declarations.clone(),
            runtime.clone(),
            vec![],
            None,
        )?;
        let scope = RuntimeScope::from_template(template, None, runtime.clone(), vec![])?;
        let ret = Self {
            scope,
            compilation_scope: comp_scope,
            runtime,
        };
        Ok(ret)
    }

    pub fn get_value(&self, name: &str) -> Result<&EvaluatedValue<W>, GetValueError> {
        let id = self.compilation_scope.get_identifer(name);
        if let Some(id) = id {
            let cell_idx = self
                .compilation_scope
                .scope
                .get_variable_cell(&id)
                .ok_or(GetValueError::NotFound)?;
            let v = self.scope.get_cell_value(*cell_idx);
            if let EvaluationCell::Value(v) = v {
                Ok(v)
            } else {
                Err(GetValueError::NonValueCell)
            }
        } else {
            Err(GetValueError::NotFound)
        }
    }

    pub fn get_user_defined_function(
        &self,
        name: &str,
    ) -> Result<Option<&XFunction<W>>, GetUniqueFunctionError> {
        let id = self.compilation_scope.get_identifer(name);
        if let Some(id) = id {
            let overload = self
                .compilation_scope
                .scope
                .get_unique_function(&id)
                .map_err(|_| GetUniqueFunctionError::OverloadedFunction)?
                .ok_or(GetUniqueFunctionError::NotFound)?;
            match overload {
                Overload::Factory(..) => Err(GetUniqueFunctionError::FactoryFunction),
                Overload::Static { cell_idx, .. } => {
                    let v = self.scope.get_cell_value(*cell_idx);
                    if let EvaluationCell::Value(v) = v {
                        let XValue::Function(func) = &v.as_ref().unwrap().value else {unreachable!()};
                        Ok(Some(func))
                    } else {
                        Err(GetUniqueFunctionError::NonValueCell)
                    }
                }
            }
        } else {
            Err(GetUniqueFunctionError::NotFound)
        }
    }

    pub fn run_function(
        &self,
        function: &XFunction<W>,
        args: Vec<EvaluatedValue<W>>,
    ) -> Result<TailedEvalResult<W>, RuntimeViolation> {
        self.scope
            .eval_func_with_values(function, args, self.runtime.clone(), false)
    }
}
