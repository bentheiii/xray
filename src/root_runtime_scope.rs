use crate::runtime::RTCell;
use crate::xexpr::TailedEvalResult;
use crate::xvalue::{ManagedXError, ManagedXValue, XFunction, XValue};

use std::rc::Rc;

use crate::compilation_scope::{CellSpec, OverloadWithForwardReq};
use crate::runtime_scope::{EvaluationCell, RuntimeScope, RuntimeScopeTemplate};
use crate::runtime_violation::RuntimeViolation;
use crate::RootCompilationScope;

pub type EvaluatedValue<W, R, T> = Result<Rc<ManagedXValue<W, R, T>>, Rc<ManagedXError<W, R, T>>>;

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
    ForwardRefFunction(Vec<String>),
}

pub struct RootEvaluationScope<'c, W: 'static, R: 'static, T: 'static> {
    scope: Rc<RuntimeScope<'static, W, R, T>>,
    compilation_scope: &'c RootCompilationScope<W, R, T>,
    runtime: RTCell<W, R, T>,
}

impl<'c, W, R, T> RootEvaluationScope<'c, W, R, T> {
    pub fn from_compilation_scope(
        comp_scope: &'c RootCompilationScope<W, R, T>,
        runtime: RTCell<W, R, T>,
    ) -> RuntimeResult<Self> {
        let cell_specs = comp_scope
            .scope
            .cells
            .iter()
            .map(|c| CellSpec::from(c.clone()))
            .collect::<Vec<_>>();
        let template = RuntimeScopeTemplate::from_specs(
            comp_scope.scope.id,
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

    pub fn get_value(&self, name: &str) -> Result<&EvaluatedValue<W, R, T>, GetValueError> {
        let id = self.compilation_scope.get_identifier(name);
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
    ) -> Result<&XFunction<W, R, T>, GetUniqueFunctionError> {
        let id = self.compilation_scope.get_identifier(name);
        if let Some(id) = id {
            let overload = self
                .compilation_scope
                .scope
                .get_unique_function(&id)
                .map_err(|_| GetUniqueFunctionError::OverloadedFunction)?
                .ok_or(GetUniqueFunctionError::NotFound)?;
            match overload {
                OverloadWithForwardReq::Factory(..) => Err(GetUniqueFunctionError::FactoryFunction),
                OverloadWithForwardReq::Static {
                    cell_idx,
                    forward_requirements,
                    ..
                } => {
                    let v = self.scope.get_cell_value(cell_idx);
                    if let EvaluationCell::Value(v) = v {
                        let unmet_freq: Vec<_> = forward_requirements
                            .iter()
                            .filter_map(|freq| {
                                let fref = self.compilation_scope.scope.forward_ref(freq);
                                if fref.fulfilled {
                                    None
                                } else {
                                    let interner = self.compilation_scope.interner.borrow();
                                    let name = interner.resolve(fref.name).unwrap();
                                    Some(name.to_string())
                                }
                            })
                            .collect();
                        if !unmet_freq.is_empty() {
                            return Err(GetUniqueFunctionError::ForwardRefFunction(unmet_freq));
                        }
                        let XValue::Function(func) = &v.as_ref().unwrap().value else {unreachable!()};
                        Ok(func)
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
        function: &XFunction<W, R, T>,
        args: Vec<EvaluatedValue<W, R, T>>,
    ) -> RuntimeResult<TailedEvalResult<W, R, T>> {
        self.scope
            .eval_func_with_values(function, args, self.runtime.clone(), false)
    }
}

pub type RuntimeResult<T> = Result<T, RuntimeViolation>;