use crate::runtime::RTCell;
use crate::xexpr::{TailedEvalResult};
use crate::xvalue::{ManagedXValue, XFunction, XValue};

use std::fmt::{Debug, Formatter};
use std::io::Write;
use std::rc::Rc;



use crate::{RootCompilationScope};
use crate::compilation_scope::{CellSpec, Overload};
use crate::runtime_err::RuntimeError;
use crate::runtime_scope::{EvaluatedValue, EvaluationCell, RuntimeScope, RuntimeScopeTemplate};

pub type EvaluatedVariable<W> = Result<Rc<ManagedXValue<W>>, String>;

pub struct MultipleUD; // for trying to access an ambiguous user-defined

impl Debug for MultipleUD {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Multiple user-defined functions with the queried name")
    }
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
    ) -> Result<Self, RuntimeError> {
        let cell_specs = comp_scope.scope.cells.iter().map(|c| CellSpec::from(c.clone())).collect::<Vec<_>>();
        let template = RuntimeScopeTemplate::from_specs(comp_scope.scope.id, None, 0, &cell_specs, None, None, comp_scope.scope.declarations.clone(), runtime.clone(), vec![], None)?;
        let scope = RuntimeScope::from_template(template, None, runtime.clone(), vec![], &[])?;
        let ret = Self {
            scope,
            compilation_scope: comp_scope,
            runtime,
        };
        Ok(ret)
    }

    pub fn get_value(&self, name: &str) -> Option<&EvaluatedVariable<W>> {
        let id = self.compilation_scope.get_identifer(name);
        if let Some(id) = id {
            let cell_idx = self.compilation_scope.scope.get_variable_cell(&id)?;
            let v = self.scope.get_cell_value(*cell_idx);
            if let EvaluationCell::Value(v) = v {
                Some(v)
            } else {
                todo!("handle better")
            }
        } else {
            None
        }
    }

    pub fn get_user_defined_function(
        &self,
        name: &str,
    ) -> Result<Option<&XFunction<W>>, MultipleUD> {
        let id = self.compilation_scope.get_identifer(name);
        if let Some(id) = id {
            let overload = self.compilation_scope.scope.get_unique_function(&id)?;
            overload.map_or(
                Ok(None),
                |ov| match ov {
                    Overload::Factory(..) => Err(MultipleUD), // todo improve this error,
                    Overload::Static { cell_idx, .. } => {
                        let v = self.scope.get_cell_value(*cell_idx);
                        if let EvaluationCell::Value(v) = v {
                            if let XValue::Function(func) = &v.as_ref().unwrap().value {
                                Ok(Some(func))
                            } else {
                                todo!("handle better")
                            }
                        } else {
                            todo!("handle better")
                        }
                    }
                }
            )
        } else {
            Ok(None)
        }
    }
    
    pub fn run_function(&self, function: &XFunction<W>, args: Vec<EvaluatedValue<W>>)->Result<TailedEvalResult<W>, RuntimeError>{
        self.scope.eval_func_with_values(function, args, self.runtime.clone(), false)
    }
}
