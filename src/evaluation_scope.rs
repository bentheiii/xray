use crate::runtime::RTCell;
use crate::xexpr::XStaticFunction;
use crate::xvalue::{ManagedXValue, XFunction};
use std::collections::HashMap;
use std::rc::Rc;
use string_interner::DefaultSymbol;

use crate::compilation_scope::Declaration;

pub struct XEvaluationScope<'p> {
    pub values: HashMap<DefaultSymbol, Rc<ManagedXValue>>,
    pub recourse: Option<&'p XFunction>,
    ud_functions: HashMap<DefaultSymbol, Rc<XStaticFunction>>,
    parent: Option<&'p XEvaluationScope<'p>>,
    depth: usize,
}

impl<'p> XEvaluationScope<'p> {
    pub fn root() -> Self {
        XEvaluationScope {
            values: HashMap::new(),
            recourse: None,
            ud_functions: HashMap::new(),
            parent: None,
            depth: 0,
        }
    }

    pub fn from_parent(
        parent: &'p XEvaluationScope<'p>,
        recourse: &'p XFunction,
        runtime: RTCell,
    ) -> Result<Self, String> {
        let new_depth = parent.depth + 1;
        if let Some(depth_limit) = runtime.borrow().limits.depth_limit {
            if new_depth > depth_limit {
                return Err(format!("Maximum stack depth of {} exceeded", depth_limit));
            }
        }
        Ok(XEvaluationScope {
            values: HashMap::new(),
            recourse: Some(recourse),
            ud_functions: HashMap::new(),
            parent: Some(parent),
            depth: parent.depth + 1,
        })
    }

    pub fn get(&self, name: DefaultSymbol) -> Option<Rc<ManagedXValue>> {
        self.values
            .get(&name)
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.get(name)))
    }

    pub fn get_ud_func(&self, name: DefaultSymbol) -> Option<Rc<XStaticFunction>> {
        self.ud_functions.get(&name).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.get_ud_func(name))
        })
    }

    pub fn add(&mut self, name: DefaultSymbol, value: Rc<ManagedXValue>) {
        self.values.insert(name, value);
    }

    pub fn add_from(&mut self, other: &Vec<Declaration>, runtime: RTCell) -> Result<(), String> {
        for decl in other {
            match decl {
                Declaration::Value(name, expr) => {
                    let value = expr.eval(self, false, runtime.clone())?.unwrap_value();
                    self.add(*name, value);
                }
                Declaration::UserFunction(name, func) => {
                    self.ud_functions.insert(*name, func.clone());
                }
                _ => {}
            }
        }
        Ok(())
    }

    pub fn ancestor(&'p self, depth: usize) -> &'p XEvaluationScope<'p> {
        if depth == 0 {
            self
        } else {
            self.parent.unwrap().ancestor(depth - 1)
        }
    }
}
