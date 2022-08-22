use crate::runtime::RTCell;
use crate::xexpr::XStaticFunction;
use crate::xvalue::{ManagedXValue, XFunction};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;
use string_interner::DefaultSymbol;

use crate::compilation_scope::Declaration;
use crate::util::rc_hash::RcHash;

pub struct XEvaluationScope<'p> {
    pub values: HashMap<DefaultSymbol, Rc<ManagedXValue>>,
    pub recourse: Option<&'p XFunction>,
    ud_static_functions: HashMap<DefaultSymbol, Vec<Rc<XStaticFunction>>>,
    ud_functions: HashMap<RcHash<XStaticFunction>, XFunction>,
    parent: Option<&'p XEvaluationScope<'p>>,
    depth: usize,
}

pub struct MultipleUD; // for trying to access an ambiguous user-defined

impl Debug for MultipleUD {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Multiple user-defined functions with the queried name")
    }
}

impl<'p> XEvaluationScope<'p> {
    pub fn root() -> Self {
        XEvaluationScope {
            values: HashMap::new(),
            recourse: None,
            ud_static_functions: HashMap::new(),
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
            ud_static_functions: HashMap::new(),
            ud_functions: HashMap::new(),
            parent: Some(parent),
            depth: parent.depth + 1,
        })
    }

    pub fn get_value(&self, name: DefaultSymbol) -> Option<Rc<ManagedXValue>> {
        self.values.get(&name).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.get_value(name))
        })
    }

    pub fn get_unique_ud_func(
        &self,
        name: DefaultSymbol,
    ) -> Result<Option<&XFunction>, MultipleUD> {
        // todo better return type
        let own = self
            .ud_static_functions
            .get(&name)
            .map(|m| {
                if m.len() > 1 {
                    Err(MultipleUD)
                } else {
                    Ok(self
                        .ud_functions
                        .get(&RcHash(m.iter().next().unwrap().clone()))
                        .unwrap())
                }
            })
            .transpose()?;
        let parents = self
            .parent
            .as_ref()
            .map(|parent| parent.get_unique_ud_func(name))
            .transpose()?
            .flatten();

        if own.is_some() && parents.is_some() {
            Err(MultipleUD)
        } else {
            Ok(own.or(parents))
        }
    }

    pub fn get_ud_func(&self, key: RcHash<XStaticFunction>) -> &XFunction {
        self.ud_functions
            .get(&key)
            .or_else(|| self.parent.map(|p| p.get_ud_func(key)))
            .unwrap()
    }

    pub fn add_value(&mut self, name: DefaultSymbol, value: Rc<ManagedXValue>) {
        self.values.insert(name, value);
    }

    pub fn add_from(&mut self, decl: &Declaration, runtime: RTCell) -> Result<(), String> {
        match decl {
            Declaration::Value(name, expr) => {
                let value = expr.eval(self, false, runtime)?.unwrap_value();
                self.add_value(*name, value);
            }
            Declaration::UserFunction(name, func) => {
                let evaled = self.lock_closure(func);
                self.ud_static_functions
                    .entry(*name)
                    .or_insert_with(Vec::new)
                    .push(func.clone());
                let key = RcHash(func.clone());
                self.ud_functions.insert(key, evaled);
            }
            _ => {}
        }
        Ok(())
    }

    pub fn lock_closure(&self, func: &Rc<XStaticFunction>) -> XFunction {
        if let XStaticFunction::UserFunction(uf) = func.as_ref() {
            let closure = Rc::new(
                uf.cvars
                    .iter()
                    .map(|&name| (name, self.get_value(name).unwrap()))
                    .collect(),
            );
            XFunction::UserFunction(func.clone(), closure)
        } else {
            unreachable!()
        }
    }

    pub fn ancestor(&'p self, depth: usize) -> &'p XEvaluationScope<'p> {
        if depth == 0 {
            self
        } else {
            self.parent.unwrap().ancestor(depth - 1)
        }
    }
}
