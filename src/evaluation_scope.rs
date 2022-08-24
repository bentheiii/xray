use crate::runtime::RTCell;
use crate::xexpr::XStaticFunction;
use crate::xvalue::{ManagedXValue, XFunction};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

use crate::compilation_scope::Declaration;
use crate::util::rc_hash::RcHash;
use crate::{let_match, Identifier, RootCompilationScope};

pub struct XEvaluationScope<'p> {
    pub values: HashMap<Identifier, Rc<ManagedXValue>>,
    pub recourse: Option<&'p XFunction>,
    ud_static_functions: HashMap<Identifier, Vec<Rc<XStaticFunction>>>,
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
    pub(crate) fn root() -> Self {
        XEvaluationScope {
            values: HashMap::new(),
            recourse: None,
            ud_static_functions: HashMap::new(),
            ud_functions: HashMap::new(),
            parent: None,
            depth: 0,
        }
    }

    pub(crate) fn from_parent(
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

    pub(crate) fn get_value(&self, name: Identifier) -> Option<Rc<ManagedXValue>> {
        self.values.get(&name).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.get_value(name))
        })
    }

    fn get_unique_ud_func(&self, name: Identifier) -> Result<Option<&XFunction>, MultipleUD> {
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

    pub(crate) fn get_ud_func(&self, key: RcHash<XStaticFunction>) -> &XFunction {
        self.ud_functions
            .get(&key)
            .or_else(|| self.parent.map(|p| p.get_ud_func(key)))
            .unwrap()
    }

    pub(crate) fn add_value(&mut self, name: Identifier, value: Rc<ManagedXValue>) {
        self.values.insert(name, value);
    }

    pub(crate) fn add_from(&mut self, decl: &Declaration, runtime: RTCell) -> Result<(), String> {
        match decl {
            Declaration::Value(name, expr, ..) => {
                let value = expr.eval(self, false, runtime)?.unwrap_value();
                self.add_value(*name, value);
            }
            Declaration::Function(name, func) => {
                if let XStaticFunction::UserFunction(..) = func.as_ref() {
                    let evaled = self.lock_closure(func);
                    self.ud_static_functions
                        .entry(*name)
                        .or_insert_with(Vec::new)
                        .push(func.clone());
                    let key = RcHash(func.clone());
                    self.ud_functions.insert(key, evaled);
                }
            }
            _ => {}
        }
        Ok(())
    }

    pub(crate) fn lock_closure(&self, func: &Rc<XStaticFunction>) -> XFunction {
        let uf = let_match!(func.as_ref(); XStaticFunction::UserFunction(uf) => uf);
        let closure = Rc::new(
            uf.cvars
                .iter()
                .map(|&name| (name, self.get_value(name).unwrap()))
                .collect(),
        );
        XFunction::UserFunction(func.clone(), closure)
    }

    pub(crate) fn ancestor(&'p self, depth: usize) -> &'p XEvaluationScope<'p> {
        if depth == 0 {
            self
        } else {
            self.parent.unwrap().ancestor(depth - 1)
        }
    }
}

pub struct RootEvaluationScope<'c> {
    scope: XEvaluationScope<'static>,
    compilation_scope: &'c RootCompilationScope,
}

impl<'c> RootEvaluationScope<'c> {
    pub fn from_compilation_scope(
        comp_scope: &'c RootCompilationScope,
    ) -> Result<Self, String> {
        let mut ret = Self {
            scope: XEvaluationScope::root(),
            compilation_scope: comp_scope,
        };
        for decl in &comp_scope.scope.declarations {
            ret.declare(decl)?
        }
        Ok(ret)
    }

    pub fn get_value(&mut self, name: &str) -> Option<Rc<ManagedXValue>> {
        self.compilation_scope
            .get_identifer(name)
            .and_then(|id| self.scope.get_value(id))
    }

    pub fn get_user_defined_function(&self, name: &str) -> Result<Option<&XFunction>, MultipleUD> {
        self.compilation_scope
            .get_identifer(name)
            .and_then(|id| self.scope.get_unique_ud_func(id).transpose())
            .transpose()
    }

    pub fn declare(&mut self, decl: &Declaration) -> Result<(), String> {
        self.scope.add_from(decl, self.compilation_scope.runtime.clone())
    }

    pub fn eval(
        &self,
        func: &XFunction,
        args: &[Rc<ManagedXValue>],
    ) -> Result<Rc<ManagedXValue>, String> {
        func.eval_values(args, &self.scope, self.compilation_scope.runtime.clone())
    }
}
