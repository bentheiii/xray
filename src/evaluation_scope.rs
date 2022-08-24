use crate::runtime::RTCell;
use crate::xexpr::XStaticFunction;
use crate::xvalue::{ManagedXValue, XFunction};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::io::Write;
use std::rc::Rc;

use crate::compilation_scope::Declaration;
use crate::util::rc_hash::RcHash;
use crate::{let_match, Identifier, RootCompilationScope};

pub type EvaluatedVariable<W> = Result<Rc<ManagedXValue<W>>, String>;

pub struct XEvaluationScope<'p, W: Write + 'static> {
    pub values: HashMap<Identifier, EvaluatedVariable<W>>,
    pub recourse: Option<&'p XFunction<W>>,
    ud_static_functions: HashMap<Identifier, Vec<Rc<XStaticFunction<W>>>>,
    ud_functions: HashMap<RcHash<XStaticFunction<W>>, XFunction<W>>,
    parent: Option<&'p XEvaluationScope<'p, W>>,
    depth: usize,
}

pub struct MultipleUD; // for trying to access an ambiguous user-defined

impl Debug for MultipleUD {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Multiple user-defined functions with the queried name")
    }
}

impl<'p, W: Write + 'static> XEvaluationScope<'p, W> {
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
        parent: &'p XEvaluationScope<'p, W>,
        recourse: &'p XFunction<W>,
        runtime: RTCell<W>,
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

    pub(crate) fn get_value(&self, name: Identifier) -> Option<EvaluatedVariable<W>> {
        self.values.get(&name).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.get_value(name))
        })
    }

    fn get_unique_ud_func(&self, name: Identifier) -> Result<Option<&XFunction<W>>, MultipleUD> {
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

    pub(crate) fn get_ud_func(&self, key: RcHash<XStaticFunction<W>>) -> &XFunction<W> {
        self.ud_functions
            .get(&key)
            .or_else(|| self.parent.map(|p| p.get_ud_func(key)))
            .unwrap()
    }

    pub(crate) fn add_value(&mut self, name: Identifier, value: EvaluatedVariable<W>) {
        self.values.insert(name, value);
    }

    pub(crate) fn add_from_declaration(&mut self, decl: &Declaration<W>, runtime: RTCell<W>) -> Result<(), String> {
        match decl {
            Declaration::Value(name, expr, ..) => {
                let value = expr.eval(self, false, runtime)
                    .map(|v| v.unwrap_value());
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

    pub(crate) fn lock_closure(&self, func: &Rc<XStaticFunction<W>>) -> XFunction<W> {
        let uf = let_match!(func.as_ref(); XStaticFunction::UserFunction(uf) => uf);
        let closure = Rc::new(
            uf.cvars
                .iter()
                .map(|&name| (name, self.get_value(name).unwrap()))
                .collect(),
        );
        XFunction::UserFunction(func.clone(), closure)
    }

    pub(crate) fn ancestor(&'p self, depth: usize) -> &'p XEvaluationScope<'p, W> {
        if depth == 0 {
            self
        } else {
            self.parent.unwrap().ancestor(depth - 1)
        }
    }
}

pub struct RootEvaluationScope<'c, W: Write + 'static> {
    scope: XEvaluationScope<'static, W>,
    compilation_scope: &'c RootCompilationScope<W>,
}

impl<'c, W: Write + 'static> RootEvaluationScope<'c, W> {
    pub fn from_compilation_scope(
        comp_scope: &'c RootCompilationScope<W>,
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

    pub fn get_value(&mut self, name: &str) -> Option<EvaluatedVariable<W>> {
        self.compilation_scope
            .get_identifer(name)
            .and_then(|id| self.scope.get_value(id))
    }

    pub fn get_user_defined_function(&self, name: &str) -> Result<Option<&XFunction<W>>, MultipleUD> {
        self.compilation_scope
            .get_identifer(name)
            .and_then(|id| self.scope.get_unique_ud_func(id).transpose())
            .transpose()
    }

    pub fn declare(&mut self, decl: &Declaration<W>) -> Result<(), String> {
        self.scope.add_from_declaration(decl, self.compilation_scope.runtime.clone())
    }

    pub fn eval(
        &self,
        func: &XFunction<W>,
        args: &[Rc<ManagedXValue<W>>],
    ) -> Result<Rc<ManagedXValue<W>>, String> {
        func.eval_values(args, &self.scope, self.compilation_scope.runtime.clone())
    }
}
