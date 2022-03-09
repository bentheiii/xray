use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::iter::from_fn;
use std::rc::Rc;
use std::sync::Arc;
use crate::xexpr::{XExpr, XStaticFunction};
use crate::xtype::{XFuncSpec, XStructSpec, XType};
use crate::xvalue::{XFunction, XValue};

pub struct XCompilationScope<'p> {
    pub values: HashMap<String, Arc<XType>>,
    pub types: HashMap<String, Arc<XType>>,
    pub structs: HashMap<String, XStructSpec>,
    pub functions: HashMap<String, Vec<XStaticFunction>>,
    pub parent: Option<&'p XCompilationScope<'p>>,
    pub recourse: Option<(String, XFuncSpec)>,
}

#[derive(Debug, Clone)]
pub enum XCompilationScopeItem {
    Value(Arc<XType>),
    NativeType(Arc<XType>),
    Struct(XStructSpec),
    Overload(Vec<XStaticFunction>),
}

impl<'p> XCompilationScope<'p>{
    pub fn root() -> Self {
        XCompilationScope {
            values: HashMap::new(),
            types: HashMap::new(),
            structs: HashMap::new(),
            functions: HashMap::new(),
            parent: None,
            recourse: None,
        }
    }

    pub fn from_parent(parent: &'p XCompilationScope<'p>, recourse_name: String,
                       recourse_spec: XFuncSpec) -> Self {
        XCompilationScope {
            values: HashMap::new(),
            types: HashMap::new(),
            structs: HashMap::new(),
            functions: HashMap::new(),
            parent: Some(parent),
            recourse: Some((recourse_name, recourse_spec)),
        }
    }

    fn ancestors(&self) -> impl Iterator<Item=&XCompilationScope<'p>> {
        let mut scope = self;
        return from_fn(move || {
            if let Some(parent) = scope.parent.as_deref() {
                scope = parent;
                return Some(parent);
            }
            return None
        });
    }

    pub fn get(&self, name: &str) -> Option<XCompilationScopeItem> {
        let mut overloads = self.functions.get(name).map_or_else(|| vec![], |x| x.clone());
        match &self.recourse {
            Some((rec_name, spec)) if rec_name == name => {
                overloads.push(XStaticFunction::Recourse(spec.clone()));
            },
            _ => (),
        }
        if overloads.len() > 0 {
            for ancestor in self.ancestors() {
                if let Some(ancestor_overloads) = ancestor.functions.get(name) {
                    overloads.append(&mut ancestor_overloads.clone());
                }
            }
            return Some(XCompilationScopeItem::Overload(overloads));
        }
        if let Some(value) = self.values.get(name) {
            return Some(XCompilationScopeItem::Value(value.clone()));
        }
        if let Some(struct_spec) = self.structs.get(name) {
            return Some(XCompilationScopeItem::Struct(struct_spec.clone()));
        }
        if let Some(type_spec) = self.types.get(name) {
            return Some(XCompilationScopeItem::NativeType(type_spec.clone()));
        }
        self.parent.as_ref().and_then(|parent| parent.get(name))
    }

    pub fn add_param(&mut self, name: &str, type_: Arc<XType>) -> Result<(), String> {
        if self.get(name).is_some() {
            Err(format!("Variable {} already defined", name))
        }
        else {
            self.values.insert(name.to_string(), type_);
            Ok(())
        }
    }

    pub fn add_var(&mut self, name: &str, expr: XExpr) -> Result<Declaration, String> {
        if self.get(name).is_some() {
            Err(format!("Variable {} already defined", name))
        }
        else {
            self.values.insert(name.to_string(), expr.xtype()?);
            Ok(Declaration::Value(name.to_string(), expr))
        }
    }

    pub fn add_func(&mut self, name: &str, func: XStaticFunction)-> Result<Declaration, String> {
        // todo ensure no shadowing
        self.functions.entry(name.to_string()).or_insert_with(|| vec![]).push( func.clone());
        Ok(Declaration::UserFunction(name.to_string(), func))
    }

    pub fn add_struct(&mut self, name: &str, struct_spec: XStructSpec)-> Result<Declaration, String> {
        // todo ensure no shadowing
        self.structs.insert(name.to_string(), struct_spec.clone());
        Ok(Declaration::Struct(struct_spec))
    }

    pub fn add_native_type(&mut self, name: &str, type_: Arc<XType>) -> Result<(), String> {
        if self.get(name).is_some() {
            Err(format!("Native type {} already defined", name))
        }
        else {
            self.types.insert(name.to_string(), type_);
            Ok(())
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Declaration {
    Value(String, XExpr),
    Struct(XStructSpec),
    UserFunction(String, XStaticFunction),
}

impl Hash for Declaration {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Declaration::Value(name, _) => name.hash(state),
            Declaration::Struct(spec) => spec.hash(state),
            Declaration::UserFunction(name, _) => {
                name.hash(state);
            }
        }
    }
}

pub struct XEvaluationScope<'p>{
    pub values: HashMap<String, Rc<XValue>>,
    pub recourse: Option<&'p XFunction>,
    parent: Option<&'p XEvaluationScope<'p>>,
}

impl<'p> XEvaluationScope<'p>{
    pub fn root() -> Self {
        XEvaluationScope {
            values: HashMap::new(),
            recourse: None,
            parent: None,
        }
    }

    pub fn from_parent(parent: &'p XEvaluationScope<'p>, recourse: &'p XFunction) -> Self {
        XEvaluationScope {
            values: HashMap::new(),
            recourse: Some(recourse),
            parent: Some(parent),
        }
    }

    pub fn get(&self, name: &str) -> Option<Rc<XValue>> {
        self.values.get(name).cloned().or_else(|| self.parent.as_ref().and_then(|parent| parent.get(name)))
    }

    pub fn add(&mut self, name: &str, value: Rc<XValue>) {
        self.values.insert(name.to_string(), value);
    }

    pub fn add_from(&mut self, other: &Vec<Declaration>)-> Result<(), String> {
        for decl in other {
            match decl {
                Declaration::Value(name, expr) => {
                    let value = expr.eval(self, false)?.unwrap_value();
                    self.add(&name, value);
                }
                _=>{}
            }
        };
        Ok(())
    }
}