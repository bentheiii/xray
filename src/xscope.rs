use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::iter::from_fn;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use crate::xexpr::{XExpr, XStaticFunction};
use crate::XStaticExpr;
use crate::xtype::{XFuncSpec, XStructSpec, XType};
use crate::xvalue::{XFunction, XValue};

pub struct XCompilationScope<'p, 's: 'p> {
    _marker: PhantomData<&'s ()>,

    pub values: HashMap<String, XType>,
    pub types: HashMap<String, XType>,
    pub structs: HashMap<String, XStructSpec>,
    pub functions: HashMap<String, Vec<XStaticFunction<'s>>>,
    pub parent: Option<&'p XCompilationScope<'p, 's>>,
    pub recourse: Option<(String, XFuncSpec)>,
}

#[derive(Debug, Clone)]
pub enum XCompilationScopeItem<'a> {
    Value(XType),
    NativeType(XType),
    Struct(XStructSpec),
    Overload(Vec<XStaticFunction<'a>>),
}

impl<'p, 's: 'p> XCompilationScope<'p, 's>{
    pub fn root() -> Self {
        XCompilationScope {
            _marker: PhantomData,
            values: HashMap::new(),
            types: HashMap::new(),
            structs: HashMap::new(),
            functions: HashMap::new(),
            parent: None,
            recourse: None,
        }
    }

    pub fn from_parent(parent: &'p XCompilationScope<'p, 's>, recourse_name: String,
                       recourse_spec: XFuncSpec) -> Self {
        XCompilationScope {
            _marker: PhantomData,
            values: HashMap::new(),
            types: HashMap::new(),
            structs: HashMap::new(),
            functions: HashMap::new(),
            parent: Some(parent),
            recourse: Some((recourse_name, recourse_spec)),
        }
    }

    fn ancestors(&self) -> impl Iterator<Item=&XCompilationScope<'p, 's>> {
        let mut scope = self;
        return from_fn(move || {
            if let Some(parent) = scope.parent.as_deref() {
                scope = parent;
                return Some(parent);
            }
            return None
        });
    }

    pub fn get(&self, name: &str) -> Option<XCompilationScopeItem<'s>> {
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

    pub fn add_param(&mut self, name: &str, type_: XType) -> Result<(), String> {
        if self.get(name).is_some() {
            Err(format!("Variable {} already defined", name))
        }
        else {
            self.values.insert(name.to_string(), type_);
            Ok(())
        }
    }

    pub fn add_var(&mut self, name: &str, expr: XExpr<'s>) -> Result<Declaration<'s>, String> {
        if self.get(name).is_some() {
            Err(format!("Variable {} already defined", name))
        }
        else {
            self.values.insert(name.to_string(), expr.xtype()?);
            Ok(Declaration::Value(name.to_string(), expr))
        }
    }

    pub fn add_func(&mut self, name: &str, func: XStaticFunction<'s>)-> Result<Declaration<'s>, String> {
        // todo ensure no shadowing
        self.functions.entry(name.to_string()).or_insert_with(|| vec![]).push( func.clone());
        Ok(Declaration::UserFunction(name.to_string(), func))
    }

    pub fn add_struct(&mut self, name: &str, struct_spec: XStructSpec)-> Result<Declaration<'s>, String> {
        // todo ensure no shadowing
        self.structs.insert(name.to_string(), struct_spec.clone());
        Ok(Declaration::Struct(struct_spec))
    }

    pub fn add_native_type(&mut self, name: &str, type_: XType) -> Result<(), String> {
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
pub enum Declaration<'a> {
    Value(String, XExpr<'a>),
    Struct(XStructSpec),
    UserFunction(String, XStaticFunction<'a>),
}

impl Hash for Declaration<'_> {
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

pub struct XEvaluationScope<'p, 's: 'p>{
    _marker: PhantomData<&'s ()>,

    pub values: HashMap<String, Rc<XValue<'s>>>,
    pub recourse: Option<&'p XFunction<'s>>,
    parent: Option<&'p XEvaluationScope<'p, 's>>,
}

impl<'p, 's> XEvaluationScope<'p, 's>{
    pub fn root() -> Self {
        XEvaluationScope {
            _marker: PhantomData,
            values: HashMap::new(),
            recourse: None,
            parent: None,
        }
    }

    pub fn from_parent(parent: &'p XEvaluationScope<'p, 's>, recourse: &'p XFunction<'s>) -> Self {
        XEvaluationScope {
            _marker: PhantomData,
            values: HashMap::new(),
            recourse: Some(recourse),
            parent: Some(parent),
        }
    }

    pub fn get(&self, name: &str) -> Option<Rc<XValue<'s>>> {
        self.values.get(name).cloned().or_else(|| self.parent.as_ref().and_then(|parent| parent.get(name)))
    }

    pub fn add(&mut self, name: &str, value: Rc<XValue<'s>>) {
        self.values.insert(name.to_string(), value);
    }

    pub fn add_from(&mut self, other: &'s Vec<Declaration<'s>>)-> Result<(), String> {
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