use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::iter::from_fn;
use std::rc::Rc;
use std::sync::Arc;
use string_interner::{DefaultSymbol, StringInterner};
use crate::{CompilationResult, CompilationError};
use crate::runtime::{RTCell};
use crate::xexpr::{resolve_overload, XExpr, XStaticFunction};
use crate::xtype::{XFuncSpec, XCompoundSpec, XType, CompoundKind};
use crate::xvalue::{DynBind, ManagedXValue, XFunction};

use derivative::Derivative;
use regex::internal::Exec;

pub type Identifier = DefaultSymbol;

pub struct XCompilationScope<'p> {
    pub values: HashMap<Identifier, (Option<XExpr>, Arc<XType>)>,
    pub types: HashMap<Identifier, Arc<XType>>,
    pub structs: HashMap<Identifier, Arc<XCompoundSpec>>,
    pub unions: HashMap<Identifier, Arc<XCompoundSpec>>,
    pub functions: HashMap<Identifier, Vec<Rc<XFunctionFactory>>>,
    pub recourse: Option<(Identifier, Rc<XFuncSpec>)>,
    pub closure_variables: HashSet<Identifier>,
    pub parent: Option<&'p XCompilationScope<'p>>,

    pub height: usize,
}

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub enum XFunctionFactory {
    Static(Rc<XStaticFunction>),
    Dynamic(
        #[derivative(Debug="ignore")]
        DynBind),
}

#[derive(Debug, Clone)]
pub enum XCompilationScopeItem {
    Value(Arc<XType>),
    NativeType(Arc<XType>),
    Compound(CompoundKind, Arc<XCompoundSpec>),
    Overload(Vec<Rc<XFunctionFactory>>),
}

impl<'p> XCompilationScope<'p> {
    pub fn root() -> Self {
        XCompilationScope {
            values: HashMap::new(),
            types: HashMap::new(),
            structs: HashMap::new(),
            functions: HashMap::new(),
            unions: HashMap::new(),
            parent: None,
            recourse: None,
            closure_variables: HashSet::new(),
            height: 0,
        }
    }

    pub fn from_parent(parent: &'p XCompilationScope<'p>, recourse_name: DefaultSymbol,
                       recourse_spec: XFuncSpec) -> Self {
        XCompilationScope {
            values: HashMap::new(),
            types: HashMap::new(),
            structs: HashMap::new(),
            functions: HashMap::new(),
            unions: HashMap::new(),
            parent: Some(parent),
            recourse: Some((recourse_name, Rc::new(recourse_spec))),
            closure_variables: HashSet::new(),
            height: parent.height + 1,
        }
    }

    pub fn from_parent_lambda(parent: &'p XCompilationScope<'p>) -> Self {
        XCompilationScope {
            values: HashMap::new(),
            types: HashMap::new(),
            structs: HashMap::new(),
            functions: HashMap::new(),
            unions: HashMap::new(),
            parent: Some(parent),
            recourse: None,
            closure_variables: HashSet::new(),
            height: parent.height + 1,
        }
    }

    fn ancestors(&self) -> impl Iterator<Item=&XCompilationScope<'p>> {
        let mut scope = self;
        return from_fn(move || {
            if let Some(parent) = scope.parent.as_deref() {
                scope = parent;
                return Some(parent);
            }
            return None;
        });
    }

    pub fn get(&self, name: DefaultSymbol) -> Option<XCompilationScopeItem> {
        self.get_with_depth(name).map(|i| i.0)
    }

    pub fn get_with_depth(&self, name: DefaultSymbol) -> Option<(XCompilationScopeItem, usize)> {
        fn helper(scope: &XCompilationScope<'_>, name: DefaultSymbol, depth: usize) -> Option<(XCompilationScopeItem, usize)> {
            let mut overloads = scope.functions.get(&name).map_or_else(|| vec![], |x| x.clone());
            match &scope.recourse {
                Some((rec_name, spec)) if rec_name == &name => {
                    overloads.push(Rc::new(XFunctionFactory::Static(Rc::new(XStaticFunction::Recourse(spec.clone(), 0)))));
                }
                _ => (),
            }
            if overloads.len() > 0 {
                for (depth, ancestor) in scope.ancestors().enumerate() {
                    if let Some(ancestor_overloads) = ancestor.functions.get(&name) {
                        // a scope might send us a recurse, in which case we need to increment its depth
                        let ancestor_overloads = ancestor_overloads.iter().map(|x| {
                            if let XFunctionFactory::Static(stat) = x.as_ref() {
                                if let XStaticFunction::Recourse(spec, ..) = stat.as_ref() {
                                    Rc::new(XFunctionFactory::Static(Rc::new(XStaticFunction::Recourse(spec.clone(), depth + 1))))
                                } else {
                                    x.clone()
                                }
                            } else {
                                x.clone()
                            }
                        });
                        overloads.extend(ancestor_overloads);
                    }
                }
                return Some((XCompilationScopeItem::Overload(overloads), depth));
            }
            if let Some((_, value)) = scope.values.get(&name) {
                return Some((XCompilationScopeItem::Value(value.clone()), depth));
            }
            if let Some(struct_spec) = scope.structs.get(&name) {
                return Some((XCompilationScopeItem::Compound(CompoundKind::Struct, struct_spec.clone()), depth));
            }
            if let Some(struct_spec) = scope.unions.get(&name) {
                return Some((XCompilationScopeItem::Compound(CompoundKind::Union, struct_spec.clone()), depth));
            }
            if let Some(type_spec) = scope.types.get(&name) {
                return Some((XCompilationScopeItem::NativeType(type_spec.clone()), depth));
            }
            scope.parent.as_ref()
                .and_then(|parent| helper(parent, name, depth + 1))
                .map(|(item, depth)| (match item {
                    XCompilationScopeItem::Overload(overloads) => {
                        XCompilationScopeItem::Overload(overloads.iter().map(|x| {
                            if let XFunctionFactory::Static(stat) = x.as_ref() {
                                if let XStaticFunction::Recourse(spec, ..) = stat.as_ref() {
                                    Rc::new(XFunctionFactory::Static(Rc::new(XStaticFunction::Recourse(spec.clone(), depth + 1))))
                                } else {
                                    x.clone()
                                }
                            } else {
                                x.clone()
                            }
                        }).collect())
                    }
                    other => other,
                }, depth))
        }
        helper(self, name, 0)
    }

    pub fn add_param(&mut self, name: DefaultSymbol, type_: Arc<XType>) -> Result<(), CompilationError> {
        if let Some(other) = self.get(name) {
            Err(CompilationError::NameAlreadyDefined { name, other })
        } else {
            self.values.insert(name, (None, type_));
            Ok(())
        }
    }

    pub fn add_var(&mut self, name: DefaultSymbol, expr: XExpr) -> Result<Declaration, CompilationError> {
        if let Some(other) = self.get(name) {
            Err(CompilationError::NameAlreadyDefined { name, other })
        } else {
            self.values.insert(name, (Some(expr.clone()), expr.xtype()?));
            Ok(Declaration::Value(name, expr))
        }
    }

    pub fn add_func(&mut self, name: DefaultSymbol, func: XStaticFunction) -> Result<Declaration, CompilationError> {
        // todo ensure no shadowing
        let item = Rc::new(func);
        self.functions.entry(name).or_insert_with(|| vec![]).push(Rc::new(XFunctionFactory::Static(item.clone())));
        Ok(Declaration::UserFunction(name, item))
    }

    pub fn add_dyn_func(&mut self, name: DefaultSymbol, func: impl Fn(Option<&Vec<XExpr>>, &Vec<Arc<XType>>, &XCompilationScope<'_>) -> Result<Rc<XStaticFunction>, String> + 'static) -> Result<(), CompilationError> {
        // todo ensure no shadowing?
        Ok(self.functions.entry(name).or_insert_with(|| vec![]).push(Rc::new(XFunctionFactory::Dynamic(Rc::new(func)))))
    }

    pub fn add_func_intern(&mut self, name: &'static str, func: XStaticFunction, interner: &mut StringInterner) -> Result<Declaration, CompilationError> {
        self.add_func(interner.get_or_intern_static(name), func)
    }

    pub fn add_dyn_func_intern(&mut self, name: &'static str, func: impl Fn(Option<&Vec<XExpr>>, &Vec<Arc<XType>>, &XCompilationScope<'_>) -> Result<Rc<XStaticFunction>, String> + 'static, interner: &mut StringInterner) -> Result<(), CompilationError> {
        self.add_dyn_func(interner.get_or_intern_static(name), func)
    }

    pub fn add_compound(&mut self, name: DefaultSymbol, kind: CompoundKind, spec: XCompoundSpec) -> Result<Declaration, CompilationError> {
        if kind == CompoundKind::Struct {
            self.add_struct(name, spec)
        } else {
            self.add_union(name, spec)
        }
    }

    fn add_struct(&mut self, name: DefaultSymbol, struct_spec: XCompoundSpec) -> Result<Declaration, CompilationError> {
        // todo ensure no shadowing
        self.structs.insert(name, Arc::new(struct_spec.clone()));
        Ok(Declaration::Struct(struct_spec))
    }

    fn add_union(&mut self, name: DefaultSymbol, union_spec: XCompoundSpec) -> Result<Declaration, CompilationError> {
        // todo ensure no shadowing
        self.unions.insert(name, Arc::new(union_spec.clone()));
        Ok(Declaration::Union(union_spec))
    }

    pub fn add_native_type(&mut self, name: DefaultSymbol, type_: Arc<XType>) -> Result<(), CompilationError> {
        if let Some(other) = self.get(name) {
            Err(CompilationError::NameAlreadyDefined { name, other })
        } else {
            self.types.insert(name, type_);
            Ok(())
        }
    }

    pub fn add_native_type_intern(&mut self, name: &'static str, type_: Arc<XType>, interner: &mut StringInterner) -> Result<(), CompilationError> {
        self.add_native_type(interner.get_or_intern_static(name), type_)
    }

    pub fn to_eval_scope(&self, runtime: RTCell) -> Result<XEvaluationScope, CompilationError> {
        let mut ret = XEvaluationScope::root();
        let mut current_scope = Some(self);
        while let Some(s) = current_scope {
            for (name, (expr, _)) in &s.values {
                match expr {
                    None => {}
                    Some(expr) => {
                        let evaluated = expr.eval(&ret, false, runtime.clone());
                        let value = match evaluated {
                            Ok(value) => value.unwrap_value(),
                            Err(_) => {
                                // we actually allow this error to happen, since some expressions might depend on params or other unknown values
                                // todo find some way to report this
                                // todo catch limit errors
                                continue;
                            }
                        };
                        ret.add(*name, value);
                    }
                }
            }
            current_scope = s.parent;
        }
        Ok(ret)
    }

    pub fn resolve_overload(&self, name: Identifier, types: Vec<Arc<XType>>)->Result<XExpr, String>{
        let overloads = match self.get(name){
            Some(XCompilationScopeItem::Overload(overloads)) => overloads,
            _ => return Err(format!("{:?} is not an overload", name)) // todo better error
        };
        resolve_overload(overloads, None, &types, name, &self)
            .map_err(|_| "overload resolution failed".to_string())
    }
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Value(DefaultSymbol, XExpr),
    Struct(XCompoundSpec),
    Union(XCompoundSpec),
    UserFunction(DefaultSymbol, Rc<XStaticFunction>),
}

impl Hash for Declaration {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Declaration::Value(name, _) => name.hash(state),
            Declaration::Struct(spec) => spec.hash(state),
            Declaration::UserFunction(name, _) => {
                name.hash(state);
            }
            Declaration::Union(spec) => spec.hash(state),
        }
    }
}

pub struct XEvaluationScope<'p> {
    pub values: HashMap<DefaultSymbol, Rc<ManagedXValue>>,
    pub recourse: Option<&'p XFunction>,
    parent: Option<&'p XEvaluationScope<'p>>,
    depth: usize,
}

impl<'p> XEvaluationScope<'p> {
    pub fn root() -> Self {
        XEvaluationScope {
            values: HashMap::new(),
            recourse: None,
            parent: None,
            depth: 0,
        }
    }

    pub fn from_parent(parent: &'p XEvaluationScope<'p>, recourse: &'p XFunction, runtime: RTCell) -> Result<Self, String> {
        let new_depth = parent.depth + 1;
        if let Some(depth_limit) = runtime.borrow().limits.depth_limit {
            if new_depth > depth_limit {
                return Err(format!("Maximum stack depth of {} exceeded", depth_limit));
            }
        }
        Ok(XEvaluationScope {
            values: HashMap::new(),
            recourse: Some(recourse),
            parent: Some(parent),
            depth: parent.depth + 1,
        })
    }

    pub fn get(&self, name: DefaultSymbol) -> Option<Rc<ManagedXValue>> {
        self.values.get(&name).cloned().or_else(|| self.parent.as_ref().and_then(|parent| parent.get(name)))
    }

    pub fn add(&mut self, name: DefaultSymbol, value: Rc<ManagedXValue>) {
        self.values.insert(name, value);
    }

    pub fn add_from(&mut self, other: &Vec<Declaration>, runtime: RTCell) -> Result<(), String> {
        for decl in other {
            match decl {
                Declaration::Value(name, expr) => {
                    let value = expr.eval(self, false, runtime.clone())?.unwrap_value();
                    self.add(name.clone(), value);
                }
                _ => {}
            }
        };
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