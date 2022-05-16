use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::iter::from_fn;
use std::rc::Rc;
use std::sync::Arc;
use string_interner::{DefaultSymbol, StringInterner};
use crate::xexpr::{XExpr, XStaticFunction};
use crate::xtype::{XFuncSpec, XStructSpec, XType};
use crate::xvalue::{XFunction, XValue};

pub type Identifier = DefaultSymbol;

pub struct XCompilationScope<'p> {
    pub values: HashMap<Identifier, (Option<XExpr>, Arc<XType>)>,
    pub types: HashMap<Identifier, Arc<XType>>,
    pub structs: HashMap<Identifier, XStructSpec>,
    pub functions: HashMap<Identifier, Vec<Rc<XStaticFunction>>>,
    pub recourse: Option<(Identifier, XFuncSpec)>,
    pub closure_variables: HashSet<Identifier>,
    pub parent: Option<&'p XCompilationScope<'p>>,

    pub height: usize,
}

#[derive(Debug, Clone)]
pub enum XCompilationScopeItem {
    Value(Arc<XType>),
    NativeType(Arc<XType>),
    Struct(XStructSpec),
    Overload(Vec<Rc<XStaticFunction>>),
}

impl<'p> XCompilationScope<'p> {
    pub fn root() -> Self {
        XCompilationScope {
            values: HashMap::new(),
            types: HashMap::new(),
            structs: HashMap::new(),
            functions: HashMap::new(),
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
            parent: Some(parent),
            recourse: Some((recourse_name, recourse_spec)),
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
                    overloads.push(Rc::new(XStaticFunction::Recourse(spec.clone())));
                }
                _ => (),
            }
            if overloads.len() > 0 {
                for ancestor in scope.ancestors() {
                    if let Some(ancestor_overloads) = ancestor.functions.get(&name) {
                        overloads.append(&mut ancestor_overloads.clone());
                    }
                }
                return Some((XCompilationScopeItem::Overload(overloads), depth));
            }
            if let Some((_, value)) = scope.values.get(&name) {
                return Some((XCompilationScopeItem::Value(value.clone()), depth));
            }
            if let Some(struct_spec) = scope.structs.get(&name) {
                return Some((XCompilationScopeItem::Struct(struct_spec.clone()), depth));
            }
            if let Some(type_spec) = scope.types.get(&name) {
                return Some((XCompilationScopeItem::NativeType(type_spec.clone()), depth));
            }
            scope.parent.as_ref().and_then(|parent| helper(parent, name, depth + 1))
        }
        helper(self, name, 0)
    }

    pub fn add_param(&mut self, name: DefaultSymbol, type_: Arc<XType>) -> Result<(), String> {
        if self.get(name).is_some() {
            // todo fix symbol shit
            Err(format!("Variable {:?} already defined", name))
        } else {
            self.values.insert(name, (None, type_));
            Ok(())
        }
    }

    pub fn add_var(&mut self, name: DefaultSymbol, expr: XExpr) -> Result<Declaration, String> {
        if self.get(name).is_some() {
            // todo fix symbol shit
            Err(format!("Variable {:?} already defined", name))
        } else {
            self.values.insert(name, (Some(expr.clone()), expr.xtype()?));
            Ok(Declaration::Value(name, expr))
        }
    }

    pub fn add_func(&mut self, name: DefaultSymbol, func: XStaticFunction) -> Result<Declaration, String> {
        // todo ensure no shadowing
        let item = Rc::new(func);
        self.functions.entry(name).or_insert_with(|| vec![]).push(item.clone());
        Ok(Declaration::UserFunction(name, item))
    }

    pub fn add_func_intern(&mut self, name: &'static str, func: XStaticFunction, interner: &mut StringInterner) -> Result<Declaration, String> {
        self.add_func(interner.get_or_intern_static(name), func)
    }

    pub fn add_struct(&mut self, name: DefaultSymbol, struct_spec: XStructSpec) -> Result<Declaration, String> {
        // todo ensure no shadowing
        self.structs.insert(name, struct_spec.clone());
        Ok(Declaration::Struct(struct_spec))
    }

    pub fn add_native_type(&mut self, name: DefaultSymbol, type_: Arc<XType>) -> Result<(), String> {
        if self.get(name).is_some() {
            Err(format!("Native type {:?} already defined", name))
        } else {
            self.types.insert(name, type_);
            Ok(())
        }
    }

    pub fn add_native_type_intern(&mut self, name: &'static str, type_: Arc<XType>, interner: &mut StringInterner) -> Result<(), String> {
        self.add_native_type(interner.get_or_intern_static(name), type_)
    }

    pub fn to_eval_scope(&self) -> Result<XEvaluationScope, String> {
        let mut ret = XEvaluationScope::root();
        let mut current_scope = Some(self);
        while let Some(s) = current_scope {
            for (name, (expr, _)) in &s.values {
                match expr {
                    None => {}
                    Some(expr) => {
                        let evaluated = expr.eval(&ret, false);
                        let value = match evaluated {
                            Ok(value) => value.unwrap_value(),
                            Err(_) => {
                                // we actually allow this error to happen, since some expressions might depend on params or other unknown values
                                // todo find some way to report this
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
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Declaration {
    Value(DefaultSymbol, XExpr),
    Struct(XStructSpec),
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
        }
    }
}

pub struct XEvaluationScope<'p> {
    pub values: HashMap<DefaultSymbol, Rc<XValue>>,
    pub recourse: Option<&'p XFunction>,
    parent: Option<&'p XEvaluationScope<'p>>,
}

impl<'p> XEvaluationScope<'p> {
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

    pub fn get(&self, name: DefaultSymbol) -> Option<Rc<XValue>> {
        self.values.get(&name).cloned().or_else(|| self.parent.as_ref().and_then(|parent| parent.get(name)))
    }

    pub fn add(&mut self, name: DefaultSymbol, value: Rc<XValue>) {
        self.values.insert(name, value);
    }

    pub fn add_from(&mut self, other: &Vec<Declaration>) -> Result<(), String> {
        for decl in other {
            match decl {
                Declaration::Value(name, expr) => {
                    let value = expr.eval(self, false)?.unwrap_value();
                    self.add(name.clone(), value);
                }
                _ => {}
            }
        };
        Ok(())
    }

    pub fn to_closure(&self) -> HashMap<DefaultSymbol, Rc<XValue>>
    {
        if let Some(parent) = self.parent {
            let mut values = parent.to_closure();
            values.extend(self.values.iter().map(|(k, v)| (k.clone(), v.clone())));
            values
        } else {
            self.values.clone()
        }
    }
}