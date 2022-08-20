use crate::parser::Rule;
use crate::runtime::RTCell;
use crate::xexpr::{resolve_overload, XExpr, XStaticFunction};
use crate::xtype::{CompoundKind, XCompoundSpec, XFuncSpec, XType};
use crate::xvalue::{DynBind, ManagedXValue, XFunction};
use crate::{
    Bind, CompilationError, CompilationResult, Identifier, TracedCompilationError, UfData,
    XCallableSpec, XCompoundFieldSpec, XEvaluationScope, XExplicitArgSpec, XExplicitFuncSpec,
    XStaticExpr,
};
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::iter;
use std::iter::from_fn;
use std::rc::Rc;
use std::sync::Arc;
use string_interner::{DefaultSymbol, StringInterner};

use derivative::Derivative;
use pest::iterators::Pair;
use pest::prec_climber::Assoc::{Left, Right};
use pest::prec_climber::{Operator, PrecClimber};

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
    Dynamic(#[derivative(Debug = "ignore")] DynBind),
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

    pub fn from_parent(
        parent: &'p XCompilationScope<'p>,
        recourse_name: DefaultSymbol,
        recourse_spec: XFuncSpec,
    ) -> Self {
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

    fn ancestors(&self) -> impl Iterator<Item = &XCompilationScope<'p>> {
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
        fn helper(
            scope: &XCompilationScope<'_>,
            name: DefaultSymbol,
            depth: usize,
        ) -> Option<(XCompilationScopeItem, usize)> {
            let mut overloads = scope
                .functions
                .get(&name)
                .map_or_else(|| vec![], |x| x.clone());
            match &scope.recourse {
                Some((rec_name, spec)) if rec_name == &name => {
                    overloads.push(Rc::new(XFunctionFactory::Static(Rc::new(
                        XStaticFunction::Recourse(spec.clone(), 0),
                    ))));
                }
                _ => (),
            }
            if overloads.len() > 0 {
                for (depth, ancestor) in scope.ancestors().enumerate() {
                    if let Some(ancestor_overloads) = ancestor.functions.get(&name) {
                        let ancestor_overloads = ancestor_overloads.iter().map(|x| {
                            if let XFunctionFactory::Static(stat) = x.as_ref() {
                                if let XStaticFunction::Recourse(spec, ..) = stat.as_ref() {
                                    Rc::new(XFunctionFactory::Static(Rc::new(
                                        XStaticFunction::Recourse(spec.clone(), depth),
                                    )))
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
                return Some((
                    XCompilationScopeItem::Compound(CompoundKind::Struct, struct_spec.clone()),
                    depth,
                ));
            }
            if let Some(struct_spec) = scope.unions.get(&name) {
                return Some((
                    XCompilationScopeItem::Compound(CompoundKind::Union, struct_spec.clone()),
                    depth,
                ));
            }
            if let Some(type_spec) = scope.types.get(&name) {
                return Some((XCompilationScopeItem::NativeType(type_spec.clone()), depth));
            }
            scope
                .parent
                .as_ref()
                .and_then(|parent| helper(parent, name, depth + 1))
                .map(|(item, depth)| {
                    (
                        match item {
                            XCompilationScopeItem::Overload(overloads) => {
                                XCompilationScopeItem::Overload(
                                    overloads
                                        .iter()
                                        .map(|x| {
                                            if let XFunctionFactory::Static(stat) = x.as_ref() {
                                                if let XStaticFunction::Recourse(spec, ..) =
                                                    stat.as_ref()
                                                {
                                                    Rc::new(XFunctionFactory::Static(Rc::new(
                                                        XStaticFunction::Recourse(
                                                            spec.clone(),
                                                            depth,
                                                        ),
                                                    )))
                                                } else {
                                                    x.clone()
                                                }
                                            } else {
                                                x.clone()
                                            }
                                        })
                                        .collect(),
                                )
                            }
                            other => other,
                        },
                        depth,
                    )
                })
        }
        helper(self, name, 0)
    }

    pub fn add_param(
        &mut self,
        name: DefaultSymbol,
        type_: Arc<XType>,
    ) -> Result<(), CompilationError> {
        if let Some(other) = self.get(name) {
            Err(CompilationError::NameAlreadyDefined { name, other })
        } else {
            self.values.insert(name, (None, type_));
            Ok(())
        }
    }

    pub fn add_var(
        &mut self,
        name: DefaultSymbol,
        expr: XExpr,
    ) -> Result<Declaration, CompilationError> {
        if let Some(other) = self.get(name) {
            Err(CompilationError::NameAlreadyDefined { name, other })
        } else {
            self.values
                .insert(name, (Some(expr.clone()), expr.xtype()?));
            Ok(Declaration::Value(name, expr))
        }
    }

    pub fn add_func(
        &mut self,
        name: DefaultSymbol,
        func: XStaticFunction,
    ) -> Result<Declaration, CompilationError> {
        // todo ensure no shadowing
        let item = Rc::new(func);
        self.functions
            .entry(name)
            .or_insert_with(|| vec![])
            .push(Rc::new(XFunctionFactory::Static(item.clone())));
        Ok(Declaration::UserFunction(name, item))
    }

    pub fn add_dyn_func(
        &mut self,
        name: DefaultSymbol,
        func: impl Fn(
                Option<&[XExpr]>,
                &[Arc<XType>],
                &XCompilationScope<'_>,
            ) -> Result<Rc<XStaticFunction>, String>
            + 'static,
    ) -> Result<(), CompilationError> {
        // todo ensure no shadowing?
        Ok(self
            .functions
            .entry(name)
            .or_insert_with(|| vec![])
            .push(Rc::new(XFunctionFactory::Dynamic(Rc::new(func)))))
    }

    pub fn add_func_intern(
        &mut self,
        name: &'static str,
        func: XStaticFunction,
        interner: &mut StringInterner,
    ) -> Result<Declaration, CompilationError> {
        self.add_func(interner.get_or_intern_static(name), func)
    }

    pub fn add_dyn_func_intern(
        &mut self,
        name: &'static str,
        func: impl Fn(
                Option<&[XExpr]>,
                &[Arc<XType>],
                &XCompilationScope<'_>,
            ) -> Result<Rc<XStaticFunction>, String>
            + 'static,
        interner: &mut StringInterner,
    ) -> Result<(), CompilationError> {
        self.add_dyn_func(interner.get_or_intern_static(name), func)
    }

    pub fn add_compound(
        &mut self,
        name: DefaultSymbol,
        kind: CompoundKind,
        spec: XCompoundSpec,
    ) -> Result<Declaration, CompilationError> {
        if kind == CompoundKind::Struct {
            self.add_struct(name, spec)
        } else {
            self.add_union(name, spec)
        }
    }

    fn add_struct(
        &mut self,
        name: DefaultSymbol,
        struct_spec: XCompoundSpec,
    ) -> Result<Declaration, CompilationError> {
        // todo ensure no shadowing
        self.structs.insert(name, Arc::new(struct_spec.clone()));
        Ok(Declaration::Struct(struct_spec))
    }

    fn add_union(
        &mut self,
        name: DefaultSymbol,
        union_spec: XCompoundSpec,
    ) -> Result<Declaration, CompilationError> {
        // todo ensure no shadowing
        self.unions.insert(name, Arc::new(union_spec.clone()));
        Ok(Declaration::Union(union_spec))
    }

    pub fn add_native_type(
        &mut self,
        name: DefaultSymbol,
        type_: Arc<XType>,
    ) -> Result<(), CompilationError> {
        if let Some(other) = self.get(name) {
            Err(CompilationError::NameAlreadyDefined { name, other })
        } else {
            self.types.insert(name, type_);
            Ok(())
        }
    }

    pub fn add_native_type_intern(
        &mut self,
        name: &'static str,
        type_: Arc<XType>,
        interner: &mut StringInterner,
    ) -> Result<(), CompilationError> {
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

    pub fn resolve_overload(
        &self,
        name: Identifier,
        types: &[Arc<XType>],
    ) -> Result<XExpr, String> {
        let overloads = match self.get(name) {
            Some(XCompilationScopeItem::Overload(overloads)) => overloads,
            _ => return Err(format!("{:?} is not an overload", name)), // todo better error
        };
        resolve_overload(overloads, None, types, name, &self)
            .map_err(|e| format!("overload resolution failed for {types:?} ({e:?})"))
    }

    fn parse_param_specs(
        &self,
        param_pairs: Pair<Rule>,
        gen_param_names: &HashSet<String>,
        interner: &mut StringInterner,
        runtime: RTCell,
        fn_symbol: Option<Identifier>,
    ) -> Result<Vec<XExplicitArgSpec>, TracedCompilationError> {
        param_pairs
            .into_inner()
            .map(|p| -> Result<_, TracedCompilationError> {
                let mut param_iter = p.clone().into_inner();
                let param_name = param_iter.next().unwrap().as_str();
                let param_symbol = interner.get_or_intern(param_name);
                let type_ = self.get_complete_type(
                    param_iter.next().unwrap(),
                    &gen_param_names,
                    interner,
                    None,
                )?;
                let default = param_iter
                    .next()
                    .map(|d| -> Result<_, TracedCompilationError> {
                        let d = d.into_inner().next().unwrap();
                        let e_scope = self
                            .to_eval_scope(runtime.clone())
                            .map_err(|e| e.trace(&d))?;
                        self.to_expr(d.clone(), interner, runtime.clone())?
                            .compile(&self)
                            .and_then(|c| {
                                c.expr
                                    .eval(&e_scope, false, runtime.clone())
                                    .map(|r| r.unwrap_value())
                                    .map_err(|error| CompilationError::DefaultEvaluationError {
                                        function_name: fn_symbol,
                                        param_name: param_symbol,
                                        error,
                                    })
                            })
                            .map_err(|e| e.trace(&d))
                    })
                    .transpose()?;
                Ok(XExplicitArgSpec {
                    name: param_symbol,
                    type_,
                    default,
                })
            })
            .collect::<Result<Vec<_>, _>>()
    }

    pub fn feed(
        &mut self,
        input: Pair<Rule>,
        parent_gen_param_names: &HashSet<String>,
        interner: &mut StringInterner,
        runtime: RTCell,
    ) -> Result<Vec<Declaration>, TracedCompilationError> {
        match input.as_rule() {
            Rule::header | Rule::top_level_execution | Rule::execution | Rule::declaration => {
                let mut declarations = Vec::new();
                for inner in input.into_inner() {
                    declarations.extend(self.feed(
                        inner,
                        parent_gen_param_names,
                        interner,
                        runtime.clone(),
                    )?);
                }
                Ok(declarations)
            }
            Rule::value => {
                let mut inners = input.clone().into_inner();
                let _pub_opt = inners.next().unwrap();
                let var_name = inners.next().unwrap().as_str();
                let explicit_type_opt = inners.next().unwrap();
                let complete_type = explicit_type_opt
                    .clone()
                    .into_inner()
                    .next()
                    .map(|et| self.get_complete_type(et, parent_gen_param_names, interner, None))
                    .transpose()?;
                let expr = self.to_expr(inners.next().unwrap(), interner, runtime)?;
                let CompilationResult {
                    expr: compiled,
                    closure_vars: cvars,
                } = expr.compile(&self).map_err(|e| e.trace(&input))?;
                let symbol = interner.get_or_intern(var_name);
                if let Some(complete_type) = complete_type {
                    let comp_xtype = compiled.xtype().map_err(|e| e.trace(&explicit_type_opt))?;
                    if complete_type != comp_xtype {
                        return Err(CompilationError::VariableTypeMismatch {
                            variable_name: symbol,
                            expected_type: complete_type,
                            actual_type: comp_xtype,
                        }
                        .trace(&input));
                    }
                }
                self.closure_variables.extend(cvars);
                Ok(vec![self
                    .add_var(interner.get_or_intern(var_name), compiled)
                    .map_err(|e| e.trace(&input))?])
            }
            Rule::function => {
                let mut inners = input.clone().into_inner();
                let _pub_opt = inners.next().unwrap();
                let fn_name = inners.next().unwrap().as_str();
                let fn_symbol = interner.get_or_intern(fn_name);
                let gen_params = inners.next().unwrap();
                let specific_gen_params;
                let mut gen_param_names = parent_gen_param_names.clone();
                if let Some(gen_params) = gen_params.into_inner().next() {
                    let mut _names = vec![];
                    for param in gen_params.into_inner() {
                        _names.push(interner.get_or_intern(param.as_str()));
                        gen_param_names.insert(param.as_str().to_string());
                    }
                    specific_gen_params = Some(_names);
                } else {
                    specific_gen_params = None;
                }
                let params = match inners.next().unwrap().into_inner().next() {
                    None => vec![],
                    Some(param_pairs) => self.parse_param_specs(
                        param_pairs,
                        &gen_param_names,
                        interner,
                        runtime.clone(),
                        Some(fn_symbol),
                    )?,
                };
                // check that there are no required params after optional ones
                if let Some(out_of_order_param) = params
                    .iter()
                    .skip_while(|p| p.default.is_none())
                    .find(|p| p.default.is_none())
                {
                    return Err(CompilationError::RequiredParamsAfterOptionalParams {
                        function_name: Some(fn_symbol),
                        param_name: out_of_order_param.name,
                    }
                    .trace(&input));
                }
                let rtype = self.get_complete_type(
                    inners.next().unwrap(),
                    &gen_param_names,
                    interner,
                    None,
                )?;
                let body = inners.next().unwrap();
                let spec = XExplicitFuncSpec {
                    generic_params: specific_gen_params,
                    args: params,
                    ret: rtype,
                };
                let mut subscope = XCompilationScope::from_parent(self, fn_symbol, spec.to_spec());
                for param in &spec.args {
                    subscope
                        .add_param(param.name, param.type_.clone())
                        .map_err(|e| e.trace(&input))?; // todo improve trace?
                }
                let mut body_iter = body.clone().into_inner();
                let declarations = subscope.feed(
                    body_iter.next().unwrap(),
                    &gen_param_names,
                    interner,
                    runtime.clone(),
                )?;
                let compiled_output = self
                    .to_expr(body_iter.next().unwrap(), interner, runtime)?
                    .compile(&subscope)
                    .map_err(|e| e.trace(&input))?;
                let output = Box::new(compiled_output.expr);
                let out_type = output.xtype().map_err(|e| e.trace(&body))?; // todo improve trace?
                if spec.ret.bind_in_assignment(&out_type).is_none() {
                    return Err(CompilationError::FunctionOutputTypeMismatch {
                        function_name: fn_symbol,
                        expected_type: spec.ret,
                        actual_type: out_type,
                    }
                    .trace(&input));
                }
                let cvars = subscope
                    .closure_variables
                    .iter()
                    .chain(compiled_output.closure_vars.iter())
                    .cloned()
                    .collect::<HashSet<_>>();
                let func =
                    XStaticFunction::UserFunction(UfData::new(spec, declarations, output, cvars));
                Ok(vec![self
                    .add_func(fn_symbol, func)
                    .map_err(|e| e.trace(&input))?])
            }
            Rule::compound_def => {
                let mut inners = input.clone().into_inner();
                let _pub_opt = inners.next().unwrap();
                let compound_kind_str = inners.next().unwrap().as_str();
                let compound_kind = match compound_kind_str {
                    "struct" => CompoundKind::Struct,
                    "union" => CompoundKind::Union,
                    _ => unreachable!(),
                };
                let var_name = inners.next().unwrap().as_str();
                let gen_params = inners.next().unwrap();
                let mut gen_param_names = HashSet::new();
                let mut gen_param_symbols = Vec::new();
                if let Some(gen_params) = gen_params.into_inner().next() {
                    for param in gen_params.into_inner() {
                        gen_param_names.insert(param.as_str().to_string());
                        gen_param_symbols.push(interner.get_or_intern(param.as_str()));
                    }
                }
                let param_pairs = inners.next().unwrap();
                let params = param_pairs
                    .into_inner()
                    .map(|p| {
                        let mut param_iter = p.into_inner();
                        let name = param_iter.next().unwrap().as_str();
                        let type_ = self.get_complete_type(
                            param_iter.next().unwrap(),
                            &gen_param_names,
                            interner,
                            Some(var_name),
                        )?;
                        Ok(XCompoundFieldSpec {
                            name: name.to_string(),
                            type_,
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let symbol = interner.get_or_intern(var_name);
                let spec = XCompoundSpec::new(symbol, gen_param_symbols, params);
                Ok(vec![self
                    .add_compound(spec.name, compound_kind, spec)
                    .map_err(|e| e.trace(&input))?])
            }
            Rule::EOI => Ok(Vec::new()),
            _ => {
                println!("{:?}", input);
                Ok(vec![])
            }
        }
    }

    fn get_complete_type(
        &self,
        input: Pair<Rule>,
        generic_param_names: &HashSet<String>,
        interner: &mut StringInterner,
        tail_name: Option<&str>,
    ) -> Result<Arc<XType>, TracedCompilationError> {
        match input.as_rule() {
            Rule::complete_type => {
                let mut inners = input.clone().into_inner();
                let part1 = inners.next().unwrap();
                match part1.as_rule() {
                    Rule::signature => {
                        let mut sig_inners = part1.into_inner();
                        let param_spec_opt = sig_inners.next().unwrap();
                        let param_types = param_spec_opt
                            .into_inner()
                            .next()
                            .map(|p| {
                                p.into_inner()
                                    .map(|i| {
                                        self.get_complete_type(
                                            i,
                                            generic_param_names,
                                            interner,
                                            tail_name,
                                        )
                                    })
                                    .collect::<Result<Vec<_>, _>>()
                            })
                            .transpose()?
                            .unwrap_or_default();
                        let return_type = self.get_complete_type(
                            sig_inners.next().unwrap(),
                            generic_param_names,
                            interner,
                            tail_name,
                        )?;
                        Ok(Arc::new(XType::XCallable(XCallableSpec {
                            param_types,
                            return_type,
                        })))
                    }
                    Rule::tup_type => {
                        let mut tup_inners = part1.into_inner();
                        match tup_inners.next() {
                            None => Ok(Arc::new(XType::Tuple(vec![]))), // todo make this a singleton?
                            Some(inner) => {
                                let tup_types = inner
                                    .into_inner()
                                    .map(|i| {
                                        self.get_complete_type(
                                            i,
                                            generic_param_names,
                                            interner,
                                            tail_name,
                                        )
                                    })
                                    .collect::<Result<Vec<_>, _>>()?;
                                Ok(Arc::new(XType::Tuple(tup_types)))
                            }
                        }
                    }
                    _ => {
                        // cname
                        let name = part1.as_str();
                        let gen_params = inners.next().map_or_else(
                            || Ok(vec![]),
                            |p| {
                                p.into_inner()
                                    .map(|p| {
                                        self.get_complete_type(
                                            p,
                                            generic_param_names,
                                            interner,
                                            tail_name,
                                        )
                                    })
                                    .collect::<Result<Vec<_>, _>>()
                            },
                        )?;
                        if let Some(tail_name) = tail_name {
                            if tail_name == name {
                                return Ok(Arc::new(XType::XTail(gen_params)));
                            }
                        }
                        let symbol = interner.get_or_intern(name);
                        let t = self.get(symbol);
                        if t.is_none() {
                            if generic_param_names.contains(name) {
                                return Ok(Arc::new(XType::XGeneric(symbol)));
                            }
                            return Err(CompilationError::TypeNotFound {
                                name: name.to_string(),
                            }
                            .trace(&input));
                        }
                        match t.unwrap() {
                            XCompilationScopeItem::NativeType(t) => {
                                if let XType::XNative(t, _) = t.as_ref() {
                                    if gen_params.len() != t.generic_names().len() {
                                        return Err(CompilationError::GenericParamCountMismatch {
                                            type_name: name.to_string(),
                                            expected_count: t.generic_names().len(),
                                            actual_count: gen_params.len(),
                                        }
                                        .trace(&input));
                                    }
                                    return Ok(Arc::new(XType::XNative(t.clone(), gen_params)));
                                } else {
                                    Ok(t)
                                }
                            }
                            XCompilationScopeItem::Compound(kind, t) => {
                                if gen_params.len() != t.generic_names.len() {
                                    return Err(CompilationError::GenericParamCountMismatch {
                                        type_name: name.to_string(),
                                        expected_count: t.generic_names.len(),
                                        actual_count: gen_params.len(),
                                    }
                                    .trace(&input));
                                }
                                let bind = Bind::from_iter(
                                    t.generic_names.iter().cloned().zip(gen_params.into_iter()),
                                );
                                Ok(Arc::new(XType::Compound(kind, t, bind)))
                            }
                            other => Err(CompilationError::ValueIsNotType {
                                name: symbol,
                                item: other,
                            }
                            .trace(&input)),
                        }
                    }
                }
            }
            _ => {
                println!("{:?}", input.as_str());
                Err(CompilationError::PairNotType.trace(&input))
            }
        }
    }

    fn to_expr(
        &self,
        input: Pair<Rule>,
        interner: &mut StringInterner,
        runtime: RTCell,
    ) -> Result<XStaticExpr, TracedCompilationError> {
        match input.as_rule() {
            Rule::expression => {
                let (
                    add_symbol,
                    sub_symbol,
                    mul_symbol,
                    div_symbol,
                    mod_symbol,
                    pow_symbol,
                    and_symbol,
                    or_symbol,
                    lt_symbol,
                    gt_symbol,
                    eq_symbol,
                    ne_symbol,
                    le_symbol,
                    ge_symbol,
                    bit_or_symbol,
                    bit_and_symbol,
                    bit_xor_symbol,
                ) = (
                    interner.get_or_intern_static("add"),
                    interner.get_or_intern_static("sub"),
                    interner.get_or_intern_static("mul"),
                    interner.get_or_intern_static("div"),
                    interner.get_or_intern_static("mod"),
                    interner.get_or_intern_static("pow"),
                    interner.get_or_intern_static("and"),
                    interner.get_or_intern_static("or"),
                    interner.get_or_intern_static("lt"),
                    interner.get_or_intern_static("gt"),
                    interner.get_or_intern_static("eq"),
                    interner.get_or_intern_static("ne"),
                    interner.get_or_intern_static("le"),
                    interner.get_or_intern_static("ge"),
                    interner.get_or_intern_static("bit_or"),
                    interner.get_or_intern_static("bit_and"),
                    interner.get_or_intern_static("bit_xor"),
                );
                CLIMBER.climb(
                    input.into_inner(),
                    |pair| self.to_expr(pair, interner, runtime.clone()),
                    |lhs, op, rhs| {
                        let lhs = lhs?;
                        let rhs = rhs?;
                        let func = match op.as_rule() {
                            Rule::BINARY_ADD => add_symbol,
                            Rule::BINARY_SUB => sub_symbol,
                            Rule::BINARY_MUL => mul_symbol,
                            Rule::BINARY_DIV => div_symbol,
                            Rule::BINARY_MOD => mod_symbol,
                            Rule::BINARY_POW => pow_symbol,
                            Rule::BINARY_AND => and_symbol,
                            Rule::BINARY_OR => or_symbol,
                            Rule::BINARY_LT => lt_symbol,
                            Rule::BINARY_GT => gt_symbol,
                            Rule::BINARY_EQ => eq_symbol,
                            Rule::BINARY_NE => ne_symbol,
                            Rule::BINARY_LE => le_symbol,
                            Rule::BINARY_GE => ge_symbol,
                            Rule::BINARY_BIT_OR => bit_or_symbol,
                            Rule::BINARY_BIT_AND => bit_and_symbol,
                            Rule::BINARY_BIT_XOR => bit_xor_symbol,
                            _ => unreachable!(),
                        };
                        Ok(XStaticExpr::new_call_sym(func, vec![lhs, rhs]))
                    },
                )
            }
            Rule::expression1 => {
                let mut iter = input.into_inner().rev();
                let mut expr = self.to_expr(iter.next().unwrap(), interner, runtime)?;
                for inner in iter {
                    let func = match inner.as_rule() {
                        Rule::UNARY_PLUS => "pos",
                        Rule::UNARY_MINUS => "neg",
                        Rule::UNARY_NOT => "not",
                        _ => unreachable!(),
                    };

                    expr = XStaticExpr::new_call(func, vec![expr], interner);
                }
                Ok(expr)
            }
            Rule::expression2 => {
                let mut iter = input.into_inner();
                let mut ret = self.to_expr(iter.next().unwrap(), interner, runtime.clone())?;
                for accessor in iter {
                    match accessor.as_rule() {
                        Rule::method => {
                            let mut meth_call_iter = accessor.into_inner();
                            let method = XStaticExpr::Ident(
                                interner.get_or_intern(meth_call_iter.next().unwrap().as_str()),
                            );
                            let args = match meth_call_iter.next().unwrap().into_inner().next() {
                                None => Ok(vec![ret]),
                                Some(c) => iter::once(Ok(ret))
                                    .chain(
                                        c.into_inner()
                                            .map(|p| self.to_expr(p, interner, runtime.clone())),
                                    )
                                    .collect(),
                            }?;
                            ret = XStaticExpr::Call(Box::new(method), args);
                        }
                        Rule::member => {
                            let member = accessor.into_inner().next().unwrap();
                            ret = XStaticExpr::Member(Box::new(ret), member.as_str().to_string());
                        }
                        Rule::call => {
                            let mut iter = accessor.into_inner();
                            let raw_args = iter.next().unwrap();
                            let args = raw_args.into_inner().next().map_or_else(
                                || Ok(vec![]),
                                |c| {
                                    c.into_inner()
                                        .map(|p| self.to_expr(p, interner, runtime.clone()))
                                        .collect()
                                },
                            )?;
                            ret = XStaticExpr::Call(Box::new(ret), args);
                        }
                        _ => {
                            unreachable!()
                        }
                    }
                }
                Ok(ret)
            }
            Rule::NUMBER_ANY => {
                let mut to_parse = Cow::Borrowed(input.as_str());
                if to_parse.contains('_') {
                    to_parse = Cow::Owned(to_parse.replace('_', ""));
                }
                if let Ok(whole) = to_parse.parse::<i64>() {
                    return Ok(XStaticExpr::LiteralInt(whole));
                }
                if let Ok(float) = to_parse.parse::<f64>() {
                    return Ok(XStaticExpr::LiteralFloat(float));
                }
                panic!("{} is not a number", input.as_str());
            }
            Rule::CNAME => {
                return Ok(XStaticExpr::Ident(interner.get_or_intern(input.as_str())));
            }
            Rule::STRING => {
                return Ok(XStaticExpr::LiteralString(
                    input.into_inner().next().unwrap().as_str().to_string(),
                ));
            }
            Rule::bool => {
                return Ok(XStaticExpr::LiteralBool(input.as_str() == "true"));
            }
            Rule::container => {
                let mut iter = input.into_inner();
                let parts = iter.next().map_or_else(
                    || Ok(vec![]),
                    |c| {
                        c.into_inner()
                            .map(|p| self.to_expr(p, interner, runtime.clone()))
                            .collect()
                    },
                )?;
                Ok(XStaticExpr::Array(parts))
            }
            Rule::tuple => {
                let mut iter = input.into_inner();
                let parts = iter.next().map_or_else(
                    || Ok(vec![]),
                    |c| {
                        c.into_inner()
                            .map(|p| self.to_expr(p, interner, runtime.clone()))
                            .collect()
                    },
                )?;
                Ok(XStaticExpr::Tuple(parts))
            }
            Rule::specialized_cname => {
                let mut iter = input.into_inner();
                let cname = iter.next().unwrap().as_str();
                let args = iter
                    .map(|p| self.get_complete_type(p, &HashSet::new(), interner, None))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(XStaticExpr::SpecializedIdent(
                    interner.get_or_intern(cname),
                    args,
                ))
            }
            Rule::lambda_func => {
                let mut iter = input.clone().into_inner();
                let params = match iter.next().unwrap().into_inner().next() {
                    None => vec![],
                    Some(c) => self.parse_param_specs(
                        c,
                        &HashSet::new(), // todo
                        interner,
                        runtime.clone(),
                        None,
                    )?,
                };
                // check that there are no required params after optional ones
                // todo add this logic to parse_param_specs
                if let Some(out_of_order_param) = params
                    .iter()
                    .skip_while(|p| p.default.is_none())
                    .find(|p| p.default.is_none())
                {
                    return Err(CompilationError::RequiredParamsAfterOptionalParams {
                        function_name: None,
                        param_name: out_of_order_param.name,
                    }
                    .trace(&input));
                }
                let body = iter.next().unwrap();
                let ret = self.to_expr(body.clone(), interner, runtime)?;
                Ok(XStaticExpr::Lambda(params, Box::new(ret)))
            }
            _ => {
                panic!("not an expression {:?}", input);
            }
        }
    }
}

lazy_static! {
    static ref CLIMBER: PrecClimber<Rule> = {
        PrecClimber::new(vec![
            Operator::new(Rule::BINARY_AND, Left) | Operator::new(Rule::BINARY_OR, Left),
            Operator::new(Rule::BINARY_LT, Left)
                | Operator::new(Rule::BINARY_GT, Left)
                | Operator::new(Rule::BINARY_EQ, Left)
                | Operator::new(Rule::BINARY_NE, Left)
                | Operator::new(Rule::BINARY_LE, Left)
                | Operator::new(Rule::BINARY_GE, Left),
            Operator::new(Rule::BINARY_BIT_OR, Left)
                | Operator::new(Rule::BINARY_BIT_AND, Left)
                | Operator::new(Rule::BINARY_BIT_XOR, Left),
            Operator::new(Rule::BINARY_ADD, Left) | Operator::new(Rule::BINARY_SUB, Left),
            Operator::new(Rule::BINARY_MUL, Left)
                | Operator::new(Rule::BINARY_DIV, Left)
                | Operator::new(Rule::BINARY_MOD, Left),
            Operator::new(Rule::BINARY_POW, Right),
        ])
    };
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Value(DefaultSymbol, XExpr),
    Struct(XCompoundSpec),
    Union(XCompoundSpec),
    UserFunction(DefaultSymbol, Rc<XStaticFunction>),
}
