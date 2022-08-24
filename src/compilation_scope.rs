use crate::parser::Rule;
use crate::runtime::RTCell;
use crate::xexpr::{resolve_overload, XExpr, XStaticFunction};
use crate::xtype::{CompoundKind, XCompoundSpec, XFuncSpec, XType};
use crate::xvalue::DynBind;
use crate::{
    let_match, Bind, CompilationError, CompilationResult, Identifier, TracedCompilationError,
    UfData, XCallableSpec, XCompoundFieldSpec, XEvaluationScope, XExplicitArgSpec,
    XExplicitFuncSpec, XRayParser, XStaticExpr,
};

use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::iter;
use std::iter::from_fn;
use std::iter::FromIterator;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::Arc;
use string_interner::StringInterner;

use crate::compile_err::ResolvedTracedCompilationError;
use crate::pest::Parser;
use crate::util::ipush::IPush;
use derivative::Derivative;
use pest::iterators::Pair;
use pest::prec_climber::Assoc::{Left, Right};
use pest::prec_climber::{Operator, PrecClimber};

enum CompilationScopeValue {
    Parameter(Arc<XType>),
    Declared(usize),
}

enum CompilationScopeType {
    Native(Arc<XType>),
    Declared(usize),
}

enum CompilationScopeFunction {
    Factory(XFunctionFactory),
    Declared(usize),
}

pub struct XCompilationScope<'p> {
    values: HashMap<Identifier, CompilationScopeValue>,
    types: HashMap<Identifier, CompilationScopeType>,
    functions: HashMap<Identifier, Vec<CompilationScopeFunction>>,
    recourse: Option<(Identifier, Rc<XFuncSpec>)>,
    closure_variables: HashSet<Identifier>,
    parent: Option<&'p XCompilationScope<'p>>,

    pub(crate) declarations: IPush<Declaration>,

    pub(crate) height: usize,
}

#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub enum XFunctionFactory {
    Static(Rc<XStaticFunction>),
    // todo is this ever constructed?
    Dynamic(#[derivative(Debug = "ignore")] DynBind),
}

#[derive(Debug)]
pub enum XCompilationScopeItem {
    Value(Arc<XType>),
    NativeType(Arc<XType>),
    Compound(CompoundKind, Arc<XCompoundSpec>),
    Overload(Vec<XFunctionFactory>),
}

impl<'p> XCompilationScope<'p> {
    fn root() -> Self {
        XCompilationScope {
            values: HashMap::new(),
            types: HashMap::new(),
            functions: HashMap::new(),
            parent: None,
            recourse: None,
            closure_variables: HashSet::new(),
            declarations: IPush::new(),
            height: 0,
        }
    }

    fn from_parent(
        parent: &'p XCompilationScope<'p>,
        recourse_name: Identifier,
        recourse_spec: XFuncSpec,
    ) -> Self {
        XCompilationScope {
            parent: Some(parent),
            recourse: Some((recourse_name, Rc::new(recourse_spec))),
            height: parent.height + 1,

            ..Self::root()
        }
    }

    pub(crate) fn from_parent_lambda(parent: &'p XCompilationScope<'p>) -> Self {
        XCompilationScope {
            parent: Some(parent),
            recourse: None,
            height: parent.height + 1,

            ..Self::root()
        }
    }

    fn ancestors(&self) -> impl Iterator<Item=&XCompilationScope<'p>> {
        let mut scope = self;
        from_fn(move || {
            if let Some(parent) = scope.parent {
                scope = parent;
                return Some(parent);
            }
            None
        })
    }

    pub(crate) fn get(&self, name: Identifier) -> Option<XCompilationScopeItem> {
        self.get_with_depth(name).map(|i| i.0)
    }

    fn get_function_factory(&self, comp_scope_func: &CompilationScopeFunction) -> XFunctionFactory {
        match comp_scope_func {
            CompilationScopeFunction::Declared(idx) => XFunctionFactory::Static(
                let_match!(&self.declarations[*idx]; Declaration::Function(_, sf) => sf.clone()),
            ),
            CompilationScopeFunction::Factory(fact) => fact.clone(),
        }
    }

    pub(crate) fn get_with_depth(
        &self,
        name: Identifier,
    ) -> Option<(XCompilationScopeItem, usize)> {
        fn helper(
            scope: &XCompilationScope<'_>,
            name: Identifier,
            depth: usize,
        ) -> Option<(XCompilationScopeItem, usize)> {
            let mut overloads = scope.functions.get(&name).map_or_else(Vec::new, |x| {
                x.iter().map(|i| scope.get_function_factory(i)).collect()
            });
            match &scope.recourse {
                Some((rec_name, spec)) if rec_name == &name => {
                    // todo just put this in declarations?
                    overloads.push(XFunctionFactory::Static(Rc::new(
                        XStaticFunction::Recourse(spec.clone(), 0),
                    )));
                }
                _ => (),
            }
            if !overloads.is_empty() {
                for (depth, ancestor) in scope.ancestors().enumerate() {
                    if let Some(ancestor_overloads) = ancestor.functions.get(&name) {
                        let ancestor_overloads = ancestor_overloads.iter().map(|x| {
                            let mut func = ancestor.get_function_factory(x);
                            if let XFunctionFactory::Static(stat) = &func {
                                if let XStaticFunction::Recourse(spec, ..) = stat.as_ref() {
                                    func = XFunctionFactory::Static(Rc::new(
                                        XStaticFunction::Recourse(spec.clone(), depth),
                                    ));
                                }
                            }
                            func
                        });
                        overloads.extend(ancestor_overloads);
                    }
                }
                return Some((XCompilationScopeItem::Overload(overloads), depth));
            }
            if let Some(value) = scope.values.get(&name) {
                let ty = match value {
                    CompilationScopeValue::Declared(idx) => {
                        let_match!(&scope.declarations[*idx]; Declaration::Value(.., ty) => ty)
                    }
                    CompilationScopeValue::Parameter(ty) => ty,
                };
                return Some((XCompilationScopeItem::Value(ty.clone()), depth));
            }
            if let Some(type_) = scope.types.get(&name) {
                let item = match type_ {
                    CompilationScopeType::Declared(idx) => {
                        let_match!(&scope.declarations[*idx]; Declaration::Compound(_, kind, spec) => XCompilationScopeItem::Compound(*kind, spec.clone()))
                    }
                    CompilationScopeType::Native(ty) => {
                        XCompilationScopeItem::NativeType(ty.clone())
                    }
                };
                return Some((item, depth));
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
                                            if let XFunctionFactory::Static(stat) = x {
                                                if let XStaticFunction::Recourse(spec, ..) =
                                                stat.as_ref()
                                                {
                                                    XFunctionFactory::Static(Rc::new(
                                                        XStaticFunction::Recourse(
                                                            spec.clone(),
                                                            depth,
                                                        ),
                                                    ))
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

    pub(crate) fn add_param(
        &mut self,
        name: Identifier,
        type_: Arc<XType>,
    ) -> Result<(), CompilationError> {
        if let Some(other) = self.get(name) {
            Err(CompilationError::NameAlreadyDefined { name, other })
        } else {
            self.values
                .insert(name, CompilationScopeValue::Parameter(type_));
            Ok(())
        }
    }

    fn add_var(&mut self, name: Identifier, expr: XExpr) -> Result<(), CompilationError> {
        if let Some(other) = self.get(name) {
            Err(CompilationError::NameAlreadyDefined { name, other })
        } else {
            let xtype = expr.xtype()?;
            self.values.insert(
                name,
                CompilationScopeValue::Declared(
                    self.declarations
                        .ipush(Declaration::Value(name, expr, xtype)),
                ),
            );
            Ok(())
        }
    }

    fn add_func(
        &mut self,
        name: Identifier,
        func: XStaticFunction,
    ) -> Result<(), CompilationError> {
        // todo ensure no shadowing
        let item = Rc::new(func);
        self.functions.entry(name).or_insert_with(Vec::new).push(
            CompilationScopeFunction::Declared(
                self.declarations.ipush(Declaration::Function(name, item)),
            ),
        );
        Ok(())
    }

    fn add_dyn_func(
        &mut self,
        name: Identifier,
        func: impl Fn(
            Option<&[XExpr]>,
            &[Arc<XType>],
            &XCompilationScope<'_>,
        ) -> Result<Rc<XStaticFunction>, String>
        + 'static,
    ) -> Result<(), CompilationError> {
        // todo ensure no shadowing?
        self.functions.entry(name).or_insert_with(Vec::new).push(
            CompilationScopeFunction::Factory(XFunctionFactory::Dynamic(Rc::new(func))),
        );
        Ok(())
    }

    fn add_compound(
        &mut self,
        name: Identifier,
        kind: CompoundKind,
        struct_spec: XCompoundSpec,
    ) -> Result<(), CompilationError> {
        // todo ensure no shadowing
        self.types.insert(
            name,
            CompilationScopeType::Declared(self.declarations.ipush(Declaration::Compound(
                name,
                kind,
                Arc::new(struct_spec),
            ))),
        );
        Ok(())
    }

    fn add_native_type(
        &mut self,
        name: Identifier,
        type_: Arc<XType>,
    ) -> Result<(), CompilationError> {
        if let Some(other) = self.get(name) {
            Err(CompilationError::NameAlreadyDefined { name, other })
        } else {
            self.types.insert(name, CompilationScopeType::Native(type_));
            Ok(())
        }
    }

    fn to_eval_scope(&self, runtime: RTCell) -> Result<XEvaluationScope, CompilationError> {
        let mut ret = self.parent.map_or_else(
            || Ok(XEvaluationScope::root()),
            |p| p.to_eval_scope(runtime.clone()),
        )?;
        for decl in &self.declarations {
            ret.add_from(decl, runtime.clone()).unwrap_or(
                ()
                // we actually allow errors to happen here, since some expressions might depend
                // on params or other unknown values
                // todo find some way to report this
                // todo catch limit errors
            );
        }
        Ok(ret)
    }

    pub(crate) fn resolve_overload(
        &self,
        name: Identifier,
        types: &[Arc<XType>],
    ) -> Result<XExpr, String> {
        let overloads = match self.get(name) {
            Some(XCompilationScopeItem::Overload(overloads)) => overloads,
            _ => return Err(format!("{:?} is not an overload", name)), // todo better error
        };
        resolve_overload(&overloads, None, types, name, self)
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
                    gen_param_names,
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
                            .compile(self)
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

    fn feed(
        &mut self,
        input: Pair<Rule>,
        parent_gen_param_names: &HashSet<String>,
        interner: &mut StringInterner,
        runtime: RTCell,
    ) -> Result<(), TracedCompilationError> {
        match input.as_rule() {
            Rule::header | Rule::top_level_execution | Rule::execution | Rule::declaration => {
                for inner in input.into_inner() {
                    self.feed(inner, parent_gen_param_names, interner, runtime.clone())?;
                }
                Ok(())
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
                } = expr.compile(self).map_err(|e| e.trace(&input))?;
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
                self.add_var(interner.get_or_intern(var_name), compiled)
                    .map_err(|e| e.trace(&input))?;
                Ok(())
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
                subscope.feed(
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
                let func = XStaticFunction::UserFunction(UfData::new(
                    spec,
                    subscope.declarations.into(),
                    output,
                    cvars,
                ));
                self.add_func(fn_symbol, func)
                    .map_err(|e| e.trace(&input))?;
                Ok(())
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
                self.add_compound(spec.name, compound_kind, spec)
                    .map_err(|e| e.trace(&input))?;
                Ok(())
            }
            Rule::EOI => Ok(()),
            _ => {
                println!("{:?}", input);
                Ok(())
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
                                    Ok(Arc::new(XType::XNative(t.clone(), gen_params)))
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

#[derive(Debug)]
pub enum Declaration {
    Value(Identifier, XExpr, Arc<XType>),
    Compound(Identifier, CompoundKind, Arc<XCompoundSpec>),
    Function(Identifier, Rc<XStaticFunction>),
}

pub struct RootCompilationScope {
    pub(crate) scope: XCompilationScope<'static>,
    interner: StringInterner,
}

impl RootCompilationScope {
    pub fn new() -> Self {
        Self {
            scope: XCompilationScope::root(),
            interner: StringInterner::default(),
        }
    }

    pub fn add_native_type(
        &mut self,
        name: &'static str,
        type_: Arc<XType>,
    ) -> Result<(), CompilationError> {
        self.scope
            .add_native_type(self.interner.get_or_intern_static(name), type_)
    }

    pub fn add_func(
        &mut self,
        name: &'static str,
        func: XStaticFunction,
    ) -> Result<(), CompilationError> {
        self.scope
            .add_func(self.interner.get_or_intern_static(name), func)
            .map(|_| ())
    }

    pub fn add_dyn_func(
        &mut self,
        name: &'static str,
        func: impl Fn(
            Option<&[XExpr]>,
            &[Arc<XType>],
            &XCompilationScope<'_>,
        ) -> Result<Rc<XStaticFunction>, String>
        + 'static,
    ) -> Result<(), CompilationError> {
        self.scope
            .add_dyn_func(self.interner.get_or_intern_static(name), func)
            .map(|_| ())
    }

    pub(crate) fn generics_from_names<const N: usize>(
        &mut self,
        names: [&'static str; N],
    ) -> ([Arc<XType>; N], Vec<Identifier>) {
        let (v0, v1) = names
            .iter()
            .map(|name| {
                let ident = self.interner.get_or_intern_static(name);
                (XType::XGeneric(ident).into(), ident)
            })
            .unzip::<_, _, Vec<_>, Vec<_>>();
        (v0.try_into().unwrap(), v1)
    }

    pub fn identifier(&mut self, name: &'static str) -> Identifier {
        self.interner.get_or_intern_static(name)
    }

    pub fn get_identifer(&self, name: &str) -> Option<Identifier> {
        self.interner.get(name)
    }

    pub fn feed_file(
        &mut self,
        input: &str,
        runtime: RTCell,
    ) -> Result<(), ResolvedTracedCompilationError> {
        let body = XRayParser::parse(Rule::header, input)
            .map(|mut p| p.next().unwrap())
            .map_err(ResolvedTracedCompilationError::Syntax)?;
        self.scope
            .feed(body, &HashSet::new(), &mut self.interner, runtime)
            .map_err(|e| e.resolve_with_input(&self.interner, input))
    }

    pub fn describe_type(&self, t: impl Deref<Target=XType>) -> String {
        t.to_string_with_interner(&self.interner)
    }

    pub fn get(&self, name: &str) -> Option<XCompilationScopeItem> {
        self.interner.get(name).and_then(|i| self.scope.get(i))
    }
}

impl Default for RootCompilationScope {
    fn default() -> Self {
        Self::new()
    }
}
