use crate::compilation_scope::{CompilationItem, CompilationScope};
use crate::{
    Bind, CompilationError, Identifier, TracedCompilationError, XCallableSpec, XCompoundFieldSpec,
    XCompoundSpec, XFuncSpec, XStaticExpr, XStaticFunction, XType,
};
use itertools::Itertools;
use pest::iterators::{Pair, Pairs};
use std::borrow::Cow;
use std::collections::HashSet;
use std::hash::Hash;
use std::iter;
use std::sync::Arc;

use crate::root_compilation_scope::Interner;

use crate::util::str_escapes::{apply_brace_escape, apply_escapes};
use crate::xexpr::{OverloadSpecialization, XExpr};
use crate::xtype::{CompoundKind, XFuncParamSpec};
use pest::prec_climber::Assoc::{Left, Right};
use pest::prec_climber::{Operator, PrecClimber};
use std::iter::FromIterator;
use std::rc::Rc;

#[derive(Parser)]
#[grammar = "xray.pest"]
pub struct XRayParser;

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

type ParamSpecs<W, R, T> = Vec<(Identifier, Arc<XType>, Option<XStaticExpr<W, R, T>>)>;

struct ParsedFunctionHeader<'b, W, R, T> {
    param_names: Vec<Identifier>,
    fn_symbol: Identifier,
    spec: XFuncSpec,
    defaults: Vec<XExpr<W, R, T>>,
    param_len: usize,
    inners: Pairs<'b, Rule>,
    params_pair: Pair<'b, Rule>,
    gen_param_names: HashSet<String>,
    specific_gen_params: Option<Vec<Identifier>>,
}

impl<W, R, T> CompilationScope<'_, W, R, T> {
    fn parse_function_header<'b>(
        &mut self,
        input: &Pair<'b, Rule>,
        parent_gen_param_names: &HashSet<String>,
        interner: &mut Interner,
    ) -> Result<ParsedFunctionHeader<'b, W, R, T>, TracedCompilationError> {
        let mut inners = input.clone().into_inner();
        let fn_name = inners.next().unwrap().as_str();
        let fn_symbol = interner.get_or_intern(fn_name);
        let gen_params = inners.next().unwrap();
        let mut gen_param_names = parent_gen_param_names.clone();
        let specific_gen_params = gen_params.into_inner().next().map(|gen_params| {
            let mut names = vec![];
            for param in gen_params.into_inner() {
                names.push(interner.get_or_intern(param.as_str()));
                // todo this is necessary because the subscope is built all-at once, but maybe we can change that?
                gen_param_names.insert(param.as_str().to_string());
            }
            names
        });
        let params_pair = inners.next().unwrap();
        let params = match params_pair.clone().into_inner().next() {
            None => vec![],
            Some(param_pairs) => {
                self.parse_param_specs(param_pairs, &gen_param_names, interner, Some(fn_symbol))?
            }
        };
        let rtype = self.get_complete_type(
            inners.next().unwrap(),
            &gen_param_names,
            interner,
            None,
            false,
        )?;

        let (param_names, param_specs, param_static_defaults): (Vec<_>, Vec<_>, Vec<_>) = params
            .into_iter()
            .map(|(name, xtype, default)| {
                (
                    name,
                    XFuncParamSpec {
                        type_: xtype,
                        required: default.is_none(),
                    },
                    default,
                )
            })
            .multiunzip();
        let spec = XFuncSpec {
            generic_params: specific_gen_params.clone(),
            params: param_specs,
            ret: rtype,
            short_circuit_overloads: false,
        };
        let defaults = param_static_defaults
            .into_iter()
            .filter_map(|s| s.map(|s| self.compile(s)))
            .collect::<Result<_, _>>()
            .map_err(|e| e.trace(input))?;
        let param_len = param_names.len();
        Ok(ParsedFunctionHeader {
            param_names,
            fn_symbol,
            spec,
            defaults,
            param_len,
            inners,
            params_pair,
            gen_param_names,
            specific_gen_params,
        })
    }

    pub(crate) fn feed(
        &mut self,
        input: Pair<Rule>,
        parent_gen_param_names: &HashSet<String>,
        interner: &mut Interner,
    ) -> Result<(), TracedCompilationError> {
        match input.as_rule() {
            Rule::header | Rule::execution | Rule::declaration => {
                for inner in input.into_inner() {
                    self.feed(inner, parent_gen_param_names, interner)?;
                }
                Ok(())
            }
            Rule::forward_ref => {
                let ParsedFunctionHeader {
                    fn_symbol, spec, ..
                } = self.parse_function_header(&input, parent_gen_param_names, interner)?;
                self.add_forward_func(fn_symbol, spec)
                    .map_err(|e| e.trace(&input))?;
                Ok(())
            }
            Rule::value => {
                let mut inners = input.clone().into_inner();
                let var_name = inners.next().unwrap().as_str();
                let explicit_type_opt = inners.next().unwrap();
                let complete_type = explicit_type_opt
                    .clone()
                    .into_inner()
                    .next()
                    .map(|et| {
                        self.get_complete_type(et, parent_gen_param_names, interner, None, false)
                    })
                    .transpose()?;
                let expr_pair = inners.next().unwrap();
                let expr = self.parse_expr(expr_pair.clone(), interner)?;
                let compiled = self.compile(expr).map_err(|e| e.trace(&input))?;
                let symbol = interner.get_or_intern(var_name);
                let compiled_xtype = self.type_of(&compiled).map_err(|e| e.trace(&expr_pair))?;
                let declared_type = if let Some(complete_type) = complete_type {
                    match complete_type.bind_in_assignment(&compiled_xtype) {
                        None => {
                            return Err(CompilationError::VariableTypeMismatch {
                                variable_name: symbol,
                                expected_type: complete_type,
                                actual_type: compiled_xtype,
                            }
                            .trace(&input));
                        }
                        Some(bind) if !bind.is_empty() => {
                            return Err(CompilationError::VariableTypeMismatch {
                                variable_name: symbol,
                                expected_type: complete_type,
                                actual_type: compiled_xtype,
                            }
                            .trace(&input));
                        }
                        _ => {}
                    }
                    complete_type
                } else {
                    compiled_xtype
                };
                self.add_variable(interner.get_or_intern(var_name), compiled, declared_type)
                    .map_err(|e| e.trace(&input))
            }
            Rule::function => {
                let ParsedFunctionHeader {
                    param_names,
                    fn_symbol,
                    spec,
                    defaults,
                    param_len,
                    mut inners,
                    params_pair,
                    gen_param_names,
                    specific_gen_params,
                } = self.parse_function_header(&input, parent_gen_param_names, interner)?;
                let mut subscope =
                    CompilationScope::from_parent(self, param_names, fn_symbol, spec.clone())
                        .map_err(|e| e.trace(&params_pair))?;
                for gen_param in specific_gen_params.unwrap_or_default() {
                    subscope
                        .add_type(gen_param, Arc::new(XType::XGeneric(gen_param)))
                        .map_err(|e| e.trace(&params_pair))?;
                }
                let body = inners.next().unwrap();
                let mut body_iter = body.clone().into_inner();
                subscope.feed(body_iter.next().unwrap(), &gen_param_names, interner)?;
                let out_pair = body_iter.next().unwrap();
                let out_static_expr = subscope.parse_expr(out_pair.clone(), interner)?;
                let out = subscope
                    .compile(out_static_expr)
                    .map_err(|e| e.trace(&out_pair))?;

                let out_type = subscope.type_of(&out).map_err(|e| e.trace(&out_pair))?;
                let output = Box::new(out);
                match spec.ret.bind_in_assignment(&out_type) {
                    None => {
                        return Err(CompilationError::FunctionOutputTypeMismatch {
                            function_name: fn_symbol,
                            expected_type: spec.ret,
                            actual_type: out_type,
                        }
                        .trace(&out_pair));
                    }
                    Some(return_bind) if !return_bind.is_empty() => {
                        return Err(CompilationError::FunctionOutputTypeMismatch {
                            function_name: fn_symbol,
                            expected_type: spec.ret,
                            actual_type: out_type,
                        }
                        .trace(&out_pair));
                    }
                    _ => {}
                }
                let (func, parent_capture_requests) =
                    subscope.into_static_ud(defaults, param_len, output, self.id);
                for pr in parent_capture_requests {
                    self.cells.ipush(pr);
                }
                self.add_static_func(
                    fn_symbol,
                    spec,
                    XStaticFunction::UserFunction(Rc::new(func)),
                )
                .map_err(|e| e.trace(&input))?;
                Ok(())
            }
            Rule::compound_def => {
                let mut inners = input.clone().into_inner();
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
                            false,
                        )?;
                        Ok(XCompoundFieldSpec {
                            name: interner.get_or_intern(name),
                            type_,
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let symbol = interner.get_or_intern(var_name);
                let spec = XCompoundSpec::new(symbol, gen_param_symbols, params);
                self.add_compound(spec.name, compound_kind, spec)
                    .map_err(|e| e.trace(&input))
            }
            Rule::type_def => {
                let mut inners = input.clone().into_inner();
                let var_name = inners.next().unwrap().as_str();
                let symbol = interner.get_or_intern(var_name);
                let type_ = inners.next().unwrap();
                let complete_type =
                    self.get_complete_type(type_, parent_gen_param_names, interner, None, false)?;
                self.add_type(symbol, complete_type)
                    .map_err(|e| e.trace(&input))
            }
            Rule::EOI => Ok(()),
            _ => {
                println!("{input:?}");
                unreachable!()
            }
        }
    }

    fn get_complete_type(
        &self,
        input: Pair<Rule>,
        generic_param_names: &HashSet<String>,
        interner: &mut Interner,
        tail_name: Option<&str>,
        auto_allowed: bool,
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
                                            false,
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
                            false,
                        )?;
                        Ok(Arc::new(XType::XCallable(XCallableSpec {
                            param_types,
                            return_type,
                        })))
                    }
                    Rule::tup_type => {
                        let mut tup_inners = part1.into_inner();
                        match tup_inners.next() {
                            None => Ok(Arc::new(XType::Tuple(vec![]))),
                            Some(inner) => {
                                let tup_types = inner
                                    .into_inner()
                                    .map(|i| {
                                        self.get_complete_type(
                                            i,
                                            generic_param_names,
                                            interner,
                                            tail_name,
                                            false,
                                        )
                                    })
                                    .collect::<Result<Vec<_>, _>>()?;
                                Ok(Arc::new(XType::Tuple(tup_types)))
                            }
                        }
                    }
                    Rule::auto_type => {
                        if auto_allowed {
                            Ok(Arc::new(XType::Auto))
                        } else {
                            Err(CompilationError::InvalidAutoLocation.trace(&part1))
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
                                            false,
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

                        let t = self.get_item(&symbol);
                        match t {
                            None => {
                                if generic_param_names.contains(name) {
                                    return Ok(Arc::new(XType::XGeneric(symbol)));
                                }
                                Err(CompilationError::TypeNotFound {
                                    name: name.to_string(),
                                }
                                .trace(&input))
                            }
                            Some(CompilationItem::Type(t)) => match t.as_ref() {
                                XType::XNative(t, ..) => {
                                    if gen_params.len() != t.generic_names().len() {
                                        return Err(CompilationError::GenericParamCountMismatch {
                                            type_name: name.to_string(),
                                            expected_count: t.generic_names().len(),
                                            actual_count: gen_params.len(),
                                        }
                                        .trace(&input));
                                    }
                                    Ok(Arc::new(XType::XNative(t.clone(), gen_params)))
                                }
                                XType::Compound(kind, t, ..) => {
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
                                    Ok(Arc::new(XType::Compound(*kind, t.clone(), bind)))
                                }
                                _ => Ok(t),
                            },
                            Some(CompilationItem::Value(..)) => {
                                Err(CompilationError::VariableAsType { name: symbol }.trace(&input))
                            }
                            Some(CompilationItem::Overload(..)) => {
                                Err(CompilationError::OverloadAsType { name: symbol }.trace(&input))
                            }
                        }
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    fn parse_expr(
        &mut self,
        input: Pair<Rule>,
        interner: &mut Interner,
    ) -> Result<XStaticExpr<W, R, T>, TracedCompilationError> {
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
                    |pair| self.parse_expr(pair, interner),
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
                let mut expr = self.parse_expr(iter.next().unwrap(), interner)?;
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
                let mut ret = self.parse_expr(iter.next().unwrap(), interner)?;
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
                                    .chain(c.into_inner().map(|p| self.parse_expr(p, interner)))
                                    .collect(),
                            }?;
                            ret = XStaticExpr::Call(Box::new(method), args);
                        }
                        Rule::member => {
                            let member = accessor.into_inner().next().unwrap();
                            ret = XStaticExpr::Member(
                                Box::new(ret),
                                interner.get_or_intern(member.as_str()),
                            );
                        }
                        Rule::member_value => {
                            let member = accessor.into_inner().next().unwrap();
                            ret = XStaticExpr::MemberValue(
                                Box::new(ret),
                                interner.get_or_intern(member.as_str()),
                            );
                        }
                        Rule::member_opt_value => {
                            let member = accessor.into_inner().next().unwrap();
                            ret = XStaticExpr::MemberOptValue(
                                Box::new(ret),
                                interner.get_or_intern(member.as_str()),
                            );
                        }
                        Rule::call => {
                            let mut iter = accessor.into_inner();
                            let raw_args = iter.next().unwrap();
                            let args = raw_args.into_inner().next().map_or_else(
                                || Ok(vec![]),
                                |c| {
                                    c.into_inner()
                                        .map(|p| self.parse_expr(p, interner))
                                        .collect()
                                },
                            )?;
                            ret = XStaticExpr::Call(Box::new(ret), args);
                        }
                        Rule::index => {
                            let idxs_pair = accessor.into_inner().next().unwrap();
                            let args = iter::once(Ok(ret))
                            .chain(
                                idxs_pair
                                    .into_inner()
                                    .map(|p| self.parse_expr(p, interner)),
                            ).collect::<Result<_,_>>()?;
                            ret = XStaticExpr::new_call("get", args, interner)
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
                if let Some(whole) = to_parse
                    .parse::<i128>()
                    .ok()
                    .or_else(|| {
                        to_parse
                            .strip_prefix("0x")
                            .and_then(|s| i128::from_str_radix(s, 16).ok())
                    })
                    .or_else(|| {
                        to_parse
                            .strip_prefix("0b")
                            .and_then(|s| i128::from_str_radix(s, 2).ok())
                    })
                {
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
            Rule::STRING => Ok(XStaticExpr::LiteralString(
                apply_escapes(
                    input
                        .clone()
                        .into_inner()
                        .next()
                        .unwrap()
                        .into_inner()
                        .next()
                        .unwrap()
                        .as_str(),
                )
                .map_err(|e| e.trace(&input))?,
            )),
            Rule::RAW_STRING => Ok(XStaticExpr::LiteralString(
                input
                    .into_inner()
                    .next()
                    .unwrap()
                    .into_inner()
                    .next()
                    .unwrap()
                    .as_str()
                    .to_string(),
            )),
            Rule::FORMATTED_STRING => {
                let join_sym = interner.get_or_intern_static("join");
                let to_str_sym = interner.get_or_intern_static("to_str");
                let format_sym = interner.get_or_intern_static("format");
                let mut parts = Vec::new();
                for part in input
                    .into_inner()
                    .next()
                    .unwrap()
                    .into_inner()
                    .next()
                    .unwrap()
                    .into_inner()
                {
                    let expr = match part.as_rule() {
                        Rule::expression => XStaticExpr::new_call_sym(
                            to_str_sym,
                            vec![self.parse_expr(part, interner)?],
                        ),
                        Rule::f_with_formatting => {
                            let mut inner = part.into_inner();
                            let expr = self.parse_expr(inner.next().unwrap(), interner)?;
                            let formatting = XStaticExpr::LiteralString(
                                inner.next().unwrap().as_str().to_string(),
                            );
                            XStaticExpr::new_call_sym(format_sym, vec![expr, formatting])
                        }
                        _ => XStaticExpr::LiteralString(apply_brace_escape(
                            &apply_escapes(part.clone().as_str()).map_err(|e| e.trace(&part))?,
                        )),
                    };
                    parts.push(expr);
                }
                Ok(XStaticExpr::new_call_sym(
                    join_sym,
                    vec![XStaticExpr::Array(parts)],
                ))
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
                            .map(|p| self.parse_expr(p, interner))
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
                            .map(|p| self.parse_expr(p, interner))
                            .collect()
                    },
                )?;
                Ok(XStaticExpr::Tuple(parts))
            }
            Rule::turbofish_cname => {
                let mut iter = input.into_inner();
                let cname = iter.next().unwrap().as_str();
                let args = iter
                    .map(|p| self.get_complete_type(p, &HashSet::new(), interner, None, true))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(XStaticExpr::SpecializedIdent(
                    interner.get_or_intern(cname),
                    OverloadSpecialization::ParamTypes(args),
                ))
            }
            Rule::dyn_bind_cname => {
                let mut iter = input.into_inner();
                let cname = iter.next().unwrap().as_str();
                let args = iter
                    .map(|p| self.get_complete_type(p, &HashSet::new(), interner, None, false))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(XStaticExpr::SpecializedIdent(
                    interner.get_or_intern(cname),
                    OverloadSpecialization::Binding(args),
                ))
            }
            Rule::lambda_func => {
                let mut iter = input.clone().into_inner();
                let params_pair = iter.next().unwrap();
                let params = match params_pair.clone().into_inner().next() {
                    None => vec![],
                    Some(c) => self.parse_param_specs(c, &HashSet::new(), interner, None)?,
                };
                let body = iter.next().unwrap();
                let mut body_iter = body.into_inner();
                let (param_names, param_specs, param_static_defaults): (Vec<_>, Vec<_>, Vec<_>) =
                    params
                        .into_iter()
                        .map(|(name, xtype, default)| {
                            (
                                name,
                                XFuncParamSpec {
                                    type_: xtype,
                                    required: default.is_none(),
                                },
                                default,
                            )
                        })
                        .multiunzip();
                let param_len = param_specs.len();
                let defaults = param_static_defaults
                    .into_iter()
                    .filter_map(|s| s.map(|s| self.compile(s)))
                    .collect::<Result<_, _>>()
                    .map_err(|e| e.trace(&input))?;
                let mut subscope = CompilationScope::from_parent_lambda(
                    self,
                    param_names
                        .into_iter()
                        .zip(param_specs.iter().map(|s| s.type_.clone())),
                )
                .map_err(|e| e.trace(&params_pair))?;
                subscope.feed(
                    body_iter.next().unwrap(),
                    &HashSet::new(), // todo introduce new gen params names?
                    interner,
                )?;
                let out_pair = body_iter.next().unwrap();
                let out_static_expr = subscope.parse_expr(out_pair.clone(), interner)?;
                let out = subscope
                    .compile(out_static_expr)
                    .map_err(|e| e.trace(&out_pair))?;

                let out_type = subscope.type_of(&out).map_err(|e| e.trace(&out_pair))?;
                let output = Box::new(out);
                let spec = XFuncSpec {
                    generic_params: None, // todo
                    params: param_specs,
                    ret: out_type,
                    short_circuit_overloads: false,
                };
                let (func, parent_capture_requests) =
                    subscope.into_static_ud(defaults, param_len, output, self.id);
                for pr in parent_capture_requests {
                    self.cells.ipush(pr);
                }
                Ok(XStaticExpr::Lambda(
                    spec,
                    Box::new(XStaticFunction::UserFunction(Rc::new(func))),
                ))
            }
            _ => {
                panic!("not an expression {input:?}");
            }
        }
    }

    fn parse_param_specs(
        &mut self,
        param_pairs: Pair<Rule>,
        gen_param_names: &HashSet<String>,
        interner: &mut Interner,
        function_name: Option<Identifier>,
    ) -> Result<ParamSpecs<W, R, T>, TracedCompilationError> {
        let ret = param_pairs
            .clone()
            .into_inner()
            .map(|p| -> Result<_, TracedCompilationError> {
                let mut param_iter = p.clone().into_inner();
                let param_name = param_iter.next().unwrap().as_str();
                let name = interner.get_or_intern(param_name);
                let type_ = self.get_complete_type(
                    param_iter.next().unwrap(),
                    gen_param_names,
                    interner,
                    None,
                    false,
                )?;
                let default = param_iter
                    .next()
                    .map(|d| {
                        let d = d.into_inner().next().unwrap();
                        self.parse_expr(d.clone(), interner)
                    })
                    .transpose()?;
                Ok((name, type_, default))
            })
            .collect::<Result<Vec<_>, _>>()?;
        if let Some((out_of_order_param_name, ..)) = ret
            .iter()
            .skip_while(|(.., default)| default.is_none())
            .find(|(.., default)| default.is_none())
        {
            return Err(CompilationError::RequiredParamsAfterOptionalParams {
                function_name,
                param_name: *out_of_order_param_name,
            }
            .trace(&param_pairs));
        }
        Ok(ret)
    }
}
