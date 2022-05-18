#![feature(trait_upcasting)]
#[macro_use]
extern crate lazy_static;
extern crate derivative;
extern crate mopa;
extern crate dyn_clone;

mod xscope;
mod xtype;
mod xvalue;
mod xexpr;
mod builtin;
mod native_types;
mod runtime;

extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate core;

use std::collections::{HashSet};
use std::iter;
use std::ops::Deref;
use std::sync::Arc;
use itertools::Itertools;
use pest::iterators::Pair;
use pest::Parser;
use pest::prec_climber::{Operator, PrecClimber};
use pest::prec_climber::Assoc::{Left, Right};
use num::BigRational;
use string_interner::StringInterner;

use crate::builtin::int::{*};
use crate::builtin::array::{*};
use crate::builtin::str::{*};
use crate::builtin::bool::{*};
use crate::builtin::rational::{*};
use crate::builtin::generic::{*};
use crate::builtin::optional::{*};
use crate::builtin::set::{*};
use crate::builtin::stack::{*};
use crate::runtime::{RTCell, RuntimeLimits};

use crate::xexpr::{CompilationResult, UfData, XExplicitArgSpec, XExplicitFuncSpec, XStaticExpr, XStaticFunction};
use crate::xscope::{Declaration, XCompilationScope, XCompilationScopeItem, XEvaluationScope};
use crate::xtype::{Bind, TRef, XCallableSpec, XFuncSpec, XStructFieldSpec, XStructSpec, XType};

#[derive(Parser)]
#[grammar = "xray.pest"]
struct XRayParser;

fn main() {
    let input = r#"
    union nest<T>(
        v: T,
        arr: Array<nest<T>>
    )

    fn flatten<T>(n: nest<T>) -> Stack<T> {
        fn helper(n: nest<T>, acc: Stack<T>) -> Stack<T> {
            fn arr_case(arr: Array<nest<T>>) -> Stack<T>{
                fn arr_case_helper(ret: Stack<T>, idx: int) -> Stack<T> {
                    (idx == arr.len()).if(
                        ret,
                        arr_case_helper(helper(arr.get(idx), ret), idx + 1)
                    )
                }
                arr_case_helper(acc, 0)
            }
            fn v_case(v: T) -> Stack<T> {
                acc.push(v)
            }
            (n::arr.map(arr_case) || n::v.map(v_case)).unwrap()
        }
        helper(n, stack())
    }

    let z = flatten(nest::arr([nest::v(1), nest::arr([nest::v(2), nest::arr([nest::v(3)])])]));
    "#;
    let mut parser = XRayParser::parse(Rule::header, input).unwrap();
    let body = parser.next().unwrap();
    let mut root_scope = XCompilationScope::root();
    let mut interner = StringInterner::default();

    add_int_type(&mut root_scope, &mut interner).unwrap();
    add_int_add(&mut root_scope, &mut interner).unwrap();
    add_int_mul(&mut root_scope, &mut interner).unwrap();
    add_int_mod(&mut root_scope, &mut interner).unwrap();
    add_int_div(&mut root_scope, &mut interner).unwrap();
    add_int_eq(&mut root_scope, &mut interner).unwrap();
    add_int_sub(&mut root_scope, &mut interner).unwrap();
    add_int_lt(&mut root_scope, &mut interner).unwrap();
    add_int_ge(&mut root_scope, &mut interner).unwrap();
    add_int_gt(&mut root_scope, &mut interner).unwrap();
    add_int_neg(&mut root_scope, &mut interner).unwrap();
    add_int_to_str(&mut root_scope, &mut interner).unwrap();
    add_int_digits(&mut root_scope, &mut interner).unwrap();

    add_rational_type(&mut root_scope, &mut interner).unwrap();
    add_rational_add(&mut root_scope, &mut interner).unwrap();
    add_rational_mul(&mut root_scope, &mut interner).unwrap();
    add_rational_floor(&mut root_scope, &mut interner).unwrap();
    add_rational_to_str(&mut root_scope, &mut interner).unwrap();

    add_str_type(&mut root_scope, &mut interner).unwrap();
    add_str_eq(&mut root_scope, &mut interner).unwrap();

    add_bool_type(&mut root_scope, &mut interner).unwrap();
    add_and(&mut root_scope, &mut interner).unwrap();
    add_or(&mut root_scope, &mut interner).unwrap();
    add_bool_not(&mut root_scope, &mut interner).unwrap();


    add_if(&mut root_scope, &mut interner).unwrap();
    add_error(&mut root_scope, &mut interner).unwrap();
    add_cast(&mut root_scope, &mut interner).unwrap();

    add_array_type(&mut root_scope, &mut interner).unwrap();
    add_array_get(&mut root_scope, &mut interner).unwrap();
    add_array_len(&mut root_scope, &mut interner).unwrap();
    add_array_add(&mut root_scope, &mut interner).unwrap();
    add_array_pop(&mut root_scope, &mut interner).unwrap();
    add_array_to_stack(&mut root_scope, &mut interner).unwrap();
    add_array_map(&mut root_scope, &mut interner).unwrap();
    add_array_push(&mut root_scope, &mut interner).unwrap();

    add_set_type(&mut root_scope, &mut interner).unwrap();
    add_set_bitor(&mut root_scope, &mut interner).unwrap();

    add_stack_type(&mut root_scope, &mut interner).unwrap();
    add_stack_new(&mut root_scope, &mut interner).unwrap();
    add_stack_push(&mut root_scope, &mut interner).unwrap();
    add_stack_to_array(&mut root_scope, &mut interner).unwrap();
    add_stack_to_array_reversed(&mut root_scope, &mut interner).unwrap();
    add_stack_len(&mut root_scope, &mut interner).unwrap();
    add_stack_head(&mut root_scope, &mut interner).unwrap();
    add_stack_tail(&mut root_scope, &mut interner).unwrap();

    add_optional_type(&mut root_scope, &mut interner).unwrap();
    add_optional_some(&mut root_scope, &mut interner).unwrap();
    add_optional_null(&mut root_scope, &mut interner).unwrap();
    add_optional_map(&mut root_scope, &mut interner).unwrap();
    add_optional_or_unwrap(&mut root_scope, &mut interner).unwrap();
    add_optional_or(&mut root_scope, &mut interner).unwrap();
    add_optional_and(&mut root_scope, &mut interner).unwrap();
    add_optional_value(&mut root_scope, &mut interner).unwrap();

    let limits = RuntimeLimits{..RuntimeLimits::default()
    };
    let runtime = limits.to_runtime();

    let decals = root_scope.feed(body, &HashSet::new(), &mut interner, runtime.clone()).unwrap();
    println!("compiled!");


    let mut eval_scope = XEvaluationScope::root();
    eval_scope.add_from(
        &decals, runtime
    ).unwrap();
    println!("z={:?}", eval_scope.get(interner.get_or_intern_static("z")).unwrap().value);
}

impl<'p> XCompilationScope<'p> {
    fn feed(&mut self, input: Pair<Rule>, parent_gen_param_names: &HashSet<String>, interner: &mut StringInterner, runtime: RTCell) -> Result<Vec<Declaration>, String> {
        match input.as_rule() {
            Rule::header | Rule::top_level_execution | Rule::execution | Rule::declaration => {
                let mut declarations = Vec::new();
                for inner in input.into_inner() {
                    declarations.extend(self.feed(inner, parent_gen_param_names, interner, runtime.clone())?);
                }
                Ok(declarations)
            }
            Rule::value => {
                let mut inners = input.into_inner();
                let _pub_opt = inners.next().unwrap();
                let var_name = inners.next().unwrap().as_str();
                let explicit_type_opt = inners.next().unwrap();
                let complete_type = explicit_type_opt.into_inner().next().map(|et| self.get_complete_type(et, parent_gen_param_names, interner)).transpose()?;
                let expr = to_expr(inners.next().unwrap(), &self, interner)?;
                let CompilationResult { expr: compiled, closure_vars: cvars } = expr.compile(&self)?;
                if let Some(complete_type) = complete_type {
                    let comp_xtype = compiled.xtype()?;
                    if complete_type != comp_xtype {
                        return Err(format!("type mismatch: expected {:?}, got {:?}", complete_type, comp_xtype));
                    }
                }
                self.closure_variables.extend(cvars);
                Ok(vec![self.add_var(interner.get_or_intern(var_name), compiled)?])
            }
            Rule::function => {
                let mut inners = input.into_inner();
                let _pub_opt = inners.next().unwrap();
                let var_name = inners.next().unwrap().as_str();
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
                    Some(param_pairs) => param_pairs.into_inner().map(|p| {
                        let mut param_iter = p.into_inner();
                        let name = param_iter.next().unwrap().as_str();
                        let type_ = self.get_complete_type(param_iter.next().unwrap(), &gen_param_names, interner)?;
                        let default = param_iter.next().map(|d| {
                            let d = d.into_inner().next().unwrap();
                            let e_scope = self.to_eval_scope(runtime.clone())?;
                            to_expr(d, &self, interner)?.compile(&self).and_then(|c| {
                                c.expr.eval(&e_scope, false, runtime.clone()).map(|r| r.unwrap_value())
                            })
                        }).transpose()?;
                        Ok(XExplicitArgSpec { name: interner.get_or_intern(name), type_, default })
                    }).collect::<Result<Vec<_>, String>>()?
                };
                // check that there are no required params after optional ones
                if let Some(out_of_order_param) = params.iter().skip_while(|p| p.default.is_none()).find(|p| p.default.is_none()) {
                    // todo
                    return Err(format!("required parameter '{:?}' cannot follow optional parameters", out_of_order_param.name));
                }
                let rtype = self.get_complete_type(inners.next().unwrap(), &gen_param_names, interner)?;
                let body = inners.next().unwrap();
                let spec = XExplicitFuncSpec {
                    generic_params: specific_gen_params,
                    args: params,
                    ret: rtype,
                };
                let name = interner.get_or_intern(var_name);
                let mut subscope = XCompilationScope::from_parent(self, name, spec.to_spec());
                for param in &spec.args {
                    subscope.add_param(param.name, param.type_.clone())?;
                }
                let mut body_iter = body.into_inner();
                let declarations = subscope.feed(body_iter.next().unwrap(), &gen_param_names, interner, runtime.clone())?;
                let compiled_output = to_expr(body_iter.next().unwrap(), &self, interner)?.compile(&subscope)?;
                let output = Box::new(compiled_output.expr);
                let out_type = output.xtype()?;
                if out_type != spec.ret {
                    return Err(format!("Function output type {} does not match expected type {}", out_type, spec.ret));
                }
                let cvars = subscope.closure_variables.iter().chain(compiled_output.closure_vars.iter()).cloned().collect::<HashSet<_>>();
                let func = XStaticFunction::UserFunction(
                    UfData::new(
                        spec,
                        declarations,
                        output,
                        cvars,
                    )
                );
                Ok(vec![self.add_func(name, func)?])
            }
            Rule::struct_def => {
                let mut inners = input.into_inner();
                let _pub_opt = inners.next().unwrap();
                let var_name = inners.next().unwrap().as_str();
                let gen_params = inners.next().unwrap();
                let mut gen_param_names = HashSet::new();
                if let Some(gen_params) = gen_params.into_inner().next() {
                    for param in gen_params.into_inner() {
                        gen_param_names.insert(param.as_str().to_string());
                    }
                }
                let param_pairs = inners.next().unwrap();
                let params = param_pairs.into_inner().map(|p| {
                    let mut param_iter = p.into_inner();
                    let name = param_iter.next().unwrap().as_str();
                    let type_ = self.get_complete_type(param_iter.next().unwrap(), &gen_param_names, interner)?;
                    Ok(XStructFieldSpec { name: name.to_string(), type_ })
                }).collect::<Result<Vec<_>, String>>()?;
                let symbol = interner.get_or_intern(var_name);
                let struct_ = XStructSpec::new(symbol, params);
                Ok(vec![self.add_struct(symbol, struct_)?])
            }
            Rule::union_def => {
                let mut inners = input.into_inner();
                let _pub_opt = inners.next().unwrap();
                let var_name = inners.next().unwrap().as_str();
                let gen_params = inners.next().unwrap();
                let mut gen_param_names = HashSet::new();
                if let Some(gen_params) = gen_params.into_inner().next() {
                    for param in gen_params.into_inner() {
                        gen_param_names.insert(param.as_str().to_string());
                    }
                }
                let param_pairs = inners.next().unwrap();
                let params = param_pairs.into_inner().map(|p| {
                    let mut param_iter = p.into_inner();
                    let name = param_iter.next().unwrap().as_str();
                    let type_ = self.get_complete_type(param_iter.next().unwrap(), &gen_param_names, interner)?;
                    Ok(XStructFieldSpec { name: name.to_string(), type_ })
                }).collect::<Result<Vec<_>, String>>()?;
                let symbol = interner.get_or_intern(var_name);
                let struct_ = XStructSpec::new(symbol, params);
                Ok(vec![self.add_union(symbol, struct_)?])
            }
            Rule::EOI => Ok(Vec::new()),
            _ => {
                println!("{:?}", input);
                Ok(vec![])
            }
        }
    }

    fn get_complete_type(&self, input: Pair<Rule>, generic_param_names: &HashSet<String>, interner: &mut StringInterner) -> Result<TRef, String> {
        match input.as_rule() {
            Rule::complete_type => {
                let mut inners = input.into_inner();
                let part1 = inners.next().unwrap();
                match part1.as_rule() {
                    Rule::signature => {
                        let mut sig_inners = part1.into_inner();
                        let param_spec_opt = sig_inners.next().unwrap();
                        let param_types = param_spec_opt.into_inner().next().map(|p| {
                            p.into_inner().map(|i| self.get_complete_type(i, generic_param_names, interner)).collect::<Result<Vec<_>, _>>()
                        }).transpose()?.unwrap_or_default();
                        let return_type = self.get_complete_type(sig_inners.next().unwrap(), generic_param_names, interner)?;
                        Ok(TRef::from(XType::XCallable(XCallableSpec {
                            param_types,
                            return_type,
                        })))
                    }
                    _ => {
                        // cname
                        let name = part1.as_str();
                        let gen_params = inners.next().map_or_else(|| Ok(vec![]), |p| {
                            p.into_inner().map(|p| {
                                self.get_complete_type(p, generic_param_names, interner)
                            }).collect::<Result<Vec<_>, _>>()
                        })?;
                        let symbol = interner.get_or_intern(name);
                        let t = self.get(symbol);
                        if t.is_none() {
                            if generic_param_names.contains(name) {
                                return Ok(TRef::from(XType::XGeneric(symbol)));
                            }
                            return Err(format!("type {} not found", name));
                        }
                        match t.unwrap() {
                            XCompilationScopeItem::NativeType(t) => {
                                if let XType::XNative(t, _) = t.to_arc().as_ref() {
                                    if gen_params.len() != t.generic_names().len() {
                                        return Err(format!("Generic parameter count mismatch for type {}, expected {}, got {}", name, t.generic_names().len(), gen_params.len()));
                                    }
                                    return Ok(TRef::from(XType::XNative(t.clone(), gen_params)));
                                } else {
                                    Ok(t)
                                }
                            }
                            // todo gen params
                            XCompilationScopeItem::Struct(t) => Ok(TRef::from(XType::XStruct(t, Bind::new()))),
                            XCompilationScopeItem::Union(t) => Ok(TRef::from(XType::XUnion(t, Bind::new()))),
                            other => Err(format!("{:?} is not a type", other))
                        }
                    }
                }
            }
            _ => {
                println!("{:?}", input.as_str());
                Err(format!("{} is not a type", input))
            }
        }
    }
}

lazy_static! {
    static ref CLIMBER: PrecClimber<Rule> = {
        PrecClimber::new(
            vec![
                Operator::new(Rule::BINARY_AND, Left)
                    | Operator::new(Rule::BINARY_OR, Left),
                Operator::new(Rule::BINARY_LT, Left)
                    | Operator::new(Rule::BINARY_GT, Left)
                    | Operator::new(Rule::BINARY_EQ, Left)
                    | Operator::new(Rule::BINARY_NE, Left)
                    | Operator::new(Rule::BINARY_LE, Left)
                    | Operator::new(Rule::BINARY_GE, Left),
                Operator::new(Rule::BINARY_BIT_OR, Left)
                    | Operator::new(Rule::BINARY_BIT_AND, Left)
                    | Operator::new(Rule::BINARY_BIT_XOR, Left),
                Operator::new(Rule::BINARY_ADD, Left)
                    | Operator::new(Rule::BINARY_SUB, Left),
                Operator::new(Rule::BINARY_MUL, Left)
                    | Operator::new(Rule::BINARY_DIV, Left)
                    | Operator::new(Rule::BINARY_MOD, Left),
                Operator::new(Rule::BINARY_POW, Right),
            ],
        )
    };
}

fn to_expr(input: Pair<Rule>, xscope: &XCompilationScope, interner: &mut StringInterner) -> Result<XStaticExpr, String> {
    match input.as_rule() {
        Rule::expression => {
            let (add_symbol, sub_symbol, mul_symbol, div_symbol, mod_symbol, pow_symbol, and_symbol, or_symbol, lt_symbol, gt_symbol, eq_symbol, ne_symbol, le_symbol, ge_symbol, bit_or_symbol, bit_and_symbol, bit_xor_symbol, ) = (interner.get_or_intern_static("add"), interner.get_or_intern_static("sub"), interner.get_or_intern_static("mul"), interner.get_or_intern_static("div"), interner.get_or_intern_static("mod"), interner.get_or_intern_static("pow"), interner.get_or_intern_static("and"), interner.get_or_intern_static("or"), interner.get_or_intern_static("lt"), interner.get_or_intern_static("gt"), interner.get_or_intern_static("eq"), interner.get_or_intern_static("ne"), interner.get_or_intern_static("le"), interner.get_or_intern_static("ge"), interner.get_or_intern_static("bit_or"), interner.get_or_intern_static("bit_and"), interner.get_or_intern_static("bit_xor"), );
            CLIMBER.climb(input.into_inner(), |pair| to_expr(pair, xscope, interner),
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
                          })
        }
        Rule::expression1 => {
            let mut iter = input.into_inner().rev();
            let mut expr = to_expr(iter.next().unwrap(), xscope, interner)?;
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
            let mut ret = to_expr(iter.next().unwrap(), xscope, interner)?;
            for meth_call in &iter.chunks(2) {
                let mut meth_call_iter = meth_call.into_iter();
                let method = XStaticExpr::Ident(interner.get_or_intern(meth_call_iter.next().unwrap().as_str()));
                let args = match meth_call_iter.next().unwrap().into_inner().next() {
                    None => Ok(vec![ret]),
                    Some(c) => iter::once(Ok(ret)).chain(c.into_inner().map(|p| to_expr(p, xscope, interner))).collect()
                }?;
                ret = XStaticExpr::Call(Box::new(method), args);
            };
            Ok(ret)
        }
        Rule::expression4 => {
            let mut iter = input.into_inner();
            let mut ret = to_expr(iter.next().unwrap(), xscope, interner)?;
            for next_args in iter {
                let member = next_args.as_str();
                ret = XStaticExpr::Member(Box::new(ret), member.to_string());
            }
            Ok(ret)
        }
        Rule::expression3 => {
            let mut iter = input.into_inner();
            let raw_callable = iter.next().unwrap();
            match iter.next() {
                None => to_expr(raw_callable, xscope, interner),
                Some(raw_first_args) => {
                    let args = raw_first_args.into_inner().next().map_or_else(|| Ok(vec![]), |c| {
                        c.into_inner().map(|p| to_expr(p, xscope, interner)).collect()
                    })?;
                    let mut ret;
                    if raw_callable.as_rule() == Rule::CNAME {}
                    let callable = Box::new(to_expr(raw_callable, xscope, interner)?);
                    ret = XStaticExpr::Call(callable, args);

                    for next_args in iter {
                        let args = next_args.into_inner().next().map_or_else(|| Ok(vec![]), |c| {
                            c.into_inner().map(|p| to_expr(p, xscope, interner)).collect()
                        })?;
                        ret = XStaticExpr::Call(Box::new(ret), args);
                    }
                    Ok(ret)
                }
            }
        }
        Rule::NUMBER_ANY => {
            if let Ok(whole) = input.as_str().parse::<i64>() {
                return Ok(XStaticExpr::LiteralInt(whole));
            }
            if let Ok(float) = input.as_str().parse::<f64>() {
                return Ok(XStaticExpr::LiteralRational(BigRational::from_float(float).unwrap()));
            }
            panic!("{} is not a number", input.as_str());
        }
        Rule::CNAME => {
            return Ok(XStaticExpr::Ident(interner.get_or_intern(input.as_str())));
        }
        Rule::STRING => {
            return Ok(XStaticExpr::LiteralString(input.into_inner().next().unwrap().as_str().to_string()));
        }
        Rule::bool => {
            return Ok(XStaticExpr::LiteralBool(input.as_str() == "true"));
        }
        Rule::container => {
            let mut iter = input.into_inner();
            let container_type = iter.next().unwrap().into_inner().next().map_or("array", |c| c.as_str());
            let parts = iter.next().map_or_else(
                || Ok(vec![]),
                |c| c.into_inner().map(|p| to_expr(p, xscope, interner)).collect(),
            )?;
            Ok(match container_type {
                "array" => XStaticExpr::Array(parts),
                "set" => XStaticExpr::Set(parts),
                _ => unreachable!()
            })
        }
        Rule::specialized_cname => {
            let mut iter = input.into_inner();
            let cname = iter.next().unwrap().as_str();
            let args = iter.map(|p| xscope.get_complete_type(p, &HashSet::new(), interner)).collect::<Result<Vec<_>, _>>()?;
            Ok(XStaticExpr::SpecializedIdent(interner.get_or_intern(cname), args))
        }
        _ => {
            panic!("not an expression {:?}", input);
        }
    }
}
