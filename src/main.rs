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

extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate core;

use std::collections::{HashMap, HashSet};
use std::iter;
use std::sync::Arc;
use itertools::Itertools;
use pest::iterators::Pair;
use pest::Parser;
use pest::prec_climber::{Operator, PrecClimber};
use pest::prec_climber::Assoc::{Left, Right};
use std::iter::FromIterator;
use num::BigRational;

use crate::builtin::int::{*};
use crate::builtin::array::{*};
use crate::builtin::str::{*};
use crate::builtin::bool::{*};
use crate::builtin::rational::{*};
use crate::builtin::generic::{*};
use crate::builtin::set::{*};
use crate::builtin::stack::{add_stack_head, add_stack_len, add_stack_new, add_stack_push, add_stack_tail, add_stack_to_array, add_stack_to_array_reversed, add_stack_type};

use crate::xexpr::{XExplicitArgSpec, XExplicitFuncSpec, XStaticExpr, XStaticFunction};
use crate::xscope::{Declaration, XCompilationScope, XCompilationScopeItem, XEvaluationScope};
use crate::xtype::{Bind, XCallableSpec, XFuncSpec, XStructFieldSpec, XStructSpec, XType};

#[derive(Parser)]
#[grammar = "xray.pest"]
struct XRayParser;

fn main() {
    let input = r#"
    fn map<T, V>(xs: Array<T>, f: (T) -> (V)) -> Array<V> {
        fn helper(input_idx: int, output: Stack<V>)->Stack<V> {
            (input_idx == xs.len()).if(
                output,
                helper(input_idx+1, output.push(f(xs.get(input_idx))))
                )
        }
        let output_stk = stack();
        helper(0, output_stk).to_array()
    }

    fn inverse(x: int) -> rational {
        1 / x
    }

    let z = map([1, 2, 3], inverse);
    "#;
    let mut parser = XRayParser::parse(Rule::header, input).unwrap();
    let body = parser.next().unwrap();
    let mut root_scope = XCompilationScope::root();

    add_int_type(&mut root_scope).unwrap();
    add_int_add(&mut root_scope).unwrap();
    add_int_mul(&mut root_scope).unwrap();
    add_int_mod(&mut root_scope).unwrap();
    add_int_div(&mut root_scope).unwrap();
    add_int_eq(&mut root_scope).unwrap();
    add_int_sub(&mut root_scope).unwrap();
    add_int_lt(&mut root_scope).unwrap();
    add_int_ge(&mut root_scope).unwrap();
    add_int_gt(&mut root_scope).unwrap();
    add_int_neg(&mut root_scope).unwrap();
    add_int_to_str(&mut root_scope).unwrap();

    add_rational_type(&mut root_scope).unwrap();
    add_rational_add(&mut root_scope).unwrap();
    add_rational_mul(&mut root_scope).unwrap();
    add_rational_floor(&mut root_scope).unwrap();
    add_rational_to_str(&mut root_scope).unwrap();

    add_str_type(&mut root_scope).unwrap();
    add_str_eq(&mut root_scope).unwrap();

    add_bool_type(&mut root_scope).unwrap();
    add_and(&mut root_scope).unwrap();
    add_or(&mut root_scope).unwrap();
    add_bool_not(&mut root_scope).unwrap();


    add_if(&mut root_scope).unwrap();
    add_panic(&mut root_scope).unwrap();

    add_array_type(&mut root_scope).unwrap();
    add_array_get(&mut root_scope).unwrap();
    add_array_len(&mut root_scope).unwrap();
    add_array_add(&mut root_scope).unwrap();
    add_array_pop(&mut root_scope).unwrap();
    add_array_to_stack(&mut root_scope).unwrap();

    add_set_type(&mut root_scope).unwrap();
    add_set_bitor(&mut root_scope).unwrap();

    add_stack_type(&mut root_scope).unwrap();
    add_stack_new(&mut root_scope).unwrap();
    add_stack_push(&mut root_scope).unwrap();
    add_stack_to_array(&mut root_scope).unwrap();
    add_stack_to_array_reversed(&mut root_scope).unwrap();
    add_stack_len(&mut root_scope).unwrap();
    add_stack_head(&mut root_scope).unwrap();
    add_stack_tail(&mut root_scope).unwrap();

    let decals = root_scope.feed(body).unwrap();
    println!("compiled!");

    let mut eval_scope = XEvaluationScope::root();
    eval_scope.add_from(
        &decals
    ).unwrap();
    //println!("{:?}", root_scope.get("z"));
    println!("z={:?}", eval_scope.get("z"));
}

impl<'p> XCompilationScope<'p> {
    fn feed(&mut self, input: Pair<Rule>) -> Result<Vec<Declaration>, String> {
        match input.as_rule() {
            Rule::header | Rule::top_level_execution | Rule::execution | Rule::declaration => {
                let mut declarations = Vec::new();
                for inner in input.into_inner() {
                    declarations.extend(self.feed(inner)?);
                }
                Ok(declarations)
            }
            Rule::value => {
                let mut inners = input.into_inner();
                let _pub_opt = inners.next().unwrap();
                let var_name = inners.next().unwrap().as_str();
                let expr = to_expr(inners.next().unwrap(), &self)?;
                let compiled = expr.compile(&self)?;
                Ok(vec![self.add_var(&var_name, compiled)?])
            }
            Rule::function => {
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
                let params = match inners.next().unwrap().into_inner().next() {
                    None => vec![],
                    Some(param_pairs) => param_pairs.into_inner().map(|p| {
                        let mut param_iter = p.into_inner();
                        let name = param_iter.next().unwrap().as_str();
                        let type_ = self.get_complete_type(param_iter.next().unwrap(), &gen_param_names)?;
                        let default = param_iter.next().map(|d| {
                            let e_scope = XEvaluationScope::root(); // todo fix
                            to_expr(d, &self)?.compile(&self).and_then(|c| c.eval(&e_scope, false).map(|r| r.unwrap_value()))
                        }).transpose()?;
                        Ok(XExplicitArgSpec { name: name.to_string(), type_, default })
                    }).collect::<Result<Vec<_>, String>>()?
                };
                let rtype = self.get_complete_type(inners.next().unwrap(), &gen_param_names)?;
                let body = inners.next().unwrap();
                let spec = XExplicitFuncSpec {
                    generic_params: None,
                    args: params,
                    ret: rtype,
                };
                let mut subscope = XCompilationScope::from_parent(self, var_name.to_string(), spec.to_spec());
                for param in &spec.args {
                    subscope.add_param(&param.name, param.type_.clone())?;
                }
                let mut body_iter = body.into_inner();
                let declarations = subscope.feed(body_iter.next().unwrap())?;
                let output = Box::new(to_expr(body_iter.next().unwrap(), &self)?.compile(&subscope)?);
                let out_type = output.xtype()?;
                if out_type != spec.ret {
                    return Err(format!("Function output type {} does not match expected type {}", out_type, spec.ret));
                }
                let func = XStaticFunction::UserFunction(
                    spec,
                    declarations,
                    output,
                );
                Ok(vec![self.add_func(&var_name, func.clone())?])
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
                    let type_ = self.get_complete_type(param_iter.next().unwrap(), &gen_param_names)?;
                    Ok(XStructFieldSpec { name: name.to_string(), type_ })
                }).collect::<Result<Vec<_>, String>>()?;
                let struct_ = XStructSpec::new(var_name.to_string(), params);
                Ok(vec![self.add_struct(&var_name, struct_.clone())?])
            }
            Rule::EOI => Ok(Vec::new()),
            _ => {
                println!("{:?}", input);
                Ok(vec![])
            }
        }
    }

    fn get_complete_type(&self, input: Pair<Rule>, generic_param_names: &HashSet<String>) -> Result<Arc<XType>, String> {
        match input.as_rule() {
            Rule::complete_type => {
                let mut inners = input.into_inner();
                let part1 = inners.next().unwrap();
                match part1.as_rule() {
                    Rule::signature => {
                        let mut sig_inners = part1.into_inner();
                        let param_spec_opt = sig_inners.next().unwrap();
                        let param_types = param_spec_opt.into_inner().next().map(|p| {
                            p.into_inner().map(|i| self.get_complete_type(i, generic_param_names)).collect::<Result<Vec<_>, _>>()
                        }).transpose()?.unwrap_or_default();
                        let return_type = self.get_complete_type(sig_inners.next().unwrap(), generic_param_names)?;
                        Ok(Arc::new(XType::XCallable(XCallableSpec {
                            param_types,
                            return_type,
                        })))
                    }
                    _ => {
                        // cname
                        let name = part1.as_str();
                        let gen_params = inners.next().map_or_else(|| Ok(vec![]), |p| {
                            p.into_inner().map(|p| {
                                self.get_complete_type(p, generic_param_names)
                            }).collect::<Result<Vec<_>, _>>()
                        })?;
                        let t = self.get(name);
                        if t.is_none() {
                            if generic_param_names.contains(name) {
                                return Ok(Arc::new(XType::XGeneric(name.to_string())));
                            }
                            return Err(format!("type {} not found", name));
                        }
                        match t.unwrap() {
                            XCompilationScopeItem::NativeType(t) => {
                                if let XType::XNative(t, _) = t.as_ref() {
                                    if gen_params.len() != t.generic_names().len() {
                                        return Err(format!("Generic parameter count mismatch for type {}, expected {}, got {}", name, t.generic_names().len(), gen_params.len()));
                                    }
                                    let bind: Bind = HashMap::from_iter(t.generic_names().into_iter().zip(gen_params.clone().into_iter()));
                                    return Ok(Arc::new(XType::XNative(t.clone(), bind)));
                                } else {
                                    Ok(t)
                                }
                            }
                            XCompilationScopeItem::Struct(t) => Ok(Arc::new(XType::XStruct(t, HashMap::new()))),
                            other => Err(format!("{:?} is not a type", other))
                        }
                    }
                }
            }
            _ => {
                println!("{:?}", input);
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

fn to_expr(input: Pair<Rule>, xscope: &XCompilationScope) -> Result<XStaticExpr, String> {
    match input.as_rule() {
        Rule::expression => {
            CLIMBER.climb(input.into_inner(), |pair| to_expr(pair, xscope),
                          |lhs, op, rhs| {
                              let lhs = lhs?;
                              let rhs = rhs?;
                              let func = match op.as_rule() {
                                  Rule::BINARY_ADD => "add",
                                  Rule::BINARY_SUB => "sub",
                                  Rule::BINARY_MUL => "mul",
                                  Rule::BINARY_DIV => "div",
                                  Rule::BINARY_MOD => "mod",
                                  Rule::BINARY_POW => "pow",
                                  Rule::BINARY_AND => "and",
                                  Rule::BINARY_OR => "or",
                                  Rule::BINARY_LT => "lt",
                                  Rule::BINARY_GT => "gt",
                                  Rule::BINARY_EQ => "eq",
                                  Rule::BINARY_NE => "ne",
                                  Rule::BINARY_LE => "le",
                                  Rule::BINARY_GE => "ge",
                                  Rule::BINARY_BIT_OR => "bit_or",
                                  Rule::BINARY_BIT_AND => "bit_and",
                                  Rule::BINARY_BIT_XOR => "bit_xor",
                                  _ => unreachable!(),
                              };
                              Ok(XStaticExpr::new_call(func, vec![lhs, rhs]))
                          })
        }
        Rule::expression1 => {
            let mut iter = input.into_inner().rev();
            let mut expr = to_expr(iter.next().unwrap(), xscope)?;
            for inner in iter {
                let func = match inner.as_rule() {
                    Rule::UNARY_PLUS => "pos",
                    Rule::UNARY_MINUS => "neg",
                    Rule::UNARY_NOT => "not",
                    _ => unreachable!(),
                };
                expr = XStaticExpr::new_call(func, vec![expr]);
            }
            Ok(expr)
        }
        Rule::expression2 => {
            let mut iter = input.into_inner();
            let mut ret = to_expr(iter.next().unwrap(), xscope)?;
            for meth_call in &iter.chunks(2) {
                let mut meth_call_iter = meth_call.into_iter();
                let method = XStaticExpr::Ident(meth_call_iter.next().unwrap().as_str().to_string());
                let args = match meth_call_iter.next().unwrap().into_inner().next() {
                    None => Ok(vec![ret]),
                    Some(c) => iter::once(Ok(ret)).chain(c.into_inner().map(|p| to_expr(p, xscope))).collect()
                }?;
                ret = XStaticExpr::Call(Box::new(method), args);
            };
            Ok(ret)
        }
        Rule::expression3 => {
            let mut iter = input.into_inner();
            let mut ret = to_expr(iter.next().unwrap(), xscope)?;
            for next_args in iter {
                let member = next_args.as_str();
                ret = XStaticExpr::Member(Box::new(ret), member.to_string());
            }
            Ok(ret)
        }
        Rule::expression4 => {
            let mut iter = input.into_inner();
            let raw_callable = iter.next().unwrap();
            match iter.next() {
                None => to_expr(raw_callable, xscope),
                Some(raw_first_args) => {
                    let args = raw_first_args.into_inner().next().map_or_else(|| Ok(vec![]), |c| {
                        c.into_inner().map(|p| to_expr(p, xscope)).collect()
                    })?;
                    let mut ret;
                    if raw_callable.as_rule() == Rule::CNAME {}
                    let callable = Box::new(to_expr(raw_callable, xscope)?);
                    ret = XStaticExpr::Call(callable, args);

                    for next_args in iter {
                        let args = next_args.into_inner().next().map_or_else(|| Ok(vec![]), |c| {
                            c.into_inner().map(|p| to_expr(p, xscope)).collect()
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
            return Ok(XStaticExpr::Ident(input.as_str().to_string()));
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
                |c| c.into_inner().map(|p| to_expr(p, xscope)).collect(),
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
            let args = iter.map(|p| xscope.get_complete_type(p, &HashSet::new())).collect::<Result<Vec<_>, _>>()?;
            Ok(XStaticExpr::SpecializedIdent(cname.to_string(), args))
        }
        _ => {
            panic!("not an expression {:?}", input);
        }
    }
}
