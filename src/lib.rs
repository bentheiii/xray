#![feature(trait_upcasting)]

#[macro_use]
extern crate lazy_static;
extern crate derivative;
extern crate dyn_clone;
extern crate mopa;

pub mod builtin;
pub mod compilation_scope;
pub mod compile_err;
pub mod evaluation_scope;
pub mod native_types;
pub mod parser;
pub mod runtime;
pub mod util;
pub mod xexpr;
pub mod xtype;
pub mod xvalue;

extern crate pest;
#[macro_use]
pub extern crate pest_derive;

use crate::builtin::bool::*;
use crate::builtin::floats::*;
use crate::builtin::generic::*;
use crate::builtin::int::*;
use crate::builtin::mapping::*;
use crate::builtin::optional::*;
use crate::builtin::sequence::*;
use crate::builtin::stack::*;
use crate::builtin::str::*;
use crate::builtin::unknown::add_unknown_eq;
use crate::compilation_scope::{Declaration, XCompilationScope};
use crate::compile_err::{CompilationError, TracedCompilationError};
use crate::parser::{Rule, XRayParser};
use crate::pest::Parser;
use crate::runtime::RTCell;
use pest::error::Error;
use pest::iterators::Pair;
use std::collections::HashSet;
use string_interner::{DefaultSymbol, StringInterner};

use crate::evaluation_scope::XEvaluationScope;
use crate::xexpr::{
    CompilationResult, UfData, XExplicitArgSpec, XExplicitFuncSpec, XStaticExpr, XStaticFunction,
};
use crate::xtype::{
    Bind, CompoundKind, XCallableSpec, XCompoundFieldSpec, XCompoundSpec, XFuncParamSpec,
    XFuncSpec, XType,
};

pub type Identifier = DefaultSymbol;

pub fn std_compilation_scope<'a>(interner: &'_ mut StringInterner) -> XCompilationScope<'a> {
    let mut ret = XCompilationScope::root();

    add_int_type(&mut ret, interner).unwrap();
    add_int_add(&mut ret, interner).unwrap();
    add_int_mul(&mut ret, interner).unwrap();
    add_int_mod(&mut ret, interner).unwrap();
    add_int_div(&mut ret, interner).unwrap();
    add_int_eq(&mut ret, interner).unwrap();
    add_int_sub(&mut ret, interner).unwrap();
    add_int_lt(&mut ret, interner).unwrap();
    add_int_le(&mut ret, interner).unwrap();
    add_int_ge(&mut ret, interner).unwrap();
    add_int_gt(&mut ret, interner).unwrap();
    add_int_neg(&mut ret, interner).unwrap();
    add_int_to_str(&mut ret, interner).unwrap();
    add_int_digits(&mut ret, interner).unwrap();

    add_float_type(&mut ret, interner).unwrap();
    add_float_add(&mut ret, interner).unwrap();
    add_float_sub(&mut ret, interner).unwrap();
    add_float_mul(&mut ret, interner).unwrap();
    add_float_floor(&mut ret, interner).unwrap();
    add_float_to_str(&mut ret, interner).unwrap();
    add_float_eq(&mut ret, interner).unwrap();

    add_str_type(&mut ret, interner).unwrap();
    add_str_eq(&mut ret, interner).unwrap();
    add_str_add(&mut ret, interner).unwrap();

    add_bool_type(&mut ret, interner).unwrap();
    add_and(&mut ret, interner).unwrap();
    add_or(&mut ret, interner).unwrap();
    add_bool_not(&mut ret, interner).unwrap();
    add_bool_then(&mut ret, interner).unwrap();
    add_bool_eq(&mut ret, interner).unwrap();

    add_if(&mut ret, interner).unwrap();
    add_assert(&mut ret, interner).unwrap();
    add_error(&mut ret, interner).unwrap();
    add_cast(&mut ret, interner).unwrap();
    add_debug(&mut ret, interner).unwrap();
    add_ne(&mut ret, interner).unwrap();
    add_is_error(&mut ret, interner).unwrap();

    add_unknown_eq(&mut ret, interner).unwrap();

    add_sequence_type(&mut ret, interner).unwrap();
    add_sequence_get(&mut ret, interner).unwrap();
    add_sequence_len(&mut ret, interner).unwrap();
    add_sequence_add(&mut ret, interner).unwrap();
    add_sequence_add_stack(&mut ret, interner).unwrap();
    add_sequence_addrev_stack(&mut ret, interner).unwrap();
    add_sequence_pop(&mut ret, interner).unwrap();
    add_sequence_to_stack(&mut ret, interner).unwrap();
    add_sequence_map(&mut ret, interner).unwrap();
    add_sequence_push(&mut ret, interner).unwrap();
    add_sequence_eq(&mut ret, interner).unwrap();
    add_sequence_sort(&mut ret, interner).unwrap();
    add_sequence_reduce2(&mut ret, interner).unwrap();
    add_sequence_reduce3(&mut ret, interner).unwrap();
    add_sequence_range(&mut ret, interner).unwrap();
    add_sequence_filter(&mut ret, interner).unwrap();
    add_sequence_nth(&mut ret, interner).unwrap();
    add_sequence_take_while(&mut ret, interner).unwrap();
    add_sequence_skip_until(&mut ret, interner).unwrap();
    add_sequence_take(&mut ret, interner).unwrap();
    add_sequence_skip(&mut ret, interner).unwrap();

    add_stack_type(&mut ret, interner).unwrap();
    add_stack_new(&mut ret, interner).unwrap();
    add_stack_push(&mut ret, interner).unwrap();
    add_stack_to_array(&mut ret, interner).unwrap();
    add_stack_to_array_reversed(&mut ret, interner).unwrap();
    add_stack_len(&mut ret, interner).unwrap();
    add_stack_head(&mut ret, interner).unwrap();
    add_stack_tail(&mut ret, interner).unwrap();

    add_optional_type(&mut ret, interner).unwrap();
    add_optional_some(&mut ret, interner).unwrap();
    add_optional_null(&mut ret, interner).unwrap();
    add_optional_map(&mut ret, interner).unwrap();
    add_optional_map_or(&mut ret, interner).unwrap();
    add_optional_or_unwrap(&mut ret, interner).unwrap();
    add_optional_or(&mut ret, interner).unwrap();
    add_optional_and(&mut ret, interner).unwrap();
    add_optional_value(&mut ret, interner).unwrap();
    add_optional_has_value(&mut ret, interner).unwrap();
    add_optional_eq(&mut ret, interner).unwrap();

    add_mapping_new(&mut ret, interner).unwrap();
    add_mapping_set(&mut ret, interner).unwrap();
    add_mapping_get(&mut ret, interner).unwrap();
    add_mapping_update(&mut ret, interner).unwrap();
    add_mapping_len(&mut ret, interner).unwrap();
    add_mapping_entries(&mut ret, interner).unwrap();
    add_mapping_contains(&mut ret, interner).unwrap();
    add_mapping_pop(&mut ret, interner).unwrap();
    add_mapping_discard(&mut ret, interner).unwrap();

    ret
}

impl XCompilationScope<'_> {
    pub fn feed_file(
        &mut self,
        input: &str,
        interner: &mut StringInterner,
        runtime: RTCell,
    ) -> Result<Vec<Declaration>, TracedCompilationError> {
        let body = XRayParser::parse(Rule::header, input)
            .map(|mut p| p.next().unwrap())
            .map_err(TracedCompilationError::Syntax)?;
        self.feed(body, &HashSet::new(), interner, runtime.clone())
    }
}
