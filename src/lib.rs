#![allow(incomplete_features)]
#![feature(trait_upcasting)]
#![warn(clippy::all)]

#[macro_use]
extern crate lazy_static;
extern crate derivative;
extern crate dyn_clone;

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
extern crate core;

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
use crate::compilation_scope::{Declaration, RootCompilationScope, XCompilationScope};
use crate::compile_err::{CompilationError, TracedCompilationError};
use crate::parser::XRayParser;

use std::io::Write;

use crate::runtime::RTCell;

use string_interner::DefaultSymbol;

use crate::evaluation_scope::XEvaluationScope;
use crate::xexpr::{
    CompilationResult, UfData, XExplicitArgSpec, XExplicitFuncSpec, XStaticExpr, XStaticFunction,
};
use crate::xtype::{Bind, XCallableSpec, XCompoundFieldSpec, XCompoundSpec, XFuncSpec, XType};

pub type Identifier = DefaultSymbol;

pub fn std_compilation_scope<W: Write + 'static>(runtime: RTCell<W>) -> RootCompilationScope<W> {
    let mut ret = RootCompilationScope::new(runtime);

    add_int_type(&mut ret).unwrap();
    add_int_add(&mut ret).unwrap();
    add_int_mul(&mut ret).unwrap();
    add_int_mod(&mut ret).unwrap();
    add_int_div(&mut ret).unwrap();
    add_int_eq(&mut ret).unwrap();
    add_int_sub(&mut ret).unwrap();
    add_int_lt(&mut ret).unwrap();
    add_int_le(&mut ret).unwrap();
    add_int_ge(&mut ret).unwrap();
    add_int_gt(&mut ret).unwrap();
    add_int_neg(&mut ret).unwrap();
    add_int_to_str(&mut ret).unwrap();
    add_int_digits(&mut ret).unwrap();
    add_int_bit_or(&mut ret).unwrap();
    add_int_bit_and(&mut ret).unwrap();
    add_int_pow(&mut ret).unwrap();
    add_int_ne(&mut ret).unwrap();
    add_int_hash(&mut ret).unwrap();
    add_int_cmp(&mut ret).unwrap();

    add_float_type(&mut ret).unwrap();
    add_float_add(&mut ret).unwrap();
    add_float_sub(&mut ret).unwrap();
    add_float_mul(&mut ret).unwrap();
    add_float_mod(&mut ret).unwrap();
    add_float_div(&mut ret).unwrap();
    add_float_ceil(&mut ret).unwrap();
    add_float_trunc(&mut ret).unwrap();
    add_float_cmp(&mut ret).unwrap();
    add_float_floor(&mut ret).unwrap();
    add_float_to_str(&mut ret).unwrap();
    add_float_eq(&mut ret).unwrap();
    add_float_neg(&mut ret).unwrap();
    add_float_sqrt(&mut ret).unwrap();
    add_float_is_close(&mut ret).unwrap();

    add_str_type(&mut ret).unwrap();
    add_str_eq(&mut ret).unwrap();
    add_str_add(&mut ret).unwrap();
    add_str_hash(&mut ret).unwrap();
    add_str_cmp(&mut ret).unwrap();
    add_str_to_str(&mut ret).unwrap();

    add_bool_type(&mut ret).unwrap();
    add_bool_and(&mut ret).unwrap();
    add_bool_or(&mut ret).unwrap();
    add_bool_not(&mut ret).unwrap();
    add_bool_then(&mut ret).unwrap();
    add_bool_eq(&mut ret).unwrap();
    add_bool_hash(&mut ret).unwrap();
    add_bool_cmp(&mut ret).unwrap();
    add_bool_to_str(&mut ret).unwrap();

    add_if(&mut ret).unwrap();
    add_bool_assert(&mut ret).unwrap();
    add_error(&mut ret).unwrap();
    add_cast(&mut ret).unwrap();
    add_debug(&mut ret).unwrap();
    add_ne(&mut ret).unwrap();
    add_is_error(&mut ret).unwrap();
    add_if_error(&mut ret).unwrap();
    add_display(&mut ret).unwrap();
    add_cmp_lt(&mut ret).unwrap();
    add_cmp_gt(&mut ret).unwrap();
    add_cmp_le(&mut ret).unwrap();
    add_cmp_ge(&mut ret).unwrap();

    add_unknown_eq(&mut ret).unwrap();

    add_sequence_type(&mut ret).unwrap();
    add_sequence_get(&mut ret).unwrap();
    add_sequence_len(&mut ret).unwrap();
    add_sequence_add(&mut ret).unwrap();
    add_sequence_add_stack(&mut ret).unwrap();
    add_sequence_addrev_stack(&mut ret).unwrap();
    add_sequence_pop(&mut ret).unwrap();
    add_sequence_to_stack(&mut ret).unwrap();
    add_sequence_map(&mut ret).unwrap();
    add_sequence_push(&mut ret).unwrap();
    add_sequence_eq(&mut ret).unwrap();
    add_sequence_sort(&mut ret).unwrap();
    add_sequence_reduce2(&mut ret).unwrap();
    add_sequence_reduce3(&mut ret).unwrap();
    add_sequence_range(&mut ret).unwrap();
    add_sequence_filter(&mut ret).unwrap();
    add_sequence_nth(&mut ret).unwrap();
    add_sequence_take_while(&mut ret).unwrap();
    add_sequence_skip_until(&mut ret).unwrap();
    add_sequence_take(&mut ret).unwrap();
    add_sequence_skip(&mut ret).unwrap();
    add_sequence_rpush(&mut ret).unwrap();
    add_sequence_insert(&mut ret).unwrap();
    add_sequence_set(&mut ret).unwrap();
    add_sequence_swap(&mut ret).unwrap();
    add_sequence_dyn_sort(&mut ret).unwrap();

    add_stack_type(&mut ret).unwrap();
    add_stack_new(&mut ret).unwrap();
    add_stack_push(&mut ret).unwrap();
    add_stack_to_array(&mut ret).unwrap();
    add_stack_to_array_reversed(&mut ret).unwrap();
    add_stack_len(&mut ret).unwrap();
    add_stack_head(&mut ret).unwrap();
    add_stack_tail(&mut ret).unwrap();

    add_optional_type(&mut ret).unwrap();
    add_optional_some(&mut ret).unwrap();
    add_optional_null(&mut ret).unwrap();
    add_optional_map(&mut ret).unwrap();
    add_optional_map_or(&mut ret).unwrap();
    add_optional_or_unwrap(&mut ret).unwrap();
    add_optional_or(&mut ret).unwrap();
    add_optional_and(&mut ret).unwrap();
    add_optional_value(&mut ret).unwrap();
    add_optional_has_value(&mut ret).unwrap();
    add_optional_eq(&mut ret).unwrap();

    add_mapping_type(&mut ret).unwrap();
    add_mapping_new(&mut ret).unwrap();
    add_mapping_set(&mut ret).unwrap();
    add_mapping_get(&mut ret).unwrap();
    add_mapping_update(&mut ret).unwrap();
    add_mapping_len(&mut ret).unwrap();
    add_mapping_entries(&mut ret).unwrap();
    add_mapping_contains(&mut ret).unwrap();
    add_mapping_pop(&mut ret).unwrap();
    add_mapping_discard(&mut ret).unwrap();
    add_mapping_lookup(&mut ret).unwrap();
    add_mapping_new_dyn(&mut ret).unwrap();

    ret
}
