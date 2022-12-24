use crate::builtin::bool::*;
use crate::builtin::floats::*;
use crate::builtin::generic::*;
use crate::builtin::int::*;
use crate::builtin::mapping::*;
use crate::builtin::optional::*;
use crate::builtin::sequence::*;
use crate::builtin::stack::*;
use crate::builtin::str::*;
use crate::builtin::unknown::*;


use std::io::Write;



use crate::builtin::generators::*;
use crate::builtin::include::INCLUDE;
use crate::builtin::set::*;
use crate::builtin::tuple::*;
use crate::root_compilation_scope::RootCompilationScope;

pub mod bool;
pub mod builtin_permissions;
mod core;
pub mod floats;
pub mod generators;
pub mod generic;
pub mod include;
pub mod int;
pub mod mapping;
pub mod optional;
pub mod sequence;
pub mod set;
pub mod stack;
pub mod str;
pub mod tuple;
pub mod unknown;

pub(crate) fn load_builtin<W: Write + 'static>(scope: &mut RootCompilationScope<W>) {
    add_int_type(scope).unwrap();
    add_int_add(scope).unwrap();
    add_int_bit_and(scope).unwrap();
    add_int_bit_or(scope).unwrap();
    add_int_chr(scope).unwrap();
    add_int_cmp(scope).unwrap();
    add_int_div(scope).unwrap();
    add_int_digits(scope).unwrap();
    add_int_eq(scope).unwrap();
    add_int_ge(scope).unwrap();
    add_int_gt(scope).unwrap();
    add_int_hash(scope).unwrap();
    add_int_le(scope).unwrap();
    add_int_lt(scope).unwrap();
    add_int_mod(scope).unwrap();
    add_int_mul(scope).unwrap();
    add_int_ne(scope).unwrap();
    add_int_neg(scope).unwrap();
    add_int_pow(scope).unwrap();
    add_int_sub(scope).unwrap();
    add_int_to_str(scope).unwrap();

    add_float_type(scope).unwrap();
    add_float_add(scope).unwrap();
    add_float_ceil(scope).unwrap();
    add_float_cmp(scope).unwrap();
    add_float_div(scope).unwrap();
    add_float_eq(scope).unwrap();
    add_float_floor(scope).unwrap();
    add_float_is_close(scope).unwrap();
    add_float_mod(scope).unwrap();
    add_float_mul(scope).unwrap();
    add_float_neg(scope).unwrap();
    add_float_pow(scope).unwrap();
    add_float_sqrt(scope).unwrap();
    add_float_sub(scope).unwrap();
    add_float_to_str(scope).unwrap();
    add_float_trunc(scope).unwrap();

    add_str_type(scope).unwrap();
    add_str_add(scope).unwrap();
    add_str_chars(scope).unwrap();
    add_str_cmp(scope).unwrap();
    add_str_eq(scope).unwrap();
    add_str_get(scope).unwrap();
    add_str_hash(scope).unwrap();
    add_str_ord(scope).unwrap();
    add_str_split(scope).unwrap();
    add_str_to_str(scope).unwrap();

    add_bool_type(scope).unwrap();
    add_bool_and(scope).unwrap();
    add_bool_assert(scope).unwrap();
    add_bool_cmp(scope).unwrap();
    add_bool_eq(scope).unwrap();
    add_bool_hash(scope).unwrap();
    add_bool_not(scope).unwrap();
    add_bool_or(scope).unwrap();
    add_bool_then(scope).unwrap();
    add_bool_to_str(scope).unwrap();

    add_unknown_eq(scope).unwrap();

    add_sequence_type(scope).unwrap();
    add_sequence_add(scope).unwrap();
    add_sequence_add_stack(scope).unwrap();
    add_sequence_addrev_stack(scope).unwrap();
    add_sequence_count(scope).unwrap();
    add_sequence_get(scope).unwrap();
    add_sequence_insert(scope).unwrap();
    add_sequence_len(scope).unwrap();
    add_sequence_map(scope).unwrap();
    add_sequence_nth(scope).unwrap();
    add_sequence_pop(scope).unwrap();
    add_sequence_push(scope).unwrap();
    add_sequence_range(scope).unwrap();
    add_sequence_reduce2(scope).unwrap();
    add_sequence_reduce3(scope).unwrap();
    add_sequence_rpush(scope).unwrap();
    add_sequence_set(scope).unwrap();
    add_sequence_skip(scope).unwrap();
    add_sequence_skip_until(scope).unwrap();
    add_sequence_sort(scope).unwrap();
    add_sequence_swap(scope).unwrap();
    add_sequence_take(scope).unwrap();
    add_sequence_take_while(scope).unwrap();
    add_sequence_to_array(scope).unwrap();
    add_sequence_to_generator(scope).unwrap();
    add_sequence_to_stack(scope).unwrap();
    add_sequence_dyn_eq(scope).unwrap();
    add_sequence_dyn_sort(scope).unwrap();
    add_sequence_dyn_unzip(scope).unwrap();
    add_sequence_dyn_zip(scope).unwrap();

    add_stack_type(scope).unwrap();
    add_stack_head(scope).unwrap();
    add_stack_len(scope).unwrap();
    add_stack_new(scope).unwrap();
    add_stack_push(scope).unwrap();
    add_stack_tail(scope).unwrap();
    add_stack_to_array(scope).unwrap();
    add_stack_to_array_reversed(scope).unwrap();

    add_optional_type(scope).unwrap();
    add_optional_and(scope).unwrap();
    add_optional_has_value(scope).unwrap();
    add_optional_map(scope).unwrap();
    add_optional_map_or(scope).unwrap();
    add_optional_null(scope).unwrap();
    add_optional_or(scope).unwrap();
    add_optional_or_unwrap(scope).unwrap();
    add_optional_some(scope).unwrap();
    add_optional_value(scope).unwrap();
    add_optional_dyn_eq(scope).unwrap();

    add_mapping_type(scope).unwrap();
    add_mapping_contains(scope).unwrap();
    add_mapping_discard(scope).unwrap();
    add_mapping_entries(scope).unwrap();
    add_mapping_get(scope).unwrap();
    add_mapping_len(scope).unwrap();
    add_mapping_lookup(scope).unwrap();
    add_mapping_new(scope).unwrap();
    add_mapping_pop(scope).unwrap();
    add_mapping_set(scope).unwrap();
    add_mapping_update(scope).unwrap();
    add_mapping_dyn_new(scope).unwrap();

    add_set_type(scope).unwrap();
    add_set_add(scope).unwrap();
    add_set_bit_and(scope).unwrap();
    add_set_contains(scope).unwrap();
    add_set_discard(scope).unwrap();
    add_set_len(scope).unwrap();
    add_set_new(scope).unwrap();
    add_set_remove(scope).unwrap();
    add_set_to_array(scope).unwrap();
    add_set_update(scope).unwrap();
    add_set_dyn_new(scope).unwrap();

    add_tuple_empty_and(scope).unwrap();
    add_tuple_dyn_eq(scope).unwrap();

    add_generator_type(scope).unwrap();
    add_generator_add(scope).unwrap();
    add_generator_filter(scope).unwrap();
    add_generator_get(scope).unwrap();
    add_generator_len(scope).unwrap();
    add_generator_map(scope).unwrap();
    add_generator_nth(scope).unwrap();
    add_generator_repeat(scope).unwrap();
    add_generator_skip(scope).unwrap();
    add_generator_skip_until(scope).unwrap();
    add_generator_successors(scope).unwrap();
    add_generator_take(scope).unwrap();
    add_generator_take_while(scope).unwrap();
    add_generator_to_array(scope).unwrap();
    add_generator_dyn_zip(scope).unwrap();
    add_generator_dyn_unzip(scope).unwrap();

    add_cast(scope).unwrap();
    add_debug(scope).unwrap();
    add_display(scope).unwrap();
    add_error(scope).unwrap();
    add_cmp_ge(scope).unwrap();
    add_cmp_gt(scope).unwrap();
    add_if(scope).unwrap();
    add_if_error(scope).unwrap();
    add_is_error(scope).unwrap();
    add_cmp_le(scope).unwrap();
    add_cmp_lt(scope).unwrap();
    add_ne(scope).unwrap();

    scope.feed_file(INCLUDE).unwrap();
}
