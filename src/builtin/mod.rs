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

use rand::{RngCore, SeedableRng};
use std::io::Write;

use crate::builtin::cont_distributions::*;
use crate::builtin::datetime::*;
use crate::builtin::disc_distributions::*;
use crate::builtin::generators::*;
use crate::builtin::include::INCLUDE;
use crate::builtin::regex::*;
use crate::builtin::set::*;
use crate::builtin::structs::*;
use crate::builtin::tuple::*;
use crate::builtin::unions::*;
use crate::root_compilation_scope::RootCompilationScope;

pub mod bool;
pub mod builtin_permissions;
pub mod cont_distributions;
mod core;
pub mod datetime;
pub mod disc_distributions;
pub mod floats;
pub mod generators;
pub mod generic;
pub mod include;
pub mod int;
pub mod mapping;
pub mod optional;
pub mod regex;
pub mod sequence;
pub mod set;
pub mod stack;
pub mod str;
pub mod structs;
pub mod tuple;
pub mod unions;
pub mod unknown;

pub(crate) fn load_builtin<W: Write, R: RngCore + SeedableRng>(
    scope: &mut RootCompilationScope<W, R>,
) {
    add_int_type(scope).unwrap();
    add_int_add(scope).unwrap();
    add_int_binom(scope).unwrap();
    add_int_bit_and(scope).unwrap();
    add_int_bit_or(scope).unwrap();
    add_int_chr(scope).unwrap();
    add_int_cmp(scope).unwrap();
    add_int_div(scope).unwrap();
    add_int_digits(scope).unwrap();
    add_int_eq(scope).unwrap();
    add_int_format(scope).unwrap();
    add_int_ge(scope).unwrap();
    add_int_gt(scope).unwrap();
    add_int_hash(scope).unwrap();
    add_int_le(scope).unwrap();
    add_int_lt(scope).unwrap();
    add_int_mod(scope).unwrap();
    add_int_multinom(scope).unwrap();
    add_int_mul(scope).unwrap();
    add_int_ne(scope).unwrap();
    add_int_neg(scope).unwrap();
    add_int_pow(scope).unwrap();
    add_int_sub(scope).unwrap();
    add_int_to_float(scope).unwrap();
    add_int_to_str(scope).unwrap();
    add_int_bit_xor(scope).unwrap();

    add_float_type(scope).unwrap();
    add_float_acos(scope).unwrap();
    add_float_acosh(scope).unwrap();
    add_float_add(scope).unwrap();
    add_float_asin(scope).unwrap();
    add_float_asinh(scope).unwrap();
    add_float_atan(scope).unwrap();
    add_float_atan2(scope).unwrap();
    add_float_atanh(scope).unwrap();
    add_float_cbrt(scope).unwrap();
    add_float_ceil(scope).unwrap();
    add_float_cmp(scope).unwrap();
    add_float_cos(scope).unwrap();
    add_float_cosh(scope).unwrap();
    add_float_div(scope).unwrap();
    add_float_eq(scope).unwrap();
    add_float_erf(scope).unwrap();
    add_float_erfc(scope).unwrap();
    add_float_floor(scope).unwrap();
    add_float_format(scope).unwrap();
    add_float_gamma(scope).unwrap();
    add_float_gammaln(scope).unwrap();
    add_float_is_close(scope).unwrap();
    add_float_ln(scope).unwrap();
    add_float_mod(scope).unwrap();
    add_float_mul(scope).unwrap();
    add_float_neg(scope).unwrap();
    add_float_pow(scope).unwrap();
    add_float_sin(scope).unwrap();
    add_float_sqrt(scope).unwrap();
    add_float_sub(scope).unwrap();
    add_float_tan(scope).unwrap();
    add_float_tanh(scope).unwrap();
    add_float_to_str(scope).unwrap();
    add_float_trunc(scope).unwrap();

    add_str_type(scope).unwrap();
    add_str_add(scope).unwrap();
    add_str_chars(scope).unwrap();
    add_str_cmp(scope).unwrap();
    add_str_code_point(scope).unwrap();
    add_str_eq(scope).unwrap();
    add_str_find(scope).unwrap();
    add_str_format(scope).unwrap();
    add_str_get(scope).unwrap();
    add_str_hash(scope).unwrap();
    add_str_len(scope).unwrap();
    add_str_lower(scope).unwrap();
    add_str_rfind(scope).unwrap();
    add_str_substring(scope).unwrap();
    add_str_to_int(scope).unwrap();
    add_str_to_str(scope).unwrap();
    add_str_upper(scope).unwrap();

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
    add_unknown_hash(scope).unwrap();
    add_unknown_to_str(scope).unwrap();

    add_sequence_type(scope).unwrap();
    add_sequence_add(scope).unwrap();
    add_sequence_add_stack(scope).unwrap();
    add_sequence_addrev_stack(scope).unwrap();
    add_sequence_count(scope).unwrap();
    add_sequence_get(scope).unwrap();
    add_sequence_insert(scope).unwrap();
    add_sequence_len(scope).unwrap();
    add_sequence_map(scope).unwrap();
    add_sequence_n_largest(scope).unwrap();
    add_sequence_nth_largest(scope).unwrap();
    add_sequence_n_smallest(scope).unwrap();
    add_sequence_nth_smallest(scope).unwrap();
    add_sequence_nth(scope).unwrap();
    add_sequence_pop(scope).unwrap();
    add_sequence_push(scope).unwrap();
    add_sequence_range(scope).unwrap();
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
    add_sequence_dyn_cmp(scope).unwrap();
    add_sequence_dyn_eq(scope).unwrap();
    add_sequence_dyn_hash(scope).unwrap();
    add_sequence_dyn_mean(scope).unwrap();
    add_sequence_dyn_geo_mean(scope).unwrap();
    add_sequence_dyn_median(scope).unwrap();
    add_sequence_dyn_n_largest(scope).unwrap();
    add_sequence_dyn_n_smallest(scope).unwrap();
    add_sequence_dyn_nth_largest(scope).unwrap();
    add_sequence_dyn_nth_smallest(scope).unwrap();
    add_sequence_dyn_rank_avg(scope).unwrap();
    add_sequence_dyn_rank_eq(scope).unwrap();
    add_sequence_dyn_rank_sorted_avg(scope).unwrap();
    add_sequence_dyn_rank_sorted_eq(scope).unwrap();
    add_sequence_dyn_sort(scope).unwrap();
    add_sequence_dyn_to_str(scope).unwrap();
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
    add_stack_dyn_eq(scope).unwrap();
    add_stack_dyn_hash(scope).unwrap();

    add_optional_type(scope).unwrap();
    add_optional_and(scope).unwrap();
    add_optional_has_value(scope).unwrap();
    add_optional_map(scope).unwrap();
    add_optional_map_or(scope).unwrap();
    add_optional_none(scope).unwrap();
    add_optional_or(scope).unwrap();
    add_optional_or_unwrap(scope).unwrap();
    add_optional_some(scope).unwrap();
    add_optional_value(scope).unwrap();
    add_optional_dyn_eq(scope).unwrap();
    add_optional_dyn_hash(scope).unwrap();
    add_optional_dyn_to_string(scope).unwrap();

    add_mapping_type(scope).unwrap();
    add_mapping_discard(scope).unwrap();
    add_mapping_len(scope).unwrap();
    add_mapping_lookup(scope).unwrap();
    add_mapping_new(scope).unwrap();
    add_mapping_pop(scope).unwrap();
    add_mapping_set(scope).unwrap();
    add_mapping_to_generator(scope).unwrap();
    add_mapping_update(scope).unwrap();
    add_mapping_update_from_keys(scope).unwrap();
    add_mapping_dyn_eq(scope).unwrap();
    add_mapping_dyn_hash(scope).unwrap();
    add_mapping_dyn_new(scope).unwrap();

    add_set_type(scope).unwrap();
    add_set_add(scope).unwrap();
    add_set_clear(scope).unwrap();
    add_set_contains(scope).unwrap();
    add_set_discard(scope).unwrap();
    add_set_hash(scope).unwrap();
    add_set_len(scope).unwrap();
    add_set_new(scope).unwrap();
    add_set_remove(scope).unwrap();
    add_set_to_generator(scope).unwrap();
    add_set_update(scope).unwrap();
    add_set_dyn_new(scope).unwrap();

    add_tuple_empty_and(scope).unwrap();
    add_tuple_dyn_cmp(scope).unwrap();
    add_tuple_dyn_eq(scope).unwrap();
    add_tuple_dyn_hash(scope).unwrap();
    add_tuple_dyn_to_str(scope).unwrap();

    add_generator_type(scope).unwrap();
    add_generator_add(scope).unwrap();
    add_generator_aggregate(scope).unwrap();
    add_generator_filter(scope).unwrap();
    add_generator_get(scope).unwrap();
    add_generator_join(scope).unwrap();
    add_generator_last(scope).unwrap();
    add_generator_len(scope).unwrap();
    add_generator_map(scope).unwrap();
    add_generator_nth(scope).unwrap();
    add_generator_repeat(scope).unwrap();
    add_generator_skip(scope).unwrap();
    add_generator_skip_until(scope).unwrap();
    add_generator_successors_until(scope).unwrap();
    add_generator_take(scope).unwrap();
    add_generator_take_while(scope).unwrap();
    add_generator_to_array(scope).unwrap();
    add_generator_with_count(scope).unwrap();
    add_generator_dyn_mean(scope).unwrap();
    add_generator_dyn_geo_mean(scope).unwrap();
    add_generator_dyn_unique(scope).unwrap();
    add_generator_dyn_unzip(scope).unwrap();
    add_generator_dyn_with_count(scope).unwrap();
    add_generator_dyn_zip(scope).unwrap();

    add_regex_type(scope).unwrap();
    add_regex_match(scope).unwrap();
    add_regex_new(scope).unwrap();

    add_continuous_distribution_type(scope).unwrap();
    add_contdist_beta(scope).unwrap();
    add_contdist_cdf(scope).unwrap();
    add_contdist_exp(scope).unwrap();
    add_contdist_fs(scope).unwrap();
    add_contdist_gamma(scope).unwrap();
    add_contdist_lognormal(scope).unwrap();
    add_contdist_mean(scope).unwrap();
    add_contdist_normal(scope).unwrap();
    add_contdist_pdf(scope).unwrap();
    add_contdist_quantile(scope).unwrap();
    add_contdist_rectangular(scope).unwrap();
    add_contdist_sample(scope).unwrap();
    add_contdist_skewness(scope).unwrap();
    add_contdist_students_t(scope).unwrap();
    add_contdist_variance(scope).unwrap();
    add_contdist_weibull(scope).unwrap();

    add_discrete_distribution_type(scope).unwrap();
    add_discdist_binomial(scope).unwrap();
    add_discdist_cdf(scope).unwrap();
    add_discdist_hypergeometric(scope).unwrap();
    add_discdist_mean(scope).unwrap();
    add_discdist_negative_binomial(scope).unwrap();
    add_discdist_pmf(scope).unwrap();
    add_discdist_quantile(scope).unwrap();
    add_discdist_poisson(scope).unwrap();
    add_discdist_custom(scope).unwrap();
    add_discdist_sample(scope).unwrap();
    add_discdist_skewness(scope).unwrap();
    add_discdist_variance(scope).unwrap();
    add_discdist_uniform(scope).unwrap();

    add_cast(scope).unwrap();
    add_debug(scope).unwrap();
    add_display(scope).unwrap();
    add_error(scope).unwrap();
    add_cmp_ge(scope).unwrap();
    add_cmp_gt(scope).unwrap();
    add_get_error(scope).unwrap();
    add_if(scope).unwrap();
    add_if_error(scope).unwrap();
    add_if_error_specific(scope).unwrap();
    add_is_error(scope).unwrap();
    add_cmp_le(scope).unwrap();
    add_cmp_lt(scope).unwrap();
    add_ne(scope).unwrap();
    add_partial(scope).unwrap();

    add_generic_dyn_contains(scope).unwrap();
    add_generic_dyn_harmonic_mean(scope).unwrap();
    add_generic_dyn_max(scope).unwrap();
    add_generic_dyn_min(scope).unwrap();
    add_generic_dyn_max_iter(scope).unwrap();
    add_generic_dyn_min_iter(scope).unwrap();
    add_generic_dyn_product(scope).unwrap();
    add_generic_dyn_sum(scope).unwrap();

    add_datetime_now(scope).unwrap();

    add_struct_members(scope).unwrap();

    add_union_members(scope).unwrap();

    scope.feed_file(INCLUDE).unwrap();
}
