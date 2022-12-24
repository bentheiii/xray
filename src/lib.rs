#![allow(incomplete_features)]
#![warn(clippy::all)]

#[macro_use]
extern crate lazy_static;
extern crate derivative;
extern crate dyn_clone;

pub mod builtin;
//pub mod __compilation_scope;
mod compilation_scope;
pub mod compile_err;
pub mod native_types;
pub mod parser;
pub mod permissions;
pub mod root_compilation_scope;
pub mod root_runtime_scope;
pub mod runtime;
mod runtime_scope;
mod runtime_violation;
mod units;
pub mod util;
pub mod xexpr;
pub mod xtype;
pub mod xvalue;

extern crate pest;
#[macro_use]
pub extern crate pest_derive;
extern crate core;

use crate::compile_err::{CompilationError, TracedCompilationError};
use crate::parser::XRayParser;
use crate::root_compilation_scope::{Declaration, RootCompilationScope};

use std::io::Write;

use crate::runtime::RTCell;

use crate::builtin::include::INCLUDE;
use crate::builtin::load_builtin;
use crate::builtin::set::{
    add_set_add, add_set_bit_and, add_set_contains, add_set_discard, add_set_dyn_new, add_set_len,
    add_set_new, add_set_remove, add_set_to_array, add_set_type, add_set_update,
};
use crate::builtin::tuple::{add_tuple_dyn_eq, add_tuple_empty_and};
use crate::util::special_prefix_interner::SpecialPrefixSymbol;
use string_interner::{DefaultBackend, DefaultSymbol};

use crate::xexpr::{XExplicitStaticArgSpec, XStaticExpr, XStaticFunction};
use crate::xtype::{Bind, XCallableSpec, XCompoundFieldSpec, XCompoundSpec, XFuncSpec, XType};

pub type Identifier = SpecialPrefixSymbol<DefaultBackend<DefaultSymbol>>;

pub fn std_compilation_scope<W: Write + 'static>() -> RootCompilationScope<W> {
    let mut ret = RootCompilationScope::new();

    load_builtin(&mut ret);

    ret
}
