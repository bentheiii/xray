extern crate pest;

use std::collections::{HashSet};
use string_interner::StringInterner;
use xray::parser::{XRayParser, Rule};
use xray::runtime::RuntimeLimits;
use xray::std_compilation_scope;
use xray::xscope::{XCompilationScopeItem, XEvaluationScope};
use xray::xtype::common_type;


use crate::pest::Parser;

fn main() {
    let input = r###"
    let z = (null() && error('')) == null();
    "###;
    let mut parser = XRayParser::parse(Rule::header, input).unwrap();
    let body = parser.next().unwrap();
    let mut interner = StringInterner::default();
    let mut root_scope = std_compilation_scope(&mut interner);

    let limits = RuntimeLimits {
        ..RuntimeLimits::default()
    };
    let runtime = limits.to_runtime();

    let decals = root_scope.feed(body, &HashSet::new(), &mut interner, runtime.clone()).map_err(|e| e.display(&interner, input)).unwrap();
    println!("compiled!");


    let mut eval_scope = XEvaluationScope::root();
    eval_scope.add_from(
        &decals, runtime,
    ).unwrap();
    println!("z={:?}", eval_scope.get(interner.get_or_intern_static("z")).unwrap().value);
    let z_static = root_scope.get(interner.get_or_intern_static("z")).unwrap();
    if let XCompilationScopeItem::Value(t) = z_static{
        println!("z: {:?}", t.display_with_interner(&interner));
    } else {
        println!("z: {:?}", z_static);
    }
}


