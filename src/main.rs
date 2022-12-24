extern crate core;
extern crate pest;

use std::io::{stdout, Stdout};
use std::mem::size_of;
use xray::root_runtime_scope::RootEvaluationScope;

use xray::runtime::RuntimeLimits;
use xray::std_compilation_scope;
use xray::xvalue::XValue;

fn main() {
    let input = r###"
    let s = successors(5, (x: int) -> {if(x%2 == 0, floor(x/2), 3*x+1)}).zip(count());
    let z = s.nth(0, (x: (int, int))->{x::item0==1}).value()::item1;
    "###;
    let limits = RuntimeLimits {
        ..RuntimeLimits::default()
    };
    let runtime = limits.to_runtime(stdout());

    let mut root_scope = std_compilation_scope();

    root_scope.feed_file(input).unwrap();
    println!("compiled!");

    let eval_scope = RootEvaluationScope::from_compilation_scope(&root_scope, runtime).unwrap();
    let z_value = &eval_scope.get_value("z").unwrap().clone().unwrap().value;
    println!("z={z_value:?}");

    println!("!!! {}", size_of::<XValue<Stdout>>())
}
