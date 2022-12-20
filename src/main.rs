extern crate core;
extern crate pest;

use std::io::stdout;
use xray::root_runtime_scope::RootEvaluationScope;

use xray::runtime::RuntimeLimits;
use xray::std_compilation_scope;

fn main() {
    let input = r###"
    let s = set((x:int)->{0}, eq{int, int}).update([1,2,3,4,5]);
    let z = s.remove(4);
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
}
