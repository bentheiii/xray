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
    fn foo()->Sequence<int>{
        let n = 6;
        fn tee(x:int)->int{
            x*n
        }
        fn bar()->Sequence<int>{
            [1,2,3].map((x: int)->{tee(x)})
        }
        bar()
    }
    let z = foo()[1];
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
