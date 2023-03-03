extern crate core;
extern crate pest;

use std::io::stdout;

use xray::root_runtime_scope::RootEvaluationScope;

use xray::runtime::RuntimeLimits;
use xray::std_compilation_scope;

fn main() {
    let input = r###"
    forward fn foo(i: int)->int;

    fn fool(i: int)->int {
        foo(i*i)
    }

    fn foo(i: int)->int{ i+2 }

    let z = fool(5);
    "###;
    /*
    fn foo()->()->(int){ // 32
        let i = 5;
        fn bar()->int{ // 33
            fn fib()->int{ // 34
                i
            }
            fib()
        }
        bar
    }
    let z = foo()();

     */
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
