extern crate core;
extern crate pest;

use rand::rngs::StdRng;
use xray::time_provider::SystemTimeProvider;
use std::io::stdout;

use xray::root_runtime_scope::RootEvaluationScope;

use xray::runtime::{RTCell, RuntimeLimits};
use xray::std_compilation_scope;

fn main() {
    let input = r###"
    let z = f"{1/-3:#=10.3f}";
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
    let runtime: RTCell<_, StdRng, _> = limits.to_runtime(stdout(), SystemTimeProvider);

    let mut root_scope = std_compilation_scope();

    root_scope.feed_file(input).unwrap();
    println!("compiled!");

    let eval_scope = RootEvaluationScope::from_compilation_scope(&root_scope, runtime).unwrap();
    let z_value = &eval_scope.get_value("z").unwrap().clone().unwrap().value;
    println!("z={z_value:?}");
}
