extern crate core;
extern crate pest;

use std::io::stdout;
use xray::compilation_scope::XCompilationScopeItem;
use xray::compile_err::ResolvedTracedCompilationError;
use xray::evaluation_scope::RootEvaluationScope;

use xray::runtime::RuntimeLimits;
use xray::std_compilation_scope;

fn main() {
    let input = r###"
    fn a(x: int) -> int {
        fn b(y: int) -> int {
            if (y < -10,
                error('aaa'),
                a(y-1))
        }
        if (x <= 0,
            x,
            b(x))
    }

    let z = a(2);
    "###;
    let limits = RuntimeLimits {
        ..RuntimeLimits::default()
    };
    let runtime = limits.to_runtime(stdout());

    let mut root_scope = std_compilation_scope();

    match root_scope.feed_file(input) {
        Ok(v) => v,
        Err(e @ ResolvedTracedCompilationError::Compilation(..)) => panic!("{}", e),
        Err(ResolvedTracedCompilationError::Syntax(s)) => panic!("{}", s),
    };
    println!("compiled!");

    let mut eval_scope = RootEvaluationScope::from_compilation_scope(&root_scope, runtime).unwrap();
    println!("z={:?}", eval_scope.get_value("z").unwrap().unwrap().value);
    let z_static = root_scope.get("z").unwrap();
    if let XCompilationScopeItem::Value(_, t) = z_static {
        println!("z: {:?}", root_scope.describe_type(t));
    } else {
        println!("z: {:?}", z_static);
    }
}
