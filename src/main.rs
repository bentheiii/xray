extern crate core;
extern crate pest;

use std::io::stdout;
use xray::compile_err::ResolvedTracedCompilationError;
use xray::evaluation_scope::RootEvaluationScope;

use xray::runtime::RuntimeLimits;
use xray::std_compilation_scope;

fn main() {
    let input = r###"
    struct A(x: int)
    let z = is_error(A(error("")));
    "###;
    let limits = RuntimeLimits {
        ..RuntimeLimits::default()
    };
    let runtime = limits.to_runtime(stdout());

    let mut root_scope = std_compilation_scope();

    match root_scope.feed_file(input) {
        Ok(v) => v,
        Err(b) => match b.as_ref() {
            e @ ResolvedTracedCompilationError::Compilation(..) => panic!("{}", e),
            ResolvedTracedCompilationError::Syntax(s) => panic!("{}", s),
        },
    };
    println!("compiled!");

    let eval_scope = RootEvaluationScope::from_compilation_scope(&root_scope, runtime).unwrap();
    let z_value = &eval_scope.get_value("z").unwrap().clone().unwrap().value;
    println!("z={z_value:?}");
}
