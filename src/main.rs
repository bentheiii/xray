extern crate core;
extern crate pest;

use xray::compilation_scope::XCompilationScopeItem;
use xray::compile_err::ResolvedTracedCompilationError;
use xray::evaluation_scope::XEvaluationScope;

use xray::runtime::RuntimeLimits;
use xray::std_compilation_scope;

fn main() {
    let input = r###"
    let foo = () -> {1};
    let z = foo();
    "###;
    let mut root_scope = std_compilation_scope();

    let limits = RuntimeLimits {
        ..RuntimeLimits::default()
    };
    let runtime = limits.to_runtime();

    let decals = match root_scope.feed_file(input, runtime.clone()) {
        Ok(v) => v,
        Err(e @ ResolvedTracedCompilationError::Compilation(..)) => panic!("{}", e),
        Err(ResolvedTracedCompilationError::Syntax(s)) => panic!("{}", s),
    };
    println!("compiled!");

    let mut eval_scope = XEvaluationScope::root();
    let z_ident = root_scope.get_identifier("z");
    for decl in decals {
        eval_scope.add_from(&decl, runtime.clone()).unwrap();
    }
    println!("z={:?}", eval_scope.get_value(z_ident).unwrap().value);
    let z_static = root_scope.get(z_ident).unwrap();
    if let XCompilationScopeItem::Value(t) = z_static {
        println!("z: {:?}", root_scope.describe_type(t));
    } else {
        println!("z: {:?}", z_static);
    }
}
