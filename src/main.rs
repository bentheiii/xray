extern crate pest;




use xray::compilation_scope::XCompilationScopeItem;
use xray::evaluation_scope::XEvaluationScope;

use xray::runtime::RuntimeLimits;
use xray::std_compilation_scope;



fn main() {
    let input = r###"
    let z = (null() && error('')) == null();
    "###;
    let mut root_scope = std_compilation_scope();

    let limits = RuntimeLimits {
        ..RuntimeLimits::default()
    };
    let runtime = limits.to_runtime();

    let decals = root_scope
        .feed_file(input, runtime.clone())
        .map_err(|e| format!("{}", e)).unwrap();
    println!("compiled!");

    let mut eval_scope = XEvaluationScope::root();
    let z_ident = root_scope.get_identifier("z");
    eval_scope.add_from(&decals, runtime).unwrap();
    println!(
        "z={:?}",
        eval_scope
            .get(z_ident)
            .unwrap()
            .value
    );
    let z_static = root_scope.get(z_ident).unwrap();
    if let XCompilationScopeItem::Value(t) = z_static {
        println!("z: {:?}", root_scope.describe_type(t));
    } else {
        println!("z: {:?}", z_static);
    }
}
