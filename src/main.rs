extern crate core;
extern crate pest;

use std::io::stdout;
use xray::_compilation_scope::XCompilationScopeItem;
use xray::compile_err::ResolvedTracedCompilationError;
use xray::evaluation_scope::RootEvaluationScope;

use xray::runtime::RuntimeLimits;
use xray::std_compilation_scope;

fn main() {
    let input = r###"
    let t = 4;
    fn a(i:int, j: int) -> int{
        fn s()->int{
            t+j
        }
        s()
    }

    fn b(i:int)->int{
        a(i, 1)
    }

    let z = b(3);
    "###;
    /*

     */
    /*

    fn my_range(start: int, stop: int, step:int)->Sequence<int> {
        fn range_helper(ret: Stack<int>, i:int)->Stack<int> {
            (i >= stop).if(
                ret,
                range_helper(ret.push(i), i + step)
            )
        }
        range_helper(stack(), start).to_array()
    }


    fn main()->Sequence<int> {
        my_range(1,20,3)
    }

    let z = main();
     */
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
    let z_value =  &eval_scope.get_value("z").unwrap().clone().unwrap().value;
    println!("z={:?}", z_value);
}
