extern crate core;
extern crate pest;

use std::io::stdout;
use xray::compile_err::ResolvedTracedCompilationError;
use xray::evaluation_scope::RootEvaluationScope;

use xray::runtime::RuntimeLimits;
use xray::std_compilation_scope;

fn main() {
    let input = r###"
    fn foo(x: int, y: Optional<int>)->int{1}
    fn foo(x: int, y: Optional<str>)->int{2}
    fn foo(x: float, y: Optional<int>)->int{3}
    let n = null();
    let z = foo::<$, Optional<int>>(1, n);
    "###;
    /*

    fn d(i: int)->(int)->(int){
        fn ret(x:int)->int{
            x+i
        }
        ret
    }
    let f = d(6);
    let z = f(9);
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

    let eval_scope = RootEvaluationScope::from_compilation_scope(&root_scope, runtime).unwrap();
    let z_value = &eval_scope.get_value("z").unwrap().clone().unwrap().value;
    println!("z={z_value:?}");
}
