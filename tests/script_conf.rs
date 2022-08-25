use std::io::stdout;
use either::{Either};
use serde::Deserialize;
use xray::compile_err::ResolvedTracedCompilationError;
use xray::evaluation_scope::RootEvaluationScope;
use xray::runtime::RuntimeLimits;
use xray::std_compilation_scope;
use xray::xvalue::XValue;

#[derive(Deserialize, Default)]
pub struct ScriptConfig {
    #[serde(default)]
    expected_stdout: Option<String>,
}

impl ScriptConfig {
    pub fn run(&self, input: &str) {
        let limits = RuntimeLimits::default();
        let output = if self.expected_stdout.is_some() {
            Either::Left(Vec::<u8>::new())
        } else {
            Either::Right(stdout())
        };
        let runtime = limits.to_runtime(output);
        let mut comp_scope = std_compilation_scope(runtime);

        match comp_scope.feed_file(&input) {
            Ok(v) => v,
            Err(e @ ResolvedTracedCompilationError::Compilation(..)) => panic!("{}", e),
            Err(ResolvedTracedCompilationError::Syntax(s)) => panic!("{}", s),
        };

        let eval_scope = RootEvaluationScope::from_compilation_scope(&comp_scope).unwrap();

        let main_fn = eval_scope
            .get_user_defined_function("main")
            .expect(r#"function "main" found more than once"#)
            .expect(r#"function "main" not found"#);
        let main_output = &eval_scope.eval(main_fn, &[]).unwrap().value;
        if let XValue::Bool(true) = main_output {} else {
            panic!("main outputted {:?}, expected true", main_output)
        }

        if let Some(expected_output) = &self.expected_stdout{
            let actual_stdout = comp_scope.runtime.borrow().stdout.as_ref().unwrap_left().clone();
            assert_eq!(expected_output, &String::from_utf8(actual_stdout).unwrap())
        }
    }
}

