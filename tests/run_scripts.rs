mod utils;

use glob::glob;
use itertools::Itertools;
use std::fs;

use crate::utils::capture_writer::CaptureWriter;
use crate::utils::memory_writer::MemoryWriter;
use either::Either;
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
            Either::Left(MemoryWriter::new(CaptureWriter))
        } else {
            Either::Right(CaptureWriter)
        };
        let mut comp_scope = std_compilation_scope();

        match comp_scope.feed_file(input) {
            Ok(v) => v,
            Err(e @ ResolvedTracedCompilationError::Compilation(..)) => panic!("{}", e),
            Err(ResolvedTracedCompilationError::Syntax(s)) => panic!("{}", s),
        };

        let runtime = limits.to_runtime(output);
        let eval_scope =
            RootEvaluationScope::from_compilation_scope(&comp_scope, runtime.clone()).unwrap();

        let main_fn = eval_scope
            .get_user_defined_function("main")
            .expect(r#"function "main" found more than once"#)
            .expect(r#"function "main" not found"#);
        let main_output = &eval_scope.run_function(main_fn, vec![]).unwrap().unwrap_value().unwrap().value;
        if !matches!(main_output, XValue::Bool(true)) {
            panic!("main outputted {:?}, expected true", main_output)
        }

        if let Some(expected_output) = &self.expected_stdout {
            let actual_stdout = runtime
                .borrow()
                .stdout
                .as_ref()
                .unwrap_left()
                .memory
                .clone();
            assert_eq!(expected_output, &String::from_utf8(actual_stdout).unwrap())
        }
    }
}

fn test_script(script_number: usize) {
    let file_pattern = format!("test_scripts/{script_number:0>3}_*.xr");
    let file_path = glob(&file_pattern)
        .unwrap()
        .exactly_one()
        .map_err(|e| {
            format!(
                "multiple files matched {file_pattern}: {}",
                e.map(|b| format!("{b:?}")).join(", ")
            )
        })
        .unwrap()
        .unwrap();
    let input = fs::read_to_string(&file_path).unwrap_or_else(|_| { panic!("{}", file_path.to_str().unwrap().to_string()) });

    let config: ScriptConfig =
        match fs::read_to_string(format!("test_scripts/{script_number:0>3}.toml")) {
            Ok(content) => toml::from_str(&content).unwrap(),
            Err(e) => match e.kind() {
                std::io::ErrorKind::NotFound => Default::default(),
                _ => panic!("{:?}", e),
            },
        };

    config.run(&input);
}

#[test]
fn test_script_001() {
    test_script(1);
}

#[test]
fn test_script_002() {
    test_script(2);
}

#[test]
fn test_script_003() {
    test_script(3);
}

#[test]
fn test_script_004() {
    test_script(4);
}

#[test]
fn test_script_005() {
    test_script(5);
}

#[test]
fn test_script_006() {
    test_script(6);
}

#[test]
fn test_script_007() {
    test_script(7);
}

#[test]
fn test_script_008() {
    test_script(8);
}

#[test]
fn test_script_009() {
    test_script(9);
}

#[test]
fn test_script_010() {
    test_script(10);
}

#[test]
fn test_script_011() {
    test_script(11);
}

#[test]
fn test_script_012() {
    test_script(12);
}

#[test]
fn test_script_013() {
    test_script(13);
}

#[test]
fn test_script_014() {
    test_script(14);
}

#[test]
fn test_script_015() {
    test_script(15);
}

#[test]
fn test_script_016() {
    test_script(16);
}

#[test]
fn test_script_017() {
    test_script(17);
}

#[test]
fn test_script_018() {
    test_script(18);
}

#[test]
fn test_script_019() {
    test_script(19);
}

#[test]
fn test_script_020() {
    test_script(20);
}

#[test]
fn test_script_021() {
    test_script(21);
}

#[test]
#[ignore]
fn test_script_022() {
    test_script(22);
}

#[test]
fn test_script_023() {
    test_script(23);
}

#[test]
fn test_script_024() {
    test_script(24);
}

#[test]
fn test_script_025() {
    test_script(25);
}

#[test]
fn test_script_026() {
    test_script(26);
}

#[test]
fn test_script_027() {
    test_script(27);
}

#[test]
fn test_script_028() {
    test_script(28);
}

#[test]
fn test_script_029() {
    test_script(29);
}

#[test]
fn test_script_030() {
    test_script(30);
}

#[test]
fn test_script_031() {
    test_script(31);
}

#[test]
fn test_script_032() {
    test_script(32);
}

#[test]
fn test_script_033() {
    test_script(33);
}

#[test]
fn test_script_034() {
    test_script(34);
}

#[test]
fn test_script_035() {
    test_script(35);
}

#[test]
fn test_script_036() {
    test_script(36);
}

#[test]
fn test_script_037() {
    test_script(37);
}

#[test]
fn test_script_038() {
    test_script(38);
}

#[test]
fn test_script_039() {
    test_script(39);
}

#[test]
fn test_script_040() {
    test_script(40);
}

#[test]
fn test_script_041() {
    test_script(41);
}

#[test]
fn test_script_042() {
    test_script(42);
}

#[test]
fn test_script_043() {
    test_script(43);
}

#[test]
fn test_script_044() {
    test_script(44);
}

#[test]
fn test_script_045() {
    test_script(45);
}

#[test]
fn test_script_046() {
    test_script(46);
}

#[test]
fn test_script_047() {
    test_script(47);
}

#[test]
fn test_script_048() {
    test_script(48);
}

#[test]
fn test_script_049() {
    test_script(49);
}

#[test]
fn test_script_050() {
    test_script(50);
}

#[test]
fn test_script_051() {
    test_script(51);
}

#[test]
fn test_script_052() {
    test_script(52);
}

#[test]
fn test_script_053() {
    test_script(53);
}

#[test]
fn test_script_054() {
    test_script(54);
}

#[test]
fn test_script_055() {
    test_script(55);
}

#[test]
fn test_script_056() {
    test_script(56);
}

#[test]
fn test_script_057() {
    test_script(57);
}

#[test]
fn test_script_058() {
    test_script(58);
}

#[test]
fn test_script_059() {
    test_script(59);
}

#[test]
fn test_script_060() {
    test_script(60);
}

#[test]
fn test_script_061() {
    test_script(61);
}

#[test]
fn test_script_062() {
    test_script(62);
}

#[test]
fn test_script_063() {
    test_script(63);
}

#[test]
fn test_script_064() {
    test_script(64);
}

#[test]
fn test_script_065() {
    test_script(65);
}

#[test]
fn test_script_066() {
    test_script(66);
}

#[test]
fn test_script_067() {
    test_script(67);
}

#[test]
fn test_script_068() {
    test_script(68);
}

#[test]
fn test_script_069() {
    test_script(69);
}

#[test]
fn test_script_070() {
    test_script(70);
}

#[test]
fn test_script_071() {
    test_script(71);
}

#[test]
fn test_script_072() {
    test_script(72);
}

#[test]
fn test_script_073() {
    test_script(73);
}

#[test]
fn test_script_074() {
    test_script(74);
}

#[test]
fn test_script_075() {
    test_script(75);
}

#[test]
fn test_script_076() {
    test_script(76);
}

#[test]
fn test_script_077() {
    test_script(77);
}

#[test]
fn test_script_078() {
    test_script(78);
}

#[test]
fn test_script_079() {
    test_script(79);
}

#[test]
fn test_script_080() {
    test_script(80);
}

#[test]
fn test_script_081() {
    test_script(81);
}

#[test]
fn test_script_082() {
    test_script(82);
}

#[test]
fn test_script_083() {
    test_script(83);
}
#[test]
fn test_script_084() {
    test_script(84);
}

#[test]
fn test_script_085() {
    test_script(85);
}

#[test]
fn test_script_086() {
    test_script(86);
}

#[test]
fn test_script_087() {
    test_script(87);
}