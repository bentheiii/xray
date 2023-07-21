mod utils;

use glob::glob;
use itertools::Itertools;
use std::borrow::Borrow;
use std::collections::HashSet;
use std::fs;
use std::time::Duration;
use utils::time_provider::mk_time_provider;

use crate::utils::capture_writer::CaptureWriter;
use crate::utils::memory_writer::MemoryWriter;
use either::Either;
use rand::rngs::StdRng;
use regex::Regex;
use serde::Deserialize;
use stdext::function_name;
use xray::builtin::builtin_permissions;
use xray::permissions::PermissionSet;
use xray::root_runtime_scope::RootEvaluationScope;
use xray::runtime::{RTCell, RuntimeLimits};
use xray::std_compilation_scope;
use xray::xvalue::XValue;

#[derive(Deserialize, Default)]
#[serde(default)]
pub struct RuntimeLimitsConfig {
    size_limit: Option<usize>,
    depth_limit: Option<usize>,
    recursion_limit: Option<usize>,
    ud_call_limit: Option<usize>,
    maximum_search: Option<usize>,
    time_limit: Option<Duration>,
    forbidden_permissions: Vec<String>,
    allowed_permissions: Vec<String>,
}

impl RuntimeLimitsConfig {
    fn to_runtime_limits(&self) -> RuntimeLimits {
        let forbidden: HashSet<_> = self
            .forbidden_permissions
            .iter()
            .map(|s| s.borrow())
            .collect();
        let allowed: HashSet<_> = self
            .allowed_permissions
            .iter()
            .map(|s| s.borrow())
            .collect();
        let mut permission_set = PermissionSet::default();
        if allowed.contains("regex") {
            permission_set.allow(&builtin_permissions::REGEX)
        }
        if allowed.contains("sleep") {
            permission_set.allow(&builtin_permissions::SLEEP)
        }

        if forbidden.contains("print_debug") {
            permission_set.forbid(&builtin_permissions::PRINT_DEBUG)
        }
        if forbidden.contains("print") {
            permission_set.forbid(&builtin_permissions::PRINT)
        }
        if forbidden.contains("now") {
            permission_set.forbid(&builtin_permissions::NOW)
        }
        RuntimeLimits {
            size_limit: self.size_limit,
            depth_limit: self.depth_limit,
            recursion_limit: self.recursion_limit,
            ud_call_limit: self.ud_call_limit,
            maximum_search: self.maximum_search,
            time_limit: self.time_limit,
            permissions: permission_set,
        }
    }
}

#[derive(Deserialize, Default)]
#[serde(default)]
pub struct ScriptConfig {
    expected_stdout: Option<String>,
    expected_compilation_error: Option<String>,
    expected_violation: Option<String>,
    now: Option<f64>,
    limits: RuntimeLimitsConfig,
}

impl ScriptConfig {
    fn run(&self, input: &str) {
        let limits = self.limits.to_runtime_limits();
        let output = if self.expected_stdout.is_some() {
            Either::Left(MemoryWriter::new(CaptureWriter))
        } else {
            Either::Right(CaptureWriter)
        };
        let mut comp_scope = std_compilation_scope();

        match (
            comp_scope.feed_file(input),
            &self.expected_compilation_error,
        ) {
            (r, None) => r.unwrap(),
            (Ok(_), Some(_)) => panic!("expected compilation error"),
            (Err(e), Some(pat)) => {
                let pat = Regex::new(pat).unwrap();
                if pat.is_match(&format!("{e}")) {
                    return;
                }
                panic!("expected error that matches {pat:?}, got \"{e}\"")
            }
        }

        let time_provider = mk_time_provider(self.now);

        let runtime: RTCell<_, StdRng, _> = limits.to_runtime(output, time_provider);

        let eval_scope_results =
            RootEvaluationScope::from_compilation_scope(&comp_scope, runtime.clone());

        let eval_scope = match (eval_scope_results, &self.expected_violation) {
            (r, None) => r.unwrap(),
            (Ok(..), Some(v)) => panic!("expected violation error {v}"),
            (Err(e), Some(expected)) => {
                let description = format!("{e:?}");
                if &description == expected {
                    return;
                }
                panic!("expected runtime violation {expected}, got {e:?}")
            }
        };

        let main_fn = eval_scope.get_user_defined_function("main").unwrap();
        let main_output = &eval_scope
            .run_function(main_fn, vec![])
            .unwrap()
            .unwrap_value()
            .unwrap()
            .value;
        if !matches!(main_output, XValue::Bool(true)) {
            panic!("main returned {main_output:?}, expected true")
        }

        if let Some(expected_output) = &self.expected_stdout {
            let output = &runtime.stats.borrow().stdout;
            let actual_stdout = output.as_ref().unwrap_left().memory.clone();
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
    let input = fs::read_to_string(&file_path)
        .unwrap_or_else(|_| panic!("{}", file_path.to_str().unwrap().to_string()));

    let config: ScriptConfig =
        match fs::read_to_string(format!("test_scripts/{script_number:0>3}.toml")) {
            Ok(content) => toml::from_str(&content).unwrap(),
            Err(e) => match e.kind() {
                std::io::ErrorKind::NotFound => Default::default(),
                _ => panic!("{e:?}"),
            },
        };

    config.run(&input);
}

fn run_script_from_name(fn_name: &str) {
    let script_number = fn_name[fn_name.len() - 3..].parse().unwrap();
    test_script(script_number)
}

#[test]
fn test_script_001() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_002() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_003() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_004() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_005() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_006() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_007() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_008() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_009() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_010() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_011() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_012() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_013() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_014() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_015() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_016() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_017() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_018() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_019() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_020() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_021() {
    run_script_from_name(function_name!());
}

#[test]
#[ignore]
fn test_script_022() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_023() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_024() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_025() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_026() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_027() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_028() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_029() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_030() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_031() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_032() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_033() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_034() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_035() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_036() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_037() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_038() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_039() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_040() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_041() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_042() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_043() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_044() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_045() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_046() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_047() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_048() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_049() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_050() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_051() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_052() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_053() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_054() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_055() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_056() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_057() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_058() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_059() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_060() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_061() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_062() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_063() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_064() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_065() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_066() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_067() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_068() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_069() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_070() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_071() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_072() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_073() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_074() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_075() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_076() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_077() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_078() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_079() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_080() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_081() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_082() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_083() {
    run_script_from_name(function_name!());
}
#[test]
fn test_script_084() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_085() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_086() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_087() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_088() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_089() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_090() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_091() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_092() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_093() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_094() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_095() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_096() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_097() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_098() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_099() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_100() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_101() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_102() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_103() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_104() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_105() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_106() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_107() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_108() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_109() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_110() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_111() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_112() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_113() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_114() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_115() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_116() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_117() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_118() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_119() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_120() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_121() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_122() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_123() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_124() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_125() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_126() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_127() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_128() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_129() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_130() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_131() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_132() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_133() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_134() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_135() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_136() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_137() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_138() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_139() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_140() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_141() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_142() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_143() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_144() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_145() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_146() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_147() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_148() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_149() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_150() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_151() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_152() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_153() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_154() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_155() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_156() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_157() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_158() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_159() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_160() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_161() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_162() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_163() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_164() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_165() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_166() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_167() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_168() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_169() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_170() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_171() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_172() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_173() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_174() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_175() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_176() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_177() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_178() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_179() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_180() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_181() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_182() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_183() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_184() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_185() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_186() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_187() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_188() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_189() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_190() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_191() {
    run_script_from_name(function_name!());
}

#[test]
fn test_script_192() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_193() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_194() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_195() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_196() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_197() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_198() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_199() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_200() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_201() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_202() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_203() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_204() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_205() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_206() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_207() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_208() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_209() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_210() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_211() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_212() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_213() {
    run_script_from_name(function_name!())
}
#[test]
fn test_script_214() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_215() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_216() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_217() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_218() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_219() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_220() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_221() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_222() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_223() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_224() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_225() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_226() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_227() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_228() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_229() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_230() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_231() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_232() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_233() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_234() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_235() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_236() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_237() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_238() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_239() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_240() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_241() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_242() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_243() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_244() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_245() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_246() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_247() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_248() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_249() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_250() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_251() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_252() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_253() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_254() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_255() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_256() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_257() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_258() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_259() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_260() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_261() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_262() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_263() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_264() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_265() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_266() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_267() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_268() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_269() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_270() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_271() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_272() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_273() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_274() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_275() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_276() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_277() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_278() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_279() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_280() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_281() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_282() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_283() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_284() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_285() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_286() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_287() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_288() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_289() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_290() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_291() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_292() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_293() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_294() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_295() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_296() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_297() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_298() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_299() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_300() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_301() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_302() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_303() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_304() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_305() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_306() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_307() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_308() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_309() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_310() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_311() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_312() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_313() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_314() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_315() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_316() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_317() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_318() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_319() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_320() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_321() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_322() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_323() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_324() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_325() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_326() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_327() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_328() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_329() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_330() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_331() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_332() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_333() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_334() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_335() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_336() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_337() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_338() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_339() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_340() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_341() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_342() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_343() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_344() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_345() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_346() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_347() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_348() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_349() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_350() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_351() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_352() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_353() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_354() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_355() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_356() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_357() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_358() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_359() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_360() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_361() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_362() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_363() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_364() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_365() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_366() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_367() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_368() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_369() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_370() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_371() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_372() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_373() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_374() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_375() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_376() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_377() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_378() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_379() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_380() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_381() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_382() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_383() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_384() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_385() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_386() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_387() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_388() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_389() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_390() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_391() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_392() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_393() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_394() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_395() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_396() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_397() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_398() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_399() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_400() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_401() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_402() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_403() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_404() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_405() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_406() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_407() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_408() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_409() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_410() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_411() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_412() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_413() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_414() {
    run_script_from_name(function_name!())
}

#[test]
fn test_script_415() {
    run_script_from_name(function_name!())
}