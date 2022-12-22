mod utils;

use glob::glob;
use itertools::Itertools;
use std::fs;
use std::time::Duration;

use crate::utils::capture_writer::CaptureWriter;
use crate::utils::memory_writer::MemoryWriter;
use either::Either;
use regex::Regex;
use serde::Deserialize;
use xray::root_runtime_scope::RootEvaluationScope;
use xray::runtime::RuntimeLimits;
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
}

impl RuntimeLimitsConfig {
    fn to_runtime_limits(&self) -> RuntimeLimits {
        RuntimeLimits {
            size_limit: self.size_limit,
            depth_limit: self.depth_limit,
            recursion_limit: self.recursion_limit,
            ud_call_limit: self.ud_call_limit,
            maximum_search: self.maximum_search,
            time_limit: self.time_limit,
        }
    }
}

#[derive(Deserialize, Default)]
#[serde(default)]
pub struct ScriptConfig {
    expected_stdout: Option<String>,
    expected_compilation_error: Option<String>,
    expected_violation: Option<String>,
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
            (Ok(_), None) => {}
            (Err(e), None) => panic!("{e:?}"),
            (Ok(_), Some(_)) => panic!("expected compilation error"),
            (Err(e), Some(pat)) => {
                let pat = Regex::new(pat).unwrap();
                if pat.is_match(&format!("{e}")) {
                    return;
                }
                panic!("expected error that matches {pat:?}, got \"{e}\"")
            }
        }

        let runtime = limits.to_runtime(output);

        let eval_scope_results =
            RootEvaluationScope::from_compilation_scope(&comp_scope, runtime.clone());

        let eval_scope = match (eval_scope_results, &self.expected_violation) {
            (Ok(s), None) => s,
            (Ok(..), Some(v)) => panic!("expected violation error {v}"),
            (Err(e), None) => panic!("{e:?}"),
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
            panic!("main returned {:?}, expected true", main_output)
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
    let input = fs::read_to_string(&file_path)
        .unwrap_or_else(|_| panic!("{}", file_path.to_str().unwrap().to_string()));

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

#[test]
fn test_script_088() {
    test_script(88);
}

#[test]
fn test_script_089() {
    test_script(89);
}

#[test]
fn test_script_090() {
    test_script(90);
}

#[test]
fn test_script_091() {
    test_script(91);
}

#[test]
fn test_script_092() {
    test_script(92);
}

#[test]
fn test_script_093() {
    test_script(93);
}

#[test]
fn test_script_094() {
    test_script(94);
}

#[test]
fn test_script_095() {
    test_script(95);
}

#[test]
fn test_script_096() {
    test_script(96);
}

#[test]
fn test_script_097() {
    test_script(97);
}

#[test]
fn test_script_098() {
    test_script(98);
}

#[test]
fn test_script_099() {
    test_script(99);
}

#[test]
fn test_script_100() {
    test_script(100);
}

#[test]
fn test_script_101() {
    test_script(101);
}

#[test]
fn test_script_102() {
    test_script(102);
}

#[test]
fn test_script_103() {
    test_script(103);
}

#[test]
fn test_script_104() {
    test_script(104);
}

#[test]
fn test_script_105() {
    test_script(105);
}

#[test]
fn test_script_106() {
    test_script(106);
}

#[test]
fn test_script_107() {
    test_script(107);
}

#[test]
fn test_script_108() {
    test_script(108);
}

#[test]
fn test_script_109() {
    test_script(109);
}

#[test]
fn test_script_110() {
    test_script(110);
}

#[test]
fn test_script_111() {
    test_script(111);
}

#[test]
fn test_script_112() {
    test_script(112);
}

#[test]
fn test_script_113() {
    test_script(113);
}

#[test]
fn test_script_114() {
    test_script(114);
}

#[test]
fn test_script_115() {
    test_script(115);
}

#[test]
fn test_script_116() {
    test_script(116);
}

#[test]
fn test_script_117() {
    test_script(117);
}

#[test]
fn test_script_118() {
    test_script(118);
}

#[test]
fn test_script_119() {
    test_script(119);
}

#[test]
fn test_script_120() {
    test_script(120);
}

#[test]
fn test_script_121() {
    test_script(121);
}

#[test]
fn test_script_122() {
    test_script(122);
}

#[test]
fn test_script_123() {
    test_script(123);
}

#[test]
fn test_script_124() {
    test_script(124);
}

#[test]
fn test_script_125() {
    test_script(125);
}

#[test]
fn test_script_126() {
    test_script(126);
}

#[test]
fn test_script_127() {
    test_script(127);
}

#[test]
fn test_script_128() {
    test_script(128);
}

#[test]
fn test_script_129() {
    test_script(129);
}

#[test]
fn test_script_130() {
    test_script(130);
}

#[test]
fn test_script_131() {
    test_script(131);
}

#[test]
fn test_script_132() {
    test_script(132);
}

#[test]
fn test_script_133() {
    test_script(133);
}

#[test]
fn test_script_134() {
    test_script(134);
}

#[test]
fn test_script_135() {
    test_script(135);
}

#[test]
fn test_script_136() {
    test_script(136);
}

#[test]
fn test_script_137() {
    test_script(137);
}

#[test]
fn test_script_138() {
    test_script(138);
}

#[test]
fn test_script_139() {
    test_script(139);
}

#[test]
fn test_script_140() {
    test_script(140);
}

#[test]
fn test_script_141() {
    test_script(141);
}

#[test]
fn test_script_142() {
    test_script(142);
}

#[test]
fn test_script_143() {
    test_script(143);
}

#[test]
fn test_script_144() {
    test_script(144);
}

#[test]
fn test_script_145() {
    test_script(145);
}

#[test]
fn test_script_146() {
    test_script(146);
}

#[test]
fn test_script_147() {
    test_script(147);
}

#[test]
fn test_script_148() {
    test_script(148);
}

#[test]
fn test_script_149() {
    test_script(149);
}

#[test]
fn test_script_150() {
    test_script(150);
}

#[test]
fn test_script_151() {
    test_script(151);
}

#[test]
fn test_script_152() {
    test_script(152);
}

#[test]
fn test_script_153() {
    test_script(153);
}

#[test]
fn test_script_154() {
    test_script(154);
}

#[test]
fn test_script_155() {
    test_script(155);
}

#[test]
fn test_script_156() {
    test_script(156);
}

#[test]
fn test_script_157() {
    test_script(157);
}

#[test]
fn test_script_158() {
    test_script(158);
}

#[test]
fn test_script_159() {
    test_script(159);
}

#[test]
fn test_script_160() {
    test_script(160);
}

#[test]
fn test_script_161() {
    test_script(161);
}

#[test]
fn test_script_162() {
    test_script(162);
}

#[test]
fn test_script_163() {
    test_script(163);
}

#[test]
fn test_script_164() {
    test_script(164);
}