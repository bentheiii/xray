use std::fs;
use string_interner::StringInterner;

use xray::evaluation_scope::XEvaluationScope;
use xray::runtime::RuntimeLimits;
use xray::std_compilation_scope;
use xray::xvalue::XValue;

fn test_script(script_number: usize) {
    let mut interner = StringInterner::default();
    let mut comp_scope = std_compilation_scope(&mut interner);
    let file_path = format!("test_scripts/{:0>3}.xr", script_number);
    let input = fs::read_to_string(&file_path).expect(&file_path);
    let limits = RuntimeLimits::default();
    let runtime = limits.to_runtime();
    let decls = comp_scope
        .feed_file(&input, &mut interner, runtime.clone())
        .map_err(|e| e.display(&interner, &input))
        .unwrap();

    let mut eval_scope = XEvaluationScope::root();
    eval_scope.add_from(&decls, runtime.clone()).unwrap();

    let main_fn = eval_scope
        .get_ud_func(interner.get_or_intern_static("main"))
        .expect(r#"function "main" not found"#);
    let main_output = &main_fn
        .to_function(&eval_scope)
        .eval_values(&[], &eval_scope, runtime)
        .unwrap()
        .value;
    if let XValue::Bool(true) = main_output {
    } else {
        panic!("main outputted {:?}, expected true", main_output)
    }
}

#[test]
fn test_script_001() {
    test_script(001);
}

#[test]
fn test_script_002() {
    test_script(002);
}

#[test]
fn test_script_003() {
    test_script(003);
}

#[test]
fn test_script_004() {
    test_script(004);
}

#[test]
fn test_script_005() {
    test_script(005);
}

#[test]
fn test_script_006() {
    test_script(006);
}

#[test]
fn test_script_007() {
    test_script(007);
}

#[test]
fn test_script_008() {
    test_script(008);
}

#[test]
fn test_script_009() {
    test_script(009);
}

#[test]
fn test_script_010() {
    test_script(010);
}

#[test]
fn test_script_011() {
    test_script(011);
}

#[test]
fn test_script_012() {
    test_script(012);
}

#[test]
fn test_script_013() {
    test_script(013);
}

#[test]
fn test_script_014() {
    test_script(014);
}

#[test]
fn test_script_015() {
    test_script(015);
}

#[test]
fn test_script_016() {
    test_script(016);
}

#[test]
fn test_script_017() {
    test_script(017);
}

#[test]
fn test_script_018() {
    test_script(018);
}

#[test]
fn test_script_019() {
    test_script(019);
}

#[test]
fn test_script_020() {
    test_script(020);
}

#[test]
fn test_script_021() {
    test_script(021);
}

#[test]
#[ignore]
fn test_script_022() {
    test_script(022);
}

#[test]
fn test_script_023() {
    test_script(023);
}

#[test]
fn test_script_024() {
    test_script(024);
}

#[test]
fn test_script_025() {
    test_script(025);
}

#[test]
fn test_script_026() {
    test_script(026);
}

#[test]
fn test_script_027() {
    test_script(027);
}

#[test]
fn test_script_028() {
    test_script(028);
}

#[test]
fn test_script_029() {
    test_script(029);
}

#[test]
fn test_script_030() {
    test_script(030);
}

#[test]
fn test_script_031() {
    test_script(031);
}

#[test]
fn test_script_032() {
    test_script(032);
}

#[test]
fn test_script_033() {
    test_script(033);
}

#[test]
fn test_script_034() {
    test_script(034);
}

#[test]
fn test_script_035() {
    test_script(035);
}

#[test]
fn test_script_036() {
    test_script(036);
}

#[test]
fn test_script_037() {
    test_script(037);
}

#[test]
fn test_script_038() {
    test_script(038);
}

#[test]
fn test_script_039() {
    test_script(039);
}

#[test]
fn test_script_040() {
    test_script(040);
}

#[test]
fn test_script_041() {
    test_script(041);
}

#[test]
fn test_script_042() {
    test_script(042);
}

#[test]
fn test_script_043() {
    test_script(043);
}

#[test]
fn test_script_044() {
    test_script(044);
}

#[test]
fn test_script_045() {
    test_script(045);
}

#[test]
fn test_script_046() {
    test_script(046);
}

#[test]
fn test_script_047() {
    test_script(047);
}

#[test]
fn test_script_048() {
    test_script(048);
}

#[test]
fn test_script_049() {
    test_script(049);
}

#[test]
fn test_script_050() {
    test_script(050);
}

#[test]
fn test_script_051() {
    test_script(051);
}

#[test]
fn test_script_052() {
    test_script(052);
}

#[test]
fn test_script_053() {
    test_script(053);
}

#[test]
fn test_script_054() {
    test_script(054);
}

#[test]
fn test_script_055() {
    test_script(055);
}

#[test]
fn test_script_056() {
    test_script(056);
}

#[test]
fn test_script_057() {
    test_script(057);
}
