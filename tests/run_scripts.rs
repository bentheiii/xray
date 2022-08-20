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
