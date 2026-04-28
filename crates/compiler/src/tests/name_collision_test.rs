use std::path::PathBuf;

use crate::pipeline::pipeline::compile_single_file;

fn run_crasher(name: &str) -> String {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers")
        .join(name)
        .join("main.gom");
    let src = std::fs::read_to_string(&path).unwrap_or_else(|err| {
        panic!("failed to read {}: {err}", path.display());
    });
    let compilation = compile_single_file(&path, &src).unwrap_or_else(|err| {
        panic!("compilation failed for {}: {:?}", path.display(), err);
    });
    let go = compilation.go.to_pretty(&compilation.goenv, 120);
    super::execute_go_source(&go, &path.to_string_lossy()).unwrap()
}

#[test]
fn mono_function_name_collision_executes() {
    let output = run_crasher("mono_name_collision_user_symbol");

    assert_eq!(output, "10\n11\n");
}

#[test]
fn mono_type_name_collision_executes() {
    let output = run_crasher("mono_type_name_collision_user_symbol");

    assert_eq!(output, "30\n");
}

#[test]
fn tuple_go_type_name_collision_executes() {
    let output = run_crasher("tuple_go_type_name_collision_user_symbol");

    assert_eq!(output, "10\n");
}

#[test]
fn closure_apply_go_name_collision_executes() {
    let output = run_crasher("closure_apply_go_name_collision_user_symbol");

    assert_eq!(output, "6\n");
}

#[test]
fn enum_variant_go_type_name_collision_executes() {
    let output = run_crasher("enum_variant_go_type_name_collision_user_symbol");

    assert_eq!(output, "7\n");
}
