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
fn function_tuple_type_collision_executes() {
    let output = run_crasher("function_tuple_type_collision");

    assert_eq!(output, "29\n");
}

#[test]
fn closure_apply_go_name_collision_executes() {
    let output = run_crasher("closure_apply_go_name_collision_user_symbol");

    assert_eq!(output, "6\n");
}

#[test]
fn function_closure_env_type_collision_executes() {
    let output = run_crasher("function_closure_env_type_collision");

    assert_eq!(output, "37\n");
}

#[test]
fn enum_variant_go_type_name_collision_executes() {
    let output = run_crasher("enum_variant_go_type_name_collision_user_symbol");

    assert_eq!(output, "7\n");
}

#[test]
fn enum_variant_same_name_as_enum_executes() {
    let output = run_crasher("enum_variant_same_name_as_enum");

    assert_eq!(output, "1\n");
}

#[test]
fn dyn_trait_go_type_name_collision_executes() {
    let output = run_crasher("dyn_trait_go_type_name_collision_user_symbol");

    assert_eq!(output, "ok1\n");
}

#[test]
fn dyn_trait_go_function_name_collision_executes() {
    let output = run_crasher("dyn_trait_go_function_name_collision_user_symbol");

    assert_eq!(output, "ok1\n");
}

#[test]
fn dyn_type_vtable_name_collision_executes() {
    let output = run_crasher("dyn_type_vtable_name_collision");

    assert_eq!(output, "7\n");
}

#[test]
fn dyn_wrap_generated_name_collision_executes() {
    let output = run_crasher("dyn_wrap_generated_name_collision");

    assert_eq!(output, "2\n10\n");
}

#[test]
fn trait_impl_generated_name_collision_executes() {
    let output = run_crasher("trait_impl_generated_name_collision");

    assert_eq!(output, "10\n20\n");
}

#[test]
fn ref_runtime_type_name_collision_executes() {
    let output = run_crasher("ref_runtime_type_name_collision_user_symbol");

    assert_eq!(output, "5\n");
}

#[test]
fn hashmap_runtime_type_name_collision_executes() {
    let output = run_crasher("hashmap_runtime_type_name_collision_user_symbol");

    assert_eq!(output, "9\n");
}

#[test]
fn runtime_json_escape_function_collision_executes() {
    let output = run_crasher("runtime_json_escape_function_collision_user_symbol");

    assert_eq!(output, "user:x\n");
}

#[test]
fn runtime_missing_function_collision_executes() {
    let output = run_crasher("runtime_missing_function_collision_user_symbol");

    assert_eq!(output, "user:x\n");
}

#[test]
fn hashmap_lookup_helper_function_name_collision_executes() {
    let output = run_crasher("hashmap_lookup_helper_function_name_collision_user_symbol");

    assert_eq!(output, "3\n");
}

#[test]
fn import_alias_function_name_collision_executes() {
    let output = run_crasher("import_alias_function_name_collision");

    assert_eq!(output, "1\nuser-fmt\n");
}

#[test]
fn import_alias_type_name_collision_executes() {
    let output = run_crasher("import_alias_type_name_collision");

    assert_eq!(output, "2\n");
}

#[test]
fn user_type_main_go_entry_collision_executes() {
    let output = run_crasher("user_type_main_go_entry_collision");

    assert_eq!(output, "18\n");
}

#[test]
fn go_init_function_name_collision_executes() {
    let output = run_crasher("go_init_function_name_collision");

    assert_eq!(output, "5\n");
}

#[test]
fn enum_variant_runtime_function_collision_executes() {
    let output = run_crasher("enum_variant_runtime_function_collision");

    assert_eq!(output, "22\n");
}

#[test]
fn enum_variant_tuple_type_collision_executes() {
    let output = run_crasher("enum_variant_tuple_type_collision");

    assert_eq!(output, "23\n");
}
