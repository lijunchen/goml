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
fn runtime_go_error_type_collision_executes() {
    let output = run_crasher("runtime_go_error_type_collision_user_symbol");

    assert_eq!(output, "7\n");
}

#[test]
fn user_go_error_local_tostring_impl_executes() {
    let output = run_crasher("user_go_error_local_tostring_impl");

    assert_eq!(output, "7\n");
}

#[test]
fn native_helper_function_name_collision_executes() {
    let output = run_crasher("native_helper_function_name_collision_user_symbol");

    assert_eq!(output, "2\n");
}

#[test]
fn hashmap_native_helper_function_name_collision_executes() {
    let output = run_crasher("hashmap_native_helper_function_name_collision_user_symbol");

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
fn extern_import_alias_function_name_collision_executes() {
    let output = run_crasher("extern_import_alias_function_name_collision");

    assert_eq!(output, "OK\nuser-strings\n");
}

#[test]
fn raw_extern_value_import_alias_function_collision_executes() {
    let output = run_crasher("raw_extern_value_import_alias_function_collision");

    assert_eq!(output, "user-strings\n");
}

#[test]
fn raw_extern_value_import_alias_type_collision_executes() {
    let output = run_crasher("raw_extern_value_import_alias_type_collision");

    assert_eq!(output, "7\n");
}

#[test]
fn qualified_extern_type_import_alias_collision_executes() {
    let output = run_crasher("qualified_extern_type_import_alias_collision");

    assert_eq!(output, "11\n");
}

#[test]
fn extern_type_function_name_collision_executes() {
    let output = run_crasher("extern_type_function_name_collision");

    assert_eq!(output, "13\n");
}

#[test]
fn extern_type_runtime_function_name_collision_executes() {
    let output = run_crasher("extern_type_runtime_function_name_collision");

    assert_eq!(output, "14\n");
}

#[test]
fn user_type_main_go_entry_collision_executes() {
    let output = run_crasher("user_type_main_go_entry_collision");

    assert_eq!(output, "18\n");
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

#[test]
fn extern_import_alias_enum_variant_collision_executes() {
    let output = run_crasher("extern_import_alias_enum_variant_collision");

    assert_eq!(output, "OK\nvariant\n");
}

#[test]
fn extern_wrapper_function_name_collision_executes() {
    let output = run_crasher("extern_wrapper_function_name_collision");

    assert_eq!(output, "ab\nuser-wrapper\n");
}

#[test]
fn extern_bridge_import_alias_collision_executes() {
    let output = run_crasher("extern_bridge_import_alias_collision");

    assert_eq!(output, "OK\n");
}

#[test]
fn raw_go_import_alias_rewrite_preserves_string_literal() {
    let output = run_crasher("raw_go_import_alias_rewrite_string_literal");

    assert_eq!(output, "strings.Builder\n");
}

#[test]
fn duplicate_go_import_default_alias_executes() {
    let output = run_crasher("duplicate_go_import_default_alias");

    assert_eq!(output, "0\n");
}
