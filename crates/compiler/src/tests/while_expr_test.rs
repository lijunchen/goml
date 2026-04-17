use std::path::PathBuf;

use crate::pipeline::pipeline::compile_single_file;

fn compile_single_file_go(path: PathBuf) -> String {
    let src = std::fs::read_to_string(&path).unwrap_or_else(|err| {
        panic!("failed to read {}: {err}", path.display());
    });
    let compilation = compile_single_file(&path, &src).unwrap_or_else(|err| {
        panic!("compilation failed for {}: {:?}", path.display(), err);
    });
    compilation.go.to_pretty(&compilation.goenv, 120)
}

fn compile_src_go(name: &str, src: &str) -> String {
    let path = PathBuf::from(name);
    let compilation = compile_single_file(&path, src).unwrap_or_else(|err| {
        panic!("compilation failed for {}: {:?}", path.display(), err);
    });
    compilation.go.to_pretty(&compilation.goenv, 120)
}

#[test]
fn all_exit_match_while_condition_compiles_in_single_file_mode() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/while_condition_all_exit_match/main.gom");

    let go = compile_single_file_go(path);

    assert!(go.contains("func main()"), "{go}");
    assert!(go.contains("for {"), "{go}");
}

#[test]
fn call_wrapped_all_exit_while_condition_compiles_in_single_file_mode() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/while_condition_non_bool_break_continue_arg/main.gom");

    let go = compile_single_file_go(path);

    assert!(go.contains("func main()"), "{go}");
    assert!(go.contains("for {"), "{go}");
    assert!(go.contains("if true {"), "{go}");
}

#[test]
fn struct_wrapped_all_exit_match_while_condition_compiles_in_single_file_mode() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/while_condition_all_exit_match_wrapped_struct/main.gom");

    let go = compile_single_file_go(path);

    assert!(go.contains("func main()"), "{go}");
    assert!(go.contains("for {"), "{go}");
    assert!(go.contains("switch"), "{go}");
}

#[test]
fn field_wrapped_all_exit_match_while_condition_compiles_in_single_file_mode() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/while_condition_all_exit_match_wrapped_field_access/main.gom");

    let go = compile_single_file_go(path);

    assert!(go.contains("func main()"), "{go}");
    assert!(go.contains("for {"), "{go}");
    assert!(go.contains("switch"), "{go}");
}

#[test]
fn tuple_projection_wrapped_all_exit_match_while_condition_compiles_in_single_file_mode() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(
        "src/tests/crashers/while_condition_all_exit_match_wrapped_tuple_projection/main.gom",
    );

    let go = compile_single_file_go(path);

    assert!(go.contains("func main()"), "{go}");
    assert!(go.contains("for {"), "{go}");
    assert!(go.contains("switch"), "{go}");
}

#[test]
fn index_wrapped_all_exit_match_while_condition_compiles_in_single_file_mode() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/while_condition_all_exit_match_wrapped_index_base/main.gom");

    let go = compile_single_file_go(path);

    assert!(go.contains("func main()"), "{go}");
    assert!(go.contains("for {"), "{go}");
    assert!(go.contains("switch"), "{go}");
}

#[test]
fn match_wrapped_all_exit_match_while_condition_compiles_in_single_file_mode() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/while_condition_all_exit_match_wrapped_match/main.gom");

    let go = compile_single_file_go(path);

    assert!(go.contains("func main()"), "{go}");
    assert!(go.contains("for {"), "{go}");
    assert!(go.contains("switch"), "{go}");
}

#[test]
fn enum_wrapped_all_exit_match_while_condition_compiles_in_single_file_mode() {
    let go = compile_src_go(
        "while_condition_all_exit_match_wrapped_enum.gom",
        r#"
enum WrapBool {
    Value(bool),
}

fn read_wrap(w: WrapBool) -> bool {
    match w {
        WrapBool::Value(value) => value,
    }
}

fn main() -> unit {
    while read_wrap(WrapBool::Value(match 0i32 {
        0i32 => break,
        _ => continue,
    })) {
        break;
    };
}
"#,
    );

    assert!(go.contains("func main()"), "{go}");
    assert!(go.contains("for {"), "{go}");
    assert!(go.contains("switch"), "{go}");
}

#[test]
fn tuple_wrapped_all_exit_match_while_condition_compiles_in_single_file_mode() {
    let go = compile_src_go(
        "while_condition_all_exit_match_wrapped_tuple.gom",
        r#"
fn first(pair: (bool, int32)) -> bool {
    pair.0
}

fn main() -> unit {
    while first((match 0i32 {
        0i32 => break,
        _ => continue,
    }, 0i32)) {
        break;
    };
}
"#,
    );

    assert!(go.contains("func main()"), "{go}");
    assert!(go.contains("for {"), "{go}");
    assert!(go.contains("switch"), "{go}");
}

#[test]
fn array_wrapped_all_exit_match_while_condition_compiles_in_single_file_mode() {
    let go = compile_src_go(
        "while_condition_all_exit_match_wrapped_array.gom",
        r#"
fn first(items: [bool; 2]) -> bool {
    items[0i32]
}

fn main() -> unit {
    while first([match 0i32 {
        0i32 => break,
        _ => continue,
    }, false]) {
        break;
    };
}
"#,
    );

    assert!(go.contains("func main()"), "{go}");
    assert!(go.contains("for {"), "{go}");
    assert!(go.contains("switch"), "{go}");
}

#[test]
fn if_match_call_wrapped_all_exit_match_while_condition_compiles_in_single_file_mode() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/while_condition_wrapped_if_match_call_stack_overflow/main.gom");

    let go = compile_single_file_go(path);

    assert!(go.contains("func main()"), "{go}");
    assert!(go.contains("for {"), "{go}");
    assert!(go.contains("switch"), "{go}");
    assert!(go.contains("id_bool"), "{go}");
}

#[test]
fn nested_match_wrapped_all_exit_match_while_condition_compiles_in_single_file_mode() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/while_condition_nested_match_bool_join_mismatch/main.gom");

    let go = compile_single_file_go(path);

    assert!(go.contains("func main()"), "{go}");
    assert!(go.contains("for {"), "{go}");
    assert!(go.contains("switch"), "{go}");
}
