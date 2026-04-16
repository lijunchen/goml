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
