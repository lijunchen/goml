use std::path::PathBuf;

use crate::env::format_typer_diagnostics;
use crate::pipeline::pipeline::{CompilationError, compile_single_file};

fn compile_single_file_typer_diagnostics(path: PathBuf) -> String {
    let src = std::fs::read_to_string(&path).unwrap_or_else(|err| {
        panic!("failed to read {}: {err}", path.display());
    });
    match compile_single_file(&path, &src) {
        Err(CompilationError::Typer { diagnostics }) => {
            format_typer_diagnostics(&diagnostics, &src).join("\n")
        }
        Err(err) => panic!("expected typer error for {}: {err:?}", path.display()),
        Ok(_) => panic!("expected typer error for {}", path.display()),
    }
}

fn compile_src_typer_diagnostics(name: &str, src: &str) -> String {
    let path = PathBuf::from(name);
    match compile_single_file(&path, src) {
        Err(CompilationError::Typer { diagnostics }) => {
            format_typer_diagnostics(&diagnostics, src).join("\n")
        }
        Err(err) => panic!("expected typer error for {}: {err:?}", path.display()),
        Ok(_) => panic!("expected typer error for {}", path.display()),
    }
}

fn assert_reports_while_condition_error(diagnostics: &str) {
    assert!(diagnostics.contains("while condition"), "{diagnostics}");
}

#[test]
fn all_exit_match_while_condition_reports_typer_error() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/while_condition_all_exit_match/main.gom");

    let diagnostics = compile_single_file_typer_diagnostics(path);

    assert_reports_while_condition_error(&diagnostics);
}

#[test]
fn call_wrapped_all_exit_while_condition_reports_typer_error() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/while_condition_non_bool_break_continue_arg/main.gom");

    let diagnostics = compile_single_file_typer_diagnostics(path);

    assert_reports_while_condition_error(&diagnostics);
}

#[test]
fn struct_wrapped_all_exit_match_while_condition_reports_typer_error() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/while_condition_all_exit_match_wrapped_struct/main.gom");

    let diagnostics = compile_single_file_typer_diagnostics(path);

    assert_reports_while_condition_error(&diagnostics);
}

#[test]
fn field_wrapped_all_exit_match_while_condition_reports_typer_error() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/while_condition_all_exit_match_wrapped_field_access/main.gom");

    let diagnostics = compile_single_file_typer_diagnostics(path);

    assert_reports_while_condition_error(&diagnostics);
}

#[test]
fn tuple_projection_wrapped_all_exit_match_while_condition_reports_typer_error() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(
        "src/tests/crashers/while_condition_all_exit_match_wrapped_tuple_projection/main.gom",
    );

    let diagnostics = compile_single_file_typer_diagnostics(path);

    assert_reports_while_condition_error(&diagnostics);
}

#[test]
fn index_wrapped_all_exit_match_while_condition_reports_typer_error() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/while_condition_all_exit_match_wrapped_index_base/main.gom");

    let diagnostics = compile_single_file_typer_diagnostics(path);

    assert_reports_while_condition_error(&diagnostics);
}

#[test]
fn match_wrapped_all_exit_match_while_condition_reports_typer_error() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/while_condition_all_exit_match_wrapped_match/main.gom");

    let diagnostics = compile_single_file_typer_diagnostics(path);

    assert_reports_while_condition_error(&diagnostics);
}

#[test]
fn enum_wrapped_all_exit_match_while_condition_reports_typer_error() {
    let diagnostics = compile_src_typer_diagnostics(
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

    assert_reports_while_condition_error(&diagnostics);
}

#[test]
fn tuple_wrapped_all_exit_match_while_condition_reports_typer_error() {
    let diagnostics = compile_src_typer_diagnostics(
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

    assert_reports_while_condition_error(&diagnostics);
}

#[test]
fn array_wrapped_all_exit_match_while_condition_reports_typer_error() {
    let diagnostics = compile_src_typer_diagnostics(
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

    assert_reports_while_condition_error(&diagnostics);
}

#[test]
fn if_match_call_wrapped_all_exit_match_while_condition_reports_typer_error() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/while_condition_wrapped_if_match_call_stack_overflow/main.gom");

    let diagnostics = compile_single_file_typer_diagnostics(path);

    assert_reports_while_condition_error(&diagnostics);
}

#[test]
fn nested_match_wrapped_all_exit_match_while_condition_reports_typer_error() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/while_condition_nested_match_bool_join_mismatch/main.gom");

    let diagnostics = compile_single_file_typer_diagnostics(path);

    assert_reports_while_condition_error(&diagnostics);
}

#[test]
fn deep_call_wrapped_all_exit_match_while_condition_reports_typer_error() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/while_condition_deep_call_nesting_stack_overflow/main.gom");

    let diagnostics = compile_single_file_typer_diagnostics(path);

    assert_reports_while_condition_error(&diagnostics);
}
