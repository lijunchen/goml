use std::path::PathBuf;

use crate::env::format_typer_diagnostics;
use crate::pipeline::pipeline::{CompilationError, compile_single_file};

#[test]
fn enum_closure_payload_return_executes() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/enum_closure_payload_return/main.gom");
    let src = std::fs::read_to_string(&path).unwrap();
    let compilation = compile_single_file(&path, &src).unwrap();
    let go = compilation.go.to_pretty(&compilation.goenv, 120);
    let output = super::execute_go_source(&go, &path.to_string_lossy()).unwrap();

    assert_eq!(output, "42\n");
}

#[test]
fn option_function_payload_with_other_instantiation_executes() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/option_function_payload_with_other_instantiation/main.gom");
    let src = std::fs::read_to_string(&path).unwrap();
    let compilation = compile_single_file(&path, &src).unwrap();
    let go = compilation.go.to_pretty(&compilation.goenv, 120);
    let output = super::execute_go_source(&go, &path.to_string_lossy()).unwrap();

    assert_eq!(output, "1\n");
}

#[test]
fn closure_with_vec_capture_equality_reports_typer_error() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/closure_equality_vec_capture/main.gom");
    let src = std::fs::read_to_string(&path).unwrap();
    let diagnostics = match compile_single_file(&path, &src) {
        Err(CompilationError::Typer { diagnostics }) => {
            format_typer_diagnostics(&diagnostics, &src)
        }
        Err(err) => panic!("expected typer error for {}: {err:?}", path.display()),
        Ok(_) => panic!("expected typer error for {}", path.display()),
    };
    let diagnostics = diagnostics.join("\n");

    assert!(diagnostics.contains("Operator =="), "{diagnostics}");
    assert!(diagnostics.contains("() -> int32"), "{diagnostics}");
}
