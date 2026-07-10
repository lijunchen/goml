use std::path::PathBuf;

use crate::{
    env::format_typer_diagnostics,
    pipeline::{
        pipeline::{CompilationError, compile_single_file},
        separate::{self, PackageInputs},
    },
};

#[test]
fn main_function_with_parameter_is_rejected() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/main_with_parameter/main.gom");
    let src = std::fs::read_to_string(&path).unwrap_or_else(|err| {
        panic!("failed to read {}: {err}", path.display());
    });
    let err = compile_single_file(&path, &src).expect_err("expected typer error");

    match err {
        CompilationError::Typer { diagnostics } => {
            let diagnostics = format_typer_diagnostics(&diagnostics, &src);
            assert!(
                diagnostics
                    .iter()
                    .any(|line| line.contains("main function must not have parameters")),
                "{diagnostics:?}"
            );
        }
        other => panic!("expected typer error, got {other:?}"),
    }
}

#[test]
fn missing_main_function_is_rejected() {
    let path =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/tests/crashers/missing_main/main.gom");
    let src = std::fs::read_to_string(&path).unwrap_or_else(|err| {
        panic!("failed to read {}: {err}", path.display());
    });
    let err = compile_single_file(&path, &src).expect_err("expected typer error");

    match err {
        CompilationError::Typer { diagnostics } => {
            let diagnostics = format_typer_diagnostics(&diagnostics, &src);
            assert!(
                diagnostics
                    .iter()
                    .any(|line| line.contains("main function is required")),
                "{diagnostics:?}"
            );
        }
        other => panic!("expected typer error, got {other:?}"),
    }
}

fn check_canonical_main(source: &str) -> CompilationError {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("main.gom");
    std::fs::write(&path, source).unwrap();
    separate::check_package(PackageInputs {
        package: "example::cmd".to_string(),
        input_files: vec![path],
        interface_files: Vec::new(),
    })
    .expect_err("expected entrypoint error")
}

#[test]
fn canonical_main_package_rejects_parameter() {
    let error = check_canonical_main(
        r#"package main;

fn main(value: int32) -> unit {
    ()
}
"#,
    );
    assert!(error.diagnostics().iter().any(|diagnostic| {
        diagnostic
            .message()
            .contains("main function must not have parameters")
    }));
}

#[test]
fn canonical_main_package_rejects_type_parameter() {
    let error = check_canonical_main(
        r#"package main;

fn main[T]() -> unit {
    ()
}
"#,
    );
    assert!(error.diagnostics().iter().any(|diagnostic| {
        diagnostic
            .message()
            .contains("main function must not have type parameters")
    }));
}

#[test]
fn canonical_main_package_requires_main_function() {
    let error = check_canonical_main(
        r#"package main;

fn helper() -> unit {
    ()
}
"#,
    );
    assert!(
        error
            .diagnostics()
            .iter()
            .any(|diagnostic| { diagnostic.message().contains("main function is required") })
    );
}
