use std::path::PathBuf;

use crate::{
    env::format_typer_diagnostics,
    pipeline::pipeline::{CompilationError, compile_single_file},
};

#[test]
fn shadowed_ref_get_array_assignment_is_rejected() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/ref_get_shadow_array_assignment_target/main.gom");
    let src = std::fs::read_to_string(&path).unwrap_or_else(|err| {
        panic!("failed to read {}: {err}", path.display());
    });
    let err = compile_single_file(&path, &src).expect_err("expected typer error");

    match err {
        CompilationError::Typer { diagnostics } => {
            let diagnostics = format_typer_diagnostics(&diagnostics, &src);
            assert!(
                diagnostics.iter().any(|line| line.contains(
                    "array indexed assignment requires a writable root such as a mutable local or `ref.get()`"
                )),
                "{diagnostics:?}"
            );
        }
        other => panic!("expected typer error, got {other:?}"),
    }
}
