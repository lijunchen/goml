use std::path::PathBuf;

use crate::{
    env::format_typer_diagnostics,
    pipeline::pipeline::{CompilationError, compile_single_file},
};

#[test]
fn user_lang_item_declaration_is_rejected() {
    let path = PathBuf::from("user_lang_item_declaration.gom");
    let src = r#"
#[lang("option")]
enum Maybe[T] {
    None,
    Some(T),
}

fn main() -> unit {
    ()
}
"#;
    let err = compile_single_file(&path, src).expect_err("expected typer error");

    match err {
        CompilationError::Typer { diagnostics } => {
            let diagnostics = format_typer_diagnostics(&diagnostics, src);
            assert!(
                diagnostics
                    .iter()
                    .any(|line| line.contains("lang item option is not permitted in this source")),
                "{diagnostics:?}"
            );
        }
        other => panic!("expected typer error, got {other:?}"),
    }
}
