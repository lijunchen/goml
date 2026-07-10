use std::path::PathBuf;

use crate::{go::mangle::MAX_GO_IDENT_LEN, pipeline::pipeline::compile_single_file};

fn go_identifiers(source: &str) -> impl Iterator<Item = &str> {
    source
        .split(|ch: char| !(ch.is_ascii_alphanumeric() || ch == '_'))
        .filter(|candidate| {
            candidate
                .as_bytes()
                .first()
                .is_some_and(|first| first.is_ascii_alphabetic() || *first == b'_')
        })
}

#[test]
fn complex_generated_names_are_bounded_and_execute() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/pipeline/151_generic_closure_multi_instantiation/main.gom");
    let source = std::fs::read_to_string(&path).unwrap();
    let compilation = compile_single_file(&path, &source).unwrap();
    let go = compilation.go.to_pretty(&compilation.goenv, 120);
    let longest = go_identifiers(&go).max_by_key(|ident| ident.len()).unwrap();

    assert!(
        longest.len() <= MAX_GO_IDENT_LEN,
        "generated Go identifier has {} bytes: {}",
        longest.len(),
        longest
    );
    assert!(
        go_identifiers(&go).any(|ident| ident.len() == MAX_GO_IDENT_LEN && ident.contains("_h"))
    );

    let output = super::execute_go_source(&go, &path.to_string_lossy()).unwrap();
    assert_eq!(output, "a:7\nb:ok\n");
}
