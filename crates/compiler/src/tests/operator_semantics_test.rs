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
fn logical_operators_short_circuit_side_effects() {
    let output = run_crasher("short_circuit_side_effect");

    assert_eq!(output, "a\nfalse\nc\ntrue\n");
}
