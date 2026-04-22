use std::path::PathBuf;

use crate::pipeline::pipeline::compile_single_file;

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
