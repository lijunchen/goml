use std::path::PathBuf;

use crate::pipeline::pipeline::{compile, compile_single_file};

fn compile_go(src: &str, name: &str) -> String {
    let path = PathBuf::from(name);
    let compilation = compile(&path, src).unwrap_or_else(|err| {
        panic!("compilation failed for {}: {:?}", path.display(), err);
    });
    compilation.go.to_pretty(&compilation.goenv, 120)
}

#[test]
fn discarded_vec_push_updates_binding_in_go_codegen() {
    let src = r#"
fn main() -> unit {
    let v: Vec[int32] = vec_new();
    vec_push(v, 1i32);
    println(vec_len(v).to_string());
}
"#;

    let go = compile_go(src, "discarded_vec_push_updates_binding.gom");

    assert!(go.contains("v__0 = append(v__0, 1)"), "{go}");
}

#[test]
fn discarded_let_vec_push_preserves_side_effect() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/discarded_let_vec_push_side_effect/main.gom");
    let src = std::fs::read_to_string(&path).unwrap();
    let compilation = compile_single_file(&path, &src).unwrap();
    let go = compilation.go.to_pretty(&compilation.goenv, 120);
    let output = super::execute_go_source(&go, &path.to_string_lossy()).unwrap();

    assert_eq!(output, "1\n");
}

#[test]
fn vec_push_preserves_existing_binding_value() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/vec_push_preserves_existing_binding/main.gom");
    let src = std::fs::read_to_string(&path).unwrap();
    let compilation = compile_single_file(&path, &src).unwrap();
    let go = compilation.go.to_pretty(&compilation.goenv, 120);
    let output = super::execute_go_source(&go, &path.to_string_lossy()).unwrap();

    assert_eq!(output, "2\n");
}
