use std::path::PathBuf;

use crate::pipeline::pipeline::compile;

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
