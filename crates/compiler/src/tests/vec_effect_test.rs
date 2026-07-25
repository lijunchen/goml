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
fn vec_method_push_mutates_shared_storage_in_go_codegen() {
    let src = r#"
fn main() -> unit {
    let v: Vec[int32] = Vec::new();
    v.push(1);
    println(v.len().to_string());
}
"#;

    let go = compile_go(src, "vec_method_push_mutates_shared_storage.gom");

    assert!(go.contains("vec.items = append(vec.items, elem)"), "{go}");
}
