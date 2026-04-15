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
fn function_value_tuple_return_can_be_projected_without_annotation() {
    let src = r#"
fn pair(a: int32, b: string) -> (int32, string) {
    (a, b)
}

fn main() -> unit {
    let mk: (int32, string) -> (int32, string) = pair;
    let out = mk(1, "x");
    println(out.0.to_string() + out.1);
}
"#;

    let go = compile_go(src, "function_value_tuple_return_projection.gom");

    assert!(go.contains("var out__"));
    assert!(go.contains("._0"));
    assert!(go.contains("._1"));
}
