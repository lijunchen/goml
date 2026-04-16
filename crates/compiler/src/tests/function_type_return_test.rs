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
fn struct_field_function_type_can_return_generic_application() {
    let src = r#"
enum Result[T, E] {
    Ok(T),
    Err(E),
}

struct Worker {
    run: () -> Result[int32, string>,
}

fn build() -> Worker {
    Worker { run: || Result::Ok(21) }
}

fn main() -> unit {
    let _ = build().run();
}
"#;

    let go = compile_go(src, "struct_field_function_type_generic_return.gom");

    assert!(go.contains("type Worker struct"));
    assert!(go.contains("func() Result__int32__string"));
}
