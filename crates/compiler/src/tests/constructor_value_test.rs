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
fn payload_enum_constructor_values_compile() {
    let src = r#"
enum List[T] {
    Nil,
    Cons(T, List[T]),
}

fn prepend_with(make: (int32, List[int32]) -> List[int32], x: int32, xs: List[int32]) -> List[int32] {
    make(x, xs)
}

fn main() -> unit {
    let cons: (int32, List[int32]) -> List[int32] = List::Cons;
    let xs = prepend_with(cons, 1i32, prepend_with(cons, 2i32, List::Nil));
    let _ = xs;
}
"#;

    let go = compile_go(src, "payload_enum_constructor_values.gom");

    assert!(go.contains("type List__int32 interface"));
}
