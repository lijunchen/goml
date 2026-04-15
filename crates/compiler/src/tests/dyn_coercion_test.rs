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
fn mixed_dyn_vec_push_with_distinct_impls_compiles() {
    let src = r#"
trait Show {
    fn show(Self) -> string;
}

struct Wrap {
    value: string,
}

impl Show for int32 {
    fn show(self: int32) -> string {
        self.to_string()
    }
}

impl Show for Wrap {
    fn show(self: Wrap) -> string {
        self.value
    }
}

fn main() -> unit {
    let v: Vec[dyn Show] = vec_new();
    let v = vec_push(v, 10i32);
    let v = vec_push(v, Wrap { value: "ok" });
    let _ = println(Show::show(vec_get(v, 0i32)));
    let _ = println(Show::show(vec_get(v, 1i32)));
}
"#;

    let go = compile_go(src, "mixed_dyn_vec_push.gom");

    assert!(go.contains("dyn__Show__vtable__Wrap()"));
    assert!(go.contains("dyn__Show__vtable__int32()"));
}
