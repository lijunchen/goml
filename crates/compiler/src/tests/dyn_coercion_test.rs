use std::path::PathBuf;

use crate::pipeline::pipeline::{compile, compile_single_file};

fn compile_go(src: &str, name: &str) -> String {
    let path = PathBuf::from(name);
    let compilation = compile(&path, src).unwrap_or_else(|err| {
        panic!("compilation failed for {}: {:?}", path.display(), err);
    });
    compilation.go.to_pretty(&compilation.goenv, 120)
}

fn compile_single_file_go(path: PathBuf) -> String {
    let src = std::fs::read_to_string(&path).unwrap_or_else(|err| {
        panic!("failed to read {}: {err}", path.display());
    });
    let compilation = compile_single_file(&path, &src).unwrap_or_else(|err| {
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

#[test]
fn implicit_dyn_coercion_from_generic_call_result_compiles() {
    let src = r#"
trait Show {
    fn show(Self) -> string;
}

struct Wrap[T] {
    value: T,
}

impl Show for Wrap[int32] {
    fn show(self: Wrap[int32]) -> string {
        self.value.to_string()
    }
}

fn make_wrap[T](x: T) -> Wrap[T] {
    Wrap { value: x }
}

fn render(x: dyn Show) -> string {
    Show::show(x)
}

fn main() -> unit {
    let _ = println(render(make_wrap(42i32)));
}
"#;

    let go = compile_go(src, "implicit_dyn_coercion_from_generic_call_result.gom");

    assert!(go.contains("dyn__Show__vtable__Wrap__int32()"));
}

#[test]
fn implicit_dyn_coercion_from_generic_enum_call_result_compiles() {
    let src = r#"
trait Show {
    fn show(Self) -> string;
}

enum Boxed[T] {
    One(T),
}

impl Show for Boxed[int32] {
    fn show(self: Boxed[int32]) -> string {
        match self {
            Boxed::One(x) => x.to_string(),
        }
    }
}

fn make_boxed[T](x: T) -> Boxed[T] {
    Boxed::One(x)
}

fn render(x: dyn Show) -> string {
    Show::show(x)
}

fn main() -> unit {
    let _ = println(render(make_boxed(42i32)));
}
"#;

    let go = compile_go(
        src,
        "implicit_dyn_coercion_from_generic_enum_call_result.gom",
    );

    assert!(go.contains("dyn__Show__vtable__Boxed__int32()"));
}

#[test]
fn implicit_dyn_coercion_from_generic_enum_constructor_compiles() {
    let src = r#"
trait Show {
    fn show(Self) -> string;
}

enum Boxed[T] {
    One(T),
}

impl Show for Boxed[int32] {
    fn show(self: Boxed[int32]) -> string {
        match self {
            Boxed::One(x) => x.to_string(),
        }
    }
}

fn render(x: dyn Show) -> string {
    Show::show(x)
}

fn main() -> unit {
    let _ = println(render(Boxed::One(42i32)));
}
"#;

    let go = compile_go(
        src,
        "implicit_dyn_coercion_from_generic_enum_constructor.gom",
    );

    assert!(go.contains("dyn__Show__vtable__Boxed__int32()"));
}

#[test]
fn dyn_trait_types_are_emitted_for_early_return_subexpressions() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/dyn_trait_type_emission_return_subexpr/main.gom");

    let go = compile_single_file_go(path);

    assert!(go.contains("type dyn__Display_vtable struct"), "{go}");
    assert!(go.contains("type dyn__Display struct"), "{go}");
}

#[test]
fn dyn_trait_types_are_emitted_for_enum_fields_in_early_return_subexpressions() {
    let src = r#"
trait Display {
    fn show(Self) -> string;
}

impl Display for int32 {
    fn show(self: int32) -> string {
        self.to_string()
    }
}

enum Boxed {
    One(dyn Display),
}

fn build() -> int32 {
    let _: Boxed = Boxed::One(return 9i32);
    0i32
}

fn main() -> unit {
    println(int32_to_string(build()))
}
"#;

    let go = compile_go(src, "dyn_enum_return_subexpr.gom");

    assert!(go.contains("type dyn__Display_vtable struct"), "{go}");
    assert!(go.contains("type dyn__Display struct"), "{go}");
}
