use std::path::Path;

use compiler::pipeline;

#[test]
fn pipeline_generates_trait_impl_functions_for_struct_and_generic_types() {
    let src = r#"
struct Point { x: int, y: int }

enum Option[T] {
    Some(T),
    None,
}

trait Display {
    fn show(Self) -> string;
}

impl Display for Point {
    fn show(self: Point) -> string { "Point" }
}

impl Display for Option[int] {
    fn show(self: Option[int]) -> string { "Option" }
}
"#;

    let compilation = pipeline::compile(Path::new("inline.src"), src)
        .expect("pipeline compilation should succeed");

    let keys: Vec<(String, String, String)> = compilation
        .env
        .trait_impls
        .keys()
        .map(|(trait_name, ty_enc, method)| (trait_name.clone(), ty_enc.clone(), method.0.clone()))
        .collect();

    assert!(
        keys.contains(&(
            "Display".to_string(),
            "Point".to_string(),
            "show".to_string()
        )),
        "expected struct impl registration in environment"
    );
    assert!(
        keys.contains(&(
            "Display".to_string(),
            "Option_int".to_string(),
            "show".to_string(),
        )),
        "expected generic impl registration in environment"
    );
}
