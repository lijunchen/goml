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
fn reversed_result_variants_work_with_try() {
    let src = r#"
enum Result[T, E] {
    Err(E),
    Ok(T),
}

fn parse(flag: bool) -> Result[int32, string] {
    if flag {
        Result::Ok(41)
    } else {
        Result::Err("bad")
    }
}

fn compute(flag: bool) -> Result[int32, string] {
    let value = parse(flag)?;
    Result::Ok(value + 1)
}

fn show(x: Result[int32, string]) -> string {
    match x {
        Result::Ok(v) => v.to_string(),
        Result::Err(e) => e,
    }
}

fn main() -> unit {
    println(show(compute(true)));
    println(show(compute(false)));
}
"#;

    let go = compile_go(src, "reversed_result_try.gom");

    assert!(go.contains("case Err:"));
    assert!(go.contains("case Ok:"));
    assert!(go.contains("Result__int32__string = Err{"));
    assert!(go.contains("Result__int32__string = Ok{"));
}

#[test]
fn user_defined_option_variants_work_with_try() {
    let src = r#"
enum Option[T] {
    Some(T),
    None,
}

fn maybe_value(flag: bool) -> Option[int32] {
    if flag {
        Option::Some(41)
    } else {
        Option::None
    }
}

fn compute(flag: bool) -> Option[int32] {
    let value = maybe_value(flag)?;
    Option::Some(value + 1)
}

fn show(x: Option[int32]) -> string {
    match x {
        Option::Some(v) => v.to_string(),
        Option::None => "none",
    }
}

fn main() -> unit {
    println(show(compute(true)));
    println(show(compute(false)));
}
"#;

    let go = compile_go(src, "user_option_try.gom");

    assert!(go.contains("case Some:"));
    assert!(go.contains("case None:"));
    assert!(go.contains("return Some{"));
    assert!(go.contains("return None{}"));
}
