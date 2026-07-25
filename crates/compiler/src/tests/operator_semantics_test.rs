use std::path::PathBuf;

use crate::{
    env::format_typer_diagnostics,
    pipeline::pipeline::{CompilationError, compile_single_file},
};

fn crasher_source(name: &str) -> (PathBuf, String) {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers")
        .join(name)
        .join("main.gom");
    let src = std::fs::read_to_string(&path).unwrap_or_else(|err| {
        panic!("failed to read {}: {err}", path.display());
    });
    (path, src)
}

fn run_crasher(name: &str) -> String {
    let (path, src) = crasher_source(name);
    let compilation = compile_single_file(&path, &src).unwrap_or_else(|err| {
        panic!("compilation failed for {}: {:?}", path.display(), err);
    });
    let go = compilation.go.to_pretty(&compilation.goenv, 120);
    super::execute_go_source(&go, &path.to_string_lossy()).unwrap()
}

fn typer_errors(name: &str) -> Vec<String> {
    let (path, src) = crasher_source(name);
    let err = compile_single_file(&path, &src).expect_err("expected typer error");
    match err {
        CompilationError::Typer { diagnostics } => format_typer_diagnostics(&diagnostics, &src),
        other => panic!("expected typer error, got {other:?}"),
    }
}

#[test]
fn logical_operators_short_circuit_side_effects() {
    let output = run_crasher("short_circuit_side_effect");

    assert_eq!(output, "a\nfalse\nc\ntrue\n");
}

#[test]
fn numeric_bit_operators_and_casts_execute() {
    let path = PathBuf::from("numeric_bit_operators_and_casts_execute.gom");
    let src = r#"
fn main() -> unit {
    let byte: uint8 = 240;
    println(byte & 15);
    println(byte | 15);
    println(byte ^ 255);
    println(1 << 7);
    println(byte >> 4);
    println(~byte);
    println(-13 % 5);
    println(511 as uint8);
    println(-1 as uint8);
    println(('A' as uint32).to_string());
    match char_from_uint32(128512) {
        Option::Some(value) => println(value.to_string()),
        Option::None => println("invalid"),
    }
}
"#;
    let compilation = compile_single_file(&path, src).expect("expected compilation to succeed");
    let go = compilation.go.to_pretty(&compilation.goenv, 120);
    let output = super::execute_go_source(&go, &path.to_string_lossy()).unwrap();

    assert_eq!(output, "0\n255\n15\n128\n15\n15\n-3\n255\n255\n65\n😀\n");
}

#[test]
fn remainder_rejects_float_operands() {
    let diagnostics = typer_errors("numeric_bit_float_operand");

    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("Operator % is not defined for type float64")),
        "{diagnostics:?}"
    );
}

#[test]
fn bitwise_operators_reject_bool_operands() {
    let diagnostics = typer_errors("numeric_bit_bool_operand");

    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("Operator & is not defined for type bool")),
        "{diagnostics:?}"
    );
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("Operator ~ is not defined for type bool")),
        "{diagnostics:?}"
    );
}

#[test]
fn bitwise_operators_reject_mixed_widths() {
    let diagnostics = typer_errors("numeric_bit_mixed_width");
    let rendered = diagnostics.join("\n");

    assert!(rendered.contains("uint8"), "{diagnostics:?}");
    assert!(rendered.contains("uint16"), "{diagnostics:?}");
}

#[test]
fn shifts_reject_non_integer_counts() {
    let diagnostics = typer_errors("numeric_shift_bool_count");

    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("Operator << is not defined for type bool")),
        "{diagnostics:?}"
    );
}

#[test]
fn casts_reject_float_to_integer() {
    let diagnostics = typer_errors("numeric_cast_float_to_int");

    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("Invalid cast from float64 to int32")),
        "{diagnostics:?}"
    );
}

#[test]
fn casts_reject_bool_to_integer() {
    let diagnostics = typer_errors("numeric_cast_bool_to_int");

    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("Invalid cast from bool to uint8")),
        "{diagnostics:?}"
    );
}

#[test]
fn casts_only_allow_char_to_uint32() {
    let diagnostics = typer_errors("numeric_cast_invalid_char_width");

    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("Invalid cast from char to uint16")),
        "{diagnostics:?}"
    );
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("Invalid cast from uint16 to char")),
        "{diagnostics:?}"
    );
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("Invalid cast from uint32 to char")),
        "{diagnostics:?}"
    );
}
