use std::path::PathBuf;

use crate::pipeline::{self, pipeline::CompilationError};

fn deep_struct_pattern_source(depth: usize) -> String {
    let mut src = String::from("package main;\n\n");
    src.push_str("struct S0 { value: int32, }\n");
    for index in 1..=depth {
        src.push_str(&format!(
            "struct S{} {{ inner: S{}, other: int32, }}\n",
            index,
            index - 1
        ));
    }

    src.push_str("\nfn main() -> unit {\n");
    src.push_str("    let s0 = S0 { value: 7i32 };\n");
    for index in 1..=depth {
        src.push_str(&format!(
            "    let s{} = S{} {{ inner: s{}, other: {}i32 }};\n",
            index,
            index,
            index - 1,
            index
        ));
    }

    let mut pattern = "S0 { value }".to_string();
    for index in 1..=depth {
        pattern = format!("S{} {{ inner: {}, other: _ }}", index, pattern);
    }
    src.push_str(&format!("    let {} = s{};\n", pattern, depth));
    src.push_str("    println(value);\n");
    src.push_str("}\n");
    src
}

#[test]
fn deep_struct_pattern_reports_lower_error() {
    let src = deep_struct_pattern_source(150);
    let path = PathBuf::from("deep_struct_pattern.gom");
    let err = pipeline::pipeline::compile(&path, &src).expect_err("expected lower error");

    match err {
        CompilationError::Lower { diagnostics } => {
            assert!(
                diagnostics
                    .iter()
                    .any(|diagnostic| diagnostic.message() == "pattern is too deeply nested")
            );
        }
        other => panic!("expected lower error, got {:?}", other),
    }
}
