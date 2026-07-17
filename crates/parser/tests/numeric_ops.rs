use std::path::Path;

use parser::{debug_tree, parse};

#[test]
fn parses_numeric_bit_operators_and_casts() {
    let source = r#"
fn f(x: uint32, y: uint32) -> uint8 {
    (~x & y | x ^ y) % 17u32 << 2u32 >> 1u32 as uint8
}
"#;
    let result = parse(Path::new("numeric_ops.gom"), source);

    assert!(
        !result.has_errors(),
        "{}",
        result.format_errors(source).join("\n")
    );
    let tree = debug_tree(&result.green_node);
    assert!(tree.contains("EXPR_CAST"), "{tree}");
    assert!(tree.contains("Percent"), "{tree}");
    assert!(tree.contains("LessLess"), "{tree}");
    assert!(tree.contains("GreaterGreater"), "{tree}");
    assert!(tree.contains("Tilde"), "{tree}");
}

#[test]
fn reports_missing_cast_target_type() {
    let source = "fn main() -> unit { let value = 1 as; }";
    let result = parse(Path::new("missing_cast_type.gom"), source);

    assert!(result.has_errors());
}
