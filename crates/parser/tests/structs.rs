use expect_test::{Expect, expect};
use parser::{debug_tree, parse};
use std::path::Path;

fn check(input: &str, expect: Expect) {
    let path = Path::new("test.goml");
    let result = parse(path, input);
    if result.has_errors() {
        panic!(
            "unexpected parse errors:\n{}",
            result.format_errors(input).join("\n")
        );
    }
    expect.assert_eq(&debug_tree(&result.green_node));
}

#[test]
fn struct_with_fields() {
    check(
        r#"struct Point {
    x: int32,
    y: int32,
}
"#,
        expect![[r#"
            FILE@0..45
              STRUCT@0..45
                StructKeyword@0..6 "struct"
                Whitespace@6..7 " "
                Ident@7..12 "Point"
                Whitespace@12..13 " "
                LBrace@13..14 "{"
                Whitespace@14..19 "\n    "
                STRUCT_FIELD_LIST@19..45
                  STRUCT_FIELD@19..27
                    Ident@19..20 "x"
                    Colon@20..21 ":"
                    Whitespace@21..22 " "
                    TYPE_INT32@22..27
                      Int32Keyword@22..27 "int32"
                  Comma@27..28 ","
                  Whitespace@28..33 "\n    "
                  STRUCT_FIELD@33..41
                    Ident@33..34 "y"
                    Colon@34..35 ":"
                    Whitespace@35..36 " "
                    TYPE_INT32@36..41
                      Int32Keyword@36..41 "int32"
                  Comma@41..42 ","
                  Whitespace@42..43 "\n"
                  RBrace@43..44 "}"
                  Whitespace@44..45 "\n""#]],
    );
}

#[test]
fn struct_with_int8_field() {
    check(
        r#"struct Byte {
    value: int8,
}
"#,
        expect![[r#"
            FILE@0..33
              STRUCT@0..33
                StructKeyword@0..6 "struct"
                Whitespace@6..7 " "
                Ident@7..11 "Byte"
                Whitespace@11..12 " "
                LBrace@12..13 "{"
                Whitespace@13..18 "\n    "
                STRUCT_FIELD_LIST@18..33
                  STRUCT_FIELD@18..29
                    Ident@18..23 "value"
                    Colon@23..24 ":"
                    Whitespace@24..25 " "
                    TYPE_INT8@25..29
                      Int8Keyword@25..29 "int8"
                  Comma@29..30 ","
                  Whitespace@30..31 "\n"
                  RBrace@31..32 "}"
                  Whitespace@32..33 "\n""#]],
    );
}

#[test]
fn struct_with_generics() {
    check(
        r#"struct Wrapper[T] {
    value: T,
}
"#,
        expect![[r#"
            FILE@0..36
              STRUCT@0..36
                StructKeyword@0..6 "struct"
                Whitespace@6..7 " "
                Ident@7..14 "Wrapper"
                GENERIC_LIST@14..18
                  LBracket@14..15 "["
                  GENERIC@15..16
                    Ident@15..16 "T"
                  RBracket@16..17 "]"
                  Whitespace@17..18 " "
                LBrace@18..19 "{"
                Whitespace@19..24 "\n    "
                STRUCT_FIELD_LIST@24..36
                  STRUCT_FIELD@24..32
                    Ident@24..29 "value"
                    Colon@29..30 ":"
                    Whitespace@30..31 " "
                    TYPE_TAPP@31..32
                      Ident@31..32 "T"
                  Comma@32..33 ","
                  Whitespace@33..34 "\n"
                  RBrace@34..35 "}"
                  Whitespace@35..36 "\n""#]],
    );
}

#[test]
fn struct_literal_expr() {
    check(
        "Point { x: 1, y: 2 }",
        expect![[r#"
            FILE@0..20
              EXPR_STRUCT_LITERAL@0..20
                Ident@0..5 "Point"
                Whitespace@5..6 " "
                STRUCT_LITERAL_FIELD_LIST@6..20
                  LBrace@6..7 "{"
                  Whitespace@7..8 " "
                  STRUCT_LITERAL_FIELD@8..12
                    Ident@8..9 "x"
                    Colon@9..10 ":"
                    Whitespace@10..11 " "
                    EXPR_INT@11..12
                      Int@11..12 "1"
                  Comma@12..13 ","
                  Whitespace@13..14 " "
                  STRUCT_LITERAL_FIELD@14..19
                    Ident@14..15 "y"
                    Colon@15..16 ":"
                    Whitespace@16..17 " "
                    EXPR_INT@17..19
                      Int@17..18 "2"
                      Whitespace@18..19 " "
                  RBrace@19..20 "}""#]],
    );
}

#[test]
fn struct_pattern_shorthand() {
    check(
        "fn main() { let Point { x, y } = point; }",
        expect![[r#"
            FILE@0..41
              FN@0..41
                FnKeyword@0..2 "fn"
                Whitespace@2..3 " "
                Ident@3..7 "main"
                PARAM_LIST@7..10
                  LParen@7..8 "("
                  RParen@8..9 ")"
                  Whitespace@9..10 " "
                BLOCK@10..41
                  LBrace@10..11 "{"
                  Whitespace@11..12 " "
                  STMT_LET@12..40
                    LetKeyword@12..15 "let"
                    Whitespace@15..16 " "
                    PATTERN_CONSTR@16..31
                      Ident@16..21 "Point"
                      Whitespace@21..22 " "
                      STRUCT_PATTERN_FIELD_LIST@22..31
                        LBrace@22..23 "{"
                        Whitespace@23..24 " "
                        STRUCT_PATTERN_FIELD@24..25
                          Ident@24..25 "x"
                        Comma@25..26 ","
                        Whitespace@26..27 " "
                        STRUCT_PATTERN_FIELD@27..29
                          Ident@27..28 "y"
                          Whitespace@28..29 " "
                        RBrace@29..30 "}"
                        Whitespace@30..31 " "
                    Eq@31..32 "="
                    Whitespace@32..33 " "
                    EXPR_IDENT@33..38
                      Ident@33..38 "point"
                    Semi@38..39 ";"
                    Whitespace@39..40 " "
                  RBrace@40..41 "}""#]],
    );
}

#[test]
fn reports_parse_errors_without_panicking() {
    let path = Path::new("test.goml");
    let src = "fn foo(";
    let result = parse(path, src);
    assert!(result.has_errors());
    let errors = result.format_errors(src);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("expect"));
}

#[test]
fn let_expression_without_pattern_reports_error() {
    let path = Path::new("test.goml");
    let src = "fn main() { let = 42; foo }";
    let result = parse(path, src);
    assert!(result.has_errors());
    let errors = result.format_errors(src);
    assert!(errors.iter().any(|msg| msg.contains("expected a pattern")));
}
