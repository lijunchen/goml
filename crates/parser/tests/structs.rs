use expect_test::{Expect, expect};
use parser::{debug_tree, parse};
use std::path::Path;

fn check(input: &str, expect: Expect) {
    let path = Path::new("test.goml");
    let result = parse(path, input);
    expect.assert_eq(&debug_tree(&result.green_node));
}

#[test]
fn struct_with_fields() {
    check(
        r#"struct Point {
    x: int,
    y: int,
}
"#,
        expect![[r#"
            FILE@0..41
              STRUCT@0..41
                StructKeyword@0..6 "struct"
                Whitespace@6..7 " "
                Uident@7..12 "Point"
                Whitespace@12..13 " "
                LBrace@13..14 "{"
                Whitespace@14..19 "\n    "
                STRUCT_FIELD_LIST@19..41
                  STRUCT_FIELD@19..25
                    Lident@19..20 "x"
                    Colon@20..21 ":"
                    Whitespace@21..22 " "
                    TYPE_INT@22..25
                      IntKeyword@22..25 "int"
                  Comma@25..26 ","
                  Whitespace@26..31 "\n    "
                  STRUCT_FIELD@31..37
                    Lident@31..32 "y"
                    Colon@32..33 ":"
                    Whitespace@33..34 " "
                    TYPE_INT@34..37
                      IntKeyword@34..37 "int"
                  Comma@37..38 ","
                  Whitespace@38..39 "\n"
                  RBrace@39..40 "}"
                  Whitespace@40..41 "\n""#]],
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
                Uident@7..14 "Wrapper"
                GENERIC_LIST@14..18
                  LBracket@14..15 "["
                  GENERIC@15..16
                    Uident@15..16 "T"
                  RBracket@16..17 "]"
                  Whitespace@17..18 " "
                LBrace@18..19 "{"
                Whitespace@19..24 "\n    "
                STRUCT_FIELD_LIST@24..36
                  STRUCT_FIELD@24..32
                    Lident@24..29 "value"
                    Colon@29..30 ":"
                    Whitespace@30..31 " "
                    TYPE_TAPP@31..32
                      Uident@31..32 "T"
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
                Uident@0..5 "Point"
                Whitespace@5..6 " "
                STRUCT_LITERAL_FIELD_LIST@6..20
                  LBrace@6..7 "{"
                  Whitespace@7..8 " "
                  STRUCT_LITERAL_FIELD@8..12
                    Lident@8..9 "x"
                    Colon@9..10 ":"
                    Whitespace@10..11 " "
                    EXPR_INT@11..12
                      Int@11..12 "1"
                  Comma@12..13 ","
                  Whitespace@13..14 " "
                  STRUCT_LITERAL_FIELD@14..19
                    Lident@14..15 "y"
                    Colon@15..16 ":"
                    Whitespace@16..17 " "
                    EXPR_INT@17..19
                      Int@17..18 "2"
                      Whitespace@18..19 " "
                  RBrace@19..20 "}""#]],
    );
}
