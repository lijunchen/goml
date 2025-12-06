use super::*;
use expect_test::{Expect, expect};

#[allow(unused)]
fn check(input: &str, expect: Expect) {
    let toks = lex(input);
    expect.assert_debug_eq(&toks)
}

#[test]
fn test_1() {
    check(
        "let a = 123",
        expect![[r#"
            [
                {kind: LetKeyword, text: "let"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "a"},
                {kind: Whitespace, text: " "},
                {kind: Eq, text: "="},
                {kind: Whitespace, text: " "},
                {kind: Int, text: "123"},
            ]
        "#]],
    )
}

#[test]
fn test_2() {
    check(
        "fn f1(x: i32,
                fn f2(x: i32,, z: i32) {}
                fn f3() {}",
        expect![[r#"
            [
                {kind: FnKeyword, text: "fn"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "f1"},
                {kind: LParen, text: "("},
                {kind: Ident, text: "x"},
                {kind: Colon, text: ":"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "i32"},
                {kind: Comma, text: ","},
                {kind: Whitespace, text: "\n                "},
                {kind: FnKeyword, text: "fn"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "f2"},
                {kind: LParen, text: "("},
                {kind: Ident, text: "x"},
                {kind: Colon, text: ":"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "i32"},
                {kind: Comma, text: ","},
                {kind: Comma, text: ","},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "z"},
                {kind: Colon, text: ":"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "i32"},
                {kind: RParen, text: ")"},
                {kind: Whitespace, text: " "},
                {kind: LBrace, text: "{"},
                {kind: RBrace, text: "}"},
                {kind: Whitespace, text: "\n                "},
                {kind: FnKeyword, text: "fn"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "f3"},
                {kind: LParen, text: "("},
                {kind: RParen, text: ")"},
                {kind: Whitespace, text: " "},
                {kind: LBrace, text: "{"},
                {kind: RBrace, text: "}"},
            ]
        "#]],
    )
}

#[test]
fn test_error_token() {
    check(
        "let a = $ + 123",
        expect![[r#"
            [
                {kind: LetKeyword, text: "let"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "a"},
                {kind: Whitespace, text: " "},
                {kind: Eq, text: "="},
                {kind: Whitespace, text: " "},
                {kind: Error, text: "$"},
                {kind: Whitespace, text: " "},
                {kind: Plus, text: "+"},
                {kind: Whitespace, text: " "},
                {kind: Int, text: "123"},
            ]
        "#]],
    )
}

#[test]
fn test_comment() {
    check(
        "// let a = $
            let a = 1
            // comment",
        expect![[r#"
            [
                {kind: Comment, text: "// let a = $"},
                {kind: Whitespace, text: "\n            "},
                {kind: LetKeyword, text: "let"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "a"},
                {kind: Whitespace, text: " "},
                {kind: Eq, text: "="},
                {kind: Whitespace, text: " "},
                {kind: Int, text: "1"},
                {kind: Whitespace, text: "\n            "},
                {kind: Comment, text: "// comment"},
            ]
        "#]],
    )
}

#[test]
fn lexes_logical_operators() {
    check(
        "!a && b || c",
        expect![[r#"
            [
                {kind: Bang, text: "!"},
                {kind: Ident, text: "a"},
                {kind: Whitespace, text: " "},
                {kind: AndAnd, text: "&&"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "b"},
                {kind: Whitespace, text: " "},
                {kind: OrOr, text: "||"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "c"},
            ]
        "#]],
    )
}

#[test]
fn lexes_ref_builtins() {
    check(
        "ref_set(r, ref_get(a))",
        expect![[r#"
            [
                {kind: Ident, text: "ref_set"},
                {kind: LParen, text: "("},
                {kind: Ident, text: "r"},
                {kind: Comma, text: ","},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "ref_get"},
                {kind: LParen, text: "("},
                {kind: Ident, text: "a"},
                {kind: RParen, text: ")"},
                {kind: RParen, text: ")"},
            ]
        "#]],
    )
}

#[test]
fn lexes_multiline_string() {
    check(
        "let s = \\\\hello\n    \\\\world",
        expect![[r#"
            [
                {kind: LetKeyword, text: "let"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "s"},
                {kind: Whitespace, text: " "},
                {kind: Eq, text: "="},
                {kind: Whitespace, text: " "},
                {kind: MultilineStr, text: "\\\\hello\n    \\\\world"},
            ]
        "#]],
    )
}

#[test]
fn lexes_multiline_string_with_trailing_tokens() {
    check(
        "let s = \\\\hello\n    \\\\world\n    ;",
        expect![[r#"
            [
                {kind: LetKeyword, text: "let"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "s"},
                {kind: Whitespace, text: " "},
                {kind: Eq, text: "="},
                {kind: Whitespace, text: " "},
                {kind: MultilineStr, text: "\\\\hello\n    \\\\world"},
                {kind: Whitespace, text: "\n    "},
                {kind: Semi, text: ";"},
            ]
        "#]],
    )
}

#[test]
fn lexes_int8_keyword() {
    check(
        "int8",
        expect![[r#"
            [
                {kind: Int8Keyword, text: "int8"},
            ]
        "#]],
    );
}
