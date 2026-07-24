use super::*;
use expect_test::{Expect, expect};

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
fn pattern_operator_tokens() {
    check(
        "name @ 1..2 | 3..=4",
        expect![[r#"
            [
                {kind: Ident, text: "name"},
                {kind: Whitespace, text: " "},
                {kind: At, text: "@"},
                {kind: Whitespace, text: " "},
                {kind: Int, text: "1"},
                {kind: DotDot, text: ".."},
                {kind: Int, text: "2"},
                {kind: Whitespace, text: " "},
                {kind: Pipe, text: "|"},
                {kind: Whitespace, text: " "},
                {kind: Int, text: "3"},
                {kind: DotDotEq, text: "..="},
                {kind: Int, text: "4"},
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
fn lexes_numeric_bit_operators() {
    check(
        "a % b & c | d ^ ~e << f >> g as uint32",
        expect![[r#"
            [
                {kind: Ident, text: "a"},
                {kind: Whitespace, text: " "},
                {kind: Percent, text: "%"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "b"},
                {kind: Whitespace, text: " "},
                {kind: Amp, text: "&"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "c"},
                {kind: Whitespace, text: " "},
                {kind: Pipe, text: "|"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "d"},
                {kind: Whitespace, text: " "},
                {kind: Caret, text: "^"},
                {kind: Whitespace, text: " "},
                {kind: Tilde, text: "~"},
                {kind: Ident, text: "e"},
                {kind: Whitespace, text: " "},
                {kind: LessLess, text: "<<"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "f"},
                {kind: Whitespace, text: " "},
                {kind: GreaterGreater, text: ">>"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "g"},
                {kind: Whitespace, text: " "},
                {kind: AsKeyword, text: "as"},
                {kind: Whitespace, text: " "},
                {kind: Uint32Keyword, text: "uint32"},
            ]
        "#]],
    )
}

#[test]
fn lexes_ref_builtins() {
    check(
        "r.set(a.get())",
        expect![[r#"
            [
                {kind: Ident, text: "r"},
                {kind: Dot, text: "."},
                {kind: Ident, text: "set"},
                {kind: LParen, text: "("},
                {kind: Ident, text: "a"},
                {kind: Dot, text: "."},
                {kind: Ident, text: "get"},
                {kind: LParen, text: "("},
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

#[test]
fn lexes_char_keyword() {
    check(
        "char",
        expect![[r#"
            [
                {kind: CharKeyword, text: "char"},
            ]
        "#]],
    );
}

#[test]
fn lexes_module_keywords() {
    check(
        "pub mod crate super self import array",
        expect![[r#"
            [
                {kind: PubKeyword, text: "pub"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "mod"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "crate"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "super"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "self"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "import"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "array"},
            ]
        "#]],
    );
}

#[test]
fn lexes_char_literal() {
    check(
        "let c = 'a'",
        expect![[r#"
            [
                {kind: LetKeyword, text: "let"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "c"},
                {kind: Whitespace, text: " "},
                {kind: Eq, text: "="},
                {kind: Whitespace, text: " "},
                {kind: CharLit, text: "'a'"},
            ]
        "#]],
    )
}

#[test]
fn lexes_separated_and_exponent_numbers() {
    check(
        "1_000 12_345i32 4_294_967_296u64 1.25e2 2_5.0_0e-1f32 1e6f64",
        expect![[r#"
            [
                {kind: Int, text: "1_000"},
                {kind: Whitespace, text: " "},
                {kind: Int32Lit, text: "12_345i32"},
                {kind: Whitespace, text: " "},
                {kind: UInt64Lit, text: "4_294_967_296u64"},
                {kind: Whitespace, text: " "},
                {kind: Float, text: "1.25e2"},
                {kind: Whitespace, text: " "},
                {kind: Float32Lit, text: "2_5.0_0e-1f32"},
                {kind: Whitespace, text: " "},
                {kind: Float64Lit, text: "1e6f64"},
            ]
        "#]],
    );
}

#[test]
fn keeps_tuple_projection_token_boundaries() {
    check(
        "export.1.to_string() signature.0.get(index)",
        expect![[r#"
            [
                {kind: Ident, text: "export"},
                {kind: Dot, text: "."},
                {kind: Int, text: "1"},
                {kind: Dot, text: "."},
                {kind: Ident, text: "to_string"},
                {kind: LParen, text: "("},
                {kind: RParen, text: ")"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "signature"},
                {kind: Dot, text: "."},
                {kind: Int, text: "0"},
                {kind: Dot, text: "."},
                {kind: Ident, text: "get"},
                {kind: LParen, text: "("},
                {kind: Ident, text: "index"},
                {kind: RParen, text: ")"},
            ]
        "#]],
    );
}

#[test]
fn lexes_underscore_prefixed_identifiers() {
    check(
        "let _x = _y + _;",
        expect![[r#"
            [
                {kind: LetKeyword, text: "let"},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "_x"},
                {kind: Whitespace, text: " "},
                {kind: Eq, text: "="},
                {kind: Whitespace, text: " "},
                {kind: Ident, text: "_y"},
                {kind: Whitespace, text: " "},
                {kind: Plus, text: "+"},
                {kind: Whitespace, text: " "},
                {kind: WildcardKeyword, text: "_"},
                {kind: Semi, text: ";"},
            ]
        "#]],
    )
}

fn assert_token_partition(input: &str) {
    let tokens = lex(input);
    let mut cursor = 0usize;

    for token in tokens {
        let start = u32::from(token.range.start()) as usize;
        let end = u32::from(token.range.end()) as usize;
        assert_eq!(start, cursor, "gap before {token:?} in {input:?}");
        assert!(end > start, "empty token {token:?} in {input:?}");
        assert!(input.is_char_boundary(start));
        assert!(input.is_char_boundary(end));
        assert_eq!(token.text, &input[start..end]);
        cursor = end;
    }

    assert_eq!(cursor, input.len(), "unlexed suffix in {input:?}");
}

fn next_noise(state: &mut u64) -> u64 {
    *state ^= *state << 13;
    *state ^= *state >> 7;
    *state ^= *state << 17;
    *state
}

#[test]
fn token_ranges_partition_generated_utf8_inputs() {
    let fragments = [
        "fn", "let", "::", "->", "=>", "&&", "||", "<<", ">>", "(", ")", "{", "}", "[", "]", "'",
        "\"", "\\", "//", "\n", "\r\n", "\t", "0", "9i32", "_name", "é", "中", "🦀", "\u{301}",
        "\0",
    ];
    let mut state = 0x4d59_5df4_d0f3_3173u64;

    for _ in 0..4096 {
        let count = (next_noise(&mut state) % 48) as usize;
        let mut input = String::new();
        for _ in 0..count {
            let index = (next_noise(&mut state) as usize) % fragments.len();
            input.push_str(fragments[index]);
        }
        assert_token_partition(&input);
    }
}

#[test]
fn token_ranges_partition_every_valid_prefix() {
    let input = "package main;\nfn main() -> unit {\n    let text = \"héllo 🦀\";\n    let code = '中';\n    // λ\n    text\n}\n";

    for end in input
        .char_indices()
        .map(|(index, _)| index)
        .chain(std::iter::once(input.len()))
    {
        assert_token_partition(&input[..end]);
    }
}
