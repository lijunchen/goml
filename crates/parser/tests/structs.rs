use expect_test::{Expect, expect};
use parser::{debug_tree, parse};
use std::collections::BTreeSet;
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
                      PATH@31..32
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
                PATH@0..6
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
fn struct_literal_expr_shorthand_field() {
    check(
        "Point { x, y }",
        expect![[r#"
            FILE@0..14
              EXPR_STRUCT_LITERAL@0..14
                PATH@0..6
                  Ident@0..5 "Point"
                  Whitespace@5..6 " "
                STRUCT_LITERAL_FIELD_LIST@6..14
                  LBrace@6..7 "{"
                  Whitespace@7..8 " "
                  STRUCT_LITERAL_FIELD@8..9
                    Ident@8..9 "x"
                  Comma@9..10 ","
                  Whitespace@10..11 " "
                  STRUCT_LITERAL_FIELD@11..13
                    Ident@11..12 "y"
                    Whitespace@12..13 " "
                  RBrace@13..14 "}""#]],
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
                      PATH@16..22
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
                      PATH@33..38
                        Ident@33..38 "point"
                    Semi@38..39 ";"
                    Whitespace@39..40 " "
                  RBrace@40..41 "}""#]],
    );
}

#[test]
fn package_and_trait_use() {
    check(
        "package main;\nuse example::math;\nuse math::Add;\npub fn main() -> unit { () }\n",
        expect![[r#"
            FILE@0..77
              PACKAGE@0..14
                PackageKeyword@0..7 "package"
                Whitespace@7..8 " "
                Ident@8..12 "main"
                Semi@12..13 ";"
                Whitespace@13..14 "\n"
              USE@14..33
                UseKeyword@14..17 "use"
                Whitespace@17..18 " "
                PATH@18..31
                  Ident@18..25 "example"
                  ColonColon@25..27 "::"
                  Ident@27..31 "math"
                Semi@31..32 ";"
                Whitespace@32..33 "\n"
              USE@33..48
                UseKeyword@33..36 "use"
                Whitespace@36..37 " "
                PATH@37..46
                  Ident@37..41 "math"
                  ColonColon@41..43 "::"
                  Ident@43..46 "Add"
                Semi@46..47 ";"
                Whitespace@47..48 "\n"
              FN@48..77
                PubKeyword@48..51 "pub"
                Whitespace@51..52 " "
                FnKeyword@52..54 "fn"
                Whitespace@54..55 " "
                Ident@55..59 "main"
                PARAM_LIST@59..62
                  LParen@59..60 "("
                  RParen@60..61 ")"
                  Whitespace@61..62 " "
                Arrow@62..64 "->"
                Whitespace@64..65 " "
                TYPE_UNIT@65..70
                  UnitKeyword@65..69 "unit"
                  Whitespace@69..70 " "
                BLOCK@70..77
                  LBrace@70..71 "{"
                  Whitespace@71..72 " "
                  EXPR_UNIT@72..75
                    LParen@72..73 "("
                    RParen@73..74 ")"
                    Whitespace@74..75 " "
                  RBrace@75..76 "}"
                  Whitespace@76..77 "\n""#]],
    );
}

#[test]
fn package_alias_paths_in_types_and_exprs() {
    check(
        "fn f(x: math::Thing) -> math::Other { math::make(x) }",
        expect![[r#"
            FILE@0..53
              FN@0..53
                FnKeyword@0..2 "fn"
                Whitespace@2..3 " "
                Ident@3..4 "f"
                PARAM_LIST@4..21
                  LParen@4..5 "("
                  PARAM@5..19
                    Ident@5..6 "x"
                    Colon@6..7 ":"
                    Whitespace@7..8 " "
                    TYPE_TAPP@8..19
                      PATH@8..19
                        Ident@8..12 "math"
                        ColonColon@12..14 "::"
                        Ident@14..19 "Thing"
                  RParen@19..20 ")"
                  Whitespace@20..21 " "
                Arrow@21..23 "->"
                Whitespace@23..24 " "
                TYPE_TAPP@24..36
                  PATH@24..36
                    Ident@24..28 "math"
                    ColonColon@28..30 "::"
                    Ident@30..35 "Other"
                    Whitespace@35..36 " "
                BLOCK@36..53
                  LBrace@36..37 "{"
                  Whitespace@37..38 " "
                  EXPR_CALL@38..52
                    EXPR_IDENT@38..48
                      PATH@38..48
                        Ident@38..42 "math"
                        ColonColon@42..44 "::"
                        Ident@44..48 "make"
                    ARG_LIST@48..52
                      LParen@48..49 "("
                      ARG@49..50
                        EXPR_IDENT@49..50
                          PATH@49..50
                            Ident@49..50 "x"
                      RParen@50..51 ")"
                      Whitespace@51..52 " "
                  RBrace@52..53 "}""#]],
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
fn import_keyword_reports_use_migration() {
    let path = Path::new("test.goml");
    let src = "import example::math;";
    let result = parse(path, src);
    assert!(result.has_errors());
    let errors = result.format_errors(src);
    assert!(
        errors
            .iter()
            .any(|message| message.contains("`import` has been removed; use `use` instead"))
    );
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

#[test]
fn parses_namespaced_expr_ident() {
    check(
        "foo::bar",
        expect![[r#"
            FILE@0..8
              EXPR_IDENT@0..8
                PATH@0..8
                  Ident@0..3 "foo"
                  ColonColon@3..5 "::"
                  Ident@5..8 "bar""#]],
    );
}

#[test]
fn parses_namespaced_type() {
    check(
        "fn main(x: foo::Bar) {}",
        expect![[r#"
            FILE@0..23
              FN@0..23
                FnKeyword@0..2 "fn"
                Whitespace@2..3 " "
                Ident@3..7 "main"
                PARAM_LIST@7..21
                  LParen@7..8 "("
                  PARAM@8..19
                    Ident@8..9 "x"
                    Colon@9..10 ":"
                    Whitespace@10..11 " "
                    TYPE_TAPP@11..19
                      PATH@11..19
                        Ident@11..14 "foo"
                        ColonColon@14..16 "::"
                        Ident@16..19 "Bar"
                  RParen@19..20 ")"
                  Whitespace@20..21 " "
                BLOCK@21..23
                  LBrace@21..22 "{"
                  RBrace@22..23 "}""#]],
    );
}

fn utf8_prefixes(input: &str) -> impl Iterator<Item = usize> + '_ {
    std::iter::once(0)
        .chain(input.char_indices().map(|(idx, _)| idx).skip(1))
        .chain(std::iter::once(input.len()))
}

fn sampled_prefixes(input: &str, max_points: usize) -> Vec<usize> {
    let boundaries: Vec<usize> = utf8_prefixes(input).collect();
    if boundaries.len() <= max_points {
        return boundaries;
    }

    let len = input.len();
    let mut points = BTreeSet::new();
    points.insert(0);
    points.insert(len);

    for point in boundaries.iter().take(32) {
        points.insert(*point);
    }
    for point in boundaries.iter().rev().take(32) {
        points.insert(*point);
    }

    let dense_slots = 64usize;
    for i in 0..dense_slots {
        let idx = i * (boundaries.len() - 1) / (dense_slots - 1);
        points.insert(boundaries[idx]);
    }

    for (idx, ch) in input.char_indices() {
        if matches!(
            ch,
            '\n' | '{'
                | '}'
                | '('
                | ')'
                | '['
                | ']'
                | ';'
                | ','
                | ':'
                | '.'
                | '#'
                | '|'
                | '&'
                | '+'
                | '-'
                | '*'
                | '/'
                | '<'
                | '>'
                | '='
                | '!'
                | '"'
                | '\''
        ) {
            points.insert(idx);
            points.insert((idx + ch.len_utf8()).min(len));
            if idx > 0 {
                let prev = input[..idx]
                    .char_indices()
                    .last()
                    .map(|(pos, _)| pos)
                    .unwrap_or(0);
                points.insert(prev);
            }
        }
    }

    let mut collected: Vec<usize> = points
        .into_iter()
        .filter(|point| input.is_char_boundary(*point))
        .collect();
    collected.sort_unstable();
    collected.dedup();

    if collected.len() <= max_points {
        return collected;
    }

    let mut reduced = BTreeSet::new();
    reduced.insert(0);
    reduced.insert(len);
    for i in 0..max_points {
        let idx = i * (collected.len() - 1) / (max_points - 1);
        reduced.insert(collected[idx]);
    }
    reduced.into_iter().collect()
}

fn assert_prefix_parsing_never_panics(case_name: &str, input: &str) {
    let path = Path::new("prefix_robustness.gom");
    for end in sampled_prefixes(input, 128) {
        let prefix = &input[..end];
        let result = std::panic::catch_unwind(|| {
            let parsed = parse(path, prefix);
            let _ = parsed.format_errors(prefix);
        });
        if let Err(payload) = result {
            let panic_message = if let Some(message) = payload.downcast_ref::<String>() {
                message.clone()
            } else if let Some(message) = payload.downcast_ref::<&str>() {
                message.to_string()
            } else {
                "non-string panic payload".to_string()
            };
            panic!(
                "parser panicked for case={case_name}, prefix_end={end}, panic={panic_message}, prefix={:?}",
                prefix
            );
        }
    }
}

#[test]
fn parser_handles_all_prefixes_of_hm_typechecker_without_panicking() {
    let input = include_str!("../../compiler/src/tests/pipeline/080_hm_typechecker/main.gom");
    assert_prefix_parsing_never_panics("pipeline_080_hm_typechecker", input);
}

#[test]
fn parser_handles_tricky_inputs_without_panicking() {
    let cases = [
        (
            "unterminated_string_and_block",
            "fn main() { let msg = \"hello\\nworld; if true { msg }",
        ),
        (
            "unterminated_char_and_tuple_pattern",
            "fn main() { let (a, b, _) = (1, 2, 3); let c = '\\u12",
        ),
        (
            "attribute_and_extern_mixture",
            "#[derive(ToString)] #[cfg(test) fn foo[T: Eq + Hash](x: T) -> dyn Show {",
        ),
        (
            "dense_operators_and_partial_tokens",
            "fn main(){let x=1<<<<=>>>==!=&&||::..,,;;",
        ),
        (
            "deep_nesting",
            "fn main() { (((((((((((((((((((((1))))))))))))))))))))) }",
        ),
        (
            "multiline_string_prefixes",
            "fn main() { let s = \\\\line1\\n  \\\\line2\\n    \\\\line3\\nlet x = 1; }",
        ),
        (
            "invalid_tokens_and_escape_like_sequence",
            "fn main() { let y = \\u2028; @@@ }",
        ),
        (
            "unbalanced_generics_and_trait_bounds",
            "trait T { fn f(self: Self) -> Self; } fn id[T: A + B +](x: T) -> T { x }",
        ),
    ];

    for (case_name, input) in cases {
        assert_prefix_parsing_never_panics(case_name, input);
    }
}

fn assert_recovers_and_parses_following_fn(input: &str, following_fn_name: &str) {
    let path = Path::new("recovery.gom");
    let result = parse(path, input);
    assert!(
        result.has_errors(),
        "expected parse errors for malformed input"
    );

    let tree = debug_tree(&result.green_node);
    let fn_count = tree.matches("FN@").count();
    assert!(
        fn_count >= 2,
        "expected parser to recover and parse at least two top-level functions, got {fn_count}\n{tree}"
    );
    assert!(
        tree.contains(&format!("\"{following_fn_name}\"")),
        "expected recovered tree to contain function name `{following_fn_name}`\n{tree}"
    );
}

#[test]
fn missing_block_rbrace_does_not_block_following_top_level_fn() {
    let input = r#"fn broken() {
    let x = 1;

fn after() {
    let y = 2;
}
"#;
    assert_recovers_and_parses_following_fn(input, "after");
}

#[test]
fn missing_param_rparen_does_not_block_following_top_level_fn() {
    let input = r#"fn broken(x: int32 {
    let x = 1;
}

fn after() {
    let y = 2;
}
"#;
    assert_recovers_and_parses_following_fn(input, "after");
}
