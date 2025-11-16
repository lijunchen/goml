use expect_test::{Expect, expect};

use crate::query::{dot_completions, hover_type};

fn check(src: &str, line: u32, col: u32, expected: Expect) {
    let result = hover_type(src, line, col);
    expected.assert_debug_eq(&result.unwrap_or("<None>".to_string()));
}

fn check_completions(src: &str, line: u32, col: u32, expected: Expect) {
    let result = dot_completions(src, line, col).unwrap_or_default();
    expected.assert_debug_eq(&result);
}

#[test]
#[rustfmt::skip]
fn smoke_test() {
    let src = r#"enum Color { Red, Green, Blue }

fn main() {
    let a = 1;
    let a = (true, 2);
    let a = Green;
    ()
}
"#;

    check(src, 3, 8, expect![[r#"
        "int32"
    "#]]);
    check(src, 3, 9, expect![[r#"
        "int32"
    "#]]);
    check(src, 3, 10, expect![[r#"
        "<None>"
    "#]]);

    check(src, 4, 8, expect![[r#"
        "(bool, int32)"
    "#]]);

    check(src, 5, 8, expect![[r#"
        "Color"
    "#]]);
}

#[test]
#[rustfmt::skip]
fn struct_field_completions() {
    let src = r#"
struct Point {
    x: int32,
    y: int32,
}

fn main() {
    let p = Point { x: 1, y: 2 };
    p.
}
"#;

    check_completions(
        src,
        8,
        6,
        expect![[r#"
            [
                DotCompletionItem {
                    name: "x",
                    kind: Field,
                    detail: Some(
                        "int32",
                    ),
                },
                DotCompletionItem {
                    name: "y",
                    kind: Field,
                    detail: Some(
                        "int32",
                    ),
                },
            ]
        "#]],
    );
}

#[test]
#[rustfmt::skip]
fn inherent_method_completion() {
    let src = r#"
fn main() {
    let value = 3;
    value.
}
"#;

    check_completions(
        src,
        3,
        10,
        expect![[r#"
            [
                DotCompletionItem {
                    name: "to_string",
                    kind: Method,
                    detail: Some(
                        "(int32) -> string",
                    ),
                },
            ]
        "#]],
    );
}
