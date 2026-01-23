use expect_test::{Expect, expect};
use std::path::Path;
use tempfile::tempdir;

use crate::query::{colon_colon_completions, dot_completions, hover_type};

fn check(src: &str, line: u32, col: u32, expected: Expect) {
    let result = hover_type(Path::new("dummy"), src, line, col);
    expected.assert_debug_eq(&result.unwrap_or("<None>".to_string()));
}

fn check_completions(src: &str, line: u32, col: u32, expected: Expect) {
    let result = dot_completions(Path::new("dummy"), src, line, col).unwrap_or_default();
    expected.assert_debug_eq(&result);
}

fn check_completions_with_path(path: &Path, src: &str, line: u32, col: u32, expected: Expect) {
    let result = dot_completions(path, src, line, col).unwrap_or_default();
    expected.assert_debug_eq(&result);
}

fn check_colon_colon_completions(src: &str, line: u32, col: u32, expected: Expect) {
    let result = colon_colon_completions(Path::new("dummy"), src, line, col).unwrap_or_default();
    expected.assert_debug_eq(&result);
}

fn check_colon_colon_completions_with_path(
    path: &Path,
    src: &str,
    line: u32,
    col: u32,
    expected: Expect,
) {
    let result = colon_colon_completions(path, src, line, col).unwrap_or_default();
    expected.assert_debug_eq(&result);
}

fn check_with_path(path: &Path, src: &str, line: u32, col: u32, expected: Expect) {
    let result = hover_type(path, src, line, col);
    expected.assert_debug_eq(&result.unwrap_or("<None>".to_string()));
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
fn hover_literals() {
    let src = r#"
fn main() {
    let _ = 1;
    let _ = true;
    let _ = "x";
}
"#;

    check(src, 2, 12, expect![[r#"
        "int32"
    "#]]);
    check(src, 3, 12, expect![[r#"
        "bool"
    "#]]);
    check(src, 4, 12, expect![[r#"
        "string"
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
fn struct_field_completion_with_prefix() {
    let src = r#"
struct Point {
    x: int32,
    y: int32,
}

fn main() {
    let p = Point { x: 1, y: 2 };
    let _ = p.x;
}
"#;

    check_completions(
        src,
        8,
        15,
        expect![[r#"
            [
                DotCompletionItem {
                    name: "x",
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

#[test]
#[rustfmt::skip]
fn enum_variant_completion() {
    let src = r#"enum Color { Red, Green }

fn main() {
    let _ = Color::;
}
"#;

    check_colon_colon_completions(
        src,
        3,
        19,
        expect![[r#"
            [
                ColonColonCompletionItem {
                    name: "Green",
                    kind: Variant,
                    detail: Some(
                        "Color",
                    ),
                },
                ColonColonCompletionItem {
                    name: "Red",
                    kind: Variant,
                    detail: Some(
                        "Color",
                    ),
                },
            ]
        "#]],
    );
}

#[test]
#[rustfmt::skip]
fn multi_package_query() {
    let dir = tempdir().unwrap();
    let lib_dir = dir.path().join("Lib");
    std::fs::create_dir_all(&lib_dir).unwrap();

    let lib_src = r#"package Lib

enum Color {
    Red,
    Green,
}

struct Point {
    x: int32,
    y: int32,
}

fn color_to_int(c: Color) -> int32 {
    match c {
        Color::Red => 1,
        Color::Green => 2,
    }
}
"#;
    let lib_path = lib_dir.join("main.gom");
    std::fs::write(&lib_path, lib_src).unwrap();

    let hover_src = r#"package Main
import Lib

fn main() {
    let f = Lib::color_to_int;
    let i = f(Lib::Color::Red);
    let _ = i;
    let p = Lib::Point { x: 1, y: 2 };
}
"#;
    let completion_src = r#"package Main
import Lib

fn main() {
    let f = Lib::color_to_int;
    let i = f(Lib::Color::Red);
    let _ = i;
    let p = Lib::Point { x: 1, y: 2 };
    p.
}
"#;
    let main_path = dir.path().join("main.gom");
    std::fs::write(&main_path, hover_src).unwrap();

    check_with_path(&main_path, hover_src, 4, 17, expect![[r#"
        "(Lib::Color) -> int32"
    "#]]);

    check_completions_with_path(
        &main_path,
        completion_src,
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
fn multi_package_colon_colon_completions() {
    let dir = tempdir().unwrap();
    let lib_dir = dir.path().join("Lib");
    std::fs::create_dir_all(&lib_dir).unwrap();

    let lib_src = r#"package Lib

enum Color {
    Red,
    Green,
}

struct Point {
    x: int32,
    y: int32,
}

fn color_to_int(c: Color) -> int32 {
    match c {
        Color::Red => 1,
        Color::Green => 2,
    }
}
"#;
    let lib_path = lib_dir.join("main.gom");
    std::fs::write(&lib_path, lib_src).unwrap();

    let src = r#"package Main
import Lib

fn main() {
    let _ = Lib::;
}
"#;
    let src_with_prefix = r#"package Main
import Lib

fn main() {
    let _ = Lib::co;
}
"#;

    let main_path = dir.path().join("main.gom");
    std::fs::write(&main_path, src).unwrap();

    check_colon_colon_completions_with_path(
        &main_path,
        src,
        4,
        17,
        expect![[r#"
            [
                ColonColonCompletionItem {
                    name: "Color",
                    kind: Type,
                    detail: Some(
                        "enum",
                    ),
                },
                ColonColonCompletionItem {
                    name: "Point",
                    kind: Type,
                    detail: Some(
                        "struct",
                    ),
                },
                ColonColonCompletionItem {
                    name: "color_to_int",
                    kind: Value,
                    detail: Some(
                        "fn",
                    ),
                },
            ]
        "#]],
    );

    check_colon_colon_completions_with_path(
        &main_path,
        src_with_prefix,
        4,
        19,
        expect![[r#"
            [
                ColonColonCompletionItem {
                    name: "color_to_int",
                    kind: Value,
                    detail: Some(
                        "fn",
                    ),
                },
            ]
        "#]],
    );
}
