use expect_test::{Expect, expect};
use std::path::Path;
use tempfile::tempdir;

use crate::query::{
    colon_colon_completions, dot_completions, hover_type, inlay_hints, signature_help,
    value_completions,
};

fn check(src: &str, line: u32, col: u32, expected: Expect) {
    let result = hover_type(Path::new("dummy"), src, line, col);
    expected.assert_debug_eq(&result.unwrap_or("<None>".to_string()));
}

fn check_completions(src: &str, line: u32, col: u32, expected: Expect) {
    let result = dot_completions(Path::new("dummy"), src, line, col).unwrap_or_default();
    expected.assert_debug_eq(&result);
}

fn check_completion_labels(src: &str, line: u32, col: u32, expected: Expect) {
    let result = dot_completions(Path::new("dummy"), src, line, col).unwrap_or_default();
    let mut labels = result.into_iter().map(|item| item.name).collect::<Vec<_>>();
    labels.sort();
    expected.assert_debug_eq(&labels);
}

fn check_completions_with_path(path: &Path, src: &str, line: u32, col: u32, expected: Expect) {
    let result = dot_completions(path, src, line, col).unwrap_or_default();
    expected.assert_debug_eq(&result);
}

fn check_colon_colon_completions(src: &str, line: u32, col: u32, expected: Expect) {
    let result = colon_colon_completions(Path::new("dummy"), src, line, col).unwrap_or_default();
    expected.assert_debug_eq(&result);
}

fn check_colon_colon_completion_labels(src: &str, line: u32, col: u32, expected: Expect) {
    let result = colon_colon_completions(Path::new("dummy"), src, line, col).unwrap_or_default();
    let mut labels = result.into_iter().map(|item| item.name).collect::<Vec<_>>();
    labels.sort();
    expected.assert_debug_eq(&labels);
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

fn check_value_completions(src: &str, line: u32, col: u32, expected: Expect) {
    let result = value_completions(Path::new("dummy"), src, line, col).unwrap_or_default();
    expected.assert_debug_eq(&result);
}

fn check_signature_help(src: &str, line: u32, col: u32, expected: Expect) {
    let result = signature_help(Path::new("dummy"), src, line, col);
    expected.assert_debug_eq(&result);
}

fn check_inlay_hints(src: &str, expected: Expect) {
    let result = inlay_hints(Path::new("dummy"), src).unwrap_or_default();
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
fn builtin_value_completions() {
    let src = r#"
fn main() {
    s
}
"#;

    check_value_completions(src, 2, 5, expect![[r#"
        [
            ValueCompletionItem {
                name: "string_get",
                kind: Function,
                detail: Some(
                    "(string, int32) -> char",
                ),
            },
            ValueCompletionItem {
                name: "string_hash",
                kind: Function,
                detail: Some(
                    "(string) -> uint64",
                ),
            },
            ValueCompletionItem {
                name: "string_len",
                kind: Function,
                detail: Some(
                    "(string) -> int32",
                ),
            },
            ValueCompletionItem {
                name: "string_print",
                kind: Function,
                detail: Some(
                    "(string) -> unit",
                ),
            },
            ValueCompletionItem {
                name: "string_println",
                kind: Function,
                detail: Some(
                    "(string) -> unit",
                ),
            },
        ]
    "#]]);
}

#[test]
#[rustfmt::skip]
fn user_value_completions() {
    let src = r#"
fn helper() -> int32 {
    42
}

fn main() {
    hel
}
"#;

    check_value_completions(src, 6, 7, expect![[r#"
        [
            ValueCompletionItem {
                name: "helper",
                kind: Function,
                detail: Some(
                    "() -> int32",
                ),
            },
        ]
    "#]]);
}

#[test]
#[rustfmt::skip]
fn signature_help_for_function_call() {
    let src = r#"
fn add(x: int32, y: string) -> bool {
    true
}

fn main() {
    let _ = add(1, 2);
}
"#;

    check_signature_help(src, 6, 16, expect![[r#"
        Some(
            SignatureHelpItem {
                label: "(int32, string) -> bool",
                parameters: [
                    "int32",
                    "string",
                ],
                active_parameter: 0,
            },
        )
    "#]]);

    check_signature_help(src, 6, 18, expect![[r#"
        Some(
            SignatureHelpItem {
                label: "(int32, string) -> bool",
                parameters: [
                    "int32",
                    "string",
                ],
                active_parameter: 1,
            },
        )
    "#]]);
}

#[test]
#[rustfmt::skip]
fn signature_help_for_method_call_hides_receiver() {
    let src = r#"
fn main() {
    let x = 1;
    let _ = x.to_string();
}
"#;

    check_signature_help(src, 3, 24, expect![[r#"
        Some(
            SignatureHelpItem {
                label: "() -> string",
                parameters: [],
                active_parameter: 0,
            },
        )
    "#]]);
}

#[test]
#[rustfmt::skip]
fn inlay_hints_for_let_bindings() {
    let src = r#"
fn main() {
    let x = 1;
    let y: int32 = 2;
    let _ = 3;
    ()
}
"#;

    check_inlay_hints(src, expect![[r#"
        [
            InlayHintItem {
                offset: 23,
                label: ": int32",
                kind: Type,
            },
        ]
    "#]]);
}

#[test]
#[rustfmt::skip]
fn inlay_hints_for_closure_params() {
    let src = r#"
fn main() {
    let f = |x| x + 1;
    ()
}
"#;

    check_inlay_hints(src, expect![[r#"
        [
            InlayHintItem {
                offset: 23,
                label: ": (int32) -> int32",
                kind: Type,
            },
            InlayHintItem {
                offset: 27,
                label: ": int32",
                kind: Type,
            },
        ]
    "#]]);
}

#[test]
#[rustfmt::skip]
fn hover_fn_params() {
    let src = r#"
fn f(x: int32, y: string) -> int32 { x }
"#;

    check(src, 1, 5, expect![[r#"
        "int32"
    "#]]);
    check(src, 1, 15, expect![[r#"
        "string"
    "#]]);
}

#[test]
#[rustfmt::skip]
fn hover_closure_params() {
    let src = r#"
fn main() {
    let f = |x| x + 1;
    ()
}
"#;

    check(src, 2, 13, expect![[r#"
        "int32"
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
fn builtin_vec_dot_method_completion() {
    let src = r#"
fn main() {
    let v: Vec[int32] = Vec::new();
    v.
}
"#;

    check_completion_labels(
        src,
        3,
        6,
        expect![[r#"
            [
                "get",
                "len",
                "new",
                "push",
            ]
        "#]],
    );
}

#[test]
#[rustfmt::skip]
fn builtin_hashmap_dot_method_completion() {
    let src = r#"
fn main() {
    let m: HashMap[string, int32] = HashMap::new();
    m.
}
"#;

    check_completion_labels(
        src,
        3,
        6,
        expect![[r#"
            [
                "contains",
                "get",
                "len",
                "new",
                "remove",
                "set",
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
fn builtin_vec_colon_colon_method_completion() {
    let src = r#"
fn main() {
    let _ = Vec::;
}
"#;

    check_colon_colon_completion_labels(
        src,
        2,
        17,
        expect![[r#"
            [
                "get",
                "len",
                "new",
                "push",
            ]
        "#]],
    );
}

#[test]
#[rustfmt::skip]
fn builtin_hashmap_colon_colon_method_completion() {
    let src = r#"
fn main() {
    let _ = HashMap::;
}
"#;

    check_colon_colon_completion_labels(
        src,
        2,
        21,
        expect![[r#"
            [
                "contains",
                "get",
                "len",
                "new",
                "remove",
                "set",
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

    let lib_src = r#"package Lib;

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

    let hover_src = r#"package Main;
use Lib;

fn main() {
    let f = Lib::color_to_int;
    let i = f(Lib::Color::Red);
    let _ = i;
    let p = Lib::Point { x: 1, y: 2 };
}
"#;
    let completion_src = r#"package Main;
use Lib;

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

    let lib_src = r#"package Lib;

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

    let src = r#"package Main;
use Lib;

fn main() {
    let _ = Lib::;
}
"#;
    let src_with_prefix = r#"package Main;
use Lib;

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
