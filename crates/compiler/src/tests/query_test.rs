use expect_test::{Expect, expect};
use std::path::Path;
use std::sync::{Mutex, OnceLock};
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

fn check_value_completions_with_path(
    path: &Path,
    src: &str,
    line: u32,
    col: u32,
    expected: Expect,
) {
    let result = value_completions(path, src, line, col).unwrap_or_default();
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

fn env_lock() -> &'static Mutex<()> {
    static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    LOCK.get_or_init(|| Mutex::new(()))
}

fn with_goml_home<T>(home: &Path, f: impl FnOnce() -> T) -> T {
    let _guard = env_lock().lock().unwrap();
    let previous = std::env::var_os("GOML_HOME");
    unsafe {
        std::env::set_var("GOML_HOME", home);
    }
    let result = f();
    match previous {
        Some(value) => unsafe {
            std::env::set_var("GOML_HOME", value);
        },
        None => unsafe {
            std::env::remove_var("GOML_HOME");
        },
    }
    result
}

fn write_cached_registry(home: &Path) {
    let registry = home.join("cache/registry");
    std::fs::create_dir_all(registry.join("alice/http/1.2.0/client")).unwrap();
    std::fs::write(
        registry.join("index.toml"),
        r#"[modules."alice::http"]
latest = "1.2.0"
versions = ["1.2.0"]
"#,
    )
    .unwrap();
    std::fs::write(
        registry.join("alice/http/1.2.0/goml.toml"),
        r#"[crate]
name = "http"
kind = "lib"
root = "lib.gom"
"#,
    )
    .unwrap();
    std::fs::write(
        registry.join("alice/http/1.2.0/lib.gom"),
        r#"
mod client;

use crate::client;

fn make_client() -> client::Client {
    client::Client { name: "alice" }
}
"#,
    )
    .unwrap();
    std::fs::write(
        registry.join("alice/http/1.2.0/client/mod.gom"),
        r#"

struct Client {
    name: string,
}

fn tag() -> string {
    "client"
}
"#,
    )
    .unwrap();
}

#[test]
#[rustfmt::skip]
fn use_statement_package_completions() {
    let dir = tempdir().unwrap();
    let home = dir.path().join(".goml");
    write_cached_registry(&home);

    std::fs::create_dir_all(dir.path().join("util")).unwrap();
    std::fs::write(
        dir.path().join("util/mod.gom"),
        r#"

fn ping() -> string {
    "pong"
}
"#,
    )
    .unwrap();

    std::fs::write(
        dir.path().join("goml.toml"),
        r#"[crate]
name = "demo"
kind = "bin"
root = "main.gom"

[dependencies]
"alice::http" = "1.2.0"
"#,
    )
    .unwrap();

    let src = r#"

use 

fn main() -> unit {
    ()
}
"#;
    let main_path = dir.path().join("main.gom");
    std::fs::write(&main_path, src).unwrap();

    with_goml_home(&home, || {
        check_value_completions_with_path(
            &main_path,
            src,
            2,
            4,
            expect![[r#"
                [
                    ValueCompletionItem {
                        name: "alice::http",
                        kind: Package,
                        detail: Some(
                            "package",
                        ),
                    },
                    ValueCompletionItem {
                        name: "util",
                        kind: Package,
                        detail: Some(
                            "package",
                        ),
                    },
                ]
            "#]],
        );
    });
}

#[test]
#[rustfmt::skip]
fn imported_package_value_completions() {
    let dir = tempdir().unwrap();
    let home = dir.path().join(".goml");
    write_cached_registry(&home);

    std::fs::create_dir_all(dir.path().join("util")).unwrap();
    std::fs::write(
        dir.path().join("util/mod.gom"),
        r#"

fn ping() -> string {
    "pong"
}
"#,
    )
    .unwrap();

    std::fs::write(
        dir.path().join("goml.toml"),
        r#"[crate]
name = "demo"
kind = "bin"
root = "main.gom"

[dependencies]
"alice::http" = "1.2.0"
"#,
    )
    .unwrap();

    let src = r#"
mod util;

use crate::util;
use alice::http;
use alice::http::client;

fn main() -> unit {
    ut
    ht
    cl
}
"#;
    let main_path = dir.path().join("main.gom");
    std::fs::write(&main_path, src).unwrap();

    with_goml_home(&home, || {
        check_value_completions_with_path(
            &main_path,
            src,
            8,
            6,
            expect![[r#"
                [
                    ValueCompletionItem {
                        name: "util",
                        kind: Package,
                        detail: Some(
                            "package",
                        ),
                    },
                ]
            "#]],
        );
        check_value_completions_with_path(
            &main_path,
            src,
            9,
            6,
            expect![[r#"
                [
                    ValueCompletionItem {
                        name: "http",
                        kind: Package,
                        detail: Some(
                            "package",
                        ),
                    },
                ]
            "#]],
        );
        check_value_completions_with_path(
            &main_path,
            src,
            10,
            6,
            expect![[r#"
                [
                    ValueCompletionItem {
                        name: "client",
                        kind: Package,
                        detail: Some(
                            "package",
                        ),
                    },
                ]
            "#]],
        );
    });
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
fn hover_on_minimal_function_name() {
    let src = r#"

fn main() {
}
"#;

    check(src, 0, 9, expect![[r#"
        "() -> unit"
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
                name: "slice",
                kind: Function,
                detail: Some(
                    "(Vec[T], int32, int32) -> Slice[T]",
                ),
            },
            ValueCompletionItem {
                name: "slice_get",
                kind: Function,
                detail: Some(
                    "(Slice[T], int32) -> T",
                ),
            },
            ValueCompletionItem {
                name: "slice_len",
                kind: Function,
                detail: Some(
                    "(Slice[T]) -> int32",
                ),
            },
            ValueCompletionItem {
                name: "slice_sub",
                kind: Function,
                detail: Some(
                    "(Slice[T], int32, int32) -> Slice[T]",
                ),
            },
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
            ValueCompletionItem {
                name: "string",
                kind: Keyword,
                detail: None,
            },
            ValueCompletionItem {
                name: "struct",
                kind: Keyword,
                detail: None,
            },
            ValueCompletionItem {
                name: "super",
                kind: Keyword,
                detail: None,
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
fn local_value_completions() {
    let src = r#"
fn main() {
    let count = 1;
    cou
}
"#;

    check_value_completions(src, 3, 7, expect![[r#"
        [
            ValueCompletionItem {
                name: "count",
                kind: Variable,
                detail: Some(
                    "int32",
                ),
            },
        ]
    "#]]);
}

#[test]
#[rustfmt::skip]
fn call_argument_value_completions_are_empty_without_prefix() {
    let src = r#"
fn takes(count: int32, label: string) -> unit {
    ()
}

fn main() {
    let count = 1;
    let label = "ok";
    takes(
    )
}
"#;

    check_value_completions(src, 8, 10, expect![[r#"
        []
    "#]]);
}

#[test]
#[rustfmt::skip]
fn call_argument_value_completions_with_prefix() {
    let src = r#"
fn takes(count: int32, label: string) -> unit {
    ()
}

fn main() {
    let count = 1;
    let label = "ok";
    takes(count, la)
}
"#;

    check_value_completions(src, 8, 19, expect![[r#"
        [
            ValueCompletionItem {
                name: "label",
                kind: Variable,
                detail: Some(
                    "string",
                ),
            },
        ]
    "#]]);
}

#[test]
#[rustfmt::skip]
fn keyword_value_completions() {
    let src = r#"
fn main() {
    le
}
"#;

    check_value_completions(src, 2, 6, expect![[r#"
        [
            ValueCompletionItem {
                name: "let",
                kind: Keyword,
                detail: None,
            },
        ]
    "#]]);
}

#[test]
#[rustfmt::skip]
fn keyword_and_function_value_completions() {
    let src = r#"
fn map() -> int32 {
    1
}

fn main() {
    ma
}
"#;

    check_value_completions(src, 6, 6, expect![[r#"
        [
            ValueCompletionItem {
                name: "main",
                kind: Function,
                detail: Some(
                    "() -> unit",
                ),
            },
            ValueCompletionItem {
                name: "map",
                kind: Function,
                detail: Some(
                    "() -> int32",
                ),
            },
            ValueCompletionItem {
                name: "match",
                kind: Keyword,
                detail: None,
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
                label: "(x: int32, y: string) -> bool",
                parameters: [
                    "x: int32",
                    "y: string",
                ],
                active_parameter: 0,
            },
        )
    "#]]);

    check_signature_help(src, 6, 18, expect![[r#"
        Some(
            SignatureHelpItem {
                label: "(x: int32, y: string) -> bool",
                parameters: [
                    "x: int32",
                    "y: string",
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
                "set",
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
fn builtin_slice_dot_method_completion() {
    let src = r#"
fn main() {
    let v: Vec[int32] = Vec::new();
    let v = v.push(1);
    let v = v.push(2);
    let s: Slice[int32] = slice(v, 0, 2);
    s.
}
"#;

    check_completion_labels(
        src,
        6,
        6,
        expect![[r#"
            [
                "get",
                "len",
                "sub",
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
                "set",
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
fn builtin_slice_colon_colon_method_completion() {
    let src = r#"
fn main() {
    let _ = Slice::;
}
"#;

    check_colon_colon_completion_labels(
        src,
        2,
        19,
        expect![[r#"
            [
                "get",
                "len",
                "sub",
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

    let lib_src = r#"

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

    let hover_src = r#"
use Lib;

fn main() {
    let f = Lib::color_to_int;
    let i = f(Lib::Color::Red);
    let _ = i;
    let p = Lib::Point { x: 1, y: 2 };
}
"#;
    let completion_src = r#"
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
fn multi_package_inherent_method_completion() {
    let dir = tempdir().unwrap();
    let lib_dir = dir.path().join("Lib");
    std::fs::create_dir_all(&lib_dir).unwrap();

    let lib_src = r#"

struct Item {
    value: int32,
}

fn make(value: int32) -> Item {
    Item { value: value }
}

impl Item {
    fn text(self: Item) -> string {
        int32_to_string(self.value)
    }

    fn touch(self: Item) -> unit {
        ()
    }
}
"#;
    let lib_path = lib_dir.join("main.gom");
    std::fs::write(&lib_path, lib_src).unwrap();

    let src = r#"
use Lib;

fn main() {
    let item = Lib::make(1);
    item.
}
"#;
    let main_path = dir.path().join("main.gom");
    std::fs::write(&main_path, src).unwrap();

    check_completions_with_path(
        &main_path,
        src,
        5,
        9,
        expect![[r#"
            [
                DotCompletionItem {
                    name: "value",
                    kind: Field,
                    detail: Some(
                        "int32",
                    ),
                },
                DotCompletionItem {
                    name: "text",
                    kind: Method,
                    detail: Some(
                        "(Lib::Item) -> string",
                    ),
                },
                DotCompletionItem {
                    name: "touch",
                    kind: Method,
                    detail: Some(
                        "(Lib::Item) -> unit",
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

    let lib_src = r#"

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

    let src = r#"
use Lib;

fn main() {
    let _ = Lib::;
}
"#;
    let src_with_prefix = r#"
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

#[test]
#[rustfmt::skip]
fn registry_dependency_hover_and_completion() {
    let dir = tempdir().unwrap();
    let home = dir.path().join(".goml");
    write_cached_registry(&home);

    let main_path = dir.path().join("main.gom");
    let valid_src = r#"

use alice::http;
use alice::http::client;

fn main() -> unit {
    let client = http::make_client();
    let _ = client.name;
}
"#;
    let namespace_src = r#"

use alice::http;
use alice::http::client;

fn main() -> unit {
    let _ = http::;
}
"#;
    let nested_namespace_src = r#"

use alice::http;
use alice::http::client;

fn main() -> unit {
    let _ = client::;
}
"#;
    let use_namespace_src = r#"

use alice::http::

fn main() -> unit {
    ()
}
"#;
    std::fs::write(
        dir.path().join("goml.toml"),
        r#"[crate]
name = "demo"
kind = "bin"
root = "main.gom"

[dependencies]
"alice::http" = "1.2.0"
"#,
    )
    .unwrap();
    std::fs::write(&main_path, valid_src).unwrap();

    with_goml_home(&home, || {
        let (_hir_table, _results, genv, _diagnostics) =
            crate::pipeline::pipeline::typecheck_with_packages_and_results(&main_path, valid_src)
                .unwrap();
        assert!(
            genv.structs()
                .contains_key(&crate::tast::TastIdent("client::Client".to_string()))
        );

        check_colon_colon_completions_with_path(
            &main_path,
            namespace_src,
            6,
            18,
            expect![[r#"
                [
                    ColonColonCompletionItem {
                        name: "make_client",
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
            nested_namespace_src,
            6,
            20,
            expect![[r#"
                [
                    ColonColonCompletionItem {
                        name: "Client",
                        kind: Type,
                        detail: Some(
                            "struct",
                        ),
                    },
                    ColonColonCompletionItem {
                        name: "tag",
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
            use_namespace_src,
            2,
            17,
            expect![[r#"
                [
                    ColonColonCompletionItem {
                        name: "client",
                        kind: Package,
                        detail: Some(
                            "package",
                        ),
                    },
                    ColonColonCompletionItem {
                        name: "make_client",
                        kind: Value,
                        detail: Some(
                            "fn",
                        ),
                    },
                ]
            "#]],
        );
    });
}
