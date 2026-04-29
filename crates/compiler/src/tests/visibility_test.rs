use std::fs;
use std::path::PathBuf;

use diagnostics::Diagnostics;
use tempfile::TempDir;

use crate::pipeline::pipeline::{self, CompilationError};

fn write_project(files: &[(&str, &str)]) -> (TempDir, PathBuf, String) {
    let dir = tempfile::tempdir().unwrap();
    fs::write(
        dir.path().join("goml.toml"),
        r#"[crate]
name = "visibility_test"
kind = "bin"
root = "main.gom"
"#,
    )
    .unwrap();
    for (path, src) in files {
        let path = dir.path().join(path);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).unwrap();
        }
        fs::write(path, src).unwrap();
    }
    let main_path = dir.path().join("main.gom");
    let main_src = fs::read_to_string(&main_path).unwrap();
    (dir, main_path, main_src)
}

fn typecheck(files: &[(&str, &str)]) -> Result<Diagnostics, CompilationError> {
    let (_dir, main_path, main_src) = write_project(files);
    pipeline::typecheck_with_packages(&main_path, &main_src)
        .map(|(_tast, _genv, diagnostics)| diagnostics)
}

fn assert_ok(files: &[(&str, &str)]) {
    match typecheck(files) {
        Ok(diagnostics) => assert!(!diagnostics.has_errors(), "{diagnostics:#?}"),
        Err(error) => panic!("{:#?}", error.diagnostics()),
    }
}

fn assert_err(files: &[(&str, &str)]) {
    match typecheck(files) {
        Ok(diagnostics) => assert!(diagnostics.has_errors(), "expected visibility error"),
        Err(error) => assert!(
            error.diagnostics().has_errors(),
            "{:#?}",
            error.diagnostics()
        ),
    }
}

fn assert_err_contains(files: &[(&str, &str)], expected: &str) {
    let diagnostics = match typecheck(files) {
        Ok(diagnostics) => diagnostics,
        Err(error) => error.diagnostics().clone(),
    };
    let messages = diagnostics
        .iter()
        .map(|diagnostic| diagnostic.message().to_string())
        .collect::<Vec<_>>();
    assert!(
        messages.iter().any(|message| message.contains(expected)),
        "{messages:#?}"
    );
}

#[test]
fn public_function_is_visible() {
    assert_ok(&[
        (
            "main.gom",
            r#"
mod api;

fn main() -> unit {
    let _ = crate::api::answer();
}
"#,
        ),
        (
            "api/mod.gom",
            r#"
pub fn answer() -> int32 {
    42
}
"#,
        ),
    ]);
}

#[test]
fn private_function_is_hidden() {
    assert_err(&[
        (
            "main.gom",
            r#"
mod api;

fn main() -> unit {
    let _ = crate::api::secret();
}
"#,
        ),
        (
            "api/mod.gom",
            r#"
fn secret() -> int32 {
    7
}
"#,
        ),
    ]);
}

#[test]
fn private_function_is_hidden_from_sibling_module() {
    assert_err(&[
        (
            "main.gom",
            r#"
mod internal;
pub mod api;

fn main() -> unit {
    let _ = crate::api::call();
}
"#,
        ),
        (
            "internal.gom",
            r#"
fn hidden() -> int64 {
    1
}

pub fn exposed_inside_crate() -> int64 {
    2
}
"#,
        ),
        (
            "api.gom",
            r#"
pub fn call() -> int64 {
    crate::internal::hidden()
}
"#,
        ),
    ]);
}

#[test]
fn private_function_can_feed_public_function() {
    assert_ok(&[
        (
            "main.gom",
            r#"
mod api;

fn main() -> unit {
    let _ = crate::api::answer();
}
"#,
        ),
        (
            "api/mod.gom",
            r#"
fn secret() -> int32 {
    35
}

pub fn answer() -> int32 {
    secret() + 7
}
"#,
        ),
    ]);
}

#[test]
fn crate_path_resolves_module_value() {
    assert_ok(&[
        (
            "main.gom",
            r#"
mod math;

fn main() -> unit {
    let _ = crate::math::add(1, 2);
}
"#,
        ),
        (
            "math.gom",
            r#"
pub fn add(a: int64, b: int64) -> int64 {
    a + b
}
"#,
        ),
    ]);
}

#[test]
fn use_path_imports_module_value() {
    assert_ok(&[
        (
            "main.gom",
            r#"
mod math;

use crate::math::add;

fn main() -> unit {
    let _ = add(1, 2);
}
"#,
        ),
        (
            "math.gom",
            r#"
pub fn add(a: int64, b: int64) -> int64 {
    a + b
}
"#,
        ),
    ]);
}

#[test]
fn value_use_is_module_local() {
    assert_err_contains(
        &[
            (
                "main.gom",
                r#"
mod math;
mod a;
mod b;

fn main() -> unit {
    let _ = crate::a::call();
}
"#,
            ),
            (
                "math.gom",
                r#"
pub fn add(a: int64, b: int64) -> int64 {
    a + b
}
"#,
            ),
            (
                "a.gom",
                r#"
use crate::math::add;

pub fn call() -> int64 {
    add(1, 2)
}
"#,
            ),
            (
                "b.gom",
                r#"
pub fn call() -> int64 {
    add(3, 4)
}
"#,
            ),
        ],
        "Unresolved callee add",
    );
}

#[test]
fn local_binding_shadows_value_use() {
    assert_ok(&[
        (
            "main.gom",
            r#"
mod math;

use crate::math::add;

fn shadow(add: int64) -> int64 {
    add
}

fn main() -> unit {
    let _ = shadow(7);
}
"#,
        ),
        (
            "math.gom",
            r#"
pub fn add(a: int64, b: int64) -> int64 {
    a + b
}
"#,
        ),
    ]);
}

#[test]
fn self_path_resolves_current_module_value() {
    assert_ok(&[(
        "main.gom",
        r#"
fn helper() -> int64 {
    1
}

fn f() -> int64 {
    self::helper()
}

fn main() -> unit {
    let _ = f();
}
"#,
    )]);
}

#[test]
fn public_struct_is_visible() {
    assert_ok(&[
        (
            "main.gom",
            r#"
mod api;

fn main() -> unit {
    let p = crate::api::Point { x: 1 };
    let _ = p.x;
}
"#,
        ),
        (
            "api/mod.gom",
            r#"
pub struct Point {
    x: int32,
}
"#,
        ),
    ]);
}

#[test]
fn private_struct_is_hidden() {
    assert_err(&[
        (
            "main.gom",
            r#"
mod api;

fn main() -> unit {
    let _ = crate::api::Secret { x: 1 };
}
"#,
        ),
        (
            "api/mod.gom",
            r#"
struct Secret {
    x: int32,
}
"#,
        ),
    ]);
}

#[test]
fn public_enum_is_visible() {
    assert_ok(&[
        (
            "main.gom",
            r#"
mod api;

fn main() -> unit {
    let _ = crate::api::Choice::B(1);
}
"#,
        ),
        (
            "api/mod.gom",
            r#"
pub enum Choice {
    A,
    B(int32),
}
"#,
        ),
    ]);
}

#[test]
fn private_enum_is_hidden() {
    assert_err(&[
        (
            "main.gom",
            r#"
mod api;

fn main() -> unit {
    let _ = crate::api::Hidden::A;
}
"#,
        ),
        (
            "api/mod.gom",
            r#"
enum Hidden {
    A,
}
"#,
        ),
    ]);
}

#[test]
fn public_trait_import_enables_method_syntax() {
    assert_ok(&[
        (
            "main.gom",
            r#"
mod api;

use crate::api::Label;

fn main() -> unit {
    let item = crate::api::item();
    let _ = item.label();
}
"#,
        ),
        (
            "api/mod.gom",
            r#"
pub trait Label {
    fn label(Self) -> string;
}

pub struct Item {
    value: string,
}

impl Label for Item {
    fn label(self: Item) -> string {
        self.value
    }
}

pub fn item() -> Item {
    Item { value: "ok" }
}
"#,
        ),
    ]);
}

#[test]
fn trait_method_syntax_requires_trait_import() {
    assert_err(&[
        (
            "main.gom",
            r#"
mod api;

fn main() -> unit {
    let item = crate::api::item();
    let _ = item.label();
}
"#,
        ),
        (
            "api/mod.gom",
            r#"
pub trait Label {
    fn label(Self) -> string;
}

pub struct Item {
    value: string,
}

impl Label for Item {
    fn label(self: Item) -> string {
        self.value
    }
}

pub fn item() -> Item {
    Item { value: "ok" }
}
"#,
        ),
    ]);
}

#[test]
fn private_trait_import_is_hidden() {
    assert_err(&[
        (
            "main.gom",
            r#"
mod api;

use crate::api::Hidden;

fn main() -> unit {
    let item = crate::api::item();
    let _ = item.hidden();
}
"#,
        ),
        (
            "api/mod.gom",
            r#"
trait Hidden {
    fn hidden(Self) -> string;
}

pub struct Item {
    value: string,
}

impl Hidden for Item {
    fn hidden(self: Item) -> string {
        self.value
    }
}

pub fn item() -> Item {
    Item { value: "ok" }
}
"#,
        ),
    ]);
}

#[test]
fn public_extern_items_are_visible() {
    assert_ok(&[
        (
            "main.gom",
            r#"
mod api;

fn main() -> unit {
    let _ = crate::api::new_reader("ok");
}
"#,
        ),
        (
            "api/mod.gom",
            r#"
pub extern "go" "strings" "*Reader" type Reader
pub extern "go" "strings" "NewReader" new_reader(text: string) -> Reader
"#,
        ),
    ]);
}

#[test]
fn private_extern_function_is_hidden() {
    assert_err(&[
        (
            "main.gom",
            r#"
mod api;

fn main() -> unit {
    let _ = crate::api::new_reader("ok");
}
"#,
        ),
        (
            "api/mod.gom",
            r#"
pub extern "go" "strings" "*Reader" type Reader
extern "go" "strings" "NewReader" new_reader(text: string) -> Reader
"#,
        ),
    ]);
}
