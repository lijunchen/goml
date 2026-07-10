use std::fs;
use std::path::PathBuf;

use diagnostics::Diagnostics;
use tempfile::TempDir;

use crate::pipeline::pipeline::{self, CompilationError};

fn write_project(files: &[(&str, &str)]) -> (TempDir, PathBuf, String) {
    let dir = tempfile::tempdir().unwrap();
    fs::write(
        dir.path().join("goml.toml"),
        r#"[module]
path = "visibility_test"
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

#[test]
fn public_function_is_visible() {
    assert_ok(&[
        (
            "main.gom",
            r#"
package main;

use visibility_test::api;

fn main() -> unit {
    let _ = api::answer();
}
"#,
        ),
        (
            "api/api.gom",
            r#"
package api;

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
package main;

use visibility_test::api;

fn main() -> unit {
    let _ = api::secret();
}
"#,
        ),
        (
            "api/api.gom",
            r#"
package api;

fn secret() -> int32 {
    7
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
package main;

use visibility_test::api;

fn main() -> unit {
    let _ = api::answer();
}
"#,
        ),
        (
            "api/api.gom",
            r#"
package api;

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
fn public_struct_is_visible() {
    assert_ok(&[
        (
            "main.gom",
            r#"
package main;

use visibility_test::api;

fn main() -> unit {
    let p = api::Point { x: 1 };
    let _ = p.x;
}
"#,
        ),
        (
            "api/api.gom",
            r#"
package api;

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
package main;

use visibility_test::api;

fn main() -> unit {
    let _ = api::Secret { x: 1 };
}
"#,
        ),
        (
            "api/api.gom",
            r#"
package api;

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
package main;

use visibility_test::api;

fn main() -> unit {
    let _ = api::Choice::B(1);
}
"#,
        ),
        (
            "api/api.gom",
            r#"
package api;

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
package main;

use visibility_test::api;

fn main() -> unit {
    let _ = api::Hidden::A;
}
"#,
        ),
        (
            "api/api.gom",
            r#"
package api;

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
package main;

use visibility_test::api;

use api::Label;

fn main() -> unit {
    let item = api::item();
    let _ = item.label();
}
"#,
        ),
        (
            "api/api.gom",
            r#"
package api;

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
package main;

use visibility_test::api;

use api::Hidden;

fn main() -> unit {
    let item = api::item();
    let _ = item.hidden();
}
"#,
        ),
        (
            "api/api.gom",
            r#"
package api;

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
