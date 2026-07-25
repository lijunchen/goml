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

fn assert_err_contains(files: &[(&str, &str)], expected: &str) {
    let diagnostics = match typecheck(files) {
        Ok(diagnostics) => diagnostics,
        Err(error) => error.diagnostics().clone(),
    };
    let actual = format!("{diagnostics:#?}");
    assert!(actual.contains(expected), "{actual}");
}

#[test]
fn public_field_cannot_expose_private_type() {
    assert_err_contains(
        &[
            (
                "main.gom",
                r#"
package main;

use visibility_test::api;

fn main() -> unit {
    let _ = api::make();
}
"#,
            ),
            (
                "api/api.gom",
                r#"
package api;

struct Hidden {}

pub struct Value {
    pub hidden: Hidden,
}

pub fn make() -> Value {
    Value { hidden: Hidden {} }
}
"#,
            ),
        ],
        "Public field visibility_test::api::Value.hidden exposes private type visibility_test::api::Hidden",
    );
}

#[test]
fn public_inherent_method_cannot_expose_private_type() {
    assert_err_contains(
        &[
            (
                "main.gom",
                r#"
package main;

use visibility_test::api;

fn main() -> unit {
    let _ = api::make();
}
"#,
            ),
            (
                "api/api.gom",
                r#"
package api;

struct Hidden {}
pub struct Value {}

impl Value {
    pub fn reveal(self: Value) -> Hidden { Hidden {} }
}

pub fn make() -> Value { Value {} }
"#,
            ),
        ],
        "Public inherent method reveal exposes private type visibility_test::api::Hidden",
    );
}
