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

fn assert_err_contains(files: &[(&str, &str)], expected: &str) {
    let diagnostics = match typecheck(files) {
        Ok(diagnostics) => diagnostics,
        Err(error) => error.diagnostics().clone(),
    };
    let actual = format!("{diagnostics:#?}");
    assert!(actual.contains(expected), "{actual}");
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
    pub x: int32,
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
    fn label(self: Self) -> string;
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
    fn hidden(self: Self) -> string;
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
fn public_associated_type_cannot_expose_private_type() {
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

pub trait Source {
    type Item;
    fn get(self: Self) -> Self::Item;
}

pub struct Public {}
struct Hidden {}

impl Source for Public {
    type Item = Hidden;
    fn get(self: Public) -> Hidden { Hidden {} }
}

pub fn make() -> Public { Public {} }
"#,
            ),
        ],
        "Public trait implementation visibility_test::api::Source exposes private type visibility_test::api::Hidden",
    );
}

#[test]
fn private_struct_field_is_hidden() {
    assert_err(&[
        (
            "main.gom",
            r#"
package main;

use visibility_test::api;

fn main() -> unit {
    let value = api::make();
    let _ = value.hidden;
}
"#,
        ),
        (
            "api/api.gom",
            r#"
package api;

pub struct Value {
    pub visible: int32,
    hidden: int32,
}

pub fn make() -> Value {
    Value { visible: 1, hidden: 2 }
}
"#,
        ),
    ]);
}

#[test]
fn struct_with_private_fields_cannot_be_constructed_cross_package() {
    assert_err_contains(
        &[
            (
                "main.gom",
                r#"
package main;

use visibility_test::api;

fn main() -> unit {
    let _ = api::Value { visible: 1 };
}
"#,
            ),
            (
                "api/api.gom",
                r#"
package api;

pub struct Value {
    pub visible: int32,
    hidden: int32,
}
"#,
            ),
        ],
        "has private fields and cannot be constructed here",
    );
}

#[test]
fn struct_pattern_with_private_fields_requires_rest() {
    assert_err_contains(
        &[
            (
                "main.gom",
                r#"
package main;

use visibility_test::api;

fn main() -> unit {
    match api::make() {
        api::Value { visible } => { let _ = visible; },
    }
}
"#,
            ),
            (
                "api/api.gom",
                r#"
package api;

pub struct Value {
    pub visible: int32,
    hidden: int32,
}

pub fn make() -> Value {
    Value { visible: 1, hidden: 2 }
}
"#,
            ),
        ],
        "must use `..` because it has private fields",
    );
}

#[test]
fn struct_pattern_with_private_fields_accepts_rest() {
    assert_ok(&[
        (
            "main.gom",
            r#"
package main;

use visibility_test::api;

fn main() -> unit {
    match api::make() {
        api::Value { visible, .. } => { let _ = visible; },
    }
}
"#,
        ),
        (
            "api/api.gom",
            r#"
package api;

pub struct Value {
    pub visible: int32,
    hidden: int32,
}

pub fn make() -> Value {
    Value { visible: 1, hidden: 2 }
}
"#,
        ),
    ]);
}

#[test]
fn public_inherent_method_is_visible() {
    assert_ok(&[
        (
            "main.gom",
            r#"
package main;

use visibility_test::api;

fn main() -> unit {
    let _ = api::make().open();
}
"#,
        ),
        (
            "api/api.gom",
            r#"
package api;

pub struct Value {}

impl Value {
    pub fn open(self: Value) -> int32 { 1 }
    fn closed(self: Value) -> int32 { 2 }
}

pub fn make() -> Value { Value {} }
"#,
        ),
    ]);
}

#[test]
fn private_inherent_method_is_hidden() {
    assert_err(&[
        (
            "main.gom",
            r#"
package main;

use visibility_test::api;

fn main() -> unit {
    let _ = api::make().closed();
}
"#,
        ),
        (
            "api/api.gom",
            r#"
package api;

pub struct Value {}

impl Value {
    pub fn open(self: Value) -> int32 { 1 }
    fn closed(self: Value) -> int32 { 2 }
}

pub fn make() -> Value { Value {} }
"#,
        ),
    ]);
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

#[test]
fn enum_variant_fields_cannot_use_pub() {
    assert_err_contains(
        &[
            (
                "main.gom",
                r#"
package main;

use visibility_test::api;

fn main() -> unit {
    let _ = api::Value::Item { field: 1 };
}
"#,
            ),
            (
                "api/api.gom",
                r#"
package api;

pub enum Value {
    Item { pub field: int32 },
}
"#,
            ),
        ],
        "Enum variant fields inherit the enum visibility and must not use `pub`",
    );
}

#[test]
fn trait_implementation_methods_cannot_use_pub() {
    assert_err_contains(
        &[
            (
                "main.gom",
                r#"
package main;

use visibility_test::api;

fn main() -> unit {}
"#,
            ),
            (
                "api/api.gom",
                r#"
package api;

pub trait Read {
    fn read(self: Self) -> int32;
}

pub struct Value {}

impl Read for Value {
    pub fn read(self: Value) -> int32 { 1 }
}
"#,
            ),
        ],
        "Trait implementation method read must not use `pub`",
    );
}
