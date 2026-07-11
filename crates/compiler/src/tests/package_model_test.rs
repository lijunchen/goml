use std::fs;
use std::path::PathBuf;

use diagnostics::Diagnostics;
use tempfile::TempDir;

use crate::pipeline::pipeline::{self, CompilationError};

fn write_project(files: &[(&str, &str)]) -> (TempDir, PathBuf, String) {
    let dir = tempfile::tempdir().unwrap();
    fs::write(
        dir.path().join("goml.toml"),
        "[module]\npath = \"package_model_test\"\n",
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

fn typecheck_project(files: &[(&str, &str)]) -> Result<(TempDir, Diagnostics), CompilationError> {
    let (dir, main_path, main_src) = write_project(files);
    let (_, _, diagnostics) = pipeline::typecheck_with_packages(&main_path, &main_src)?;
    Ok((dir, diagnostics))
}

#[test]
fn imports_are_file_scoped() {
    let result = typecheck_project(&[
        (
            "main.gom",
            r#"package main;

use package_model_test::dep;

fn main() -> unit {
    let _ = dep::value();
    helper()
}
"#,
        ),
        (
            "helper.gom",
            r#"package main;

fn helper() -> unit {
    let _ = dep::value();
}
"#,
        ),
        (
            "dep/dep.gom",
            r#"package dep;

pub fn value() -> int32 {
    1
}
"#,
        ),
    ]);
    let (_, diagnostics) = result.unwrap();
    assert!(diagnostics.has_errors());
    assert!(
        diagnostics.iter().any(|diagnostic| diagnostic
            .message()
            .contains("package_model_test::dep not found")),
        "{diagnostics:#?}"
    );
}

#[test]
fn explicit_aliases_allow_same_declared_package_name() {
    let (_, diagnostics) = typecheck_project(&[
        (
            "main.gom",
            r#"package main;

use package_model_test::first as first_shared;
use package_model_test::second as second_shared;

fn main() -> unit {
    println(first_shared::value() + second_shared::value())
}
"#,
        ),
        (
            "first/first.gom",
            r#"package shared;

pub fn value() -> int32 {
    20
}
"#,
        ),
        (
            "second/second.gom",
            r#"package shared;

pub fn value() -> int32 {
    22
}
"#,
        ),
    ])
    .unwrap();
    assert!(!diagnostics.has_errors(), "{diagnostics:#?}");
}

#[test]
fn package_alias_trait_use_is_order_independent() {
    let (_, diagnostics) = typecheck_project(&[
        (
            "main.gom",
            r#"package main;

use traits::Render;
use package_model_test::trait_source as traits;

struct Item {}

impl traits::Render for Item {
    fn render(self: Item) -> string {
        "ok"
    }
}

fn main() -> unit {
    let item = Item {};
    let _ = item.render();
}
"#,
        ),
        (
            "trait_source/trait_source.gom",
            r#"package trait_source;

pub trait Render {
    fn render(Self) -> string;
}
"#,
        ),
    ])
    .unwrap();
    assert!(!diagnostics.has_errors(), "{diagnostics:#?}");
}

#[test]
fn declared_package_name_is_the_default_alias() {
    let (_, diagnostics) = typecheck_project(&[
        (
            "main.gom",
            r#"package main;

use package_model_test::directory_name;

fn main() -> unit {
    println(declared_name::value())
}
"#,
        ),
        (
            "directory_name/value.gom",
            r#"package declared_name;

pub fn value() -> int32 {
    42
}
"#,
        ),
    ])
    .unwrap();
    assert!(!diagnostics.has_errors(), "{diagnostics:#?}");
}

#[test]
fn every_project_file_requires_a_package_declaration() {
    let (dir, main_path, main_src) = write_project(&[
        (
            "main.gom",
            r#"package main;

fn main() -> unit {
    helper()
}
"#,
        ),
        (
            "helper.gom",
            r#"fn helper() -> unit {
    ()
}
"#,
        ),
    ]);
    let error = pipeline::typecheck_with_packages(&main_path, &main_src).unwrap_err();
    assert!(
        error.diagnostics().iter().any(|diagnostic| diagnostic
            .message()
            .contains("must declare `package <name>;`")),
        "{}",
        dir.path().display()
    );
}

#[test]
fn files_in_one_directory_must_declare_one_package() {
    let (_dir, main_path, main_src) = write_project(&[
        (
            "main.gom",
            r#"package main;

fn main() -> unit {
    ()
}
"#,
        ),
        (
            "other.gom",
            r#"package other;

fn helper() -> unit {
    ()
}
"#,
        ),
    ]);
    let error = pipeline::typecheck_with_packages(&main_path, &main_src).unwrap_err();
    assert!(
        error
            .diagnostics()
            .iter()
            .any(|diagnostic| diagnostic.message().contains("package mismatch"))
    );
}

#[test]
fn duplicate_package_import_is_rejected() {
    let (_, diagnostics) = typecheck_project(&[
        (
            "main.gom",
            r#"package main;

use package_model_test::dep;
use package_model_test::dep as another;

fn main() -> unit {
    ()
}
"#,
        ),
        (
            "dep/dep.gom",
            r#"package dep;

pub fn value() -> int32 {
    1
}
"#,
        ),
    ])
    .unwrap();
    assert!(diagnostics.iter().any(|diagnostic| {
        diagnostic
            .message()
            .contains("Duplicate package use package_model_test::dep")
    }));
}

#[test]
fn ambiguous_package_alias_is_rejected() {
    let (_, diagnostics) = typecheck_project(&[
        (
            "main.gom",
            r#"package main;

use package_model_test::first as shared;
use package_model_test::second as shared;

fn main() -> unit {
    ()
}
"#,
        ),
        ("first/first.gom", "package first;\n"),
        ("second/second.gom", "package second;\n"),
    ])
    .unwrap();
    assert!(diagnostics.iter().any(|diagnostic| {
        diagnostic
            .message()
            .contains("Ambiguous package use alias shared")
    }));
}

#[test]
fn nested_module_is_not_loaded_as_a_package() {
    let (_dir, main_path, main_src) = write_project(&[
        (
            "main.gom",
            r#"package main;

use package_model_test::nested;

fn main() -> unit {
    ()
}
"#,
        ),
        ("nested/goml.toml", "[module]\npath = \"nested\"\n"),
        ("nested/nested.gom", "package nested;\n"),
    ]);
    let error = pipeline::typecheck_with_packages(&main_path, &main_src).unwrap_err();
    assert!(error.diagnostics().iter().any(|diagnostic| {
        diagnostic
            .message()
            .contains("package package_model_test::nested is not provided")
    }));
}

#[test]
fn transitive_public_type_metadata_is_available() {
    let (_dir, main_path, main_src) = write_project(&[
        (
            "main.gom",
            r#"package main;

use package_model_test::facade;

fn main() -> unit {
    println(facade::make().value.to_string())
}
"#,
        ),
        (
            "model/model.gom",
            r#"package model;

pub struct Box {
    value: int32,
}

pub fn make() -> Box {
    Box { value: 42i32 }
}
"#,
        ),
        (
            "facade/facade.gom",
            r#"package facade;

use package_model_test::model;

pub fn make() -> model::Box {
    model::make()
}
"#,
        ),
    ]);
    pipeline::compile(&main_path, &main_src).unwrap();
}

#[test]
fn transitive_dependencies_are_not_source_visible() {
    let (_, diagnostics) = typecheck_project(&[
        (
            "main.gom",
            r#"package main;

use package_model_test::facade;

fn main() -> unit {
    let _ = facade::make();
    let _ = model::make();
}
"#,
        ),
        (
            "model/model.gom",
            r#"package model;

pub fn make() -> int32 {
    42i32
}
"#,
        ),
        (
            "facade/facade.gom",
            r#"package facade;

use package_model_test::model;

pub fn make() -> int32 {
    model::make()
}
"#,
        ),
    ])
    .unwrap();
    assert!(diagnostics.has_errors());
    assert!(diagnostics.iter().any(|diagnostic| {
        diagnostic
            .message()
            .contains("package_model_test::model not found")
    }));
}
