use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use expect_test::expect;
use tempfile::TempDir;

const HELLO_PROGRAM: &str = r#"fn main() -> unit {
    string_println("hello")
}
"#;

const PROJECT_CONFIG: &str = r#"[module]
name = "demo"

[package]
name = "main"
entry = "main.gom"
"#;

const PROJECT_MAIN: &str = r#"package main;

fn main() -> unit {
    string_println("hello")
}
"#;

fn goml_bin() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_goml"))
}

fn write_program(contents: &str) -> anyhow::Result<(TempDir, PathBuf)> {
    let dir = tempfile::tempdir()?;
    let path = dir.path().join("main.gom");
    fs::write(&path, contents)?;
    Ok((dir, path))
}

fn write_project(root: &Path) -> anyhow::Result<()> {
    fs::write(root.join("goml.toml"), PROJECT_CONFIG)?;
    fs::write(root.join("main.gom"), PROJECT_MAIN)?;
    Ok(())
}

fn run_goml(args: &[&str], cwd: &Path) -> anyhow::Result<std::process::Output> {
    Ok(Command::new(goml_bin())
        .args(args)
        .current_dir(cwd)
        .output()?)
}

fn normalize_temp_prefix(text: &str, root: &Path) -> String {
    text.replace(root.to_string_lossy().as_ref(), "<TMP>")
}

fn module_fixtures_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../compiler/src/tests/module")
}

fn copy_dir_recursive(src: &Path, dst: &Path) -> anyhow::Result<()> {
    fs::create_dir_all(dst)?;
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let file_type = entry.file_type()?;
        let target = dst.join(entry.file_name());
        if file_type.is_dir() {
            if entry.file_name().to_string_lossy() == "target" {
                continue;
            }
            copy_dir_recursive(&entry.path(), &target)?;
        } else if file_type.is_file() {
            fs::copy(entry.path(), target)?;
        }
    }
    Ok(())
}

fn copy_module_fixture(project: &str) -> anyhow::Result<(TempDir, PathBuf)> {
    let fixture = module_fixtures_root().join(project);
    let dir = tempfile::tempdir()?;
    copy_dir_recursive(&fixture, dir.path())?;
    Ok((dir, fixture))
}

#[test]
fn compiler_run_single_executes_program() -> anyhow::Result<()> {
    let (_dir, path) = write_program(HELLO_PROGRAM)?;

    let output = Command::new(goml_bin())
        .arg("compiler")
        .arg("run-single")
        .arg(&path)
        .output()?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(output.status.success(), "stderr: {stderr}");
    expect!["hello\n"].assert_eq(&stdout);
    expect![""].assert_eq(&stderr);

    Ok(())
}

#[test]
fn compiler_run_single_dumps_requested_stages() -> anyhow::Result<()> {
    let (_dir, path) = write_program(HELLO_PROGRAM)?;

    let output = Command::new(goml_bin())
        .arg("compiler")
        .arg("run-single")
        .args([
            "--dump-ast",
            "--dump-hir",
            "--dump-tast",
            "--dump-core",
            "--dump-mono",
            "--dump-lift",
            "--dump-anf",
            "--dump-go",
        ])
        .arg(&path)
        .output()?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(output.status.success(), "stderr: {stderr}");
    expect![[r#"
        == AST ==
        package main;
        fn main() -> unit {
          string_println("hello");
        }



        == HIR ==
        package main
        file main.gom
          fn main() -> unit {
              string_println("hello");
            }

        == Typed AST ==
        fn print(value/0: T) -> unit {
            (string_print : (string) -> unit)((ToString::to_string : (T) -> string)((value/0 : T)));
        }

        fn println(value/1: T) -> unit {
            (string_println : (string) -> unit)((ToString::to_string : (T) -> string)((value/1 : T)));
        }

        fn main() -> unit {
            (string_println : (string) -> unit)("hello");
        }

        == Core ==
        fn print(value/0: T) -> unit {
          string_print(trait_call[ToString::to_string](value/0))
        }

        fn println(value/1: T) -> unit {
          string_println(trait_call[ToString::to_string](value/1))
        }

        fn main() -> unit {
          string_println("hello")
        }

        == Mono ==
        fn main() -> unit {
          string_println("hello")
        }

        == Lifted ==
        fn main() -> unit {
          string_println("hello")
        }

        == ANF ==
        fn main() -> unit {
          string_println("hello")
        }

        == Go ==
        package main

        import (
            "fmt"
        )

        func string_println(s string) struct{} {
            fmt.Println(s)
            return struct{}{}
        }

        func main0() struct{} {
            var ret0 struct{}
            ret0 = string_println("hello")
            return ret0
        }

        func main() {
            main0()
        }

        hello
    "#]]
    .assert_eq(&stdout);
    expect![""].assert_eq(&stderr);

    Ok(())
}

#[test]
fn project_check_checks_module_from_cwd() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    write_project(dir.path())?;

    let output = Command::new(goml_bin())
        .arg("check")
        .current_dir(dir.path())
        .output()?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(output.status.success(), "stderr: {stderr}");
    expect![""].assert_eq(&stdout);
    expect![""].assert_eq(&stderr);

    Ok(())
}

#[test]
fn project_build_writes_target_goml_main_go() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    write_project(dir.path())?;

    let output = Command::new(goml_bin())
        .arg("build")
        .current_dir(dir.path())
        .output()?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(output.status.success(), "stderr: {stderr}");
    expect![""].assert_eq(&stdout);
    expect![""].assert_eq(&stderr);

    let go_file = dir.path().join("target/goml/main.go");
    assert!(go_file.exists());

    let go_output = Command::new("go")
        .arg("run")
        .arg(&go_file)
        .current_dir(dir.path())
        .output()?;

    let go_stdout = String::from_utf8_lossy(&go_output.stdout);
    let go_stderr = String::from_utf8_lossy(&go_output.stderr);

    assert!(go_output.status.success(), "stderr: {go_stderr}");
    expect!["hello\n"].assert_eq(&go_stdout);

    Ok(())
}

#[test]
fn new_creates_two_package_scaffold() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;

    let output = Command::new(goml_bin())
        .arg("new")
        .arg("demo")
        .current_dir(dir.path())
        .output()?;

    let stdout = normalize_temp_prefix(&String::from_utf8_lossy(&output.stdout), dir.path());
    let stderr = normalize_temp_prefix(&String::from_utf8_lossy(&output.stderr), dir.path());

    assert!(output.status.success(), "stderr: {stderr}");
    expect![[r#"
        Created project at ./demo
        Next steps:
          cd ./demo
          goml check
          goml build
    "#]]
    .assert_eq(&stdout);
    expect![""].assert_eq(&stderr);

    let project_dir = dir.path().join("demo");
    let root_toml = fs::read_to_string(project_dir.join("goml.toml"))?;
    let main_gom = fs::read_to_string(project_dir.join("main.gom"))?;
    let lib_toml = fs::read_to_string(project_dir.join("lib/goml.toml"))?;
    let lib_gom = fs::read_to_string(project_dir.join("lib/lib.gom"))?;

    expect![[r#"
        [module]
        name = "demo"

        [package]
        name = "main"
        entry = "main.gom"
    "#]]
    .assert_eq(&root_toml);
    expect![[r#"
        package main;

        use lib;

        fn main() -> unit {
            string_println(lib::message())
        }
    "#]]
    .assert_eq(&main_gom);
    expect![[r#"
        [package]
        name = "lib"
    "#]]
    .assert_eq(&lib_toml);
    expect![[r#"
        package lib;

        fn message() -> string {
            "hello from lib"
        }
    "#]]
    .assert_eq(&lib_gom);

    Ok(())
}

#[test]
fn new_fails_when_target_exists_and_not_empty() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let project_dir = dir.path().join("demo");
    fs::create_dir_all(&project_dir)?;
    fs::write(project_dir.join("keep.txt"), "x")?;

    let output = Command::new(goml_bin())
        .arg("new")
        .arg("demo")
        .current_dir(dir.path())
        .output()?;

    let stderr = normalize_temp_prefix(&String::from_utf8_lossy(&output.stderr), dir.path());
    assert!(!output.status.success());
    expect!["target directory ./demo already exists and is not empty\n"].assert_eq(&stderr);

    Ok(())
}

#[test]
fn new_project_can_check_and_build() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;

    let new_output = Command::new(goml_bin())
        .arg("new")
        .arg("demo")
        .current_dir(dir.path())
        .output()?;
    let new_stderr = String::from_utf8_lossy(&new_output.stderr);
    assert!(new_output.status.success(), "stderr: {new_stderr}");

    let project_dir = dir.path().join("demo");

    let check_output = Command::new(goml_bin())
        .arg("check")
        .current_dir(&project_dir)
        .output()?;
    let check_stderr = String::from_utf8_lossy(&check_output.stderr);
    assert!(check_output.status.success(), "stderr: {check_stderr}");

    let build_output = Command::new(goml_bin())
        .arg("build")
        .current_dir(&project_dir)
        .output()?;
    let build_stderr = String::from_utf8_lossy(&build_output.stderr);
    assert!(build_output.status.success(), "stderr: {build_stderr}");

    let go_file = project_dir.join("target/goml/main.go");
    assert!(go_file.exists());
    assert!(project_dir.join("target/goml/build/main.core").exists());
    assert!(
        project_dir
            .join("target/goml/build/main.interface")
            .exists()
    );
    assert!(project_dir.join("target/goml/build/lib.core").exists());
    assert!(project_dir.join("target/goml/build/lib.interface").exists());

    let go_output = Command::new("go")
        .arg("run")
        .arg(&go_file)
        .current_dir(&project_dir)
        .output()?;

    let go_stdout = String::from_utf8_lossy(&go_output.stdout);
    let go_stderr = String::from_utf8_lossy(&go_output.stderr);
    assert!(go_output.status.success(), "stderr: {go_stderr}");
    expect!["hello from lib\n"].assert_eq(&go_stdout);

    Ok(())
}

#[test]
fn project_check_dry_run_prints_compiler_check_commands() -> anyhow::Result<()> {
    let (dir, _) = copy_module_fixture("project008_trait_bounds_across_packages")?;
    let output = run_goml(&["check", "--dry-run"], dir.path())?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "stderr: {stderr}");
    expect![[r#"
        goml compiler check --package traitpkg --input traitpkg/lib.gom --output target/goml/check/traitpkg
        goml compiler check --package datapkg --input datapkg/lib.gom --interface-path target/goml/check/traitpkg.interface --output target/goml/check/datapkg
        goml compiler check --package usepkg --input usepkg/lib.gom --interface-path target/goml/check/traitpkg.interface --output target/goml/check/usepkg
        goml compiler check --package main --input main.gom --interface-path target/goml/check/datapkg.interface --interface-path target/goml/check/usepkg.interface --output target/goml/check/main
    "#]]
    .assert_eq(&stdout);
    assert!(!dir.path().join("target/goml/check/main.interface").exists());
    expect![""].assert_eq(&stderr);

    Ok(())
}

#[test]
fn project_build_dry_run_prints_compiler_build_and_link_commands() -> anyhow::Result<()> {
    let (dir, _) = copy_module_fixture("project008_trait_bounds_across_packages")?;
    let output = run_goml(&["build", "--dry-run"], dir.path())?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "stderr: {stderr}");
    expect![[r#"
        goml compiler build --package traitpkg --input traitpkg/lib.gom --output target/goml/build/traitpkg
        goml compiler build --package datapkg --input datapkg/lib.gom --interface-path target/goml/build/traitpkg.interface --output target/goml/build/datapkg
        goml compiler build --package usepkg --input usepkg/lib.gom --interface-path target/goml/build/traitpkg.interface --output target/goml/build/usepkg
        goml compiler build --package main --input main.gom --interface-path target/goml/build/datapkg.interface --interface-path target/goml/build/usepkg.interface --output target/goml/build/main
        goml compiler link --input target/goml/build/traitpkg.core target/goml/build/datapkg.core target/goml/build/usepkg.core target/goml/build/main.core --output target/goml/main.go
    "#]]
    .assert_eq(&stdout);
    assert!(!dir.path().join("target/goml/main.go").exists());
    expect![""].assert_eq(&stderr);

    Ok(())
}

#[test]
fn project_check_and_build_work_for_complex_dependency_fixtures() -> anyhow::Result<()> {
    let projects = [
        "project001",
        "project002",
        "project003",
        "project004",
        "project005",
        "project006",
        "project007_trait_impl_orphan_ok",
        "project008_trait_bounds_across_packages",
        "project009_builtin_option_result",
    ];

    for project in projects {
        let (dir, fixture) = copy_module_fixture(project)?;

        let check_output = run_goml(&["check"], dir.path())?;
        let check_stderr = String::from_utf8_lossy(&check_output.stderr);
        assert!(
            check_output.status.success(),
            "project={project}, stderr={check_stderr}"
        );

        let build_output = run_goml(&["build"], dir.path())?;
        let build_stderr = String::from_utf8_lossy(&build_output.stderr);
        assert!(
            build_output.status.success(),
            "project={project}, stderr={build_stderr}"
        );

        let go_file = dir.path().join("target/goml/main.go");
        assert!(go_file.exists(), "project={project}");

        let go_output = Command::new("go")
            .arg("run")
            .arg(&go_file)
            .current_dir(dir.path())
            .output()?;
        let go_stdout = String::from_utf8_lossy(&go_output.stdout);
        let go_stderr = String::from_utf8_lossy(&go_output.stderr);
        assert!(
            go_output.status.success(),
            "project={project}, stderr={go_stderr}"
        );
        let actual_output = format!("{go_stdout}{go_stderr}");

        let expected = fs::read_to_string(fixture.join("main.gom.out"))?;
        assert_eq!(actual_output, expected, "project={project}");
    }

    Ok(())
}

#[test]
fn project_build_stops_when_compiler_subcommand_fails() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let root = dir.path();

    fs::write(
        root.join("goml.toml"),
        r#"[module]
name = "demo"

[package]
name = "main"
entry = "main.gom"
"#,
    )?;
    fs::write(
        root.join("main.gom"),
        r#"package main;

use A;

fn main() -> unit {
    string_println(A::msg())
}
"#,
    )?;

    fs::create_dir_all(root.join("A"))?;
    fs::write(
        root.join("A/goml.toml"),
        r#"[package]
name = "A"
"#,
    )?;
    fs::write(
        root.join("A/lib.gom"),
        r#"package A;

use B;

fn msg() -> string {
    B::value()
}
"#,
    )?;

    fs::create_dir_all(root.join("B"))?;
    fs::write(
        root.join("B/goml.toml"),
        r#"[package]
name = "B"
"#,
    )?;
    fs::write(
        root.join("B/lib.gom"),
        r#"package B;

fn value() -> int32 {
    1
}
"#,
    )?;

    let output = run_goml(&["build"], root)?;
    let stderr = normalize_temp_prefix(&String::from_utf8_lossy(&output.stderr), root);
    assert!(!output.status.success());
    expect![[r#"
        build failed: Typer { diagnostics: Diagnostics { items: [Diagnostic { stage: Typer, severity: Error, message: "Type mismatch: expected int32, found string", range: Some(45..56) }, Diagnostic { stage: Typer, severity: Error, message: "Type mismatch: expected int32, found string", range: Some(39..58) }] } }
        subcommand failed: goml compiler build --package A --input A/lib.gom --interface-path target/goml/build/B.interface --output target/goml/build/A
    "#]]
    .assert_eq(&stderr);
    assert!(!root.join("target/goml/main.go").exists());

    Ok(())
}

#[test]
fn project_build_dry_run_preserves_entry_directory_structure() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let root = dir.path();

    fs::write(
        root.join("goml.toml"),
        r#"[module]
name = "demo"

[package]
name = "main"
entry = "main.gom"
"#,
    )?;
    fs::write(
        root.join("main.gom"),
        r#"package main;

use Lib;

fn main() -> unit {
    string_println(Lib::msg())
}
"#,
    )?;

    fs::create_dir_all(root.join("Lib/src"))?;
    fs::write(
        root.join("Lib/goml.toml"),
        r#"[package]
name = "Lib"
entry = "src/lib_entry.gom"
"#,
    )?;
    fs::write(
        root.join("Lib/src/lib_entry.gom"),
        r#"package Lib;

fn msg() -> string {
    "ok"
}
"#,
    )?;

    let output = run_goml(&["build", "--dry-run"], root)?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "stderr: {stderr}");
    expect![[r#"
        goml compiler build --package Lib --input Lib/src/lib_entry.gom --output target/goml/build/Lib
        goml compiler build --package main --input main.gom --interface-path target/goml/build/Lib.interface --output target/goml/build/main
        goml compiler link --input target/goml/build/Lib.core target/goml/build/main.core --output target/goml/main.go
    "#]]
    .assert_eq(&stdout);
    expect![""].assert_eq(&stderr);

    Ok(())
}
