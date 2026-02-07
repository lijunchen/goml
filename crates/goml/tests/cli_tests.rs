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
        package Main;
        fn main() -> unit {
          string_println("hello");
        }



        == HIR ==
        package Main
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

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(output.status.success(), "stderr: {stderr}");
    assert!(stdout.contains("Created project at"));
    expect![""].assert_eq(&stderr);

    let project_dir = dir.path().join("demo");
    let root_toml = fs::read_to_string(project_dir.join("goml.toml"))?;
    let main_gom = fs::read_to_string(project_dir.join("main.gom"))?;
    let lib_toml = fs::read_to_string(project_dir.join("lib/goml.toml"))?;
    let lib_gom = fs::read_to_string(project_dir.join("lib/lib.gom"))?;

    assert!(root_toml.contains("[module]"));
    assert!(root_toml.contains("name = \"demo\""));
    assert!(root_toml.contains("entry = \"main.gom\""));
    assert!(root_toml.contains("name = \"main\""));
    assert!(main_gom.contains("package main;"));
    assert!(main_gom.contains("use lib;"));
    assert!(lib_toml.contains("name = \"lib\""));
    assert!(lib_gom.contains("package lib;"));
    assert!(lib_gom.contains("fn message() -> string"));

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

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(!output.status.success());
    assert!(stderr.contains("already exists and is not empty"));

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
