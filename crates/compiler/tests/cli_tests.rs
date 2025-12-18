use std::fs;
use std::path::PathBuf;
use std::process::Command;

use expect_test::expect;
use tempfile::TempDir;

const HELLO_PROGRAM: &str = r#"fn main() -> unit {
    string_println("hello")
}
"#;

fn compiler_bin() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_compiler"))
}

fn write_program(contents: &str) -> anyhow::Result<(TempDir, PathBuf)> {
    let dir = tempfile::tempdir()?;
    let path = dir.path().join("main.gom");
    fs::write(&path, contents)?;
    Ok((dir, path))
}

#[test]
fn run_executes_program() -> anyhow::Result<()> {
    let (_dir, path) = write_program(HELLO_PROGRAM)?;

    let output = Command::new(compiler_bin())
        .arg("run")
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
fn run_dumps_requested_stages() -> anyhow::Result<()> {
    let (_dir, path) = write_program(HELLO_PROGRAM)?;

    let output = Command::new(compiler_bin())
        .arg("run")
        .args([
            "--dump-ast",
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
        fn main() -> unit {
          string_println("hello");
        }



        == Typed AST ==
        fn main() -> unit {
            (string_println : (string) -> unit)("hello");
        }

        == Core ==
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
