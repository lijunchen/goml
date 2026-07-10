use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::OnceLock;

use expect_test::expect;
use tempfile::TempDir;

const HELLO_PROGRAM: &str = r#"fn main() -> unit {
    println("hello")
}
"#;

const FUNCTION_VECTOR_PROGRAM: &str = r#"fn inc(x: int32) -> int32 {
    x + 1
}

fn dec(x: int32) -> int32 {
    x - 1
}

fn main() -> unit {
    let fs: Vec[(int32) -> int32] = Vec::new();
    let fs = fs.push(inc);
    let fs = fs.push(dec);
    let f = fs[0];
    println(f(10));
}
"#;

fn gomlc_bin() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_gomlc"))
}

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("gomlc crate must live under crates/gomlc")
        .to_path_buf()
}

fn write_program(contents: &str) -> anyhow::Result<(TempDir, PathBuf)> {
    let dir = tempfile::tempdir()?;
    let path = dir.path().join("main.gom");
    let contents = if contents.trim_start().starts_with("package ") {
        contents.to_string()
    } else {
        format!("package main;\n\n{contents}")
    };
    fs::write(&path, contents)?;
    Ok((dir, path))
}

fn deep_left_nested_tuple_type(depth: usize) -> String {
    let mut ty = "int32".to_string();
    for _ in 0..depth {
        ty = format!("({}, int32)", ty);
    }
    ty
}

fn wide_struct_pattern_program(field_count: usize) -> String {
    let fields = (0..field_count)
        .map(|idx| format!("f{}: int32", idx))
        .collect::<Vec<_>>()
        .join(", ");
    let values = (0..field_count)
        .map(|idx| format!("f{}: {}i32", idx, idx))
        .collect::<Vec<_>>()
        .join(", ");
    let pattern_fields = (0..field_count)
        .map(|idx| format!("f{}: x{}", idx, idx))
        .collect::<Vec<_>>()
        .join(", ");
    let sum = (0..200)
        .map(|idx| format!("x{}", idx))
        .collect::<Vec<_>>()
        .join(" + ");
    format!(
        "package main;\n\nstruct S {{ {fields} }}\nfn main() -> unit {{ let s = S {{ {values} }}; let total = match s {{ S {{ {pattern_fields} }} => {sum} }}; println(total.to_string()) }}\n"
    )
}

fn go_available() -> bool {
    static AVAILABLE: OnceLock<bool> = OnceLock::new();
    *AVAILABLE.get_or_init(|| {
        Command::new("go")
            .arg("version")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .is_ok_and(|status| status.success())
    })
}

fn yaegi_available() -> bool {
    static AVAILABLE: OnceLock<bool> = OnceLock::new();
    *AVAILABLE.get_or_init(|| {
        Command::new("yaegi")
            .arg("help")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .is_ok_and(|status| status.success())
    })
}

fn runtime_executor_available() -> bool {
    yaegi_available() || go_available()
}

fn copy_dir_recursive(src: &Path, dst: &Path) -> anyhow::Result<()> {
    fs::create_dir_all(dst)?;
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let target = dst.join(entry.file_name());
        if entry.file_type()?.is_dir() {
            copy_dir_recursive(&entry.path(), &target)?;
        } else {
            fs::copy(entry.path(), target)?;
        }
    }
    Ok(())
}

fn run_goml_with_goml_home(
    args: &[&str],
    cwd: &Path,
    home: &Path,
) -> anyhow::Result<std::process::Output> {
    let args = args.strip_prefix(&["compiler"]).unwrap_or(args);
    Ok(Command::new(gomlc_bin())
        .args(args)
        .current_dir(cwd)
        .env("GOML_HOME", home)
        .output()?)
}

#[test]
fn gomlc_run_single_executes_program() -> anyhow::Result<()> {
    if !runtime_executor_available() {
        return Ok(());
    }

    let (_dir, path) = write_program(HELLO_PROGRAM)?;

    let output = Command::new(gomlc_bin())
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
fn gomlc_run_single_falls_back_when_yaegi_cannot_run_function_vectors() -> anyhow::Result<()> {
    if !runtime_executor_available() {
        return Ok(());
    }

    let (_dir, path) = write_program(FUNCTION_VECTOR_PROGRAM)?;

    let output = Command::new(gomlc_bin())
        .arg("run-single")
        .arg(&path)
        .output()?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(output.status.success(), "stderr: {stderr}");
    expect!["11\n"].assert_eq(&stdout);
    expect![""].assert_eq(&stderr);

    Ok(())
}

#[test]
fn gomlc_loads_std_from_goml_home() -> anyhow::Result<()> {
    if !runtime_executor_available() {
        return Ok(());
    }

    let dir = tempfile::tempdir()?;
    let home = dir.path().join("goml-home");
    copy_dir_recursive(&workspace_root().join("stdlib/std"), &home.join("lib/std"))?;
    let path = dir.path().join("main.gom");
    fs::write(
        &path,
        r#"
use std::io;

fn main() -> unit {
    io::println("std-home")
}
"#,
    )?;

    let output = run_goml_with_goml_home(
        &["compiler", "run-single", path.to_string_lossy().as_ref()],
        dir.path(),
        &home,
    )?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(output.status.success(), "stderr: {stderr}");
    expect!["std-home\n"].assert_eq(&stdout);
    expect![""].assert_eq(&stderr);

    Ok(())
}

#[test]
fn gomlc_build_handles_deep_tuple_projection() -> anyhow::Result<()> {
    let input = workspace_root()
        .join("crates/compiler/src/tests/crashers/deep_tuple_projection_stack/main.gom");
    let dir = tempfile::tempdir()?;
    let output_path = dir.path().join("main");

    let output = Command::new(gomlc_bin())
        .arg("build")
        .arg("--package")
        .arg("main")
        .arg("--input")
        .arg(&input)
        .arg("--output")
        .arg(&output_path)
        .output()?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert_eq!(
        output.status.code(),
        Some(1),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    assert!(
        stderr.contains("expression is too deeply nested"),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    expect![""].assert_eq(&stdout);

    Ok(())
}

#[test]
fn gomlc_build_handles_deep_tuple_type() -> anyhow::Result<()> {
    let ty = deep_left_nested_tuple_type(4000);
    let program = format!("fn take(x: {ty}) -> unit {{ () }}\nfn main() -> unit {{ () }}\n");
    let (_input_dir, input) = write_program(&program)?;
    let output_dir = tempfile::tempdir()?;
    let output_path = output_dir.path().join("main");

    let output = Command::new(gomlc_bin())
        .arg("build")
        .arg("--package")
        .arg("main")
        .arg("--input")
        .arg(&input)
        .arg("--output")
        .arg(&output_path)
        .output()?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert_eq!(
        output.status.code(),
        Some(1),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    assert!(
        stderr.contains("type is too deeply nested"),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    expect![""].assert_eq(&stdout);

    Ok(())
}

#[test]
fn gomlc_build_rejects_reserved_array_wildcard_length() -> anyhow::Result<()> {
    let program = r#"
fn take(x: [int32; 18446744073709551615]) -> int32 {
    x[0]
}

fn main() -> unit {
    let a: [int32; 18446744073709551615] = [7i32, 8i32];
    println(take(a).to_string())
}
"#;
    let (_input_dir, input) = write_program(program)?;
    let output_dir = tempfile::tempdir()?;
    let output_path = output_dir.path().join("main");

    let output = Command::new(gomlc_bin())
        .arg("build")
        .arg("--package")
        .arg("main")
        .arg("--input")
        .arg(&input)
        .arg("--output")
        .arg(&output_path)
        .output()?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert_eq!(
        output.status.code(),
        Some(1),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    assert!(
        stderr.contains("Invalid array length"),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    expect![""].assert_eq(&stdout);

    Ok(())
}

#[test]
fn gomlc_build_rejects_array_length_above_go_int() -> anyhow::Result<()> {
    let program = r#"
fn take(x: [int32; 18446744073709551614]) -> int32 {
    x[0]
}

fn main() -> unit {
    let dummy = [0i32];
    let a: [int32; 18446744073709551614] = [7i32, 8i32];
    println(take(a).to_string())
}
"#;
    let (_input_dir, input) = write_program(program)?;
    let output_dir = tempfile::tempdir()?;
    let output_path = output_dir.path().join("main");

    let output = Command::new(gomlc_bin())
        .arg("build")
        .arg("--package")
        .arg("main")
        .arg("--input")
        .arg(&input)
        .arg("--output")
        .arg(&output_path)
        .output()?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert_eq!(
        output.status.code(),
        Some(1),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    assert!(
        stderr.contains("Invalid array length"),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    expect![""].assert_eq(&stdout);

    Ok(())
}

#[test]
fn gomlc_build_handles_wide_struct_pattern() -> anyhow::Result<()> {
    let program = wide_struct_pattern_program(1000);
    let (_input_dir, input) = write_program(&program)?;
    let output_dir = tempfile::tempdir()?;
    let output_path = output_dir.path().join("main");

    let output = Command::new(gomlc_bin())
        .arg("build")
        .arg("--package")
        .arg("main")
        .arg("--input")
        .arg(&input)
        .arg("--output")
        .arg(&output_path)
        .output()?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        output.status.success(),
        "stdout: {stdout}\nstderr: {stderr}"
    );

    Ok(())
}

#[test]
fn gomlc_build_handles_very_wide_struct_pattern() -> anyhow::Result<()> {
    let program = wide_struct_pattern_program(2600);
    let (_input_dir, input) = write_program(&program)?;
    let output_dir = tempfile::tempdir()?;
    let output_path = output_dir.path().join("main");

    let output = Command::new(gomlc_bin())
        .arg("build")
        .arg("--package")
        .arg("main")
        .arg("--input")
        .arg(&input)
        .arg("--output")
        .arg(&output_path)
        .output()?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        output.status.success(),
        "stdout: {stdout}\nstderr: {stderr}"
    );

    Ok(())
}

#[test]
fn gomlc_run_single_dumps_requested_stages() -> anyhow::Result<()> {
    if !runtime_executor_available() {
        return Ok(());
    }

    let (_dir, path) = write_program(HELLO_PROGRAM)?;

    let output = Command::new(gomlc_bin())
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
            println("hello")
        }



        == HIR ==
        package main
        file main.gom
          fn main() -> unit {
                println("hello")
            }

        == Typed AST ==
        fn print(value/0: T) -> unit {
            (string_print : (string) -> unit)((ToString::to_string : (T) -> string)((value/0 : T)))
        }

        fn println(value/1: T) -> unit {
            (string_println : (string) -> unit)((ToString::to_string : (T) -> string)((value/1 : T)))
        }

        fn main() -> unit {
            (println : (string) -> unit)("hello")
        }

        == Core ==
        fn print(value/0: T) -> unit {
          string_print(trait_call[ToString::to_string](value/0))
        }

        fn println(value/1: T) -> unit {
          string_println(trait_call[ToString::to_string](value/1))
        }

        fn main() -> unit {
          println("hello")
        }

        == Mono ==
        fn main() -> unit {
          println__T_string("hello")
        }

        fn println__T_string(value/1: string) -> unit {
          string_println(value/1)
        }

        == Lifted ==
        fn main() -> unit {
          println__T_string("hello")
        }

        fn println__T_string(value/1: string) -> unit {
          string_println(value/1)
        }

        == ANF ==
        fn main() -> unit {
          join ret0() -> unit {
            ()
          } in
          let t1 = println__T_string("hello") in
          jump ret0()
        }

        fn println__T_string(value/1: string) -> unit {
          join ret2() -> unit {
            ()
          } in
          let t3 = string_println(value/1) in
          jump ret2()
        }

        == Go ==
        package main

        import (
            _goml_fmt "fmt"
        )

        func string_println(s string) struct{} {
            _goml_fmt.Println(s)
            return struct{}{}
        }

        func main0() struct{} {
            println__T_string("hello")
            return struct{}{}
        }

        func println__T_string(value__1 string) struct{} {
            string_println(value__1)
            return struct{}{}
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
fn version_reports_text_and_protocol() -> anyhow::Result<()> {
    let text = Command::new(gomlc_bin()).arg("version").output()?;
    assert!(text.status.success());
    assert!(String::from_utf8_lossy(&text.stdout).starts_with("gomlc 0.0.0"));

    let json = Command::new(gomlc_bin())
        .args(["version", "--format", "json"])
        .output()?;
    assert!(json.status.success());
    let value: serde_json::Value = serde_json::from_slice(&json.stdout)?;
    assert_eq!(value["tool"], "gomlc");
    assert_eq!(value["driver_protocol"], 1);
    assert_eq!(value["artifact_format"], compiler::artifact::FORMAT_VERSION);
    assert_eq!(value["compiler_abi"], compiler::artifact::COMPILER_ABI);
    Ok(())
}
