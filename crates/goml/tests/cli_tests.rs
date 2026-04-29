use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};
use std::sync::OnceLock;

use compiler::pipeline::pipeline::compile_single_file;
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
    let fs: Vec[(int32) -> int32] = vec_new();
    let fs = vec_push(fs, inc);
    let fs = vec_push(fs, dec);
    let f = vec_get(fs, 0);
    println(f(10));
}
"#;

const GO_OPTION_LAST_TUPLE_PROGRAM: &str = r#"#[go_option_last]
extern "go" "strings" "Cut" cut_pair(text: string, sep: string) -> Option[(string, string)]

fn describe(text: string) -> string {
    match cut_pair(text, ":") {
        Option::Some((before, after)) => before + "|" + after,
        Option::None => "missing",
    }
}

fn main() {
    println(describe("alpha:beta"));
    println(describe("plain"));
}
"#;

const GO_ERROR_LAST_TUPLE_PROGRAM: &str = r#"#[go_error_last]
extern "go" "net" "SplitHostPort" split_host_port(text: string) -> Result[(string, string), GoError]

fn render(text: string) -> Result[string, GoError] {
    let (host, port) = split_host_port(text)?;
    Result::Ok(host + "|" + port)
}

fn show(res: Result[string, GoError]) -> string {
    match res {
        Result::Ok(text) => text,
        Result::Err(err) => err.to_string(),
    }
}

fn main() {
    println(show(render("example.com:443")));
}
"#;

const PROJECT_CONFIG: &str = r#"[crate]
name = "demo"
kind = "bin"
root = "main.gom"
"#;

const PROJECT_MAIN: &str = r#"

fn main() -> unit {
    println("hello")
}
"#;

fn goml_bin() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_goml"))
}

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("goml crate must live under crates/goml")
        .to_path_buf()
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

fn deep_left_nested_tuple_type(depth: usize) -> String {
    let mut ty = "int32".to_string();
    for _ in 0..depth {
        ty = format!("({}, int32)", ty);
    }
    ty
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

fn run_goml(args: &[&str], cwd: &Path) -> anyhow::Result<std::process::Output> {
    Ok(Command::new(goml_bin())
        .args(args)
        .current_dir(cwd)
        .output()?)
}

fn run_goml_with_home(
    args: &[&str],
    cwd: &Path,
    home: &Path,
) -> anyhow::Result<std::process::Output> {
    Ok(Command::new(goml_bin())
        .args(args)
        .current_dir(cwd)
        .env("HOME", home)
        .output()?)
}

fn normalize_temp_prefix(text: &str, root: &Path) -> String {
    text.replace(root.to_string_lossy().as_ref(), "<TMP>")
}

fn current_version_output() -> String {
    match (option_env!("GOML_GIT_HASH"), option_env!("GOML_GIT_DATE")) {
        (Some(hash), Some(date)) => format!("goml {} ({hash} {date})\n", env!("CARGO_PKG_VERSION")),
        _ => format!("goml {}\n", env!("CARGO_PKG_VERSION")),
    }
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

fn create_local_registry(root: &Path) -> anyhow::Result<PathBuf> {
    let registry = root.join("registry");
    fs::create_dir_all(registry.join("alice/http/1.0.0/client"))?;
    fs::create_dir_all(registry.join("alice/http/1.2.0/client"))?;
    fs::create_dir_all(registry.join("alice/net/0.1.0"))?;
    fs::create_dir_all(registry.join("alice/appdep/0.1.0"))?;

    fs::write(
        registry.join("index.toml"),
        r#"[modules."alice::http"]
latest = "1.2.0"
versions = ["1.0.0", "1.2.0"]

[modules."alice::net"]
latest = "0.1.0"
versions = ["0.1.0"]

[modules."alice::appdep"]
latest = "0.1.0"
versions = ["0.1.0"]
"#,
    )?;

    fs::write(
        registry.join("alice/http/1.0.0/goml.toml"),
        r#"[crate]
name = "http"
kind = "lib"
root = "lib.gom"
"#,
    )?;
    fs::write(
        registry.join("alice/http/1.0.0/lib.gom"),
        r#"

pub fn version() -> string {
    "1.0.0"
}
"#,
    )?;
    fs::write(
        registry.join("alice/http/1.0.0/client/mod.gom"),
        r#"

pub fn tag() -> string {
    "client-1.0.0"
}
"#,
    )?;

    fs::write(
        registry.join("alice/http/1.2.0/goml.toml"),
        r#"[crate]
name = "http"
kind = "lib"
root = "lib.gom"

[dependencies]
net = { package = "alice::net", version = "0.1.0" }
"#,
    )?;
    fs::write(
        registry.join("alice/http/1.2.0/lib.gom"),
        r#"
mod client;

use crate::client;

pub fn version() -> string {
    client::tag()
}
"#,
    )?;
    fs::write(
        registry.join("alice/http/1.2.0/client/mod.gom"),
        r#"

pub fn tag() -> string {
    "client-1.2.0"
}
"#,
    )?;

    fs::write(
        registry.join("alice/net/0.1.0/goml.toml"),
        r#"[crate]
name = "net"
kind = "lib"
root = "lib.gom"
"#,
    )?;
    fs::write(
        registry.join("alice/net/0.1.0/lib.gom"),
        r#"

pub fn version() -> string {
    "0.1.0"
}
"#,
    )?;
    fs::write(
        registry.join("alice/appdep/0.1.0/goml.toml"),
        r#"[crate]
name = "appdep"
kind = "lib"
root = "lib.gom"

[dependencies]
http = { package = "alice::http", version = "1.2.0" }
"#,
    )?;
    fs::write(
        registry.join("alice/appdep/0.1.0/lib.gom"),
        r#"

pub fn marker() -> string {
    "appdep"
}
"#,
    )?;

    Command::new("git")
        .args(["init", "--quiet"])
        .current_dir(&registry)
        .output()?;
    Command::new("git")
        .args(["config", "user.email", "goml@example.com"])
        .current_dir(&registry)
        .output()?;
    Command::new("git")
        .args(["config", "user.name", "goml"])
        .current_dir(&registry)
        .output()?;
    Command::new("git")
        .args(["add", "."])
        .current_dir(&registry)
        .output()?;
    Command::new("git")
        .args(["commit", "-m", "init", "--quiet"])
        .current_dir(&registry)
        .output()?;

    Ok(registry)
}

fn run_go_main(path: &Path, cwd: &Path) -> anyhow::Result<Output> {
    if yaegi_available() {
        return Ok(Command::new("yaegi")
            .arg("run")
            .arg(path)
            .current_dir(cwd)
            .output()?);
    }

    Ok(Command::new("go")
        .arg("run")
        .arg(path)
        .current_dir(cwd)
        .env("GOWORK", "off")
        .env("GO111MODULE", "off")
        .output()?)
}

#[test]
fn compiler_run_single_executes_program() -> anyhow::Result<()> {
    if !runtime_executor_available() {
        return Ok(());
    }

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
fn compiler_run_single_falls_back_when_yaegi_cannot_run_function_vectors() -> anyhow::Result<()> {
    if !runtime_executor_available() {
        return Ok(());
    }

    let (_dir, path) = write_program(FUNCTION_VECTOR_PROGRAM)?;

    let output = Command::new(goml_bin())
        .arg("compiler")
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
fn compiler_build_handles_deep_tuple_projection() -> anyhow::Result<()> {
    let input = workspace_root()
        .join("crates/compiler/src/tests/crashers/deep_tuple_projection_stack/main.gom");
    let dir = tempfile::tempdir()?;
    let output_path = dir.path().join("main");

    let output = Command::new(goml_bin())
        .arg("compiler")
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
fn compiler_build_handles_deep_tuple_type() -> anyhow::Result<()> {
    let ty = deep_left_nested_tuple_type(4000);
    let program = format!("fn take(x: {ty}) -> unit {{ () }}\nfn main() -> unit {{ () }}\n");
    let (_input_dir, input) = write_program(&program)?;
    let output_dir = tempfile::tempdir()?;
    let output_path = output_dir.path().join("main");

    let output = Command::new(goml_bin())
        .arg("compiler")
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
fn compile_single_file_supports_go_option_last_tuple_payload() -> anyhow::Result<()> {
    let (_dir, path) = write_program(GO_OPTION_LAST_TUPLE_PROGRAM)?;
    let compilation = compile_single_file(&path, GO_OPTION_LAST_TUPLE_PROGRAM)
        .map_err(|err| anyhow::anyhow!("compilation failed: {:?}", err))?;
    let go = compilation.go.to_pretty(&compilation.goenv, 120);

    assert!(go.contains("func cut_pair_ffi_wrap("), "{go}");
    assert!(go.contains("strings.Cut(p0, p1)"), "{go}");
    assert!(go.contains("return Some{"), "{go}");
    assert!(go.contains("return None{}"), "{go}");

    Ok(())
}

#[test]
fn compile_single_file_supports_go_error_last_tuple_payload() -> anyhow::Result<()> {
    let (_dir, path) = write_program(GO_ERROR_LAST_TUPLE_PROGRAM)?;
    let compilation = compile_single_file(&path, GO_ERROR_LAST_TUPLE_PROGRAM)
        .map_err(|err| anyhow::anyhow!("compilation failed: {:?}", err))?;
    let go = compilation.go.to_pretty(&compilation.goenv, 120);

    assert!(go.contains("func render__native("), "{go}");
    assert!(go.contains("net.SplitHostPort(text__0)"), "{go}");
    assert!(go.contains("return t14, nil"), "{go}");
    assert!(go.contains("return Result__string__GoError_Ok{"), "{go}");
    assert!(go.contains("return Result__string__GoError_Err{"), "{go}");

    Ok(())
}

#[test]
fn update_clones_local_registry_into_cache() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let registry = create_local_registry(dir.path())?;
    let home = dir.path().join("home");
    fs::create_dir_all(&home)?;

    let output = run_goml_with_home(
        &[
            "update",
            "--local-registry",
            registry.to_string_lossy().as_ref(),
        ],
        dir.path(),
        &home,
    )?;
    let stdout = normalize_temp_prefix(&String::from_utf8_lossy(&output.stdout), dir.path());
    let stderr = normalize_temp_prefix(&String::from_utf8_lossy(&output.stderr), dir.path());

    assert!(output.status.success(), "stderr: {stderr}");
    expect!["updated registry cache at <TMP>/home/.goml/cache/registry\n"].assert_eq(&stdout);
    expect![""].assert_eq(&stderr);
    assert!(home.join(".goml/cache/registry/index.toml").exists());

    Ok(())
}

#[test]
fn add_uses_latest_version_from_local_registry() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let registry = create_local_registry(dir.path())?;
    let project_dir = dir.path().join("demo");
    fs::create_dir_all(&project_dir)?;
    write_project(&project_dir)?;

    let output = run_goml(
        &[
            "add",
            "alice::http",
            "--local-registry",
            registry.to_string_lossy().as_ref(),
        ],
        &project_dir,
    )?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(output.status.success(), "stderr: {stderr}");
    expect!["added http = { package = \"alice::http\", version = \"1.2.0\" }\n"].assert_eq(&stdout);
    expect![""].assert_eq(&stderr);

    let manifest = fs::read_to_string(project_dir.join("goml.toml"))?;
    expect![[r#"
        [crate]
        name = "demo"
        kind = "bin"
        root = "main.gom"

        [dependencies]
        http = { package = "alice::http", version = "1.2.0" }
    "#]]
    .assert_eq(&manifest);

    Ok(())
}

#[test]
fn add_reports_dependency_alias_collision() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let registry = create_local_registry(dir.path())?;
    let project_dir = dir.path().join("demo");
    fs::create_dir_all(&project_dir)?;
    fs::write(
        project_dir.join("goml.toml"),
        r#"[crate]
name = "demo"
kind = "bin"
root = "main.gom"

[dependencies]
http = { package = "bob::http", version = "2.0.0" }
"#,
    )?;
    fs::write(project_dir.join("main.gom"), PROJECT_MAIN)?;

    let output = run_goml(
        &[
            "add",
            "alice::http",
            "--local-registry",
            registry.to_string_lossy().as_ref(),
        ],
        &project_dir,
    )?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(!output.status.success());
    expect![""].assert_eq(&stdout);
    expect![
        "dependency alias `http` already exists for package `bob::http`; rename one dependency key manually in goml.toml\n"
    ]
    .assert_eq(&stderr);

    let manifest = fs::read_to_string(project_dir.join("goml.toml"))?;
    expect![[r#"
        [crate]
        name = "demo"
        kind = "bin"
        root = "main.gom"

        [dependencies]
        http = { package = "bob::http", version = "2.0.0" }
    "#]]
    .assert_eq(&manifest);

    Ok(())
}

#[test]
fn add_with_explicit_version_and_remove_updates_manifest() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let registry = create_local_registry(dir.path())?;
    let project_dir = dir.path().join("demo");
    fs::create_dir_all(&project_dir)?;
    write_project(&project_dir)?;

    let add_output = run_goml(
        &[
            "add",
            "alice::http@1.0.0",
            "--local-registry",
            registry.to_string_lossy().as_ref(),
        ],
        &project_dir,
    )?;
    assert!(
        add_output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&add_output.stderr)
    );

    let remove_output = run_goml(
        &[
            "remove",
            "http",
            "--local-registry",
            registry.to_string_lossy().as_ref(),
        ],
        &project_dir,
    )?;
    let stdout = String::from_utf8_lossy(&remove_output.stdout);
    let stderr = String::from_utf8_lossy(&remove_output.stderr);
    assert!(remove_output.status.success(), "stderr: {stderr}");
    expect!["removed http\n"].assert_eq(&stdout);
    expect![""].assert_eq(&stderr);

    let manifest = fs::read_to_string(project_dir.join("goml.toml"))?;
    expect![[r#"
        [crate]
        name = "demo"
        kind = "bin"
        root = "main.gom"
    "#]]
    .assert_eq(&manifest);

    Ok(())
}

#[test]
fn project_build_with_cached_registry_dependencies_uses_external_modules() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let registry = create_local_registry(dir.path())?;
    let home = dir.path().join("home");
    fs::create_dir_all(&home)?;

    let project_dir = dir.path().join("demo");
    fs::create_dir_all(&project_dir)?;
    fs::write(
        project_dir.join("goml.toml"),
        r#"[crate]
name = "demo"
kind = "bin"
root = "main.gom"

[dependencies]
http = { package = "alice::http", version = "1.0.0" }
appdep = { package = "alice::appdep", version = "0.1.0" }
"#,
    )?;
    fs::write(
        project_dir.join("main.gom"),
        r#"

use alice::http;
use alice::http::client;
use alice::appdep;

fn main() -> unit {
    println(http::version() + ":" + client::tag() + ":" + appdep::marker())
}
"#,
    )?;

    let update_output = run_goml_with_home(
        &[
            "update",
            "--local-registry",
            registry.to_string_lossy().as_ref(),
        ],
        &project_dir,
        &home,
    )?;
    assert!(
        update_output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&update_output.stderr)
    );

    let check_output = run_goml_with_home(&["check"], &project_dir, &home)?;
    assert!(
        check_output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&check_output.stderr)
    );

    let build_output = run_goml_with_home(&["build"], &project_dir, &home)?;
    assert!(
        build_output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&build_output.stderr)
    );

    assert!(
        project_dir
            .join("target/goml/check/deps/alice/http/1.2.0/http.interface")
            .exists()
    );
    assert!(
        project_dir
            .join("target/goml/build/deps/alice/http/1.2.0/http.core")
            .exists()
    );
    assert!(
        project_dir
            .join("target/goml/build/deps/alice/appdep/0.1.0/appdep.core")
            .exists()
    );

    if !runtime_executor_available() {
        return Ok(());
    }
    let go_output = run_go_main(&project_dir.join("target/goml/main.go"), &project_dir)?;
    let go_stdout = String::from_utf8_lossy(&go_output.stdout);
    let go_stderr = String::from_utf8_lossy(&go_output.stderr);
    assert!(go_output.status.success(), "stderr: {go_stderr}");
    expect!["client-1.2.0:client-1.2.0:appdep\n"].assert_eq(&go_stdout);

    Ok(())
}

#[test]
fn compiler_run_single_dumps_requested_stages() -> anyhow::Result<()> {
    if !runtime_executor_available() {
        return Ok(());
    }

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

        type GoError = error

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
fn version_prints_crate_version() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let output = Command::new(goml_bin())
        .arg("version")
        .current_dir(dir.path())
        .output()?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(output.status.success(), "stderr: {stderr}");
    assert_eq!(stdout, current_version_output());
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

    if !runtime_executor_available() {
        return Ok(());
    }
    let go_output = run_go_main(&go_file, dir.path())?;

    let go_stdout = String::from_utf8_lossy(&go_output.stdout);
    let go_stderr = String::from_utf8_lossy(&go_output.stderr);

    assert!(go_output.status.success(), "stderr: {go_stderr}");
    expect!["hello\n"].assert_eq(&go_stdout);

    Ok(())
}

#[test]
fn project_build_bin_crate_reports_missing_main() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let root = dir.path();
    fs::write(
        root.join("goml.toml"),
        r#"[crate]
name = "demo"
kind = "bin"
root = "main.gom"
"#,
    )?;
    fs::write(
        root.join("main.gom"),
        r#"
fn not_main() -> unit {
    ()
}
"#,
    )?;

    let output = run_goml(&["build"], root)?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(!output.status.success());
    expect![""].assert_eq(&stdout);
    expect![[r#"
        link failed: Compile { diagnostics: Diagnostics { items: [Diagnostic { stage: Other("compile"), severity: Error, message: "main package missing main function", range: None }] } }
        subcommand failed: goml compiler link --input target/goml/build/main.core --output target/goml/main.go
    "#]]
    .assert_eq(&stderr);
    assert!(!root.join("target/goml/main.go").exists());

    Ok(())
}

#[test]
fn project_build_lib_crate_does_not_link_or_require_main() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let root = dir.path();
    fs::write(
        root.join("goml.toml"),
        r#"[crate]
name = "math"
kind = "lib"
root = "src/lib.gom"
"#,
    )?;
    fs::create_dir_all(root.join("src"))?;
    fs::write(
        root.join("src/lib.gom"),
        r#"
pub fn add(a: int64, b: int64) -> int64 {
    a + b
}
"#,
    )?;

    let output = run_goml(&["build"], root)?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(output.status.success(), "stderr: {stderr}");
    expect![""].assert_eq(&stdout);
    expect![""].assert_eq(&stderr);
    assert!(root.join("target/goml/build/src/lib.core").exists());
    assert!(root.join("target/goml/build/src/lib.interface").exists());
    assert!(!root.join("target/goml/main.go").exists());

    Ok(())
}

#[test]
fn project_build_lib_crate_dry_run_omits_link() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let root = dir.path();
    fs::write(
        root.join("goml.toml"),
        r#"[crate]
name = "math"
kind = "lib"
root = "src/lib.gom"
"#,
    )?;
    fs::create_dir_all(root.join("src"))?;
    fs::write(
        root.join("src/lib.gom"),
        r#"
pub fn add(a: int64, b: int64) -> int64 {
    a + b
}
"#,
    )?;

    let output = run_goml(&["build", "--dry-run"], root)?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(output.status.success(), "stderr: {stderr}");
    expect!["goml compiler build --package main --input src/lib.gom --output target/goml/build/src/lib\n"]
        .assert_eq(&stdout);
    expect![""].assert_eq(&stderr);

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
    let main_gom = fs::read_to_string(project_dir.join("src/main.gom"))?;
    let lib_gom = fs::read_to_string(project_dir.join("src/lib.gom"))?;

    expect![[r#"
        [crate]
        name = "demo"
        kind = "bin"
        root = "src/main.gom"
    "#]]
    .assert_eq(&root_toml);
    expect![[r#"
        mod lib;

        fn main() -> unit {
            println(crate::lib::message())
        }
    "#]]
    .assert_eq(&main_gom);
    expect![[r#"
        pub fn message() -> string {
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
    assert!(project_dir.join("target/goml/build/src/main.core").exists());
    assert!(
        project_dir
            .join("target/goml/build/src/main.interface")
            .exists()
    );
    assert!(project_dir.join("target/goml/build/src/lib.core").exists());
    assert!(
        project_dir
            .join("target/goml/build/src/lib.interface")
            .exists()
    );

    if !runtime_executor_available() {
        return Ok(());
    }
    let go_output = run_go_main(&go_file, &project_dir)?;

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
        goml compiler check --package traitpkg --input traitpkg/mod.gom --output target/goml/check/traitpkg/traitpkg
        goml compiler check --package datapkg --input datapkg/mod.gom --interface-path target/goml/check/traitpkg/traitpkg.interface --output target/goml/check/datapkg/datapkg
        goml compiler check --package usepkg --input usepkg/mod.gom --interface-path target/goml/check/traitpkg/traitpkg.interface --output target/goml/check/usepkg/usepkg
        goml compiler check --package main --input main.gom --interface-path target/goml/check/datapkg/datapkg.interface --interface-path target/goml/check/traitpkg/traitpkg.interface --interface-path target/goml/check/usepkg/usepkg.interface --output target/goml/check/main
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
        goml compiler build --package traitpkg --input traitpkg/mod.gom --output target/goml/build/traitpkg/traitpkg
        goml compiler build --package datapkg --input datapkg/mod.gom --interface-path target/goml/build/traitpkg/traitpkg.interface --output target/goml/build/datapkg/datapkg
        goml compiler build --package usepkg --input usepkg/mod.gom --interface-path target/goml/build/traitpkg/traitpkg.interface --output target/goml/build/usepkg/usepkg
        goml compiler build --package main --input main.gom --interface-path target/goml/build/datapkg/datapkg.interface --interface-path target/goml/build/traitpkg/traitpkg.interface --interface-path target/goml/build/usepkg/usepkg.interface --output target/goml/build/main
        goml compiler link --input target/goml/build/traitpkg/traitpkg.core target/goml/build/datapkg/datapkg.core target/goml/build/usepkg/usepkg.core target/goml/build/main.core --output target/goml/main.go
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

        if !runtime_executor_available() {
            continue;
        }
        let go_output = run_go_main(&go_file, dir.path())?;
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
        r#"[crate]
name = "demo"
kind = "bin"
root = "main.gom"
"#,
    )?;
    fs::write(
        root.join("main.gom"),
        r#"

mod A;
mod B;

fn main() -> unit {
    println(crate::A::msg())
}
"#,
    )?;

    fs::create_dir_all(root.join("A"))?;
    fs::write(
        root.join("A/mod.gom"),
        r#"

use crate::B;

pub fn msg() -> string {
    crate::B::value()
}
"#,
    )?;

    fs::create_dir_all(root.join("B"))?;
    fs::write(
        root.join("B/mod.gom"),
        r#"

pub fn value() -> int32 {
    1
}
"#,
    )?;

    let output = run_goml(&["build"], root)?;
    let stderr = normalize_temp_prefix(&String::from_utf8_lossy(&output.stderr), root);
    assert!(!output.status.success());
    expect![[r#"
        build failed: Typer { diagnostics: Diagnostics { items: [Diagnostic { stage: Typer, severity: Error, message: "Type mismatch: expected int32, found string", range: Some(46..64) }, Diagnostic { stage: Typer, severity: Error, message: "Type mismatch: expected int32, found string", range: Some(46..64) }] } }
        subcommand failed: goml compiler build --package A --input A/mod.gom --interface-path target/goml/build/B/B.interface --output target/goml/build/A/A
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
        r#"[crate]
name = "demo"
kind = "bin"
root = "src/main.gom"
"#,
    )?;
    fs::create_dir_all(root.join("src"))?;
    fs::write(
        root.join("src/main.gom"),
        r#"

mod Lib;

fn main() -> unit {
    println(crate::Lib::msg())
}
"#,
    )?;

    fs::create_dir_all(root.join("src/Lib"))?;
    fs::write(
        root.join("src/Lib/mod.gom"),
        r#"

pub fn msg() -> string {
    "ok"
}
"#,
    )?;

    let output = run_goml(&["build", "--dry-run"], root)?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "stderr: {stderr}");
    expect![[r#"
        goml compiler build --package Lib --input src/Lib/mod.gom --output target/goml/build/src/Lib/Lib
        goml compiler build --package main --input src/main.gom --interface-path target/goml/build/src/Lib/Lib.interface --output target/goml/build/src/main
        goml compiler link --input target/goml/build/src/Lib/Lib.core target/goml/build/src/main.core --output target/goml/main.go
    "#]]
    .assert_eq(&stdout);
    expect![""].assert_eq(&stderr);

    Ok(())
}
