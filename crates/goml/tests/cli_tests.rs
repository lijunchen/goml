use std::fs;
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};
use std::sync::OnceLock;

use expect_test::expect;
use tempfile::TempDir;

const PROJECT_CONFIG: &str = r#"[module]
path = "demo"
"#;

const PROJECT_MAIN: &str = r#"
package main;

fn main() -> unit {
    println("hello")
}
"#;

fn goml_bin() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_goml"))
}

fn write_project(root: &Path) -> anyhow::Result<()> {
    fs::write(root.join("goml.toml"), PROJECT_CONFIG)?;
    fs::write(root.join("main.gom"), PROJECT_MAIN)?;
    Ok(())
}

fn deep_ref_type(depth: usize) -> String {
    let mut ty = "int32".to_string();
    for _ in 0..depth {
        ty = format!("Ref[{ty}]");
    }
    ty
}

fn wide_struct_pattern_program(field_count: usize) -> String {
    let fields = (0..field_count)
        .map(|idx| format!("f{}: int32", idx))
        .collect::<Vec<_>>()
        .join(", ");
    let values = (0..field_count)
        .map(|idx| format!("f{}: {}", idx, idx))
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

fn run_goml(args: &[&str], cwd: &Path) -> anyhow::Result<std::process::Output> {
    Ok(Command::new(goml_bin())
        .args(args)
        .current_dir(cwd)
        .output()?)
}

#[cfg(unix)]
fn write_executable(path: &Path, source: &str) -> anyhow::Result<()> {
    fs::write(path, source)?;
    let mut permissions = fs::metadata(path)?.permissions();
    permissions.set_mode(0o755);
    fs::set_permissions(path, permissions)?;
    Ok(())
}

#[cfg(unix)]
fn path_with_first(first: &Path) -> anyhow::Result<std::ffi::OsString> {
    let mut paths = vec![first.to_path_buf()];
    if let Some(path) = std::env::var_os("PATH") {
        paths.extend(std::env::split_paths(&path));
    }
    Ok(std::env::join_paths(paths)?)
}

#[cfg(unix)]
fn write_fake_go(root: &Path) -> anyhow::Result<(PathBuf, PathBuf)> {
    let bin = root.join("fake-bin");
    fs::create_dir_all(&bin)?;
    let runner = root.join("fake-runner");
    write_executable(
        &runner,
        r#"#!/bin/sh
if [ -n "$RUN_CWD_LOG" ]; then printf '%s\n' "$PWD" > "$RUN_CWD_LOG"; fi
if [ -n "$RUN_ENV_LOG" ]; then printf '%s\n' "$RUN_MARKER" > "$RUN_ENV_LOG"; fi
if [ -n "$RUN_ARGS_LOG" ]; then printf '%s\n' "$@" > "$RUN_ARGS_LOG"; fi
if [ -n "$RUN_STDOUT" ]; then printf '%s' "$RUN_STDOUT"; fi
if [ -n "$RUN_STDERR" ]; then printf '%s' "$RUN_STDERR" >&2; fi
exit "${RUN_EXIT:-0}"
"#,
    )?;
    write_executable(
        &bin.join("go"),
        r#"#!/bin/sh
if [ "$GOWORK" != "off" ] || [ "$GO111MODULE" != "off" ]; then exit 91; fi
if [ "$1" != "build" ] || [ "$2" != "-o" ]; then exit 92; fi
if [ -n "$FAKE_GO_ENV_LOG" ]; then printf '%s\n%s\n' "$GOWORK" "$GO111MODULE" > "$FAKE_GO_ENV_LOG"; fi
if [ -n "$FAKE_GO_ARGS_LOG" ]; then printf '%s\n' "$@" > "$FAKE_GO_ARGS_LOG"; fi
if [ -n "$FAKE_GO_EXIT" ]; then exit "$FAKE_GO_EXIT"; fi
mkdir -p "$(dirname "$3")"
cp "$FAKE_RUNNER" "$3"
chmod +x "$3"
"#,
    )?;
    Ok((bin, runner))
}

#[cfg(unix)]
fn write_fake_gomlc(path: &Path, identity: &str) -> anyhow::Result<()> {
    write_executable(
        path,
        &format!(
            r#"#!/bin/sh
identity={identity}
if [ "$1" = "version" ]; then
    printf '%s\n' '{{"driver_protocol":1}}'
    exit 0
fi
kind=$1
shift
inputs=
output=
manifest=
package=
while [ "$#" -gt 0 ]; do
    case "$1" in
        --input)
            shift
            while [ "$#" -gt 0 ]; do
                case "$1" in --*) break ;; esac
                inputs="$inputs $1"
                shift
            done
            ;;
        --interface-path)
            inputs="$inputs $2"
            shift 2
            ;;
        --output)
            output=$2
            shift 2
            ;;
        --manifest)
            manifest=$2
            shift 2
            ;;
        --package)
            package=$2
            shift 2
            ;;
        --entry)
            shift 2
            ;;
        *)
            shift
            ;;
    esac
done
if [ -n "$FAKE_GOMLC_LOG" ]; then
    printf '%s:%s\n' "$kind" "$package" >> "$FAKE_GOMLC_LOG"
fi
if [ "$FAKE_GOMLC_FAIL_BEFORE" = "$kind" ]; then
    exit 71
fi
if [ "$FAKE_GOMLC_OMIT_OUTPUT" != "$kind" ]; then
    case "$kind" in
        check|test-check)
            mkdir -p "$(dirname "$output")"
            {{
                printf '%s:%s\n' "$identity" "$kind"
                for input in $inputs; do
                    case "$input" in
                        *.gom) sed -n '/^[[:space:]]*pub /p' "$input" ;;
                        *) cat "$input" ;;
                    esac
                done
            }} > "$output.interface"
            ;;
        build|test-build)
            mkdir -p "$(dirname "$output")"
            {{
                printf '%s:%s\n' "$identity" "$kind"
                for input in $inputs; do
                    case "$input" in
                        *.gom) sed -n '/^[[:space:]]*pub /p' "$input" ;;
                        *) cat "$input" ;;
                    esac
                done
            }} > "$output.interface"
            {{
                printf '%s:%s\n' "$identity" "$kind"
                for input in $inputs; do cat "$input"; done
            }} > "$output.core"
            ;;
        link)
            mkdir -p "$(dirname "$output")"
            {{
                printf 'package main\nfunc main() {{}}\n'
                for input in $inputs; do cat "$input"; done
            }} > "$output"
            ;;
        test-link)
            mkdir -p "$(dirname "$output")"
            printf 'package main\nfunc main() {{}}\n' > "$output"
            printf '[]\n' > "$manifest"
            ;;
    esac
fi
if [ "$FAKE_GOMLC_FAIL_AFTER_OUTPUT" = "$kind" ]; then
    exit 72
fi
if [ "$FAKE_GOMLC_MUTATE" = "$kind" ]; then
    printf '\n' >> "$0"
fi
if [ "$FAKE_GOMLC_MUTATE_INPUT" = "$kind" ]; then
    set -- $inputs
    printf '\n' >> "$1"
fi
"#
        ),
    )
}

#[cfg(unix)]
fn fake_compiler_command(args: &[&str], cwd: &Path, compiler: &Path, log: &Path) -> Command {
    let mut command = Command::new(goml_bin());
    command
        .args(args)
        .arg("--compiler")
        .arg(compiler)
        .current_dir(cwd)
        .env("FAKE_GOMLC_LOG", log);
    command
}

#[cfg(unix)]
fn invocation_log(path: &Path) -> anyhow::Result<Vec<String>> {
    match fs::read_to_string(path) {
        Ok(log) => Ok(log.lines().map(str::to_string).collect()),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(Vec::new()),
        Err(error) => Err(error.into()),
    }
}

#[cfg(unix)]
fn clear_invocation_log(path: &Path) -> anyhow::Result<()> {
    if path.exists() {
        fs::remove_file(path)?;
    }
    Ok(())
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

fn run_goml_with_goml_home(
    args: &[&str],
    cwd: &Path,
    home: &Path,
) -> anyhow::Result<std::process::Output> {
    Ok(Command::new(goml_bin())
        .args(args)
        .current_dir(cwd)
        .env("GOML_HOME", home)
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
    fs::create_dir_all(registry.join("alice/traits/1.0.0"))?;
    fs::create_dir_all(registry.join("alice/shared/1.0.0"))?;
    fs::create_dir_all(registry.join("alice/stdio/1.0.0"))?;
    fs::create_dir_all(registry.join("alice/model/1.0.0"))?;
    fs::create_dir_all(registry.join("bob/data/1.0.0"))?;
    fs::create_dir_all(registry.join("bob/facade/1.0.0"))?;
    fs::create_dir_all(registry.join("bob/shared/1.0.0"))?;

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

[modules."alice::traits"]
latest = "1.0.0"
versions = ["1.0.0"]

[modules."alice::shared"]
latest = "1.0.0"
versions = ["1.0.0"]

[modules."alice::stdio"]
latest = "1.0.0"
versions = ["1.0.0"]

[modules."alice::model"]
latest = "1.0.0"
versions = ["1.0.0"]

[modules."bob::data"]
latest = "1.0.0"
versions = ["1.0.0"]

[modules."bob::facade"]
latest = "1.0.0"
versions = ["1.0.0"]

[modules."bob::shared"]
latest = "1.0.0"
versions = ["1.0.0"]
"#,
    )?;

    fs::write(
        registry.join("alice/http/1.0.0/goml.toml"),
        r#"[module]
path = "alice::http"
"#,
    )?;
    fs::write(
        registry.join("alice/http/1.0.0/lib.gom"),
        r#"
package http;

pub fn version() -> string {
    "1.0.0"
}
"#,
    )?;
    fs::write(
        registry.join("alice/http/1.0.0/client/client.gom"),
        r#"
package client;

pub fn tag() -> string {
    "client-1.0.0"
}
"#,
    )?;

    fs::write(
        registry.join("alice/http/1.2.0/goml.toml"),
        r#"[module]
path = "alice::http"

[dependencies]
"alice::net" = "0.1.0"
"#,
    )?;
    fs::write(
        registry.join("alice/http/1.2.0/lib.gom"),
        r#"
package http;

use alice::http::client;

pub fn version() -> string {
    client::tag()
}
"#,
    )?;
    fs::write(
        registry.join("alice/http/1.2.0/client/client.gom"),
        r#"
package client;

pub fn tag() -> string {
    "client-1.2.0"
}
"#,
    )?;

    fs::write(
        registry.join("alice/net/0.1.0/goml.toml"),
        r#"[module]
path = "alice::net"
"#,
    )?;
    fs::write(
        registry.join("alice/net/0.1.0/lib.gom"),
        r#"
package net;

pub fn version() -> string {
    "0.1.0"
}
"#,
    )?;
    fs::write(
        registry.join("alice/appdep/0.1.0/goml.toml"),
        r#"[module]
path = "alice::appdep"

[dependencies]
"alice::http" = "1.2.0"
"#,
    )?;
    fs::write(
        registry.join("alice/appdep/0.1.0/lib.gom"),
        r#"
package appdep;

pub fn marker() -> string {
    "appdep"
}
"#,
    )?;
    fs::write(
        registry.join("alice/traits/1.0.0/goml.toml"),
        r#"[module]
path = "alice::traits"
"#,
    )?;
    fs::write(
        registry.join("alice/traits/1.0.0/lib.gom"),
        r#"
package traits;

pub trait Show {
    fn show(self: Self) -> string;
}
"#,
    )?;
    fs::write(
        registry.join("bob/data/1.0.0/goml.toml"),
        r#"[module]
path = "bob::data"

[dependencies]
"alice::traits" = "1.0.0"
"#,
    )?;
    fs::write(
        registry.join("bob/data/1.0.0/lib.gom"),
        r#"
package data;

use alice::traits;

pub struct Box {
    pub value: int32,
}

impl traits::Show for Box {
    fn show(self: Box) -> string {
        self.value.to_string()
    }
}

pub fn make() -> Box {
    Box { value: 21 }
}
"#,
    )?;
    fs::write(
        registry.join("alice/shared/1.0.0/goml.toml"),
        "[module]\npath = \"alice::shared\"\n",
    )?;
    fs::write(
        registry.join("alice/shared/1.0.0/lib.gom"),
        "package shared;\n\npub fn value() -> int32 { 20 }\n",
    )?;
    fs::write(
        registry.join("alice/stdio/1.0.0/goml.toml"),
        "[module]\npath = \"alice::stdio\"\n",
    )?;
    fs::write(
        registry.join("alice/stdio/1.0.0/lib.gom"),
        r#"package stdio;

use std::io;

pub fn write(value: string) -> unit {
    io::println(value)
}
"#,
    )?;
    fs::write(
        registry.join("alice/model/1.0.0/goml.toml"),
        "[module]\npath = \"alice::model\"\n",
    )?;
    fs::write(
        registry.join("alice/model/1.0.0/lib.gom"),
        r#"package model;

pub struct Box {
    pub value: int32,
}

pub fn make() -> Box {
    Box { value: 42 }
}
"#,
    )?;
    fs::write(
        registry.join("bob/facade/1.0.0/goml.toml"),
        r#"[module]
path = "bob::facade"

[dependencies]
"alice::model" = "1.0.0"
"#,
    )?;
    fs::write(
        registry.join("bob/facade/1.0.0/lib.gom"),
        r#"package facade;

use alice::model;

pub fn make() -> model::Box {
    model::make()
}
"#,
    )?;
    fs::write(
        registry.join("bob/shared/1.0.0/goml.toml"),
        "[module]\npath = \"bob::shared\"\n",
    )?;
    fs::write(
        registry.join("bob/shared/1.0.0/lib.gom"),
        "package shared;\n\npub fn value() -> int32 { 22 }\n",
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
fn project_build_handles_very_wide_struct_pattern() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    fs::write(
        dir.path().join("goml.toml"),
        r#"[module]
path = "wideproj"
"#,
    )?;
    fs::write(
        dir.path().join("main.gom"),
        wide_struct_pattern_program(2600),
    )?;

    let output = run_goml(&["build"], dir.path())?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        output.status.success(),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    assert!(dir.path().join("_artifact/main.go").exists());

    Ok(())
}

#[test]
fn project_build_handles_deep_public_interface_type() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    fs::write(
        dir.path().join("goml.toml"),
        r#"[module]
path = "iface_deep"
"#,
    )?;
    fs::create_dir_all(dir.path().join("Lib"))?;
    fs::write(
        dir.path().join("Lib/Lib.gom"),
        format!(
            "package lib;\n\npub struct Wrap {{ value: {} }}\n\npub fn ping() -> int32 {{ 1 }}\n",
            deep_ref_type(200)
        ),
    )?;
    fs::write(
        dir.path().join("main.gom"),
        "package main;\n\nuse iface_deep::Lib;\n\nfn main() -> unit { println(lib::ping().to_string()) }\n",
    )?;

    let output = run_goml(&["build"], dir.path())?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        output.status.success(),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    assert!(dir.path().join("_artifact/main.go").exists());

    Ok(())
}

#[test]
fn project_build_loads_transitive_public_type_interfaces() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    fs::write(
        dir.path().join("goml.toml"),
        "[module]\npath = \"transitive_types\"\n",
    )?;
    fs::create_dir_all(dir.path().join("model"))?;
    fs::create_dir_all(dir.path().join("facade"))?;
    fs::write(
        dir.path().join("model/model.gom"),
        r#"package model;

pub struct Box {
    pub value: int32,
}

pub fn make() -> Box {
    Box { value: 42 }
}
"#,
    )?;
    fs::write(
        dir.path().join("facade/facade.gom"),
        r#"package facade;

use transitive_types::model;

pub fn make() -> model::Box {
    model::make()
}
"#,
    )?;
    fs::write(
        dir.path().join("main.gom"),
        r#"package main;

use transitive_types::facade;

fn main() -> unit {
    println(facade::make().value.to_string())
}
"#,
    )?;

    let output = run_goml(&["build"], dir.path())?;
    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    if runtime_executor_available() {
        let go_output = run_go_main(&dir.path().join("_artifact/main.go"), dir.path())?;
        assert!(
            go_output.status.success(),
            "stderr: {}",
            String::from_utf8_lossy(&go_output.stderr)
        );
        expect!["42\n"].assert_eq(&String::from_utf8_lossy(&go_output.stdout));
    }

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
    expect!["added alice::http = 1.2.0\n"].assert_eq(&stdout);
    expect![""].assert_eq(&stderr);

    let manifest = fs::read_to_string(project_dir.join("goml.toml"))?;
    expect![[r#"
        [module]
        path = "demo"

        [dependencies]
        "alice::http" = "1.2.0"
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
    fs::write(
        project_dir.join("goml.toml"),
        "[module]\npath = \"demo\"\n\n[build]\ntarget-dir = \"out\"\n",
    )?;

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
            "alice::http",
            "--local-registry",
            registry.to_string_lossy().as_ref(),
        ],
        &project_dir,
    )?;
    let stdout = String::from_utf8_lossy(&remove_output.stdout);
    let stderr = String::from_utf8_lossy(&remove_output.stderr);
    assert!(remove_output.status.success(), "stderr: {stderr}");
    expect!["removed alice::http\n"].assert_eq(&stdout);
    expect![""].assert_eq(&stderr);

    let manifest = fs::read_to_string(project_dir.join("goml.toml"))?;
    expect![[r#"
        [module]
        path = "demo"

        [build]
        target-dir = "out"
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
        r#"[module]
path = "demo"

[dependencies]
"alice::http" = "1.0.0"
"alice::appdep" = "0.1.0"
"#,
    )?;
    fs::write(
        project_dir.join("main.gom"),
        r#"
package main;

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
            .join("_artifact/check/deps/alice/http/1.2.0/pkg/alice/http/package.interface")
            .exists()
    );
    assert!(
        project_dir
            .join("_artifact/build/deps/alice/http/1.2.0/pkg/alice/http/package.core")
            .exists()
    );
    assert!(
        project_dir
            .join("_artifact/build/deps/alice/appdep/0.1.0/pkg/alice/appdep/package.core")
            .exists()
    );

    if !runtime_executor_available() {
        return Ok(());
    }
    let go_output = run_go_main(&project_dir.join("_artifact/main.go"), &project_dir)?;
    let go_stdout = String::from_utf8_lossy(&go_output.stdout);
    let go_stderr = String::from_utf8_lossy(&go_output.stderr);
    assert!(go_output.status.success(), "stderr: {go_stderr}");
    expect!["client-1.2.0:client-1.2.0:appdep\n"].assert_eq(&go_stdout);

    Ok(())
}

#[test]
fn project_build_external_package_can_import_std() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let registry = create_local_registry(dir.path())?;
    let home = dir.path().join("home");
    fs::create_dir_all(&home)?;
    let project_dir = dir.path().join("demo");
    fs::create_dir_all(&project_dir)?;
    fs::write(
        project_dir.join("goml.toml"),
        r#"[module]
path = "demo"

[dependencies]
"alice::stdio" = "1.0.0"
"#,
    )?;
    fs::write(
        project_dir.join("main.gom"),
        r#"package main;

use alice::stdio;

fn main() -> unit {
    stdio::write("external-std")
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
    let build_output = run_goml_with_home(&["build"], &project_dir, &home)?;
    assert!(
        build_output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&build_output.stderr)
    );

    if runtime_executor_available() {
        let go_output = run_go_main(&project_dir.join("_artifact/main.go"), &project_dir)?;
        assert!(
            go_output.status.success(),
            "stderr: {}",
            String::from_utf8_lossy(&go_output.stderr)
        );
        expect!["external-std\n"].assert_eq(&String::from_utf8_lossy(&go_output.stdout));
    }

    Ok(())
}

#[test]
fn project_build_loads_transitive_external_type_interfaces() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let registry = create_local_registry(dir.path())?;
    let home = dir.path().join("home");
    fs::create_dir_all(&home)?;
    let project_dir = dir.path().join("demo");
    fs::create_dir_all(&project_dir)?;
    fs::write(
        project_dir.join("goml.toml"),
        r#"[module]
path = "demo"

[dependencies]
"bob::facade" = "1.0.0"
"#,
    )?;
    fs::write(
        project_dir.join("main.gom"),
        r#"package main;

use bob::facade;

fn main() -> unit {
    println(facade::make().value.to_string())
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
    let build_output = run_goml_with_home(&["build"], &project_dir, &home)?;
    assert!(
        build_output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&build_output.stderr)
    );

    if runtime_executor_available() {
        let go_output = run_go_main(&project_dir.join("_artifact/main.go"), &project_dir)?;
        assert!(
            go_output.status.success(),
            "stderr: {}",
            String::from_utf8_lossy(&go_output.stderr)
        );
        expect!["42\n"].assert_eq(&String::from_utf8_lossy(&go_output.stdout));
    }

    Ok(())
}

#[test]
fn project_build_supports_same_named_external_packages_with_aliases() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let registry = create_local_registry(dir.path())?;
    let home = dir.path().join("home");
    fs::create_dir_all(&home)?;
    let project_dir = dir.path().join("demo");
    fs::create_dir_all(&project_dir)?;
    fs::write(
        project_dir.join("goml.toml"),
        r#"[module]
path = "demo"

[dependencies]
"alice::shared" = "1.0.0"
"bob::shared" = "1.0.0"
"#,
    )?;
    fs::write(
        project_dir.join("main.gom"),
        r#"package main;

use alice::shared as alice_shared;
use bob::shared as bob_shared;

fn main() -> unit {
    println(alice_shared::value() + bob_shared::value())
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
    let build_output = run_goml_with_home(&["build"], &project_dir, &home)?;
    assert!(
        build_output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&build_output.stderr)
    );

    if !runtime_executor_available() {
        return Ok(());
    }
    let go_output = run_go_main(&project_dir.join("_artifact/main.go"), &project_dir)?;
    assert!(
        go_output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&go_output.stderr)
    );
    expect!["42\n"].assert_eq(&String::from_utf8_lossy(&go_output.stdout));

    Ok(())
}

#[test]
fn project_build_imports_transitive_external_trait() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let registry = create_local_registry(dir.path())?;
    let home = dir.path().join("home");
    fs::create_dir_all(&home)?;

    let project_dir = dir.path().join("demo");
    fs::create_dir_all(&project_dir)?;
    fs::write(
        project_dir.join("goml.toml"),
        r#"[module]
path = "demo"

[dependencies]
"bob::data" = "1.0.0"
"#,
    )?;
    fs::write(
        project_dir.join("main.gom"),
        r#"
package main;

use bob::data;
use alice::traits;

use traits::Show;

fn main() -> unit {
    let value = data::make();
    println(value.show())
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

    let build_output = run_goml_with_home(&["build"], &project_dir, &home)?;
    assert!(
        build_output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&build_output.stderr)
    );

    if !runtime_executor_available() {
        return Ok(());
    }
    let go_output = run_go_main(&project_dir.join("_artifact/main.go"), &project_dir)?;
    let go_stdout = String::from_utf8_lossy(&go_output.stdout);
    let go_stderr = String::from_utf8_lossy(&go_output.stderr);
    assert!(go_output.status.success(), "stderr: {go_stderr}");
    expect!["21\n"].assert_eq(&go_stdout);

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
fn home_prints_goml_home_layout() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let home = dir.path().join("goml-home");
    let output = run_goml_with_goml_home(&["home"], dir.path(), &home)?;

    let stdout = normalize_temp_prefix(&String::from_utf8_lossy(&output.stdout), dir.path());
    let stderr = normalize_temp_prefix(&String::from_utf8_lossy(&output.stderr), dir.path());

    assert!(output.status.success(), "stderr: {stderr}");
    expect![[r#"
        GOML_HOME=<TMP>/goml-home
        bin=<TMP>/goml-home/bin
        lib=<TMP>/goml-home/lib
        std=<TMP>/goml-home/lib/std
        cache=<TMP>/goml-home/cache
    "#]]
    .assert_eq(&stdout);
    expect![""].assert_eq(&stderr);
    assert!(home.join("bin").is_dir());
    assert!(home.join("lib").is_dir());
    assert!(home.join("cache").is_dir());

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
fn project_build_writes_default_artifact_layout() -> anyhow::Result<()> {
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

    let go_file = dir.path().join("_artifact/main.go");
    assert!(go_file.exists());
    assert!(
        dir.path()
            .join("_artifact/bin")
            .join(format!("demo{}", std::env::consts::EXE_SUFFIX))
            .exists()
    );
    assert!(!dir.path().join("target/goml").exists());

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
fn project_uses_manifest_and_cli_target_directories() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    write_project(dir.path())?;
    fs::write(
        dir.path().join("goml.toml"),
        "[module]\npath = \"demo\"\n\n[build]\ntarget-dir = \"out/generated\"\n",
    )?;
    fs::write(
        dir.path().join("main_test.gom"),
        "package main;\n#[test]\nfn works() -> unit { () }\n",
    )?;
    fs::create_dir_all(dir.path().join("tests/api"))?;
    fs::write(
        dir.path().join("tests/api/api_test.gom"),
        "package api;\n#[test]\nfn external_works() -> unit { () }\n",
    )?;

    let build = run_goml(&["build"], dir.path())?;
    assert!(
        build.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&build.stderr)
    );
    assert!(dir.path().join("out/generated/main.go").exists());
    assert!(
        dir.path()
            .join("out/generated/bin")
            .join(format!("demo{}", std::env::consts::EXE_SUFFIX))
            .exists()
    );
    assert!(
        dir.path()
            .join("out/generated/build/pkg/demo/package.core")
            .exists()
    );
    assert!(!dir.path().join("_artifact").exists());

    let test = run_goml(&["test", "--dry-run"], dir.path())?;
    assert!(test.status.success());
    let test_stdout = String::from_utf8(test.stdout)?;
    assert!(test_stdout.contains("out/generated/test/internal/pkg/demo/package.core"));
    assert!(test_stdout.contains("out/generated/test/internal/tests.json"));
    assert!(test_stdout.contains("out/generated/test/external/tests.json"));

    if go_available() {
        let test = run_goml(&["test"], dir.path())?;
        assert!(
            test.status.success(),
            "stderr: {}",
            String::from_utf8_lossy(&test.stderr)
        );
        let runner = format!("runner{}", std::env::consts::EXE_SUFFIX);
        assert!(
            dir.path()
                .join("out/generated/test/internal")
                .join(&runner)
                .exists()
        );
        assert!(
            dir.path()
                .join("out/generated/test/external")
                .join(runner)
                .exists()
        );
    }

    let check = run_goml(&["check", "--target-dir", "cli output"], dir.path())?;
    assert!(
        check.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&check.stderr)
    );
    assert!(
        dir.path()
            .join("cli output/check/pkg/demo/package.interface")
            .exists()
    );

    let absolute_target = dir.path().join("absolute output");
    let absolute_target_text = absolute_target.to_string_lossy();
    let absolute = run_goml(
        &["check", "--target-dir", absolute_target_text.as_ref()],
        dir.path(),
    )?;
    assert!(
        absolute.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&absolute.stderr)
    );
    assert!(
        absolute_target
            .join("check/pkg/demo/package.interface")
            .exists()
    );

    let invalid = run_goml(&["check", "--target-dir", "."], dir.path())?;
    assert!(!invalid.status.success());
    assert!(String::from_utf8(invalid.stderr)?.contains("must not be the module root"));

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
    let gitignore = fs::read_to_string(project_dir.join(".gitignore"))?;
    let main_gom = fs::read_to_string(project_dir.join("main.gom"))?;
    let lib_gom = fs::read_to_string(project_dir.join("lib/lib.gom"))?;

    expect![[r#"
        [module]
        path = "demo"
    "#]]
    .assert_eq(&root_toml);
    expect![[r#"
        /_artifact/
    "#]]
    .assert_eq(&gitignore);
    expect![[r#"
        package main;

        use demo::lib;

        fn main() -> unit {
            println(lib::message())
        }
    "#]]
    .assert_eq(&main_gom);
    expect![[r#"
        package lib;

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

    let go_file = project_dir.join("_artifact/main.go");
    assert!(go_file.exists());
    assert!(
        project_dir
            .join("_artifact/build/pkg/demo/package.core")
            .exists()
    );
    assert!(
        project_dir
            .join("_artifact/build/pkg/demo/package.interface")
            .exists()
    );
    assert!(
        project_dir
            .join("_artifact/build/pkg/demo/lib/package.core")
            .exists()
    );
    assert!(
        project_dir
            .join("_artifact/build/pkg/demo/lib/package.interface")
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
fn project_check_and_build_support_std_imports() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    fs::write(dir.path().join("goml.toml"), PROJECT_CONFIG)?;
    fs::write(
        dir.path().join("main.gom"),
        r#"
package main;

use std::io;

fn main() -> unit {
    io::println("std-project")
}
"#,
    )?;

    let check_output = run_goml(&["check"], dir.path())?;
    let check_stderr = String::from_utf8_lossy(&check_output.stderr);
    assert!(check_output.status.success(), "stderr: {check_stderr}");

    let build_output = run_goml(&["build"], dir.path())?;
    let build_stderr = String::from_utf8_lossy(&build_output.stderr);
    assert!(build_output.status.success(), "stderr: {build_stderr}");

    let go_file = dir.path().join("_artifact/main.go");
    assert!(go_file.exists());

    if runtime_executor_available() {
        let go_output = run_go_main(&go_file, dir.path())?;
        let go_stdout = String::from_utf8_lossy(&go_output.stdout);
        let go_stderr = String::from_utf8_lossy(&go_output.stderr);
        assert!(go_output.status.success(), "stderr: {go_stderr}");
        expect!["std-project\n"].assert_eq(&go_stdout);
    }

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
        gomlc check --package project008::traitpkg --input traitpkg/traitpkg.gom --output _artifact/check/pkg/project008/traitpkg/package
        gomlc check --package project008::datapkg --input datapkg/datapkg.gom --interface-path _artifact/check/pkg/project008/traitpkg/package.interface --output _artifact/check/pkg/project008/datapkg/package
        gomlc check --package project008::usepkg --input usepkg/usepkg.gom --interface-path _artifact/check/pkg/project008/traitpkg/package.interface --output _artifact/check/pkg/project008/usepkg/package
        gomlc check --package project008 --input main.gom --interface-path _artifact/check/pkg/project008/datapkg/package.interface --interface-path _artifact/check/pkg/project008/traitpkg/package.interface --interface-path _artifact/check/pkg/project008/usepkg/package.interface --output _artifact/check/pkg/project008/package
    "#]]
    .assert_eq(&stdout);
    assert!(!dir.path().join("_artifact/check/main.interface").exists());
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
        gomlc build --package project008::traitpkg --input traitpkg/traitpkg.gom --output _artifact/build/pkg/project008/traitpkg/package
        gomlc build --package project008::datapkg --input datapkg/datapkg.gom --interface-path _artifact/build/pkg/project008/traitpkg/package.interface --output _artifact/build/pkg/project008/datapkg/package
        gomlc build --package project008::usepkg --input usepkg/usepkg.gom --interface-path _artifact/build/pkg/project008/traitpkg/package.interface --output _artifact/build/pkg/project008/usepkg/package
        gomlc build --package project008 --input main.gom --interface-path _artifact/build/pkg/project008/datapkg/package.interface --interface-path _artifact/build/pkg/project008/traitpkg/package.interface --interface-path _artifact/build/pkg/project008/usepkg/package.interface --output _artifact/build/pkg/project008/package
        gomlc link --input _artifact/build/pkg/project008/traitpkg/package.core _artifact/build/pkg/project008/datapkg/package.core _artifact/build/pkg/project008/usepkg/package.core _artifact/build/pkg/project008/package.core --output _artifact/main.go --entry project008
        go build -o _artifact/bin/project008 _artifact/main.go
    "#]]
    .assert_eq(&stdout);
    assert!(!dir.path().join("_artifact/main.go").exists());
    expect![""].assert_eq(&stderr);

    Ok(())
}

#[test]
fn project_run_dry_run_prints_the_complete_plan_without_tools() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    write_project(dir.path())?;
    let missing = dir.path().join("missing-gomlc");
    let output = Command::new(goml_bin())
        .args(["run", "--dry-run", "--compiler"])
        .arg(&missing)
        .args(["--", "alpha", "--flag", "two words"])
        .current_dir(dir.path())
        .env("PATH", "")
        .env("GOMLC", &missing)
        .output()?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "stderr: {stderr}");
    expect![[r#"
        gomlc build --package demo --input main.gom --output _artifact/build/pkg/demo/package
        gomlc link --input _artifact/build/pkg/demo/package.core --output _artifact/main.go --entry demo
        go build -o _artifact/bin/demo _artifact/main.go
        _artifact/bin/demo alpha --flag "two words"
    "#]]
    .assert_eq(&stdout);
    assert!(!dir.path().join("_artifact").exists());
    expect![""].assert_eq(&stderr);

    Ok(())
}

#[test]
fn project_build_creates_and_runs_the_native_binary() -> anyhow::Result<()> {
    if !go_available() {
        return Ok(());
    }
    let dir = tempfile::tempdir()?;
    write_project(dir.path())?;
    let output = run_goml(&["build"], dir.path())?;

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "stderr: {stderr}");
    let binary = dir
        .path()
        .join("_artifact/bin")
        .join(format!("demo{}", std::env::consts::EXE_SUFFIX));
    assert!(binary.is_file());
    let execution = Command::new(binary).output()?;
    assert!(execution.status.success());
    expect!["hello\n"].assert_eq(&String::from_utf8_lossy(&execution.stdout));

    Ok(())
}

#[cfg(unix)]
#[test]
fn project_build_isolates_go_and_writes_the_expected_binary() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    write_project(dir.path())?;
    let (fake_bin, runner) = write_fake_go(dir.path())?;
    let env_log = dir.path().join("go-env.log");
    let args_log = dir.path().join("go-args.log");
    let output = Command::new(goml_bin())
        .arg("build")
        .current_dir(dir.path())
        .env("PATH", path_with_first(&fake_bin)?)
        .env("FAKE_RUNNER", &runner)
        .env("FAKE_GO_ENV_LOG", &env_log)
        .env("FAKE_GO_ARGS_LOG", &args_log)
        .env("GOWORK", "unexpected")
        .env("GO111MODULE", "unexpected")
        .output()?;

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "stderr: {stderr}");
    expect!["off\noff\n"].assert_eq(&fs::read_to_string(env_log)?);
    let binary = dir.path().join("_artifact/bin/demo");
    assert!(binary.is_file());
    expect![[r#"
        build
        -o
        _artifact/bin/demo
        _artifact/main.go
    "#]]
    .assert_eq(&fs::read_to_string(args_log)?);

    Ok(())
}

#[cfg(unix)]
#[test]
fn project_run_forwards_args_env_io_and_preserves_the_callers_cwd() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let project = dir.path().join("project");
    let caller = dir.path().join("caller");
    fs::create_dir_all(&project)?;
    fs::create_dir_all(&caller)?;
    write_project(&project)?;
    let (fake_bin, runner) = write_fake_go(dir.path())?;
    let cwd_log = dir.path().join("run-cwd.log");
    let env_log = dir.path().join("run-env.log");
    let args_log = dir.path().join("run-args.log");
    let output = Command::new(goml_bin())
        .arg("run")
        .arg(&project)
        .args(["--", "alpha", "--flag", "two words"])
        .current_dir(&caller)
        .env("PATH", path_with_first(&fake_bin)?)
        .env("FAKE_RUNNER", &runner)
        .env("RUN_CWD_LOG", &cwd_log)
        .env("RUN_ENV_LOG", &env_log)
        .env("RUN_ARGS_LOG", &args_log)
        .env("RUN_MARKER", "inherited")
        .env("RUN_STDOUT", "program stdout\n")
        .env("RUN_STDERR", "program stderr\n")
        .output()?;

    assert!(output.status.success());
    expect!["program stdout\n"].assert_eq(&String::from_utf8_lossy(&output.stdout));
    expect!["program stderr\n"].assert_eq(&String::from_utf8_lossy(&output.stderr));
    assert_eq!(
        fs::read_to_string(cwd_log)?,
        format!("{}\n", caller.display())
    );
    expect!["inherited\n"].assert_eq(&fs::read_to_string(env_log)?);
    expect!["alpha\n--flag\ntwo words\n"].assert_eq(&fs::read_to_string(args_log)?);

    Ok(())
}

#[cfg(unix)]
#[test]
fn project_run_propagates_the_program_exit_code() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    write_project(dir.path())?;
    let (fake_bin, runner) = write_fake_go(dir.path())?;
    let output = Command::new(goml_bin())
        .arg("run")
        .current_dir(dir.path())
        .env("PATH", path_with_first(&fake_bin)?)
        .env("FAKE_RUNNER", &runner)
        .env("RUN_EXIT", "37")
        .output()?;

    assert_eq!(output.status.code(), Some(37));
    expect![""].assert_eq(&String::from_utf8_lossy(&output.stderr));

    Ok(())
}

#[cfg(unix)]
#[test]
fn project_build_reports_go_build_failures() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    write_project(dir.path())?;
    let (fake_bin, runner) = write_fake_go(dir.path())?;
    let output = Command::new(goml_bin())
        .arg("build")
        .current_dir(dir.path())
        .env("PATH", path_with_first(&fake_bin)?)
        .env("FAKE_RUNNER", &runner)
        .env("FAKE_GO_EXIT", "23")
        .output()?;

    assert_eq!(output.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("go build failed with status"));
    assert!(!dir.path().join("_artifact/bin/demo").exists());

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

        let go_file = dir.path().join("_artifact/main.go");
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
        r#"[module]
path = "demo"
"#,
    )?;
    fs::write(
        root.join("main.gom"),
        r#"
package main;

use demo::A;

fn main() -> unit {
    println(A::msg())
}
"#,
    )?;

    fs::create_dir_all(root.join("A"))?;
    fs::write(
        root.join("A/A.gom"),
        r#"
package A;

use demo::B;

pub fn msg() -> string {
    B::value()
}
"#,
    )?;

    fs::create_dir_all(root.join("B"))?;
    fs::write(
        root.join("B/B.gom"),
        r#"
package B;

pub fn value() -> int32 {
    1
}
"#,
    )?;

    let output = run_goml(&["build"], root)?;
    let stderr = normalize_temp_prefix(&String::from_utf8_lossy(&output.stderr), root);
    assert!(!output.status.success());
    expect![[r#"
        project build failed: 2:9: package name must start with a lowercase letter or `_`
    "#]]
    .assert_eq(&stderr);
    assert!(!root.join("_artifact/main.go").exists());

    Ok(())
}

#[test]
fn project_build_dry_run_preserves_entry_directory_structure() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let root = dir.path();

    fs::write(
        root.join("goml.toml"),
        r#"[module]
path = "demo"
"#,
    )?;
    fs::create_dir_all(root.join("src"))?;
    fs::write(
        root.join("src/main.gom"),
        r#"
package main;

use demo::src::Lib;

fn main() -> unit {
    println(lib::msg())
}
"#,
    )?;

    fs::create_dir_all(root.join("src/Lib"))?;
    fs::write(
        root.join("src/Lib/Lib.gom"),
        r#"
package lib;

pub fn msg() -> string {
    "ok"
}
"#,
    )?;

    let output = run_goml(&["build", "src", "--dry-run"], root)?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "stderr: {stderr}");
    expect![[r#"
        gomlc build --package demo::src::Lib --input src/Lib/Lib.gom --output _artifact/build/pkg/demo/src/Lib/package
        gomlc build --package demo::src --input src/main.gom --interface-path _artifact/build/pkg/demo/src/Lib/package.interface --output _artifact/build/pkg/demo/src/package
        gomlc link --input _artifact/build/pkg/demo/src/Lib/package.core _artifact/build/pkg/demo/src/package.core --output _artifact/main.go --entry demo::src
        go build -o _artifact/bin/src _artifact/main.go
    "#]]
    .assert_eq(&stdout);
    expect![""].assert_eq(&stderr);

    let output = run_goml(
        &[
            "run",
            "src",
            "--target-dir",
            "output",
            "--dry-run",
            "--",
            "value",
        ],
        root,
    )?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "stderr: {stderr}");
    assert!(stdout.contains("go build -o output/bin/src output/main.go\n"));
    assert!(stdout.ends_with("output/bin/src value\n"));
    assert!(!root.join("output").exists());

    Ok(())
}

#[test]
fn project_test_runs_private_tests_and_ignores_test_sources_in_check() -> anyhow::Result<()> {
    if !go_available() {
        return Ok(());
    }
    let dir = tempfile::tempdir()?;
    let root = dir.path();
    fs::write(root.join("goml.toml"), "[module]\npath = \"demo\"\n")?;
    fs::create_dir_all(root.join("math"))?;
    fs::write(
        root.join("math/math.gom"),
        r#"
package math;

fn double(value: int32) -> int32 {
    value * 2
}

pub fn add(left: int32, right: int32) -> int32 {
    left + right
}
"#,
    )?;
    fs::write(
        root.join("math/math_test.gom"),
        r#"
package math;

use std::testing;

#[test]
fn add_works() -> unit {
    string_println("captured marker");
    testing::assert_eq(add(2, 3), 5)
}

#[test]
fn private_helper_works() -> unit {
    testing::assert_eq(double(4), 8)
}

#[test]
#[ignore("later")]
fn ignored_case() -> unit {
    testing::assert(true)
}
"#,
    )?;

    let check = run_goml(&["check", "math", "--dry-run"], root)?;
    assert!(check.status.success());
    let check_stdout = String::from_utf8(check.stdout)?;
    assert!(check_stdout.contains("--input math/math.gom"));
    assert!(!check_stdout.contains("math_test.gom"));

    let test = run_goml(&["test", "math", "--jobs", "2"], root)?;
    let stdout = String::from_utf8(test.stdout)?;
    let stderr = String::from_utf8(test.stderr)?;
    assert!(test.status.success(), "stderr: {stderr}");
    expect![[r#"
        running 3 tests

        test demo::math::add_works ... ok
        test demo::math::ignored_case ... ignored: later
        test demo::math::private_helper_works ... ok

        result: ok. 2 passed; 0 failed; 1 ignored
    "#]]
    .assert_eq(&stdout);
    expect![""].assert_eq(&stderr);

    let list = run_goml(&["test", "math", "--list"], root)?;
    assert!(list.status.success());
    expect![[r#"
        demo::math::add_works
        demo::math::ignored_case: ignored
        demo::math::private_helper_works
    "#]]
    .assert_eq(&String::from_utf8(list.stdout)?);

    let included = run_goml(&["test", "math", "--include-ignored"], root)?;
    assert!(included.status.success());
    assert!(String::from_utf8(included.stdout)?.contains("3 passed; 0 failed; 0 ignored"));

    let uncaptured = run_goml(&["test", "math", "add_works", "--nocapture"], root)?;
    assert!(uncaptured.status.success());
    assert!(String::from_utf8(uncaptured.stdout)?.contains("captured marker"));

    let json = run_goml(&["test", "math", "add_works", "--format", "json"], root)?;
    assert!(json.status.success());
    let events = String::from_utf8(json.stdout)?
        .lines()
        .map(serde_json::from_str::<serde_json::Value>)
        .collect::<Result<Vec<_>, _>>()?;
    assert_eq!(events[0]["event"], "result");
    assert_eq!(events[0]["status"], "passed");
    assert_eq!(events[0]["stdout"], "captured marker\n");
    assert_eq!(events[1]["event"], "summary");

    Ok(())
}

#[test]
fn project_check_selects_normal_internal_and_external_test_modes() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let root = dir.path();
    fs::write(root.join("goml.toml"), "[module]\npath = \"demo\"\n")?;
    fs::create_dir_all(root.join("math/tests/api"))?;
    fs::create_dir_all(root.join("math/tests/smoke"))?;
    fs::write(
        root.join("math/math.gom"),
        r#"
package math;

fn private_value() -> int32 {
    41
}

pub fn public_value() -> int32 {
    private_value() + 1
}
"#,
    )?;
    fs::write(
        root.join("math/math_test.gom"),
        r#"
package math;

use std::testing;

#[test]
fn white_box() -> unit {
    testing::assert_eq(private_value(), 41)
}
"#,
    )?;
    fs::write(
        root.join("math/tests/api/api_test.gom"),
        r#"
package api;

use demo::math;
use std::testing;

#[test]
fn black_box() -> unit {
    testing::assert_eq(math::public_value(), 42)
}
"#,
    )?;
    fs::write(
        root.join("math/tests/smoke/smoke_test.gom"),
        r#"
package smoke;

use demo::math;
use std::testing;

#[test]
fn second_black_box_suite() -> unit {
    testing::assert_eq(math::public_value(), 42)
}
"#,
    )?;

    let normal = run_goml(&["check", "math"], root)?;
    assert!(
        normal.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&normal.stderr)
    );
    let all = run_goml(&["check", "math", "--tests"], root)?;
    assert!(
        all.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&all.stderr)
    );
    let internal = run_goml(&["check", "math/math_test.gom"], root)?;
    assert!(
        internal.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&internal.stderr)
    );
    let external = run_goml(&["check", "math/tests/api/api_test.gom"], root)?;
    assert!(
        external.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&external.stderr)
    );

    let dry_run = run_goml(&["test", "math", "--dry-run"], root)?;
    assert!(dry_run.status.success());
    let dry_run = String::from_utf8(dry_run.stdout)?;
    assert!(dry_run.contains("gomlc check --package demo::math --input math/math.gom"));
    assert!(dry_run.contains(
        "gomlc test-build --package demo::math --input math/math.gom --input math/math_test.gom"
    ));
    assert!(dry_run.contains(
        "gomlc build --package demo::math --input math/math.gom --output _artifact/test/external/pkg/demo/math/package"
    ));
    assert!(dry_run.contains("gomlc test-build --package demo::math::tests::api"));
    assert!(dry_run.contains("gomlc test-build --package demo::math::tests::smoke"));
    assert_eq!(dry_run.matches("gomlc test-link").count(), 2);

    if go_available() {
        let external_tests = run_goml(&["test", "math", "--kind", "external"], root)?;
        assert!(
            external_tests.status.success(),
            "stderr: {}",
            String::from_utf8_lossy(&external_tests.stderr)
        );
        let stdout = String::from_utf8(external_tests.stdout)?;
        assert!(stdout.contains("demo::math::tests::api::black_box ... ok"));
        assert!(stdout.contains("demo::math::tests::smoke::second_black_box_suite ... ok"));
        assert!(!stdout.contains("white_box"));

        let internal_tests = run_goml(&["test", "math", "--kind", "internal"], root)?;
        assert!(
            internal_tests.status.success(),
            "stderr: {}",
            String::from_utf8_lossy(&internal_tests.stderr)
        );
        let stdout = String::from_utf8(internal_tests.stdout)?;
        assert!(stdout.contains("demo::math::white_box ... ok"));
        assert!(!stdout.contains("black_box"));
    }

    fs::write(
        root.join("math/tests/api/api_test.gom"),
        r#"
package api;

use demo::math;

#[test]
fn cannot_see_private_items() -> unit {
    let _ = math::private_value();
    ()
}
"#,
    )?;
    let private = run_goml(&["check", "math/tests/api/api_test.gom"], root)?;
    assert!(!private.status.success());
    assert!(String::from_utf8(private.stderr)?.contains("private_value"));

    fs::write(
        root.join("math/math_test.gom"),
        "package math;\n#[test]\nfn broken() -> unit { missing_test_value() }\n",
    )?;
    let normal = run_goml(&["check", "math"], root)?;
    assert!(normal.status.success());
    let tests = run_goml(&["check", "math", "--tests"], root)?;
    assert!(!tests.status.success());
    assert!(String::from_utf8(tests.stderr)?.contains("missing_test_value"));

    Ok(())
}

#[test]
fn test_sources_cannot_repair_a_broken_normal_package() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let root = dir.path();
    std::fs::write(root.join("goml.toml"), "[module]\npath = \"demo\"\n")?;
    std::fs::create_dir_all(root.join("value"))?;
    std::fs::write(
        root.join("value/value.gom"),
        "package value;\npub fn value() -> int32 { test_only_value() }\n",
    )?;
    std::fs::write(
        root.join("value/value_test.gom"),
        r#"package value;

fn test_only_value() -> int32 {
    1
}

#[test]
fn value_works() -> unit {
    let _ = value();
    ()
}
"#,
    )?;

    let checked = run_goml(&["check", "value", "--tests"], root)?;
    assert!(!checked.status.success());
    assert!(String::from_utf8(checked.stderr)?.contains("test_only_value"));
    let tested = run_goml(&["test", "value"], root)?;
    assert!(!tested.status.success());
    assert!(String::from_utf8(tested.stderr)?.contains("test_only_value"));
    Ok(())
}

#[test]
fn project_test_reports_assertion_failures_and_timeouts() -> anyhow::Result<()> {
    if !go_available() {
        return Ok(());
    }
    let dir = tempfile::tempdir()?;
    let root = dir.path();
    fs::write(root.join("goml.toml"), "[module]\npath = \"demo\"\n")?;
    fs::create_dir_all(root.join("checks"))?;
    fs::write(
        root.join("checks/checks.gom"),
        "package checks;\npub fn value() -> int32 { 1 }\n",
    )?;
    fs::write(
        root.join("checks/checks_test.gom"),
        r#"
package checks;

use std::testing;

#[test]
#[ignore("failure case")]
fn failure() -> unit {
    testing::assert_eq(1, 2)
}

#[test]
#[ignore("timeout case")]
fn timeout() -> unit {
    while true {
        ()
    }
}
"#,
    )?;

    let failure = run_goml(&["test", "checks", "failure", "--ignored"], root)?;
    assert!(!failure.status.success());
    let failure_stdout = String::from_utf8(failure.stdout)?;
    assert!(failure_stdout.contains("FAILED (exit code 101)"));
    assert!(failure_stdout.contains("actual: 1"));
    assert!(failure_stdout.contains("expected: 2"));

    let timeout = run_goml(
        &[
            "test",
            "checks",
            "timeout",
            "--ignored",
            "--timeout",
            "20ms",
        ],
        root,
    )?;
    assert!(!timeout.status.success());
    assert!(String::from_utf8(timeout.stdout)?.contains("FAILED (timed out)"));

    Ok(())
}

#[test]
fn project_test_dry_run_and_invalid_signature_diagnostics() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let root = dir.path();
    fs::write(root.join("goml.toml"), "[module]\npath = \"demo\"\n")?;
    fs::create_dir_all(root.join("value"))?;
    fs::write(
        root.join("value/value.gom"),
        "package value;\npub fn value() -> int32 { 1 }\n",
    )?;
    fs::write(
        root.join("value/value_test.gom"),
        r#"
package value;

#[test]
fn invalid[T](value: T) -> bool {
    true
}
"#,
    )?;

    let dry_run = run_goml(&["test", "value", "--dry-run"], root)?;
    assert!(dry_run.status.success());
    let stdout = String::from_utf8(dry_run.stdout)?;
    assert!(stdout.contains(
        "gomlc test-build --package demo::value --input value/value.gom --input value/value_test.gom"
    ));
    assert!(stdout.contains(
        "gomlc test-link --input _artifact/test/internal/pkg/demo/value/package.core --output _artifact/test/internal/main.go --manifest _artifact/test/internal/tests.json --package demo::value"
    ));

    let output = run_goml(&["test", "value"], root)?;
    assert!(!output.status.success());
    let stderr = String::from_utf8(output.stderr)?;
    assert!(stderr.contains("must not have type parameters"));
    assert!(stderr.contains("must not have parameters"));
    assert!(stderr.contains("must return unit"));

    Ok(())
}

#[cfg(unix)]
#[test]
fn incremental_build_recompiles_only_changed_packages_and_dependents() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let root = dir.path();
    fs::write(root.join("goml.toml"), "[module]\npath = \"demo\"\n")?;
    fs::create_dir_all(root.join("dep"))?;
    fs::write(
        root.join("dep/dep.gom"),
        "package dep;\n\npub fn value() -> int32 {\n    private_value()\n}\n\nfn private_value() -> int32 {\n    1\n}\n",
    )?;
    fs::write(
        root.join("main.gom"),
        "package main;\n\nuse demo::dep;\n\nfn main() -> unit {\n    println(dep::value().to_string())\n}\n",
    )?;
    let compiler = root.join("fake-gomlc");
    let log = root.join("gomlc.log");
    write_fake_gomlc(&compiler, "first")?;
    let (fake_bin, runner) = write_fake_go(root)?;

    let mut build = fake_compiler_command(&["build"], root, &compiler, &log);
    let first = build
        .env("PATH", path_with_first(&fake_bin)?)
        .env("FAKE_RUNNER", &runner)
        .output()?;
    assert!(
        first.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&first.stderr)
    );
    assert_eq!(
        invocation_log(&log)?,
        ["build:demo::dep", "build:demo", "link:"]
    );

    clear_invocation_log(&log)?;
    let second = build.output()?;
    assert!(second.status.success());
    assert!(invocation_log(&log)?.is_empty());

    fs::write(
        root.join("dep/dep.gom"),
        "package dep;\n\npub fn value() -> int32 {\n    private_value()\n}\n\nfn private_value() -> int32 {\n    2\n}\n",
    )?;
    clear_invocation_log(&log)?;
    let private_change = build.output()?;
    assert!(private_change.status.success());
    assert_eq!(invocation_log(&log)?, ["build:demo::dep", "link:"]);

    fs::write(
        root.join("dep/dep.gom"),
        "package dep;\n\npub fn value() -> int64 {\n    private_value()\n}\n\nfn private_value() -> int64 {\n    2\n}\n",
    )?;
    clear_invocation_log(&log)?;
    let public_change = build.output()?;
    assert!(public_change.status.success());
    assert_eq!(
        invocation_log(&log)?,
        ["build:demo::dep", "build:demo", "link:"]
    );

    Ok(())
}

#[cfg(unix)]
#[test]
fn incremental_cache_invalidates_missing_corrupt_and_compiler_changed_outputs() -> anyhow::Result<()>
{
    let dir = tempfile::tempdir()?;
    let root = dir.path();
    write_project(root)?;
    let compiler = root.join("fake-gomlc");
    let log = root.join("gomlc.log");
    write_fake_gomlc(&compiler, "first")?;

    let mut check = fake_compiler_command(&["check"], root, &compiler, &log);
    let first = check.output()?;
    assert!(first.status.success());
    assert_eq!(invocation_log(&log)?, ["check:demo"]);

    clear_invocation_log(&log)?;
    let hit = check.output()?;
    assert!(hit.status.success());
    assert!(invocation_log(&log)?.is_empty());

    let interface = root.join("_artifact/check/pkg/demo/package.interface");
    fs::remove_file(&interface)?;
    clear_invocation_log(&log)?;
    let missing = check.output()?;
    assert!(missing.status.success());
    assert_eq!(invocation_log(&log)?, ["check:demo"]);

    fs::write(&interface, "corrupt")?;
    clear_invocation_log(&log)?;
    let corrupt = check.output()?;
    assert!(corrupt.status.success());
    assert_eq!(invocation_log(&log)?, ["check:demo"]);

    write_fake_gomlc(&compiler, "second")?;
    clear_invocation_log(&log)?;
    let changed_compiler = check.output()?;
    assert!(changed_compiler.status.success());
    assert_eq!(invocation_log(&log)?, ["check:demo"]);

    let stdlib = root.join("stdlib");
    fs::create_dir_all(&stdlib)?;
    fs::write(stdlib.join("goml.toml"), "[module]\npath = \"std\"\n")?;
    fs::write(stdlib.join("lib.gom"), "first")?;
    check.env("GOML_STD_PATH", "stdlib");
    clear_invocation_log(&log)?;
    assert!(check.output()?.status.success());
    assert_eq!(invocation_log(&log)?, ["check:demo"]);
    clear_invocation_log(&log)?;
    assert!(check.output()?.status.success());
    assert!(invocation_log(&log)?.is_empty());
    fs::write(stdlib.join("README.md"), "ignored")?;
    fs::create_dir_all(stdlib.join("_artifact/cache"))?;
    fs::write(stdlib.join("_artifact/cache/data"), "ignored")?;
    clear_invocation_log(&log)?;
    assert!(check.output()?.status.success());
    assert!(invocation_log(&log)?.is_empty());
    fs::write(stdlib.join("lib.gom"), "second")?;
    clear_invocation_log(&log)?;
    assert!(check.output()?.status.success());
    assert_eq!(invocation_log(&log)?, ["check:demo"]);
    fs::write(
        stdlib.join("goml.toml"),
        "[module]\npath = \"std_changed\"\n",
    )?;
    clear_invocation_log(&log)?;
    assert!(check.output()?.status.success());
    assert_eq!(invocation_log(&log)?, ["check:demo"]);

    Ok(())
}

#[cfg(unix)]
#[test]
fn incremental_build_invalidates_each_missing_expected_output() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let root = dir.path();
    write_project(root)?;
    let compiler = root.join("fake-gomlc");
    let log = root.join("gomlc.log");
    write_fake_gomlc(&compiler, "first")?;
    let (fake_bin, runner) = write_fake_go(root)?;
    let mut build = fake_compiler_command(&["build"], root, &compiler, &log);
    build
        .env("PATH", path_with_first(&fake_bin)?)
        .env("FAKE_RUNNER", &runner);

    assert!(build.output()?.status.success());
    clear_invocation_log(&log)?;
    assert!(build.output()?.status.success());
    assert!(invocation_log(&log)?.is_empty());

    fs::remove_file(root.join("_artifact/build/pkg/demo/package.interface"))?;
    clear_invocation_log(&log)?;
    assert!(build.output()?.status.success());
    assert_eq!(invocation_log(&log)?, ["build:demo"]);

    fs::remove_file(root.join("_artifact/build/pkg/demo/package.core"))?;
    clear_invocation_log(&log)?;
    assert!(build.output()?.status.success());
    assert_eq!(invocation_log(&log)?, ["build:demo"]);

    fs::remove_file(root.join("_artifact/main.go"))?;
    clear_invocation_log(&log)?;
    assert!(build.output()?.status.success());
    assert_eq!(invocation_log(&log)?, ["link:"]);

    Ok(())
}

#[cfg(unix)]
#[test]
fn failed_or_incomplete_commands_never_leave_valid_fingerprints() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let root = dir.path();
    write_project(root)?;
    let compiler = root.join("fake-gomlc");
    let log = root.join("gomlc.log");
    write_fake_gomlc(&compiler, "first")?;
    let fingerprint = root.join("_artifact/check/pkg/demo/package.goml-check-fingerprint");

    let mut check = fake_compiler_command(&["check"], root, &compiler, &log);
    assert!(check.output()?.status.success());
    assert!(fingerprint.is_file());

    fs::write(
        root.join("main.gom"),
        "package main;\nfn main() -> unit { println(\"changed\") }\n",
    )?;
    let failed = check
        .env("FAKE_GOMLC_FAIL_AFTER_OUTPUT", "check")
        .output()?;
    assert!(!failed.status.success());
    assert!(!fingerprint.exists());

    clear_invocation_log(&log)?;
    check.env_remove("FAKE_GOMLC_FAIL_AFTER_OUTPUT");
    assert!(check.output()?.status.success());
    assert_eq!(invocation_log(&log)?, ["check:demo"]);
    assert!(fingerprint.is_file());

    fs::write(
        root.join("main.gom"),
        "package main;\nfn main() -> unit { println(\"changed again\") }\n",
    )?;
    let incomplete = check.env("FAKE_GOMLC_OMIT_OUTPUT", "check").output()?;
    assert!(!incomplete.status.success());
    assert!(!fingerprint.exists());
    assert!(
        !root
            .join("_artifact/check/pkg/demo/package.interface")
            .exists()
    );

    clear_invocation_log(&log)?;
    check.env_remove("FAKE_GOMLC_OMIT_OUTPUT");
    assert!(check.output()?.status.success());
    assert_eq!(invocation_log(&log)?, ["check:demo"]);

    fs::write(
        root.join("main.gom"),
        "package main;\nfn main() -> unit { println(\"toolchain race\") }\n",
    )?;
    let changed_during_plan = check.env("FAKE_GOMLC_MUTATE", "check").output()?;
    assert!(!changed_during_plan.status.success());
    assert!(
        String::from_utf8(changed_during_plan.stderr)?
            .contains("compiler or standard library changed while executing the command plan")
    );
    assert!(!fingerprint.exists());
    assert!(
        !root
            .join("_artifact/check/pkg/demo/package.interface")
            .exists()
    );

    clear_invocation_log(&log)?;
    check.env_remove("FAKE_GOMLC_MUTATE");
    assert!(check.output()?.status.success());
    assert_eq!(invocation_log(&log)?, ["check:demo"]);

    fs::write(
        root.join("main.gom"),
        "package main;\nfn main() -> unit { println(\"input race\") }\n",
    )?;
    let changed_input = check.env("FAKE_GOMLC_MUTATE_INPUT", "check").output()?;
    assert!(!changed_input.status.success());
    assert!(
        String::from_utf8(changed_input.stderr)?
            .contains("cache input changed while executing subcommand")
    );
    assert!(!fingerprint.exists());
    assert!(
        !root
            .join("_artifact/check/pkg/demo/package.interface")
            .exists()
    );

    clear_invocation_log(&log)?;
    check.env_remove("FAKE_GOMLC_MUTATE_INPUT");
    assert!(check.output()?.status.success());
    assert_eq!(invocation_log(&log)?, ["check:demo"]);

    Ok(())
}

#[cfg(unix)]
#[test]
fn incremental_cache_isolates_check_build_and_test_commands() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let root = dir.path();
    write_project(root)?;
    fs::write(
        root.join("main_test.gom"),
        "package main;\n#[test]\nfn works() -> unit { () }\n",
    )?;
    let compiler = root.join("fake-gomlc");
    let log = root.join("gomlc.log");
    write_fake_gomlc(&compiler, "first")?;

    let mut check = fake_compiler_command(&["check"], root, &compiler, &log);
    assert!(check.output()?.status.success());
    assert_eq!(invocation_log(&log)?, ["check:demo"]);

    let (fake_bin, runner) = write_fake_go(root)?;
    clear_invocation_log(&log)?;
    let mut build = fake_compiler_command(&["build"], root, &compiler, &log);
    build
        .env("PATH", path_with_first(&fake_bin)?)
        .env("FAKE_RUNNER", &runner);
    assert!(build.output()?.status.success());
    assert_eq!(invocation_log(&log)?, ["build:demo", "link:"]);

    clear_invocation_log(&log)?;
    let mut test = fake_compiler_command(
        &["test", "--list", "--kind", "internal"],
        root,
        &compiler,
        &log,
    );
    assert!(test.output()?.status.success());
    assert_eq!(invocation_log(&log)?, ["test-build:demo", "test-link:demo"]);

    clear_invocation_log(&log)?;
    assert!(test.output()?.status.success());
    assert!(invocation_log(&log)?.is_empty());

    clear_invocation_log(&log)?;
    let mut tests_check = fake_compiler_command(&["check", "--tests"], root, &compiler, &log);
    assert!(tests_check.output()?.status.success());
    assert_eq!(invocation_log(&log)?, ["test-check:demo"]);
    let anchor = root.join("_artifact/check/pkg/demo/package");
    assert!(PathBuf::from(format!("{}.goml-check-fingerprint", anchor.display())).is_file());
    assert!(PathBuf::from(format!("{}.goml-test-check-fingerprint", anchor.display())).is_file());

    clear_invocation_log(&log)?;
    assert!(check.output()?.status.success());
    assert_eq!(invocation_log(&log)?, ["check:demo"]);

    Ok(())
}
