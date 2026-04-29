use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::OnceLock;

use compiler::artifact::{CoreUnit, InterfaceUnit};
use expect_test::expect_file;
use xshell::{Shell, cmd};

fn goml_bin() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_goml"))
}

fn write_file(path: &Path, content: &str) -> anyhow::Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(path, content)?;
    Ok(())
}

fn write_manifest(dir: &Path, name: &str, kind: &str, root: &str) -> anyhow::Result<()> {
    write_file(
        &dir.join("goml.toml"),
        &format!(
            r#"[crate]
name = "{name}"
kind = "{kind}"
root = "{root}"
"#
        ),
    )
}

fn read_json<T: serde::de::DeserializeOwned>(path: &Path) -> anyhow::Result<T> {
    let json = std::fs::read_to_string(path)?;
    Ok(serde_json::from_str(&json)?)
}

fn run_go(sh: &Shell, go_file: &Path) -> anyhow::Result<String> {
    if yaegi_available() {
        return Ok(cmd!(sh, "yaegi run {go_file}").read()?);
    }
    Ok(cmd!(sh, "go run {go_file}").read()?)
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

fn expect_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("expect")
        .join("cli_commands_test")
}

fn snapshot_path(test_name: &str, file_name: &str) -> PathBuf {
    expect_root().join(test_name).join(file_name)
}

fn assert_snapshot_file(
    test_name: &str,
    file_name: &str,
    actual_path: &Path,
    temp_root: &Path,
) -> anyhow::Result<()> {
    let content = std::fs::read_to_string(actual_path)?;
    let content = normalize_temp_paths(&content, temp_root);
    let expected = snapshot_path(test_name, file_name);
    expect_file![expected].assert_eq(&content);
    Ok(())
}

fn normalize_temp_paths(text: &str, root: &Path) -> String {
    let root_str = root.to_string_lossy();
    text.replace(root_str.as_ref(), "<TMP>")
}

#[test]
fn cli_check_build_link_single_crate() -> anyhow::Result<()> {
    let sh = Shell::new()?;
    let dir = tempfile::tempdir()?;
    let root = dir.path();
    let out = root.join("out");
    let crate_name = "single";

    write_manifest(root, crate_name, "bin", "main.gom")?;
    let main_gom = root.join("main.gom");
    write_file(
        &main_gom,
        r#"
fn main() -> unit {
    println("ok")
}
"#,
    )?;

    let bin = goml_bin();
    let main_interface = out.join("main.interface");
    cmd!(
        sh,
        "{bin} compiler check-crate --crate-dir {root} --output {main_interface}"
    )
    .run()?;
    assert_snapshot_file("single", "main.interface", &main_interface, root)?;
    let unit: InterfaceUnit = read_json(&main_interface)?;
    assert_eq!(unit.package, crate_name);
    assert!(unit.validate_hash());

    let main_out = out.join("main");
    cmd!(
        sh,
        "{bin} compiler build-crate --crate-dir {root} --output {main_out}"
    )
    .run()?;
    assert!(out.join("main.interface").exists());
    assert!(out.join("main.core").exists());
    assert_snapshot_file("single", "main.core", &out.join("main.core"), root)?;
    let core: CoreUnit = read_json(&out.join("main.core"))?;
    assert!(core.validate());

    let go_file = out.join("main.go");
    let main_core = out.join("main.core");
    cmd!(
        sh,
        "{bin} compiler link-crates --root-crate {crate_name} --input {main_core} --output {go_file}"
    )
    .run()?;
    assert_snapshot_file("single", "main.go", &go_file, root)?;
    if !runtime_executor_available() {
        return Ok(());
    }
    let output = run_go(&sh, &go_file)?;
    {
        let expected = snapshot_path("single", "output.txt");
        expect_file![expected].assert_eq(&output);
    }

    Ok(())
}

#[test]
fn cli_check_build_link_with_dep() -> anyhow::Result<()> {
    let sh = Shell::new()?;
    let dir = tempfile::tempdir()?;
    let root = dir.path();
    let out = root.join("out");
    std::fs::create_dir_all(&out)?;

    let dep_dir = root.join("dep");
    write_manifest(&dep_dir, "dep", "lib", "lib.gom")?;
    let lib_gom = dep_dir.join("lib.gom");
    write_file(
        &lib_gom,
        r#"


pub fn msg() -> string {
    "hi"
}
"#,
    )?;

    let app_dir = root.join("app");
    write_manifest(&app_dir, "app", "bin", "main.gom")?;
    let main_gom = app_dir.join("main.gom");
    write_file(
        &main_gom,
        r#"
use dep;

fn main() -> unit {
    println(dep::msg())
}
"#,
    )?;

    let bin = goml_bin();
    let lib_interface = out.join("Lib.interface");
    let main_interface = out.join("main.interface");
    cmd!(
        sh,
        "{bin} compiler check-crate --crate-dir {dep_dir} --output {lib_interface}"
    )
    .run()?;
    assert_snapshot_file("with_dep", "Lib.interface", &lib_interface, root)?;
    cmd!(
        sh,
        "{bin} compiler check-crate --crate-dir {app_dir} --output {main_interface} --interface-path {lib_interface}"
    )
    .run()?;
    assert_snapshot_file("with_dep", "main.interface", &main_interface, root)?;

    let lib_out = out.join("Lib");
    let main_out = out.join("main");
    cmd!(
        sh,
        "{bin} compiler build-crate --crate-dir {dep_dir} --output {lib_out}"
    )
    .run()?;
    cmd!(
        sh,
        "{bin} compiler build-crate --crate-dir {app_dir} --output {main_out} --interface-path {lib_interface}"
    )
    .run()?;
    assert_snapshot_file("with_dep", "Lib.core", &out.join("Lib.core"), root)?;
    assert_snapshot_file("with_dep", "main.core", &out.join("main.core"), root)?;

    let go_file = out.join("main.go");
    let lib_core = out.join("Lib.core");
    let main_core = out.join("main.core");
    cmd!(
        sh,
        "{bin} compiler link-crates --root-crate app --input {lib_core} {main_core} --output {go_file}"
    )
    .run()?;
    assert_snapshot_file("with_dep", "main.go", &go_file, root)?;
    if !runtime_executor_available() {
        return Ok(());
    }
    let output = run_go(&sh, &go_file)?;
    {
        let expected = snapshot_path("with_dep", "output.txt");
        expect_file![expected].assert_eq(&output);
    }

    Ok(())
}

#[test]
fn cli_check_build_link_rejects_hash_mismatch() -> anyhow::Result<()> {
    let sh = Shell::new()?;
    let dir = tempfile::tempdir()?;
    let root = dir.path();
    let out = root.join("out");
    std::fs::create_dir_all(&out)?;

    let dep_v1_dir = root.join("dep_v1");
    write_manifest(&dep_v1_dir, "dep", "lib", "lib.gom")?;
    let lib_gom_v1 = dep_v1_dir.join("lib.gom");
    write_file(
        &lib_gom_v1,
        r#"


pub fn msg() -> string {
    "v1"
}
"#,
    )?;

    let dep_v2_dir = root.join("dep_v2");
    write_manifest(&dep_v2_dir, "dep", "lib", "lib.gom")?;
    let lib_gom_v2 = dep_v2_dir.join("lib.gom");
    write_file(
        &lib_gom_v2,
        r#"


pub fn msg() -> string {
    "v2"
}

pub fn extra() -> int32 {
    2
}
"#,
    )?;

    let app_dir = root.join("app");
    write_manifest(&app_dir, "app", "bin", "main.gom")?;
    let main_gom = app_dir.join("main.gom");
    write_file(
        &main_gom,
        r#"
use dep;

fn main() -> unit {
    println(dep::msg())
}
"#,
    )?;

    let bin = goml_bin();
    let lib_interface = out.join("Lib.interface");
    cmd!(
        sh,
        "{bin} compiler check-crate --crate-dir {dep_v1_dir} --output {lib_interface}"
    )
    .run()?;
    assert_snapshot_file("hash_mismatch", "Lib.interface", &lib_interface, root)?;

    let main_interface = out.join("main.interface");
    cmd!(
        sh,
        "{bin} compiler check-crate --crate-dir {app_dir} --output {main_interface} --interface-path {lib_interface}"
    )
    .run()?;
    assert_snapshot_file("hash_mismatch", "main.interface", &main_interface, root)?;

    let lib_out = out.join("Lib");
    cmd!(
        sh,
        "{bin} compiler build-crate --crate-dir {dep_v2_dir} --output {lib_out}"
    )
    .run()?;
    assert_snapshot_file("hash_mismatch", "Lib.core", &out.join("Lib.core"), root)?;

    let main_out = out.join("main");
    cmd!(
        sh,
        "{bin} compiler build-crate --crate-dir {app_dir} --output {main_out} --interface-path {lib_interface}"
    )
    .run()?;
    assert_snapshot_file("hash_mismatch", "main.core", &out.join("main.core"), root)?;

    let go_file = out.join("main.go");
    let lib_core = out.join("Lib.core");
    let main_core = out.join("main.core");
    let err = cmd!(
        sh,
        "{bin} compiler link-crates --root-crate app --input {lib_core} {main_core} --output {go_file}"
    )
    .read_stderr()?;

    let normalized = normalize_temp_paths(&err, root);
    let expected = snapshot_path("hash_mismatch", "stderr.txt");
    expect_file![expected].assert_eq(&normalized);

    Ok(())
}

#[test]
fn cli_check_rejects_interface_directory_path() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let root = dir.path();
    let out = root.join("out");
    std::fs::create_dir_all(&out)?;

    write_manifest(root, "app", "bin", "main.gom")?;
    let main_gom = root.join("main.gom");
    write_file(
        &main_gom,
        r#"
fn main() -> unit {
    println("ok")
}
"#,
    )?;

    let main_interface = out.join("main.interface");
    let output = std::process::Command::new(goml_bin())
        .arg("compiler")
        .arg("check-crate")
        .arg("--crate-dir")
        .arg(root)
        .arg("--output")
        .arg(&main_interface)
        .arg("--interface-path")
        .arg(&out)
        .output()?;

    assert!(!output.status.success());
    let stderr = String::from_utf8(output.stderr)?;
    assert!(stderr.contains("is a directory"));

    Ok(())
}
