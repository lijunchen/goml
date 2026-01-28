use std::path::{Path, PathBuf};

use compiler::artifact::{CoreUnit, InterfaceUnit};
use expect_test::expect_file;
use xshell::{Shell, cmd};

fn goml_bin() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_compiler"))
}

fn write_file(path: &Path, content: &str) -> anyhow::Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(path, content)?;
    Ok(())
}

fn read_json<T: serde::de::DeserializeOwned>(path: &Path) -> anyhow::Result<T> {
    let json = std::fs::read_to_string(path)?;
    Ok(serde_json::from_str(&json)?)
}

fn run_go(sh: &Shell, go_file: &Path) -> anyhow::Result<String> {
    Ok(cmd!(sh, "go run {go_file}").read()?)
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
fn cli_check_build_link_single_package() -> anyhow::Result<()> {
    let sh = Shell::new()?;
    let dir = tempfile::tempdir()?;
    let root = dir.path();
    let out = root.join("out");

    let main_gom = root.join("main.gom");
    write_file(
        &main_gom,
        r#"
package Main

fn main() -> unit {
    string_println("ok")
}
"#,
    )?;

    let bin = goml_bin();
    let main_interface = out.join("Main.interface");
    cmd!(
        sh,
        "{bin} check --package Main --input {main_gom} --output {main_interface}"
    )
    .run()?;
    assert_snapshot_file("single", "Main.interface", &main_interface, root)?;
    let unit: InterfaceUnit = read_json(&main_interface)?;
    assert_eq!(unit.package, "Main");
    assert!(unit.validate_hash());

    let main_out = out.join("Main");
    cmd!(
        sh,
        "{bin} build --package Main --input {main_gom} --output {main_out}"
    )
    .run()?;
    assert!(out.join("Main.interface").exists());
    assert!(out.join("Main.core").exists());
    assert_snapshot_file("single", "Main.core", &out.join("Main.core"), root)?;
    let core: CoreUnit = read_json(&out.join("Main.core"))?;
    assert!(core.validate());

    let go_file = out.join("main.go");
    let main_core = out.join("Main.core");
    cmd!(sh, "{bin} link --input {main_core} --output {go_file}").run()?;
    assert_snapshot_file("single", "main.go", &go_file, root)?;
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

    let lib_gom = root.join("lib.gom");
    write_file(
        &lib_gom,
        r#"
package Lib

fn msg() -> string {
    "hi"
}
"#,
    )?;

    let main_gom = root.join("main.gom");
    write_file(
        &main_gom,
        r#"
package Main

import Lib

fn main() -> unit {
    string_println(Lib::msg())
}
"#,
    )?;

    let bin = goml_bin();
    let lib_interface = out.join("Lib.interface");
    let main_interface = out.join("Main.interface");
    cmd!(
        sh,
        "{bin} check --package Lib --input {lib_gom} --output {lib_interface}"
    )
    .run()?;
    assert_snapshot_file("with_dep", "Lib.interface", &lib_interface, root)?;
    cmd!(
        sh,
        "{bin} check --package Main --input {main_gom} --output {main_interface} --interface-path {out}"
    )
    .run()?;
    assert_snapshot_file("with_dep", "Main.interface", &main_interface, root)?;

    let lib_out = out.join("Lib");
    let main_out = out.join("Main");
    cmd!(
        sh,
        "{bin} build --package Lib --input {lib_gom} --output {lib_out}"
    )
    .run()?;
    cmd!(
        sh,
        "{bin} build --package Main --input {main_gom} --output {main_out} --interface-path {out}"
    )
    .run()?;
    assert_snapshot_file("with_dep", "Lib.core", &out.join("Lib.core"), root)?;
    assert_snapshot_file("with_dep", "Main.core", &out.join("Main.core"), root)?;

    let go_file = out.join("main.go");
    let lib_core = out.join("Lib.core");
    let main_core = out.join("Main.core");
    cmd!(
        sh,
        "{bin} link --input {lib_core} {main_core} --output {go_file}"
    )
    .run()?;
    assert_snapshot_file("with_dep", "main.go", &go_file, root)?;
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

    let lib_gom = root.join("lib.gom");
    write_file(
        &lib_gom,
        r#"
package Lib

fn msg() -> string {
    "v1"
}
"#,
    )?;

    let main_gom = root.join("main.gom");
    write_file(
        &main_gom,
        r#"
package Main

import Lib

fn main() -> unit {
    string_println(Lib::msg())
}
"#,
    )?;

    let bin = goml_bin();
    let lib_interface = out.join("Lib.interface");
    cmd!(
        sh,
        "{bin} check --package Lib --input {lib_gom} --output {lib_interface}"
    )
    .run()?;
    assert_snapshot_file("hash_mismatch", "Lib.interface", &lib_interface, root)?;
    let lib_out = out.join("Lib");
    cmd!(
        sh,
        "{bin} build --package Lib --input {lib_gom} --output {lib_out}"
    )
    .run()?;
    let main_out = out.join("Main");
    cmd!(
        sh,
        "{bin} build --package Main --input {main_gom} --output {main_out} --interface-path {out}"
    )
    .run()?;
    assert_snapshot_file("hash_mismatch", "Main.core", &out.join("Main.core"), root)?;

    write_file(
        &lib_gom,
        r#"
package Lib

fn msg() -> string {
    "v2"
}

fn extra() -> int32 {
    2
}
"#,
    )?;
    cmd!(
        sh,
        "{bin} build --package Lib --input {lib_gom} --output {lib_out}"
    )
    .run()?;
    assert_snapshot_file("hash_mismatch", "Lib.core", &out.join("Lib.core"), root)?;

    let go_file = out.join("main.go");
    let main_core = out.join("Main.core");
    let lib_core = out.join("Lib.core");
    let result = cmd!(
        sh,
        "{bin} link --input {main_core} {lib_core} --output {go_file}"
    )
    .ignore_status()
    .output()?;
    assert!(!result.status.success());
    let stderr = String::from_utf8_lossy(&result.stderr);
    assert!(stderr.contains("expects interface_hash"));
    {
        let expected = snapshot_path("hash_mismatch", "stderr.txt");
        let normalized = normalize_temp_paths(stderr.as_ref(), root);
        expect_file![expected].assert_eq(&normalized);
    }

    Ok(())
}
