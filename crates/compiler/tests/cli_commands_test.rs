use std::path::{Path, PathBuf};

use compiler::artifact::{CoreUnit, InterfaceUnit};
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
    let core: CoreUnit = read_json(&out.join("Main.core"))?;
    assert!(core.validate());

    let go_file = out.join("main.go");
    let main_core = out.join("Main.core");
    cmd!(sh, "{bin} link --input {main_core} --output {go_file}").run()?;
    let output = run_go(&sh, &go_file)?;
    assert_eq!(output, "ok");

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
    cmd!(
        sh,
        "{bin} check --package Main --input {main_gom} --output {main_interface} --interface-path {out}"
    )
    .run()?;

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

    let go_file = out.join("main.go");
    let lib_core = out.join("Lib.core");
    let main_core = out.join("Main.core");
    cmd!(
        sh,
        "{bin} link --input {lib_core} {main_core} --output {go_file}"
    )
    .run()?;
    let output = run_go(&sh, &go_file)?;
    assert_eq!(output, "hi");

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

    Ok(())
}
