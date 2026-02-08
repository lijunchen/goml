use std::path::{Path, PathBuf};

use compiler::artifact::{CoreUnit, InterfaceUnit};
use compiler::package_names::ROOT_PACKAGE;
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
        &format!(
            r#"
package {ROOT_PACKAGE};

fn main() -> unit {{
    string_println("ok")
}}
"#
        ),
    )?;

    let bin = goml_bin();
    let main_interface = out.join(format!("{ROOT_PACKAGE}.interface"));
    cmd!(
        sh,
        "{bin} compiler check --package {ROOT_PACKAGE} --input {main_gom} --output {main_interface}"
    )
    .run()?;
    assert_snapshot_file("single", "main.interface", &main_interface, root)?;
    let unit: InterfaceUnit = read_json(&main_interface)?;
    assert_eq!(unit.package, ROOT_PACKAGE);
    assert!(unit.validate_hash());

    let main_out = out.join(ROOT_PACKAGE);
    cmd!(
        sh,
        "{bin} compiler build --package {ROOT_PACKAGE} --input {main_gom} --output {main_out}"
    )
    .run()?;
    assert!(out.join(format!("{ROOT_PACKAGE}.interface")).exists());
    assert!(out.join(format!("{ROOT_PACKAGE}.core")).exists());
    assert_snapshot_file(
        "single",
        "main.core",
        &out.join(format!("{ROOT_PACKAGE}.core")),
        root,
    )?;
    let core: CoreUnit = read_json(&out.join(format!("{ROOT_PACKAGE}.core")))?;
    assert!(core.validate());

    let go_file = out.join("main.go");
    let main_core = out.join(format!("{ROOT_PACKAGE}.core"));
    cmd!(
        sh,
        "{bin} compiler link --input {main_core} --output {go_file}"
    )
    .run()?;
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
package Lib;

fn msg() -> string {
    "hi"
}
"#,
    )?;

    let main_gom = root.join("main.gom");
    write_file(
        &main_gom,
        &format!(
            r#"
package {ROOT_PACKAGE};

use Lib;

fn main() -> unit {{
    string_println(Lib::msg())
}}
"#
        ),
    )?;

    let bin = goml_bin();
    let lib_interface = out.join("Lib.interface");
    let main_interface = out.join(format!("{ROOT_PACKAGE}.interface"));
    cmd!(
        sh,
        "{bin} compiler check --package Lib --input {lib_gom} --output {lib_interface}"
    )
    .run()?;
    assert_snapshot_file("with_dep", "Lib.interface", &lib_interface, root)?;
    cmd!(
        sh,
        "{bin} compiler check --package {ROOT_PACKAGE} --input {main_gom} --output {main_interface} --interface-path {lib_interface}"
    )
    .run()?;
    assert_snapshot_file("with_dep", "main.interface", &main_interface, root)?;

    let lib_out = out.join("Lib");
    let main_out = out.join(ROOT_PACKAGE);
    cmd!(
        sh,
        "{bin} compiler build --package Lib --input {lib_gom} --output {lib_out}"
    )
    .run()?;
    cmd!(
        sh,
        "{bin} compiler build --package {ROOT_PACKAGE} --input {main_gom} --output {main_out} --interface-path {lib_interface}"
    )
    .run()?;
    assert_snapshot_file("with_dep", "Lib.core", &out.join("Lib.core"), root)?;
    assert_snapshot_file(
        "with_dep",
        "main.core",
        &out.join(format!("{ROOT_PACKAGE}.core")),
        root,
    )?;

    let go_file = out.join("main.go");
    let lib_core = out.join("Lib.core");
    let main_core = out.join(format!("{ROOT_PACKAGE}.core"));
    cmd!(
        sh,
        "{bin} compiler link --input {lib_core} {main_core} --output {go_file}"
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

    let lib_gom_v1 = root.join("lib_v1.gom");
    write_file(
        &lib_gom_v1,
        r#"
package Lib;

fn msg() -> string {
    "v1"
}
"#,
    )?;

    let lib_gom_v2 = root.join("lib_v2.gom");
    write_file(
        &lib_gom_v2,
        r#"
package Lib;

fn msg() -> string {
    "v2"
}

fn extra() -> int32 {
    2
}
"#,
    )?;

    let main_gom = root.join("main.gom");
    write_file(
        &main_gom,
        &format!(
            r#"
package {ROOT_PACKAGE};

use Lib;

fn main() -> unit {{
    string_println(Lib::msg())
}}
"#
        ),
    )?;

    let bin = goml_bin();
    let lib_interface = out.join("Lib.interface");
    cmd!(
        sh,
        "{bin} compiler check --package Lib --input {lib_gom_v1} --output {lib_interface}"
    )
    .run()?;
    assert_snapshot_file("hash_mismatch", "Lib.interface", &lib_interface, root)?;

    let main_interface = out.join(format!("{ROOT_PACKAGE}.interface"));
    cmd!(
        sh,
        "{bin} compiler check --package {ROOT_PACKAGE} --input {main_gom} --output {main_interface} --interface-path {lib_interface}"
    )
    .run()?;
    assert_snapshot_file("hash_mismatch", "main.interface", &main_interface, root)?;

    let lib_out = out.join("Lib");
    cmd!(
        sh,
        "{bin} compiler build --package Lib --input {lib_gom_v2} --output {lib_out}"
    )
    .run()?;
    assert_snapshot_file("hash_mismatch", "Lib.core", &out.join("Lib.core"), root)?;

    let main_out = out.join(ROOT_PACKAGE);
    cmd!(
        sh,
        "{bin} compiler build --package {ROOT_PACKAGE} --input {main_gom} --output {main_out} --interface-path {lib_interface}"
    )
    .run()?;
    assert_snapshot_file(
        "hash_mismatch",
        "main.core",
        &out.join(format!("{ROOT_PACKAGE}.core")),
        root,
    )?;

    let go_file = out.join("main.go");
    let lib_core = out.join("Lib.core");
    let main_core = out.join(format!("{ROOT_PACKAGE}.core"));
    let err = cmd!(
        sh,
        "{bin} compiler link --input {lib_core} {main_core} --output {go_file}"
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

    let main_gom = root.join("main.gom");
    write_file(
        &main_gom,
        &format!(
            r#"
package {ROOT_PACKAGE};

use Lib;

fn main() -> unit {{
    string_println(Lib::msg())
}}
"#
        ),
    )?;

    let main_interface = out.join(format!("{ROOT_PACKAGE}.interface"));
    let output = std::process::Command::new(goml_bin())
        .arg("compiler")
        .arg("check")
        .arg("--package")
        .arg(ROOT_PACKAGE)
        .arg("--input")
        .arg(&main_gom)
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
