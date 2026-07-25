use crate::package_names::ROOT_PACKAGE;
use crate::pipeline::separate;

#[test]
fn link_rejects_interface_hash_mismatch() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let iface_dir = dir.path().join("artifacts");
    std::fs::create_dir_all(&iface_dir)?;

    let lib_path = dir.path().join("lib.gom");
    std::fs::write(
        &lib_path,
        r#"
package lib;

pub fn foo() -> int32 {
    1
}
"#,
    )?;

    let lib_unit_v1 = separate::build_package(separate::PackageInputs {
        package: "Lib".to_string(),
        input_files: vec![lib_path.clone()],
        interface_files: vec![],
    })
    .map_err(|err| anyhow::anyhow!("build Lib failed: {:?}", err))?;
    std::fs::write(
        iface_dir.join("Lib.interface"),
        serde_json::to_string_pretty(&lib_unit_v1.interface)?,
    )?;
    std::fs::write(
        iface_dir.join("Lib.core"),
        serde_json::to_string_pretty(&lib_unit_v1)?,
    )?;

    let main_path = dir.path().join("main.gom");
    std::fs::write(
        &main_path,
        r#"
package main;

use Lib as lib;

fn main() -> unit {
    println(lib::foo())
}
"#,
    )?;

    let main_unit = separate::build_package(separate::PackageInputs {
        package: ROOT_PACKAGE.to_string(),
        input_files: vec![main_path.clone()],
        interface_files: vec![iface_dir.join("Lib.interface")],
    })
    .map_err(|err| anyhow::anyhow!("build main failed: {:?}", err))?;
    std::fs::write(
        iface_dir.join(format!("{ROOT_PACKAGE}.interface")),
        serde_json::to_string_pretty(&main_unit.interface)?,
    )?;
    std::fs::write(
        iface_dir.join(format!("{ROOT_PACKAGE}.core")),
        serde_json::to_string_pretty(&main_unit)?,
    )?;

    std::fs::write(
        &lib_path,
        r#"
package lib;

pub fn foo() -> int32 {
    1
}

pub fn bar() -> int32 {
    2
}
"#,
    )?;

    let lib_unit_v2 = separate::build_package(separate::PackageInputs {
        package: "Lib".to_string(),
        input_files: vec![lib_path],
        interface_files: vec![],
    })
    .map_err(|err| anyhow::anyhow!("rebuild Lib failed: {:?}", err))?;
    std::fs::write(
        iface_dir.join("Lib.interface"),
        serde_json::to_string_pretty(&lib_unit_v2.interface)?,
    )?;
    std::fs::write(
        iface_dir.join("Lib.core"),
        serde_json::to_string_pretty(&lib_unit_v2)?,
    )?;

    let main_core = separate::read_core(&iface_dir.join(format!("{ROOT_PACKAGE}.core")))
        .map_err(|err| anyhow::anyhow!("failed to read main.core: {:?}", err))?;
    let lib_core = separate::read_core(&iface_dir.join("Lib.core"))
        .map_err(|err| anyhow::anyhow!("failed to read Lib.core: {:?}", err))?;

    let err = separate::link_cores(
        crate::package_names::ROOT_PACKAGE,
        vec![main_core, lib_core],
    )
    .unwrap_err();
    let msg = format!("{:?}", err);
    assert!(msg.contains("expects interface_hash"));

    Ok(())
}

#[test]
fn user_package_cannot_import_std_internal_host() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let main_path = dir.path().join("main.gom");
    std::fs::write(
        &main_path,
        r#"
package main;

use std::internal::host;

fn main() -> unit {
    host::println("hidden")
}
"#,
    )?;

    let err = separate::build_package(separate::PackageInputs {
        package: ROOT_PACKAGE.to_string(),
        input_files: vec![main_path],
        interface_files: vec![],
    })
    .expect_err("expected internal package visibility error");
    let message = format!("{err:?}");
    assert!(
        message.contains("package std::internal::host is internal to std"),
        "{message}"
    );
    assert_eq!(
        message
            .matches("package std::internal::host is internal to std")
            .count(),
        1,
        "{message}"
    );

    Ok(())
}

#[test]
fn link_ignores_unreachable_core_inputs() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let main_path = dir.path().join("main.gom");
    std::fs::write(
        &main_path,
        r#"package main;

fn main() -> unit {
    ()
}
"#,
    )?;
    let extra_path = dir.path().join("extra.gom");
    std::fs::write(
        &extra_path,
        r#"package extra;

pub fn unused() -> unit {
    ()
}
"#,
    )?;

    let main = separate::build_package(separate::PackageInputs {
        package: ROOT_PACKAGE.to_string(),
        input_files: vec![main_path],
        interface_files: Vec::new(),
    })
    .map_err(|err| anyhow::anyhow!("build main failed: {:?}", err))?;
    let extra = separate::build_package(separate::PackageInputs {
        package: "example::extra".to_string(),
        input_files: vec![extra_path],
        interface_files: Vec::new(),
    })
    .map_err(|err| anyhow::anyhow!("build extra failed: {:?}", err))?;

    let linked = separate::link_cores(ROOT_PACKAGE, vec![extra, main])
        .map_err(|err| anyhow::anyhow!("link failed: {:?}", err))?;
    assert!(
        linked
            .core
            .toplevels
            .iter()
            .all(|function| function.name != "example::extra::unused")
    );

    Ok(())
}
