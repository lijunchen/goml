use std::path::PathBuf;

use expect_test::expect_file;

use crate::package_names::ROOT_PACKAGE;
use crate::pipeline::{modules, separate};

#[test]
fn separate_build_link_matches_project_008() -> anyhow::Result<()> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/module")
        .join("project008_trait_bounds_across_packages");
    let main_path = root.join("main.gom");
    let crate_unit = modules::discover_crate_from_dir(&root)
        .map_err(|err| anyhow::anyhow!("failed to discover crate: {:?}", err))?;
    let root_crate = crate_unit.config.name.clone();
    let unit = separate::build_crate(separate::CrateInputs {
        crate_unit,
        interface_files: Vec::new(),
    })
    .map_err(|err| anyhow::anyhow!("build crate failed: {:?}", err))?;

    let outdir = tempfile::tempdir()?;
    let core_path = outdir.path().join(format!("{root_crate}.core"));
    std::fs::write(&core_path, serde_json::to_string_pretty(&unit)?)?;
    let core = separate::read_core(&core_path)
        .map_err(|err| anyhow::anyhow!("failed to read crate core: {:?}", err))?;
    let linked = separate::link_crates(&root_crate, vec![core])
        .map_err(|err| anyhow::anyhow!("link crate failed: {:?}", err))?;
    let go_source = linked.go.to_pretty(&linked.goenv, 120);
    if !super::runtime_executor_available() {
        println!(
            "Skipping separate compile runtime output: {}",
            main_path.display()
        );
        return Ok(());
    }
    let output = super::execute_go_source(&go_source, &main_path.to_string_lossy())?;

    let out_path = root.join("main.gom.out");
    expect_file![out_path].assert_eq(&output);

    Ok(())
}

#[test]
fn link_rejects_interface_hash_mismatch() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let iface_dir = dir.path().join("artifacts");
    std::fs::create_dir_all(&iface_dir)?;

    let lib_path = dir.path().join("lib.gom");
    std::fs::write(
        &lib_path,
        r#"


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
use Lib;

fn main() -> unit {
    println(Lib::foo())
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

    let err = separate::link_cores(vec![main_core, lib_core]).unwrap_err();
    let msg = format!("{:?}", err);
    assert!(msg.contains("expects interface_hash"));

    Ok(())
}

#[test]
fn link_rejects_invalid_custom_result_for_go_error_wrapper() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let main_path = dir.path().join("main.gom");
    std::fs::write(
        &main_path,
        r#"
enum Result[T, E] {
    Only,
}

#[go_error]
extern "go" "os" "Chdir" chdir(path: string) -> Result[unit, GoError]

fn main() -> unit {
    let _ = chdir(".");
}
"#,
    )?;

    let msg = match separate::build_package(separate::PackageInputs {
        package: ROOT_PACKAGE.to_string(),
        input_files: vec![main_path],
        interface_files: vec![],
    }) {
        Ok(unit) => format!("{:?}", separate::link_cores(vec![unit]).unwrap_err()),
        Err(err) => format!("{:?}", err),
    };
    assert!(
        msg.contains("#[go_error]"),
        "unexpected error message: {msg}"
    );

    Ok(())
}

#[test]
fn link_accepts_reversed_custom_result_for_go_error_wrapper() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let main_path = dir.path().join("main.gom");
    std::fs::write(
        &main_path,
        r#"
enum Result[T, E] {
    Err(E),
    Ok(T),
}

#[go_error]
extern "go" "os" "Chdir" chdir(path: string) -> Result[unit, GoError]

fn main() -> unit {
    let _ = chdir(".");
}
"#,
    )?;

    let unit = separate::build_package(separate::PackageInputs {
        package: ROOT_PACKAGE.to_string(),
        input_files: vec![main_path],
        interface_files: vec![],
    })
    .map_err(|err| anyhow::anyhow!("build main failed: {:?}", err))?;

    separate::link_cores(vec![unit]).map_err(|err| anyhow::anyhow!("link failed: {:?}", err))?;

    Ok(())
}
