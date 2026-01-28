use std::path::{Path, PathBuf};

use expect_test::expect_file;

use crate::pipeline::{packages, pipeline, separate};

fn gom_files_in_dir(dir: &Path) -> anyhow::Result<Vec<PathBuf>> {
    let mut files = Vec::new();
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.extension().is_some_and(|ext| ext == "gom") {
            files.push(path);
        }
    }
    files.sort();
    Ok(files)
}

#[test]
fn separate_build_link_matches_project_008() -> anyhow::Result<()> {
    let root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/package")
        .join("project008_trait_bounds_across_packages");
    let main_path = root.join("main.gom");
    let main_src = std::fs::read_to_string(&main_path)?;
    let entry_ast = pipeline::parse_ast_file(&main_path, &main_src)
        .map_err(|err| anyhow::anyhow!("failed to parse entry ast: {:?}", err))?;

    let graph = packages::discover_packages(&root, Some(&main_path), Some(entry_ast))
        .map_err(|err| anyhow::anyhow!("failed to discover packages: {:?}", err))?;
    let order = packages::topo_sort_packages(&graph)
        .map_err(|err| anyhow::anyhow!("failed to topo sort packages: {:?}", err))?;

    let outdir = tempfile::tempdir()?;
    let iface_dir = outdir.path().to_path_buf();

    for pkg in order.iter() {
        let pkg_dir = graph
            .package_dirs
            .get(pkg)
            .ok_or_else(|| anyhow::anyhow!("missing package dir for {}", pkg))?;
        let inputs = gom_files_in_dir(pkg_dir)?;
        let unit = separate::build_package(separate::PackageInputs {
            package: pkg.clone(),
            input_files: inputs,
            interface_paths: vec![iface_dir.clone()],
        })
        .map_err(|err| anyhow::anyhow!("build failed for {}: {:?}", pkg, err))?;

        let interface_path = iface_dir.join(format!("{}.interface", pkg));
        let core_path = iface_dir.join(format!("{}.core", pkg));
        std::fs::write(
            interface_path,
            serde_json::to_string_pretty(&unit.interface)?,
        )?;
        std::fs::write(core_path, serde_json::to_string_pretty(&unit)?)?;
    }

    let mut cores = Vec::new();
    for pkg in order.iter() {
        let path = iface_dir.join(format!("{}.core", pkg));
        cores.push(
            separate::read_core(&path)
                .map_err(|err| anyhow::anyhow!("failed to read core {}: {:?}", pkg, err))?,
        );
    }

    let linked =
        separate::link_cores(cores).map_err(|err| anyhow::anyhow!("link failed: {:?}", err))?;
    let go_source = linked.go.to_pretty(&linked.goenv, 120);
    let output = super::execute_go_source(&go_source)?;

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
package Lib

fn foo() -> int32 {
    1
}
"#,
    )?;

    let lib_unit_v1 = separate::build_package(separate::PackageInputs {
        package: "Lib".to_string(),
        input_files: vec![lib_path.clone()],
        interface_paths: vec![iface_dir.clone()],
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
package Main

import Lib

fn main() -> unit {
    string_println(int32_to_string(Lib::foo()))
}
"#,
    )?;

    let main_unit = separate::build_package(separate::PackageInputs {
        package: "Main".to_string(),
        input_files: vec![main_path.clone()],
        interface_paths: vec![iface_dir.clone()],
    })
    .map_err(|err| anyhow::anyhow!("build Main failed: {:?}", err))?;
    std::fs::write(
        iface_dir.join("Main.interface"),
        serde_json::to_string_pretty(&main_unit.interface)?,
    )?;
    std::fs::write(
        iface_dir.join("Main.core"),
        serde_json::to_string_pretty(&main_unit)?,
    )?;

    std::fs::write(
        &lib_path,
        r#"
package Lib

fn foo() -> int32 {
    1
}

fn bar() -> int32 {
    2
}
"#,
    )?;

    let lib_unit_v2 = separate::build_package(separate::PackageInputs {
        package: "Lib".to_string(),
        input_files: vec![lib_path],
        interface_paths: vec![iface_dir.clone()],
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

    let main_core = separate::read_core(&iface_dir.join("Main.core"))
        .map_err(|err| anyhow::anyhow!("failed to read Main.core: {:?}", err))?;
    let lib_core = separate::read_core(&iface_dir.join("Lib.core"))
        .map_err(|err| anyhow::anyhow!("failed to read Lib.core: {:?}", err))?;

    let err = separate::link_cores(vec![main_core, lib_core]).unwrap_err();
    let msg = format!("{:?}", err);
    assert!(msg.contains("expects interface_hash"));

    Ok(())
}
