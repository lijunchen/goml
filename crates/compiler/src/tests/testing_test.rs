use std::collections::HashMap;
use std::fs;

use crate::pipeline::pipeline::compile_for_analysis_with_overrides;
use crate::pipeline::separate::{PackageInputs, build_test_package, link_test_cores_multi};

#[test]
fn test_link_accepts_multiple_test_package_roots() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let first = dir.path().join("first.gom");
    let second = dir.path().join("second.gom");
    fs::write(
        &first,
        "package first;\n#[test]\nfn first_test() -> unit { () }\n",
    )?;
    fs::write(
        &second,
        "package second;\n#[test]\nfn second_test() -> unit { () }\n",
    )?;
    let first = build_test_package(PackageInputs {
        package: "example::tests::first".to_string(),
        input_files: vec![first],
        interface_files: Vec::new(),
    })
    .map_err(|error| anyhow::anyhow!("first test build failed: {error:?}"))?;
    let second = build_test_package(PackageInputs {
        package: "example::tests::second".to_string(),
        input_files: vec![second],
        interface_files: Vec::new(),
    })
    .map_err(|error| anyhow::anyhow!("second test build failed: {error:?}"))?;
    let roots = vec![
        "example::tests::first".to_string(),
        "example::tests::second".to_string(),
    ];
    let output = link_test_cores_multi(&roots, vec![first, second])
        .map_err(|error| anyhow::anyhow!("test link failed: {error:?}"))?;
    assert_eq!(output.tests.len(), 2);
    let go = output.link.go.to_pretty(&output.link.goenv, 120);
    assert!(go.contains("case \"example::tests::first::first_test\""));
    assert!(go.contains("case \"example::tests::second::second_test\""));
    Ok(())
}

#[test]
fn analysis_compilation_retains_exact_override_sources() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let main_path = dir.path().join("main.gom");
    let helper_path = dir.path().join("helper.gom");
    fs::write(
        dir.path().join("goml.toml"),
        "[module]\npath = \"example::snapshot\"\n",
    )?;
    fs::write(
        &main_path,
        "package main;\nfn main() -> unit { string_println(\"disk\") }\n",
    )?;
    fs::write(
        &helper_path,
        "package main;\nfn message() -> string { \"disk\" }\n",
    )?;
    let main_source =
        "package main;\nfn main() -> unit { string_println(message()) }\n".to_string();
    let helper_source = "package main;\nfn message() -> string { \"unsaved 🦀\" }\n".to_string();
    let overrides = HashMap::from([(helper_path.clone(), helper_source.clone())]);

    let compilation = compile_for_analysis_with_overrides(&main_path, &main_source, &overrides)
        .map_err(|error| anyhow::anyhow!("analysis compilation failed: {error:?}"))?;
    assert_eq!(compilation.source_map.len(), 2);
    assert_eq!(
        compilation
            .source_map
            .file(compilation.source_map.find(&main_path).unwrap())?
            .text(),
        main_source
    );
    assert_eq!(
        compilation
            .source_map
            .file(compilation.source_map.find(&helper_path).unwrap())?
            .text(),
        helper_source
    );

    Ok(())
}
