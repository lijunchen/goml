use std::collections::HashMap;
use std::fs;

use diagnostics::{LabelSeverity, Severity};

use crate::pipeline::pipeline::compile_for_analysis_with_overrides;
use crate::pipeline::separate::{
    PackageInputs, build_test_package, link_test_cores, link_test_cores_multi,
    link_test_cores_multi_to_go,
};

#[test]
fn test_build_collects_and_links_top_level_tests() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let source = dir.path().join("math.gom");
    fs::write(
        &source,
        r#"
package math;

fn private_value() -> int32 {
    4
}

#[test]
fn private_value_works() -> unit {
    let _ = private_value();
    ()
}

#[test]
#[ignore("later: \"quoted\"")]
fn ignored_case() -> unit {
    ()
}
"#,
    )?;

    let unit = build_test_package(PackageInputs {
        package: "example::math".to_string(),
        input_files: vec![source],
        interface_files: Vec::new(),
    })
    .map_err(|error| anyhow::anyhow!("test build failed: {error:?}"))?;

    assert_eq!(unit.tests.len(), 2);
    assert_eq!(unit.tests[0].id, "example::math::ignored_case");
    assert!(unit.tests[0].ignored);
    assert_eq!(
        unit.tests[0].ignore_reason.as_deref(),
        Some("later: \"quoted\"")
    );
    assert_eq!(unit.tests[1].id, "example::math::private_value_works");
    assert!(!unit.tests[1].ignored);

    let packages = vec!["example::math".to_string()];
    let go_only = link_test_cores_multi_to_go(&packages, vec![unit.clone()])
        .map_err(|error| anyhow::anyhow!("test link failed: {error:?}"))?;
    let output = link_test_cores("example::math", vec![unit])
        .map_err(|error| anyhow::anyhow!("test link failed: {error:?}"))?;
    let go = output.link.go.to_pretty(&output.link.goenv, 120);
    assert_eq!(go, go_only.link.go.to_pretty(&go_only.link.goenv, 120));
    assert_eq!(
        serde_json::to_string(&output.tests)?,
        serde_json::to_string(&go_only.tests)?,
    );
    assert!(go.contains("switch _goml_os.Args[1]"));
    assert!(go.contains("case \"example::math::private_value_works\""));
    assert!(go.contains("case \"example::math::ignored_case\""));
    assert!(!go.contains("func main() {\n\t_goml_entry()"));

    Ok(())
}

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
fn invalid_test_signatures_are_rejected() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let source = dir.path().join("invalid.gom");
    fs::write(
        &source,
        r#"
package invalid;

#[test]
fn generic_test[T](value: T) -> bool {
    true
}
"#,
    )?;

    let error = build_test_package(PackageInputs {
        package: "example::invalid".to_string(),
        input_files: vec![source],
        interface_files: Vec::new(),
    })
    .unwrap_err();
    let error = format!("{error:?}");
    assert!(error.contains("must not have type parameters"));
    assert!(error.contains("must not have parameters"));
    assert!(error.contains("must return unit"));

    Ok(())
}

#[test]
fn test_attribute_rejects_non_top_level_functions() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let source = dir.path().join("invalid.gom");
    fs::write(
        &source,
        r#"
package invalid;

struct Value {}

#[test]
impl Value {
    fn value(self: Value) -> int32 {
        1
    }
}
"#,
    )?;

    let error = build_test_package(PackageInputs {
        package: "example::invalid".to_string(),
        input_files: vec![source],
        interface_files: Vec::new(),
    })
    .unwrap_err();
    assert!(format!("{error:?}").contains("can only be applied to top-level functions"));

    Ok(())
}

#[test]
fn malformed_test_attributes_are_rejected() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let source = dir.path().join("invalid.gom");
    fs::write(
        &source,
        r#"
package invalid;

#[ignore]
fn ignored_without_test() -> unit {
    ()
}

#[test()]
fn test_with_arguments() -> unit {
    ()
}

#[test]
#[test]
fn duplicate_test() -> unit {
    ()
}

#[test]
#[ignore(1)]
fn invalid_ignore_reason() -> unit {
    ()
}
"#,
    )?;

    let error = build_test_package(PackageInputs {
        package: "example::invalid".to_string(),
        input_files: vec![source],
        interface_files: Vec::new(),
    })
    .unwrap_err();
    let error = format!("{error:?}");
    assert!(error.contains("`#[ignore]` requires `#[test]`"));
    assert!(error.contains("`#[test]` does not accept arguments"));
    assert!(error.contains("duplicate `#[test]` attribute"));
    assert!(error.contains("one string reason"));

    Ok(())
}

#[test]
fn duplicate_test_ids_have_cross_file_labels() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let first = dir.path().join("first.gom");
    let second = dir.path().join("second.gom");
    let first_source = "package tests;\n#[test]\nfn duplicate() -> unit { () }\n";
    let second_source = "package tests;\n\n#[test]\nfn duplicate() -> unit { () }\n";
    fs::write(&first, first_source)?;
    fs::write(&second, second_source)?;

    let error = build_test_package(PackageInputs {
        package: "example::tests".to_string(),
        input_files: vec![second.clone(), first.clone()],
        interface_files: Vec::new(),
    })
    .unwrap_err();
    let diagnostics = error.diagnostics();
    let source_map = diagnostics.source_map().expect("source map");
    assert_eq!(source_map.len(), 2);
    assert_eq!(
        source_map.file(source_map.find(&first).unwrap())?.text(),
        first_source
    );
    assert_eq!(
        source_map.file(source_map.find(&second).unwrap())?.text(),
        second_source
    );

    let duplicate = diagnostics
        .iter()
        .find(|diagnostic| diagnostic.message().contains("duplicate test id"))
        .expect("duplicate test diagnostic");
    assert_eq!(duplicate.severity(), Severity::Error);
    let primary = duplicate
        .labels()
        .iter()
        .find(|label| label.severity() == LabelSeverity::Primary)
        .expect("primary label");
    let secondary = duplicate
        .labels()
        .iter()
        .find(|label| label.severity() == LabelSeverity::Secondary)
        .expect("secondary label");
    assert_eq!(source_map.file(primary.span().source())?.path(), second);
    assert_eq!(source_map.file(secondary.span().source())?.path(), first);
    assert_eq!(source_map.slice(primary.span())?, "#[test]");
    assert_eq!(source_map.slice(secondary.span())?, "#[test]");

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
