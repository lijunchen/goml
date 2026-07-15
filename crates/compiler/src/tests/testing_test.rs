use std::fs;

use crate::pipeline::separate::{
    PackageInputs, build_test_package, link_test_cores, link_test_cores_multi,
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

    let output = link_test_cores("example::math", vec![unit])
        .map_err(|error| anyhow::anyhow!("test link failed: {error:?}"))?;
    let go = output.link.go.to_pretty(&output.link.goenv, 120);
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
