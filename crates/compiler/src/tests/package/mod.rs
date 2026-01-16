use expect_test::expect_file;

use crate::pipeline;

#[test]
fn multi_package_compile_001() -> anyhow::Result<()> {
    run_project("project001")
}

#[test]
fn multi_package_compile_project_002() -> anyhow::Result<()> {
    run_project("project002")
}

#[test]
fn multi_package_compile_project_003() -> anyhow::Result<()> {
    run_project("project003")
}

#[test]
fn multi_package_compile_project_004() -> anyhow::Result<()> {
    run_project("project004")
}

#[test]
fn multi_package_compile_project_005() -> anyhow::Result<()> {
    run_project("project005")
}

#[test]
fn multi_package_compile_project_006() -> anyhow::Result<()> {
    run_project("project006")
}

#[test]
fn multi_package_compile_project_007() -> anyhow::Result<()> {
    run_project("project007_trait_impl_orphan_ok")
}

fn run_project(name: &str) -> anyhow::Result<()> {
    let root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/package")
        .join(name);
    let main_path = root.join("main.gom");
    let main_src = std::fs::read_to_string(&main_path)?;
    let compilation = pipeline::pipeline::compile(&main_path, &main_src)
        .map_err(|err| anyhow::anyhow!("compilation failed: {:?}", err))?;
    let go_source = compilation.go.to_pretty(&compilation.goenv, 120);
    let output = super::execute_go_source(&go_source)?;
    let out_path = root.join("main.gom.out");
    expect_file![out_path].assert_eq(&output);

    Ok(())
}
