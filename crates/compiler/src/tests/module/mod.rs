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

#[test]
fn multi_package_compile_project_008() -> anyhow::Result<()> {
    run_project("project008_trait_bounds_across_packages")
}

#[test]
fn multi_package_compile_project_009() -> anyhow::Result<()> {
    run_project("project009_builtin_option_result")
}

#[test]
fn multi_package_compile_project_010() -> anyhow::Result<()> {
    run_project("project010_builtin_trait_show")
}

#[test]
fn multi_package_compile_project_011() -> anyhow::Result<()> {
    run_project("project011_complex_dependency_graph")
}

#[test]
fn multi_package_compile_project_012() -> anyhow::Result<()> {
    run_project("project012_trait_scope_and_ufcs")
}

#[test]
fn multi_package_compile_project_013() -> anyhow::Result<()> {
    run_project("project013_dyn_coercion_across_packages")
}

#[test]
fn multi_package_compile_project_014() -> anyhow::Result<()> {
    run_project("project014_generic_bounds_cross_package_ufcs")
}

#[test]
fn multi_package_compile_project_015() -> anyhow::Result<()> {
    run_project("project015_trait_impl_visibility_for_builtin_container")
}

#[test]
fn multi_package_compile_project_016() -> anyhow::Result<()> {
    run_project("project016_try_option_cross_package")
}

#[test]
fn multi_package_compile_project_017() -> anyhow::Result<()> {
    run_project("project017_try_go_error_cross_package")
}

#[test]
fn multi_package_compile_project_018() -> anyhow::Result<()> {
    run_project("project018_goml_go_shims")
}

#[test]
fn multi_package_compile_project_019() -> anyhow::Result<()> {
    run_project("project019_cross_package_inherent_method")
}

#[test]
fn multi_package_compile_project_020() -> anyhow::Result<()> {
    run_project("project020_pascal_package_custom_entry")
}

#[test]
fn multi_package_compile_project_021() -> anyhow::Result<()> {
    run_project("project021_library_main_function")
}

#[test]
fn multi_package_compile_project_022() -> anyhow::Result<()> {
    run_project("project022_path_escape_type_collision")
}

fn run_project(name: &str) -> anyhow::Result<()> {
    let name = name.to_string();
    let handle = std::thread::Builder::new()
        .stack_size(16 * 1024 * 1024)
        .spawn(move || run_project_inner(&name))?;
    match handle.join() {
        Ok(result) => result,
        Err(panic) => std::panic::resume_unwind(panic),
    }
}

fn run_project_inner(name: &str) -> anyhow::Result<()> {
    let root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/module")
        .join(name);
    let crate_unit = pipeline::modules::discover_crate_from_dir(&root)
        .map_err(|err| anyhow::anyhow!("crate module discovery failed: {:?}", err))?;
    let main_path = crate_unit.root_file;
    let main_src = std::fs::read_to_string(&main_path)?;
    let compilation = pipeline::pipeline::compile(&main_path, &main_src)
        .map_err(|err| anyhow::anyhow!("compilation failed: {:?}", err))?;
    let go_source = compilation.go.to_pretty(&compilation.goenv, 120);
    if !super::runtime_executor_available() {
        println!("Skipping module runtime output: {}", main_path.display());
        return Ok(());
    }
    let output = super::execute_go_source(&go_source, &main_path.to_string_lossy())?;
    let out_path = root.join("main.gom.out");
    expect_file![out_path].assert_eq(&output);

    Ok(())
}
