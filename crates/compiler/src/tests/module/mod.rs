use expect_test::expect_file;
use std::io::Write;

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

#[test]
fn multi_package_compile_project_023() -> anyhow::Result<()> {
    run_project("project023_private_type_public_signature")
}

#[test]
fn multi_package_compile_project_024() -> anyhow::Result<()> {
    run_project("project024_builtin_runtime_marker_collision")
}

#[test]
fn multi_package_compile_project_025() -> anyhow::Result<()> {
    run_project("project025_nested_module_path")
}

#[test]
fn multi_package_compile_project_026() -> anyhow::Result<()> {
    run_project("project026_nested_module_impl_locality")
}

#[test]
fn multi_package_compile_project_027() -> anyhow::Result<()> {
    run_project("project027_generic_trait")
}

#[test]
fn multi_package_compile_project_028() -> anyhow::Result<()> {
    run_project("project028_where_predicates")
}

#[test]
fn multi_package_compile_project_029() -> anyhow::Result<()> {
    run_project("project029_associated_types")
}

#[test]
fn multi_package_compile_project_030() -> anyhow::Result<()> {
    run_project("project030_supertraits")
}

#[test]
fn multi_package_compile_project_031() -> anyhow::Result<()> {
    run_project("project031_into_iterator")
}

#[test]
fn multi_package_compile_project_032() -> anyhow::Result<()> {
    run_project("project032_std_host_api")
}

#[test]
fn multi_package_compile_project_033() -> anyhow::Result<()> {
    run_project("project033_cross_file_pattern_constructor")
}

#[test]
fn multi_package_compile_project_034() -> anyhow::Result<()> {
    run_project("project034_packages_basic")
}

#[test]
fn std_host_binary_stdio_round_trip() -> anyhow::Result<()> {
    if !super::go_available() {
        return Ok(());
    }
    let dir = tempfile::tempdir()?;
    let main_path = dir.path().join("main.gom");
    let source = r#"
package main;

use std::bytes;
use std::env;
use std::io;
use std::process;

fn main() -> unit {
    match env::var("GOML_EMPTY") {
        Option::Some(value) => {
            if value != "" {
                io::eprintln("empty environment value changed");
                process::exit(1)
            } else {
                ()
            }
        },
        Option::None => {
            io::eprintln("empty environment value is missing");
            process::exit(1)
        },
    };
    match env::var("GOML_MISSING_ENVIRONMENT_VALUE") {
        Option::Some(value) => {
            io::eprintln("missing environment value exists: " + value);
            process::exit(1)
        },
        Option::None => (),
    };
    match io::read_stdin() {
        Result::Ok(data) => {
            match io::write_stdout(data) {
                Result::Ok(value) => value,
                Result::Err(error) => {
                    io::eprintln(error);
                    process::exit(1)
                },
            };
            match io::write_stderr(bytes::Bytes::from_string("binary stderr")) {
                Result::Ok(value) => value,
                Result::Err(error) => {
                    io::eprintln(error);
                    process::exit(1)
                },
            }
        },
        Result::Err(error) => {
            io::eprintln(error);
            process::exit(1)
        },
    }
}
"#;
    std::fs::write(
        dir.path().join("goml.toml"),
        "[module]\npath = \"stdio_test\"\n",
    )?;
    std::fs::write(&main_path, source)?;
    let compilation = pipeline::pipeline::compile(&main_path, source)
        .map_err(|error| anyhow::anyhow!("compilation failed: {error:?}"))?;
    let go_source = compilation.go.to_pretty(&compilation.goenv, 120);
    std::fs::write(dir.path().join("main.go"), go_source)?;
    let mut child = std::process::Command::new("go")
        .arg("run")
        .arg("main.go")
        .current_dir(dir.path())
        .env("GO111MODULE", "off")
        .env("GOWORK", "off")
        .env("GOML_EMPTY", "")
        .env_remove("GOML_MISSING_ENVIRONMENT_VALUE")
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()?;
    let input = [0u8, 127, 128, 255];
    child.stdin.take().unwrap().write_all(&input)?;
    let output = child.wait_with_output()?;
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(output.stdout, input);
    assert_eq!(output.stderr, b"binary stderr");
    Ok(())
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
    let mut root_sources = std::fs::read_dir(&root)?
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|path| path.extension().is_some_and(|extension| extension == "gom"))
        .collect::<Vec<_>>();
    root_sources.sort();
    let main_path = root_sources
        .into_iter()
        .next()
        .ok_or_else(|| anyhow::anyhow!("project has no root package source"))?;
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
