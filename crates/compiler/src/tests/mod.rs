use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use std::process::{Command, Stdio};

use anyhow::Context;
use anyhow::bail;
use diagnostics::Diagnostics;
use parser::{debug_tree, format_parser_diagnostics};

use crate::{
    env::{format_compile_diagnostics, format_typer_diagnostics},
    pipeline::{self, CompilationError},
};

mod builtin_functions_test;
mod multiline_string_test;
mod query_test;
mod ref_type_test;
mod struct_type_test;
mod trait_impl_test;

#[test]
fn test_cases() -> anyhow::Result<()> {
    let root_dir = std::path::PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let cases_dir = root_dir.join("src/tests/pipeline");
    run_test_cases(&cases_dir)
}

#[test]
fn test_parse_error_cases() -> anyhow::Result<()> {
    let root_dir = std::path::PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let diagnostics_dir = root_dir.join("src/tests/diagnostics");
    run_parse_error_cases(&diagnostics_dir)
}

#[test]
fn test_typer_error_cases() -> anyhow::Result<()> {
    let root_dir = std::path::PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let diagnostics_dir = root_dir.join("src/tests/typer");
    run_typer_error_cases(&diagnostics_dir)
}

#[test]
fn test_compile_error_cases() -> anyhow::Result<()> {
    let root_dir = std::path::PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let diagnostics_dir = root_dir.join("src/tests/diagnostics/compile");
    run_compile_error_cases(&diagnostics_dir)
}

#[test]
fn reference_runtime_executes() -> anyhow::Result<()> {
    let src = r#"
fn main() -> unit {
    let r = ref(3);
    let _ = ref_set(r, 5);
    string_println(int32_to_string(ref_get(r)))
}
"#;

    let path = PathBuf::from("ref_runtime.go");
    let compilation = pipeline::compile(&path, src)
        .map_err(|err| anyhow::anyhow!("compilation failed: {:?}", err))?;

    let go_source = compilation.go.to_pretty(&compilation.goenv, 120);
    let go_output = execute_go_source(&go_source)?;

    assert_eq!(go_output, "5\n");

    Ok(())
}

fn execute_with_go_run(dir: &Path, file: &Path) -> anyhow::Result<String> {
    let child = Command::new("go")
        .arg("run")
        .arg(file)
        .current_dir(dir)
        .env("TZ", "UTC")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    let output = child.wait_with_output()?;
    let ret = if !output.status.success() {
        String::from_utf8_lossy(&output.stderr).to_string()
    } else {
        String::from_utf8_lossy(&output.stdout).to_string()
    };
    Ok(ret.replace(dir.to_str().unwrap(), "${WORKDIR}"))
}

fn execute_with_yaegi(dir: &Path, file: &Path) -> anyhow::Result<String> {
    let status = Command::new("yaegi")
        .arg("help")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status();

    if status.is_err() || !status.unwrap().success() {
        return execute_with_go_run(dir, file);
    }

    let child = Command::new("yaegi")
        .arg("run")
        .arg(file)
        .current_dir(dir)
        .env("TZ", "UTC")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    let output = child.wait_with_output()?;
    let ret = if !output.status.success() {
        return execute_with_go_run(dir, file);
    } else {
        String::from_utf8_lossy(&output.stdout).to_string()
    };
    Ok(ret.replace(dir.to_str().unwrap(), "${WORKDIR}"))
}

fn execute_go_source(source: &str) -> anyhow::Result<String> {
    let dir = tempfile::tempdir().with_context(|| "Failed to create temporary directory")?;

    let main_go_file = dir.path().join("main.go");
    std::fs::write(&main_go_file, source)
        .with_context(|| format!("Failed to write go source to {}", main_go_file.display()))?;

    let ret = execute_with_yaegi(dir.path(), &main_go_file)?;

    Ok(ret.replace(dir.path().to_str().unwrap(), "${WORKDIR}"))
}

fn run_test_cases(dir: &Path) -> anyhow::Result<()> {
    let mut case_paths = Vec::new();
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        if entry.file_type()?.is_dir() {
            let main_gom = entry.path().join("main.gom");
            if main_gom.exists() {
                case_paths.push(main_gom);
            }
        }
    }

    if case_paths.is_empty() {
        return Ok(());
    }

    let available = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1);
    let worker_count = std::cmp::min(available, case_paths.len());
    let cases: Arc<Mutex<Vec<PathBuf>>> = Arc::new(Mutex::new(case_paths));

    let mut handles = Vec::with_capacity(worker_count);
    for _ in 0..worker_count {
        let cases = Arc::clone(&cases);
        handles.push(std::thread::spawn(move || -> anyhow::Result<()> {
            loop {
                let case = {
                    let mut cases = cases.lock().unwrap();
                    cases.pop()
                };

                match case {
                    Some(case) => run_single_test_case(case)?,
                    None => break,
                }
            }

            Ok(())
        }));
    }

    for handle in handles {
        match handle.join() {
            Ok(result) => result?,
            Err(panic) => std::panic::resume_unwind(panic),
        }
    }

    Ok(())
}

fn run_single_test_case(p: PathBuf) -> anyhow::Result<()> {
    println!("Testing file: {}", p.display());
    let filename = p.file_name().unwrap().to_str().unwrap();
    let cst_filename = p.with_file_name(format!("{}.cst", filename));
    let ast_filename = p.with_file_name(format!("{}.ast", filename));
    let hir_filename = p.with_file_name(format!("{}.hir", filename));
    let tast_filename = p.with_file_name(format!("{}.tast", filename));
    let core_filename = p.with_file_name(format!("{}.core", filename));
    let mono_filename = p.with_file_name(format!("{}.mono", filename));
    let anf_filename = p.with_file_name(format!("{}.anf", filename));
    let go_filename = p.with_file_name(format!("{}.go", filename));
    let result_filename = p.with_file_name(format!("{}.out", filename));

    let input = std::fs::read_to_string(&p)?;

    let compilation = pipeline::compile(&p, &input).map_err(|err| match err {
        CompilationError::Parser { diagnostics } => anyhow::anyhow!(
            "Parse errors in {}:\n{}",
            p.display(),
            format_parser_diagnostics(&diagnostics, &input).join("\n")
        ),
        CompilationError::Lower { diagnostics } => anyhow::anyhow!(
            "Lowering errors in {}:\n{}",
            p.display(),
            format_lower_diagnostics(&diagnostics).join("\n")
        ),
        CompilationError::Typer { diagnostics } => anyhow::anyhow!(
            "Typer errors in {}:\n{}",
            p.display(),
            format_typer_diagnostics(&diagnostics).join("\n")
        ),
        CompilationError::Compile { diagnostics } => anyhow::anyhow!(
            "Compile errors in {}:\n{}",
            p.display(),
            format_compile_diagnostics(&diagnostics, &input).join("\n")
        ),
    })?;

    let cst_debug = debug_tree(&compilation.green_node);
    expect_test::expect_file![cst_filename].assert_eq(&cst_debug);

    expect_test::expect_file![ast_filename].assert_eq(&compilation.ast.to_pretty(120));
    expect_test::expect_file![hir_filename].assert_eq(&compilation.hir.to_pretty(120));
    expect_test::expect_file![tast_filename]
        .assert_eq(&compilation.tast.to_pretty(&compilation.genv, 120));
    expect_test::expect_file![core_filename]
        .assert_eq(&compilation.core.to_pretty(&compilation.genv, 120));
    expect_test::expect_file![mono_filename]
        .assert_eq(&compilation.mono.to_pretty(&compilation.monoenv, 120));
    expect_test::expect_file![anf_filename]
        .assert_eq(&compilation.anf.to_pretty(&compilation.anfenv, 120));

    let go_source = compilation.go.to_pretty(&compilation.goenv, 120);
    expect_test::expect_file![&go_filename].assert_eq(&go_source);

    let go_output = execute_go_source(&go_source)?;

    expect_test::expect_file![result_filename].assert_eq(&go_output);

    Ok(())
}

fn run_parse_error_cases(dir: &Path) -> anyhow::Result<()> {
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        if entry.file_type()?.is_file()
            && entry.path().extension().and_then(std::ffi::OsStr::to_str) == Some("gom")
        {
            let p = entry.path();
            println!("Testing diagnostics: {}", p.display());
            let filename = p.file_name().unwrap().to_str().unwrap();
            let diag_filename = p.with_file_name(format!("{}.diag", filename));

            let input = std::fs::read_to_string(&p)?;
            match pipeline::compile(&p, &input) {
                Err(CompilationError::Parser { diagnostics }) => {
                    let mut formatted = format_parser_diagnostics(&diagnostics, &input).join("\n");
                    if !formatted.is_empty() {
                        formatted.push('\n');
                    }

                    expect_test::expect_file![diag_filename].assert_eq(&formatted);
                }
                Err(CompilationError::Lower { diagnostics }) => {
                    bail!(
                        "Expected parse errors in {}, but lowering reported diagnostics: {}",
                        p.display(),
                        format_lower_diagnostics(&diagnostics).join("\n")
                    );
                }
                Err(CompilationError::Typer { diagnostics }) => {
                    bail!(
                        "Expected parse errors in {}, but typer reported diagnostics: {}",
                        p.display(),
                        format_typer_diagnostics(&diagnostics).join("\n")
                    );
                }
                Err(CompilationError::Compile { diagnostics }) => {
                    bail!(
                        "Expected parse errors in {}, but compile stage reported diagnostics: {}",
                        p.display(),
                        format_compile_diagnostics(&diagnostics, &input).join("\n")
                    );
                }
                Ok(_) => {
                    bail!(
                        "Expected parse errors in {}, but compilation succeeded",
                        p.display()
                    );
                }
            }
        }
    }
    Ok(())
}

fn run_compile_error_cases(dir: &Path) -> anyhow::Result<()> {
    if !dir.exists() {
        return Ok(());
    }

    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        if entry.file_type()?.is_file()
            && entry.path().extension().and_then(std::ffi::OsStr::to_str) == Some("gom")
        {
            let p = entry.path();
            println!("Testing compile diagnostics: {}", p.display());
            let filename = p.file_name().unwrap().to_str().unwrap();
            let diag_filename = p.with_file_name(format!("{}.diag", filename));

            let input = std::fs::read_to_string(&p)?;
            match pipeline::compile(&p, &input) {
                Err(CompilationError::Compile { diagnostics }) => {
                    let mut formatted = format_compile_diagnostics(&diagnostics, &input).join("\n");
                    if !formatted.is_empty() {
                        formatted.push('\n');
                    }

                    expect_test::expect_file![diag_filename].assert_eq(&formatted);
                }
                Err(CompilationError::Parser { diagnostics }) => {
                    bail!(
                        "Expected compile diagnostics in {}, but parser reported diagnostics: {}",
                        p.display(),
                        format_parser_diagnostics(&diagnostics, &input).join("\n")
                    );
                }
                Err(CompilationError::Lower { diagnostics }) => {
                    bail!(
                        "Expected compile diagnostics in {}, but lowering reported diagnostics: {}",
                        p.display(),
                        format_lower_diagnostics(&diagnostics).join("\n")
                    );
                }
                Err(CompilationError::Typer { diagnostics }) => {
                    bail!(
                        "Expected compile diagnostics in {}, but typer reported diagnostics: {}",
                        p.display(),
                        format_typer_diagnostics(&diagnostics).join("\n")
                    );
                }
                Ok(_) => {
                    bail!(
                        "Expected compile diagnostics in {}, but compilation succeeded",
                        p.display()
                    );
                }
            }
        }
    }

    Ok(())
}

fn run_typer_error_cases(dir: &Path) -> anyhow::Result<()> {
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        if entry.file_type()?.is_file()
            && entry.path().extension().and_then(std::ffi::OsStr::to_str) == Some("gom")
        {
            let p = entry.path();
            println!("Testing typer diagnostics: {}", p.display());
            let filename = p.file_name().unwrap().to_str().unwrap();
            let diag_filename = p.with_file_name(format!("{}.diag", filename));

            let input = std::fs::read_to_string(&p)?;
            match pipeline::compile(&p, &input) {
                Err(CompilationError::Typer { diagnostics }) => {
                    let mut formatted = format_typer_diagnostics(&diagnostics).join("\n");
                    if !formatted.is_empty() {
                        formatted.push('\n');
                    }

                    expect_test::expect_file![diag_filename].assert_eq(&formatted);
                }
                Err(CompilationError::Parser { diagnostics }) => {
                    bail!(
                        "Expected typer diagnostics in {}, but parser reported diagnostics: {}",
                        p.display(),
                        format_parser_diagnostics(&diagnostics, &input).join("\n")
                    );
                }
                Err(CompilationError::Lower { diagnostics }) => {
                    bail!(
                        "Expected typer diagnostics in {}, but lowering reported diagnostics: {}",
                        p.display(),
                        format_lower_diagnostics(&diagnostics).join("\n")
                    );
                }
                Err(CompilationError::Compile { diagnostics }) => {
                    bail!(
                        "Expected typer diagnostics in {}, but compile stage reported diagnostics: {}",
                        p.display(),
                        format_compile_diagnostics(&diagnostics, &input).join("\n")
                    );
                }
                Ok(_) => {
                    bail!(
                        "Expected typer diagnostics in {}, but compilation succeeded",
                        p.display()
                    );
                }
            }
        }
    }

    Ok(())
}

fn format_lower_diagnostics(diagnostics: &Diagnostics) -> Vec<String> {
    diagnostics
        .iter()
        .map(|diagnostic| diagnostic.message().to_string())
        .collect()
}
