use std::path::{Path, PathBuf};
use std::sync::OnceLock;
use std::sync::{Arc, Mutex};
use std::time::Instant;

use std::process::{Command, Stdio};

use anyhow::Context;
use anyhow::bail;
use diagnostics::Diagnostics;
use parser::{debug_tree, format_parser_diagnostics};

use crate::{
    env::{format_compile_diagnostics, format_typer_diagnostics},
    pipeline::{self, pipeline::CompilationError},
};

mod builtin_functions_test;
mod constructor_value_test;
mod dyn_coercion_test;
mod e2e;
mod module;
mod multiline_string_test;
mod query_test;
mod ref_type_test;
mod separate_compile_test;
mod struct_type_test;
mod trait_impl_test;
mod try_expr_test;
mod tuple_projection_test;

#[test]
fn test_cases() -> anyhow::Result<()> {
    let start = Instant::now();
    let root_dir = std::path::PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let cases_dir = root_dir.join("src/tests/pipeline");
    if test_log_enabled() {
        eprintln!(
            "[test_cases] start root_dir={} cases_dir={}",
            root_dir.display(),
            cases_dir.display()
        );
        eprintln!("[test_cases] yaegi_available={}", yaegi_available());
    }

    let result = run_test_cases(&cases_dir);
    if test_log_enabled() {
        eprintln!("[test_cases] done elapsed={:?}", start.elapsed());
    }
    result
}

fn test_log_enabled() -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(|| {
        std::env::var("GOML_TEST_LOG")
            .ok()
            .is_some_and(|v| v == "1" || v == "true")
    })
}

fn yaegi_available() -> bool {
    static AVAILABLE: OnceLock<bool> = OnceLock::new();
    *AVAILABLE.get_or_init(|| {
        Command::new("yaegi")
            .arg("help")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .is_ok_and(|s| s.success())
    })
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
fn test_e2e_cases() -> anyhow::Result<()> {
    let root_dir = std::path::PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let e2e_dir = root_dir.join("src/tests/e2e");
    e2e::run_e2e_cases(&e2e_dir)
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
    let compilation = pipeline::pipeline::compile(&path, src)
        .map_err(|err| anyhow::anyhow!("compilation failed: {:?}", err))?;

    let go_source = compilation.go.to_pretty(&compilation.goenv, 120);
    let go_output = execute_go_source(&go_source, "reference_runtime_executes")?;

    assert_eq!(go_output, "5\n");

    Ok(())
}

fn execute_with_go_run(label: &str, dir: &Path, file: &Path) -> anyhow::Result<String> {
    let start = Instant::now();
    if test_log_enabled() {
        eprintln!(
            "[go_run] start label={} dir={} file={}",
            label,
            dir.display(),
            file.display()
        );
    }
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
    if test_log_enabled() {
        eprintln!("[go_run] spawned label={} pid={}", label, child.id());
    }
    let output = child.wait_with_output()?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        bail!(
            "go run failed for {} (status: {}):\nstdout:\n{}\nstderr:\n{}",
            label,
            output.status,
            stdout,
            stderr
        );
    }
    let ret = String::from_utf8_lossy(&output.stdout).to_string();
    if test_log_enabled() {
        eprintln!(
            "[go_run] done label={} status={} elapsed={:?}",
            label,
            output.status,
            start.elapsed()
        );
    }
    Ok(ret.replace(dir.to_str().unwrap(), "${WORKDIR}"))
}

fn execute_with_yaegi(label: &str, dir: &Path, file: &Path) -> anyhow::Result<String> {
    if !yaegi_available() {
        return execute_with_go_run(label, dir, file);
    }

    let start = Instant::now();
    if test_log_enabled() {
        eprintln!(
            "[yaegi] start label={} dir={} file={}",
            label,
            dir.display(),
            file.display()
        );
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
    if test_log_enabled() {
        eprintln!("[yaegi] spawned label={} pid={}", label, child.id());
    }
    let output = child.wait_with_output()?;
    if test_log_enabled() {
        if output.status.success() {
            eprintln!(
                "[yaegi] done label={} status={} elapsed={:?}",
                label,
                output.status,
                start.elapsed()
            );
        } else {
            let stderr = String::from_utf8_lossy(&output.stderr);
            let stderr_trimmed = stderr.trim();
            let stderr_short = if stderr_trimmed.len() > 800 {
                &stderr_trimmed[..800]
            } else {
                stderr_trimmed
            };
            eprintln!(
                "[yaegi] failed label={} status={} elapsed={:?} stderr=<<<{}>>>",
                label,
                output.status,
                start.elapsed(),
                stderr_short
            );
        }
    }

    let ret = if !output.status.success() {
        return execute_with_go_run(label, dir, file);
    } else {
        String::from_utf8_lossy(&output.stdout).to_string()
    };
    Ok(ret.replace(dir.to_str().unwrap(), "${WORKDIR}"))
}

fn execute_go_source(source: &str, label: &str) -> anyhow::Result<String> {
    let start = Instant::now();
    let dir = tempfile::tempdir().with_context(|| "Failed to create temporary directory")?;

    let main_go_file = dir.path().join("main.go");
    std::fs::write(&main_go_file, source)
        .with_context(|| format!("Failed to write go source to {}", main_go_file.display()))?;

    if test_log_enabled() {
        eprintln!(
            "[go_exec] wrote {} bytes label={} to {} (workdir={})",
            source.len(),
            label,
            main_go_file.display(),
            dir.path().display()
        );
    }

    let ret = execute_with_yaegi(label, dir.path(), &main_go_file)?;

    if test_log_enabled() {
        eprintln!("[go_exec] done elapsed={:?}", start.elapsed());
    }

    Ok(ret.replace(dir.path().to_str().unwrap(), "${WORKDIR}"))
}

#[test]
fn go_run_failure_is_error() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let main_go_file = dir.path().join("main.go");
    std::fs::write(
        &main_go_file,
        r#"
package main

func main() {
    undefined_symbol()
}
"#,
    )?;
    let err = execute_with_go_run("go_run_failure_is_error", dir.path(), &main_go_file)
        .expect_err("go run should fail for invalid source");
    let msg = format!("{err:#}");
    assert!(
        msg.contains("go run failed"),
        "unexpected error message: {msg}"
    );
    Ok(())
}

fn run_test_cases(dir: &Path) -> anyhow::Result<()> {
    let start = Instant::now();
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

    if let Ok(filter) = std::env::var("GOML_TEST_FILTER")
        && !filter.is_empty()
    {
        case_paths.retain(|p| p.to_string_lossy().contains(&filter));
        if test_log_enabled() {
            eprintln!(
                "[test_cases] filter={} matched={}",
                filter,
                case_paths.len()
            );
        }
    }

    if case_paths.is_empty() {
        return Ok(());
    }

    let available = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1);

    let default_worker_count = std::cmp::min(available, case_paths.len());
    let worker_count = std::env::var("GOML_TEST_WORKERS")
        .ok()
        .and_then(|s| s.parse::<usize>().ok())
        .filter(|&n| n > 0)
        .map(|n| n.min(case_paths.len()))
        .unwrap_or(default_worker_count);

    if test_log_enabled() {
        eprintln!(
            "[test_cases] discovered {} cases workers={} available={} elapsed={:?}",
            case_paths.len(),
            worker_count,
            available,
            start.elapsed()
        );
    }
    let cases: Arc<Mutex<Vec<PathBuf>>> = Arc::new(Mutex::new(case_paths));

    let mut handles = Vec::with_capacity(worker_count);
    for _ in 0..worker_count {
        let cases = Arc::clone(&cases);
        let handle = std::thread::Builder::new()
            .stack_size(16 * 1024 * 1024)
            .spawn(move || -> anyhow::Result<()> {
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
            })?;
        handles.push(handle);
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
    let start = Instant::now();
    println!("Testing file: {}", p.display());
    if test_log_enabled() {
        eprintln!("[case] start file={}", p.display());
    }
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

    let t_read = Instant::now();
    let input = std::fs::read_to_string(&p)?;
    if test_log_enabled() {
        eprintln!(
            "[case] read ok bytes={} elapsed={:?}",
            input.len(),
            t_read.elapsed()
        );
    }

    let t_compile = Instant::now();
    let compilation = pipeline::pipeline::compile(&p, &input).map_err(|err| match err {
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
            format_typer_diagnostics(&diagnostics, &input).join("\n")
        ),
        CompilationError::Compile { diagnostics } => anyhow::anyhow!(
            "Compile errors in {}:\n{}",
            p.display(),
            format_compile_diagnostics(&diagnostics, &input).join("\n")
        ),
    })?;
    if test_log_enabled() {
        eprintln!("[case] compile ok elapsed={:?}", t_compile.elapsed());
    }

    let t_snapshots = Instant::now();
    let t_cst = Instant::now();
    if test_log_enabled() {
        eprintln!("[case] cst debug_tree start");
    }
    let cst_debug = debug_tree(&compilation.green_node);
    if test_log_enabled() {
        eprintln!(
            "[case] cst debug_tree done bytes={} elapsed={:?}",
            cst_debug.len(),
            t_cst.elapsed()
        );
        eprintln!("[case] cst expect start");
    }
    expect_test::expect_file![cst_filename].assert_eq(&cst_debug);
    if test_log_enabled() {
        eprintln!("[case] snapshot cst elapsed={:?}", t_cst.elapsed());
    }

    let t_ast = Instant::now();
    expect_test::expect_file![ast_filename].assert_eq(&compilation.ast.to_pretty(120));
    if test_log_enabled() {
        eprintln!("[case] snapshot ast elapsed={:?}", t_ast.elapsed());
    }

    let t_hir = Instant::now();
    let hir_ctx = crate::pprint::hir_pprint::HirPrintCtx::new(&compilation.hir_table);
    expect_test::expect_file![hir_filename].assert_eq(&compilation.hir.to_pretty(&hir_ctx, 120));
    if test_log_enabled() {
        eprintln!("[case] snapshot hir elapsed={:?}", t_hir.elapsed());
    }

    let t_tast = Instant::now();
    expect_test::expect_file![tast_filename]
        .assert_eq(&compilation.tast.to_pretty(&compilation.genv, 120));
    if test_log_enabled() {
        eprintln!("[case] snapshot tast elapsed={:?}", t_tast.elapsed());
    }

    let t_core = Instant::now();
    expect_test::expect_file![core_filename]
        .assert_eq(&compilation.core.to_pretty(&compilation.genv, 120));
    if test_log_enabled() {
        eprintln!("[case] snapshot core elapsed={:?}", t_core.elapsed());
    }

    let t_mono = Instant::now();
    expect_test::expect_file![mono_filename]
        .assert_eq(&compilation.mono.to_pretty(&compilation.monoenv, 120));
    if test_log_enabled() {
        eprintln!("[case] snapshot mono elapsed={:?}", t_mono.elapsed());
    }

    let t_anf = Instant::now();
    expect_test::expect_file![anf_filename]
        .assert_eq(&compilation.anf.to_pretty(&compilation.anfenv, 120));
    if test_log_enabled() {
        eprintln!("[case] snapshot anf elapsed={:?}", t_anf.elapsed());
    }

    let t_go = Instant::now();
    let go_source = compilation.go.to_pretty(&compilation.goenv, 120);
    expect_test::expect_file![&go_filename].assert_eq(&go_source);
    if test_log_enabled() {
        eprintln!("[case] snapshot go elapsed={:?}", t_go.elapsed());
        eprintln!("[case] snapshots total elapsed={:?}", t_snapshots.elapsed());
    }

    let t_exec = Instant::now();
    let go_output = execute_go_source(&go_source, &p.to_string_lossy())?;
    if test_log_enabled() {
        eprintln!("[case] execute ok elapsed={:?}", t_exec.elapsed());
    }

    expect_test::expect_file![result_filename].assert_eq(&go_output);

    if test_log_enabled() {
        eprintln!("[case] done elapsed={:?}", start.elapsed());
    }

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
            match pipeline::pipeline::compile(&p, &input) {
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
                        format_typer_diagnostics(&diagnostics, &input).join("\n")
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
            let tmpdir = tempfile::tempdir()?;
            let tmpfile = tmpdir.path().join("main.gom");
            std::fs::write(&tmpfile, &input)?;
            match pipeline::pipeline::compile(&tmpfile, &input) {
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
                        format_typer_diagnostics(&diagnostics, &input).join("\n")
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
            let tmpdir = tempfile::tempdir()?;
            let tmpfile = tmpdir.path().join("main.gom");
            std::fs::write(&tmpfile, &input)?;
            match pipeline::pipeline::compile(&tmpfile, &input) {
                Err(CompilationError::Typer { diagnostics }) => {
                    let mut formatted = format_typer_diagnostics(&diagnostics, &input).join("\n");
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
