use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, OnceLock};

use anyhow::Context;
use anyhow::{anyhow, bail};
use diagnostics::Diagnostics;
use parser::{debug_tree, format_parser_diagnostics};

use crate::{
    env::format_typer_diagnostics,
    pipeline::{self, CompilationError},
};

mod builtin_functions_test;
mod query_test;
mod struct_type_test;
mod trait_impl_test;

#[test]
fn test_cases() -> anyhow::Result<()> {
    let root_dir = std::path::PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let cases_dir = root_dir.join("src/tests/pipeline");
    run_test_cases(&cases_dir)
}

#[test]
fn test_examples() -> anyhow::Result<()> {
    let root_dir = std::path::PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let examples_dir = root_dir.join("src/tests/examples");
    run_test_cases(&examples_dir)
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

fn execute_go_source(source: &str) -> anyhow::Result<String> {
    go_executor()?.execute(source)
}

fn go_executor() -> anyhow::Result<&'static GoExecutor> {
    static EXECUTOR: OnceLock<GoExecutor> = OnceLock::new();
    if let Some(executor) = EXECUTOR.get() {
        return Ok(executor);
    }

    let executor = initialize_go_executor()?;
    let _ = EXECUTOR.set(executor);
    Ok(EXECUTOR.get().expect("go executor must be initialized"))
}

fn initialize_go_executor() -> anyhow::Result<GoExecutor> {
    let requested = std::env::var("GOML_GO_EXECUTOR");
    let requested_value = requested.as_deref().unwrap_or("yaegi");
    let allow_fallback = requested.is_err();

    match requested_value {
        "yaegi" => match YaegiExecutor::new() {
            Ok(executor) => Ok(GoExecutor::Yaegi(executor)),
            Err(err) if allow_fallback => {
                eprintln!(
                    "Falling back to `go run` executor because Yaegi is unavailable: {err:?}"
                );
                let fallback = GoRunExecutor::new()?;
                Ok(GoExecutor::GoRun(fallback))
            }
            Err(err) => Err(err),
        },
        "go" => {
            let executor = GoRunExecutor::new()?;
            Ok(GoExecutor::GoRun(executor))
        }
        other => Err(anyhow!("Unknown GOML_GO_EXECUTOR value `{}`", other)),
    }
}

enum GoExecutor {
    GoRun(GoRunExecutor),
    Yaegi(YaegiExecutor),
}

impl GoExecutor {
    fn execute(&self, source: &str) -> anyhow::Result<String> {
        match self {
            GoExecutor::GoRun(executor) => executor.execute(source),
            GoExecutor::Yaegi(executor) => executor.execute(source),
        }
    }
}

struct GoRunExecutor {
    go_bin: PathBuf,
}

impl GoRunExecutor {
    fn new() -> anyhow::Result<Self> {
        Ok(Self { go_bin: go_bin() })
    }

    fn execute(&self, source: &str) -> anyhow::Result<String> {
        use std::io::Write;
        use std::process::{Command, Stdio};

        let (dir, _) = prepare_go_execution(source)?;
        let mut child = Command::new(&self.go_bin)
            .arg("run")
            .arg("main.go")
            .current_dir(dir.path())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .with_context(|| format!("Failed to spawn go binary `{}`", self.go_bin.display()))?;
        if let Some(stdin) = child.stdin.as_mut() {
            stdin
                .write_all(b"")
                .context("Failed to write to go stdin")?;
        }
        let output = child
            .wait_with_output()
            .context("Failed to read go run output")?;
        Ok(format_execution_output(dir.path(), output))
    }
}

struct YaegiExecutor {
    yaegi_bin: PathBuf,
}

impl YaegiExecutor {
    fn new() -> anyhow::Result<Self> {
        use std::process::{Command, Stdio};

        let yaegi_bin = yaegi_bin();
        Command::new(&yaegi_bin)
            .arg("-help")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .with_context(|| {
                format!(
                    "Failed to spawn Yaegi interpreter `{}`",
                    yaegi_bin.display()
                )
            })?;
        Ok(Self { yaegi_bin })
    }

    fn execute(&self, source: &str) -> anyhow::Result<String> {
        use std::io::Write;
        use std::process::{Command, Stdio};

        let (dir, _) = prepare_go_execution(source)?;
        let mut child = Command::new(&self.yaegi_bin)
            .arg("run")
            .arg("main.go")
            .current_dir(dir.path())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .with_context(|| {
                format!(
                    "Failed to spawn Yaegi interpreter `{}`",
                    self.yaegi_bin.display()
                )
            })?;
        if let Some(stdin) = child.stdin.as_mut() {
            stdin
                .write_all(b"")
                .context("Failed to write to Yaegi stdin")?;
        }
        let output = child
            .wait_with_output()
            .context("Failed to read Yaegi output")?;
        Ok(format_execution_output(dir.path(), output))
    }
}

fn go_bin() -> PathBuf {
    if std::env::consts::OS == "linux" {
        let p = PathBuf::from("/usr/lib/go-1.25/bin/go");
        if p.exists() { p } else { PathBuf::from("go") }
    } else {
        PathBuf::from("go")
    }
}

fn yaegi_bin() -> PathBuf {
    std::env::var_os("YAEGI_BIN")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("yaegi"))
}

fn prepare_go_execution(source: &str) -> anyhow::Result<(tempfile::TempDir, PathBuf)> {
    let dir = tempfile::tempdir().with_context(|| "Failed to create temporary directory")?;
    let main_go_file = dir.path().join("main.go");
    std::fs::write(&main_go_file, source)
        .with_context(|| format!("Failed to write go source to {}", main_go_file.display()))?;
    Ok((dir, main_go_file))
}

fn format_execution_output(dir: &Path, output: std::process::Output) -> String {
    let raw = if output.status.success() {
        output.stdout
    } else {
        output.stderr
    };
    let text = String::from_utf8_lossy(&raw).to_string();
    match dir.to_str() {
        Some(path) => text.replace(path, "${WORKDIR}"),
        None => text,
    }
}

fn run_test_cases(dir: &Path) -> anyhow::Result<()> {
    let mut case_paths = Vec::new();
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        if entry.file_type()?.is_file()
            && entry.path().extension().and_then(std::ffi::OsStr::to_str) == Some("src")
        {
            case_paths.push(entry.path());
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
    let tast_filename = p.with_file_name(format!("{}.tast", filename));
    let core_filename = p.with_file_name(format!("{}.core", filename));
    let mono_filename = p.with_file_name(format!("{}.mono", filename));
    let anf_filename = p.with_file_name(format!("{}.anf", filename));
    let go_filename = p.with_file_name(format!("{}.gom", filename));
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
    })?;

    let cst_debug = debug_tree(&compilation.green_node);
    expect_test::expect_file![cst_filename].assert_eq(&cst_debug);

    expect_test::expect_file![ast_filename].assert_eq(&compilation.ast.to_pretty(120));
    expect_test::expect_file![tast_filename]
        .assert_eq(&compilation.tast.to_pretty(&compilation.typer_env, 120));
    expect_test::expect_file![core_filename]
        .assert_eq(&compilation.core.to_pretty(&compilation.typer_env, 120));
    expect_test::expect_file![mono_filename]
        .assert_eq(&compilation.mono.to_pretty(&compilation.env, 120));
    expect_test::expect_file![anf_filename]
        .assert_eq(&compilation.anf.to_pretty(&compilation.env, 120));

    let go_source = compilation.go.to_pretty(&compilation.env, 120);
    expect_test::expect_file![&go_filename].assert_eq(&go_source);

    let go_output = execute_go_source(&go_source)?;

    expect_test::expect_file![result_filename].assert_eq(&go_output);

    Ok(())
}

fn run_parse_error_cases(dir: &Path) -> anyhow::Result<()> {
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        if entry.file_type()?.is_file()
            && entry.path().extension().and_then(std::ffi::OsStr::to_str) == Some("src")
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

fn run_typer_error_cases(dir: &Path) -> anyhow::Result<()> {
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        if entry.file_type()?.is_file()
            && entry.path().extension().and_then(std::ffi::OsStr::to_str) == Some("src")
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
