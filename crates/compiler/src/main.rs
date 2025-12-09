use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use anyhow::{Context, anyhow, bail};
use compiler::env::{format_compile_diagnostics, format_typer_diagnostics};
use compiler::pipeline::{Compilation, CompilationError, compile};
use parser::format_parser_diagnostics;
use tempfile::tempdir;

const PRETTY_WIDTH: usize = 120;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum DumpStage {
    Ast,
    Tast,
    Core,
    Mono,
    Lift,
    Anf,
    Go,
}

impl DumpStage {
    fn label(&self) -> &'static str {
        match self {
            DumpStage::Ast => "AST",
            DumpStage::Tast => "Typed AST",
            DumpStage::Core => "Core",
            DumpStage::Mono => "Mono",
            DumpStage::Lift => "Lifted",
            DumpStage::Anf => "ANF",
            DumpStage::Go => "Go",
        }
    }

    fn order(&self) -> usize {
        match self {
            DumpStage::Ast => 0,
            DumpStage::Tast => 1,
            DumpStage::Core => 2,
            DumpStage::Mono => 3,
            DumpStage::Lift => 4,
            DumpStage::Anf => 5,
            DumpStage::Go => 6,
        }
    }
}

struct RunOptions {
    file_path: PathBuf,
    dumps: Vec<DumpStage>,
}

fn main() {
    if let Err(err) = run_cli() {
        eprintln!("{err}");
        std::process::exit(1);
    }
}

fn run_cli() -> anyhow::Result<()> {
    let mut args = std::env::args();
    let program = args.next().unwrap_or_else(|| "goml".to_string());
    let Some(command) = args.next() else {
        print_usage(&program);
        bail!("missing command");
    };

    match command.as_str() {
        "run" => {
            let options = parse_run_args(args)?;
            execute_run(options)
        }
        "-h" | "--help" | "help" => {
            print_usage(&program);
            Ok(())
        }
        other => {
            print_usage(&program);
            bail!("unknown command: {other}");
        }
    }
}

fn parse_run_args<I>(args: I) -> anyhow::Result<RunOptions>
where
    I: IntoIterator<Item = String>,
{
    let mut dumps = Vec::new();
    let mut file_path = None;

    for arg in args {
        match arg.as_str() {
            "--dump-ast" => dumps.push(DumpStage::Ast),
            "--dump-tast" => dumps.push(DumpStage::Tast),
            "--dump-core" => dumps.push(DumpStage::Core),
            "--dump-mono" => dumps.push(DumpStage::Mono),
            "--dump-lift" => dumps.push(DumpStage::Lift),
            "--dump-anf" => dumps.push(DumpStage::Anf),
            "--dump-go" => dumps.push(DumpStage::Go),
            flag if flag.starts_with("--dump-") => {
                bail!("unknown dump flag: {flag}");
            }
            flag if flag.starts_with("--") => {
                bail!("unknown flag: {flag}");
            }
            path => {
                if file_path.is_some() {
                    bail!("multiple input files provided: {path}");
                }
                file_path = Some(PathBuf::from(path));
            }
        }
    }

    let file_path = file_path.ok_or_else(|| anyhow!("missing input file path"))?;

    dumps.sort_by_key(|stage| stage.order());
    dumps.dedup();

    Ok(RunOptions { file_path, dumps })
}

fn execute_run(options: RunOptions) -> anyhow::Result<()> {
    let src = fs::read_to_string(&options.file_path)
        .with_context(|| format!("error reading goml file: {}", options.file_path.display()))?;

    let compilation = match compile(&options.file_path, &src) {
        Ok(compilation) => compilation,
        Err(err) => {
            report_compilation_error(&options.file_path, &src, err);
            std::process::exit(1);
        }
    };

    if !options.dumps.is_empty() {
        print_dumps(&compilation, &options.dumps);
    }

    let go_source = compilation.go.to_pretty(&compilation.goenv, PRETTY_WIDTH);
    let output = execute_go_source(&go_source)?;
    print!("{output}");

    Ok(())
}

fn print_dumps(compilation: &Compilation, dumps: &[DumpStage]) {
    for (idx, stage) in dumps.iter().enumerate() {
        if idx > 0 {
            println!();
        }
        print_dump(compilation, *stage);
    }
}

fn print_dump(compilation: &Compilation, stage: DumpStage) {
    let content = match stage {
        DumpStage::Ast => compilation.ast.to_pretty(PRETTY_WIDTH),
        DumpStage::Tast => compilation.tast.to_pretty(&compilation.genv, PRETTY_WIDTH),
        DumpStage::Core => compilation.core.to_pretty(&compilation.genv, PRETTY_WIDTH),
        DumpStage::Mono => compilation
            .mono
            .to_pretty(&compilation.monoenv, PRETTY_WIDTH),
        DumpStage::Lift => compilation
            .lambda
            .to_pretty(&compilation.liftenv, PRETTY_WIDTH),
        DumpStage::Anf => compilation.anf.to_pretty(&compilation.anfenv, PRETTY_WIDTH),
        DumpStage::Go => compilation.go.to_pretty(&compilation.goenv, PRETTY_WIDTH),
    };

    println!("== {} ==", stage.label());
    println!("{content}");
}

fn report_compilation_error(file_path: &Path, src: &str, err: CompilationError) {
    match &err {
        CompilationError::Parser { diagnostics } => {
            for error in format_parser_diagnostics(diagnostics, src) {
                eprintln!("error: {}: {}", file_path.display(), error);
            }
        }
        CompilationError::Lower { diagnostics } => {
            for diagnostic in diagnostics.iter() {
                eprintln!(
                    "error (lower): {}: {}",
                    file_path.display(),
                    diagnostic.message()
                );
            }
        }
        CompilationError::Typer { diagnostics } => {
            for error in format_typer_diagnostics(diagnostics) {
                eprintln!("error (typer): {}: {}", file_path.display(), error);
            }
        }
        CompilationError::Compile { diagnostics } => {
            for error in format_compile_diagnostics(diagnostics, src) {
                eprintln!("error (compile): {}: {}", file_path.display(), error);
            }
        }
    }
}

fn execute_go_source(source: &str) -> anyhow::Result<String> {
    let dir = tempdir().context("failed to create temporary directory for Go output")?;
    let main_go_file = dir.path().join("main.go");
    fs::write(&main_go_file, source).with_context(|| {
        format!(
            "failed to write generated Go source to {}",
            main_go_file.display()
        )
    })?;

    if let Some(output) = try_execute_with_yaegi(dir.path(), &main_go_file)? {
        return Ok(output);
    }

    execute_with_go_run(dir.path(), &main_go_file)
}

fn try_execute_with_yaegi(dir: &Path, file: &Path) -> anyhow::Result<Option<String>> {
    let status = Command::new("yaegi")
        .arg("help")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status();

    if status.is_err() || !status.unwrap().success() {
        return Ok(None);
    }

    let output = Command::new("yaegi")
        .arg("run")
        .arg(file)
        .current_dir(dir)
        .env("TZ", "UTC")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .with_context(|| "failed to execute yaegi")?;

    if output.status.success() {
        return Ok(Some(String::from_utf8_lossy(&output.stdout).to_string()));
    }

    Ok(None)
}

fn execute_with_go_run(dir: &Path, file: &Path) -> anyhow::Result<String> {
    let output = Command::new("go")
        .arg("run")
        .arg(file)
        .current_dir(dir)
        .env("TZ", "UTC")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .with_context(|| "failed to execute go")?;

    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        if stderr.is_empty() {
            bail!("go run failed:\n{}", stdout.trim_end());
        } else {
            bail!("go run failed:\n{}", stderr.trim_end());
        }
    }
}

fn print_usage(program: &str) {
    eprintln!("Usage:");
    eprintln!(
        "  {program} run [--dump-ast --dump-tast --dump-core --dump-mono --dump-lift --dump-anf --dump-go] <file>"
    );
    eprintln!();
    eprintln!("Examples:");
    eprintln!("  {program} run examples/hello.gom");
    eprintln!("  {program} run --dump-ast --dump-go examples/hello.gom");
}
