use std::fs;
use std::path::Path;
use std::process::{Command, Stdio};

use anyhow::{Context, Result, bail};
use compiler::pipeline::pipeline::{Compilation, CompilationError, compile_single_file};
use compiler::pipeline::with_compiler_stack;
use tempfile::tempdir;

use crate::cli::{DumpStage, RunArgs};

use super::diagnostics::{render_compilation_error, source_map_from_source};

const PRETTY_WIDTH: usize = 120;

pub(crate) fn execute(args: RunArgs) -> Result<()> {
    let src = fs::read_to_string(&args.file)
        .with_context(|| format!("error reading goml file: {}", args.file.display()))?;
    let compilation = match compile_single_file(&args.file, &src) {
        Ok(compilation) => compilation,
        Err(err) => {
            report_compilation_error(&args.file, &src, err);
            std::process::exit(1);
        }
    };

    print_dumps(&compilation, &args.dumps());
    let go_source =
        with_compiler_stack(|| compilation.go.to_pretty(&compilation.goenv, PRETTY_WIDTH));
    print!("{}", execute_go_source(&go_source)?);
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
    let content = with_compiler_stack(|| match stage {
        DumpStage::Ast => compilation.ast.to_pretty(PRETTY_WIDTH),
        DumpStage::Hir => {
            let ctx = compiler::pprint::hir_pprint::HirPrintCtx::new(&compilation.hir_table);
            compilation.hir.to_pretty(&ctx, PRETTY_WIDTH)
        }
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
    });
    println!("== {} ==", stage.label());
    println!("{content}");
}

fn report_compilation_error(file_path: &Path, src: &str, err: CompilationError) {
    let source_map = source_map_from_source(file_path, src);
    eprintln!("{}", render_compilation_error(err, source_map));
}

fn execute_go_source(source: &str) -> Result<String> {
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

fn try_execute_with_yaegi(dir: &Path, file: &Path) -> Result<Option<String>> {
    let status = Command::new("yaegi")
        .arg("help")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status();
    if !status.is_ok_and(|status| status.success()) {
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
        .context("failed to execute yaegi")?;
    if !output.status.success() {
        return Ok(None);
    }
    Ok(Some(String::from_utf8_lossy(&output.stdout).to_string()))
}

fn execute_with_go_run(dir: &Path, file: &Path) -> Result<String> {
    let output = Command::new("go")
        .arg("run")
        .arg(file)
        .current_dir(dir)
        .env("TZ", "UTC")
        .env("GOWORK", "off")
        .env("GO111MODULE", "off")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .context("failed to execute go run")?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!("go run failed: {}", stderr.trim());
    }
    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}
