use std::path::Path;

use diagnostics::Diagnostics;
use parser::format_parser_diagnostics;

use crate::{
    env::{format_compile_diagnostics, format_typer_diagnostics},
    pipeline::{self, pipeline::CompilationError},
};

use super::{execute_go_source, test_log_enabled};

fn format_lower_diagnostics(diagnostics: &Diagnostics) -> Vec<String> {
    diagnostics
        .iter()
        .map(|diagnostic| diagnostic.message().to_string())
        .collect()
}

pub fn run_e2e_cases(dir: &Path) -> anyhow::Result<()> {
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

    case_paths.sort();

    if let Ok(filter) = std::env::var("GOML_TEST_FILTER")
        && !filter.is_empty()
    {
        case_paths.retain(|p| p.to_string_lossy().contains(&filter));
    }

    for p in case_paths {
        run_single_e2e_case(&p)?;
    }

    Ok(())
}

fn run_single_e2e_case(p: &Path) -> anyhow::Result<()> {
    println!("Testing e2e: {}", p.display());
    let filename = p.file_name().unwrap().to_str().unwrap();
    let result_filename = p.with_file_name(format!("{}.out", filename));

    if !result_filename.exists() {
        println!("  skipping (no .out file)");
        return Ok(());
    }

    let input = std::fs::read_to_string(p)?;

    let tmpdir = tempfile::tempdir()?;
    let tmpfile = tmpdir.path().join("main.gom");
    std::fs::write(&tmpfile, &input)?;

    let output = match pipeline::pipeline::compile(&tmpfile, &input) {
        Ok(compilation) => {
            let go_source = compilation.go.to_pretty(&compilation.goenv, 120);
            execute_go_source(&go_source, &p.to_string_lossy())?
        }
        Err(err) => format_compilation_error(&err, &input),
    };

    expect_test::expect_file![result_filename].assert_eq(&output);

    if test_log_enabled() {
        eprintln!("[e2e] done file={}", p.display());
    }

    Ok(())
}

fn format_compilation_error(err: &CompilationError, input: &str) -> String {
    let lines = match err {
        CompilationError::Parser { diagnostics } => {
            format_parser_diagnostics(diagnostics, input)
        }
        CompilationError::Lower { diagnostics } => {
            format_lower_diagnostics(diagnostics)
        }
        CompilationError::Typer { diagnostics } => {
            format_typer_diagnostics(diagnostics, input)
        }
        CompilationError::Compile { diagnostics } => {
            format_compile_diagnostics(diagnostics, input)
        }
    };
    let mut result = lines.join("\n");
    if !result.is_empty() {
        result.push('\n');
    }
    result
}
