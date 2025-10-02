use compiler::env::format_typer_diagnostics;
use compiler::pipeline::{CompilationError, compile};

use parser::format_parser_diagnostics;

fn main() {
    let file_arg = std::env::args().nth(1).unwrap_or_else(|| {
        eprintln!("Usage: {} <file_path>", std::env::args().next().unwrap());
        std::process::exit(1);
    });
    let file_path = std::path::PathBuf::from(&file_arg);
    let content = std::fs::read_to_string(&file_path).unwrap_or_else(|_| {
        eprintln!("Error reading file: {}", file_path.display());
        std::process::exit(1);
    });
    let src = content;
    match compile(&file_path, &src) {
        Ok(compilation) => {
            println!("{}", compilation.go.to_pretty(&compilation.env, 120));
        }
        Err(err) => {
            match &err {
                CompilationError::Parser { diagnostics } => {
                    for error in format_parser_diagnostics(diagnostics, &src) {
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
            }
            std::process::exit(1);
        }
    }
}
