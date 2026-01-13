use crate::pipeline::pipeline::CompilationError;
use diagnostics::Diagnostics;

pub mod packages;
pub mod pipeline;

pub fn compile_error(message: String) -> CompilationError {
    let mut diagnostics = Diagnostics::new();
    diagnostics.push(diagnostics::Diagnostic::new(
        diagnostics::Stage::other("compile"),
        diagnostics::Severity::Error,
        message,
    ));
    CompilationError::Compile { diagnostics }
}
