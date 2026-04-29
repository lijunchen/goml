use crate::pipeline::pipeline::CompilationError;
use diagnostics::Diagnostics;

pub(crate) mod builtin_inherent;
pub mod modules;
pub mod packages;
#[allow(clippy::module_inception)]
pub mod pipeline;
pub mod separate;

pub(crate) fn with_compiler_stack<T>(f: impl FnOnce() -> T) -> T {
    #[cfg(not(target_arch = "wasm32"))]
    {
        stacker::grow(16 * 1024 * 1024, f)
    }
    #[cfg(target_arch = "wasm32")]
    {
        f()
    }
}

pub(crate) fn with_src_compiler_stack<T>(src: &str, f: impl FnOnce() -> T) -> T {
    let _ = src;
    with_compiler_stack(f)
}

pub fn compile_error(message: String) -> CompilationError {
    let mut diagnostics = Diagnostics::new();
    diagnostics.push(diagnostics::Diagnostic::new(
        diagnostics::Stage::other("compile"),
        diagnostics::Severity::Error,
        message,
    ));
    CompilationError::Compile { diagnostics }
}
