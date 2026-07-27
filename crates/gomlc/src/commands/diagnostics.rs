use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use compiler::pipeline::pipeline::CompilationError;
use diagnostics::{SourceMap, TextRenderer};

pub(crate) fn source_map_from_source(path: &Path, source: &str) -> SourceMap {
    let mut source_map = SourceMap::new();
    source_map.add_file(path, source);
    source_map
}

pub(crate) fn source_map_from_paths(paths: &[PathBuf]) -> SourceMap {
    let mut source_map = SourceMap::new();
    let mut seen = HashSet::new();
    for path in paths {
        if !seen.insert(path) {
            continue;
        }
        if let Ok(source) = fs::read_to_string(path) {
            source_map.add_file(path, source);
        }
    }
    source_map
}

pub(crate) fn render_compilation_error(
    error: CompilationError,
    mut source_map: SourceMap,
) -> String {
    for path in error
        .diagnostics()
        .iter()
        .filter_map(|diagnostic| diagnostic.source())
    {
        if source_map.find(path).is_none()
            && let Ok(source) = fs::read_to_string(path)
        {
            source_map.add_file(path, source);
        }
    }
    let mut diagnostics = error.into_diagnostics();
    let sources = diagnostics
        .source_map_arc()
        .cloned()
        .unwrap_or_else(|| Arc::new(source_map));
    diagnostics.attach_source_map(Arc::clone(&sources));
    TextRenderer::new().render_all(&sources, &diagnostics)
}

pub(crate) fn compilation_error(error: CompilationError, source_map: SourceMap) -> anyhow::Error {
    anyhow::Error::msg(render_compilation_error(error, source_map))
}
