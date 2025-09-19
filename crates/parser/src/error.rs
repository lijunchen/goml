use diagnostics::{Diagnostic, Diagnostics, Severity, Stage};

pub trait DiagnosticFormatExt {
    fn format_with_line_index(&self, index: &line_index::LineIndex) -> String;
}

impl DiagnosticFormatExt for Diagnostic {
    fn format_with_line_index(&self, index: &line_index::LineIndex) -> String {
        if let Some(range) = self.range() {
            let line_col = index.line_col(range.start());
            format!(
                "{}:{}: {}",
                line_col.line + 1,
                line_col.col + 1,
                self.message()
            )
        } else {
            self.message().to_string()
        }
    }
}

pub fn format_parser_diagnostics(diagnostics: &Diagnostics, src: &str) -> Vec<String> {
    let index = line_index::LineIndex::new(src);
    diagnostics
        .iter()
        .filter(|diagnostic| {
            diagnostic.severity() == Severity::Error && diagnostic.stage() == &Stage::Parser
        })
        .map(|diagnostic| diagnostic.format_with_line_index(&index))
        .collect()
}
