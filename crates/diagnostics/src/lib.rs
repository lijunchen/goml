use std::borrow::Cow;
use std::fmt;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use text_size::TextRange;

mod render;
mod source;

pub use render::TextRenderer;
pub use source::{Position, SourceError, SourceFile, SourceId, SourceMap, Span};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Severity {
    Error,
    Warning,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LabelSeverity {
    Primary,
    Secondary,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Label {
    severity: LabelSeverity,
    span: Span,
    message: Option<String>,
}

impl Label {
    pub fn new(severity: LabelSeverity, span: Span) -> Self {
        Self {
            severity,
            span,
            message: None,
        }
    }

    pub fn primary(span: Span) -> Self {
        Self::new(LabelSeverity::Primary, span)
    }

    pub fn secondary(span: Span) -> Self {
        Self::new(LabelSeverity::Secondary, span)
    }

    pub fn with_message(mut self, message: impl Into<String>) -> Self {
        self.message = Some(message.into());
        self
    }

    pub const fn severity(&self) -> LabelSeverity {
        self.severity
    }

    pub const fn span(&self) -> Span {
        self.span
    }

    pub fn message(&self) -> Option<&str> {
        self.message.as_deref()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Note(String);

impl Note {
    pub fn new(text: impl Into<String>) -> Self {
        Self(text.into())
    }

    pub fn text(&self) -> &str {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Help(String);

impl Help {
    pub fn new(text: impl Into<String>) -> Self {
        Self(text.into())
    }

    pub fn text(&self) -> &str {
        &self.0
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FixApplicability {
    MachineApplicable,
    MaybeIncorrect,
    HasPlaceholders,
    #[default]
    Unspecified,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FixIt {
    span: Span,
    replacement: String,
    message: Option<String>,
    applicability: FixApplicability,
}

impl FixIt {
    pub fn new(span: Span, replacement: impl Into<String>) -> Self {
        Self {
            span,
            replacement: replacement.into(),
            message: None,
            applicability: FixApplicability::Unspecified,
        }
    }

    pub fn with_message(mut self, message: impl Into<String>) -> Self {
        self.message = Some(message.into());
        self
    }

    pub fn with_applicability(mut self, applicability: FixApplicability) -> Self {
        self.applicability = applicability;
        self
    }

    pub const fn span(&self) -> Span {
        self.span
    }

    pub fn replacement(&self) -> &str {
        &self.replacement
    }

    pub fn message(&self) -> Option<&str> {
        self.message.as_deref()
    }

    pub const fn applicability(&self) -> FixApplicability {
        self.applicability
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stage {
    Parser,
    Typer,
    Other(Cow<'static, str>),
}

impl Stage {
    pub fn other(name: impl Into<Cow<'static, str>>) -> Self {
        Stage::Other(name.into())
    }

    pub fn as_str(&self) -> &str {
        match self {
            Stage::Parser => "parser",
            Stage::Typer => "typer",
            Stage::Other(name) => name.as_ref(),
        }
    }
}

#[derive(Clone)]
pub struct Diagnostic {
    stage: Stage,
    severity: Severity,
    message: String,
    range: Option<TextRange>,
    source: Option<PathBuf>,
    details: Box<DiagnosticDetails>,
}

#[derive(Default, Clone)]
struct DiagnosticDetails {
    labels: Vec<Label>,
    notes: Vec<Note>,
    helps: Vec<Help>,
    fixes: Vec<FixIt>,
}

impl fmt::Debug for Diagnostic {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("Diagnostic")
            .field("stage", &self.stage)
            .field("severity", &self.severity)
            .field("message", &self.message)
            .field("range", &self.range)
            .finish()
    }
}

impl Diagnostic {
    pub fn new(stage: Stage, severity: Severity, message: impl Into<String>) -> Self {
        Self {
            stage,
            severity,
            message: message.into(),
            range: None,
            source: None,
            details: Box::default(),
        }
    }

    pub fn with_range(mut self, range: impl Into<Option<TextRange>>) -> Self {
        self.range = range.into();
        self
    }

    pub fn stage(&self) -> &Stage {
        &self.stage
    }

    pub fn severity(&self) -> Severity {
        self.severity
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn range(&self) -> Option<TextRange> {
        self.range
    }

    pub fn with_source(mut self, source: impl Into<PathBuf>) -> Self {
        self.source = Some(source.into());
        self
    }

    pub fn source(&self) -> Option<&Path> {
        self.source.as_deref()
    }

    pub fn with_label(mut self, label: Label) -> Self {
        self.details.labels.push(label);
        self
    }

    pub fn with_primary_label(self, span: Span, message: impl Into<String>) -> Self {
        self.with_label(Label::primary(span).with_message(message))
    }

    pub fn with_secondary_label(self, span: Span, message: impl Into<String>) -> Self {
        self.with_label(Label::secondary(span).with_message(message))
    }

    pub fn add_label(&mut self, label: Label) {
        self.details.labels.push(label);
    }

    pub fn labels(&self) -> &[Label] {
        &self.details.labels
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.details.notes.push(Note::new(note));
        self
    }

    pub fn add_note(&mut self, note: impl Into<String>) {
        self.details.notes.push(Note::new(note));
    }

    pub fn notes(&self) -> &[Note] {
        &self.details.notes
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.details.helps.push(Help::new(help));
        self
    }

    pub fn add_help(&mut self, help: impl Into<String>) {
        self.details.helps.push(Help::new(help));
    }

    pub fn helps(&self) -> &[Help] {
        &self.details.helps
    }

    pub fn with_fix(mut self, fix: FixIt) -> Self {
        self.details.fixes.push(fix);
        self
    }

    pub fn add_fix(&mut self, fix: FixIt) {
        self.details.fixes.push(fix);
    }

    pub fn fixes(&self) -> &[FixIt] {
        &self.details.fixes
    }
}

#[derive(Default, Clone)]
pub struct Diagnostics {
    items: Vec<Diagnostic>,
    source: Option<PathBuf>,
    source_map: Option<Arc<SourceMap>>,
}

impl fmt::Debug for Diagnostics {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("Diagnostics")
            .field("items", &self.items)
            .finish()
    }
}

impl Diagnostics {
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            source: None,
            source_map: None,
        }
    }

    pub fn push(&mut self, mut diagnostic: Diagnostic) {
        if diagnostic.source.is_none() {
            diagnostic.source.clone_from(&self.source);
        }
        materialize_primary_label(self.source_map.as_deref(), &mut diagnostic);
        self.items.push(diagnostic);
    }

    pub fn extend(&mut self, diagnostics: impl IntoIterator<Item = Diagnostic>) {
        for diagnostic in diagnostics {
            self.push(diagnostic);
        }
    }

    pub fn append(&mut self, other: &mut Diagnostics) {
        if self.source_map.is_none()
            && let Some(source_map) = other.source_map.clone()
        {
            self.attach_source_map(source_map);
        }
        let appended_at = self.items.len();
        self.items.append(&mut other.items);
        if let Some(source_map) = self.source_map.as_deref() {
            for diagnostic in &mut self.items[appended_at..] {
                materialize_primary_label(Some(source_map), diagnostic);
            }
        }
    }

    pub fn set_source(&mut self, source: impl Into<PathBuf>) {
        self.source = Some(source.into());
    }

    pub fn clear_source(&mut self) {
        self.source = None;
    }

    pub fn set_source_for_missing(&mut self, source: impl Into<PathBuf>) {
        let source = source.into();
        for diagnostic in &mut self.items {
            if diagnostic.source.is_none() {
                diagnostic.source = Some(source.clone());
            }
            materialize_primary_label(self.source_map.as_deref(), diagnostic);
        }
    }

    pub fn attach_source_map(&mut self, source_map: Arc<SourceMap>) {
        for diagnostic in &mut self.items {
            materialize_primary_label(Some(&source_map), diagnostic);
        }
        self.source_map = Some(source_map);
    }

    pub fn with_source_map(mut self, source_map: Arc<SourceMap>) -> Self {
        self.attach_source_map(source_map);
        self
    }

    pub fn source_map(&self) -> Option<&SourceMap> {
        self.source_map.as_deref()
    }

    pub fn source_map_arc(&self) -> Option<&Arc<SourceMap>> {
        self.source_map.as_ref()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Diagnostic> {
        self.items.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Diagnostic> {
        self.items.iter_mut()
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn has_errors(&self) -> bool {
        self.items
            .iter()
            .any(|diagnostic| diagnostic.severity == Severity::Error)
    }
}

fn materialize_primary_label(source_map: Option<&SourceMap>, diagnostic: &mut Diagnostic) {
    if diagnostic
        .labels()
        .iter()
        .any(|label| label.severity() == LabelSeverity::Primary)
    {
        return;
    }
    let (Some(source_map), Some(source), Some(range)) =
        (source_map, diagnostic.source(), diagnostic.range())
    else {
        return;
    };
    let Some(source_id) = source_map.find(source) else {
        return;
    };
    let start = u32::from(range.start()) as usize;
    let end = u32::from(range.end()) as usize;
    let Ok(span) = source_map.span(source_id, start, end) else {
        return;
    };
    diagnostic.details.labels.insert(0, Label::primary(span));
}

impl IntoIterator for Diagnostics {
    type Item = Diagnostic;
    type IntoIter = std::vec::IntoIter<Diagnostic>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

impl<'a> IntoIterator for &'a Diagnostics {
    type Item = &'a Diagnostic;
    type IntoIter = std::slice::Iter<'a, Diagnostic>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.iter()
    }
}

impl<'a> IntoIterator for &'a mut Diagnostics {
    type Item = &'a mut Diagnostic;
    type IntoIter = std::slice::IterMut<'a, Diagnostic>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.iter_mut()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use text_size::{TextRange, TextSize};

    fn range(start: u32, end: u32) -> TextRange {
        TextRange::new(TextSize::from(start), TextSize::from(end))
    }

    #[test]
    fn attached_sources_materialize_legacy_ranges_as_primary_labels() {
        let mut diagnostics = Diagnostics::new();
        diagnostics.set_source("main.gom");
        diagnostics.push(
            Diagnostic::new(Stage::Typer, Severity::Error, "bad value").with_range(range(4, 9)),
        );
        assert!(diagnostics.iter().next().unwrap().labels().is_empty());

        let mut sources = SourceMap::new();
        let source = sources.add("main.gom", "let value\n");
        diagnostics.attach_source_map(Arc::new(sources));

        let diagnostic = diagnostics.iter().next().unwrap();
        assert_eq!(diagnostic.labels().len(), 1);
        assert_eq!(diagnostic.labels()[0].severity(), LabelSeverity::Primary);
        assert_eq!(diagnostic.labels()[0].span().source(), source);
        assert_eq!(diagnostic.labels()[0].span().range(), 4..9);
    }

    #[test]
    fn attached_sources_materialize_new_and_backfilled_diagnostics() {
        let mut sources = SourceMap::new();
        sources.add("main.gom", "abc\n");
        let mut diagnostics = Diagnostics::new().with_source_map(Arc::new(sources));
        diagnostics.set_source("main.gom");
        diagnostics
            .push(Diagnostic::new(Stage::Parser, Severity::Error, "first").with_range(range(0, 1)));
        diagnostics.clear_source();
        diagnostics.push(
            Diagnostic::new(Stage::Parser, Severity::Error, "second").with_range(range(1, 2)),
        );
        diagnostics.set_source_for_missing("main.gom");

        assert!(diagnostics.iter().all(|diagnostic| {
            diagnostic
                .labels()
                .iter()
                .any(|label| label.severity() == LabelSeverity::Primary)
        }));
    }

    #[test]
    fn attaching_sources_preserves_existing_primary_labels() {
        let mut sources = SourceMap::new();
        let source = sources.add("main.gom", "abc");
        let span = sources.span(source, 1, 2).unwrap();
        let mut diagnostics = Diagnostics::new();
        diagnostics.push(
            Diagnostic::new(Stage::Typer, Severity::Error, "bad value")
                .with_source("main.gom")
                .with_range(range(0, 1))
                .with_primary_label(span, "specific value"),
        );

        diagnostics.attach_source_map(Arc::new(sources));

        let labels = diagnostics.iter().next().unwrap().labels();
        assert_eq!(labels.len(), 1);
        assert_eq!(labels[0].span(), span);
        assert_eq!(labels[0].message(), Some("specific value"));
    }

    #[test]
    fn append_adopts_sources_and_materializes_appended_ranges() {
        let mut sources = SourceMap::new();
        sources.add("dependency.gom", "abc");
        let mut dependency = Diagnostics::new().with_source_map(Arc::new(sources));
        dependency.push(
            Diagnostic::new(Stage::Parser, Severity::Error, "bad dependency")
                .with_source("dependency.gom")
                .with_range(range(1, 2)),
        );
        let mut combined = Diagnostics::new();
        combined.set_source("main.gom");

        combined.append(&mut dependency);

        assert!(dependency.is_empty());
        assert!(combined.source_map().is_some());
        let diagnostic = combined.iter().next().unwrap();
        assert_eq!(diagnostic.source(), Some(Path::new("dependency.gom")));
        assert_eq!(diagnostic.labels().len(), 1);
        assert_eq!(diagnostic.labels()[0].span().range(), 1..2);
    }
}
