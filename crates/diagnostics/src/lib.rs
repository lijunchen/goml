use std::borrow::Cow;
use std::fmt;
use std::path::{Path, PathBuf};

use text_size::TextRange;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Severity {
    Error,
    Warning,
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
}

#[derive(Default, Clone)]
pub struct Diagnostics {
    items: Vec<Diagnostic>,
    source: Option<PathBuf>,
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
        }
    }

    pub fn push(&mut self, mut diagnostic: Diagnostic) {
        if diagnostic.source.is_none() {
            diagnostic.source.clone_from(&self.source);
        }
        self.items.push(diagnostic);
    }

    pub fn extend(&mut self, diagnostics: impl IntoIterator<Item = Diagnostic>) {
        for diagnostic in diagnostics {
            self.push(diagnostic);
        }
    }

    pub fn append(&mut self, other: &mut Diagnostics) {
        self.items.append(&mut other.items);
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
        }
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
