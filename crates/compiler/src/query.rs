mod completion;
mod context;
mod definition;
mod hir_index;
mod hover;
mod inlay;
mod signature;
mod symbol_index;
mod syntax;
mod typecheck;

use std::path::PathBuf;

use text_size::{TextRange, TextSize};

pub use completion::{colon_colon_completions, dot_completions, value_completions};
pub use definition::{goto_definition, goto_definition_locations};
pub use hover::hover_type;
pub use inlay::inlay_hints;
pub use signature::signature_help;
pub use typecheck::diagnostics_for_query;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DotCompletionKind {
    Field,
    Method,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DotCompletionItem {
    pub name: String,
    pub kind: DotCompletionKind,
    pub detail: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ColonColonCompletionKind {
    Package,
    Type,
    Value,
    Trait,
    Variant,
    Method,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ColonColonCompletionItem {
    pub name: String,
    pub kind: ColonColonCompletionKind,
    pub detail: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueCompletionKind {
    Variable,
    Package,
    Function,
    Keyword,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueCompletionItem {
    pub name: String,
    pub kind: ValueCompletionKind,
    pub detail: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignatureHelpItem {
    pub label: String,
    pub parameters: Vec<String>,
    pub active_parameter: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum InlayHintKind {
    Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InlayHintItem {
    pub offset: TextSize,
    pub label: String,
    pub kind: InlayHintKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefinitionLocation {
    pub path: PathBuf,
    pub range: TextRange,
}
