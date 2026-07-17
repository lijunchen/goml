mod completion;
mod context;
mod definition;
mod hir_index;
mod hover;
mod inlay;
mod signature;
mod symbol_index;
mod syntax;
mod testing;
mod typecheck;

use std::path::PathBuf;

use text_size::{TextRange, TextSize};

pub use completion::{
    colon_colon_completions, colon_colon_completions_with_overrides, dot_completions,
    dot_completions_with_overrides, value_completions, value_completions_with_overrides,
};
pub use definition::{
    goto_definition, goto_definition_locations, goto_definition_locations_with_overrides,
    goto_definition_with_overrides,
};
pub use hover::{hover_type, hover_type_with_analysis, hover_type_with_overrides};
pub use inlay::{inlay_hints, inlay_hints_with_overrides};
pub use signature::{signature_help, signature_help_with_overrides};
pub use testing::{TestItem, TestItemKind, test_items};
pub use typecheck::{Analysis, analyze, analyze_with_overrides};

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
