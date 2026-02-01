use std::path::Path;

use compiler::query::{
    self, ColonColonCompletionItem, ColonColonCompletionKind, DotCompletionItem, DotCompletionKind,
    ValueCompletionItem,
};
use tower_lsp::lsp_types::*;

use crate::Document;

pub fn get_diagnostics(path: &Path, src: &str, doc: &Document) -> Vec<Diagnostic> {
    let result = compiler::pipeline::pipeline::compile(path, src);

    match result {
        Ok(_) => Vec::new(),
        Err(err) => {
            let diags = err.diagnostics();
            diags
                .iter()
                .map(|d| {
                    let range = d
                        .range()
                        .and_then(|r| doc.range(r))
                        .unwrap_or_else(|| Range {
                            start: Position {
                                line: 0,
                                character: 0,
                            },
                            end: Position {
                                line: 0,
                                character: 0,
                            },
                        });

                    let severity = match d.severity() {
                        diagnostics::Severity::Error => DiagnosticSeverity::ERROR,
                        diagnostics::Severity::Warning => DiagnosticSeverity::WARNING,
                    };

                    Diagnostic {
                        range,
                        severity: Some(severity),
                        source: Some("goml".to_string()),
                        message: d.message().to_string(),
                        ..Default::default()
                    }
                })
                .collect()
        }
    }
}

pub fn hover(path: &Path, src: &str, position: Position) -> Option<Hover> {
    let type_info = query::hover_type(path, src, position.line, position.character).ok()?;

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!("```goml\n{}\n```", type_info),
        }),
        range: None,
    })
}

pub fn completion(path: &Path, src: &str, position: Position) -> Option<CompletionResponse> {
    let line = position.line;
    let col = position.character;

    if let Some(items) = query::dot_completions(path, src, line, col) {
        let completions = items.into_iter().map(dot_item_to_completion).collect();
        return Some(CompletionResponse::Array(completions));
    }

    if let Some(items) = query::colon_colon_completions(path, src, line, col) {
        let completions = items.into_iter().map(colon_item_to_completion).collect();
        return Some(CompletionResponse::Array(completions));
    }

    if let Some(items) = query::value_completions(path, src, line, col) {
        let completions = items.into_iter().map(value_item_to_completion).collect();
        return Some(CompletionResponse::Array(completions));
    }

    None
}

fn dot_item_to_completion(item: DotCompletionItem) -> CompletionItem {
    CompletionItem {
        label: item.name.clone(),
        kind: Some(match item.kind {
            DotCompletionKind::Field => CompletionItemKind::FIELD,
            DotCompletionKind::Method => CompletionItemKind::METHOD,
        }),
        detail: item.detail,
        ..Default::default()
    }
}

fn colon_item_to_completion(item: ColonColonCompletionItem) -> CompletionItem {
    CompletionItem {
        label: item.name.clone(),
        kind: Some(match item.kind {
            ColonColonCompletionKind::Type => CompletionItemKind::CLASS,
            ColonColonCompletionKind::Value => CompletionItemKind::VALUE,
            ColonColonCompletionKind::Trait => CompletionItemKind::INTERFACE,
            ColonColonCompletionKind::Variant => CompletionItemKind::ENUM_MEMBER,
            ColonColonCompletionKind::Method => CompletionItemKind::METHOD,
        }),
        detail: item.detail,
        ..Default::default()
    }
}

fn value_item_to_completion(item: ValueCompletionItem) -> CompletionItem {
    CompletionItem {
        label: item.name.clone(),
        kind: Some(CompletionItemKind::FUNCTION),
        detail: item.detail,
        ..Default::default()
    }
}

pub fn goto_definition(
    uri: &Url,
    path: &Path,
    src: &str,
    position: Position,
    doc: &Document,
) -> Option<GotoDefinitionResponse> {
    let def_range = query::goto_definition(path, src, position.line, position.character).ok()?;
    let lsp_range = doc.range(def_range)?;

    Some(GotoDefinitionResponse::Scalar(Location {
        uri: uri.clone(),
        range: lsp_range,
    }))
}
