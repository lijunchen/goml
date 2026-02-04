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
                    let range = d.range().and_then(|r| doc.range(r)).unwrap_or(Range {
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
    let locations =
        query::goto_definition_locations(path, src, position.line, position.character).ok()?;
    if locations.is_empty() {
        return None;
    }

    let mut lsp_locations = Vec::new();
    for loc in locations {
        let target_uri = if loc.path == path {
            uri.clone()
        } else {
            let Some(u) = Url::from_file_path(&loc.path).ok() else {
                continue;
            };
            u
        };
        let range = if loc.path == path {
            doc.range(loc.range)
        } else {
            let Some(target_src) = std::fs::read_to_string(&loc.path).ok() else {
                continue;
            };
            let target_doc = Document::new(target_src);
            target_doc.range(loc.range)
        };
        if let Some(range) = range {
            lsp_locations.push(Location {
                uri: target_uri,
                range,
            });
        }
    }

    lsp_locations.sort_by(|a, b| {
        a.uri
            .cmp(&b.uri)
            .then(a.range.start.line.cmp(&b.range.start.line))
            .then(a.range.start.character.cmp(&b.range.start.character))
    });
    lsp_locations.dedup_by(|a, b| a.uri == b.uri && a.range == b.range);

    match lsp_locations.len() {
        0 => None,
        1 => Some(GotoDefinitionResponse::Scalar(
            lsp_locations.into_iter().next()?,
        )),
        _ => Some(GotoDefinitionResponse::Array(lsp_locations)),
    }
}
