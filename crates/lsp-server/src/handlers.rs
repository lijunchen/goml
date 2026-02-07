use std::collections::HashSet;
use std::path::Path;

use compiler::query::{
    self, ColonColonCompletionItem, ColonColonCompletionKind, DotCompletionItem, DotCompletionKind,
    InlayHintItem, InlayHintKind as QueryInlayHintKind, SignatureHelpItem, ValueCompletionItem,
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
    let type_info = query::hover_type(path, src, position.line, position.character).ok();
    let diagnostics = diagnostics_for_hover(path, src, position);
    if type_info.is_none() && diagnostics.is_empty() {
        return None;
    }

    let mut sections = Vec::new();
    if let Some(type_info) = type_info {
        sections.push(format!("```goml\n{}\n```", type_info));
    }
    if !diagnostics.is_empty() {
        let lines = diagnostics
            .iter()
            .map(|(severity, message)| {
                let marker = match severity {
                    diagnostics::Severity::Error => "-",
                    diagnostics::Severity::Warning => "+",
                };
                format!("{} {}", marker, message)
            })
            .collect::<Vec<_>>()
            .join("\n");
        sections.push(format!("**Diagnostics**\n```diff\n{}\n```", lines));
    }

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: sections.join("\n\n"),
        }),
        range: None,
    })
}

fn diagnostics_for_hover(
    path: &Path,
    src: &str,
    position: Position,
) -> Vec<(diagnostics::Severity, String)> {
    let doc = Document::new(src.to_string());
    let mut messages = Vec::new();
    let mut seen: HashSet<(diagnostics::Severity, String)> = HashSet::new();
    let result = compiler::pipeline::pipeline::compile(path, src);
    let diagnostics = match result {
        Ok(_) => return messages,
        Err(err) => err.into_diagnostics(),
    };
    for diagnostic in diagnostics.iter() {
        let Some(text_range) = diagnostic.range() else {
            continue;
        };
        let Some(range) = doc.range(text_range) else {
            continue;
        };
        if !position_in_range(position, range) {
            continue;
        }
        let item = (diagnostic.severity(), diagnostic.message().to_string());
        if seen.insert(item.clone()) {
            messages.push(item);
        }
    }
    messages
}

fn position_in_range(position: Position, range: Range) -> bool {
    let at_or_after_start = position.line > range.start.line
        || (position.line == range.start.line && position.character >= range.start.character);
    let strictly_before_end = position.line < range.end.line
        || (position.line == range.end.line && position.character < range.end.character);
    let zero_width_match = range.start == range.end
        && position.line == range.start.line
        && position.character == range.start.character;
    at_or_after_start && (strictly_before_end || zero_width_match)
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

pub fn signature_help(path: &Path, src: &str, position: Position) -> Option<SignatureHelp> {
    let item = query::signature_help(path, src, position.line, position.character)?;
    Some(signature_item_to_lsp(item))
}

pub fn inlay_hints(path: &Path, src: &str, range: Range, doc: &Document) -> Option<Vec<InlayHint>> {
    let items = query::inlay_hints(path, src)?;
    let hints = items
        .into_iter()
        .filter_map(|item| inlay_item_to_lsp(item, range, doc))
        .collect::<Vec<_>>();
    Some(hints)
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
        kind: Some(match item.kind {
            query::ValueCompletionKind::Function => CompletionItemKind::FUNCTION,
            query::ValueCompletionKind::Keyword => CompletionItemKind::KEYWORD,
        }),
        detail: item.detail,
        ..Default::default()
    }
}

fn signature_item_to_lsp(item: SignatureHelpItem) -> SignatureHelp {
    let parameters = item
        .parameters
        .into_iter()
        .map(|parameter| ParameterInformation {
            label: ParameterLabel::Simple(parameter),
            documentation: None,
        })
        .collect::<Vec<_>>();

    SignatureHelp {
        signatures: vec![SignatureInformation {
            label: item.label,
            documentation: None,
            parameters: Some(parameters),
            active_parameter: Some(item.active_parameter),
        }],
        active_signature: Some(0),
        active_parameter: Some(item.active_parameter),
    }
}

fn inlay_item_to_lsp(item: InlayHintItem, range: Range, doc: &Document) -> Option<InlayHint> {
    let offset = adjust_inlay_offset(item.offset, doc);
    let position = doc.position(offset)?;
    if !position_in_range(position, range) {
        return None;
    }

    Some(InlayHint {
        position,
        label: InlayHintLabel::String(item.label),
        kind: Some(match item.kind {
            QueryInlayHintKind::Type => tower_lsp::lsp_types::InlayHintKind::TYPE,
        }),
        text_edits: None,
        tooltip: None,
        padding_left: None,
        padding_right: Some(true),
        data: None,
    })
}

fn adjust_inlay_offset(offset: text_size::TextSize, doc: &Document) -> text_size::TextSize {
    let bytes = doc.content.as_bytes();
    let mut idx = u32::from(offset) as usize;
    if idx > bytes.len() {
        return offset;
    }

    while idx > 0 {
        match bytes[idx - 1] {
            b' ' | b'\t' => idx -= 1,
            _ => break,
        }
    }

    text_size::TextSize::from(idx as u32)
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
