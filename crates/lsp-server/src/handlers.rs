use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use compiler::query::{
    self, ColonColonCompletionItem, ColonColonCompletionKind, DotCompletionItem, DotCompletionKind,
    InlayHintItem, InlayHintKind as QueryInlayHintKind, SignatureHelpItem, ValueCompletionItem,
};
use diagnostics::{FixApplicability, LabelSeverity, SourceMap, Span};
use serde::{Deserialize, Serialize};
use tower_lsp::lsp_types::*;

use crate::Document;

pub fn get_diagnostics(path: &Path, src: &str, doc: &Document) -> Vec<Diagnostic> {
    get_diagnostics_with_overrides(path, src, doc, &HashMap::new())
}

pub fn get_diagnostics_with_overrides(
    path: &Path,
    src: &str,
    doc: &Document,
    source_overrides: &HashMap<PathBuf, String>,
) -> Vec<Diagnostic> {
    let result = compiler::pipeline::pipeline::compile_for_analysis_with_overrides(
        path,
        src,
        source_overrides,
    );

    match result {
        Ok(_) => Vec::new(),
        Err(err) => {
            let mut diagnostics = err.into_diagnostics();
            let sources = diagnostics.source_map_arc().cloned().unwrap_or_else(|| {
                Arc::new(source_map_for_diagnostics(
                    path,
                    src,
                    source_overrides,
                    &diagnostics,
                ))
            });
            diagnostics.attach_source_map(Arc::clone(&sources));
            diagnostics
                .iter()
                .filter_map(|diagnostic| diagnostic_to_lsp(path, doc, &sources, diagnostic))
                .collect()
        }
    }
}

fn source_map_for_diagnostics(
    path: &Path,
    src: &str,
    source_overrides: &HashMap<PathBuf, String>,
    diagnostics: &diagnostics::Diagnostics,
) -> SourceMap {
    let mut source_texts = BTreeMap::new();
    for (source_path, source) in source_overrides {
        source_texts.insert(source_path.clone(), source.clone());
    }
    for diagnostic in diagnostics {
        let Some(source_path) = diagnostic.source() else {
            continue;
        };
        if source_texts.contains_key(source_path) {
            continue;
        }
        if let Ok(source) = std::fs::read_to_string(source_path) {
            source_texts.insert(source_path.to_path_buf(), source);
        }
    }
    source_texts.insert(path.to_path_buf(), src.to_string());

    let mut sources = SourceMap::new();
    for (source_path, source) in source_texts {
        sources.add(source_path, source);
    }
    sources
}

pub(crate) fn diagnostic_to_lsp(
    path: &Path,
    doc: &Document,
    sources: &SourceMap,
    diagnostic: &diagnostics::Diagnostic,
) -> Option<Diagnostic> {
    let anchor = diagnostic
        .labels()
        .iter()
        .enumerate()
        .filter(|(_, label)| label.severity() == LabelSeverity::Primary)
        .find_map(|(index, label)| {
            let file = sources.get(label.span().source())?;
            if file.path() != path {
                return None;
            }
            span_range(label.span(), doc).map(|range| (index, range))
        });
    let (anchor_index, range) = match anchor {
        Some(anchor) => anchor,
        None if diagnostic.labels().is_empty()
            && diagnostic.range().is_none()
            && diagnostic.source().is_none_or(|source| source == path) =>
        {
            (usize::MAX, Range::default())
        }
        None => return None,
    };

    let mut message = diagnostic.message().to_string();
    if let Some(label_message) = diagnostic
        .labels()
        .get(anchor_index)
        .and_then(|label| label.message())
    {
        message.push('\n');
        message.push_str(label_message);
    }
    for note in diagnostic.notes() {
        message.push_str("\nnote: ");
        message.push_str(note.text());
    }
    for help in diagnostic.helps() {
        message.push_str("\nhelp: ");
        message.push_str(help.text());
    }

    let related_information = diagnostic
        .labels()
        .iter()
        .enumerate()
        .filter(|(index, _)| *index != anchor_index)
        .filter_map(|(_, label)| related_information(sources, label))
        .collect::<Vec<_>>();
    let fixes = diagnostic
        .fixes()
        .iter()
        .filter_map(|fix| fix_data(sources, fix))
        .collect::<Vec<_>>();
    let data = if fixes.is_empty() {
        None
    } else {
        serde_json::to_value(DiagnosticData { fixes }).ok()
    };

    Some(Diagnostic {
        range,
        severity: Some(match diagnostic.severity() {
            diagnostics::Severity::Error => DiagnosticSeverity::ERROR,
            diagnostics::Severity::Warning => DiagnosticSeverity::WARNING,
        }),
        source: Some("goml".to_string()),
        message,
        related_information: (!related_information.is_empty()).then_some(related_information),
        data,
        ..Default::default()
    })
}

fn span_range(span: Span, doc: &Document) -> Option<Range> {
    let start = u32::try_from(span.start()).ok()?;
    let end = u32::try_from(span.end()).ok()?;
    doc.range(text_size::TextRange::new(start.into(), end.into()))
}

fn related_information(
    sources: &SourceMap,
    label: &diagnostics::Label,
) -> Option<DiagnosticRelatedInformation> {
    let file = sources.get(label.span().source())?;
    let uri = Url::from_file_path(file.path()).ok()?;
    let doc = Document::new(file.text().to_string());
    let range = span_range(label.span(), &doc)?;
    let message = label.message().unwrap_or(match label.severity() {
        LabelSeverity::Primary => "related primary location",
        LabelSeverity::Secondary => "related location",
    });
    Some(DiagnosticRelatedInformation {
        location: Location { uri, range },
        message: message.to_string(),
    })
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct DiagnosticData {
    fixes: Vec<FixData>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct FixData {
    uri: Url,
    range: Range,
    replacement: String,
    title: String,
    preferred: bool,
}

fn fix_data(sources: &SourceMap, fix: &diagnostics::FixIt) -> Option<FixData> {
    let file = sources.get(fix.span().source())?;
    let uri = Url::from_file_path(file.path()).ok()?;
    let doc = Document::new(file.text().to_string());
    Some(FixData {
        uri,
        range: span_range(fix.span(), &doc)?,
        replacement: fix.replacement().to_string(),
        title: fix.message().unwrap_or("Apply suggested fix").to_string(),
        preferred: fix.applicability() == FixApplicability::MachineApplicable,
    })
}

pub fn hover(path: &Path, src: &str, position: Position) -> Option<Hover> {
    hover_with_overrides(path, src, position, &HashMap::new())
}

pub fn hover_with_overrides(
    path: &Path,
    src: &str,
    position: Position,
    source_overrides: &HashMap<PathBuf, String>,
) -> Option<Hover> {
    let doc = Document::new(src.to_string());
    let (line, character) = doc.utf8_position(position)?;
    let type_info =
        query::hover_type_with_overrides(path, src, line, character, source_overrides).ok();
    let diagnostics = diagnostics_for_hover(path, src, position, source_overrides);
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
    source_overrides: &HashMap<PathBuf, String>,
) -> Vec<(diagnostics::Severity, String)> {
    let doc = Document::new(src.to_string());
    let mut messages = Vec::new();
    let mut seen: HashSet<(diagnostics::Severity, String)> = HashSet::new();
    for diagnostic in get_diagnostics_with_overrides(path, src, &doc, source_overrides) {
        if !position_in_range(position, diagnostic.range) {
            continue;
        }
        let severity = match diagnostic.severity {
            Some(DiagnosticSeverity::WARNING) => diagnostics::Severity::Warning,
            _ => diagnostics::Severity::Error,
        };
        let item = (severity, diagnostic.message);
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
    completion_with_overrides(path, src, position, &HashMap::new())
}

pub fn completion_with_overrides(
    path: &Path,
    src: &str,
    position: Position,
    source_overrides: &HashMap<PathBuf, String>,
) -> Option<CompletionResponse> {
    let doc = Document::new(src.to_string());
    let (line, col) = doc.utf8_position(position)?;

    if let Some(items) =
        query::dot_completions_with_overrides(path, src, line, col, source_overrides)
    {
        let completions = items.into_iter().map(dot_item_to_completion).collect();
        return Some(CompletionResponse::Array(completions));
    }

    if let Some(items) =
        query::colon_colon_completions_with_overrides(path, src, line, col, source_overrides)
    {
        let completions = items.into_iter().map(colon_item_to_completion).collect();
        return Some(CompletionResponse::Array(completions));
    }

    if let Some(items) =
        query::value_completions_with_overrides(path, src, line, col, source_overrides)
    {
        let completions = items.into_iter().map(value_item_to_completion).collect();
        return Some(CompletionResponse::Array(completions));
    }

    None
}

pub fn signature_help(path: &Path, src: &str, position: Position) -> Option<SignatureHelp> {
    signature_help_with_overrides(path, src, position, &HashMap::new())
}

pub fn signature_help_with_overrides(
    path: &Path,
    src: &str,
    position: Position,
    source_overrides: &HashMap<PathBuf, String>,
) -> Option<SignatureHelp> {
    let doc = Document::new(src.to_string());
    let (line, character) = doc.utf8_position(position)?;
    let item = query::signature_help_with_overrides(path, src, line, character, source_overrides)?;
    Some(signature_item_to_lsp(item))
}

pub fn inlay_hints(path: &Path, src: &str, range: Range, doc: &Document) -> Option<Vec<InlayHint>> {
    inlay_hints_with_overrides(path, src, range, doc, &HashMap::new())
}

pub fn inlay_hints_with_overrides(
    path: &Path,
    src: &str,
    range: Range,
    doc: &Document,
    source_overrides: &HashMap<PathBuf, String>,
) -> Option<Vec<InlayHint>> {
    let items = query::inlay_hints_with_overrides(path, src, source_overrides)?;
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
            ColonColonCompletionKind::Package => CompletionItemKind::MODULE,
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
            query::ValueCompletionKind::Variable => CompletionItemKind::VARIABLE,
            query::ValueCompletionKind::Package => CompletionItemKind::MODULE,
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
    goto_definition_with_overrides(uri, path, src, position, doc, &HashMap::new())
}

pub fn goto_definition_with_overrides(
    uri: &Url,
    path: &Path,
    src: &str,
    position: Position,
    doc: &Document,
    source_overrides: &HashMap<PathBuf, String>,
) -> Option<GotoDefinitionResponse> {
    let (line, character) = doc.utf8_position(position)?;
    let locations = query::goto_definition_locations_with_overrides(
        path,
        src,
        line,
        character,
        source_overrides,
    )
    .ok()?;
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
            let target_src = source_overrides
                .get(&loc.path)
                .cloned()
                .or_else(|| std::fs::read_to_string(&loc.path).ok());
            let Some(target_src) = target_src else {
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

pub fn code_lenses(uri: &Url, path: &Path, src: &str, doc: &Document) -> Vec<CodeLens> {
    query::test_items(path, src)
        .into_iter()
        .filter_map(|item| {
            let range = doc.range(item.range)?;
            let kind = match item.kind {
                query::TestItemKind::Internal => "internal",
                query::TestItemKind::External => "external",
            };
            Some(CodeLens {
                range,
                command: Some(Command {
                    title: "Run Test".to_string(),
                    command: "goml.runTest".to_string(),
                    arguments: Some(vec![
                        serde_json::json!(uri.as_str()),
                        serde_json::json!(item.name),
                        serde_json::json!(kind),
                    ]),
                }),
                data: None,
            })
        })
        .collect()
}

pub fn code_actions(context: &CodeActionContext) -> Option<CodeActionResponse> {
    let mut actions = Vec::new();
    for diagnostic in &context.diagnostics {
        let Some(data) = diagnostic.data.clone() else {
            continue;
        };
        let Ok(data) = serde_json::from_value::<DiagnosticData>(data) else {
            continue;
        };
        for fix in data.fixes {
            let changes = HashMap::from([(
                fix.uri,
                vec![TextEdit {
                    range: fix.range,
                    new_text: fix.replacement,
                }],
            )]);
            actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                title: fix.title,
                kind: Some(CodeActionKind::QUICKFIX),
                diagnostics: Some(vec![diagnostic.clone()]),
                edit: Some(WorkspaceEdit {
                    changes: Some(changes),
                    ..Default::default()
                }),
                is_preferred: Some(fix.preferred),
                ..Default::default()
            }));
        }
    }
    (!actions.is_empty()).then_some(actions)
}
