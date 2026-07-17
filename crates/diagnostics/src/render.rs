use std::cmp::Ordering;

use crate::{
    Diagnostic, Diagnostics, FixApplicability, FixIt, Label, LabelSeverity, Severity, SourceError,
    SourceMap, Span,
};

#[derive(Debug, Default, Clone, Copy)]
pub struct TextRenderer;

impl TextRenderer {
    pub const fn new() -> Self {
        Self
    }

    pub fn render<'a>(
        &self,
        sources: &SourceMap,
        diagnostics: impl IntoIterator<Item = &'a Diagnostic>,
    ) -> String {
        let mut diagnostics = diagnostics.into_iter().enumerate().collect::<Vec<_>>();
        diagnostics.sort_by(|(left_index, left), (right_index, right)| {
            compare_diagnostics(sources, left, right).then_with(|| left_index.cmp(right_index))
        });
        diagnostics
            .into_iter()
            .map(|(_, diagnostic)| self.render_diagnostic(sources, diagnostic))
            .collect::<Vec<_>>()
            .join("\n\n")
    }

    pub fn render_all(&self, sources: &SourceMap, diagnostics: &Diagnostics) -> String {
        self.render(sources, diagnostics.iter())
    }

    pub fn render_one(&self, sources: &SourceMap, diagnostic: &Diagnostic) -> String {
        self.render_diagnostic(sources, diagnostic)
    }

    fn render_diagnostic(&self, sources: &SourceMap, diagnostic: &Diagnostic) -> String {
        let mut lines = vec![format!(
            "{}[{}]: {}",
            severity_name(diagnostic.severity()),
            diagnostic.stage().as_str(),
            diagnostic.message()
        )];
        let mut labels = diagnostic.labels().iter().enumerate().collect::<Vec<_>>();
        labels.sort_by(|(left_index, left), (right_index, right)| {
            compare_labels(sources, left, right).then_with(|| left_index.cmp(right_index))
        });
        for (_, label) in labels {
            lines.extend(render_label(sources, label));
        }
        if diagnostic.labels().is_empty() {
            render_legacy_location(sources, diagnostic, &mut lines);
        }
        for note in diagnostic.notes() {
            lines.push(format!("note: {}", note.text()));
        }
        for help in diagnostic.helps() {
            lines.push(format!("help: {}", help.text()));
        }
        let mut fixes = diagnostic.fixes().iter().enumerate().collect::<Vec<_>>();
        fixes.sort_by(|(left_index, left), (right_index, right)| {
            compare_fixes(sources, left, right).then_with(|| left_index.cmp(right_index))
        });
        for (_, fix) in fixes {
            lines.push(render_fix(sources, fix));
        }
        lines.join("\n")
    }
}

fn compare_diagnostics(sources: &SourceMap, left: &Diagnostic, right: &Diagnostic) -> Ordering {
    severity_rank(left.severity())
        .cmp(&severity_rank(right.severity()))
        .then_with(|| diagnostic_location(sources, left).cmp(&diagnostic_location(sources, right)))
        .then_with(|| left.stage().as_str().cmp(right.stage().as_str()))
        .then_with(|| left.message().cmp(right.message()))
}

fn compare_labels(sources: &SourceMap, left: &Label, right: &Label) -> Ordering {
    label_rank(left.severity())
        .cmp(&label_rank(right.severity()))
        .then_with(|| span_key(sources, left.span()).cmp(&span_key(sources, right.span())))
        .then_with(|| left.message().cmp(&right.message()))
}

fn compare_fixes(sources: &SourceMap, left: &FixIt, right: &FixIt) -> Ordering {
    span_key(sources, left.span())
        .cmp(&span_key(sources, right.span()))
        .then_with(|| left.replacement().cmp(right.replacement()))
        .then_with(|| left.message().cmp(&right.message()))
        .then_with(|| left.applicability().cmp(&right.applicability()))
}

fn diagnostic_location(sources: &SourceMap, diagnostic: &Diagnostic) -> (String, usize, usize) {
    diagnostic
        .labels()
        .iter()
        .filter(|label| label.severity() == LabelSeverity::Primary)
        .map(|label| span_key(sources, label.span()))
        .min()
        .or_else(|| {
            diagnostic
                .labels()
                .iter()
                .map(|label| span_key(sources, label.span()))
                .min()
        })
        .or_else(|| {
            diagnostic.source().map(|source| {
                let start = diagnostic
                    .range()
                    .map(|range| u32::from(range.start()) as usize)
                    .unwrap_or_default();
                let end = diagnostic
                    .range()
                    .map(|range| u32::from(range.end()) as usize)
                    .unwrap_or(start);
                (source.to_string_lossy().into_owned(), start, end)
            })
        })
        .unwrap_or_else(|| (String::new(), usize::MAX, usize::MAX))
}

fn span_key(sources: &SourceMap, span: Span) -> (String, usize, usize) {
    let path = sources
        .get(span.source())
        .map(|file| file.path().to_string_lossy().into_owned())
        .unwrap_or_else(|| format!("~source-{}", span.source().index()));
    (path, span.start(), span.end())
}

fn render_label(sources: &SourceMap, label: &Label) -> Vec<String> {
    let prefix = match label.severity() {
        LabelSeverity::Primary => "primary",
        LabelSeverity::Secondary => "secondary",
    };
    match span_details(sources, label.span()) {
        Ok(details) => {
            let mut first = format!("{prefix}: {}", details.location);
            if let Some(message) = label.message() {
                first.push_str(": ");
                first.push_str(message);
            }
            vec![first, details.source_line, details.marker_line]
        }
        Err(error) => {
            let mut line = format!("{prefix}: <invalid: {error}>");
            if let Some(message) = label.message() {
                line.push_str(": ");
                line.push_str(message);
            }
            vec![line]
        }
    }
}

fn render_fix(sources: &SourceMap, fix: &FixIt) -> String {
    let location = match span_location(sources, fix.span()) {
        Ok(location) => location,
        Err(error) => format!("<invalid: {error}>"),
    };
    let mut line = format!(
        "fix: {location} => {:?} [{}]",
        fix.replacement(),
        applicability_name(fix.applicability())
    );
    if let Some(message) = fix.message() {
        line.push_str(": ");
        line.push_str(message);
    }
    line
}

fn render_legacy_location(sources: &SourceMap, diagnostic: &Diagnostic, lines: &mut Vec<String>) {
    let Some(source) = diagnostic.source() else {
        return;
    };
    let Some(range) = diagnostic.range() else {
        lines.push(format!("at: {}", source.display()));
        return;
    };
    let start = u32::from(range.start()) as usize;
    let end = u32::from(range.end()) as usize;
    if let Some(source_id) = sources.find(source) {
        match Span::new(source_id, start, end).and_then(|span| span_details(sources, span)) {
            Ok(details) => {
                lines.push(format!("at: {}", details.location));
                lines.push(details.source_line);
                lines.push(details.marker_line);
            }
            Err(error) => lines.push(format!("at: <invalid: {error}>")),
        }
    } else {
        lines.push(format!("at: {}:{start}..{end}", source.display()));
    }
}

struct SpanDetails {
    location: String,
    source_line: String,
    marker_line: String,
}

fn span_details(sources: &SourceMap, span: Span) -> Result<SpanDetails, SourceError> {
    sources.validate_span(span)?;
    let file = sources.file(span.source())?;
    let start = file.position(span.start())?;
    let line_text = file.line_text(start.line)?;
    let line_range = file.line_range(start.line)?;
    let highlight_start = span.start().clamp(line_range.start, line_range.end);
    let highlight_end = span.end().clamp(highlight_start, line_range.end);
    let prefix = &file.text()[line_range.start..highlight_start];
    let highlighted = &file.text()[highlight_start..highlight_end];
    let indentation = visual_width(prefix);
    let marker_width = visual_width(highlighted).max(1);
    let gutter = (start.line + 1).to_string().len();
    Ok(SpanDetails {
        location: span_location(sources, span)?,
        source_line: format!("{:>gutter$} | {}", start.line + 1, expand_tabs(line_text)),
        marker_line: format!(
            "{} | {}{}",
            " ".repeat(gutter),
            " ".repeat(indentation),
            "^".repeat(marker_width)
        ),
    })
}

fn span_location(sources: &SourceMap, span: Span) -> Result<String, SourceError> {
    sources.validate_span(span)?;
    let file = sources.file(span.source())?;
    let start = file.position(span.start())?;
    let end = file.position(span.end())?;
    let path = file.path().display();
    if span.is_empty() {
        Ok(format!("{path}:{}:{}", start.line + 1, start.column + 1))
    } else {
        Ok(format!(
            "{path}:{}:{}-{}:{}",
            start.line + 1,
            start.column + 1,
            end.line + 1,
            end.column + 1
        ))
    }
}

fn visual_width(text: &str) -> usize {
    text.chars().fold(0, |width, character| {
        if character == '\t' {
            width + (4 - width % 4)
        } else {
            width + 1
        }
    })
}

fn expand_tabs(text: &str) -> String {
    let mut expanded = String::new();
    let mut width = 0;
    for character in text.chars() {
        if character == '\t' {
            let spaces = 4 - width % 4;
            expanded.push_str(&" ".repeat(spaces));
            width += spaces;
        } else {
            expanded.push(character);
            width += 1;
        }
    }
    expanded
}

const fn severity_rank(severity: Severity) -> u8 {
    match severity {
        Severity::Error => 0,
        Severity::Warning => 1,
    }
}

const fn label_rank(severity: LabelSeverity) -> u8 {
    match severity {
        LabelSeverity::Primary => 0,
        LabelSeverity::Secondary => 1,
    }
}

const fn severity_name(severity: Severity) -> &'static str {
    match severity {
        Severity::Error => "error",
        Severity::Warning => "warning",
    }
}

const fn applicability_name(applicability: FixApplicability) -> &'static str {
    match applicability {
        FixApplicability::MachineApplicable => "machine-applicable",
        FixApplicability::MaybeIncorrect => "maybe-incorrect",
        FixApplicability::HasPlaceholders => "has-placeholders",
        FixApplicability::Unspecified => "unspecified",
    }
}

#[cfg(test)]
mod tests {
    use text_size::{TextRange, TextSize};

    use super::*;
    use crate::{FixIt, Label, Stage};

    #[test]
    fn renders_primary_secondary_notes_help_and_fix() {
        let mut sources = SourceMap::new();
        let source = sources.add("main.gom", "let value = old\n");
        let primary = sources.span(source, 12, 15).unwrap();
        let secondary = sources.span(source, 4, 9).unwrap();
        let diagnostic = Diagnostic::new(Stage::Typer, Severity::Error, "type mismatch")
            .with_primary_label(primary, "has the wrong type")
            .with_secondary_label(secondary, "declared here")
            .with_note("expected int32")
            .with_help("replace the expression")
            .with_fix(
                FixIt::new(primary, "0")
                    .with_message("use an integer")
                    .with_applicability(FixApplicability::MachineApplicable),
            );
        assert_eq!(
            TextRenderer::new().render_one(&sources, &diagnostic),
            "error[typer]: type mismatch\nprimary: main.gom:1:13-1:16: has the wrong type\n1 | let value = old\n  |             ^^^\nsecondary: main.gom:1:5-1:10: declared here\n1 | let value = old\n  |     ^^^^^\nnote: expected int32\nhelp: replace the expression\nfix: main.gom:1:13-1:16 => \"0\" [machine-applicable]: use an integer"
        );
    }

    #[test]
    fn unicode_locations_are_utf8_columns_and_markers_are_scalar_aligned() {
        let mut sources = SourceMap::new();
        let source = sources.add("unicode.gom", "a中🙂z");
        let span = sources.span(source, 8, 9).unwrap();
        let diagnostic = Diagnostic::new(Stage::Parser, Severity::Error, "unexpected name")
            .with_primary_label(span, "starts at UTF-8 byte 8");
        assert_eq!(
            TextRenderer::new().render_one(&sources, &diagnostic),
            "error[parser]: unexpected name\nprimary: unicode.gom:1:9-1:10: starts at UTF-8 byte 8\n1 | a中🙂z\n  |    ^"
        );
    }

    #[test]
    fn crlf_is_not_included_in_source_lines() {
        let mut sources = SourceMap::new();
        let source = sources.add("windows.gom", "first\r\nsecond\r\n");
        let span = sources.span(source, 7, 13).unwrap();
        let diagnostic = Diagnostic::new(Stage::Parser, Severity::Warning, "second line")
            .with_primary_label(span, "content");
        assert_eq!(
            TextRenderer::new().render_one(&sources, &diagnostic),
            "warning[parser]: second line\nprimary: windows.gom:2:1-2:7: content\n2 | second\n  | ^^^^^^"
        );
    }

    #[test]
    fn zero_length_span_at_empty_eof_is_rendered() {
        let mut sources = SourceMap::new();
        let source = sources.add("empty.gom", "");
        let diagnostic = Diagnostic::new(Stage::Parser, Severity::Error, "expected item")
            .with_label(Label::primary(Span::at(source, 0)));
        assert_eq!(
            TextRenderer::new().render_one(&sources, &diagnostic),
            "error[parser]: expected item\nprimary: empty.gom:1:1\n1 | \n  | ^"
        );
    }

    #[test]
    fn invalid_labels_and_fixes_are_rendered_without_panicking() {
        let sources = SourceMap::new();
        let span = Span::new(crate::SourceId::new(99), 0, 4).unwrap();
        let diagnostic = Diagnostic::new(Stage::Typer, Severity::Error, "bad location")
            .with_primary_label(span, "missing source")
            .with_fix(FixIt::new(span, "replacement"));
        assert_eq!(
            TextRenderer::new().render_one(&sources, &diagnostic),
            "error[typer]: bad location\nprimary: <invalid: unknown source 99>: missing source\nfix: <invalid: unknown source 99> => \"replacement\" [unspecified]"
        );
    }

    #[test]
    fn out_of_bounds_spans_are_rendered_without_panicking() {
        let mut sources = SourceMap::new();
        let source = sources.add("short.gom", "abc");
        let span = Span::new(source, 0, 8).unwrap();
        let diagnostic = Diagnostic::new(Stage::Parser, Severity::Error, "bad range")
            .with_primary_label(span, "too long");
        assert_eq!(
            TextRenderer::new().render_one(&sources, &diagnostic),
            "error[parser]: bad range\nprimary: <invalid: byte offset 8 is outside source 0 with length 3>: too long"
        );
    }

    #[test]
    fn labels_are_sorted_by_kind_path_and_byte_range() {
        let mut sources = SourceMap::new();
        let second_file = sources.add("b.gom", "bbbb");
        let first_file = sources.add("a.gom", "aaaa");
        let diagnostic = Diagnostic::new(Stage::Typer, Severity::Error, "ordered")
            .with_secondary_label(sources.span(first_file, 0, 1).unwrap(), "secondary a")
            .with_primary_label(sources.span(second_file, 2, 3).unwrap(), "primary b")
            .with_primary_label(sources.span(first_file, 1, 2).unwrap(), "primary a")
            .with_secondary_label(sources.span(second_file, 0, 1).unwrap(), "secondary b");
        let rendered = TextRenderer::new().render_one(&sources, &diagnostic);
        let headings = rendered
            .lines()
            .filter(|line| line.starts_with("primary:") || line.starts_with("secondary:"))
            .collect::<Vec<_>>();
        assert_eq!(
            headings,
            vec![
                "primary: a.gom:1:2-1:3: primary a",
                "primary: b.gom:1:3-1:4: primary b",
                "secondary: a.gom:1:1-1:2: secondary a",
                "secondary: b.gom:1:1-1:2: secondary b"
            ]
        );
    }

    #[test]
    fn diagnostics_are_sorted_by_severity_location_and_message() {
        let mut sources = SourceMap::new();
        let second_file = sources.add("b.gom", "b");
        let first_file = sources.add("a.gom", "a");
        let warning = Diagnostic::new(Stage::Parser, Severity::Warning, "warning")
            .with_primary_label(sources.span(first_file, 0, 1).unwrap(), "a");
        let later_error = Diagnostic::new(Stage::Parser, Severity::Error, "later")
            .with_primary_label(sources.span(second_file, 0, 1).unwrap(), "b");
        let first_error = Diagnostic::new(Stage::Parser, Severity::Error, "first")
            .with_primary_label(sources.span(first_file, 0, 1).unwrap(), "a");
        let rendered = TextRenderer::new().render(&sources, [&warning, &later_error, &first_error]);
        let headings = rendered
            .lines()
            .filter(|line| line.starts_with("error") || line.starts_with("warning"))
            .collect::<Vec<_>>();
        assert_eq!(
            headings,
            vec![
                "error[parser]: first",
                "error[parser]: later",
                "warning[parser]: warning"
            ]
        );
    }

    #[test]
    fn diagnostic_sorting_prefers_primary_locations() {
        let mut sources = SourceMap::new();
        let first_file = sources.add("a.gom", "a");
        let second_file = sources.add("b.gom", "b");
        let first = Diagnostic::new(Stage::Parser, Severity::Error, "first")
            .with_primary_label(sources.span(first_file, 0, 1).unwrap(), "primary");
        let second = Diagnostic::new(Stage::Parser, Severity::Error, "second")
            .with_secondary_label(sources.span(first_file, 0, 1).unwrap(), "secondary")
            .with_primary_label(sources.span(second_file, 0, 1).unwrap(), "primary");
        let rendered = TextRenderer::new().render(&sources, [&second, &first]);
        let headings = rendered
            .lines()
            .filter(|line| line.starts_with("error"))
            .collect::<Vec<_>>();
        assert_eq!(
            headings,
            vec!["error[parser]: first", "error[parser]: second"]
        );
    }

    #[test]
    fn equal_sort_keys_preserve_input_order() {
        let sources = SourceMap::new();
        let first = Diagnostic::new(Stage::Parser, Severity::Error, "same").with_note("first");
        let second = Diagnostic::new(Stage::Parser, Severity::Error, "same").with_note("second");
        let rendered = TextRenderer::new().render(&sources, [&second, &first]);
        assert!(rendered.find("note: second").unwrap() < rendered.find("note: first").unwrap());
    }

    #[test]
    fn fixes_are_sorted_across_multiple_files() {
        let mut sources = SourceMap::new();
        let second_file = sources.add("z.gom", "z");
        let first_file = sources.add("a.gom", "a");
        let diagnostic = Diagnostic::new(Stage::Typer, Severity::Error, "fixes")
            .with_fix(FixIt::new(sources.span(second_file, 0, 1).unwrap(), "last"))
            .with_fix(
                FixIt::new(sources.span(first_file, 0, 1).unwrap(), "first")
                    .with_applicability(FixApplicability::MaybeIncorrect),
            );
        let rendered = TextRenderer::new().render_one(&sources, &diagnostic);
        let fixes = rendered
            .lines()
            .filter(|line| line.starts_with("fix:"))
            .collect::<Vec<_>>();
        assert_eq!(
            fixes,
            vec![
                "fix: a.gom:1:1-1:2 => \"first\" [maybe-incorrect]",
                "fix: z.gom:1:1-1:2 => \"last\" [unspecified]"
            ]
        );
    }

    #[test]
    fn legacy_source_and_range_render_with_registered_source() {
        let mut sources = SourceMap::new();
        sources.add("legacy.gom", "abc");
        let diagnostic = Diagnostic::new(Stage::Parser, Severity::Error, "legacy")
            .with_source("legacy.gom")
            .with_range(TextRange::new(TextSize::new(1), TextSize::new(2)));
        assert_eq!(
            TextRenderer::new().render_one(&sources, &diagnostic),
            "error[parser]: legacy\nat: legacy.gom:1:2-1:3\n1 | abc\n  |  ^"
        );
    }

    #[test]
    fn multiline_spans_report_full_location_and_mark_the_first_line() {
        let mut sources = SourceMap::new();
        let source = sources.add("multi.gom", "one\ntwo\nthree");
        let span = sources.span(source, 1, 7).unwrap();
        let diagnostic = Diagnostic::new(Stage::Parser, Severity::Error, "multiline")
            .with_primary_label(span, "range");
        assert_eq!(
            TextRenderer::new().render_one(&sources, &diagnostic),
            "error[parser]: multiline\nprimary: multi.gom:1:2-2:4: range\n1 | one\n  |  ^^"
        );
    }

    #[test]
    fn tabs_are_expanded_consistently_in_source_and_markers() {
        let mut sources = SourceMap::new();
        let source = sources.add("tabs.gom", "\tvalue");
        let span = sources.span(source, 1, 6).unwrap();
        let diagnostic = Diagnostic::new(Stage::Parser, Severity::Error, "tab")
            .with_primary_label(span, "value");
        assert_eq!(
            TextRenderer::new().render_one(&sources, &diagnostic),
            "error[parser]: tab\nprimary: tabs.gom:1:2-1:7: value\n1 |     value\n  |     ^^^^^"
        );
    }
}
