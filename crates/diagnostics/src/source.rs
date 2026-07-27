use std::fmt;
use std::ops::Range;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceId(usize);

impl SourceId {
    pub const fn new(value: usize) -> Self {
        Self(value)
    }

    pub const fn index(self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    source: SourceId,
    start: usize,
    end: usize,
}

impl Span {
    pub fn new(source: SourceId, start: usize, end: usize) -> Result<Self, SourceError> {
        if start > end {
            return Err(SourceError::InvalidRange { start, end });
        }
        Ok(Self { source, start, end })
    }

    pub fn at(source: SourceId, offset: usize) -> Self {
        Self {
            source,
            start: offset,
            end: offset,
        }
    }

    pub const fn source(self) -> SourceId {
        self.source
    }

    pub const fn source_id(self) -> SourceId {
        self.source
    }

    pub const fn start(self) -> usize {
        self.start
    }

    pub const fn end(self) -> usize {
        self.end
    }

    pub const fn len(self) -> usize {
        self.end - self.start
    }

    pub const fn is_empty(self) -> bool {
        self.start == self.end
    }

    pub fn range(self) -> Range<usize> {
        self.start..self.end
    }

    pub fn merge(self, other: Self) -> Result<Self, SourceError> {
        if self.source != other.source {
            return Err(SourceError::DifferentSources {
                left: self.source,
                right: other.source,
            });
        }
        Ok(Self {
            source: self.source,
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        })
    }

    pub const fn contains(self, offset: usize) -> bool {
        self.start <= offset && offset < self.end
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Position {
    pub source: SourceId,
    pub offset: usize,
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub const fn utf8_column(self) -> usize {
        self.column
    }

    pub const fn source_id(self) -> SourceId {
        self.source
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SourceError {
    UnknownSource(SourceId),
    InvalidRange {
        start: usize,
        end: usize,
    },
    OffsetOutOfBounds {
        source: SourceId,
        offset: usize,
        len: usize,
    },
    InvalidUtf8Boundary {
        source: SourceId,
        offset: usize,
    },
    LineOutOfBounds {
        source: SourceId,
        line: usize,
        line_count: usize,
    },
    DifferentSources {
        left: SourceId,
        right: SourceId,
    },
}

impl fmt::Display for SourceError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SourceError::UnknownSource(source) => {
                write!(formatter, "unknown source {}", source.index())
            }
            SourceError::InvalidRange { start, end } => {
                write!(formatter, "invalid byte range {start}..{end}")
            }
            SourceError::OffsetOutOfBounds {
                source,
                offset,
                len,
            } => write!(
                formatter,
                "byte offset {offset} is outside source {} with length {len}",
                source.index()
            ),
            SourceError::InvalidUtf8Boundary { source, offset } => write!(
                formatter,
                "byte offset {offset} is not a UTF-8 boundary in source {}",
                source.index()
            ),
            SourceError::LineOutOfBounds {
                source,
                line,
                line_count,
            } => write!(
                formatter,
                "line {line} is outside source {} with {line_count} lines",
                source.index()
            ),
            SourceError::DifferentSources { left, right } => write!(
                formatter,
                "cannot combine source {} with source {}",
                left.index(),
                right.index()
            ),
        }
    }
}

impl std::error::Error for SourceError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SourceLine {
    start: usize,
    content_end: usize,
    end: usize,
}

#[derive(Debug, Clone)]
pub struct SourceFile {
    id: SourceId,
    path: PathBuf,
    text: String,
    lines: Vec<SourceLine>,
}

impl SourceFile {
    fn new(id: SourceId, path: PathBuf, text: String) -> Self {
        let lines = source_lines(&text);
        Self {
            id,
            path,
            text,
            lines,
        }
    }

    pub const fn id(&self) -> SourceId {
        self.id
    }

    pub const fn source_id(&self) -> SourceId {
        self.id
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    pub fn len(&self) -> usize {
        self.text.len()
    }

    pub fn is_empty(&self) -> bool {
        self.text.is_empty()
    }

    pub fn line_count(&self) -> usize {
        self.lines.len()
    }

    pub fn position(&self, offset: usize) -> Result<Position, SourceError> {
        self.validate_offset(offset)?;
        let line = self
            .lines
            .partition_point(|source_line| source_line.start <= offset)
            .saturating_sub(1);
        Ok(Position {
            source: self.id,
            offset,
            line,
            column: offset - self.lines[line].start,
        })
    }

    pub fn line_text(&self, line: usize) -> Result<&str, SourceError> {
        let source_line = self.line(line)?;
        Ok(&self.text[source_line.start..source_line.content_end])
    }

    pub fn line_range(&self, line: usize) -> Result<Range<usize>, SourceError> {
        let source_line = self.line(line)?;
        Ok(source_line.start..source_line.content_end)
    }

    pub fn line_full_range(&self, line: usize) -> Result<Range<usize>, SourceError> {
        let source_line = self.line(line)?;
        Ok(source_line.start..source_line.end)
    }

    pub fn validate_span(&self, span: Span) -> Result<(), SourceError> {
        if span.source != self.id {
            return Err(SourceError::DifferentSources {
                left: self.id,
                right: span.source,
            });
        }
        self.validate_offset(span.start)?;
        self.validate_offset(span.end)
    }

    pub fn span(&self, start: usize, end: usize) -> Result<Span, SourceError> {
        let span = Span::new(self.id, start, end)?;
        self.validate_span(span)?;
        Ok(span)
    }

    pub fn slice(&self, span: Span) -> Result<&str, SourceError> {
        self.validate_span(span)?;
        Ok(&self.text[span.start..span.end])
    }

    fn line(&self, line: usize) -> Result<SourceLine, SourceError> {
        self.lines
            .get(line)
            .copied()
            .ok_or(SourceError::LineOutOfBounds {
                source: self.id,
                line,
                line_count: self.lines.len(),
            })
    }

    fn validate_offset(&self, offset: usize) -> Result<(), SourceError> {
        if offset > self.text.len() {
            return Err(SourceError::OffsetOutOfBounds {
                source: self.id,
                offset,
                len: self.text.len(),
            });
        }
        if !self.text.is_char_boundary(offset) {
            return Err(SourceError::InvalidUtf8Boundary {
                source: self.id,
                offset,
            });
        }
        Ok(())
    }
}

#[derive(Debug, Default, Clone)]
pub struct SourceMap {
    files: Vec<SourceFile>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_file(&mut self, path: impl Into<PathBuf>, text: impl Into<String>) -> SourceId {
        let id = SourceId::new(self.files.len());
        self.files
            .push(SourceFile::new(id, path.into(), text.into()));
        id
    }

    pub fn add(&mut self, path: impl Into<PathBuf>, text: impl Into<String>) -> SourceId {
        self.add_file(path, text)
    }

    pub fn get(&self, source: SourceId) -> Option<&SourceFile> {
        self.files.get(source.index())
    }

    pub fn file(&self, source: SourceId) -> Result<&SourceFile, SourceError> {
        self.get(source).ok_or(SourceError::UnknownSource(source))
    }

    pub fn find(&self, path: impl AsRef<Path>) -> Option<SourceId> {
        let path = path.as_ref();
        self.files
            .iter()
            .find(|file| file.path() == path)
            .map(SourceFile::id)
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = &SourceFile> {
        self.files.iter()
    }

    pub fn len(&self) -> usize {
        self.files.len()
    }

    pub fn is_empty(&self) -> bool {
        self.files.is_empty()
    }

    pub fn span(&self, source: SourceId, start: usize, end: usize) -> Result<Span, SourceError> {
        let span = Span::new(source, start, end)?;
        self.validate_span(span)?;
        Ok(span)
    }

    pub fn validate_span(&self, span: Span) -> Result<(), SourceError> {
        self.file(span.source)?.validate_span(span)
    }

    pub fn position(&self, source: SourceId, offset: usize) -> Result<Position, SourceError> {
        self.file(source)?.position(offset)
    }

    pub fn line_text(&self, source: SourceId, line: usize) -> Result<&str, SourceError> {
        self.file(source)?.line_text(line)
    }

    pub fn line_range(&self, source: SourceId, line: usize) -> Result<Range<usize>, SourceError> {
        self.file(source)?.line_range(line)
    }

    pub fn slice(&self, span: Span) -> Result<&str, SourceError> {
        self.file(span.source)?.slice(span)
    }
}

fn source_lines(text: &str) -> Vec<SourceLine> {
    let bytes = text.as_bytes();
    let mut lines = Vec::new();
    let mut start = 0;
    let mut index = 0;
    while index < bytes.len() {
        let line_end = match bytes[index] {
            b'\n' => Some(index + 1),
            b'\r' if bytes.get(index + 1) == Some(&b'\n') => Some(index + 2),
            b'\r' => Some(index + 1),
            _ => None,
        };
        if let Some(end) = line_end {
            lines.push(SourceLine {
                start,
                content_end: index,
                end,
            });
            start = end;
            index = end;
        } else {
            index += 1;
        }
    }
    lines.push(SourceLine {
        start,
        content_end: text.len(),
        end: text.len(),
    });
    lines
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_source_has_one_empty_line() {
        let mut sources = SourceMap::new();
        let id = sources.add("empty.gom", "");
        let file = sources.file(id).unwrap();
        assert_eq!(file.line_count(), 1);
        assert_eq!(file.line_text(0).unwrap(), "");
        assert_eq!(file.position(0).unwrap().line, 0);
        assert_eq!(file.position(0).unwrap().column, 0);
    }

    #[test]
    fn trailing_newline_creates_an_empty_final_line() {
        let mut sources = SourceMap::new();
        let id = sources.add("end.gom", "one\n");
        let file = sources.file(id).unwrap();
        assert_eq!(file.line_count(), 2);
        assert_eq!(file.line_text(0).unwrap(), "one");
        assert_eq!(file.line_text(1).unwrap(), "");
        assert_eq!(file.position(4).unwrap().line, 1);
        assert_eq!(file.position(4).unwrap().column, 0);
    }

    #[test]
    fn crlf_and_standalone_carriage_returns_are_line_endings() {
        let mut sources = SourceMap::new();
        let id = sources.add("lines.gom", "a\r\nb\rc\nd");
        let file = sources.file(id).unwrap();
        assert_eq!(file.line_count(), 4);
        assert_eq!(file.line_text(0).unwrap(), "a");
        assert_eq!(file.line_text(1).unwrap(), "b");
        assert_eq!(file.line_text(2).unwrap(), "c");
        assert_eq!(file.line_text(3).unwrap(), "d");
        assert_eq!(file.position(3).unwrap().line, 1);
        assert_eq!(file.position(5).unwrap().line, 2);
        assert_eq!(file.position(7).unwrap().line, 3);
    }

    #[test]
    fn positions_use_utf8_byte_columns() {
        let mut sources = SourceMap::new();
        let id = sources.add("unicode.gom", "a中🙂z\nβ");
        assert_eq!(sources.position(id, 0).unwrap().column, 0);
        assert_eq!(sources.position(id, 1).unwrap().column, 1);
        assert_eq!(sources.position(id, 4).unwrap().column, 4);
        assert_eq!(sources.position(id, 8).unwrap().column, 8);
        assert_eq!(sources.position(id, 10).unwrap().column, 0);
        assert_eq!(sources.position(id, 12).unwrap().column, 2);
    }

    #[test]
    fn positions_reject_offsets_inside_utf8_code_points() {
        let mut sources = SourceMap::new();
        let id = sources.add("unicode.gom", "中");
        assert_eq!(
            sources.position(id, 1),
            Err(SourceError::InvalidUtf8Boundary {
                source: id,
                offset: 1
            })
        );
        assert_eq!(
            sources.position(id, 2),
            Err(SourceError::InvalidUtf8Boundary {
                source: id,
                offset: 2
            })
        );
    }

    #[test]
    fn positions_reject_offsets_after_the_file() {
        let mut sources = SourceMap::new();
        let id = sources.add("short.gom", "abc");
        assert_eq!(
            sources.position(id, 4),
            Err(SourceError::OffsetOutOfBounds {
                source: id,
                offset: 4,
                len: 3
            })
        );
    }

    #[test]
    fn spans_validate_order_bounds_and_utf8_boundaries() {
        let mut sources = SourceMap::new();
        let id = sources.add("span.gom", "a中z");
        assert_eq!(sources.span(id, 1, 4).unwrap().range(), 1..4);
        assert_eq!(
            sources.span(id, 4, 1),
            Err(SourceError::InvalidRange { start: 4, end: 1 })
        );
        assert!(matches!(
            sources.span(id, 1, 99),
            Err(SourceError::OffsetOutOfBounds { .. })
        ));
        assert!(matches!(
            sources.span(id, 1, 2),
            Err(SourceError::InvalidUtf8Boundary { .. })
        ));
    }

    #[test]
    fn slicing_validated_spans_is_safe() {
        let mut sources = SourceMap::new();
        let id = sources.add("slice.gom", "zero中two");
        let span = sources.span(id, 4, 7).unwrap();
        assert_eq!(sources.slice(span).unwrap(), "中");
    }

    #[test]
    fn span_merge_forms_a_union_in_one_source() {
        let id = SourceId::new(0);
        let first = Span::new(id, 8, 12).unwrap();
        let second = Span::new(id, 2, 5).unwrap();
        assert_eq!(first.merge(second).unwrap(), Span::new(id, 2, 12).unwrap());
    }

    #[test]
    fn span_merge_rejects_different_sources() {
        let left = Span::at(SourceId::new(0), 1);
        let right = Span::at(SourceId::new(1), 1);
        assert_eq!(
            left.merge(right),
            Err(SourceError::DifferentSources {
                left: SourceId::new(0),
                right: SourceId::new(1)
            })
        );
    }

    #[test]
    fn multiple_files_keep_independent_coordinates() {
        let mut sources = SourceMap::new();
        let first = sources.add("one.gom", "a\nb");
        let second = sources.add("two.gom", "中\nz");
        assert_ne!(first, second);
        assert_eq!(sources.position(first, 2).unwrap().line, 1);
        assert_eq!(sources.position(second, 3).unwrap().column, 3);
        assert_eq!(sources.position(second, 4).unwrap().line, 1);
        assert_eq!(sources.find("one.gom"), Some(first));
        assert_eq!(sources.find("two.gom"), Some(second));
    }

    #[test]
    fn unknown_sources_and_lines_return_errors() {
        let mut sources = SourceMap::new();
        let id = sources.add("one.gom", "one");
        let missing = SourceId::new(10);
        assert!(matches!(
            sources.file(missing),
            Err(SourceError::UnknownSource(source)) if source == missing
        ));
        assert_eq!(
            sources.line_text(id, 1),
            Err(SourceError::LineOutOfBounds {
                source: id,
                line: 1,
                line_count: 1
            })
        );
    }
}
