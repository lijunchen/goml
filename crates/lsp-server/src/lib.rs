use line_index::{LineIndex, WideEncoding, WideLineCol};
use text_size::TextRange;
use tower_lsp::lsp_types::*;

pub mod handlers;

#[cfg(test)]
mod tests;

#[derive(Debug)]
pub struct Document {
    pub content: String,
    pub line_index: LineIndex,
}

impl Document {
    pub fn new(content: String) -> Self {
        let line_index = LineIndex::new(&content);
        Self {
            content,
            line_index,
        }
    }

    pub fn position(&self, offset: text_size::TextSize) -> Option<Position> {
        let line_col = self.line_index.try_line_col(offset)?;
        let line_col = self.line_index.to_wide(WideEncoding::Utf16, line_col)?;
        Some(Position {
            line: line_col.line,
            character: line_col.col,
        })
    }

    pub fn offset(&self, position: Position) -> Option<text_size::TextSize> {
        let wide = WideLineCol {
            line: position.line,
            col: position.character,
        };
        let utf8 = self.line_index.to_utf8(WideEncoding::Utf16, wide)?;
        let offset = self.line_index.offset(utf8)?;
        let actual = self.line_index.try_line_col(offset)?;
        let actual_wide = self.line_index.to_wide(WideEncoding::Utf16, actual)?;
        (actual == utf8 && actual_wide == wide).then_some(offset)
    }

    pub fn utf8_position(&self, position: Position) -> Option<(u32, u32)> {
        let offset = self.offset(position)?;
        let line_col = self.line_index.try_line_col(offset)?;
        Some((line_col.line, line_col.col))
    }

    pub fn range(&self, text_range: TextRange) -> Option<Range> {
        let start = self.position(text_range.start())?;
        let end = self.position(text_range.end())?;
        Some(Range { start, end })
    }
}
