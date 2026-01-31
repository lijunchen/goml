use line_index::LineIndex;
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
        let line_col = self.line_index.line_col(offset);
        Some(Position {
            line: line_col.line,
            character: line_col.col,
        })
    }

    pub fn range(&self, text_range: TextRange) -> Option<Range> {
        let start = self.position(text_range.start())?;
        let end = self.position(text_range.end())?;
        Some(Range { start, end })
    }
}
