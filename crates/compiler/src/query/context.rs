use std::path::Path;

use cst::cst::CstNode;
use parser::syntax::{MySyntaxKind, MySyntaxNode, MySyntaxToken};
use text_size::TextSize;

pub(crate) struct QueryContext {
    root: MySyntaxNode,
    file: cst::cst::File,
    offset: TextSize,
}

impl QueryContext {
    pub(crate) fn from_position(
        path: &Path,
        src: &str,
        line: u32,
        col: u32,
    ) -> Result<Self, String> {
        let result = parser::parse(path, src);
        let root = MySyntaxNode::new_root(result.green_node);
        let file = cst::cst::File::cast(root.clone())
            .ok_or_else(|| "failed to cast syntax tree".to_string())?;
        let line_index = line_index::LineIndex::new(src);
        let offset = line_index
            .offset(line_index::LineCol { line, col })
            .ok_or_else(|| "failed to get offset from line and column".to_string())?;

        Ok(Self { root, file, offset })
    }

    pub(crate) fn root(&self) -> &MySyntaxNode {
        &self.root
    }

    pub(crate) fn syntax(&self) -> &MySyntaxNode {
        self.file.syntax()
    }

    pub(crate) fn offset(&self) -> TextSize {
        self.offset
    }

    pub(crate) fn token_prefer_ident(&self) -> Option<MySyntaxToken> {
        match self.syntax().token_at_offset(self.offset) {
            rowan::TokenAtOffset::None => None,
            rowan::TokenAtOffset::Single(token) => Some(token),
            rowan::TokenAtOffset::Between(left, right) => {
                if left.kind() == MySyntaxKind::Ident {
                    Some(left)
                } else {
                    Some(right)
                }
            }
        }
    }
}
