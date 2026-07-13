use std::path::{Component, Path};

use cst::cst::CstNode;
use parser::syntax::MySyntaxNode;
use text_size::TextRange;

use crate::hir::SourceFileAst;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TestItemKind {
    Internal,
    External,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TestItem {
    pub name: String,
    pub range: TextRange,
    pub kind: TestItemKind,
}

pub fn test_items(path: &Path, src: &str) -> Vec<TestItem> {
    let kind = if path
        .components()
        .any(|component| matches!(component, Component::Normal(segment) if segment == "tests"))
    {
        TestItemKind::External
    } else if goml_project::package_graph::is_internal_test_source(path) {
        TestItemKind::Internal
    } else {
        return Vec::new();
    };
    let parsed = parser::parse(path, src);
    let root = MySyntaxNode::new_root(parsed.green_node);
    let Some(cst) = cst::cst::File::cast(root) else {
        return Vec::new();
    };
    let (Some(ast), _) = ::ast::lower::lower(cst).into_parts() else {
        return Vec::new();
    };
    let (candidates, _) =
        crate::testing::collect_test_candidates(&[SourceFileAst::new(path.to_path_buf(), ast)]);
    candidates
        .into_iter()
        .map(|candidate| TestItem {
            name: candidate.name,
            range: candidate.range,
            kind,
        })
        .collect()
}
