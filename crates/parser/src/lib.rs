use std::path::Path;

pub use diagnostics::{
    Diagnostic, Diagnostics, Severity as DiagnosticSeverity, Stage as DiagnosticStage,
};
use parser::{ParseResult, Parser};
use syntax::MySyntaxNode;

pub mod error;
pub mod event;
pub mod expr;
pub mod file;
pub mod input;
pub mod parser;
pub mod pattern;
pub mod syntax;

pub use error::{DiagnosticFormatExt, format_parser_diagnostics};

pub fn parse(filename: &Path, input: &str) -> ParseResult {
    let toks = lexer::lex(input);
    let mut parser = Parser::new(filename, toks);
    file::file(&mut parser);
    parser.build_tree()
}

pub fn debug_tree(node: &rowan::GreenNode) -> String {
    let mut s = String::new();
    let root: MySyntaxNode = rowan::SyntaxNode::new_root(node.clone());
    let tree = format!("{:#?}", root);
    s.push_str(&tree[0..tree.len() - 1]);
    s
}
