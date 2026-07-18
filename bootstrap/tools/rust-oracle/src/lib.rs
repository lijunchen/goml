use std::fmt::Write;
use std::path::Path;

mod ast_encode;

pub use ast_encode::encode_ast;

pub fn encode(source: &str) -> String {
    let mut output = String::new();
    for token in lexer::lex(source) {
        let start = u32::from(token.range.start());
        let end = u32::from(token.range.end());
        write!(output, "{:?}\t{start}\t{end}\t", token.kind).unwrap();
        for byte in token.text.bytes() {
            write!(output, "{byte:02x}").unwrap();
        }
        output.push('\n');
    }
    output
}

fn encode_hex(output: &mut String, value: &str) {
    for byte in value.bytes() {
        write!(output, "{byte:02x}").unwrap();
    }
}

fn encode_node(output: &mut String, node: &parser::syntax::MySyntaxNode) {
    let range = node.text_range();
    writeln!(
        output,
        "N\t{:?}\t{}\t{}",
        node.kind(),
        u32::from(range.start()),
        u32::from(range.end())
    )
    .unwrap();
    for child in node.children_with_tokens() {
        if let Some(child_node) = child.as_node() {
            encode_node(output, child_node);
        } else if let Some(token) = child.as_token() {
            let range = token.text_range();
            write!(
                output,
                "T\t{:?}\t{}\t{}\t",
                token.kind(),
                u32::from(range.start()),
                u32::from(range.end())
            )
            .unwrap();
            encode_hex(output, token.text());
            output.push('\n');
        }
    }
    output.push_str("E\n");
}

pub fn encode_parse(path: &Path, source: &str) -> String {
    let result = parser::parse(path, source);
    let root = parser::syntax::MySyntaxNode::new_root(result.green_node);
    let mut output = String::new();
    encode_node(&mut output, &root);
    for diagnostic in &result.diagnostics {
        let range = diagnostic.range().unwrap_or_default();
        write!(
            output,
            "D\t{}\t{}\t",
            u32::from(range.start()),
            u32::from(range.end())
        )
        .unwrap();
        encode_hex(&mut output, diagnostic.message());
        output.push('\n');
    }
    output
}
