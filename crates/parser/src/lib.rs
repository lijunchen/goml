use std::path::Path;

pub use diagnostics::{
    Diagnostic, Diagnostics, Severity as DiagnosticSeverity, Stage as DiagnosticStage,
};
#[cfg(any(debug_assertions, test))]
use lexer::Token;
use parser::{ParseResult, Parser};
use syntax::MySyntaxNode;

#[cfg(any(debug_assertions, test))]
lalrpop_util::lalrpop_mod!(grammar);

pub mod error;
pub mod event;
pub mod expr;
pub mod file;
pub mod input;
#[cfg(any(debug_assertions, test))]
mod lalr;
pub mod parser;
pub mod path;
pub mod pattern;
pub mod stmt;
pub mod syntax;

pub use error::{DiagnosticFormatExt, format_parser_diagnostics};

pub fn parse(filename: &Path, input: &str) -> ParseResult {
    let toks = lexer::lex(input);
    #[cfg(debug_assertions)]
    let validation_toks = toks.clone();
    let mut parser = Parser::new(filename, toks);
    file::file(&mut parser);
    let result = parser.build_tree();
    #[cfg(debug_assertions)]
    validate_lalrpop(filename, &validation_toks, &result);
    result
}

#[cfg(any(debug_assertions, test))]
fn parse_lalrpop_tokens(
    toks: &[Token<'_>],
) -> Result<
    lalr::CstNode,
    lalrpop_util::ParseError<usize, lalr::ParserToken, std::convert::Infallible>,
> {
    let first = toks
        .iter()
        .find(|token| !token.kind.is_trivia())
        .map(|token| token.kind);
    if first.is_none()
        || first.is_some_and(|kind| {
            matches!(
                kind,
                lexer::TokenKind::PackageKeyword
                    | lexer::TokenKind::UseKeyword
                    | lexer::TokenKind::Pound
                    | lexer::TokenKind::PubKeyword
                    | lexer::TokenKind::ExternKeyword
                    | lexer::TokenKind::FnKeyword
                    | lexer::TokenKind::EnumKeyword
                    | lexer::TokenKind::StructKeyword
                    | lexer::TokenKind::TraitKeyword
                    | lexer::TokenKind::ImplKeyword
            )
        })
    {
        grammar::FileParser::new().parse(lalr::tokens(toks))
    } else {
        grammar::ExpressionFileParser::new().parse(lalr::tokens(toks))
    }
}

#[cfg(debug_assertions)]
fn validate_lalrpop(filename: &Path, toks: &[Token<'_>], result: &ParseResult) {
    if result.has_errors() || !lalr::handles(toks) {
        return;
    }
    let Ok(root) = parse_lalrpop_tokens(toks) else {
        return;
    };
    let validation = lalr::finish(root, toks);
    assert_eq!(
        result.green_node,
        validation.green_node,
        "{}",
        filename.display()
    );
}

pub fn debug_tree(node: &rowan::GreenNode) -> String {
    let mut s = String::new();
    let root: MySyntaxNode = rowan::SyntaxNode::new_root(node.clone());
    let tree = format!("{:#?}", root);
    s.push_str(&tree[0..tree.len() - 1]);
    s
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::{Path, PathBuf};

    use super::*;

    #[test]
    fn lalrpop_matches_handwritten_parser_on_valid_corpus() {
        let workspace = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let roots = [
            workspace.join("stdlib"),
            workspace.join("bootstrap"),
            workspace.join("crates/compiler/src/tests"),
        ];
        let mut files = roots
            .iter()
            .flat_map(|root| goml_files(root))
            .collect::<Vec<_>>();
        files.push(workspace.join("crates/compiler/src/builtin_contract.gom"));
        files.sort();

        for path in files {
            let source = fs::read_to_string(&path).unwrap();
            let tokens = lexer::lex(&source);
            let mut handwritten = Parser::new(&path, tokens.clone());
            file::file(&mut handwritten);
            let handwritten = handwritten.build_tree();
            if handwritten.has_errors() {
                continue;
            }
            assert!(lalr::handles(&tokens), "{}", path.display());
            let root = parse_lalrpop_tokens(&tokens)
                .unwrap_or_else(|error| panic!("{}: {error:?}", path.display()));
            let lalr = lalr::finish(root, &tokens);
            if lalr.green_node != handwritten.green_node {
                let actual = debug_tree(&lalr.green_node);
                let expected = debug_tree(&handwritten.green_node);
                let mismatch = actual
                    .lines()
                    .zip(expected.lines())
                    .enumerate()
                    .find(|(_, (actual, expected))| actual != expected)
                    .unwrap();
                panic!(
                    "{}:{}\nlalr: {}\nhandwritten: {}",
                    path.display(),
                    mismatch.0 + 1,
                    mismatch.1.0,
                    mismatch.1.1
                );
            }
        }
    }

    #[test]
    fn contextual_identifiers_parse_as_ordinary_names() {
        let source = r#"
struct Value {
    import: int32,
}

fn import(crate: int32, super: int32, array: int32, mod: int32) -> int32 {
    crate + super + array + mod
}

fn id[T](value: T) -> T {
    value
}

fn main() -> unit {
    let mod = import(1, 2, 3, 4);
    let _ = if mod == 10 { mod } else if false { array } else { crate };
    let _ = id::[int32](mod);
    let _ = 0..mod;
}
"#;
        let result = parse(Path::new("test.gom"), source);
        assert!(
            !result.has_errors(),
            "unexpected parse errors: {:?}",
            result.format_errors(source)
        );
    }

    #[test]
    fn revised_syntax_reports_targeted_errors() {
        for (source, message) in [
            ("import old::path;", "`import` has been removed"),
            ("mod old;", "module declarations have been removed"),
            (
                "fn Main() -> unit { () }",
                "function name must start with a lowercase",
            ),
            (
                "struct value {}",
                "struct name must start with an uppercase",
            ),
            (
                "enum Choice { first }",
                "enum variant name must start with an uppercase",
            ),
            (
                "struct Value { Field: int32 }",
                "field name must start with a lowercase",
            ),
            (
                "fn f(value: Value) -> unit { match value { Value { Field: _ } => (), } }",
                "struct pattern field name must start with a lowercase",
            ),
            (
                "fn f[T U]() -> unit { () }",
                "expected `,` between generic parameters",
            ),
            ("fn f() -> () { () }", "use `unit` as the unit type"),
            (
                "fn f() -> unit { let _ = 1 < 2 < 3; }",
                "comparison operators cannot be chained",
            ),
            (
                "fn f() -> unit { let _ = 0..1..2; }",
                "range expressions cannot be chained",
            ),
            (
                "fn f() -> unit { while true (); }",
                "`while` bodies require a block",
            ),
            (
                "struct Value",
                "struct declarations require a brace-delimited body",
            ),
            (
                "fn f -> unit { () }",
                "functions require an explicit parameter list",
            ),
            (
                "fn f[T, U](value: T) -> U where T == U { value }",
                "`==` in where predicates has been replaced by `=`",
            ),
            (
                "fn id[T](value: T) -> T { value } fn f() -> unit { let _ = id[int32]::call(1); }",
                "generic expression arguments must use `::[...]`",
            ),
        ] {
            let result = parse(Path::new("test.gom"), source);
            let errors = result.format_errors(source);
            assert!(
                errors.iter().any(|error| error.contains(message)),
                "{message}: {errors:?}"
            );
        }
    }

    #[test]
    fn lalrpop_rejects_standalone_unit_type() {
        let invalid = lexer::lex("fn f() -> () { () }");
        assert!(parse_lalrpop_tokens(&invalid).is_err());

        let valid = lexer::lex("fn f(callback: () -> unit) -> unit { callback() }");
        assert!(parse_lalrpop_tokens(&valid).is_ok());
    }

    fn goml_files(root: &Path) -> Vec<PathBuf> {
        let mut files = Vec::new();
        let Ok(entries) = fs::read_dir(root) else {
            return files;
        };
        for entry in entries {
            let path = entry.unwrap().path();
            if path.is_dir() {
                files.extend(goml_files(&path));
            } else if path.extension().is_some_and(|extension| extension == "gom") {
                files.push(path);
            }
        }
        files
    }
}
