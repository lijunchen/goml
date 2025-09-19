use std::path::PathBuf;

use cst::cst::CstNode;
use parser::syntax::MySyntaxNode;

use crate::{env::Env, tast};

fn typecheck(src: &str) -> (tast::File, Env) {
    let path = PathBuf::from("dummy.src");
    let parsed = parser::parse(&path, src);
    if parsed.has_errors() {
        panic!("Parse errors:\n{}", parsed.format_errors(src).join("\n"));
    }
    let root = MySyntaxNode::new_root(parsed.green_node);
    let cst = cst::cst::File::cast(root).expect("failed to cast syntax tree");
    let ast = ast::lower::lower(cst).expect("failed to lower to AST");
    crate::typer::check_file(ast)
}

#[test]
#[should_panic(expected = "Trait Display::show expected return type")]
fn impl_with_mismatched_return_type_panics() {
    let src = r#"
trait Display {
    fn show(Self) -> string;
}

impl Display for int {
    fn show(self: int) -> bool { true }
}
"#;

    let _ = typecheck(src);
}

#[test]
#[should_panic(expected = "Trait Display::show parameter 0 expected type")]
fn impl_with_mismatched_param_type_panics() {
    let src = r#"
trait Display {
    fn show(Self) -> string;
}

impl Display for int {
    fn show(self: bool) -> string { "ok" }
}
"#;

    let _ = typecheck(src);
}

#[test]
#[should_panic(expected = "Trait Add::add expects 2 parameters but impl has 1")]
fn impl_with_parameter_arity_mismatch_panics() {
    let src = r#"
trait Add {
    fn add(Self, Self) -> Self;
}

impl Add for int {
    fn add(self: int) -> int { self }
}
"#;

    let _ = typecheck(src);
}

#[test]
#[should_panic(expected = "Trait Display implementation for TInt is missing method debug")]
fn impl_missing_trait_method_panics() {
    let src = r#"
trait Display {
    fn show(Self) -> string;
    fn debug(Self) -> string;
}

impl Display for int {
    fn show(self: int) -> string { "value" }
}
"#;

    let _ = typecheck(src);
}

#[test]
#[should_panic(expected = "Method extra is not declared in trait Display")]
fn impl_with_extra_method_panics() {
    let src = r#"
trait Display {
    fn show(Self) -> string;
}

impl Display for int {
    fn show(self: int) -> string { "value" }
    fn extra(self: int) -> string { "extra" }
}
"#;

    let _ = typecheck(src);
}

#[test]
#[should_panic(expected = "Trait Unknown is not defined")]
fn impl_for_unknown_trait_panics() {
    let src = r#"
impl Unknown for int {
    fn mystery(self: int) -> int { self }
}
"#;

    let _ = typecheck(src);
}
