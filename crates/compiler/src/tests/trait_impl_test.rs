use std::path::PathBuf;

use cst::cst::CstNode;
use parser::syntax::MySyntaxNode;

use crate::{
    env::{GlobalTypeEnv, format_typer_diagnostics},
    tast,
};

fn typecheck(src: &str) -> (tast::File, GlobalTypeEnv) {
    let path = PathBuf::from("dummy.src");
    let parsed = parser::parse(&path, src);
    if parsed.has_errors() {
        panic!("Parse errors:\n{}", parsed.format_errors(src).join("\n"));
    }
    let root = MySyntaxNode::new_root(parsed.green_node);
    let cst = cst::cst::File::cast(root).expect("failed to cast syntax tree");
    let ast = ast::lower::lower(cst)
        .into_result()
        .expect("failed to lower to AST");
    crate::typer::check_file(ast)
}

fn expect_single_error(src: &str, expected: &str) {
    let (_, genv) = typecheck(src);
    let diagnostics = format_typer_diagnostics(&genv.diagnostics);
    assert_eq!(
        diagnostics.len(),
        1,
        "expected exactly one diagnostic, got {:?}",
        diagnostics
    );
    assert!(
        diagnostics[0].contains(expected),
        "expected diagnostic to contain `{}`, got `{}`",
        expected,
        diagnostics[0]
    );
}

#[test]
fn impl_with_mismatched_return_type_reports_diagnostic() {
    let src = r#"
trait Display {
    fn show(Self) -> string;
}

impl Display for int {
    fn show(self: int) -> bool { true }
}
"#;

    expect_single_error(src, "Trait Display::show expected return type");
}

#[test]
fn impl_with_mismatched_param_type_reports_diagnostic() {
    let src = r#"
trait Display {
    fn show(Self) -> string;
}

impl Display for int {
    fn show(self: bool) -> string { "ok" }
}
"#;

    expect_single_error(src, "Trait Display::show parameter 0 expected type");
}

#[test]
fn impl_with_parameter_arity_mismatch_reports_diagnostic() {
    let src = r#"
trait Add {
    fn add(Self, Self) -> Self;
}

impl Add for int {
    fn add(self: int) -> int { self }
}
"#;

    expect_single_error(src, "Trait Add::add expects 2 parameters but impl has 1");
}

#[test]
fn impl_missing_trait_method_reports_diagnostic() {
    let src = r#"
trait Display {
    fn show(Self) -> string;
    fn debug(Self) -> string;
}

impl Display for int {
    fn show(self: int) -> string { "value" }
}
"#;

    expect_single_error(
        src,
        "Trait Display implementation for TInt32 is missing method debug",
    );
}

#[test]
fn impl_with_extra_method_reports_diagnostic() {
    let src = r#"
trait Display {
    fn show(Self) -> string;
}

impl Display for int {
    fn show(self: int) -> string { "value" }
    fn extra(self: int) -> string { "extra" }
}
"#;

    expect_single_error(src, "Method extra is not declared in trait Display");
}

#[test]
fn impl_for_unknown_trait_reports_diagnostic() {
    let src = r#"
impl Unknown for int {
    fn mystery(self: int) -> int { self }
}
"#;

    expect_single_error(src, "Trait Unknown is not defined");
}

#[test]
fn impl_for_struct_reports_missing_method_diagnostic() {
    let src = r#"
struct Point { x: int, y: int }

trait Display {
    fn show(Self) -> string;
    fn debug(Self) -> string;
}

impl Display for Point {
    fn show(self: Point) -> string { "value" }
}
"#;

    expect_single_error(
        src,
        "Trait Display implementation for TCon(Point) is missing method debug",
    );
}

#[test]
fn impl_for_generic_type_reports_missing_method_diagnostic() {
    let src = r#"
enum Option[T] {
    Some(T),
    None,
}

trait Display {
    fn show(Self) -> string;
    fn debug(Self) -> string;
}

impl Display for Option[int] {
    fn show(self: Option[int]) -> string { "value" }
}
"#;

    expect_single_error(
        src,
        "Trait Display implementation for TApp(TCon(Option), [TInt32]) is missing method debug",
    );
}
