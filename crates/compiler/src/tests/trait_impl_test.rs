use std::path::PathBuf;

use cst::cst::CstNode;
use expect_test::expect;
use parser::{Diagnostics, syntax::MySyntaxNode};

use crate::{
    env::{GlobalTypeEnv, format_typer_diagnostics},
    tast,
};

fn typecheck(src: &str) -> (tast::File, GlobalTypeEnv, Diagnostics) {
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
    let (hir, hir_table, mut hir_diagnostics) = crate::hir::lower_to_hir(ast);
    let (tast, genv, mut diagnostics) = crate::typer::check_file(hir, hir_table);
    diagnostics.append(&mut hir_diagnostics);
    (tast, genv, diagnostics)
}

fn diagnostic_lines(src: &str) -> Vec<String> {
    let (_, _genv, diagnostics) = typecheck(src);
    format_typer_diagnostics(&diagnostics, src)
}

#[test]
fn ambiguous_trait_goal_does_not_commit_inference() {
    let src = r#"
trait Mark {
    fn mark(self: Self) -> unit;
}

impl Mark for int32 {
    fn mark(self: int32) -> unit { () }
}

impl Mark for string {
    fn mark(self: string) -> unit { () }
}

fn require_mark[T: Mark](value: T) -> unit {
    ()
}

fn main() -> unit {
    let consume = |value| require_mark(value);
    ()
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("Could not infer the type required to prove Mark<unknown>")),
        "{diagnostics:?}"
    );
}

#[test]
fn inherent_impl_registers_methods() {
    let src = r#"
struct Point { x: int32, y: int32 }

impl Point {
    fn new(x: int32, y: int32) -> Point { Point { x: x, y: y } }
    fn origin() -> Point { Point { x: 0, y: 0 } }
}
"#;

    let (_tast, genv, diagnostics) = typecheck(src);
    let diagnostics = format_typer_diagnostics(&diagnostics, src);
    let mut lines = Vec::new();
    lines.push(format!("diagnostics={diagnostics:?}"));

    let point_ty = tast::Ty::TStruct {
        name: "Point".to_string(),
    };

    let impl_def = genv
        .trait_env
        .inherent_impls
        .get(&crate::env::InherentImplKey::Exact(point_ty.clone()))
        .expect("inherent impl exists");
    let mut method_names: Vec<_> = impl_def.methods.keys().cloned().collect();
    method_names.sort();
    lines.push(format!("methods={method_names:?}"));

    expect![[r#"
        diagnostics=[]
        methods=["new", "origin"]"#]]
    .assert_eq(&lines.join("\n"));
}

#[test]
fn inherent_impl_instantiates_self_types() {
    let src = r#"
struct Point { x: int32, y: int32 }

impl Point {
    fn copy(self: Self, other: Self) -> Self { other }
    fn origin() -> Self { Point { x: 0, y: 0 } }
}
"#;

    let (tast_file, genv, diagnostics) = typecheck(src);
    let diagnostics = format_typer_diagnostics(&diagnostics, src);
    let mut lines = Vec::new();
    lines.push(format!("diagnostics={diagnostics:?}"));

    let point_ty = tast::Ty::TStruct {
        name: "Point".to_string(),
    };

    let impl_def = genv
        .trait_env
        .inherent_impls
        .get(&crate::env::InherentImplKey::Exact(point_ty))
        .expect("inherent impl registered");

    let copy_scheme = impl_def
        .methods
        .get("copy")
        .expect("copy method registered");
    lines.push(format!("copy_scheme={:?}", copy_scheme.ty));

    let impl_block = tast_file
        .toplevels
        .iter()
        .find_map(|item| {
            if let tast::Item::ImplBlock(block) = item {
                Some(block)
            } else {
                None
            }
        })
        .expect("expected inherent impl block in tast");

    let copy_fn = impl_block
        .methods
        .iter()
        .find(|f| f.name == "copy")
        .expect("copy method present in tast");
    lines.push(format!("copy_fn.params={:?}", copy_fn.params));
    lines.push(format!("copy_fn.ret_ty={:?}", copy_fn.ret_ty));

    let origin_scheme = impl_def
        .methods
        .get("origin")
        .expect("origin method registered");
    lines.push(format!("origin_scheme={:?}", origin_scheme.ty));

    expect![[r#"
        diagnostics=[]
        copy_scheme=TFunc([TStruct(Point), TStruct(Point)], TStruct(Point))
        copy_fn.params=[("self/0", TStruct(Point)), ("other/1", TStruct(Point))]
        copy_fn.ret_ty=TStruct(Point)
        origin_scheme=TFunc([], TStruct(Point))"#]]
    .assert_eq(&lines.join("\n"));
}

#[test]
fn generic_trait_parameters_and_signatures_are_validated() {
    let src = r#"
trait Duplicate[T, T] {
    fn value(self: Self) -> T;
}

trait Broken[T] {
    fn value(self: Self) -> Missing;
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("type parameter T is defined multiple times")),
        "{diagnostics:?}"
    );
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("Unknown type constructor Missing")),
        "{diagnostics:?}"
    );
}

#[test]
fn generic_trait_arguments_are_validated() {
    let src = r#"
trait Convert[T] {
    fn convert(self: Self) -> T;
}

fn convert_missing[T: Convert[Missing]](value: T) -> unit { () }
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("Unknown type constructor Missing")),
        "{diagnostics:?}"
    );
}

#[test]
fn explicit_generic_trait_arguments_are_validated() {
    let src = r#"
trait Convert[T] {
    fn convert(self: Self) -> T;
}

struct Token {}

impl Convert[int32] for Token {
    fn convert(self: Token) -> int32 { 7 }
}

fn main() -> unit {
    let _ = Convert::[int32, string]::convert(Token {});
    let _ = Convert::[Missing]::convert(Token {});
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("Trait Convert expects 1 type arguments, but got 2")),
        "{diagnostics:?}"
    );
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("Unknown type constructor Missing")),
        "{diagnostics:?}"
    );
}
