use std::path::PathBuf;

use cst::cst::CstNode;
use expect_test::{Expect, expect};
use parser::{Diagnostics, syntax::MySyntaxNode};

use crate::{
    env::{GlobalTypeEnv, format_typer_diagnostics},
    pipeline::pipeline::{CompilationError, compile_single_file},
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

fn expect_diagnostics(src: &str, expected: Expect) {
    let (_, _genv, diagnostics) = typecheck(src);
    let diagnostics = format_typer_diagnostics(&diagnostics, src);
    expected.assert_debug_eq(&diagnostics);
}

#[test]
fn builtin_generic_constraints_are_checked_at_call_site() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/println_option_without_tostring/main.gom");
    let src = std::fs::read_to_string(&path).unwrap_or_else(|err| {
        panic!("failed to read {}: {err}", path.display());
    });
    let err = compile_single_file(&path, &src).expect_err("expected typer error");

    match err {
        CompilationError::Typer { diagnostics } => {
            let diagnostics = format_typer_diagnostics(&diagnostics, &src);
            assert!(
                diagnostics
                    .iter()
                    .any(|line| line
                        .contains("No instance found for trait ToString<Option[string]>")),
                "{diagnostics:?}"
            );
        }
        other => panic!("expected typer error, got {other:?}"),
    }
}

#[test]
fn impl_with_mismatched_return_type_reports_diagnostic() {
    let src = r#"
trait Display {
    fn show(Self) -> string;
}

impl Display for int32 {
    fn show(self: int32) -> bool { true }
}
"#;

    expect_diagnostics(
        src,
        expect![[r#"
            [
                "Trait Display::show expected return type string but found bool",
            ]
        "#]],
    );
}

#[test]
fn impl_with_mismatched_param_type_reports_diagnostic() {
    let src = r#"
trait Display {
    fn show(Self) -> string;
}

impl Display for int32 {
    fn show(self: bool) -> string { "ok" }
}
"#;

    expect_diagnostics(
        src,
        expect![[r#"
            [
                "Trait Display::show parameter 0 expected type int32 but found bool",
            ]
        "#]],
    );
}

#[test]
fn impl_with_parameter_arity_mismatch_reports_diagnostic() {
    let src = r#"
trait Add {
    fn add(Self, Self) -> Self;
}

impl Add for int32 {
    fn add(self: int32) -> int32 { self }
}
"#;

    expect_diagnostics(
        src,
        expect![[r#"
            [
                "Trait Add::add expects 2 parameters but impl has 1",
            ]
        "#]],
    );
}

#[test]
fn impl_missing_trait_method_reports_diagnostic() {
    let src = r#"
trait Display {
    fn show(Self) -> string;
    fn debug(Self) -> string;
}

impl Display for int32 {
    fn show(self: int32) -> string { "value" }
}
"#;

    expect_diagnostics(
        src,
        expect![[r#"
            [
                "Trait Display implementation for int32 is missing method debug",
            ]
        "#]],
    );
}

#[test]
fn impl_with_extra_method_reports_diagnostic() {
    let src = r#"
trait Display {
    fn show(Self) -> string;
}

impl Display for int32 {
    fn show(self: int32) -> string { "value" }
    fn extra(self: int32) -> string { "extra" }
}
"#;

    expect_diagnostics(
        src,
        expect![[r#"
            [
                "Method extra is not declared in trait Display",
            ]
        "#]],
    );
}

#[test]
fn impl_for_unknown_trait_reports_diagnostic() {
    let src = r#"
impl Unknown for int32 {
    fn mystery(self: int32) -> int32 { self }
}
"#;

    expect_diagnostics(
        src,
        expect![[r#"
            [
                "Trait Unknown is not defined, cannot implement it for int32",
            ]
        "#]],
    );
}

#[test]
fn impl_for_struct_reports_missing_method_diagnostic() {
    let src = r#"
struct Point { x: int32, y: int32 }

trait Display {
    fn show(Self) -> string;
    fn debug(Self) -> string;
}

impl Display for Point {
    fn show(self: Point) -> string { "value" }
}
"#;

    expect_diagnostics(
        src,
        expect![[r#"
            [
                "Trait Display implementation for Point is missing method debug",
            ]
        "#]],
    );
}

#[test]
fn impl_for_generic_type_reports_missing_method_diagnostic() {
    let src = r#"
enum Maybe[T] {
    Just(T),
    Nothing,
}

trait Display {
    fn show(Self) -> string;
    fn debug(Self) -> string;
}

impl Display for Maybe[int32] {
    fn show(self: Maybe[int32]) -> string { "value" }
}
"#;

    expect_diagnostics(
        src,
        expect![[r#"
            [
                "Trait Display implementation for Maybe[int32] is missing method debug",
            ]
        "#]],
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
