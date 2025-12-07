use std::path::PathBuf;

use cst::cst::CstNode;
use parser::{Diagnostics, syntax::MySyntaxNode};

use crate::{
    env::{GlobalTypeEnv, format_typer_diagnostics},
    mangle::encode_ty,
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
    crate::typer::check_file(ast)
}

fn expect_single_error(src: &str, expected: &str) {
    let (_, _genv, diagnostics) = typecheck(src);
    let diagnostics = format_typer_diagnostics(&diagnostics);
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

impl Display for int32 {
    fn show(self: int32) -> bool { true }
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

impl Display for int32 {
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

impl Add for int32 {
    fn add(self: int32) -> int32 { self }
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

impl Display for int32 {
    fn show(self: int32) -> string { "value" }
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

impl Display for int32 {
    fn show(self: int32) -> string { "value" }
    fn extra(self: int32) -> string { "extra" }
}
"#;

    expect_single_error(src, "Method extra is not declared in trait Display");
}

#[test]
fn impl_for_unknown_trait_reports_diagnostic() {
    let src = r#"
impl Unknown for int32 {
    fn mystery(self: int32) -> int32 { self }
}
"#;

    expect_single_error(src, "Trait Unknown is not defined");
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

    expect_single_error(
        src,
        "Trait Display implementation for TStruct(Point) is missing method debug",
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

    expect_single_error(
        src,
        "Trait Display implementation for TApp(TEnum(Maybe), [TInt32]) is missing method debug",
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
    let diagnostics = format_typer_diagnostics(&diagnostics);
    assert!(
        diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        diagnostics
    );

    let point_ty = tast::Ty::TStruct {
        name: "Point".to_string(),
    };

    let encoded = encode_ty(&point_ty);
    let impl_def = genv
        .trait_env
        .inherent_impls
        .get(&encoded)
        .expect("inherent impl exists");
    assert!(impl_def.methods.contains_key("new"));
    assert!(impl_def.methods.contains_key("origin"));
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
    let diagnostics = format_typer_diagnostics(&diagnostics);
    assert!(
        diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        diagnostics
    );

    let point_ty = tast::Ty::TStruct {
        name: "Point".to_string(),
    };
    let encoded = encode_ty(&point_ty);

    let impl_def = genv
        .trait_env
        .inherent_impls
        .get(&encoded)
        .expect("inherent impl registered");

    let copy_scheme = impl_def
        .methods
        .get("copy")
        .expect("copy method registered");

    match &copy_scheme.ty {
        tast::Ty::TFunc { params, ret_ty } => {
            assert_eq!(
                *params,
                vec![point_ty.clone(), point_ty.clone()],
                "copy params should be instantiated to Point",
            );
            assert_eq!(
                **ret_ty,
                point_ty.clone(),
                "copy return type should be instantiated to Point",
            );
        }
        other => panic!("expected copy to have function type, found {:?}", other),
    }

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
    assert_eq!(copy_fn.params[0].1, point_ty.clone());
    assert_eq!(copy_fn.params[1].1, point_ty.clone());
    assert_eq!(copy_fn.ret_ty, point_ty.clone());

    let origin_scheme = impl_def
        .methods
        .get("origin")
        .expect("origin method registered");

    match &origin_scheme.ty {
        tast::Ty::TFunc { params, ret_ty } => {
            assert!(params.is_empty(), "origin should not take parameters");
            assert_eq!(**ret_ty, point_ty);
        }
        other => panic!("expected origin to have function type, found {:?}", other),
    }
}
