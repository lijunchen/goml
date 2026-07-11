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

fn diagnostic_lines(src: &str) -> Vec<String> {
    let (_, _genv, diagnostics) = typecheck(src);
    format_typer_diagnostics(&diagnostics, src)
}

#[test]
fn composite_type_parameter_trait_goal_requires_proof() {
    let src = r#"
trait Mark {
    fn mark(Self) -> unit;
}

struct Box[T] {
    value: T,
}

fn require_mark[T: Mark](value: T) -> unit {
    ()
}

fn use_box[T](value: Box[T]) -> unit {
    require_mark(value)
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("No instance found for trait Mark<Box[T]>")),
        "{diagnostics:?}"
    );
}

#[test]
fn overlapping_generic_and_concrete_impls_are_rejected_at_definition() {
    let src = r#"
trait Label {
    fn label(Self) -> string;
}

struct Box[T] {
    value: T,
}

impl[T] Label for Box[T] {
    fn label(self: Box[T]) -> string { "generic" }
}

impl Label for Box[int32] {
    fn label(self: Box[int32]) -> string { "concrete" }
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("overlaps with implementation")),
        "{diagnostics:?}"
    );
}

#[test]
fn invalid_impl_does_not_satisfy_trait_goal() {
    let src = r#"
trait Render {
    fn render(Self) -> string;
}

struct Item {}

impl Render for Item {}

fn require_render[T: Render](value: T) -> unit {
    ()
}

fn use_item(value: Item) -> unit {
    require_render(value)
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("is missing method render")),
        "{diagnostics:?}"
    );
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("No instance found for trait Render<Item>")),
        "{diagnostics:?}"
    );
}

#[test]
fn method_resolution_waits_for_receiver_inference() {
    let src = r#"
trait Alpha {
    fn text(Self) -> string;
}

trait Beta {
    fn text(Self) -> string;
}

struct A {}
struct B {}

impl Alpha for A {
    fn text(self: A) -> string { "a" }
}

impl Beta for B {
    fn text(self: B) -> string { "b" }
}

fn main() -> unit {
    let render = |value| value.text();
    println(render(A {}))
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn trait_goal_infers_nested_type_from_unique_impl() {
    let src = r#"
trait Mark {
    fn mark(Self) -> unit;
}

impl Mark for Vec[int32] {
    fn mark(self: Vec[int32]) -> unit { () }
}

fn require_mark[T: Mark](value: T) -> unit {
    ()
}

fn main() -> unit {
    let values = vec_new();
    require_mark(values)
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn canonical_trait_cache_replays_unique_inference() {
    let src = r#"
trait Mark {
    fn mark(Self) -> unit;
}

impl Mark for Vec[int32] {
    fn mark(self: Vec[int32]) -> unit { () }
}

fn require_mark[T: Mark](value: T) -> unit {
    ()
}

fn main() -> unit {
    let first = vec_new();
    require_mark(first);
    let second = vec_new();
    require_mark(second)
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn ambiguous_trait_goal_does_not_commit_inference() {
    let src = r#"
trait Mark {
    fn mark(Self) -> unit;
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
fn ambiguous_trait_goal_is_retried_after_unique_inference() {
    let src = r#"
trait First {
    fn first(Self) -> unit;
}

trait Second {
    fn second(Self) -> unit;
}

impl First for int32 {
    fn first(self: int32) -> unit { () }
}

impl First for string {
    fn first(self: string) -> unit { () }
}

impl Second for string {
    fn second(self: string) -> unit { () }
}

fn require_first[T: First](value: T) -> unit {
    ()
}

fn require_second[T: Second](value: T) -> unit {
    ()
}

fn main() -> unit {
    let consume = |value| {
        require_first(value);
        require_second(value)
    };
    ()
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn nested_impl_bound_can_drive_inference() {
    let src = r#"
trait Inner {
    fn inner(Self) -> unit;
}

trait Outer {
    fn outer(Self) -> unit;
}

impl Inner for int32 {
    fn inner(self: int32) -> unit { () }
}

impl[T: Inner] Outer for Vec[T] {
    fn outer(self: Vec[T]) -> unit { () }
}

fn require_outer[T: Outer](value: T) -> unit {
    ()
}

fn main() -> unit {
    let values = vec_new();
    require_outer(values)
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn deterministic_coercion_precedes_trait_inference() {
    let src = r#"
trait Mark {
    fn mark(Self) -> unit;
}

impl Mark for int32 {
    fn mark(self: int32) -> unit { () }
}

fn identity[T](value: T) -> T {
    value
}

fn require_mark[T: Mark](value: T) -> unit {
    ()
}

fn preserve[T: Mark](value: T) -> unit {
    let copied = identity(value);
    require_mark(copied)
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn unused_generic_bound_still_creates_an_obligation() {
    let src = r#"
trait Mark {
    fn mark(Self) -> unit;
}

fn require_unused[T: Mark]() -> unit {
    ()
}

fn main() -> unit {
    require_unused()
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
fn unconstrained_impl_type_parameter_is_rejected() {
    let src = r#"
trait Mark {
    fn mark(Self) -> unit;
}

impl[T] Mark for int32 {
    fn mark(self: int32) -> unit { () }
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics.iter().any(|line| line
            .contains("Implementation type parameter T is not constrained by type int32")),
        "{diagnostics:?}"
    );
}

#[test]
fn nested_impl_bound_must_be_satisfied() {
    let src = r#"
trait Mark {
    fn mark(Self) -> unit;
}

struct Wrap[T] {
    value: T,
}

impl[T: Mark] Mark for Wrap[T] {
    fn mark(self: Wrap[T]) -> unit { () }
}

fn require_mark[T: Mark](value: T) -> unit {
    ()
}

fn use_wrap(value: Wrap[int32]) -> unit {
    require_mark(value)
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("No instance found for trait Mark<Wrap[int32]>")),
        "{diagnostics:?}"
    );
}

#[test]
fn coherence_is_rechecked_after_all_impls_are_collected() {
    let src = r#"
trait Show {
    fn show(Self) -> string;
}

impl Hash for Ref[dyn Show] {
    fn hash(self: Ref[dyn Show]) -> uint64 { 0u64 }
}

impl Hash for dyn Show {
    fn hash(self: dyn Show) -> uint64 { 0u64 }
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("overlaps with implementation")),
        "{diagnostics:?}"
    );
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
fn generic_constraints_reject_overlapping_trait_impls_at_definition() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/hashmap_ref_dyn_hash_overlapping_impl/main.gom");
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
                    .any(|line| line.contains("overlaps with implementation")),
                "{diagnostics:?}"
            );
        }
        other => panic!("expected typer error, got {other:?}"),
    }
}

#[test]
fn recursive_blanket_trait_impl_bound_does_not_crash() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src/tests/crashers/recursive_blanket_trait_impl_bound/main.gom");
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
                    .any(|line| line.contains("No instance found for trait Foo<int32>")),
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
