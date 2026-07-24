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

#[test]
fn generic_trait_applications_are_distinct() {
    let src = r#"
trait Convert[T] {
    fn convert(Self) -> T;
}

struct Token {}

impl Convert[int32] for Token {
    fn convert(self: Token) -> int32 { 7 }
}

impl Convert[string] for Token {
    fn convert(self: Token) -> string { "seven" }
}

fn convert_to[T, V: Convert[T]](value: V) -> T {
    value.convert()
}

fn main() -> unit {
    let number: int32 = convert_to(Token {});
    let text: string = convert_to(Token {});
    ()
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn expected_return_type_disambiguates_generic_trait_method() {
    let src = r#"
trait Convert[T] {
    fn convert(Self) -> T;
}

struct Token {}

impl Convert[int32] for Token {
    fn convert(self: Token) -> int32 { 7 }
}

impl Convert[string] for Token {
    fn convert(self: Token) -> string { "seven" }
}

fn main() -> unit {
    let number: int32 = (Token {}).convert();
    let text: string = (Token {}).convert();
    ()
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn trait_impl_methods_cannot_add_type_parameters() {
    let cases = [
        r#"
trait Value {
    fn value(Self) -> int32;
}

struct X {}

impl Value for X {
    fn value[Unused](self: X) -> int32 { 1 }
}
"#,
        r#"
trait Identity {
    fn identity(Self) -> int32;
}

struct X {}

impl Identity for X {
    fn identity[T](self: X) -> T { self }
}
"#,
        r#"
trait Value {
    fn value(Self) -> int32;
}

struct X {}

impl Value for X {
    fn value[T: Show](self: X) -> int32 { 1 }
}
"#,
    ];

    for src in cases {
        let diagnostics = diagnostic_lines(src);
        assert!(
            diagnostics.iter().any(|line| {
                line.contains(
                    "Trait method implementation Value::value cannot declare type parameters",
                ) || line.contains(
                    "Trait method implementation Identity::identity cannot declare type parameters",
                )
            }),
            "{diagnostics:?}"
        );
    }
}

#[test]
fn overlapping_generic_trait_applications_are_rejected() {
    let src = r#"
trait Convert[T] {
    fn convert(Self) -> T;
}

struct Box[T] { value: T }

impl[T] Convert[T] for Box[T] {
    fn convert(self: Box[T]) -> T { self.value }
}

impl Convert[int32] for Box[int32] {
    fn convert(self: Box[int32]) -> int32 { self.value }
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
fn generic_trait_arity_is_checked() {
    let src = r#"
trait Convert[T] {
    fn convert(Self) -> T;
}

struct Token {}

fn missing[T: Convert](value: T) -> unit { () }

impl Convert[int32, string] for Token {
    fn convert(self: Token) -> int32 { 7 }
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("Trait Convert expects 1 type arguments, but got 0")),
        "{diagnostics:?}"
    );
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("Trait Convert expects 1 type arguments, but got 2")),
        "{diagnostics:?}"
    );
}

#[test]
fn generic_trait_parameters_and_signatures_are_validated() {
    let src = r#"
trait Duplicate[T, T] {
    fn value(Self) -> T;
}

trait Broken[T] {
    fn value(Self) -> Missing;
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
    fn convert(Self) -> T;
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
    fn convert(Self) -> T;
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

#[test]
fn generic_traits_are_rejected_as_dyn_types() {
    let src = r#"
trait Convert[T] {
    fn convert(Self) -> T;
}

fn consume(value: dyn Convert) -> unit { () }
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("Generic trait Convert cannot be used as dyn")),
        "{diagnostics:?}"
    );
}

#[test]
fn iterator_associated_item_prevents_conflicting_impls() {
    let src = r#"
struct Values {}

impl Iterator for Values {
    type Item = int32;
    fn next(self: Values) -> Option[int32] { Option::None }
}

impl Iterator for Values {
    type Item = string;
    fn next(self: Values) -> Option[string] { Option::None }
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("implementation for Values is already defined")),
        "{diagnostics:?}"
    );
}

#[test]
fn for_loop_uses_iterator_associated_item_type() {
    let src = r#"
struct Values {}

impl Iterator for Values {
    type Item = int32;
    fn next(self: Values) -> Option[int32] { Option::None }
}

fn consume_int(value: int32) -> unit { () }

fn main() -> unit {
    for value in (Values {}) {
        consume_int(value);
    };
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn for_loop_accepts_custom_into_iterator() {
    let src = r#"
struct Values {
    values: Vec[int32],
}

impl IntoIterator for Values {
    type Item = int32;
    type IntoIter = FnIterator[int32];

    fn into_iter(self: Values) -> FnIterator[int32] {
        self.values.iter()
    }
}

fn consume_int(value: int32) -> unit { () }

fn main() -> unit {
    for value in (Values { values: Vec::new() }) {
        consume_int(value);
    };
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn generic_into_iterator_bound_implies_iterator_for_into_iter() {
    let src = r#"
fn sum[S: IntoIterator](source: S) -> int32
where
    S::Item == int32,
{
    let total = Ref::new(0);
    for value in source {
        total.set(total.get() + value);
    };
    total.get()
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn into_iterator_impl_rejects_inconsistent_item() {
    let src = r#"
struct Values {}

impl IntoIterator for Values {
    type Item = string;
    type IntoIter = FnIterator[int32];

    fn into_iter(self: Values) -> FnIterator[int32] {
        range(0, 1)
    }
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics.iter().any(|line| line
            .contains("does not satisfy declared requirement string == FnIterator[int32]::Item")),
        "{diagnostics:?}"
    );
}

#[test]
fn into_iterator_impl_rejects_non_iterator_into_iter() {
    let src = r#"
struct Values {}

impl IntoIterator for Values {
    type Item = int32;
    type IntoIter = int32;

    fn into_iter(self: Values) -> int32 {
        0
    }
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics.iter().any(|line| {
            line.contains("Associated type IntoIter = int32 does not satisfy bound Iterator")
        }),
        "{diagnostics:?}"
    );
}

#[test]
fn where_predicate_accepts_constructed_trait_receiver() {
    let src = r#"
trait Render {
    fn render(Self) -> string;
}

fn render_all[T](values: Vec[T]) -> string where Vec[T]: Render {
    values.render()
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn where_type_equality_is_available_in_generic_body() {
    let src = r#"
fn convert[T, U](value: T) -> U where T == U {
    value
}

fn main() -> unit {
    let value: int32 = convert(7);
    ()
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn where_constructed_bound_is_checked_at_call_site() {
    let src = r#"
trait Render {
    fn render(Self) -> string;
}

fn require_render[T](values: Vec[T]) -> unit where Vec[T]: Render {
    ()
}

fn main() -> unit {
    let values: Vec[int32] = vec_new();
    require_render(values)
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("No instance found for trait Render<Vec[int32]>")),
        "{diagnostics:?}"
    );
}

#[test]
fn where_type_equality_is_checked_at_call_site() {
    let src = r#"
fn convert[T, U](value: T) -> U where T == U {
    value
}

fn main() -> unit {
    let value: string = convert(7);
    ()
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("Type mismatch: expected int32, found string")),
        "{diagnostics:?}"
    );
}

#[test]
fn impl_where_predicate_controls_trait_selection() {
    let accepted = r#"
trait Ready {
    fn ready(Self) -> unit;
}

trait Selected {
    fn selected(Self) -> unit;
}

struct Wrap[T] { value: T }

impl Ready for Vec[int32] {
    fn ready(self: Vec[int32]) -> unit { () }
}

impl[T] Selected for Wrap[T] where Vec[T]: Ready {
    fn selected(self: Wrap[T]) -> unit { () }
}

fn require[T: Selected](value: T) -> unit { () }

fn main() -> unit {
    require(Wrap { value: 7 })
}
"#;
    let rejected = r#"
trait Ready {
    fn ready(Self) -> unit;
}

trait Selected {
    fn selected(Self) -> unit;
}

struct Wrap[T] { value: T }

impl Ready for Vec[int32] {
    fn ready(self: Vec[int32]) -> unit { () }
}

impl[T] Selected for Wrap[T] where Vec[T]: Ready {
    fn selected(self: Wrap[T]) -> unit { () }
}

fn require[T: Selected](value: T) -> unit { () }

fn main() -> unit {
    require(Wrap { value: "no" })
}
"#;

    let accepted_diagnostics = diagnostic_lines(accepted);
    assert!(accepted_diagnostics.is_empty(), "{accepted_diagnostics:?}");
    let rejected_diagnostics = diagnostic_lines(rejected);
    assert!(
        rejected_diagnostics
            .iter()
            .any(|line| line.contains("No instance found for trait Selected<Wrap[string]>")),
        "{rejected_diagnostics:?}"
    );
}

#[test]
fn impl_equality_predicate_restricts_application() {
    let src = r#"
trait Selected {
    fn selected(Self) -> unit;
}

struct Wrap[T] { value: T }

impl[T] Selected for Wrap[T] where T == int32 {
    fn selected(self: Wrap[T]) -> unit { () }
}

fn require[T: Selected](value: T) -> unit { () }

fn main() -> unit {
    require(Wrap { value: "no" })
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("No instance found for trait Selected<Wrap[string]>")),
        "{diagnostics:?}"
    );
}

#[test]
fn structural_where_equality_relates_nested_type_parameters() {
    let src = r#"
fn convert[T, U](values: Vec[T]) -> Vec[U] where Vec[T] == Vec[U] {
    values
}

fn main() -> unit {
    let values: Vec[int32] = vec_new();
    let converted: Vec[int32] = convert(values);
    ()
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn equality_predicate_transfers_trait_bound() {
    let src = r#"
trait Render {
    fn render(Self) -> string;
}

fn render_equal[T: Render, U](value: U) -> string where T == U {
    value.render()
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn associated_type_impl_requires_complete_known_unique_bindings() {
    let duplicate_declaration = r#"
trait Source {
    type Item;
    type Item;
    fn get(Self) -> Self::Item;
}
"#;
    let missing = r#"
trait Source {
    type Item;
    fn get(Self) -> Self::Item;
}

struct Value {}

impl Source for Value {
    fn get(self: Value) -> int32 { 1 }
}
"#;
    let unknown = r#"
trait Source {
    type Item;
    fn get(Self) -> Self::Item;
}

struct Value {}

impl Source for Value {
    type Item = int32;
    type Extra = string;
    fn get(self: Value) -> int32 { 1 }
}
"#;
    let duplicate = r#"
trait Source {
    type Item;
    fn get(Self) -> Self::Item;
}

struct Value {}

impl Source for Value {
    type Item = int32;
    type Item = int32;
    fn get(self: Value) -> int32 { 1 }
}
"#;

    let duplicate_declaration_diagnostics = diagnostic_lines(duplicate_declaration);
    assert!(
        duplicate_declaration_diagnostics
            .iter()
            .any(|line| line.contains("associated type Item is defined multiple times")),
        "{duplicate_declaration_diagnostics:?}"
    );
    let missing_diagnostics = diagnostic_lines(missing);
    assert!(
        missing_diagnostics
            .iter()
            .any(|line| line.contains("is missing associated type Item")),
        "{missing_diagnostics:?}"
    );
    let unknown_diagnostics = diagnostic_lines(unknown);
    assert!(
        unknown_diagnostics
            .iter()
            .any(|line| line.contains("Associated type Extra is not declared")),
        "{unknown_diagnostics:?}"
    );
    let duplicate_diagnostics = diagnostic_lines(duplicate);
    assert!(
        duplicate_diagnostics
            .iter()
            .any(|line| line.contains("Associated type Item is bound multiple times")),
        "{duplicate_diagnostics:?}"
    );
}

#[test]
fn associated_type_binding_must_match_trait_method_signature() {
    let src = r#"
trait Source {
    type Item;
    fn get(Self) -> Self::Item;
}

struct Value {}

impl Source for Value {
    type Item = int32;
    fn get(self: Value) -> string { "wrong" }
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("expected return type int32 but found string")),
        "{diagnostics:?}"
    );
}

#[test]
fn associated_type_bound_is_checked_at_impl_definition() {
    let src = r#"
trait Mark {
    fn mark(Self) -> unit;
}

trait Source {
    type Item: Mark;
    fn get(Self) -> Self::Item;
}

struct Value {}
struct Item {}

impl Source for Value {
    type Item = Item;
    fn get(self: Value) -> Item { Item {} }
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics.iter().any(|line| {
            line.contains("Associated type Item = Item does not satisfy bound Mark")
        }),
        "{diagnostics:?}"
    );
}

#[test]
fn associated_type_projection_requires_one_defining_bound() {
    let missing = r#"
fn project[T](value: T) -> T::Item {
    value
}
"#;
    let ambiguous = r#"
trait Left {
    type Item;
}

trait Right {
    type Item;
}

fn project[T: Left + Right](value: T) -> T::Item {
    value
}
"#;

    let missing_diagnostics = diagnostic_lines(missing);
    assert!(
        missing_diagnostics
            .iter()
            .any(|line| line.contains("is not provided by a trait bound")),
        "{missing_diagnostics:?}"
    );
    let ambiguous_diagnostics = diagnostic_lines(ambiguous);
    assert!(
        ambiguous_diagnostics
            .iter()
            .any(|line| line.contains("is ambiguous between Left, Right")),
        "{ambiguous_diagnostics:?}"
    );
}

#[test]
fn associated_type_cycles_are_rejected() {
    let src = r#"
trait Pair {
    type First;
    type Second;
    fn touch(Self) -> unit;
}

struct Value {}

impl Pair for Value {
    type First = Self::Second;
    type Second = Self::First;
    fn touch(self: Value) -> unit { () }
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("has a cyclic definition")),
        "{diagnostics:?}"
    );
}

#[test]
fn supertrait_impl_is_required_at_definition() {
    let src = r#"
trait Parent {
    fn parent(Self) -> string;
}

trait Child: Parent {
    fn child(Self) -> string;
}

struct Value {}

impl Child for Value {
    fn child(self: Value) -> string { "child" }
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("requires supertrait Parent")),
        "{diagnostics:?}"
    );
}

#[test]
fn trait_parameter_bounds_are_required_at_impl_definition() {
    let src = r#"
trait Mark {
    fn mark(Self) -> unit;
}

trait Container[T: Mark] {
    fn value(Self) -> T;
}

struct Item {}
struct Box {}

impl Container[Item] for Box {
    fn value(self: Box) -> Item { Item {} }
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics
            .iter()
            .any(|line| { line.contains("does not satisfy declared requirement Item: Mark") }),
        "{diagnostics:?}"
    );
}

#[test]
fn inherited_method_name_conflicts_remain_ambiguous() {
    let src = r#"
trait Left {
    fn name(Self) -> string;
}

trait Right {
    fn name(Self) -> string;
}

trait Both: Left + Right {
    fn both(Self) -> unit;
}

fn name[T: Both](value: T) -> string {
    value.name()
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("Ambiguous method name")),
        "{diagnostics:?}"
    );
}

#[test]
fn supertrait_cycles_are_rejected() {
    let src = r#"
trait First: Second {
    fn first(Self) -> unit;
}

trait Second: First {
    fn second(Self) -> unit;
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("Supertrait cycle detected")),
        "{diagnostics:?}"
    );
}

#[test]
fn forward_and_diamond_supertraits_are_supported() {
    let src = r#"
trait Child: Left + Right {
    fn child(Self) -> unit;
}

trait Left: Base {
    fn left(Self) -> unit;
}

trait Right: Base {
    fn right(Self) -> unit;
}

trait Base {
    fn base(Self) -> string;
}

fn inherited[T: Child](value: T) -> string {
    value.base()
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn trait_coverage_generic_parameter_bound_is_implied() {
    let src = r#"
trait Mark {
    fn mark(Self) -> unit;
}

trait Container[T: Mark] {
    fn value(Self) -> T;
}

fn require_mark[T: Mark](value: T) -> unit { () }

fn consume[T, C: Container[T]](container: C) -> unit {
    require_mark(Container::[T]::value(container))
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn trait_coverage_declaration_where_predicate_is_implied() {
    let src = r#"
trait Ready {
    fn ready(Self) -> unit;
}

trait Service[T] where T: Ready {
    fn serve(Self, T) -> unit;
}

fn require_ready[T: Ready](value: T) -> unit { () }

fn consume[T, S: Service[T]](value: T) -> unit {
    require_ready(value)
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn trait_coverage_associated_type_bound_is_implied() {
    let src = r#"
trait Mark {
    fn mark(Self) -> unit;
}

trait Source {
    type Item: Mark;
    fn get(Self) -> Self::Item;
}

fn require_mark[T: Mark](value: T) -> unit { () }

fn consume[S: Source](source: S) -> unit {
    require_mark(Source::get(source))
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn trait_coverage_supertrait_associated_type_is_projectable() {
    let src = r#"
trait Child: Parent {
    fn child(Self) -> unit;
}

trait Parent {
    type Item;
    fn get(Self) -> Self::Item;
}

fn get_from_child[C: Child](value: C) -> C::Item {
    Parent::get(value)
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn trait_coverage_projection_equality_transfers_bound() {
    let src = r#"
trait Mark {
    fn mark(Self) -> unit;
}

trait Source {
    type Item;
    fn get(Self) -> Self::Item;
}

fn require_mark[T: Mark](value: T) -> unit { () }

fn consume[A: Source, B: Source](left: A, right: B) -> unit
where
    A::Item == B::Item,
    B::Item: Mark,
{
    let _ = right;
    require_mark(Source::get(left))
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn trait_coverage_constrained_blanket_is_disjoint_without_bound() {
    let src = r#"
trait Mark {
    fn mark(Self) -> unit;
}

trait Label {
    fn label(Self) -> string;
}

struct Box[T] {
    value: T,
}

impl[T: Mark] Label for Box[T] {
    fn label(self: Box[T]) -> string { "marked" }
}

impl Label for Box[string] {
    fn label(self: Box[string]) -> string { self.value }
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn trait_coverage_constrained_blanket_overlaps_when_bound_holds() {
    let src = r#"
trait Mark {
    fn mark(Self) -> unit;
}

trait Label {
    fn label(Self) -> string;
}

struct Box[T] {
    value: T,
}

impl Mark for string {
    fn mark(self: string) -> unit { () }
}

impl[T: Mark] Label for Box[T] {
    fn label(self: Box[T]) -> string { "marked" }
}

impl Label for Box[string] {
    fn label(self: Box[string]) -> string { self.value }
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
fn trait_coverage_constrained_blanket_method_is_unavailable_without_bound() {
    let src = r#"
trait Mark {
    fn mark(Self) -> unit;
}

trait Extra {
    fn extra(Self) -> int32;
}

impl[T: Mark] Extra for T {
    fn extra(self: T) -> int32 { 1 }
}

fn main() -> unit {
    let _ = 1.extra();
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("Method extra not found for type int32")),
        "{diagnostics:?}"
    );
}

#[test]
fn trait_coverage_constrained_blanket_method_is_available_with_bound() {
    let src = r#"
trait Mark {
    fn mark(Self) -> unit;
}

trait Extra {
    fn extra(Self) -> int32;
}

impl Mark for int32 {
    fn mark(self: int32) -> unit { () }
}

impl[T: Mark] Extra for T {
    fn extra(self: T) -> int32 { 1 }
}

fn main() -> unit {
    let value: int32 = 1.extra();
    let _ = value;
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn trait_coverage_generic_applications_have_distinct_associated_types() {
    let src = r#"
trait Convert[T] {
    type Output;
    fn convert(Self) -> Self::Output;
}

struct Value {}

impl Convert[int32] for Value {
    type Output = string;
    fn convert(self: Value) -> string { "int" }
}

impl Convert[string] for Value {
    type Output = int32;
    fn convert(self: Value) -> int32 { 7 }
}

fn main() -> unit {
    let text: string = Convert::[int32]::convert(Value {});
    let number: int32 = Convert::[string]::convert(Value {});
    let _ = (text, number);
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn projected_trait_application_impl_is_selectable() {
    let src = r#"
trait Source {
    type Item;
    fn get(Self) -> Self::Item;
}

trait Pick[T] {
    fn pick(Self) -> T;
}

impl[S: Source] Pick[S::Item] for S {
    fn pick(self: S) -> S::Item { Source::get(self) }
}

struct Value { value: int32 }

impl Source for Value {
    type Item = int32;
    fn get(self: Value) -> int32 { self.value }
}

fn main() -> unit {
    let value: int32 = (Value { value: 1 }).pick();
    let _ = value;
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn symbolic_projection_equality_selects_single_impl() {
    let src = r#"
trait Source {
    type Item;
    fn get(Self) -> Self::Item;
}

trait Pick[T] {
    fn pick(Self) -> T;
}

impl[S: Source, T] Pick[T] for S where S::Item == T {
    fn pick(self: S) -> T { Source::get(self) }
}

fn invoke[T, S: Pick[T]](source: S) -> T {
    source.pick()
}

fn generic_pick[S: Source](source: S) -> S::Item {
    invoke(source)
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn local_projection_type_positions_are_resolved() {
    let src = r#"
trait Source {
    type Item;
}

trait Accept[T] {
    fn accept(Self) -> T;
}

fn copy[S: Source + Accept[S::Item]](source: S) -> S::Item {
    let value: S::Item = Accept::[S::Item]::accept(source);
    let identity = |item: S::Item| item;
    identity(value)
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn method_arguments_disambiguate_generic_trait_applications() {
    let src = r#"
trait Put[T] {
    fn put(Self, T) -> string;
}

struct Value {}

impl Put[int32] for Value {
    fn put(self: Value, value: int32) -> string { value.to_string() }
}

impl Put[string] for Value {
    fn put(self: Value, value: string) -> string { value }
}

fn main() -> unit {
    let number = (Value {}).put(1);
    let text = (Value {}).put("text");
    let _ = (number, text);
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn associated_outputs_disambiguate_generic_trait_methods() {
    let src = r#"
trait Convert[T] {
    type Output;
    fn convert(Self) -> Self::Output;
}

struct Value {}

impl Convert[int32] for Value {
    type Output = string;
    fn convert(self: Value) -> string { "int" }
}

impl Convert[string] for Value {
    type Output = int32;
    fn convert(self: Value) -> int32 { 7 }
}

fn main() -> unit {
    let text: string = (Value {}).convert();
    let number: int32 = (Value {}).convert();
    let _ = (text, number);
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn trait_self_predicates_are_resolved_and_enforced() {
    let accepted = r#"
trait Source[T] where Self::Item == T {
    type Item;
    fn get(Self) -> Self::Item;
}

struct Value { value: int32 }

impl Source[int32] for Value {
    type Item = int32;
    fn get(self: Value) -> int32 { self.value }
}

fn read[T, S: Source[T]](source: S) -> T {
    Source::[T]::get(source)
}
"#;
    let rejected = r#"
trait Source[T] where Self::Item == T {
    type Item;
    fn get(Self) -> Self::Item;
}

struct Value {}

impl Source[string] for Value {
    type Item = int32;
    fn get(self: Value) -> int32 { 1 }
}
"#;

    let accepted_diagnostics = diagnostic_lines(accepted);
    assert!(accepted_diagnostics.is_empty(), "{accepted_diagnostics:?}");
    let rejected_diagnostics = diagnostic_lines(rejected);
    assert!(
        rejected_diagnostics
            .iter()
            .any(|line| line.contains("does not satisfy declared requirement int32 == string")),
        "{rejected_diagnostics:?}"
    );
}

#[test]
fn projection_only_impl_parameter_is_rejected() {
    let src = r#"
trait Source {
    type Item;
}

trait Mark {
    fn mark(Self) -> unit;
}

impl[S: Source] Mark for S::Item {
    fn mark(self: Self) -> unit { () }
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics.iter().any(|line| {
            line.contains("Implementation type parameter S is not constrained by type S::Item")
        }),
        "{diagnostics:?}"
    );
}
