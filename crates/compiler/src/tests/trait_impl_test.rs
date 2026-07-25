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
fn overlapping_generic_and_concrete_impls_are_rejected_at_definition() {
    let src = r#"
trait Label {
    fn label(self: Self) -> string;
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
    fn render(self: Self) -> string;
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
    fn text(self: Self) -> string;
}

trait Beta {
    fn text(self: Self) -> string;
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
fn ambiguous_trait_goal_is_retried_after_unique_inference() {
    let src = r#"
trait First {
    fn first(self: Self) -> unit;
}

trait Second {
    fn second(self: Self) -> unit;
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
fn unconstrained_impl_type_parameter_is_rejected() {
    let src = r#"
trait Mark {
    fn mark(self: Self) -> unit;
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
fn trait_impl_methods_cannot_add_type_parameters() {
    let cases = [
        r#"
trait Value {
    fn value(self: Self) -> int32;
}

struct X {}

impl Value for X {
    fn value[Unused](self: X) -> int32 { 1 }
}
"#,
        r#"
trait Identity {
    fn identity(self: Self) -> int32;
}

struct X {}

impl Identity for X {
    fn identity[T](self: X) -> T { self }
}
"#,
        r#"
trait Value {
    fn value(self: Self) -> int32;
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
    fn convert(self: Self) -> T;
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
    fn convert(self: Self) -> T;
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

#[test]
fn generic_traits_are_rejected_as_dyn_types() {
    let src = r#"
trait Convert[T] {
    fn convert(self: Self) -> T;
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
fn dyn_trait_parent_method_ambiguity_is_reported() {
    let src = r#"
trait Left {
    fn label(self: Self) -> string;
}

trait Right {
    fn label(self: Self) -> string;
}

trait Both: Left + Right {
    fn own(self: Self) -> string;
}

fn render(value: dyn Both) -> string {
    value.label()
}
"#;

    let diagnostics = diagnostic_lines(src);
    assert!(
        diagnostics
            .iter()
            .any(|line| line.contains("Ambiguous method") && line.contains("label")),
        "{diagnostics:?}"
    );
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
fn associated_type_bound_is_checked_at_impl_definition() {
    let src = r#"
trait Mark {
    fn mark(self: Self) -> unit;
}

trait Source {
    type Item: Mark;
    fn get(self: Self) -> Self::Item;
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
    fn touch(self: Self) -> unit;
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
    fn parent(self: Self) -> string;
}

trait Child: Parent {
    fn child(self: Self) -> string;
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
    fn mark(self: Self) -> unit;
}

trait Container[T: Mark] {
    fn value(self: Self) -> T;
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
fn supertrait_cycles_are_rejected() {
    let src = r#"
trait First: Second {
    fn first(self: Self) -> unit;
}

trait Second: First {
    fn second(self: Self) -> unit;
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
fn trait_coverage_constrained_blanket_overlaps_when_bound_holds() {
    let src = r#"
trait Mark {
    fn mark(self: Self) -> unit;
}

trait Label {
    fn label(self: Self) -> string;
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
    fn mark(self: Self) -> unit;
}

trait Extra {
    fn extra(self: Self) -> int32;
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
            .any(|line| line.contains("Method extra not found for type int")),
        "{diagnostics:?}"
    );
}

#[test]
fn trait_self_predicates_are_resolved_and_enforced() {
    let accepted = r#"
trait Source[T] where Self::Item = T {
    type Item;
    fn get(self: Self) -> Self::Item;
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
trait Source[T] where Self::Item = T {
    type Item;
    fn get(self: Self) -> Self::Item;
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
    fn mark(self: Self) -> unit;
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
