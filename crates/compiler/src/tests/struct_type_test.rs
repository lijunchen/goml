use std::path::PathBuf;

use cst::cst::CstNode;
use expect_test::{Expect, expect};
use parser::{Diagnostics, syntax::MySyntaxNode};

use crate::{
    env::{GlobalTypeEnv, StructDef},
    tast,
};

fn typecheck(src: &str) -> (tast::File, GlobalTypeEnv, Diagnostics) {
    let path = PathBuf::from("test_structs.gom");
    let parsed = parser::parse(&path, src);
    if parsed.has_errors() {
        panic!("Parse errors:\n{}", parsed.format_errors(src).join("\n"));
    }
    let root = MySyntaxNode::new_root(parsed.green_node);
    let cst = cst::cst::File::cast(root).expect("failed to cast syntax tree");
    let ast = ast::lower::lower(cst)
        .into_result()
        .expect("failed to lower to AST");
    let (fir, fir_table, mut fir_diagnostics) = crate::fir::lower_to_fir(ast);
    let (tast, genv, mut diagnostics) = crate::typer::check_file(fir, fir_table);
    diagnostics.append(&mut fir_diagnostics);
    (tast, genv, diagnostics)
}

fn snapshot_structs_and_enums(point: &StructDef, wrapper: &StructDef) -> String {
    let mut lines = Vec::new();
    lines.push(format!("Point.generics={:?}", point.generics));
    lines.push(format!("Point.fields={:?}", point.fields));
    lines.push(format!("Wrapper.generics={:?}", wrapper.generics));
    lines.push(format!("Wrapper.fields={:?}", wrapper.fields));
    lines.join("\n")
}

fn expect_typer_error(src: &str, expected: Expect) {
    let (_tast, _genv, diagnostics) = typecheck(src);
    let messages: Vec<String> = diagnostics
        .iter()
        .map(|diagnostic| diagnostic.message().to_string())
        .collect();
    expected.assert_debug_eq(&messages);
}

#[test]
fn collects_struct_definitions() {
    let src = r#"
struct Point {
    x: int32,
    y: int32,
}

struct Wrapper[T] {
    value: T,
}

fn consume_point(p: Point) -> unit { () }

fn consume_wrapper[T](value: Wrapper[T]) -> unit { () }
"#;

    let (_tast, genv, _diagnostics) = typecheck(src);

    let point = genv
        .structs()
        .get(&tast::TastIdent::new("Point"))
        .expect("Point struct to be recorded");
    let wrapper = genv
        .structs()
        .get(&tast::TastIdent::new("Wrapper"))
        .expect("Wrapper struct to be recorded");
    let wrapper_fn = genv
        .value_env
        .funcs
        .get("consume_wrapper")
        .expect("function type to be recorded");
    let mut lines = Vec::new();
    lines.push(snapshot_structs_and_enums(point, wrapper));
    lines.push(format!("consume_wrapper={:?}", wrapper_fn.ty));

    expect![[r#"
        Point.generics=[]
        Point.fields=[(TastIdent("x"), TInt32), (TastIdent("y"), TInt32)]
        Wrapper.generics=[TastIdent("T")]
        Wrapper.fields=[(TastIdent("value"), TParam(T))]
        consume_wrapper=TFunc([TApp(TStruct(Wrapper), [TParam(T)])], TUnit)"#]]
    .assert_eq(&lines.join("\n"));
}

#[test]
fn enum_variants_record_struct_types() {
    let src = r#"
struct Point {
    x: int32,
    y: int32,
}

struct Wrapper[U] {
    value: U,
}

enum Shape[T] {
    Dot(Point),
    Wrapped(Wrapper[T]),
    Origin,
}
"#;

    let (_tast, genv, _diagnostics) = typecheck(src);

    let shape = genv
        .enums()
        .get(&tast::TastIdent::new("Shape"))
        .expect("Shape enum to be recorded");
    let mut lines = Vec::new();
    lines.push(format!("Shape.generics={:?}", shape.generics));
    lines.push(format!("Shape.variants={:?}", shape.variants));

    expect![[r#"
        Shape.generics=[TastIdent("T")]
        Shape.variants=[(TastIdent("Dot"), [TStruct(Point)]), (TastIdent("Wrapped"), [TApp(TStruct(Wrapper), [TParam(T)])]), (TastIdent("Origin"), [])]"#]]
    .assert_eq(&lines.join("\n"));
}

#[test]
fn structs_and_enums_can_reference_each_other() {
    let src = r#"
struct Node {
    value: int32,
    next: List,
}

enum List {
    Cons(Node),
    Nil,
}
"#;

    let (_tast, genv, _diagnostics) = typecheck(src);

    let node = genv
        .structs()
        .get(&tast::TastIdent::new("Node"))
        .expect("Node struct to be recorded");
    let list = genv
        .enums()
        .get(&tast::TastIdent::new("List"))
        .expect("List enum to be recorded");
    let mut lines = Vec::new();
    lines.push(format!("Node.fields={:?}", node.fields));
    lines.push(format!("List.variants={:?}", list.variants));

    expect![[r#"
        Node.fields=[(TastIdent("value"), TInt32), (TastIdent("next"), TEnum(List))]
        List.variants=[(TastIdent("Cons"), [TStruct(Node)]), (TastIdent("Nil"), [])]"#]]
    .assert_eq(&lines.join("\n"));
}

#[test]
fn closure_infers_param_and_return_types() {
    let src = r#"
fn use_closure(x: int32) -> int32 {
    let add = |y| y + x;
    x
}
"#;

    let (tast, _env, _diagnostics) = typecheck(src);

    let func = match &tast.toplevels[0] {
        tast::Item::Fn(f) => f,
        _ => panic!("expected function item"),
    };

    let closure_expr = match &func.body {
        tast::Expr::EBlock { exprs, .. } => match &exprs[0] {
            tast::Expr::ELet { value, .. } => value,
            _ => panic!("expected let binding in block"),
        },
        _ => panic!("expected block in function body"),
    };

    let tast::Expr::EClosure {
        params,
        body,
        ty,
        captures: _,
    } = &**closure_expr
    else {
        panic!("expected closure expression");
    };

    let mut lines = Vec::new();
    lines.push(format!("params={:?}", params));
    lines.push(format!("body={:?}", body));
    lines.push(format!("ty={:?}", ty));

    expect![[r#"
        params=[ClosureParam { name: "y/1", ty: TInt32, astptr: Some(SyntaxNodePtr { kind: CLOSURE_PARAM, range: 52..53 }) }]
        body=EBinary { op: Add, lhs: EVar { name: "y/1", ty: TInt32, astptr: Some(SyntaxNodePtr { kind: EXPR_IDENT, range: 55..57 }) }, rhs: EVar { name: "x/0", ty: TInt32, astptr: Some(SyntaxNodePtr { kind: EXPR_IDENT, range: 59..60 }) }, ty: TInt32, resolution: Builtin }
        ty=TFunc([TInt32], TInt32)"#]]
    .assert_eq(&lines.join("\n"));
}

#[test]
fn closure_parameter_annotations_use_enclosing_generics() {
    let src = r#"
fn wrap[T](value: T) -> T {
    let id = |x: T| x;
    value
}
"#;

    let (tast, _env, _diagnostics) = typecheck(src);

    let func = match &tast.toplevels[0] {
        tast::Item::Fn(f) => f,
        _ => panic!("expected function item"),
    };

    let closure_expr = match &func.body {
        tast::Expr::EBlock { exprs, .. } => match &exprs[0] {
            tast::Expr::ELet { value, .. } => value,
            _ => panic!("expected let binding in block"),
        },
        _ => panic!("expected block in function body"),
    };

    let tast::Expr::EClosure { params, ty, .. } = &**closure_expr else {
        panic!("expected closure expression");
    };

    let mut lines = Vec::new();
    lines.push(format!("params={:?}", params));
    lines.push(format!("ty={:?}", ty));

    expect![[r#"
        params=[ClosureParam { name: "x/1", ty: TParam(T), astptr: Some(SyntaxNodePtr { kind: CLOSURE_PARAM, range: 43..47 }) }]
        ty=TFunc([TParam(T)], TParam(T))"#]]
    .assert_eq(&lines.join("\n"));
}

#[test]
fn struct_type_arity_mismatch_reports_error() {
    let src = r#"
struct Wrapper[T] {
    value: T,
}

fn bad(value: Wrapper[int32, int32]) -> unit { () }
"#;

    expect_typer_error(
        src,
        expect![[r#"
            [
                "Type Wrapper expects 1 type arguments, but got 2",
            ]
        "#]],
    );
}

#[test]
fn unknown_type_constructor_reports_error() {
    let src = r#"
fn bad(p: Undefined) -> unit { () }
"#;

    expect_typer_error(
        src,
        expect![[r#"
            [
                "Unknown type constructor Undefined",
            ]
        "#]],
    );
}

#[test]
fn unbound_type_parameter_reports_error() {
    let src = r#"
struct Wrapper[T] {
    value: T,
}

fn bad[T](value: Wrapper[U]) -> unit { () }
"#;

    expect_typer_error(
        src,
        expect![[r#"
            [
                "Unknown type constructor U",
            ]
        "#]],
    );
}

#[test]
fn enum_struct_type_arity_mismatch_reports_error() {
    let src = r#"
struct Wrapper[T] {
    value: T,
}

enum Problem {
    Bad(Wrapper[int32, int32]),
}
"#;

    expect_typer_error(
        src,
        expect![[r#"
            [
                "Type Wrapper expects 1 type arguments, but got 2",
            ]
        "#]],
    );
}

#[test]
fn enum_struct_unknown_type_constructor_reports_error() {
    let src = r#"
enum Problem {
    Bad(MissingStruct),
}
"#;

    expect_typer_error(
        src,
        expect![[r#"
            [
                "Unknown type constructor MissingStruct",
            ]
        "#]],
    );
}

#[test]
fn enum_struct_unbound_type_parameter_reports_error() {
    let src = r#"
struct Wrapper[T] {
    value: T,
}

enum Problem[T] {
    Bad(Wrapper[U]),
}
"#;

    expect_typer_error(
        src,
        expect![[r#"
            [
                "Unknown type constructor U",
            ]
        "#]],
    );
}
