use std::path::PathBuf;

use ast::ast::Uident;
use cst::cst::CstNode;
use parser::syntax::MySyntaxNode;

use crate::{env::Env, tast};

fn typecheck(src: &str) -> (tast::File, Env) {
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
    crate::typer::check_file(ast)
}

#[test]
fn collects_struct_definitions() {
    let src = r#"
struct Point {
    x: int,
    y: int,
}

struct Wrapper[T] {
    value: T,
}

fn consume_point(p: Point) -> unit { () }

fn consume_wrapper[T](value: Wrapper[T]) -> unit { () }
"#;

    let (_tast, env) = typecheck(src);

    let point = env
        .structs
        .get(&Uident::new("Point"))
        .expect("Point struct to be recorded");
    assert!(point.generics.is_empty());
    assert_eq!(point.fields.len(), 2);
    assert_eq!(point.fields[0].0.0, "x");
    assert_eq!(point.fields[0].1, tast::Ty::TInt);
    assert_eq!(point.fields[1].0.0, "y");
    assert_eq!(point.fields[1].1, tast::Ty::TInt);

    let wrapper = env
        .structs
        .get(&Uident::new("Wrapper"))
        .expect("Wrapper struct to be recorded");
    assert_eq!(wrapper.generics.len(), 1);
    assert_eq!(wrapper.generics[0].0, "T");
    assert_eq!(wrapper.fields.len(), 1);
    assert_eq!(wrapper.fields[0].0.0, "value");
    assert_eq!(
        wrapper.fields[0].1,
        tast::Ty::TParam {
            name: "T".to_string(),
        }
    );

    let wrapper_fn = env
        .funcs
        .get("consume_wrapper")
        .expect("function type to be recorded");
    if let tast::Ty::TFunc { params, ret_ty } = wrapper_fn {
        assert_eq!(params.len(), 1);
        assert_eq!(**ret_ty, tast::Ty::TUnit);
        let expected_param = tast::Ty::TApp {
            ty: Box::new(tast::Ty::TCon {
                name: "Wrapper".to_string(),
            }),
            args: vec![tast::Ty::TParam {
                name: "T".to_string(),
            }],
        };
        assert_eq!(params[0], expected_param);
    } else {
        panic!("expected consume_wrapper to have function type");
    }
}

#[test]
fn enum_variants_record_struct_types() {
    let src = r#"
struct Point {
    x: int,
    y: int,
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

    let (_tast, env) = typecheck(src);

    let shape = env
        .enums
        .get(&Uident::new("Shape"))
        .expect("Shape enum to be recorded");
    assert_eq!(shape.generics.len(), 1);
    assert_eq!(shape.generics[0].0, "T");
    assert_eq!(shape.variants.len(), 3);

    let dot = &shape.variants[0];
    assert_eq!(dot.0.0, "Dot");
    assert_eq!(dot.1.len(), 1);
    assert_eq!(
        dot.1[0],
        tast::Ty::TCon {
            name: "Point".to_string(),
        }
    );

    let wrapped = &shape.variants[1];
    assert_eq!(wrapped.0.0, "Wrapped");
    assert_eq!(wrapped.1.len(), 1);
    assert_eq!(
        wrapped.1[0],
        tast::Ty::TApp {
            ty: Box::new(tast::Ty::TCon {
                name: "Wrapper".to_string(),
            }),
            args: vec![tast::Ty::TParam {
                name: "T".to_string(),
            }],
        }
    );

    let origin = &shape.variants[2];
    assert_eq!(origin.0.0, "Origin");
    assert!(origin.1.is_empty());
}

#[test]
fn structs_and_enums_can_reference_each_other() {
    let src = r#"
struct Node {
    value: int,
    next: List,
}

enum List {
    Cons(Node),
    Nil,
}
"#;

    let (_tast, env) = typecheck(src);

    let node = env
        .structs
        .get(&Uident::new("Node"))
        .expect("Node struct to be recorded");
    assert_eq!(node.fields.len(), 2);
    assert_eq!(node.fields[1].0.0, "next");
    assert_eq!(
        node.fields[1].1,
        tast::Ty::TCon {
            name: "List".to_string(),
        }
    );

    let list = env
        .enums
        .get(&Uident::new("List"))
        .expect("List enum to be recorded");
    assert_eq!(list.variants.len(), 2);
    let cons = &list.variants[0];
    assert_eq!(cons.0.0, "Cons");
    assert_eq!(cons.1.len(), 1);
    assert_eq!(
        cons.1[0],
        tast::Ty::TCon {
            name: "Node".to_string(),
        }
    );
}

fn assert_typer_error(src: &str, expected_message: &str) {
    let (_tast, env) = typecheck(src);
    let messages: Vec<String> = env
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.message().to_string())
        .collect();
    assert!(
        messages
            .iter()
            .any(|message| message.contains(expected_message)),
        "expected diagnostics to contain `{expected_message}`, but were: {:?}",
        messages
    );
}

#[test]
fn struct_type_arity_mismatch_reports_error() {
    let src = r#"
struct Wrapper[T] {
    value: T,
}

fn bad(value: Wrapper[int, int]) -> unit { () }
"#;

    assert_typer_error(src, "Type Wrapper expects 1 type arguments, but got 2");
}

#[test]
fn unknown_type_constructor_reports_error() {
    let src = r#"
fn bad(p: Undefined) -> unit { () }
"#;

    assert_typer_error(src, "Unknown type constructor Undefined");
}

#[test]
fn unbound_type_parameter_reports_error() {
    let src = r#"
struct Wrapper[T] {
    value: T,
}

fn bad[T](value: Wrapper[U]) -> unit { () }
"#;

    assert_typer_error(src, "Unknown type constructor U");
}

#[test]
fn enum_struct_type_arity_mismatch_reports_error() {
    let src = r#"
struct Wrapper[T] {
    value: T,
}

enum Problem {
    Bad(Wrapper[int, int]),
}
"#;

    assert_typer_error(src, "Type Wrapper expects 1 type arguments, but got 2");
}

#[test]
fn enum_struct_unknown_type_constructor_reports_error() {
    let src = r#"
enum Problem {
    Bad(MissingStruct),
}
"#;

    assert_typer_error(src, "Unknown type constructor MissingStruct");
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

    assert_typer_error(src, "Unknown type constructor U");
}
