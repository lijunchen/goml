use std::path::PathBuf;

use ast::ast::Ident;
use cst::cst::CstNode;
use parser::{Diagnostics, syntax::MySyntaxNode};

use crate::{env::GlobalTypeEnv, tast};

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
    crate::typer::check_file(ast)
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
        .get(&Ident::new("Point"))
        .expect("Point struct to be recorded");
    assert!(point.generics.is_empty());
    assert_eq!(point.fields.len(), 2);
    assert_eq!(point.fields[0].0.0, "x");
    assert_eq!(point.fields[0].1, tast::Ty::TInt32);
    assert_eq!(point.fields[1].0.0, "y");
    assert_eq!(point.fields[1].1, tast::Ty::TInt32);

    let wrapper = genv
        .structs()
        .get(&Ident::new("Wrapper"))
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

    let wrapper_fn = genv
        .value_env.funcs
        .get("consume_wrapper")
        .expect("function type to be recorded");
    if let tast::Ty::TFunc { params, ret_ty } = &wrapper_fn.ty {
        assert_eq!(params.len(), 1);
        assert_eq!(**ret_ty, tast::Ty::TUnit);
        let expected_param = tast::Ty::TApp {
            ty: Box::new(tast::Ty::TStruct {
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
        .get(&Ident::new("Shape"))
        .expect("Shape enum to be recorded");
    assert_eq!(shape.generics.len(), 1);
    assert_eq!(shape.generics[0].0, "T");
    assert_eq!(shape.variants.len(), 3);

    let dot = &shape.variants[0];
    assert_eq!(dot.0.0, "Dot");
    assert_eq!(dot.1.len(), 1);
    assert_eq!(
        dot.1[0],
        tast::Ty::TStruct {
            name: "Point".to_string(),
        }
    );

    let wrapped = &shape.variants[1];
    assert_eq!(wrapped.0.0, "Wrapped");
    assert_eq!(wrapped.1.len(), 1);
    assert_eq!(
        wrapped.1[0],
        tast::Ty::TApp {
            ty: Box::new(tast::Ty::TStruct {
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
        .get(&Ident::new("Node"))
        .expect("Node struct to be recorded");
    assert_eq!(node.fields.len(), 2);
    assert_eq!(node.fields[1].0.0, "next");
    assert_eq!(
        node.fields[1].1,
        tast::Ty::TEnum {
            name: "List".to_string(),
        }
    );

    let list = genv
        .enums()
        .get(&Ident::new("List"))
        .expect("List enum to be recorded");
    assert_eq!(list.variants.len(), 2);
    let cons = &list.variants[0];
    assert_eq!(cons.0.0, "Cons");
    assert_eq!(cons.1.len(), 1);
    assert_eq!(
        cons.1[0],
        tast::Ty::TStruct {
            name: "Node".to_string(),
        }
    );
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
        tast::Expr::ELet { value, .. } => value,
        _ => panic!("expected let binding in function body"),
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

    assert_eq!(params.len(), 1);
    assert_eq!(params[0].ty, tast::Ty::TInt32);

    assert!(matches!(**body, tast::Expr::EBinary { .. }));

    let tast::Ty::TFunc {
        params: func_params,
        ret_ty,
    } = ty
    else {
        panic!("expected closure type to be a function");
    };
    assert_eq!(func_params.as_slice(), &[tast::Ty::TInt32]);
    assert_eq!(**ret_ty, tast::Ty::TInt32);
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
        tast::Expr::ELet { value, .. } => value,
        _ => panic!("expected let binding in function body"),
    };

    let tast::Expr::EClosure { params, ty, .. } = &**closure_expr else {
        panic!("expected closure expression");
    };

    assert_eq!(params.len(), 1);
    let expected = tast::Ty::TParam {
        name: "T".to_string(),
    };
    assert_eq!(params[0].ty, expected);

    let tast::Ty::TFunc {
        params: func_params,
        ret_ty,
    } = ty
    else {
        panic!("expected closure type to be a function");
    };
    assert_eq!(func_params.as_slice(), std::slice::from_ref(&expected));
    assert_eq!(**ret_ty, expected);
}

fn assert_typer_error(src: &str, expected_message: &str) {
    let (_tast, _genv, diagnostics) = typecheck(src);
    let messages: Vec<String> = diagnostics
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

fn bad(value: Wrapper[int32, int32]) -> unit { () }
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
    Bad(Wrapper[int32, int32]),
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
