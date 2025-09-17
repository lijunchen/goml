use std::path::PathBuf;

use ast::ast::Uident;
use cst::cst::CstNode;
use parser::syntax::MySyntaxNode;

use crate::{env::Env, tast};

fn typecheck(src: &str) -> (tast::File, Env) {
    let path = PathBuf::from("test_structs.gom");
    let parsed = parser::parse(&path, src);
    let root = MySyntaxNode::new_root(parsed.green_node);
    let cst = cst::cst::File::cast(root).expect("failed to cast syntax tree");
    let ast = ast::lower::lower(cst).expect("failed to lower to AST");
    crate::typer::check_file(ast)
}

#[test]
fn collects_struct_definitions() {
    let src = r#"
struct Point {
    x: Int,
    y: Int,
}

struct Wrapper[T] {
    value: T,
}

fn consume_point(p: Point) -> Unit { () }

fn consume_wrapper[T](value: Wrapper[T]) -> Unit { () }
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
            name: Uident::new("Wrapper"),
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
#[should_panic(expected = "Type Wrapper expects 1 type arguments, but got 2")]
fn struct_type_arity_mismatch_panics() {
    let src = r#"
struct Wrapper[T] {
    value: T,
}

fn bad(value: Wrapper[Int, Int]) -> Unit { () }
"#;

    let _ = typecheck(src);
}

#[test]
#[should_panic(expected = "Unknown type constructor Undefined")]
fn unknown_type_constructor_panics() {
    let src = r#"
fn bad(p: Undefined) -> Unit { () }
"#;

    let _ = typecheck(src);
}

#[test]
#[should_panic(expected = "Unknown type constructor U")]
fn unbound_type_parameter_panics() {
    let src = r#"
struct Wrapper[T] {
    value: T,
}

fn bad[T](value: Wrapper[U]) -> Unit { () }
"#;

    let _ = typecheck(src);
}
