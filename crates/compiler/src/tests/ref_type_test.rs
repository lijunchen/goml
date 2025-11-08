use std::path::PathBuf;

use cst::cst::CstNode;
use parser::syntax::MySyntaxNode;

use crate::{compile_match, env::Env, tast};

fn typecheck(src: &str) -> (ast::ast::File, tast::File, Env) {
    let path = PathBuf::from("test_refs.gom");
    let parsed = parser::parse(&path, src);
    if parsed.has_errors() {
        panic!("Parse errors:\n{}", parsed.format_errors(src).join("\n"));
    }
    let root = MySyntaxNode::new_root(parsed.green_node);
    let cst = cst::cst::File::cast(root).expect("failed to cast syntax tree");
    let ast = ast::lower::lower(cst)
        .into_result()
        .expect("failed to lower to AST");
    let ast_clone = ast.clone();
    let (tast, mut env) = crate::typer::check_file(ast);
    let core = compile_match::compile_file(&env, &tast);
    env.record_tuple_types_from_core(&core);
    (ast_clone, tast, env)
}

#[test]
fn references_typecheck_and_collect() {
    let src = r#"
fn main() -> int {
    let r = ref(1);
    let _ = ref_set(r, 2);
    ref_get(r)
}
"#;

    let (ast, _tast, env) = typecheck(src);

    let main_fn = match &ast.toplevels[0] {
        ast::ast::Item::Fn(f) => f,
        other => panic!("expected function, got {:?}", other),
    };

    let first_let = match &main_fn.body {
        ast::ast::Expr::ELet {
            pat,
            annotation,
            value,
            body,
        } => {
            assert!(annotation.is_none());
            match pat {
                ast::ast::Pat::PVar { name, .. } => assert_eq!(name.0, "r"),
                other => panic!("unexpected first let pattern: {:?}", other),
            }
            match value.as_ref() {
                ast::ast::Expr::ECall { func, args } => {
                    assert_eq!(args.len(), 1);
                    assert!(matches!(
                        func.as_ref(),
                        ast::ast::Expr::EVar { name, .. } if name.0 == "ref"
                    ));
                    assert!(matches!(
                        args[0],
                        ast::ast::Expr::EInt { ref value } if value == "1"
                    ));
                }
                other => panic!("unexpected first let value: {:?}", other),
            }
            body
        }
        other => panic!("unexpected main body: {:?}", other),
    };

    if let ast::ast::Expr::ELet {
        pat,
        annotation,
        value,
        body,
    } = first_let.as_ref()
    {
        assert!(annotation.is_none());
        assert!(matches!(pat, ast::ast::Pat::PWild));
        match value.as_ref() {
            ast::ast::Expr::ECall { func, args } => {
                assert!(matches!(
                    func.as_ref(),
                    ast::ast::Expr::EVar { name, .. } if name.0 == "ref_set"
                ));
                assert_eq!(args.len(), 2);
                assert!(matches!(
                    args[0],
                    ast::ast::Expr::EVar { ref name, .. } if name.0 == "r"
                ));
                assert!(matches!(
                    args[1],
                    ast::ast::Expr::EInt { ref value } if value == "2"
                ));
            }
            other => panic!("unexpected assignment value: {:?}", other),
        }
        match body.as_ref() {
            ast::ast::Expr::ECall { func, args } => {
                assert!(matches!(
                    func.as_ref(),
                    ast::ast::Expr::EVar { name, .. } if name.0 == "ref_get"
                ));
                assert_eq!(args.len(), 1);
                assert!(matches!(
                    args[0],
                    ast::ast::Expr::EVar { ref name, .. } if name.0 == "r"
                ));
            }
            other => panic!("unexpected final body: {:?}", other),
        }
    } else {
        panic!("expected nested let expression");
    }

    let expected_ref_ty = tast::Ty::TRef {
        elem: Box::new(tast::Ty::TInt),
    };
    assert!(
        env.ref_types.contains(&expected_ref_ty),
        "expected ref type {:?} to be collected, got {:?}",
        expected_ref_ty,
        env.ref_types
    );
    assert!(
        env.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        env.diagnostics
    );

    let ref_ty = tast::Ty::TRef {
        elem: Box::new(tast::Ty::TInt),
    };
    assert_eq!(ref_ty.get_constr_name_unsafe(), "Ref");
}
