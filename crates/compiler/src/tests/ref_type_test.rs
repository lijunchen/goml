use std::path::PathBuf;

use cst::cst::CstNode;
use parser::syntax::MySyntaxNode;

use crate::{env::GlobalTypeEnv, tast};
use diagnostics::Diagnostics;

fn typecheck(src: &str) -> (ast::ast::File, tast::File, GlobalTypeEnv, Diagnostics) {
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
    let (tast, genv, diagnostics) = crate::typer::check_file(ast);
    (ast_clone, tast, genv, diagnostics)
}

#[test]
fn references_typecheck_and_collect() {
    let src = r#"
fn main() -> int32 {
    let r = ref(1);
    let _ = ref_set(r, 2);
    ref_get(r)
}
"#;

    let (ast, _tast, _genv, diagnostics) = typecheck(src);

    let main_fn = match &ast.toplevels[0] {
        ast::ast::Item::Fn(f) => f,
        other => panic!("expected function, got {:?}", other),
    };

    // The function body is now an EBlock containing the statements
    let exprs = match &main_fn.body {
        ast::ast::Expr::EBlock { exprs } => exprs,
        other => panic!("unexpected main body, expected EBlock: {:?}", other),
    };

    assert_eq!(exprs.len(), 3, "expected 3 expressions in block");

    // First expression: let r = ref(1)
    match &exprs[0] {
        ast::ast::Expr::ELet {
            pat,
            annotation,
            value,
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
                        ast::ast::Expr::EPath { path, .. } if path.last_ident().map(|i| &i.0) == Some(&"ref".to_string())
                    ));
                    assert!(matches!(
                        args[0],
                        ast::ast::Expr::EInt { value: ref v } if v == "1"
                    ));
                }
                other => panic!("unexpected first let value: {:?}", other),
            }
        }
        other => panic!("expected first ELet, got {:?}", other),
    }

    // Second expression: let _ = ref_set(r, 2)
    match &exprs[1] {
        ast::ast::Expr::ELet { pat, value, .. } => {
            assert!(matches!(pat, ast::ast::Pat::PWild));
            match value.as_ref() {
                ast::ast::Expr::ECall { func, args } => {
                    assert!(matches!(
                        func.as_ref(),
                        ast::ast::Expr::EPath { path, .. } if path.last_ident().map(|i| &i.0) == Some(&"ref_set".to_string())
                    ));
                    assert_eq!(args.len(), 2);
                    assert!(matches!(
                        args[0],
                        ast::ast::Expr::EPath { ref path, .. } if path.last_ident().map(|i| &i.0) == Some(&"r".to_string())
                    ));
                    assert!(matches!(
                        args[1],
                        ast::ast::Expr::EInt { value: ref v } if v == "2"
                    ));
                }
                other => panic!("unexpected second let value: {:?}", other),
            }
        }
        other => panic!("unexpected second expression, expected ELet: {:?}", other),
    }

    // Third expression: ref_get(r)
    match &exprs[2] {
        ast::ast::Expr::ECall { func, args } => {
            assert!(matches!(
                func.as_ref(),
                ast::ast::Expr::EPath { path, .. } if path.last_ident().map(|i| &i.0) == Some(&"ref_get".to_string())
            ));
            assert_eq!(args.len(), 1);
            assert!(matches!(
                args[0],
                ast::ast::Expr::EPath { ref path, .. } if path.last_ident().map(|i| &i.0) == Some(&"r".to_string())
            ));
        }
        other => panic!("unexpected third expression: {:?}", other),
    }

    assert!(
        diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        diagnostics
    );

    let ref_ty = tast::Ty::TRef {
        elem: Box::new(tast::Ty::TInt32),
    };
    assert_eq!(ref_ty.get_constr_name_unsafe(), "Ref");
}
