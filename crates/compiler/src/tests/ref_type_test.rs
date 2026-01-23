use std::path::PathBuf;

use cst::cst::CstNode;
use expect_test::expect;
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
    let (hir, hir_table, mut hir_diagnostics) = crate::hir::lower_to_hir(ast);
    let (tast, genv, mut diagnostics) = crate::typer::check_file(hir, hir_table);
    diagnostics.append(&mut hir_diagnostics);
    (ast_clone, tast, genv, diagnostics)
}

fn describe_pat(pat: &ast::ast::Pat) -> String {
    match pat {
        ast::ast::Pat::PVar { name, .. } => name.0.clone(),
        ast::ast::Pat::PWild { .. } => "_".to_string(),
        other => format!("{other:?}"),
    }
}

fn describe_path(path: &ast::ast::Path) -> String {
    path.last_ident()
        .map(|ident| ident.0.clone())
        .unwrap_or_default()
}

fn describe_expr(expr: &ast::ast::Expr) -> String {
    match expr {
        ast::ast::Expr::ELet {
            pat,
            annotation,
            value,
            ..
        } => {
            let annotation = annotation
                .as_ref()
                .map(|ty| format!(": {ty:?}"))
                .unwrap_or_default();
            format!(
                "let {}{} = {}",
                describe_pat(pat),
                annotation,
                describe_expr(value)
            )
        }
        ast::ast::Expr::ECall { func, args, .. } => {
            let args = args
                .iter()
                .map(describe_expr)
                .collect::<Vec<_>>()
                .join(", ");
            format!("call {}({})", describe_expr(func), args)
        }
        ast::ast::Expr::EPath { path, .. } => describe_path(path),
        ast::ast::Expr::EInt { value, .. } => value.clone(),
        ast::ast::Expr::EBlock { exprs, .. } => format!(
            "block [{}]",
            exprs
                .iter()
                .map(describe_expr)
                .collect::<Vec<_>>()
                .join("; ")
        ),
        other => format!("{other:?}"),
    }
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
        ast::ast::Expr::EBlock { exprs, .. } => exprs,
        other => panic!("unexpected main body, expected EBlock: {:?}", other),
    };

    let mut lines = Vec::new();
    lines.push(format!("expr_count={}", exprs.len()));
    for (index, expr) in exprs.iter().enumerate() {
        lines.push(format!("expr{index}={}", describe_expr(expr)));
    }
    let messages: Vec<String> = diagnostics
        .iter()
        .map(|diagnostic| diagnostic.message().to_string())
        .collect();
    lines.push(format!("diagnostics={messages:?}"));

    let ref_ty = tast::Ty::TRef {
        elem: Box::new(tast::Ty::TInt32),
    };
    lines.push(format!("ref_constr={}", ref_ty.get_constr_name_unsafe()));

    expect![[r#"
        expr_count=3
        expr0=let r = call ref(1)
        expr1=let _ = call ref_set(r, 2)
        expr2=call ref_get(r)
        diagnostics=[]
        ref_constr=Ref"#]]
    .assert_eq(&lines.join("\n"));
}
