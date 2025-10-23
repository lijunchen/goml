use std::path::PathBuf;

use cst::cst::CstNode;
use parser::syntax::MySyntaxNode;

use crate::{compile_match, env::Env, tast};

fn typecheck(src: &str) -> (tast::File, Env) {
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
    let (tast, mut env) = crate::typer::check_file(ast);
    let core = compile_match::compile_file(&env, &tast);
    env.record_tuple_types_from_core(&core);
    (tast, env)
}

#[test]
fn references_typecheck_and_collect() {
    let src = r#"
fn main() -> int {
    let r = ref 1 in
    let _ = r = 2 in
    !r
}
"#;

    let (_tast, env) = typecheck(src);

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
}
