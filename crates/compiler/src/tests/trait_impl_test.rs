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
