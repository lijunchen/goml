use std::{collections::HashMap, path::Path};

use cst::cst::CstNode;
use parser::syntax::MySyntaxNode;

use crate::{
    artifact::PackageExports, builtins, env::GlobalTypeEnv, hir, pipeline,
    typer::results::TypeckResults,
};

pub(crate) type QueryTypecheck = (
    hir::HirTable,
    TypeckResults,
    GlobalTypeEnv,
    diagnostics::Diagnostics,
);

pub(crate) fn typecheck_single_file_for_query(
    path: &Path,
    src: &str,
) -> Result<QueryTypecheck, String> {
    let result = parser::parse(path, src);
    let (green_node, mut parse_diagnostics) = result.into_parts();
    let root = MySyntaxNode::new_root(green_node);
    let cst = cst::cst::File::cast(root).ok_or_else(|| "failed to cast CST".to_string())?;

    let lower = ::ast::lower::lower(cst);
    let (ast, mut lower_diagnostics) = lower.into_parts();
    parse_diagnostics.append(&mut lower_diagnostics);
    let ast = ast.ok_or_else(|| "AST lowering error".to_string())?;

    if !ast.imports.is_empty() || !ast.use_traits.is_empty() {
        return Err("package uses are not supported in this context".to_string());
    }

    let original_ast = ast.clone();
    let ast = match crate::derive::expand(ast) {
        Ok(ast) => ast,
        Err(mut derive_diagnostics) => {
            parse_diagnostics.append(&mut derive_diagnostics);
            original_ast
        }
    };

    let (hir, hir_table, mut hir_diagnostics) = crate::hir::lower_to_hir(ast);
    let package = hir.name.0.clone();
    let (hir_table, results, genv, mut type_diagnostics) =
        crate::typer::check_file_with_env_and_results(
            hir,
            hir_table,
            GlobalTypeEnv::new(),
            builtins::builtin_env(),
            &package,
            HashMap::new(),
        );
    type_diagnostics.append(&mut hir_diagnostics);
    parse_diagnostics.append(&mut type_diagnostics);

    let exports = PackageExports {
        type_env: genv.type_env.clone(),
        trait_env: genv.trait_env.clone(),
        value_env: genv.value_env.clone(),
    };
    let mut full_env = builtins::builtin_env();
    exports.apply_to(&mut full_env);

    Ok((hir_table, results, full_env, parse_diagnostics))
}

pub(crate) fn typecheck_for_query(path: &Path, src: &str) -> Result<QueryTypecheck, String> {
    typecheck_single_file_for_query(path, src).or_else(|_| {
        pipeline::pipeline::typecheck_with_packages_and_results(path, src)
            .map_err(|e| format!("{:?}", e))
    })
}
