use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use ast::ast;
use cst::cst::CstNode;
use diagnostics::Diagnostics;
use parser::syntax::MySyntaxNode;

use crate::{
    artifact::CrateExports, builtins, env::GlobalTypeEnv, hir, interface,
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

    if !ast.uses.is_empty() {
        return Err("use declarations are not supported in this context".to_string());
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
    let crate_name = hir.name.0.clone();
    let (hir_table, results, genv, mut type_diagnostics) =
        crate::typer::check_file_with_env_and_results(
            hir,
            hir_table,
            GlobalTypeEnv::new(),
            builtins::builtin_env(),
            &crate_name,
            HashMap::new(),
        );
    type_diagnostics.append(&mut hir_diagnostics);
    parse_diagnostics.append(&mut type_diagnostics);

    Ok((
        hir_table,
        results,
        builtins::merge_with_builtin_env(&genv),
        parse_diagnostics,
    ))
}

fn parse_ast_for_query(path: &Path, src: &str) -> Result<(ast::File, Diagnostics), String> {
    let result = parser::parse(path, src);
    let (green_node, mut diagnostics) = result.into_parts();
    let root = MySyntaxNode::new_root(green_node);
    let cst = cst::cst::File::cast(root).ok_or_else(|| "failed to cast CST".to_string())?;

    let lower = ::ast::lower::lower(cst);
    let (ast, mut lower_diagnostics) = lower.into_parts();
    diagnostics.append(&mut lower_diagnostics);
    let ast = ast.ok_or_else(|| "AST lowering error".to_string())?;

    Ok((ast, diagnostics))
}

fn typecheck_crate_for_query(path: &Path, src: &str) -> Result<QueryTypecheck, String> {
    let (entry_ast, mut diagnostics) = parse_ast_for_query(path, src)?;
    let start_dir = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    let Some((crate_dir, _)) = crate::config::find_crate_root(start_dir) else {
        return Err("crate root not found".to_string());
    };

    let mut crate_unit = crate::pipeline::modules::discover_crate_from_dir(&crate_dir)
        .map_err(|err| format!("crate module discovery failed: {:?}", err))?;
    let current_path = canonical_path(path);
    let mut found = false;
    for module in crate_unit.modules.iter_mut() {
        if canonical_path(&module.file_path) == current_path {
            module.ast = entry_ast.clone();
            found = true;
        }
    }
    if !found {
        return Err("query file is not part of the crate".to_string());
    }

    let files = expand_derives(crate_unit.source_files(), &mut diagnostics);
    let crate_name = crate_unit.config.name.clone();
    let (_module_dir, dependencies) =
        crate::pipeline::packages::discover_crate_dependency_versions_from_file(path)
            .map_err(|err| format!("{:?}", err))?;
    let external_deps = crate::external::resolve_dependency_versions(&dependencies)?;
    let deps_interfaces = external_deps.namespace_interfaces();
    let deps_envs = external_deps.namespace_envs();

    let crate_id = interface::crate_id_for_name(&crate_name);
    let (hir, hir_table, mut hir_diagnostics) =
        hir::lower_to_hir_files_with_env(crate_id, files, &deps_interfaces);
    let (hir_table, results, crate_genv, mut type_diagnostics) =
        crate::typer::check_file_with_env_and_results(
            hir,
            hir_table,
            GlobalTypeEnv::new(),
            builtins::builtin_env(),
            &crate_name,
            deps_envs,
        );
    type_diagnostics.append(&mut hir_diagnostics);
    diagnostics.append(&mut type_diagnostics);

    let mut genv = builtins::builtin_env();
    for module in external_deps.modules.values() {
        module.interface.exports.apply_to(&mut genv);
    }
    CrateExports::from_genv(&crate_genv).apply_to(&mut genv);

    Ok((hir_table, results, genv, diagnostics))
}

fn expand_derives(
    files: Vec<hir::SourceFileAst>,
    diagnostics: &mut Diagnostics,
) -> Vec<hir::SourceFileAst> {
    files
        .into_iter()
        .map(|mut file| {
            let original_ast = file.ast.clone();
            file.ast = match crate::derive::expand(file.ast) {
                Ok(ast) => ast,
                Err(mut derive_diagnostics) => {
                    diagnostics.append(&mut derive_diagnostics);
                    original_ast
                }
            };
            file
        })
        .collect()
}

fn canonical_path(path: &Path) -> PathBuf {
    path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
}

pub(crate) fn typecheck_for_query(path: &Path, src: &str) -> Result<QueryTypecheck, String> {
    let start_dir = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    if crate::config::find_crate_root(start_dir).is_some() {
        typecheck_crate_for_query(path, src)
    } else {
        typecheck_single_file_for_query(path, src)
    }
}

pub fn diagnostics_for_query(path: &Path, src: &str) -> Result<Diagnostics, String> {
    typecheck_for_query(path, src).map(|(_hir_table, _results, _genv, diagnostics)| diagnostics)
}
