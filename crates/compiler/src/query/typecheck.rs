use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use cst::cst::CstNode;
use diagnostics::Stage;
use parser::syntax::MySyntaxNode;

use crate::{
    builtins,
    env::{Gensym, GlobalTypeEnv},
    hir, pipeline,
    typer::results::TypeckResults,
};

pub(crate) type QueryTypecheck = (
    hir::HirTable,
    TypeckResults,
    GlobalTypeEnv,
    diagnostics::Diagnostics,
);

#[derive(Debug, Clone)]
pub struct Analysis {
    hir_table: Option<hir::HirTable>,
    results: Option<TypeckResults>,
    genv: Option<GlobalTypeEnv>,
    diagnostics: diagnostics::Diagnostics,
}

impl Analysis {
    fn complete(
        hir_table: hir::HirTable,
        results: TypeckResults,
        genv: GlobalTypeEnv,
        diagnostics: diagnostics::Diagnostics,
    ) -> Self {
        Self {
            hir_table: Some(hir_table),
            results: Some(results),
            genv: Some(genv),
            diagnostics,
        }
    }

    fn failed(diagnostics: diagnostics::Diagnostics) -> Self {
        Self {
            hir_table: None,
            results: None,
            genv: None,
            diagnostics,
        }
    }

    pub fn diagnostics(&self) -> &diagnostics::Diagnostics {
        &self.diagnostics
    }

    pub(crate) fn parts(&self) -> Option<(&hir::HirTable, &TypeckResults, &GlobalTypeEnv)> {
        Some((
            self.hir_table.as_ref()?,
            self.results.as_ref()?,
            self.genv.as_ref()?,
        ))
    }

    fn into_typecheck(self) -> Result<QueryTypecheck, String> {
        let message = self
            .diagnostics
            .iter()
            .next()
            .map(|diagnostic| diagnostic.message().to_string())
            .unwrap_or_else(|| "typecheck failed".to_string());
        match (self.hir_table, self.results, self.genv) {
            (Some(hir_table), Some(results), Some(genv)) => {
                Ok((hir_table, results, genv, self.diagnostics))
            }
            _ => Err(message),
        }
    }
}

pub fn analyze(path: &Path, src: &str) -> Analysis {
    analyze_with_overrides(path, src, &HashMap::new())
}

pub fn analyze_with_overrides(
    path: &Path,
    src: &str,
    source_overrides: &HashMap<PathBuf, String>,
) -> Analysis {
    analyze_typecheck_with_overrides(path, src, source_overrides, true)
}

pub(crate) fn analyze_for_query_with_overrides(
    path: &Path,
    src: &str,
    source_overrides: &HashMap<PathBuf, String>,
) -> Analysis {
    analyze_typecheck_with_overrides(path, src, source_overrides, false)
}

fn analyze_typecheck_with_overrides(
    path: &Path,
    src: &str,
    source_overrides: &HashMap<PathBuf, String>,
    check_matches: bool,
) -> Analysis {
    let package_dir = path.parent().unwrap_or_else(|| Path::new("."));
    let in_project = crate::config::find_module_root(package_dir)
        .ok()
        .flatten()
        .is_some();
    if !in_project && let Some(analysis) = analyze_single_file(path, src, check_matches) {
        return analysis;
    }

    let result = if check_matches {
        pipeline::pipeline::analyze_with_packages_and_results_with_overrides(
            path,
            src,
            source_overrides,
        )
    } else {
        pipeline::pipeline::typecheck_with_packages_and_results_with_overrides(
            path,
            src,
            source_overrides,
        )
    };
    match result {
        Ok((hir_table, results, genv, diagnostics)) => {
            Analysis::complete(hir_table, results, genv, diagnostics)
        }
        Err(error) => Analysis::failed(prefer_parser_diagnostics(error.into_diagnostics())),
    }
}

fn prefer_parser_diagnostics(diagnostics: diagnostics::Diagnostics) -> diagnostics::Diagnostics {
    let mut parser_diagnostics = diagnostics::Diagnostics::new();
    parser_diagnostics.extend(
        diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.stage() == &Stage::Parser)
            .cloned(),
    );
    if parser_diagnostics.has_errors() {
        parser_diagnostics
    } else {
        diagnostics
    }
}

fn analyze_single_file(path: &Path, src: &str, check_matches: bool) -> Option<Analysis> {
    let result = parser::parse(path, src);
    let (green_node, mut diagnostics) = result.into_parts();
    let root = MySyntaxNode::new_root(green_node);
    let cst = cst::cst::File::cast(root)?;

    let lower = ::ast::lower::lower(cst);
    let (ast, mut lower_diagnostics) = lower.into_parts();
    diagnostics.append(&mut lower_diagnostics);
    let ast = ast?;
    if !ast.uses.is_empty() {
        return None;
    }

    let original_ast = ast.clone();
    let ast = match crate::derive::expand(ast) {
        Ok(ast) => ast,
        Err(mut derive_diagnostics) => {
            diagnostics.append(&mut derive_diagnostics);
            original_ast
        }
    };
    let syntax_diagnostics = if check_matches && diagnostics.has_errors() {
        Some(prefer_parser_diagnostics(diagnostics.clone()))
    } else {
        None
    };

    let (hir, hir_table, mut hir_diagnostics) =
        hir::lower_to_hir_files(vec![hir::SourceFileAst::new(path.to_path_buf(), ast)]);
    let package = hir.name.0.clone();
    let (tast, hir_table, results, genv, mut type_diagnostics) = if check_matches {
        let (tast, hir_table, results, genv, diagnostics) =
            crate::typer::check_file_with_env_tast_and_results(
                hir,
                hir_table,
                GlobalTypeEnv::new(),
                builtins::builtin_env(),
                &package,
                HashMap::new(),
            );
        (Some(tast), hir_table, results, genv, diagnostics)
    } else {
        let (hir_table, results, genv, diagnostics) = crate::typer::check_file_with_env_and_results(
            hir,
            hir_table,
            GlobalTypeEnv::new(),
            builtins::builtin_env(),
            &package,
            HashMap::new(),
        );
        (None, hir_table, results, genv, diagnostics)
    };
    type_diagnostics.append(&mut hir_diagnostics);
    diagnostics.append(&mut type_diagnostics);

    let genv = builtins::merge_with_builtin_env(&genv);
    if !diagnostics.has_errors()
        && let Some(tast) = tast
    {
        diagnostics.set_source(path);
        drop(crate::compile_match::compile_file(
            &genv,
            &Gensym::new(),
            &mut diagnostics,
            &tast,
        ));
        diagnostics.clear_source();
    }
    if let Some(syntax_diagnostics) = syntax_diagnostics {
        diagnostics = syntax_diagnostics;
    }

    Some(Analysis::complete(hir_table, results, genv, diagnostics))
}

pub(crate) fn typecheck_for_query_with_overrides(
    path: &Path,
    src: &str,
    source_overrides: &HashMap<PathBuf, String>,
) -> Result<QueryTypecheck, String> {
    analyze_for_query_with_overrides(path, src, source_overrides).into_typecheck()
}
