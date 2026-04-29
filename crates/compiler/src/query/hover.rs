use std::path::Path;

use cst::cst::CstNode;
use parser::syntax::{MySyntaxNodePtr, MySyntaxToken};

use crate::{env::GlobalTypeEnv, tast};

use super::{
    context::QueryContext,
    hir_index::{
        ClosureParamIndex, HirResultsIndex, find_mapped_expr_id_from_token,
        find_mapped_local_id_from_token, find_mapped_pat_id_from_token,
    },
    syntax::{path_segments_at_offset, path_segments_at_range, path_segments_from_token},
    typecheck::typecheck_for_query,
};

pub fn hover_type(path: &Path, src: &str, line: u32, col: u32) -> Result<String, String> {
    crate::pipeline::with_compiler_stack(|| {
        let context = QueryContext::from_position(path, src, line, col)?;
        let token = context.token_prefer_ident();
        let range = token.as_ref().map(|tok| tok.text_range());
        let offset = context.offset();

        let (hir_table, results, genv, _diagnostics) = typecheck_for_query(path, src)?;
        let index = HirResultsIndex::new(&hir_table);
        let closure_params = ClosureParamIndex::new(&hir_table);

        if let Some(token) = token.as_ref() {
            if let Some(ty) = param_type_from_token(token, &closure_params, &results) {
                return Ok(ty);
            }

            if let Some(pat_id) = find_mapped_pat_id_from_token(token, &index)
                && let Some(ty) = results.pat_ty(pat_id)
            {
                return Ok(ty.to_pretty(80));
            }

            if let Some(expr_id) = find_mapped_expr_id_from_token(token, &index)
                && let Some(ty) = results.expr_ty(expr_id)
            {
                return Ok(ty.to_pretty(80));
            }

            if let Some(local_id) = find_mapped_local_id_from_token(token, &index)
                && let Some(ty) = results.local_ty(local_id)
            {
                return Ok(ty.to_pretty(80));
            }
        }

        if let Some(token) = token.as_ref()
            && let Some(segments) = path_segments_from_token(token)
            && let Some(ty) = lookup_type_from_segments(path, &genv, &segments)
        {
            return Ok(ty);
        }

        if let Some(segments) = path_segments_at_offset(src, offset)
            && let Some(ty) = lookup_type_from_segments(path, &genv, &segments)
        {
            return Ok(ty);
        }

        if let Some(range) = range
            && let Some(ty) = lookup_type_from_path_range(path, &genv, context.syntax(), &range)
        {
            return Ok(ty);
        }

        if let Some(ty) = lookup_type_from_path_offset(path, &genv, src, offset) {
            return Ok(ty);
        }

        Err("no type information found".to_string())
    })
}

fn param_type_from_token(
    token: &MySyntaxToken,
    closure_params: &ClosureParamIndex,
    results: &crate::typer::results::TypeckResults,
) -> Option<String> {
    let mut current = token.parent();
    while let Some(node) = current {
        if let Some(param) = cst::nodes::Param::cast(node.clone()) {
            return param.ty().map(|typ| typ.to_string());
        }
        if let Some(param) = cst::nodes::ClosureParam::cast(node.clone()) {
            if let Some(ty) = param.ty() {
                return Some(ty.to_string());
            }
            let ptr = MySyntaxNodePtr::new(param.syntax());
            let local_id = closure_params.local_id(&ptr)?;
            return results.local_ty(local_id).map(|typ| typ.to_pretty(80));
        }
        current = node.parent();
    }
    None
}

fn lookup_type_from_path_range(
    path: &Path,
    genv: &GlobalTypeEnv,
    root: &parser::syntax::MySyntaxNode,
    range: &rowan::TextRange,
) -> Option<String> {
    let segments = path_segments_at_range(root, range)?;
    if segments.is_empty() {
        return None;
    }
    lookup_type_from_segments(path, genv, &segments)
}

fn lookup_type_from_path_offset(
    path: &Path,
    genv: &GlobalTypeEnv,
    src: &str,
    offset: text_size::TextSize,
) -> Option<String> {
    let segments = path_segments_at_offset(src, offset)?;
    if segments.is_empty() {
        return None;
    }
    lookup_type_from_segments(path, genv, &segments)
}

fn lookup_type_from_segments(
    path: &Path,
    genv: &GlobalTypeEnv,
    segments: &[String],
) -> Option<String> {
    for lookup_segments in hover_lookup_segments(path, segments) {
        if let Some(ty) = lookup_type_from_lookup_segments(genv, &lookup_segments) {
            return Some(ty);
        }
    }
    None
}

fn lookup_type_from_lookup_segments(genv: &GlobalTypeEnv, segments: &[String]) -> Option<String> {
    if let Some(ty) = find_function_type(genv, segments) {
        return Some(ty.to_pretty(80));
    }

    if segments.len() >= 2 {
        let constr = tast::TastIdent(segments[segments.len() - 1].clone());
        let enum_name = tast::TastIdent(segments[..segments.len() - 1].join("::"));
        if let Some((_ctor, ty)) = genv.lookup_constructor_with_namespace(Some(&enum_name), &constr)
        {
            return Some(ty.to_pretty(80));
        }

        let short_enum = tast::TastIdent(segments[segments.len() - 2].clone());
        if let Some((_ctor, ty)) =
            genv.lookup_constructor_with_namespace(Some(&short_enum), &constr)
        {
            return Some(ty.to_pretty(80));
        }
    }

    None
}

fn hover_lookup_segments(path: &Path, segments: &[String]) -> Vec<Vec<String>> {
    let mut out = Vec::new();
    if let Some(mapped) = source_rooted_lookup_segments(path, segments) {
        out.push(mapped);
    }
    out.push(segments.to_vec());
    out
}

fn source_rooted_lookup_segments(path: &Path, segments: &[String]) -> Option<Vec<String>> {
    let root = segments.first()?.as_str();
    if !matches!(root, "crate" | "self" | "super") {
        return None;
    }

    let crate_unit = discover_hover_crate(path)?;
    let mut out = vec![crate_unit.config.name.clone()];
    match root {
        "crate" => {}
        "self" => {
            out.extend(current_hover_module_path(path, &crate_unit)?);
        }
        "super" => {
            let mut module_path = current_hover_module_path(path, &crate_unit)?;
            if module_path.is_empty() {
                return None;
            }
            module_path.pop();
            out.extend(module_path);
        }
        _ => return None,
    }
    out.extend(segments.iter().skip(1).cloned());
    Some(out)
}

fn discover_hover_crate(path: &Path) -> Option<crate::pipeline::modules::CrateUnit> {
    let start_dir = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    let (crate_dir, _) = crate::config::find_crate_root(start_dir)?;
    crate::pipeline::modules::discover_crate_from_dir(&crate_dir).ok()
}

fn current_hover_module_path(
    path: &Path,
    crate_unit: &crate::pipeline::modules::CrateUnit,
) -> Option<Vec<String>> {
    let current_path = canonical_hover_path(path);
    crate_unit
        .modules
        .iter()
        .find(|module| canonical_hover_path(&module.file_path) == current_path)
        .map(|module| module.path.segments().to_vec())
}

fn canonical_hover_path(path: &Path) -> std::path::PathBuf {
    path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
}

fn find_function_type(genv: &GlobalTypeEnv, segments: &[String]) -> Option<tast::Ty> {
    let full_path = segments.join("::");
    if let Some(ty) = genv.get_type_of_function(&full_path) {
        return Some(ty);
    }

    if let Some(last) = segments.last()
        && let Some(ty) = genv.get_type_of_function(last)
    {
        return Some(ty);
    }

    let segment_strs = segments
        .iter()
        .map(|segment| segment.as_str())
        .collect::<Vec<_>>();
    let mut best: Option<tast::Ty> = None;

    for (name, scheme) in genv.value_env.funcs.iter() {
        let parts = name.split("::").collect::<Vec<_>>();
        if parts.is_empty() {
            continue;
        }

        if parts.len() >= segment_strs.len()
            && parts[parts.len() - segment_strs.len()..] == segment_strs
        {
            return Some(scheme.ty.clone());
        }

        if let Some(last) = segments.last()
            && parts.last() == Some(&last.as_str())
        {
            best = Some(scheme.ty.clone());
        }
    }

    best
}
