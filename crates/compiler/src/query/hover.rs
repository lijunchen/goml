use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use cst::{cst::CstNode, nodes};
use parser::syntax::{MySyntaxNode, MySyntaxNodePtr, MySyntaxToken};

use crate::{env::GlobalTypeEnv, hir, tast, typer::results::TypeckResults};

use super::{
    context::QueryContext,
    hir_index::{
        ClosureParamIndex, HirResultsIndex, find_mapped_expr_id_from_token,
        find_mapped_local_id_from_token, find_mapped_pat_id_from_token, local_name_range,
    },
    syntax::{path_segments_at_offset, path_segments_at_range, path_segments_from_token},
    typecheck::{Analysis, analyze_for_query_with_overrides},
};

pub fn hover_type(path: &Path, src: &str, line: u32, col: u32) -> Result<String, String> {
    hover_type_with_overrides(path, src, line, col, &HashMap::new())
}

pub fn hover_type_with_overrides(
    path: &Path,
    src: &str,
    line: u32,
    col: u32,
    source_overrides: &HashMap<PathBuf, String>,
) -> Result<String, String> {
    let analysis = analyze_for_query_with_overrides(path, src, source_overrides);
    hover_type_with_analysis(path, src, line, col, &analysis)
}

pub fn hover_type_with_analysis(
    path: &Path,
    src: &str,
    line: u32,
    col: u32,
    analysis: &Analysis,
) -> Result<String, String> {
    crate::pipeline::with_compiler_stack(|| {
        let context = QueryContext::from_position(path, src, line, col)?;
        let token = context.token_prefer_ident();

        if let Some(token) = token.as_ref()
            && let Some(hover) = syntax_hover(token)
        {
            return Ok(hover);
        }

        let (hir_table, results, genv) = analysis
            .parts()
            .ok_or_else(|| "no type information found".to_string())?;
        let index = HirResultsIndex::new(hir_table, path);
        let closure_params = ClosureParamIndex::new(hir_table, path);

        if let Some(token) = token.as_ref() {
            if let Some(ty) = struct_literal_field_type(token, &index, hir_table, results) {
                return Ok(ty);
            }

            if let Some(ty) = struct_pattern_field_type(token, &index, hir_table, results) {
                return Ok(ty);
            }

            if let Some(ty) = qualified_constructor_hover(token, genv) {
                return Ok(ty);
            }

            if let Some(ty) = param_type_from_token(token, &closure_params, results) {
                return Ok(ty);
            }

            if let Some(ty) = pattern_binder_type(token, path, hir_table, results) {
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

        let range = token.as_ref().map(|token| token.text_range());
        if let Some(token) = token.as_ref()
            && let Some(segments) = path_segments_from_token(token)
            && let Some(ty) = lookup_value_type_from_segments(genv, &segments)
        {
            return Ok(ty);
        }

        if let Some(segments) = path_segments_at_offset(src, context.offset())
            && let Some(ty) = lookup_value_type_from_segments(genv, &segments)
        {
            return Ok(ty);
        }

        if let Some(range) = range
            && let Some(segments) = path_segments_at_range(context.syntax(), &range)
            && let Some(ty) = lookup_value_type_from_segments(genv, &segments)
        {
            return Ok(ty);
        }

        Err("no type information found".to_string())
    })
}

fn pattern_binder_type(
    token: &MySyntaxToken,
    source: &Path,
    hir_table: &hir::HirTable,
    results: &TypeckResults,
) -> Option<String> {
    for idx in 0..hir_table.pat_count() {
        let pat_id = hir::PatId {
            pkg: hir_table.package(),
            idx: idx as u32,
        };
        if hir_table.pat_source(pat_id) != Some(source) {
            continue;
        }
        let (local, astptr) = match hir_table.pat(pat_id) {
            hir::Pat::PAlias { name, astptr, .. } => (*name, *astptr),
            hir::Pat::PArray {
                rest:
                    Some(hir::ArrayPatRest {
                        binding: Some(name),
                        astptr,
                    }),
                ..
            } => (*name, *astptr),
            _ => continue,
        };
        if local_name_range(hir_table, local, astptr) == token.text_range() {
            return results.local_ty(local).map(|ty| ty.to_pretty(80));
        }
    }
    None
}

fn syntax_hover(token: &MySyntaxToken) -> Option<String> {
    if let Some(package) = ancestor::<nodes::PackageDecl>(token) {
        return package
            .name_token()
            .map(|name| format!("package {}", name.text()));
    }

    if let Some(use_decl) = ancestor::<nodes::UseDecl>(token) {
        return use_decl.path().map(|path| format!("use {}", path));
    }

    if let Some(ty) = ancestor::<nodes::Type>(token) {
        return Some(syntax_text(ty));
    }

    if let Some(generic) = ancestor::<nodes::Generic>(token)
        && generic
            .uident()
            .is_some_and(|name| name.text_range() == token.text_range())
    {
        return Some(syntax_text(generic));
    }

    declaration_hover(token)
}

fn declaration_hover(token: &MySyntaxToken) -> Option<String> {
    if let Some(field) = ancestor::<nodes::StructField>(token)
        && token_matches(token, field.lident())
    {
        return field.ty().map(syntax_text);
    }

    if let Some(variant) = ancestor::<nodes::Variant>(token)
        && token_matches(token, variant.uident())
    {
        return variant_hover(&variant);
    }

    if let Some(method) = ancestor::<nodes::TraitMethod>(token)
        && token_matches(token, method.lident())
    {
        return trait_method_type(&method);
    }

    if let Some(associated) = ancestor::<nodes::TraitAssociatedType>(token)
        && token_matches(token, associated.name())
    {
        let name = associated.name()?.to_string();
        return Some(match associated.trait_set() {
            Some(bounds) => format!("type {}: {}", name, syntax_text(bounds)),
            None => format!("type {}", name),
        });
    }

    if let Some(associated) = ancestor::<nodes::ImplAssociatedType>(token)
        && token_matches(token, associated.name())
    {
        return associated.ty().map(syntax_text);
    }

    if let Some(function) = ancestor::<nodes::Fn>(token)
        && token_matches(token, function.lident())
    {
        return function_type(&function);
    }

    if let Some(extern_item) = ancestor::<nodes::Extern>(token) {
        if extern_item.type_keyword().is_some() && token_matches(token, extern_item.uident()) {
            return extern_item.uident().map(|name| format!("type {}", name));
        }
        if token_matches(token, extern_item.lident()) {
            return extern_function_type(&extern_item);
        }
    }

    if let Some(strukt) = ancestor::<nodes::Struct>(token)
        && token_matches(token, strukt.uident())
    {
        return Some(format!(
            "struct {}{}",
            strukt.uident()?.text(),
            generic_arguments(strukt.generic_list())
        ));
    }

    if let Some(enumeration) = ancestor::<nodes::Enum>(token)
        && token_matches(token, enumeration.uident())
    {
        return Some(format!(
            "enum {}{}",
            enumeration.uident()?.text(),
            generic_arguments(enumeration.generic_list())
        ));
    }

    if let Some(trait_def) = ancestor::<nodes::Trait>(token)
        && token_matches(token, trait_def.uident())
    {
        return Some(format!(
            "trait {}{}",
            trait_def.uident()?.text(),
            generic_arguments(trait_def.generic_list())
        ));
    }

    None
}

fn function_type(function: &nodes::Fn) -> Option<String> {
    let params = function
        .param_list()
        .map(|params| {
            params
                .params()
                .filter_map(|param| param.ty().map(syntax_text))
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();
    let ret = function
        .return_type()
        .map(syntax_text)
        .unwrap_or_else(|| "unit".to_string());
    Some(format!("({}) -> {}", params.join(", "), ret))
}

fn extern_function_type(function: &nodes::Extern) -> Option<String> {
    let params = function
        .param_list()
        .map(|params| {
            params
                .params()
                .filter_map(|param| param.ty().map(syntax_text))
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();
    let ret = function
        .return_type()
        .map(syntax_text)
        .unwrap_or_else(|| "unit".to_string());
    Some(format!("({}) -> {}", params.join(", "), ret))
}

fn trait_method_type(method: &nodes::TraitMethod) -> Option<String> {
    let params = method
        .type_list()
        .map(|types| types.types().map(syntax_text).collect::<Vec<_>>())
        .unwrap_or_default();
    let ret = method
        .return_type()
        .map(syntax_text)
        .unwrap_or_else(|| "unit".to_string());
    Some(format!("({}) -> {}", params.join(", "), ret))
}

fn variant_hover(variant: &nodes::Variant) -> Option<String> {
    let enumeration = ancestor_from_node::<nodes::Enum>(variant.syntax())?;
    let enum_name = format!(
        "{}{}",
        enumeration.uident()?.text(),
        generic_arguments(enumeration.generic_list())
    );
    let params = variant
        .type_list()
        .map(|types| types.types().map(syntax_text).collect::<Vec<_>>())
        .unwrap_or_default();
    if params.is_empty() {
        Some(enum_name)
    } else {
        Some(format!("({}) -> {}", params.join(", "), enum_name))
    }
}

fn generic_arguments(generics: Option<nodes::GenericList>) -> String {
    let names = generics
        .into_iter()
        .flat_map(|list| list.generics())
        .filter_map(|generic| generic.uident().map(|name| name.to_string()))
        .collect::<Vec<_>>();
    if names.is_empty() {
        String::new()
    } else {
        format!("[{}]", names.join(", "))
    }
}

fn struct_literal_field_type(
    token: &MySyntaxToken,
    index: &HirResultsIndex,
    hir_table: &hir::HirTable,
    results: &TypeckResults,
) -> Option<String> {
    let field = ancestor::<nodes::StructLiteralField>(token)?;
    if !token_matches(token, field.lident()) {
        return None;
    }
    let literal = ancestor_from_node::<nodes::StructLiteralExpr>(field.syntax())?;
    let expr_id = index.expr_id(&MySyntaxNodePtr::new(literal.syntax()))?;
    let hir::Expr::EStructLiteral { fields, .. } = hir_table.expr(expr_id) else {
        return None;
    };
    let field_name = token.text();
    let field_expr = fields
        .iter()
        .find_map(|(name, expr)| (name.to_ident_name() == field_name).then_some(*expr))?;
    results.expr_ty(field_expr).map(|ty| ty.to_pretty(80))
}

fn struct_pattern_field_type(
    token: &MySyntaxToken,
    index: &HirResultsIndex,
    hir_table: &hir::HirTable,
    results: &TypeckResults,
) -> Option<String> {
    let field = ancestor::<nodes::StructPatternField>(token)?;
    if !token_matches(token, field.lident()) {
        return None;
    }
    let pattern = ancestor_from_node::<nodes::ConstrPat>(field.syntax())?;
    let pat_id = index.pat_id(&MySyntaxNodePtr::new(pattern.syntax()))?;
    let hir::Pat::PStruct { fields, .. } = hir_table.pat(pat_id) else {
        return None;
    };
    let field_name = token.text();
    let field_pat = fields
        .iter()
        .find_map(|(name, pat)| (name.to_ident_name() == field_name).then_some(*pat))?;
    results.pat_ty(field_pat).map(|ty| ty.to_pretty(80))
}

fn qualified_constructor_hover(token: &MySyntaxToken, genv: &GlobalTypeEnv) -> Option<String> {
    let path = ancestor::<nodes::Path>(token)?;
    let tokens = path.ident_tokens().collect::<Vec<_>>();
    let token_index = tokens
        .iter()
        .position(|candidate| candidate.text_range() == token.text_range())?;
    let segments = tokens
        .iter()
        .map(|candidate| candidate.to_string())
        .collect::<Vec<_>>();
    if segments.len() < 2 {
        return None;
    }
    let constructor_ty = lookup_constructor_type(genv, &segments)?;
    if token_index == segments.len() - 1 {
        return Some(constructor_ty.to_pretty(80));
    }
    if token_index == segments.len() - 2 {
        return Some(constructor_result_type(&constructor_ty).to_pretty(80));
    }
    Some(format!("package {}", segments[..=token_index].join("::")))
}

fn param_type_from_token(
    token: &MySyntaxToken,
    closure_params: &ClosureParamIndex,
    results: &TypeckResults,
) -> Option<String> {
    let mut current = token.parent();
    while let Some(node) = current {
        if let Some(param) = nodes::Param::cast(node.clone()) {
            return param.ty().map(syntax_text);
        }
        if let Some(param) = nodes::ClosureParam::cast(node.clone()) {
            if let Some(ty) = param.ty() {
                return Some(syntax_text(ty));
            }
            let ptr = MySyntaxNodePtr::new(param.syntax());
            let local_id = closure_params.local_id(&ptr)?;
            return results.local_ty(local_id).map(|typ| typ.to_pretty(80));
        }
        current = node.parent();
    }
    None
}

fn lookup_value_type_from_segments(genv: &GlobalTypeEnv, segments: &[String]) -> Option<String> {
    find_function_type(genv, segments)
        .or_else(|| lookup_constructor_type(genv, segments))
        .map(|ty| ty.to_pretty(80))
}

fn lookup_constructor_type(genv: &GlobalTypeEnv, segments: &[String]) -> Option<tast::Ty> {
    if segments.len() < 2 {
        return None;
    }
    let constructor = tast::TastIdent(segments.last()?.clone());
    let full_enum = tast::TastIdent(segments[..segments.len() - 1].join("::"));
    if let Some((_constructor, ty)) =
        genv.lookup_constructor_with_namespace(Some(&full_enum), &constructor)
    {
        return Some(ty);
    }
    let short_enum = tast::TastIdent(segments[segments.len() - 2].clone());
    genv.lookup_constructor_with_namespace(Some(&short_enum), &constructor)
        .map(|(_constructor, ty)| ty)
}

fn constructor_result_type(ty: &tast::Ty) -> tast::Ty {
    match ty {
        tast::Ty::TFunc { ret_ty, .. } => ret_ty.as_ref().clone(),
        ty => ty.clone(),
    }
}

fn find_function_type(genv: &GlobalTypeEnv, segments: &[String]) -> Option<tast::Ty> {
    let full_path = segments.join("::");
    if let Some(ty) = genv.get_type_of_function(&full_path) {
        return Some(ty);
    }

    if segments.len() == 1
        && let Some(ty) = genv.get_type_of_function(&segments[0])
    {
        return Some(ty);
    }

    let suffix = format!("::{}", full_path);
    let mut matches = genv
        .value_env
        .funcs
        .iter()
        .filter(|(name, _)| name.ends_with(&suffix))
        .map(|(_, scheme)| scheme.ty.clone());
    let found = matches.next()?;
    matches.next().is_none().then_some(found)
}

fn ancestor<N: CstNode>(token: &MySyntaxToken) -> Option<N> {
    let mut current = token.parent();
    while let Some(node) = current {
        if let Some(found) = N::cast(node.clone()) {
            return Some(found);
        }
        current = node.parent();
    }
    None
}

fn ancestor_from_node<N: CstNode>(node: &MySyntaxNode) -> Option<N> {
    let mut current = node.parent();
    while let Some(node) = current {
        if let Some(found) = N::cast(node.clone()) {
            return Some(found);
        }
        current = node.parent();
    }
    None
}

fn token_matches(token: &MySyntaxToken, candidate: Option<MySyntaxToken>) -> bool {
    candidate.is_some_and(|candidate| candidate.text_range() == token.text_range())
}

fn syntax_text(value: impl ToString) -> String {
    value.to_string().trim().to_string()
}
