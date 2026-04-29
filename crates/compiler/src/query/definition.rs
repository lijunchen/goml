use std::{fs, path::Path};

use cst::cst::CstNode;
use parser::syntax::{MySyntaxKind, MySyntaxToken};
use text_size::{TextRange, TextSize};

use crate::{hir, tast};

use super::{
    DefinitionLocation,
    context::QueryContext,
    hir_index::{
        HirResultsIndex, expr_ids_from_token, local_def_range_from_pats, pat_ids_from_token,
    },
    symbol_index::{
        ProjectSymbolIndex, SymbolLookup, build_symbol_index, build_symbol_lookup,
        lookup_symbol_locations_for_path, namespace_nav_target_in_dir,
    },
    syntax::{
        mod_decl_from_token, path_segments_from_token, token_segment_index, use_decl_from_token,
    },
    typecheck::typecheck_for_query,
};

pub fn goto_definition(
    path: &Path,
    src: &str,
    line: u32,
    col: u32,
) -> Result<text_size::TextRange, String> {
    crate::pipeline::with_compiler_stack(|| {
        let locations = goto_definition_locations(path, src, line, col)?;
        let mut same_file = locations
            .iter()
            .filter(|loc| loc.path == path)
            .collect::<Vec<_>>();
        if same_file.len() == 1 {
            return Ok(same_file.swap_remove(0).range);
        }
        Err("no definition found".to_string())
    })
}

pub fn goto_definition_locations(
    path: &Path,
    src: &str,
    line: u32,
    col: u32,
) -> Result<Vec<DefinitionLocation>, String> {
    crate::pipeline::with_compiler_stack(|| {
        let context = QueryContext::from_position(path, src, line, col)?;
        let token = context
            .token_prefer_ident()
            .ok_or_else(|| "no token at position".to_string())?;

        if let Some(location) = resolve_mod_decl(path, &token) {
            return Ok(vec![location]);
        }

        if let Some(locations) = resolve_use_decl(path, src, &token) {
            return Ok(locations);
        }

        if let Some(location) = self_definition_location(path, &token) {
            return Ok(vec![location]);
        }

        let symbol_lookup = build_symbol_lookup(path, src);

        if let Some(locations) = resolve_semantic_definition(path, src, &token, &symbol_lookup) {
            return Ok(locations);
        }

        if let Some(locations) = resolve_struct_literal_field_syntax(&token, &symbol_lookup.index) {
            return Ok(locations);
        }

        if token.kind() == MySyntaxKind::Ident {
            let segments = path_segments_from_token(&token).unwrap_or_default();
            if !segments.is_empty() {
                let out = lookup_symbol_locations_for_path(
                    symbol_lookup.graph.as_ref(),
                    &symbol_lookup.index,
                    &token.to_string(),
                    &segments,
                );
                if !out.is_empty() {
                    return Ok(out);
                }
            }
        }

        Err("no definition found".to_string())
    })
}

fn resolve_use_decl(
    path: &Path,
    src: &str,
    token: &MySyntaxToken,
) -> Option<Vec<DefinitionLocation>> {
    let use_decl = use_decl_from_token(token)?;
    let path_node = use_decl.path()?;
    let ident_tokens = path_node.ident_tokens().collect::<Vec<_>>();
    let segments = ident_tokens
        .iter()
        .map(|token| token.to_string())
        .collect::<Vec<_>>();
    if segments.is_empty() {
        return None;
    }

    let idx = token_segment_index(token, &ident_tokens)?;
    let lookup_segments = normalize_root_segments(&segments[..=idx]);
    if lookup_segments.is_empty() {
        return None;
    }
    let lookup = lookup_segments.join("::");
    if let Some(location) = external_use_definition_location(path, &lookup) {
        return Some(vec![location]);
    }
    let Ok((_graph, index)) = build_symbol_index(path, src) else {
        return None;
    };
    if let Some(location) = namespace_definition_location(&index, &lookup) {
        return Some(vec![location]);
    }
    let mut out = index.find_type(&lookup);
    out.extend(index.find_value(&lookup));
    if out.is_empty() { None } else { Some(out) }
}

fn normalize_root_segments(segments: &[String]) -> &[String] {
    if matches!(
        segments.first().map(String::as_str),
        Some("crate" | "self" | "super")
    ) {
        &segments[1..]
    } else {
        segments
    }
}

fn resolve_mod_decl(path: &Path, token: &MySyntaxToken) -> Option<DefinitionLocation> {
    let module = mod_decl_from_token(token)?;
    let name_token = module.name_token()?;
    if token.text_range() != name_token.text_range() {
        return None;
    }
    let name = name_token.to_string();

    let start_dir = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    if let Some((crate_dir, _)) = crate::config::find_crate_root(start_dir)
        && let Ok(unit) = crate::pipeline::modules::discover_crate_from_dir(&crate_dir)
    {
        let current = canonical_path(path);
        if let Some(module) = unit
            .modules
            .iter()
            .find(|module| canonical_path(&module.file_path) == current)
            && let Some(child_id) = module.children.get(&name)
            && let Some(child) = unit.modules.get(child_id.0)
        {
            return Some(file_start_location(child.file_path.clone()));
        }
    }

    let flat = start_dir.join(format!("{name}.gom"));
    let nested = start_dir.join(&name).join("mod.gom");
    match (flat.exists(), nested.exists()) {
        (true, false) => Some(file_start_location(flat)),
        (false, true) => Some(file_start_location(nested)),
        _ => None,
    }
}

fn canonical_path(path: &Path) -> std::path::PathBuf {
    fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf())
}

fn file_start_location(path: std::path::PathBuf) -> DefinitionLocation {
    DefinitionLocation {
        path,
        range: TextRange::new(TextSize::from(0), TextSize::from(0)),
    }
}

fn resolve_semantic_definition(
    path: &Path,
    src: &str,
    token: &MySyntaxToken,
    symbols: &SymbolLookup,
) -> Option<Vec<DefinitionLocation>> {
    let (hir_table, results, _genv, _diagnostics) = typecheck_for_query(path, src).ok()?;
    let index = HirResultsIndex::new(&hir_table);

    let expr_ids = expr_ids_from_token(token, &index);
    for expr_id in expr_ids {
        if let Some(out) = resolve_expr_definition(
            path,
            token,
            expr_id,
            &hir_table,
            &results,
            symbols.graph.as_ref(),
            &symbols.index,
        ) {
            return Some(out);
        }
    }

    let pat_ids = pat_ids_from_token(token, &index);
    for pat_id in pat_ids {
        if let Some(out) = resolve_pat_definition(token, pat_id, &results, &symbols.index) {
            return Some(out);
        }
    }

    None
}

fn resolve_struct_literal_field_syntax(
    token: &MySyntaxToken,
    sym_index: &ProjectSymbolIndex,
) -> Option<Vec<DefinitionLocation>> {
    if token.kind() != MySyntaxKind::Ident {
        return None;
    }

    let mut current = token.parent();
    let mut field = None;
    while let Some(node) = current {
        if let Some(candidate) = cst::nodes::StructLiteralField::cast(node.clone())
            && candidate
                .lident()
                .is_some_and(|ident| ident.text_range() == token.text_range())
        {
            field = Some(candidate);
            break;
        }
        current = node.parent();
    }
    let field = field?;

    let mut current = field.syntax().parent();
    while let Some(node) = current {
        if let Some(struct_lit) = cst::nodes::StructLiteralExpr::cast(node.clone()) {
            let path = struct_lit.path()?;
            let segments = path
                .ident_tokens()
                .map(|token| token.to_string())
                .collect::<Vec<_>>();
            let segments = normalize_root_segments(&segments);
            if segments.is_empty() {
                return None;
            }
            let field_name = token.to_string();
            let type_name = segments.join("::");
            let mut out = sym_index.find_struct_field(&type_name, &field_name);
            if out.is_empty()
                && let Some(last) = segments.last()
            {
                out.extend(sym_index.find_struct_field(last, &field_name));
            }
            return if out.is_empty() { None } else { Some(out) };
        }
        current = node.parent();
    }

    None
}

fn resolve_expr_definition(
    path: &Path,
    token: &MySyntaxToken,
    expr_id: hir::ExprId,
    hir_table: &hir::HirTable,
    results: &crate::typer::results::TypeckResults,
    graph: Option<&crate::pipeline::packages::PackageGraph>,
    sym_index: &ProjectSymbolIndex,
) -> Option<Vec<DefinitionLocation>> {
    match hir_table.expr(expr_id) {
        hir::Expr::ENameRef { res, hint, .. } => match res {
            hir::NameRef::Local(local_id) => {
                if let Some(range) = local_def_range_from_pats(hir_table, *local_id) {
                    return Some(vec![DefinitionLocation {
                        path: path.to_path_buf(),
                        range,
                    }]);
                }
                if let Some(range) = local_def_range_from_fn_params(token, hint) {
                    return Some(vec![DefinitionLocation {
                        path: path.to_path_buf(),
                        range,
                    }]);
                }
                None
            }
            hir::NameRef::Def(_) | hir::NameRef::Builtin(_) => {
                let mut out = sym_index.find_value(hint);
                out.extend(sym_index.find_type(hint));
                if out.is_empty() { None } else { Some(out) }
            }
            hir::NameRef::Unresolved(p) => {
                let segments = p
                    .segments()
                    .iter()
                    .map(|segment| segment.seg.clone())
                    .collect::<Vec<_>>();
                let out = lookup_symbol_locations_for_path(
                    graph,
                    sym_index,
                    &token.to_string(),
                    &segments,
                );
                if out.is_empty() { None } else { Some(out) }
            }
        },
        hir::Expr::EStaticMember { path, .. } => {
            if token.kind() == MySyntaxKind::Ident
                && let Some(member) = path.last_ident()
                && token.to_string() == member.as_str()
                && let Some(elab) = results.name_ref_elab(expr_id)
            {
                match elab {
                    crate::typer::results::NameRefElab::InherentMethod {
                        receiver_ty,
                        method_name,
                        ..
                    } => {
                        let receiver_keys = tast_ty_constr_candidates(receiver_ty);
                        let mut out = Vec::new();
                        for receiver_key in receiver_keys.iter() {
                            out.extend(sym_index.find_impl_methods(receiver_key, &method_name.0));
                        }
                        if !out.is_empty() {
                            return Some(out);
                        }
                    }
                    crate::typer::results::NameRefElab::TraitMethod {
                        trait_name,
                        method_name,
                        ..
                    }
                    | crate::typer::results::NameRefElab::DynTraitMethod {
                        trait_name,
                        method_name,
                        ..
                    } => {
                        let out = sym_index.find_trait_methods(&trait_name.0, &method_name.0);
                        if !out.is_empty() {
                            return Some(out);
                        }
                    }
                    crate::typer::results::NameRefElab::Var { .. } => {}
                }
            }

            let segments = path
                .segments()
                .iter()
                .map(|segment| segment.seg.clone())
                .collect::<Vec<_>>();
            let out =
                lookup_symbol_locations_for_path(graph, sym_index, &token.to_string(), &segments);
            if out.is_empty() { None } else { Some(out) }
        }
        hir::Expr::EField { expr, field } => {
            if token.kind() != MySyntaxKind::Ident {
                return None;
            }
            let field_name = field.to_ident_name();
            if token.to_string() != field_name {
                return None;
            }

            if let Some(elab) = results.name_ref_elab(expr_id) {
                let receiver_ty = results.expr_ty(*expr).cloned().unwrap_or(tast::Ty::TUnit);
                let receiver_keys = tast_ty_constr_candidates(&receiver_ty);
                let method_name = match elab {
                    crate::typer::results::NameRefElab::InherentMethod { method_name, .. }
                    | crate::typer::results::NameRefElab::TraitMethod { method_name, .. }
                    | crate::typer::results::NameRefElab::DynTraitMethod { method_name, .. } => {
                        method_name.0.clone()
                    }
                    crate::typer::results::NameRefElab::Var { .. } => field_name.clone(),
                };

                let mut out = Vec::new();
                for receiver_key in receiver_keys.iter() {
                    out.extend(sym_index.find_impl_methods(receiver_key, &method_name));
                }

                if out.is_empty()
                    && let crate::typer::results::NameRefElab::TraitMethod { trait_name, .. }
                    | crate::typer::results::NameRefElab::DynTraitMethod { trait_name, .. } =
                        elab
                {
                    out.extend(sym_index.find_trait_methods(&trait_name.0, &method_name));
                }

                if !out.is_empty() {
                    return Some(out);
                }
            } else if let Some(receiver_ty) = results.expr_ty(*expr) {
                let receiver_keys = tast_ty_constr_candidates(receiver_ty);
                let mut out = Vec::new();
                for receiver_key in receiver_keys.iter() {
                    out.extend(sym_index.find_struct_field(receiver_key, &field_name));
                }
                if !out.is_empty() {
                    return Some(out);
                }
            }

            None
        }
        hir::Expr::EStructLiteral { .. } => {
            if let Some(elab) = results.struct_lit_elab(expr_id)
                && token.kind() == MySyntaxKind::Ident
            {
                let field_name = token.to_string();
                let struct_name = elab.constructor.type_name().0.clone();
                let mut out = sym_index.find_struct_field(&struct_name, &field_name);
                if out.is_empty()
                    && struct_name.contains("::")
                    && let Some(last) = struct_name.rsplit("::").next()
                {
                    out.extend(sym_index.find_struct_field(last, &field_name));
                }
                if !out.is_empty() {
                    return Some(out);
                }
            }
            None
        }
        hir::Expr::EConstr { .. } => {
            if let Some(constructor) = results.constructor_for_expr(expr_id) {
                if let Some(enum_ctor) = constructor.as_enum() {
                    let variant = enum_ctor.variant.0.as_str();
                    let enum_name = enum_ctor.type_name.0.as_str();
                    if token.to_string() == variant {
                        let out = sym_index.find_enum_variant(enum_name, variant);
                        if !out.is_empty() {
                            return Some(out);
                        }
                    }
                    if token.to_string()
                        == enum_ctor
                            .type_name
                            .0
                            .rsplit("::")
                            .next()
                            .unwrap_or(&enum_ctor.type_name.0)
                    {
                        let out = sym_index.find_type(enum_name);
                        if !out.is_empty() {
                            return Some(out);
                        }
                    }
                } else if let Some(struct_ctor) = constructor.as_struct() {
                    let struct_name = struct_ctor.type_name.0.as_str();
                    if token.to_string()
                        == struct_ctor
                            .type_name
                            .0
                            .rsplit("::")
                            .next()
                            .unwrap_or(&struct_ctor.type_name.0)
                    {
                        let out = sym_index.find_type(struct_name);
                        if !out.is_empty() {
                            return Some(out);
                        }
                    }
                }
            }
            None
        }
        _ => None,
    }
}

fn resolve_pat_definition(
    token: &MySyntaxToken,
    pat_id: hir::PatId,
    results: &crate::typer::results::TypeckResults,
    sym_index: &ProjectSymbolIndex,
) -> Option<Vec<DefinitionLocation>> {
    if let Some(constructor) = results.constructor_for_pat(pat_id)
        && let Some(enum_ctor) = constructor.as_enum()
    {
        let variant = enum_ctor.variant.0.as_str();
        let enum_name = enum_ctor.type_name.0.as_str();
        if token.to_string() == variant {
            let out = sym_index.find_enum_variant(enum_name, variant);
            if !out.is_empty() {
                return Some(out);
            }
        }
    }
    None
}

fn self_definition_location(path: &Path, token: &MySyntaxToken) -> Option<DefinitionLocation> {
    if token.kind() != MySyntaxKind::Ident {
        return None;
    }

    let mut current = token.parent();
    while let Some(node) = current {
        if let Some(function) = cst::nodes::Fn::cast(node.clone())
            && function
                .lident()
                .is_some_and(|t| t.text_range() == token.text_range())
        {
            return Some(DefinitionLocation {
                path: path.to_path_buf(),
                range: token.text_range(),
            });
        }
        if let Some(strukt) = cst::nodes::Struct::cast(node.clone())
            && strukt
                .uident()
                .is_some_and(|t| t.text_range() == token.text_range())
        {
            return Some(DefinitionLocation {
                path: path.to_path_buf(),
                range: token.text_range(),
            });
        }
        if let Some(enum_item) = cst::nodes::Enum::cast(node.clone())
            && enum_item
                .uident()
                .is_some_and(|t| t.text_range() == token.text_range())
        {
            return Some(DefinitionLocation {
                path: path.to_path_buf(),
                range: token.text_range(),
            });
        }
        if let Some(trait_item) = cst::nodes::Trait::cast(node.clone())
            && trait_item
                .uident()
                .is_some_and(|t| t.text_range() == token.text_range())
        {
            return Some(DefinitionLocation {
                path: path.to_path_buf(),
                range: token.text_range(),
            });
        }
        if let Some(variant) = cst::nodes::Variant::cast(node.clone())
            && variant
                .uident()
                .is_some_and(|t| t.text_range() == token.text_range())
        {
            return Some(DefinitionLocation {
                path: path.to_path_buf(),
                range: token.text_range(),
            });
        }
        if let Some(field) = cst::nodes::StructField::cast(node.clone())
            && field
                .lident()
                .is_some_and(|t| t.text_range() == token.text_range())
        {
            return Some(DefinitionLocation {
                path: path.to_path_buf(),
                range: token.text_range(),
            });
        }
        if let Some(method) = cst::nodes::TraitMethod::cast(node.clone())
            && method
                .lident()
                .is_some_and(|t| t.text_range() == token.text_range())
        {
            return Some(DefinitionLocation {
                path: path.to_path_buf(),
                range: token.text_range(),
            });
        }
        if let Some(extern_item) = cst::nodes::Extern::cast(node.clone()) {
            if extern_item.type_keyword().is_some()
                && extern_item
                    .uident()
                    .is_some_and(|t| t.text_range() == token.text_range())
            {
                return Some(DefinitionLocation {
                    path: path.to_path_buf(),
                    range: token.text_range(),
                });
            }
            if extern_item.type_keyword().is_none()
                && extern_item
                    .lident()
                    .is_some_and(|t| t.text_range() == token.text_range())
            {
                return Some(DefinitionLocation {
                    path: path.to_path_buf(),
                    range: token.text_range(),
                });
            }
        }

        current = node.parent();
    }

    None
}

fn local_def_range_from_fn_params(token: &MySyntaxToken, name: &str) -> Option<TextRange> {
    let mut current = token.parent();
    while let Some(node) = current {
        if let Some(function) = cst::nodes::Fn::cast(node.clone()) {
            let params = function.param_list()?.params();
            for param in params {
                let ident = param.lident()?;
                if ident.to_string() == name {
                    return Some(ident.text_range());
                }
            }
            return None;
        }
        current = node.parent();
    }
    None
}

fn tast_ty_constr_candidates(ty: &tast::Ty) -> Vec<String> {
    fn inner(ty: &tast::Ty, out: &mut Vec<String>) {
        match ty {
            tast::Ty::TStruct { name } | tast::Ty::TEnum { name } => out.push(name.clone()),
            tast::Ty::TApp { ty, .. } => inner(ty, out),
            tast::Ty::TSlice { .. } => out.push("Slice".to_string()),
            tast::Ty::TRef { .. } => out.push("Ref".to_string()),
            tast::Ty::TVec { .. } => out.push("Vec".to_string()),
            tast::Ty::THashMap { .. } => out.push("HashMap".to_string()),
            _ => {}
        }
    }

    let mut out = Vec::new();
    inner(ty, &mut out);

    let mut expanded = Vec::new();
    for name in out {
        expanded.push(name.clone());
        if name.contains("::")
            && let Some(last) = name.rsplit("::").next()
            && last != name
        {
            expanded.push(last.to_string());
        }
    }

    expanded
}

fn namespace_definition_location(
    index: &ProjectSymbolIndex,
    namespace: &str,
) -> Option<DefinitionLocation> {
    index
        .find_namespace(namespace)
        .map(|path| DefinitionLocation {
            path,
            range: TextRange::new(text_size::TextSize::from(0), text_size::TextSize::from(0)),
        })
}

fn external_use_definition_location(path: &Path, lookup: &str) -> Option<DefinitionLocation> {
    let (_module_dir, dependencies) =
        crate::pipeline::packages::discover_dependency_versions_from_file(path).ok()?;
    let external_deps = crate::external::resolve_dependency_versions(&dependencies).ok()?;
    let alias = external_deps.alias_for_use_path(lookup)?;
    let namespace_dir = external_deps.namespace_dirs().get(&alias)?.clone();
    let target = namespace_nav_target_in_dir(&namespace_dir)?;
    Some(DefinitionLocation {
        path: target,
        range: TextRange::new(text_size::TextSize::from(0), text_size::TextSize::from(0)),
    })
}
