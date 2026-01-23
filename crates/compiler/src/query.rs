use std::{collections::HashMap, path::Path};

use cst::cst::CstNode;
use cst::nodes::BinaryExpr;
use parser::syntax::{MySyntaxKind, MySyntaxNode, MySyntaxNodePtr, MySyntaxToken};
use text_size::TextSize;

use crate::{env::GlobalTypeEnv, hir, pipeline, tast};

const COMPLETION_PLACEHOLDER: &str = "completion_placeholder";

#[derive(Debug, Clone)]
struct HirResultsIndex {
    expr_by_ptr: HashMap<MySyntaxNodePtr, hir::ExprId>,
    pat_by_ptr: HashMap<MySyntaxNodePtr, hir::PatId>,
    local_by_ptr: HashMap<MySyntaxNodePtr, hir::LocalId>,
}

impl HirResultsIndex {
    fn new(hir_table: &hir::HirTable) -> Self {
        let mut expr_by_ptr = HashMap::new();
        let mut pat_by_ptr = HashMap::new();
        let mut local_by_ptr = HashMap::new();

        for idx in 0..hir_table.expr_count() {
            let expr_id = hir::ExprId {
                pkg: hir_table.package(),
                idx: idx as u32,
            };
            if let Some(ptr) = hir_table.expr_ptr(expr_id) {
                expr_by_ptr.insert(ptr, expr_id);
            }
        }

        for idx in 0..hir_table.pat_count() {
            let pat_id = hir::PatId {
                pkg: hir_table.package(),
                idx: idx as u32,
            };
            if let Some(ptr) = hir_table.pat_ptr(pat_id) {
                pat_by_ptr.insert(ptr, pat_id);
            }
        }

        for (local_id, _info) in hir_table.iter_locals() {
            if let Some(ptr) = hir_table.local_origin_ptr(local_id) {
                local_by_ptr.insert(ptr, local_id);
            }
        }

        Self {
            expr_by_ptr,
            pat_by_ptr,
            local_by_ptr,
        }
    }

    fn expr_id(&self, ptr: &MySyntaxNodePtr) -> Option<hir::ExprId> {
        self.expr_by_ptr.get(ptr).copied()
    }

    fn pat_id(&self, ptr: &MySyntaxNodePtr) -> Option<hir::PatId> {
        self.pat_by_ptr.get(ptr).copied()
    }

    fn local_id(&self, ptr: &MySyntaxNodePtr) -> Option<hir::LocalId> {
        self.local_by_ptr.get(ptr).copied()
    }
}

fn typecheck_single_file_for_query(
    path: &Path,
    src: &str,
) -> Result<
    (
        hir::HirTable,
        crate::typer::results::TypeckResults,
        GlobalTypeEnv,
        diagnostics::Diagnostics,
    ),
    String,
> {
    let result = parser::parse(path, src);
    let (green_node, mut parse_diagnostics) = result.into_parts();
    let root = MySyntaxNode::new_root(green_node);
    let cst = cst::cst::File::cast(root).ok_or_else(|| "failed to cast CST".to_string())?;

    let lower = ::ast::lower::lower(cst);
    let (ast, mut lower_diagnostics) = lower.into_parts();
    parse_diagnostics.append(&mut lower_diagnostics);
    let ast = ast.ok_or_else(|| "AST lowering error".to_string())?;

    if !ast.imports.is_empty() {
        return Err("package imports are not supported in this context".to_string());
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
            &package,
            HashMap::new(),
        );
    type_diagnostics.append(&mut hir_diagnostics);
    parse_diagnostics.append(&mut type_diagnostics);

    Ok((hir_table, results, genv, parse_diagnostics))
}

pub fn hover_type(path: &Path, src: &str, line: u32, col: u32) -> Result<String, String> {
    let result = parser::parse(path, src);
    let root = MySyntaxNode::new_root(result.green_node);
    let cst = cst::cst::File::cast(root).ok_or_else(|| "failed to cast syntax tree".to_string())?;

    let line_index = line_index::LineIndex::new(src);
    let offset = line_index
        .offset(line_index::LineCol { line, col })
        .ok_or_else(|| "failed to get offset from line and column".to_string())?;
    let node = cst.syntax().token_at_offset(offset);
    let token = match node {
        rowan::TokenAtOffset::None => None,
        rowan::TokenAtOffset::Single(x) => Some(x),
        rowan::TokenAtOffset::Between(x, y) => {
            if x.kind() == MySyntaxKind::Ident {
                Some(x)
            } else {
                Some(y)
            }
        }
    };
    let range = token.as_ref().map(|tok| tok.text_range());

    let (hir_table, results, genv, _diagnostics) = typecheck_single_file_for_query(path, src)
        .or_else(|_| {
            pipeline::pipeline::typecheck_with_packages_and_results(path, src)
                .map_err(|e| format!("{:?}", e))
        })?;
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
        && let Some(ty) = lookup_type_from_segments(&genv, &segments)
    {
        return Ok(ty);
    }

    if let Some(segments) = path_segments_at_offset(src, offset)
        && let Some(ty) = lookup_type_from_segments(&genv, &segments)
    {
        return Ok(ty);
    }

    if let Some(range) = range
        && let Some(ty) = lookup_type_from_path_range(&genv, cst.syntax(), &range)
    {
        return Ok(ty);
    }

    if let Some(ty) = lookup_type_from_path_offset(&genv, src, offset) {
        return Ok(ty);
    }

    Err("no type information found".to_string())
}

#[derive(Debug, Clone)]
struct ClosureParamIndex {
    local_by_ptr: HashMap<MySyntaxNodePtr, hir::LocalId>,
}

impl ClosureParamIndex {
    fn new(hir_table: &hir::HirTable) -> Self {
        let mut local_by_ptr = HashMap::new();
        for idx in 0..hir_table.expr_count() {
            let expr_id = hir::ExprId {
                pkg: hir_table.package(),
                idx: idx as u32,
            };
            if let hir::Expr::EClosure { params, .. } = hir_table.expr(expr_id) {
                for param in params {
                    local_by_ptr.insert(param.astptr, param.name);
                }
            }
        }
        Self { local_by_ptr }
    }

    fn local_id(&self, ptr: &MySyntaxNodePtr) -> Option<hir::LocalId> {
        self.local_by_ptr.get(ptr).copied()
    }
}

fn param_type_from_token(
    token: &MySyntaxToken,
    closure_params: &ClosureParamIndex,
    results: &crate::typer::results::TypeckResults,
) -> Option<String> {
    let mut current = token.parent();
    while let Some(node) = current {
        if let Some(param) = cst::nodes::Param::cast(node.clone()) {
            return param.ty().map(|t| t.to_string());
        }
        if let Some(param) = cst::nodes::ClosureParam::cast(node.clone()) {
            if let Some(ty) = param.ty() {
                return Some(ty.to_string());
            }
            let ptr = MySyntaxNodePtr::new(param.syntax());
            let local_id = closure_params.local_id(&ptr)?;
            return results.local_ty(local_id).map(|t| t.to_pretty(80));
        }
        current = node.parent();
    }
    None
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DotCompletionKind {
    Field,
    Method,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DotCompletionItem {
    pub name: String,
    pub kind: DotCompletionKind,
    pub detail: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ColonColonCompletionKind {
    Type,
    Value,
    Trait,
    Variant,
    Method,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ColonColonCompletionItem {
    pub name: String,
    pub kind: ColonColonCompletionKind,
    pub detail: Option<String>,
}

pub fn dot_completions(
    path: &Path,
    src: &str,
    line: u32,
    col: u32,
) -> Option<Vec<DotCompletionItem>> {
    let line_index = line_index::LineIndex::new(src);
    let offset = line_index.offset(line_index::LineCol { line, col })?;
    let (prefix_start, prefix) = ident_prefix_at_offset(src, offset)?;
    let dot_offset = prefix_start.checked_sub(TextSize::from(1))?;
    if src.as_bytes().get(u32::from(dot_offset) as usize) != Some(&b'.') {
        return None;
    }

    let parse_src = if prefix.is_empty() {
        let mut fixed_src = src.to_string();
        let insert_index = u32::from(offset) as usize;
        fixed_src.insert_str(insert_index, COMPLETION_PLACEHOLDER);
        fixed_src
    } else {
        src.to_string()
    };

    let result = parser::parse(path, &parse_src);

    let root = MySyntaxNode::new_root(result.green_node);
    let file = cst::cst::File::cast(root.clone())?;
    let dot_token = match file.syntax().token_at_offset(dot_offset) {
        rowan::TokenAtOffset::None => return None,
        rowan::TokenAtOffset::Single(token) => token,
        rowan::TokenAtOffset::Between(left, right) => {
            if right.kind() == MySyntaxKind::Dot {
                right
            } else if left.kind() == MySyntaxKind::Dot {
                left
            } else {
                return None;
            }
        }
    };

    if dot_token.kind() != MySyntaxKind::Dot {
        return None;
    }

    let mut current = dot_token.parent();
    let mut binary_node = None;
    while let Some(node) = current {
        if node.kind() == MySyntaxKind::EXPR_BINARY {
            binary_node = Some(node);
            break;
        }
        current = node.parent();
    }
    let binary_node = binary_node?;
    let binary_expr = BinaryExpr::cast(binary_node)?;
    binary_expr
        .op()
        .map(|tok| tok.kind())
        .filter(|kind| *kind == MySyntaxKind::Dot)?;

    let mut exprs = binary_expr.exprs();
    let lhs_expr = exprs.next()?;
    let lhs_ptr = MySyntaxNodePtr::new(lhs_expr.syntax());

    let (hir_table, results, genv, _diagnostics) =
        typecheck_single_file_for_query(path, &parse_src)
            .or_else(|_| {
                pipeline::pipeline::typecheck_with_packages_and_results(path, &parse_src)
                    .map_err(|e| format!("{:?}", e))
            })
            .ok()?;
    let index = HirResultsIndex::new(&hir_table);
    let expr_id = index.expr_id(&lhs_ptr)?;
    let ty = normalize_completion_ty(results.expr_ty(expr_id)?.clone());
    let items = completions_for_type(&genv, &ty);
    Some(filter_dot_items(items, &prefix))
}

fn normalize_completion_ty(ty: tast::Ty) -> tast::Ty {
    match ty {
        tast::Ty::TRef { elem } => normalize_completion_ty(*elem),
        other => other,
    }
}

fn completions_for_type(genv: &GlobalTypeEnv, ty: &tast::Ty) -> Vec<DotCompletionItem> {
    let mut items = Vec::new();

    if let Some(name) = type_constructor_name(ty) {
        let uident = tast::TastIdent(name.to_string());
        if let Some(struct_def) = genv.structs().get(&uident) {
            for (field_name, field_ty) in &struct_def.fields {
                items.push(DotCompletionItem {
                    name: field_name.0.clone(),
                    kind: DotCompletionKind::Field,
                    detail: Some(field_ty.to_pretty(80)),
                });
            }
        }
    }

    let mut methods: Vec<DotCompletionItem> = Vec::new();
    if let Some(impl_def) = genv
        .trait_env
        .inherent_impls
        .get(&crate::env::InherentImplKey::Exact(ty.clone()))
    {
        methods.extend(impl_def.methods.iter().map(|(method_name, method_scheme)| {
            DotCompletionItem {
                name: method_name.clone(),
                kind: DotCompletionKind::Method,
                detail: Some(method_scheme.ty.to_pretty(80)),
            }
        }));
    }
    if let tast::Ty::TApp { ty, .. } = ty {
        let base_name = ty.get_constr_name_unsafe();
        if let Some(impl_def) = genv
            .trait_env
            .inherent_impls
            .get(&crate::env::InherentImplKey::Constr(base_name))
        {
            methods.extend(impl_def.methods.iter().map(|(method_name, method_scheme)| {
                DotCompletionItem {
                    name: method_name.clone(),
                    kind: DotCompletionKind::Method,
                    detail: Some(method_scheme.ty.to_pretty(80)),
                }
            }));
        }
    }
    methods.sort_by(|a, b| a.name.cmp(&b.name));
    items.extend(methods);

    items
}

pub fn colon_colon_completions(
    path: &Path,
    src: &str,
    line: u32,
    col: u32,
) -> Option<Vec<ColonColonCompletionItem>> {
    let line_index = line_index::LineIndex::new(src);
    let offset = line_index.offset(line_index::LineCol { line, col })?;
    let (prefix_start, prefix) = ident_prefix_at_offset(src, offset)?;
    let colon_start = prefix_start.checked_sub(TextSize::from(2))?;
    if src
        .as_bytes()
        .get(u32::from(colon_start) as usize..u32::from(prefix_start) as usize)
        != Some(b"::")
    {
        return None;
    }

    let parse_src = if prefix.is_empty() {
        let mut fixed_src = src.to_string();
        let insert_index = u32::from(offset) as usize;
        fixed_src.insert_str(insert_index, COMPLETION_PLACEHOLDER);
        fixed_src
    } else {
        src.to_string()
    };

    let result = parser::parse(path, &parse_src);
    let root = MySyntaxNode::new_root(result.green_node);
    let file = cst::cst::File::cast(root.clone())?;

    let focus_offset = if prefix.is_empty() {
        offset
    } else {
        offset.checked_sub(TextSize::from(1))?
    };

    let token = match file.syntax().token_at_offset(focus_offset) {
        rowan::TokenAtOffset::None => None,
        rowan::TokenAtOffset::Single(x) => Some(x),
        rowan::TokenAtOffset::Between(x, y) => {
            if y.kind() == MySyntaxKind::Ident {
                Some(y)
            } else {
                Some(x)
            }
        }
    }?;

    let path_node = ancestor_path_from_token(&token)?;
    let segments = path_node
        .ident_tokens()
        .map(|tok| tok.to_string())
        .collect::<Vec<_>>();
    if segments.is_empty() {
        return None;
    }
    let namespace = segments[..segments.len().saturating_sub(1)].join("::");
    if namespace.is_empty() {
        return None;
    }

    let (_hir_table, _results, genv, _diagnostics) =
        typecheck_single_file_for_query(path, &parse_src)
            .or_else(|_| {
                pipeline::pipeline::typecheck_with_packages_and_results(path, &parse_src)
                    .map_err(|e| format!("{:?}", e))
            })
            .ok()?;

    let mut items = colon_colon_items_for_namespace(&genv, &namespace);
    items.sort_by(|a, b| a.name.cmp(&b.name));
    items.retain(|item| item.name.starts_with(&prefix));
    Some(items)
}

fn type_constructor_name(ty: &tast::Ty) -> Option<&str> {
    match ty {
        tast::Ty::TEnum { name } | tast::Ty::TStruct { name } => Some(name.as_str()),
        tast::Ty::TApp { ty, .. } => type_constructor_name(ty),
        tast::Ty::TRef { elem } => type_constructor_name(elem),
        _ => None,
    }
}

fn find_mapped_expr_id_from_token(
    token: &MySyntaxToken,
    index: &HirResultsIndex,
) -> Option<hir::ExprId> {
    let mut current = token.parent();
    while let Some(node) = current {
        if cst::nodes::Expr::can_cast(node.kind()) {
            let ptr = MySyntaxNodePtr::new(&node);
            if let Some(id) = index.expr_id(&ptr) {
                return Some(id);
            }
        }
        current = node.parent();
    }
    None
}

fn find_mapped_pat_id_from_token(
    token: &MySyntaxToken,
    index: &HirResultsIndex,
) -> Option<hir::PatId> {
    let mut current = token.parent();
    while let Some(node) = current {
        if cst::nodes::Pattern::can_cast(node.kind()) {
            let ptr = MySyntaxNodePtr::new(&node);
            if let Some(id) = index.pat_id(&ptr) {
                return Some(id);
            }
        }
        current = node.parent();
    }
    None
}

fn find_mapped_local_id_from_token(
    token: &MySyntaxToken,
    index: &HirResultsIndex,
) -> Option<hir::LocalId> {
    let mut current = token.parent();
    while let Some(node) = current {
        let ptr = MySyntaxNodePtr::new(&node);
        if let Some(id) = index.local_id(&ptr) {
            return Some(id);
        }
        current = node.parent();
    }
    None
}

fn lookup_type_from_path_range(
    genv: &GlobalTypeEnv,
    root: &MySyntaxNode,
    range: &rowan::TextRange,
) -> Option<String> {
    let segments = path_segments_at_range(root, range)?;
    if segments.is_empty() {
        return None;
    }
    lookup_type_from_segments(genv, &segments)
}

fn path_segments_at_range(root: &MySyntaxNode, range: &rowan::TextRange) -> Option<Vec<String>> {
    root.descendants().find_map(|node| {
        if !node.text_range().contains_range(*range) {
            return None;
        }
        let path = cst::nodes::Path::cast(node)?;
        let segments = path
            .ident_tokens()
            .map(|tok| tok.to_string())
            .collect::<Vec<_>>();
        if segments.is_empty() {
            None
        } else {
            Some(segments)
        }
    })
}

fn lookup_type_from_path_offset(
    genv: &GlobalTypeEnv,
    src: &str,
    offset: TextSize,
) -> Option<String> {
    let segments = path_segments_at_offset(src, offset)?;
    if segments.is_empty() {
        return None;
    }
    lookup_type_from_segments(genv, &segments)
}

fn path_segments_at_offset(src: &str, offset: TextSize) -> Option<Vec<String>> {
    let mut idx = u32::from(offset) as usize;
    if idx >= src.len() {
        return None;
    }
    let bytes = src.as_bytes();
    if !is_path_char(bytes[idx]) && idx > 0 && is_path_char(bytes[idx - 1]) {
        idx -= 1;
    }
    if !is_path_char(bytes[idx]) {
        return None;
    }
    let mut start = idx;
    while start > 0 && is_path_char(bytes[start - 1]) {
        start -= 1;
    }
    let mut end = idx + 1;
    while end < bytes.len() && is_path_char(bytes[end]) {
        end += 1;
    }
    let slice = &src[start..end];
    let trimmed = slice.trim_matches(':');
    if trimmed.is_empty() {
        return None;
    }
    let segments = trimmed
        .split("::")
        .filter(|s| !s.is_empty())
        .map(|s| s.to_string())
        .collect::<Vec<_>>();
    if segments.is_empty() {
        None
    } else {
        Some(segments)
    }
}

fn is_path_char(b: u8) -> bool {
    matches!(b, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' | b':')
}

fn is_ident_char(b: u8) -> bool {
    matches!(b, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_')
}

fn ident_prefix_at_offset(src: &str, offset: TextSize) -> Option<(TextSize, String)> {
    let mut idx = u32::from(offset) as usize;
    let bytes = src.as_bytes();
    if idx > bytes.len() {
        return None;
    }
    while idx > 0 && is_ident_char(bytes[idx - 1]) {
        idx -= 1;
    }
    let start = idx;
    let end = u32::from(offset) as usize;
    let prefix = src.get(start..end)?.to_string();
    Some((TextSize::from(start as u32), prefix))
}

fn filter_dot_items(items: Vec<DotCompletionItem>, prefix: &str) -> Vec<DotCompletionItem> {
    if prefix.is_empty() {
        return items;
    }
    items
        .into_iter()
        .filter(|item| item.name.starts_with(prefix))
        .collect()
}

fn ancestor_path_from_token(token: &MySyntaxToken) -> Option<cst::nodes::Path> {
    let mut current = token.parent();
    while let Some(node) = current {
        if let Some(path) = cst::nodes::Path::cast(node.clone()) {
            return Some(path);
        }
        current = node.parent();
    }
    None
}

fn colon_colon_items_for_namespace(
    genv: &GlobalTypeEnv,
    namespace: &str,
) -> Vec<ColonColonCompletionItem> {
    let mut items = Vec::new();

    if let Some(enum_def) = genv.enums().get(&tast::TastIdent(namespace.to_string())) {
        for (variant_name, payload) in &enum_def.variants {
            let detail = if payload.is_empty() {
                Some(namespace.to_string())
            } else {
                let payload_str = payload
                    .iter()
                    .map(|ty| ty.to_pretty(80))
                    .collect::<Vec<_>>()
                    .join(", ");
                Some(format!("({}) -> {}", payload_str, namespace))
            };
            items.push(ColonColonCompletionItem {
                name: variant_name.0.clone(),
                kind: ColonColonCompletionKind::Variant,
                detail,
            });
        }
        items.extend(colon_colon_inherent_methods(
            genv,
            tast::Ty::TEnum {
                name: namespace.to_string(),
            },
        ));
        return items;
    }

    if genv.trait_env.trait_defs.contains_key(namespace) {
        if let Some(trait_def) = genv.trait_env.trait_defs.get(namespace) {
            for (method_name, scheme) in trait_def.methods.iter() {
                items.push(ColonColonCompletionItem {
                    name: method_name.clone(),
                    kind: ColonColonCompletionKind::Method,
                    detail: Some(scheme.ty.to_pretty(80)),
                });
            }
        }
        return items;
    }

    if genv
        .structs()
        .contains_key(&tast::TastIdent(namespace.to_string()))
    {
        items.extend(colon_colon_inherent_methods(
            genv,
            tast::Ty::TStruct {
                name: namespace.to_string(),
            },
        ));
        return items;
    }

    let ns_prefix = format!("{}::", namespace);

    for name in genv.type_env.enums.keys() {
        if let Some(member) = strip_namespace_member(&name.0, &ns_prefix) {
            items.push(ColonColonCompletionItem {
                name: member.to_string(),
                kind: ColonColonCompletionKind::Type,
                detail: Some("enum".to_string()),
            });
        }
    }
    for name in genv.type_env.structs.keys() {
        if let Some(member) = strip_namespace_member(&name.0, &ns_prefix) {
            items.push(ColonColonCompletionItem {
                name: member.to_string(),
                kind: ColonColonCompletionKind::Type,
                detail: Some("struct".to_string()),
            });
        }
    }
    for name in genv.trait_env.trait_defs.keys() {
        if let Some(member) = strip_namespace_member(name, &ns_prefix) {
            items.push(ColonColonCompletionItem {
                name: member.to_string(),
                kind: ColonColonCompletionKind::Trait,
                detail: None,
            });
        }
    }
    for name in genv.value_env.funcs.keys() {
        if let Some(member) = strip_namespace_member(name, &ns_prefix) {
            items.push(ColonColonCompletionItem {
                name: member.to_string(),
                kind: ColonColonCompletionKind::Value,
                detail: Some("fn".to_string()),
            });
        }
    }

    items
}

fn strip_namespace_member<'a>(full: &'a str, ns_prefix: &str) -> Option<&'a str> {
    if !full.starts_with(ns_prefix) {
        return None;
    }
    let rest = &full[ns_prefix.len()..];
    if rest.is_empty() || rest.contains("::") {
        None
    } else {
        Some(rest)
    }
}

fn colon_colon_inherent_methods(
    genv: &GlobalTypeEnv,
    receiver_ty: tast::Ty,
) -> Vec<ColonColonCompletionItem> {
    let mut items = Vec::new();
    if let Some(impl_def) = genv
        .trait_env
        .inherent_impls
        .get(&crate::env::InherentImplKey::Exact(receiver_ty.clone()))
    {
        items.extend(impl_def.methods.iter().map(|(method_name, method_scheme)| {
            ColonColonCompletionItem {
                name: method_name.clone(),
                kind: ColonColonCompletionKind::Method,
                detail: Some(method_scheme.ty.to_pretty(80)),
            }
        }));
    }
    if let tast::Ty::TApp { ty, .. } = receiver_ty {
        let base_name = ty.get_constr_name_unsafe();
        if let Some(impl_def) = genv
            .trait_env
            .inherent_impls
            .get(&crate::env::InherentImplKey::Constr(base_name))
        {
            items.extend(impl_def.methods.iter().map(|(method_name, method_scheme)| {
                ColonColonCompletionItem {
                    name: method_name.clone(),
                    kind: ColonColonCompletionKind::Method,
                    detail: Some(method_scheme.ty.to_pretty(80)),
                }
            }));
        }
    }
    items.sort_by(|a, b| a.name.cmp(&b.name));
    items
}

fn path_segments_from_token(token: &MySyntaxToken) -> Option<Vec<String>> {
    let mut current = token.parent();
    while let Some(node) = current {
        if let Some(path) = cst::nodes::Path::cast(node.clone()) {
            let segments = path
                .ident_tokens()
                .map(|tok| tok.to_string())
                .collect::<Vec<_>>();
            if segments.is_empty() {
                return None;
            }
            return Some(segments);
        }
        current = node.parent();
    }
    None
}

fn lookup_type_from_segments(genv: &GlobalTypeEnv, segments: &[String]) -> Option<String> {
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
    let segment_strs = segments.iter().map(|s| s.as_str()).collect::<Vec<_>>();
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
