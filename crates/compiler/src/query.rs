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
}

impl HirResultsIndex {
    fn new(hir_table: &hir::HirTable) -> Self {
        let mut expr_by_ptr = HashMap::new();
        let mut pat_by_ptr = HashMap::new();

        for idx in 0..hir_table.expr_count() {
            let expr_id = hir::ExprId {
                pkg: hir_table.package(),
                idx: idx as u32,
            };
            if let hir::Expr::ENameRef {
                astptr: Some(astptr),
                ..
            } = hir_table.expr(expr_id)
            {
                expr_by_ptr.insert(*astptr, expr_id);
            }
        }

        for idx in 0..hir_table.pat_count() {
            let pat_id = hir::PatId {
                pkg: hir_table.package(),
                idx: idx as u32,
            };
            if let hir::Pat::PVar { astptr, .. } = hir_table.pat(pat_id) {
                pat_by_ptr.insert(*astptr, pat_id);
            }
        }

        Self {
            expr_by_ptr,
            pat_by_ptr,
        }
    }

    fn expr_id(&self, ptr: &MySyntaxNodePtr) -> Option<hir::ExprId> {
        self.expr_by_ptr.get(ptr).copied()
    }

    fn pat_id(&self, ptr: &MySyntaxNodePtr) -> Option<hir::PatId> {
        self.pat_by_ptr.get(ptr).copied()
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
    if result.has_errors() {
        return Err("parse error".to_string());
    }
    let root = MySyntaxNode::new_root(result.green_node);
    let cst = cst::cst::File::cast(root).ok_or_else(|| "failed to cast CST".to_string())?;

    let lower = ::ast::lower::lower(cst);
    let ast = lower
        .into_result()
        .map_err(|_| "AST lowering error".to_string())?;

    if !ast.imports.is_empty() {
        return Err("package imports are not supported in this context".to_string());
    }

    let ast = crate::derive::expand(ast).map_err(|_| "derive expansion error".to_string())?;

    let (hir, hir_table, mut hir_diagnostics) = crate::hir::lower_to_hir(ast);
    let package = hir.name.0.clone();
    let (hir_table, results, genv, mut diagnostics) = crate::typer::check_file_with_env_and_results(
        hir,
        hir_table,
        GlobalTypeEnv::new(),
        &package,
        HashMap::new(),
    );
    diagnostics.append(&mut hir_diagnostics);

    Ok((hir_table, results, genv, diagnostics))
}

pub fn hover_type(path: &Path, src: &str, line: u32, col: u32) -> Result<String, String> {
    let result = parser::parse(path, src);
    if result.has_errors() {
        return Err("parse error".to_string());
    }
    let root = MySyntaxNode::new_root(result.green_node);
    let cst = cst::cst::File::cast(root).ok_or_else(|| "failed to cast syntax tree".to_string())?;

    let line_index = line_index::LineIndex::new(src);
    let offset = line_index
        .offset(line_index::LineCol { line, col })
        .ok_or_else(|| "failed to get offset from line and column".to_string())?;
    let node = cst.syntax().token_at_offset(offset);
    let token = match node {
        rowan::TokenAtOffset::None => None,
        rowan::TokenAtOffset::Single(x) => {
            if x.kind() == MySyntaxKind::Ident {
                Some(x)
            } else {
                None
            }
        }
        rowan::TokenAtOffset::Between(x, y) => {
            if x.kind() == MySyntaxKind::Ident {
                Some(x)
            } else if y.kind() == MySyntaxKind::Ident {
                Some(y)
            } else {
                None
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

    if let Some(token) = token.as_ref() {
        if let Some(pat_ptr) = find_pat_ptr_from_token(token)
            && let Some(pat_id) = index.pat_id(&pat_ptr)
            && let Some(ty) = results.pat_ty(pat_id)
        {
            return Ok(ty.to_pretty(80));
        }

        if let Some(expr_ptr) = find_expr_ptr_from_token(token)
            && let Some(expr_id) = index.expr_id(&expr_ptr)
            && let Some(ty) = results.expr_ty(expr_id)
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

pub fn dot_completions(
    path: &Path,
    src: &str,
    line: u32,
    col: u32,
) -> Option<Vec<DotCompletionItem>> {
    let line_index = line_index::LineIndex::new(src);
    let offset = line_index.offset(line_index::LineCol { line, col })?;
    if offset == TextSize::from(0) {
        return None;
    }

    let dot_offset = offset.checked_sub(TextSize::from(1))?;

    let mut fixed_src = src.to_string();
    let insert_index = u32::from(offset) as usize;
    fixed_src.insert_str(insert_index, COMPLETION_PLACEHOLDER);

    let result = parser::parse(path, &fixed_src);

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
        typecheck_single_file_for_query(path, &fixed_src)
            .or_else(|_| {
                pipeline::pipeline::typecheck_with_packages_and_results(path, &fixed_src)
                    .map_err(|e| format!("{:?}", e))
            })
            .ok()?;
    let index = HirResultsIndex::new(&hir_table);
    let expr_id = index.expr_id(&lhs_ptr)?;
    let ty = normalize_completion_ty(results.expr_ty(expr_id)?.clone());
    Some(completions_for_type(&genv, &ty))
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

fn type_constructor_name(ty: &tast::Ty) -> Option<&str> {
    match ty {
        tast::Ty::TEnum { name } | tast::Ty::TStruct { name } => Some(name.as_str()),
        tast::Ty::TApp { ty, .. } => type_constructor_name(ty),
        tast::Ty::TRef { elem } => type_constructor_name(elem),
        _ => None,
    }
}

fn find_expr_ptr_from_token(token: &MySyntaxToken) -> Option<MySyntaxNodePtr> {
    let mut current = token.parent();
    while let Some(node) = current {
        if cst::nodes::Expr::can_cast(node.kind()) {
            return Some(MySyntaxNodePtr::new(&node));
        }
        current = node.parent();
    }
    None
}

fn find_pat_ptr_from_token(token: &MySyntaxToken) -> Option<MySyntaxNodePtr> {
    let mut current = token.parent();
    while let Some(node) = current {
        if cst::nodes::Pattern::can_cast(node.kind()) {
            return Some(MySyntaxNodePtr::new(&node));
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
