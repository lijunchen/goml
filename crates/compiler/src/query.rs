use std::path::Path;

use cst::cst::CstNode;
use cst::nodes::BinaryExpr;
use parser::syntax::{MySyntaxKind, MySyntaxNode, MySyntaxNodePtr, MySyntaxToken};
use text_size::TextSize;

use crate::{env::GlobalTypeEnv, mangle::encode_ty, pipeline, tast};

const COMPLETION_PLACEHOLDER: &str = "completion_placeholder";

fn typecheck_single_file(
    path: &Path,
    src: &str,
) -> Result<(tast::File, GlobalTypeEnv, diagnostics::Diagnostics), String> {
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

    let (fir, fir_table) = crate::fir::lower_to_fir(ast);
    let (tast, genv, diagnostics) = crate::typer::check_file(fir, fir_table);

    Ok((tast, genv, diagnostics))
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

    let (tast, genv, _diagnostics) = typecheck_single_file(path, src).or_else(|_| {
        pipeline::typecheck_with_packages(path, src).map_err(|e| format!("{:?}", e))
    })?;

    if let Some(range) = range
        && let Some(ty) = find_type(&tast, &range)
    {
        return Ok(ty);
    }

    if let Some(token) = token.as_ref()
        && let Some(segments) = path_segments_from_token(token)
    {
        if let Some(ty) = lookup_type_from_segments(&genv, &segments)
            .or_else(|| find_type_by_name(&tast, &segments))
        {
            return Ok(ty);
        }
    }

    if let Some(segments) = path_segments_at_offset(src, offset) {
        if let Some(ty) = lookup_type_from_segments(&genv, &segments)
            .or_else(|| find_type_by_name(&tast, &segments))
        {
            return Ok(ty);
        }
    }

    if let Some(range) = range
        && let Some(ty) = lookup_type_from_path_range(&genv, cst.syntax(), &range)
    {
        return Ok(ty);
    }

    if let Some(ty) = lookup_type_from_path_offset(&genv, src, offset) {
        return Ok(ty);
    }

    if let Some(token) = token.as_ref()
        && let Some(expr_ptr) = find_expr_ptr_from_token(token)
    {
        if let Some(expr) = find_expr_for_completion(&tast, &expr_ptr) {
            return Ok(expr.get_ty().to_pretty(80));
        }
    }

    Err("no type information found".to_string())
}

fn find_type(tast: &tast::File, range: &rowan::TextRange) -> Option<String> {
    for item in &tast.toplevels {
        match item {
            tast::Item::ImplBlock(impl_block) => {
                for item in impl_block.methods.iter() {
                    if let Some(t) = find_type_fn(item, range) {
                        return Some(t.clone());
                    }
                }
            }
            tast::Item::Fn(f) => {
                if let Some(t) = find_type_fn(f, range) {
                    return Some(t.clone());
                }
            }
            tast::Item::ExternGo(_) => {}
            tast::Item::ExternType(_) => {}
        }
    }
    None
}

fn find_type_fn(tast: &tast::Fn, range: &rowan::TextRange) -> Option<String> {
    find_type_expr(&tast.body, range)
}

fn find_type_expr(tast: &tast::Expr, range: &rowan::TextRange) -> Option<String> {
    match tast {
        tast::Expr::EVar {
            name: _,
            ty: _,
            astptr,
        } => {
            if astptr.unwrap().text_range().contains_range(*range) {
                return Some(tast.get_ty().to_pretty(80));
            }
            None
        }
        tast::Expr::EPrim { .. } => None,
        tast::Expr::EConstr { .. } => None,
        tast::Expr::ETuple { items, ty: _ } | tast::Expr::EArray { items, ty: _ } => {
            for item in items {
                if let Some(expr) = find_type_expr(item, range) {
                    return Some(expr);
                }
            }
            None
        }
        tast::Expr::EClosure {
            params,
            body,
            ty: _,
            captures: _,
        } => {
            for param in params {
                if let Some(astptr) = param.astptr
                    && astptr.text_range().contains_range(*range)
                {
                    return Some(param.ty.to_pretty(80));
                }
            }
            find_type_expr(body, range)
        }
        tast::Expr::ELet { pat, value, ty: _ } => {
            if let Some(expr) = find_type_pat(pat, range) {
                return Some(expr);
            }
            find_type_expr(value, range)
        }
        tast::Expr::EBlock { exprs, ty: _ } => {
            for expr in exprs {
                if let Some(result) = find_type_expr(expr, range) {
                    return Some(result);
                }
            }
            None
        }
        tast::Expr::EMatch {
            expr,
            arms,
            ty: _,
            astptr,
        } => {
            if let Some(expr) = find_type_expr(expr, range) {
                return Some(expr);
            }
            for arm in arms {
                if let Some(expr) = find_type_pat(&arm.pat, range) {
                    return Some(expr);
                }
                if let Some(expr) = find_type_expr(&arm.body, range) {
                    return Some(expr);
                }
            }
            if let Some(astptr) = astptr
                && astptr.text_range().contains_range(*range)
            {
                return Some(tast.get_ty().to_pretty(80));
            }
            None
        }
        tast::Expr::EIf {
            cond,
            then_branch,
            else_branch,
            ty: _,
        } => {
            if let Some(expr) = find_type_expr(cond, range) {
                return Some(expr);
            }
            if let Some(expr) = find_type_expr(then_branch, range) {
                return Some(expr);
            }
            find_type_expr(else_branch, range)
        }
        tast::Expr::EWhile { cond, body, ty: _ } => {
            if let Some(expr) = find_type_expr(cond, range) {
                return Some(expr);
            }
            find_type_expr(body, range)
        }
        tast::Expr::EGo { expr, ty: _ } => find_type_expr(expr, range),
        tast::Expr::ECall { func, args, ty: _ } => {
            if let Some(expr) = find_type_expr(func, range) {
                return Some(expr);
            }
            for arg in args {
                if let Some(expr) = find_type_expr(arg, range) {
                    return Some(expr);
                }
            }
            None
        }
        tast::Expr::EBinary {
            lhs, rhs, ty: _, ..
        } => {
            if let Some(expr) = find_type_expr(lhs, range) {
                return Some(expr);
            }
            if let Some(expr) = find_type_expr(rhs, range) {
                return Some(expr);
            }
            None
        }
        tast::Expr::EUnary { expr, ty: _, .. } => find_type_expr(expr, range),
        tast::Expr::EProj {
            tuple,
            index: _,
            ty: _,
        } => {
            if let Some(expr) = find_type_expr(tuple, range) {
                return Some(expr);
            }
            None
        }
        tast::Expr::EField {
            expr,
            field_name: _,
            ty: _,
            astptr,
        } => {
            if let Some(astptr) = astptr
                && astptr.text_range().contains_range(*range)
            {
                return Some(tast.get_ty().to_pretty(80));
            }
            if let Some(expr) = find_type_expr(expr, range) {
                return Some(expr);
            }
            None
        }
        tast::Expr::ETraitMethod {
            trait_name: _,
            method_name: _,
            ty: _,
            astptr,
        } => {
            if let Some(astptr) = astptr
                && astptr.text_range().contains_range(*range)
            {
                return Some(tast.get_ty().to_pretty(80));
            }
            None
        }
        tast::Expr::EInherentMethod {
            receiver_ty: _,
            method_name: _,
            ty: _,
            astptr,
        } => {
            if let Some(astptr) = astptr
                && astptr.text_range().contains_range(*range)
            {
                return Some(tast.get_ty().to_pretty(80));
            }
            None
        }
    }
}

fn find_type_pat(tast: &tast::Pat, range: &rowan::TextRange) -> Option<String> {
    match tast {
        tast::Pat::PVar {
            name: _,
            ty: _,
            astptr,
        } => {
            if astptr.unwrap().text_range().contains_range(*range) {
                return Some(tast.get_ty().to_pretty(80));
            }
            None
        }
        tast::Pat::PPrim { value: _, ty: _ } => None,
        tast::Pat::PConstr { args, .. } => {
            for arg in args {
                if let Some(expr) = find_type_pat(arg, range) {
                    return Some(expr);
                }
            }
            None
        }
        tast::Pat::PTuple { items, ty: _ } => {
            for item in items {
                if let Some(expr) = find_type_pat(item, range) {
                    return Some(expr);
                }
            }
            None
        }
        tast::Pat::PWild { ty: _ } => None,
    }
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

    let (tast, genv, _diagnostics) = typecheck_single_file(path, &fixed_src)
        .or_else(|_| {
            pipeline::typecheck_with_packages(path, &fixed_src).map_err(|e| format!("{:?}", e))
        })
        .ok()?;

    let target_expr = find_expr_for_completion(&tast, &lhs_ptr)?;
    let ty = normalize_completion_ty(target_expr.get_ty());
    Some(completions_for_type(&genv, &ty))
}

fn find_expr_for_completion<'a>(
    file: &'a tast::File,
    ptr: &MySyntaxNodePtr,
) -> Option<&'a tast::Expr> {
    for item in &file.toplevels {
        if let Some(expr) = find_expr_in_item(item, ptr) {
            return Some(expr);
        }
    }
    None
}

fn find_expr_in_item<'a>(item: &'a tast::Item, ptr: &MySyntaxNodePtr) -> Option<&'a tast::Expr> {
    match item {
        tast::Item::ImplBlock(block) => {
            for method in &block.methods {
                if let Some(expr) = find_expr_in_expr(&method.body, ptr) {
                    return Some(expr);
                }
            }
            None
        }
        tast::Item::Fn(func) => find_expr_in_expr(&func.body, ptr),
        tast::Item::ExternGo(_) | tast::Item::ExternType(_) => None,
    }
}

fn find_expr_in_expr<'a>(expr: &'a tast::Expr, ptr: &MySyntaxNodePtr) -> Option<&'a tast::Expr> {
    match expr {
        tast::Expr::EVar {
            astptr: Some(astptr),
            ..
        } if astptr == ptr => Some(expr),
        tast::Expr::EVar { .. } => None,
        tast::Expr::EPrim { .. } => None,
        tast::Expr::EConstr { args, .. } => args.iter().find_map(|arg| find_expr_in_expr(arg, ptr)),
        tast::Expr::ETuple { items, .. } | tast::Expr::EArray { items, .. } => {
            items.iter().find_map(|item| find_expr_in_expr(item, ptr))
        }
        tast::Expr::EClosure { body, .. } => find_expr_in_expr(body, ptr),
        tast::Expr::ELet { value, .. } => find_expr_in_expr(value, ptr),
        tast::Expr::EBlock { exprs, .. } => {
            for e in exprs {
                if let Some(found) = find_expr_in_expr(e, ptr) {
                    return Some(found);
                }
            }
            None
        }
        tast::Expr::EMatch {
            expr: scrutinee,
            arms,
            astptr,
            ..
        } => {
            if let Some(astptr) = astptr
                && astptr == ptr
            {
                return Some(expr);
            }
            if let Some(found) = find_expr_in_expr(scrutinee, ptr) {
                return Some(found);
            }
            for arm in arms {
                if let Some(found) = find_expr_in_expr(&arm.body, ptr) {
                    return Some(found);
                }
            }
            None
        }
        tast::Expr::EIf {
            cond,
            then_branch,
            else_branch,
            ..
        } => {
            if let Some(expr) = find_expr_in_expr(cond, ptr) {
                return Some(expr);
            }
            if let Some(expr) = find_expr_in_expr(then_branch, ptr) {
                return Some(expr);
            }
            find_expr_in_expr(else_branch, ptr)
        }
        tast::Expr::EWhile { cond, body, .. } => {
            if let Some(expr) = find_expr_in_expr(cond, ptr) {
                return Some(expr);
            }
            find_expr_in_expr(body, ptr)
        }
        tast::Expr::EGo { expr, .. } => find_expr_in_expr(expr, ptr),
        tast::Expr::ECall { func, args, .. } => {
            if let Some(expr) = find_expr_in_expr(func, ptr) {
                return Some(expr);
            }
            for arg in args {
                if let Some(expr) = find_expr_in_expr(arg, ptr) {
                    return Some(expr);
                }
            }
            None
        }
        tast::Expr::EBinary { lhs, rhs, .. } => {
            if let Some(expr) = find_expr_in_expr(lhs, ptr) {
                return Some(expr);
            }
            find_expr_in_expr(rhs, ptr)
        }
        tast::Expr::EUnary { expr: inner, .. } => find_expr_in_expr(inner, ptr),
        tast::Expr::EProj { tuple, .. } => find_expr_in_expr(tuple, ptr),
        tast::Expr::EField {
            expr: inner,
            astptr,
            ..
        } => {
            if let Some(astptr) = astptr
                && astptr == ptr
            {
                return Some(expr);
            }
            find_expr_in_expr(inner, ptr)
        }
        tast::Expr::ETraitMethod {
            astptr: Some(astptr),
            ..
        } if astptr == ptr => Some(expr),
        tast::Expr::ETraitMethod { .. } => None,
        tast::Expr::EInherentMethod {
            astptr: Some(astptr),
            ..
        } if astptr == ptr => Some(expr),
        tast::Expr::EInherentMethod { .. } => None,
    }
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

    let encoded = encode_ty(ty);
    let mut methods: Vec<DotCompletionItem> =
        if let Some(impl_def) = genv.trait_env.inherent_impls.get(&encoded) {
            impl_def
                .methods
                .iter()
                .map(|(method_name, method_scheme)| DotCompletionItem {
                    name: method_name.clone(),
                    kind: DotCompletionKind::Method,
                    detail: Some(method_scheme.ty.to_pretty(80)),
                })
                .collect()
        } else {
            Vec::new()
        };
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

fn find_type_by_name(file: &tast::File, segments: &[String]) -> Option<String> {
    let full_name = segments.join("::");
    if let Some(ty) = find_type_by_name_in_file(file, &full_name) {
        return Some(ty);
    }
    if let Some(last) = segments.last()
        && let Some(ty) = find_type_by_name_in_file(file, last)
    {
        return Some(ty);
    }
    None
}

fn find_type_by_name_in_file(file: &tast::File, name: &str) -> Option<String> {
    for item in &file.toplevels {
        let expr = match item {
            tast::Item::ImplBlock(block) => {
                let mut found = None;
                for method in &block.methods {
                    if let Some(ty) = find_type_by_name_in_expr(&method.body, name) {
                        found = Some(ty);
                        break;
                    }
                }
                if found.is_some() {
                    return found;
                }
                continue;
            }
            tast::Item::Fn(func) => &func.body,
            tast::Item::ExternGo(_) | tast::Item::ExternType(_) => continue,
        };
        if let Some(ty) = find_type_by_name_in_expr(expr, name) {
            return Some(ty);
        }
    }
    None
}

fn find_type_by_name_in_expr(expr: &tast::Expr, name: &str) -> Option<String> {
    match expr {
        tast::Expr::EVar { name: var, ty, .. } => {
            if var == name {
                Some(ty.to_pretty(80))
            } else {
                None
            }
        }
        tast::Expr::EPrim { .. } => None,
        tast::Expr::EConstr { args, .. } => args
            .iter()
            .find_map(|arg| find_type_by_name_in_expr(arg, name)),
        tast::Expr::ETuple { items, .. } | tast::Expr::EArray { items, .. } => items
            .iter()
            .find_map(|item| find_type_by_name_in_expr(item, name)),
        tast::Expr::EClosure { body, .. } => find_type_by_name_in_expr(body, name),
        tast::Expr::ELet { value, .. } => find_type_by_name_in_expr(value, name),
        tast::Expr::EBlock { exprs, .. } => {
            for e in exprs {
                if let Some(found) = find_type_by_name_in_expr(e, name) {
                    return Some(found);
                }
            }
            None
        }
        tast::Expr::EMatch { expr, arms, .. } => {
            if let Some(found) = find_type_by_name_in_expr(expr, name) {
                return Some(found);
            }
            for arm in arms {
                if let Some(found) = find_type_by_name_in_expr(&arm.body, name) {
                    return Some(found);
                }
            }
            None
        }
        tast::Expr::EIf {
            cond,
            then_branch,
            else_branch,
            ..
        } => find_type_by_name_in_expr(cond, name)
            .or_else(|| find_type_by_name_in_expr(then_branch, name))
            .or_else(|| find_type_by_name_in_expr(else_branch, name)),
        tast::Expr::EWhile { cond, body, .. } => {
            find_type_by_name_in_expr(cond, name).or_else(|| find_type_by_name_in_expr(body, name))
        }
        tast::Expr::EGo { expr, .. } => find_type_by_name_in_expr(expr, name),
        tast::Expr::ECall { func, args, .. } => {
            if let Some(found) = find_type_by_name_in_expr(func, name) {
                return Some(found);
            }
            for arg in args {
                if let Some(found) = find_type_by_name_in_expr(arg, name) {
                    return Some(found);
                }
            }
            None
        }
        tast::Expr::EBinary { lhs, rhs, .. } => {
            find_type_by_name_in_expr(lhs, name).or_else(|| find_type_by_name_in_expr(rhs, name))
        }
        tast::Expr::EUnary { expr: inner, .. } => find_type_by_name_in_expr(inner, name),
        tast::Expr::EProj { tuple, .. } => find_type_by_name_in_expr(tuple, name),
        tast::Expr::EField { expr: inner, .. } => find_type_by_name_in_expr(inner, name),
        tast::Expr::ETraitMethod { .. } => None,
        tast::Expr::EInherentMethod { .. } => None,
    }
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
