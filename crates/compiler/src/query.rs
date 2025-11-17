use cst::cst::CstNode;
use cst::nodes::BinaryExpr;
use parser::syntax::{MySyntaxKind, MySyntaxNode, MySyntaxNodePtr};
use text_size::TextSize;

use crate::{env::GlobalTypeEnv, tast, type_encoding::encode_ty};

const COMPLETION_PLACEHOLDER: &str = "completion_placeholder";

pub fn hover_type(src: &str, line: u32, col: u32) -> Option<String> {
    let result = parser::parse(&std::path::PathBuf::from("dummy"), src);
    if result.has_errors() {
        return None;
    }
    let root = MySyntaxNode::new_root(result.green_node);
    let cst = cst::cst::File::cast(root).unwrap_or_else(|| panic!("failed to cast syntax tree"));

    let line_index = line_index::LineIndex::new(src);
    let offset = line_index
        .offset(line_index::LineCol { line, col })
        .unwrap_or_else(|| panic!("failed to get offset from line and column"));
    let node = cst.syntax().token_at_offset(offset);
    let range = match node {
        rowan::TokenAtOffset::None => None,
        rowan::TokenAtOffset::Single(x) => {
            if x.kind() == MySyntaxKind::Ident {
                Some(x.text_range())
            } else {
                None
            }
        }
        rowan::TokenAtOffset::Between(x, y) => {
            if x.kind() == MySyntaxKind::Ident {
                Some(x.text_range())
            } else if y.kind() == MySyntaxKind::Ident {
                Some(y.text_range())
            } else {
                None
            }
        }
    }?;

    let lower = ast::lower::lower(cst);
    let ast = match lower.into_result() {
        Ok(ast) => ast,
        Err(_) => return None,
    };
    let (tast, genv) = crate::typer::check_file(ast);

    let ty = find_type(&genv, &tast, &range);

    ty.map(|node| node.to_string())
}

fn find_type(genv: &GlobalTypeEnv, tast: &tast::File, range: &rowan::TextRange) -> Option<String> {
    for item in &tast.toplevels {
        match item {
            tast::Item::ImplBlock(impl_block) => {
                for item in impl_block.methods.iter() {
                    if let Some(t) = find_type_fn(genv, item, range) {
                        return Some(t.clone());
                    }
                }
            }
            tast::Item::Fn(f) => {
                if let Some(t) = find_type_fn(genv, f, range) {
                    return Some(t.clone());
                }
            }
            tast::Item::ExternGo(_) => {}
            tast::Item::ExternType(_) => {}
        }
    }
    None
}

fn find_type_fn(genv: &GlobalTypeEnv, tast: &tast::Fn, range: &rowan::TextRange) -> Option<String> {
    find_type_expr(genv, &tast.body, range)
}

fn find_type_expr(
    genv: &GlobalTypeEnv,
    tast: &tast::Expr,
    range: &rowan::TextRange,
) -> Option<String> {
    match tast {
        tast::Expr::EVar {
            name: _,
            ty: _,
            astptr,
        } => {
            if astptr.unwrap().text_range().contains_range(*range) {
                return Some(tast.get_ty().to_pretty(genv, 80));
            }
            None
        }
        tast::Expr::EPrim { .. } => None,
        tast::Expr::EConstr { .. } => None,
        tast::Expr::ETuple { items, ty: _ } | tast::Expr::EArray { items, ty: _ } => {
            for item in items {
                if let Some(expr) = find_type_expr(genv, item, range) {
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
                    return Some(param.ty.to_pretty(genv, 80));
                }
            }
            find_type_expr(genv, body, range)
        }
        tast::Expr::ELet {
            pat,
            value,
            body,
            ty: _,
        } => {
            if let Some(expr) = find_type_pat(genv, pat, range) {
                return Some(expr);
            }
            if let Some(expr) = find_type_expr(genv, value, range) {
                return Some(expr);
            }
            find_type_expr(genv, body, range)
        }
        tast::Expr::EMatch {
            expr,
            arms,
            ty: _,
            astptr,
        } => {
            if let Some(expr) = find_type_expr(genv, expr, range) {
                return Some(expr);
            }
            for arm in arms {
                if let Some(expr) = find_type_pat(genv, &arm.pat, range) {
                    return Some(expr);
                }
                if let Some(expr) = find_type_expr(genv, &arm.body, range) {
                    return Some(expr);
                }
            }
            if let Some(astptr) = astptr
                && astptr.text_range().contains_range(*range)
            {
                return Some(tast.get_ty().to_pretty(genv, 80));
            }
            None
        }
        tast::Expr::EIf {
            cond,
            then_branch,
            else_branch,
            ty: _,
        } => {
            if let Some(expr) = find_type_expr(genv, cond, range) {
                return Some(expr);
            }
            if let Some(expr) = find_type_expr(genv, then_branch, range) {
                return Some(expr);
            }
            find_type_expr(genv, else_branch, range)
        }
        tast::Expr::EWhile { cond, body, ty: _ } => {
            if let Some(expr) = find_type_expr(genv, cond, range) {
                return Some(expr);
            }
            find_type_expr(genv, body, range)
        }
        tast::Expr::ECall { func, args, ty: _ } => {
            if let Some(expr) = find_type_expr(genv, func, range) {
                return Some(expr);
            }
            for arg in args {
                if let Some(expr) = find_type_expr(genv, arg, range) {
                    return Some(expr);
                }
            }
            None
        }
        tast::Expr::EBinary {
            lhs, rhs, ty: _, ..
        } => {
            if let Some(expr) = find_type_expr(genv, lhs, range) {
                return Some(expr);
            }
            if let Some(expr) = find_type_expr(genv, rhs, range) {
                return Some(expr);
            }
            None
        }
        tast::Expr::EUnary { expr, ty: _, .. } => find_type_expr(genv, expr, range),
        tast::Expr::EProj {
            tuple,
            index: _,
            ty: _,
        } => {
            if let Some(expr) = find_type_expr(genv, tuple, range) {
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
                return Some(tast.get_ty().to_pretty(genv, 80));
            }
            if let Some(expr) = find_type_expr(genv, expr, range) {
                return Some(expr);
            }
            None
        }
    }
}

fn find_type_pat(
    genv: &GlobalTypeEnv,
    tast: &tast::Pat,
    range: &rowan::TextRange,
) -> Option<String> {
    match tast {
        tast::Pat::PVar {
            name: _,
            ty: _,
            astptr,
        } => {
            if astptr.unwrap().text_range().contains_range(*range) {
                return Some(tast.get_ty().to_pretty(genv, 80));
            }
            None
        }
        tast::Pat::PPrim { value: _, ty: _ } => None,
        tast::Pat::PConstr { args, .. } => {
            for arg in args {
                if let Some(expr) = find_type_pat(genv, arg, range) {
                    return Some(expr);
                }
            }
            None
        }
        tast::Pat::PTuple { items, ty: _ } => {
            for item in items {
                if let Some(expr) = find_type_pat(genv, item, range) {
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

pub fn dot_completions(src: &str, line: u32, col: u32) -> Option<Vec<DotCompletionItem>> {
    let line_index = line_index::LineIndex::new(src);
    let offset = line_index.offset(line_index::LineCol { line, col })?;
    if offset == TextSize::from(0) {
        return None;
    }

    let dot_offset = offset.checked_sub(TextSize::from(1))?;

    let mut fixed_src = src.to_string();
    let insert_index = u32::from(offset) as usize;
    fixed_src.insert_str(insert_index, COMPLETION_PLACEHOLDER);

    let result = parser::parse(&std::path::PathBuf::from("dummy"), &fixed_src);

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
    if binary_expr
        .op()
        .map(|tok| tok.kind())
        .filter(|kind| *kind == MySyntaxKind::Dot)
        .is_none()
    {
        return None;
    }

    let mut exprs = binary_expr.exprs();
    let lhs_expr = exprs.next()?;
    let lhs_ptr = MySyntaxNodePtr::new(lhs_expr.syntax());

    let lower = ast::lower::lower(file);
    let ast = lower.into_result().ok()?;
    let (tast, genv) = crate::typer::check_file(ast);

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
        tast::Expr::ELet { value, body, .. } => {
            if let Some(expr) = find_expr_in_expr(value, ptr) {
                return Some(expr);
            }
            find_expr_in_expr(body, ptr)
        }
        tast::Expr::EMatch {
            expr: scrutinee,
            arms,
            astptr,
            ..
        } => {
            if let Some(astptr) = astptr {
                if astptr == ptr {
                    return Some(expr);
                }
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
            if let Some(astptr) = astptr {
                if astptr == ptr {
                    return Some(expr);
                }
            }
            find_expr_in_expr(inner, ptr)
        }
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
        let uident = ast::ast::Ident::new(name);
        if let Some(struct_def) = genv.structs.get(&uident) {
            for (field_name, field_ty) in &struct_def.fields {
                items.push(DotCompletionItem {
                    name: field_name.0.clone(),
                    kind: DotCompletionKind::Field,
                    detail: Some(field_ty.to_pretty(genv, 80)),
                });
            }
        }
    }

    let encoded = encode_ty(ty);
    let mut methods: Vec<DotCompletionItem> = genv
        .inherent_impls
        .iter()
        .filter_map(|((ty_key, method_name), (_, method_ty))| {
            if ty_key == &encoded {
                Some(DotCompletionItem {
                    name: method_name.0.clone(),
                    kind: DotCompletionKind::Method,
                    detail: Some(method_ty.to_pretty(genv, 80)),
                })
            } else {
                None
            }
        })
        .collect();
    methods.sort_by(|a, b| a.name.cmp(&b.name));
    items.extend(methods);

    items
}

fn type_constructor_name<'a>(ty: &'a tast::Ty) -> Option<&'a str> {
    match ty {
        tast::Ty::TCon { name } => Some(name.as_str()),
        tast::Ty::TApp { ty, .. } => type_constructor_name(ty),
        tast::Ty::TRef { elem } => type_constructor_name(elem),
        _ => None,
    }
}
