use cst::cst::CstNode;
use parser::syntax::{MySyntaxKind, MySyntaxNode};

use crate::{env::GlobalTypeEnv, tast};

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
            if x.kind() == MySyntaxKind::Lident {
                Some(x.text_range())
            } else {
                None
            }
        }
        rowan::TokenAtOffset::Between(x, y) => {
            if x.kind() == MySyntaxKind::Lident {
                Some(x.text_range())
            } else if y.kind() == MySyntaxKind::Lident {
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
    let (tast, env) = crate::typer::check_file(ast);

    let ty = find_type(&env, &tast, &range);

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
        tast::Expr::EMatch { expr, arms, ty: _ } => {
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
        } => {
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
