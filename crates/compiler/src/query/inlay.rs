use std::path::Path;

use parser::syntax::MySyntaxNodePtr;

use crate::{hir, tast};

use super::{InlayHintItem, InlayHintKind, typecheck::typecheck_for_query};

pub fn inlay_hints(path: &Path, src: &str) -> Option<Vec<InlayHintItem>> {
    let (hir_table, results, _genv, _diagnostics) = typecheck_for_query(path, src).ok()?;

    let mut hints = Vec::new();
    for idx in 0..hir_table.def_count() {
        let def_id = hir_table.def_id_at(idx);
        match hir_table.def(def_id) {
            hir::Def::Fn(func) => {
                collect_hints_from_block(&hir_table, &results, &func.body, &mut hints);
            }
            hir::Def::ImplBlock(impl_block) => {
                for method in &impl_block.methods {
                    if let hir::Def::Fn(func) = hir_table.def(*method) {
                        collect_hints_from_block(&hir_table, &results, &func.body, &mut hints);
                    }
                }
            }
            hir::Def::EnumDef(_)
            | hir::Def::StructDef(_)
            | hir::Def::TraitDef(_)
            | hir::Def::ExternGo(_)
            | hir::Def::ExternType(_)
            | hir::Def::ExternBuiltin(_) => {}
        }
    }

    hints.sort_by(|a, b| {
        a.offset
            .cmp(&b.offset)
            .then(a.label.cmp(&b.label))
            .then(a.kind.cmp(&b.kind))
    });
    hints.dedup_by(|a, b| a.offset == b.offset && a.label == b.label && a.kind == b.kind);

    Some(hints)
}

fn collect_hints_from_block(
    hir_table: &hir::HirTable,
    results: &crate::typer::results::TypeckResults,
    block: &hir::Block,
    hints: &mut Vec<InlayHintItem>,
) {
    for stmt in &block.stmts {
        match stmt {
            hir::Stmt::Let(stmt) => {
                if stmt.annotation.is_none() {
                    emit_hints_for_pattern(hir_table, results, stmt.pat, hints);
                }
                collect_hints_from_expr(hir_table, results, stmt.value, hints);
            }
            hir::Stmt::Assign(stmt) => {
                collect_hints_from_expr(hir_table, results, stmt.value, hints);
            }
            hir::Stmt::Expr(stmt) => {
                collect_hints_from_expr(hir_table, results, stmt.expr, hints);
            }
        }
    }
    if let Some(tail) = block.tail {
        collect_hints_from_expr(hir_table, results, tail, hints);
    }
}

fn collect_hints_from_expr(
    hir_table: &hir::HirTable,
    results: &crate::typer::results::TypeckResults,
    expr_id: hir::ExprId,
    hints: &mut Vec<InlayHintItem>,
) {
    match hir_table.expr(expr_id) {
        hir::Expr::EClosure { params, body } => {
            for param in params {
                if param.ty.is_some() {
                    continue;
                }
                if !should_emit_type_inlay_hint(hir_table, param.name) {
                    continue;
                }
                let Some(ty) = results.local_ty(param.name).cloned() else {
                    continue;
                };
                if contains_type_var(&ty) {
                    continue;
                }
                hints.push(InlayHintItem {
                    offset: param.astptr.text_range().end(),
                    label: format!(": {}", ty.to_pretty(80)),
                    kind: InlayHintKind::Type,
                });
            }
            collect_hints_from_expr(hir_table, results, *body, hints);
        }
        hir::Expr::EBlock { block } => {
            collect_hints_from_block(hir_table, results, block, hints);
        }
        hir::Expr::EMatch { expr, arms } => {
            collect_hints_from_expr(hir_table, results, *expr, hints);
            for arm in arms {
                collect_hints_from_expr(hir_table, results, arm.body, hints);
            }
        }
        hir::Expr::EIf {
            cond,
            then_branch,
            else_branch,
        } => {
            collect_hints_from_expr(hir_table, results, *cond, hints);
            collect_hints_from_expr(hir_table, results, *then_branch, hints);
            collect_hints_from_expr(hir_table, results, *else_branch, hints);
        }
        hir::Expr::EWhile { cond, body } => {
            collect_hints_from_expr(hir_table, results, *cond, hints);
            collect_hints_from_expr(hir_table, results, *body, hints);
        }
        hir::Expr::EGo { expr } | hir::Expr::EUnary { expr, .. } | hir::Expr::ETry { expr } => {
            collect_hints_from_expr(hir_table, results, *expr, hints);
        }
        hir::Expr::EReturn { expr } => {
            if let Some(expr) = expr {
                collect_hints_from_expr(hir_table, results, *expr, hints);
            }
        }
        hir::Expr::ECall { func, args } => {
            collect_hints_from_expr(hir_table, results, *func, hints);
            for arg in args {
                collect_hints_from_expr(hir_table, results, *arg, hints);
            }
        }
        hir::Expr::EBinary { lhs, rhs, .. } => {
            collect_hints_from_expr(hir_table, results, *lhs, hints);
            collect_hints_from_expr(hir_table, results, *rhs, hints);
        }
        hir::Expr::EProj { tuple, .. } | hir::Expr::EField { expr: tuple, .. } => {
            collect_hints_from_expr(hir_table, results, *tuple, hints);
        }
        hir::Expr::EConstr { args, .. }
        | hir::Expr::ETuple { items: args }
        | hir::Expr::EArray { items: args } => {
            for arg in args {
                collect_hints_from_expr(hir_table, results, *arg, hints);
            }
        }
        hir::Expr::EStructLiteral { fields, .. } => {
            for (_, field_expr) in fields {
                collect_hints_from_expr(hir_table, results, *field_expr, hints);
            }
        }
        hir::Expr::EUnit
        | hir::Expr::ENameRef { .. }
        | hir::Expr::EStaticMember { .. }
        | hir::Expr::EBool { .. }
        | hir::Expr::EInt { .. }
        | hir::Expr::EInt8 { .. }
        | hir::Expr::EInt16 { .. }
        | hir::Expr::EInt32 { .. }
        | hir::Expr::EInt64 { .. }
        | hir::Expr::EUInt8 { .. }
        | hir::Expr::EUInt16 { .. }
        | hir::Expr::EUInt32 { .. }
        | hir::Expr::EUInt64 { .. }
        | hir::Expr::EFloat { .. }
        | hir::Expr::EFloat32 { .. }
        | hir::Expr::EFloat64 { .. }
        | hir::Expr::EString { .. }
        | hir::Expr::EChar { .. }
        | hir::Expr::EBreak
        | hir::Expr::EContinue => {}
    }
}

fn emit_hints_for_pattern(
    hir_table: &hir::HirTable,
    results: &crate::typer::results::TypeckResults,
    pat: hir::PatId,
    hints: &mut Vec<InlayHintItem>,
) {
    let mut local_defs = Vec::new();
    collect_pattern_locals(hir_table, pat, &mut local_defs);
    for (local_id, astptr) in local_defs {
        if !should_emit_type_inlay_hint(hir_table, local_id) {
            continue;
        }
        let Some(ty) = results.local_ty(local_id).cloned() else {
            continue;
        };
        if contains_type_var(&ty) {
            continue;
        }
        hints.push(InlayHintItem {
            offset: astptr.text_range().end(),
            label: format!(": {}", ty.to_pretty(80)),
            kind: InlayHintKind::Type,
        });
    }
}

fn collect_pattern_locals(
    hir_table: &hir::HirTable,
    pat_id: hir::PatId,
    out: &mut Vec<(hir::LocalId, MySyntaxNodePtr)>,
) {
    match hir_table.pat(pat_id) {
        hir::Pat::PVar { name, astptr } => out.push((*name, *astptr)),
        hir::Pat::PConstr { args, .. } => {
            for arg in args {
                collect_pattern_locals(hir_table, *arg, out);
            }
        }
        hir::Pat::PStruct { fields, .. } => {
            for (_, pat) in fields {
                collect_pattern_locals(hir_table, *pat, out);
            }
        }
        hir::Pat::PTuple { pats } => {
            for pat in pats {
                collect_pattern_locals(hir_table, *pat, out);
            }
        }
        hir::Pat::PUnit
        | hir::Pat::PBool { .. }
        | hir::Pat::PInt { .. }
        | hir::Pat::PInt8 { .. }
        | hir::Pat::PInt16 { .. }
        | hir::Pat::PInt32 { .. }
        | hir::Pat::PInt64 { .. }
        | hir::Pat::PUInt8 { .. }
        | hir::Pat::PUInt16 { .. }
        | hir::Pat::PUInt32 { .. }
        | hir::Pat::PUInt64 { .. }
        | hir::Pat::PFloat { .. }
        | hir::Pat::PFloat32 { .. }
        | hir::Pat::PFloat64 { .. }
        | hir::Pat::PString { .. }
        | hir::Pat::PChar { .. }
        | hir::Pat::PWild => {}
    }
}

fn should_emit_type_inlay_hint(hir_table: &hir::HirTable, local_id: hir::LocalId) -> bool {
    hir_table.local_hint(local_id) != "_"
}

fn contains_type_var(ty: &tast::Ty) -> bool {
    match ty {
        tast::Ty::TVar(_) => true,
        tast::Ty::TTuple { typs } => typs.iter().any(contains_type_var),
        tast::Ty::TFunc { params, ret_ty } => {
            params.iter().any(contains_type_var) || contains_type_var(ret_ty)
        }
        tast::Ty::TApp { ty, args } => contains_type_var(ty) || args.iter().any(contains_type_var),
        tast::Ty::TArray { elem, .. } => contains_type_var(elem),
        tast::Ty::TSlice { elem } => contains_type_var(elem),
        tast::Ty::TVec { elem } => contains_type_var(elem),
        tast::Ty::TRef { elem } => contains_type_var(elem),
        tast::Ty::THashMap { key, value } => contains_type_var(key) || contains_type_var(value),
        tast::Ty::TUnit
        | tast::Ty::TBool
        | tast::Ty::TInt8
        | tast::Ty::TInt16
        | tast::Ty::TInt32
        | tast::Ty::TInt64
        | tast::Ty::TUint8
        | tast::Ty::TUint16
        | tast::Ty::TUint32
        | tast::Ty::TUint64
        | tast::Ty::TFloat32
        | tast::Ty::TFloat64
        | tast::Ty::TString
        | tast::Ty::TChar
        | tast::Ty::TEnum { .. }
        | tast::Ty::TStruct { .. }
        | tast::Ty::TDyn { .. }
        | tast::Ty::TParam { .. } => false,
    }
}
