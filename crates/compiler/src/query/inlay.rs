use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use text_size::TextRange;

use crate::{hir, tast};

use super::{
    InlayHintItem, InlayHintKind, hir_index::local_name_range,
    typecheck::typecheck_for_query_with_overrides,
};

pub fn inlay_hints(path: &Path, src: &str) -> Option<Vec<InlayHintItem>> {
    inlay_hints_with_overrides(path, src, &HashMap::new())
}

pub fn inlay_hints_with_overrides(
    path: &Path,
    src: &str,
    source_overrides: &HashMap<PathBuf, String>,
) -> Option<Vec<InlayHintItem>> {
    crate::pipeline::with_compiler_stack(|| {
        let (hir_table, results, _genv, _diagnostics) =
            typecheck_for_query_with_overrides(path, src, source_overrides).ok()?;

        let mut hints = Vec::new();
        for idx in 0..hir_table.def_count() {
            let def_id = hir_table.def_id_at(idx);
            if hir_table.def_source(def_id) != Some(path) {
                continue;
            }
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
                | hir::Def::ExternFn(_) => {}
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
    })
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
                emit_hints_for_pattern(hir_table, results, arm.pat, hints);
                if let Some(guard) = arm.guard {
                    collect_hints_from_expr(hir_table, results, guard, hints);
                }
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
        hir::Expr::EFor {
            pat,
            iterator,
            body,
        } => {
            emit_hints_for_pattern(hir_table, results, *pat, hints);
            collect_hints_from_expr(hir_table, results, *iterator, hints);
            collect_hints_from_expr(hir_table, results, *body, hints);
        }
        hir::Expr::EGo { expr }
        | hir::Expr::EUnary { expr, .. }
        | hir::Expr::ECast { expr, .. }
        | hir::Expr::ETry { expr } => {
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
        hir::Expr::ERange {
            start: lhs,
            end: rhs,
        }
        | hir::Expr::EBinary { lhs, rhs, .. } => {
            collect_hints_from_expr(hir_table, results, *lhs, hints);
            collect_hints_from_expr(hir_table, results, *rhs, hints);
        }
        hir::Expr::EProj { tuple, .. } | hir::Expr::EField { expr: tuple, .. } => {
            collect_hints_from_expr(hir_table, results, *tuple, hints);
        }
        hir::Expr::EIndex { base, index } => {
            collect_hints_from_expr(hir_table, results, *base, hints);
            collect_hints_from_expr(hir_table, results, *index, hints);
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
        | hir::Expr::EFloat { .. }
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
    for (local_id, range) in local_defs {
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
            offset: range.end(),
            label: format!(": {}", ty.to_pretty(80)),
            kind: InlayHintKind::Type,
        });
    }
}

fn collect_pattern_locals(
    hir_table: &hir::HirTable,
    pat_id: hir::PatId,
    out: &mut Vec<(hir::LocalId, TextRange)>,
) {
    match hir_table.pat(pat_id) {
        hir::Pat::PVar { name, astptr } => out.push((*name, astptr.text_range())),
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
        hir::Pat::PArray {
            prefix,
            rest,
            suffix,
        } => {
            for pat in prefix.iter().chain(suffix.iter()) {
                collect_pattern_locals(hir_table, *pat, out);
            }
            if let Some(hir::ArrayPatRest {
                binding: Some(name),
                astptr,
            }) = rest
            {
                out.push((*name, local_name_range(hir_table, *name, *astptr)));
            }
        }
        hir::Pat::PAlias { name, pat, astptr } => {
            out.push((*name, local_name_range(hir_table, *name, *astptr)));
            collect_pattern_locals(hir_table, *pat, out);
        }
        hir::Pat::POr { pats } => {
            for pat in pats {
                collect_pattern_locals(hir_table, *pat, out);
            }
        }
        hir::Pat::PRange { start, end, .. } => {
            collect_pattern_locals(hir_table, *start, out);
            collect_pattern_locals(hir_table, *end, out);
        }
        hir::Pat::PUnit
        | hir::Pat::PBool { .. }
        | hir::Pat::PInt { .. }
        | hir::Pat::PFloat { .. }
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
        tast::Ty::TProjection {
            trait_ref, for_ty, ..
        } => {
            contains_type_var(for_ty)
                || trait_ref
                    .as_ref()
                    .is_some_and(|trait_ref| trait_ref.args.iter().any(contains_type_var))
        }
        tast::Ty::TArray { elem, .. } => contains_type_var(elem),
        tast::Ty::TSlice { elem } => contains_type_var(elem),
        tast::Ty::TVec { elem } => contains_type_var(elem),
        tast::Ty::TRef { elem } => contains_type_var(elem),
        tast::Ty::TChannel { elem } => contains_type_var(elem),
        tast::Ty::THashMap { key, value } => contains_type_var(key) || contains_type_var(value),
        tast::Ty::TUnit
        | tast::Ty::TNever
        | tast::Ty::TBool
        | tast::Ty::TInt
        | tast::Ty::TInt8
        | tast::Ty::TInt16
        | tast::Ty::TInt32
        | tast::Ty::TInt64
        | tast::Ty::TUint
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
