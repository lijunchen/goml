use std::path::Path;

use parser::syntax::MySyntaxNodePtr;

use crate::{hir, tast};

use super::{InlayHintItem, InlayHintKind, typecheck::typecheck_for_query};

pub fn inlay_hints(path: &Path, src: &str) -> Option<Vec<InlayHintItem>> {
    let (hir_table, results, _genv, _diagnostics) = typecheck_for_query(path, src).ok()?;

    let mut hints = Vec::new();
    for idx in 0..hir_table.expr_count() {
        let expr_id = hir::ExprId {
            pkg: hir_table.package(),
            idx: idx as u32,
        };
        match hir_table.expr(expr_id) {
            hir::Expr::ELet {
                pat, annotation, ..
            } => {
                if annotation.is_some() {
                    continue;
                }

                let mut local_defs = Vec::new();
                collect_pattern_locals(&hir_table, *pat, &mut local_defs);
                for (local_id, astptr) in local_defs {
                    if !should_emit_type_inlay_hint(&hir_table, local_id) {
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
            hir::Expr::EClosure { params, .. } => {
                for param in params {
                    if param.ty.is_some() {
                        continue;
                    }
                    if !should_emit_type_inlay_hint(&hir_table, param.name) {
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
            }
            _ => {}
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
