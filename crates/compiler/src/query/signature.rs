use std::path::Path;

use crate::{hir, tast};
use cst::cst::CstNode;

use super::{
    SignatureHelpItem, context::QueryContext, hir_index::HirResultsIndex,
    syntax::call_expr_and_active_parameter, typecheck::typecheck_for_query,
};

pub fn signature_help(path: &Path, src: &str, line: u32, col: u32) -> Option<SignatureHelpItem> {
    let context = QueryContext::from_position(path, src, line, col).ok()?;
    let (call_expr, active_parameter) =
        call_expr_and_active_parameter(context.root(), context.offset())?;

    let (hir_table, results, _genv, _diagnostics) = typecheck_for_query(path, src).ok()?;
    let index = HirResultsIndex::new(&hir_table);
    let call_expr_id = index.expr_id(&parser::syntax::MySyntaxNodePtr::new(call_expr.syntax()))?;
    let (parameter_types, return_type) =
        signature_for_call_expr(&hir_table, &results, call_expr_id)?;

    let parameters = parameter_types
        .iter()
        .map(|ty| ty.to_pretty(80))
        .collect::<Vec<_>>();
    let label = format!(
        "({}) -> {}",
        parameters.join(", "),
        return_type.to_pretty(80)
    );
    let active_parameter = if parameters.is_empty() {
        0
    } else {
        active_parameter.min((parameters.len() - 1) as u32)
    };

    Some(SignatureHelpItem {
        label,
        parameters,
        active_parameter,
    })
}

fn signature_for_call_expr(
    hir_table: &hir::HirTable,
    results: &crate::typer::results::TypeckResults,
    call_expr_id: hir::ExprId,
) -> Option<(Vec<tast::Ty>, tast::Ty)> {
    let hir::Expr::ECall { func, .. } = hir_table.expr(call_expr_id) else {
        return None;
    };

    let call_ty = call_callee_type(results, call_expr_id)?;
    let tast::Ty::TFunc { params, ret_ty } = call_ty else {
        return None;
    };

    let mut params = params;
    if matches!(hir_table.expr(*func), hir::Expr::EField { .. }) && !params.is_empty() {
        params.remove(0);
    }

    Some((params, *ret_ty))
}

fn call_callee_type(
    results: &crate::typer::results::TypeckResults,
    call_expr_id: hir::ExprId,
) -> Option<tast::Ty> {
    let call_elab = results.call_elab(call_expr_id)?;
    match &call_elab.callee {
        crate::typer::results::CalleeElab::Expr(expr_id) => results.expr_ty(*expr_id).cloned(),
        crate::typer::results::CalleeElab::Var { ty, .. }
        | crate::typer::results::CalleeElab::TraitMethod { ty, .. }
        | crate::typer::results::CalleeElab::DynTraitMethod { ty, .. }
        | crate::typer::results::CalleeElab::InherentMethod { ty, .. }
        | crate::typer::results::CalleeElab::Error { ty, .. } => Some(ty.clone()),
    }
}
