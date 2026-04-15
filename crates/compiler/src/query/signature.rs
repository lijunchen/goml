use std::{fs, path::Path};

use cst::cst::CstNode;
use cst::nodes::{CallExpr, Extern, Fn, TraitMethod};
use parser::syntax::{MySyntaxNode, MySyntaxNodePtr};

use crate::{hir, tast};

use super::{
    SignatureHelpItem, context::QueryContext, hir_index::HirResultsIndex,
    symbol_index::build_symbol_lookup, syntax::call_expr_and_active_parameter,
    typecheck::typecheck_for_query,
};

#[derive(Debug, Clone)]
pub(crate) struct CallParamInfo {
    pub(crate) name: Option<String>,
    pub(crate) ty: tast::Ty,
}

impl CallParamInfo {
    pub(crate) fn label(&self) -> String {
        let ty = self.ty.to_pretty(80);
        match &self.name {
            Some(name) => format!("{}: {}", name, ty),
            None => ty,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct CallSignatureContext {
    pub(crate) params: Vec<CallParamInfo>,
    pub(crate) return_type: tast::Ty,
    pub(crate) active_parameter: u32,
}

impl CallSignatureContext {
    pub(crate) fn expected_param(&self) -> Option<&CallParamInfo> {
        self.params.get(self.active_parameter as usize)
    }
}

pub fn signature_help(path: &Path, src: &str, line: u32, col: u32) -> Option<SignatureHelpItem> {
    let context = call_signature_context(path, src, line, col)?;
    let parameters = context
        .params
        .iter()
        .map(CallParamInfo::label)
        .collect::<Vec<_>>();
    let label = format!(
        "({}) -> {}",
        parameters.join(", "),
        context.return_type.to_pretty(80)
    );

    Some(SignatureHelpItem {
        label,
        parameters,
        active_parameter: context.active_parameter,
    })
}

pub(crate) fn call_signature_context(
    path: &Path,
    src: &str,
    line: u32,
    col: u32,
) -> Option<CallSignatureContext> {
    let context = QueryContext::from_position(path, src, line, col).ok()?;
    let (call_expr, active_parameter) =
        call_expr_and_active_parameter(context.root(), context.offset())?;

    let (hir_table, results, _genv, _diagnostics) = typecheck_for_query(path, src).ok()?;
    call_signature_context_from_parts(
        path,
        src,
        &hir_table,
        &results,
        &call_expr,
        active_parameter,
    )
}

pub(crate) fn call_signature_context_from_parts(
    path: &Path,
    src: &str,
    hir_table: &hir::HirTable,
    results: &crate::typer::results::TypeckResults,
    call_expr: &CallExpr,
    active_parameter: u32,
) -> Option<CallSignatureContext> {
    let index = HirResultsIndex::new(hir_table);
    let call_expr_id = index.expr_id(&MySyntaxNodePtr::new(call_expr.syntax()))?;
    let call_elab = results.call_elab(call_expr_id)?;
    let (mut params, return_type, hide_receiver) =
        signature_for_call_expr(hir_table, results, call_expr_id)?;
    let mut names = parameter_names_for_call(path, src, call_elab);

    if names.len() < params.len() {
        names.extend((names.len()..params.len()).map(|idx| Some(format!("arg{}", idx + 1))));
    }

    if hide_receiver && !params.is_empty() {
        params.remove(0);
        if !names.is_empty() {
            names.remove(0);
        }
    }

    if names.len() < params.len() {
        names.extend((names.len()..params.len()).map(|idx| Some(format!("arg{}", idx + 1))));
    }

    let params = params
        .into_iter()
        .zip(names)
        .map(|(ty, name)| CallParamInfo { name, ty })
        .collect::<Vec<_>>();
    let active_parameter = if params.is_empty() {
        0
    } else {
        active_parameter.min((params.len() - 1) as u32)
    };

    Some(CallSignatureContext {
        params,
        return_type,
        active_parameter,
    })
}

fn signature_for_call_expr(
    hir_table: &hir::HirTable,
    results: &crate::typer::results::TypeckResults,
    call_expr_id: hir::ExprId,
) -> Option<(Vec<tast::Ty>, tast::Ty, bool)> {
    let hir::Expr::ECall { func, .. } = hir_table.expr(call_expr_id) else {
        return None;
    };

    let call_ty = call_callee_type(results, call_expr_id)?;
    let tast::Ty::TFunc { mut params, ret_ty } = call_ty else {
        return None;
    };

    let hide_receiver =
        matches!(hir_table.expr(*func), hir::Expr::EField { .. }) && !params.is_empty();
    let return_type = (*ret_ty).clone();

    Some((std::mem::take(&mut params), return_type, hide_receiver))
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

fn parameter_names_for_call(
    path: &Path,
    src: &str,
    call_elab: &crate::typer::results::CallElab,
) -> Vec<Option<String>> {
    let symbols = build_symbol_lookup(path, src);
    let locations = match &call_elab.callee {
        crate::typer::results::CalleeElab::Var { name, .. } => symbols.index.find_value(name),
        crate::typer::results::CalleeElab::TraitMethod {
            trait_name,
            method_name,
            ..
        }
        | crate::typer::results::CalleeElab::DynTraitMethod {
            trait_name,
            method_name,
            ..
        } => symbols
            .index
            .find_trait_methods(&trait_name.0, &method_name.0),
        crate::typer::results::CalleeElab::InherentMethod {
            receiver_ty,
            method_name,
            ..
        } => {
            let mut out = Vec::new();
            for receiver in tast_ty_constr_candidates(receiver_ty) {
                out.extend(symbols.index.find_impl_methods(&receiver, &method_name.0));
                if !out.is_empty() {
                    break;
                }
            }
            out
        }
        crate::typer::results::CalleeElab::Expr(_)
        | crate::typer::results::CalleeElab::Error { .. } => Vec::new(),
    };

    for location in locations {
        if let Some(names) = parameter_names_at_location(&location, path, src) {
            return names;
        }
    }

    Vec::new()
}

fn parameter_names_at_location(
    location: &super::DefinitionLocation,
    current_path: &Path,
    current_src: &str,
) -> Option<Vec<Option<String>>> {
    let src = if location.path == current_path {
        current_src.to_string()
    } else {
        fs::read_to_string(&location.path).ok()?
    };
    let result = parser::parse(&location.path, &src);
    let root = MySyntaxNode::new_root(result.green_node);
    let file = cst::cst::File::cast(root)?;

    for item in file.items() {
        match item {
            cst::nodes::Item::Fn(function) => {
                if item_name_range(function.lident()) == Some(location.range) {
                    return Some(param_names_from_fn(&function));
                }
            }
            cst::nodes::Item::Impl(impl_block) => {
                for function in impl_block.functions() {
                    if item_name_range(function.lident()) == Some(location.range) {
                        return Some(param_names_from_fn(&function));
                    }
                }
            }
            cst::nodes::Item::Trait(trait_def) => {
                if let Some(methods) = trait_def.trait_method_list() {
                    for method in methods.methods() {
                        if item_name_range(method.lident()) == Some(location.range) {
                            return Some(param_names_from_trait_method(&method));
                        }
                    }
                }
            }
            cst::nodes::Item::Extern(extern_item) => {
                if item_name_range(extern_item.lident()) == Some(location.range) {
                    return Some(param_names_from_extern(&extern_item));
                }
            }
            cst::nodes::Item::Struct(_) | cst::nodes::Item::Enum(_) => {}
        }
    }

    None
}

fn item_name_range(token: Option<parser::syntax::MySyntaxToken>) -> Option<text_size::TextRange> {
    token.map(|token| token.text_range())
}

fn param_names_from_fn(function: &Fn) -> Vec<Option<String>> {
    function
        .param_list()
        .into_iter()
        .flat_map(|params| params.params())
        .enumerate()
        .map(|(idx, param)| {
            param
                .lident()
                .map(|ident| ident.to_string())
                .or_else(|| Some(format!("arg{}", idx + 1)))
        })
        .collect()
}

fn param_names_from_extern(extern_item: &Extern) -> Vec<Option<String>> {
    extern_item
        .param_list()
        .into_iter()
        .flat_map(|params| params.params())
        .enumerate()
        .map(|(idx, param)| {
            param
                .lident()
                .map(|ident| ident.to_string())
                .or_else(|| Some(format!("arg{}", idx + 1)))
        })
        .collect()
}

fn param_names_from_trait_method(method: &TraitMethod) -> Vec<Option<String>> {
    method
        .type_list()
        .into_iter()
        .flat_map(|types| types.types())
        .enumerate()
        .map(|(idx, ty)| {
            if idx == 0 && ty.to_string() == "Self" {
                Some("self".to_string())
            } else {
                Some(format!("arg{}", idx + 1))
            }
        })
        .collect()
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
