use std::collections::{HashMap, HashSet};

use parser::Diagnostics;
use text_size::TextRange;

use crate::{
    env::{FnScheme, PackageTypeEnv},
    hir, tast,
};

use super::{
    Typer,
    localenv::LocalTypeEnv,
    obligations::{ParamEnv, TraitGoal},
    traits::solver::{SelectionResult, TraitSolver},
    type_ops::{contains_tvar, decompose_struct_type, substitute_trait_ref, substitute_ty_params},
    util::{
        format_trait_ref_for_diag, format_ty_for_diag, push_error_with_range, resolve_trait_name,
        resolve_type_name, type_expr_range, validate_ty,
    },
};

pub(crate) fn resolve_explicit_trait_args(
    genv: &PackageTypeEnv,
    local_env: &LocalTypeEnv,
    diagnostics: &mut Diagnostics,
    trait_name: &str,
    type_args: &[hir::TypeExpr],
    range: Option<TextRange>,
) -> Result<Option<Vec<tast::Ty>>, ()> {
    if type_args.is_empty() {
        return Ok(None);
    }
    let args = resolve_explicit_type_args(genv, local_env, diagnostics, type_args)?;
    if let Some((resolved, trait_env)) = resolve_trait_name(genv, trait_name)
        && let Some(definition) = trait_env.trait_env.trait_defs.get(&resolved)
        && definition.params.len() != args.len()
    {
        push_error_with_range(
            diagnostics,
            format!(
                "Trait {} expects {} type arguments, but got {}",
                trait_name,
                definition.params.len(),
                args.len()
            ),
            range,
        );
        return Err(());
    }
    Ok(Some(args))
}

pub(crate) fn resolve_explicit_type_args(
    genv: &PackageTypeEnv,
    local_env: &LocalTypeEnv,
    diagnostics: &mut Diagnostics,
    type_args: &[hir::TypeExpr],
) -> Result<Vec<tast::Ty>, ()> {
    let tparams = local_env.current_tparams_env();
    let tparam_names = tparams
        .iter()
        .map(|param| param.0.clone())
        .collect::<HashSet<_>>();
    let diagnostic_count = diagnostics.len();
    let args = type_args
        .iter()
        .map(|arg| {
            let ty = tast::Ty::from_hir(genv, arg, &tparams);
            let ty = super::toplevel::resolve_ty_projections_from_predicates(
                genv,
                diagnostics,
                &ty,
                local_env.predicates(),
                type_expr_range(arg),
            );
            validate_ty(genv, diagnostics, &ty, type_expr_range(arg), &tparam_names);
            ty
        })
        .collect::<Vec<_>>();
    if diagnostics.len() != diagnostic_count {
        return Err(());
    }
    Ok(args)
}

pub(crate) fn resolve_field_ty_eager(
    genv: &PackageTypeEnv,
    base_ty: &tast::Ty,
    field: &tast::TastIdent,
) -> Option<tast::Ty> {
    let (type_name, type_args) = decompose_struct_type(base_ty)?;
    let (resolved, env) = resolve_type_name(genv, &type_name);
    let struct_def = env.structs().get(&tast::TastIdent::new(&resolved))?;
    if struct_def.generics.len() != type_args.len() {
        return None;
    }

    let subst = struct_def
        .generics
        .iter()
        .zip(type_args.iter())
        .map(|(param, arg)| (param.0.clone(), arg.clone()))
        .collect::<HashMap<_, _>>();
    let (_, ty) = struct_def.fields.iter().find(|(name, _)| name == field)?;
    Some(substitute_ty_params(ty, &subst))
}

fn lookup_bound_trait_methods(
    genv: &PackageTypeEnv,
    bounds: &[tast::TraitRef],
    method: &tast::TastIdent,
) -> Vec<(tast::TraitRef, FnScheme)> {
    let mut result = bounds
        .iter()
        .flat_map(|trait_ref| super::util::trait_ref_closure(genv, trait_ref))
        .filter_map(|trait_ref| {
            let (_, trait_env) = resolve_trait_name(genv, &trait_ref.name.0)?;
            let scheme = trait_env.lookup_trait_method_scheme(&trait_ref, method)?;
            Some((trait_ref, scheme))
        })
        .collect::<Vec<_>>();
    result.sort_by(|(left, _), (right, _)| {
        format_trait_ref_for_diag(left).cmp(&format_trait_ref_for_diag(right))
    });
    result.dedup_by(|(left, _), (right, _)| left == right);
    result
}

pub(crate) fn lookup_dyn_trait_methods(
    genv: &PackageTypeEnv,
    trait_name: &tast::TastIdent,
    method: &tast::TastIdent,
) -> Vec<(tast::TraitRef, FnScheme)> {
    let Some((resolved, trait_env)) = resolve_trait_name(genv, &trait_name.0) else {
        return Vec::new();
    };
    let trait_ref = tast::TraitRef {
        name: tast::TastIdent::new(&resolved),
        args: Vec::new(),
    };
    if let Some(scheme) = trait_env.lookup_trait_method_scheme(&trait_ref, method) {
        return vec![(trait_ref, scheme)];
    }
    let mut result = super::util::trait_ref_closure(genv, &trait_ref)
        .into_iter()
        .skip(1)
        .filter_map(|parent| {
            let (_, parent_env) = resolve_trait_name(genv, &parent.name.0)?;
            let scheme = parent_env.lookup_trait_method_scheme(&parent, method)?;
            Some((parent, scheme))
        })
        .collect::<Vec<_>>();
    result.sort_by(|(left, _), (right, _)| {
        format_trait_ref_for_diag(left).cmp(&format_trait_ref_for_diag(right))
    });
    result.dedup_by(|(left, _), (right, _)| left == right);
    result
}

fn lookup_in_scope_trait_methods(
    typer: &mut Typer,
    genv: &PackageTypeEnv,
    in_scope_traits: &[tast::TastIdent],
    receiver_ty: &tast::Ty,
    method: &tast::TastIdent,
) -> Vec<(tast::TraitRef, FnScheme)> {
    let mut result = Vec::new();
    let param_env = ParamEnv::default();
    for trait_name in in_scope_traits {
        let Some((resolved_trait, trait_env)) = resolve_trait_name(genv, &trait_name.0) else {
            continue;
        };
        if matches!(receiver_ty, tast::Ty::TDyn { .. }) {
            continue;
        }
        if !trait_env.trait_env.trait_defs.contains_key(&resolved_trait) {
            continue;
        }
        for (origin, _, impl_trait_ref, impl_ty, impl_def) in
            genv.visible_trait_impls(&resolved_trait)
        {
            if !impl_def.valid
                || origin == "builtin" && genv.shadows_builtin_nominal_type(receiver_ty)
            {
                continue;
            }
            let snapshot = typer.snapshot_inference();
            let substitution = impl_def
                .params
                .iter()
                .map(|param| (param.0.clone(), typer.fresh_ty_var()))
                .collect::<HashMap<_, _>>();
            let candidate_ty = substitute_ty_params(impl_ty, &substitution);
            if !typer.try_unify_silent(&candidate_ty, receiver_ty) {
                typer.rollback_inference(snapshot);
                continue;
            }
            let mut trait_ref = substitute_trait_ref(impl_trait_ref, &substitution);
            for arg in &mut trait_ref.args {
                *arg = typer.norm(arg);
            }
            let mut solver = TraitSolver::new(genv, &param_env);
            if matches!(
                solver.select(
                    typer,
                    TraitGoal {
                        trait_ref: trait_ref.clone(),
                        for_ty: receiver_ty.clone(),
                    }
                ),
                SelectionResult::NoSolution | SelectionResult::Overflow
            ) {
                typer.rollback_inference(snapshot);
                continue;
            }
            for arg in &mut trait_ref.args {
                *arg = typer.norm(arg);
            }
            typer.commit_inference(snapshot);
            for method_trait_ref in super::util::trait_ref_closure(genv, &trait_ref) {
                let Some((_, method_env)) = resolve_trait_name(genv, &method_trait_ref.name.0)
                else {
                    continue;
                };
                if let Some(method_scheme) =
                    method_env.lookup_trait_method_scheme(&method_trait_ref, method)
                {
                    result.push((method_trait_ref, method_scheme));
                }
            }
        }
    }
    result.sort_by(|(left, _), (right, _)| {
        format_trait_ref_for_diag(left).cmp(&format_trait_ref_for_diag(right))
    });
    result.dedup_by(|(left, _), (right, _)| left == right);
    result
}

pub(crate) fn lookup_trait_method_from_type_name(
    typer: &mut Typer,
    genv: &PackageTypeEnv,
    type_name: &str,
    method: &tast::TastIdent,
    explicit_args: Option<Vec<tast::Ty>>,
) -> Option<(tast::TraitRef, FnScheme)> {
    let (trait_name, trait_env) = resolve_trait_name(genv, type_name)?;
    let definition = trait_env.trait_env.trait_defs.get(&trait_name)?;
    let args = explicit_args.unwrap_or_else(|| {
        definition
            .params
            .iter()
            .map(|_| typer.fresh_ty_var())
            .collect()
    });
    if args.len() != definition.params.len() {
        return None;
    }
    let trait_ref = tast::TraitRef {
        name: tast::TastIdent(trait_name),
        args,
    };
    let mut candidates = super::util::trait_ref_closure(genv, &trait_ref)
        .into_iter()
        .filter_map(|method_trait_ref| {
            let (_, method_env) = resolve_trait_name(genv, &method_trait_ref.name.0)?;
            let scheme = method_env.lookup_trait_method_scheme(&method_trait_ref, method)?;
            Some((method_trait_ref, scheme))
        })
        .collect::<Vec<_>>();
    if let Some(index) = candidates
        .iter()
        .position(|(candidate, _)| candidate == &trait_ref)
    {
        return Some(candidates.swap_remove(index));
    }
    let [candidate] = candidates.as_slice() else {
        return None;
    };
    Some(candidate.clone())
}

pub(crate) struct TraitMethodLookup {
    pub receiver: MethodLookupReceiver,
    pub candidates: Vec<(tast::TraitRef, FnScheme)>,
}

pub(crate) enum MethodLookupReceiver {
    TypeParam(String),
    Concrete(tast::Ty),
    Deferred(tast::Ty),
}

pub(crate) fn lookup_trait_method_candidates(
    typer: &mut Typer,
    genv: &PackageTypeEnv,
    local_env: &LocalTypeEnv,
    receiver_ty: &tast::Ty,
    method: &tast::TastIdent,
) -> TraitMethodLookup {
    if let tast::Ty::TParam { name } = receiver_ty {
        let bounds = local_env.tparam_trait_bounds(name).unwrap_or(&[]);
        return TraitMethodLookup {
            receiver: MethodLookupReceiver::TypeParam(name.clone()),
            candidates: lookup_bound_trait_methods(genv, bounds, method),
        };
    }
    if contains_tvar(receiver_ty) {
        return TraitMethodLookup {
            receiver: MethodLookupReceiver::Deferred(receiver_ty.clone()),
            candidates: Vec::new(),
        };
    }
    TraitMethodLookup {
        receiver: MethodLookupReceiver::Concrete(receiver_ty.clone()),
        candidates: lookup_in_scope_trait_methods(
            typer,
            genv,
            local_env.in_scope_traits(),
            receiver_ty,
            method,
        ),
    }
}

pub(crate) fn report_method_not_found(
    diagnostics: &mut Diagnostics,
    method_name: &tast::TastIdent,
    receiver: &MethodLookupReceiver,
    range: Option<TextRange>,
) {
    let message = match receiver {
        MethodLookupReceiver::TypeParam(name) => format!(
            "Method {} is not available for type parameter {}",
            method_name.0, name
        ),
        MethodLookupReceiver::Concrete(ty) | MethodLookupReceiver::Deferred(ty) => format!(
            "Method {} not found for type {}",
            method_name.0,
            format_ty_for_diag(ty)
        ),
    };
    push_error_with_range(diagnostics, message, range);
}

pub(crate) fn report_ambiguous_method(
    diagnostics: &mut Diagnostics,
    method_name: &tast::TastIdent,
    receiver: &MethodLookupReceiver,
    candidates: &[(tast::TraitRef, FnScheme)],
    range: Option<TextRange>,
) {
    let trait_names = candidates
        .iter()
        .map(|(trait_ref, _)| format_trait_ref_for_diag(trait_ref))
        .collect::<Vec<_>>()
        .join(", ");
    let receiver_label = match receiver {
        MethodLookupReceiver::TypeParam(name) => format!("type parameter {name}"),
        MethodLookupReceiver::Concrete(ty) | MethodLookupReceiver::Deferred(ty) => {
            format!("type {}", format_ty_for_diag(ty))
        }
    };
    push_error_with_range(
        diagnostics,
        format!(
            "Ambiguous method {} for {} (candidates: {}). Use UFCS like Trait::{}(...) to disambiguate",
            method_name.0, receiver_label, trait_names, method_name.0
        ),
        range,
    );
}
