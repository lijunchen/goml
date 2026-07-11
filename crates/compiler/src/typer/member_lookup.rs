use std::collections::HashMap;

use parser::Diagnostics;
use text_size::TextRange;

use crate::{
    env::{FnScheme, PackageTypeEnv},
    tast,
};

use super::{
    localenv::LocalTypeEnv,
    obligations::{ParamEnv, TraitGoal},
    traits::solver::{SelectionResult, TraitSolver},
    type_ops::{contains_tvar, decompose_struct_type, substitute_ty_params},
    util::{format_ty_for_diag, push_error_with_range, resolve_trait_name, resolve_type_name},
};

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
    bounds: &[tast::TastIdent],
    method: &tast::TastIdent,
) -> Vec<(tast::TastIdent, FnScheme)> {
    lookup_trait_methods(genv, bounds, method, None)
}

fn lookup_in_scope_trait_methods(
    genv: &PackageTypeEnv,
    in_scope_traits: &[tast::TastIdent],
    receiver_ty: &tast::Ty,
    method: &tast::TastIdent,
) -> Vec<(tast::TastIdent, FnScheme)> {
    lookup_trait_methods(genv, in_scope_traits, method, Some(receiver_ty))
}

fn lookup_trait_methods(
    genv: &PackageTypeEnv,
    trait_names: &[tast::TastIdent],
    method: &tast::TastIdent,
    receiver_ty: Option<&tast::Ty>,
) -> Vec<(tast::TastIdent, FnScheme)> {
    let mut result = Vec::new();
    let param_env = ParamEnv::default();
    let mut solver = TraitSolver::new(genv, &param_env);
    for trait_name in trait_names {
        let Some((resolved_trait, trait_env)) = resolve_trait_name(genv, &trait_name.0) else {
            continue;
        };
        let resolved_ident = tast::TastIdent(resolved_trait);
        if let Some(ty) = receiver_ty {
            if matches!(ty, tast::Ty::TDyn { .. }) {
                continue;
            }
            if !matches!(
                solver.select(TraitGoal {
                    trait_name: resolved_ident.clone(),
                    for_ty: ty.clone(),
                }),
                SelectionResult::Unique(_)
            ) {
                continue;
            }
        }
        if let Some(method_scheme) = trait_env.lookup_trait_method_scheme(&resolved_ident, method) {
            result.push((resolved_ident, method_scheme));
        }
    }
    result
}

pub(crate) fn lookup_trait_method_from_type_name(
    genv: &PackageTypeEnv,
    type_name: &str,
    method: &tast::TastIdent,
) -> Option<(tast::TastIdent, FnScheme)> {
    let (trait_name, trait_env) = resolve_trait_name(genv, type_name)?;
    let trait_ident = tast::TastIdent(trait_name);
    let method_scheme = trait_env.lookup_trait_method_scheme(&trait_ident, method)?;
    Some((trait_ident, method_scheme))
}

pub(crate) struct TraitMethodLookup {
    pub receiver: MethodLookupReceiver,
    pub candidates: Vec<(tast::TastIdent, FnScheme)>,
}

pub(crate) enum MethodLookupReceiver {
    TypeParam(String),
    Concrete(tast::Ty),
    Deferred(tast::Ty),
}

pub(crate) fn lookup_trait_method_candidates(
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
            candidates: lookup_trait_methods(genv, local_env.in_scope_traits(), method, None),
        };
    }
    TraitMethodLookup {
        receiver: MethodLookupReceiver::Concrete(receiver_ty.clone()),
        candidates: lookup_in_scope_trait_methods(
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
    candidates: &[(tast::TastIdent, FnScheme)],
    range: Option<TextRange>,
) {
    let trait_names = candidates
        .iter()
        .map(|(trait_name, _)| trait_name.0.clone())
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
