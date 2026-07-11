use std::collections::HashMap;

use super::*;
use crate::typer::util;

pub(super) struct FnSchemeApplication<'a> {
    pub(super) scheme: &'a crate::env::FnScheme,
    pub(super) template_call_ty: &'a tast::Ty,
    pub(super) actual_call_ty: &'a tast::Ty,
    pub(super) range: Option<TextRange>,
}

impl Typer {
    pub(super) fn apply_fn_scheme_constraints(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        application: FnSchemeApplication<'_>,
    ) -> bool {
        let FnSchemeApplication {
            scheme,
            template_call_ty,
            actual_call_ty,
            range,
        } = application;
        let mut subst = HashMap::new();
        collect_type_param_substitution(template_call_ty, actual_call_ty, &mut subst);
        for constraint in scheme.constraints.iter() {
            let Some(actual_ty) = subst.get(&constraint.type_param) else {
                continue;
            };
            if !self.apply_trait_requirement(
                genv,
                local_env,
                diagnostics,
                actual_ty,
                &constraint.trait_name,
                range,
            ) {
                return false;
            }
        }
        true
    }

    fn apply_trait_requirement(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        target_ty: &tast::Ty,
        required_trait: &tast::TastIdent,
        range: Option<TextRange>,
    ) -> bool {
        let Some(resolved_trait) =
            resolve_required_trait_name_or_report(genv, diagnostics, required_trait, range)
        else {
            return false;
        };

        match target_ty {
            tast::Ty::TDyn { trait_name } if trait_name == &resolved_trait => true,
            tast::Ty::TParam { name } => {
                let in_bounds = tparam_has_trait_bound(local_env, name, &resolved_trait);
                if in_bounds {
                    true
                } else {
                    diagnostics.push(
                        Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            format!(
                                "Type parameter {} is not constrained by trait {}",
                                name, resolved_trait
                            ),
                        )
                        .with_range(range),
                    );
                    false
                }
            }
            _ => {
                self.push_constraint(Constraint::Implements {
                    trait_name: tast::TastIdent(resolved_trait),
                    for_ty: target_ty.clone(),
                    origin: range,
                });
                true
            }
        }
    }
}

pub(super) fn tparam_has_trait_bound(
    local_env: &LocalTypeEnv,
    tparam_name: &str,
    trait_name: &str,
) -> bool {
    local_env
        .tparam_trait_bounds(tparam_name)
        .is_some_and(|bounds| bounds.iter().any(|bound| bound.0 == trait_name))
}

fn resolve_required_trait_name_or_report(
    genv: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    required_trait: &tast::TastIdent,
    range: Option<TextRange>,
) -> Option<String> {
    if let Some((resolved, _env)) = util::resolve_trait_name(genv, &required_trait.0) {
        return Some(resolved);
    }
    if required_trait.0.contains("::") {
        return Some(required_trait.0.clone());
    }
    diagnostics.push(
        Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!("Unknown trait {}", required_trait.0),
        )
        .with_range(range),
    );
    None
}
