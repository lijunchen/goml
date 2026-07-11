use super::constraints::tparam_has_trait_bound;
use super::*;
use crate::typer::util;

impl Typer {
    pub(super) fn check_expr_with_deferred_dyn(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        expr_id: hir::ExprId,
        expected: &tast::Ty,
    ) -> (tast::Expr, bool) {
        let deferred_start = self.deferred_dyn_coercions.len();
        let expr = self.check_expr(genv, local_env, diagnostics, expr_id, expected);
        let deferred_dyn = self.deferred_dyn_coercions[deferred_start..]
            .iter()
            .any(|coercion| coercion.expr_id == expr_id);
        (expr, deferred_dyn)
    }

    pub(super) fn coerce_to_expected_dyn(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        expr_id: hir::ExprId,
        expr: tast::Expr,
        expected: &tast::Ty,
    ) -> (tast::Expr, bool) {
        let expected_norm = self.norm(expected);
        match &expected_norm {
            tast::Ty::TVar(_) => {
                let for_ty = expr.get_ty();
                if !matches!(for_ty, tast::Ty::TDyn { .. }) {
                    self.deferred_dyn_coercions
                        .push(super::super::DeferredDynCoercion {
                            expr_id,
                            concrete_ty: for_ty.clone(),
                            expected_ty: expected_norm,
                            origin: self.expr_range(expr_id),
                        });
                    return (expr, true);
                }
                return (expr, false);
            }
            tast::Ty::TDyn { .. } => {}
            _ => return (expr, false),
        }

        let tast::Ty::TDyn { trait_name } = &expected_norm else {
            unreachable!()
        };

        if matches!(expr.get_ty(), tast::Ty::TDyn { .. }) {
            return (expr, false);
        }

        let range = self.expr_range(expr_id);
        let Some(resolved_trait) =
            resolve_trait_name_or_report(genv, diagnostics, trait_name, range)
        else {
            return (expr, false);
        };

        let for_ty = expr.get_ty();
        match &for_ty {
            tast::Ty::TParam { name } => {
                if !tparam_has_trait_bound(local_env, name, &resolved_trait) {
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
                    return (expr, false);
                }
            }
            tast::Ty::TVar(_) => {
                self.push_constraint(Constraint::Implements {
                    trait_name: tast::TastIdent(resolved_trait.clone()),
                    for_ty: for_ty.clone(),
                    origin: range,
                });
            }
            _ if contains_tvar(&for_ty) => {
                self.push_constraint(Constraint::Implements {
                    trait_name: tast::TastIdent(resolved_trait.clone()),
                    for_ty: for_ty.clone(),
                    origin: range,
                });
            }
            _ if !is_concrete_ty(&for_ty) => {}
            _ => {
                let impl_count = genv.trait_impl_count_visible(&resolved_trait, &for_ty);
                if impl_count == 0 {
                    diagnostics.push(
                        Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            format!(
                                "Type {} does not implement trait {}",
                                util::format_ty_for_diag(&for_ty),
                                resolved_trait
                            ),
                        )
                        .with_range(range),
                    );
                    return (expr, false);
                }
                if impl_count > 1 {
                    diagnostics.push(
                        Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            format!(
                                "Multiple instances found for trait {}<{}>",
                                resolved_trait,
                                util::format_ty_for_diag(&for_ty)
                            ),
                        )
                        .with_range(range),
                    );
                    return (expr, false);
                }
            }
        }

        self.results.push_coercion(
            expr_id,
            Coercion::ToDyn {
                trait_name: tast::TastIdent(resolved_trait.clone()),
                for_ty: for_ty.clone(),
                ty: expected_norm.clone(),
                astptr: None,
            },
        );
        (
            tast::Expr::EToDyn {
                trait_name: tast::TastIdent(resolved_trait.clone()),
                for_ty,
                expr: Box::new(expr),
                ty: expected_norm.clone(),
                astptr: None,
            },
            false,
        )
    }
}

fn resolve_trait_name_or_report(
    genv: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    trait_name: &str,
    range: Option<TextRange>,
) -> Option<String> {
    let Some((resolved, _env)) = util::resolve_trait_name(genv, trait_name) else {
        diagnostics.push(
            Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!("Unknown trait {}", trait_name),
            )
            .with_range(range),
        );
        return None;
    };
    Some(resolved)
}
