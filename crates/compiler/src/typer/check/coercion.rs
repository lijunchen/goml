use super::*;
use crate::typer::util;

impl Typer {
    pub(super) fn coerce_to_expected_dyn(
        &mut self,
        genv: &PackageTypeEnv,
        diagnostics: &mut Diagnostics,
        expr_id: hir::ExprId,
        expr: tast::Expr,
        expected: &tast::Ty,
    ) -> (tast::Expr, bool) {
        let expected_norm = self.norm(expected);
        match &expected_norm {
            tast::Ty::TVar(_) => {
                let for_ty = expr.get_ty();
                if self.norm(&for_ty) != expected_norm {
                    self.push_obligation(
                        Predicate::Coerce(CoercionGoal {
                            expr_id,
                            from_ty: for_ty,
                            to_ty: expected_norm,
                        }),
                        ObligationCause::new(
                            self.expr_range(expr_id),
                            ObligationCauseKind::Coercion,
                        ),
                    );
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
        self.push_obligation(
            Predicate::Coerce(CoercionGoal {
                expr_id,
                from_ty: for_ty.clone(),
                to_ty: tast::Ty::TDyn {
                    trait_name: resolved_trait.clone(),
                },
            }),
            ObligationCause::new(range, ObligationCauseKind::Coercion),
        );
        (
            tast::Expr::EToDyn {
                trait_name: tast::TastIdent(resolved_trait.clone()),
                for_ty,
                expr: Box::new(expr),
                ty: expected_norm.clone(),
                astptr: None,
            },
            true,
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
