use std::collections::{HashMap, HashSet};

use crate::{
    env::PackageTypeEnv,
    tast::{self, TastIdent, TypeVar},
    typer::{
        Typer,
        member_lookup::resolve_field_ty_eager,
        obligations::{
            ArithmeticKind, InstantiatedScheme, MethodGoal, Obligation, ObligationCause,
            ObligationCauseKind, ObligationId, ObligationWorklist, OperationGoal, ParamEnv,
            Predicate, ProjectionGoal, TraitGoal,
        },
        operators::{comparison_operand_is_valid, comparison_operator_text, is_numeric_ty},
        results::{CallElab, CalleeElab, Coercion, NameRefElab},
        traits::solver::{SelectionResult, SelectionSource, TraitSolver},
        type_ops::{
            contains_tvar, decompose_struct_type, instantiate_self_ty, same_or_unresolved_ty,
            substitute_ty_params, type_vars,
        },
    },
};
use diagnostics::{Severity, Stage};
use parser::{Diagnostic, Diagnostics};
use text_size::TextRange;

enum MethodGoalOutcome {
    Pending(MethodGoal),
    Resolved {
        generated: Vec<(Predicate, ObligationCause)>,
        changed_variables: HashSet<TypeVar>,
    },
    Failed,
}

fn instantiate_struct_field_ty(
    diagnostics: &mut Diagnostics,
    struct_def: &crate::env::StructDef,
    type_args: &[tast::Ty],
    field: &TastIdent,
) -> Option<tast::Ty> {
    const COMPLETION_PLACEHOLDER: &str = "completion_placeholder";
    if struct_def.generics.len() != type_args.len() {
        super::util::push_error(
            diagnostics,
            format!(
                "Struct {} expects {} type arguments, but got {}",
                struct_def.name.0,
                struct_def.generics.len(),
                type_args.len()
            ),
        );
        return None;
    }

    let mut subst = HashMap::new();
    for (param, arg) in struct_def.generics.iter().zip(type_args.iter()) {
        subst.insert(param.0.clone(), arg.clone());
    }

    if let Some((_, ty)) = struct_def.fields.iter().find(|(fname, _)| fname == field) {
        Some(substitute_ty_params(ty, &subst))
    } else if field.0 == COMPLETION_PLACEHOLDER {
        Some(tast::Ty::TUnit)
    } else {
        super::util::push_error(
            diagnostics,
            format!("Struct {} has no field {}", struct_def.name.0, field.0),
        );
        None
    }
}

impl Typer {
    fn predicate_type_vars(&mut self, predicate: &Predicate) -> HashSet<TypeVar> {
        let mut variables = HashSet::new();
        let mut add = |ty: &tast::Ty, typer: &mut Self| {
            variables.extend(type_vars(&typer.norm(ty)));
        };
        match predicate {
            Predicate::Trait(goal) => add(&goal.for_ty, self),
            Predicate::Method(goal) => {
                add(&goal.receiver_ty, self);
                add(&goal.call_site_type, self);
            }
            Predicate::Projection(goal) => match goal {
                ProjectionGoal::Field {
                    base_ty, result_ty, ..
                } => {
                    add(base_ty, self);
                    add(result_ty, self);
                }
                ProjectionGoal::Tuple {
                    tuple_ty,
                    result_ty,
                    ..
                } => {
                    add(tuple_ty, self);
                    add(result_ty, self);
                }
            },
            Predicate::Coerce(goal) => {
                add(&goal.from_ty, self);
                add(&goal.to_ty, self);
            }
            Predicate::Operation(goal) => match goal {
                OperationGoal::Arithmetic { ty, .. } => add(ty, self),
                OperationGoal::Comparison { lhs_ty, rhs_ty, .. } => {
                    add(lhs_ty, self);
                    add(rhs_ty, self);
                }
            },
        }
        variables
    }

    fn equate_and_collect(
        &mut self,
        diagnostics: &mut Diagnostics,
        left: &tast::Ty,
        right: &tast::Ty,
        origin: Option<TextRange>,
    ) -> HashSet<TypeVar> {
        let mut variables = type_vars(&self.norm(left));
        variables.extend(type_vars(&self.norm(right)));
        if self.unify(diagnostics, left, right, origin) {
            variables
        } else {
            HashSet::new()
        }
    }

    fn equate_and_wake(
        &mut self,
        diagnostics: &mut Diagnostics,
        worklist: &mut ObligationWorklist,
        left: &tast::Ty,
        right: &tast::Ty,
        origin: Option<TextRange>,
    ) {
        let variables = self.equate_and_collect(diagnostics, left, right, origin);
        worklist.wake(variables);
    }

    fn cause_context(&self, cause: &ObligationCause) -> String {
        let mut parent = cause.parent;
        let mut contexts = Vec::new();
        while let Some(id) = parent {
            let Some(parent_cause) = self.obligation_causes.get(&id) else {
                break;
            };
            contexts.push(parent_cause.kind.description());
            parent = parent_cause.parent;
        }
        if contexts.is_empty() {
            String::new()
        } else {
            format!("; required by {}", contexts.join(", then "))
        }
    }

    fn push_obligation_error(
        &self,
        diagnostics: &mut Diagnostics,
        reported: &mut HashSet<(String, Option<TextRange>)>,
        message: String,
        cause: &ObligationCause,
    ) {
        let message = format!("{}{}", message, self.cause_context(cause));
        if reported.insert((message.clone(), cause.span)) {
            diagnostics.push(
                Diagnostic::new(Stage::Typer, Severity::Error, message).with_range(cause.span),
            );
        }
    }

    pub(crate) fn origin_for_unresolved_type_var(&self, variable: TypeVar) -> Option<TextRange> {
        self.unresolved_type_var_origins
            .get(&variable)
            .copied()
            .flatten()
    }

    fn solve_method_goal(
        &mut self,
        genv: &PackageTypeEnv,
        trait_solver: &mut TraitSolver<'_>,
        diagnostics: &mut Diagnostics,
        mut goal: MethodGoal,
        cause: ObligationCause,
        parent: ObligationId,
    ) -> MethodGoalOutcome {
        goal.receiver_ty = self.norm(&goal.receiver_ty);
        goal.call_site_type = self.norm(&goal.call_site_type);
        if contains_tvar(&goal.receiver_ty) {
            return MethodGoalOutcome::Pending(goal);
        }

        if let Some(scheme) =
            genv.lookup_visible_inherent_method_scheme(&goal.receiver_ty, &goal.method)
        {
            let instantiated = self.instantiate_scheme(
                &scheme,
                ObligationCause::new(cause.span, ObligationCauseKind::FunctionBound)
                    .with_parent(parent),
            );
            let method_ty = instantiated.ty.clone();
            let generated = Self::scheme_obligations(&instantiated);
            let changed_variables =
                self.equate_and_collect(diagnostics, &goal.call_site_type, &method_ty, cause.span);
            self.record_inherent_method_resolution(&goal, method_ty);
            return MethodGoalOutcome::Resolved {
                generated,
                changed_variables,
            };
        }

        if let Some(field_ty) = resolve_field_ty_eager(genv, &goal.receiver_ty, &goal.method) {
            let tast::Ty::TFunc { params, ret_ty } = &field_ty else {
                super::util::push_error_with_range(
                    diagnostics,
                    format!(
                        "Field {} on type {} is not callable",
                        goal.method.0,
                        super::util::format_ty_for_diag(&goal.receiver_ty)
                    ),
                    cause.span,
                );
                return MethodGoalOutcome::Failed;
            };
            let mut method_params = vec![goal.receiver_ty.clone()];
            method_params.extend(params.iter().cloned());
            let method_ty = tast::Ty::TFunc {
                params: method_params,
                ret_ty: ret_ty.clone(),
            };
            self.results
                .record_expr_ty(goal.func_expr_id, field_ty.clone());
            self.results.record_call_elab(
                goal.call_expr_id,
                CallElab {
                    callee: CalleeElab::Expr(goal.func_expr_id),
                    args: goal.args.clone(),
                },
            );
            let changed_variables =
                self.equate_and_collect(diagnostics, &goal.call_site_type, &method_ty, cause.span);
            return MethodGoalOutcome::Resolved {
                generated: Vec::new(),
                changed_variables,
            };
        }

        let mut candidates = Vec::new();
        let mut ambiguous_impls = Vec::new();
        for trait_name in &goal.in_scope_traits {
            let Some((resolved, trait_env)) = super::util::resolve_trait_name(genv, &trait_name.0)
            else {
                continue;
            };
            let trait_name = tast::TastIdent(resolved);
            let Some(trait_scheme) =
                trait_env.lookup_trait_method_scheme(&trait_name, &goal.method)
            else {
                continue;
            };
            let trait_goal = TraitGoal {
                trait_name: trait_name.clone(),
                for_ty: goal.receiver_ty.clone(),
            };
            match trait_solver.select_ground(trait_goal) {
                SelectionResult::Unique(selection) => {
                    candidates.push((trait_name, trait_scheme, selection));
                }
                SelectionResult::Ambiguous(ids) => ambiguous_impls.extend(ids),
                SelectionResult::NoSolution => {}
                SelectionResult::Overflow => {
                    diagnostics.push(
                        Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            format!(
                                "Trait resolution overflow while resolving method {} for {}",
                                goal.method.0,
                                super::util::format_ty_for_diag(&goal.receiver_ty)
                            ),
                        )
                        .with_range(cause.span),
                    );
                    return MethodGoalOutcome::Failed;
                }
            }
        }

        if !ambiguous_impls.is_empty() {
            let impls = ambiguous_impls
                .iter()
                .map(|id| trait_solver.describe_candidate(id))
                .collect::<Vec<_>>()
                .join(", ");
            diagnostics.push(
                Diagnostic::new(
                    Stage::Typer,
                    Severity::Error,
                    format!(
                        "Multiple implementations can provide method {} for {} ({})",
                        goal.method.0,
                        super::util::format_ty_for_diag(&goal.receiver_ty),
                        impls
                    ),
                )
                .with_range(cause.span),
            );
            return MethodGoalOutcome::Failed;
        }

        let [(trait_name, trait_scheme, selection)] = candidates.as_slice() else {
            if candidates.is_empty() {
                super::util::push_error_with_range(
                    diagnostics,
                    format!(
                        "Method {} not found for type {}",
                        goal.method.0,
                        super::util::format_ty_for_diag(&goal.receiver_ty)
                    ),
                    cause.span,
                );
            } else {
                let names = candidates
                    .iter()
                    .map(|(trait_name, _, _)| trait_name.0.clone())
                    .collect::<Vec<_>>()
                    .join(", ");
                super::util::push_error_with_range(
                    diagnostics,
                    format!(
                        "Ambiguous method {} for type {} (candidates: {}). Use UFCS like Trait::{}(...) to disambiguate",
                        goal.method.0,
                        super::util::format_ty_for_diag(&goal.receiver_ty),
                        names,
                        goal.method.0
                    ),
                    cause.span,
                );
            }
            return MethodGoalOutcome::Failed;
        };

        let (scheme, substitution, instantiate_self, obligation_kind) = match &selection.source {
            SelectionSource::Impl {
                id,
                definition,
                substitution,
            } => {
                let Some(scheme) = definition.methods.get(&goal.method.0).cloned() else {
                    let candidate = trait_solver.describe_candidate(id);
                    diagnostics.push(
                        Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            format!(
                                "Selected implementation {} has no method {}",
                                candidate, goal.method.0
                            ),
                        )
                        .with_range(cause.span),
                    );
                    return MethodGoalOutcome::Failed;
                };
                (
                    scheme,
                    substitution.clone(),
                    false,
                    ObligationCauseKind::ImplBound,
                )
            }
            SelectionSource::ParamEnv | SelectionSource::Dyn => (
                trait_scheme.clone(),
                HashMap::new(),
                true,
                ObligationCauseKind::FunctionBound,
            ),
        };
        let instantiated = self.instantiate_scheme_with_substitution(
            &scheme,
            substitution,
            ObligationCause::new(cause.span, obligation_kind).with_parent(parent),
        );
        let method_ty = if instantiate_self {
            instantiate_self_ty(&instantiated.ty, &goal.receiver_ty)
        } else {
            instantiated.ty.clone()
        };
        let generated = Self::scheme_obligations(&instantiated);
        let changed_variables =
            self.equate_and_collect(diagnostics, &goal.call_site_type, &method_ty, cause.span);
        self.record_trait_method_resolution(&goal, trait_name, method_ty);
        MethodGoalOutcome::Resolved {
            generated,
            changed_variables,
        }
    }

    fn scheme_obligations(instantiated: &InstantiatedScheme) -> Vec<(Predicate, ObligationCause)> {
        instantiated
            .obligations
            .iter()
            .map(|(goal, cause)| (Predicate::Trait(goal.clone()), cause.clone()))
            .collect()
    }

    fn record_inherent_method_resolution(&mut self, goal: &MethodGoal, method_ty: tast::Ty) {
        self.results
            .record_expr_ty(goal.func_expr_id, method_ty.clone());
        self.results.record_name_ref_elab(
            goal.func_expr_id,
            NameRefElab::InherentMethod {
                receiver_ty: goal.receiver_ty.clone(),
                method_name: goal.method.clone(),
                ty: method_ty.clone(),
                astptr: None,
            },
        );
        self.results.record_call_elab(
            goal.call_expr_id,
            CallElab {
                callee: CalleeElab::InherentMethod {
                    receiver_ty: goal.receiver_ty.clone(),
                    method_name: goal.method.clone(),
                    ty: method_ty,
                    astptr: None,
                },
                args: std::iter::once(goal.receiver_expr_id)
                    .chain(goal.args.iter().copied())
                    .collect(),
            },
        );
    }

    fn record_trait_method_resolution(
        &mut self,
        goal: &MethodGoal,
        trait_name: &tast::TastIdent,
        method_ty: tast::Ty,
    ) {
        self.results
            .record_expr_ty(goal.func_expr_id, method_ty.clone());
        self.results.record_name_ref_elab(
            goal.func_expr_id,
            NameRefElab::TraitMethod {
                trait_name: trait_name.clone(),
                method_name: goal.method.clone(),
                ty: method_ty.clone(),
                astptr: None,
            },
        );
        self.results.record_call_elab(
            goal.call_expr_id,
            CallElab {
                callee: CalleeElab::TraitMethod {
                    trait_name: trait_name.clone(),
                    method_name: goal.method.clone(),
                    ty: method_ty,
                    astptr: None,
                },
                args: std::iter::once(goal.receiver_expr_id)
                    .chain(goal.args.iter().copied())
                    .collect(),
            },
        );
    }

    pub fn solve(&mut self, genv: &PackageTypeEnv, diagnostics: &mut Diagnostics) {
        self.reported_unresolved_type_origins.clear();
        let param_env = ParamEnv::from_bounds(&self.tparam_trait_bounds);
        let mut trait_solver = TraitSolver::new(genv, &param_env);
        let mut worklist = ObligationWorklist::new(std::mem::take(&mut self.obligations));
        let mut reported = HashSet::new();
        let mut allow_trait_inference = false;

        loop {
            while let Some(obligation) = worklist.pop() {
                let Obligation {
                    id,
                    predicate,
                    cause,
                } = obligation;
                match predicate {
                    Predicate::Trait(mut goal) => {
                        goal.for_ty = self.norm(&goal.for_ty);
                        if contains_tvar(&goal.for_ty) && !allow_trait_inference {
                            let predicate = Predicate::Trait(goal);
                            let variables = self.predicate_type_vars(&predicate);
                            worklist.defer(
                                Obligation {
                                    id,
                                    predicate,
                                    cause,
                                },
                                variables,
                            );
                            continue;
                        }
                        let result = if contains_tvar(&goal.for_ty) {
                            trait_solver.select(self, goal.clone())
                        } else {
                            trait_solver.select_ground(goal.clone())
                        };
                        goal.for_ty = self.norm(&goal.for_ty);
                        let unresolved = contains_tvar(&goal.for_ty);
                        match result {
                            SelectionResult::Unique(selection) => {
                                worklist.wake(selection.changed_variables);
                            }
                            SelectionResult::NoSolution | SelectionResult::Ambiguous(_)
                                if unresolved =>
                            {
                                let predicate = Predicate::Trait(goal);
                                let variables = self.predicate_type_vars(&predicate);
                                worklist.defer(
                                    Obligation {
                                        id,
                                        predicate,
                                        cause,
                                    },
                                    variables,
                                );
                            }
                            SelectionResult::NoSolution => self.push_obligation_error(
                                diagnostics,
                                &mut reported,
                                format!(
                                    "No instance found for trait {}<{}>",
                                    goal.trait_name.0,
                                    super::util::format_ty_for_diag(&goal.for_ty)
                                ),
                                &cause,
                            ),
                            SelectionResult::Ambiguous(ids) => {
                                let candidates = ids
                                    .iter()
                                    .map(|id| trait_solver.describe_candidate(id))
                                    .collect::<Vec<_>>()
                                    .join(", ");
                                self.push_obligation_error(
                                    diagnostics,
                                    &mut reported,
                                    format!(
                                        "Multiple instances found for trait {}<{}> ({})",
                                        goal.trait_name.0,
                                        super::util::format_ty_for_diag(&goal.for_ty),
                                        candidates
                                    ),
                                    &cause,
                                );
                            }
                            SelectionResult::Overflow => self.push_obligation_error(
                                diagnostics,
                                &mut reported,
                                format!(
                                    "Trait resolution overflow for {}<{}>",
                                    goal.trait_name.0,
                                    super::util::format_ty_for_diag(&goal.for_ty)
                                ),
                                &cause,
                            ),
                        }
                    }
                    Predicate::Method(goal) => {
                        match self.solve_method_goal(
                            genv,
                            &mut trait_solver,
                            diagnostics,
                            goal,
                            cause.clone(),
                            id,
                        ) {
                            MethodGoalOutcome::Pending(goal) => {
                                let predicate = Predicate::Method(goal);
                                let variables = self.predicate_type_vars(&predicate);
                                worklist.defer(
                                    Obligation {
                                        id,
                                        predicate,
                                        cause,
                                    },
                                    variables,
                                );
                            }
                            MethodGoalOutcome::Resolved {
                                generated,
                                changed_variables,
                            } => {
                                worklist.wake(changed_variables);
                                for (predicate, cause) in generated {
                                    let obligation = self.new_obligation(predicate, cause);
                                    worklist.push(obligation);
                                }
                            }
                            MethodGoalOutcome::Failed => {}
                        }
                    }
                    Predicate::Projection(goal) => match goal {
                        ProjectionGoal::Field {
                            base_ty,
                            field,
                            result_ty,
                        } => {
                            let base_ty = self.norm(&base_ty);
                            if contains_tvar(&base_ty) {
                                let predicate = Predicate::Projection(ProjectionGoal::Field {
                                    base_ty,
                                    field,
                                    result_ty,
                                });
                                let variables = self.predicate_type_vars(&predicate);
                                worklist.defer(
                                    Obligation {
                                        id,
                                        predicate,
                                        cause,
                                    },
                                    variables,
                                );
                                continue;
                            }
                            let Some((type_name, type_args)) = decompose_struct_type(&base_ty)
                            else {
                                self.push_obligation_error(
                                    diagnostics,
                                    &mut reported,
                                    format!(
                                        "Field {} not found on type {}",
                                        field.0,
                                        super::util::format_ty_for_diag(&base_ty)
                                    ),
                                    &cause,
                                );
                                continue;
                            };
                            let (resolved, env) = super::util::resolve_type_name(genv, &type_name);
                            let Some(struct_def) = env.structs().get(&TastIdent(resolved)) else {
                                self.push_obligation_error(
                                    diagnostics,
                                    &mut reported,
                                    format!(
                                        "Struct {} not found when accessing field {}",
                                        type_name, field.0
                                    ),
                                    &cause,
                                );
                                continue;
                            };
                            if let Some(field_ty) = instantiate_struct_field_ty(
                                diagnostics,
                                struct_def,
                                &type_args,
                                &field,
                            ) {
                                self.equate_and_wake(
                                    diagnostics,
                                    &mut worklist,
                                    &result_ty,
                                    &field_ty,
                                    cause.span,
                                );
                            }
                        }
                        ProjectionGoal::Tuple {
                            tuple_ty,
                            index,
                            result_ty,
                        } => {
                            let tuple_ty = self.norm(&tuple_ty);
                            match &tuple_ty {
                                tast::Ty::TTuple { typs } => {
                                    let Some(field_ty) = typs.get(index) else {
                                        self.push_obligation_error(
                                            diagnostics,
                                            &mut reported,
                                            format!(
                                                "Tuple index {} out of bounds for type {}",
                                                index,
                                                super::util::format_ty_for_diag(&tuple_ty)
                                            ),
                                            &cause,
                                        );
                                        continue;
                                    };
                                    self.equate_and_wake(
                                        diagnostics,
                                        &mut worklist,
                                        &result_ty,
                                        field_ty,
                                        cause.span,
                                    );
                                }
                                _ if contains_tvar(&tuple_ty) => {
                                    let predicate = Predicate::Projection(ProjectionGoal::Tuple {
                                        tuple_ty,
                                        index,
                                        result_ty,
                                    });
                                    let variables = self.predicate_type_vars(&predicate);
                                    worklist.defer(
                                        Obligation {
                                            id,
                                            predicate,
                                            cause,
                                        },
                                        variables,
                                    );
                                }
                                _ => self.push_obligation_error(
                                    diagnostics,
                                    &mut reported,
                                    format!(
                                        "Cannot project field {} on non-tuple type {}",
                                        index,
                                        super::util::format_ty_for_diag(&tuple_ty)
                                    ),
                                    &cause,
                                ),
                            }
                        }
                    },
                    Predicate::Coerce(goal) => {
                        let from_ty = self.norm(&goal.from_ty);
                        let to_ty = self.norm(&goal.to_ty);
                        if from_ty == to_ty {
                            continue;
                        }
                        if matches!(to_ty, tast::Ty::TVar(_)) {
                            let predicate = Predicate::Coerce(crate::typer::CoercionGoal {
                                expr_id: goal.expr_id,
                                from_ty,
                                to_ty,
                            });
                            let variables = self.predicate_type_vars(&predicate);
                            worklist.defer(
                                Obligation {
                                    id,
                                    predicate,
                                    cause,
                                },
                                variables,
                            );
                            continue;
                        }
                        let tast::Ty::TDyn { trait_name } = &to_ty else {
                            self.equate_and_wake(
                                diagnostics,
                                &mut worklist,
                                &from_ty,
                                &to_ty,
                                cause.span,
                            );
                            continue;
                        };
                        if contains_tvar(&from_ty) {
                            let predicate = Predicate::Coerce(crate::typer::CoercionGoal {
                                expr_id: goal.expr_id,
                                from_ty,
                                to_ty,
                            });
                            let variables = self.predicate_type_vars(&predicate);
                            worklist.defer(
                                Obligation {
                                    id,
                                    predicate,
                                    cause,
                                },
                                variables,
                            );
                            continue;
                        }
                        if matches!(from_ty, tast::Ty::TDyn { .. }) {
                            self.equate_and_wake(
                                diagnostics,
                                &mut worklist,
                                &from_ty,
                                &to_ty,
                                cause.span,
                            );
                            continue;
                        }
                        let Some((resolved_trait, _)) =
                            super::util::resolve_trait_name(genv, trait_name)
                        else {
                            self.push_obligation_error(
                                diagnostics,
                                &mut reported,
                                format!("Unknown trait {}", trait_name),
                                &cause,
                            );
                            continue;
                        };
                        let trait_goal = TraitGoal {
                            trait_name: tast::TastIdent(resolved_trait.clone()),
                            for_ty: from_ty.clone(),
                        };
                        match trait_solver.select_ground(trait_goal) {
                            SelectionResult::Unique(_) => {
                                self.results.push_coercion(
                                    goal.expr_id,
                                    Coercion::ToDyn {
                                        trait_name: tast::TastIdent(resolved_trait),
                                        for_ty: from_ty,
                                        ty: to_ty,
                                        astptr: None,
                                    },
                                );
                            }
                            SelectionResult::NoSolution => self.push_obligation_error(
                                diagnostics,
                                &mut reported,
                                format!(
                                    "Type {} does not implement trait {}",
                                    super::util::format_ty_for_diag(&from_ty),
                                    resolved_trait
                                ),
                                &cause,
                            ),
                            SelectionResult::Ambiguous(ids) => {
                                let candidates = ids
                                    .iter()
                                    .map(|id| trait_solver.describe_candidate(id))
                                    .collect::<Vec<_>>()
                                    .join(", ");
                                self.push_obligation_error(
                                    diagnostics,
                                    &mut reported,
                                    format!(
                                        "Multiple instances found for trait {}<{}> ({})",
                                        resolved_trait,
                                        super::util::format_ty_for_diag(&from_ty),
                                        candidates
                                    ),
                                    &cause,
                                );
                            }
                            SelectionResult::Overflow => self.push_obligation_error(
                                diagnostics,
                                &mut reported,
                                format!(
                                    "Trait resolution overflow for {}<{}>",
                                    resolved_trait,
                                    super::util::format_ty_for_diag(&from_ty)
                                ),
                                &cause,
                            ),
                        }
                    }
                    Predicate::Operation(goal) => match goal {
                        OperationGoal::Arithmetic { kind, ty, operator } => {
                            let ty = self.norm(&ty);
                            if contains_tvar(&ty) {
                                let predicate = Predicate::Operation(OperationGoal::Arithmetic {
                                    kind,
                                    ty,
                                    operator,
                                });
                                let variables = self.predicate_type_vars(&predicate);
                                worklist.defer(
                                    Obligation {
                                        id,
                                        predicate,
                                        cause,
                                    },
                                    variables,
                                );
                                continue;
                            }
                            let valid = match kind {
                                ArithmeticKind::NumericOrString => {
                                    is_numeric_ty(&ty) || matches!(ty, tast::Ty::TString)
                                }
                                ArithmeticKind::Numeric => is_numeric_ty(&ty),
                            };
                            if !valid {
                                self.push_obligation_error(
                                    diagnostics,
                                    &mut reported,
                                    format!(
                                        "Operator {} is not defined for type {}",
                                        operator,
                                        super::util::format_ty_for_diag(&ty)
                                    ),
                                    &cause,
                                );
                            }
                        }
                        OperationGoal::Comparison {
                            operator,
                            lhs_ty,
                            rhs_ty,
                        } => {
                            let lhs_ty = self.norm(&lhs_ty);
                            let rhs_ty = self.norm(&rhs_ty);
                            if contains_tvar(&lhs_ty) || contains_tvar(&rhs_ty) {
                                let predicate = Predicate::Operation(OperationGoal::Comparison {
                                    operator,
                                    lhs_ty,
                                    rhs_ty,
                                });
                                let variables = self.predicate_type_vars(&predicate);
                                worklist.defer(
                                    Obligation {
                                        id,
                                        predicate,
                                        cause,
                                    },
                                    variables,
                                );
                                continue;
                            }
                            if same_or_unresolved_ty(&lhs_ty, &rhs_ty)
                                && !comparison_operand_is_valid(genv, operator, &lhs_ty)
                            {
                                self.push_obligation_error(
                                    diagnostics,
                                    &mut reported,
                                    format!(
                                        "Operator {} is not defined for type {}",
                                        comparison_operator_text(operator),
                                        super::util::format_ty_for_diag(&lhs_ty)
                                    ),
                                    &cause,
                                );
                            }
                        }
                    },
                }
            }

            let waiting = worklist.drain_waiting();
            if waiting.is_empty() {
                break;
            }

            let mut retained = Vec::new();
            let mut fallback_progress = false;
            for obligation in waiting {
                let fallback = match &obligation.predicate {
                    Predicate::Coerce(goal) => {
                        let from_ty = self.norm(&goal.from_ty);
                        let to_ty = self.norm(&goal.to_ty);
                        if matches!(to_ty, tast::Ty::TVar(_)) && from_ty != to_ty {
                            let _ = self.equate_and_collect(
                                diagnostics,
                                &from_ty,
                                &to_ty,
                                obligation.cause.span,
                            );
                            true
                        } else {
                            false
                        }
                    }
                    _ => false,
                };
                if fallback {
                    fallback_progress = true;
                } else {
                    retained.push(obligation);
                }
            }
            if fallback_progress {
                worklist = ObligationWorklist::new(retained);
                continue;
            }
            if !allow_trait_inference {
                allow_trait_inference = true;
                worklist = ObligationWorklist::new(retained);
                continue;
            }

            for obligation in retained {
                let variables = self.predicate_type_vars(&obligation.predicate);
                for variable in variables {
                    self.unresolved_type_var_origins
                        .entry(variable)
                        .or_insert(obligation.cause.span);
                }
                let message = match &obligation.predicate {
                    Predicate::Trait(goal) => format!(
                        "Could not infer the type required to prove {}<{}>",
                        goal.trait_name.0,
                        super::util::format_ty_for_diag(&self.norm(&goal.for_ty))
                    ),
                    Predicate::Method(goal) => format!(
                        "Could not infer the receiver type for method {}",
                        goal.method.0
                    ),
                    Predicate::Projection(ProjectionGoal::Field { field, .. }) => {
                        format!("Could not infer the base type for field {}", field.0)
                    }
                    Predicate::Projection(ProjectionGoal::Tuple { index, .. }) => {
                        format!("Could not infer the tuple type for projection {}", index)
                    }
                    Predicate::Coerce(_) => {
                        "Could not infer whether a trait-object coercion is required".to_string()
                    }
                    Predicate::Operation(OperationGoal::Arithmetic { operator, .. }) => {
                        format!("Could not infer the operand type for operator {}", operator)
                    }
                    Predicate::Operation(OperationGoal::Comparison { operator, .. }) => format!(
                        "Could not infer the operand type for operator {}",
                        comparison_operator_text(*operator)
                    ),
                };
                self.push_obligation_error(diagnostics, &mut reported, message, &obligation.cause);
                self.reported_unresolved_type_origins
                    .insert(obligation.cause.span.map(|range| range.start()));
            }
            break;
        }

        self.obligation_causes.clear();
        self.next_obligation_id = 0;
    }
}
