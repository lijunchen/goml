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
        operators::{
            comparison_operand_is_valid, comparison_operator_text, is_integer_ty, is_numeric_ty,
        },
        results::{CallElab, CalleeElab, Coercion, NameRefElab},
        traits::solver::{SelectionResult, SelectionSource, TraitSolver},
        type_ops::{
            contains_tvar, decompose_struct_type, same_or_unresolved_ty, substitute_ty_params,
            type_vars,
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
            Predicate::Trait(goal) => {
                add(&goal.for_ty, self);
                for arg in &goal.trait_ref.args {
                    add(arg, self);
                }
            }
            Predicate::TypeEquality(goal) => {
                add(&goal.lhs, self);
                add(&goal.rhs, self);
            }
            Predicate::Method(goal) => {
                add(&goal.receiver_ty, self);
                add(&goal.call_site_type, self);
            }
            Predicate::Projection(goal) => match goal {
                ProjectionGoal::AssociatedType {
                    trait_ref,
                    for_ty,
                    result_ty,
                    ..
                } => {
                    add(for_ty, self);
                    for arg in &trait_ref.args {
                        add(arg, self);
                    }
                    add(result_ty, self);
                }
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

    fn depends_on_failed_obligation(
        &self,
        cause: &ObligationCause,
        failed: &HashSet<ObligationId>,
    ) -> bool {
        let mut parent = cause.parent;
        while let Some(id) = parent {
            if failed.contains(&id) {
                return true;
            }
            parent = self
                .obligation_causes
                .get(&id)
                .and_then(|cause| cause.parent);
        }
        false
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

        if contains_tvar(&goal.receiver_ty) {
            return MethodGoalOutcome::Pending(goal);
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
            let Some(definition) = trait_env.trait_env.trait_defs.get(&resolved) else {
                continue;
            };
            let mut trait_ref = tast::TraitRef {
                name: tast::TastIdent(resolved),
                args: definition
                    .params
                    .iter()
                    .map(|_| self.fresh_ty_var())
                    .collect(),
            };
            let trait_goal = TraitGoal {
                trait_ref: trait_ref.clone(),
                for_ty: goal.receiver_ty.clone(),
            };
            match trait_solver.select(self, trait_goal) {
                SelectionResult::Unique(root_selection) => {
                    for arg in &mut trait_ref.args {
                        *arg = self.norm(arg);
                    }
                    for method_trait_ref in super::util::trait_ref_closure(genv, &trait_ref) {
                        let Some((_, method_env)) =
                            super::util::resolve_trait_name(genv, &method_trait_ref.name.0)
                        else {
                            continue;
                        };
                        let Some(trait_scheme) =
                            method_env.lookup_trait_method_scheme(&method_trait_ref, &goal.method)
                        else {
                            continue;
                        };
                        let selection = if method_trait_ref == trait_ref {
                            SelectionResult::Unique(root_selection.clone())
                        } else {
                            trait_solver.select(
                                self,
                                TraitGoal {
                                    trait_ref: method_trait_ref.clone(),
                                    for_ty: goal.receiver_ty.clone(),
                                },
                            )
                        };
                        match selection {
                            SelectionResult::Unique(selection) => {
                                candidates.push((method_trait_ref, trait_scheme, selection));
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
                                            super::util::format_ty_for_diag(
                                                &goal.receiver_ty
                                            )
                                        ),
                                    )
                                    .with_range(cause.span),
                                );
                                return MethodGoalOutcome::Failed;
                            }
                        }
                    }
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

        candidates.sort_by(|(left, _, _), (right, _, _)| {
            super::util::format_trait_ref_for_diag(left)
                .cmp(&super::util::format_trait_ref_for_diag(right))
        });
        candidates.dedup_by(|(left, _, _), (right, _, _)| left == right);

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

        let [(trait_ref, trait_scheme, selection)] = candidates.as_slice() else {
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
                    .map(|(trait_ref, _, _)| super::util::format_trait_ref_for_diag(trait_ref))
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
            SelectionSource::ParamEnv | SelectionSource::Dyn | SelectionSource::BuiltinEq => (
                trait_scheme.clone(),
                HashMap::new(),
                true,
                ObligationCauseKind::FunctionBound,
            ),
        };
        let obligation_cause =
            ObligationCause::new(cause.span, obligation_kind).with_parent(parent);
        let instantiated = if instantiate_self {
            self.instantiate_scheme_with_self(&scheme, &goal.receiver_ty, obligation_cause)
        } else {
            self.instantiate_scheme_with_substitution(&scheme, substitution, obligation_cause)
        };
        let method_ty = instantiated.ty.clone();
        let generated = Self::scheme_obligations(&instantiated);
        let changed_variables =
            self.equate_and_collect(diagnostics, &goal.call_site_type, &method_ty, cause.span);
        self.record_trait_method_resolution(&goal, trait_ref, method_ty);
        MethodGoalOutcome::Resolved {
            generated,
            changed_variables,
        }
    }

    fn scheme_obligations(instantiated: &InstantiatedScheme) -> Vec<(Predicate, ObligationCause)> {
        instantiated.obligations.clone()
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
        trait_ref: &tast::TraitRef,
        method_ty: tast::Ty,
    ) {
        self.results
            .record_expr_ty(goal.func_expr_id, method_ty.clone());
        self.results.record_name_ref_elab(
            goal.func_expr_id,
            NameRefElab::TraitMethod {
                trait_ref: trait_ref.clone(),
                method_name: goal.method.clone(),
                ty: method_ty.clone(),
                astptr: None,
            },
        );
        self.results.record_call_elab(
            goal.call_expr_id,
            CallElab {
                callee: CalleeElab::TraitMethod {
                    trait_ref: trait_ref.clone(),
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
        let param_env = ParamEnv::from_predicates(&self.param_env_predicates);
        let mut trait_solver = TraitSolver::new(genv, &param_env);
        let mut worklist = ObligationWorklist::new(std::mem::take(&mut self.obligations));
        let mut reported = HashSet::new();
        let mut failed = HashSet::new();
        let mut allow_trait_inference = false;

        loop {
            while let Some(obligation) = worklist.pop() {
                let Obligation {
                    id,
                    predicate,
                    cause,
                } = obligation;
                if self.depends_on_failed_obligation(&cause, &failed) {
                    if let Predicate::Projection(ProjectionGoal::AssociatedType {
                        result_ty, ..
                    }) = &predicate
                    {
                        let variables = type_vars(&self.norm(result_ty));
                        if self.try_unify_silent(result_ty, &tast::Ty::TUnit) {
                            worklist.wake(variables);
                        }
                    }
                    continue;
                }
                match predicate {
                    Predicate::Trait(mut goal) => {
                        goal.for_ty = self.norm(&goal.for_ty);
                        for arg in &mut goal.trait_ref.args {
                            *arg = self.norm(arg);
                        }
                        let goal_has_tvar = contains_tvar(&goal.for_ty)
                            || goal.trait_ref.args.iter().any(contains_tvar);
                        if goal_has_tvar && !allow_trait_inference {
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
                        let result = if goal_has_tvar {
                            trait_solver.select(self, goal.clone())
                        } else {
                            trait_solver.select_ground(goal.clone())
                        };
                        goal.for_ty = self.norm(&goal.for_ty);
                        for arg in &mut goal.trait_ref.args {
                            *arg = self.norm(arg);
                        }
                        let unresolved = contains_tvar(&goal.for_ty)
                            || goal.trait_ref.args.iter().any(contains_tvar);
                        match result {
                            SelectionResult::Unique(selection) => {
                                worklist.wake(selection.changed_variables);
                            }
                            SelectionResult::NoSolution
                                if unresolved
                                    && !matches!(cause.kind, ObligationCauseKind::ForLoop) =>
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
                            SelectionResult::Ambiguous(_) if unresolved => {
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
                            SelectionResult::NoSolution => {
                                if matches!(cause.kind, ObligationCauseKind::ForLoop) {
                                    failed.insert(id);
                                }
                                let message = if matches!(cause.kind, ObligationCauseKind::ForLoop)
                                {
                                    format!(
                                        "for loop expects a type implementing IntoIterator, got {}",
                                        super::util::format_ty_for_diag(&goal.for_ty)
                                    )
                                } else {
                                    format!(
                                        "No instance found for trait {}<{}>",
                                        super::util::format_trait_ref_for_diag(&goal.trait_ref),
                                        super::util::format_ty_for_diag(&goal.for_ty)
                                    )
                                };
                                self.push_obligation_error(
                                    diagnostics,
                                    &mut reported,
                                    message,
                                    &cause,
                                );
                            }
                            SelectionResult::Ambiguous(ids) => {
                                if matches!(cause.kind, ObligationCauseKind::ForLoop) {
                                    failed.insert(id);
                                }
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
                                        super::util::format_trait_ref_for_diag(&goal.trait_ref),
                                        super::util::format_ty_for_diag(&goal.for_ty),
                                        candidates
                                    ),
                                    &cause,
                                );
                            }
                            SelectionResult::Overflow => {
                                if matches!(cause.kind, ObligationCauseKind::ForLoop) {
                                    failed.insert(id);
                                }
                                self.push_obligation_error(
                                    diagnostics,
                                    &mut reported,
                                    format!(
                                        "Trait resolution overflow for {}<{}>",
                                        super::util::format_trait_ref_for_diag(&goal.trait_ref),
                                        super::util::format_ty_for_diag(&goal.for_ty)
                                    ),
                                    &cause,
                                )
                            }
                        }
                    }
                    Predicate::TypeEquality(goal) => {
                        let changed_variables =
                            self.equate_and_collect(diagnostics, &goal.lhs, &goal.rhs, cause.span);
                        worklist.wake(changed_variables);
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
                        ProjectionGoal::AssociatedType {
                            mut trait_ref,
                            for_ty,
                            name,
                            result_ty,
                        } => {
                            let for_ty = self.norm(&for_ty);
                            for arg in &mut trait_ref.args {
                                *arg = self.norm(arg);
                            }
                            let unresolved =
                                contains_tvar(&for_ty) || trait_ref.args.iter().any(contains_tvar);
                            if unresolved && !allow_trait_inference {
                                let predicate =
                                    Predicate::Projection(ProjectionGoal::AssociatedType {
                                        trait_ref,
                                        for_ty,
                                        name,
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
                            let trait_goal = TraitGoal {
                                trait_ref: trait_ref.clone(),
                                for_ty: for_ty.clone(),
                            };
                            let selection = if unresolved {
                                trait_solver.select(self, trait_goal)
                            } else {
                                trait_solver.select_ground(trait_goal)
                            };
                            match selection {
                                SelectionResult::Unique(selection) => match selection.source {
                                    SelectionSource::Impl {
                                        definition,
                                        substitution,
                                        ..
                                    } => {
                                        let Some(binding) =
                                            definition.associated_types.get(&name.0)
                                        else {
                                            self.push_obligation_error(
                                                diagnostics,
                                                &mut reported,
                                                format!(
                                                    "Selected implementation of {} for {} does not bind associated type {}",
                                                    super::util::format_trait_ref_for_diag(
                                                        &trait_ref
                                                    ),
                                                    super::util::format_ty_for_diag(&for_ty),
                                                    name.0
                                                ),
                                                &cause,
                                            );
                                            continue;
                                        };
                                        let binding = substitute_ty_params(binding, &substitution);
                                        let projection_cause = ObligationCause::new(
                                            cause.span,
                                            ObligationCauseKind::Projection,
                                        )
                                        .with_parent(id);
                                        let mut generated = Vec::new();
                                        let binding = self.lower_associated_projections(
                                            &binding,
                                            &projection_cause,
                                            &mut generated,
                                        );
                                        self.equate_and_wake(
                                            diagnostics,
                                            &mut worklist,
                                            &result_ty,
                                            &binding,
                                            cause.span,
                                        );
                                        worklist.wake(selection.changed_variables);
                                        for (predicate, cause) in generated {
                                            let obligation =
                                                self.new_obligation(predicate, cause);
                                            worklist.push(obligation);
                                        }
                                    }
                                    SelectionSource::ParamEnv => {
                                        let projection = tast::Ty::TProjection {
                                            trait_ref: Some(trait_ref),
                                            for_ty: Box::new(for_ty),
                                            name,
                                        };
                                        let projection = self.norm(&projection);
                                        self.equate_and_wake(
                                            diagnostics,
                                            &mut worklist,
                                            &result_ty,
                                            &projection,
                                            cause.span,
                                        );
                                        worklist.wake(selection.changed_variables);
                                    }
                                    SelectionSource::Dyn => {
                                        self.push_obligation_error(
                                            diagnostics,
                                            &mut reported,
                                            format!(
                                                "Associated type {} cannot be projected from a trait object",
                                                name.0
                                            ),
                                            &cause,
                                        );
                                    }
                                    SelectionSource::BuiltinEq => {
                                        self.push_obligation_error(
                                            diagnostics,
                                            &mut reported,
                                            format!(
                                                "Associated type {} cannot be projected from built-in Eq evidence",
                                                name.0
                                            ),
                                            &cause,
                                        );
                                    }
                                },
                                SelectionResult::NoSolution if unresolved => {
                                    let predicate = Predicate::Projection(
                                        ProjectionGoal::AssociatedType {
                                            trait_ref,
                                            for_ty,
                                            name,
                                            result_ty,
                                        },
                                    );
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
                                        "Cannot resolve associated type {}::{} because {} does not implement {}",
                                        super::util::format_ty_for_diag(&for_ty),
                                        name.0,
                                        super::util::format_ty_for_diag(&for_ty),
                                        super::util::format_trait_ref_for_diag(&trait_ref)
                                    ),
                                    &cause,
                                ),
                                SelectionResult::Ambiguous(_) => {
                                    if unresolved {
                                        let predicate = Predicate::Projection(
                                            ProjectionGoal::AssociatedType {
                                                trait_ref,
                                                for_ty,
                                                name,
                                                result_ty,
                                            },
                                        );
                                        let variables = self.predicate_type_vars(&predicate);
                                        worklist.defer(
                                            Obligation {
                                                id,
                                                predicate,
                                                cause,
                                            },
                                            variables,
                                        );
                                    } else {
                                        self.push_obligation_error(
                                            diagnostics,
                                            &mut reported,
                                            format!(
                                                "Associated type {}::{} is ambiguous",
                                                super::util::format_ty_for_diag(&for_ty),
                                                name.0
                                            ),
                                            &cause,
                                        );
                                    }
                                }
                                SelectionResult::Overflow => self.push_obligation_error(
                                    diagnostics,
                                    &mut reported,
                                    format!(
                                        "Associated type resolution overflow for {}::{}",
                                        super::util::format_ty_for_diag(&for_ty),
                                        name.0
                                    ),
                                    &cause,
                                ),
                            }
                        }
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
                            trait_ref: tast::TraitRef::without_args(tast::TastIdent(
                                resolved_trait.clone(),
                            )),
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
                                ArithmeticKind::Integer => is_integer_ty(&ty),
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
                    Predicate::Trait(goal)
                        if matches!(obligation.cause.kind, ObligationCauseKind::ForLoop) =>
                    {
                        format!(
                            "Could not infer the iterator type for for loop over {}",
                            super::util::format_ty_for_diag(&self.norm(&goal.for_ty))
                        )
                    }
                    Predicate::Trait(goal) => format!(
                        "Could not infer the type required to prove {}<{}>",
                        super::util::format_trait_ref_for_diag(&goal.trait_ref),
                        super::util::format_ty_for_diag(&self.norm(&goal.for_ty))
                    ),
                    Predicate::TypeEquality(goal) => format!(
                        "Could not infer whether {} equals {}",
                        super::util::format_ty_for_diag(&self.norm(&goal.lhs)),
                        super::util::format_ty_for_diag(&self.norm(&goal.rhs))
                    ),
                    Predicate::Method(goal) => format!(
                        "Could not infer the receiver type for method {}",
                        goal.method.0
                    ),
                    Predicate::Projection(ProjectionGoal::AssociatedType {
                        for_ty, name, ..
                    }) => format!(
                        "Could not resolve associated type {}::{}",
                        super::util::format_ty_for_diag(&self.norm(for_ty)),
                        name.0
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
