use std::collections::{HashMap, HashSet};

use ena::unify::UnifyKey;

use crate::{
    env,
    intrinsics::LangItemId,
    tast,
    typer::{
        Typer,
        obligations::{ParamEnv, TraitGoal},
        traits::index::{ImplCandidate, ImplId, ImplIndex},
        traits::matching::trait_impl_subst,
        type_ops::{
            contains_tvar, rewrite_ty, substitute_predicate, substitute_trait_ref,
            substitute_ty_params, trait_ref_type_vars, type_vars,
        },
    },
};

const MAX_GOAL_DEPTH: usize = 64;

#[derive(Debug, Clone)]
pub(crate) enum SelectionSource {
    Impl {
        id: ImplId,
        definition: Box<env::ImplDef>,
        substitution: HashMap<String, tast::Ty>,
    },
    ParamEnv,
    Dyn,
    BuiltinEq,
}

#[derive(Debug, Clone)]
pub(crate) struct Selection {
    pub source: SelectionSource,
    pub changed_variables: HashSet<tast::TypeVar>,
}

#[derive(Debug, Clone)]
pub(crate) enum SelectionResult {
    Unique(Selection),
    NoSolution,
    Ambiguous(Vec<ImplId>),
    Overflow,
}

#[derive(Debug, Clone)]
enum CachedSelectionResult {
    Unique(ImplId),
    NoSolution,
    Ambiguous(Vec<ImplId>),
    Overflow,
}

pub(crate) struct TraitSolver<'a> {
    env: &'a env::PackageTypeEnv,
    param_env: &'a ParamEnv,
    index: ImplIndex,
    cache: HashMap<TraitGoal, CachedSelectionResult>,
    active: HashSet<TraitGoal>,
    ground_cache: HashMap<TraitGoal, SelectionResult>,
    ground_active: HashSet<TraitGoal>,
}

impl<'a> TraitSolver<'a> {
    pub(crate) fn new(env: &'a env::PackageTypeEnv, param_env: &'a ParamEnv) -> Self {
        Self {
            env,
            param_env,
            index: ImplIndex::build(env),
            cache: HashMap::new(),
            active: HashSet::new(),
            ground_cache: HashMap::new(),
            ground_active: HashSet::new(),
        }
    }

    pub(crate) fn select(&mut self, typer: &mut Typer, goal: TraitGoal) -> SelectionResult {
        self.select_at_depth(typer, goal, 0)
    }

    pub(crate) fn select_ground(&mut self, goal: TraitGoal) -> SelectionResult {
        self.select_ground_at_depth(goal, 0)
    }

    pub(crate) fn describe_candidate(&self, id: &ImplId) -> String {
        self.index.describe_candidate(id)
    }

    pub(crate) fn normalize_ty(&mut self, typer: &mut Typer, ty: &tast::Ty) -> Option<tast::Ty> {
        self.normalize_candidate_ty(typer, ty, 0).ok()
    }

    fn select_param_env(&self, typer: &mut Typer, goal: &TraitGoal) -> Option<SelectionResult> {
        let matching = self
            .param_env
            .predicates()
            .iter()
            .filter_map(|predicate| {
                let env::TypePredicate::Trait { for_ty, trait_ref } = predicate else {
                    return None;
                };
                (trait_ref.name == goal.trait_ref.name
                    && trait_ref.args.len() == goal.trait_ref.args.len())
                .then_some((for_ty, trait_ref))
            })
            .collect::<Vec<_>>();
        if matching.is_empty() {
            return None;
        }
        let mut successful = Vec::new();
        for (for_ty, bound) in matching {
            let snapshot = typer.snapshot_inference();
            let matches = typer.try_unify_silent(for_ty, &goal.for_ty)
                && bound
                    .args
                    .iter()
                    .zip(goal.trait_ref.args.iter())
                    .all(|(bound, goal)| typer.try_unify_silent(bound, goal));
            typer.rollback_inference(snapshot);
            if matches {
                successful.push((for_ty.clone(), bound.clone()));
            }
        }
        let [(for_ty, bound)] = successful.as_slice() else {
            return (!successful.is_empty()).then_some(SelectionResult::Ambiguous(Vec::new()));
        };
        let changed_variables = type_vars(&goal.for_ty)
            .into_iter()
            .chain(trait_ref_type_vars(&goal.trait_ref))
            .collect();
        let snapshot = typer.snapshot_inference();
        let matches = typer.try_unify_silent(for_ty, &goal.for_ty)
            && bound
                .args
                .iter()
                .zip(goal.trait_ref.args.iter())
                .all(|(bound, goal)| typer.try_unify_silent(bound, goal));
        if !matches {
            typer.rollback_inference(snapshot);
            return Some(SelectionResult::NoSolution);
        }
        typer.commit_inference(snapshot);
        Some(SelectionResult::Unique(Selection {
            source: SelectionSource::ParamEnv,
            changed_variables,
        }))
    }

    fn builtin_eq_members(&self, goal: &TraitGoal) -> Option<Vec<tast::Ty>> {
        if !goal.trait_ref.args.is_empty()
            || self.env.lang_item(LangItemId::Eq) != Some(&goal.trait_ref.name)
        {
            return None;
        }
        match &goal.for_ty {
            tast::Ty::TTuple { typs } => Some(typs.clone()),
            tast::Ty::TArray { elem, .. } => Some(vec![elem.as_ref().clone()]),
            _ => None,
        }
    }

    fn select_builtin_eq(
        &mut self,
        typer: &mut Typer,
        goal: &TraitGoal,
        depth: usize,
    ) -> Option<SelectionResult> {
        let members = self.builtin_eq_members(goal)?;
        let mut changed_variables = HashSet::new();
        let mut ambiguous = Vec::new();
        for member in members {
            let nested = TraitGoal {
                trait_ref: goal.trait_ref.clone(),
                for_ty: member,
            };
            match self.select_at_depth(typer, nested, depth + 1) {
                SelectionResult::Unique(selection) => {
                    changed_variables.extend(selection.changed_variables);
                }
                SelectionResult::NoSolution => return Some(SelectionResult::NoSolution),
                SelectionResult::Ambiguous(ids) => ambiguous.extend(ids),
                SelectionResult::Overflow => return Some(SelectionResult::Overflow),
            }
        }
        if ambiguous.is_empty() {
            Some(SelectionResult::Unique(Selection {
                source: SelectionSource::BuiltinEq,
                changed_variables,
            }))
        } else {
            ambiguous.sort();
            ambiguous.dedup();
            Some(SelectionResult::Ambiguous(ambiguous))
        }
    }

    fn select_ground_builtin_eq(
        &mut self,
        goal: &TraitGoal,
        depth: usize,
    ) -> Option<SelectionResult> {
        let members = self.builtin_eq_members(goal)?;
        let mut ambiguous = Vec::new();
        for member in members {
            let nested = TraitGoal {
                trait_ref: goal.trait_ref.clone(),
                for_ty: member,
            };
            match self.select_ground_at_depth(nested, depth + 1) {
                SelectionResult::Unique(_) => {}
                SelectionResult::NoSolution => return Some(SelectionResult::NoSolution),
                SelectionResult::Ambiguous(ids) => ambiguous.extend(ids),
                SelectionResult::Overflow => return Some(SelectionResult::Overflow),
            }
        }
        if ambiguous.is_empty() {
            Some(SelectionResult::Unique(Selection {
                source: SelectionSource::BuiltinEq,
                changed_variables: HashSet::new(),
            }))
        } else {
            ambiguous.sort();
            ambiguous.dedup();
            Some(SelectionResult::Ambiguous(ambiguous))
        }
    }

    fn select_at_depth(
        &mut self,
        typer: &mut Typer,
        mut goal: TraitGoal,
        depth: usize,
    ) -> SelectionResult {
        if depth >= MAX_GOAL_DEPTH {
            return SelectionResult::Overflow;
        }
        goal.for_ty = typer.norm(&goal.for_ty);
        for arg in &mut goal.trait_ref.args {
            *arg = typer.norm(arg);
        }
        if let Some(result) = self.select_param_env(typer, &goal) {
            return result;
        }
        if matches!(
            &goal.for_ty,
            tast::Ty::TDyn { trait_name }
                if goal.trait_ref.args.is_empty() && trait_name == &goal.trait_ref.name.0
        ) {
            return SelectionResult::Unique(Selection {
                source: SelectionSource::Dyn,
                changed_variables: HashSet::new(),
            });
        }
        if let Some(result) = self.select_builtin_eq(typer, &goal, depth) {
            return result;
        }

        let canonical_goal = canonicalize_goal(&goal);
        if self.active.contains(&canonical_goal) {
            return SelectionResult::NoSolution;
        }
        if let Some(result) = self.cache.get(&canonical_goal).cloned() {
            return self.materialize_cached(typer, &goal, result, depth);
        }
        self.active.insert(canonical_goal.clone());

        let candidates = self
            .index
            .candidates(&goal.trait_ref, &goal.for_ty)
            .into_iter()
            .cloned()
            .collect::<Vec<_>>();
        let mut successful = Vec::new();
        let mut ambiguous = Vec::new();
        let mut overflow = false;
        for candidate in candidates {
            let snapshot = typer.snapshot_inference();
            let result = self.confirm_candidate(typer, &goal, &candidate, depth + 1);
            typer.rollback_inference(snapshot);
            match result {
                CandidateResult::Success(_) => successful.push(candidate.id),
                CandidateResult::Ambiguous => ambiguous.push(candidate.id),
                CandidateResult::Overflow => overflow = true,
                CandidateResult::Failure => {}
            }
        }

        self.active.remove(&canonical_goal);
        let result = if overflow {
            CachedSelectionResult::Overflow
        } else if successful.len() == 1 && ambiguous.is_empty() {
            CachedSelectionResult::Unique(successful.remove(0))
        } else if successful.is_empty() && ambiguous.is_empty() {
            CachedSelectionResult::NoSolution
        } else {
            ambiguous.extend(successful);
            ambiguous.sort();
            ambiguous.dedup();
            CachedSelectionResult::Ambiguous(ambiguous)
        };
        self.cache.insert(canonical_goal, result.clone());
        self.materialize_cached(typer, &goal, result, depth)
    }

    fn materialize_cached(
        &mut self,
        typer: &mut Typer,
        goal: &TraitGoal,
        result: CachedSelectionResult,
        depth: usize,
    ) -> SelectionResult {
        let CachedSelectionResult::Unique(id) = result else {
            return match result {
                CachedSelectionResult::NoSolution => SelectionResult::NoSolution,
                CachedSelectionResult::Ambiguous(ids) => SelectionResult::Ambiguous(ids),
                CachedSelectionResult::Overflow => SelectionResult::Overflow,
                CachedSelectionResult::Unique(_) => unreachable!(),
            };
        };
        let Some(candidate) = self.index.candidate(&id).cloned() else {
            return SelectionResult::NoSolution;
        };
        let changed_variables = type_vars(&goal.for_ty)
            .into_iter()
            .chain(trait_ref_type_vars(&goal.trait_ref))
            .collect();
        let snapshot = typer.snapshot_inference();
        match self.confirm_candidate(typer, goal, &candidate, depth + 1) {
            CandidateResult::Success(substitution) => {
                typer.commit_inference(snapshot);
                SelectionResult::Unique(Selection {
                    source: SelectionSource::Impl {
                        id,
                        definition: Box::new(candidate.definition),
                        substitution,
                    },
                    changed_variables,
                })
            }
            CandidateResult::Failure => {
                typer.rollback_inference(snapshot);
                SelectionResult::NoSolution
            }
            CandidateResult::Ambiguous => {
                typer.rollback_inference(snapshot);
                SelectionResult::Ambiguous(vec![id])
            }
            CandidateResult::Overflow => {
                typer.rollback_inference(snapshot);
                SelectionResult::Overflow
            }
        }
    }

    fn confirm_candidate(
        &mut self,
        typer: &mut Typer,
        goal: &TraitGoal,
        candidate: &ImplCandidate,
        depth: usize,
    ) -> CandidateResult {
        if !candidate.definition.valid {
            return CandidateResult::Failure;
        }
        if candidate.builtin && self.env.shadows_builtin_nominal_type(&candidate.head) {
            return CandidateResult::Failure;
        }
        let substitution = candidate
            .definition
            .params
            .iter()
            .map(|param| (param.0.clone(), typer.fresh_ty_var()))
            .collect::<HashMap<_, _>>();
        let candidate_trait_ref = substitute_trait_ref(&candidate.trait_ref, &substitution);
        let head = substitute_ty_params(&candidate.head, &substitution);
        if candidate_trait_ref.name != goal.trait_ref.name
            || candidate_trait_ref.args.len() != goal.trait_ref.args.len()
        {
            return CandidateResult::Failure;
        }
        let head_shape = projection_match_shape(typer, &head);
        if !typer.try_unify_silent(&head_shape, &goal.for_ty) {
            return CandidateResult::Failure;
        }
        for (candidate, goal) in candidate_trait_ref.args.iter().zip(&goal.trait_ref.args) {
            let candidate_shape = projection_match_shape(typer, candidate);
            if !typer.try_unify_silent(&candidate_shape, goal) {
                return CandidateResult::Failure;
            }
        }
        let head = match self.normalize_candidate_ty(typer, &head, depth) {
            Ok(ty) => ty,
            Err(NormalizationFailure::Failure) => return CandidateResult::Failure,
            Err(NormalizationFailure::Ambiguous) => return CandidateResult::Ambiguous,
            Err(NormalizationFailure::Overflow) => return CandidateResult::Overflow,
        };
        let goal_head = match self.normalize_candidate_ty(typer, &goal.for_ty, depth) {
            Ok(ty) => ty,
            Err(NormalizationFailure::Failure) => return CandidateResult::Failure,
            Err(NormalizationFailure::Ambiguous) => return CandidateResult::Ambiguous,
            Err(NormalizationFailure::Overflow) => return CandidateResult::Overflow,
        };
        if !typer.try_unify_silent(&head, &goal_head) {
            return CandidateResult::Failure;
        }
        for (candidate, goal) in candidate_trait_ref.args.iter().zip(&goal.trait_ref.args) {
            let candidate = match self.normalize_candidate_ty(typer, candidate, depth) {
                Ok(ty) => ty,
                Err(NormalizationFailure::Failure) => return CandidateResult::Failure,
                Err(NormalizationFailure::Ambiguous) => return CandidateResult::Ambiguous,
                Err(NormalizationFailure::Overflow) => return CandidateResult::Overflow,
            };
            let goal = match self.normalize_candidate_ty(typer, goal, depth) {
                Ok(ty) => ty,
                Err(NormalizationFailure::Failure) => return CandidateResult::Failure,
                Err(NormalizationFailure::Ambiguous) => return CandidateResult::Ambiguous,
                Err(NormalizationFailure::Overflow) => return CandidateResult::Overflow,
            };
            if !typer.try_unify_silent(&candidate, &goal) {
                return CandidateResult::Failure;
            }
        }
        for predicate in &candidate.definition.constraints {
            match substitute_predicate(predicate, &substitution) {
                env::TypePredicate::Trait { for_ty, trait_ref } => {
                    let nested = TraitGoal {
                        trait_ref,
                        for_ty: typer.norm(&for_ty),
                    };
                    match self.select_at_depth(typer, nested.clone(), depth) {
                        SelectionResult::Unique(_) => {}
                        SelectionResult::NoSolution
                            if contains_tvar(&typer.norm(&nested.for_ty))
                                || nested
                                    .trait_ref
                                    .args
                                    .iter()
                                    .any(|arg| contains_tvar(&typer.norm(arg))) =>
                        {
                            return CandidateResult::Ambiguous;
                        }
                        SelectionResult::NoSolution => return CandidateResult::Failure,
                        SelectionResult::Ambiguous(_) => return CandidateResult::Ambiguous,
                        SelectionResult::Overflow => return CandidateResult::Overflow,
                    }
                }
                env::TypePredicate::Equality { lhs, rhs } => {
                    if typer.norm(&lhs) == typer.norm(&rhs) {
                        continue;
                    }
                    let lhs = match self.normalize_candidate_ty(typer, &lhs, depth) {
                        Ok(ty) => ty,
                        Err(NormalizationFailure::Failure) => return CandidateResult::Failure,
                        Err(NormalizationFailure::Ambiguous) => {
                            return CandidateResult::Ambiguous;
                        }
                        Err(NormalizationFailure::Overflow) => return CandidateResult::Overflow,
                    };
                    let rhs = match self.normalize_candidate_ty(typer, &rhs, depth) {
                        Ok(ty) => ty,
                        Err(NormalizationFailure::Failure) => return CandidateResult::Failure,
                        Err(NormalizationFailure::Ambiguous) => {
                            return CandidateResult::Ambiguous;
                        }
                        Err(NormalizationFailure::Overflow) => return CandidateResult::Overflow,
                    };
                    if !typer.try_unify_silent(&lhs, &rhs) {
                        return CandidateResult::Failure;
                    }
                }
            }
        }
        CandidateResult::Success(
            substitution
                .into_iter()
                .map(|(name, ty)| (name, typer.norm(&ty)))
                .collect(),
        )
    }

    fn normalize_candidate_ty(
        &mut self,
        typer: &mut Typer,
        ty: &tast::Ty,
        depth: usize,
    ) -> Result<tast::Ty, NormalizationFailure> {
        let mut current = typer.norm(ty);
        for offset in 0..MAX_GOAL_DEPTH.saturating_sub(depth) {
            let mut failure = None;
            let next = rewrite_ty(&current, &mut |ty| {
                let tast::Ty::TProjection {
                    trait_ref: Some(trait_ref),
                    for_ty,
                    name,
                } = ty
                else {
                    return None;
                };
                let trait_ref = tast::TraitRef {
                    name: trait_ref.name.clone(),
                    args: trait_ref.args.iter().map(|arg| typer.norm(arg)).collect(),
                };
                let for_ty = typer.norm(for_ty);
                if contains_tvar(&for_ty) || trait_ref.args.iter().any(contains_tvar) {
                    failure = Some(NormalizationFailure::Ambiguous);
                    return None;
                }
                match self.select_at_depth(
                    typer,
                    TraitGoal {
                        trait_ref: trait_ref.clone(),
                        for_ty: for_ty.clone(),
                    },
                    depth + offset + 1,
                ) {
                    SelectionResult::Unique(selection) => match selection.source {
                        SelectionSource::Impl {
                            definition,
                            substitution,
                            ..
                        } => definition
                            .associated_types
                            .get(&name.0)
                            .map(|binding| substitute_ty_params(binding, &substitution))
                            .or_else(|| {
                                failure = Some(NormalizationFailure::Failure);
                                None
                            }),
                        SelectionSource::ParamEnv => {
                            let projection = tast::Ty::TProjection {
                                trait_ref: Some(trait_ref.clone()),
                                for_ty: Box::new(for_ty.clone()),
                                name: name.clone(),
                            };
                            let normalized = typer.norm(&projection);
                            (normalized != projection).then_some(normalized)
                        }
                        SelectionSource::Dyn => {
                            failure = Some(NormalizationFailure::Failure);
                            None
                        }
                        SelectionSource::BuiltinEq => {
                            failure = Some(NormalizationFailure::Failure);
                            None
                        }
                    },
                    SelectionResult::NoSolution => {
                        failure = Some(NormalizationFailure::Failure);
                        None
                    }
                    SelectionResult::Ambiguous(_) => {
                        failure = Some(NormalizationFailure::Ambiguous);
                        None
                    }
                    SelectionResult::Overflow => {
                        failure = Some(NormalizationFailure::Overflow);
                        None
                    }
                }
            });
            if let Some(failure) = failure {
                return Err(failure);
            }
            let next = typer.norm(&next);
            if next == current {
                return Ok(next);
            }
            current = next;
        }
        Err(NormalizationFailure::Overflow)
    }

    fn select_ground_at_depth(&mut self, goal: TraitGoal, depth: usize) -> SelectionResult {
        if depth >= MAX_GOAL_DEPTH {
            return SelectionResult::Overflow;
        }
        if self.param_env.predicates().iter().any(|predicate| {
            matches!(
                predicate,
                env::TypePredicate::Trait { for_ty, trait_ref }
                    if for_ty == &goal.for_ty && trait_ref == &goal.trait_ref
            )
        }) {
            return SelectionResult::Unique(Selection {
                source: SelectionSource::ParamEnv,
                changed_variables: HashSet::new(),
            });
        }
        if matches!(
            &goal.for_ty,
            tast::Ty::TDyn { trait_name }
                if goal.trait_ref.args.is_empty() && trait_name == &goal.trait_ref.name.0
        ) {
            return SelectionResult::Unique(Selection {
                source: SelectionSource::Dyn,
                changed_variables: HashSet::new(),
            });
        }
        if let Some(result) = self.select_ground_builtin_eq(&goal, depth) {
            return result;
        }
        if let Some(result) = self.ground_cache.get(&goal) {
            return result.clone();
        }
        if !self.ground_active.insert(goal.clone()) {
            return SelectionResult::NoSolution;
        }

        let candidates = self
            .index
            .candidates(&goal.trait_ref, &goal.for_ty)
            .into_iter()
            .cloned()
            .collect::<Vec<_>>();
        let mut successful = Vec::new();
        let mut ambiguous = Vec::new();
        let mut overflow = false;
        for candidate in candidates {
            match self.confirm_ground_candidate(&goal, &candidate, depth + 1) {
                CandidateResult::Success(substitution) => {
                    successful.push((candidate, substitution));
                }
                CandidateResult::Ambiguous => ambiguous.push(candidate.id),
                CandidateResult::Overflow => overflow = true,
                CandidateResult::Failure => {}
            }
        }

        self.ground_active.remove(&goal);
        let result = if overflow {
            SelectionResult::Overflow
        } else if successful.len() == 1 && ambiguous.is_empty() {
            let (candidate, substitution) = successful.remove(0);
            SelectionResult::Unique(Selection {
                source: SelectionSource::Impl {
                    id: candidate.id,
                    definition: Box::new(candidate.definition),
                    substitution,
                },
                changed_variables: HashSet::new(),
            })
        } else if successful.is_empty() && ambiguous.is_empty() {
            SelectionResult::NoSolution
        } else {
            ambiguous.extend(successful.into_iter().map(|(candidate, _)| candidate.id));
            ambiguous.sort();
            ambiguous.dedup();
            SelectionResult::Ambiguous(ambiguous)
        };
        self.ground_cache.insert(goal, result.clone());
        result
    }

    fn confirm_ground_candidate(
        &mut self,
        goal: &TraitGoal,
        candidate: &ImplCandidate,
        depth: usize,
    ) -> CandidateResult {
        if !candidate.definition.valid {
            return CandidateResult::Failure;
        }
        if candidate.builtin && self.env.shadows_builtin_nominal_type(&candidate.head) {
            return CandidateResult::Failure;
        }
        let Some(substitution) = trait_impl_subst(
            &candidate.trait_ref,
            &candidate.head,
            &goal.trait_ref,
            &goal.for_ty,
        ) else {
            return CandidateResult::Failure;
        };
        if candidate
            .definition
            .params
            .iter()
            .any(|param| !substitution.contains_key(&param.0))
        {
            return CandidateResult::Failure;
        }
        let head = substitute_ty_params(&candidate.head, &substitution);
        match self.ground_candidate_types_match(&head, &goal.for_ty, depth) {
            Ok(true) => {}
            Ok(false) | Err(NormalizationFailure::Failure) => return CandidateResult::Failure,
            Err(NormalizationFailure::Ambiguous) => return CandidateResult::Ambiguous,
            Err(NormalizationFailure::Overflow) => return CandidateResult::Overflow,
        }
        let candidate_trait_ref = substitute_trait_ref(&candidate.trait_ref, &substitution);
        for (candidate, goal) in candidate_trait_ref.args.iter().zip(&goal.trait_ref.args) {
            match self.ground_candidate_types_match(candidate, goal, depth) {
                Ok(true) => {}
                Ok(false) | Err(NormalizationFailure::Failure) => {
                    return CandidateResult::Failure;
                }
                Err(NormalizationFailure::Ambiguous) => return CandidateResult::Ambiguous,
                Err(NormalizationFailure::Overflow) => return CandidateResult::Overflow,
            }
        }
        for predicate in &candidate.definition.constraints {
            match substitute_predicate(predicate, &substitution) {
                env::TypePredicate::Trait { for_ty, trait_ref } => {
                    let nested = TraitGoal { trait_ref, for_ty };
                    match self.select_ground_at_depth(nested, depth) {
                        SelectionResult::Unique(_) => {}
                        SelectionResult::NoSolution => return CandidateResult::Failure,
                        SelectionResult::Ambiguous(_) => return CandidateResult::Ambiguous,
                        SelectionResult::Overflow => return CandidateResult::Overflow,
                    }
                }
                env::TypePredicate::Equality { lhs, rhs } => {
                    if lhs == rhs {
                        continue;
                    }
                    let lhs = match self.normalize_ground_candidate_ty(&lhs, depth) {
                        Ok(ty) => ty,
                        Err(NormalizationFailure::Failure) => return CandidateResult::Failure,
                        Err(NormalizationFailure::Ambiguous) => {
                            return CandidateResult::Ambiguous;
                        }
                        Err(NormalizationFailure::Overflow) => return CandidateResult::Overflow,
                    };
                    let rhs = match self.normalize_ground_candidate_ty(&rhs, depth) {
                        Ok(ty) => ty,
                        Err(NormalizationFailure::Failure) => return CandidateResult::Failure,
                        Err(NormalizationFailure::Ambiguous) => {
                            return CandidateResult::Ambiguous;
                        }
                        Err(NormalizationFailure::Overflow) => return CandidateResult::Overflow,
                    };
                    if lhs != rhs {
                        return CandidateResult::Failure;
                    }
                }
            }
        }
        CandidateResult::Success(substitution)
    }

    fn ground_candidate_types_match(
        &mut self,
        candidate: &tast::Ty,
        goal: &tast::Ty,
        depth: usize,
    ) -> Result<bool, NormalizationFailure> {
        if candidate == goal {
            return Ok(true);
        }
        let candidate = self.normalize_ground_candidate_ty(candidate, depth)?;
        let goal = self.normalize_ground_candidate_ty(goal, depth)?;
        Ok(candidate == goal)
    }

    fn normalize_ground_candidate_ty(
        &mut self,
        ty: &tast::Ty,
        depth: usize,
    ) -> Result<tast::Ty, NormalizationFailure> {
        let mut current = ty.clone();
        for offset in 0..MAX_GOAL_DEPTH.saturating_sub(depth) {
            let mut failure = None;
            let next = rewrite_ty(&current, &mut |ty| {
                let tast::Ty::TProjection {
                    trait_ref: Some(trait_ref),
                    for_ty,
                    name,
                } = ty
                else {
                    return None;
                };
                match self.select_ground_at_depth(
                    TraitGoal {
                        trait_ref: trait_ref.clone(),
                        for_ty: for_ty.as_ref().clone(),
                    },
                    depth + offset + 1,
                ) {
                    SelectionResult::Unique(selection) => match selection.source {
                        SelectionSource::Impl {
                            definition,
                            substitution,
                            ..
                        } => definition
                            .associated_types
                            .get(&name.0)
                            .map(|binding| substitute_ty_params(binding, &substitution))
                            .or_else(|| {
                                failure = Some(NormalizationFailure::Failure);
                                None
                            }),
                        SelectionSource::ParamEnv => {
                            let projection = tast::Ty::TProjection {
                                trait_ref: Some(trait_ref.clone()),
                                for_ty: for_ty.clone(),
                                name: name.clone(),
                            };
                            let normalized = self.param_env_equality_normal_form(&projection);
                            (normalized != projection).then_some(normalized)
                        }
                        SelectionSource::Dyn => {
                            failure = Some(NormalizationFailure::Failure);
                            None
                        }
                        SelectionSource::BuiltinEq => {
                            failure = Some(NormalizationFailure::Failure);
                            None
                        }
                    },
                    SelectionResult::NoSolution => {
                        failure = Some(NormalizationFailure::Failure);
                        None
                    }
                    SelectionResult::Ambiguous(_) => {
                        failure = Some(NormalizationFailure::Ambiguous);
                        None
                    }
                    SelectionResult::Overflow => {
                        failure = Some(NormalizationFailure::Overflow);
                        None
                    }
                }
            });
            if let Some(failure) = failure {
                return Err(failure);
            }
            if next == current {
                return Ok(next);
            }
            current = next;
        }
        Err(NormalizationFailure::Overflow)
    }

    fn param_env_equality_normal_form(&self, ty: &tast::Ty) -> tast::Ty {
        let mut pending = vec![ty.clone()];
        let mut seen = HashSet::new();
        while let Some(current) = pending.pop() {
            if !seen.insert(current.clone()) {
                continue;
            }
            for predicate in self.param_env.predicates() {
                let env::TypePredicate::Equality { lhs, rhs } = predicate else {
                    continue;
                };
                if lhs == &current && !seen.contains(rhs) {
                    pending.push(rhs.clone());
                }
                if rhs == &current && !seen.contains(lhs) {
                    pending.push(lhs.clone());
                }
            }
        }
        seen.into_iter()
            .min_by_key(|candidate| {
                (
                    matches!(candidate, tast::Ty::TProjection { .. }),
                    format!("{candidate:?}"),
                )
            })
            .unwrap_or_else(|| ty.clone())
    }
}

fn projection_match_shape(typer: &mut Typer, ty: &tast::Ty) -> tast::Ty {
    rewrite_ty(ty, &mut |ty| {
        matches!(ty, tast::Ty::TProjection { .. }).then(|| typer.fresh_ty_var())
    })
}

#[derive(Debug, Clone, Copy)]
enum NormalizationFailure {
    Failure,
    Ambiguous,
    Overflow,
}

fn canonicalize_goal(goal: &TraitGoal) -> TraitGoal {
    let mut variables = HashMap::new();
    let mut next = 0;
    TraitGoal {
        trait_ref: tast::TraitRef {
            name: goal.trait_ref.name.clone(),
            args: goal
                .trait_ref
                .args
                .iter()
                .map(|arg| canonicalize_ty(arg, &mut variables, &mut next))
                .collect(),
        },
        for_ty: canonicalize_ty(&goal.for_ty, &mut variables, &mut next),
    }
}

fn canonicalize_ty(
    ty: &tast::Ty,
    variables: &mut HashMap<tast::TypeVar, tast::TypeVar>,
    next: &mut u32,
) -> tast::Ty {
    match ty {
        tast::Ty::TVar(variable) => {
            let variable = *variables.entry(*variable).or_insert_with(|| {
                let canonical = <tast::TypeVar as UnifyKey>::from_index(*next);
                *next += 1;
                canonical
            });
            tast::Ty::TVar(variable)
        }
        tast::Ty::TUnit => tast::Ty::TUnit,
        tast::Ty::TNever => tast::Ty::TNever,
        tast::Ty::TBool => tast::Ty::TBool,
        tast::Ty::TInt => tast::Ty::TInt,
        tast::Ty::TInt8 => tast::Ty::TInt8,
        tast::Ty::TInt16 => tast::Ty::TInt16,
        tast::Ty::TInt32 => tast::Ty::TInt32,
        tast::Ty::TInt64 => tast::Ty::TInt64,
        tast::Ty::TUint8 => tast::Ty::TUint8,
        tast::Ty::TUint16 => tast::Ty::TUint16,
        tast::Ty::TUint32 => tast::Ty::TUint32,
        tast::Ty::TUint64 => tast::Ty::TUint64,
        tast::Ty::TFloat32 => tast::Ty::TFloat32,
        tast::Ty::TFloat64 => tast::Ty::TFloat64,
        tast::Ty::TString => tast::Ty::TString,
        tast::Ty::TChar => tast::Ty::TChar,
        tast::Ty::TTuple { typs } => tast::Ty::TTuple {
            typs: typs
                .iter()
                .map(|ty| canonicalize_ty(ty, variables, next))
                .collect(),
        },
        tast::Ty::TEnum { name } => tast::Ty::TEnum { name: name.clone() },
        tast::Ty::TStruct { name } => tast::Ty::TStruct { name: name.clone() },
        tast::Ty::TDyn { trait_name } => tast::Ty::TDyn {
            trait_name: trait_name.clone(),
        },
        tast::Ty::TProjection {
            trait_ref,
            for_ty,
            name,
        } => tast::Ty::TProjection {
            trait_ref: trait_ref.as_ref().map(|trait_ref| tast::TraitRef {
                name: trait_ref.name.clone(),
                args: trait_ref
                    .args
                    .iter()
                    .map(|ty| canonicalize_ty(ty, variables, next))
                    .collect(),
            }),
            for_ty: Box::new(canonicalize_ty(for_ty, variables, next)),
            name: name.clone(),
        },
        tast::Ty::TApp { ty, args } => tast::Ty::TApp {
            ty: Box::new(canonicalize_ty(ty, variables, next)),
            args: args
                .iter()
                .map(|ty| canonicalize_ty(ty, variables, next))
                .collect(),
        },
        tast::Ty::TArray { len, elem } => tast::Ty::TArray {
            len: *len,
            elem: Box::new(canonicalize_ty(elem, variables, next)),
        },
        tast::Ty::TSlice { elem } => tast::Ty::TSlice {
            elem: Box::new(canonicalize_ty(elem, variables, next)),
        },
        tast::Ty::TVec { elem } => tast::Ty::TVec {
            elem: Box::new(canonicalize_ty(elem, variables, next)),
        },
        tast::Ty::TRef { elem } => tast::Ty::TRef {
            elem: Box::new(canonicalize_ty(elem, variables, next)),
        },
        tast::Ty::TChannel { elem } => tast::Ty::TChannel {
            elem: Box::new(canonicalize_ty(elem, variables, next)),
        },
        tast::Ty::THashMap { key, value } => tast::Ty::THashMap {
            key: Box::new(canonicalize_ty(key, variables, next)),
            value: Box::new(canonicalize_ty(value, variables, next)),
        },
        tast::Ty::TFunc { params, ret_ty } => tast::Ty::TFunc {
            params: params
                .iter()
                .map(|ty| canonicalize_ty(ty, variables, next))
                .collect(),
            ret_ty: Box::new(canonicalize_ty(ret_ty, variables, next)),
        },
        tast::Ty::TParam { name } => tast::Ty::TParam { name: name.clone() },
    }
}

enum CandidateResult {
    Success(HashMap<String, tast::Ty>),
    Failure,
    Ambiguous,
    Overflow,
}
