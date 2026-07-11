use std::collections::{HashMap, HashSet};

use crate::{
    env, tast,
    typer::{
        obligations::{ParamEnv, TraitGoal},
        traits::index::{ImplCandidate, ImplId, ImplIndex},
        traits::matching::trait_impl_subst,
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
}

#[derive(Debug, Clone)]
pub(crate) struct Selection {
    pub source: SelectionSource,
}

#[derive(Debug, Clone)]
pub(crate) enum SelectionResult {
    Unique(Selection),
    NoSolution,
    Ambiguous(Vec<ImplId>),
    Overflow,
}

pub(crate) struct TraitSolver<'a> {
    env: &'a env::PackageTypeEnv,
    param_env: &'a ParamEnv,
    index: ImplIndex,
    cache: HashMap<TraitGoal, SelectionResult>,
    active: HashSet<TraitGoal>,
}

impl<'a> TraitSolver<'a> {
    pub(crate) fn new(env: &'a env::PackageTypeEnv, param_env: &'a ParamEnv) -> Self {
        Self {
            env,
            param_env,
            index: ImplIndex::build(env),
            cache: HashMap::new(),
            active: HashSet::new(),
        }
    }

    pub(crate) fn select(&mut self, goal: TraitGoal) -> SelectionResult {
        self.select_at_depth(goal, 0)
    }

    pub(crate) fn describe_candidate(&self, id: &ImplId) -> String {
        self.index.describe_candidate(id)
    }

    fn select_at_depth(&mut self, goal: TraitGoal, depth: usize) -> SelectionResult {
        if depth >= MAX_GOAL_DEPTH {
            return SelectionResult::Overflow;
        }
        if self.param_env.proves(&goal) {
            return SelectionResult::Unique(Selection {
                source: SelectionSource::ParamEnv,
            });
        }
        if matches!(
            &goal.for_ty,
            tast::Ty::TDyn { trait_name } if trait_name == &goal.trait_name.0
        ) {
            return SelectionResult::Unique(Selection {
                source: SelectionSource::Dyn,
            });
        }
        if let Some(result) = self.cache.get(&goal) {
            return result.clone();
        }
        if !self.active.insert(goal.clone()) {
            return SelectionResult::NoSolution;
        }

        let candidates = self
            .index
            .candidates(&goal.trait_name.0, &goal.for_ty)
            .into_iter()
            .cloned()
            .collect::<Vec<_>>();
        let mut successful = Vec::new();
        let mut ambiguous = Vec::new();
        let mut overflow = false;
        for candidate in candidates {
            match self.confirm_candidate(&goal, &candidate, depth + 1) {
                CandidateResult::Success(substitution) => {
                    successful.push((candidate, substitution))
                }
                CandidateResult::Ambiguous => ambiguous.push(candidate.id),
                CandidateResult::Overflow => overflow = true,
                CandidateResult::Failure => {}
            }
        }

        self.active.remove(&goal);
        let result = if successful.len() == 1 && ambiguous.is_empty() {
            let (candidate, substitution) = successful.remove(0);
            SelectionResult::Unique(Selection {
                source: SelectionSource::Impl {
                    id: candidate.id,
                    definition: Box::new(candidate.definition),
                    substitution,
                },
            })
        } else if successful.is_empty() && ambiguous.is_empty() {
            if overflow {
                SelectionResult::Overflow
            } else {
                SelectionResult::NoSolution
            }
        } else {
            ambiguous.extend(successful.into_iter().map(|(candidate, _)| candidate.id));
            SelectionResult::Ambiguous(ambiguous)
        };
        self.cache.insert(goal, result.clone());
        result
    }

    fn confirm_candidate(
        &mut self,
        goal: &TraitGoal,
        candidate: &ImplCandidate,
        depth: usize,
    ) -> CandidateResult {
        if !candidate.definition.valid {
            return CandidateResult::Failure;
        }
        if candidate.builtin && self.env.shadows_builtin_nominal_type(&goal.for_ty) {
            return CandidateResult::Failure;
        }
        let Some(substitution) = trait_impl_subst(&candidate.head, &goal.for_ty) else {
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
        for constraint in &candidate.definition.constraints {
            let Some(for_ty) = substitution.get(&constraint.type_param) else {
                return CandidateResult::Failure;
            };
            let nested = TraitGoal {
                trait_name: constraint.trait_name.clone(),
                for_ty: for_ty.clone(),
            };
            match self.select_at_depth(nested, depth) {
                SelectionResult::Unique(_) => {}
                SelectionResult::NoSolution => return CandidateResult::Failure,
                SelectionResult::Ambiguous(_) => return CandidateResult::Ambiguous,
                SelectionResult::Overflow => return CandidateResult::Overflow,
            }
        }
        CandidateResult::Success(substitution)
    }
}

enum CandidateResult {
    Success(HashMap<String, tast::Ty>),
    Failure,
    Ambiguous,
    Overflow,
}
