use std::collections::{HashMap, HashSet};

use ena::unify::UnifyKey;

use crate::{
    env, tast,
    typer::{
        Typer,
        obligations::{ParamEnv, TraitGoal},
        traits::index::{ImplCandidate, ImplId, ImplIndex},
        traits::matching::trait_impl_subst,
        type_ops::{contains_tvar, substitute_ty_params, type_vars},
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
        if self.param_env.proves(&goal) {
            return SelectionResult::Unique(Selection {
                source: SelectionSource::ParamEnv,
                changed_variables: HashSet::new(),
            });
        }
        if matches!(
            &goal.for_ty,
            tast::Ty::TDyn { trait_name } if trait_name == &goal.trait_name.0
        ) {
            return SelectionResult::Unique(Selection {
                source: SelectionSource::Dyn,
                changed_variables: HashSet::new(),
            });
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
            .candidates(&goal.trait_name.0, &goal.for_ty)
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
        let changed_variables = type_vars(&goal.for_ty);
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
        if candidate.builtin && self.env.shadows_builtin_nominal_type(&goal.for_ty) {
            return CandidateResult::Failure;
        }
        let substitution = candidate
            .definition
            .params
            .iter()
            .map(|param| (param.0.clone(), typer.fresh_ty_var()))
            .collect::<HashMap<_, _>>();
        let head = substitute_ty_params(&candidate.head, &substitution);
        if !typer.try_unify_silent(&head, &goal.for_ty) {
            return CandidateResult::Failure;
        }
        for constraint in &candidate.definition.constraints {
            let Some(for_ty) = substitution.get(&constraint.type_param) else {
                return CandidateResult::Failure;
            };
            let nested = TraitGoal {
                trait_name: constraint.trait_name.clone(),
                for_ty: typer.norm(for_ty),
            };
            match self.select_at_depth(typer, nested.clone(), depth) {
                SelectionResult::Unique(_) => {}
                SelectionResult::NoSolution if contains_tvar(&typer.norm(&nested.for_ty)) => {
                    return CandidateResult::Ambiguous;
                }
                SelectionResult::NoSolution => return CandidateResult::Failure,
                SelectionResult::Ambiguous(_) => return CandidateResult::Ambiguous,
                SelectionResult::Overflow => return CandidateResult::Overflow,
            }
        }
        CandidateResult::Success(
            substitution
                .into_iter()
                .map(|(name, ty)| (name, typer.norm(&ty)))
                .collect(),
        )
    }

    fn select_ground_at_depth(&mut self, goal: TraitGoal, depth: usize) -> SelectionResult {
        if depth >= MAX_GOAL_DEPTH {
            return SelectionResult::Overflow;
        }
        if self.param_env.proves(&goal) {
            return SelectionResult::Unique(Selection {
                source: SelectionSource::ParamEnv,
                changed_variables: HashSet::new(),
            });
        }
        if matches!(
            &goal.for_ty,
            tast::Ty::TDyn { trait_name } if trait_name == &goal.trait_name.0
        ) {
            return SelectionResult::Unique(Selection {
                source: SelectionSource::Dyn,
                changed_variables: HashSet::new(),
            });
        }
        if let Some(result) = self.ground_cache.get(&goal) {
            return result.clone();
        }
        if !self.ground_active.insert(goal.clone()) {
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
            match self.select_ground_at_depth(nested, depth) {
                SelectionResult::Unique(_) => {}
                SelectionResult::NoSolution => return CandidateResult::Failure,
                SelectionResult::Ambiguous(_) => return CandidateResult::Ambiguous,
                SelectionResult::Overflow => return CandidateResult::Overflow,
            }
        }
        CandidateResult::Success(substitution)
    }
}

fn canonicalize_goal(goal: &TraitGoal) -> TraitGoal {
    let mut variables = HashMap::new();
    let mut next = 0;
    TraitGoal {
        trait_name: goal.trait_name.clone(),
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
        tast::Ty::TBool => tast::Ty::TBool,
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
