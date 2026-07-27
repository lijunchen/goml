use ena::unify::InPlaceUnificationTable;
use std::collections::{HashMap, HashSet};

use crate::tast;
use crate::tast::TypeVar;
use crate::typer::results::TypeckResultsBuilder;

mod check;
mod literals;
mod localenv;
mod match_analysis;
mod member_lookup;
pub(crate) use member_lookup::lookup_dyn_trait_methods;
pub mod name_resolution;
mod obligations;
mod operators;
pub mod results;
mod solve;
pub mod tast_builder;
mod toplevel;
pub(crate) mod traits;
pub(crate) mod type_ops;
mod unify;
mod util;

pub(crate) use obligations::{
    ArithmeticKind, CoercionGoal, MethodGoal, Obligation, ObligationCause, ObligationCauseKind,
    ObligationId, OperationGoal, ParamEnv, Predicate, ProjectionGoal, TraitGoal,
};
pub(crate) use traits::matching::impl_self_subst;
pub(crate) use util::format_ty_for_diag;

pub use toplevel::{check_file, check_file_with_env, check_file_with_env_capability};
pub use toplevel::{check_file_with_env_and_results, check_file_with_env_tast_and_results};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LoopControlContext {
    Disallowed,
    WhileCondition,
    Allowed,
}

#[derive(Debug, Clone)]
pub(crate) enum NumericLiteralKind {
    Integer(String),
    Float(f64),
}

#[derive(Debug, Clone)]
pub(crate) struct NumericLiteralConstraint {
    pub variable: TypeVar,
    pub kind: NumericLiteralKind,
    pub range: Option<text_size::TextRange>,
}

pub struct Typer {
    pub uni: InPlaceUnificationTable<TypeVar>,
    pub(crate) obligations: Vec<Obligation>,
    pub(crate) obligation_causes: HashMap<ObligationId, ObligationCause>,
    pub(crate) next_obligation_id: ObligationId,
    pub(crate) reported_unresolved_type_origins: HashSet<Option<text_size::TextSize>>,
    pub(crate) unresolved_type_var_origins: HashMap<TypeVar, Option<text_size::TextRange>>,
    pub hir_table: name_resolution::HirTable,
    pub results: TypeckResultsBuilder,
    pub(crate) loop_control_context: LoopControlContext,
    pub(crate) return_ty_stack: Vec<tast::Ty>,
    pub(crate) tparam_trait_bounds: HashMap<String, Vec<tast::TraitRef>>,
    pub(crate) param_env_predicates: Vec<crate::env::TypePredicate>,
    pub(crate) param_type_aliases: HashMap<String, tast::Ty>,
    pub(crate) param_projection_aliases: HashMap<tast::Ty, tast::Ty>,
    pub(crate) array_wildcard_counter: usize,
    pub(crate) array_wildcard_resolutions: HashMap<usize, usize>,
    pub(crate) numeric_literals: Vec<NumericLiteralConstraint>,
}

impl Typer {
    pub fn new(hir_table: name_resolution::HirTable) -> Self {
        let results = TypeckResultsBuilder::new(&hir_table);
        Self {
            uni: InPlaceUnificationTable::new(),
            obligations: Vec::new(),
            obligation_causes: HashMap::new(),
            next_obligation_id: 0,
            reported_unresolved_type_origins: HashSet::new(),
            unresolved_type_var_origins: HashMap::new(),
            hir_table,
            results,
            loop_control_context: LoopControlContext::Disallowed,
            return_ty_stack: Vec::new(),
            tparam_trait_bounds: HashMap::new(),
            param_env_predicates: Vec::new(),
            param_type_aliases: HashMap::new(),
            param_projection_aliases: HashMap::new(),
            array_wildcard_counter: 0,
            array_wildcard_resolutions: HashMap::new(),
            numeric_literals: Vec::new(),
        }
    }

    pub(crate) fn push_obligation(
        &mut self,
        predicate: Predicate,
        cause: ObligationCause,
    ) -> ObligationId {
        let obligation = self.new_obligation(predicate, cause);
        let id = obligation.id;
        self.obligations.push(obligation);
        id
    }

    pub(crate) fn new_obligation(
        &mut self,
        predicate: Predicate,
        cause: ObligationCause,
    ) -> Obligation {
        let id = self.reserve_obligation_cause(cause.clone());
        Obligation {
            id,
            predicate,
            cause,
        }
    }

    pub(crate) fn reserve_obligation_cause(&mut self, cause: ObligationCause) -> ObligationId {
        let id = self.next_obligation_id;
        self.next_obligation_id += 1;
        self.obligation_causes.insert(id, cause);
        id
    }

    pub(crate) fn push_reserved_obligation(&mut self, id: ObligationId, predicate: Predicate) {
        if let Some(cause) = self.obligation_causes.get(&id).cloned() {
            self.obligations.push(Obligation {
                id,
                predicate,
                cause,
            });
        }
    }

    pub(crate) fn fresh_array_wildcard(&mut self) -> usize {
        self.array_wildcard_counter += 1;
        usize::MAX - self.array_wildcard_counter
    }

    pub(crate) fn is_array_wildcard(&self, len: usize) -> bool {
        if len == tast::ARRAY_WILDCARD_LEN {
            return true;
        }
        if self.array_wildcard_counter == 0 {
            return false;
        }
        let min_wildcard = usize::MAX - self.array_wildcard_counter;
        len >= min_wildcard && len < usize::MAX
    }

    pub(crate) fn resolve_array_len(&self, len: usize) -> usize {
        if let Some(&target) = self.array_wildcard_resolutions.get(&len) {
            self.resolve_array_len(target)
        } else {
            len
        }
    }
}
