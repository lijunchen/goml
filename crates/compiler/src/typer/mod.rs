use ena::unify::InPlaceUnificationTable;
use std::collections::{HashMap, HashSet};

use crate::tast;
use crate::tast::TypeVar;
use crate::typer::results::TypeckResultsBuilder;

mod check;
mod literals;
mod localenv;
mod member_lookup;
pub mod name_resolution;
mod obligations;
mod operators;
pub mod results;
mod solve;
pub mod tast_builder;
mod toplevel;
mod traits;
mod type_ops;
mod unify;
mod util;

pub(crate) use obligations::{
    ArithmeticKind, CoercionGoal, MethodGoal, Obligation, ObligationCause, ObligationCauseKind,
    ObligationId, OperationGoal, Predicate, ProjectionGoal, TraitGoal,
};

pub use toplevel::check_file_with_env_and_results;
pub use toplevel::{check_file, check_file_with_env, check_file_with_env_capability};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LoopControlContext {
    Disallowed,
    WhileCondition,
    Allowed,
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
    pub(crate) tparam_trait_bounds: HashMap<String, Vec<String>>,
    pub(crate) array_wildcard_counter: usize,
    pub(crate) array_wildcard_resolutions: HashMap<usize, usize>,
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
            array_wildcard_counter: 0,
            array_wildcard_resolutions: HashMap::new(),
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
