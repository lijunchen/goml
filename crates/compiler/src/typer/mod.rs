use ena::unify::InPlaceUnificationTable;
use std::collections::{HashMap, HashSet};

use crate::typer::results::TypeckResultsBuilder;
use crate::{env::Constraint, tast::TypeVar};
use crate::{hir, tast};

mod check;
mod localenv;
pub mod name_resolution;
pub mod results;
pub mod tast_builder;
mod toplevel;
mod unify;
mod util;

pub use toplevel::check_file_with_env_and_results;
pub use toplevel::{check_file, check_file_with_env};

#[derive(Debug, Clone)]
pub(crate) enum ArithmeticKind {
    NumericOrString,
    Numeric,
}

#[derive(Debug, Clone)]
pub(crate) struct DeferredArithmeticCheck {
    pub kind: ArithmeticKind,
    pub ty: tast::Ty,
    pub op: &'static str,
    pub origin: Option<text_size::TextRange>,
}

#[derive(Debug, Clone)]
pub(crate) struct DeferredDynCoercion {
    pub expr_id: hir::ExprId,
    pub concrete_ty: tast::Ty,
    pub expected_ty: tast::Ty,
}

pub struct Typer {
    pub uni: InPlaceUnificationTable<TypeVar>,
    pub(crate) constraints: Vec<Constraint>,
    pub(crate) reported_unresolved_type_vars: HashSet<TypeVar>,
    pub hir_table: name_resolution::HirTable,
    pub results: TypeckResultsBuilder,
    pub(crate) while_depth: u32,
    pub(crate) deferred_arithmetic_checks: Vec<DeferredArithmeticCheck>,
    pub(crate) tparam_trait_bounds: HashMap<String, Vec<String>>,
    pub(crate) deferred_dyn_coercions: Vec<DeferredDynCoercion>,
    pub(crate) array_wildcard_counter: usize,
    pub(crate) array_wildcard_resolutions: HashMap<usize, usize>,
}

impl Typer {
    pub fn new(hir_table: name_resolution::HirTable) -> Self {
        let results = TypeckResultsBuilder::new(&hir_table);
        Self {
            uni: InPlaceUnificationTable::new(),
            constraints: Vec::new(),
            reported_unresolved_type_vars: HashSet::new(),
            hir_table,
            results,
            while_depth: 0,
            deferred_arithmetic_checks: Vec::new(),
            tparam_trait_bounds: HashMap::new(),
            deferred_dyn_coercions: Vec::new(),
            array_wildcard_counter: 0,
            array_wildcard_resolutions: HashMap::new(),
        }
    }

    pub(crate) fn push_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
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
