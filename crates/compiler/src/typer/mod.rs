use ena::unify::InPlaceUnificationTable;

use crate::{env::Constraint, tast::TypeVar};
use crate::typer::results::TypeckResultsBuilder;

mod check;
mod localenv;
pub mod name_resolution;
pub mod results;
pub mod tast_builder;
mod toplevel;
mod unify;
mod util;

pub use toplevel::{check_file, check_file_with_env};

pub struct Typer {
    pub uni: InPlaceUnificationTable<TypeVar>,
    pub(crate) constraints: Vec<Constraint>,
    pub hir_table: name_resolution::HirTable,
    pub results: TypeckResultsBuilder,
}

impl Typer {
    pub fn new(hir_table: name_resolution::HirTable) -> Self {
        let results = TypeckResultsBuilder::new(&hir_table);
        Self {
            uni: InPlaceUnificationTable::new(),
            constraints: Vec::new(),
            hir_table,
            results,
        }
    }

    pub(crate) fn push_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }
}
