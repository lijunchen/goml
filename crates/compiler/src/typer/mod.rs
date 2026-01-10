use ena::unify::InPlaceUnificationTable;

use crate::{env::Constraint, tast::TypeVar};

mod check;
mod localenv;
pub mod name_resolution;
mod toplevel;
mod unify;
mod util;

pub use toplevel::{check_file, check_file_with_env};

pub struct Typer {
    pub uni: InPlaceUnificationTable<TypeVar>,
    pub(crate) constraints: Vec<Constraint>,
    pub fir_table: name_resolution::FirTable,
}

impl Typer {
    pub fn new(fir_table: name_resolution::FirTable) -> Self {
        Self {
            uni: InPlaceUnificationTable::new(),
            constraints: Vec::new(),
            fir_table,
        }
    }

    pub(crate) fn push_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }
}
