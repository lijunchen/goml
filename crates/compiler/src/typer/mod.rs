use ena::unify::InPlaceUnificationTable;

use crate::{env::Constraint, tast::TypeVar};

mod check;
mod rename;
mod toplevel;
mod unify;
mod util;

pub use toplevel::{check_file, check_file_with_env};

pub struct Typer {
    pub uni: InPlaceUnificationTable<TypeVar>,
    pub(crate) constraints: Vec<Constraint>,
}

impl Default for Typer {
    fn default() -> Self {
        Self::new()
    }
}

impl Typer {
    pub fn new() -> Self {
        Self {
            uni: InPlaceUnificationTable::new(),
            constraints: Vec::new(),
        }
    }

    pub(crate) fn push_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }
}
