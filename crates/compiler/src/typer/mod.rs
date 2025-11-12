use diagnostics::{Diagnostic, Diagnostics, Severity, Stage};
use ena::unify::InPlaceUnificationTable;

use crate::{env::Constraint, tast::TypeVar};

mod check;
mod toplevel;
mod unify;
mod util;

pub use toplevel::check_file;

pub struct TypeInference {
    pub uni: InPlaceUnificationTable<TypeVar>,
    pub(crate) constraints: Vec<Constraint>,
    diagnostics: Diagnostics,
}

impl Default for TypeInference {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeInference {
    pub fn new() -> Self {
        Self {
            uni: InPlaceUnificationTable::new(),
            constraints: Vec::new(),
            diagnostics: Diagnostics::new(),
        }
    }

    pub(crate) fn push_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }

    pub(crate) fn report_error(&mut self, message: impl Into<String>) {
        self.diagnostics
            .push(Diagnostic::new(Stage::Typer, Severity::Error, message));
    }

    pub(crate) fn into_diagnostics(self) -> Diagnostics {
        self.diagnostics
    }
}
