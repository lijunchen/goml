use ena::unify::InPlaceUnificationTable;

use crate::tast::TypeVar;

mod check;
mod toplevel;
mod unify;
mod util;

pub use toplevel::check_file;

#[derive(Default)]
pub struct TypeInference {
    pub uni: InPlaceUnificationTable<TypeVar>,
}

impl TypeInference {
    pub fn new() -> Self {
        Self {
            uni: InPlaceUnificationTable::new(),
        }
    }
}
