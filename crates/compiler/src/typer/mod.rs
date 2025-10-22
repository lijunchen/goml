use ast::ast;
use ena::unify::InPlaceUnificationTable;

use crate::tast::TypeVar;

mod check;
mod toplevel;
mod util;

pub use check::check_file;

#[derive(Default)]
pub struct TypeInference {
    pub uni: InPlaceUnificationTable<TypeVar>,
    tparams_env_stack: Vec<Vec<ast::Uident>>,
}
