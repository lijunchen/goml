use ast::ast;
use ena::unify::InPlaceUnificationTable;

use crate::tast::{self, TypeVar};

#[derive(Clone)]
pub(crate) struct VarInfo {
    pub ty: tast::Ty,
    pub derived_from_ref: bool,
}

pub(crate) type VarMap = im::HashMap<ast::Lident, VarInfo>;

mod check;
mod toplevel;
mod unify;
mod util;

pub use toplevel::check_file;

#[derive(Default)]
pub struct TypeInference {
    pub uni: InPlaceUnificationTable<TypeVar>,
    tparams_env_stack: Vec<Vec<ast::Uident>>,
}

impl TypeInference {
    pub fn new() -> Self {
        Self {
            uni: InPlaceUnificationTable::new(),
            tparams_env_stack: Vec::new(),
        }
    }

    fn push_tparams_env(&mut self, tparams: &[ast::Uident]) {
        self.tparams_env_stack.push(tparams.to_vec());
    }

    fn pop_tparams_env(&mut self) {
        self.tparams_env_stack.pop();
    }

    fn current_tparams_env(&self) -> Vec<ast::Uident> {
        self.tparams_env_stack
            .iter()
            .flat_map(|env| env.iter().cloned())
            .collect()
    }

    fn with_tparams_env<F, R>(&mut self, tparams: &[ast::Uident], f: F) -> R
    where
        F: FnOnce(&mut TypeInference) -> R,
    {
        self.push_tparams_env(tparams);
        let result = f(self);
        self.pop_tparams_env();
        result
    }
}
