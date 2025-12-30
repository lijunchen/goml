use im::HashMap as ImHashMap;
use indexmap::IndexMap;

use crate::tast::{self, TastIdent};

#[derive(Debug, Clone)]
pub struct LocalTypeEnv {
    scopes: Vec<ImHashMap<TastIdent, tast::Ty>>,
    tparams_env: Vec<TastIdent>,
    capture_stack: Vec<IndexMap<TastIdent, tast::Ty>>,
}

impl Default for LocalTypeEnv {
    fn default() -> Self {
        Self::new()
    }
}

impl LocalTypeEnv {
    pub fn new() -> Self {
        Self {
            scopes: vec![ImHashMap::new()],
            tparams_env: Vec::new(),
            capture_stack: Vec::new(),
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(ImHashMap::new());
    }

    pub fn pop_scope(&mut self) {
        if self.scopes.len() <= 1 {
            panic!("attempted to pop base scope from type environment");
        }
        self.scopes.pop();
    }

    pub fn insert_var(&mut self, name: &TastIdent, ty: tast::Ty) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.clone(), ty);
        }
    }

    pub fn lookup_var(&mut self, name: &TastIdent) -> Option<tast::Ty> {
        for (depth, scope) in self.scopes.iter().enumerate().rev() {
            if let Some(ty) = scope.get(name) {
                if depth + 1 < self.scopes.len()
                    && let Some(captures) = self.capture_stack.last_mut()
                {
                    captures.entry(name.clone()).or_insert_with(|| ty.clone());
                }
                return Some(ty.clone());
            }
        }
        None
    }

    pub fn set_tparams_env(&mut self, params: &[TastIdent]) {
        self.tparams_env = params.to_vec();
    }

    pub fn clear_tparams_env(&mut self) {
        self.tparams_env.clear();
    }

    pub fn current_tparams_env(&self) -> Vec<TastIdent> {
        self.tparams_env.clone()
    }

    pub fn begin_closure(&mut self) {
        self.capture_stack.push(IndexMap::new());
        self.push_scope();
    }

    pub fn end_closure(&mut self) -> Vec<(String, tast::Ty)> {
        let captured = self
            .capture_stack
            .pop()
            .unwrap_or_default()
            .into_iter()
            .map(|(name, ty)| (name.0, ty))
            .collect();
        self.pop_scope();
        captured
    }

    pub fn capture_stack_depth(&self) -> usize {
        self.capture_stack.len()
    }
}
