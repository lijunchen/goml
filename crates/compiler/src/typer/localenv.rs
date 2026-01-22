use im::HashMap as ImHashMap;
use indexmap::IndexMap;

use parser::Diagnostics;

use crate::hir::LocalId;
use crate::tast::{self, TastIdent};
use crate::typer::util::push_ice;

#[derive(Debug, Clone)]
pub struct LocalTypeEnv {
    scopes: Vec<ImHashMap<LocalId, tast::Ty>>,
    tparams_env: Vec<TastIdent>,
    tparam_trait_bounds: IndexMap<String, Vec<TastIdent>>,
    capture_stack: Vec<IndexMap<LocalId, tast::Ty>>,
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
            tparam_trait_bounds: IndexMap::new(),
            capture_stack: Vec::new(),
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(ImHashMap::new());
    }

    pub fn pop_scope(&mut self, diagnostics: &mut Diagnostics) {
        if self.scopes.len() <= 1 {
            push_ice(
                diagnostics,
                "attempted to pop base scope from type environment",
            );
            return;
        }
        self.scopes.pop();
    }

    pub fn insert_var(&mut self, name: LocalId, ty: tast::Ty) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, ty);
        }
    }

    pub fn lookup_var(&mut self, name: LocalId) -> Option<tast::Ty> {
        for (depth, scope) in self.scopes.iter().enumerate().rev() {
            if let Some(ty) = scope.get(&name) {
                if depth + 1 < self.scopes.len()
                    && let Some(captures) = self.capture_stack.last_mut()
                {
                    captures.entry(name).or_insert_with(|| ty.clone());
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

    pub fn set_tparam_trait_bounds(&mut self, bounds: IndexMap<String, Vec<TastIdent>>) {
        self.tparam_trait_bounds = bounds;
    }

    pub fn clear_tparam_trait_bounds(&mut self) {
        self.tparam_trait_bounds.clear();
    }

    pub fn tparam_trait_bounds(&self, name: &str) -> Option<&[TastIdent]> {
        self.tparam_trait_bounds.get(name).map(|v| v.as_slice())
    }

    pub fn begin_closure(&mut self) {
        self.capture_stack.push(IndexMap::new());
        self.push_scope();
    }

    pub fn end_closure(
        &mut self,
        diagnostics: &mut Diagnostics,
        hir_table: &crate::hir::HirTable,
    ) -> Vec<(String, tast::Ty)> {
        let captured = self
            .capture_stack
            .pop()
            .unwrap_or_default()
            .into_iter()
            .map(|(id, ty)| (hir_table.local_ident_name(id), ty))
            .collect();
        self.pop_scope(diagnostics);
        captured
    }

    pub fn capture_stack_depth(&self) -> usize {
        self.capture_stack.len()
    }
}
