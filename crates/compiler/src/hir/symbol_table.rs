use std::collections::HashMap;

use super::ir::{CtorId, FieldId, GenericParamId, ImplId, ItemId, LocalId, TraitId, TypeId};

#[derive(Debug, Default)]
pub struct SymbolTable {
    pub globals: GlobalTable,
    pub fields: FieldTables,
    pub locals: LocalScopeStack,
}

#[derive(Debug, Default)]
pub struct GlobalTable {
    pub value_items: HashMap<String, ItemId>,
    pub types: HashMap<String, TypeId>,
    pub traits: HashMap<String, TraitId>,
    pub ctors: HashMap<(TypeId, String), CtorId>,
    pub ctor_by_name: HashMap<String, Vec<CtorId>>,
    pub impls: Vec<ImplId>,
}

#[derive(Debug, Default)]
pub struct FieldTables {
    pub map: HashMap<TypeId, HashMap<String, FieldId>>,
}

#[derive(Debug, Default)]
pub struct LocalScopeStack {
    pub scopes: Vec<HashMap<String, LocalId>>,
    pub next_local_id: u32,
}

#[derive(Debug, Default, Clone)]
pub struct GenericTable {
    pub generics: HashMap<String, GenericParamId>,
}

impl LocalScopeStack {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn clear(&mut self) {
        self.scopes.clear();
        self.next_local_id = 0;
    }

    pub fn insert(&mut self, name: impl Into<String>) -> LocalId {
        let id = LocalId(self.next_local_id);
        self.next_local_id += 1;

        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.into(), id);
        } else {
            let mut scope = HashMap::new();
            scope.insert(name.into(), id);
            self.scopes.push(scope);
        }

        id
    }

    pub fn resolve(&self, name: &str) -> Option<LocalId> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.get(name) {
                return Some(*id);
            }
        }
        None
    }
}

impl GenericTable {
    pub fn new() -> Self {
        Self {
            generics: HashMap::new(),
        }
    }

    pub fn insert(&mut self, name: impl Into<String>, id: GenericParamId) {
        self.generics.insert(name.into(), id);
    }

    pub fn resolve(&self, name: &str) -> Option<GenericParamId> {
        self.generics.get(name).copied()
    }
}
