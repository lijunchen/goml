use std::collections::HashMap;

use crate::{env, tast};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum TypeHead {
    Unit,
    Bool,
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Float32,
    Float64,
    String,
    Char,
    Tuple(usize),
    Enum(String),
    Struct(String),
    Dyn(String),
    Array(usize),
    Slice,
    Vec,
    Ref,
    HashMap,
    Function(usize),
    TypeVar,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct ImplId {
    pub package: String,
    pub index: usize,
}

#[derive(Debug, Clone)]
pub(crate) struct ImplCandidate {
    pub id: ImplId,
    pub head: tast::Ty,
    pub definition: env::ImplDef,
    pub builtin: bool,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct ImplIndex {
    by_head: HashMap<(String, TypeHead), Vec<ImplCandidate>>,
    blanket: HashMap<String, Vec<ImplCandidate>>,
}

impl ImplIndex {
    pub(crate) fn build(env: &env::PackageTypeEnv) -> Self {
        let mut index = Self::default();
        index.add_env("builtin", &env.builtins, true);
        index.add_env(&env.package, &env.current, false);
        let mut packages = env.deps.keys().collect::<Vec<_>>();
        packages.sort();
        for package in packages {
            if let Some(package_env) = env.deps.get(package) {
                index.add_env(package, package_env, false);
            }
        }
        index
    }

    fn add_env(&mut self, package: &str, env: &env::GlobalTypeEnv, builtin: bool) {
        for (index, ((trait_name, head), definition)) in
            env.trait_env.trait_impls.iter().enumerate()
        {
            let candidate = ImplCandidate {
                id: ImplId {
                    package: package.to_string(),
                    index,
                },
                head: head.clone(),
                definition: definition.clone(),
                builtin,
            };
            if let Some(type_head) = type_head(head) {
                self.by_head
                    .entry((trait_name.clone(), type_head))
                    .or_default()
                    .push(candidate);
            } else {
                self.blanket
                    .entry(trait_name.clone())
                    .or_default()
                    .push(candidate);
            }
        }
    }

    pub(crate) fn candidates(&self, trait_name: &str, ty: &tast::Ty) -> Vec<&ImplCandidate> {
        let mut result = Vec::new();
        if let Some(head) = type_head(ty)
            && let Some(candidates) = self.by_head.get(&(trait_name.to_string(), head))
        {
            result.extend(candidates);
        }
        if let Some(candidates) = self.blanket.get(trait_name) {
            result.extend(candidates);
        }
        result
    }

    pub(crate) fn describe_candidate(&self, id: &ImplId) -> String {
        let candidate = self
            .by_head
            .values()
            .chain(self.blanket.values())
            .flatten()
            .find(|candidate| &candidate.id == id);
        match candidate.and_then(|candidate| candidate.definition.origin) {
            Some(origin) => format!(
                "{}#{} at {}..{}",
                id.package,
                id.index,
                u32::from(origin.start()),
                u32::from(origin.end())
            ),
            None => format!("{}#{}", id.package, id.index),
        }
    }
}

pub(crate) fn type_head(ty: &tast::Ty) -> Option<TypeHead> {
    match ty {
        tast::Ty::TVar(_) => Some(TypeHead::TypeVar),
        tast::Ty::TUnit => Some(TypeHead::Unit),
        tast::Ty::TBool => Some(TypeHead::Bool),
        tast::Ty::TInt8 => Some(TypeHead::Int8),
        tast::Ty::TInt16 => Some(TypeHead::Int16),
        tast::Ty::TInt32 => Some(TypeHead::Int32),
        tast::Ty::TInt64 => Some(TypeHead::Int64),
        tast::Ty::TUint8 => Some(TypeHead::Uint8),
        tast::Ty::TUint16 => Some(TypeHead::Uint16),
        tast::Ty::TUint32 => Some(TypeHead::Uint32),
        tast::Ty::TUint64 => Some(TypeHead::Uint64),
        tast::Ty::TFloat32 => Some(TypeHead::Float32),
        tast::Ty::TFloat64 => Some(TypeHead::Float64),
        tast::Ty::TString => Some(TypeHead::String),
        tast::Ty::TChar => Some(TypeHead::Char),
        tast::Ty::TTuple { typs } => Some(TypeHead::Tuple(typs.len())),
        tast::Ty::TEnum { name } => Some(TypeHead::Enum(name.clone())),
        tast::Ty::TStruct { name } => Some(TypeHead::Struct(name.clone())),
        tast::Ty::TDyn { trait_name } => Some(TypeHead::Dyn(trait_name.clone())),
        tast::Ty::TApp { ty, .. } => type_head(ty),
        tast::Ty::TArray { len, .. } => Some(TypeHead::Array(*len)),
        tast::Ty::TSlice { .. } => Some(TypeHead::Slice),
        tast::Ty::TVec { .. } => Some(TypeHead::Vec),
        tast::Ty::TRef { .. } => Some(TypeHead::Ref),
        tast::Ty::THashMap { .. } => Some(TypeHead::HashMap),
        tast::Ty::TFunc { params, .. } => Some(TypeHead::Function(params.len())),
        tast::Ty::TParam { .. } => None,
    }
}
