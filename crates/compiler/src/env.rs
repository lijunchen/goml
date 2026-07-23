use diagnostics::{Diagnostics, Severity, Stage};
use indexmap::IndexMap;
use line_index::LineIndex;

pub use super::builtins::builtin_function_names;
use crate::{
    common::{self, Constructor},
    intrinsics::{CallableBody, ExternCapability, LangItemId, LangItemTable},
    tast::{self, TastIdent},
};
use std::cell::Cell;
use std::collections::HashMap;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct EnumDef {
    pub name: TastIdent,
    pub generics: Vec<TastIdent>,
    pub variants: Vec<EnumVariantDef>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct EnumVariantDef {
    pub name: TastIdent,
    pub fields: EnumVariantFields,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum EnumVariantFields {
    Unit,
    Tuple(Vec<tast::Ty>),
    Struct(Vec<(TastIdent, tast::Ty)>),
}

impl EnumVariantFields {
    pub fn types(&self) -> Vec<&tast::Ty> {
        match self {
            Self::Unit => Vec::new(),
            Self::Tuple(types) => types.iter().collect(),
            Self::Struct(fields) => fields.iter().map(|(_, ty)| ty).collect(),
        }
    }

    pub fn cloned_types(&self) -> Vec<tast::Ty> {
        self.types().into_iter().cloned().collect()
    }

    pub fn len(&self) -> usize {
        match self {
            Self::Unit => 0,
            Self::Tuple(types) => types.len(),
            Self::Struct(fields) => fields.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn struct_fields(&self) -> Option<&[(TastIdent, tast::Ty)]> {
        match self {
            Self::Struct(fields) => Some(fields),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct StructDef {
    pub name: TastIdent,
    pub generics: Vec<TastIdent>,
    pub fields: Vec<(TastIdent, tast::Ty)>,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum TypePredicate {
    Trait {
        for_ty: tast::Ty,
        trait_ref: tast::TraitRef,
    },
    Equality {
        lhs: tast::Ty,
        rhs: tast::Ty,
    },
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct FnScheme {
    pub type_params: Vec<String>,
    pub constraints: Vec<TypePredicate>,
    pub ty: tast::Ty,
    #[serde(default)]
    pub body: CallableBody,
}

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct TraitDef {
    pub params: Vec<TastIdent>,
    pub predicates: Vec<TypePredicate>,
    pub supertraits: Vec<tast::TraitRef>,
    pub associated_types: IndexMap<String, AssociatedTypeDef>,
    pub methods: IndexMap<String, FnScheme>,
}

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct AssociatedTypeDef {
    pub bounds: Vec<tast::TraitRef>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct TraitImplKey {
    pub trait_ref: tast::TraitRef,
    pub for_ty: tast::Ty,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ImplDef {
    pub params: Vec<TastIdent>,
    #[serde(default)]
    pub constraints: Vec<TypePredicate>,
    #[serde(default)]
    pub associated_types: IndexMap<String, tast::Ty>,
    pub methods: IndexMap<String, FnScheme>,
    #[serde(default = "impl_is_valid")]
    pub valid: bool,
    #[serde(skip)]
    pub origin: Option<text_size::TextRange>,
}

fn impl_is_valid() -> bool {
    true
}

impl Default for ImplDef {
    fn default() -> Self {
        Self {
            params: Vec::new(),
            constraints: Vec::new(),
            associated_types: IndexMap::new(),
            methods: IndexMap::new(),
            valid: true,
            origin: None,
        }
    }
}

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct TypeEnv {
    pub enums: IndexMap<TastIdent, EnumDef>,
    pub structs: IndexMap<TastIdent, StructDef>,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn enums(&self) -> &IndexMap<TastIdent, EnumDef> {
        &self.enums
    }

    pub fn ensure_enum_placeholder(
        &mut self,
        name: TastIdent,
        generics: Vec<TastIdent>,
    ) -> &mut EnumDef {
        self.enums.entry(name.clone()).or_insert_with(|| EnumDef {
            name,
            generics,
            variants: Vec::new(),
        })
    }

    pub fn insert_enum(&mut self, def: EnumDef) {
        self.enums.insert(def.name.clone(), def);
    }

    pub fn retain_enums<F>(&mut self, f: F)
    where
        F: FnMut(&TastIdent, &mut EnumDef) -> bool,
    {
        self.enums.retain(f);
    }

    pub fn structs(&self) -> &IndexMap<TastIdent, StructDef> {
        &self.structs
    }

    pub fn ensure_struct_placeholder(
        &mut self,
        name: TastIdent,
        generics: Vec<TastIdent>,
    ) -> &mut StructDef {
        self.structs
            .entry(name.clone())
            .or_insert_with(|| StructDef {
                name,
                generics,
                fields: Vec::new(),
            })
    }

    pub fn struct_def_mut(&mut self, name: &TastIdent) -> Option<&mut StructDef> {
        self.structs.get_mut(name)
    }

    pub fn enum_def_mut(&mut self, name: &TastIdent) -> Option<&mut EnumDef> {
        self.enums.get_mut(name)
    }

    pub fn insert_struct(&mut self, def: StructDef) {
        self.structs.insert(def.name.clone(), def);
    }

    pub fn lookup_constructor(&self, constr: &TastIdent) -> Option<(Constructor, tast::Ty)> {
        self.lookup_constructor_with_namespace(None, constr)
    }

    pub fn lookup_constructor_with_namespace(
        &self,
        enum_name: Option<&TastIdent>,
        constr: &TastIdent,
    ) -> Option<(Constructor, tast::Ty)> {
        match enum_name {
            Some(enum_name) => self.lookup_enum_constructor_in(enum_name, constr),
            None => self
                .lookup_enum_constructor(constr)
                .or_else(|| self.lookup_struct_constructor(constr)),
        }
    }

    fn lookup_enum_constructor(&self, constr: &TastIdent) -> Option<(Constructor, tast::Ty)> {
        let mut found: Option<(Constructor, tast::Ty)> = None;
        for (enum_name, enum_def) in self.enums.iter() {
            if let Some(candidate) = Self::enum_constructor_info(enum_name, enum_def, constr) {
                if found.is_some() {
                    return None;
                }
                found = Some(candidate);
            }
        }
        found
    }

    fn lookup_enum_constructor_in(
        &self,
        enum_name: &TastIdent,
        constr: &TastIdent,
    ) -> Option<(Constructor, tast::Ty)> {
        self.enums
            .get(enum_name)
            .and_then(|enum_def| Self::enum_constructor_info(enum_name, enum_def, constr))
    }

    fn enum_constructor_info(
        enum_name: &TastIdent,
        enum_def: &EnumDef,
        constr: &TastIdent,
    ) -> Option<(Constructor, tast::Ty)> {
        enum_def
            .variants
            .iter()
            .enumerate()
            .find(|(_, variant)| &variant.name == constr)
            .map(|(index, _)| Self::build_enum_constructor(enum_name, enum_def, index))
    }

    fn build_enum_constructor(
        enum_name: &TastIdent,
        enum_def: &EnumDef,
        index: usize,
    ) -> (Constructor, tast::Ty) {
        let variant = &enum_def.variants[index];
        let fields = variant.fields.cloned_types();
        let base = tast::Ty::TEnum {
            name: enum_name.0.clone(),
        };
        let args: Vec<tast::Ty> = enum_def
            .generics
            .iter()
            .map(|g| tast::Ty::TParam { name: g.0.clone() })
            .collect();
        let ret_ty = if args.is_empty() {
            base.clone()
        } else {
            tast::Ty::TApp {
                ty: Box::new(base.clone()),
                args,
            }
        };

        let ctor_ty = if fields.is_empty() {
            ret_ty.clone()
        } else {
            tast::Ty::TFunc {
                params: fields,
                ret_ty: Box::new(ret_ty.clone()),
            }
        };

        let constructor = Constructor::Enum(common::EnumConstructor {
            type_name: enum_name.clone(),
            variant: variant.name.clone(),
            index,
        });
        (constructor, ctor_ty)
    }

    fn lookup_struct_constructor(&self, constr: &TastIdent) -> Option<(Constructor, tast::Ty)> {
        self.structs.get(constr).map(|struct_def| {
            let base = tast::Ty::TStruct {
                name: struct_def.name.0.clone(),
            };
            let args: Vec<tast::Ty> = struct_def
                .generics
                .iter()
                .map(|g| tast::Ty::TParam { name: g.0.clone() })
                .collect();
            let ret_ty = if args.is_empty() {
                base.clone()
            } else {
                tast::Ty::TApp {
                    ty: Box::new(base.clone()),
                    args,
                }
            };
            let params: Vec<tast::Ty> =
                struct_def.fields.iter().map(|(_, ty)| ty.clone()).collect();
            let ctor_ty = if params.is_empty() {
                ret_ty.clone()
            } else {
                tast::Ty::TFunc {
                    params,
                    ret_ty: Box::new(ret_ty.clone()),
                }
            };

            let constructor = Constructor::Struct(common::StructConstructor {
                type_name: struct_def.name.clone(),
            });
            (constructor, ctor_ty)
        })
    }
}

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct TraitEnv {
    pub trait_defs: IndexMap<String, TraitDef>,
    #[serde(with = "indexmap::map::serde_seq")]
    pub trait_impls: IndexMap<TraitImplKey, ImplDef>,
    #[serde(with = "indexmap::map::serde_seq")]
    pub inherent_impls: IndexMap<InherentImplKey, ImplDef>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum InherentImplKey {
    Exact(tast::Ty),
    Constr(String),
}

impl TraitEnv {
    pub fn new() -> Self {
        Self {
            trait_defs: IndexMap::new(),
            trait_impls: IndexMap::new(),
            inherent_impls: IndexMap::new(),
        }
    }

    pub fn is_trait(&self, name: &str) -> bool {
        self.trait_defs.contains_key(name)
    }

    pub fn lookup_trait_method_scheme(
        &self,
        trait_ref: &tast::TraitRef,
        method_name: &TastIdent,
    ) -> Option<FnScheme> {
        let trait_def = self.trait_defs.get(&trait_ref.name.0)?;
        if trait_def.params.len() != trait_ref.args.len() {
            return None;
        }
        let substitution = trait_def
            .params
            .iter()
            .zip(trait_ref.args.iter())
            .map(|(param, arg)| (param.0.clone(), arg.clone()))
            .collect::<HashMap<_, _>>();
        let mut scheme = trait_def.methods.get(&method_name.0)?.clone();
        scheme.ty = crate::typer::type_ops::substitute_ty_params(&scheme.ty, &substitution);
        for predicate in &mut scheme.constraints {
            *predicate = crate::typer::type_ops::substitute_predicate(predicate, &substitution);
        }
        Some(scheme)
    }

    pub fn lookup_inherent_method(
        &self,
        receiver_ty: &tast::Ty,
        method: &TastIdent,
    ) -> Option<tast::Ty> {
        self.lookup_inherent_method_scheme(receiver_ty, method)
            .map(|scheme| scheme.ty.clone())
    }

    pub fn lookup_inherent_method_scheme(
        &self,
        receiver_ty: &tast::Ty,
        method: &TastIdent,
    ) -> Option<FnScheme> {
        if let Some(scheme) = self
            .inherent_impls
            .get(&InherentImplKey::Exact(receiver_ty.clone()))
            .and_then(|impl_def| impl_def.methods.get(&method.0))
        {
            return Some(scheme.clone());
        }

        let constr = match receiver_ty {
            tast::Ty::TEnum { name } | tast::Ty::TStruct { name } => Some(name.clone()),
            tast::Ty::TApp { ty, .. } => Some(ty.get_constr_name_unsafe()),
            tast::Ty::TSlice { .. } => Some("Slice".to_string()),
            tast::Ty::TVec { .. } => Some("Vec".to_string()),
            tast::Ty::TRef { .. } => Some("Ref".to_string()),
            tast::Ty::THashMap { .. } => Some("HashMap".to_string()),
            _ => None,
        };
        if let Some(constr) = constr {
            return self.lookup_inherent_method_by_constr(&constr, method);
        }

        None
    }

    pub fn lookup_inherent_method_by_constr(
        &self,
        constr: &str,
        method: &TastIdent,
    ) -> Option<FnScheme> {
        self.inherent_impls
            .get(&InherentImplKey::Constr(constr.to_string()))
            .and_then(|impl_def| impl_def.methods.get(&method.0))
            .cloned()
    }
}

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct ValueEnv {
    pub funcs: IndexMap<String, FnScheme>,
}

impl ValueEnv {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_type_of_function(&self, func: &str) -> Option<tast::Ty> {
        self.get_function_scheme(func).map(|scheme| scheme.ty)
    }

    pub fn get_function_scheme(&self, func: &str) -> Option<FnScheme> {
        self.funcs.get(func).cloned()
    }
}

#[derive(Debug, Clone)]
pub struct GlobalTypeEnv {
    pub type_env: TypeEnv,
    pub trait_env: TraitEnv,
    pub value_env: ValueEnv,
    pub lang_items: LangItemTable,
}

#[derive(Debug, Clone)]
pub struct PackageTypeEnv {
    pub package: String,
    pub builtins: GlobalTypeEnv,
    pub current: GlobalTypeEnv,
    pub deps: HashMap<String, GlobalTypeEnv>,
    pub extern_capability: ExternCapability,
}

impl PackageTypeEnv {
    pub fn new(
        package: String,
        builtins: GlobalTypeEnv,
        current: GlobalTypeEnv,
        deps: HashMap<String, GlobalTypeEnv>,
    ) -> Self {
        Self {
            package,
            builtins,
            current,
            deps,
            extern_capability: ExternCapability::None,
        }
    }

    pub fn with_extern_capability(mut self, capability: ExternCapability) -> Self {
        self.extern_capability = capability;
        self
    }

    pub fn builtins(&self) -> &GlobalTypeEnv {
        &self.builtins
    }

    pub fn current(&self) -> &GlobalTypeEnv {
        &self.current
    }

    pub fn current_mut(&mut self) -> &mut GlobalTypeEnv {
        &mut self.current
    }

    pub fn lang_item(&self, id: LangItemId) -> Option<&TastIdent> {
        self.current
            .lang_item(id)
            .or_else(|| self.builtins.lang_item(id))
            .or_else(|| self.deps.values().find_map(|env| env.lang_item(id)))
    }

    pub(crate) fn shadows_builtin_nominal_type(&self, ty: &tast::Ty) -> bool {
        let Some(name) = nominal_type_name(ty) else {
            return false;
        };
        self.current.defines_struct_or_enum(name)
            || self
                .deps
                .values()
                .any(|env| env.defines_struct_or_enum(name))
    }

    pub fn visible_trait_impls(
        &self,
        trait_name: &str,
    ) -> Vec<(String, usize, &tast::TraitRef, &tast::Ty, &ImplDef)> {
        let mut result = Vec::new();
        result.extend(
            self.builtins
                .trait_env
                .trait_impls
                .iter()
                .enumerate()
                .filter_map(|(index, (key, impl_def))| {
                    (key.trait_ref.name.0 == trait_name).then_some((
                        "builtin".to_string(),
                        index,
                        &key.trait_ref,
                        &key.for_ty,
                        impl_def,
                    ))
                }),
        );
        result.extend(
            self.current
                .trait_env
                .trait_impls
                .iter()
                .enumerate()
                .filter_map(|(index, (key, impl_def))| {
                    (key.trait_ref.name.0 == trait_name).then_some((
                        self.package.clone(),
                        index,
                        &key.trait_ref,
                        &key.for_ty,
                        impl_def,
                    ))
                }),
        );
        let mut packages = self.deps.keys().collect::<Vec<_>>();
        packages.sort();
        for package in packages {
            let Some(env) = self.deps.get(package) else {
                continue;
            };
            result.extend(env.trait_env.trait_impls.iter().enumerate().filter_map(
                |(index, (key, impl_def))| {
                    (key.trait_ref.name.0 == trait_name).then_some((
                        package.clone(),
                        index,
                        &key.trait_ref,
                        &key.for_ty,
                        impl_def,
                    ))
                },
            ));
        }
        result
    }

    pub fn lookup_visible_inherent_method_scheme(
        &self,
        receiver_ty: &tast::Ty,
        method: &TastIdent,
    ) -> Option<FnScheme> {
        let builtin = if self.shadows_builtin_nominal_type(receiver_ty) {
            None
        } else {
            self.builtins
                .lookup_inherent_method_scheme(receiver_ty, method)
        };
        builtin
            .or_else(|| {
                self.current
                    .lookup_inherent_method_scheme(receiver_ty, method)
            })
            .or_else(|| {
                self.deps
                    .values()
                    .find_map(|env| env.lookup_inherent_method_scheme(receiver_ty, method))
            })
    }

    pub fn get_function_scheme(&self, name: &str) -> Option<FnScheme> {
        if let Some(scheme) = self
            .current
            .get_function_scheme(name)
            .or_else(|| self.builtins.get_function_scheme(name))
        {
            return Some(scheme);
        }

        let mut packages = self.deps.keys().collect::<Vec<_>>();
        packages.sort_by_key(|package| std::cmp::Reverse(package.len()));
        packages.into_iter().find_map(|package| {
            name.strip_prefix(package)
                .filter(|rest| rest.starts_with("::"))
                .and_then(|_| self.deps.get(package)?.get_function_scheme(name))
        })
    }
}

fn nominal_type_name(ty: &tast::Ty) -> Option<&str> {
    match ty {
        tast::Ty::TStruct { name } | tast::Ty::TEnum { name } => Some(name),
        tast::Ty::TApp { ty, .. } => nominal_type_name(ty),
        _ => None,
    }
}

impl Default for GlobalTypeEnv {
    fn default() -> Self {
        Self::new()
    }
}

impl GlobalTypeEnv {
    pub fn new() -> Self {
        Self::new_empty()
    }

    pub fn new_empty() -> Self {
        Self {
            type_env: TypeEnv::new(),
            trait_env: TraitEnv {
                trait_defs: IndexMap::new(),
                trait_impls: IndexMap::new(),
                inherent_impls: IndexMap::new(),
            },
            value_env: ValueEnv::new(),
            lang_items: LangItemTable::default(),
        }
    }

    pub fn lang_item(&self, id: LangItemId) -> Option<&TastIdent> {
        self.lang_items.get(id)
    }

    pub fn enums(&self) -> &IndexMap<TastIdent, EnumDef> {
        self.type_env.enums()
    }

    pub fn ensure_enum_placeholder(
        &mut self,
        name: TastIdent,
        generics: Vec<TastIdent>,
    ) -> &mut EnumDef {
        self.type_env.ensure_enum_placeholder(name, generics)
    }

    pub fn insert_enum(&mut self, def: EnumDef) {
        self.type_env.insert_enum(def)
    }

    pub fn retain_enums<F>(&mut self, f: F)
    where
        F: FnMut(&TastIdent, &mut EnumDef) -> bool,
    {
        self.type_env.retain_enums(f)
    }

    pub fn structs(&self) -> &IndexMap<TastIdent, StructDef> {
        self.type_env.structs()
    }

    pub fn ensure_struct_placeholder(
        &mut self,
        name: TastIdent,
        generics: Vec<TastIdent>,
    ) -> &mut StructDef {
        self.type_env.ensure_struct_placeholder(name, generics)
    }

    pub fn struct_def_mut(&mut self, name: &TastIdent) -> Option<&mut StructDef> {
        self.type_env.struct_def_mut(name)
    }

    pub fn enum_def_mut(&mut self, name: &TastIdent) -> Option<&mut EnumDef> {
        self.type_env.enum_def_mut(name)
    }

    pub fn insert_struct(&mut self, def: StructDef) {
        self.type_env.insert_struct(def)
    }

    pub fn lookup_constructor(&self, constr: &TastIdent) -> Option<(Constructor, tast::Ty)> {
        self.type_env.lookup_constructor(constr)
    }

    pub fn lookup_constructor_with_namespace(
        &self,
        enum_name: Option<&tast::TastIdent>,
        constr: &tast::TastIdent,
    ) -> Option<(Constructor, tast::Ty)> {
        self.type_env
            .lookup_constructor_with_namespace(enum_name, constr)
    }

    pub fn is_trait(&self, name: &str) -> bool {
        self.trait_env.is_trait(name)
    }

    pub fn lookup_inherent_method(
        &self,
        receiver_ty: &tast::Ty,
        method: &TastIdent,
    ) -> Option<tast::Ty> {
        self.trait_env.lookup_inherent_method(receiver_ty, method)
    }

    pub fn get_type_of_function(&self, func: &str) -> Option<tast::Ty> {
        self.value_env.get_type_of_function(func)
    }

    fn defines_struct_or_enum(&self, name: &str) -> bool {
        let ident = TastIdent::new(name);
        self.type_env.structs.contains_key(&ident) || self.type_env.enums.contains_key(&ident)
    }

    pub fn lookup_trait_method_scheme(
        &self,
        trait_ref: &tast::TraitRef,
        method_name: &TastIdent,
    ) -> Option<FnScheme> {
        self.trait_env
            .lookup_trait_method_scheme(trait_ref, method_name)
    }

    pub fn lookup_inherent_method_scheme(
        &self,
        receiver_ty: &tast::Ty,
        method: &TastIdent,
    ) -> Option<FnScheme> {
        self.trait_env
            .lookup_inherent_method_scheme(receiver_ty, method)
    }

    pub fn lookup_inherent_method_by_constr(
        &self,
        constr: &str,
        method: &TastIdent,
    ) -> Option<FnScheme> {
        self.trait_env
            .lookup_inherent_method_by_constr(constr, method)
    }

    pub fn get_function_scheme(&self, func: &str) -> Option<FnScheme> {
        self.value_env.get_function_scheme(func)
    }
}

#[derive(Debug, Clone, Default)]
pub struct Gensym {
    counter: Cell<i32>,
}

impl Gensym {
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns a fresh identifier prefixed by `prefix`.
    pub fn gensym(&self, prefix: &str) -> String {
        let current = self.counter.get();
        self.counter.set(current + 1);
        format!("{}{}", prefix, current)
    }
}

pub fn format_typer_diagnostics(diagnostics: &Diagnostics, src: &str) -> Vec<String> {
    let index = LineIndex::new(src);
    diagnostics
        .iter()
        .filter(|diagnostic| {
            diagnostic.severity() == Severity::Error && diagnostic.stage() == &Stage::Typer
        })
        .map(|diagnostic| {
            if let Some(range) = diagnostic.range() {
                let line_col = index.line_col(range.start());
                format!(
                    "{}:{}: {}",
                    line_col.line + 1,
                    line_col.col + 1,
                    diagnostic.message()
                )
            } else {
                diagnostic.message().to_string()
            }
        })
        .collect()
}

pub fn format_compile_diagnostics(diagnostics: &Diagnostics, src: &str) -> Vec<String> {
    let compile_stage = Stage::other("compile");
    let index = LineIndex::new(src);
    diagnostics
        .iter()
        .filter(|diagnostic| {
            diagnostic.severity() == Severity::Error && diagnostic.stage() == &compile_stage
        })
        .map(|diagnostic| {
            if let Some(range) = diagnostic.range() {
                let line_col = index.line_col(range.start());
                format!(
                    "{}:{}: {}",
                    line_col.line + 1,
                    line_col.col + 1,
                    diagnostic.message()
                )
            } else {
                diagnostic.message().to_string()
            }
        })
        .collect()
}
