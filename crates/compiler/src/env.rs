use diagnostics::{Diagnostics, Severity, Stage};
use indexmap::IndexMap;
use line_index::LineIndex;

pub use super::builtins::builtin_function_names;
use crate::{
    common::{self, Constructor},
    package_names::BUILTIN_PACKAGE,
    tast::{self, TastIdent},
};
use std::cell::Cell;
use std::collections::HashMap;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct EnumDef {
    pub name: TastIdent,
    pub generics: Vec<TastIdent>,
    pub variants: Vec<(TastIdent, Vec<tast::Ty>)>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct StructDef {
    pub name: TastIdent,
    pub generics: Vec<TastIdent>,
    pub fields: Vec<(TastIdent, tast::Ty)>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ExternFunc {
    pub package_path: String,
    pub go_name: String,
    pub ty: tast::Ty,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ExternType {
    pub go_name: String,
    pub package_path: Option<String>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum Constraint {
    TypeEqual(
        tast::Ty,
        tast::Ty,
        #[serde(skip)] Option<text_size::TextRange>,
    ),
    Overloaded {
        op: TastIdent,
        trait_name: TastIdent,
        call_site_type: tast::Ty,
        #[serde(skip)]
        origin: Option<text_size::TextRange>,
    },
    Implements {
        trait_name: TastIdent,
        for_ty: tast::Ty,
        #[serde(skip)]
        origin: Option<text_size::TextRange>,
    },
    StructFieldAccess {
        expr_ty: tast::Ty,
        field: TastIdent,
        result_ty: tast::Ty,
        #[serde(skip)]
        origin: Option<text_size::TextRange>,
    },
}

/// Origin of a function definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, serde::Serialize, serde::Deserialize)]
pub enum FnOrigin {
    /// User-defined function in source code
    #[default]
    User,
    /// Built-in function provided by the runtime
    Builtin,
    /// Compiler-generated function (e.g., from lambda lifting)
    Compiler,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct FnConstraint {
    pub type_param: String,
    pub trait_name: TastIdent,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct FnScheme {
    pub type_params: Vec<String>,
    pub constraints: Vec<FnConstraint>,
    pub ty: tast::Ty,
    /// Origin of this function (user-defined, builtin, or compiler-generated)
    pub origin: FnOrigin,
}

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct TraitDef {
    pub methods: IndexMap<String, FnScheme>,
}

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct ImplDef {
    pub params: Vec<TastIdent>,
    pub methods: IndexMap<String, FnScheme>,
}

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct TypeEnv {
    pub enums: IndexMap<TastIdent, EnumDef>,
    pub structs: IndexMap<TastIdent, StructDef>,
    pub extern_types: IndexMap<String, ExternType>,
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

    pub fn insert_struct(&mut self, def: StructDef) {
        self.structs.insert(def.name.clone(), def);
    }

    pub fn register_extern_type(&mut self, goml_name: String) {
        self.extern_types
            .entry(goml_name.clone())
            .or_insert_with(|| ExternType {
                go_name: goml_name.clone(),
                package_path: None,
            });
    }

    fn assign_package_to_extern_type(&mut self, type_name: &str, package_path: &str) {
        if let Some(ext_ty) = self.extern_types.get_mut(type_name) {
            match &ext_ty.package_path {
                Some(existing) => {
                    if existing != package_path {
                        // keep the first associated package to avoid conflicting bindings
                    }
                }
                None => {
                    ext_ty.package_path = Some(package_path.to_string());
                }
            }
        }
    }

    fn record_extern_type_usage(&mut self, ty: &tast::Ty, package_path: &str) {
        match ty {
            tast::Ty::TVar(_)
            | tast::Ty::TUnit
            | tast::Ty::TBool
            | tast::Ty::TInt8
            | tast::Ty::TInt16
            | tast::Ty::TInt32
            | tast::Ty::TInt64
            | tast::Ty::TUint8
            | tast::Ty::TUint16
            | tast::Ty::TUint32
            | tast::Ty::TUint64
            | tast::Ty::TFloat32
            | tast::Ty::TFloat64
            | tast::Ty::TString
            | tast::Ty::TChar => {}
            tast::Ty::TTuple { typs } => {
                for ty in typs {
                    self.record_extern_type_usage(ty, package_path);
                }
            }
            tast::Ty::TFunc { params, ret_ty } => {
                for param in params {
                    self.record_extern_type_usage(param, package_path);
                }
                self.record_extern_type_usage(ret_ty, package_path);
            }
            tast::Ty::TParam { .. } => {}
            tast::Ty::TEnum { name } | tast::Ty::TStruct { name } => {
                self.assign_package_to_extern_type(name, package_path);
            }
            tast::Ty::TApp { ty, args } => {
                self.record_extern_type_usage(ty, package_path);
                for arg in args {
                    self.record_extern_type_usage(arg, package_path);
                }
            }
            tast::Ty::TArray { elem, .. } => {
                self.record_extern_type_usage(elem, package_path);
            }
            tast::Ty::TSlice { elem } => {
                self.record_extern_type_usage(elem, package_path);
            }
            tast::Ty::TVec { elem } => {
                self.record_extern_type_usage(elem, package_path);
            }
            tast::Ty::TRef { elem } => {
                self.record_extern_type_usage(elem, package_path);
            }
            tast::Ty::THashMap { key, value } => {
                self.record_extern_type_usage(key, package_path);
                self.record_extern_type_usage(value, package_path);
            }
            tast::Ty::TDyn { .. } => {}
        }
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
            .find(|(_, (variant_name, _))| variant_name == constr)
            .map(|(index, _)| Self::build_enum_constructor(enum_name, enum_def, index))
    }

    fn build_enum_constructor(
        enum_name: &TastIdent,
        enum_def: &EnumDef,
        index: usize,
    ) -> (Constructor, tast::Ty) {
        let (_, fields) = &enum_def.variants[index];
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
                params: fields.clone(),
                ret_ty: Box::new(ret_ty.clone()),
            }
        };

        let constructor = Constructor::Enum(common::EnumConstructor {
            type_name: enum_name.clone(),
            variant: enum_def.variants[index].0.clone(),
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
    pub trait_impls: IndexMap<(String, tast::Ty), ImplDef>,
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

    pub fn lookup_trait_method(
        &self,
        trait_name: &TastIdent,
        method_name: &TastIdent,
    ) -> Option<tast::Ty> {
        self.lookup_trait_method_scheme(trait_name, method_name)
            .map(|scheme| scheme.ty.clone())
    }

    pub fn lookup_trait_method_scheme(
        &self,
        trait_name: &TastIdent,
        method_name: &TastIdent,
    ) -> Option<FnScheme> {
        self.trait_defs
            .get(&trait_name.0)
            .and_then(|trait_def| trait_def.methods.get(&method_name.0))
            .cloned()
    }

    pub fn get_trait_impl(
        &self,
        trait_name: &TastIdent,
        type_name: &tast::Ty,
        func_name: &TastIdent,
    ) -> Option<tast::Ty> {
        self.get_trait_impl_scheme(trait_name, type_name, func_name)
            .map(|scheme| scheme.ty.clone())
    }

    pub fn get_trait_impl_scheme(
        &self,
        trait_name: &TastIdent,
        type_name: &tast::Ty,
        func_name: &TastIdent,
    ) -> Option<FnScheme> {
        let key = (trait_name.0.clone(), type_name.clone());
        if let Some(scheme) = self
            .trait_impls
            .get(&key)
            .and_then(|impl_def| impl_def.methods.get(&func_name.0))
        {
            return Some(scheme.clone());
        }

        let mut found: Option<FnScheme> = None;
        for ((impl_trait_name, impl_for_ty), impl_def) in self.trait_impls.iter() {
            if impl_trait_name != &trait_name.0 {
                continue;
            }
            let Some(method) = impl_def.methods.get(&func_name.0) else {
                continue;
            };
            if !trait_impl_matches(impl_for_ty, type_name) {
                continue;
            }
            if found.is_some() {
                return None;
            }
            found = Some(method.clone());
        }

        found
    }

    pub fn has_trait_impl(&self, trait_name: &str, type_name: &tast::Ty) -> bool {
        if self
            .trait_impls
            .contains_key(&(trait_name.to_string(), type_name.clone()))
        {
            return true;
        }
        self.trait_impls
            .iter()
            .any(|((impl_trait_name, impl_ty), _)| {
                impl_trait_name == trait_name && trait_impl_matches(impl_ty, type_name)
            })
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

fn trait_impl_matches(template: &tast::Ty, actual: &tast::Ty) -> bool {
    fn go(template: &tast::Ty, actual: &tast::Ty, subst: &mut HashMap<String, tast::Ty>) -> bool {
        match template {
            tast::Ty::TParam { name } => match subst.get(name) {
                Some(bound) => bound == actual,
                None => {
                    subst.insert(name.clone(), actual.clone());
                    true
                }
            },
            tast::Ty::TVar(_)
            | tast::Ty::TUnit
            | tast::Ty::TBool
            | tast::Ty::TInt8
            | tast::Ty::TInt16
            | tast::Ty::TInt32
            | tast::Ty::TInt64
            | tast::Ty::TUint8
            | tast::Ty::TUint16
            | tast::Ty::TUint32
            | tast::Ty::TUint64
            | tast::Ty::TFloat32
            | tast::Ty::TFloat64
            | tast::Ty::TString
            | tast::Ty::TChar => template == actual,
            tast::Ty::TTuple { typs } => match actual {
                tast::Ty::TTuple { typs: actual_typs } if typs.len() == actual_typs.len() => typs
                    .iter()
                    .zip(actual_typs.iter())
                    .all(|(t, a)| go(t, a, subst)),
                _ => false,
            },
            tast::Ty::TEnum { name } => matches!(actual, tast::Ty::TEnum { name: n } if n == name),
            tast::Ty::TStruct { name } => {
                matches!(actual, tast::Ty::TStruct { name: n } if n == name)
            }
            tast::Ty::TDyn { trait_name } => {
                matches!(actual, tast::Ty::TDyn { trait_name: n } if n == trait_name)
            }
            tast::Ty::TApp { ty, args } => match actual {
                tast::Ty::TApp {
                    ty: actual_ty,
                    args: actual_args,
                } if args.len() == actual_args.len() => {
                    go(ty, actual_ty, subst)
                        && args
                            .iter()
                            .zip(actual_args.iter())
                            .all(|(t, a)| go(t, a, subst))
                }
                _ => false,
            },
            tast::Ty::TArray { len, elem } => match actual {
                tast::Ty::TArray {
                    len: actual_len,
                    elem: actual_elem,
                } => len == actual_len && go(elem, actual_elem, subst),
                _ => false,
            },
            tast::Ty::TSlice { elem } => match actual {
                tast::Ty::TSlice { elem: actual_elem } => go(elem, actual_elem, subst),
                _ => false,
            },
            tast::Ty::TVec { elem } => match actual {
                tast::Ty::TVec { elem: actual_elem } => go(elem, actual_elem, subst),
                _ => false,
            },
            tast::Ty::TRef { elem } => match actual {
                tast::Ty::TRef { elem: actual_elem } => go(elem, actual_elem, subst),
                _ => false,
            },
            tast::Ty::THashMap { key, value } => match actual {
                tast::Ty::THashMap {
                    key: actual_key,
                    value: actual_value,
                } => go(key, actual_key, subst) && go(value, actual_value, subst),
                _ => false,
            },
            tast::Ty::TFunc { params, ret_ty } => match actual {
                tast::Ty::TFunc {
                    params: actual_params,
                    ret_ty: actual_ret,
                } if params.len() == actual_params.len() => {
                    params
                        .iter()
                        .zip(actual_params.iter())
                        .all(|(t, a)| go(t, a, subst))
                        && go(ret_ty, actual_ret, subst)
                }
                _ => false,
            },
        }
    }

    let mut subst = HashMap::new();
    go(template, actual, &mut subst)
}

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct ValueEnv {
    pub funcs: IndexMap<String, FnScheme>,
    pub extern_funcs: IndexMap<String, ExternFunc>,
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
}

#[derive(Debug, Clone)]
pub struct PackageTypeEnv {
    pub package: String,
    pub builtins: GlobalTypeEnv,
    pub current: GlobalTypeEnv,
    pub deps: HashMap<String, GlobalTypeEnv>,
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
        }
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

    pub fn env_for_package(&self, package: &str) -> Option<&GlobalTypeEnv> {
        if package == BUILTIN_PACKAGE {
            return Some(&self.builtins);
        }
        if package == self.package {
            Some(&self.current)
        } else {
            self.deps.get(package)
        }
    }

    pub fn get_type_of_function_unqualified(&self, name: &str) -> Option<tast::Ty> {
        self.get_function_scheme_unqualified(name)
            .map(|scheme| scheme.ty)
    }

    pub fn get_function_scheme_unqualified(&self, name: &str) -> Option<FnScheme> {
        self.current
            .get_function_scheme(name)
            .or_else(|| self.builtins.get_function_scheme(name))
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
        }
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

    pub fn insert_struct(&mut self, def: StructDef) {
        self.type_env.insert_struct(def)
    }

    pub fn register_extern_type(&mut self, goml_name: String) {
        self.type_env.register_extern_type(goml_name)
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

    pub fn lookup_trait_method(
        &self,
        trait_name: &TastIdent,
        method_name: &TastIdent,
    ) -> Option<tast::Ty> {
        self.trait_env.lookup_trait_method(trait_name, method_name)
    }

    pub fn get_trait_impl(
        &self,
        trait_name: &TastIdent,
        type_name: &tast::Ty,
        func_name: &TastIdent,
    ) -> Option<tast::Ty> {
        self.trait_env
            .get_trait_impl(trait_name, type_name, func_name)
    }

    pub fn has_trait_impl(&self, trait_name: &str, type_name: &tast::Ty) -> bool {
        self.trait_env.has_trait_impl(trait_name, type_name)
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

    pub fn lookup_trait_method_scheme(
        &self,
        trait_name: &TastIdent,
        method_name: &TastIdent,
    ) -> Option<FnScheme> {
        self.trait_env
            .lookup_trait_method_scheme(trait_name, method_name)
    }

    pub fn get_trait_impl_scheme(
        &self,
        trait_name: &TastIdent,
        type_name: &tast::Ty,
        func_name: &TastIdent,
    ) -> Option<FnScheme> {
        self.trait_env
            .get_trait_impl_scheme(trait_name, type_name, func_name)
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

    pub fn register_extern_function(
        &mut self,
        goml_name: String,
        package_path: String,
        go_name: String,
        ty: tast::Ty,
    ) {
        self.value_env.funcs.insert(
            goml_name.clone(),
            FnScheme {
                type_params: vec![],
                constraints: vec![],
                ty: ty.clone(),
                origin: FnOrigin::User,
            },
        );
        self.type_env.record_extern_type_usage(&ty, &package_path);
        self.value_env.extern_funcs.insert(
            goml_name,
            ExternFunc {
                package_path,
                go_name,
                ty,
            },
        );
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

    #[allow(unused)]
    pub fn reset(&self) {
        self.counter.set(0);
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
