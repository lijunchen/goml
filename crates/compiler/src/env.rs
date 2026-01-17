use diagnostics::{Diagnostics, Severity, Stage};
use indexmap::IndexMap;
use line_index::LineIndex;

pub use super::builtins::builtin_function_names;
use super::builtins::{builtin_env, builtin_inherent_methods};
use crate::{
    common::{self, Constructor},
    tast::{self, TastIdent},
};
use std::cell::Cell;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct EnumDef {
    pub name: TastIdent,
    pub generics: Vec<TastIdent>,
    pub variants: Vec<(TastIdent, Vec<tast::Ty>)>,
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: TastIdent,
    pub generics: Vec<TastIdent>,
    pub fields: Vec<(TastIdent, tast::Ty)>,
}

#[derive(Debug, Clone)]
pub struct ExternFunc {
    pub package_path: String,
    pub go_name: String,
    pub ty: tast::Ty,
}

#[derive(Debug, Clone)]
pub struct ExternType {
    pub go_name: String,
    pub package_path: Option<String>,
}

#[derive(Debug, Clone)]
pub enum Constraint {
    TypeEqual(tast::Ty, tast::Ty),
    Overloaded {
        op: TastIdent,
        trait_name: TastIdent,
        call_site_type: tast::Ty,
    },
    StructFieldAccess {
        expr_ty: tast::Ty,
        field: TastIdent,
        result_ty: tast::Ty,
    },
}

/// Origin of a function definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FnOrigin {
    /// User-defined function in source code
    #[default]
    User,
    /// Built-in function provided by the runtime
    Builtin,
    /// Compiler-generated function (e.g., from lambda lifting)
    Compiler,
}

#[derive(Debug, Clone)]
pub struct FnScheme {
    pub type_params: Vec<String>,
    pub constraints: (), // placeholder for future use
    pub ty: tast::Ty,
    /// Origin of this function (user-defined, builtin, or compiler-generated)
    pub origin: FnOrigin,
}

#[derive(Debug, Clone, Default)]
pub struct TraitDef {
    pub methods: IndexMap<String, FnScheme>,
}

#[derive(Debug, Clone, Default)]
pub struct ImplDef {
    pub params: Vec<TastIdent>,
    pub methods: IndexMap<String, FnScheme>,
}

#[derive(Debug, Clone, Default)]
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
            | tast::Ty::TString => {}
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
            tast::Ty::TVec { elem } => {
                self.record_extern_type_usage(elem, package_path);
            }
            tast::Ty::TRef { elem } => {
                self.record_extern_type_usage(elem, package_path);
            }
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
                    panic!(
                        "Constructor {} is defined in multiple enums; use Enum::{} to disambiguate",
                        constr.0, constr.0
                    );
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

#[derive(Debug, Clone, Default)]
pub struct TraitEnv {
    pub trait_defs: IndexMap<String, TraitDef>,
    pub trait_impls: IndexMap<(String, tast::Ty), ImplDef>,
    pub inherent_impls: IndexMap<InherentImplKey, ImplDef>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InherentImplKey {
    Exact(tast::Ty),
    Constr(String),
}

impl TraitEnv {
    pub fn new() -> Self {
        Self {
            trait_defs: IndexMap::new(),
            trait_impls: IndexMap::new(),
            inherent_impls: builtin_inherent_methods(),
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
        self.trait_defs
            .get(&trait_name.0)
            .and_then(|trait_def| trait_def.methods.get(&method_name.0))
            .map(|scheme| scheme.ty.clone())
    }

    pub fn get_trait_impl(
        &self,
        trait_name: &TastIdent,
        type_name: &tast::Ty,
        func_name: &TastIdent,
    ) -> Option<tast::Ty> {
        let key = (trait_name.0.clone(), type_name.clone());
        self.trait_impls
            .get(&key)
            .and_then(|impl_def| impl_def.methods.get(&func_name.0))
            .map(|scheme| scheme.ty.clone())
    }

    pub fn lookup_inherent_method(
        &self,
        receiver_ty: &tast::Ty,
        method: &TastIdent,
    ) -> Option<tast::Ty> {
        // First try exact match
        if let Some(scheme) = self
            .inherent_impls
            .get(&InherentImplKey::Exact(receiver_ty.clone()))
            .and_then(|impl_def| impl_def.methods.get(&method.0))
        {
            return Some(scheme.ty.clone());
        }

        let constr = match receiver_ty {
            tast::Ty::TEnum { name } | tast::Ty::TStruct { name } => Some(name.clone()),
            tast::Ty::TApp { ty, .. } => Some(ty.get_constr_name_unsafe()),
            _ => None,
        };
        if let Some(constr) = constr
            && let Some(scheme) = self
                .inherent_impls
                .get(&InherentImplKey::Constr(constr))
                .and_then(|impl_def| impl_def.methods.get(&method.0))
        {
            return Some(scheme.ty.clone());
        }

        None
    }
}

#[derive(Debug, Clone, Default)]
pub struct ValueEnv {
    pub funcs: IndexMap<String, FnScheme>,
    pub extern_funcs: IndexMap<String, ExternFunc>,
}

impl ValueEnv {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_type_of_function(&self, func: &str) -> Option<tast::Ty> {
        self.funcs.get(func).map(|scheme| scheme.ty.clone())
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
    pub current: GlobalTypeEnv,
    pub deps: HashMap<String, GlobalTypeEnv>,
}

impl PackageTypeEnv {
    pub fn new(
        package: String,
        current: GlobalTypeEnv,
        deps: HashMap<String, GlobalTypeEnv>,
    ) -> Self {
        Self {
            package,
            current,
            deps,
        }
    }

    pub fn current(&self) -> &GlobalTypeEnv {
        &self.current
    }

    pub fn current_mut(&mut self) -> &mut GlobalTypeEnv {
        &mut self.current
    }

    pub fn env_for_package(&self, package: &str) -> Option<&GlobalTypeEnv> {
        if package == self.package {
            Some(&self.current)
        } else {
            self.deps.get(package)
        }
    }
}

impl Default for GlobalTypeEnv {
    fn default() -> Self {
        Self::new()
    }
}

impl GlobalTypeEnv {
    pub fn new() -> Self {
        builtin_env()
    }

    /// Create an empty GlobalTypeEnv without any builtins.
    /// This is used during the initial parsing of builtin.gom to avoid recursion.
    pub fn new_empty() -> Self {
        Self {
            type_env: TypeEnv::new(),
            trait_env: TraitEnv {
                trait_defs: IndexMap::new(),
                trait_impls: IndexMap::new(),
                inherent_impls: IndexMap::new(),
            },
            value_env: ValueEnv {
                funcs: IndexMap::new(),
                extern_funcs: IndexMap::new(),
            },
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
                constraints: (),
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

pub fn format_typer_diagnostics(diagnostics: &Diagnostics) -> Vec<String> {
    diagnostics
        .iter()
        .filter(|diagnostic| {
            diagnostic.severity() == Severity::Error && diagnostic.stage() == &Stage::Typer
        })
        .map(|diagnostic| diagnostic.message().to_string())
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
