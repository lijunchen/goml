use ast::ast::Ident;
use diagnostics::{Diagnostics, Severity, Stage};
use im::HashMap as ImHashMap;
use indexmap::{IndexMap, IndexSet};
use line_index::LineIndex;

pub use super::builtins::builtin_function_names;
use super::builtins::{builtin_functions, builtin_inherent_methods};
use crate::{
    core,
    mangle::encode_ty,
    tast::{self, Constructor},
};
use std::cell::Cell;

#[derive(Debug, Clone)]
pub struct EnumDef {
    pub name: Ident,
    pub generics: Vec<Ident>,
    pub variants: Vec<(Ident, Vec<tast::Ty>)>,
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: Ident,
    pub generics: Vec<Ident>,
    pub fields: Vec<(Ident, tast::Ty)>,
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
        op: Ident,
        trait_name: Ident,
        call_site_type: tast::Ty,
    },
    StructFieldAccess {
        expr_ty: tast::Ty,
        field: Ident,
        result_ty: tast::Ty,
    },
}

#[derive(Debug, Clone)]
pub struct GlobalTypeEnv {
    pub enums: IndexMap<Ident, EnumDef>,
    pub structs: IndexMap<Ident, StructDef>,
    pub closure_env_apply: IndexMap<String, String>,
    pub trait_defs: IndexMap<(String, String), tast::Ty>,
    pub overloaded_funcs_to_trait_name: IndexMap<String, Ident>,
    pub trait_impls: IndexMap<(String, String, Ident), tast::Ty>,
    pub inherent_impls: IndexMap<(String, Ident), (String, tast::Ty)>,
    pub funcs: IndexMap<String, tast::Ty>,
    pub extern_funcs: IndexMap<String, ExternFunc>,
    pub extern_types: IndexMap<String, ExternType>,
    pub tuple_types: IndexSet<tast::Ty>,
    pub array_types: IndexSet<tast::Ty>,
    pub ref_types: IndexSet<tast::Ty>,
}

impl Default for GlobalTypeEnv {
    fn default() -> Self {
        Self::new()
    }
}

impl GlobalTypeEnv {
    pub fn new() -> Self {
        Self {
            enums: IndexMap::new(),
            structs: IndexMap::new(),
            closure_env_apply: IndexMap::new(),
            funcs: builtin_functions(),
            extern_funcs: IndexMap::new(),
            extern_types: IndexMap::new(),
            trait_defs: IndexMap::new(),
            overloaded_funcs_to_trait_name: IndexMap::new(),
            trait_impls: IndexMap::new(),
            inherent_impls: builtin_inherent_methods(),
            tuple_types: IndexSet::new(),
            array_types: IndexSet::new(),
            ref_types: IndexSet::new(),
        }
    }

    pub fn enums(&self) -> &IndexMap<Ident, EnumDef> {
        &self.enums
    }

    pub fn ensure_enum_placeholder(&mut self, name: Ident, generics: Vec<Ident>) -> &mut EnumDef {
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
        F: FnMut(&Ident, &mut EnumDef) -> bool,
    {
        self.enums.retain(f);
    }

    pub fn structs(&self) -> &IndexMap<Ident, StructDef> {
        &self.structs
    }

    pub fn ensure_struct_placeholder(
        &mut self,
        name: Ident,
        generics: Vec<Ident>,
    ) -> &mut StructDef {
        self.structs
            .entry(name.clone())
            .or_insert_with(|| StructDef {
                name,
                generics,
                fields: Vec::new(),
            })
    }

    pub fn struct_def_mut(&mut self, name: &Ident) -> Option<&mut StructDef> {
        self.structs.get_mut(name)
    }

    pub fn insert_struct(&mut self, def: StructDef) {
        self.structs.insert(def.name.clone(), def);
    }

    pub fn register_extern_function(
        &mut self,
        goml_name: String,
        package_path: String,
        go_name: String,
        ty: tast::Ty,
    ) {
        self.funcs.insert(goml_name.clone(), ty.clone());
        self.record_extern_type_usage(&ty, &package_path);
        self.extern_funcs.insert(
            goml_name,
            ExternFunc {
                package_path,
                go_name,
                ty,
            },
        );
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
            tast::Ty::TCon { name } => {
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
            tast::Ty::TRef { elem } => {
                self.record_extern_type_usage(elem, package_path);
            }
        }
    }

    pub fn closure_apply_fn(&self, struct_name: &str) -> Option<&str> {
        self.closure_env_apply.get(struct_name).map(|s| s.as_str())
    }

    pub fn get_trait_impl(
        &self,
        trait_name: &Ident,
        type_name: &tast::Ty,
        func_name: &Ident,
    ) -> Option<tast::Ty> {
        self.trait_impls
            .get(&(
                trait_name.0.clone(),
                encode_ty(type_name),
                func_name.clone(),
            ))
            .cloned()
    }

    pub fn lookup_constructor(&self, constr: &Ident) -> Option<(Constructor, tast::Ty)> {
        self.lookup_constructor_with_namespace(None, constr)
    }

    pub fn lookup_constructor_with_namespace(
        &self,
        enum_name: Option<&Ident>,
        constr: &Ident,
    ) -> Option<(Constructor, tast::Ty)> {
        match enum_name {
            Some(enum_name) => self.lookup_enum_constructor_in(enum_name, constr),
            None => self
                .lookup_enum_constructor(constr)
                .or_else(|| self.lookup_struct_constructor(constr)),
        }
    }

    fn lookup_enum_constructor(&self, constr: &Ident) -> Option<(Constructor, tast::Ty)> {
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
        enum_name: &Ident,
        constr: &Ident,
    ) -> Option<(Constructor, tast::Ty)> {
        self.enums
            .get(enum_name)
            .and_then(|enum_def| Self::enum_constructor_info(enum_name, enum_def, constr))
    }

    fn enum_constructor_info(
        enum_name: &Ident,
        enum_def: &EnumDef,
        constr: &Ident,
    ) -> Option<(Constructor, tast::Ty)> {
        enum_def
            .variants
            .iter()
            .enumerate()
            .find(|(_, (variant_name, _))| variant_name == constr)
            .map(|(index, _)| Self::build_enum_constructor(enum_name, enum_def, index))
    }

    fn build_enum_constructor(
        enum_name: &Ident,
        enum_def: &EnumDef,
        index: usize,
    ) -> (Constructor, tast::Ty) {
        let (_, fields) = &enum_def.variants[index];
        let base = tast::Ty::TCon {
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

        let constructor = Constructor::Enum(tast::EnumConstructor {
            type_name: enum_name.clone(),
            variant: enum_def.variants[index].0.clone(),
            index,
        });
        (constructor, ctor_ty)
    }

    fn lookup_struct_constructor(&self, constr: &Ident) -> Option<(Constructor, tast::Ty)> {
        self.structs.get(constr).map(|struct_def| {
            let base = tast::Ty::TCon {
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

            let constructor = Constructor::Struct(tast::StructConstructor {
                type_name: struct_def.name.clone(),
            });
            (constructor, ctor_ty)
        })
    }

    pub fn get_type_of_function(&self, func: &str) -> Option<tast::Ty> {
        self.funcs.get(func).cloned()
    }

    pub fn lookup_inherent_method(
        &self,
        receiver_ty: &tast::Ty,
        method: &Ident,
    ) -> Option<(String, tast::Ty)> {
        let key = (encode_ty(receiver_ty), method.clone());
        self.inherent_impls.get(&key).cloned()
    }

    pub fn record_tuple_types_from_core(&mut self, file: &core::File) {
        struct TypeCollector {
            tuples: IndexSet<tast::Ty>,
            arrays: IndexSet<tast::Ty>,
            refs: IndexSet<tast::Ty>,
        }

        impl TypeCollector {
            fn new() -> Self {
                Self {
                    tuples: IndexSet::new(),
                    arrays: IndexSet::new(),
                    refs: IndexSet::new(),
                }
            }

            fn finish(
                mut self,
                file: &core::File,
            ) -> (IndexSet<tast::Ty>, IndexSet<tast::Ty>, IndexSet<tast::Ty>) {
                for item in &file.toplevels {
                    self.collect_fn(item);
                }
                (self.tuples, self.arrays, self.refs)
            }

            fn collect_fn(&mut self, item: &core::Fn) {
                for (_, ty) in &item.params {
                    self.collect_type(ty);
                }
                self.collect_type(&item.ret_ty);
                self.collect_expr(&item.body);
            }

            fn collect_expr(&mut self, expr: &core::Expr) {
                match expr {
                    core::Expr::EVar { ty, .. } | core::Expr::EPrim { ty, .. } => {
                        self.collect_type(ty);
                    }
                    core::Expr::EConstr {
                        constructor: _,
                        args,
                        ty,
                    } => {
                        for arg in args {
                            self.collect_expr(arg);
                        }
                        self.collect_type(ty);
                    }
                    core::Expr::ETuple { items, ty } | core::Expr::EArray { items, ty } => {
                        for item in items {
                            self.collect_expr(item);
                        }
                        self.collect_type(ty);
                    }
                    core::Expr::EClosure { params, body, ty } => {
                        for param in params {
                            self.collect_type(&param.get_ty());
                        }
                        self.collect_expr(body);
                        self.collect_type(ty);
                    }
                    core::Expr::ELet {
                        value, body, ty, ..
                    } => {
                        self.collect_expr(value);
                        self.collect_expr(body);
                        self.collect_type(ty);
                    }
                    core::Expr::EMatch {
                        expr,
                        arms,
                        default,
                        ty,
                    } => {
                        self.collect_expr(expr);
                        for arm in arms {
                            self.collect_expr(&arm.lhs);
                            self.collect_expr(&arm.body);
                        }
                        if let Some(default) = default {
                            self.collect_expr(default);
                        }
                        self.collect_type(ty);
                    }
                    core::Expr::EIf {
                        cond,
                        then_branch,
                        else_branch,
                        ty,
                    } => {
                        self.collect_expr(cond);
                        self.collect_expr(then_branch);
                        self.collect_expr(else_branch);
                        self.collect_type(ty);
                    }
                    core::Expr::EWhile { cond, body, ty } => {
                        self.collect_expr(cond);
                        self.collect_expr(body);
                        self.collect_type(ty);
                    }
                    core::Expr::EConstrGet {
                        expr,
                        constructor: _,
                        ty,
                        ..
                    } => {
                        self.collect_expr(expr);
                        self.collect_type(ty);
                    }
                    core::Expr::ECall { func, args, ty } => {
                        self.collect_expr(func);
                        for arg in args {
                            self.collect_expr(arg);
                        }
                        self.collect_type(ty);
                    }
                    core::Expr::EProj { tuple, ty, .. } => {
                        self.collect_expr(tuple);
                        self.collect_type(ty);
                    }
                }
            }

            fn collect_type(&mut self, ty: &tast::Ty) {
                match ty {
                    tast::Ty::TTuple { typs } => {
                        if self.tuples.insert(ty.clone()) {
                            for inner in typs {
                                self.collect_type(inner);
                            }
                        }
                    }
                    tast::Ty::TArray { elem, .. } => {
                        if self.arrays.insert(ty.clone()) {
                            self.collect_type(elem);
                        }
                    }
                    tast::Ty::TRef { elem } => {
                        if self.refs.insert(ty.clone()) {
                            self.collect_type(elem);
                        }
                    }
                    tast::Ty::TCon { .. } => {}
                    tast::Ty::TApp { ty, args } => {
                        self.collect_type(ty.as_ref());
                        for arg in args {
                            self.collect_type(arg);
                        }
                    }
                    tast::Ty::TFunc { params, ret_ty } => {
                        for param in params {
                            self.collect_type(param);
                        }
                        self.collect_type(ret_ty);
                    }
                    _ => {}
                }
            }
        }

        let (tuples, arrays, refs) = TypeCollector::new().finish(file);
        self.tuple_types = tuples;
        self.array_types = arrays;
        self.ref_types = refs;
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

#[derive(Debug, Clone)]
pub struct LocalTypeEnv {
    scopes: Vec<ImHashMap<Ident, tast::Ty>>,
    tparams_env: Vec<Ident>,
    capture_stack: Vec<IndexMap<Ident, tast::Ty>>,
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

    pub fn insert_var(&mut self, name: &Ident, ty: tast::Ty) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.clone(), ty);
        }
    }

    pub fn lookup_var(&mut self, name: &Ident) -> Option<tast::Ty> {
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

    pub fn set_tparams_env(&mut self, params: &[Ident]) {
        self.tparams_env = params.to_vec();
    }

    pub fn clear_tparams_env(&mut self) {
        self.tparams_env.clear();
    }

    pub fn current_tparams_env(&self) -> Vec<Ident> {
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
