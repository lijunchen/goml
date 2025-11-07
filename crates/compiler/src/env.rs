use ast::ast::{Lident, Uident};
use diagnostics::{Diagnostic, Diagnostics, Severity, Stage};
use indexmap::{IndexMap, IndexSet};

use crate::{
    core,
    tast::{self, Constructor},
    type_encoding::{decode_ty, encode_ty},
};
use std::cell::Cell;

#[derive(Debug, Clone)]
pub struct EnumDef {
    pub name: Uident,
    pub generics: Vec<Uident>,
    pub variants: Vec<(Uident, Vec<tast::Ty>)>,
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: Uident,
    pub generics: Vec<Uident>,
    pub fields: Vec<(Lident, tast::Ty)>,
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

fn builtin_functions() -> IndexMap<String, tast::Ty> {
    let mut funcs = IndexMap::new();

    let make_fn_ty = |params: Vec<tast::Ty>, ret: tast::Ty| tast::Ty::TFunc {
        params,
        ret_ty: Box::new(ret),
    };

    funcs.insert(
        "unit_to_string".to_string(),
        make_fn_ty(vec![tast::Ty::TUnit], tast::Ty::TString),
    );
    funcs.insert(
        "bool_to_string".to_string(),
        make_fn_ty(vec![tast::Ty::TBool], tast::Ty::TString),
    );
    funcs.insert(
        "bool_not".to_string(),
        make_fn_ty(vec![tast::Ty::TBool], tast::Ty::TBool),
    );
    funcs.insert(
        "bool_and".to_string(),
        make_fn_ty(vec![tast::Ty::TBool, tast::Ty::TBool], tast::Ty::TBool),
    );
    funcs.insert(
        "bool_or".to_string(),
        make_fn_ty(vec![tast::Ty::TBool, tast::Ty::TBool], tast::Ty::TBool),
    );

    funcs.insert(
        "int_to_string".to_string(),
        make_fn_ty(vec![tast::Ty::TInt], tast::Ty::TString),
    );
    funcs.insert(
        "int_neg".to_string(),
        make_fn_ty(vec![tast::Ty::TInt], tast::Ty::TInt),
    );
    funcs.insert(
        "int_add".to_string(),
        make_fn_ty(vec![tast::Ty::TInt, tast::Ty::TInt], tast::Ty::TInt),
    );
    funcs.insert(
        "int_sub".to_string(),
        make_fn_ty(vec![tast::Ty::TInt, tast::Ty::TInt], tast::Ty::TInt),
    );
    funcs.insert(
        "int_mul".to_string(),
        make_fn_ty(vec![tast::Ty::TInt, tast::Ty::TInt], tast::Ty::TInt),
    );
    funcs.insert(
        "int_div".to_string(),
        make_fn_ty(vec![tast::Ty::TInt, tast::Ty::TInt], tast::Ty::TInt),
    );
    funcs.insert(
        "int_less".to_string(),
        make_fn_ty(vec![tast::Ty::TInt, tast::Ty::TInt], tast::Ty::TBool),
    );
    funcs.insert(
        "int8_to_string".to_string(),
        make_fn_ty(vec![tast::Ty::TInt8], tast::Ty::TString),
    );
    funcs.insert(
        "int8_neg".to_string(),
        make_fn_ty(vec![tast::Ty::TInt8], tast::Ty::TInt8),
    );
    funcs.insert(
        "int8_add".to_string(),
        make_fn_ty(vec![tast::Ty::TInt8, tast::Ty::TInt8], tast::Ty::TInt8),
    );
    funcs.insert(
        "int8_sub".to_string(),
        make_fn_ty(vec![tast::Ty::TInt8, tast::Ty::TInt8], tast::Ty::TInt8),
    );
    funcs.insert(
        "int8_mul".to_string(),
        make_fn_ty(vec![tast::Ty::TInt8, tast::Ty::TInt8], tast::Ty::TInt8),
    );
    funcs.insert(
        "int8_div".to_string(),
        make_fn_ty(vec![tast::Ty::TInt8, tast::Ty::TInt8], tast::Ty::TInt8),
    );
    funcs.insert(
        "int8_less".to_string(),
        make_fn_ty(vec![tast::Ty::TInt8, tast::Ty::TInt8], tast::Ty::TBool),
    );
    funcs.insert(
        "string_add".to_string(),
        make_fn_ty(
            vec![tast::Ty::TString, tast::Ty::TString],
            tast::Ty::TString,
        ),
    );
    funcs.insert(
        "string_print".to_string(),
        make_fn_ty(vec![tast::Ty::TString], tast::Ty::TUnit),
    );
    funcs.insert(
        "string_println".to_string(),
        make_fn_ty(vec![tast::Ty::TString], tast::Ty::TUnit),
    );

    let array_elem_param = tast::Ty::TParam {
        name: "T".to_string(),
    };
    let array_ty = tast::Ty::TArray {
        len: tast::ARRAY_WILDCARD_LEN,
        elem: Box::new(array_elem_param.clone()),
    };
    funcs.insert(
        "array_get".to_string(),
        make_fn_ty(
            vec![array_ty.clone(), tast::Ty::TInt],
            array_elem_param.clone(),
        ),
    );
    funcs.insert(
        "array_set".to_string(),
        make_fn_ty(
            vec![array_ty.clone(), tast::Ty::TInt, array_elem_param.clone()],
            array_ty,
        ),
    );

    let ref_elem_param = tast::Ty::TParam {
        name: "T".to_string(),
    };
    let ref_ty = tast::Ty::TRef {
        elem: Box::new(ref_elem_param.clone()),
    };
    funcs.insert(
        "ref".to_string(),
        make_fn_ty(
            vec![ref_elem_param.clone()],
            tast::Ty::TRef {
                elem: Box::new(ref_elem_param.clone()),
            },
        ),
    );
    funcs.insert(
        "ref_get".to_string(),
        make_fn_ty(vec![ref_ty.clone()], ref_elem_param.clone()),
    );
    funcs.insert(
        "ref_set".to_string(),
        make_fn_ty(
            vec![ref_ty.clone(), ref_elem_param.clone()],
            tast::Ty::TUnit,
        ),
    );

    funcs.insert(
        "spawn".to_string(),
        make_fn_ty(
            vec![tast::Ty::TFunc {
                params: Vec::new(),
                ret_ty: Box::new(tast::Ty::TUnit),
            }],
            tast::Ty::TUnit,
        ),
    );

    funcs
}

pub fn builtin_function_names() -> Vec<String> {
    builtin_functions().into_keys().collect()
}

pub fn encode_trait_impl(trait_name: &Uident, type_name: &tast::Ty) -> String {
    let trait_name = &trait_name.0;
    let type_repr = encode_ty(type_name);
    format!("__{}%{}", trait_name, type_repr)
}

pub fn decode_trait_impl(impl_name: &str) -> (Uident, tast::Ty) {
    let impl_name = impl_name.strip_prefix("__").unwrap_or(impl_name);
    let (trait_part, type_part) = impl_name
        .split_once('%')
        .expect("Invalid trait impl name format");
    let trait_name = Uident::new(trait_part);
    let ty = decode_ty(type_part)
        .unwrap_or_else(|err| panic!("Failed to decode trait impl type `{}`: {}", type_part, err));
    (trait_name, ty)
}

#[derive(Debug, Clone)]
pub enum Constraint {
    TypeEqual(tast::Ty, tast::Ty),
    Overloaded {
        op: Lident,
        trait_name: Uident,
        call_site_type: tast::Ty,
    },
}

#[derive(Debug, Clone)]
#[allow(unused)]
pub struct Env {
    counter: Cell<i32>,
    pub enums: IndexMap<Uident, EnumDef>,
    pub structs: IndexMap<Uident, StructDef>,
    pub closure_env_apply: IndexMap<String, String>,
    pub trait_defs: IndexMap<(String, String), tast::Ty>,
    pub overloaded_funcs_to_trait_name: IndexMap<String, Uident>,
    pub trait_impls: IndexMap<(String, String, Lident), tast::Ty>,
    pub funcs: IndexMap<String, tast::Ty>,
    pub extern_funcs: IndexMap<String, ExternFunc>,
    pub extern_types: IndexMap<String, ExternType>,
    pub constraints: Vec<Constraint>,
    pub tuple_types: IndexSet<tast::Ty>,
    pub array_types: IndexSet<tast::Ty>,
    pub ref_types: IndexSet<tast::Ty>,
    pub diagnostics: Diagnostics,
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

impl Env {
    pub fn new() -> Self {
        Self {
            counter: Cell::new(0),
            enums: IndexMap::new(),
            structs: IndexMap::new(),
            closure_env_apply: IndexMap::new(),
            funcs: builtin_functions(),
            extern_funcs: IndexMap::new(),
            extern_types: IndexMap::new(),
            trait_defs: IndexMap::new(),
            overloaded_funcs_to_trait_name: IndexMap::new(),
            trait_impls: IndexMap::new(),
            constraints: Vec::new(),
            tuple_types: IndexSet::new(),
            array_types: IndexSet::new(),
            ref_types: IndexSet::new(),
            diagnostics: Diagnostics::new(),
        }
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
            | tast::Ty::TInt
            | tast::Ty::TInt8
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

    pub fn register_closure_apply(&mut self, struct_name: &Uident, apply_fn: String) {
        self.closure_env_apply
            .insert(struct_name.0.clone(), apply_fn);
    }

    pub fn closure_apply_fn(&self, struct_name: &str) -> Option<&str> {
        self.closure_env_apply.get(struct_name).map(|s| s.as_str())
    }

    pub fn report_typer_error(&mut self, message: impl Into<String>) {
        self.diagnostics
            .push(Diagnostic::new(Stage::Typer, Severity::Error, message));
    }

    pub fn extend_diagnostics(&mut self, diagnostics: impl IntoIterator<Item = Diagnostic>) {
        self.diagnostics.extend(diagnostics);
    }

    pub fn get_trait_impl(
        &self,
        trait_name: &Uident,
        type_name: &tast::Ty,
        func_name: &Lident,
    ) -> Option<tast::Ty> {
        self.trait_impls
            .get(&(
                trait_name.0.clone(),
                encode_ty(type_name),
                func_name.clone(),
            ))
            .cloned()
    }

    pub fn get_variant_name(&self, tenum_name: &str, index: i32) -> String {
        let enum_def = self.enums.get(&Uident::new(tenum_name)).unwrap();
        let variant = &enum_def.variants[index as usize];
        format!("{}::{}", enum_def.name.0, variant.0.0)
    }

    pub fn gensym(&self, prefix: &str) -> String {
        let count = self.counter.get();
        self.counter.set(count + 1);
        format!("{}{}", prefix, count)
    }

    #[allow(unused)]
    pub fn reset(&self) {
        self.counter.set(0);
    }

    pub fn lookup_constructor(&self, constr: &Uident) -> Option<(Constructor, tast::Ty)> {
        for (enum_name, enum_def) in self.enums.iter() {
            for (index, (variant_name, fields)) in enum_def.variants.iter().enumerate() {
                if variant_name == constr {
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
                        variant: variant_name.clone(),
                        index,
                    });
                    return Some((constructor, ctor_ty));
                }
            }
        }

        if let Some(struct_def) = self.structs.get(constr) {
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
            return Some((constructor, ctor_ty));
        }

        None
    }

    pub fn get_type_of_constructor(&self, constr: &str) -> Option<tast::Ty> {
        let uident = Uident::new(constr);
        self.lookup_constructor(&uident).map(|(_, ty)| ty)
    }

    pub fn get_index_of_constructor(&self, constr: &str) -> Option<i32> {
        for (_, enum_def) in self.enums.iter() {
            for (index, variant) in enum_def.variants.iter().enumerate() {
                if variant.0.0 == constr {
                    return Some(index as i32);
                }
            }
        }
        None
    }

    pub fn get_type_of_function(&self, func: &str) -> Option<tast::Ty> {
        self.funcs.get(func).cloned()
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
                    core::Expr::EVar { ty, .. }
                    | core::Expr::EUnit { ty }
                    | core::Expr::EBool { ty, .. }
                    | core::Expr::EInt { ty, .. }
                    | core::Expr::EString { ty, .. } => {
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

pub fn format_typer_diagnostics(diagnostics: &Diagnostics) -> Vec<String> {
    diagnostics
        .iter()
        .filter(|diagnostic| {
            diagnostic.severity() == Severity::Error && diagnostic.stage() == &Stage::Typer
        })
        .map(|diagnostic| diagnostic.message().to_string())
        .collect()
}
