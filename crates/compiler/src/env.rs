use ast::ast::{Lident, Uident};
use indexmap::{IndexMap, IndexSet};

use crate::{
    core,
    tast::{self, Constructor, ConstructorKind},
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

pub fn encode_trait_impl(trait_name: &Uident, type_name: &tast::Ty) -> String {
    let trait_name = trait_name.0.clone();
    let type_name = type_name.clone();
    // impl ToString for Int
    // __ToString%Int
    format!(
        "__{}%{}",
        trait_name,
        match type_name {
            tast::Ty::TUnit => "Unit".to_string(),
            tast::Ty::TInt => "Int".to_string(),
            tast::Ty::TBool => "Bool".to_string(),
            tast::Ty::TString => "String".to_string(),
            _ => {
                todo!()
            }
        }
    )
}

pub fn decode_trait_impl(impl_name: &str) -> (Uident, tast::Ty) {
    let parts: Vec<&str> = impl_name.split('%').collect();
    if parts.len() != 2 {
        panic!("Invalid trait impl name format");
    }
    let trait_name = Uident::new(parts[0].trim_start_matches("__"));
    let type_name = match parts[1] {
        "Unit" => tast::Ty::TUnit,
        "Bool" => tast::Ty::TBool,
        "Int" => tast::Ty::TInt,
        "String" => tast::Ty::TString,
        _ => {
            todo!()
        }
    };
    (trait_name, type_name)
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

#[derive(Debug)]
#[allow(unused)]
pub struct Env {
    counter: Cell<i32>,
    pub enums: IndexMap<Uident, EnumDef>,
    pub structs: IndexMap<Uident, StructDef>,
    pub trait_defs: IndexMap<String, tast::Ty>,
    pub overloaded_funcs_to_trait_name: IndexMap<String, Uident>,
    pub trait_impls: IndexMap<(String, tast::Ty, Lident), tast::Ty>,
    pub funcs: IndexMap<String, tast::Ty>,
    pub constraints: Vec<Constraint>,
    pub tuple_types: IndexSet<tast::Ty>,
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
            funcs: IndexMap::new(),
            trait_defs: IndexMap::new(),
            overloaded_funcs_to_trait_name: IndexMap::new(),
            trait_impls: IndexMap::new(),
            constraints: Vec::new(),
            tuple_types: IndexSet::new(),
        }
    }

    pub fn get_trait_impl(
        &self,
        trait_name: &Uident,
        type_name: &tast::Ty,
        func_name: &Lident,
    ) -> Option<tast::Ty> {
        self.trait_impls
            .get(&(trait_name.0.clone(), type_name.clone(), func_name.clone()))
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
                    let ret_ty = tast::Ty::TApp {
                        name: enum_name.clone(),
                        args: enum_def
                            .generics
                            .iter()
                            .map(|g| tast::Ty::TParam { name: g.0.clone() })
                            .collect(),
                    };

                    let ctor_ty = if fields.is_empty() {
                        ret_ty.clone()
                    } else {
                        tast::Ty::TFunc {
                            params: fields.clone(),
                            ret_ty: Box::new(ret_ty.clone()),
                        }
                    };

                    let constructor = Constructor {
                        name: variant_name.clone(),
                        kind: ConstructorKind::Enum {
                            type_name: enum_name.clone(),
                            index,
                        },
                    };
                    return Some((constructor, ctor_ty));
                }
            }
        }

        if let Some(struct_def) = self.structs.get(constr) {
            let ret_ty = tast::Ty::TApp {
                name: struct_def.name.clone(),
                args: struct_def
                    .generics
                    .iter()
                    .map(|g| tast::Ty::TParam { name: g.0.clone() })
                    .collect(),
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

            let constructor = Constructor {
                name: struct_def.name.clone(),
                kind: ConstructorKind::Struct {
                    type_name: struct_def.name.clone(),
                },
            };
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
        struct TupleTypeCollector {
            seen: IndexSet<tast::Ty>,
        }

        impl TupleTypeCollector {
            fn new() -> Self {
                Self {
                    seen: IndexSet::new(),
                }
            }

            fn finish(mut self, file: &core::File) -> IndexSet<tast::Ty> {
                for item in &file.toplevels {
                    self.collect_fn(item);
                }
                self.seen
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
                    core::Expr::ETuple { items, ty } => {
                        for item in items {
                            self.collect_expr(item);
                        }
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
                    core::Expr::EConstrGet {
                        expr,
                        constructor: _,
                        ty,
                        ..
                    } => {
                        self.collect_expr(expr);
                        self.collect_type(ty);
                    }
                    core::Expr::ECall { args, ty, .. } => {
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
                    tast::Ty::TTuple { .. } => {
                        if self.seen.insert(ty.clone())
                            && let tast::Ty::TTuple { typs } = ty
                        {
                            for inner in typs {
                                self.collect_type(inner);
                            }
                        }
                    }
                    tast::Ty::TApp { args, .. } => {
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

        let seen = TupleTypeCollector::new().finish(file);
        self.tuple_types = seen;
    }
}
