use ast::ast::{Lident, Uident};
use diagnostics::{Diagnostic, Diagnostics, Severity, Stage};
use indexmap::{IndexMap, IndexSet};

use crate::{
    core,
    tast::{self, Constructor},
    ty_codec::encode_ty,
};
use std::{cell::Cell, collections::HashMap};

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

    funcs
}

pub fn encode_trait_impl(trait_name: &Uident, type_name: &tast::Ty) -> String {
    format!("__{}%{}", trait_name.0, encode_ty(type_name))
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
    pub trait_defs: IndexMap<(String, String), tast::Ty>,
    pub overloaded_funcs_to_trait_name: IndexMap<String, Uident>,
    pub trait_impls: IndexMap<(String, String, Lident), tast::Ty>,
    pub funcs: IndexMap<String, tast::Ty>,
    pub constraints: Vec<Constraint>,
    pub tuple_types: IndexSet<tast::Ty>,
    pub diagnostics: Diagnostics,
}

fn type_constructor_arity(env: &Env, name: &str) -> Option<usize> {
    let ident = Uident::new(name);
    if let Some(def) = env.structs.get(&ident) {
        Some(def.generics.len())
    } else if let Some(def) = env.enums.get(&ident) {
        Some(def.generics.len())
    } else {
        None
    }
}

fn decode_type_sequence(
    env: &Env,
    encoded: &str,
    expected_count: Option<usize>,
) -> Result<Vec<tast::Ty>, String> {
    if encoded.is_empty() {
        return match expected_count {
            Some(0) | None => Ok(vec![]),
            Some(expected) => Err(format!(
                "expected {} type arguments but encoding was empty",
                expected
            )),
        };
    }

    let tokens = encoded.split('_').collect::<Vec<_>>();
    let mut memo: HashMap<(usize, usize), Option<Vec<tast::Ty>>> = HashMap::new();

    fn helper(
        env: &Env,
        tokens: &[&str],
        start: usize,
        remaining: Option<usize>,
        memo: &mut HashMap<(usize, usize), Option<Vec<tast::Ty>>>,
    ) -> Option<Vec<tast::Ty>> {
        let key = (start, remaining.unwrap_or(usize::MAX));
        if let Some(cached) = memo.get(&key) {
            return cached.clone();
        }

        if start == tokens.len() {
            let res = match remaining {
                Some(0) | None => Some(vec![]),
                _ => None,
            };
            memo.insert(key, res.clone());
            return res;
        }

        if let Some(0) = remaining {
            memo.insert(key, None);
            return None;
        }

        let tokens_left = tokens.len() - start;
        if let Some(rem) = remaining {
            if rem > tokens_left {
                memo.insert(key, None);
                return None;
            }
        }

        for k in 1..=tokens_left {
            if let Some(rem) = remaining {
                if tokens_left - k < rem.saturating_sub(1) {
                    continue;
                }
            }

            let prefix = tokens[start..start + k].join("_");
            if let Ok(first) = decode_type_internal(env, &prefix) {
                let next_remaining = remaining.map(|r| r - 1);
                if let Some(mut rest) = helper(env, tokens, start + k, next_remaining, memo) {
                    let mut result = Vec::with_capacity(1 + rest.len());
                    result.push(first);
                    result.append(&mut rest);
                    memo.insert(key, Some(result.clone()));
                    return Some(result);
                }
            }
        }

        memo.insert(key, None);
        None
    }

    helper(env, &tokens, 0, expected_count, &mut memo)
        .ok_or_else(|| format!("failed to decode type list from `{}`", encoded))
}

fn decode_type_internal(env: &Env, encoded: &str) -> Result<tast::Ty, String> {
    if encoded.is_empty() {
        return Err("empty type encoding".to_string());
    }

    match encoded {
        "unit" => return Ok(tast::Ty::TUnit),
        "bool" => return Ok(tast::Ty::TBool),
        "int" => return Ok(tast::Ty::TInt),
        "string" => return Ok(tast::Ty::TString),
        "Var" => {
            return Err(
                "type variable encodings are not supported in trait impl names".to_string(),
            );
        }
        _ => {}
    }

    if let Some(rest) = encoded.strip_prefix("TParam_") {
        return Ok(tast::Ty::TParam {
            name: rest.to_string(),
        });
    }

    if let Some(rest) = encoded.strip_prefix("Tuple_") {
        let elements = decode_type_sequence(env, rest, None)?;
        if elements.len() < 2 {
            return Err(format!(
                "tuple encoding `{}` must contain at least two elements",
                encoded
            ));
        }
        return Ok(tast::Ty::TTuple { typs: elements });
    }

    if let Some(rest) = encoded.strip_prefix("Fn_") {
        let (params_str, ret_str) = rest
            .split_once("_to_")
            .ok_or_else(|| format!("invalid function type encoding `{}`", encoded))?;
        let params = if params_str.is_empty() {
            Vec::new()
        } else {
            decode_type_sequence(env, params_str, None)?
        };
        let ret_ty = Box::new(decode_type_internal(env, ret_str)?);
        return Ok(tast::Ty::TFunc { params, ret_ty });
    }

    if let Some((base, args_str)) = encoded.split_once('_') {
        if matches!(base, "unit" | "bool" | "int" | "string") {
            return Err(format!(
                "primitive type `{}` does not take type arguments",
                base
            ));
        }
        let arity = type_constructor_arity(env, base);
        if let Some(0) = arity {
            return Err(format!("type `{}` does not take type arguments", base));
        }
        let args = decode_type_sequence(env, args_str, arity)?;
        if let Some(expected_count) = arity {
            if args.len() != expected_count {
                return Err(format!(
                    "type `{}` expected {} arguments but decoded {}",
                    base,
                    expected_count,
                    args.len()
                ));
            }
        }
        let base_ty = tast::Ty::TCon {
            name: base.to_string(),
        };
        if args.is_empty() {
            return Ok(base_ty);
        }
        return Ok(tast::Ty::TApp {
            ty: Box::new(base_ty),
            args,
        });
    }

    if let Some(arity) = type_constructor_arity(env, encoded) {
        if arity != 0 {
            return Err(format!(
                "type `{}` expected {} arguments but none were provided",
                encoded, arity
            ));
        }
    }

    Ok(tast::Ty::TCon {
        name: encoded.to_string(),
    })
}

pub fn decode_trait_impl(env: &Env, impl_name: &str) -> (Uident, tast::Ty) {
    let parts: Vec<&str> = impl_name.split('%').collect();
    if parts.len() != 2 {
        panic!("Invalid trait impl name format: {}", impl_name);
    }
    let trait_name = Uident::new(parts[0].trim_start_matches("__"));
    let ty = decode_type_internal(env, parts[1]).unwrap_or_else(|err| {
        panic!("{}", err);
    });
    (trait_name, ty)
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
            funcs: builtin_functions(),
            trait_defs: IndexMap::new(),
            overloaded_funcs_to_trait_name: IndexMap::new(),
            trait_impls: IndexMap::new(),
            constraints: Vec::new(),
            tuple_types: IndexSet::new(),
            diagnostics: Diagnostics::new(),
        }
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

        let seen = TupleTypeCollector::new().finish(file);
        self.tuple_types = seen;
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
