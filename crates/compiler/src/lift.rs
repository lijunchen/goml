use ast::ast::Ident;
use indexmap::{IndexMap, IndexSet};
use std::collections::HashMap;

use crate::{
    common::{self, Constructor, Prim, StructConstructor},
    core,
    env::{EnumDef, ExternFunc, ExternType, Gensym, GlobalTypeEnv, StructDef},
    mangle::decode_ty,
    tast::{self, Ty},
};

#[derive(Debug, Clone)]
pub struct GlobalLiftEnv {
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

impl GlobalLiftEnv {
    pub fn from_genv(genv: GlobalTypeEnv) -> Self {
        Self {
            enums: genv.enums,
            structs: genv.structs,
            closure_env_apply: IndexMap::new(),
            trait_defs: genv.trait_defs,
            overloaded_funcs_to_trait_name: genv.overloaded_funcs_to_trait_name,
            trait_impls: genv.trait_impls,
            inherent_impls: genv.inherent_impls,
            funcs: genv.funcs,
            extern_funcs: genv.extern_funcs,
            extern_types: genv.extern_types,
            tuple_types: genv.tuple_types,
            array_types: genv.array_types,
            ref_types: genv.ref_types,
        }
    }
    pub fn register_closure_apply(&mut self, struct_name: &Ident, apply_fn: String) {
        self.closure_env_apply
            .insert(struct_name.0.clone(), apply_fn);
    }

    pub fn enums(&self) -> &IndexMap<Ident, EnumDef> {
        &self.enums
    }

    pub fn struct_def_mut(&mut self, name: &Ident) -> Option<&mut StructDef> {
        self.structs.get_mut(name)
    }

    pub fn insert_struct(&mut self, def: StructDef) {
        self.structs.insert(def.name.clone(), def);
    }

    pub fn structs(&self) -> &IndexMap<Ident, StructDef> {
        &self.structs
    }
}

// Lift IR: Like Core IR but without closures.
// Closures are converted to struct construction + lifted functions.

#[derive(Debug, Clone)]
pub struct LiftFile {
    pub toplevels: Vec<LiftFn>,
}

#[derive(Debug, Clone)]
pub struct LiftFn {
    pub name: String,
    pub params: Vec<(String, Ty)>,
    pub ret_ty: Ty,
    pub body: LiftExpr,
}

#[derive(Debug, Clone)]
pub enum LiftExpr {
    EVar {
        name: String,
        ty: Ty,
    },
    EPrim {
        value: Prim,
        ty: Ty,
    },
    EConstr {
        constructor: Constructor,
        args: Vec<LiftExpr>,
        ty: Ty,
    },
    ETuple {
        items: Vec<LiftExpr>,
        ty: Ty,
    },
    EArray {
        items: Vec<LiftExpr>,
        ty: Ty,
    },
    ELet {
        name: String,
        value: Box<LiftExpr>,
        body: Box<LiftExpr>,
        ty: Ty,
    },
    EMatch {
        expr: Box<LiftExpr>,
        arms: Vec<LiftArm>,
        default: Option<Box<LiftExpr>>,
        ty: Ty,
    },
    EIf {
        cond: Box<LiftExpr>,
        then_branch: Box<LiftExpr>,
        else_branch: Box<LiftExpr>,
        ty: Ty,
    },
    EWhile {
        cond: Box<LiftExpr>,
        body: Box<LiftExpr>,
        ty: Ty,
    },
    EConstrGet {
        expr: Box<LiftExpr>,
        constructor: Constructor,
        field_index: usize,
        ty: Ty,
    },
    ECall {
        func: Box<LiftExpr>,
        args: Vec<LiftExpr>,
        ty: Ty,
    },
    EProj {
        tuple: Box<LiftExpr>,
        index: usize,
        ty: Ty,
    },
}

impl LiftExpr {
    pub fn get_ty(&self) -> Ty {
        match self {
            LiftExpr::EVar { ty, .. } => ty.clone(),
            LiftExpr::EPrim { ty, .. } => ty.clone(),
            LiftExpr::EConstr { ty, .. } => ty.clone(),
            LiftExpr::ETuple { ty, .. } => ty.clone(),
            LiftExpr::EArray { ty, .. } => ty.clone(),
            LiftExpr::ELet { ty, .. } => ty.clone(),
            LiftExpr::EMatch { ty, .. } => ty.clone(),
            LiftExpr::EIf { ty, .. } => ty.clone(),
            LiftExpr::EWhile { ty, .. } => ty.clone(),
            LiftExpr::EConstrGet { ty, .. } => ty.clone(),
            LiftExpr::ECall { ty, .. } => ty.clone(),
            LiftExpr::EProj { ty, .. } => ty.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LiftArm {
    pub lhs: LiftExpr,
    pub body: LiftExpr,
}

struct ClosureTypeInfo {
    apply_fn: String,
}

struct State<'env> {
    liftenv: &'env mut GlobalLiftEnv,
    gensym: &'env Gensym,
    next_id: usize,
    new_functions: Vec<LiftFn>,
    closure_types: IndexMap<String, ClosureTypeInfo>,
    context_stack: Vec<String>,
}

impl<'env> State<'env> {
    fn new(liftenv: &'env mut GlobalLiftEnv, gensym: &'env Gensym) -> Self {
        Self {
            liftenv,
            gensym,
            next_id: 0,
            new_functions: Vec::new(),
            closure_types: IndexMap::new(),
            context_stack: Vec::new(),
        }
    }

    fn fresh_struct_name(&mut self, hint: Option<&str>) -> Ident {
        let name = if let Some(hint) = hint {
            format!("closure_env_{}_{}", hint, self.next_id)
        } else {
            format!("closure_env_{}", self.next_id)
        };
        self.next_id += 1;
        Ident::new(&name)
    }

    fn register_closure_type(&mut self, struct_name: &Ident, apply_fn: String) {
        self.closure_types.insert(
            struct_name.0.clone(),
            ClosureTypeInfo {
                apply_fn: apply_fn.clone(),
            },
        );
        self.liftenv.register_closure_apply(struct_name, apply_fn);
    }

    fn closure_struct_for_ty(&self, ty: &Ty) -> Option<String> {
        match ty {
            Ty::TStruct { name } => self.closure_types.contains_key(name).then(|| name.clone()),
            _ => None,
        }
    }

    fn ty_contains_closure(&self, ty: &Ty) -> bool {
        match ty {
            Ty::TStruct { name } => self.closure_types.contains_key(name),
            Ty::TTuple { typs } => typs.iter().any(|t| self.ty_contains_closure(t)),
            Ty::TArray { elem, .. } => self.ty_contains_closure(elem),
            Ty::TFunc { params, ret_ty } => {
                params.iter().any(|t| self.ty_contains_closure(t))
                    || self.ty_contains_closure(ret_ty)
            }
            Ty::TApp { ty, args } => {
                self.ty_contains_closure(ty) || args.iter().any(|t| self.ty_contains_closure(t))
            }
            _ => false,
        }
    }

    fn apply_fn_for_struct(&self, struct_name: &str) -> Option<&str> {
        self.closure_types
            .get(struct_name)
            .map(|info| info.apply_fn.as_str())
    }

    fn push_context_name(&mut self, name: String) {
        self.context_stack.push(name);
    }

    fn pop_context_name(&mut self) {
        self.context_stack.pop();
    }

    fn current_context_name(&self) -> Option<&str> {
        self.context_stack.last().map(|s| s.as_str())
    }
}

#[derive(Clone)]
struct ScopeEntry {
    ty: Ty,
    closure_struct: Option<String>,
}

struct Scope {
    layers: Vec<IndexMap<String, ScopeEntry>>,
}

impl Scope {
    fn new() -> Self {
        Self {
            layers: vec![IndexMap::new()],
        }
    }

    fn push_layer(&mut self) {
        self.layers.push(IndexMap::new());
    }

    fn pop_layer(&mut self) {
        self.layers.pop();
    }

    fn insert(&mut self, name: String, entry: ScopeEntry) {
        if let Some(layer) = self.layers.last_mut() {
            layer.insert(name, entry);
        }
    }

    fn get(&self, name: &str) -> Option<&ScopeEntry> {
        for layer in self.layers.iter().rev() {
            if let Some(entry) = layer.get(name) {
                return Some(entry);
            }
        }
        None
    }
}

pub fn lambda_lift(
    genv: GlobalTypeEnv,
    gensym: &Gensym,
    file: core::File,
) -> (LiftFile, GlobalLiftEnv) {
    let mut liftenv = GlobalLiftEnv::from_genv(genv);
    let mut state = State::new(&mut liftenv, gensym);
    let mut toplevels = Vec::new();

    for f in file.toplevels.into_iter() {
        let mut scope = Scope::new();
        scope.push_layer();
        for (name, ty) in f.params.iter() {
            let closure_struct = state.closure_struct_for_ty(ty);
            scope.insert(
                name.clone(),
                ScopeEntry {
                    ty: ty.clone(),
                    closure_struct,
                },
            );
        }

        let fn_context = sanitize_env_name(&f.name);
        if let Some(ctx) = fn_context.as_ref() {
            state.push_context_name(ctx.clone());
        }
        let body = transform_expr(&mut state, &mut scope, f.body);
        if fn_context.is_some() {
            state.pop_context_name();
        }
        scope.pop_layer();

        let body_ty = body.get_ty();
        let ret_ty = if body_ty != f.ret_ty && state.ty_contains_closure(&body_ty) {
            body_ty
        } else {
            f.ret_ty.clone()
        };

        let fn_ty = Ty::TFunc {
            params: f.params.iter().map(|(_, ty)| ty.clone()).collect(),
            ret_ty: Box::new(ret_ty.clone()),
        };
        state.liftenv.funcs.insert(f.name.clone(), fn_ty);

        toplevels.push(LiftFn {
            name: f.name,
            params: f.params,
            ret_ty,
            body,
        });
    }

    // Convert newly generated closure apply functions to LiftFn
    for f in state.new_functions.into_iter() {
        toplevels.push(LiftFn {
            name: f.name,
            params: f.params,
            ret_ty: f.ret_ty,
            body: f.body,
        });
    }

    (LiftFile { toplevels }, liftenv)
}

fn transform_expr(state: &mut State<'_>, scope: &mut Scope, expr: core::Expr) -> LiftExpr {
    match expr {
        core::Expr::EVar { name, ty } => {
            if let Some(entry) = scope.get(&name) {
                if let Some(struct_name) = entry.closure_struct.clone() {
                    LiftExpr::EVar {
                        name,
                        ty: Ty::TStruct { name: struct_name },
                    }
                } else {
                    LiftExpr::EVar {
                        name,
                        ty: entry.ty.clone(),
                    }
                }
            } else if let Some(func_ty) = state.liftenv.funcs.get(&name) {
                LiftExpr::EVar {
                    name,
                    ty: func_ty.clone(),
                }
            } else {
                LiftExpr::EVar { name, ty }
            }
        }
        core::Expr::EPrim { value, ty } => LiftExpr::EPrim { value, ty },
        core::Expr::EConstr {
            constructor,
            args,
            ty,
        } => {
            let args = args
                .into_iter()
                .map(|arg| transform_expr(state, scope, arg))
                .collect::<Vec<_>>();

            if let Constructor::Struct(struct_constructor) = &constructor {
                let closure_field_types: Vec<_> = args
                    .iter()
                    .map(|arg| state.closure_struct_for_ty(&arg.get_ty()))
                    .collect();

                if let Some(struct_def) =
                    state.liftenv.struct_def_mut(&struct_constructor.type_name)
                {
                    for (index, closure_struct_name) in closure_field_types.into_iter().enumerate()
                    {
                        if let Some(struct_name) = closure_struct_name {
                            struct_def.fields[index].1 = Ty::TStruct { name: struct_name };
                        }
                    }
                }
            }

            LiftExpr::EConstr {
                constructor,
                args,
                ty,
            }
        }
        core::Expr::ETuple { items, ty: _ } => {
            let items = items
                .into_iter()
                .map(|item| transform_expr(state, scope, item))
                .collect::<Vec<_>>();
            let typs = items.iter().map(|item| item.get_ty()).collect();
            LiftExpr::ETuple {
                items,
                ty: Ty::TTuple { typs },
            }
        }
        core::Expr::EArray { items, ty } => {
            let items = items
                .into_iter()
                .map(|item| transform_expr(state, scope, item))
                .collect();
            LiftExpr::EArray { items, ty }
        }
        core::Expr::EClosure { params, body, ty } => {
            transform_closure(state, scope, params, *body, ty, None)
        }
        core::Expr::ELet {
            name, value, body, ..
        } => {
            let value = match *value {
                core::Expr::EClosure { params, body, ty } => {
                    transform_closure(state, scope, params, *body, ty, Some(name.clone()))
                }
                other => transform_expr(state, scope, other),
            };
            let value_ty = value.get_ty();
            scope.push_layer();
            let closure_struct = state.closure_struct_for_ty(&value_ty);
            scope.insert(
                name.clone(),
                ScopeEntry {
                    ty: value_ty.clone(),
                    closure_struct,
                },
            );
            let body = transform_expr(state, scope, *body);
            scope.pop_layer();
            let body_ty = body.get_ty();
            LiftExpr::ELet {
                name,
                value: Box::new(value),
                body: Box::new(body),
                ty: body_ty,
            }
        }
        core::Expr::EMatch {
            expr,
            arms,
            default,
            ty,
        } => {
            let expr = Box::new(transform_expr(state, scope, *expr));
            let arms = arms
                .into_iter()
                .map(|arm| LiftArm {
                    lhs: transform_expr(state, scope, arm.lhs),
                    body: transform_expr(state, scope, arm.body),
                })
                .collect();
            let default = default.map(|d| Box::new(transform_expr(state, scope, *d)));
            LiftExpr::EMatch {
                expr,
                arms,
                default,
                ty,
            }
        }
        core::Expr::EIf {
            cond,
            then_branch,
            else_branch,
            ty,
        } => {
            let cond = Box::new(transform_expr(state, scope, *cond));
            let then_branch = Box::new(transform_expr(state, scope, *then_branch));
            let else_branch = Box::new(transform_expr(state, scope, *else_branch));
            LiftExpr::EIf {
                cond,
                then_branch,
                else_branch,
                ty,
            }
        }
        core::Expr::EWhile { cond, body, ty } => {
            let cond = Box::new(transform_expr(state, scope, *cond));
            let body = Box::new(transform_expr(state, scope, *body));
            LiftExpr::EWhile { cond, body, ty }
        }
        core::Expr::EConstrGet {
            expr,
            constructor,
            field_index,
            ty,
        } => {
            let expr = Box::new(transform_expr(state, scope, *expr));
            let expr_ty = expr.get_ty();
            let instantiated_ty = match &constructor {
                Constructor::Struct(struct_constructor) => instantiate_struct_field_ty(
                    state,
                    &struct_constructor.type_name,
                    field_index,
                    &expr_ty,
                ),
                Constructor::Enum(enum_constructor) => {
                    instantiate_enum_field_ty(state, enum_constructor, field_index, &expr_ty)
                }
            };
            let mut result_ty = ty;
            if let Some(field_ty) = instantiated_ty
                && (state.ty_contains_closure(&field_ty)
                    || matches!(result_ty, Ty::TParam { .. })
                    || ty_contains_type_param(&result_ty))
            {
                result_ty = field_ty;
            }
            LiftExpr::EConstrGet {
                expr,
                constructor,
                field_index,
                ty: result_ty,
            }
        }
        core::Expr::ECall { func, args, ty } => {
            let func_expr = transform_expr(state, scope, *func);
            let args = args
                .into_iter()
                .map(|arg| transform_expr(state, scope, arg))
                .collect::<Vec<_>>();

            if let LiftExpr::EVar { name, .. } = &func_expr
                && let Some(entry) = scope.get(name)
                && let Some(struct_name) = entry
                    .closure_struct
                    .clone()
                    .or_else(|| state.closure_struct_for_ty(&entry.ty))
                && let Some(apply_fn) = state.apply_fn_for_struct(&struct_name)
            {
                let mut call_args = Vec::with_capacity(args.len() + 1);
                call_args.push(LiftExpr::EVar {
                    name: name.clone(),
                    ty: Ty::TStruct {
                        name: struct_name.clone(),
                    },
                });
                call_args.extend(args);
                let func_ty = entry.ty.clone();
                return LiftExpr::ECall {
                    func: Box::new(LiftExpr::EVar {
                        name: apply_fn.to_string(),
                        ty: func_ty,
                    }),
                    args: call_args,
                    ty,
                };
            }
            let func_ty = func_expr.get_ty();
            let call_ty = match func_ty {
                Ty::TFunc { ref ret_ty, .. } if state.ty_contains_closure(ret_ty) => {
                    *ret_ty.clone()
                }
                _ => ty,
            };
            LiftExpr::ECall {
                func: Box::new(func_expr),
                args,
                ty: call_ty,
            }
        }
        core::Expr::EProj { tuple, index, ty } => {
            let tuple = Box::new(transform_expr(state, scope, *tuple));
            let proj_ty = match tuple.get_ty() {
                Ty::TTuple { typs } if index < typs.len() => typs[index].clone(),
                _ => ty,
            };
            LiftExpr::EProj {
                tuple,
                index,
                ty: proj_ty,
            }
        }
    }
}

fn transform_closure(
    state: &mut State<'_>,
    scope: &mut Scope,
    params: Vec<tast::ClosureParam>,
    body: core::Expr,
    ty: Ty,
    name_hint: Option<String>,
) -> LiftExpr {
    let (param_tys, ret_ty) = match ty.clone() {
        Ty::TFunc { params, ret_ty } => (params, *ret_ty),
        other => {
            panic!("expected function type for closure, found {:?}", other);
        }
    };

    if params.len() != param_tys.len() {
        panic!(
            "mismatched closure params: {} params but {} types",
            params.len(),
            param_tys.len()
        );
    }

    let mut lowered_params = Vec::with_capacity(params.len());
    let mut bound_names = Vec::new();

    scope.push_layer();
    for (param, param_ty) in params.iter().zip(param_tys.iter()) {
        scope.insert(
            param.name.clone(),
            ScopeEntry {
                ty: param_ty.clone(),
                closure_struct: state.closure_struct_for_ty(param_ty),
            },
        );
        lowered_params.push((param.name.clone(), param_ty.clone()));
        bound_names.push(param.name.clone());
    }

    let mut sanitized_hint = name_hint.as_deref().and_then(sanitize_env_name);
    if sanitized_hint.is_none() {
        sanitized_hint = state.current_context_name().and_then(sanitize_env_name);
    }

    if let Some(ref hint) = sanitized_hint {
        state.push_context_name(hint.clone());
    }
    let body = transform_expr(state, scope, body);
    if sanitized_hint.is_some() {
        state.pop_context_name();
    }
    scope.pop_layer();

    let mut captured = IndexMap::new();
    let mut bound = bound_names.clone();
    collect_captured(&body, &mut bound, &mut captured, scope);

    let struct_name = state.fresh_struct_name(sanitized_hint.as_deref());
    let env_ty = Ty::TStruct {
        name: struct_name.0.clone(),
    };

    let mut struct_fields = Vec::new();
    let mut captured_args = Vec::new();

    for (index, (name, field_ty)) in captured.iter().enumerate() {
        let field_name = make_field_name(name, index);
        struct_fields.push((Ident(field_name), field_ty.clone()));
        captured_args.push(LiftExpr::EVar {
            name: name.clone(),
            ty: field_ty.clone(),
        });
    }

    let struct_def = StructDef {
        name: struct_name.clone(),
        generics: Vec::new(),
        fields: struct_fields,
    };
    state.liftenv.insert_struct(struct_def);

    let apply_fn_name = state.gensym.gensym("__closure_apply");
    state.register_closure_type(&struct_name, apply_fn_name.clone());

    let mut fn_params = Vec::with_capacity(lowered_params.len() + 1);
    let env_param_name = state.gensym.gensym("env");
    fn_params.push((env_param_name.clone(), env_ty.clone()));
    fn_params.extend(lowered_params.iter().cloned());

    // Build the function body by prepending let bindings for captured variables
    let mut fn_body = body;
    for (index, (name, field_ty)) in captured.iter().enumerate().rev() {
        let field_expr = LiftExpr::EConstrGet {
            expr: Box::new(LiftExpr::EVar {
                name: env_param_name.clone(),
                ty: env_ty.clone(),
            }),
            constructor: Constructor::Struct(StructConstructor {
                type_name: struct_name.clone(),
            }),
            field_index: index,
            ty: field_ty.clone(),
        };
        let body_ty = fn_body.get_ty();
        fn_body = LiftExpr::ELet {
            name: name.clone(),
            value: Box::new(field_expr),
            body: Box::new(fn_body),
            ty: body_ty,
        };
    }

    // Store the new function (will be converted to LiftFn later in lambda_lift)
    state.new_functions.push(LiftFn {
        name: apply_fn_name.clone(),
        params: fn_params,
        ret_ty: ret_ty.clone(),
        body: fn_body,
    });

    let mut func_param_tys = Vec::with_capacity(param_tys.len() + 1);
    func_param_tys.push(env_ty.clone());
    func_param_tys.extend(param_tys.clone());
    state.liftenv.funcs.insert(
        apply_fn_name,
        Ty::TFunc {
            params: func_param_tys,
            ret_ty: Box::new(ret_ty),
        },
    );

    LiftExpr::EConstr {
        constructor: Constructor::Struct(StructConstructor {
            type_name: struct_name,
        }),
        args: captured_args,
        ty: env_ty,
    }
}

fn collect_captured(
    expr: &LiftExpr,
    bound: &mut Vec<String>,
    captured: &mut IndexMap<String, Ty>,
    scope: &Scope,
) {
    match expr {
        LiftExpr::EVar { name, .. } => {
            if !bound.iter().any(|n| n == name)
                && let Some(entry) = scope.get(name)
            {
                captured
                    .entry(name.clone())
                    .or_insert_with(|| entry.ty.clone());
            }
        }
        LiftExpr::EPrim { .. } => {}
        LiftExpr::EConstr { args, .. } => {
            for arg in args {
                collect_captured(arg, bound, captured, scope);
            }
        }
        LiftExpr::ETuple { items, .. } | LiftExpr::EArray { items, .. } => {
            for item in items {
                collect_captured(item, bound, captured, scope);
            }
        }
        LiftExpr::ELet {
            name, value, body, ..
        } => {
            collect_captured(value, bound, captured, scope);
            bound.push(name.clone());
            collect_captured(body, bound, captured, scope);
            bound.pop();
        }
        LiftExpr::EMatch {
            expr,
            arms,
            default,
            ..
        } => {
            collect_captured(expr, bound, captured, scope);
            for arm in arms {
                collect_captured(&arm.lhs, bound, captured, scope);
                collect_captured(&arm.body, bound, captured, scope);
            }
            if let Some(default) = default {
                collect_captured(default, bound, captured, scope);
            }
        }
        LiftExpr::EIf {
            cond,
            then_branch,
            else_branch,
            ..
        } => {
            collect_captured(cond, bound, captured, scope);
            collect_captured(then_branch, bound, captured, scope);
            collect_captured(else_branch, bound, captured, scope);
        }
        LiftExpr::EWhile { cond, body, .. } => {
            collect_captured(cond, bound, captured, scope);
            collect_captured(body, bound, captured, scope);
        }
        LiftExpr::EConstrGet { expr, .. } => {
            collect_captured(expr, bound, captured, scope);
        }
        LiftExpr::ECall { func, args, .. } => {
            collect_captured(func, bound, captured, scope);
            for arg in args {
                collect_captured(arg, bound, captured, scope);
            }
        }
        LiftExpr::EProj { tuple, .. } => {
            collect_captured(tuple, bound, captured, scope);
        }
    }
}

fn sanitize_env_name(name: &str) -> Option<String> {
    let primary = name.split('/').next().unwrap_or(name);
    let mut sanitized = primary
        .chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect::<String>();
    sanitized = sanitized
        .split('_')
        .filter(|part| !part.is_empty())
        .collect::<Vec<_>>()
        .join("_");
    if sanitized.is_empty() {
        return None;
    }
    if sanitized.chars().next().is_some_and(|c| c.is_ascii_digit()) {
        sanitized.insert(0, '_');
    }
    Some(sanitized)
}

fn make_field_name(name: &str, index: usize) -> String {
    let base = sanitize_env_name(name).unwrap_or_else(|| "field".to_string());
    format!("{}_{}", base, index)
}

fn instantiate_struct_field_ty(
    state: &State<'_>,
    struct_name: &Ident,
    field_index: usize,
    instance_ty: &Ty,
) -> Option<Ty> {
    let struct_def = state.liftenv.structs().get(struct_name)?;
    let (_, raw_field_ty) = struct_def.fields.get(field_index)?;
    if struct_def.generics.is_empty() {
        return Some(raw_field_ty.clone());
    }

    let Some(args) = resolve_type_arguments(&struct_name.0, instance_ty) else {
        return Some(raw_field_ty.clone());
    };
    if args.len() != struct_def.generics.len() {
        return Some(raw_field_ty.clone());
    }

    let subst = struct_def
        .generics
        .iter()
        .zip(args)
        .map(|(g, arg)| (g.0.clone(), arg))
        .collect::<HashMap<_, _>>();

    Some(substitute_ty_params(raw_field_ty, &subst))
}

fn instantiate_enum_field_ty(
    state: &State<'_>,
    constructor: &common::EnumConstructor,
    field_index: usize,
    instance_ty: &Ty,
) -> Option<Ty> {
    let enum_def = state.liftenv.enums().get(&constructor.type_name)?;
    let (_, fields) = enum_def
        .variants
        .iter()
        .find(|(variant, _)| variant == &constructor.variant)?;
    let raw_field_ty = fields.get(field_index)?.clone();
    if enum_def.generics.is_empty() {
        return Some(raw_field_ty);
    }

    let Some(args) = resolve_type_arguments(&enum_def.name.0, instance_ty) else {
        return Some(raw_field_ty);
    };
    if args.len() != enum_def.generics.len() {
        return Some(raw_field_ty);
    }

    let subst = enum_def
        .generics
        .iter()
        .zip(args)
        .map(|(g, arg)| (g.0.clone(), arg))
        .collect::<HashMap<_, _>>();

    Some(substitute_ty_params(&raw_field_ty, &subst))
}

fn substitute_ty_params(ty: &Ty, subst: &HashMap<String, Ty>) -> Ty {
    match ty {
        Ty::TUnit
        | Ty::TBool
        | Ty::TInt8
        | Ty::TInt16
        | Ty::TInt32
        | Ty::TInt64
        | Ty::TUint8
        | Ty::TUint16
        | Ty::TUint32
        | Ty::TUint64
        | Ty::TFloat32
        | Ty::TFloat64
        | Ty::TString => ty.clone(),
        Ty::TEnum { .. } | Ty::TStruct { .. } => ty.clone(),
        Ty::TVar { .. } => ty.clone(),
        Ty::TParam { name } => subst.get(name).cloned().unwrap_or_else(|| ty.clone()),
        Ty::TFunc { params, ret_ty } => Ty::TFunc {
            params: params
                .iter()
                .map(|t| substitute_ty_params(t, subst))
                .collect(),
            ret_ty: Box::new(substitute_ty_params(ret_ty, subst)),
        },
        Ty::TTuple { typs } => Ty::TTuple {
            typs: typs
                .iter()
                .map(|t| substitute_ty_params(t, subst))
                .collect(),
        },
        Ty::TArray { len, elem } => Ty::TArray {
            len: *len,
            elem: Box::new(substitute_ty_params(elem, subst)),
        },
        Ty::TRef { elem } => Ty::TRef {
            elem: Box::new(substitute_ty_params(elem, subst)),
        },
        Ty::TApp { ty, args } => Ty::TApp {
            ty: Box::new(substitute_ty_params(ty, subst)),
            args: args
                .iter()
                .map(|t| substitute_ty_params(t, subst))
                .collect(),
        },
    }
}

fn ty_contains_type_param(ty: &Ty) -> bool {
    match ty {
        Ty::TParam { .. } => true,
        Ty::TArray { elem, .. } | Ty::TRef { elem } => ty_contains_type_param(elem),
        Ty::TTuple { typs } => typs.iter().any(ty_contains_type_param),
        Ty::TFunc { params, ret_ty } => {
            params.iter().any(ty_contains_type_param) || ty_contains_type_param(ret_ty)
        }
        Ty::TApp { ty, args } => {
            ty_contains_type_param(ty) || args.iter().any(ty_contains_type_param)
        }
        _ => false,
    }
}

fn resolve_type_arguments(base_name: &str, instance_ty: &Ty) -> Option<Vec<Ty>> {
    match instance_ty {
        Ty::TApp { ty, args } => {
            if let Ty::TEnum { name } | Ty::TStruct { name } = ty.as_ref()
                && name == base_name
            {
                return Some(args.clone());
            }
            None
        }
        Ty::TEnum { name } | Ty::TStruct { name } => {
            let prefix = format!("{}__", base_name);
            let encoded = name.strip_prefix(&prefix)?;
            decode_encoded_type_list(encoded)
        }
        _ => None,
    }
}

fn decode_encoded_type_list(encoded: &str) -> Option<Vec<Ty>> {
    if encoded.is_empty() {
        return Some(Vec::new());
    }
    let tokens: Vec<&str> = encoded.split('_').collect();
    let mut items = Vec::new();
    let mut cur = 0;
    while cur < tokens.len() {
        let mut found = None;
        for mid in (cur + 1..=tokens.len()).rev() {
            let slice = tokens[cur..mid].join("_");
            if let Ok(ty) = decode_ty(&slice) {
                items.push(ty);
                found = Some(mid);
                break;
            }
        }
        let next = found?;
        cur = next;
    }
    Some(items)
}
