use ast::ast::{Lident, Uident};
use indexmap::IndexMap;

use crate::{
    core,
    env::{Env, StructDef},
    tast::{self, Constructor, StructConstructor, Ty},
};

struct ClosureTypeInfo {
    apply_fn: String,
}

struct State<'env> {
    env: &'env mut Env,
    next_id: usize,
    new_functions: Vec<core::Fn>,
    closure_types: IndexMap<String, ClosureTypeInfo>,
}

impl<'env> State<'env> {
    fn new(env: &'env mut Env) -> Self {
        Self {
            env,
            next_id: 0,
            new_functions: Vec::new(),
            closure_types: IndexMap::new(),
        }
    }

    fn fresh_struct_name(&mut self) -> Uident {
        let name = format!("ClosureEnv{}", self.next_id);
        self.next_id += 1;
        Uident::new(&name)
    }

    fn register_closure_type(&mut self, struct_name: &Uident, apply_fn: String) {
        self.closure_types
            .insert(struct_name.0.clone(), ClosureTypeInfo { apply_fn });
    }

    fn closure_struct_for_ty(&self, ty: &Ty) -> Option<String> {
        match ty {
            Ty::TCon { name } => self.closure_types.contains_key(name).then(|| name.clone()),
            _ => None,
        }
    }

    fn apply_fn_for_struct(&self, struct_name: &str) -> Option<&str> {
        self.closure_types
            .get(struct_name)
            .map(|info| info.apply_fn.as_str())
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

pub fn lambda_lift(env: &mut Env, file: core::File) -> core::File {
    let mut state = State::new(env);
    let mut toplevels = Vec::new();

    for mut f in file.toplevels.into_iter() {
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

        let body = transform_expr(&mut state, &mut scope, f.body);
        scope.pop_layer();
        f.body = body;
        toplevels.push(f);
    }

    toplevels.extend(state.new_functions.into_iter());

    core::File { toplevels }
}

fn transform_expr(state: &mut State<'_>, scope: &mut Scope, expr: core::Expr) -> core::Expr {
    match expr {
        core::Expr::EVar { name, ty } => {
            if let Some(entry) = scope.get(&name) {
                if let Some(struct_name) = entry.closure_struct.clone() {
                    core::Expr::EVar {
                        name,
                        ty: Ty::TCon { name: struct_name },
                    }
                } else {
                    core::Expr::EVar { name, ty }
                }
            } else {
                core::Expr::EVar { name, ty }
            }
        }
        core::Expr::EUnit { .. }
        | core::Expr::EBool { .. }
        | core::Expr::EInt { .. }
        | core::Expr::EString { .. } => expr,
        core::Expr::EConstr {
            constructor,
            args,
            ty,
        } => {
            let args = args
                .into_iter()
                .map(|arg| transform_expr(state, scope, arg))
                .collect();
            core::Expr::EConstr {
                constructor,
                args,
                ty,
            }
        }
        core::Expr::ETuple { items, ty } => {
            let items = items
                .into_iter()
                .map(|item| transform_expr(state, scope, item))
                .collect();
            core::Expr::ETuple { items, ty }
        }
        core::Expr::EArray { items, ty } => {
            let items = items
                .into_iter()
                .map(|item| transform_expr(state, scope, item))
                .collect();
            core::Expr::EArray { items, ty }
        }
        core::Expr::EClosure { params, body, ty } => {
            transform_closure(state, scope, params, body, ty)
        }
        core::Expr::ELet {
            name, value, body, ..
        } => {
            let value = transform_expr(state, scope, *value);
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
            core::Expr::ELet {
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
                .map(|arm| core::Arm {
                    lhs: transform_expr(state, scope, arm.lhs),
                    body: transform_expr(state, scope, arm.body),
                })
                .collect();
            let default = default.map(|d| Box::new(transform_expr(state, scope, *d)));
            core::Expr::EMatch {
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
            core::Expr::EIf {
                cond,
                then_branch,
                else_branch,
                ty,
            }
        }
        core::Expr::EConstrGet {
            expr,
            constructor,
            field_index,
            ty,
        } => {
            let expr = Box::new(transform_expr(state, scope, *expr));
            core::Expr::EConstrGet {
                expr,
                constructor,
                field_index,
                ty,
            }
        }
        core::Expr::ECall { func, args, ty } => {
            let args = args
                .into_iter()
                .map(|arg| transform_expr(state, scope, arg))
                .collect::<Vec<_>>();
            if let Some(entry) = scope.get(&func) {
                if let Some(struct_name) = entry
                    .closure_struct
                    .clone()
                    .or_else(|| state.closure_struct_for_ty(&entry.ty))
                {
                    if let Some(apply_fn) = state.apply_fn_for_struct(&struct_name) {
                        let mut call_args = Vec::with_capacity(args.len() + 1);
                        call_args.push(core::Expr::EVar {
                            name: func,
                            ty: Ty::TCon {
                                name: struct_name.clone(),
                            },
                        });
                        call_args.extend(args);
                        return core::Expr::ECall {
                            func: apply_fn.to_string(),
                            args: call_args,
                            ty,
                        };
                    }
                }
            }
            core::Expr::ECall { func, args, ty }
        }
        core::Expr::EProj { tuple, index, ty } => {
            let tuple = Box::new(transform_expr(state, scope, *tuple));
            core::Expr::EProj { tuple, index, ty }
        }
    }
}

fn transform_closure(
    state: &mut State<'_>,
    scope: &mut Scope,
    params: Vec<tast::ClosureParam>,
    body: Box<core::Expr>,
    ty: Ty,
) -> core::Expr {
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
        match &param.pat {
            tast::Pat::PVar { name, .. } => {
                scope.insert(
                    name.clone(),
                    ScopeEntry {
                        ty: param_ty.clone(),
                        closure_struct: state.closure_struct_for_ty(param_ty),
                    },
                );
                lowered_params.push((name.clone(), param_ty.clone()));
                bound_names.push(name.clone());
            }
            tast::Pat::PWild { .. } => {
                let fresh = state.env.gensym("arg");
                scope.insert(
                    fresh.clone(),
                    ScopeEntry {
                        ty: param_ty.clone(),
                        closure_struct: state.closure_struct_for_ty(param_ty),
                    },
                );
                lowered_params.push((fresh, param_ty.clone()));
            }
            _ => {
                panic!("complex closure parameters are not supported yet");
            }
        }
    }

    let body = transform_expr(state, scope, *body);
    scope.pop_layer();

    let mut captured = IndexMap::new();
    let mut bound = bound_names.clone();
    collect_captured(&body, &mut bound, &mut captured, scope);

    let struct_name = state.fresh_struct_name();
    let env_ty = Ty::TCon {
        name: struct_name.0.clone(),
    };

    let mut struct_fields = Vec::new();
    let mut captured_args = Vec::new();

    for (index, (name, field_ty)) in captured.iter().enumerate() {
        let field_name = make_field_name(name, index);
        struct_fields.push((Lident(field_name), field_ty.clone()));
        captured_args.push(core::Expr::EVar {
            name: name.clone(),
            ty: field_ty.clone(),
        });
    }

    let struct_def = StructDef {
        name: struct_name.clone(),
        generics: Vec::new(),
        fields: struct_fields,
    };
    state.env.structs.insert(struct_name.clone(), struct_def);

    let apply_fn_name = state.env.gensym("__closure_apply");
    state.register_closure_type(&struct_name, apply_fn_name.clone());

    let mut fn_params = Vec::with_capacity(lowered_params.len() + 1);
    let env_param_name = state.env.gensym("env");
    fn_params.push((env_param_name.clone(), env_ty.clone()));
    fn_params.extend(lowered_params.iter().cloned());

    let mut fn_body = body;
    for (index, (name, field_ty)) in captured.iter().enumerate().rev() {
        let field_expr = core::Expr::EConstrGet {
            expr: Box::new(core::Expr::EVar {
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
        fn_body = core::Expr::ELet {
            name: name.clone(),
            value: Box::new(field_expr),
            body: Box::new(fn_body),
            ty: body_ty,
        };
    }

    state.new_functions.push(core::Fn {
        name: apply_fn_name.clone(),
        params: fn_params,
        ret_ty: ret_ty.clone(),
        body: fn_body,
    });

    let mut func_param_tys = Vec::with_capacity(param_tys.len() + 1);
    func_param_tys.push(env_ty.clone());
    func_param_tys.extend(param_tys.clone());
    state.env.funcs.insert(
        apply_fn_name,
        Ty::TFunc {
            params: func_param_tys,
            ret_ty: Box::new(ret_ty),
        },
    );

    core::Expr::EConstr {
        constructor: Constructor::Struct(StructConstructor {
            type_name: struct_name,
        }),
        args: captured_args,
        ty: env_ty,
    }
}

fn collect_captured(
    expr: &core::Expr,
    bound: &mut Vec<String>,
    captured: &mut IndexMap<String, Ty>,
    scope: &Scope,
) {
    match expr {
        core::Expr::EVar { name, .. } => {
            if !bound.iter().any(|n| n == name) {
                if let Some(entry) = scope.get(name) {
                    captured
                        .entry(name.clone())
                        .or_insert_with(|| entry.ty.clone());
                }
            }
        }
        core::Expr::EUnit { .. }
        | core::Expr::EBool { .. }
        | core::Expr::EInt { .. }
        | core::Expr::EString { .. } => {}
        core::Expr::EConstr { args, .. } => {
            for arg in args {
                collect_captured(arg, bound, captured, scope);
            }
        }
        core::Expr::ETuple { items, .. } | core::Expr::EArray { items, .. } => {
            for item in items {
                collect_captured(item, bound, captured, scope);
            }
        }
        core::Expr::EClosure { params, body, .. } => {
            let mut nested_bound = bound.clone();
            for param in params {
                if let tast::Pat::PVar { name, .. } = &param.pat {
                    nested_bound.push(name.clone());
                }
            }
            collect_captured(body, &mut nested_bound, captured, scope);
        }
        core::Expr::ELet {
            name, value, body, ..
        } => {
            collect_captured(value, bound, captured, scope);
            bound.push(name.clone());
            collect_captured(body, bound, captured, scope);
            bound.pop();
        }
        core::Expr::EMatch {
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
        core::Expr::EIf {
            cond,
            then_branch,
            else_branch,
            ..
        } => {
            collect_captured(cond, bound, captured, scope);
            collect_captured(then_branch, bound, captured, scope);
            collect_captured(else_branch, bound, captured, scope);
        }
        core::Expr::EConstrGet { expr, .. } => {
            collect_captured(expr, bound, captured, scope);
        }
        core::Expr::ECall { args, .. } => {
            for arg in args {
                collect_captured(arg, bound, captured, scope);
            }
        }
        core::Expr::EProj { tuple, .. } => {
            collect_captured(tuple, bound, captured, scope);
        }
    }
}

fn make_field_name(name: &str, index: usize) -> String {
    let mut result = String::new();
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() {
            if ch == '/' {
                result.push('_');
            } else {
                result.push(ch);
            }
        } else {
            result.push('_');
        }
    }
    if result.is_empty() {
        result = "field".to_string();
    }
    if result.chars().next().is_some_and(|c| c.is_ascii_digit()) {
        result.insert(0, '_');
    }
    format!("{}_{}", result, index)
}
