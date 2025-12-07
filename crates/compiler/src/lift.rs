use ast::ast::Ident;
use indexmap::IndexMap;

use crate::{
    common::{self, Constructor, Prim, StructConstructor},
    core::{BinaryOp, UnaryOp},
    env::{EnumDef, FnOrigin, FnScheme, Gensym, ImplDef, StructDef},
    mangle::{encode_ty, mangle_inherent_name},
    mono::{GlobalMonoEnv, MonoExpr, MonoFile},
    tast::{self, Ty},
};

const CLOSURE_ENV_PREFIX: &str = "closure_env_";
const CLOSURE_APPLY_METHOD: &str = "apply";

/// Checks if a struct name is a closure environment struct.
pub fn is_closure_env_struct(struct_name: &str) -> bool {
    struct_name.starts_with(CLOSURE_ENV_PREFIX)
}

#[derive(Debug, Clone)]
pub struct GlobalLiftEnv {
    pub monoenv: GlobalMonoEnv,
    pub lifted_structs: IndexMap<Ident, StructDef>,
    pub lifted_inherent_impls: IndexMap<String, ImplDef>,
    pub lifted_funcs: IndexMap<String, tast::Ty>,
}

impl GlobalLiftEnv {
    pub fn from_monoenv(monoenv: GlobalMonoEnv) -> Self {
        Self {
            monoenv,
            lifted_structs: IndexMap::new(),
            lifted_inherent_impls: IndexMap::new(),
            lifted_funcs: IndexMap::new(),
        }
    }

    pub fn enums(&self) -> impl Iterator<Item = (&Ident, &EnumDef)> {
        self.monoenv.enums()
    }

    pub fn get_enum(&self, name: &Ident) -> Option<&EnumDef> {
        self.monoenv.get_enum(name)
    }

    pub fn struct_def_mut(&mut self, name: &Ident) -> Option<&mut StructDef> {
        if self.lifted_structs.contains_key(name) {
            self.lifted_structs.get_mut(name)
        } else {
            self.monoenv.struct_def_mut(name)
        }
    }

    pub fn insert_struct(&mut self, def: StructDef) {
        self.lifted_structs.insert(def.name.clone(), def);
    }

    pub fn structs(&self) -> impl Iterator<Item = (&Ident, &StructDef)> {
        self.monoenv.structs().chain(self.lifted_structs.iter())
    }

    pub fn inherent_impls(&self) -> impl Iterator<Item = (&String, &ImplDef)> {
        self.monoenv
            .genv
            .trait_env
            .inherent_impls
            .iter()
            .chain(self.lifted_inherent_impls.iter())
    }

    pub fn lifted_inherent_impls_mut(&mut self) -> &mut IndexMap<String, ImplDef> {
        &mut self.lifted_inherent_impls
    }

    pub fn get_func(&self, name: &str) -> Option<tast::Ty> {
        self.lifted_funcs
            .get(name)
            .cloned()
            .or_else(|| self.monoenv.get_func(name).cloned())
    }

    pub fn insert_func(&mut self, name: String, ty: tast::Ty) {
        self.lifted_funcs.insert(name, ty);
    }

    pub fn get_struct(&self, name: &Ident) -> Option<&StructDef> {
        self.lifted_structs
            .get(name)
            .or_else(|| self.monoenv.get_struct(name))
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
    EGo {
        expr: Box<LiftExpr>,
        ty: Ty,
    },
    EConstrGet {
        expr: Box<LiftExpr>,
        constructor: Constructor,
        field_index: usize,
        ty: Ty,
    },
    EUnary {
        op: UnaryOp,
        expr: Box<LiftExpr>,
        ty: Ty,
    },
    EBinary {
        op: BinaryOp,
        lhs: Box<LiftExpr>,
        rhs: Box<LiftExpr>,
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
            LiftExpr::EGo { ty, .. } => ty.clone(),
            LiftExpr::EConstrGet { ty, .. } => ty.clone(),
            LiftExpr::EUnary { ty, .. } => ty.clone(),
            LiftExpr::EBinary { ty, .. } => ty.clone(),
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
            format!("{}{}_{}", CLOSURE_ENV_PREFIX, hint, self.next_id)
        } else {
            format!("{}{}", CLOSURE_ENV_PREFIX, self.next_id)
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
    monoenv: GlobalMonoEnv,
    gensym: &Gensym,
    file: MonoFile,
) -> (LiftFile, GlobalLiftEnv) {
    let mut liftenv = GlobalLiftEnv::from_monoenv(monoenv);
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
        state.liftenv.insert_func(f.name.clone(), fn_ty);

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

fn transform_expr(state: &mut State<'_>, scope: &mut Scope, expr: MonoExpr) -> LiftExpr {
    match expr {
        MonoExpr::EVar { name, ty } => {
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
            } else if let Some(func_ty) = state.liftenv.get_func(&name) {
                LiftExpr::EVar {
                    name,
                    ty: func_ty.clone(),
                }
            } else {
                LiftExpr::EVar { name, ty }
            }
        }
        MonoExpr::EPrim { value, ty } => LiftExpr::EPrim { value, ty },
        MonoExpr::EConstr {
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
        MonoExpr::ETuple { items, ty: _ } => {
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
        MonoExpr::EArray { items, ty } => {
            let items = items
                .into_iter()
                .map(|item| transform_expr(state, scope, item))
                .collect();
            LiftExpr::EArray { items, ty }
        }
        MonoExpr::EClosure { params, body, ty } => {
            transform_closure(state, scope, params, *body, ty, None)
        }
        MonoExpr::ELet {
            name, value, body, ..
        } => {
            let value = match *value {
                MonoExpr::EClosure { params, body, ty } => {
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
        MonoExpr::EMatch {
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
        MonoExpr::EIf {
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
        MonoExpr::EWhile { cond, body, ty } => {
            let cond = Box::new(transform_expr(state, scope, *cond));
            let body = Box::new(transform_expr(state, scope, *body));
            LiftExpr::EWhile { cond, body, ty }
        }
        MonoExpr::EGo { expr, ty } => {
            let expr = Box::new(transform_expr(state, scope, *expr));
            LiftExpr::EGo { expr, ty }
        }
        MonoExpr::EConstrGet {
            expr,
            constructor,
            field_index,
            ty,
        } => {
            let expr = Box::new(transform_expr(state, scope, *expr));
            let result_ty = match &constructor {
                Constructor::Struct(sc) => {
                    get_struct_field_ty(state, &sc.type_name, field_index).unwrap_or(ty)
                }
                Constructor::Enum(ec) => get_enum_field_ty(state, ec, field_index).unwrap_or(ty),
            };
            LiftExpr::EConstrGet {
                expr,
                constructor,
                field_index,
                ty: result_ty,
            }
        }
        MonoExpr::EUnary { op, expr, ty } => {
            let expr = Box::new(transform_expr(state, scope, *expr));
            LiftExpr::EUnary { op, expr, ty }
        }
        MonoExpr::EBinary { op, lhs, rhs, ty } => {
            let lhs = Box::new(transform_expr(state, scope, *lhs));
            let rhs = Box::new(transform_expr(state, scope, *rhs));
            LiftExpr::EBinary { op, lhs, rhs, ty }
        }
        MonoExpr::ECall { func, args, ty } => {
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
        MonoExpr::EProj { tuple, index, ty } => {
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
    body: MonoExpr,
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

    // Use mangle_inherent_name to generate apply function name as an inherent method
    let apply_fn_name = mangle_inherent_name(&env_ty, CLOSURE_APPLY_METHOD);

    let struct_def = StructDef {
        name: struct_name.clone(),
        generics: Vec::new(),
        fields: struct_fields,
    };
    state.liftenv.insert_struct(struct_def);

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
        params: fn_params.clone(),
        ret_ty: ret_ty.clone(),
        body: fn_body,
    });

    // Build the function type for the apply method
    let func_param_tys: Vec<_> = fn_params.iter().map(|(_, ty)| ty.clone()).collect();
    let apply_fn_ty = Ty::TFunc {
        params: func_param_tys,
        ret_ty: Box::new(ret_ty),
    };

    // Register the apply function both in funcs and as an inherent method
    state
        .liftenv
        .insert_func(apply_fn_name.clone(), apply_fn_ty.clone());

    // Register as inherent method: encoded_env_ty -> ImplDef with apply method
    let encoded_ty = encode_ty(&env_ty);
    let impl_def = state
        .liftenv
        .lifted_inherent_impls_mut()
        .entry(encoded_ty)
        .or_default();
    impl_def.methods.insert(
        CLOSURE_APPLY_METHOD.to_string(),
        FnScheme {
            type_params: vec![],
            constraints: (),
            ty: apply_fn_ty,
            origin: FnOrigin::Compiler,
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
        LiftExpr::EGo { expr, .. } => {
            collect_captured(expr, bound, captured, scope);
        }
        LiftExpr::EConstrGet { expr, .. } => {
            collect_captured(expr, bound, captured, scope);
        }
        LiftExpr::EUnary { expr, .. } => {
            collect_captured(expr, bound, captured, scope);
        }
        LiftExpr::EBinary { lhs, rhs, .. } => {
            collect_captured(lhs, bound, captured, scope);
            collect_captured(rhs, bound, captured, scope);
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

fn get_struct_field_ty(state: &State<'_>, struct_name: &Ident, field_index: usize) -> Option<Ty> {
    let struct_def = state.liftenv.get_struct(struct_name)?;
    let (_, raw_field_ty) = struct_def.fields.get(field_index)?;
    Some(raw_field_ty.clone())
}

fn get_enum_field_ty(
    state: &State<'_>,
    constructor: &common::EnumConstructor,
    field_index: usize,
) -> Option<Ty> {
    let enum_def = state.liftenv.get_enum(&constructor.type_name)?;
    let (_, fields) = enum_def
        .variants
        .iter()
        .find(|(variant, _)| variant == &constructor.variant)?;
    let raw_field_ty = fields.get(field_index)?.clone();
    Some(raw_field_ty)
}
