use std::collections::HashMap;

use ast::ast;

use crate::env;
use crate::fir;
use crate::fir::FirIdent;

pub type FirTable = fir::FirTable;

#[derive(Default)]
pub struct NameResolution {}

#[derive(Debug)]
struct ResolveLocalEnv(im::Vector<(ast::AstIdent, fir::LocalId)>);

impl ResolveLocalEnv {
    pub fn new() -> Self {
        Self(im::Vector::new())
    }

    #[allow(unused)]
    pub fn enter_scope(&self) -> Self {
        Self(self.0.clone())
    }

    pub fn add(&mut self, name: &ast::AstIdent, new_name: fir::LocalId) {
        self.0.push_back((name.clone(), new_name));
    }

    pub fn rfind(&self, key: &ast::AstIdent) -> Option<fir::LocalId> {
        self.0
            .iter()
            .rfind(|(name, _)| name == key)
            .map(|(_, new_name)| *new_name)
    }
}

struct ResolutionContext {
    builtin_names: HashMap<String, fir::BuiltinId>,
    def_names: HashMap<String, fir::DefId>,
}

impl ResolutionContext {
    fn resolve_name(
        &self,
        name: &str,
        local_env: &ResolveLocalEnv,
        ast_ident: &ast::AstIdent,
    ) -> fir::NameRef {
        if let Some(local_id) = local_env.rfind(ast_ident) {
            return fir::NameRef::Local(local_id);
        }

        if let Some(&def_id) = self.def_names.get(name) {
            return fir::NameRef::Def(def_id);
        }

        if let Some(&builtin_id) = self.builtin_names.get(name) {
            return fir::NameRef::Builtin(builtin_id);
        }

        fir::NameRef::Unresolved(fir::Path::from_ident(name.to_string()))
    }
}

impl NameResolution {
    fn fresh_name(&self, name: &str, fir_table: &mut FirTable) -> fir::LocalId {
        fir_table.fresh_local(name)
    }

    pub fn resolve_file(&self, ast: ast::File) -> (fir::File, FirTable) {
        let mut fir_table = FirTable::new();

        let mut builtin_names = HashMap::new();
        for name in env::builtin_function_names() {
            if let Some(id) = fir::BuiltinId::from_name(&name) {
                builtin_names.insert(name, id);
            }
        }

        let mut def_names = HashMap::new();
        let mut toplevels = Vec::new();

        for item in ast.toplevels.iter() {
            let def_id = match item {
                ast::Item::Fn(func) => {
                    let id = fir_table.alloc_def(fir::Def::Fn(fir::Fn {
                        attrs: Vec::new(),
                        name: func.name.0.clone(),
                        generics: Vec::new(),
                        params: Vec::new(),
                        ret_ty: None,
                        body: fir::ExprId(0),
                    }));
                    def_names.insert(func.name.0.clone(), id);
                    id
                }
                ast::Item::ExternGo(ext) => {
                    let id = fir_table.alloc_def(fir::Def::ExternGo(ext.into()));
                    def_names.insert(ext.goml_name.0.clone(), id);
                    id
                }
                ast::Item::ExternBuiltin(ext) => {
                    let id = fir_table.alloc_def(fir::Def::ExternBuiltin(ext.into()));
                    def_names.insert(ext.name.0.clone(), id);
                    id
                }
                ast::Item::EnumDef(e) => {
                    let enum_def_id = fir_table.alloc_def(fir::Def::EnumDef(e.into()));
                    let variant_count = e.variants.len();
                    let mut variant_ctors = Vec::with_capacity(variant_count);
                    for variant_idx in 0..variant_count {
                        let ctor_id = fir_table.alloc_constructor(fir::Constructor::EnumVariant {
                            enum_def: enum_def_id,
                            variant_idx,
                        });
                        variant_ctors.push(ctor_id);
                    }
                    if let fir::Def::EnumDef(enum_def) = fir_table.def_mut(enum_def_id) {
                        enum_def.variant_ctors = variant_ctors;
                    }
                    enum_def_id
                }
                ast::Item::StructDef(s) => fir_table.alloc_def(fir::Def::StructDef(s.into())),
                ast::Item::TraitDef(t) => fir_table.alloc_def(fir::Def::TraitDef(t.into())),
                ast::Item::ImplBlock(_i) => {
                    fir_table.alloc_def(fir::Def::ImplBlock(fir::ImplBlock {
                        attrs: Vec::new(),
                        generics: Vec::new(),
                        trait_name: None,
                        for_type: fir::TypeExpr::TUnit,
                        methods: Vec::new(),
                    }))
                }
                ast::Item::ExternType(ext) => fir_table.alloc_def(fir::Def::ExternType(ext.into())),
            };
            toplevels.push(def_id);
        }

        let ctx = ResolutionContext {
            builtin_names,
            def_names,
        };

        let mut toplevels_idx = 0;
        for item in ast.toplevels.iter() {
            match item {
                ast::Item::Fn(func) => {
                    let def_id = toplevels[toplevels_idx];
                    toplevels_idx += 1;
                    let resolved_fn = self.resolve_fn(func, &ctx, &mut fir_table);
                    *fir_table.def_mut(def_id) = fir::Def::Fn(resolved_fn);
                }
                ast::Item::ImplBlock(i) => {
                    let def_id = toplevels[toplevels_idx];
                    toplevels_idx += 1;
                    let methods = i
                        .methods
                        .iter()
                        .map(|m| self.resolve_fn_def(m, &ctx, &mut fir_table))
                        .collect();
                    let impl_block = fir::ImplBlock {
                        attrs: i.attrs.iter().map(|a| a.into()).collect(),
                        generics: i.generics.iter().map(|g| FirIdent::name(&g.0)).collect(),
                        trait_name: i.trait_name.as_ref().map(|t| FirIdent::name(&t.0)),
                        for_type: (&i.for_type).into(),
                        methods,
                    };
                    *fir_table.def_mut(def_id) = fir::Def::ImplBlock(impl_block);
                }
                _ => {
                    toplevels_idx += 1;
                }
            }
        }

        (fir::File { toplevels }, fir_table)
    }

    fn resolve_fn_def(
        &self,
        func: &ast::Fn,
        ctx: &ResolutionContext,
        fir_table: &mut FirTable,
    ) -> fir::DefId {
        let func = self.resolve_fn(func, ctx, fir_table);
        fir_table.alloc_def(fir::Def::Fn(func))
    }

    fn resolve_fn(
        &self,
        func: &ast::Fn,
        ctx: &ResolutionContext,
        fir_table: &mut FirTable,
    ) -> fir::Fn {
        let ast::Fn {
            attrs,
            name,
            generics,
            params,
            ret_ty,
            body,
        } = func;
        let mut env = ResolveLocalEnv::new();
        for param in params {
            env.add(&param.0, self.fresh_name(&param.0.0, fir_table));
        }
        let new_params = params
            .iter()
            .map(|param| (env.rfind(&param.0).unwrap(), (&param.1).into()))
            .collect();
        fir::Fn {
            attrs: attrs.iter().map(|a| a.into()).collect(),
            name: name.0.clone(),
            generics: generics.iter().map(|g| FirIdent::name(&g.0)).collect(),
            params: new_params,
            ret_ty: ret_ty.as_ref().map(|t| t.into()),
            body: self.resolve_expr(body, &mut env, ctx, fir_table),
        }
    }

    fn resolve_expr(
        &self,
        expr: &ast::Expr,
        env: &mut ResolveLocalEnv,
        ctx: &ResolutionContext,
        fir_table: &mut FirTable,
    ) -> fir::ExprId {
        match expr {
            ast::Expr::EPath { path, astptr } => {
                if path.len() == 1 {
                    let ident = path.last_ident().unwrap();
                    let name_str = &ident.0;
                    let res = ctx.resolve_name(name_str, env, ident);
                    fir_table.alloc_expr(fir::Expr::ENameRef {
                        res,
                        hint: name_str.clone(),
                        astptr: Some(*astptr),
                    })
                } else {
                    fir_table.alloc_expr(fir::Expr::ENameRef {
                        res: fir::NameRef::Unresolved(path.into()),
                        hint: path.display(),
                        astptr: Some(*astptr),
                    })
                }
            }
            ast::Expr::EUnit => fir_table.alloc_expr(fir::Expr::EUnit),
            ast::Expr::EBool { value } => fir_table.alloc_expr(fir::Expr::EBool { value: *value }),
            ast::Expr::EInt { value } => fir_table.alloc_expr(fir::Expr::EInt {
                value: value.clone(),
            }),
            ast::Expr::EInt8 { value } => fir_table.alloc_expr(fir::Expr::EInt8 {
                value: value.clone(),
            }),
            ast::Expr::EInt16 { value } => fir_table.alloc_expr(fir::Expr::EInt16 {
                value: value.clone(),
            }),
            ast::Expr::EInt32 { value } => fir_table.alloc_expr(fir::Expr::EInt32 {
                value: value.clone(),
            }),
            ast::Expr::EInt64 { value } => fir_table.alloc_expr(fir::Expr::EInt64 {
                value: value.clone(),
            }),
            ast::Expr::EUInt8 { value } => fir_table.alloc_expr(fir::Expr::EUInt8 {
                value: value.clone(),
            }),
            ast::Expr::EUInt16 { value } => fir_table.alloc_expr(fir::Expr::EUInt16 {
                value: value.clone(),
            }),
            ast::Expr::EUInt32 { value } => fir_table.alloc_expr(fir::Expr::EUInt32 {
                value: value.clone(),
            }),
            ast::Expr::EUInt64 { value } => fir_table.alloc_expr(fir::Expr::EUInt64 {
                value: value.clone(),
            }),
            ast::Expr::EFloat { value } => {
                fir_table.alloc_expr(fir::Expr::EFloat { value: *value })
            }
            ast::Expr::EFloat32 { value } => fir_table.alloc_expr(fir::Expr::EFloat32 {
                value: value.clone(),
            }),
            ast::Expr::EFloat64 { value } => fir_table.alloc_expr(fir::Expr::EFloat64 {
                value: value.clone(),
            }),
            ast::Expr::EString { value } => fir_table.alloc_expr(fir::Expr::EString {
                value: value.clone(),
            }),
            ast::Expr::EConstr { constructor, args } => {
                let new_args = args
                    .iter()
                    .map(|arg| self.resolve_expr(arg, env, ctx, fir_table))
                    .collect();
                fir_table.alloc_expr(fir::Expr::EConstr {
                    constructor: fir::ConstructorRef::Unresolved(constructor.into()),
                    args: new_args,
                })
            }
            ast::Expr::EStructLiteral { name, fields } => {
                let new_fields = fields
                    .iter()
                    .map(|(field_name, expr)| {
                        (
                            FirIdent::name(&field_name.0),
                            self.resolve_expr(expr, env, ctx, fir_table),
                        )
                    })
                    .collect();
                fir_table.alloc_expr(fir::Expr::EStructLiteral {
                    name: FirIdent::name(&name.0),
                    fields: new_fields,
                })
            }
            ast::Expr::ETuple { items } => {
                let new_items = items
                    .iter()
                    .map(|item| self.resolve_expr(item, env, ctx, fir_table))
                    .collect();
                fir_table.alloc_expr(fir::Expr::ETuple { items: new_items })
            }
            ast::Expr::EArray { items } => {
                let new_items = items
                    .iter()
                    .map(|item| self.resolve_expr(item, env, ctx, fir_table))
                    .collect();
                fir_table.alloc_expr(fir::Expr::EArray { items: new_items })
            }
            ast::Expr::EClosure { params, body } => {
                let mut closure_env = env.enter_scope();
                let new_params = params
                    .iter()
                    .map(|param| self.resolve_closure_param(param, &mut closure_env, fir_table))
                    .collect();
                let new_body_expr = self.resolve_expr(body, &mut closure_env, ctx, fir_table);

                fir_table.alloc_expr(fir::Expr::EClosure {
                    params: new_params,
                    body: new_body_expr,
                })
            }
            ast::Expr::ELet {
                pat,
                annotation,
                value,
            } => {
                let new_value = self.resolve_expr(value, env, ctx, fir_table);
                let new_pat = self.resolve_pat(pat, env, fir_table);
                fir_table.alloc_expr(fir::Expr::ELet {
                    pat: new_pat,
                    annotation: annotation.as_ref().map(|t| t.into()),
                    value: new_value,
                })
            }
            ast::Expr::EMatch {
                expr,
                arms,
                astptr: _,
            } => {
                let new_expr = self.resolve_expr(expr, env, ctx, fir_table);
                let new_arms = arms
                    .iter()
                    .map(|arm| {
                        let new_pat = self.resolve_pat(&arm.pat, env, fir_table);
                        let new_body = self.resolve_expr(&arm.body, env, ctx, fir_table);
                        fir::Arm {
                            pat: new_pat,
                            body: new_body,
                        }
                    })
                    .collect();
                fir_table.alloc_expr(fir::Expr::EMatch {
                    expr: new_expr,
                    arms: new_arms,
                })
            }
            ast::Expr::EIf {
                cond,
                then_branch,
                else_branch,
            } => {
                let new_cond = self.resolve_expr(cond, env, ctx, fir_table);
                let new_then = self.resolve_expr(then_branch, env, ctx, fir_table);
                let new_else = self.resolve_expr(else_branch, env, ctx, fir_table);
                fir_table.alloc_expr(fir::Expr::EIf {
                    cond: new_cond,
                    then_branch: new_then,
                    else_branch: new_else,
                })
            }
            ast::Expr::EWhile { cond, body } => {
                let new_cond = self.resolve_expr(cond, env, ctx, fir_table);
                let new_body = self.resolve_expr(body, env, ctx, fir_table);
                fir_table.alloc_expr(fir::Expr::EWhile {
                    cond: new_cond,
                    body: new_body,
                })
            }
            ast::Expr::EGo { expr } => {
                let new_expr = self.resolve_expr(expr, env, ctx, fir_table);
                fir_table.alloc_expr(fir::Expr::EGo { expr: new_expr })
            }
            ast::Expr::ECall { func, args } => {
                let new_func = self.resolve_expr(func, env, ctx, fir_table);
                let new_args = args
                    .iter()
                    .map(|arg| self.resolve_expr(arg, env, ctx, fir_table))
                    .collect();
                fir_table.alloc_expr(fir::Expr::ECall {
                    func: new_func,
                    args: new_args,
                })
            }
            ast::Expr::EUnary { op, expr } => {
                let new_expr = self.resolve_expr(expr, env, ctx, fir_table);
                fir_table.alloc_expr(fir::Expr::EUnary {
                    op: *op,
                    expr: new_expr,
                })
            }
            ast::Expr::EBinary { op, lhs, rhs } => {
                let new_lhs = self.resolve_expr(lhs, env, ctx, fir_table);
                let new_rhs = self.resolve_expr(rhs, env, ctx, fir_table);
                fir_table.alloc_expr(fir::Expr::EBinary {
                    op: *op,
                    lhs: new_lhs,
                    rhs: new_rhs,
                })
            }
            ast::Expr::EProj { tuple, index } => {
                let new_tuple = self.resolve_expr(tuple, env, ctx, fir_table);
                fir_table.alloc_expr(fir::Expr::EProj {
                    tuple: new_tuple,
                    index: *index,
                })
            }
            ast::Expr::EField {
                expr,
                field,
                astptr: _,
            } => {
                let new_expr = self.resolve_expr(expr, env, ctx, fir_table);
                fir_table.alloc_expr(fir::Expr::EField {
                    expr: new_expr,
                    field: FirIdent::name(&field.0),
                })
            }
            ast::Expr::EBlock { exprs } => {
                let new_exprs = exprs
                    .iter()
                    .map(|e| self.resolve_expr(e, env, ctx, fir_table))
                    .collect();
                fir_table.alloc_expr(fir::Expr::EBlock { exprs: new_exprs })
            }
        }
    }

    fn resolve_pat(
        &self,
        pat: &ast::Pat,
        env: &mut ResolveLocalEnv,
        fir_table: &mut FirTable,
    ) -> fir::PatId {
        match pat {
            ast::Pat::PVar { name, astptr } => {
                let newname = self.fresh_name(&name.0, fir_table);
                env.add(name, newname);
                fir_table.alloc_pat(fir::Pat::PVar {
                    name: newname,
                    astptr: *astptr,
                })
            }
            ast::Pat::PUnit => fir_table.alloc_pat(fir::Pat::PUnit),
            ast::Pat::PBool { value } => fir_table.alloc_pat(fir::Pat::PBool { value: *value }),
            ast::Pat::PInt { value } => fir_table.alloc_pat(fir::Pat::PInt {
                value: value.clone(),
            }),
            ast::Pat::PInt8 { value } => fir_table.alloc_pat(fir::Pat::PInt8 {
                value: value.clone(),
            }),
            ast::Pat::PInt16 { value } => fir_table.alloc_pat(fir::Pat::PInt16 {
                value: value.clone(),
            }),
            ast::Pat::PInt32 { value } => fir_table.alloc_pat(fir::Pat::PInt32 {
                value: value.clone(),
            }),
            ast::Pat::PInt64 { value } => fir_table.alloc_pat(fir::Pat::PInt64 {
                value: value.clone(),
            }),
            ast::Pat::PUInt8 { value } => fir_table.alloc_pat(fir::Pat::PUInt8 {
                value: value.clone(),
            }),
            ast::Pat::PUInt16 { value } => fir_table.alloc_pat(fir::Pat::PUInt16 {
                value: value.clone(),
            }),
            ast::Pat::PUInt32 { value } => fir_table.alloc_pat(fir::Pat::PUInt32 {
                value: value.clone(),
            }),
            ast::Pat::PUInt64 { value } => fir_table.alloc_pat(fir::Pat::PUInt64 {
                value: value.clone(),
            }),
            ast::Pat::PString { value } => fir_table.alloc_pat(fir::Pat::PString {
                value: value.clone(),
            }),
            ast::Pat::PConstr { constructor, args } => {
                let new_args = args
                    .iter()
                    .map(|arg| self.resolve_pat(arg, env, fir_table))
                    .collect();
                fir_table.alloc_pat(fir::Pat::PConstr {
                    constructor: fir::ConstructorRef::Unresolved(constructor.into()),
                    args: new_args,
                })
            }
            ast::Pat::PStruct { name, fields } => {
                let new_fields = fields
                    .iter()
                    .map(|(fname, pat)| {
                        (
                            FirIdent::name(&fname.0),
                            self.resolve_pat(pat, env, fir_table),
                        )
                    })
                    .collect();
                fir_table.alloc_pat(fir::Pat::PStruct {
                    name: FirIdent::name(&name.0),
                    fields: new_fields,
                })
            }
            ast::Pat::PTuple { pats } => {
                let new_pats = pats
                    .iter()
                    .map(|pat| self.resolve_pat(pat, env, fir_table))
                    .collect();
                fir_table.alloc_pat(fir::Pat::PTuple { pats: new_pats })
            }
            ast::Pat::PWild => fir_table.alloc_pat(fir::Pat::PWild),
        }
    }

    fn resolve_closure_param(
        &self,
        param: &ast::ClosureParam,
        env: &mut ResolveLocalEnv,
        fir_table: &mut FirTable,
    ) -> fir::ClosureParam {
        let new_name = self.fresh_name(&param.name.0, fir_table);
        env.add(&param.name, new_name);
        fir::ClosureParam {
            name: new_name,
            ty: param.ty.as_ref().map(|t| t.into()),
            astptr: param.astptr,
        }
    }
}
