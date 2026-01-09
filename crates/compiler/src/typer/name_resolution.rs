use std::collections::HashSet;

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

impl NameResolution {
    fn fresh_name(&self, name: &str, fir_table: &mut FirTable) -> fir::LocalId {
        fir_table.fresh_local(name)
    }

    pub fn resolve_file(&self, ast: ast::File) -> (fir::File, FirTable) {
        let mut global_funcs: HashSet<String> = env::builtin_function_names().into_iter().collect();
        for item in ast.toplevels.iter() {
            match item {
                ast::Item::Fn(func) => {
                    global_funcs.insert(func.name.0.clone());
                }
                ast::Item::ExternGo(ext) => {
                    global_funcs.insert(ext.goml_name.0.clone());
                }
                ast::Item::ExternType(_) => {}
                _ => {}
            }
        }

        let mut fir_table = FirTable::new();
        let toplevels = ast
            .toplevels
            .iter()
            .map(|it| self.resolve_item(it, &global_funcs, &mut fir_table))
            .collect();

        (fir::File { toplevels }, fir_table)
    }

    pub fn resolve_item(
        &self,
        item: &ast::Item,
        global_funcs: &HashSet<String>,
        fir_table: &mut FirTable,
    ) -> fir::DefId {
        match item {
            ast::Item::EnumDef(e) => fir_table.alloc_def(fir::Def::EnumDef(e.into())),
            ast::Item::StructDef(s) => fir_table.alloc_def(fir::Def::StructDef(s.into())),
            ast::Item::TraitDef(t) => fir_table.alloc_def(fir::Def::TraitDef(t.into())),
            ast::Item::ImplBlock(i) => {
                let methods = i
                    .methods
                    .iter()
                    .map(|m| self.resolve_fn_def(m, global_funcs, fir_table))
                    .collect();
                fir_table.alloc_def(fir::Def::ImplBlock(fir::ImplBlock {
                    attrs: i.attrs.iter().map(|a| a.into()).collect(),
                    generics: i.generics.iter().map(|g| FirIdent::new(-1, &g.0)).collect(),
                    trait_name: i.trait_name.as_ref().map(|t| FirIdent::new(-1, &t.0)),
                    for_type: (&i.for_type).into(),
                    methods,
                }))
            }
            ast::Item::Fn(func) => self.resolve_fn_def(func, global_funcs, fir_table),
            ast::Item::ExternGo(ext) => fir_table.alloc_def(fir::Def::ExternGo(ext.into())),
            ast::Item::ExternType(ext) => fir_table.alloc_def(fir::Def::ExternType(ext.into())),
            ast::Item::ExternBuiltin(ext) => {
                fir_table.alloc_def(fir::Def::ExternBuiltin(ext.into()))
            }
        }
    }

    pub fn resolve_fn_def(
        &self,
        func: &ast::Fn,
        global_funcs: &HashSet<String>,
        fir_table: &mut FirTable,
    ) -> fir::DefId {
        let func = self.resolve_fn(func, global_funcs, fir_table);
        fir_table.alloc_def(fir::Def::Fn(func))
    }

    pub fn resolve_fn(
        &self,
        func: &ast::Fn,
        global_funcs: &HashSet<String>,
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
            generics: generics.iter().map(|g| FirIdent::new(-1, &g.0)).collect(),
            params: new_params,
            ret_ty: ret_ty.as_ref().map(|t| t.into()),
            body: {
                let body_expr = self.resolve_expr(body, &mut env, global_funcs, fir_table);
                fir_table.alloc_body(fir::Body { expr: body_expr })
            },
        }
    }

    fn resolve_expr(
        &self,
        expr: &ast::Expr,
        env: &mut ResolveLocalEnv,
        global_funcs: &HashSet<String>,
        fir_table: &mut FirTable,
    ) -> fir::ExprId {
        let _ = global_funcs;
        match expr {
            ast::Expr::EPath { path, astptr } => {
                // Only single-segment paths can be local variables
                if path.len() == 1 {
                    let name = path.last_ident().unwrap();
                    if let Some(new_name) = env.rfind(name) {
                        fir_table.alloc_expr(fir::Expr::EVar {
                            name: new_name,
                            astptr: Some(*astptr),
                        })
                    } else {
                        fir_table.alloc_expr(fir::Expr::EPath { path: path.into() })
                    }
                } else {
                    // Multi-segment paths (like Type::method) are kept as-is
                    fir_table.alloc_expr(fir::Expr::EPath { path: path.into() })
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
                    .map(|arg| self.resolve_expr(arg, env, global_funcs, fir_table))
                    .collect();
                fir_table.alloc_expr(fir::Expr::EConstr {
                    constructor: constructor.into(),
                    args: new_args,
                })
            }
            ast::Expr::EStructLiteral { name, fields } => {
                let new_fields = fields
                    .iter()
                    .map(|(field_name, expr)| {
                        (
                            FirIdent::new(-1, &field_name.0),
                            self.resolve_expr(expr, env, global_funcs, fir_table),
                        )
                    })
                    .collect();
                fir_table.alloc_expr(fir::Expr::EStructLiteral {
                    name: FirIdent::new(-1, &name.0),
                    fields: new_fields,
                })
            }
            ast::Expr::ETuple { items } => {
                let new_items = items
                    .iter()
                    .map(|item| self.resolve_expr(item, env, global_funcs, fir_table))
                    .collect();
                fir_table.alloc_expr(fir::Expr::ETuple { items: new_items })
            }
            ast::Expr::EArray { items } => {
                let new_items = items
                    .iter()
                    .map(|item| self.resolve_expr(item, env, global_funcs, fir_table))
                    .collect();
                fir_table.alloc_expr(fir::Expr::EArray { items: new_items })
            }
            ast::Expr::EClosure { params, body } => {
                let mut closure_env = env.enter_scope();
                let new_params = params
                    .iter()
                    .map(|param| self.resolve_closure_param(param, &mut closure_env, fir_table))
                    .collect();
                let new_body_expr =
                    self.resolve_expr(body, &mut closure_env, global_funcs, fir_table);
                let body_id = fir_table.alloc_body(fir::Body {
                    expr: new_body_expr,
                });

                fir_table.alloc_expr(fir::Expr::EClosure {
                    params: new_params,
                    body: body_id,
                })
            }
            ast::Expr::ELet {
                pat,
                annotation,
                value,
            } => {
                let new_value = self.resolve_expr(value, env, global_funcs, fir_table);
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
                let new_expr = self.resolve_expr(expr, env, global_funcs, fir_table);
                let new_arms = arms
                    .iter()
                    .map(|arm| {
                        let new_pat = self.resolve_pat(&arm.pat, env, fir_table);
                        let new_body = self.resolve_expr(&arm.body, env, global_funcs, fir_table);
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
                let new_cond = self.resolve_expr(cond, env, global_funcs, fir_table);
                let new_then = self.resolve_expr(then_branch, env, global_funcs, fir_table);
                let new_else = self.resolve_expr(else_branch, env, global_funcs, fir_table);
                fir_table.alloc_expr(fir::Expr::EIf {
                    cond: new_cond,
                    then_branch: new_then,
                    else_branch: new_else,
                })
            }
            ast::Expr::EWhile { cond, body } => {
                let new_cond = self.resolve_expr(cond, env, global_funcs, fir_table);
                let new_body = self.resolve_expr(body, env, global_funcs, fir_table);
                fir_table.alloc_expr(fir::Expr::EWhile {
                    cond: new_cond,
                    body: new_body,
                })
            }
            ast::Expr::EGo { expr } => {
                let new_expr = self.resolve_expr(expr, env, global_funcs, fir_table);
                fir_table.alloc_expr(fir::Expr::EGo { expr: new_expr })
            }
            ast::Expr::ECall { func, args } => {
                let new_func = match func.as_ref() {
                    ast::Expr::EPath { path, astptr } if path.len() == 1 => {
                        let name = path.last_ident().unwrap();
                        if let Some(new_name) = env.rfind(name) {
                            fir_table.alloc_expr(fir::Expr::EVar {
                                name: new_name,
                                astptr: Some(*astptr),
                            })
                        } else {
                            fir_table.alloc_expr(fir::Expr::EPath { path: path.into() })
                        }
                    }
                    _ => self.resolve_expr(func, env, global_funcs, fir_table),
                };
                let new_args = args
                    .iter()
                    .map(|arg| self.resolve_expr(arg, env, global_funcs, fir_table))
                    .collect();
                fir_table.alloc_expr(fir::Expr::ECall {
                    func: new_func,
                    args: new_args,
                })
            }
            ast::Expr::EUnary { op, expr } => {
                let new_expr = self.resolve_expr(expr, env, global_funcs, fir_table);
                fir_table.alloc_expr(fir::Expr::EUnary {
                    op: *op,
                    expr: new_expr,
                })
            }
            ast::Expr::EBinary { op, lhs, rhs } => {
                let new_lhs = self.resolve_expr(lhs, env, global_funcs, fir_table);
                let new_rhs = self.resolve_expr(rhs, env, global_funcs, fir_table);
                fir_table.alloc_expr(fir::Expr::EBinary {
                    op: *op,
                    lhs: new_lhs,
                    rhs: new_rhs,
                })
            }
            ast::Expr::EProj { tuple, index } => {
                let new_tuple = self.resolve_expr(tuple, env, global_funcs, fir_table);
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
                let new_expr = self.resolve_expr(expr, env, global_funcs, fir_table);
                fir_table.alloc_expr(fir::Expr::EField {
                    expr: new_expr,
                    field: FirIdent::new(-1, &field.0),
                })
            }
            ast::Expr::EBlock { exprs } => {
                let new_exprs = exprs
                    .iter()
                    .map(|e| self.resolve_expr(e, env, global_funcs, fir_table))
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
                    constructor: constructor.into(),
                    args: new_args,
                })
            }
            ast::Pat::PStruct { name, fields } => {
                let new_fields = fields
                    .iter()
                    .map(|(fname, pat)| {
                        (
                            FirIdent::new(-1, &fname.0),
                            self.resolve_pat(pat, env, fir_table),
                        )
                    })
                    .collect();
                fir_table.alloc_pat(fir::Pat::PStruct {
                    name: FirIdent::new(-1, &name.0),
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
