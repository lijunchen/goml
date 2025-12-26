use std::cell::Cell;
use std::collections::HashSet;

use ast::ast;

use crate::env;
use crate::fir;

#[derive(Default)]
pub struct NameResolution {
    counter: Cell<u32>,
}

#[derive(Debug)]
struct NameResolutionEnv(im::Vector<(ast::Ident, fir::Ident)>);

impl NameResolutionEnv {
    pub fn new() -> Self {
        Self(im::Vector::new())
    }

    #[allow(unused)]
    pub fn enter_scope(&self) -> Self {
        Self(self.0.clone())
    }

    pub fn add(&mut self, name: &ast::Ident, new_name: fir::Ident) {
        self.0.push_back((name.clone(), new_name));
    }

    pub fn rfind(&self, key: &ast::Ident) -> Option<&fir::Ident> {
        self.0
            .iter()
            .rfind(|(name, _)| name == key)
            .map(|(_, new_name)| new_name)
    }
}

impl NameResolution {
    fn fresh_name(&self, name: &str) -> fir::Ident {
        let new_name = format!("{}/{}", name, self.counter.get());
        self.counter.set(self.counter.get() + 1);
        fir::Ident(new_name)
    }

    pub fn resolve_file(&self, ast: ast::File) -> fir::File {
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

        fir::File {
            toplevels: ast
                .toplevels
                .iter()
                .map(|it| self.resolve_item(it, &global_funcs))
                .collect(),
        }
    }

    pub fn resolve_item(&self, item: &ast::Item, global_funcs: &HashSet<String>) -> fir::Item {
        match item {
            ast::Item::EnumDef(e) => fir::Item::EnumDef(e.into()),
            ast::Item::StructDef(s) => fir::Item::StructDef(s.into()),
            ast::Item::TraitDef(t) => fir::Item::TraitDef(t.into()),
            ast::Item::ImplBlock(i) => fir::Item::ImplBlock(fir::ImplBlock {
                attrs: i.attrs.iter().map(|a| a.into()).collect(),
                generics: i.generics.iter().map(|g| g.into()).collect(),
                trait_name: i.trait_name.as_ref().map(|t| t.into()),
                for_type: (&i.for_type).into(),
                methods: i
                    .methods
                    .iter()
                    .map(|m| self.resolve_fn(m, global_funcs))
                    .collect(),
            }),
            ast::Item::Fn(func) => fir::Item::Fn(self.resolve_fn(func, global_funcs)),
            ast::Item::ExternGo(ext) => fir::Item::ExternGo(ext.into()),
            ast::Item::ExternType(ext) => fir::Item::ExternType(ext.into()),
            ast::Item::ExternBuiltin(ext) => fir::Item::ExternBuiltin(ext.into()),
        }
    }

    pub fn resolve_fn(&self, func: &ast::Fn, global_funcs: &HashSet<String>) -> fir::Fn {
        let ast::Fn {
            attrs,
            name,
            generics,
            params,
            ret_ty,
            body,
        } = func;
        let mut env = NameResolutionEnv::new();
        for param in params {
            env.add(&param.0, self.fresh_name(&param.0.0));
        }
        let new_params = params
            .iter()
            .map(|param| (env.rfind(&param.0).unwrap().clone(), (&param.1).into()))
            .collect();
        fir::Fn {
            attrs: attrs.iter().map(|a| a.into()).collect(),
            name: name.into(),
            generics: generics.iter().map(|g| g.into()).collect(),
            params: new_params,
            ret_ty: ret_ty.as_ref().map(|t| t.into()),
            body: self.resolve_expr(body, &mut env, global_funcs),
        }
    }

    fn resolve_expr(
        &self,
        expr: &ast::Expr,
        env: &mut NameResolutionEnv,
        global_funcs: &HashSet<String>,
    ) -> fir::Expr {
        match expr {
            ast::Expr::EPath { path, astptr } => {
                // Only single-segment paths can be local variables
                if path.len() == 1 {
                    let name = path.last_ident().unwrap();
                    if let Some(new_name) = env.rfind(name) {
                        fir::Expr::EPath {
                            path: fir::Path::from_ident(new_name.clone()),
                            astptr: *astptr,
                        }
                    } else if global_funcs.contains(&name.0) {
                        fir::Expr::EPath {
                            path: path.into(),
                            astptr: *astptr,
                        }
                    } else {
                        panic!("Variable {} not found in environment", name.0);
                    }
                } else {
                    // Multi-segment paths (like Type::method) are kept as-is
                    fir::Expr::EPath {
                        path: path.into(),
                        astptr: *astptr,
                    }
                }
            }
            ast::Expr::EUnit => fir::Expr::EUnit,
            ast::Expr::EBool { value } => fir::Expr::EBool { value: *value },
            ast::Expr::EInt { value } => fir::Expr::EInt {
                value: value.clone(),
            },
            ast::Expr::EInt8 { value } => fir::Expr::EInt8 {
                value: value.clone(),
            },
            ast::Expr::EInt16 { value } => fir::Expr::EInt16 {
                value: value.clone(),
            },
            ast::Expr::EInt32 { value } => fir::Expr::EInt32 {
                value: value.clone(),
            },
            ast::Expr::EInt64 { value } => fir::Expr::EInt64 {
                value: value.clone(),
            },
            ast::Expr::EUInt8 { value } => fir::Expr::EUInt8 {
                value: value.clone(),
            },
            ast::Expr::EUInt16 { value } => fir::Expr::EUInt16 {
                value: value.clone(),
            },
            ast::Expr::EUInt32 { value } => fir::Expr::EUInt32 {
                value: value.clone(),
            },
            ast::Expr::EUInt64 { value } => fir::Expr::EUInt64 {
                value: value.clone(),
            },
            ast::Expr::EFloat { value } => fir::Expr::EFloat { value: *value },
            ast::Expr::EFloat32 { value } => fir::Expr::EFloat32 {
                value: value.clone(),
            },
            ast::Expr::EFloat64 { value } => fir::Expr::EFloat64 {
                value: value.clone(),
            },
            ast::Expr::EString { value } => fir::Expr::EString {
                value: value.clone(),
            },
            ast::Expr::EConstr { constructor, args } => fir::Expr::EConstr {
                constructor: constructor.into(),
                args: args
                    .iter()
                    .map(|arg| self.resolve_expr(arg, env, global_funcs))
                    .collect(),
            },
            ast::Expr::EStructLiteral { name, fields } => fir::Expr::EStructLiteral {
                name: name.into(),
                fields: fields
                    .iter()
                    .map(|(field_name, expr)| {
                        (
                            field_name.into(),
                            self.resolve_expr(expr, env, global_funcs),
                        )
                    })
                    .collect(),
            },
            ast::Expr::ETuple { items } => fir::Expr::ETuple {
                items: items
                    .iter()
                    .map(|item| self.resolve_expr(item, env, global_funcs))
                    .collect(),
            },
            ast::Expr::EArray { items } => fir::Expr::EArray {
                items: items
                    .iter()
                    .map(|item| self.resolve_expr(item, env, global_funcs))
                    .collect(),
            },
            ast::Expr::EClosure { params, body } => {
                let mut closure_env = env.enter_scope();
                let new_params = params
                    .iter()
                    .map(|param| self.resolve_closure_param(param, &mut closure_env))
                    .collect();
                let new_body = self.resolve_expr(body, &mut closure_env, global_funcs);

                fir::Expr::EClosure {
                    params: new_params,
                    body: Box::new(new_body),
                }
            }
            ast::Expr::ELet {
                pat,
                annotation,
                value,
            } => {
                let new_value = self.resolve_expr(value, env, global_funcs);
                let new_pat = self.resolve_pat(pat, env);
                fir::Expr::ELet {
                    pat: new_pat,
                    annotation: annotation.as_ref().map(|t| t.into()),
                    value: Box::new(new_value),
                }
            }
            ast::Expr::EMatch { expr, arms, astptr } => {
                let new_expr = self.resolve_expr(expr, env, global_funcs);
                let new_arms = arms
                    .iter()
                    .map(|arm| {
                        let new_pat = self.resolve_pat(&arm.pat, env);
                        let new_body = self.resolve_expr(&arm.body, env, global_funcs);
                        fir::Arm {
                            pat: new_pat,
                            body: new_body,
                        }
                    })
                    .collect();
                fir::Expr::EMatch {
                    expr: Box::new(new_expr),
                    arms: new_arms,
                    astptr: *astptr,
                }
            }
            ast::Expr::EIf {
                cond,
                then_branch,
                else_branch,
            } => fir::Expr::EIf {
                cond: Box::new(self.resolve_expr(cond, env, global_funcs)),
                then_branch: Box::new(self.resolve_expr(then_branch, env, global_funcs)),
                else_branch: Box::new(self.resolve_expr(else_branch, env, global_funcs)),
            },
            ast::Expr::EWhile { cond, body } => fir::Expr::EWhile {
                cond: Box::new(self.resolve_expr(cond, env, global_funcs)),
                body: Box::new(self.resolve_expr(body, env, global_funcs)),
            },
            ast::Expr::EGo { expr } => fir::Expr::EGo {
                expr: Box::new(self.resolve_expr(expr, env, global_funcs)),
            },
            ast::Expr::ECall { func, args } => {
                let new_func = match func.as_ref() {
                    ast::Expr::EPath { path, astptr } if path.len() == 1 => {
                        let name = path.last_ident().unwrap();
                        if let Some(new_name) = env.rfind(name) {
                            fir::Expr::EPath {
                                path: fir::Path::from_ident(new_name.clone()),
                                astptr: *astptr,
                            }
                        } else {
                            fir::Expr::EPath {
                                path: path.into(),
                                astptr: *astptr,
                            }
                        }
                    }
                    _ => self.resolve_expr(func, env, global_funcs),
                };
                let new_args = args
                    .iter()
                    .map(|arg| self.resolve_expr(arg, env, global_funcs))
                    .collect();
                fir::Expr::ECall {
                    func: Box::new(new_func),
                    args: new_args,
                }
            }
            ast::Expr::EUnary { op, expr } => fir::Expr::EUnary {
                op: (*op).into(),
                expr: Box::new(self.resolve_expr(expr, env, global_funcs)),
            },
            ast::Expr::EBinary { op, lhs, rhs } => fir::Expr::EBinary {
                op: (*op).into(),
                lhs: Box::new(self.resolve_expr(lhs, env, global_funcs)),
                rhs: Box::new(self.resolve_expr(rhs, env, global_funcs)),
            },
            ast::Expr::EProj { tuple, index } => fir::Expr::EProj {
                tuple: Box::new(self.resolve_expr(tuple, env, global_funcs)),
                index: *index,
            },
            ast::Expr::EField {
                expr,
                field,
                astptr,
            } => fir::Expr::EField {
                expr: Box::new(self.resolve_expr(expr, env, global_funcs)),
                field: field.into(),
                astptr: *astptr,
            },
            ast::Expr::EBlock { exprs } => {
                let new_exprs = exprs
                    .iter()
                    .map(|e| self.resolve_expr(e, env, global_funcs))
                    .collect();
                fir::Expr::EBlock { exprs: new_exprs }
            }
        }
    }

    fn resolve_pat(&self, pat: &ast::Pat, env: &mut NameResolutionEnv) -> fir::Pat {
        match pat {
            ast::Pat::PVar { name, astptr } => {
                let newname = self.fresh_name(&name.0);
                env.add(name, newname.clone());
                fir::Pat::PVar {
                    name: newname,
                    astptr: *astptr,
                }
            }
            ast::Pat::PUnit => fir::Pat::PUnit,
            ast::Pat::PBool { value } => fir::Pat::PBool { value: *value },
            ast::Pat::PInt { value } => fir::Pat::PInt {
                value: value.clone(),
            },
            ast::Pat::PInt8 { value } => fir::Pat::PInt8 {
                value: value.clone(),
            },
            ast::Pat::PInt16 { value } => fir::Pat::PInt16 {
                value: value.clone(),
            },
            ast::Pat::PInt32 { value } => fir::Pat::PInt32 {
                value: value.clone(),
            },
            ast::Pat::PInt64 { value } => fir::Pat::PInt64 {
                value: value.clone(),
            },
            ast::Pat::PUInt8 { value } => fir::Pat::PUInt8 {
                value: value.clone(),
            },
            ast::Pat::PUInt16 { value } => fir::Pat::PUInt16 {
                value: value.clone(),
            },
            ast::Pat::PUInt32 { value } => fir::Pat::PUInt32 {
                value: value.clone(),
            },
            ast::Pat::PUInt64 { value } => fir::Pat::PUInt64 {
                value: value.clone(),
            },
            ast::Pat::PString { value } => fir::Pat::PString {
                value: value.clone(),
            },
            ast::Pat::PConstr { constructor, args } => {
                let new_args = args.iter().map(|arg| self.resolve_pat(arg, env)).collect();
                fir::Pat::PConstr {
                    constructor: constructor.into(),
                    args: new_args,
                }
            }
            ast::Pat::PStruct { name, fields } => {
                let new_fields = fields
                    .iter()
                    .map(|(fname, pat)| (fname.into(), self.resolve_pat(pat, env)))
                    .collect();
                fir::Pat::PStruct {
                    name: name.into(),
                    fields: new_fields,
                }
            }
            ast::Pat::PTuple { pats } => {
                let new_pats = pats.iter().map(|pat| self.resolve_pat(pat, env)).collect();
                fir::Pat::PTuple { pats: new_pats }
            }
            ast::Pat::PWild => fir::Pat::PWild,
        }
    }
}

impl NameResolution {
    fn resolve_closure_param(
        &self,
        param: &ast::ClosureParam,
        env: &mut NameResolutionEnv,
    ) -> fir::ClosureParam {
        let new_name = self.fresh_name(&param.name.0);
        env.add(&param.name, new_name.clone());
        fir::ClosureParam {
            name: new_name,
            ty: param.ty.as_ref().map(|t| t.into()),
            astptr: param.astptr,
        }
    }
}
