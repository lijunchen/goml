use std::cell::Cell;
use std::collections::HashSet;

use ::ast::ast::Ident;
use ast::ast;

use crate::env;

#[derive(Default)]
pub struct Rename {
    counter: Cell<u32>,
}

#[derive(Debug)]
struct Env(im::Vector<(Ident, Ident)>);

impl Env {
    pub fn new() -> Self {
        Self(im::Vector::new())
    }

    #[allow(unused)]
    pub fn enter_scope(&self) -> Self {
        Self(self.0.clone())
    }

    pub fn add(&mut self, name: &Ident, new_name: &Ident) {
        self.0.push_back((name.clone(), new_name.clone()));
    }

    pub fn rfind(&self, key: &Ident) -> Option<&Ident> {
        self.0
            .iter()
            .rfind(|(name, _)| name == key)
            .map(|(_, new_name)| new_name)
    }
}

impl Rename {
    fn fresh_name(&self, name: &str) -> Ident {
        let new_name = format!("{}/{}", name, self.counter.get());
        self.counter.set(self.counter.get() + 1);
        Ident(new_name)
    }

    pub fn rename_file(&self, ast: ast::File) -> ast::File {
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

        ast::File {
            toplevels: ast
                .toplevels
                .iter()
                .map(|it| self.rename_item(it, &global_funcs))
                .collect(),
        }
    }

    pub fn rename_item(&self, item: &ast::Item, global_funcs: &HashSet<String>) -> ast::Item {
        match item {
            ast::Item::EnumDef(_) => item.clone(),
            ast::Item::StructDef(_) => item.clone(),
            ast::Item::TraitDef(_) => item.clone(),
            ast::Item::ImplBlock(i) => ast::Item::ImplBlock(ast::ImplBlock {
                trait_name: i.trait_name.clone(),
                for_type: i.for_type.clone(),
                methods: i
                    .methods
                    .iter()
                    .map(|m| self.rename_fn(m, global_funcs))
                    .collect(),
            }),
            ast::Item::Fn(func) => ast::Item::Fn(self.rename_fn(func, global_funcs)),
            ast::Item::ExternGo(ext) => ast::Item::ExternGo(ext.clone()),
            ast::Item::ExternType(ext) => ast::Item::ExternType(ext.clone()),
        }
    }

    pub fn rename_fn(&self, func: &ast::Fn, global_funcs: &HashSet<String>) -> ast::Fn {
        let ast::Fn {
            name,
            generics,
            params,
            ret_ty,
            body,
        } = func;
        let mut env = Env::new();
        for param in params {
            env.add(&param.0, &self.fresh_name(&param.0.0));
        }
        let new_params = params
            .iter()
            .map(|param| (env.rfind(&param.0).unwrap().clone(), param.1.clone()))
            .collect();
        ast::Fn {
            name: name.clone(),
            generics: generics.clone(),
            params: new_params,
            ret_ty: ret_ty.clone(),
            body: self.rename_expr(body, &mut env, global_funcs),
        }
    }

    fn rename_expr(
        &self,
        expr: &ast::Expr,
        env: &mut Env,
        global_funcs: &HashSet<String>,
    ) -> ast::Expr {
        match expr {
            ast::Expr::EVar { name, astptr } => {
                if let Some(new_name) = env.rfind(name) {
                    ast::Expr::EVar {
                        name: new_name.clone(),
                        astptr: *astptr,
                    }
                } else if global_funcs.contains(&name.0) {
                    ast::Expr::EVar {
                        name: name.clone(),
                        astptr: *astptr,
                    }
                } else {
                    panic!("Variable {} not found in environment", name.0);
                }
            }
            ast::Expr::EUnit => expr.clone(),
            ast::Expr::EBool { .. } => expr.clone(),
            ast::Expr::EInt { .. } => expr.clone(),
            ast::Expr::EFloat { .. } => expr.clone(),
            ast::Expr::EString { .. } => expr.clone(),
            ast::Expr::EConstr { constructor, args } => ast::Expr::EConstr {
                constructor: constructor.clone(),
                args: args
                    .iter()
                    .map(|arg| self.rename_expr(arg, env, global_funcs))
                    .collect(),
            },
            ast::Expr::EStructLiteral { name, fields } => ast::Expr::EStructLiteral {
                name: name.clone(),
                fields: fields
                    .iter()
                    .map(|(field_name, expr)| {
                        (
                            field_name.clone(),
                            self.rename_expr(expr, env, global_funcs),
                        )
                    })
                    .collect(),
            },
            ast::Expr::ETuple { items } => ast::Expr::ETuple {
                items: items
                    .iter()
                    .map(|item| self.rename_expr(item, env, global_funcs))
                    .collect(),
            },
            ast::Expr::EArray { items } => ast::Expr::EArray {
                items: items
                    .iter()
                    .map(|item| self.rename_expr(item, env, global_funcs))
                    .collect(),
            },
            ast::Expr::EClosure { params, body } => {
                let mut closure_env = env.enter_scope();
                let new_params = params
                    .iter()
                    .map(|param| self.rename_closure_param(param, &mut closure_env))
                    .collect();
                let new_body = self.rename_expr(body, &mut closure_env, global_funcs);

                ast::Expr::EClosure {
                    params: new_params,
                    body: Box::new(new_body),
                }
            }
            ast::Expr::ELet {
                pat,
                annotation,
                value,
                body,
            } => {
                let new_value = self.rename_expr(value, env, global_funcs);
                let new_pat = self.rename_pat(pat, env);
                let new_body = self.rename_expr(body, env, global_funcs);
                ast::Expr::ELet {
                    pat: new_pat,
                    annotation: annotation.clone(),
                    value: Box::new(new_value),
                    body: Box::new(new_body),
                }
            }
            ast::Expr::EMatch { expr, arms, astptr } => {
                let new_expr = self.rename_expr(expr, env, global_funcs);
                let new_arms = arms
                    .iter()
                    .map(|arm| {
                        let new_pat = self.rename_pat(&arm.pat, env);
                        let new_body = self.rename_expr(&arm.body, env, global_funcs);
                        ast::Arm {
                            pat: new_pat,
                            body: new_body,
                        }
                    })
                    .collect();
                ast::Expr::EMatch {
                    expr: Box::new(new_expr),
                    arms: new_arms,
                    astptr: *astptr,
                }
            }
            ast::Expr::EIf {
                cond,
                then_branch,
                else_branch,
            } => ast::Expr::EIf {
                cond: Box::new(self.rename_expr(cond, env, global_funcs)),
                then_branch: Box::new(self.rename_expr(then_branch, env, global_funcs)),
                else_branch: Box::new(self.rename_expr(else_branch, env, global_funcs)),
            },
            ast::Expr::EWhile { cond, body } => ast::Expr::EWhile {
                cond: Box::new(self.rename_expr(cond, env, global_funcs)),
                body: Box::new(self.rename_expr(body, env, global_funcs)),
            },
            ast::Expr::ECall { func, args } => {
                let new_func = match func.as_ref() {
                    ast::Expr::EVar { name, astptr } => {
                        if let Some(new_name) = env.rfind(name) {
                            ast::Expr::EVar {
                                name: new_name.clone(),
                                astptr: *astptr,
                            }
                        } else {
                            ast::Expr::EVar {
                                name: name.clone(),
                                astptr: *astptr,
                            }
                        }
                    }
                    _ => self.rename_expr(func, env, global_funcs),
                };
                let new_args = args
                    .iter()
                    .map(|arg| self.rename_expr(arg, env, global_funcs))
                    .collect();
                ast::Expr::ECall {
                    func: Box::new(new_func),
                    args: new_args,
                }
            }
            ast::Expr::EUnary { op, expr } => ast::Expr::EUnary {
                op: *op,
                expr: Box::new(self.rename_expr(expr, env, global_funcs)),
            },
            ast::Expr::EBinary { op, lhs, rhs } => ast::Expr::EBinary {
                op: *op,
                lhs: Box::new(self.rename_expr(lhs, env, global_funcs)),
                rhs: Box::new(self.rename_expr(rhs, env, global_funcs)),
            },
            ast::Expr::EProj { tuple, index } => ast::Expr::EProj {
                tuple: Box::new(self.rename_expr(tuple, env, global_funcs)),
                index: *index,
            },
            ast::Expr::EField {
                expr,
                field,
                astptr,
            } => ast::Expr::EField {
                expr: Box::new(self.rename_expr(expr, env, global_funcs)),
                field: field.clone(),
                astptr: astptr.clone(),
            },
        }
    }

    fn rename_pat(&self, pat: &ast::Pat, env: &mut Env) -> ast::Pat {
        match pat {
            ast::Pat::PVar { name, astptr } => {
                let newname = self.fresh_name(&name.0);
                env.add(name, &newname);
                ast::Pat::PVar {
                    name: newname,
                    astptr: *astptr,
                }
            }
            ast::Pat::PUnit => pat.clone(),
            ast::Pat::PBool { .. } => pat.clone(),
            ast::Pat::PInt { .. } => pat.clone(),
            ast::Pat::PString { .. } => pat.clone(),
            ast::Pat::PConstr { constructor, args } => {
                let new_args = args.iter().map(|arg| self.rename_pat(arg, env)).collect();
                ast::Pat::PConstr {
                    constructor: constructor.clone(),
                    args: new_args,
                }
            }
            ast::Pat::PStruct { name, fields } => {
                let new_fields = fields
                    .iter()
                    .map(|(fname, pat)| (fname.clone(), self.rename_pat(pat, env)))
                    .collect();
                ast::Pat::PStruct {
                    name: name.clone(),
                    fields: new_fields,
                }
            }
            ast::Pat::PTuple { pats } => {
                let new_pats = pats.iter().map(|pat| self.rename_pat(pat, env)).collect();
                ast::Pat::PTuple { pats: new_pats }
            }
            ast::Pat::PWild => pat.clone(),
        }
    }
}

impl Rename {
    fn rename_closure_param(&self, param: &ast::ClosureParam, env: &mut Env) -> ast::ClosureParam {
        let new_name = self.fresh_name(&param.name.0);
        env.add(&param.name, &new_name);
        ast::ClosureParam {
            name: new_name,
            ty: param.ty.clone(),
            astptr: param.astptr,
        }
    }
}
