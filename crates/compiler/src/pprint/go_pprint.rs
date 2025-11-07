use pretty::RcDoc;

use crate::{
    env::Env,
    go::{
        goast::{
            BinaryOp, Block, Expr, Field, File, Fn, ImportDecl, ImportSpec, Interface, Item,
            Method, MethodElem, Package, Receiver, Stmt, Struct, TypeAlias, UnaryOp,
        },
        goty::GoType,
    },
};
fn go_type_name(ty: &GoType) -> String {
    match ty {
        GoType::TVoid => "void".to_string(),
        GoType::TUnit => "struct{}".to_string(),
        GoType::TBool => "bool".to_string(),
        GoType::TInt => "int".to_string(),
        GoType::TInt8 => "int8".to_string(),
        GoType::TString => "string".to_string(),
        GoType::TStruct { name, .. } => name.clone(),
        GoType::TPointer { elem } => format!("*{}", go_type_name(elem)),
        GoType::TName { name } => name.clone(),
        GoType::TArray { len, elem } => format!("[{}]{}", len, go_type_name(elem)),
        GoType::TSlice { elem } => format!("[]{}", go_type_name(elem)),
        GoType::TFunc { .. } => "func".to_string(),
    }
}

fn go_type_doc(ty: &GoType) -> RcDoc<'_, ()> {
    match ty {
        GoType::TFunc { params, ret_ty } => {
            let params_doc = if params.is_empty() {
                RcDoc::nil()
            } else {
                RcDoc::intersperse(params.iter().map(go_type_doc), RcDoc::text(", "))
            };
            let ret_doc = match &**ret_ty {
                GoType::TVoid => RcDoc::nil(),
                other => RcDoc::space().append(go_type_doc(other)),
            };
            RcDoc::text("func(")
                .append(params_doc)
                .append(RcDoc::text(")"))
                .append(ret_doc)
        }
        GoType::TArray { len, elem } => RcDoc::text(format!("[{}]", len)).append(go_type_doc(elem)),
        GoType::TSlice { elem } => RcDoc::text("[]").append(go_type_doc(elem)),
        GoType::TPointer { elem } => RcDoc::text("*").append(go_type_doc(elem)),
        other => RcDoc::text(go_type_name(other)),
    }
}

impl File {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        RcDoc::intersperse(
            self.toplevels.iter().map(|item| item.to_doc(env)),
            RcDoc::hardline().append(RcDoc::hardline()),
        )
        .append(RcDoc::hardline())
    }

    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Item {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        match self {
            Item::Package(package) => package.to_doc(env),
            Item::Import(import_decl) => import_decl.to_doc(env),
            Item::Interface(interface) => interface.to_doc(env),
            Item::Struct(struct_def) => struct_def.to_doc(env),
            Item::TypeAlias(alias) => alias.to_doc(env),
            Item::Fn(func) => func.to_doc(env),
        }
    }

    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Package {
    pub fn to_doc(&self, _env: &Env) -> RcDoc<'_, ()> {
        RcDoc::text("package")
            .append(RcDoc::space())
            .append(RcDoc::text(&self.name))
    }

    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl ImportDecl {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        if self.specs.is_empty() {
            RcDoc::text("import ()")
        } else {
            RcDoc::text("import (")
                .append(
                    RcDoc::hardline()
                        .append(RcDoc::intersperse(
                            self.specs.iter().map(|spec| spec.to_doc(env)),
                            RcDoc::hardline(),
                        ))
                        .nest(4),
                )
                .append(RcDoc::hardline())
                .append(RcDoc::text(")"))
        }
    }

    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl ImportSpec {
    pub fn to_doc(&self, _env: &Env) -> RcDoc<'_, ()> {
        let alias = if let Some(alias) = &self.alias {
            RcDoc::text(alias).append(RcDoc::space())
        } else {
            RcDoc::nil()
        };

        alias
            .append(RcDoc::text("\""))
            .append(RcDoc::text(&self.path))
            .append(RcDoc::text("\""))
    }

    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Interface {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        let name = RcDoc::text(&self.name);
        let methods = if self.methods.is_empty() {
            RcDoc::nil()
        } else {
            RcDoc::hardline()
                .append(RcDoc::intersperse(
                    self.methods.iter().map(|method| method.to_doc(env)),
                    RcDoc::hardline(),
                ))
                .append(RcDoc::hardline())
                .nest(4)
        };

        RcDoc::text("type")
            .append(RcDoc::space())
            .append(name)
            .append(RcDoc::space())
            .append(RcDoc::text("interface"))
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(methods)
            .append(RcDoc::text("}"))
    }

    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl MethodElem {
    pub fn to_doc(&self, _env: &Env) -> RcDoc<'_, ()> {
        let name = RcDoc::text(&self.name);
        let params = if self.params.is_empty() {
            RcDoc::nil()
        } else {
            RcDoc::intersperse(
                self.params.iter().map(|(param_name, param_ty)| {
                    RcDoc::text(param_name)
                        .append(RcDoc::space())
                        .append(go_type_doc(param_ty))
                }),
                RcDoc::text(", "),
            )
        };

        let ret_type = if let Some(ret_ty) = &self.ret {
            RcDoc::space().append(go_type_doc(ret_ty))
        } else {
            RcDoc::nil()
        };

        name.append(RcDoc::text("("))
            .append(params)
            .append(RcDoc::text(")"))
            .append(ret_type)
    }

    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Struct {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        let name = RcDoc::text(&self.name);
        let fields = if self.fields.is_empty() {
            RcDoc::nil()
        } else {
            RcDoc::hardline()
                .append(RcDoc::intersperse(
                    self.fields.iter().map(|field| field.to_doc(env)),
                    RcDoc::hardline(),
                ))
                .append(RcDoc::hardline())
                .nest(4)
        };

        let struct_def = RcDoc::text("type")
            .append(RcDoc::space())
            .append(name)
            .append(RcDoc::space())
            .append(RcDoc::text("struct"))
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(fields)
            .append(RcDoc::text("}"));

        if self.methods.is_empty() {
            struct_def
        } else {
            let methods = RcDoc::intersperse(
                self.methods.iter().map(|method| method.to_doc(env)),
                RcDoc::hardline().append(RcDoc::hardline()),
            );

            struct_def
                .append(RcDoc::hardline())
                .append(RcDoc::hardline())
                .append(methods)
        }
    }

    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl TypeAlias {
    pub fn to_doc(&self, _env: &Env) -> RcDoc<'_, ()> {
        RcDoc::text("type")
            .append(RcDoc::space())
            .append(RcDoc::text(&self.name))
            .append(RcDoc::space())
            .append(RcDoc::text("="))
            .append(RcDoc::space())
            .append(go_type_doc(&self.ty))
    }

    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Field {
    pub fn to_doc(&self, _env: &Env) -> RcDoc<'_, ()> {
        RcDoc::text(&self.name)
            .append(RcDoc::space())
            .append(go_type_doc(&self.ty))
    }

    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Fn {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        let name = RcDoc::text(&self.name);
        let params = if self.params.is_empty() {
            RcDoc::nil()
        } else {
            RcDoc::intersperse(
                self.params.iter().map(|(param_name, param_ty)| {
                    RcDoc::text(param_name)
                        .append(RcDoc::space())
                        .append(go_type_doc(param_ty))
                }),
                RcDoc::text(", "),
            )
        };

        let body = self.body.to_doc(env);

        RcDoc::text("func")
            .append(RcDoc::space())
            .append(name)
            .append(RcDoc::text("("))
            .append(params)
            .append(RcDoc::text(")"))
            .append(RcDoc::space())
            .append(if let Some(ret_ty) = &self.ret_ty {
                go_type_doc(ret_ty).append(RcDoc::space())
            } else {
                RcDoc::nil()
            })
            .append(body)
    }

    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Method {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        let receiver = self.receiver.to_doc(env);
        let name = RcDoc::text(&self.name);
        let params = if self.params.is_empty() {
            RcDoc::nil()
        } else {
            RcDoc::intersperse(
                self.params.iter().map(|(param_name, param_ty)| {
                    RcDoc::text(param_name)
                        .append(RcDoc::space())
                        .append(go_type_doc(param_ty))
                }),
                RcDoc::text(", "),
            )
        };

        let body = self.body.to_doc(env);

        RcDoc::text("func")
            .append(RcDoc::space())
            .append(receiver)
            .append(RcDoc::space())
            .append(name)
            .append(RcDoc::text("("))
            .append(params)
            .append(RcDoc::text(")"))
            .append(RcDoc::space())
            .append(body)
    }

    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Receiver {
    pub fn to_doc(&self, _env: &Env) -> RcDoc<'_, ()> {
        RcDoc::text("(")
            .append(RcDoc::text(&self.name))
            .append(RcDoc::space())
            .append(go_type_doc(&self.ty))
            .append(RcDoc::text(")"))
    }

    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Block {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        if self.stmts.is_empty() {
            RcDoc::text("{}")
        } else {
            let stmts = RcDoc::intersperse(
                self.stmts.iter().map(|stmt| stmt.to_doc(env)),
                RcDoc::hardline(),
            );

            RcDoc::text("{")
                .append(RcDoc::hardline().append(stmts).nest(4))
                .append(RcDoc::hardline())
                .append(RcDoc::text("}"))
        }
    }

    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Stmt {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        match self {
            Stmt::Expr(expr) => expr.to_doc(env),
            Stmt::Go { call } => RcDoc::text("go")
                .append(RcDoc::space())
                .append(call.to_doc(env)),
            Stmt::VarDecl { name, ty, value } => {
                let var_decl = RcDoc::text("var")
                    .append(RcDoc::space())
                    .append(RcDoc::text(name))
                    .append(RcDoc::space())
                    .append(go_type_doc(ty));

                if let Some(value) = value {
                    var_decl
                        .append(RcDoc::space())
                        .append(RcDoc::text("="))
                        .append(RcDoc::space())
                        .append(value.to_doc(env))
                } else {
                    var_decl
                }
            }
            Stmt::Assignment { name, value } => RcDoc::text(name)
                .append(RcDoc::space())
                .append(RcDoc::text("="))
                .append(RcDoc::space())
                .append(value.to_doc(env)),
            Stmt::FieldAssign { target, value } => target
                .to_doc(env)
                .append(RcDoc::space())
                .append(RcDoc::text("="))
                .append(RcDoc::space())
                .append(value.to_doc(env)),
            Stmt::PointerAssign { pointer, value } => RcDoc::text("*")
                .append(pointer.to_doc(env))
                .append(RcDoc::space())
                .append(RcDoc::text("="))
                .append(RcDoc::space())
                .append(value.to_doc(env)),
            Stmt::IndexAssign {
                array,
                index,
                value,
            } => array
                .to_doc(env)
                .append(RcDoc::text("["))
                .append(index.to_doc(env))
                .append(RcDoc::text("]"))
                .append(RcDoc::space())
                .append(RcDoc::text("="))
                .append(RcDoc::space())
                .append(value.to_doc(env)),
            Stmt::Return { expr } => {
                let return_stmt = RcDoc::text("return");
                if let Some(expr) = expr {
                    return_stmt.append(RcDoc::space()).append(expr.to_doc(env))
                } else {
                    return_stmt
                }
            }
            Stmt::Loop { body } => {
                let body_doc = if body.stmts.is_empty() {
                    RcDoc::nil()
                } else {
                    RcDoc::hardline()
                        .append(RcDoc::intersperse(
                            body.stmts.iter().map(|s| s.to_doc(env)),
                            RcDoc::hardline(),
                        ))
                        .nest(4)
                        .append(RcDoc::hardline())
                };

                RcDoc::text("for")
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"))
                    .append(body_doc)
                    .append(RcDoc::text("}"))
            }
            Stmt::Break => RcDoc::text("break"),
            Stmt::If { cond, then, else_ } => {
                let if_part = RcDoc::text("if")
                    .append(RcDoc::space())
                    .append(cond.to_doc(env))
                    .append(RcDoc::space())
                    .append(then.to_doc(env));

                if let Some(else_block) = else_ {
                    if_part
                        .append(RcDoc::space())
                        .append(RcDoc::text("else"))
                        .append(RcDoc::space())
                        .append(else_block.to_doc(env))
                } else {
                    if_part
                }
            }
            Stmt::SwitchExpr {
                expr,
                cases,
                default,
            } => {
                let cases_doc = RcDoc::intersperse(
                    cases.iter().map(|(val, blk)| {
                        let body = if blk.stmts.is_empty() {
                            RcDoc::nil()
                        } else {
                            RcDoc::hardline()
                                .append(RcDoc::intersperse(
                                    blk.stmts.iter().map(|s| s.to_doc(env)),
                                    RcDoc::hardline(),
                                ))
                                .nest(4)
                        };
                        RcDoc::text("case")
                            .append(RcDoc::space())
                            .append(val.to_doc(env))
                            .append(RcDoc::text(":"))
                            .append(body)
                    }),
                    RcDoc::hardline(),
                );

                let default_doc = if let Some(blk) = default {
                    let body = if blk.stmts.is_empty() {
                        RcDoc::nil()
                    } else {
                        RcDoc::hardline()
                            .append(RcDoc::intersperse(
                                blk.stmts.iter().map(|s| s.to_doc(env)),
                                RcDoc::hardline(),
                            ))
                            .nest(4)
                    };
                    RcDoc::hardline()
                        .append(RcDoc::text("default:"))
                        .append(body)
                } else {
                    RcDoc::nil()
                };

                RcDoc::text("switch")
                    .append(RcDoc::space())
                    .append(expr.to_doc(env))
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"))
                    .append(RcDoc::hardline())
                    .append(cases_doc)
                    .append(default_doc)
                    .append(RcDoc::hardline())
                    .append(RcDoc::text("}"))
            }
            Stmt::SwitchType {
                bind,
                expr,
                cases,
                default,
            } => {
                let cases_doc = RcDoc::intersperse(
                    cases.iter().map(|(ty, blk)| {
                        let body = if blk.stmts.is_empty() {
                            RcDoc::nil()
                        } else {
                            RcDoc::hardline()
                                .append(RcDoc::intersperse(
                                    blk.stmts.iter().map(|s| s.to_doc(env)),
                                    RcDoc::hardline(),
                                ))
                                .nest(4)
                        };
                        RcDoc::text("case")
                            .append(RcDoc::space())
                            .append(go_type_doc(ty))
                            .append(RcDoc::text(":"))
                            .append(body)
                    }),
                    RcDoc::hardline(),
                );

                let default_doc = if let Some(blk) = default {
                    let body = if blk.stmts.is_empty() {
                        RcDoc::nil()
                    } else {
                        RcDoc::hardline()
                            .append(RcDoc::intersperse(
                                blk.stmts.iter().map(|s| s.to_doc(env)),
                                RcDoc::hardline(),
                            ))
                            .nest(4)
                    };
                    RcDoc::hardline()
                        .append(RcDoc::text("default:"))
                        .append(body)
                } else {
                    RcDoc::nil()
                };

                RcDoc::text("switch")
                    .append(RcDoc::space())
                    .append(if let Some(b) = bind {
                        RcDoc::text(b)
                            .append(RcDoc::space())
                            .append(RcDoc::text(":="))
                            .append(RcDoc::space())
                    } else {
                        RcDoc::nil()
                    })
                    .append(expr.to_doc(env))
                    .append(RcDoc::text(".(type)"))
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"))
                    .append(RcDoc::hardline())
                    .append(cases_doc)
                    .append(default_doc)
                    .append(RcDoc::hardline())
                    .append(RcDoc::text("}"))
            }
        }
    }

    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Expr {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        match self {
            Expr::Nil { ty: _ } => RcDoc::text("nil"),
            Expr::Void { ty: _ } => RcDoc::text(""),
            Expr::Unit { ty: _ } => RcDoc::text("struct{}{}"),
            Expr::Var { name, ty: _ } => RcDoc::text(name),
            Expr::Bool { value, ty: _ } => RcDoc::text(if *value { "true" } else { "false" }),
            Expr::Int { value, ty: _ } => RcDoc::as_string(value),
            Expr::String { value, ty: _ } => RcDoc::text("\"")
                .append(RcDoc::text(value))
                .append(RcDoc::text("\"")),
            Expr::Call { func, args, ty: _ } => {
                let args_doc = if args.is_empty() {
                    RcDoc::nil()
                } else {
                    RcDoc::intersperse(args.iter().map(|arg| arg.to_doc(env)), RcDoc::text(", "))
                };

                func.to_doc(env)
                    .append(RcDoc::text("("))
                    .append(args_doc)
                    .append(RcDoc::text(")"))
            }
            Expr::UnaryOp { op, expr, ty: _ } => op.doc().append(expr.to_doc(env)),
            Expr::BinaryOp {
                op,
                lhs,
                rhs,
                ty: _,
            } => lhs
                .to_doc(env)
                .append(RcDoc::space())
                .append(op.doc())
                .append(RcDoc::space())
                .append(rhs.to_doc(env)),
            Expr::FieldAccess { obj, field, ty: _ } => obj
                .to_doc(env)
                .append(RcDoc::text("."))
                .append(RcDoc::text(field)),
            Expr::Index {
                array,
                index,
                ty: _,
            } => array
                .to_doc(env)
                .append(RcDoc::text("["))
                .append(index.to_doc(env))
                .append(RcDoc::text("]")),
            Expr::Cast { expr, ty } => expr
                .to_doc(env)
                .append(RcDoc::text(".("))
                .append(go_type_doc(ty))
                .append(RcDoc::text(")")),
            Expr::StructLiteral { ty, fields } => {
                let fields_doc = if fields.is_empty() {
                    RcDoc::nil()
                } else {
                    RcDoc::hardline()
                        .append(RcDoc::intersperse(
                            fields.iter().map(|(field_name, field_expr)| {
                                RcDoc::text(field_name)
                                    .append(RcDoc::text(":"))
                                    .append(RcDoc::space())
                                    .append(field_expr.to_doc(env))
                                    .append(RcDoc::text(","))
                            }),
                            RcDoc::hardline(),
                        ))
                        .append(RcDoc::hardline())
                        .nest(4)
                };

                RcDoc::text(go_type_name(ty))
                    .append(RcDoc::text("{"))
                    .append(fields_doc)
                    .append(RcDoc::text("}"))
            }
            Expr::ArrayLiteral { ty, elems } => {
                let (len_prefix, elem_ty_doc) = match ty {
                    GoType::TArray { len, elem } => (format!("[{}]", len), go_type_doc(elem)),
                    GoType::TSlice { elem } => ("[]".to_string(), go_type_doc(elem)),
                    other => panic!(
                        "Array literal must have array or slice type, got {:?}",
                        other
                    ),
                };
                let elems_doc = if elems.is_empty() {
                    RcDoc::nil()
                } else {
                    RcDoc::intersperse(
                        elems.iter().map(|elem_expr| elem_expr.to_doc(env)),
                        RcDoc::text(", "),
                    )
                };

                RcDoc::text(len_prefix)
                    .append(elem_ty_doc)
                    .append(RcDoc::text("{"))
                    .append(elems_doc)
                    .append(RcDoc::text("}"))
            }
            Expr::Block { stmts, expr, ty: _ } => {
                let stmts_doc = if stmts.is_empty() {
                    RcDoc::nil()
                } else {
                    RcDoc::intersperse(stmts.iter().map(|stmt| stmt.to_doc(env)), RcDoc::hardline())
                        .append(if expr.is_some() {
                            RcDoc::hardline()
                        } else {
                            RcDoc::nil()
                        })
                };

                let expr_doc = if let Some(expr) = expr {
                    expr.to_doc(env)
                } else {
                    RcDoc::nil()
                };

                let content = if stmts.is_empty() && expr.is_none() {
                    RcDoc::nil()
                } else {
                    RcDoc::hardline()
                        .append(stmts_doc)
                        .append(expr_doc)
                        .append(RcDoc::hardline())
                        .nest(4)
                };

                RcDoc::text("{").append(content).append(RcDoc::text("}"))
            }
        }
    }

    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl UnaryOp {
    fn doc(&self) -> RcDoc<'_, ()> {
        match self {
            UnaryOp::Neg => RcDoc::text("-"),
            UnaryOp::Not => RcDoc::text("!"),
            UnaryOp::AddrOf => RcDoc::text("&"),
            UnaryOp::Deref => RcDoc::text("*"),
        }
    }
}

impl BinaryOp {
    fn doc(&self) -> RcDoc<'_, ()> {
        match self {
            BinaryOp::Add => RcDoc::text("+"),
            BinaryOp::Sub => RcDoc::text("-"),
            BinaryOp::Mul => RcDoc::text("*"),
            BinaryOp::Div => RcDoc::text("/"),
            BinaryOp::Less => RcDoc::text("<"),
            BinaryOp::And => RcDoc::text("&&"),
            BinaryOp::Or => RcDoc::text("||"),
        }
    }
}
