use pretty::RcDoc;

use crate::{
    env::Env,
    go::{Block, Expr, Field, File, Fn, Interface, Item, Method, MethodElem, Stmt, Struct},
};

impl File {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        RcDoc::intersperse(
            self.toplevels.iter().map(|item| item.to_doc(env)),
            RcDoc::hardline().append(RcDoc::hardline()),
        )
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
            Item::Interface(interface) => interface.to_doc(env),
            Item::Struct(struct_) => struct_.to_doc(env),
            Item::Fn(fn_) => fn_.to_doc(env),
        }
    }
}

impl Interface {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        let methods_doc = if self.methods.is_empty() {
            RcDoc::nil()
        } else {
            RcDoc::intersperse(
                self.methods.iter().map(|method| method.to_doc(env)),
                RcDoc::hardline(),
            )
            .nest(4)
        };

        RcDoc::text("type")
            .append(RcDoc::space())
            .append(RcDoc::text(&self.name))
            .append(RcDoc::space())
            .append(RcDoc::text("interface"))
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(if self.methods.is_empty() {
                RcDoc::nil()
            } else {
                RcDoc::hardline()
                    .append(methods_doc)
                    .append(RcDoc::hardline())
            })
            .append(RcDoc::text("}"))
    }
}

impl MethodElem {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        let params_doc = RcDoc::intersperse(
            self.params.iter().map(|(name, ty)| {
                RcDoc::text(name)
                    .append(RcDoc::space())
                    .append(ty.to_doc(env))
            }),
            RcDoc::text(", "),
        );

        let ret_doc = if let Some(ret_ty) = &self.ret {
            RcDoc::space().append(ret_ty.to_doc(env))
        } else {
            RcDoc::nil()
        };

        RcDoc::text(&self.name)
            .append(RcDoc::text("("))
            .append(params_doc)
            .append(RcDoc::text(")"))
            .append(ret_doc)
    }
}

impl Struct {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        let fields_doc = if self.fields.is_empty() {
            RcDoc::nil()
        } else {
            RcDoc::intersperse(
                self.fields.iter().map(|field| field.to_doc(env)),
                RcDoc::hardline(),
            )
            .nest(4)
        };

        let struct_doc = RcDoc::text("type")
            .append(RcDoc::space())
            .append(RcDoc::text(&self.name))
            .append(RcDoc::space())
            .append(RcDoc::text("struct"))
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(if self.fields.is_empty() {
                RcDoc::space()
            } else {
                RcDoc::hardline()
                    .append(fields_doc)
                    .append(RcDoc::hardline())
            })
            .append(RcDoc::text("}"));

        if self.methods.is_empty() {
            struct_doc
        } else {
            let methods_doc = RcDoc::intersperse(
                self.methods.iter().map(|method| method.to_doc(env)),
                RcDoc::hardline().append(RcDoc::hardline()),
            );

            struct_doc
                .append(RcDoc::hardline())
                .append(RcDoc::hardline())
                .append(methods_doc)
        }
    }
}

impl Field {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        RcDoc::text(&self.name)
            .append(RcDoc::space())
            .append(self.ty.to_doc(env))
    }
}

impl Fn {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        let params_doc = RcDoc::intersperse(
            self.params.iter().map(|(name, ty)| {
                RcDoc::text(name)
                    .append(RcDoc::space())
                    .append(ty.to_doc(env))
            }),
            RcDoc::text(", "),
        );

        RcDoc::text("func")
            .append(RcDoc::space())
            .append(RcDoc::text(&self.name))
            .append(RcDoc::text("("))
            .append(params_doc)
            .append(RcDoc::text(")"))
            .append(RcDoc::space())
            .append(self.body.to_doc(env))
    }
}

impl Method {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        let params_doc = RcDoc::intersperse(
            self.params.iter().map(|(name, ty)| {
                RcDoc::text(name)
                    .append(RcDoc::space())
                    .append(ty.to_doc(env))
            }),
            RcDoc::text(", "),
        );

        RcDoc::text("func")
            .append(RcDoc::space())
            .append(RcDoc::text("("))
            .append(RcDoc::text(&self.receiver))
            .append(RcDoc::text(")"))
            .append(RcDoc::space())
            .append(RcDoc::text(&self.name))
            .append(RcDoc::text("("))
            .append(params_doc)
            .append(RcDoc::text(")"))
            .append(RcDoc::space())
            .append(self.body.to_doc(env))
    }
}

impl Block {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        if self.stmts.is_empty() {
            RcDoc::text("{}")
        } else {
            let stmts_doc = RcDoc::intersperse(
                self.stmts.iter().map(|stmt| stmt.to_doc(env)),
                RcDoc::hardline(),
            );

            RcDoc::text("{")
                .append(RcDoc::hardline().append(stmts_doc).nest(4))
                .append(RcDoc::hardline())
                .append(RcDoc::text("}"))
        }
    }
}

impl Stmt {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        match self {
            Stmt::Expr(expr) => expr.to_doc(env),
            Stmt::VarDecl { name, ty, value } => {
                let mut doc = RcDoc::text("var")
                    .append(RcDoc::space())
                    .append(RcDoc::text(name));

                if let Some(ty) = ty {
                    doc = doc.append(RcDoc::space()).append(ty.to_doc(env));
                }

                if let Some(value) = value {
                    doc = doc
                        .append(RcDoc::space())
                        .append(RcDoc::text("="))
                        .append(RcDoc::space())
                        .append(value.to_doc(env));
                }

                doc
            }
            Stmt::Assignment { name, value } => RcDoc::text(name)
                .append(RcDoc::space())
                .append(RcDoc::text("="))
                .append(RcDoc::space())
                .append(value.to_doc(env)),
            Stmt::Return { expr } => {
                let mut doc = RcDoc::text("return");
                if let Some(expr) = expr {
                    doc = doc.append(RcDoc::space()).append(expr.to_doc(env));
                }
                doc
            }
            Stmt::If { cond, then, else_ } => {
                let mut doc = RcDoc::text("if")
                    .append(RcDoc::space())
                    .append(cond.to_doc(env))
                    .append(RcDoc::space())
                    .append(then.to_doc(env));

                if let Some(else_block) = else_ {
                    doc = doc
                        .append(RcDoc::space())
                        .append(RcDoc::text("else"))
                        .append(RcDoc::space())
                        .append(else_block.to_doc(env));
                }

                doc
            }
        }
    }
}

impl Expr {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        match self {
            Expr::Var { name } => RcDoc::text(name),
            Expr::Literal { value } => RcDoc::text(value),
            Expr::Call { func, args } => {
                let args_doc =
                    RcDoc::intersperse(args.iter().map(|arg| arg.to_doc(env)), RcDoc::text(", "));

                RcDoc::text(func)
                    .append(RcDoc::text("("))
                    .append(args_doc)
                    .append(RcDoc::text(")"))
            }
            Expr::FieldAccess { obj, field } => obj
                .to_doc(env)
                .append(RcDoc::text("."))
                .append(RcDoc::text(field)),
            Expr::Cast { expr, ty } => expr
                .to_doc(env)
                .append(RcDoc::text(".("))
                .append(ty.to_doc(env))
                .append(RcDoc::text(")")),
            Expr::StructLiteral { ty, fields } => {
                let fields_doc = RcDoc::intersperse(
                    fields.iter().map(|(name, expr)| {
                        RcDoc::text(name)
                            .append(RcDoc::text(": "))
                            .append(expr.to_doc(env))
                    }),
                    RcDoc::text(", "),
                );

                RcDoc::text(ty)
                    .append(RcDoc::text("{"))
                    .append(fields_doc)
                    .append(RcDoc::text("}"))
            }
            Expr::If { cond, then, else_ } => RcDoc::text("if")
                .append(RcDoc::space())
                .append(cond.to_doc(env))
                .append(RcDoc::space())
                .append(RcDoc::text("{"))
                .append(RcDoc::space())
                .append(then.to_doc(env))
                .append(RcDoc::space())
                .append(RcDoc::text("}"))
                .append(RcDoc::space())
                .append(RcDoc::text("else"))
                .append(RcDoc::space())
                .append(RcDoc::text("{"))
                .append(RcDoc::space())
                .append(else_.to_doc(env))
                .append(RcDoc::space())
                .append(RcDoc::text("}")),
            Expr::Block { stmts, expr } => {
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

                RcDoc::text("{")
                    .append(RcDoc::hardline().append(stmts_doc).append(expr_doc).nest(4))
                    .append(RcDoc::hardline())
                    .append(RcDoc::text("}"))
            }
        }
    }
}
