use pretty::RcDoc;

use crate::{
    core::{Arm, Expr, File, Fn},
    env::GlobalTypeEnv,
    pprint::common_pprint::{constr_get_accessor_doc, constructor_to_doc},
};

impl File {
    pub fn to_doc(&self, genv: &GlobalTypeEnv) -> RcDoc<'_, ()> {
        RcDoc::intersperse(
            self.toplevels.iter().map(|item| item.to_doc(genv)),
            RcDoc::hardline().append(RcDoc::hardline()),
        )
    }

    pub fn to_pretty(&self, genv: &GlobalTypeEnv, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(genv).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Fn {
    pub fn to_doc(&self, genv: &GlobalTypeEnv) -> RcDoc<'_, ()> {
        let name = RcDoc::text(self.name.clone());
        let params = RcDoc::intersperse(
            self.params.iter().map(|(name, ty)| {
                RcDoc::text(name.clone())
                    .append(RcDoc::text(":"))
                    .append(RcDoc::space())
                    .append(ty.to_doc())
            }),
            RcDoc::text(", "),
        );

        let ret_ty = self.ret_ty.to_doc();

        let body = self.body.to_doc(genv);

        RcDoc::text("fn")
            .append(RcDoc::space())
            .append(name)
            .append(RcDoc::text("("))
            .append(params)
            .append(RcDoc::text(")"))
            .append(RcDoc::space())
            .append(RcDoc::text("->"))
            .append(RcDoc::space())
            .append(ret_ty)
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(RcDoc::hardline().append(body).nest(2))
            .append(RcDoc::hardline())
            .append(RcDoc::text("}"))
    }

    pub fn to_pretty(&self, genv: &GlobalTypeEnv, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(genv).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Expr {
    pub fn to_doc(&self, genv: &GlobalTypeEnv) -> RcDoc<'_, ()> {
        match self {
            Expr::EVar { name, ty: _ } => RcDoc::text(name.clone()),

            Expr::EPrim { value, ty: _ } => RcDoc::text(value.to_string()),
            Expr::EConstr {
                constructor,
                args,
                ty: _,
            } => {
                let args_docs = args.iter().map(|arg| arg.to_doc(genv));
                let struct_def = constructor
                    .as_struct()
                    .and_then(|s| genv.structs().get(&s.type_name));
                constructor_to_doc(constructor, args_docs, struct_def)
            }

            Expr::ETuple { items, ty: _ } => {
                if items.is_empty() {
                    RcDoc::text("()")
                } else {
                    let items_doc = RcDoc::intersperse(
                        items.iter().map(|item| item.to_doc(genv)),
                        RcDoc::text(", "),
                    );

                    RcDoc::text("(").append(items_doc).append(RcDoc::text(")"))
                }
            }
            Expr::EArray { items, ty: _ } => {
                if items.is_empty() {
                    RcDoc::text("[]")
                } else {
                    let items_doc = RcDoc::intersperse(
                        items.iter().map(|item| item.to_doc(genv)),
                        RcDoc::text(", "),
                    );

                    RcDoc::text("[").append(items_doc).append(RcDoc::text("]"))
                }
            }
            Expr::EClosure {
                params,
                body,
                ty: _,
            } => {
                let params_doc = if params.is_empty() {
                    RcDoc::text("||")
                } else {
                    let list = RcDoc::intersperse(
                        params.iter().map(|param| {
                            RcDoc::text(param.name.clone())
                                .append(RcDoc::text(": "))
                                .append(param.ty.to_doc())
                        }),
                        RcDoc::text(", "),
                    );
                    RcDoc::text("|").append(list).append(RcDoc::text("|"))
                };

                params_doc
                    .append(RcDoc::space())
                    .append(RcDoc::text("=>"))
                    .append(RcDoc::space())
                    .append(body.to_doc(genv))
                    .group()
            }

            Expr::ELet {
                name,
                value,
                body,
                ty: _,
            } => RcDoc::text("let")
                .append(RcDoc::space())
                .append(RcDoc::text(name))
                .append(RcDoc::space())
                .append(RcDoc::text("="))
                .append(RcDoc::space())
                .append(value.to_doc(genv))
                .append(RcDoc::space())
                .append(RcDoc::text("in"))
                .append(RcDoc::hardline())
                .append(body.to_doc(genv))
                .group(),

            Expr::EMatch {
                expr,
                arms,
                default,
                ty: _,
            } => {
                let match_expr = RcDoc::text("match")
                    .append(RcDoc::space())
                    .append(expr.to_doc(genv))
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"));

                let arms_doc = RcDoc::concat(
                    arms.iter()
                        .map(|arm| RcDoc::hardline().append(arm.to_doc(genv))),
                );

                let default_doc = if let Some(default_expr) = default {
                    RcDoc::hardline()
                        .append(RcDoc::text("_"))
                        .append(RcDoc::space())
                        .append(RcDoc::text("=>"))
                        .append(RcDoc::space())
                        .append(default_expr.to_doc(genv))
                        .append(RcDoc::text(","))
                } else {
                    RcDoc::nil()
                };

                match_expr
                    .append(arms_doc.nest(2))
                    .append(default_doc.nest(2))
                    .append(RcDoc::line())
                    .append(RcDoc::text("}"))
                    .group()
            }
            Expr::EIf {
                cond,
                then_branch,
                else_branch,
                ty: _,
            } => {
                let then_block = RcDoc::hardline()
                    .append(then_branch.to_doc(genv))
                    .nest(4)
                    .append(RcDoc::hardline());
                let else_block = RcDoc::hardline()
                    .append(else_branch.to_doc(genv))
                    .nest(4)
                    .append(RcDoc::hardline());

                RcDoc::text("if")
                    .append(RcDoc::space())
                    .append(cond.to_doc(genv))
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"))
                    .append(then_block)
                    .append(RcDoc::text("}"))
                    .append(RcDoc::space())
                    .append(RcDoc::text("else"))
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"))
                    .append(else_block)
                    .append(RcDoc::text("}"))
                    .group()
            }
            Expr::EWhile { cond, body, ty: _ } => {
                let body_block = RcDoc::hardline()
                    .append(body.to_doc(genv))
                    .nest(4)
                    .append(RcDoc::hardline());

                RcDoc::text("while")
                    .append(RcDoc::space())
                    .append(cond.to_doc(genv))
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"))
                    .append(body_block)
                    .append(RcDoc::text("}"))
                    .group()
            }

            Expr::EGo { expr, ty: _ } => RcDoc::text("go")
                .append(RcDoc::space())
                .append(expr.to_doc(genv)),

            Expr::EUnary { op, expr, ty: _ } => {
                let expr_doc = expr.to_doc(genv);
                RcDoc::text("(")
                    .append(RcDoc::text(op.symbol()))
                    .append(expr_doc)
                    .append(RcDoc::text(")"))
            }
            Expr::EBinary {
                op,
                lhs,
                rhs,
                ty: _,
            } => {
                let lhs_doc = lhs.to_doc(genv);
                let rhs_doc = rhs.to_doc(genv);
                RcDoc::text("(")
                    .append(lhs_doc)
                    .append(RcDoc::space())
                    .append(RcDoc::text(op.symbol()))
                    .append(RcDoc::space())
                    .append(rhs_doc)
                    .append(RcDoc::text(")"))
            }
            Expr::ECall { func, args, ty: _ } => {
                let args_doc =
                    RcDoc::intersperse(args.iter().map(|arg| arg.to_doc(genv)), RcDoc::text(", "));

                func.to_doc(genv)
                    .append(RcDoc::text("("))
                    .append(args_doc)
                    .append(RcDoc::text(")"))
            }
            Expr::EToDyn {
                trait_name,
                for_ty,
                expr,
                ty: _,
            } => RcDoc::text("to_dyn[")
                .append(RcDoc::text(trait_name.0.clone()))
                .append(RcDoc::text("]{"))
                .append(for_ty.to_doc())
                .append(RcDoc::text("}("))
                .append(expr.to_doc(genv))
                .append(RcDoc::text(")")),
            Expr::EDynCall {
                trait_name,
                method_name,
                receiver,
                args,
                ty: _,
            } => {
                let args_doc = RcDoc::intersperse(
                    std::iter::once(receiver.as_ref())
                        .chain(args.iter())
                        .map(|arg| arg.to_doc(genv)),
                    RcDoc::text(", "),
                );
                RcDoc::text("dyn_call[")
                    .append(RcDoc::text(trait_name.0.clone()))
                    .append(RcDoc::text("::"))
                    .append(RcDoc::text(method_name.0.clone()))
                    .append(RcDoc::text("]("))
                    .append(args_doc)
                    .append(RcDoc::text(")"))
            }
            Expr::EProj {
                tuple,
                index,
                ty: _,
            } => tuple
                .to_doc(genv)
                .append(RcDoc::text("."))
                .append(RcDoc::text(index.to_string())),
            Expr::EConstrGet {
                expr,
                constructor,
                field_index,
                ty: _,
            } => {
                let struct_def = constructor
                    .as_struct()
                    .and_then(|s| genv.structs().get(&s.type_name));
                let accessor = constr_get_accessor_doc(constructor, *field_index, struct_def);

                accessor
                    .append(RcDoc::text("("))
                    .append(expr.to_doc(genv))
                    .append(RcDoc::text(")"))
            }
        }
    }

    pub fn to_pretty(&self, genv: &GlobalTypeEnv, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(genv).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Arm {
    pub fn to_doc(&self, genv: &GlobalTypeEnv) -> RcDoc<'_, ()> {
        self.lhs
            .to_doc(genv)
            .append(RcDoc::space())
            .append(RcDoc::text("=>"))
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(
                RcDoc::hardline()
                    .append(self.body.to_doc(genv))
                    .append(RcDoc::hardline())
                    .nest(2),
            )
            .append(RcDoc::text("}"))
            .append(RcDoc::text(","))
    }

    pub fn to_pretty(&self, genv: &GlobalTypeEnv, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(genv).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}
