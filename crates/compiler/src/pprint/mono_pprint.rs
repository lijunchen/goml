use pretty::RcDoc;

use crate::{
    mono::{GlobalMonoEnv, MonoArm, MonoExpr, MonoFile, MonoFn},
    pprint::common_pprint::{constr_get_accessor_doc, constructor_to_doc},
};

impl MonoFile {
    pub fn to_doc(&self, monoenv: &GlobalMonoEnv) -> RcDoc<'_, ()> {
        RcDoc::intersperse(
            self.toplevels.iter().map(|item| item.to_doc(monoenv)),
            RcDoc::hardline().append(RcDoc::hardline()),
        )
    }

    pub fn to_pretty(&self, monoenv: &GlobalMonoEnv, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(monoenv).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl MonoFn {
    pub fn to_doc(&self, monoenv: &GlobalMonoEnv) -> RcDoc<'_, ()> {
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

        let body = self.body.to_doc(monoenv);

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

    pub fn to_pretty(&self, monoenv: &GlobalMonoEnv, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(monoenv).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl MonoExpr {
    pub fn to_doc(&self, monoenv: &GlobalMonoEnv) -> RcDoc<'_, ()> {
        match self {
            MonoExpr::EVar { name, ty: _ } => RcDoc::text(name.clone()),

            MonoExpr::EPrim { value, ty: _ } => RcDoc::text(value.to_string()),
            MonoExpr::EConstr {
                constructor,
                args,
                ty: _,
            } => {
                let args_docs = args.iter().map(|arg| arg.to_doc(monoenv));
                let struct_def = constructor
                    .as_struct()
                    .and_then(|s| monoenv.get_struct(&s.type_name));
                constructor_to_doc(constructor, args_docs, struct_def)
            }

            MonoExpr::ETuple { items, ty: _ } => {
                if items.is_empty() {
                    RcDoc::text("()")
                } else {
                    let items_doc = RcDoc::intersperse(
                        items.iter().map(|item| item.to_doc(monoenv)),
                        RcDoc::text(", "),
                    );

                    RcDoc::text("(").append(items_doc).append(RcDoc::text(")"))
                }
            }
            MonoExpr::EArray { items, ty: _ } => {
                if items.is_empty() {
                    RcDoc::text("[]")
                } else {
                    let items_doc = RcDoc::intersperse(
                        items.iter().map(|item| item.to_doc(monoenv)),
                        RcDoc::text(", "),
                    );

                    RcDoc::text("[").append(items_doc).append(RcDoc::text("]"))
                }
            }

            MonoExpr::ELet {
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
                .append(value.to_doc(monoenv))
                .append(RcDoc::space())
                .append(RcDoc::text("in"))
                .append(RcDoc::hardline())
                .append(body.to_doc(monoenv))
                .group(),

            MonoExpr::EMatch {
                expr,
                arms,
                default,
                ty: _,
            } => {
                let match_expr = RcDoc::text("match")
                    .append(RcDoc::space())
                    .append(expr.to_doc(monoenv))
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"));

                let arms_doc = RcDoc::concat(
                    arms.iter()
                        .map(|arm| RcDoc::hardline().append(arm.to_doc(monoenv))),
                );

                let default_doc = if let Some(default_expr) = default {
                    RcDoc::hardline()
                        .append(RcDoc::text("_"))
                        .append(RcDoc::space())
                        .append(RcDoc::text("=>"))
                        .append(RcDoc::space())
                        .append(default_expr.to_doc(monoenv))
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
            MonoExpr::EIf {
                cond,
                then_branch,
                else_branch,
                ty: _,
            } => {
                let then_block = RcDoc::hardline()
                    .append(then_branch.to_doc(monoenv))
                    .nest(4)
                    .append(RcDoc::hardline());
                let else_block = RcDoc::hardline()
                    .append(else_branch.to_doc(monoenv))
                    .nest(4)
                    .append(RcDoc::hardline());

                RcDoc::text("if")
                    .append(RcDoc::space())
                    .append(cond.to_doc(monoenv))
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
            MonoExpr::EWhile { cond, body, ty: _ } => {
                let body_block = RcDoc::hardline()
                    .append(body.to_doc(monoenv))
                    .nest(4)
                    .append(RcDoc::hardline());

                RcDoc::text("while")
                    .append(RcDoc::space())
                    .append(cond.to_doc(monoenv))
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"))
                    .append(body_block)
                    .append(RcDoc::text("}"))
                    .group()
            }

            MonoExpr::EUnary { op, expr, ty: _ } => {
                let expr_doc = expr.to_doc(monoenv);
                RcDoc::text("(")
                    .append(RcDoc::text(op.symbol()))
                    .append(expr_doc)
                    .append(RcDoc::text(")"))
            }
            MonoExpr::EBinary {
                op,
                lhs,
                rhs,
                ty: _,
            } => {
                let lhs_doc = lhs.to_doc(monoenv);
                let rhs_doc = rhs.to_doc(monoenv);
                RcDoc::text("(")
                    .append(lhs_doc)
                    .append(RcDoc::space())
                    .append(RcDoc::text(op.symbol()))
                    .append(RcDoc::space())
                    .append(rhs_doc)
                    .append(RcDoc::text(")"))
            }
            MonoExpr::ECall { func, args, ty: _ } => {
                let args_doc = RcDoc::intersperse(
                    args.iter().map(|arg| arg.to_doc(monoenv)),
                    RcDoc::text(", "),
                );

                func.to_doc(monoenv)
                    .append(RcDoc::text("("))
                    .append(args_doc)
                    .append(RcDoc::text(")"))
            }
            MonoExpr::EProj {
                tuple,
                index,
                ty: _,
            } => tuple
                .to_doc(monoenv)
                .append(RcDoc::text("."))
                .append(RcDoc::text(index.to_string())),
            MonoExpr::EConstrGet {
                expr,
                constructor,
                field_index,
                ty: _,
            } => {
                let struct_def = constructor
                    .as_struct()
                    .and_then(|s| monoenv.get_struct(&s.type_name));
                let accessor = constr_get_accessor_doc(constructor, *field_index, struct_def);

                accessor
                    .append(RcDoc::text("("))
                    .append(expr.to_doc(monoenv))
                    .append(RcDoc::text(")"))
            }
            MonoExpr::EClosure {
                params,
                body,
                ty: _,
            } => {
                let params_doc = RcDoc::intersperse(
                    params.iter().map(|p| RcDoc::text(p.name.clone())),
                    RcDoc::text(", "),
                );
                let body_doc = body.to_doc(monoenv);

                RcDoc::text("|")
                    .append(params_doc)
                    .append(RcDoc::text("|"))
                    .append(RcDoc::space())
                    .append(body_doc)
            }
        }
    }

    pub fn to_pretty(&self, monoenv: &GlobalMonoEnv, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(monoenv).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl MonoArm {
    pub fn to_doc(&self, monoenv: &GlobalMonoEnv) -> RcDoc<'_, ()> {
        self.lhs
            .to_doc(monoenv)
            .append(RcDoc::space())
            .append(RcDoc::text("=>"))
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(
                RcDoc::hardline()
                    .append(self.body.to_doc(monoenv))
                    .append(RcDoc::hardline())
                    .nest(2),
            )
            .append(RcDoc::text("}"))
            .append(RcDoc::text(","))
    }

    pub fn to_pretty(&self, monoenv: &GlobalMonoEnv, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(monoenv).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}
