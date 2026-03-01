use pretty::RcDoc;

use crate::{
    anf::{Arm, Bind, Block, File, Fn, GlobalAnfEnv, ImmExpr, JoinBind, LetBind, Term, ValueExpr},
    pprint::common_pprint::{constr_get_accessor_doc, constructor_to_doc},
};

impl File {
    pub fn to_doc(&self, anfenv: &GlobalAnfEnv) -> RcDoc<'_, ()> {
        RcDoc::intersperse(
            self.toplevels.iter().map(|item| item.to_doc(anfenv)),
            RcDoc::hardline().append(RcDoc::hardline()),
        )
    }

    pub fn to_pretty(&self, anfenv: &GlobalAnfEnv, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(anfenv).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Fn {
    pub fn to_doc(&self, anfenv: &GlobalAnfEnv) -> RcDoc<'_, ()> {
        let name = RcDoc::text(self.name.clone());
        let params = RcDoc::intersperse(
            self.params.iter().map(|(name, ty)| {
                RcDoc::text(name.0.clone())
                    .append(RcDoc::text(":"))
                    .append(RcDoc::space())
                    .append(ty.to_doc())
            }),
            RcDoc::text(", "),
        );

        let ret_ty = self.ret_ty.to_doc();

        let body = self.body.to_doc(anfenv);

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

    pub fn to_pretty(&self, anfenv: &GlobalAnfEnv, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(anfenv).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl LetBind {
    pub fn to_doc(&self, anfenv: &GlobalAnfEnv) -> RcDoc<'_, ()> {
        RcDoc::text("let")
            .append(RcDoc::space())
            .append(RcDoc::text(self.id.0.clone()))
            .append(RcDoc::space())
            .append(RcDoc::text("="))
            .append(RcDoc::space())
            .append(self.value.to_doc(anfenv))
            .append(RcDoc::space())
            .append(RcDoc::text("in"))
    }
}

impl JoinBind {
    pub fn to_doc(&self, anfenv: &GlobalAnfEnv) -> RcDoc<'_, ()> {
        let params = RcDoc::intersperse(
            self.params.iter().map(|(name, ty)| {
                RcDoc::text(name.0.clone())
                    .append(RcDoc::text(":"))
                    .append(RcDoc::space())
                    .append(ty.to_doc())
            }),
            RcDoc::text(", "),
        );

        RcDoc::text("join")
            .append(RcDoc::space())
            .append(RcDoc::text(self.id.0.clone()))
            .append(RcDoc::text("("))
            .append(params)
            .append(RcDoc::text(")"))
            .append(RcDoc::space())
            .append(RcDoc::text("->"))
            .append(RcDoc::space())
            .append(self.ret_ty.to_doc())
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(RcDoc::hardline().append(self.body.to_doc(anfenv)).nest(2))
            .append(RcDoc::hardline())
            .append(RcDoc::text("}"))
            .append(RcDoc::space())
            .append(RcDoc::text("in"))
    }
}

impl Bind {
    pub fn to_doc(&self, anfenv: &GlobalAnfEnv) -> RcDoc<'_, ()> {
        match self {
            Bind::Let(b) => b.to_doc(anfenv),
            Bind::Join(b) => b.to_doc(anfenv),
            Bind::JoinRec(bs) => {
                let items = RcDoc::concat(
                    bs.iter()
                        .map(|b| RcDoc::hardline().append(b.to_doc(anfenv))),
                );

                RcDoc::text("joinrec")
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"))
                    .append(items.nest(2))
                    .append(RcDoc::hardline())
                    .append(RcDoc::text("}"))
                    .append(RcDoc::space())
                    .append(RcDoc::text("in"))
            }
        }
    }
}

impl Block {
    pub fn to_doc(&self, anfenv: &GlobalAnfEnv) -> RcDoc<'_, ()> {
        let mut docs = self
            .binds
            .iter()
            .map(|bind| bind.to_doc(anfenv))
            .collect::<Vec<_>>();
        docs.push(self.term.to_doc(anfenv));
        RcDoc::intersperse(docs, RcDoc::hardline())
    }

    pub fn to_pretty(&self, anfenv: &GlobalAnfEnv, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(anfenv).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl ValueExpr {
    pub fn to_doc(&self, anfenv: &GlobalAnfEnv) -> RcDoc<'_, ()> {
        match self {
            ValueExpr::Imm(imm) => imm.to_doc(),
            ValueExpr::Constr {
                constructor,
                args,
                ty: _,
            } => {
                let args_docs = args.iter().map(|arg| arg.to_doc());
                let struct_def = constructor
                    .as_struct()
                    .and_then(|s| anfenv.get_struct(&s.type_name));
                constructor_to_doc(constructor, args_docs, struct_def)
            }
            ValueExpr::Tuple { items, ty: _ } => {
                if items.is_empty() {
                    RcDoc::text("()")
                } else {
                    let items_doc = RcDoc::intersperse(
                        items.iter().map(|item| item.to_doc()),
                        RcDoc::text(", "),
                    );

                    RcDoc::text("(").append(items_doc).append(RcDoc::text(")"))
                }
            }
            ValueExpr::Array { items, ty: _ } => {
                if items.is_empty() {
                    RcDoc::text("[]")
                } else {
                    let items_doc = RcDoc::intersperse(
                        items.iter().map(|item| item.to_doc()),
                        RcDoc::text(", "),
                    );

                    RcDoc::text("[").append(items_doc).append(RcDoc::text("]"))
                }
            }
            ValueExpr::ConstrGet {
                expr,
                constructor,
                field_index,
                ty: _,
            } => {
                let struct_def = constructor
                    .as_struct()
                    .and_then(|s| anfenv.get_struct(&s.type_name));
                let accessor = constr_get_accessor_doc(constructor, *field_index, struct_def);

                accessor
                    .append(RcDoc::text("("))
                    .append(expr.to_doc())
                    .append(RcDoc::text(")"))
            }
            ValueExpr::Unary { op, expr, ty: _ } => {
                let expr_doc = expr.to_doc();
                RcDoc::text("(")
                    .append(RcDoc::text(op.symbol()))
                    .append(expr_doc)
                    .append(RcDoc::text(")"))
            }
            ValueExpr::Binary {
                op,
                lhs,
                rhs,
                ty: _,
            } => {
                let lhs_doc = lhs.to_doc();
                let rhs_doc = rhs.to_doc();
                RcDoc::text("(")
                    .append(lhs_doc)
                    .append(RcDoc::space())
                    .append(RcDoc::text(op.symbol()))
                    .append(RcDoc::space())
                    .append(rhs_doc)
                    .append(RcDoc::text(")"))
            }
            ValueExpr::Assign {
                name,
                value,
                target_ty: _,
                ty: _,
            } => RcDoc::text(name.0.clone())
                .append(RcDoc::space())
                .append(RcDoc::text("="))
                .append(RcDoc::space())
                .append(value.to_doc()),
            ValueExpr::Call { func, args, ty: _ } => {
                let args_doc =
                    RcDoc::intersperse(args.iter().map(|arg| arg.to_doc()), RcDoc::text(", "));

                func.to_doc()
                    .append(RcDoc::text("("))
                    .append(args_doc)
                    .append(RcDoc::text(")"))
            }
            ValueExpr::ToDyn {
                trait_name,
                for_ty,
                expr,
                ty: _,
            } => RcDoc::text("to_dyn[")
                .append(RcDoc::text(trait_name.0.clone()))
                .append(RcDoc::text("]{"))
                .append(for_ty.to_doc())
                .append(RcDoc::text("}("))
                .append(expr.to_doc())
                .append(RcDoc::text(")")),
            ValueExpr::DynCall {
                trait_name,
                method_name,
                receiver,
                args,
                ty: _,
            } => {
                let args_doc = RcDoc::intersperse(
                    std::iter::once(receiver)
                        .chain(args.iter())
                        .map(|arg| arg.to_doc()),
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
            ValueExpr::Go { closure, ty: _ } => RcDoc::text("go")
                .append(RcDoc::space())
                .append(closure.to_doc()),
            ValueExpr::Proj {
                tuple,
                index,
                ty: _,
            } => tuple
                .to_doc()
                .append(RcDoc::text("."))
                .append(RcDoc::text(index.to_string())),
        }
    }

    pub fn to_pretty(&self, anfenv: &GlobalAnfEnv, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(anfenv).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}
impl ImmExpr {
    pub fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            ImmExpr::Var { id, ty: _ } => RcDoc::text(id.0.clone()),
            ImmExpr::Prim { value, ty: _ } => RcDoc::text(value.to_string()),
            ImmExpr::Tag { index, ty: _ } => RcDoc::text(format!("Tag_{}", index)),
        }
    }

    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Arm {
    pub fn to_doc(&self, anfenv: &GlobalAnfEnv) -> RcDoc<'_, ()> {
        self.lhs
            .to_doc()
            .append(RcDoc::space())
            .append(RcDoc::text("=>"))
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(
                RcDoc::hardline()
                    .append(self.body.to_doc(anfenv))
                    .append(RcDoc::hardline())
                    .nest(2),
            )
            .append(RcDoc::text("}"))
            .append(RcDoc::text(","))
    }

    pub fn to_pretty(&self, anfenv: &GlobalAnfEnv, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(anfenv).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Term {
    pub fn to_doc(&self, anfenv: &GlobalAnfEnv) -> RcDoc<'_, ()> {
        match self {
            Term::Return(imm) => imm.to_doc(),
            Term::Jump { target, args, .. } => {
                let args_doc =
                    RcDoc::intersperse(args.iter().map(|arg| arg.to_doc()), RcDoc::text(", "));
                RcDoc::text("jump")
                    .append(RcDoc::space())
                    .append(RcDoc::text(target.0.clone()))
                    .append(RcDoc::text("("))
                    .append(args_doc)
                    .append(RcDoc::text(")"))
            }
            Term::If {
                cond, then_, else_, ..
            } => RcDoc::text("if")
                .append(RcDoc::space())
                .append(cond.to_doc())
                .append(RcDoc::space())
                .append(RcDoc::text("{"))
                .append(RcDoc::hardline().append(then_.to_doc(anfenv)).nest(2))
                .append(RcDoc::hardline())
                .append(RcDoc::text("}"))
                .append(RcDoc::space())
                .append(RcDoc::text("else"))
                .append(RcDoc::space())
                .append(RcDoc::text("{"))
                .append(RcDoc::hardline().append(else_.to_doc(anfenv)).nest(2))
                .append(RcDoc::hardline())
                .append(RcDoc::text("}"))
                .group(),
            Term::Match {
                scrut,
                arms,
                default,
                ..
            } => {
                let match_expr = RcDoc::text("match")
                    .append(RcDoc::space())
                    .append(scrut.to_doc())
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"));

                let arms_doc = RcDoc::concat(
                    arms.iter()
                        .map(|arm| RcDoc::hardline().append(arm.to_doc(anfenv))),
                );

                let default_doc = if let Some(default_expr) = default {
                    RcDoc::hardline()
                        .append(RcDoc::text("_"))
                        .append(RcDoc::space())
                        .append(RcDoc::text("=>"))
                        .append(RcDoc::space())
                        .append(RcDoc::text("{"))
                        .append(
                            RcDoc::hardline()
                                .append(default_expr.to_doc(anfenv))
                                .append(RcDoc::hardline())
                                .nest(2),
                        )
                        .append(RcDoc::text("}"))
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
            Term::Unreachable { .. } => RcDoc::text("unreachable"),
        }
    }
}
