use pretty::RcDoc;

use crate::{
    anf::{AExpr, Arm, CExpr, File, Fn, GlobalAnfEnv, ImmExpr},
    tast::Constructor,
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
                RcDoc::text(name.clone())
                    .append(RcDoc::text(":"))
                    .append(RcDoc::space())
                    .append(ty.to_doc(&anfenv.to_type_env()))
            }),
            RcDoc::text(", "),
        );

        let ret_ty = self.ret_ty.to_doc(&anfenv.to_type_env());

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

impl AExpr {
    pub fn to_doc(&self, anfenv: &GlobalAnfEnv) -> RcDoc<'_, ()> {
        match self {
            AExpr::ACExpr { expr } => expr.to_doc(anfenv),
            AExpr::ALet {
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
                .append(value.to_doc(anfenv))
                .append(RcDoc::space())
                .append(RcDoc::text("in"))
                .append(RcDoc::hardline())
                .append(body.to_doc(anfenv))
                .group(),
        }
    }

    pub fn to_pretty(&self, anfenv: &GlobalAnfEnv, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(anfenv).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl CExpr {
    pub fn to_doc(&self, anfenv: &GlobalAnfEnv) -> RcDoc<'_, ()> {
        match self {
            CExpr::CImm { imm } => imm.to_doc(),
            CExpr::EConstr {
                constructor,
                args,
                ty: _,
            } => match constructor {
                Constructor::Enum(enum_constructor) => {
                    let name_doc = RcDoc::text(format!(
                        "{}::{}",
                        enum_constructor.type_name.0, enum_constructor.variant.0
                    ));

                    if args.is_empty() {
                        name_doc
                    } else {
                        let args_doc = RcDoc::intersperse(
                            args.iter().map(|arg| arg.to_doc()),
                            RcDoc::text(", "),
                        );

                        name_doc
                            .append(RcDoc::text("("))
                            .append(args_doc)
                            .append(RcDoc::text(")"))
                    }
                }
                Constructor::Struct(struct_constructor) => {
                    let name_doc = RcDoc::text(struct_constructor.type_name.0.clone());

                    if let Some(struct_def) = anfenv.structs().get(&struct_constructor.type_name) {
                        if struct_def.fields.is_empty() {
                            name_doc.append(RcDoc::space()).append(RcDoc::text("{}"))
                        } else if struct_def.fields.len() == args.len() {
                            let fields_doc = RcDoc::intersperse(
                                struct_def.fields.iter().zip(args.iter()).map(
                                    |((fname, _), arg)| {
                                        RcDoc::text(fname.0.clone())
                                            .append(RcDoc::text(": "))
                                            .append(arg.to_doc())
                                    },
                                ),
                                RcDoc::text(", "),
                            );

                            name_doc
                                .append(RcDoc::space())
                                .append(RcDoc::text("{ "))
                                .append(fields_doc)
                                .append(RcDoc::text(" }"))
                        } else if args.is_empty() {
                            name_doc
                        } else {
                            let args_doc = RcDoc::intersperse(
                                args.iter().map(|arg| arg.to_doc()),
                                RcDoc::text(", "),
                            );

                            name_doc
                                .append(RcDoc::text("("))
                                .append(args_doc)
                                .append(RcDoc::text(")"))
                        }
                    } else if args.is_empty() {
                        name_doc
                    } else {
                        let args_doc = RcDoc::intersperse(
                            args.iter().map(|arg| arg.to_doc()),
                            RcDoc::text(", "),
                        );

                        name_doc
                            .append(RcDoc::text("("))
                            .append(args_doc)
                            .append(RcDoc::text(")"))
                    }
                }
            },
            CExpr::ETuple { items, ty: _ } => {
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
            CExpr::EArray { items, ty: _ } => {
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
            CExpr::EMatch {
                expr,
                arms,
                default,
                ty: _,
            } => {
                let match_expr = RcDoc::text("match")
                    .append(RcDoc::space())
                    .append(expr.to_doc())
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
                        .append(default_expr.to_doc(anfenv)) // Default case body is AExpr
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
            CExpr::EIf {
                cond,
                then,
                else_,
                ty: _,
            } => RcDoc::text("if")
                .append(RcDoc::space())
                .append(cond.to_doc())
                .append(RcDoc::space())
                .append(RcDoc::text("{"))
                .append(RcDoc::hardline().append(then.to_doc(anfenv)).nest(2))
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
            CExpr::EWhile { cond, body, ty: _ } => RcDoc::text("while")
                .append(RcDoc::space())
                .append(cond.to_doc(anfenv))
                .append(RcDoc::space())
                .append(RcDoc::text("{"))
                .append(RcDoc::hardline().append(body.to_doc(anfenv)).nest(2))
                .append(RcDoc::hardline())
                .append(RcDoc::text("}"))
                .group(),
            CExpr::EConstrGet {
                expr,
                constructor,
                field_index,
                ty: _,
            } => {
                let accessor = match constructor {
                    Constructor::Enum(enum_constructor) => RcDoc::text(format!(
                        "{}::{}._{}",
                        enum_constructor.type_name.0, enum_constructor.variant.0, field_index
                    )),
                    Constructor::Struct(struct_constructor) => {
                        let field_name = anfenv
                            .structs()
                            .get(&struct_constructor.type_name)
                            .and_then(|def| def.fields.get(*field_index))
                            .map(|(fname, _)| fname.0.clone())
                            .unwrap_or_else(|| format!("_{}", field_index));
                        RcDoc::text(format!(
                            "{}.{field}",
                            struct_constructor.type_name.0,
                            field = field_name
                        ))
                    }
                };

                accessor
                    .append(RcDoc::text("("))
                    .append(expr.to_doc())
                    .append(RcDoc::text(")"))
            }
            CExpr::ECall { func, args, ty: _ } => {
                let args_doc =
                    RcDoc::intersperse(args.iter().map(|arg| arg.to_doc()), RcDoc::text(", "));

                func.to_doc()
                    .append(RcDoc::text("("))
                    .append(args_doc)
                    .append(RcDoc::text(")"))
            }
            CExpr::EProj {
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
            ImmExpr::ImmVar { name, ty: _ } => RcDoc::text(name.clone()),
            ImmExpr::ImmPrim { value, ty: _ } => RcDoc::text(value.to_string()),
            ImmExpr::ImmTag { index, ty: _ } => RcDoc::text(format!("Tag_{}", index)),
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
