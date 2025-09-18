use pretty::RcDoc;

use crate::{
    anf::{AExpr, Arm, CExpr, File, Fn, ImmExpr},
    env::Env,
    tast::Constructor,
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

impl Fn {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        let name = RcDoc::text(self.name.clone());
        let params = RcDoc::intersperse(
            self.params.iter().map(|(name, ty)| {
                RcDoc::text(name.clone())
                    .append(RcDoc::text(":"))
                    .append(RcDoc::space())
                    .append(ty.to_doc(env))
            }),
            RcDoc::text(", "),
        );

        let ret_ty = self.ret_ty.to_doc(env);

        let body = self.body.to_doc(env);

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

    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl AExpr {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        match self {
            AExpr::ACExpr { expr } => expr.to_doc(env),
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
                .append(value.to_doc(env))
                .append(RcDoc::space())
                .append(RcDoc::text("in"))
                .append(RcDoc::hardline())
                .append(body.to_doc(env))
                .group(),
        }
    }

    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl CExpr {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        match self {
            CExpr::CImm { imm } => imm.to_doc(env),
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
                            args.iter().map(|arg| arg.to_doc(env)),
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

                    if let Some(struct_def) = env.structs.get(&struct_constructor.type_name) {
                        if struct_def.fields.is_empty() {
                            name_doc.append(RcDoc::space()).append(RcDoc::text("{}"))
                        } else if struct_def.fields.len() == args.len() {
                            let fields_doc = RcDoc::intersperse(
                                struct_def.fields.iter().zip(args.iter()).map(
                                    |((fname, _), arg)| {
                                        RcDoc::text(fname.0.clone())
                                            .append(RcDoc::text(": "))
                                            .append(arg.to_doc(env))
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
                                args.iter().map(|arg| arg.to_doc(env)),
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
                            args.iter().map(|arg| arg.to_doc(env)),
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
                        items.iter().map(|item| item.to_doc(env)),
                        RcDoc::text(", "),
                    );

                    RcDoc::text("(").append(items_doc).append(RcDoc::text(")"))
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
                    .append(expr.to_doc(env))
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"));

                let arms_doc = RcDoc::concat(
                    arms.iter()
                        .map(|arm| RcDoc::hardline().append(arm.to_doc(env))),
                );

                let default_doc = if let Some(default_expr) = default {
                    RcDoc::hardline()
                        .append(RcDoc::text("_"))
                        .append(RcDoc::space())
                        .append(RcDoc::text("=>"))
                        .append(RcDoc::space())
                        .append(default_expr.to_doc(env)) // Default case body is AExpr
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
                .append(cond.to_doc(env))
                .append(RcDoc::space())
                .append(RcDoc::text("{"))
                .append(RcDoc::hardline().append(then.to_doc(env)).nest(2))
                .append(RcDoc::hardline())
                .append(RcDoc::text("}"))
                .append(RcDoc::space())
                .append(RcDoc::text("else"))
                .append(RcDoc::space())
                .append(RcDoc::text("{"))
                .append(RcDoc::hardline().append(else_.to_doc(env)).nest(2))
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
                        let field_name = env
                            .structs
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
                    .append(expr.to_doc(env))
                    .append(RcDoc::text(")"))
            }
            CExpr::ECall { func, args, ty: _ } => {
                let args_doc =
                    RcDoc::intersperse(args.iter().map(|arg| arg.to_doc(env)), RcDoc::text(", "));

                RcDoc::text(func)
                    .append(RcDoc::text("("))
                    .append(args_doc)
                    .append(RcDoc::text(")"))
            }
            CExpr::EProj {
                tuple,
                index,
                ty: _,
            } => tuple
                .to_doc(env)
                .append(RcDoc::text("."))
                .append(RcDoc::text(index.to_string())),
        }
    }

    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl ImmExpr {
    pub fn to_doc(&self, _env: &Env) -> RcDoc<'_, ()> {
        match self {
            ImmExpr::ImmVar { name, ty: _ } => RcDoc::text(name.clone()),
            ImmExpr::ImmUnit { ty: _ } => RcDoc::text("()"),
            ImmExpr::ImmBool { value, ty: _ } => {
                if *value {
                    RcDoc::text("true")
                } else {
                    RcDoc::text("false")
                }
            }
            ImmExpr::ImmInt { value, ty: _ } => RcDoc::text(value.to_string()),
            ImmExpr::ImmString { value, ty: _ } => RcDoc::text(format!("{:?}", value)),
            ImmExpr::ImmTag { index, ty: _ } => RcDoc::text(format!("Tag_{}", index)),
        }
    }

    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Arm {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        self.lhs
            .to_doc(env)
            .append(RcDoc::space())
            .append(RcDoc::text("=>"))
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(
                RcDoc::hardline()
                    .append(self.body.to_doc(env))
                    .append(RcDoc::hardline())
                    .nest(2),
            )
            .append(RcDoc::text("}"))
            .append(RcDoc::text(","))
    }

    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}
