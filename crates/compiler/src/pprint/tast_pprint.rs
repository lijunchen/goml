use pretty::RcDoc;

use crate::env::Env;
use crate::tast::Constructor;
use crate::tast::Expr;
use crate::tast::File;
use crate::tast::Fn;
use crate::tast::ImplBlock;
use crate::tast::Item;
use crate::tast::Pat;
use crate::tast::Ty;

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
            Self::ImplBlock(impl_block) => impl_block.to_doc(env),
            Self::Fn(func) => func.to_doc(env),
            Self::ExternGo(ext) => ext.to_doc(env),
        }
    }

    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl ImplBlock {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        let trait_name = RcDoc::text(self.trait_name.0.clone());
        let for_type = self.for_type.to_doc(env);
        let methods = RcDoc::intersperse(
            self.methods.iter().map(|method| method.to_doc(env)),
            RcDoc::hardline(),
        );

        RcDoc::text("impl")
            .append(RcDoc::space())
            .append(trait_name)
            .append(RcDoc::space())
            .append(RcDoc::text("for"))
            .append(RcDoc::space())
            .append(for_type)
            .append(RcDoc::text("{"))
            .append(RcDoc::hardline().append(methods).nest(2))
            .append(RcDoc::hardline())
            .append(RcDoc::text("}"))
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

impl crate::tast::ExternGo {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        let params = RcDoc::intersperse(
            self.params.iter().map(|(name, ty)| {
                RcDoc::text(name.clone())
                    .append(RcDoc::text(":"))
                    .append(RcDoc::space())
                    .append(ty.to_doc(env))
            }),
            RcDoc::text(", "),
        );

        RcDoc::text("extern")
            .append(RcDoc::space())
            .append(RcDoc::text("\"go\""))
            .append(RcDoc::space())
            .append(RcDoc::text(format!("\"{}\"", self.package_path)))
            .append(RcDoc::space())
            .append(RcDoc::text(self.goml_name.clone()))
            .append(RcDoc::text("("))
            .append(params)
            .append(RcDoc::text(")"))
            .append(RcDoc::space())
            .append(RcDoc::text("->"))
            .append(RcDoc::space())
            .append(self.ret_ty.to_doc(env))
    }

    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Ty {
    pub fn to_doc(&self, _env: &Env) -> RcDoc<'_, ()> {
        match self {
            Self::TVar(x) => RcDoc::text(format!("{:?}", x)),
            Self::TUnit => RcDoc::text("unit"),
            Self::TBool => RcDoc::text("bool"),
            Self::TInt => RcDoc::text("int"),
            Self::TString => RcDoc::text("string"),
            Self::TTuple { typs } => {
                let mut doc = RcDoc::text("(");

                if !typs.is_empty() {
                    let mut iter = typs.iter();
                    if let Some(first) = iter.next() {
                        doc = doc.append(first.to_doc(_env));
                    }
                    for item in iter {
                        doc = doc.append(RcDoc::text(", ")).append(item.to_doc(_env));
                    }
                }

                doc.append(RcDoc::text(")"))
            }
            Self::TCon { name } => RcDoc::text(name.clone()),
            Self::TApp { ty, args } => {
                let mut doc = ty.to_doc(_env);

                if !args.is_empty() {
                    let mut iter = args.iter();
                    if let Some(first) = iter.next() {
                        doc = doc.append(RcDoc::text("[")).append(first.to_doc(_env));
                    }
                    for item in iter {
                        doc = doc.append(RcDoc::text(", ")).append(item.to_doc(_env));
                    }
                    doc = doc.append(RcDoc::text("]"));
                }

                doc
            }
            Self::TArray { len, elem } => RcDoc::text("[")
                .append(elem.to_doc(_env))
                .append(RcDoc::text("; "))
                .append(RcDoc::as_string(len))
                .append(RcDoc::text("]")),
            Self::TFunc { params, ret_ty } => {
                let mut doc = RcDoc::text("(");

                if !params.is_empty() {
                    let mut iter = params.iter();
                    if let Some(first) = iter.next() {
                        doc = doc.append(first.to_doc(_env));
                    }
                    for item in iter {
                        doc = doc.append(RcDoc::text(", ")).append(item.to_doc(_env));
                    }
                }

                doc.append(RcDoc::text(") -> ")).append(ret_ty.to_doc(_env))
            }
            Self::TParam { name } => RcDoc::text(name.clone()),
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
            Self::EVar {
                name,
                ty,
                astptr: _,
            } => RcDoc::text("(")
                .append(RcDoc::text(name.clone()))
                .append(RcDoc::text(" : "))
                .append(ty.to_doc(env))
                .append(RcDoc::text(")")),

            Self::EUnit { ty: _ } => RcDoc::text("()"),

            Self::EBool { value, ty: _ } => {
                if *value {
                    RcDoc::text("true")
                } else {
                    RcDoc::text("false")
                }
            }
            Self::EInt { value, ty: _ } => RcDoc::text(value.to_string()),
            Self::EString { value, ty: _ } => RcDoc::text(format!("{:?}", value)),
            Expr::EConstr {
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

            Self::ETuple { items, ty: _ } => {
                if items.is_empty() {
                    RcDoc::text("()")
                } else {
                    let items_doc = RcDoc::intersperse(
                        items.iter().map(|item| item.to_doc(env)),
                        RcDoc::text(", "),
                    );

                    RcDoc::text("(")
                        .append(items_doc)
                        .nest(2)
                        .append(RcDoc::text(")"))
                        .group()
                }
            }
            Self::EArray { items, ty: _ } => {
                if items.is_empty() {
                    RcDoc::text("[]")
                } else {
                    let items_doc = RcDoc::intersperse(
                        items.iter().map(|item| item.to_doc(env)),
                        RcDoc::text(", "),
                    );

                    RcDoc::text("[")
                        .append(items_doc)
                        .nest(2)
                        .append(RcDoc::text("]"))
                        .group()
                }
            }

            Self::ELet {
                pat,
                value,
                body,
                ty: _,
            } => RcDoc::text("let")
                .append(RcDoc::space())
                .append(pat.to_doc(env))
                .append(RcDoc::space())
                .append(RcDoc::text("="))
                .append(RcDoc::space())
                .append(value.to_doc(env))
                .append(RcDoc::space())
                .append(RcDoc::text("in"))
                .append(RcDoc::hardline())
                .append(body.to_doc(env))
                .group(),

            Self::EMatch { expr, arms, ty: _ } => {
                let match_expr = RcDoc::text("match")
                    .append(RcDoc::space())
                    .append(expr.to_doc(env))
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"));

                let arms_doc = RcDoc::concat(
                    arms.iter()
                        .map(|arm| RcDoc::hardline().append(arm.to_doc(env))),
                );

                match_expr
                    .append(arms_doc.nest(4))
                    .append(RcDoc::hardline())
                    .append(RcDoc::text("}"))
                    .group()
            }
            Self::EIf {
                cond,
                then_branch,
                else_branch,
                ty: _,
            } => {
                let then_block = RcDoc::hardline()
                    .append(then_branch.to_doc(env))
                    .nest(4)
                    .append(RcDoc::hardline());
                let else_block = RcDoc::hardline()
                    .append(else_branch.to_doc(env))
                    .nest(4)
                    .append(RcDoc::hardline());

                RcDoc::text("if")
                    .append(RcDoc::space())
                    .append(cond.to_doc(env))
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
            Self::EBinary {
                op,
                lhs,
                rhs,
                ty: _,
                resolution: _,
            } => lhs
                .to_doc(env)
                .append(RcDoc::space())
                .append(RcDoc::text(op.symbol()))
                .append(RcDoc::space())
                .append(rhs.to_doc(env))
                .group(),
            Self::EUnary {
                op,
                expr,
                ty: _,
                resolution: _,
            } => RcDoc::text(op.symbol()).append(expr.to_doc(env)).group(),

            Self::ECall { func, args, ty: _ } => {
                let args_doc =
                    RcDoc::intersperse(args.iter().map(|arg| arg.to_doc(env)), RcDoc::text(", "));

                RcDoc::text(func)
                    .append(RcDoc::text("("))
                    .append(args_doc)
                    .append(RcDoc::text(")"))
            }
            Self::EProj {
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

impl Pat {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        match self {
            Pat::PVar {
                name,
                ty,
                astptr: _,
            } => RcDoc::text(name.clone())
                .append(RcDoc::text(":"))
                .append(RcDoc::space())
                .append(ty.to_doc(env)),
            Pat::PUnit { .. } => RcDoc::text("()"),
            Pat::PBool { value, ty: _ } => {
                if *value {
                    RcDoc::text("true")
                } else {
                    RcDoc::text("false")
                }
            }
            Pat::PInt { value, ty: _ } => RcDoc::text(value.to_string()),
            Pat::PString { value, ty: _ } => RcDoc::text(format!("{:?}", value)),
            Pat::PConstr {
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
            Pat::PTuple { items, ty: _ } => {
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
            Pat::PWild { ty } => RcDoc::text("_")
                .append(RcDoc::text(" : "))
                .append(ty.to_doc(env)),
        }
    }
    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl crate::tast::Arm {
    pub fn to_doc(&self, env: &Env) -> RcDoc<'_, ()> {
        self.pat
            .to_doc(env)
            .append(RcDoc::space())
            .append(RcDoc::text("=>"))
            .append(RcDoc::space())
            .append(
                self.body.to_doc(env).nest(2), // Properly indent the body of the arm
            )
            .append(RcDoc::text(","))
    }

    pub fn to_pretty(&self, env: &Env, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(env).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}
