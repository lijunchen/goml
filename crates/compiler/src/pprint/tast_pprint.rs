use pretty::RcDoc;

use crate::common::Constructor;
use crate::env::GlobalTypeEnv;
use crate::tast::ClosureParam;
use crate::tast::Expr;
use crate::tast::File;
use crate::tast::Fn;
use crate::tast::ImplBlock;
use crate::tast::Item;
use crate::tast::Pat;
use crate::tast::Ty;

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

impl Item {
    pub fn to_doc(&self, genv: &GlobalTypeEnv) -> RcDoc<'_, ()> {
        match self {
            Self::ImplBlock(impl_block) => impl_block.to_doc(genv),
            Self::Fn(func) => func.to_doc(genv),
            Self::ExternGo(ext) => ext.to_doc(genv),
            Self::ExternType(ext) => ext.to_doc(genv),
        }
    }

    pub fn to_pretty(&self, genv: &GlobalTypeEnv, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(genv).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl ImplBlock {
    pub fn to_doc(&self, genv: &GlobalTypeEnv) -> RcDoc<'_, ()> {
        let for_type = self.for_type.to_doc(genv);
        let methods = RcDoc::intersperse(
            self.methods.iter().map(|method| method.to_doc(genv)),
            RcDoc::hardline(),
        );

        let header = if let Some(trait_name) = &self.trait_name {
            RcDoc::text("impl")
                .append(RcDoc::space())
                .append(RcDoc::text(trait_name.0.clone()))
                .append(RcDoc::space())
                .append(RcDoc::text("for"))
                .append(RcDoc::space())
                .append(for_type)
                .append(RcDoc::text("{"))
        } else {
            RcDoc::text("impl")
                .append(RcDoc::space())
                .append(for_type)
                .append(RcDoc::text("{"))
        };

        header
            .append(RcDoc::hardline().append(methods).nest(2))
            .append(RcDoc::hardline())
            .append(RcDoc::text("}"))
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
                    .append(ty.to_doc(genv))
            }),
            RcDoc::text(", "),
        );

        let ret_ty = self.ret_ty.to_doc(genv);

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

impl crate::tast::ExternGo {
    pub fn to_doc(&self, genv: &GlobalTypeEnv) -> RcDoc<'_, ()> {
        let params = RcDoc::intersperse(
            self.params.iter().map(|(name, ty)| {
                RcDoc::text(name.clone())
                    .append(RcDoc::text(":"))
                    .append(RcDoc::space())
                    .append(ty.to_doc(genv))
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
            .append(self.ret_ty.to_doc(genv))
    }

    pub fn to_pretty(&self, genv: &GlobalTypeEnv, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(genv).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl crate::tast::ExternType {
    pub fn to_doc(&self, _genv: &GlobalTypeEnv) -> RcDoc<'_, ()> {
        RcDoc::text("extern")
            .append(RcDoc::space())
            .append(RcDoc::text("type"))
            .append(RcDoc::space())
            .append(RcDoc::text(self.goml_name.clone()))
    }
}

impl ClosureParam {
    pub fn to_doc(&self, genv: &GlobalTypeEnv) -> RcDoc<'_, ()> {
        RcDoc::text(self.name.clone())
            .append(RcDoc::text(": "))
            .append(self.ty.to_doc(genv))
    }
}

impl Ty {
    pub fn to_doc(&self, _genv: &GlobalTypeEnv) -> RcDoc<'_, ()> {
        match self {
            Self::TVar(x) => RcDoc::text(format!("{:?}", x)),
            Self::TUnit => RcDoc::text("unit"),
            Self::TBool => RcDoc::text("bool"),
            Self::TInt8 => RcDoc::text("int8"),
            Self::TInt16 => RcDoc::text("int16"),
            Self::TInt32 => RcDoc::text("int32"),
            Self::TInt64 => RcDoc::text("int64"),
            Self::TUint8 => RcDoc::text("uint8"),
            Self::TUint16 => RcDoc::text("uint16"),
            Self::TUint32 => RcDoc::text("uint32"),
            Self::TUint64 => RcDoc::text("uint64"),
            Self::TFloat32 => RcDoc::text("float32"),
            Self::TFloat64 => RcDoc::text("float64"),
            Self::TString => RcDoc::text("string"),
            Self::TTuple { typs } => {
                let mut doc = RcDoc::text("(");

                if !typs.is_empty() {
                    let mut iter = typs.iter();
                    if let Some(first) = iter.next() {
                        doc = doc.append(first.to_doc(_genv));
                    }
                    for item in iter {
                        doc = doc.append(RcDoc::text(", ")).append(item.to_doc(_genv));
                    }
                }

                doc.append(RcDoc::text(")"))
            }
            Self::TCon { name } => RcDoc::text(name.clone()),
            Self::TApp { ty, args } => {
                let mut doc = ty.to_doc(_genv);

                if !args.is_empty() {
                    let mut iter = args.iter();
                    if let Some(first) = iter.next() {
                        doc = doc.append(RcDoc::text("[")).append(first.to_doc(_genv));
                    }
                    for item in iter {
                        doc = doc.append(RcDoc::text(", ")).append(item.to_doc(_genv));
                    }
                    doc = doc.append(RcDoc::text("]"));
                }

                doc
            }
            Self::TArray { len, elem } => RcDoc::text("[")
                .append(elem.to_doc(_genv))
                .append(RcDoc::text("; "))
                .append(RcDoc::as_string(len))
                .append(RcDoc::text("]")),
            Self::TRef { elem } => RcDoc::text("Ref[")
                .append(elem.to_doc(_genv))
                .append(RcDoc::text("]")),
            Self::TFunc { params, ret_ty } => {
                let mut doc = RcDoc::text("(");

                if !params.is_empty() {
                    let mut iter = params.iter();
                    if let Some(first) = iter.next() {
                        doc = doc.append(first.to_doc(_genv));
                    }
                    for item in iter {
                        doc = doc.append(RcDoc::text(", ")).append(item.to_doc(_genv));
                    }
                }

                doc.append(RcDoc::text(") -> "))
                    .append(ret_ty.to_doc(_genv))
            }
            Self::TParam { name } => RcDoc::text(name.clone()),
        }
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
            Self::EVar {
                name,
                ty,
                astptr: _,
            } => RcDoc::text("(")
                .append(RcDoc::text(name.clone()))
                .append(RcDoc::text(" : "))
                .append(ty.to_doc(genv))
                .append(RcDoc::text(")")),

            Self::EPrim { value, ty: _ } => RcDoc::text(value.to_string()),
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
                            args.iter().map(|arg| arg.to_doc(genv)),
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

                    if let Some(struct_def) = genv.structs().get(&struct_constructor.type_name) {
                        if struct_def.fields.is_empty() {
                            name_doc.append(RcDoc::space()).append(RcDoc::text("{}"))
                        } else if struct_def.fields.len() == args.len() {
                            let fields_doc = RcDoc::intersperse(
                                struct_def.fields.iter().zip(args.iter()).map(
                                    |((fname, _), arg)| {
                                        RcDoc::text(fname.0.clone())
                                            .append(RcDoc::text(": "))
                                            .append(arg.to_doc(genv))
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
                                args.iter().map(|arg| arg.to_doc(genv)),
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
                            args.iter().map(|arg| arg.to_doc(genv)),
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
                        items.iter().map(|item| item.to_doc(genv)),
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
                        items.iter().map(|item| item.to_doc(genv)),
                        RcDoc::text(", "),
                    );

                    RcDoc::text("[")
                        .append(items_doc)
                        .nest(2)
                        .append(RcDoc::text("]"))
                        .group()
                }
            }
            Self::EClosure {
                params,
                body,
                ty: _,
                captures: _,
            } => {
                let params_doc = if params.is_empty() {
                    RcDoc::text("||")
                } else {
                    let list = RcDoc::intersperse(
                        params.iter().map(|param| param.to_doc(genv)),
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

            Self::ELet {
                pat,
                value,
                body,
                ty: _,
            } => RcDoc::text("let")
                .append(RcDoc::space())
                .append(pat.to_doc(genv))
                .append(RcDoc::space())
                .append(RcDoc::text("="))
                .append(RcDoc::space())
                .append(value.to_doc(genv))
                .append(RcDoc::space())
                .append(RcDoc::text("in"))
                .append(RcDoc::hardline())
                .append(body.to_doc(genv))
                .group(),

            Self::EMatch {
                expr,
                arms,
                ty: _,
                astptr: _,
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
            Self::EWhile { cond, body, .. } => {
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
            Self::EBinary {
                op,
                lhs,
                rhs,
                ty: _,
                resolution: _,
            } => lhs
                .to_doc(genv)
                .append(RcDoc::space())
                .append(RcDoc::text(op.symbol()))
                .append(RcDoc::space())
                .append(rhs.to_doc(genv))
                .group(),
            Self::EUnary {
                op,
                expr,
                ty: _,
                resolution: _,
            } => RcDoc::text(op.symbol()).append(expr.to_doc(genv)).group(),

            Self::ECall { func, args, ty: _ } => {
                let args_doc =
                    RcDoc::intersperse(args.iter().map(|arg| arg.to_doc(genv)), RcDoc::text(", "));

                func.to_doc(genv)
                    .append(RcDoc::text("("))
                    .append(args_doc)
                    .append(RcDoc::text(")"))
            }
            Self::EProj {
                tuple,
                index,
                ty: _,
            } => tuple
                .to_doc(genv)
                .append(RcDoc::text("."))
                .append(RcDoc::text(index.to_string())),
            Self::EField {
                expr,
                field_name,
                ty: _,
                ..
            } => expr
                .to_doc(genv)
                .append(RcDoc::text("."))
                .append(RcDoc::text(field_name.clone())),
        }
    }

    pub fn to_pretty(&self, genv: &GlobalTypeEnv, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(genv).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Pat {
    pub fn to_doc(&self, genv: &GlobalTypeEnv) -> RcDoc<'_, ()> {
        match self {
            Pat::PVar {
                name,
                ty,
                astptr: _,
            } => RcDoc::text(name.clone())
                .append(RcDoc::text(":"))
                .append(RcDoc::space())
                .append(ty.to_doc(genv)),
            Pat::PPrim { value, ty: _ } => RcDoc::text(value.to_string()),
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
                            args.iter().map(|arg| arg.to_doc(genv)),
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

                    if let Some(struct_def) = genv.structs().get(&struct_constructor.type_name) {
                        if struct_def.fields.is_empty() {
                            name_doc.append(RcDoc::space()).append(RcDoc::text("{}"))
                        } else if struct_def.fields.len() == args.len() {
                            let fields_doc = RcDoc::intersperse(
                                struct_def.fields.iter().zip(args.iter()).map(
                                    |((fname, _), arg)| {
                                        RcDoc::text(fname.0.clone())
                                            .append(RcDoc::text(": "))
                                            .append(arg.to_doc(genv))
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
                                args.iter().map(|arg| arg.to_doc(genv)),
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
                            args.iter().map(|arg| arg.to_doc(genv)),
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
                        items.iter().map(|item| item.to_doc(genv)),
                        RcDoc::text(", "),
                    );
                    RcDoc::text("(").append(items_doc).append(RcDoc::text(")"))
                }
            }
            Pat::PWild { ty } => RcDoc::text("_")
                .append(RcDoc::text(" : "))
                .append(ty.to_doc(genv)),
        }
    }
    pub fn to_pretty(&self, genv: &GlobalTypeEnv, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(genv).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl crate::tast::Arm {
    pub fn to_doc(&self, genv: &GlobalTypeEnv) -> RcDoc<'_, ()> {
        self.pat
            .to_doc(genv)
            .append(RcDoc::space())
            .append(RcDoc::text("=>"))
            .append(RcDoc::space())
            .append(
                self.body.to_doc(genv).nest(2), // Properly indent the body of the arm
            )
            .append(RcDoc::text(","))
    }

    pub fn to_pretty(&self, genv: &GlobalTypeEnv, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(genv).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}
