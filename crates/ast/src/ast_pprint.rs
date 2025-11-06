use crate::ast::{
    Arm, ClosureParam, EnumDef, Expr, ExternGo, ExternType, File, Fn, ImplBlock, Item, Pat,
    StructDef, TraitDef, TraitMethodSignature, Ty, Uident,
};
use pretty::RcDoc;

impl Ty {
    pub fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            Self::TUnit => RcDoc::text("unit"),
            Self::TBool => RcDoc::text("bool"),
            Self::TInt => RcDoc::text("int"),
            Self::TString => RcDoc::text("string"),
            Self::TTuple { typs } => {
                let mut doc = RcDoc::text("(");

                if !typs.is_empty() {
                    let mut iter = typs.iter();
                    if let Some(first) = iter.next() {
                        doc = doc.append(first.to_doc());
                    }
                    for item in iter {
                        doc = doc.append(RcDoc::text(", ")).append(item.to_doc());
                    }
                }

                doc.append(RcDoc::text(")"))
            }
            Self::TCon { name } => RcDoc::text(name.clone()),
            Self::TApp { ty, args } => {
                let mut doc = ty.to_doc();
                if !args.is_empty() {
                    let mut iter = args.iter();
                    if let Some(first) = iter.next() {
                        doc = doc.append(RcDoc::text("[")).append(first.to_doc());
                    }
                    for item in iter {
                        doc = doc.append(RcDoc::text(", ")).append(item.to_doc());
                    }
                    doc = doc.append(RcDoc::text("]"));
                }
                doc
            }
            Self::TArray { len, elem } => RcDoc::text("[")
                .append(elem.to_doc())
                .append(RcDoc::text("; "))
                .append(RcDoc::as_string(len))
                .append(RcDoc::text("]")),
            Self::TRef { elem } => RcDoc::text("Ref[")
                .append(elem.to_doc())
                .append(RcDoc::text("]")),
            Self::TFunc { params, ret_ty } => {
                let mut doc = RcDoc::text("(");

                if !params.is_empty() {
                    let mut iter = params.iter();
                    if let Some(first) = iter.next() {
                        doc = doc.append(first.to_doc());
                    }
                    for item in iter {
                        doc = doc.append(RcDoc::text(", ")).append(item.to_doc());
                    }
                }

                doc.append(RcDoc::text(")"))
                    .append(RcDoc::text(" -> "))
                    .append(ret_ty.to_doc())
            }
        }
    }

    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Expr {
    pub fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            Self::EVar { name, astptr: _ } => RcDoc::text(name.0.clone()),

            Self::EUnit => RcDoc::text("()"),

            Self::EBool { value } => {
                if *value {
                    RcDoc::text("true")
                } else {
                    RcDoc::text("false")
                }
            }

            Self::EInt { value } => RcDoc::text(value.to_string()),
            Self::EString { value } => RcDoc::text(format!("{:?}", value)),

            Self::EConstr { vcon, args } => {
                let prefix = RcDoc::text(vcon.0.clone());

                if args.is_empty() {
                    prefix
                } else {
                    let args_doc =
                        RcDoc::intersperse(args.iter().map(|arg| arg.to_doc()), RcDoc::text(", "));

                    prefix
                        .append(RcDoc::text("("))
                        .append(args_doc)
                        .append(RcDoc::text(")"))
                }
            }

            Self::EStructLiteral { name, fields } => {
                if fields.is_empty() {
                    RcDoc::text(name.0.clone())
                        .append(RcDoc::space())
                        .append(RcDoc::text("{}"))
                } else {
                    let fields_doc =
                        RcDoc::concat(fields.iter().enumerate().map(|(i, (fname, expr))| {
                            let prefix = if i == 0 {
                                RcDoc::hardline()
                            } else {
                                RcDoc::text(",").append(RcDoc::hardline())
                            };
                            prefix
                                .append(RcDoc::text(&fname.0))
                                .append(RcDoc::text(": "))
                                .append(expr.to_doc())
                        }));

                    RcDoc::text(name.0.clone())
                        .append(RcDoc::space())
                        .append(RcDoc::text("{"))
                        .append(fields_doc.nest(4))
                        .append(RcDoc::hardline())
                        .append(RcDoc::text("}"))
                        .group()
                }
            }

            Self::ETuple { items } => {
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

            Self::EArray { items } => {
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

            Self::EClosure { params, body } => {
                let params_doc = RcDoc::intersperse(
                    params.iter().map(|param| param.to_doc()),
                    RcDoc::text(", "),
                );

                RcDoc::text("|")
                    .append(params_doc)
                    .append(RcDoc::text("| "))
                    .append(body.to_doc())
            }
            Self::ELet { pat, value, body } => RcDoc::text("let")
                .append(RcDoc::space())
                .append(pat.to_doc())
                .append(RcDoc::space())
                .append(RcDoc::text("="))
                .append(RcDoc::space())
                .append(value.to_doc())
                .append(RcDoc::space())
                .append(RcDoc::text("in"))
                .append(RcDoc::hardline())
                .append(body.to_doc())
                .group(),

            Self::EMatch { expr, arms } => {
                let match_expr = RcDoc::text("match")
                    .append(RcDoc::space())
                    .append(expr.to_doc())
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"));

                let arms_doc = RcDoc::concat(
                    arms.iter()
                        .map(|arm| RcDoc::hardline().append(arm.to_doc())),
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
            } => {
                let then_doc = RcDoc::hardline()
                    .append(then_branch.to_doc())
                    .nest(4)
                    .append(RcDoc::hardline());
                let else_doc = RcDoc::hardline()
                    .append(else_branch.to_doc())
                    .nest(4)
                    .append(RcDoc::hardline());

                RcDoc::text("if")
                    .append(RcDoc::space())
                    .append(cond.to_doc())
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"))
                    .append(then_doc)
                    .append(RcDoc::text("}"))
                    .append(RcDoc::space())
                    .append(RcDoc::text("else"))
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"))
                    .append(else_doc)
                    .append(RcDoc::text("}"))
                    .group()
            }
            Self::EWhile { cond, body } => {
                let body_doc = RcDoc::hardline()
                    .append(body.to_doc())
                    .nest(4)
                    .append(RcDoc::hardline());

                RcDoc::text("while")
                    .append(RcDoc::space())
                    .append(cond.to_doc())
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"))
                    .append(body_doc)
                    .append(RcDoc::text("}"))
                    .group()
            }

            Self::ECall { func, args } => {
                let func_doc = func.to_doc();
                if args.is_empty() {
                    func_doc.append(RcDoc::text("()"))
                } else {
                    let args_doc =
                        RcDoc::intersperse(args.iter().map(|arg| arg.to_doc()), RcDoc::text(", "));

                    func_doc
                        .append(RcDoc::text("("))
                        .append(args_doc)
                        .nest(2)
                        .append(RcDoc::text(")"))
                        .group()
                }
            }
            Self::EUnary { op, expr } => RcDoc::text(op.symbol()).append(expr.to_doc()).group(),
            Self::EBinary { op, lhs, rhs } => lhs
                .to_doc()
                .append(RcDoc::space())
                .append(RcDoc::text(op.symbol()))
                .append(RcDoc::space())
                .append(rhs.to_doc()),
            Self::EProj { tuple, index } => tuple
                .to_doc()
                .append(RcDoc::text("."))
                .append(RcDoc::text(index.to_string())),
        }
    }

    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Pat {
    pub fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            Pat::PVar { name, astptr: _ } => RcDoc::text(name.0.clone()),
            Pat::PUnit => RcDoc::text("()"),
            Pat::PBool { value } => {
                if *value {
                    RcDoc::text("true")
                } else {
                    RcDoc::text("false")
                }
            }
            Pat::PInt { value } => RcDoc::text(value.to_string()),
            Pat::PString { value } => RcDoc::text(format!("{:?}", value)),
            Pat::PConstr { vcon, args } => {
                let prefix = RcDoc::text(vcon.0.clone());

                if args.is_empty() {
                    prefix
                } else {
                    let args_doc =
                        RcDoc::intersperse(args.iter().map(|arg| arg.to_doc()), RcDoc::text(", "));
                    prefix
                        .append(RcDoc::text("("))
                        .append(args_doc)
                        .append(RcDoc::text(")"))
                }
            }
            Pat::PStruct { name, fields } => {
                if fields.is_empty() {
                    RcDoc::text(name.0.clone())
                        .append(RcDoc::space())
                        .append(RcDoc::text("{}"))
                } else {
                    let fields_doc = RcDoc::intersperse(
                        fields.iter().map(|(fname, pat)| {
                            RcDoc::text(fname.0.clone())
                                .append(RcDoc::text(": "))
                                .append(pat.to_doc())
                        }),
                        RcDoc::text(", "),
                    );
                    RcDoc::text(name.0.clone())
                        .append(RcDoc::space())
                        .append(RcDoc::text("{ "))
                        .append(fields_doc)
                        .append(RcDoc::text(" }"))
                }
            }
            Pat::PTuple { pats } => {
                if pats.is_empty() {
                    RcDoc::text("()")
                } else {
                    let items_doc = RcDoc::intersperse(
                        pats.iter().map(|item| item.to_doc()),
                        RcDoc::text(", "),
                    );
                    RcDoc::text("(").append(items_doc).append(RcDoc::text(")"))
                }
            }
            Pat::PWild => RcDoc::text("_"),
        }
    }

    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Arm {
    pub fn to_doc(&self) -> RcDoc<'_, ()> {
        self.pat
            .to_doc()
            .append(RcDoc::space())
            .append(RcDoc::text("=>"))
            .append(RcDoc::space())
            .append(
                self.body.to_doc().nest(2), // Properly indent the body of the arm
            )
            .append(RcDoc::text(","))
    }

    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

fn generics_to_doc(generics: &[Uident]) -> RcDoc<'_, ()> {
    if generics.is_empty() {
        RcDoc::nil()
    } else {
        RcDoc::text("[")
            .append(RcDoc::intersperse(
                generics.iter().map(|g| RcDoc::text(g.0.clone())),
                RcDoc::text(", "),
            ))
            .append(RcDoc::text("]"))
    }
}

impl EnumDef {
    pub fn to_doc(&self) -> RcDoc<'_, ()> {
        let header = RcDoc::text("enum")
            .append(RcDoc::space())
            .append(RcDoc::text(&self.name.0))
            .append(generics_to_doc(&self.generics));

        let variants_doc =
            RcDoc::concat(self.variants.iter().enumerate().map(|(i, (name, types))| {
                let variant = if i == 0 {
                    RcDoc::hardline()
                } else {
                    RcDoc::text(",").append(RcDoc::hardline())
                }
                .append(RcDoc::text(&name.0));

                if types.is_empty() {
                    variant
                } else {
                    let types_doc =
                        RcDoc::intersperse(types.iter().map(|ty| ty.to_doc()), RcDoc::text(", "));

                    variant
                        .append(RcDoc::text("("))
                        .append(types_doc)
                        .append(RcDoc::text(")"))
                }
            }));

        header
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(variants_doc.nest(4))
            .append(RcDoc::hardline())
            .append(RcDoc::text("}"))
            .group()
    }

    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl StructDef {
    pub fn to_doc(&self) -> RcDoc<'_, ()> {
        let header = RcDoc::text("struct")
            .append(RcDoc::space())
            .append(RcDoc::text(&self.name.0))
            .append(generics_to_doc(&self.generics));

        let fields_doc = RcDoc::concat(self.fields.iter().map(|(name, ty)| {
            RcDoc::hardline()
                .append(RcDoc::text(&name.0))
                .append(RcDoc::text(":"))
                .append(RcDoc::space())
                .append(ty.to_doc())
                .append(RcDoc::text(","))
        }));

        header
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(fields_doc.nest(4))
            .append(RcDoc::hardline())
            .append(RcDoc::text("}"))
            .group()
    }

    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl TraitDef {
    pub fn to_doc(&self) -> RcDoc<'_, ()> {
        let header = RcDoc::text("trait")
            .append(RcDoc::space())
            .append(RcDoc::text(&self.name.0));

        let methods_doc = RcDoc::intersperse(
            self.method_sigs.iter().map(|sig| sig.to_doc()),
            RcDoc::hardline(),
        );

        header
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(RcDoc::hardline().append(methods_doc).nest(2))
            .append(RcDoc::hardline())
            .append(RcDoc::text("}"))
    }

    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl TraitMethodSignature {
    pub fn to_doc(&self) -> RcDoc<'_, ()> {
        let header = RcDoc::text("fn")
            .append(RcDoc::space())
            .append(RcDoc::text(&self.name.0))
            .append(RcDoc::text("("));

        let params_doc =
            RcDoc::intersperse(self.params.iter().map(|ty| ty.to_doc()), RcDoc::text(", "));

        let ret_ty_doc = RcDoc::text(" -> ").append(self.ret_ty.to_doc());

        header
            .append(params_doc)
            .append(RcDoc::text(")"))
            .append(ret_ty_doc)
            .append(RcDoc::text(";"))
    }

    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl ImplBlock {
    pub fn to_doc(&self) -> RcDoc<'_, ()> {
        let header = RcDoc::text("impl")
            .append(RcDoc::space())
            .append(RcDoc::text(&self.trait_name.0))
            .append(RcDoc::text(" for "))
            .append(self.for_type.to_doc())
            .append(RcDoc::text(" {"));

        let methods_doc = RcDoc::intersperse(
            self.methods.iter().map(|method| method.to_doc()),
            RcDoc::hardline(),
        );

        header
            .append(RcDoc::hardline().append(methods_doc).nest(2))
            .append(RcDoc::hardline())
            .append(RcDoc::text("}"))
    }

    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Fn {
    pub fn to_doc(&self) -> RcDoc<'_, ()> {
        let header = RcDoc::text("fn")
            .append(RcDoc::space())
            .append(RcDoc::text(&self.name.0))
            .append(RcDoc::text("("));

        let params_doc = RcDoc::intersperse(
            self.params.iter().map(|(name, ty)| {
                RcDoc::text(name.0.clone())
                    .append(RcDoc::text(":"))
                    .append(RcDoc::space())
                    .append(ty.to_doc())
            }),
            RcDoc::text(", "),
        );

        let ret_ty_doc = if let Some(ret_ty) = &self.ret_ty {
            RcDoc::text(" -> ").append(ret_ty.to_doc())
        } else {
            RcDoc::nil()
        };

        header
            .append(params_doc)
            .append(RcDoc::text(")"))
            .append(ret_ty_doc)
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(RcDoc::hardline().append(self.body.to_doc()).nest(4))
            .append(RcDoc::hardline())
            .append(RcDoc::text("}"))
    }

    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl ExternGo {
    pub fn to_doc(&self) -> RcDoc<'_, ()> {
        let params_doc = RcDoc::intersperse(
            self.params.iter().map(|(name, ty)| {
                RcDoc::text(name.0.clone())
                    .append(RcDoc::text(":"))
                    .append(RcDoc::space())
                    .append(ty.to_doc())
            }),
            RcDoc::text(", "),
        );

        let ret_ty_doc = if let Some(ret_ty) = &self.ret_ty {
            RcDoc::text(" -> ").append(ret_ty.to_doc())
        } else {
            RcDoc::nil()
        };

        RcDoc::text("extern")
            .append(RcDoc::space())
            .append(RcDoc::text("\"go\""))
            .append(RcDoc::space())
            .append(RcDoc::text(format!("\"{}\"", self.package_path)))
            .append(if self.explicit_go_symbol {
                RcDoc::space().append(RcDoc::text(format!("\"{}\"", self.go_symbol)))
            } else {
                RcDoc::nil()
            })
            .append(RcDoc::space())
            .append(RcDoc::text(self.goml_name.0.clone()))
            .append(RcDoc::text("("))
            .append(params_doc)
            .append(RcDoc::text(")"))
            .append(ret_ty_doc)
    }

    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl ExternType {
    pub fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::text("extern")
            .append(RcDoc::space())
            .append(RcDoc::text("type"))
            .append(RcDoc::space())
            .append(RcDoc::text(self.goml_name.0.clone()))
    }
}

impl File {
    pub fn to_doc(&self) -> RcDoc<'_, ()> {
        RcDoc::concat(self.toplevels.iter().map(|item| {
            item.to_doc()
                .append(RcDoc::hardline())
                .append(RcDoc::hardline())
        }))
    }

    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Item {
    pub fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
            Item::EnumDef(def) => def.to_doc(),
            Item::StructDef(def) => def.to_doc(),
            Item::TraitDef(def) => def.to_doc(),
            Item::ImplBlock(def) => def.to_doc(),
            Item::Fn(func) => func.to_doc(),
            Item::ExternGo(ext) => ext.to_doc(),
            Item::ExternType(ext) => ext.to_doc(),
        }
    }

    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl ClosureParam {
    pub fn to_doc(&self) -> RcDoc<'_, ()> {
        let mut doc = RcDoc::text(self.name.0.clone());
        if let Some(ty) = &self.ty {
            doc = doc.append(RcDoc::text(": ")).append(ty.to_doc());
        }
        doc
    }
}
