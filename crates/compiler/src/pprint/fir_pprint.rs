use crate::fir;
use crate::fir::{
    Arm, Attribute, ClosureParam, Def, DefId, EnumDef, Expr, ExprId, ExternBuiltin, ExternGo,
    ExternType, File, FirIdent, FirTable, Fn, ImplBlock, Pat, PatId, StructDef, TraitDef,
    TraitMethodSignature, TypeExpr,
};
use pretty::RcDoc;

pub struct FirPrintCtx<'a> {
    pub fir_table: &'a FirTable,
}

impl<'a> FirPrintCtx<'a> {
    pub fn new(fir_table: &'a FirTable) -> Self {
        Self { fir_table }
    }

    fn expr_to_doc(&'a self, id: ExprId) -> RcDoc<'a, ()> {
        self.fir_table.expr(id).to_doc(self)
    }

    fn pat_to_doc(&'a self, id: PatId) -> RcDoc<'a, ()> {
        self.fir_table.pat(id).to_doc(self)
    }

    fn def_to_doc(&'a self, id: DefId) -> RcDoc<'a, ()> {
        match self.fir_table.def(id) {
            Def::EnumDef(def) => def.to_doc(),
            Def::StructDef(def) => def.to_doc(),
            Def::TraitDef(def) => def.to_doc(),
            Def::ImplBlock(def) => def.to_doc(self),
            Def::Fn(func) => func.to_doc(self),
            Def::ExternGo(ext) => ext.to_doc(),
            Def::ExternType(ext) => ext.to_doc(),
            Def::ExternBuiltin(ext) => ext.to_doc(),
        }
    }
}

fn attrs_doc(attrs: &[Attribute]) -> RcDoc<'_, ()> {
    if attrs.is_empty() {
        RcDoc::nil()
    } else {
        RcDoc::intersperse(
            attrs.iter().map(|attr| RcDoc::text(attr.text.clone())),
            RcDoc::hardline(),
        )
    }
}

impl TypeExpr {
    pub fn to_doc(&self) -> RcDoc<'_, ()> {
        match self {
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
    pub fn to_doc<'a>(&'a self, ctx: &'a FirPrintCtx<'a>) -> RcDoc<'a, ()> {
        match self {
            fir::Expr::ENameRef { res, .. } => RcDoc::text(res.display(ctx.fir_table)),

            fir::Expr::EUnit => RcDoc::text("()"),

            fir::Expr::EBool { value } => {
                if *value {
                    RcDoc::text("true")
                } else {
                    RcDoc::text("false")
                }
            }

            fir::Expr::EInt { value } => RcDoc::text(value.clone()),
            fir::Expr::EInt8 { value } => RcDoc::text(format!("{}i8", value)),
            fir::Expr::EInt16 { value } => RcDoc::text(format!("{}i16", value)),
            fir::Expr::EInt32 { value } => RcDoc::text(format!("{}i32", value)),
            fir::Expr::EInt64 { value } => RcDoc::text(format!("{}i64", value)),
            fir::Expr::EUInt8 { value } => RcDoc::text(format!("{}u8", value)),
            fir::Expr::EUInt16 { value } => RcDoc::text(format!("{}u16", value)),
            fir::Expr::EUInt32 { value } => RcDoc::text(format!("{}u32", value)),
            fir::Expr::EUInt64 { value } => RcDoc::text(format!("{}u64", value)),
            fir::Expr::EFloat { value } => RcDoc::text(value.to_string()),
            fir::Expr::EFloat32 { value } => RcDoc::text(format!("{}f32", value)),
            fir::Expr::EFloat64 { value } => RcDoc::text(format!("{}f64", value)),
            fir::Expr::EString { value } => RcDoc::text(format!("{:?}", value)),

            fir::Expr::EConstr { constructor, args } => {
                let prefix = RcDoc::text(constructor.display());

                if args.is_empty() {
                    prefix
                } else {
                    let args_doc = RcDoc::intersperse(
                        args.iter().map(|arg| ctx.expr_to_doc(*arg)),
                        RcDoc::text(", "),
                    );

                    prefix
                        .append(RcDoc::text("("))
                        .append(args_doc)
                        .append(RcDoc::text(")"))
                }
            }

            fir::Expr::EStructLiteral { name, fields } => {
                if fields.is_empty() {
                    RcDoc::text(name.to_ident_name())
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
                                .append(RcDoc::text(fname.to_ident_name()))
                                .append(RcDoc::text(": "))
                                .append(ctx.expr_to_doc(*expr))
                        }));

                    RcDoc::text(name.to_ident_name())
                        .append(RcDoc::space())
                        .append(RcDoc::text("{"))
                        .append(fields_doc.nest(4))
                        .append(RcDoc::hardline())
                        .append(RcDoc::text("}"))
                        .group()
                }
            }

            fir::Expr::ETuple { items } => {
                if items.is_empty() {
                    RcDoc::text("()")
                } else {
                    let items_doc = RcDoc::intersperse(
                        items.iter().map(|item| ctx.expr_to_doc(*item)),
                        RcDoc::text(", "),
                    );

                    RcDoc::text("(").append(items_doc).append(RcDoc::text(")"))
                }
            }

            fir::Expr::EArray { items } => {
                if items.is_empty() {
                    RcDoc::text("[]")
                } else {
                    let items_doc = RcDoc::intersperse(
                        items.iter().map(|item| ctx.expr_to_doc(*item)),
                        RcDoc::text(", "),
                    );

                    RcDoc::text("[").append(items_doc).append(RcDoc::text("]"))
                }
            }

            fir::Expr::EClosure { params, body } => {
                let params_doc = RcDoc::intersperse(
                    params.iter().map(|param| param.to_doc()),
                    RcDoc::text(", "),
                );

                RcDoc::text("|")
                    .append(params_doc)
                    .append(RcDoc::text("| "))
                    .append(ctx.expr_to_doc(*body))
            }
            fir::Expr::ELet {
                pat,
                annotation,
                value,
            } => {
                let base_pat_doc = ctx.pat_to_doc(*pat);
                let pat_doc = if let Some(ty) = annotation {
                    base_pat_doc.append(RcDoc::text(": ")).append(ty.to_doc())
                } else {
                    base_pat_doc
                };

                RcDoc::text("let")
                    .append(RcDoc::space())
                    .append(pat_doc)
                    .append(RcDoc::space())
                    .append(RcDoc::text("="))
                    .append(RcDoc::space())
                    .append(ctx.expr_to_doc(*value))
                    .group()
            }

            fir::Expr::EMatch { expr, arms } => {
                let match_expr = RcDoc::text("match")
                    .append(RcDoc::space())
                    .append(ctx.expr_to_doc(*expr))
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"));

                let arms_doc = RcDoc::concat(
                    arms.iter()
                        .map(|arm| RcDoc::hardline().append(arm.to_doc(ctx))),
                );

                match_expr
                    .append(arms_doc.nest(4))
                    .append(RcDoc::hardline())
                    .append(RcDoc::text("}"))
                    .group()
            }
            fir::Expr::EIf {
                cond,
                then_branch,
                else_branch,
            } => {
                let then_doc = RcDoc::hardline()
                    .append(ctx.expr_to_doc(*then_branch))
                    .nest(4)
                    .append(RcDoc::hardline());
                let else_doc = RcDoc::hardline()
                    .append(ctx.expr_to_doc(*else_branch))
                    .nest(4)
                    .append(RcDoc::hardline());

                RcDoc::text("if")
                    .append(RcDoc::space())
                    .append(ctx.expr_to_doc(*cond))
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
            fir::Expr::EWhile { cond, body } => {
                let body_doc = RcDoc::hardline()
                    .append(ctx.expr_to_doc(*body))
                    .nest(4)
                    .append(RcDoc::hardline());

                RcDoc::text("while")
                    .append(RcDoc::space())
                    .append(ctx.expr_to_doc(*cond))
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"))
                    .append(body_doc)
                    .append(RcDoc::text("}"))
                    .group()
            }

            fir::Expr::EGo { expr } => RcDoc::text("go")
                .append(RcDoc::space())
                .append(ctx.expr_to_doc(*expr)),

            fir::Expr::ECall { func, args } => {
                let func_doc = ctx.expr_to_doc(*func);
                if args.is_empty() {
                    func_doc.append(RcDoc::text("()"))
                } else {
                    let args_doc = RcDoc::intersperse(
                        args.iter().map(|arg| ctx.expr_to_doc(*arg)),
                        RcDoc::text(", "),
                    );

                    func_doc
                        .append(RcDoc::text("("))
                        .append(args_doc)
                        .nest(2)
                        .append(RcDoc::text(")"))
                        .group()
                }
            }
            fir::Expr::EUnary { op, expr } => RcDoc::text(op.symbol())
                .append(ctx.expr_to_doc(*expr))
                .group(),
            fir::Expr::EBinary { op, lhs, rhs } => ctx
                .expr_to_doc(*lhs)
                .append(RcDoc::space())
                .append(RcDoc::text(op.symbol()))
                .append(RcDoc::space())
                .append(ctx.expr_to_doc(*rhs)),
            fir::Expr::EProj { tuple, index } => ctx
                .expr_to_doc(*tuple)
                .append(RcDoc::text("."))
                .append(RcDoc::text(index.to_string())),
            fir::Expr::EField { expr, field } => ctx
                .expr_to_doc(*expr)
                .append(RcDoc::text("."))
                .append(RcDoc::text(field.to_ident_name())),
            fir::Expr::EBlock { exprs } => {
                if exprs.is_empty() {
                    RcDoc::text("{}")
                } else {
                    let exprs_doc = RcDoc::concat(exprs.iter().map(|e| {
                        RcDoc::hardline()
                            .append(ctx.expr_to_doc(*e))
                            .append(RcDoc::text(";"))
                    }));
                    RcDoc::text("{")
                        .append(exprs_doc.nest(4))
                        .append(RcDoc::hardline())
                        .append(RcDoc::text("}"))
                        .group()
                }
            }
        }
    }

    pub fn to_pretty(&self, ctx: &FirPrintCtx, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(ctx).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Pat {
    pub fn to_doc<'a>(&'a self, ctx: &'a FirPrintCtx<'a>) -> RcDoc<'a, ()> {
        match self {
            fir::Pat::PVar { name, .. } => RcDoc::text(name.to_debug_string()),
            fir::Pat::PUnit => RcDoc::text("()"),
            fir::Pat::PBool { value } => {
                if *value {
                    RcDoc::text("true")
                } else {
                    RcDoc::text("false")
                }
            }
            fir::Pat::PInt { value } => RcDoc::text(value.clone()),
            fir::Pat::PInt8 { value } => RcDoc::text(format!("{}i8", value)),
            fir::Pat::PInt16 { value } => RcDoc::text(format!("{}i16", value)),
            fir::Pat::PInt32 { value } => RcDoc::text(format!("{}i32", value)),
            fir::Pat::PInt64 { value } => RcDoc::text(format!("{}i64", value)),
            fir::Pat::PUInt8 { value } => RcDoc::text(format!("{}u8", value)),
            fir::Pat::PUInt16 { value } => RcDoc::text(format!("{}u16", value)),
            fir::Pat::PUInt32 { value } => RcDoc::text(format!("{}u32", value)),
            fir::Pat::PUInt64 { value } => RcDoc::text(format!("{}u64", value)),
            fir::Pat::PString { value } => RcDoc::text(format!("{:?}", value)),
            fir::Pat::PConstr { constructor, args } => {
                let prefix = RcDoc::text(constructor.display());

                if args.is_empty() {
                    prefix
                } else {
                    let args_doc = RcDoc::intersperse(
                        args.iter().map(|arg| ctx.pat_to_doc(*arg)),
                        RcDoc::text(", "),
                    );
                    prefix
                        .append(RcDoc::text("("))
                        .append(args_doc)
                        .append(RcDoc::text(")"))
                }
            }
            fir::Pat::PStruct { name, fields } => {
                if fields.is_empty() {
                    RcDoc::text(name.to_ident_name())
                        .append(RcDoc::space())
                        .append(RcDoc::text("{}"))
                } else {
                    let fields_doc = RcDoc::intersperse(
                        fields.iter().map(|(fname, pat)| {
                            RcDoc::text(fname.to_ident_name())
                                .append(RcDoc::text(": "))
                                .append(ctx.pat_to_doc(*pat))
                        }),
                        RcDoc::text(", "),
                    );
                    RcDoc::text(name.to_ident_name())
                        .append(RcDoc::space())
                        .append(RcDoc::text("{ "))
                        .append(fields_doc)
                        .append(RcDoc::text(" }"))
                }
            }
            fir::Pat::PTuple { pats } => {
                if pats.is_empty() {
                    RcDoc::text("()")
                } else {
                    let items_doc = RcDoc::intersperse(
                        pats.iter().map(|item| ctx.pat_to_doc(*item)),
                        RcDoc::text(", "),
                    );
                    RcDoc::text("(").append(items_doc).append(RcDoc::text(")"))
                }
            }
            fir::Pat::PWild => RcDoc::text("_"),
        }
    }

    pub fn to_pretty(&self, ctx: &FirPrintCtx, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(ctx).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Arm {
    pub fn to_doc<'a>(&'a self, ctx: &'a FirPrintCtx<'a>) -> RcDoc<'a, ()> {
        ctx.pat_to_doc(self.pat)
            .append(RcDoc::space())
            .append(RcDoc::text("=>"))
            .append(RcDoc::space())
            .append(ctx.expr_to_doc(self.body).nest(2))
            .append(RcDoc::text(","))
    }

    pub fn to_pretty(&self, ctx: &FirPrintCtx, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(ctx).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

fn generics_to_doc(generics: &[FirIdent]) -> RcDoc<'_, ()> {
    if generics.is_empty() {
        RcDoc::nil()
    } else {
        RcDoc::text("[")
            .append(RcDoc::intersperse(
                generics.iter().map(|g| RcDoc::text(g.to_ident_name())),
                RcDoc::text(", "),
            ))
            .append(RcDoc::text("]"))
    }
}

impl EnumDef {
    pub fn to_doc(&self) -> RcDoc<'_, ()> {
        let header = RcDoc::text("enum")
            .append(RcDoc::space())
            .append(RcDoc::text(self.name.to_ident_name()))
            .append(generics_to_doc(&self.generics));

        let variants_doc =
            RcDoc::concat(self.variants.iter().enumerate().map(|(i, (name, types))| {
                let variant = if i == 0 {
                    RcDoc::hardline()
                } else {
                    RcDoc::text(",").append(RcDoc::hardline())
                }
                .append(RcDoc::text(name.to_ident_name()));

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

        attrs_doc(&self.attrs).append(
            header
                .append(RcDoc::space())
                .append(RcDoc::text("{"))
                .append(variants_doc.nest(4))
                .append(RcDoc::hardline())
                .append(RcDoc::text("}"))
                .group(),
        )
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
            .append(RcDoc::text(self.name.to_ident_name()))
            .append(generics_to_doc(&self.generics));

        let fields_doc = RcDoc::concat(self.fields.iter().map(|(name, ty)| {
            RcDoc::hardline()
                .append(RcDoc::text(name.to_ident_name()))
                .append(RcDoc::text(":"))
                .append(RcDoc::space())
                .append(ty.to_doc())
                .append(RcDoc::text(","))
        }));
        attrs_doc(&self.attrs).append(
            header
                .append(RcDoc::space())
                .append(RcDoc::text("{"))
                .append(fields_doc.nest(4))
                .append(RcDoc::hardline())
                .append(RcDoc::text("}"))
                .group(),
        )
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
            .append(RcDoc::text(self.name.to_ident_name()));

        let methods_doc = RcDoc::intersperse(
            self.method_sigs.iter().map(|sig| sig.to_doc()),
            RcDoc::hardline(),
        );
        attrs_doc(&self.attrs).append(
            header
                .append(RcDoc::space())
                .append(RcDoc::text("{"))
                .append(RcDoc::hardline().append(methods_doc).nest(2))
                .append(RcDoc::hardline())
                .append(RcDoc::text("}")),
        )
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
            .append(RcDoc::text(self.name.to_ident_name()))
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
    pub fn to_doc<'a>(&'a self, ctx: &'a FirPrintCtx<'a>) -> RcDoc<'a, ()> {
        let mut base = RcDoc::text("impl");

        if !self.generics.is_empty() {
            base = base
                .append(RcDoc::text("["))
                .append(RcDoc::intersperse(
                    self.generics.iter().map(|g| RcDoc::text(g.to_ident_name())),
                    RcDoc::text(", "),
                ))
                .append(RcDoc::text("]"));
        }

        let header = if let Some(trait_name) = &self.trait_name {
            base.append(RcDoc::space())
                .append(RcDoc::text(trait_name.to_ident_name()))
                .append(RcDoc::text(" for "))
                .append(self.for_type.to_doc())
                .append(RcDoc::text(" {"))
        } else {
            base.append(RcDoc::space())
                .append(self.for_type.to_doc())
                .append(RcDoc::text(" {"))
        };

        let methods_doc = RcDoc::intersperse(
            self.methods.iter().map(|method| ctx.def_to_doc(*method)),
            RcDoc::hardline(),
        );

        attrs_doc(&self.attrs).append(
            header
                .append(RcDoc::hardline().append(methods_doc).nest(2))
                .append(RcDoc::hardline())
                .append(RcDoc::text("}")),
        )
    }

    pub fn to_pretty(&self, ctx: &FirPrintCtx, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(ctx).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Fn {
    pub fn to_doc<'a>(&'a self, ctx: &'a FirPrintCtx<'a>) -> RcDoc<'a, ()> {
        let header = RcDoc::text("fn")
            .append(RcDoc::space())
            .append(RcDoc::text(&self.name))
            .append(RcDoc::text("("));

        let params_doc = RcDoc::intersperse(
            self.params.iter().map(|(name, ty)| {
                RcDoc::text(name.to_debug_string())
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
        let body = ctx.fir_table.expr(self.body);
        attrs_doc(&self.attrs).append(
            header
                .append(params_doc)
                .append(RcDoc::text(")"))
                .append(ret_ty_doc)
                .append(RcDoc::space())
                .append(match body {
                    fir::Expr::EBlock { exprs } => {
                        if exprs.is_empty() {
                            RcDoc::text("{}")
                        } else {
                            let exprs_doc = RcDoc::concat(exprs.iter().map(|e| {
                                RcDoc::hardline()
                                    .append(ctx.expr_to_doc(*e))
                                    .append(RcDoc::text(";"))
                            }));
                            RcDoc::text("{")
                                .append(exprs_doc.nest(2))
                                .append(RcDoc::hardline())
                                .append(RcDoc::text("}"))
                                .group()
                        }
                    }
                    _ => RcDoc::text("{")
                        .append(RcDoc::hardline().append(ctx.expr_to_doc(self.body)).nest(2))
                        .append(RcDoc::hardline())
                        .append(RcDoc::text("}")),
                }),
        )
    }

    pub fn to_pretty(&self, ctx: &FirPrintCtx, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(ctx).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl ExternGo {
    pub fn to_doc(&self) -> RcDoc<'_, ()> {
        let params_doc = RcDoc::intersperse(
            self.params.iter().map(|(name, ty)| {
                RcDoc::text(name.to_ident_name())
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

        let header = RcDoc::text("extern")
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
            .append(RcDoc::text(self.goml_name.to_ident_name()))
            .append(RcDoc::text("("))
            .append(params_doc)
            .append(RcDoc::text(")"))
            .append(ret_ty_doc);

        attrs_doc(&self.attrs).append(header)
    }

    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl ExternType {
    pub fn to_doc(&self) -> RcDoc<'_, ()> {
        attrs_doc(&self.attrs).append(
            RcDoc::text("extern")
                .append(RcDoc::space())
                .append(RcDoc::text("type"))
                .append(RcDoc::space())
                .append(RcDoc::text(self.goml_name.to_ident_name())),
        )
    }
}

impl ExternBuiltin {
    pub fn to_doc(&self) -> RcDoc<'_, ()> {
        let params_doc = RcDoc::intersperse(
            self.params.iter().map(|(name, ty)| {
                RcDoc::text(name.to_ident_name())
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

        attrs_doc(&self.attrs).append(
            RcDoc::text("extern")
                .append(RcDoc::space())
                .append(RcDoc::text("fn"))
                .append(RcDoc::space())
                .append(RcDoc::text(self.name.to_ident_name()))
                .append(RcDoc::text("("))
                .append(params_doc)
                .append(RcDoc::text(")"))
                .append(ret_ty_doc),
        )
    }
}

impl File {
    pub fn to_doc<'a>(&'a self, ctx: &'a FirPrintCtx<'a>) -> RcDoc<'a, ()> {
        RcDoc::concat(self.toplevels.iter().map(|item| {
            ctx.def_to_doc(*item)
                .append(RcDoc::hardline())
                .append(RcDoc::hardline())
        }))
    }

    pub fn to_pretty(&self, ctx: &FirPrintCtx, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc(ctx).render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl ClosureParam {
    pub fn to_doc(&self) -> RcDoc<'_, ()> {
        let mut doc = RcDoc::text(self.name.to_debug_string());
        if let Some(ty) = &self.ty {
            doc = doc.append(RcDoc::text(": ")).append(ty.to_doc());
        }
        doc
    }
}
