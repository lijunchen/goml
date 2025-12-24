use std::collections::HashMap;

use pretty::RcDoc;

use crate::hir::{
    ir::*,
    lower::{HirLowerResult, HirTables},
    symbol_table::SymbolTable,
};

pub struct HirPrintCtx<'a> {
    pub tables: &'a HirTables,
    pub symbols: &'a SymbolTable,

    /// Reverse mappings
    pub item_names: HashMap<ItemId, String>,
    pub type_names: HashMap<TypeId, String>,
    pub trait_names: HashMap<TraitId, String>,
    pub field_names: HashMap<FieldId, String>,
    pub ctor_names: HashMap<CtorId, (String, String)>,
    pub local_names: HashMap<LocalId, String>,
    pub generic_names: HashMap<GenericParamId, String>,
}

impl<'a> HirPrintCtx<'a> {
    pub fn new(tables: &'a HirTables, symbols: &'a SymbolTable) -> Self {
        let mut ctx = Self {
            tables,
            symbols,
            item_names: HashMap::new(),
            type_names: HashMap::new(),
            trait_names: HashMap::new(),
            field_names: HashMap::new(),
            ctor_names: HashMap::new(),
            local_names: HashMap::new(),
            generic_names: HashMap::new(),
        };

        for (name, &id) in &symbols.globals.value_items {
            ctx.item_names.insert(id, name.clone());
        }

        for (name, &id) in &symbols.globals.types {
            ctx.type_names.insert(id, name.clone());
        }

        for (name, &id) in &symbols.globals.traits {
            ctx.trait_names.insert(id, name.clone());
        }

        for fields in symbols.fields.map.values() {
            for (name, &field_id) in fields {
                ctx.field_names.insert(field_id, name.clone());
            }
        }

        for ((type_id, variant_name), &ctor_id) in &symbols.globals.ctors {
            let type_name = ctx
                .type_names
                .get(type_id)
                .cloned()
                .unwrap_or_else(|| format!("Type{}", type_id.0));
            ctx.ctor_names
                .insert(ctor_id, (type_name, variant_name.clone()));
        }

        ctx
    }

    pub fn get_item_name(&self, id: ItemId) -> String {
        self.item_names
            .get(&id)
            .cloned()
            .unwrap_or_else(|| format!("item_{}", id.0))
    }

    pub fn get_type_name(&self, id: TypeId) -> String {
        self.type_names
            .get(&id)
            .cloned()
            .unwrap_or_else(|| format!("Type{}", id.0))
    }

    pub fn get_trait_name(&self, id: TraitId) -> String {
        self.trait_names
            .get(&id)
            .cloned()
            .unwrap_or_else(|| format!("Trait{}", id.0))
    }

    pub fn get_field_name(&self, id: FieldId) -> String {
        self.field_names
            .get(&id)
            .cloned()
            .unwrap_or_else(|| format!("field_{}", id.0))
    }

    pub fn get_ctor_name(&self, id: CtorId) -> (String, String) {
        self.ctor_names
            .get(&id)
            .cloned()
            .unwrap_or_else(|| (format!("Type{}", id.0), format!("Variant{}", id.0)))
    }

    pub fn get_local_name(&self, id: LocalId) -> String {
        self.local_names
            .get(&id)
            .cloned()
            .unwrap_or_else(|| format!("#{}", id.0))
    }

    pub fn get_generic_name(&self, id: GenericParamId) -> String {
        self.generic_names
            .get(&id)
            .cloned()
            .unwrap_or_else(|| format!("T{}", id.0))
    }

    pub fn register_local(&mut self, id: LocalId, name: String) {
        self.local_names.insert(id, name);
    }

    pub fn register_generic(&mut self, id: GenericParamId, name: String) {
        self.generic_names.insert(id, name);
    }
}

impl HirLowerResult {
    pub fn to_doc(&self) -> RcDoc<'_, ()> {
        let ctx = HirPrintCtx::new(&self.tables, &self.symbols);
        self.file.to_doc(&ctx)
    }

    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl HirFile {
    pub fn to_doc<'a>(&self, ctx: &HirPrintCtx<'a>) -> RcDoc<'a, ()> {
        RcDoc::intersperse(
            self.items.iter().map(|&item_id| {
                let item = &ctx.tables.items[item_id];
                item.to_doc_with_id(ctx, item_id)
            }),
            RcDoc::hardline().append(RcDoc::hardline()),
        )
    }
}

impl HirItem {
    pub fn to_doc<'a>(&self, ctx: &HirPrintCtx<'a>) -> RcDoc<'a, ()> {
        match self {
            HirItem::Fn(f) => f.to_doc(ctx),
            HirItem::Struct(s) => s.to_doc(ctx),
            HirItem::Enum(e) => e.to_doc(ctx),
            HirItem::Trait(t) => t.to_doc(ctx),
            HirItem::ImplTrait(i) => i.to_doc(ctx),
            HirItem::ImplInherent(i) => i.to_doc(ctx),
            HirItem::ExternGo(e) => e.to_doc(ctx),
            HirItem::ExternType(e) => e.to_doc(ctx),
            HirItem::ExternBuiltin(e) => e.to_doc(ctx),
        }
    }

    pub fn to_doc_with_id<'a>(&self, ctx: &HirPrintCtx<'a>, id: HirItemId) -> RcDoc<'a, ()> {
        let base_doc = self.to_doc(ctx);
        base_doc.append(RcDoc::text(format!(" ITEM{}", id.into_raw().into_u32())))
    }
}

impl HirFn {
    pub fn to_doc<'a>(&self, ctx: &HirPrintCtx<'a>) -> RcDoc<'a, ()> {
        let name = ctx.get_item_name(self.fn_id);

        let generics = if self.generics.is_empty() {
            RcDoc::nil()
        } else {
            let params = RcDoc::intersperse(
                self.generics
                    .iter()
                    .map(|&g| RcDoc::text(ctx.get_generic_name(g))),
                RcDoc::text(", "),
            );
            RcDoc::text("[").append(params).append(RcDoc::text("]"))
        };

        let params = RcDoc::intersperse(
            self.params.iter().map(|(local_id, ty_id)| {
                RcDoc::text(ctx.get_local_name(*local_id))
                    .append(RcDoc::text(": "))
                    .append(ctx.tables.types[*ty_id].to_doc(ctx))
            }),
            RcDoc::text(", "),
        );

        let ret_ty = ctx.tables.types[self.ret_ty].to_doc(ctx);
        let body = match ctx.tables.exprs[self.body].kind {
            HirExprKind::Block { .. } => ctx.tables.exprs[self.body].to_doc(ctx),
            _ => RcDoc::text("{")
                .append(
                    RcDoc::hardline()
                        .append(ctx.tables.exprs[self.body].to_doc(ctx))
                        .nest(2),
                )
                .append(RcDoc::hardline())
                .append(RcDoc::text("}")),
        };

        RcDoc::text("fn")
            .append(RcDoc::space())
            .append(RcDoc::text(name))
            .append(generics)
            .append(RcDoc::text("("))
            .append(params)
            .append(RcDoc::text(")"))
            .append(RcDoc::space())
            .append(RcDoc::text("->"))
            .append(RcDoc::space())
            .append(ret_ty)
            .append(RcDoc::space())
            .append(body)
    }
}

impl HirStruct {
    pub fn to_doc<'a>(&self, ctx: &HirPrintCtx<'a>) -> RcDoc<'a, ()> {
        let name = ctx.get_type_name(self.type_id);

        let generics = if self.generics.is_empty() {
            RcDoc::nil()
        } else {
            let params = RcDoc::intersperse(
                self.generics
                    .iter()
                    .map(|&g| RcDoc::text(ctx.get_generic_name(g))),
                RcDoc::text(", "),
            );
            RcDoc::text("[").append(params).append(RcDoc::text("]"))
        };

        let fields = RcDoc::intersperse(
            self.fields.iter().map(|(field_id, ty_id)| {
                RcDoc::text(ctx.get_field_name(*field_id))
                    .append(RcDoc::text(": "))
                    .append(ctx.tables.types[*ty_id].to_doc(ctx))
            }),
            RcDoc::text(", "),
        );

        RcDoc::text("struct")
            .append(RcDoc::space())
            .append(RcDoc::text(name))
            .append(generics)
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(RcDoc::space())
            .append(fields)
            .append(RcDoc::space())
            .append(RcDoc::text("}"))
    }
}

impl HirEnum {
    pub fn to_doc<'a>(&self, ctx: &HirPrintCtx<'a>) -> RcDoc<'a, ()> {
        let name = ctx.get_type_name(self.type_id);

        let generics = if self.generics.is_empty() {
            RcDoc::nil()
        } else {
            let params = RcDoc::intersperse(
                self.generics
                    .iter()
                    .map(|&g| RcDoc::text(ctx.get_generic_name(g))),
                RcDoc::text(", "),
            );
            RcDoc::text("[").append(params).append(RcDoc::text("]"))
        };

        let variants = RcDoc::intersperse(
            self.variants.iter().map(|v| v.to_doc(ctx)),
            RcDoc::text(", "),
        );

        RcDoc::text("enum")
            .append(RcDoc::space())
            .append(RcDoc::text(name))
            .append(generics)
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(RcDoc::space())
            .append(variants)
            .append(RcDoc::space())
            .append(RcDoc::text("}"))
    }
}

impl HirEnumVariant {
    pub fn to_doc<'a>(&self, ctx: &HirPrintCtx<'a>) -> RcDoc<'a, ()> {
        let (_, variant_name) = ctx.get_ctor_name(self.ctor_id);

        if self.payload.is_empty() {
            RcDoc::text(variant_name)
        } else {
            let types = RcDoc::intersperse(
                self.payload
                    .iter()
                    .map(|ty_id| ctx.tables.types[*ty_id].to_doc(ctx)),
                RcDoc::text(", "),
            );
            RcDoc::text(variant_name)
                .append(RcDoc::text("("))
                .append(types)
                .append(RcDoc::text(")"))
        }
    }
}

impl HirTrait {
    pub fn to_doc<'a>(&self, ctx: &HirPrintCtx<'a>) -> RcDoc<'a, ()> {
        let name = ctx.get_trait_name(self.trait_id);

        let methods = RcDoc::intersperse(
            self.methods.iter().map(|m| m.to_doc(ctx)),
            RcDoc::hardline(),
        );

        RcDoc::text("trait")
            .append(RcDoc::space())
            .append(RcDoc::text(name))
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(RcDoc::hardline().append(methods).nest(2))
            .append(RcDoc::hardline())
            .append(RcDoc::text("}"))
    }
}

impl HirTraitMethodSig {
    pub fn to_doc<'a>(&self, ctx: &HirPrintCtx<'a>) -> RcDoc<'a, ()> {
        let name = ctx.get_item_name(self.name);

        let params = RcDoc::intersperse(
            self.params
                .iter()
                .map(|ty_id| ctx.tables.types[*ty_id].to_doc(ctx)),
            RcDoc::text(", "),
        );

        let ret_ty = ctx.tables.types[self.ret_ty].to_doc(ctx);

        RcDoc::text("fn")
            .append(RcDoc::space())
            .append(RcDoc::text(name))
            .append(RcDoc::text("("))
            .append(params)
            .append(RcDoc::text(")"))
            .append(RcDoc::space())
            .append(RcDoc::text("->"))
            .append(RcDoc::space())
            .append(ret_ty)
    }
}

impl HirImplTrait {
    pub fn to_doc<'a>(&self, ctx: &HirPrintCtx<'a>) -> RcDoc<'a, ()> {
        let trait_name = ctx.get_trait_name(self.trait_id);
        let for_type = ctx.tables.types[self.for_type].to_doc(ctx);

        let generics = if self.generics.is_empty() {
            RcDoc::nil()
        } else {
            let params = RcDoc::intersperse(
                self.generics
                    .iter()
                    .map(|&g| RcDoc::text(ctx.get_generic_name(g))),
                RcDoc::text(", "),
            );
            RcDoc::text("[").append(params).append(RcDoc::text("]"))
        };

        let methods = RcDoc::intersperse(
            self.methods.iter().map(|&item_id| {
                let item = &ctx.tables.items[item_id];
                item.to_doc_with_id(ctx, item_id)
            }),
            RcDoc::hardline(),
        );

        RcDoc::text("impl")
            .append(generics)
            .append(RcDoc::space())
            .append(RcDoc::text(trait_name))
            .append(RcDoc::space())
            .append(RcDoc::text("for"))
            .append(RcDoc::space())
            .append(for_type)
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(RcDoc::hardline().append(methods).nest(2))
            .append(RcDoc::hardline())
            .append(RcDoc::text("}"))
    }
}

impl HirImplInherent {
    pub fn to_doc<'a>(&self, ctx: &HirPrintCtx<'a>) -> RcDoc<'a, ()> {
        let for_type = ctx.tables.types[self.for_type].to_doc(ctx);

        let methods = RcDoc::intersperse(
            self.methods.iter().map(|&item_id| {
                let item = &ctx.tables.items[item_id];
                item.to_doc_with_id(ctx, item_id)
            }),
            RcDoc::hardline(),
        );

        RcDoc::text("impl")
            .append(RcDoc::space())
            .append(for_type)
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(RcDoc::hardline().append(methods).nest(2))
            .append(RcDoc::hardline())
            .append(RcDoc::text("}"))
    }
}

impl HirExternGo {
    pub fn to_doc<'a>(&self, ctx: &HirPrintCtx<'a>) -> RcDoc<'a, ()> {
        let name = ctx.get_item_name(self.item_id);

        let params = RcDoc::intersperse(
            self.params.iter().map(|(local_id, ty_id)| {
                RcDoc::text(ctx.get_local_name(*local_id))
                    .append(RcDoc::text(": "))
                    .append(ctx.tables.types[*ty_id].to_doc(ctx))
            }),
            RcDoc::text(", "),
        );

        let ret_ty = ctx.tables.types[self.ret_ty].to_doc(ctx);

        RcDoc::text("extern")
            .append(RcDoc::space())
            .append(RcDoc::text("\"go\""))
            .append(RcDoc::space())
            .append(RcDoc::text(format!("\"{}\"", self.package_path)))
            .append(RcDoc::space())
            .append(RcDoc::text(name))
            .append(RcDoc::text("("))
            .append(params)
            .append(RcDoc::text(")"))
            .append(RcDoc::space())
            .append(RcDoc::text("->"))
            .append(RcDoc::space())
            .append(ret_ty)
    }
}

impl HirExternType {
    pub fn to_doc<'a>(&self, ctx: &HirPrintCtx<'a>) -> RcDoc<'a, ()> {
        let name = ctx.get_type_name(self.type_id);

        RcDoc::text("extern")
            .append(RcDoc::space())
            .append(RcDoc::text("type"))
            .append(RcDoc::space())
            .append(RcDoc::text(name))
    }
}

impl HirExternBuiltin {
    pub fn to_doc<'a>(&self, ctx: &HirPrintCtx<'a>) -> RcDoc<'a, ()> {
        let name = ctx.get_item_name(self.item_id);

        let params = RcDoc::intersperse(
            self.params.iter().map(|(local_id, ty_id)| {
                RcDoc::text(ctx.get_local_name(*local_id))
                    .append(RcDoc::text(": "))
                    .append(ctx.tables.types[*ty_id].to_doc(ctx))
            }),
            RcDoc::text(", "),
        );

        let ret_ty = ctx.tables.types[self.ret_ty].to_doc(ctx);

        RcDoc::text("extern")
            .append(RcDoc::space())
            .append(RcDoc::text("builtin"))
            .append(RcDoc::space())
            .append(RcDoc::text(name))
            .append(RcDoc::text("("))
            .append(params)
            .append(RcDoc::text(")"))
            .append(RcDoc::space())
            .append(RcDoc::text("->"))
            .append(RcDoc::space())
            .append(ret_ty)
    }
}

// Types

impl HirType {
    pub fn to_doc<'a>(&self, ctx: &HirPrintCtx<'a>) -> RcDoc<'a, ()> {
        match self {
            HirType::Builtin(b) => b.to_doc(),
            HirType::Tuple(items) => {
                if items.is_empty() {
                    RcDoc::text("()")
                } else {
                    let items_doc = RcDoc::intersperse(
                        items
                            .iter()
                            .map(|ty_id| ctx.tables.types[*ty_id].to_doc(ctx)),
                        RcDoc::text(", "),
                    );
                    RcDoc::text("(").append(items_doc).append(RcDoc::text(")"))
                }
            }
            HirType::Func { params, ret } => {
                let params_doc = RcDoc::intersperse(
                    params
                        .iter()
                        .map(|ty_id| ctx.tables.types[*ty_id].to_doc(ctx)),
                    RcDoc::text(", "),
                );
                let ret_doc = ctx.tables.types[*ret].to_doc(ctx);
                RcDoc::text("fn(")
                    .append(params_doc)
                    .append(RcDoc::text(")"))
                    .append(RcDoc::space())
                    .append(RcDoc::text("->"))
                    .append(RcDoc::space())
                    .append(ret_doc)
            }
            HirType::Array { elem, len } => {
                let elem_doc = ctx.tables.types[*elem].to_doc(ctx);
                RcDoc::text("[")
                    .append(elem_doc)
                    .append(RcDoc::text("; "))
                    .append(RcDoc::text(len.to_string()))
                    .append(RcDoc::text("]"))
            }
            HirType::App { base, args } => {
                let base_doc = ctx.tables.types[*base].to_doc(ctx);
                let args_doc = RcDoc::intersperse(
                    args.iter()
                        .map(|ty_id| ctx.tables.types[*ty_id].to_doc(ctx)),
                    RcDoc::text(", "),
                );
                base_doc
                    .append(RcDoc::text("["))
                    .append(args_doc)
                    .append(RcDoc::text("]"))
            }
            HirType::Named(type_id, args) => {
                let name = ctx.get_type_name(*type_id);
                if args.is_empty() {
                    RcDoc::text(name)
                } else {
                    let args_doc = RcDoc::intersperse(
                        args.iter()
                            .map(|ty_id| ctx.tables.types[*ty_id].to_doc(ctx)),
                        RcDoc::text(", "),
                    );
                    RcDoc::text(name)
                        .append(RcDoc::text("["))
                        .append(args_doc)
                        .append(RcDoc::text("]"))
                }
            }
            HirType::Generic(g) => RcDoc::text(ctx.get_generic_name(*g)),
        }
    }
}

impl BuiltinType {
    pub fn to_doc<'a>(&self) -> RcDoc<'a, ()> {
        let name = match self {
            BuiltinType::Unit => "()",
            BuiltinType::Bool => "bool",
            BuiltinType::Int8 => "int8",
            BuiltinType::Int16 => "int16",
            BuiltinType::Int32 => "int32",
            BuiltinType::Int64 => "int64",
            BuiltinType::Uint8 => "uint8",
            BuiltinType::Uint16 => "uint16",
            BuiltinType::Uint32 => "uint32",
            BuiltinType::Uint64 => "uint64",
            BuiltinType::Float32 => "float32",
            BuiltinType::Float64 => "float64",
            BuiltinType::String => "string",
        };
        RcDoc::text(name)
    }
}

// Expressions

impl HirExpr {
    pub fn to_doc<'a>(&self, ctx: &HirPrintCtx<'a>) -> RcDoc<'a, ()> {
        self.kind.to_doc(ctx)
    }

    pub fn to_doc_with_id<'a>(&self, ctx: &HirPrintCtx<'a>, id: HirExprId) -> RcDoc<'a, ()> {
        self.kind
            .to_doc(ctx)
            .append(RcDoc::text(format!(" ϵ{}", id.into_raw().into_u32())))
    }
}

impl HirExprKind {
    pub fn to_doc<'a>(&self, ctx: &HirPrintCtx<'a>) -> RcDoc<'a, ()> {
        match self {
            HirExprKind::LocalVar(id) => RcDoc::text(ctx.get_local_name(*id)),
            HirExprKind::GlobalItem(id) => RcDoc::text(ctx.get_item_name(*id)),
            HirExprKind::Unit => RcDoc::text("()"),
            HirExprKind::Bool(b) => RcDoc::text(b.to_string()),
            HirExprKind::Int(s) => RcDoc::text(s.clone()),
            HirExprKind::Int8(s) => RcDoc::text(format!("{}i8", s)),
            HirExprKind::Int16(s) => RcDoc::text(format!("{}i16", s)),
            HirExprKind::Int32(s) => RcDoc::text(format!("{}i32", s)),
            HirExprKind::Int64(s) => RcDoc::text(format!("{}i64", s)),
            HirExprKind::UInt8(s) => RcDoc::text(format!("{}u8", s)),
            HirExprKind::UInt16(s) => RcDoc::text(format!("{}u16", s)),
            HirExprKind::UInt32(s) => RcDoc::text(format!("{}u32", s)),
            HirExprKind::UInt64(s) => RcDoc::text(format!("{}u64", s)),
            HirExprKind::Float(f) => RcDoc::text(f.to_string()),
            HirExprKind::Float32(s) => RcDoc::text(format!("{}f32", s)),
            HirExprKind::Float64(s) => RcDoc::text(format!("{}f64", s)),
            HirExprKind::String(s) => RcDoc::text(format!("\"{}\"", s.escape_default())),
            HirExprKind::EnumCtor { ctor_id, args } => {
                let (type_name, variant_name) = ctx.get_ctor_name(*ctor_id);
                let name_doc = RcDoc::text(format!("{}::{}", type_name, variant_name));

                if args.is_empty() {
                    name_doc
                } else {
                    let args_doc = RcDoc::intersperse(
                        args.iter().map(|expr_id| {
                            ctx.tables.exprs[*expr_id].to_doc_with_id(ctx, *expr_id)
                        }),
                        RcDoc::text(", "),
                    );
                    name_doc
                        .append(RcDoc::text("("))
                        .append(args_doc)
                        .append(RcDoc::text(")"))
                }
            }
            HirExprKind::UnresolvedEnumCtor { name, args } => {
                if args.is_empty() {
                    RcDoc::text(name.clone())
                } else {
                    let args_doc = RcDoc::intersperse(
                        args.iter().map(|expr_id| {
                            ctx.tables.exprs[*expr_id].to_doc_with_id(ctx, *expr_id)
                        }),
                        RcDoc::text(", "),
                    );
                    RcDoc::text(name.clone())
                        .append(RcDoc::text("("))
                        .append(args_doc)
                        .append(RcDoc::text(")"))
                }
            }
            HirExprKind::StructLiteral { type_id, fields } => {
                let name = ctx.get_type_name(*type_id);
                let fields_doc = RcDoc::intersperse(
                    fields.iter().map(|(field_id, expr_id)| {
                        RcDoc::text(ctx.get_field_name(*field_id))
                            .append(RcDoc::text(": "))
                            .append(ctx.tables.exprs[*expr_id].to_doc_with_id(ctx, *expr_id))
                    }),
                    RcDoc::text(", "),
                );
                RcDoc::text(name)
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"))
                    .append(RcDoc::space())
                    .append(fields_doc)
                    .append(RcDoc::space())
                    .append(RcDoc::text("}"))
            }
            HirExprKind::Tuple(items) => {
                if items.is_empty() {
                    RcDoc::text("()")
                } else {
                    let items_doc = RcDoc::intersperse(
                        items.iter().map(|expr_id| {
                            ctx.tables.exprs[*expr_id].to_doc_with_id(ctx, *expr_id)
                        }),
                        RcDoc::text(", "),
                    );
                    RcDoc::text("(").append(items_doc).append(RcDoc::text(")"))
                }
            }
            HirExprKind::Array(items) => {
                if items.is_empty() {
                    RcDoc::text("[]")
                } else {
                    let items_doc = RcDoc::intersperse(
                        items.iter().map(|expr_id| {
                            ctx.tables.exprs[*expr_id].to_doc_with_id(ctx, *expr_id)
                        }),
                        RcDoc::text(", "),
                    );
                    RcDoc::text("[").append(items_doc).append(RcDoc::text("]"))
                }
            }
            HirExprKind::Let {
                pat,
                value,
                annotation,
            } => {
                let pat_doc = ctx.tables.pats[*pat].to_doc_with_id(ctx, *pat);
                let value_doc = ctx.tables.exprs[*value].to_doc_with_id(ctx, *value);

                let mut let_doc = RcDoc::text("let").append(RcDoc::space()).append(pat_doc);

                if let Some(ty_id) = annotation {
                    let_doc = let_doc
                        .append(RcDoc::text(": "))
                        .append(ctx.tables.types[*ty_id].to_doc(ctx));
                }

                let_doc
                    .append(RcDoc::space())
                    .append(RcDoc::text("="))
                    .append(RcDoc::space())
                    .append(value_doc)
                    .group()
            }
            HirExprKind::Block { exprs } => {
                if exprs.is_empty() {
                    RcDoc::text("{}")
                } else {
                    let exprs_doc = RcDoc::intersperse(
                        exprs.iter().map(|expr_id| {
                            ctx.tables.exprs[*expr_id].to_doc_with_id(ctx, *expr_id)
                        }),
                        RcDoc::text(";").append(RcDoc::hardline()),
                    );
                    RcDoc::text("{")
                        .append(RcDoc::hardline())
                        .append(exprs_doc)
                        .nest(2)
                        .append(RcDoc::hardline())
                        .append(RcDoc::text("}"))
                        .group()
                }
            }
            HirExprKind::Closure { params, body } => {
                let params_doc = if params.is_empty() {
                    RcDoc::text("||")
                } else {
                    let list = RcDoc::intersperse(
                        params.iter().map(|id| RcDoc::text(ctx.get_local_name(*id))),
                        RcDoc::text(", "),
                    );
                    RcDoc::text("|").append(list).append(RcDoc::text("|"))
                };
                let body_doc = ctx.tables.exprs[*body].to_doc_with_id(ctx, *body);

                params_doc
                    .append(RcDoc::space())
                    .append(RcDoc::text("=>"))
                    .append(RcDoc::space())
                    .append(body_doc)
                    .group()
            }
            HirExprKind::Match { scrutinee, arms } => {
                let scrutinee_doc = ctx.tables.exprs[*scrutinee].to_doc_with_id(ctx, *scrutinee);
                let arms_doc =
                    RcDoc::intersperse(arms.iter().map(|arm| arm.to_doc(ctx)), RcDoc::hardline());

                RcDoc::text("match")
                    .append(RcDoc::space())
                    .append(scrutinee_doc)
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"))
                    .append(RcDoc::hardline().append(arms_doc).nest(2))
                    .append(RcDoc::hardline())
                    .append(RcDoc::text("}"))
            }
            HirExprKind::If {
                cond,
                then_branch,
                else_branch,
            } => {
                let cond_doc = ctx.tables.exprs[*cond].to_doc_with_id(ctx, *cond);
                let then_doc = ctx.tables.exprs[*then_branch].to_doc_with_id(ctx, *then_branch);
                let else_doc = ctx.tables.exprs[*else_branch].to_doc_with_id(ctx, *else_branch);

                RcDoc::text("if")
                    .append(RcDoc::space())
                    .append(cond_doc)
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"))
                    .append(RcDoc::hardline().append(then_doc).nest(2))
                    .append(RcDoc::hardline())
                    .append(RcDoc::text("}"))
                    .append(RcDoc::space())
                    .append(RcDoc::text("else"))
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"))
                    .append(RcDoc::hardline().append(else_doc).nest(2))
                    .append(RcDoc::hardline())
                    .append(RcDoc::text("}"))
            }
            HirExprKind::While { cond, body } => {
                let cond_doc = ctx.tables.exprs[*cond].to_doc_with_id(ctx, *cond);
                let body_doc = ctx.tables.exprs[*body].to_doc_with_id(ctx, *body);

                RcDoc::text("while")
                    .append(RcDoc::space())
                    .append(cond_doc)
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"))
                    .append(RcDoc::hardline().append(body_doc).nest(2))
                    .append(RcDoc::hardline())
                    .append(RcDoc::text("}"))
            }
            HirExprKind::Go { expr } => {
                let expr_doc = ctx.tables.exprs[*expr].to_doc_with_id(ctx, *expr);
                RcDoc::text("go").append(RcDoc::space()).append(expr_doc)
            }
            HirExprKind::Call { callee, args } => {
                let callee_doc = ctx.tables.exprs[*callee].to_doc_with_id(ctx, *callee);
                let args_doc = RcDoc::intersperse(
                    args.iter()
                        .map(|expr_id| ctx.tables.exprs[*expr_id].to_doc_with_id(ctx, *expr_id)),
                    RcDoc::text(", "),
                );

                callee_doc
                    .append(RcDoc::text("("))
                    .append(args_doc)
                    .append(RcDoc::text(")"))
            }
            HirExprKind::MethodCall {
                recv,
                method_name,
                args,
                explicit_trait,
                explicit_type,
            } => {
                let recv_doc = ctx.tables.exprs[*recv].to_doc_with_id(ctx, *recv);
                let args_doc = RcDoc::intersperse(
                    args.iter()
                        .map(|expr_id| ctx.tables.exprs[*expr_id].to_doc_with_id(ctx, *expr_id)),
                    RcDoc::text(", "),
                );

                let mut method_doc = recv_doc.append(RcDoc::text("."));

                if let Some(trait_id) = explicit_trait {
                    method_doc = method_doc
                        .append(RcDoc::text(ctx.get_trait_name(*trait_id)))
                        .append(RcDoc::text("::"));
                } else if let Some(type_id) = explicit_type {
                    method_doc = method_doc
                        .append(RcDoc::text(ctx.get_type_name(*type_id)))
                        .append(RcDoc::text("::"));
                }

                method_doc
                    .append(RcDoc::text(method_name.clone()))
                    .append(RcDoc::text("("))
                    .append(args_doc)
                    .append(RcDoc::text(")"))
            }
            HirExprKind::Unary { op, expr } => {
                let op_str = match op {
                    ast::ast::UnaryOp::Neg => "-",
                    ast::ast::UnaryOp::Not => "!",
                };
                let expr_doc = ctx.tables.exprs[*expr].to_doc_with_id(ctx, *expr);
                RcDoc::text(op_str).append(expr_doc)
            }
            HirExprKind::Binary { op, lhs, rhs } => {
                let op_str = match op {
                    ast::ast::BinaryOp::Add => "+",
                    ast::ast::BinaryOp::Sub => "-",
                    ast::ast::BinaryOp::Mul => "*",
                    ast::ast::BinaryOp::Div => "/",
                    ast::ast::BinaryOp::And => "&&",
                    ast::ast::BinaryOp::Or => "||",
                    ast::ast::BinaryOp::Less => "<",
                    ast::ast::BinaryOp::Greater => ">",
                    ast::ast::BinaryOp::LessEq => "<=",
                    ast::ast::BinaryOp::GreaterEq => ">=",
                    ast::ast::BinaryOp::Eq => "==",
                    ast::ast::BinaryOp::NotEq => "!=",
                };
                let lhs_doc = ctx.tables.exprs[*lhs].to_doc_with_id(ctx, *lhs);
                let rhs_doc = ctx.tables.exprs[*rhs].to_doc_with_id(ctx, *rhs);

                lhs_doc
                    .append(RcDoc::space())
                    .append(RcDoc::text(op_str))
                    .append(RcDoc::space())
                    .append(rhs_doc)
            }
            HirExprKind::Projection { expr, index } => {
                let expr_doc = ctx.tables.exprs[*expr].to_doc_with_id(ctx, *expr);
                expr_doc
                    .append(RcDoc::text("."))
                    .append(RcDoc::text(index.to_string()))
            }
            HirExprKind::Field { expr, field_name } => {
                let expr_doc = ctx.tables.exprs[*expr].to_doc_with_id(ctx, *expr);
                expr_doc
                    .append(RcDoc::text("."))
                    .append(RcDoc::text(field_name.clone()))
            }
        }
    }
}

impl HirArm {
    pub fn to_doc<'a>(&self, ctx: &HirPrintCtx<'a>) -> RcDoc<'a, ()> {
        let pat_doc = ctx.tables.pats[self.pat].to_doc_with_id(ctx, self.pat);
        let body_doc = ctx.tables.exprs[self.body].to_doc_with_id(ctx, self.body);

        pat_doc
            .append(RcDoc::space())
            .append(RcDoc::text("=>"))
            .append(RcDoc::space())
            .append(body_doc)
            .append(RcDoc::text(","))
    }
}

// Patterns

impl HirPat {
    pub fn to_doc<'a>(&self, ctx: &HirPrintCtx<'a>) -> RcDoc<'a, ()> {
        self.kind.to_doc(ctx)
    }

    pub fn to_doc_with_id<'a>(&self, ctx: &HirPrintCtx<'a>, id: HirPatId) -> RcDoc<'a, ()> {
        self.kind
            .to_doc(ctx)
            .append(RcDoc::text(format!(" ρ{}", id.into_raw().into_u32())))
    }
}

impl HirPatKind {
    pub fn to_doc<'a>(&self, ctx: &HirPrintCtx<'a>) -> RcDoc<'a, ()> {
        match self {
            HirPatKind::Wild => RcDoc::text("_"),
            HirPatKind::Unit => RcDoc::text("()"),
            HirPatKind::Bool(b) => RcDoc::text(b.to_string()),
            HirPatKind::Int(s) => RcDoc::text(s.clone()),
            HirPatKind::Int8(s) => RcDoc::text(format!("{}i8", s)),
            HirPatKind::Int16(s) => RcDoc::text(format!("{}i16", s)),
            HirPatKind::Int32(s) => RcDoc::text(format!("{}i32", s)),
            HirPatKind::Int64(s) => RcDoc::text(format!("{}i64", s)),
            HirPatKind::UInt8(s) => RcDoc::text(format!("{}u8", s)),
            HirPatKind::UInt16(s) => RcDoc::text(format!("{}u16", s)),
            HirPatKind::UInt32(s) => RcDoc::text(format!("{}u32", s)),
            HirPatKind::UInt64(s) => RcDoc::text(format!("{}u64", s)),
            HirPatKind::String(s) => RcDoc::text(format!("\"{}\"", s.escape_default())),
            HirPatKind::Var(id) => RcDoc::text(ctx.get_local_name(*id)),
            HirPatKind::Tuple(items) => {
                if items.is_empty() {
                    RcDoc::text("()")
                } else {
                    let items_doc = RcDoc::intersperse(
                        items
                            .iter()
                            .map(|pat_id| ctx.tables.pats[*pat_id].to_doc_with_id(ctx, *pat_id)),
                        RcDoc::text(", "),
                    );
                    RcDoc::text("(").append(items_doc).append(RcDoc::text(")"))
                }
            }
            HirPatKind::Struct { type_id, fields } => {
                let name = ctx.get_type_name(*type_id);
                let fields_doc = RcDoc::intersperse(
                    fields.iter().map(|(field_id, pat_id)| {
                        RcDoc::text(ctx.get_field_name(*field_id))
                            .append(RcDoc::text(": "))
                            .append(ctx.tables.pats[*pat_id].to_doc_with_id(ctx, *pat_id))
                    }),
                    RcDoc::text(", "),
                );
                RcDoc::text(name)
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"))
                    .append(RcDoc::space())
                    .append(fields_doc)
                    .append(RcDoc::space())
                    .append(RcDoc::text("}"))
            }
            HirPatKind::Enum { ctor_id, args } => {
                let (type_name, variant_name) = ctx.get_ctor_name(*ctor_id);
                let name_doc = RcDoc::text(format!("{}::{}", type_name, variant_name));

                if args.is_empty() {
                    name_doc
                } else {
                    let args_doc = RcDoc::intersperse(
                        args.iter()
                            .map(|pat_id| ctx.tables.pats[*pat_id].to_doc_with_id(ctx, *pat_id)),
                        RcDoc::text(", "),
                    );
                    name_doc
                        .append(RcDoc::text("("))
                        .append(args_doc)
                        .append(RcDoc::text(")"))
                }
            }
            HirPatKind::UnresolvedVariant(name) => RcDoc::text(name.clone()),
        }
    }
}
