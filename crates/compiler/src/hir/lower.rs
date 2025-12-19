use std::collections::HashMap;

use ast::ast;
use diagnostics::{Diagnostic, Diagnostics, Severity, Stage};
use parser::syntax::MySyntaxNodePtr;

use super::ir::*;
use super::symbol_table::{GenericTable, SymbolTable};

#[derive(Debug, Default, Clone)]
pub struct HirTables {
    pub items: la_arena::Arena<HirItem>,
    pub exprs: la_arena::Arena<HirExpr>,
    pub pats: la_arena::Arena<HirPat>,
    pub types: la_arena::Arena<HirType>,
}

#[derive(Debug)]
pub struct HirLowerResult {
    pub file: HirFile,
    pub tables: HirTables,
    pub symbols: SymbolTable,
    pub diagnostics: Diagnostics,
}

impl HirLowerResult {
    pub fn has_errors(&self) -> bool {
        self.diagnostics.has_errors()
    }
}

pub fn lower_file(file: ast::File) -> HirLowerResult {
    let mut ctx = LowerCtx::new();
    let mut diagnostics = Diagnostics::new();
    ctx.pre_scan(&file, &mut diagnostics);
    let hir_file = ctx.lower_file(file, &mut diagnostics);

    HirLowerResult {
        file: hir_file,
        tables: ctx.tables,
        symbols: ctx.symbols,
        diagnostics,
    }
}

pub fn lower_file_with_builtins(builtins: ast::File, file: ast::File) -> HirLowerResult {
    let mut ctx = LowerCtx::new();
    let mut diagnostics = Diagnostics::new();
    ctx.register_builtin_types(&mut diagnostics);
    ctx.pre_scan(&builtins, &mut diagnostics);
    ctx.pre_scan(&file, &mut diagnostics);
    let hir_file = ctx.lower_file(file, &mut diagnostics);

    HirLowerResult {
        file: hir_file,
        tables: ctx.tables,
        symbols: ctx.symbols,
        diagnostics,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TypeKind {
    Struct,
    Enum,
    ExternType,
}

struct LowerCtx {
    stage: Stage,
    symbols: SymbolTable,
    tables: HirTables,

    next_item_id: u32,
    next_type_id: u32,
    next_trait_id: u32,
    next_impl_id: u32,
    next_ctor_id: u32,
    next_field_id: u32,
    next_generic_id: u32,

    type_kinds: HashMap<TypeId, TypeKind>,
    struct_field_order: HashMap<TypeId, Vec<String>>,
    trait_method_ids: HashMap<(TraitId, String), ItemId>,
    impl_cursor: usize,
}

impl LowerCtx {
    fn new() -> Self {
        Self {
            stage: Stage::other("hir"),
            symbols: SymbolTable::default(),
            tables: HirTables::default(),
            next_item_id: 0,
            next_type_id: 0,
            next_trait_id: 0,
            next_impl_id: 0,
            next_ctor_id: 0,
            next_field_id: 0,
            next_generic_id: 0,
            type_kinds: HashMap::new(),
            struct_field_order: HashMap::new(),
            trait_method_ids: HashMap::new(),
            impl_cursor: 0,
        }
    }

    fn pre_scan(&mut self, file: &ast::File, diagnostics: &mut Diagnostics) {
        for item in &file.toplevels {
            match item {
                ast::Item::Fn(func) => {
                    let name = func.name.0.clone();
                    let item_id = self.alloc_item_id();
                    self.insert_value_item(name, item_id, diagnostics);
                }
                ast::Item::ExternGo(ext) => {
                    let name = ext.goml_name.0.clone();
                    let item_id = self.alloc_item_id();
                    self.insert_value_item(name, item_id, diagnostics);
                }
                ast::Item::ExternBuiltin(ext) => {
                    let name = ext.name.0.clone();
                    let item_id = self.alloc_item_id();
                    self.insert_value_item(name, item_id, diagnostics);
                }
                ast::Item::StructDef(def) => self.pre_scan_struct(def, diagnostics),
                ast::Item::EnumDef(def) => self.pre_scan_enum(def, diagnostics),
                ast::Item::TraitDef(def) => self.pre_scan_trait(def, diagnostics),
                ast::Item::ImplBlock(block) => self.pre_scan_impl(block),
                ast::Item::ExternType(ext) => {
                    let name = ext.goml_name.0.clone();
                    let type_id = self.alloc_type_id();
                    self.insert_type(name, type_id, diagnostics);
                    self.type_kinds.insert(type_id, TypeKind::ExternType);
                }
            }
        }
    }

    fn pre_scan_struct(&mut self, def: &ast::StructDef, diagnostics: &mut Diagnostics) {
        let name = def.name.0.clone();
        let type_id = self.alloc_type_id();
        self.insert_type(name.clone(), type_id, diagnostics);
        self.type_kinds.insert(type_id, TypeKind::Struct);

        let mut field_map = HashMap::new();
        let mut order = Vec::new();
        for (field_ident, _) in &def.fields {
            let field_name = field_ident.0.clone();
            let field_id = self.alloc_field_id();
            if field_map.insert(field_name.clone(), field_id).is_some() {
                self.push_error(
                    format!("duplicate field `{}` in struct", field_name),
                    diagnostics,
                );
            }
            order.push(field_name);
        }
        self.struct_field_order.insert(type_id, order);
        self.symbols.fields.map.insert(type_id, field_map);

        // Treat struct constructors as ctor entries so bare `Foo` can be resolved.
        let ctor_id = self.alloc_ctor_id();
        self.symbols
            .globals
            .ctors
            .insert((type_id, def.name.0.clone()), ctor_id);
        self.symbols
            .globals
            .ctor_by_name
            .entry(def.name.0.clone())
            .or_default()
            .push(ctor_id);
    }

    fn pre_scan_enum(&mut self, def: &ast::EnumDef, diagnostics: &mut Diagnostics) {
        let name = def.name.0.clone();
        let type_id = self.alloc_type_id();
        self.insert_type(name.clone(), type_id, diagnostics);
        self.type_kinds.insert(type_id, TypeKind::Enum);

        for (variant_name, _) in &def.variants {
            let ctor_id = self.alloc_ctor_id();
            let vname = variant_name.0.clone();
            self.symbols
                .globals
                .ctors
                .insert((type_id, vname.clone()), ctor_id);
            self.symbols
                .globals
                .ctor_by_name
                .entry(vname)
                .or_default()
                .push(ctor_id);
        }
    }

    fn pre_scan_trait(&mut self, def: &ast::TraitDef, diagnostics: &mut Diagnostics) {
        let name = def.name.0.clone();
        let trait_id = self.alloc_trait_id();
        self.insert_trait(name, trait_id, diagnostics);

        for sig in &def.method_sigs {
            let item_id = self.alloc_item_id();
            self.trait_method_ids
                .insert((trait_id, sig.name.0.clone()), item_id);
        }
    }

    fn pre_scan_impl(&mut self, _block: &ast::ImplBlock) {
        let impl_id = self.alloc_impl_id();
        self.symbols.globals.impls.push(impl_id);
    }

    /// Register built-in generic types (Ref, Vec) and functions that are not defined in AST
    /// These are added dynamically by the compiler in builtins.rs
    fn register_builtin_types(&mut self, diagnostics: &mut Diagnostics) {
        // Register Ref[T] as an extern type
        let ref_type_id = self.alloc_type_id();
        self.insert_type("Ref".to_string(), ref_type_id, diagnostics);
        self.type_kinds.insert(ref_type_id, TypeKind::ExternType);

        // Register Vec[T] as an extern type
        let vec_type_id = self.alloc_type_id();
        self.insert_type("Vec".to_string(), vec_type_id, diagnostics);
        self.type_kinds.insert(vec_type_id, TypeKind::ExternType);

        // Register dynamically-added builtin functions (from add_ref_builtins, add_vec_builtins, add_array_builtins)
        let builtin_funcs = [
            "ref",
            "ref_get",
            "ref_set",
            "vec_new",
            "vec_push",
            "vec_get",
            "vec_len",
            "array_get",
            "array_set",
        ];
        for name in builtin_funcs {
            let item_id = self.alloc_item_id();
            self.insert_value_item(name.to_string(), item_id, diagnostics);
        }
    }

    fn lower_file(&mut self, file: ast::File, diagnostics: &mut Diagnostics) -> HirFile {
        let mut items = Vec::new();
        for item in file.toplevels {
            if let Some(hir_item) = self.lower_item(item, diagnostics) {
                items.push(hir_item);
            }
        }
        HirFile { items }
    }

    fn lower_item(&mut self, item: ast::Item, diagnostics: &mut Diagnostics) -> Option<HirItemId> {
        match item {
            ast::Item::Fn(func) => self.lower_fn(func, None, None, diagnostics),
            ast::Item::StructDef(def) => self.lower_struct(def, diagnostics),
            ast::Item::EnumDef(def) => self.lower_enum(def, diagnostics),
            ast::Item::TraitDef(def) => self.lower_trait(def, diagnostics),
            ast::Item::ImplBlock(block) => self.lower_impl(block, diagnostics),
            ast::Item::ExternGo(ext) => self.lower_extern_go(ext, diagnostics),
            ast::Item::ExternBuiltin(ext) => self.lower_extern_builtin(ext, diagnostics),
            ast::Item::ExternType(ext) => self.lower_extern_type(ext),
        }
    }

    fn lower_fn(
        &mut self,
        func: ast::Fn,
        parent_generics: Option<&GenericTable>,
        impl_item_id: Option<ItemId>,
        diagnostics: &mut Diagnostics,
    ) -> Option<HirItemId> {
        let item_id = impl_item_id.unwrap_or_else(|| {
            match self.symbols.globals.value_items.get(&func.name.0) {
                Some(id) => *id,
                None => {
                    // Methods inside impl blocks are not part of the global namespace.
                    self.alloc_item_id()
                }
            }
        });

        let (fn_generics, generics_table) = self.extend_generics(parent_generics, &func.generics);

        self.symbols.locals.clear();
        self.symbols.locals.enter_scope();

        let mut params = Vec::new();
        for (ident, ty_expr) in func.params {
            let ty_id = match self.lower_type_expr(&ty_expr, &generics_table, diagnostics) {
                Some(id) => id,
                None => {
                    // Use unit type as placeholder if type lowering fails
                    // Still register the parameter in locals so body can reference it
                    self.push_type(HirType::Builtin(BuiltinType::Unit))
                }
            };
            let local_id = self.symbols.locals.insert(ident.0);
            params.push((local_id, ty_id));
        }

        let ret_ty = match func.ret_ty {
            Some(ty) => self
                .lower_type_expr(&ty, &generics_table, diagnostics)
                .unwrap_or_else(|| self.push_type(HirType::Builtin(BuiltinType::Unit))),
            None => self.push_type(HirType::Builtin(BuiltinType::Unit)),
        };

        let body = match self.lower_expr(&func.body, &generics_table, diagnostics) {
            Some(id) => id,
            None => {
                self.symbols.locals.exit_scope();
                return None;
            }
        };

        self.symbols.locals.exit_scope();

        let hir_fn = HirFn {
            fn_id: item_id,
            generics: fn_generics,
            params,
            ret_ty,
            body,
            astptr: None,
        };

        Some(self.push_item(HirItem::Fn(hir_fn)))
    }

    fn lower_struct(
        &mut self,
        def: ast::StructDef,
        diagnostics: &mut Diagnostics,
    ) -> Option<HirItemId> {
        let type_id = match self.symbols.globals.types.get(&def.name.0) {
            Some(id) => *id,
            None => {
                self.push_error(format!("unknown struct `{}`", def.name.0), diagnostics);
                return None;
            }
        };

        let (generics, generic_table) = self.extend_generics(None, &def.generics);

        let mut fields = Vec::new();
        for (ident, ty_expr) in def.fields {
            let field_map = self.symbols.fields.map.get(&type_id);
            let field_id = match field_map.and_then(|m| m.get(&ident.0)) {
                Some(id) => *id,
                None => {
                    self.push_error(
                        format!("unknown field `{}` on struct", ident.0),
                        diagnostics,
                    );
                    continue;
                }
            };
            let ty_id = match self.lower_type_expr(&ty_expr, &generic_table, diagnostics) {
                Some(id) => id,
                None => continue,
            };
            fields.push((field_id, ty_id));
        }

        let hir_struct = HirStruct {
            type_id,
            generics,
            fields,
            astptr: None,
        };

        Some(self.push_item(HirItem::Struct(hir_struct)))
    }

    fn lower_enum(
        &mut self,
        def: ast::EnumDef,
        diagnostics: &mut Diagnostics,
    ) -> Option<HirItemId> {
        let type_id = match self.symbols.globals.types.get(&def.name.0) {
            Some(id) => *id,
            None => {
                self.push_error(format!("unknown enum `{}`", def.name.0), diagnostics);
                return None;
            }
        };

        let (generics, generic_table) = self.extend_generics(None, &def.generics);

        let mut variants = Vec::new();
        for (variant_ident, payload) in def.variants {
            let ctor_id = match self
                .symbols
                .globals
                .ctors
                .get(&(type_id, variant_ident.0.clone()))
            {
                Some(id) => *id,
                None => {
                    self.push_error(
                        format!("missing constructor id for variant `{}`", variant_ident.0),
                        diagnostics,
                    );
                    continue;
                }
            };

            let mut payload_ids = Vec::new();
            for ty_expr in payload {
                if let Some(id) = self.lower_type_expr(&ty_expr, &generic_table, diagnostics) {
                    payload_ids.push(id);
                }
            }

            variants.push(HirEnumVariant {
                ctor_id,
                payload: payload_ids,
                astptr: None,
            });
        }

        let hir_enum = HirEnum {
            type_id,
            generics,
            variants,
            astptr: None,
        };

        Some(self.push_item(HirItem::Enum(hir_enum)))
    }

    fn lower_trait(
        &mut self,
        def: ast::TraitDef,
        diagnostics: &mut Diagnostics,
    ) -> Option<HirItemId> {
        let trait_id = match self.symbols.globals.traits.get(&def.name.0) {
            Some(id) => *id,
            None => {
                self.push_error(format!("unknown trait `{}`", def.name.0), diagnostics);
                return None;
            }
        };

        let mut methods = Vec::new();
        for sig in def.method_sigs {
            let item_id = self
                .trait_method_ids
                .get(&(trait_id, sig.name.0.clone()))
                .copied()
                .unwrap_or_else(|| self.alloc_item_id());

            let params = sig
                .params
                .iter()
                .filter_map(|ty| self.lower_type_expr(ty, &GenericTable::new(), diagnostics))
                .collect();

            let ret_ty = self
                .lower_type_expr(&sig.ret_ty, &GenericTable::new(), diagnostics)
                .unwrap_or_else(|| self.push_type(HirType::Builtin(BuiltinType::Unit)));

            methods.push(HirTraitMethodSig {
                name: item_id,
                params,
                ret_ty,
                astptr: None,
            });
        }

        let hir_trait = HirTrait {
            trait_id,
            methods,
            astptr: None,
        };

        Some(self.push_item(HirItem::Trait(hir_trait)))
    }

    fn lower_impl(
        &mut self,
        block: ast::ImplBlock,
        diagnostics: &mut Diagnostics,
    ) -> Option<HirItemId> {
        let impl_id = if self.impl_cursor < self.symbols.globals.impls.len() {
            let id = self.symbols.globals.impls[self.impl_cursor];
            self.impl_cursor += 1;
            id
        } else {
            self.alloc_impl_id()
        };

        let (impl_generics, generics_table) = self.extend_generics(None, &block.generics);

        let for_type = self.lower_type_expr(&block.for_type, &generics_table, diagnostics)?;

        let mut method_ids = Vec::new();
        for method in block.methods {
            let fn_item_id = self.alloc_item_id();
            if let Some(hir_fn_id) =
                self.lower_fn(method, Some(&generics_table), Some(fn_item_id), diagnostics)
            {
                method_ids.push(hir_fn_id);
            }
        }

        if let Some(trait_ident) = block.trait_name {
            let trait_id = match self.symbols.globals.traits.get(&trait_ident.0) {
                Some(id) => *id,
                None => {
                    self.push_error(
                        format!("unknown trait `{}` in impl", trait_ident.0),
                        diagnostics,
                    );
                    return None;
                }
            };

            let hir_impl = HirImplTrait {
                impl_id,
                trait_id,
                for_type,
                generics: impl_generics,
                methods: method_ids,
                astptr: None,
            };

            Some(self.push_item(HirItem::ImplTrait(hir_impl)))
        } else {
            let hir_impl = HirImplInherent {
                impl_id,
                for_type,
                methods: method_ids,
                astptr: None,
            };

            Some(self.push_item(HirItem::ImplInherent(hir_impl)))
        }
    }

    fn lower_extern_go(
        &mut self,
        ext: ast::ExternGo,
        diagnostics: &mut Diagnostics,
    ) -> Option<HirItemId> {
        let item_id = match self.symbols.globals.value_items.get(&ext.goml_name.0) {
            Some(id) => *id,
            None => self.alloc_item_id(),
        };

        self.symbols.locals.clear();
        self.symbols.locals.enter_scope();

        let mut params = Vec::new();
        for (ident, ty_expr) in ext.params {
            let ty_id = match self.lower_type_expr(&ty_expr, &GenericTable::new(), diagnostics) {
                Some(id) => id,
                None => continue,
            };
            let local_id = self.symbols.locals.insert(ident.0);
            params.push((local_id, ty_id));
        }

        self.symbols.locals.exit_scope();

        let ret_ty = match ext.ret_ty {
            Some(ty) => self
                .lower_type_expr(&ty, &GenericTable::new(), diagnostics)
                .unwrap_or_else(|| self.push_type(HirType::Builtin(BuiltinType::Unit))),
            None => self.push_type(HirType::Builtin(BuiltinType::Unit)),
        };

        let extern_go = HirExternGo {
            item_id,
            package_path: ext.package_path,
            go_symbol: ext.go_symbol,
            explicit_go_symbol: ext.explicit_go_symbol,
            params,
            ret_ty,
            astptr: None,
        };

        Some(self.push_item(HirItem::ExternGo(extern_go)))
    }

    fn lower_extern_builtin(
        &mut self,
        ext: ast::ExternBuiltin,
        diagnostics: &mut Diagnostics,
    ) -> Option<HirItemId> {
        let item_id = match self.symbols.globals.value_items.get(&ext.name.0) {
            Some(id) => *id,
            None => self.alloc_item_id(),
        };

        self.symbols.locals.clear();
        self.symbols.locals.enter_scope();

        let mut params = Vec::new();
        for (ident, ty_expr) in ext.params {
            let ty_id = match self.lower_type_expr(&ty_expr, &GenericTable::new(), diagnostics) {
                Some(id) => id,
                None => continue,
            };
            let local_id = self.symbols.locals.insert(ident.0);
            params.push((local_id, ty_id));
        }

        self.symbols.locals.exit_scope();

        let ret_ty = match ext.ret_ty {
            Some(ty) => self
                .lower_type_expr(&ty, &GenericTable::new(), diagnostics)
                .unwrap_or_else(|| self.push_type(HirType::Builtin(BuiltinType::Unit))),
            None => self.push_type(HirType::Builtin(BuiltinType::Unit)),
        };

        let extern_builtin = HirExternBuiltin {
            item_id,
            params,
            ret_ty,
            astptr: None,
        };

        Some(self.push_item(HirItem::ExternBuiltin(extern_builtin)))
    }

    fn lower_extern_type(&mut self, ext: ast::ExternType) -> Option<HirItemId> {
        let type_id = match self.symbols.globals.types.get(&ext.goml_name.0) {
            Some(id) => *id,
            None => self.alloc_type_id(),
        };

        let extern_type = HirExternType {
            type_id,
            astptr: None,
        };

        Some(self.push_item(HirItem::ExternType(extern_type)))
    }

    fn lower_expr(
        &mut self,
        expr: &ast::Expr,
        generics: &GenericTable,
        diagnostics: &mut Diagnostics,
    ) -> Option<HirExprId> {
        match expr {
            ast::Expr::EPath { path, astptr } => {
                self.lower_path_expr(path, Some(*astptr), generics, diagnostics)
            }
            ast::Expr::EUnit => Some(self.push_expr(HirExprKind::Unit, None)),
            ast::Expr::EBool { value } => Some(self.push_expr(HirExprKind::Bool(*value), None)),
            ast::Expr::EInt { value } => {
                Some(self.push_expr(HirExprKind::Int(value.clone()), None))
            }
            ast::Expr::EFloat { value } => Some(self.push_expr(HirExprKind::Float(*value), None)),
            ast::Expr::EString { value } => {
                Some(self.push_expr(HirExprKind::String(value.clone()), None))
            }
            ast::Expr::EConstr { constructor, args } => {
                self.lower_constructor_expr(constructor, args, None, generics, diagnostics)
            }
            ast::Expr::EStructLiteral { name, fields } => {
                self.lower_struct_literal(name, fields, None, generics, diagnostics)
            }
            ast::Expr::ETuple { items } => {
                let elems = items
                    .iter()
                    .filter_map(|e| self.lower_expr(e, generics, diagnostics))
                    .collect();
                Some(self.push_expr(HirExprKind::Tuple(elems), None))
            }
            ast::Expr::EArray { items } => {
                let elems = items
                    .iter()
                    .filter_map(|e| self.lower_expr(e, generics, diagnostics))
                    .collect();
                Some(self.push_expr(HirExprKind::Array(elems), None))
            }
            ast::Expr::ELet {
                pat,
                annotation,
                value,
            } => {
                let value_id = self.lower_expr(value, generics, diagnostics)?;
                let annotation_id = match annotation {
                    Some(ty) => self.lower_type_expr(ty, generics, diagnostics),
                    None => None,
                };

                self.symbols.locals.enter_scope();
                let pat_id = self.lower_pat(pat, generics, diagnostics)?;
                self.symbols.locals.exit_scope();

                Some(self.push_expr(
                    HirExprKind::Let {
                        pat: pat_id,
                        value: value_id,
                        annotation: annotation_id,
                    },
                    None,
                ))
            }
            ast::Expr::EClosure { params, body } => {
                self.symbols.locals.enter_scope();
                let mut locals = Vec::new();
                for param in params {
                    let local_id = self.symbols.locals.insert(param.name.0.clone());
                    locals.push(local_id);
                    if let Some(ty) = &param.ty {
                        // resolve the type to surface errors early; result unused in HIR
                        let _ = self.lower_type_expr(ty, generics, diagnostics);
                    }
                }
                let body_id = self.lower_expr(body, generics, diagnostics)?;
                self.symbols.locals.exit_scope();
                Some(self.push_expr(
                    HirExprKind::Closure {
                        params: locals,
                        body: body_id,
                    },
                    None,
                ))
            }
            ast::Expr::EMatch { expr, arms, astptr } => {
                let scrutinee = self.lower_expr(expr, generics, diagnostics)?;
                let mut hir_arms = Vec::new();
                for arm in arms {
                    self.symbols.locals.enter_scope();
                    let pat_id = self.lower_pat(&arm.pat, generics, diagnostics)?;
                    let body_id = self.lower_expr(&arm.body, generics, diagnostics)?;
                    self.symbols.locals.exit_scope();
                    hir_arms.push(HirArm {
                        pat: pat_id,
                        body: body_id,
                    });
                }

                Some(self.push_expr(
                    HirExprKind::Match {
                        scrutinee,
                        arms: hir_arms,
                    },
                    Some(*astptr),
                ))
            }
            ast::Expr::EIf {
                cond,
                then_branch,
                else_branch,
            } => {
                let cond_id = self.lower_expr(cond, generics, diagnostics)?;
                let then_id = self.lower_expr(then_branch, generics, diagnostics)?;
                let else_id = self.lower_expr(else_branch, generics, diagnostics)?;
                Some(self.push_expr(
                    HirExprKind::If {
                        cond: cond_id,
                        then_branch: then_id,
                        else_branch: else_id,
                    },
                    None,
                ))
            }
            ast::Expr::EWhile { cond, body } => {
                let cond_id = self.lower_expr(cond, generics, diagnostics)?;
                let body_id = self.lower_expr(body, generics, diagnostics)?;
                Some(self.push_expr(
                    HirExprKind::While {
                        cond: cond_id,
                        body: body_id,
                    },
                    None,
                ))
            }
            ast::Expr::EGo { expr } => {
                let expr_id = self.lower_expr(expr, generics, diagnostics)?;
                Some(self.push_expr(HirExprKind::Go { expr: expr_id }, None))
            }
            ast::Expr::ECall { func, args } => {
                let callee = self.lower_expr(func, generics, diagnostics)?;
                let args = args
                    .iter()
                    .filter_map(|arg| self.lower_expr(arg, generics, diagnostics))
                    .collect();
                Some(self.push_expr(HirExprKind::Call { callee, args }, None))
            }
            ast::Expr::EUnary { op, expr } => {
                let expr_id = self.lower_expr(expr, generics, diagnostics)?;
                Some(self.push_expr(
                    HirExprKind::Unary {
                        op: *op,
                        expr: expr_id,
                    },
                    None,
                ))
            }
            ast::Expr::EBinary { op, lhs, rhs } => {
                let lhs_id = self.lower_expr(lhs, generics, diagnostics)?;
                let rhs_id = self.lower_expr(rhs, generics, diagnostics)?;
                Some(self.push_expr(
                    HirExprKind::Binary {
                        op: *op,
                        lhs: lhs_id,
                        rhs: rhs_id,
                    },
                    None,
                ))
            }
            ast::Expr::EProj { tuple, index } => {
                let expr_id = self.lower_expr(tuple, generics, diagnostics)?;
                Some(self.push_expr(
                    HirExprKind::Projection {
                        expr: expr_id,
                        index: *index,
                    },
                    None,
                ))
            }
            ast::Expr::EField {
                expr,
                field,
                astptr,
            } => {
                let expr_id = self.lower_expr(expr, generics, diagnostics)?;
                Some(self.push_expr(
                    HirExprKind::Field {
                        expr: expr_id,
                        field_name: field.0.clone(),
                    },
                    Some(*astptr),
                ))
            }
            ast::Expr::EBlock { exprs } => self.lower_block_exprs(exprs, generics, diagnostics),
        }
    }

    fn lower_block_exprs(
        &mut self,
        exprs: &[ast::Expr],
        generics: &GenericTable,
        diagnostics: &mut Diagnostics,
    ) -> Option<HirExprId> {
        if exprs.is_empty() {
            return Some(self.push_expr(HirExprKind::Unit, None));
        }

        if exprs.len() == 1 {
            return self.lower_expr(&exprs[0], generics, diagnostics);
        }

        // Lower all expressions in the block, creating scopes for let bindings
        let mut hir_exprs = Vec::new();
        let mut scope_depth = 0;

        for expr in exprs {
            match expr {
                ast::Expr::ELet {
                    pat,
                    annotation,
                    value,
                } => {
                    let value_id = self.lower_expr(value, generics, diagnostics)?;
                    let annotation_id = match annotation {
                        Some(ty) => self.lower_type_expr(ty, generics, diagnostics),
                        None => None,
                    };

                    self.symbols.locals.enter_scope();
                    scope_depth += 1;
                    let pat_id = self.lower_pat(pat, generics, diagnostics)?;

                    hir_exprs.push(self.push_expr(
                        HirExprKind::Let {
                            pat: pat_id,
                            value: value_id,
                            annotation: annotation_id,
                        },
                        None,
                    ));
                }
                _ => {
                    let expr_id = self.lower_expr(expr, generics, diagnostics)?;
                    hir_exprs.push(expr_id);
                }
            }
        }

        // Exit all scopes we entered
        for _ in 0..scope_depth {
            self.symbols.locals.exit_scope();
        }

        Some(self.push_expr(HirExprKind::Block { exprs: hir_exprs }, None))
    }

    fn lower_struct_literal(
        &mut self,
        name: &ast::Ident,
        fields: &Vec<(ast::Ident, ast::Expr)>,
        astptr: Option<MySyntaxNodePtr>,
        generics: &GenericTable,
        diagnostics: &mut Diagnostics,
    ) -> Option<HirExprId> {
        let type_id = match self.symbols.globals.types.get(&name.0) {
            Some(id) => *id,
            None => {
                diagnostics.push(Diagnostic::new(
                    self.stage.clone(),
                    Severity::Error,
                    format!("unknown struct `{}`", name.0),
                ));
                return None;
            }
        };

        let field_map = match self.symbols.fields.map.get(&type_id).cloned() {
            Some(map) => map,
            None => {
                diagnostics.push(Diagnostic::new(
                    self.stage.clone(),
                    Severity::Error,
                    format!("struct `{}` has no recorded fields", name.0),
                ));
                return None;
            }
        };

        let mut hir_fields = Vec::new();
        for (ident, expr) in fields {
            let field_id = match field_map.get(&ident.0) {
                Some(id) => *id,
                None => {
                    diagnostics.push(Diagnostic::new(
                        self.stage.clone(),
                        Severity::Error,
                        format!("unknown field `{}` on struct `{}`", ident.0, name.0),
                    ));
                    continue;
                }
            };
            let expr_id = match self.lower_expr(expr, generics, diagnostics) {
                Some(id) => id,
                None => continue,
            };
            hir_fields.push((field_id, expr_id));
        }

        Some(self.push_expr(
            HirExprKind::StructLiteral {
                type_id,
                fields: hir_fields,
            },
            astptr,
        ))
    }

    fn lower_constructor_expr(
        &mut self,
        constructor: &ast::Path,
        args: &[ast::Expr],
        astptr: Option<MySyntaxNodePtr>,
        generics: &GenericTable,
        diagnostics: &mut Diagnostics,
    ) -> Option<HirExprId> {
        let segments: Vec<&ast::Ident> = constructor.segments.iter().map(|s| &s.ident).collect();
        match segments.len() {
            0 => {
                diagnostics.push(Diagnostic::new(
                    self.stage.clone(),
                    Severity::Error,
                    "empty constructor path",
                ));
                None
            }
            1 => self.lower_bare_constructor(&segments[0].0, args, astptr, generics, diagnostics),
            2 => self.lower_qualified_constructor(
                &segments[0].0,
                &segments[1].0,
                args,
                astptr,
                generics,
                diagnostics,
            ),
            _ => {
                diagnostics.push(Diagnostic::new(
                    self.stage.clone(),
                    Severity::Error,
                    "unsupported nested constructor path",
                ));
                None
            }
        }
    }

    fn lower_bare_constructor(
        &mut self,
        name: &str,
        args: &[ast::Expr],
        astptr: Option<MySyntaxNodePtr>,
        generics: &GenericTable,
        diagnostics: &mut Diagnostics,
    ) -> Option<HirExprId> {
        if let Some(ctors) = self.symbols.globals.ctor_by_name.get(name) {
            if ctors.len() == 1 {
                let ctor_id = ctors[0];
                let args = args
                    .iter()
                    .filter_map(|e| self.lower_expr(e, generics, diagnostics))
                    .collect();
                return Some(self.push_expr(HirExprKind::EnumCtor { ctor_id, args }, astptr));
            }

            let args = args
                .iter()
                .filter_map(|e| self.lower_expr(e, generics, diagnostics))
                .collect();
            return Some(self.push_expr(
                HirExprKind::UnresolvedEnumCtor {
                    name: name.to_string(),
                    args,
                },
                astptr,
            ));
        }

        // Struct constructor fallback using positional fields.
        if let Some(type_id) = self.symbols.globals.types.get(name)
            && self.type_kinds.get(type_id) == Some(&TypeKind::Struct)
        {
            return self.lower_struct_ctor(*type_id, args, astptr, generics, diagnostics);
        }

        diagnostics.push(Diagnostic::new(
            self.stage.clone(),
            Severity::Error,
            format!("unresolved constructor `{}`", name),
        ));
        None
    }

    fn lower_qualified_constructor(
        &mut self,
        type_name: &str,
        variant_name: &str,
        args: &[ast::Expr],
        astptr: Option<MySyntaxNodePtr>,
        generics: &GenericTable,
        diagnostics: &mut Diagnostics,
    ) -> Option<HirExprId> {
        let type_id = match self.symbols.globals.types.get(type_name) {
            Some(id) => *id,
            None => {
                diagnostics.push(Diagnostic::new(
                    self.stage.clone(),
                    Severity::Error,
                    format!("unknown type `{}` in constructor path", type_name),
                ));
                return None;
            }
        };

        match self.type_kinds.get(&type_id) {
            Some(TypeKind::Enum) => {
                let ctor_id = match self
                    .symbols
                    .globals
                    .ctors
                    .get(&(type_id, variant_name.to_string()))
                {
                    Some(id) => *id,
                    None => {
                        diagnostics.push(Diagnostic::new(
                            self.stage.clone(),
                            Severity::Error,
                            format!("unknown variant `{}` on enum", variant_name),
                        ));
                        return None;
                    }
                };
                let args = args
                    .iter()
                    .filter_map(|e| self.lower_expr(e, generics, diagnostics))
                    .collect();
                Some(self.push_expr(HirExprKind::EnumCtor { ctor_id, args }, astptr))
            }
            Some(TypeKind::Struct) => {
                self.lower_struct_ctor(type_id, args, astptr, generics, diagnostics)
            }
            Some(TypeKind::ExternType) | None => {
                diagnostics.push(Diagnostic::new(
                    self.stage.clone(),
                    Severity::Error,
                    "constructors not supported for this type",
                ));
                None
            }
        }
    }

    fn lower_struct_ctor(
        &mut self,
        type_id: TypeId,
        args: &[ast::Expr],
        astptr: Option<MySyntaxNodePtr>,
        generics: &GenericTable,
        diagnostics: &mut Diagnostics,
    ) -> Option<HirExprId> {
        let order = match self.struct_field_order.get(&type_id) {
            Some(order) => order.clone(),
            None => {
                diagnostics.push(Diagnostic::new(
                    self.stage.clone(),
                    Severity::Error,
                    "missing field order for struct constructor",
                ));
                return None;
            }
        };

        if order.len() != args.len() {
            diagnostics.push(Diagnostic::new(
                self.stage.clone(),
                Severity::Error,
                format!(
                    "struct constructor expected {} args, found {}",
                    order.len(),
                    args.len()
                ),
            ));
            return None;
        }

        let field_map = match self.symbols.fields.map.get(&type_id).cloned() {
            Some(map) => map,
            None => {
                diagnostics.push(Diagnostic::new(
                    self.stage.clone(),
                    Severity::Error,
                    "struct has no recorded fields",
                ));
                return None;
            }
        };

        let mut fields = Vec::new();
        for (field_name, arg_expr) in order.iter().zip(args.iter()) {
            let field_id = match field_map.get(field_name) {
                Some(id) => *id,
                None => {
                    diagnostics.push(Diagnostic::new(
                        self.stage.clone(),
                        Severity::Error,
                        format!(
                            "unknown field `{}` when lowering struct constructor",
                            field_name
                        ),
                    ));
                    continue;
                }
            };
            let expr_id = match self.lower_expr(arg_expr, generics, diagnostics) {
                Some(id) => id,
                None => continue,
            };
            fields.push((field_id, expr_id));
        }

        Some(self.push_expr(HirExprKind::StructLiteral { type_id, fields }, astptr))
    }

    fn lower_path_expr(
        &mut self,
        path: &ast::Path,
        astptr: Option<MySyntaxNodePtr>,
        generics: &GenericTable,
        diagnostics: &mut Diagnostics,
    ) -> Option<HirExprId> {
        let segments: Vec<&ast::Ident> = path.segments.iter().map(|s| &s.ident).collect();
        match segments.len() {
            0 => {
                diagnostics.push(Diagnostic::new(
                    self.stage.clone(),
                    Severity::Error,
                    "empty path expression",
                ));
                None
            }
            1 => {
                let name = &segments[0].0;
                if let Some(local_id) = self.symbols.locals.resolve(name) {
                    return Some(self.push_expr(HirExprKind::LocalVar(local_id), astptr));
                }
                if let Some(item_id) = self.symbols.globals.value_items.get(name) {
                    return Some(self.push_expr(HirExprKind::GlobalItem(*item_id), astptr));
                }
                if let Some(ctors) = self.symbols.globals.ctor_by_name.get(name) {
                    if ctors.len() == 1 {
                        let ctor_id = ctors[0];
                        return Some(self.push_expr(
                            HirExprKind::EnumCtor {
                                ctor_id,
                                args: vec![],
                            },
                            astptr,
                        ));
                    }
                    return Some(self.push_expr(
                        HirExprKind::UnresolvedEnumCtor {
                            name: name.clone(),
                            args: Vec::new(),
                        },
                        astptr,
                    ));
                }

                diagnostics.push(Diagnostic::new(
                    self.stage.clone(),
                    Severity::Error,
                    format!("unresolved identifier `{}`", name),
                ));
                None
            }
            2 => {
                let ty_name = &segments[0].0;
                let method_or_variant = &segments[1].0;

                if let Some(type_id) = self.symbols.globals.types.get(ty_name)
                    && self
                        .symbols
                        .globals
                        .ctors
                        .contains_key(&(*type_id, method_or_variant.clone()))
                {
                    return self.lower_qualified_constructor(
                        ty_name,
                        method_or_variant,
                        &[],
                        astptr,
                        generics,
                        diagnostics,
                    );
                }

                let full_name = format!("{}::{}", ty_name, method_or_variant);
                let item_id = if let Some(&id) = self.symbols.globals.value_items.get(&full_name) {
                    id
                } else {
                    let temp_id = self.alloc_item_id();
                    self.symbols.globals.value_items.insert(full_name, temp_id);
                    temp_id
                };

                Some(self.push_expr(HirExprKind::GlobalItem(item_id), astptr))
            }
            _ => {
                diagnostics.push(Diagnostic::new(
                    self.stage.clone(),
                    Severity::Error,
                    "unsupported nested path expression",
                ));
                None
            }
        }
    }

    fn lower_pat(
        &mut self,
        pat: &ast::Pat,
        _generics: &GenericTable,
        diagnostics: &mut Diagnostics,
    ) -> Option<HirPatId> {
        match pat {
            ast::Pat::PVar { name, .. } => {
                let local_id = self.symbols.locals.insert(name.0.clone());
                Some(self.push_pat(HirPatKind::Var(local_id), None))
            }
            ast::Pat::PUnit => Some(self.push_pat(HirPatKind::Unit, None)),
            ast::Pat::PBool { value } => Some(self.push_pat(HirPatKind::Bool(*value), None)),
            ast::Pat::PInt { value } => Some(self.push_pat(HirPatKind::Int(value.clone()), None)),
            ast::Pat::PString { value } => {
                Some(self.push_pat(HirPatKind::String(value.clone()), None))
            }
            ast::Pat::PConstr { constructor, args } => {
                self.lower_pat_constructor(constructor, args, None, diagnostics)
            }
            ast::Pat::PStruct { name, fields } => {
                let type_id = match self.symbols.globals.types.get(&name.0) {
                    Some(id) => *id,
                    None => {
                        diagnostics.push(Diagnostic::new(
                            self.stage.clone(),
                            Severity::Error,
                            format!("unknown struct `{}` in pattern", name.0),
                        ));
                        return None;
                    }
                };
                let field_map = match self.symbols.fields.map.get(&type_id).cloned() {
                    Some(map) => map,
                    None => {
                        diagnostics.push(Diagnostic::new(
                            self.stage.clone(),
                            Severity::Error,
                            "struct pattern has no recorded fields",
                        ));
                        return None;
                    }
                };

                let mut hir_fields = Vec::new();
                for (ident, pat) in fields {
                    let field_id = match field_map.get(&ident.0) {
                        Some(id) => *id,
                        None => {
                            diagnostics.push(Diagnostic::new(
                                self.stage.clone(),
                                Severity::Error,
                                format!("unknown field `{}` in struct pattern", ident.0),
                            ));
                            continue;
                        }
                    };
                    let pat_id = match self.lower_pat(pat, _generics, diagnostics) {
                        Some(id) => id,
                        None => continue,
                    };
                    hir_fields.push((field_id, pat_id));
                }

                Some(self.push_pat(
                    HirPatKind::Struct {
                        type_id,
                        fields: hir_fields,
                    },
                    None,
                ))
            }
            ast::Pat::PTuple { pats } => {
                let items = pats
                    .iter()
                    .filter_map(|p| self.lower_pat(p, _generics, diagnostics))
                    .collect();
                Some(self.push_pat(HirPatKind::Tuple(items), None))
            }
            ast::Pat::PWild => Some(self.push_pat(HirPatKind::Wild, None)),
        }
    }

    fn lower_pat_constructor(
        &mut self,
        constructor: &ast::Path,
        args: &[ast::Pat],
        astptr: Option<MySyntaxNodePtr>,
        diagnostics: &mut Diagnostics,
    ) -> Option<HirPatId> {
        let segments: Vec<&ast::Ident> = constructor.segments.iter().map(|s| &s.ident).collect();
        match segments.len() {
            0 => {
                diagnostics.push(Diagnostic::new(
                    self.stage.clone(),
                    Severity::Error,
                    "empty constructor pattern",
                ));
                None
            }
            1 => {
                let name = &segments[0].0;
                if let Some(ctors) = self.symbols.globals.ctor_by_name.get(name) {
                    if ctors.len() == 1 {
                        let ctor_id = ctors[0];
                        let args = args
                            .iter()
                            .filter_map(|p| self.lower_pat(p, &GenericTable::new(), diagnostics))
                            .collect();
                        return Some(self.push_pat(HirPatKind::Enum { ctor_id, args }, astptr));
                    }
                    diagnostics.push(Diagnostic::new(
                        self.stage.clone(),
                        Severity::Error,
                        "ambiguous constructor patterns not supported yet",
                    ));
                    return Some(
                        self.push_pat(HirPatKind::UnresolvedVariant(name.clone()), astptr),
                    );
                }

                // Struct positional pattern
                if let Some(type_id) = self.symbols.globals.types.get(name)
                    && self.type_kinds.get(type_id) == Some(&TypeKind::Struct)
                {
                    return self.lower_struct_pat(*type_id, args, astptr, diagnostics);
                }

                diagnostics.push(Diagnostic::new(
                    self.stage.clone(),
                    Severity::Error,
                    format!("unknown constructor `{}` in pattern", name),
                ));
                None
            }
            2 => {
                let ty = &segments[0].0;
                let variant = &segments[1].0;
                self.lower_qualified_pat_constructor(ty, variant, args, astptr, diagnostics)
            }
            _ => {
                diagnostics.push(Diagnostic::new(
                    self.stage.clone(),
                    Severity::Error,
                    "unsupported nested constructor pattern",
                ));
                None
            }
        }
    }

    fn lower_qualified_pat_constructor(
        &mut self,
        type_name: &str,
        variant_name: &str,
        args: &[ast::Pat],
        astptr: Option<MySyntaxNodePtr>,
        diagnostics: &mut Diagnostics,
    ) -> Option<HirPatId> {
        let type_id = match self.symbols.globals.types.get(type_name) {
            Some(id) => *id,
            None => {
                diagnostics.push(Diagnostic::new(
                    self.stage.clone(),
                    Severity::Error,
                    format!("unknown type `{}` in pattern", type_name),
                ));
                return None;
            }
        };

        match self.type_kinds.get(&type_id) {
            Some(TypeKind::Enum) => {
                let ctor_id = match self
                    .symbols
                    .globals
                    .ctors
                    .get(&(type_id, variant_name.to_string()))
                {
                    Some(id) => *id,
                    None => {
                        diagnostics.push(Diagnostic::new(
                            self.stage.clone(),
                            Severity::Error,
                            format!("unknown variant `{}` on enum `{}`", variant_name, type_name),
                        ));
                        return None;
                    }
                };
                let args = args
                    .iter()
                    .filter_map(|p| self.lower_pat(p, &GenericTable::new(), diagnostics))
                    .collect();
                Some(self.push_pat(HirPatKind::Enum { ctor_id, args }, astptr))
            }
            Some(TypeKind::Struct) => self.lower_struct_pat(type_id, args, astptr, diagnostics),
            Some(TypeKind::ExternType) | None => {
                diagnostics.push(Diagnostic::new(
                    self.stage.clone(),
                    Severity::Error,
                    "constructors not supported for this type in pattern",
                ));
                None
            }
        }
    }

    fn lower_struct_pat(
        &mut self,
        type_id: TypeId,
        args: &[ast::Pat],
        astptr: Option<MySyntaxNodePtr>,
        diagnostics: &mut Diagnostics,
    ) -> Option<HirPatId> {
        let order = match self.struct_field_order.get(&type_id) {
            Some(order) => order.clone(),
            None => {
                diagnostics.push(Diagnostic::new(
                    self.stage.clone(),
                    Severity::Error,
                    "missing field order for struct pattern",
                ));
                return None;
            }
        };

        if order.len() != args.len() {
            diagnostics.push(Diagnostic::new(
                self.stage.clone(),
                Severity::Error,
                format!(
                    "struct pattern expected {} fields, found {}",
                    order.len(),
                    args.len()
                ),
            ));
            return None;
        }

        let mut fields = Vec::new();
        for (field_name, pat_expr) in order.iter().zip(args.iter()) {
            let field_id = match self
                .symbols
                .fields
                .map
                .get(&type_id)
                .and_then(|map| map.get(field_name))
                .copied()
            {
                Some(id) => id,
                None => {
                    diagnostics.push(Diagnostic::new(
                        self.stage.clone(),
                        Severity::Error,
                        format!("unknown field `{}` in struct pattern", field_name),
                    ));
                    continue;
                }
            };
            let pat_id = match self.lower_pat(pat_expr, &GenericTable::new(), diagnostics) {
                Some(id) => id,
                None => continue,
            };
            fields.push((field_id, pat_id));
        }

        Some(self.push_pat(HirPatKind::Struct { type_id, fields }, astptr))
    }

    fn lower_type_expr(
        &mut self,
        ty: &ast::TypeExpr,
        generics: &GenericTable,
        diagnostics: &mut Diagnostics,
    ) -> Option<HirTypeId> {
        match ty {
            ast::TypeExpr::TUnit => Some(self.push_type(HirType::Builtin(BuiltinType::Unit))),
            ast::TypeExpr::TBool => Some(self.push_type(HirType::Builtin(BuiltinType::Bool))),
            ast::TypeExpr::TInt8 => Some(self.push_type(HirType::Builtin(BuiltinType::Int8))),
            ast::TypeExpr::TInt16 => Some(self.push_type(HirType::Builtin(BuiltinType::Int16))),
            ast::TypeExpr::TInt32 => Some(self.push_type(HirType::Builtin(BuiltinType::Int32))),
            ast::TypeExpr::TInt64 => Some(self.push_type(HirType::Builtin(BuiltinType::Int64))),
            ast::TypeExpr::TUint8 => Some(self.push_type(HirType::Builtin(BuiltinType::Uint8))),
            ast::TypeExpr::TUint16 => Some(self.push_type(HirType::Builtin(BuiltinType::Uint16))),
            ast::TypeExpr::TUint32 => Some(self.push_type(HirType::Builtin(BuiltinType::Uint32))),
            ast::TypeExpr::TUint64 => Some(self.push_type(HirType::Builtin(BuiltinType::Uint64))),
            ast::TypeExpr::TFloat32 => Some(self.push_type(HirType::Builtin(BuiltinType::Float32))),
            ast::TypeExpr::TFloat64 => Some(self.push_type(HirType::Builtin(BuiltinType::Float64))),
            ast::TypeExpr::TString => Some(self.push_type(HirType::Builtin(BuiltinType::String))),
            ast::TypeExpr::TTuple { typs } => {
                let mut elems = Vec::new();
                for t in typs {
                    if let Some(id) = self.lower_type_expr(t, generics, diagnostics) {
                        elems.push(id);
                    }
                }
                Some(self.push_type(HirType::Tuple(elems)))
            }
            ast::TypeExpr::TCon { name } => {
                if let Some(id) = generics.resolve(name) {
                    return Some(self.push_type(HirType::Generic(id)));
                }
                if let Some(type_id) = self.symbols.globals.types.get(name) {
                    return Some(self.push_type(HirType::Named(*type_id, Vec::new())));
                }
                self.push_error(format!("unknown type `{}`", name), diagnostics);
                None
            }
            ast::TypeExpr::TApp { ty, args } => {
                if let ast::TypeExpr::TCon { name } = ty.as_ref()
                    && let Some(type_id) = self.symbols.globals.types.get(name).copied()
                {
                    let mut args_ids = Vec::new();
                    for a in args {
                        if let Some(id) = self.lower_type_expr(a, generics, diagnostics) {
                            args_ids.push(id);
                        }
                    }
                    return Some(self.push_type(HirType::Named(type_id, args_ids)));
                }

                let base = self.lower_type_expr(ty, generics, diagnostics)?;
                let mut args_lowered = Vec::new();
                for a in args {
                    if let Some(id) = self.lower_type_expr(a, generics, diagnostics) {
                        args_lowered.push(id);
                    }
                }
                Some(self.push_type(HirType::App {
                    base,
                    args: args_lowered,
                }))
            }
            ast::TypeExpr::TArray { len, elem } => {
                let elem_id = self.lower_type_expr(elem, generics, diagnostics)?;
                Some(self.push_type(HirType::Array {
                    elem: elem_id,
                    len: *len,
                }))
            }
            ast::TypeExpr::TFunc { params, ret_ty } => {
                let mut param_ids = Vec::new();
                for p in params {
                    if let Some(id) = self.lower_type_expr(p, generics, diagnostics) {
                        param_ids.push(id);
                    }
                }
                let ret = self.lower_type_expr(ret_ty, generics, diagnostics)?;
                Some(self.push_type(HirType::Func {
                    params: param_ids,
                    ret,
                }))
            }
        }
    }

    fn extend_generics(
        &mut self,
        base: Option<&GenericTable>,
        params: &[ast::Ident],
    ) -> (Vec<GenericParamId>, GenericTable) {
        let mut table = base.cloned().unwrap_or_else(GenericTable::new);
        let mut ids = Vec::new();
        for ident in params {
            if let Some(existing) = table.resolve(&ident.0) {
                ids.push(existing);
                continue;
            }
            let gid = self.alloc_generic_id();
            table.insert(ident.0.clone(), gid);
            ids.push(gid);
        }
        (ids, table)
    }

    fn push_expr(&mut self, kind: HirExprKind, astptr: Option<MySyntaxNodePtr>) -> HirExprId {
        self.tables.exprs.alloc(HirExpr { kind, astptr })
    }

    fn push_pat(&mut self, kind: HirPatKind, astptr: Option<MySyntaxNodePtr>) -> HirPatId {
        self.tables.pats.alloc(HirPat { kind, astptr })
    }

    fn push_type(&mut self, ty: HirType) -> HirTypeId {
        self.tables.types.alloc(ty)
    }

    fn push_item(&mut self, item: HirItem) -> HirItemId {
        self.tables.items.alloc(item)
    }

    fn alloc_item_id(&mut self) -> ItemId {
        let id = ItemId(self.next_item_id);
        self.next_item_id += 1;
        id
    }

    fn alloc_type_id(&mut self) -> TypeId {
        let id = TypeId(self.next_type_id);
        self.next_type_id += 1;
        id
    }

    fn alloc_trait_id(&mut self) -> TraitId {
        let id = TraitId(self.next_trait_id);
        self.next_trait_id += 1;
        id
    }

    fn alloc_impl_id(&mut self) -> ImplId {
        let id = ImplId(self.next_impl_id);
        self.next_impl_id += 1;
        id
    }

    fn alloc_ctor_id(&mut self) -> CtorId {
        let id = CtorId(self.next_ctor_id);
        self.next_ctor_id += 1;
        id
    }

    fn alloc_field_id(&mut self) -> FieldId {
        let id = FieldId(self.next_field_id);
        self.next_field_id += 1;
        id
    }

    fn alloc_generic_id(&mut self) -> GenericParamId {
        let id = GenericParamId(self.next_generic_id);
        self.next_generic_id += 1;
        id
    }

    fn insert_value_item(&mut self, name: String, id: ItemId, diagnostics: &mut Diagnostics) {
        if let Some(existing) = self.symbols.globals.value_items.get(&name) {
            self.push_error(
                format!(
                    "duplicate value item `{}` (existing id {:?}, new id {:?})",
                    name, existing, id
                ),
                diagnostics,
            );
            return;
        }
        self.symbols.globals.value_items.insert(name, id);
    }

    fn insert_type(&mut self, name: String, id: TypeId, diagnostics: &mut Diagnostics) {
        if let Some(existing) = self.symbols.globals.types.get(&name) {
            self.push_error(
                format!(
                    "duplicate type `{}` (existing id {:?}, new id {:?})",
                    name, existing, id
                ),
                diagnostics,
            );
            return;
        }
        self.symbols.globals.types.insert(name, id);
    }

    fn insert_trait(&mut self, name: String, id: TraitId, diagnostics: &mut Diagnostics) {
        if let Some(existing) = self.symbols.globals.traits.get(&name) {
            self.push_error(
                format!(
                    "duplicate trait `{}` (existing id {:?}, new id {:?})",
                    name, existing, id
                ),
                diagnostics,
            );
            return;
        }
        self.symbols.globals.traits.insert(name, id);
    }

    fn push_error(&mut self, message: impl Into<String>, diagnostics: &mut Diagnostics) {
        let diagnostic = Diagnostic::new(self.stage.clone(), Severity::Error, message);
        diagnostics.push(diagnostic);
    }
}
