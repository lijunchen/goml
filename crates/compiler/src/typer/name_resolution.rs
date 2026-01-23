use std::collections::{HashMap, HashSet};

use ast::ast;

use crate::builtins;
use crate::env;
use crate::hir;
use crate::hir::HirIdent;
use diagnostics::{Diagnostic, Diagnostics, Severity, Stage};

pub type HirTable = hir::HirTable;

#[derive(Default)]
pub struct NameResolution {
    diagnostics: Diagnostics,
}

#[derive(Debug)]
struct ResolveLocalEnv(im::Vector<(ast::AstIdent, hir::LocalId)>);

impl ResolveLocalEnv {
    pub fn new() -> Self {
        Self(im::Vector::new())
    }

    #[allow(unused)]
    pub fn enter_scope(&self) -> Self {
        Self(self.0.clone())
    }

    pub fn add(&mut self, name: &ast::AstIdent, new_name: hir::LocalId) {
        self.0.push_back((name.clone(), new_name));
    }

    pub fn rfind(&self, key: &ast::AstIdent) -> Option<hir::LocalId> {
        self.0
            .iter()
            .rfind(|(name, _)| name == key)
            .map(|(_, new_name)| *new_name)
    }
}

struct ResolutionContext<'a> {
    builtin_names: &'a HashMap<String, hir::BuiltinId>,
    def_names: &'a HashMap<String, hir::DefId>,
    deps: &'a HashMap<String, hir::PackageInterface>,
    current_package: &'a str,
    imports: &'a HashSet<String>,
    constructor_index: &'a ConstructorIndex,
}

fn full_def_name(package: &str, name: &str) -> String {
    if package == "Builtin" || package == "Main" {
        name.to_string()
    } else {
        format!("{}::{}", package, name)
    }
}

fn package_allowed(package: &str, current_package: &str, imports: &HashSet<String>) -> bool {
    package == current_package || package == "Builtin" || imports.contains(package)
}

struct ConstructorIndex {
    enums_by_package: HashMap<String, HashMap<String, HashSet<String>>>,
}

impl ConstructorIndex {
    fn new_with_deps(
        files: &[hir::SourceFileAst],
        deps: &HashMap<String, hir::PackageInterface>,
    ) -> Self {
        let mut index = Self {
            enums_by_package: HashMap::new(),
        };
        index.add_files(files);
        if !files.iter().any(|file| file.ast.package.0 == "Builtin") {
            let builtin_ast = builtins::get_builtin_ast();
            let builtin_file = hir::SourceFileAst {
                path: "<builtin>".into(),
                ast: builtin_ast,
            };
            index.add_files(std::slice::from_ref(&builtin_file));
        }
        for (package, interface) in deps {
            index.add_interface(package, interface);
        }
        index
    }

    fn add_files(&mut self, files: &[hir::SourceFileAst]) {
        for file in files {
            let package = file.ast.package.0.clone();
            let entry = self.enums_by_package.entry(package).or_default();
            for item in &file.ast.toplevels {
                if let ast::Item::EnumDef(def) = item {
                    let variants = entry.entry(def.name.0.clone()).or_default();
                    for (variant, _) in &def.variants {
                        variants.insert(variant.0.clone());
                    }
                }
            }
        }
    }

    fn add_interface(&mut self, package: &str, interface: &hir::PackageInterface) {
        let entry = self
            .enums_by_package
            .entry(package.to_string())
            .or_default();
        for (enum_name, variants) in interface.enum_variants.iter() {
            let entry_variants = entry.entry(enum_name.clone()).or_default();
            for variant in variants {
                entry_variants.insert(variant.clone());
            }
        }
    }

    fn enum_has_variant(&self, package: &str, enum_name: &str, variant: &str) -> bool {
        self.enums_by_package
            .get(package)
            .and_then(|enums| enums.get(enum_name))
            .is_some_and(|variants| variants.contains(variant))
    }

    fn unique_enum_for_variant(&self, package: &str, variant: &str) -> Option<String> {
        let enums = self.enums_by_package.get(package)?;
        let mut found = None;
        for (enum_name, variants) in enums {
            if variants.contains(variant) {
                if found.is_some() {
                    return None;
                }
                found = Some(enum_name.clone());
            }
        }
        found
    }

    fn has_variant(&self, package: &str, variant: &str) -> bool {
        self.enums_by_package
            .get(package)
            .is_some_and(|enums| enums.values().any(|vars| vars.contains(variant)))
    }
}

impl ResolutionContext<'_> {
    fn package_allowed(&self, package: &str) -> bool {
        package == self.current_package || package == "Builtin" || self.imports.contains(package)
    }
}

impl NameResolution {
    fn error(&mut self, message: impl Into<String>) {
        self.diagnostics
            .push(Diagnostic::new(Stage::Typer, Severity::Error, message));
    }

    fn ice(&mut self, message: impl Into<String>) {
        self.error(format!("Internal error: {}", message.into()));
    }

    fn fresh_name(&self, name: &str, hir_table: &mut HirTable) -> hir::LocalId {
        hir_table.fresh_local(name)
    }

    fn constructor_path_for(
        &mut self,
        path: &ast::Path,
        ctx: &ResolutionContext,
    ) -> Option<hir::Path> {
        let segments = path.segments();
        let last = path.last_ident()?;
        match segments.len() {
            1 => {
                let variant = &last.0;
                if let Some(enum_name) = ctx
                    .constructor_index
                    .unique_enum_for_variant(ctx.current_package, variant)
                {
                    Some(constructor_path(ctx.current_package, &enum_name, variant))
                } else if ctx
                    .constructor_index
                    .has_variant(ctx.current_package, variant)
                {
                    Some(hir::Path::from_ident(variant.clone()))
                } else {
                    None
                }
            }
            2 => {
                let enum_name = segments.first().map(|seg| &seg.ident.0)?;
                let variant = segments.get(1).map(|seg| &seg.ident.0)?;
                if ctx
                    .constructor_index
                    .enum_has_variant(ctx.current_package, enum_name, variant)
                {
                    Some(constructor_path(ctx.current_package, enum_name, variant))
                } else {
                    None
                }
            }
            3 => {
                let package = segments.first().map(|seg| &seg.ident.0)?;
                let enum_name = segments.get(1).map(|seg| &seg.ident.0)?;
                let variant = segments.get(2).map(|seg| &seg.ident.0)?;
                let exists = ctx
                    .constructor_index
                    .enum_has_variant(package, enum_name, variant);
                if exists && !ctx.package_allowed(package) {
                    self.error(format!(
                        "package {} not imported in package {}",
                        package, ctx.current_package
                    ));
                    return None;
                }
                exists.then(|| constructor_path(package, enum_name, variant))
            }
            _ => None,
        }
    }

    fn normalize_constructor_path(
        &mut self,
        path: &ast::Path,
        ctx: &ResolutionContext,
    ) -> hir::Path {
        self.constructor_path_for(path, ctx)
            .unwrap_or_else(|| path.into())
    }

    pub fn resolve_files(self, files: Vec<ast::File>) -> (hir::ResolvedHir, HirTable, Diagnostics) {
        let deps = HashMap::new();
        let package_name = files
            .first()
            .map(|file| file.package.0.as_str())
            .unwrap_or("Main");
        let package_id = match package_name {
            "Builtin" => hir::PackageId(0),
            "Main" => hir::PackageId(1),
            _ => hir::PackageId(2),
        };
        let files = files
            .into_iter()
            .enumerate()
            .map(|(idx, ast)| hir::SourceFileAst {
                path: format!("<unknown:{}>", idx).into(),
                ast,
            })
            .collect();
        self.resolve_files_with_env(package_id, files, &deps)
    }

    pub fn resolve_files_with_env(
        mut self,
        package_id: hir::PackageId,
        files: Vec<hir::SourceFileAst>,
        deps: &HashMap<String, hir::PackageInterface>,
    ) -> (hir::ResolvedHir, HirTable, Diagnostics) {
        let mut hir_table = HirTable::new(package_id);

        let mut builtin_names = HashMap::new();
        for name in env::builtin_function_names() {
            if let Some(id) = hir::BuiltinId::from_name(&name) {
                builtin_names.insert(name, id);
            }
        }

        let mut def_names = HashMap::new();
        let ctor_index = ConstructorIndex::new_with_deps(&files, deps);
        let mut toplevels = Vec::new();
        let mut per_file_defs = Vec::new();

        for file in files.iter() {
            let package_name = file.ast.package.0.as_str();
            let imports = file
                .ast
                .imports
                .iter()
                .map(|import| import.0.clone())
                .collect::<HashSet<_>>();
            let mut def_ids = Vec::new();
            for item in file.ast.toplevels.iter() {
                let def_id = match item {
                    ast::Item::Fn(func) => {
                        let full_name = full_def_name(package_name, &func.name.0);
                        let path = full_def_path(package_name, &func.name.0);
                        let id = hir_table.alloc_def_with_path(
                            path,
                            hir::DefKind::Fn,
                            hir::Def::Fn(hir::Fn {
                                attrs: Vec::new(),
                                name: full_name.clone(),
                                generics: Vec::new(),
                                generic_bounds: Vec::new(),
                                params: Vec::new(),
                                ret_ty: None,
                                body: hir_table.dummy_expr(),
                            }),
                        );
                        def_names.insert(full_name, id);
                        id
                    }
                    ast::Item::ExternGo(ext) => {
                        let full_name = full_def_name(package_name, &ext.goml_name.0);
                        let path = full_def_path(package_name, &ext.goml_name.0);
                        let ext_def = self.lower_extern_go(ext, package_name, &imports);
                        let id = hir_table.alloc_def_with_path(
                            path,
                            hir::DefKind::ExternGo,
                            hir::Def::ExternGo(ext_def),
                        );
                        def_names.insert(full_name, id);
                        id
                    }
                    ast::Item::ExternBuiltin(ext) => {
                        let full_name = full_def_name(package_name, &ext.name.0);
                        let path = full_def_path(package_name, &ext.name.0);
                        let ext_def = self.lower_extern_builtin(ext, package_name, &imports);
                        let id = hir_table.alloc_def_with_path(
                            path,
                            hir::DefKind::ExternBuiltin,
                            hir::Def::ExternBuiltin(ext_def),
                        );
                        def_names.insert(full_name, id);
                        id
                    }
                    ast::Item::EnumDef(e) => {
                        let full_name = full_def_name(package_name, &e.name.0);
                        let path = full_def_path(package_name, &e.name.0);
                        let enum_def = self.lower_enum_def(e, package_name, &imports);
                        let id = hir_table.alloc_def_with_path(
                            path,
                            hir::DefKind::EnumDef,
                            hir::Def::EnumDef(enum_def),
                        );
                        def_names.insert(full_name, id);
                        id
                    }
                    ast::Item::StructDef(s) => {
                        let full_name = full_def_name(package_name, &s.name.0);
                        let path = full_def_path(package_name, &s.name.0);
                        let struct_def = self.lower_struct_def(s, package_name, &imports);
                        let id = hir_table.alloc_def_with_path(
                            path,
                            hir::DefKind::StructDef,
                            hir::Def::StructDef(struct_def),
                        );
                        def_names.insert(full_name, id);
                        id
                    }
                    ast::Item::TraitDef(t) => {
                        let full_name = full_def_name(package_name, &t.name.0);
                        let path = full_def_path(package_name, &t.name.0);
                        let trait_def = self.lower_trait_def(t, package_name, &imports);
                        let id = hir_table.alloc_def_with_path(
                            path,
                            hir::DefKind::TraitDef,
                            hir::Def::TraitDef(trait_def),
                        );
                        def_names.insert(full_name, id);
                        id
                    }
                    ast::Item::ImplBlock(_i) => hir_table.alloc_def_with_path(
                        full_def_path(package_name, "impl"),
                        hir::DefKind::ImplBlock,
                        hir::Def::ImplBlock(hir::ImplBlock {
                            attrs: Vec::new(),
                            generics: Vec::new(),
                            trait_name: None,
                            for_type: hir::TypeExpr::TUnit,
                            methods: Vec::new(),
                        }),
                    ),
                    ast::Item::ExternType(ext) => {
                        let full_name = full_def_name(package_name, &ext.goml_name.0);
                        let path = full_def_path(package_name, &ext.goml_name.0);
                        let ext_def = self.lower_extern_type(ext, package_name);
                        let id = hir_table.alloc_def_with_path(
                            path,
                            hir::DefKind::ExternType,
                            hir::Def::ExternType(ext_def),
                        );
                        def_names.insert(full_name, id);
                        id
                    }
                };
                toplevels.push(def_id);
                def_ids.push(def_id);
            }
            per_file_defs.push(def_ids);
        }

        for (file_idx, file) in files.iter().enumerate() {
            let package_name = file.ast.package.0.as_str();
            let imports = file
                .ast
                .imports
                .iter()
                .map(|import| import.0.clone())
                .collect::<HashSet<_>>();
            let ctx = ResolutionContext {
                builtin_names: &builtin_names,
                def_names: &def_names,
                deps,
                current_package: package_name,
                imports: &imports,
                constructor_index: &ctor_index,
            };

            let mut toplevel_idx = 0;
            for item in file.ast.toplevels.iter() {
                match item {
                    ast::Item::Fn(func) => {
                        let def_id = per_file_defs
                            .get(file_idx)
                            .and_then(|defs| defs.get(toplevel_idx))
                            .copied();
                        let Some(def_id) = def_id else {
                            self.ice("missing def id for function");
                            break;
                        };
                        toplevel_idx += 1;
                        hir_table.set_current_owner(def_id);
                        let full_name = full_def_name(package_name, &func.name.0);
                        let resolved_fn = self.resolve_fn(func, &ctx, &mut hir_table, full_name);
                        *hir_table.def_mut(def_id) = hir::Def::Fn(resolved_fn);
                    }
                    ast::Item::ImplBlock(i) => {
                        let def_id = per_file_defs
                            .get(file_idx)
                            .and_then(|defs| defs.get(toplevel_idx))
                            .copied();
                        let Some(def_id) = def_id else {
                            self.ice("missing def id for impl block");
                            break;
                        };
                        toplevel_idx += 1;
                        let methods = i
                            .methods
                            .iter()
                            .map(|m| self.resolve_fn_def(m, &ctx, &mut hir_table))
                            .collect();
                        let tparams = type_param_set(&i.generics);
                        let trait_name = i.trait_name.as_ref().map(|t| {
                            HirIdent::name(self.lower_impl_trait_name(t, package_name, &imports))
                        });
                        let impl_block = hir::ImplBlock {
                            attrs: i.attrs.iter().map(|a| a.into()).collect(),
                            generics: i.generics.iter().map(|g| HirIdent::name(&g.0)).collect(),
                            trait_name,
                            for_type: self.lower_type_expr(
                                &i.for_type,
                                &tparams,
                                package_name,
                                &imports,
                            ),
                            methods,
                        };
                        *hir_table.def_mut(def_id) = hir::Def::ImplBlock(impl_block);
                    }
                    _ => {
                        toplevel_idx += 1;
                    }
                }
            }
        }

        let files = files
            .iter()
            .enumerate()
            .map(|(idx, file)| {
                let package = file.ast.package.0.clone();
                let file_name = file
                    .path
                    .file_name()
                    .and_then(|name| name.to_str())
                    .unwrap_or("<unknown>");
                let path = if package == "Main" || package == "Builtin" {
                    file_name.to_string()
                } else {
                    format!("{}/{}", package, file_name)
                };
                hir::SourceFileHir {
                    path,
                    package: hir::PackageName(package),
                    imports: file
                        .ast
                        .imports
                        .iter()
                        .map(|import| hir::PackageName(import.0.clone()))
                        .collect(),
                    toplevels: per_file_defs.get(idx).cloned().unwrap_or_default(),
                }
            })
            .collect();

        let diagnostics = self.diagnostics;
        (
            hir::ResolvedHir { files, toplevels },
            hir_table,
            diagnostics,
        )
    }

    fn resolve_fn_def(
        &mut self,
        func: &ast::Fn,
        ctx: &ResolutionContext,
        hir_table: &mut HirTable,
    ) -> hir::DefId {
        let def_id = hir_table.alloc_def(
            func.name.0.clone(),
            hir::DefKind::Fn,
            hir::Def::Fn(hir::Fn {
                attrs: Vec::new(),
                name: func.name.0.clone(),
                generics: Vec::new(),
                generic_bounds: Vec::new(),
                params: Vec::new(),
                ret_ty: None,
                body: hir_table.dummy_expr(),
            }),
        );
        hir_table.set_current_owner(def_id);
        let func = self.resolve_fn(func, ctx, hir_table, func.name.0.clone());
        *hir_table.def_mut(def_id) = hir::Def::Fn(func);
        def_id
    }

    fn resolve_fn(
        &mut self,
        func: &ast::Fn,
        ctx: &ResolutionContext,
        hir_table: &mut HirTable,
        resolved_name: String,
    ) -> hir::Fn {
        let ast::Fn {
            attrs,
            generics,
            generic_bounds,
            params,
            ret_ty,
            body,
            ..
        } = func;
        let mut env = ResolveLocalEnv::new();
        for param in params {
            env.add(&param.0, self.fresh_name(&param.0.0, hir_table));
        }
        let tparams = type_param_set(generics);
        let new_params = params
            .iter()
            .map(|param| {
                let local_id = env.rfind(&param.0).unwrap_or_else(|| {
                    self.ice(format!("missing local id for param {}", param.0.0));
                    self.fresh_name(&param.0.0, hir_table)
                });
                (
                    local_id,
                    self.lower_type_expr(&param.1, &tparams, ctx.current_package, ctx.imports),
                )
            })
            .collect();

        let new_generic_bounds = generic_bounds
            .iter()
            .map(|(param, traits)| {
                let traits = traits
                    .iter()
                    .map(|path| {
                        hir::Path::new(path.segments().iter().map(hir::PathSegment::from).collect())
                    })
                    .collect::<Vec<_>>();
                (HirIdent::name(&param.0), traits)
            })
            .collect();
        hir::Fn {
            attrs: attrs.iter().map(|a| a.into()).collect(),
            name: resolved_name,
            generics: generics.iter().map(|g| HirIdent::name(&g.0)).collect(),
            generic_bounds: new_generic_bounds,
            params: new_params,
            ret_ty: ret_ty
                .as_ref()
                .map(|t| self.lower_type_expr(t, &tparams, ctx.current_package, ctx.imports)),
            body: self.resolve_expr(body, &mut env, ctx, hir_table),
        }
    }

    fn resolve_expr(
        &mut self,
        expr: &ast::Expr,
        env: &mut ResolveLocalEnv,
        ctx: &ResolutionContext,
        hir_table: &mut HirTable,
    ) -> hir::ExprId {
        match expr {
            ast::Expr::EPath { path, astptr } => {
                if let Some(constructor) = self.constructor_path_for(path, ctx) {
                    return hir_table.alloc_expr(hir::Expr::EConstr {
                        constructor: hir::ConstructorRef::Unresolved(constructor),
                        args: Vec::new(),
                    });
                }
                if path.len() == 1 {
                    let Some(ident) = path.last_ident() else {
                        self.ice("path length 1 missing last ident");
                        return hir_table.alloc_expr(hir::Expr::ENameRef {
                            res: hir::NameRef::Unresolved(hir::Path::from_ident(
                                "<error>".to_string(),
                            )),
                            hint: "<error>".to_string(),
                            astptr: Some(*astptr),
                        });
                    };
                    let name_str = &ident.0;
                    let res = if let Some(local_id) = env.rfind(ident) {
                        hir::NameRef::Local(local_id)
                    } else {
                        let full_name = full_def_name(ctx.current_package, name_str);
                        if let Some(&def_id) = ctx.def_names.get(&full_name) {
                            hir::NameRef::Def(def_id)
                        } else if let Some(&builtin_id) = ctx.builtin_names.get(name_str) {
                            hir::NameRef::Builtin(builtin_id)
                        } else {
                            hir::NameRef::Unresolved(hir::Path::from_ident(name_str.clone()))
                        }
                    };
                    let hint = match res {
                        hir::NameRef::Def(_) => full_def_name(ctx.current_package, name_str),
                        _ => name_str.clone(),
                    };
                    hir_table.alloc_expr(hir::Expr::ENameRef {
                        res,
                        hint,
                        astptr: Some(*astptr),
                    })
                } else {
                    let full_name = path.display();
                    let package = path
                        .segments()
                        .first()
                        .map(|seg| seg.ident().0.as_str())
                        .unwrap_or_default();
                    if package != ctx.current_package
                        && package != "Builtin"
                        && ctx.deps.contains_key(package)
                        && !ctx.imports.contains(package)
                    {
                        self.error(format!(
                            "package {} not imported in package {}",
                            package, ctx.current_package
                        ));
                    }

                    let res = if package == ctx.current_package || package == "Builtin" {
                        ctx.def_names
                            .get(&full_name)
                            .copied()
                            .map(hir::NameRef::Def)
                            .unwrap_or_else(|| hir::NameRef::Unresolved(path.into()))
                    } else if ctx.imports.contains(package) {
                        ctx.deps
                            .get(package)
                            .and_then(|interface| interface.exports.get(&full_name))
                            .copied()
                            .map(hir::NameRef::Def)
                            .unwrap_or_else(|| hir::NameRef::Unresolved(path.into()))
                    } else {
                        hir::NameRef::Unresolved(path.into())
                    };
                    hir_table.alloc_expr(hir::Expr::ENameRef {
                        res,
                        hint: full_name,
                        astptr: Some(*astptr),
                    })
                }
            }
            ast::Expr::EUnit => hir_table.alloc_expr(hir::Expr::EUnit),
            ast::Expr::EBool { value } => hir_table.alloc_expr(hir::Expr::EBool { value: *value }),
            ast::Expr::EInt { value } => hir_table.alloc_expr(hir::Expr::EInt {
                value: value.clone(),
            }),
            ast::Expr::EInt8 { value } => hir_table.alloc_expr(hir::Expr::EInt8 {
                value: value.clone(),
            }),
            ast::Expr::EInt16 { value } => hir_table.alloc_expr(hir::Expr::EInt16 {
                value: value.clone(),
            }),
            ast::Expr::EInt32 { value } => hir_table.alloc_expr(hir::Expr::EInt32 {
                value: value.clone(),
            }),
            ast::Expr::EInt64 { value } => hir_table.alloc_expr(hir::Expr::EInt64 {
                value: value.clone(),
            }),
            ast::Expr::EUInt8 { value } => hir_table.alloc_expr(hir::Expr::EUInt8 {
                value: value.clone(),
            }),
            ast::Expr::EUInt16 { value } => hir_table.alloc_expr(hir::Expr::EUInt16 {
                value: value.clone(),
            }),
            ast::Expr::EUInt32 { value } => hir_table.alloc_expr(hir::Expr::EUInt32 {
                value: value.clone(),
            }),
            ast::Expr::EUInt64 { value } => hir_table.alloc_expr(hir::Expr::EUInt64 {
                value: value.clone(),
            }),
            ast::Expr::EFloat { value } => {
                hir_table.alloc_expr(hir::Expr::EFloat { value: *value })
            }
            ast::Expr::EFloat32 { value } => hir_table.alloc_expr(hir::Expr::EFloat32 {
                value: value.clone(),
            }),
            ast::Expr::EFloat64 { value } => hir_table.alloc_expr(hir::Expr::EFloat64 {
                value: value.clone(),
            }),
            ast::Expr::EString { value } => hir_table.alloc_expr(hir::Expr::EString {
                value: value.clone(),
            }),
            ast::Expr::EConstr { constructor, args } => {
                let new_args = args
                    .iter()
                    .map(|arg| self.resolve_expr(arg, env, ctx, hir_table))
                    .collect();
                let constructor = self.normalize_constructor_path(constructor, ctx);
                hir_table.alloc_expr(hir::Expr::EConstr {
                    constructor: hir::ConstructorRef::Unresolved(constructor),
                    args: new_args,
                })
            }
            ast::Expr::EStructLiteral { name, fields } => {
                let new_fields = fields
                    .iter()
                    .map(|(field_name, expr)| {
                        (
                            HirIdent::name(&field_name.0),
                            self.resolve_expr(expr, env, ctx, hir_table),
                        )
                    })
                    .collect();
                let qualified: hir::QualifiedPath = name.into();
                if let Some(package) = &qualified.package
                    && !ctx.package_allowed(package.as_str())
                {
                    self.error(format!(
                        "package {} not imported in package {}",
                        package.0, ctx.current_package
                    ));
                }
                hir_table.alloc_expr(hir::Expr::EStructLiteral {
                    name: qualified,
                    fields: new_fields,
                })
            }
            ast::Expr::ETuple { items } => {
                let new_items = items
                    .iter()
                    .map(|item| self.resolve_expr(item, env, ctx, hir_table))
                    .collect();
                hir_table.alloc_expr(hir::Expr::ETuple { items: new_items })
            }
            ast::Expr::EArray { items } => {
                let new_items = items
                    .iter()
                    .map(|item| self.resolve_expr(item, env, ctx, hir_table))
                    .collect();
                hir_table.alloc_expr(hir::Expr::EArray { items: new_items })
            }
            ast::Expr::EClosure { params, body } => {
                let mut closure_env = env.enter_scope();
                let new_params = params
                    .iter()
                    .map(|param| {
                        self.resolve_closure_param(param, &mut closure_env, ctx, hir_table)
                    })
                    .collect();
                let new_body_expr = self.resolve_expr(body, &mut closure_env, ctx, hir_table);

                hir_table.alloc_expr(hir::Expr::EClosure {
                    params: new_params,
                    body: new_body_expr,
                })
            }
            ast::Expr::ELet {
                pat,
                annotation,
                value,
            } => {
                let new_value = self.resolve_expr(value, env, ctx, hir_table);
                let new_pat = self.resolve_pat(pat, env, ctx, hir_table);
                hir_table.alloc_expr(hir::Expr::ELet {
                    pat: new_pat,
                    annotation: annotation.as_ref().map(|t| t.into()),
                    value: new_value,
                })
            }
            ast::Expr::EMatch {
                expr,
                arms,
                astptr: _,
            } => {
                let new_expr = self.resolve_expr(expr, env, ctx, hir_table);
                let new_arms = arms
                    .iter()
                    .map(|arm| {
                        let new_pat = self.resolve_pat(&arm.pat, env, ctx, hir_table);
                        let new_body = self.resolve_expr(&arm.body, env, ctx, hir_table);
                        hir::Arm {
                            pat: new_pat,
                            body: new_body,
                        }
                    })
                    .collect();
                hir_table.alloc_expr(hir::Expr::EMatch {
                    expr: new_expr,
                    arms: new_arms,
                })
            }
            ast::Expr::EIf {
                cond,
                then_branch,
                else_branch,
            } => {
                let new_cond = self.resolve_expr(cond, env, ctx, hir_table);
                let new_then = self.resolve_expr(then_branch, env, ctx, hir_table);
                let new_else = self.resolve_expr(else_branch, env, ctx, hir_table);
                hir_table.alloc_expr(hir::Expr::EIf {
                    cond: new_cond,
                    then_branch: new_then,
                    else_branch: new_else,
                })
            }
            ast::Expr::EWhile { cond, body } => {
                let new_cond = self.resolve_expr(cond, env, ctx, hir_table);
                let new_body = self.resolve_expr(body, env, ctx, hir_table);
                hir_table.alloc_expr(hir::Expr::EWhile {
                    cond: new_cond,
                    body: new_body,
                })
            }
            ast::Expr::EGo { expr } => {
                let new_expr = self.resolve_expr(expr, env, ctx, hir_table);
                hir_table.alloc_expr(hir::Expr::EGo { expr: new_expr })
            }
            ast::Expr::ECall { func, args } => {
                if let ast::Expr::EPath { path, .. } = func.as_ref()
                    && let Some(constructor) = self.constructor_path_for(path, ctx)
                {
                    let new_args = args
                        .iter()
                        .map(|arg| self.resolve_expr(arg, env, ctx, hir_table))
                        .collect();
                    return hir_table.alloc_expr(hir::Expr::EConstr {
                        constructor: hir::ConstructorRef::Unresolved(constructor),
                        args: new_args,
                    });
                }
                let new_func = self.resolve_expr(func, env, ctx, hir_table);
                let new_args = args
                    .iter()
                    .map(|arg| self.resolve_expr(arg, env, ctx, hir_table))
                    .collect();
                hir_table.alloc_expr(hir::Expr::ECall {
                    func: new_func,
                    args: new_args,
                })
            }
            ast::Expr::EUnary { op, expr } => {
                let new_expr = self.resolve_expr(expr, env, ctx, hir_table);
                hir_table.alloc_expr(hir::Expr::EUnary {
                    op: *op,
                    expr: new_expr,
                })
            }
            ast::Expr::EBinary { op, lhs, rhs } => {
                let new_lhs = self.resolve_expr(lhs, env, ctx, hir_table);
                let new_rhs = self.resolve_expr(rhs, env, ctx, hir_table);
                hir_table.alloc_expr(hir::Expr::EBinary {
                    op: *op,
                    lhs: new_lhs,
                    rhs: new_rhs,
                })
            }
            ast::Expr::EProj { tuple, index } => {
                let new_tuple = self.resolve_expr(tuple, env, ctx, hir_table);
                hir_table.alloc_expr(hir::Expr::EProj {
                    tuple: new_tuple,
                    index: *index,
                })
            }
            ast::Expr::EField {
                expr,
                field,
                astptr: _,
            } => {
                let new_expr = self.resolve_expr(expr, env, ctx, hir_table);
                hir_table.alloc_expr(hir::Expr::EField {
                    expr: new_expr,
                    field: HirIdent::name(&field.0),
                })
            }
            ast::Expr::EBlock { exprs } => {
                let new_exprs = exprs
                    .iter()
                    .map(|e| self.resolve_expr(e, env, ctx, hir_table))
                    .collect();
                hir_table.alloc_expr(hir::Expr::EBlock { exprs: new_exprs })
            }
        }
    }

    fn resolve_pat(
        &mut self,
        pat: &ast::Pat,
        env: &mut ResolveLocalEnv,
        ctx: &ResolutionContext,
        hir_table: &mut HirTable,
    ) -> hir::PatId {
        match pat {
            ast::Pat::PVar { name, astptr } => {
                let newname = self.fresh_name(&name.0, hir_table);
                env.add(name, newname);
                hir_table.alloc_pat(hir::Pat::PVar {
                    name: newname,
                    astptr: *astptr,
                })
            }
            ast::Pat::PUnit => hir_table.alloc_pat(hir::Pat::PUnit),
            ast::Pat::PBool { value } => hir_table.alloc_pat(hir::Pat::PBool { value: *value }),
            ast::Pat::PInt { value } => hir_table.alloc_pat(hir::Pat::PInt {
                value: value.clone(),
            }),
            ast::Pat::PInt8 { value } => hir_table.alloc_pat(hir::Pat::PInt8 {
                value: value.clone(),
            }),
            ast::Pat::PInt16 { value } => hir_table.alloc_pat(hir::Pat::PInt16 {
                value: value.clone(),
            }),
            ast::Pat::PInt32 { value } => hir_table.alloc_pat(hir::Pat::PInt32 {
                value: value.clone(),
            }),
            ast::Pat::PInt64 { value } => hir_table.alloc_pat(hir::Pat::PInt64 {
                value: value.clone(),
            }),
            ast::Pat::PUInt8 { value } => hir_table.alloc_pat(hir::Pat::PUInt8 {
                value: value.clone(),
            }),
            ast::Pat::PUInt16 { value } => hir_table.alloc_pat(hir::Pat::PUInt16 {
                value: value.clone(),
            }),
            ast::Pat::PUInt32 { value } => hir_table.alloc_pat(hir::Pat::PUInt32 {
                value: value.clone(),
            }),
            ast::Pat::PUInt64 { value } => hir_table.alloc_pat(hir::Pat::PUInt64 {
                value: value.clone(),
            }),
            ast::Pat::PString { value } => hir_table.alloc_pat(hir::Pat::PString {
                value: value.clone(),
            }),
            ast::Pat::PConstr { constructor, args } => {
                let new_args = args
                    .iter()
                    .map(|arg| self.resolve_pat(arg, env, ctx, hir_table))
                    .collect();
                let constructor = self.normalize_constructor_path(constructor, ctx);
                hir_table.alloc_pat(hir::Pat::PConstr {
                    constructor: hir::ConstructorRef::Unresolved(constructor),
                    args: new_args,
                })
            }
            ast::Pat::PStruct { name, fields } => {
                let new_fields = fields
                    .iter()
                    .map(|(fname, pat)| {
                        (
                            HirIdent::name(&fname.0),
                            self.resolve_pat(pat, env, ctx, hir_table),
                        )
                    })
                    .collect();
                let qualified: hir::QualifiedPath = name.into();
                if let Some(package) = &qualified.package
                    && !ctx.package_allowed(package.as_str())
                {
                    self.error(format!(
                        "package {} not imported in package {}",
                        package.0, ctx.current_package
                    ));
                }
                hir_table.alloc_pat(hir::Pat::PStruct {
                    name: qualified,
                    fields: new_fields,
                })
            }
            ast::Pat::PTuple { pats } => {
                let new_pats = pats
                    .iter()
                    .map(|pat| self.resolve_pat(pat, env, ctx, hir_table))
                    .collect();
                hir_table.alloc_pat(hir::Pat::PTuple { pats: new_pats })
            }
            ast::Pat::PWild => hir_table.alloc_pat(hir::Pat::PWild),
        }
    }

    fn lower_type_expr(
        &mut self,
        ty: &ast::TypeExpr,
        _tparams: &HashSet<String>,
        current_package: &str,
        imports: &HashSet<String>,
    ) -> hir::TypeExpr {
        match ty {
            ast::TypeExpr::TUnit => hir::TypeExpr::TUnit,
            ast::TypeExpr::TBool => hir::TypeExpr::TBool,
            ast::TypeExpr::TInt8 => hir::TypeExpr::TInt8,
            ast::TypeExpr::TInt16 => hir::TypeExpr::TInt16,
            ast::TypeExpr::TInt32 => hir::TypeExpr::TInt32,
            ast::TypeExpr::TInt64 => hir::TypeExpr::TInt64,
            ast::TypeExpr::TUint8 => hir::TypeExpr::TUint8,
            ast::TypeExpr::TUint16 => hir::TypeExpr::TUint16,
            ast::TypeExpr::TUint32 => hir::TypeExpr::TUint32,
            ast::TypeExpr::TUint64 => hir::TypeExpr::TUint64,
            ast::TypeExpr::TFloat32 => hir::TypeExpr::TFloat32,
            ast::TypeExpr::TFloat64 => hir::TypeExpr::TFloat64,
            ast::TypeExpr::TString => hir::TypeExpr::TString,
            ast::TypeExpr::TTuple { typs } => hir::TypeExpr::TTuple {
                typs: typs
                    .iter()
                    .map(|ty| self.lower_type_expr(ty, _tparams, current_package, imports))
                    .collect(),
            },
            ast::TypeExpr::TCon { path } => {
                let qualified = if path.len() == 1 {
                    let name = match path.last_ident() {
                        Some(ident) => ident.0.clone(),
                        None => {
                            self.ice("type path length 1 missing last ident");
                            "<error>".to_string()
                        }
                    };
                    hir::QualifiedPath {
                        package: None,
                        path: hir::Path::from_ident(name),
                    }
                } else {
                    let qualified: hir::QualifiedPath = path.into();
                    if let Some(package) = &qualified.package
                        && !package_allowed(package.as_str(), current_package, imports)
                    {
                        self.error(format!(
                            "package {} not imported in package {}",
                            package.0, current_package
                        ));
                    }
                    qualified
                };
                hir::TypeExpr::TCon { path: qualified }
            }
            ast::TypeExpr::TDyn { trait_path } => {
                let qualified = if trait_path.len() == 1 {
                    let name = match trait_path.last_ident() {
                        Some(ident) => ident.0.clone(),
                        None => {
                            self.ice("trait path length 1 missing last ident");
                            "<error>".to_string()
                        }
                    };
                    hir::QualifiedPath {
                        package: None,
                        path: hir::Path::from_ident(name),
                    }
                } else {
                    let qualified: hir::QualifiedPath = trait_path.into();
                    if let Some(package) = &qualified.package
                        && !package_allowed(package.as_str(), current_package, imports)
                    {
                        self.error(format!(
                            "package {} not imported in package {}",
                            package.0, current_package
                        ));
                    }
                    qualified
                };
                hir::TypeExpr::TDyn {
                    trait_path: qualified,
                }
            }
            ast::TypeExpr::TApp { ty, args } => hir::TypeExpr::TApp {
                ty: Box::new(self.lower_type_expr(ty.as_ref(), _tparams, current_package, imports)),
                args: args
                    .iter()
                    .map(|arg| self.lower_type_expr(arg, _tparams, current_package, imports))
                    .collect(),
            },
            ast::TypeExpr::TArray { len, elem } => hir::TypeExpr::TArray {
                len: *len,
                elem: Box::new(self.lower_type_expr(
                    elem.as_ref(),
                    _tparams,
                    current_package,
                    imports,
                )),
            },
            ast::TypeExpr::TFunc { params, ret_ty } => hir::TypeExpr::TFunc {
                params: params
                    .iter()
                    .map(|param| self.lower_type_expr(param, _tparams, current_package, imports))
                    .collect(),
                ret_ty: Box::new(self.lower_type_expr(
                    ret_ty.as_ref(),
                    _tparams,
                    current_package,
                    imports,
                )),
            },
        }
    }

    fn lower_enum_def(
        &mut self,
        def: &ast::EnumDef,
        current_package: &str,
        imports: &HashSet<String>,
    ) -> hir::EnumDef {
        let tparams = type_param_set(&def.generics);
        let name = full_def_name(current_package, &def.name.0);
        let variants = def
            .variants
            .iter()
            .map(|(variant_name, tys)| {
                let types = tys
                    .iter()
                    .map(|ty| self.lower_type_expr(ty, &tparams, current_package, imports))
                    .collect();
                (HirIdent::name(&variant_name.0), types)
            })
            .collect();
        hir::EnumDef {
            attrs: def.attrs.iter().map(|a| a.into()).collect(),
            name: HirIdent::name(&name),
            generics: def.generics.iter().map(|g| HirIdent::name(&g.0)).collect(),
            variants,
        }
    }

    fn lower_struct_def(
        &mut self,
        def: &ast::StructDef,
        current_package: &str,
        imports: &HashSet<String>,
    ) -> hir::StructDef {
        let tparams = type_param_set(&def.generics);
        let name = full_def_name(current_package, &def.name.0);
        let fields = def
            .fields
            .iter()
            .map(|(field_name, ty)| {
                (
                    HirIdent::name(&field_name.0),
                    self.lower_type_expr(ty, &tparams, current_package, imports),
                )
            })
            .collect();
        hir::StructDef {
            attrs: def.attrs.iter().map(|a| a.into()).collect(),
            name: HirIdent::name(&name),
            generics: def.generics.iter().map(|g| HirIdent::name(&g.0)).collect(),
            fields,
        }
    }

    fn lower_trait_def(
        &mut self,
        def: &ast::TraitDef,
        current_package: &str,
        imports: &HashSet<String>,
    ) -> hir::TraitDef {
        let name = full_def_name(current_package, &def.name.0);
        let method_sigs = def
            .method_sigs
            .iter()
            .map(|sig| hir::TraitMethodSignature {
                name: HirIdent::name(&sig.name.0),
                params: sig
                    .params
                    .iter()
                    .map(|ty| self.lower_type_expr(ty, &HashSet::new(), current_package, imports))
                    .collect(),
                ret_ty: self.lower_type_expr(
                    &sig.ret_ty,
                    &HashSet::new(),
                    current_package,
                    imports,
                ),
            })
            .collect();
        hir::TraitDef {
            attrs: def.attrs.iter().map(|a| a.into()).collect(),
            name: HirIdent::name(&name),
            method_sigs,
        }
    }

    fn lower_impl_trait_name(
        &mut self,
        path: &ast::Path,
        current_package: &str,
        imports: &HashSet<String>,
    ) -> String {
        if path.len() == 1 {
            let name = match path.last_ident() {
                Some(ident) => ident.0.clone(),
                None => {
                    self.ice("impl trait path length 1 missing last ident");
                    "<error>".to_string()
                }
            };
            return full_def_name(current_package, &name);
        }

        let qualified: hir::QualifiedPath = path.into();
        if let Some(package) = &qualified.package
            && !package_allowed(package.as_str(), current_package, imports)
        {
            self.error(format!(
                "package {} not imported in package {}",
                package.0, current_package
            ));
        }
        qualified.display()
    }

    fn lower_extern_go(
        &mut self,
        def: &ast::ExternGo,
        current_package: &str,
        imports: &HashSet<String>,
    ) -> hir::ExternGo {
        let name = full_def_name(current_package, &def.goml_name.0);
        hir::ExternGo {
            attrs: def.attrs.iter().map(|a| a.into()).collect(),
            package_path: def.package_path.clone(),
            go_symbol: def.go_symbol.clone(),
            goml_name: HirIdent::name(&name),
            explicit_go_symbol: def.explicit_go_symbol,
            params: def
                .params
                .iter()
                .map(|(param, ty)| {
                    (
                        HirIdent::name(&param.0),
                        self.lower_type_expr(ty, &HashSet::new(), current_package, imports),
                    )
                })
                .collect(),
            ret_ty: def
                .ret_ty
                .as_ref()
                .map(|ty| self.lower_type_expr(ty, &HashSet::new(), current_package, imports)),
        }
    }

    fn lower_extern_type(&self, def: &ast::ExternType, current_package: &str) -> hir::ExternType {
        let name = full_def_name(current_package, &def.goml_name.0);
        hir::ExternType {
            attrs: def.attrs.iter().map(|a| a.into()).collect(),
            goml_name: HirIdent::name(&name),
        }
    }

    fn lower_extern_builtin(
        &mut self,
        def: &ast::ExternBuiltin,
        current_package: &str,
        imports: &HashSet<String>,
    ) -> hir::ExternBuiltin {
        let name = full_def_name(current_package, &def.name.0);
        hir::ExternBuiltin {
            attrs: def.attrs.iter().map(|a| a.into()).collect(),
            name: HirIdent::name(&name),
            params: def
                .params
                .iter()
                .map(|(param, ty)| {
                    (
                        HirIdent::name(&param.0),
                        self.lower_type_expr(ty, &HashSet::new(), current_package, imports),
                    )
                })
                .collect(),
            ret_ty: def
                .ret_ty
                .as_ref()
                .map(|ty| self.lower_type_expr(ty, &HashSet::new(), current_package, imports)),
        }
    }

    fn resolve_closure_param(
        &mut self,
        param: &ast::ClosureParam,
        env: &mut ResolveLocalEnv,
        ctx: &ResolutionContext,
        hir_table: &mut HirTable,
    ) -> hir::ClosureParam {
        let new_name = self.fresh_name(&param.name.0, hir_table);
        env.add(&param.name, new_name);
        hir::ClosureParam {
            name: new_name,
            ty: param.ty.as_ref().map(|t| {
                self.lower_type_expr(t, &HashSet::new(), ctx.current_package, ctx.imports)
            }),
            astptr: param.astptr,
        }
    }
}

fn type_param_set(params: &[ast::AstIdent]) -> HashSet<String> {
    params.iter().map(|param| param.0.clone()).collect()
}

fn full_def_path(package: &str, name: &str) -> hir::Path {
    if package == "Builtin" || package == "Main" {
        hir::Path::from_ident(name.to_string())
    } else {
        hir::Path::from_idents(vec![package.to_string(), name.to_string()])
    }
}

fn constructor_path(package: &str, enum_name: &str, variant: &str) -> hir::Path {
    if package == "Builtin" || package == "Main" {
        hir::Path::from_idents(vec![enum_name.to_string(), variant.to_string()])
    } else {
        hir::Path::from_idents(vec![
            package.to_string(),
            enum_name.to_string(),
            variant.to_string(),
        ])
    }
}
