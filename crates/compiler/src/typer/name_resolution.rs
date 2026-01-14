use std::collections::{HashMap, HashSet};

use ast::ast;

use crate::builtins;
use crate::env;
use crate::fir;
use crate::fir::FirIdent;

pub type FirTable = fir::FirTable;

#[derive(Default)]
pub struct NameResolution {}

#[derive(Debug)]
struct ResolveLocalEnv(im::Vector<(ast::AstIdent, fir::LocalId)>);

impl ResolveLocalEnv {
    pub fn new() -> Self {
        Self(im::Vector::new())
    }

    #[allow(unused)]
    pub fn enter_scope(&self) -> Self {
        Self(self.0.clone())
    }

    pub fn add(&mut self, name: &ast::AstIdent, new_name: fir::LocalId) {
        self.0.push_back((name.clone(), new_name));
    }

    pub fn rfind(&self, key: &ast::AstIdent) -> Option<fir::LocalId> {
        self.0
            .iter()
            .rfind(|(name, _)| name == key)
            .map(|(_, new_name)| *new_name)
    }
}

struct ResolutionContext<'a> {
    builtin_names: &'a HashMap<String, fir::BuiltinId>,
    def_names: &'a HashMap<String, fir::DefId>,
    deps: &'a HashMap<String, fir::PackageInterface>,
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
        files: &[fir::SourceFileAst],
        deps: &HashMap<String, fir::PackageInterface>,
    ) -> Self {
        let mut index = Self {
            enums_by_package: HashMap::new(),
        };
        index.add_files(files);
        if !files.iter().any(|file| file.ast.package.0 == "Builtin") {
            let builtin_ast = builtins::get_builtin_ast();
            let builtin_file = fir::SourceFileAst {
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

    fn add_files(&mut self, files: &[fir::SourceFileAst]) {
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

    fn add_interface(&mut self, package: &str, interface: &fir::PackageInterface) {
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
            .map_or(false, |variants| variants.contains(variant))
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
        self.enums_by_package.get(package).map_or(false, |enums| {
            enums.values().any(|vars| vars.contains(variant))
        })
    }
}

impl ResolutionContext<'_> {
    fn package_allowed(&self, package: &str) -> bool {
        package == self.current_package || package == "Builtin" || self.imports.contains(package)
    }
}

impl NameResolution {
    fn fresh_name(&self, name: &str, fir_table: &mut FirTable) -> fir::LocalId {
        fir_table.fresh_local(name)
    }

    fn constructor_path_for(&self, path: &ast::Path, ctx: &ResolutionContext) -> Option<fir::Path> {
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
                    Some(fir::Path::from_ident(variant.clone()))
                } else {
                    None
                }
            }
            2 => {
                let enum_name = &segments[0].ident.0;
                let variant = &segments[1].ident.0;
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
                let package = &segments[0].ident.0;
                let enum_name = &segments[1].ident.0;
                let variant = &segments[2].ident.0;
                let exists = ctx
                    .constructor_index
                    .enum_has_variant(package, enum_name, variant);
                if exists && !ctx.package_allowed(package) {
                    panic!(
                        "package {} not imported in package {}",
                        package, ctx.current_package
                    );
                }
                exists.then(|| constructor_path(package, enum_name, variant))
            }
            _ => None,
        }
    }

    fn normalize_constructor_path(&self, path: &ast::Path, ctx: &ResolutionContext) -> fir::Path {
        self.constructor_path_for(path, ctx)
            .unwrap_or_else(|| path.into())
    }

    pub fn resolve_files(&self, files: Vec<ast::File>) -> (fir::ResolvedFir, FirTable) {
        let deps = HashMap::new();
        let package_name = files
            .first()
            .map(|file| file.package.0.as_str())
            .unwrap_or("Main");
        let package_id = match package_name {
            "Builtin" => fir::PackageId(0),
            "Main" => fir::PackageId(1),
            _ => fir::PackageId(2),
        };
        let files = files
            .into_iter()
            .enumerate()
            .map(|(idx, ast)| fir::SourceFileAst {
                path: format!("<unknown:{}>", idx).into(),
                ast,
            })
            .collect();
        self.resolve_files_with_env(package_id, files, &deps)
    }

    pub fn resolve_files_with_env(
        &self,
        package_id: fir::PackageId,
        files: Vec<fir::SourceFileAst>,
        deps: &HashMap<String, fir::PackageInterface>,
    ) -> (fir::ResolvedFir, FirTable) {
        let mut fir_table = FirTable::new(package_id);

        let mut builtin_names = HashMap::new();
        for name in env::builtin_function_names() {
            if let Some(id) = fir::BuiltinId::from_name(&name) {
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
                        let id = fir_table.alloc_def_with_path(
                            path,
                            fir::DefKind::Fn,
                            fir::Def::Fn(fir::Fn {
                                attrs: Vec::new(),
                                name: full_name.clone(),
                                generics: Vec::new(),
                                params: Vec::new(),
                                ret_ty: None,
                                body: fir_table.dummy_expr(),
                            }),
                        );
                        def_names.insert(full_name, id);
                        id
                    }
                    ast::Item::ExternGo(ext) => {
                        let full_name = full_def_name(package_name, &ext.goml_name.0);
                        let path = full_def_path(package_name, &ext.goml_name.0);
                        let ext_def = self.lower_extern_go(ext, package_name, &imports);
                        let id = fir_table.alloc_def_with_path(
                            path,
                            fir::DefKind::ExternGo,
                            fir::Def::ExternGo(ext_def),
                        );
                        def_names.insert(full_name, id);
                        id
                    }
                    ast::Item::ExternBuiltin(ext) => {
                        let full_name = full_def_name(package_name, &ext.name.0);
                        let path = full_def_path(package_name, &ext.name.0);
                        let ext_def = self.lower_extern_builtin(ext, package_name, &imports);
                        let id = fir_table.alloc_def_with_path(
                            path,
                            fir::DefKind::ExternBuiltin,
                            fir::Def::ExternBuiltin(ext_def),
                        );
                        def_names.insert(full_name, id);
                        id
                    }
                    ast::Item::EnumDef(e) => {
                        let full_name = full_def_name(package_name, &e.name.0);
                        let path = full_def_path(package_name, &e.name.0);
                        let enum_def = self.lower_enum_def(e, package_name, &imports);
                        let id = fir_table.alloc_def_with_path(
                            path,
                            fir::DefKind::EnumDef,
                            fir::Def::EnumDef(enum_def),
                        );
                        def_names.insert(full_name, id);
                        id
                    }
                    ast::Item::StructDef(s) => {
                        let full_name = full_def_name(package_name, &s.name.0);
                        let path = full_def_path(package_name, &s.name.0);
                        let struct_def = self.lower_struct_def(s, package_name, &imports);
                        let id = fir_table.alloc_def_with_path(
                            path,
                            fir::DefKind::StructDef,
                            fir::Def::StructDef(struct_def),
                        );
                        def_names.insert(full_name, id);
                        id
                    }
                    ast::Item::TraitDef(t) => {
                        let full_name = full_def_name(package_name, &t.name.0);
                        let path = full_def_path(package_name, &t.name.0);
                        let trait_def = self.lower_trait_def(t, package_name, &imports);
                        let id = fir_table.alloc_def_with_path(
                            path,
                            fir::DefKind::TraitDef,
                            fir::Def::TraitDef(trait_def),
                        );
                        def_names.insert(full_name, id);
                        id
                    }
                    ast::Item::ImplBlock(_i) => fir_table.alloc_def_with_path(
                        full_def_path(package_name, "impl"),
                        fir::DefKind::ImplBlock,
                        fir::Def::ImplBlock(fir::ImplBlock {
                            attrs: Vec::new(),
                            generics: Vec::new(),
                            trait_name: None,
                            for_type: fir::TypeExpr::TUnit,
                            methods: Vec::new(),
                        }),
                    ),
                    ast::Item::ExternType(ext) => {
                        let full_name = full_def_name(package_name, &ext.goml_name.0);
                        let path = full_def_path(package_name, &ext.goml_name.0);
                        let ext_def = self.lower_extern_type(ext, package_name);
                        let id = fir_table.alloc_def_with_path(
                            path,
                            fir::DefKind::ExternType,
                            fir::Def::ExternType(ext_def),
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
                        let def_id = per_file_defs[file_idx][toplevel_idx];
                        toplevel_idx += 1;
                        fir_table.set_current_owner(def_id);
                        let full_name = full_def_name(package_name, &func.name.0);
                        let resolved_fn = self.resolve_fn(func, &ctx, &mut fir_table, full_name);
                        *fir_table.def_mut(def_id) = fir::Def::Fn(resolved_fn);
                    }
                    ast::Item::ImplBlock(i) => {
                        let def_id = per_file_defs[file_idx][toplevel_idx];
                        toplevel_idx += 1;
                        let methods = i
                            .methods
                            .iter()
                            .map(|m| self.resolve_fn_def(m, &ctx, &mut fir_table))
                            .collect();
                        let tparams = type_param_set(&i.generics);
                        let impl_block = fir::ImplBlock {
                            attrs: i.attrs.iter().map(|a| a.into()).collect(),
                            generics: i.generics.iter().map(|g| FirIdent::name(&g.0)).collect(),
                            trait_name: i
                                .trait_name
                                .as_ref()
                                .map(|t| FirIdent::name(full_def_name(package_name, &t.0))),
                            for_type: Self::lower_type_expr(
                                &i.for_type,
                                &tparams,
                                package_name,
                                &imports,
                            ),
                            methods,
                        };
                        *fir_table.def_mut(def_id) = fir::Def::ImplBlock(impl_block);
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
                fir::SourceFileFir {
                    path,
                    package: fir::PackageName(package),
                    imports: file
                        .ast
                        .imports
                        .iter()
                        .map(|import| fir::PackageName(import.0.clone()))
                        .collect(),
                    toplevels: per_file_defs[idx].clone(),
                }
            })
            .collect();

        (
            fir::ResolvedFir { files, toplevels },
            fir_table,
        )
    }

    fn resolve_fn_def(
        &self,
        func: &ast::Fn,
        ctx: &ResolutionContext,
        fir_table: &mut FirTable,
    ) -> fir::DefId {
        let def_id = fir_table.alloc_def(
            func.name.0.clone(),
            fir::DefKind::Fn,
            fir::Def::Fn(fir::Fn {
                attrs: Vec::new(),
                name: func.name.0.clone(),
                generics: Vec::new(),
                params: Vec::new(),
                ret_ty: None,
                body: fir_table.dummy_expr(),
            }),
        );
        fir_table.set_current_owner(def_id);
        let func = self.resolve_fn(func, ctx, fir_table, func.name.0.clone());
        *fir_table.def_mut(def_id) = fir::Def::Fn(func);
        def_id
    }

    fn resolve_fn(
        &self,
        func: &ast::Fn,
        ctx: &ResolutionContext,
        fir_table: &mut FirTable,
        resolved_name: String,
    ) -> fir::Fn {
        let ast::Fn {
            attrs,
            generics,
            params,
            ret_ty,
            body,
            ..
        } = func;
        let mut env = ResolveLocalEnv::new();
        for param in params {
            env.add(&param.0, self.fresh_name(&param.0.0, fir_table));
        }
        let tparams = type_param_set(generics);
        let new_params = params
            .iter()
            .map(|param| {
                (
                    env.rfind(&param.0).unwrap(),
                    Self::lower_type_expr(&param.1, &tparams, ctx.current_package, ctx.imports),
                )
            })
            .collect();
        fir::Fn {
            attrs: attrs.iter().map(|a| a.into()).collect(),
            name: resolved_name,
            generics: generics.iter().map(|g| FirIdent::name(&g.0)).collect(),
            params: new_params,
            ret_ty: ret_ty
                .as_ref()
                .map(|t| Self::lower_type_expr(t, &tparams, ctx.current_package, ctx.imports)),
            body: self.resolve_expr(body, &mut env, ctx, fir_table),
        }
    }

    fn resolve_expr(
        &self,
        expr: &ast::Expr,
        env: &mut ResolveLocalEnv,
        ctx: &ResolutionContext,
        fir_table: &mut FirTable,
    ) -> fir::ExprId {
        match expr {
            ast::Expr::EPath { path, astptr } => {
                if let Some(constructor) = self.constructor_path_for(path, ctx) {
                    return fir_table.alloc_expr(fir::Expr::EConstr {
                        constructor: fir::ConstructorRef::Unresolved(constructor),
                        args: Vec::new(),
                    });
                }
                if path.len() == 1 {
                    let ident = path.last_ident().unwrap();
                    let name_str = &ident.0;
                    let res = if let Some(local_id) = env.rfind(ident) {
                        fir::NameRef::Local(local_id)
                    } else {
                        let full_name = full_def_name(ctx.current_package, name_str);
                        if let Some(&def_id) = ctx.def_names.get(&full_name) {
                            fir::NameRef::Def(def_id)
                        } else if let Some(&builtin_id) = ctx.builtin_names.get(name_str) {
                            fir::NameRef::Builtin(builtin_id)
                        } else {
                            fir::NameRef::Unresolved(fir::Path::from_ident(name_str.clone()))
                        }
                    };
                    let hint = match res {
                        fir::NameRef::Def(_) => full_def_name(ctx.current_package, name_str),
                        _ => name_str.clone(),
                    };
                    fir_table.alloc_expr(fir::Expr::ENameRef {
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
                        panic!(
                            "package {} not imported in package {}",
                            package, ctx.current_package
                        );
                    }

                    let res = if package == ctx.current_package || package == "Builtin" {
                        ctx.def_names
                            .get(&full_name)
                            .copied()
                            .map(fir::NameRef::Def)
                            .unwrap_or_else(|| fir::NameRef::Unresolved(path.into()))
                    } else if ctx.imports.contains(package) {
                        ctx.deps
                            .get(package)
                            .and_then(|interface| interface.exports.get(&full_name))
                            .copied()
                            .map(fir::NameRef::Def)
                            .unwrap_or_else(|| fir::NameRef::Unresolved(path.into()))
                    } else {
                        fir::NameRef::Unresolved(path.into())
                    };
                    fir_table.alloc_expr(fir::Expr::ENameRef {
                        res,
                        hint: full_name,
                        astptr: Some(*astptr),
                    })
                }
            }
            ast::Expr::EUnit => fir_table.alloc_expr(fir::Expr::EUnit),
            ast::Expr::EBool { value } => fir_table.alloc_expr(fir::Expr::EBool { value: *value }),
            ast::Expr::EInt { value } => fir_table.alloc_expr(fir::Expr::EInt {
                value: value.clone(),
            }),
            ast::Expr::EInt8 { value } => fir_table.alloc_expr(fir::Expr::EInt8 {
                value: value.clone(),
            }),
            ast::Expr::EInt16 { value } => fir_table.alloc_expr(fir::Expr::EInt16 {
                value: value.clone(),
            }),
            ast::Expr::EInt32 { value } => fir_table.alloc_expr(fir::Expr::EInt32 {
                value: value.clone(),
            }),
            ast::Expr::EInt64 { value } => fir_table.alloc_expr(fir::Expr::EInt64 {
                value: value.clone(),
            }),
            ast::Expr::EUInt8 { value } => fir_table.alloc_expr(fir::Expr::EUInt8 {
                value: value.clone(),
            }),
            ast::Expr::EUInt16 { value } => fir_table.alloc_expr(fir::Expr::EUInt16 {
                value: value.clone(),
            }),
            ast::Expr::EUInt32 { value } => fir_table.alloc_expr(fir::Expr::EUInt32 {
                value: value.clone(),
            }),
            ast::Expr::EUInt64 { value } => fir_table.alloc_expr(fir::Expr::EUInt64 {
                value: value.clone(),
            }),
            ast::Expr::EFloat { value } => {
                fir_table.alloc_expr(fir::Expr::EFloat { value: *value })
            }
            ast::Expr::EFloat32 { value } => fir_table.alloc_expr(fir::Expr::EFloat32 {
                value: value.clone(),
            }),
            ast::Expr::EFloat64 { value } => fir_table.alloc_expr(fir::Expr::EFloat64 {
                value: value.clone(),
            }),
            ast::Expr::EString { value } => fir_table.alloc_expr(fir::Expr::EString {
                value: value.clone(),
            }),
            ast::Expr::EConstr { constructor, args } => {
                let new_args = args
                    .iter()
                    .map(|arg| self.resolve_expr(arg, env, ctx, fir_table))
                    .collect();
                let constructor = self.normalize_constructor_path(constructor, ctx);
                fir_table.alloc_expr(fir::Expr::EConstr {
                    constructor: fir::ConstructorRef::Unresolved(constructor),
                    args: new_args,
                })
            }
            ast::Expr::EStructLiteral { name, fields } => {
                let new_fields = fields
                    .iter()
                    .map(|(field_name, expr)| {
                        (
                            FirIdent::name(&field_name.0),
                            self.resolve_expr(expr, env, ctx, fir_table),
                        )
                    })
                    .collect();
                let qualified: fir::QualifiedPath = name.into();
                if let Some(package) = &qualified.package
                    && !ctx.package_allowed(package.as_str())
                {
                    panic!(
                        "package {} not imported in package {}",
                        package.0, ctx.current_package
                    );
                }
                fir_table.alloc_expr(fir::Expr::EStructLiteral {
                    name: qualified,
                    fields: new_fields,
                })
            }
            ast::Expr::ETuple { items } => {
                let new_items = items
                    .iter()
                    .map(|item| self.resolve_expr(item, env, ctx, fir_table))
                    .collect();
                fir_table.alloc_expr(fir::Expr::ETuple { items: new_items })
            }
            ast::Expr::EArray { items } => {
                let new_items = items
                    .iter()
                    .map(|item| self.resolve_expr(item, env, ctx, fir_table))
                    .collect();
                fir_table.alloc_expr(fir::Expr::EArray { items: new_items })
            }
            ast::Expr::EClosure { params, body } => {
                let mut closure_env = env.enter_scope();
                let new_params = params
                    .iter()
                    .map(|param| {
                        self.resolve_closure_param(param, &mut closure_env, ctx, fir_table)
                    })
                    .collect();
                let new_body_expr = self.resolve_expr(body, &mut closure_env, ctx, fir_table);

                fir_table.alloc_expr(fir::Expr::EClosure {
                    params: new_params,
                    body: new_body_expr,
                })
            }
            ast::Expr::ELet {
                pat,
                annotation,
                value,
            } => {
                let new_value = self.resolve_expr(value, env, ctx, fir_table);
                let new_pat = self.resolve_pat(pat, env, ctx, fir_table);
                fir_table.alloc_expr(fir::Expr::ELet {
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
                let new_expr = self.resolve_expr(expr, env, ctx, fir_table);
                let new_arms = arms
                    .iter()
                    .map(|arm| {
                        let new_pat = self.resolve_pat(&arm.pat, env, ctx, fir_table);
                        let new_body = self.resolve_expr(&arm.body, env, ctx, fir_table);
                        fir::Arm {
                            pat: new_pat,
                            body: new_body,
                        }
                    })
                    .collect();
                fir_table.alloc_expr(fir::Expr::EMatch {
                    expr: new_expr,
                    arms: new_arms,
                })
            }
            ast::Expr::EIf {
                cond,
                then_branch,
                else_branch,
            } => {
                let new_cond = self.resolve_expr(cond, env, ctx, fir_table);
                let new_then = self.resolve_expr(then_branch, env, ctx, fir_table);
                let new_else = self.resolve_expr(else_branch, env, ctx, fir_table);
                fir_table.alloc_expr(fir::Expr::EIf {
                    cond: new_cond,
                    then_branch: new_then,
                    else_branch: new_else,
                })
            }
            ast::Expr::EWhile { cond, body } => {
                let new_cond = self.resolve_expr(cond, env, ctx, fir_table);
                let new_body = self.resolve_expr(body, env, ctx, fir_table);
                fir_table.alloc_expr(fir::Expr::EWhile {
                    cond: new_cond,
                    body: new_body,
                })
            }
            ast::Expr::EGo { expr } => {
                let new_expr = self.resolve_expr(expr, env, ctx, fir_table);
                fir_table.alloc_expr(fir::Expr::EGo { expr: new_expr })
            }
            ast::Expr::ECall { func, args } => {
                if let ast::Expr::EPath { path, .. } = func.as_ref()
                    && let Some(constructor) = self.constructor_path_for(path, ctx)
                {
                    let new_args = args
                        .iter()
                        .map(|arg| self.resolve_expr(arg, env, ctx, fir_table))
                        .collect();
                    return fir_table.alloc_expr(fir::Expr::EConstr {
                        constructor: fir::ConstructorRef::Unresolved(constructor),
                        args: new_args,
                    });
                }
                let new_func = self.resolve_expr(func, env, ctx, fir_table);
                let new_args = args
                    .iter()
                    .map(|arg| self.resolve_expr(arg, env, ctx, fir_table))
                    .collect();
                fir_table.alloc_expr(fir::Expr::ECall {
                    func: new_func,
                    args: new_args,
                })
            }
            ast::Expr::EUnary { op, expr } => {
                let new_expr = self.resolve_expr(expr, env, ctx, fir_table);
                fir_table.alloc_expr(fir::Expr::EUnary {
                    op: *op,
                    expr: new_expr,
                })
            }
            ast::Expr::EBinary { op, lhs, rhs } => {
                let new_lhs = self.resolve_expr(lhs, env, ctx, fir_table);
                let new_rhs = self.resolve_expr(rhs, env, ctx, fir_table);
                fir_table.alloc_expr(fir::Expr::EBinary {
                    op: *op,
                    lhs: new_lhs,
                    rhs: new_rhs,
                })
            }
            ast::Expr::EProj { tuple, index } => {
                let new_tuple = self.resolve_expr(tuple, env, ctx, fir_table);
                fir_table.alloc_expr(fir::Expr::EProj {
                    tuple: new_tuple,
                    index: *index,
                })
            }
            ast::Expr::EField {
                expr,
                field,
                astptr: _,
            } => {
                let new_expr = self.resolve_expr(expr, env, ctx, fir_table);
                fir_table.alloc_expr(fir::Expr::EField {
                    expr: new_expr,
                    field: FirIdent::name(&field.0),
                })
            }
            ast::Expr::EBlock { exprs } => {
                let new_exprs = exprs
                    .iter()
                    .map(|e| self.resolve_expr(e, env, ctx, fir_table))
                    .collect();
                fir_table.alloc_expr(fir::Expr::EBlock { exprs: new_exprs })
            }
        }
    }

    fn resolve_pat(
        &self,
        pat: &ast::Pat,
        env: &mut ResolveLocalEnv,
        ctx: &ResolutionContext,
        fir_table: &mut FirTable,
    ) -> fir::PatId {
        match pat {
            ast::Pat::PVar { name, astptr } => {
                let newname = self.fresh_name(&name.0, fir_table);
                env.add(name, newname);
                fir_table.alloc_pat(fir::Pat::PVar {
                    name: newname,
                    astptr: *astptr,
                })
            }
            ast::Pat::PUnit => fir_table.alloc_pat(fir::Pat::PUnit),
            ast::Pat::PBool { value } => fir_table.alloc_pat(fir::Pat::PBool { value: *value }),
            ast::Pat::PInt { value } => fir_table.alloc_pat(fir::Pat::PInt {
                value: value.clone(),
            }),
            ast::Pat::PInt8 { value } => fir_table.alloc_pat(fir::Pat::PInt8 {
                value: value.clone(),
            }),
            ast::Pat::PInt16 { value } => fir_table.alloc_pat(fir::Pat::PInt16 {
                value: value.clone(),
            }),
            ast::Pat::PInt32 { value } => fir_table.alloc_pat(fir::Pat::PInt32 {
                value: value.clone(),
            }),
            ast::Pat::PInt64 { value } => fir_table.alloc_pat(fir::Pat::PInt64 {
                value: value.clone(),
            }),
            ast::Pat::PUInt8 { value } => fir_table.alloc_pat(fir::Pat::PUInt8 {
                value: value.clone(),
            }),
            ast::Pat::PUInt16 { value } => fir_table.alloc_pat(fir::Pat::PUInt16 {
                value: value.clone(),
            }),
            ast::Pat::PUInt32 { value } => fir_table.alloc_pat(fir::Pat::PUInt32 {
                value: value.clone(),
            }),
            ast::Pat::PUInt64 { value } => fir_table.alloc_pat(fir::Pat::PUInt64 {
                value: value.clone(),
            }),
            ast::Pat::PString { value } => fir_table.alloc_pat(fir::Pat::PString {
                value: value.clone(),
            }),
            ast::Pat::PConstr { constructor, args } => {
                let new_args = args
                    .iter()
                    .map(|arg| self.resolve_pat(arg, env, ctx, fir_table))
                    .collect();
                let constructor = self.normalize_constructor_path(constructor, ctx);
                fir_table.alloc_pat(fir::Pat::PConstr {
                    constructor: fir::ConstructorRef::Unresolved(constructor),
                    args: new_args,
                })
            }
            ast::Pat::PStruct { name, fields } => {
                let new_fields = fields
                    .iter()
                    .map(|(fname, pat)| {
                        (
                            FirIdent::name(&fname.0),
                            self.resolve_pat(pat, env, ctx, fir_table),
                        )
                    })
                    .collect();
                let qualified: fir::QualifiedPath = name.into();
                if let Some(package) = &qualified.package
                    && !ctx.package_allowed(package.as_str())
                {
                    panic!(
                        "package {} not imported in package {}",
                        package.0, ctx.current_package
                    );
                }
                fir_table.alloc_pat(fir::Pat::PStruct {
                    name: qualified,
                    fields: new_fields,
                })
            }
            ast::Pat::PTuple { pats } => {
                let new_pats = pats
                    .iter()
                    .map(|pat| self.resolve_pat(pat, env, ctx, fir_table))
                    .collect();
                fir_table.alloc_pat(fir::Pat::PTuple { pats: new_pats })
            }
            ast::Pat::PWild => fir_table.alloc_pat(fir::Pat::PWild),
        }
    }

    fn lower_type_expr(
        ty: &ast::TypeExpr,
        tparams: &HashSet<String>,
        current_package: &str,
        imports: &HashSet<String>,
    ) -> fir::TypeExpr {
        match ty {
            ast::TypeExpr::TUnit => fir::TypeExpr::TUnit,
            ast::TypeExpr::TBool => fir::TypeExpr::TBool,
            ast::TypeExpr::TInt8 => fir::TypeExpr::TInt8,
            ast::TypeExpr::TInt16 => fir::TypeExpr::TInt16,
            ast::TypeExpr::TInt32 => fir::TypeExpr::TInt32,
            ast::TypeExpr::TInt64 => fir::TypeExpr::TInt64,
            ast::TypeExpr::TUint8 => fir::TypeExpr::TUint8,
            ast::TypeExpr::TUint16 => fir::TypeExpr::TUint16,
            ast::TypeExpr::TUint32 => fir::TypeExpr::TUint32,
            ast::TypeExpr::TUint64 => fir::TypeExpr::TUint64,
            ast::TypeExpr::TFloat32 => fir::TypeExpr::TFloat32,
            ast::TypeExpr::TFloat64 => fir::TypeExpr::TFloat64,
            ast::TypeExpr::TString => fir::TypeExpr::TString,
            ast::TypeExpr::TTuple { typs } => fir::TypeExpr::TTuple {
                typs: typs
                    .iter()
                    .map(|ty| Self::lower_type_expr(ty, tparams, current_package, imports))
                    .collect(),
            },
            ast::TypeExpr::TCon { path } => {
                let qualified = if path.len() == 1 {
                    let name = path.last_ident().unwrap().0.clone();
                    fir::QualifiedPath {
                        package: None,
                        path: fir::Path::from_ident(name),
                    }
                } else {
                    let qualified: fir::QualifiedPath = path.into();
                    if let Some(package) = &qualified.package
                        && !package_allowed(package.as_str(), current_package, imports)
                    {
                        panic!(
                            "package {} not imported in package {}",
                            package.0, current_package
                        );
                    }
                    qualified
                };
                fir::TypeExpr::TCon { path: qualified }
            }
            ast::TypeExpr::TApp { ty, args } => fir::TypeExpr::TApp {
                ty: Box::new(Self::lower_type_expr(
                    ty.as_ref(),
                    tparams,
                    current_package,
                    imports,
                )),
                args: args
                    .iter()
                    .map(|arg| Self::lower_type_expr(arg, tparams, current_package, imports))
                    .collect(),
            },
            ast::TypeExpr::TArray { len, elem } => fir::TypeExpr::TArray {
                len: *len,
                elem: Box::new(Self::lower_type_expr(
                    elem.as_ref(),
                    tparams,
                    current_package,
                    imports,
                )),
            },
            ast::TypeExpr::TFunc { params, ret_ty } => fir::TypeExpr::TFunc {
                params: params
                    .iter()
                    .map(|param| Self::lower_type_expr(param, tparams, current_package, imports))
                    .collect(),
                ret_ty: Box::new(Self::lower_type_expr(
                    ret_ty.as_ref(),
                    tparams,
                    current_package,
                    imports,
                )),
            },
        }
    }

    fn lower_enum_def(
        &self,
        def: &ast::EnumDef,
        current_package: &str,
        imports: &HashSet<String>,
    ) -> fir::EnumDef {
        let tparams = type_param_set(&def.generics);
        let name = full_def_name(current_package, &def.name.0);
        let variants = def
            .variants
            .iter()
            .map(|(variant_name, tys)| {
                let types = tys
                    .iter()
                    .map(|ty| Self::lower_type_expr(ty, &tparams, current_package, imports))
                    .collect();
                (FirIdent::name(&variant_name.0), types)
            })
            .collect();
        fir::EnumDef {
            attrs: def.attrs.iter().map(|a| a.into()).collect(),
            name: FirIdent::name(&name),
            generics: def.generics.iter().map(|g| FirIdent::name(&g.0)).collect(),
            variants,
        }
    }

    fn lower_struct_def(
        &self,
        def: &ast::StructDef,
        current_package: &str,
        imports: &HashSet<String>,
    ) -> fir::StructDef {
        let tparams = type_param_set(&def.generics);
        let name = full_def_name(current_package, &def.name.0);
        let fields = def
            .fields
            .iter()
            .map(|(field_name, ty)| {
                (
                    FirIdent::name(&field_name.0),
                    Self::lower_type_expr(ty, &tparams, current_package, imports),
                )
            })
            .collect();
        fir::StructDef {
            attrs: def.attrs.iter().map(|a| a.into()).collect(),
            name: FirIdent::name(&name),
            generics: def.generics.iter().map(|g| FirIdent::name(&g.0)).collect(),
            fields,
        }
    }

    fn lower_trait_def(
        &self,
        def: &ast::TraitDef,
        current_package: &str,
        imports: &HashSet<String>,
    ) -> fir::TraitDef {
        let name = full_def_name(current_package, &def.name.0);
        let method_sigs = def
            .method_sigs
            .iter()
            .map(|sig| fir::TraitMethodSignature {
                name: FirIdent::name(&sig.name.0),
                params: sig
                    .params
                    .iter()
                    .map(|ty| {
                        Self::lower_type_expr(ty, &HashSet::new(), current_package, imports)
                    })
                    .collect(),
                ret_ty: Self::lower_type_expr(&sig.ret_ty, &HashSet::new(), current_package, imports),
            })
            .collect();
        fir::TraitDef {
            attrs: def.attrs.iter().map(|a| a.into()).collect(),
            name: FirIdent::name(&name),
            method_sigs,
        }
    }

    fn lower_extern_go(
        &self,
        def: &ast::ExternGo,
        current_package: &str,
        imports: &HashSet<String>,
    ) -> fir::ExternGo {
        let name = full_def_name(current_package, &def.goml_name.0);
        fir::ExternGo {
            attrs: def.attrs.iter().map(|a| a.into()).collect(),
            package_path: def.package_path.clone(),
            go_symbol: def.go_symbol.clone(),
            goml_name: FirIdent::name(&name),
            explicit_go_symbol: def.explicit_go_symbol,
            params: def
                .params
                .iter()
                .map(|(param, ty)| {
                    (
                        FirIdent::name(&param.0),
                        Self::lower_type_expr(ty, &HashSet::new(), current_package, imports),
                    )
                })
                .collect(),
            ret_ty: def
                .ret_ty
                .as_ref()
                .map(|ty| Self::lower_type_expr(ty, &HashSet::new(), current_package, imports)),
        }
    }

    fn lower_extern_type(&self, def: &ast::ExternType, current_package: &str) -> fir::ExternType {
        let name = full_def_name(current_package, &def.goml_name.0);
        fir::ExternType {
            attrs: def.attrs.iter().map(|a| a.into()).collect(),
            goml_name: FirIdent::name(&name),
        }
    }

    fn lower_extern_builtin(
        &self,
        def: &ast::ExternBuiltin,
        current_package: &str,
        imports: &HashSet<String>,
    ) -> fir::ExternBuiltin {
        let name = full_def_name(current_package, &def.name.0);
        fir::ExternBuiltin {
            attrs: def.attrs.iter().map(|a| a.into()).collect(),
            name: FirIdent::name(&name),
            params: def
                .params
                .iter()
                .map(|(param, ty)| {
                    (
                        FirIdent::name(&param.0),
                        Self::lower_type_expr(ty, &HashSet::new(), current_package, imports),
                    )
                })
                .collect(),
            ret_ty: def
                .ret_ty
                .as_ref()
                .map(|ty| Self::lower_type_expr(ty, &HashSet::new(), current_package, imports)),
        }
    }

    fn resolve_closure_param(
        &self,
        param: &ast::ClosureParam,
        env: &mut ResolveLocalEnv,
        ctx: &ResolutionContext,
        fir_table: &mut FirTable,
    ) -> fir::ClosureParam {
        let new_name = self.fresh_name(&param.name.0, fir_table);
        env.add(&param.name, new_name);
        fir::ClosureParam {
            name: new_name,
            ty: param
                .ty
                .as_ref()
                .map(|t| {
                    Self::lower_type_expr(t, &HashSet::new(), ctx.current_package, ctx.imports)
                }),
            astptr: param.astptr,
        }
    }
}

fn type_param_set(params: &[ast::AstIdent]) -> HashSet<String> {
    params.iter().map(|param| param.0.clone()).collect()
}

fn full_def_path(package: &str, name: &str) -> fir::Path {
    if package == "Builtin" || package == "Main" {
        fir::Path::from_ident(name.to_string())
    } else {
        fir::Path::from_idents(vec![package.to_string(), name.to_string()])
    }
}

fn constructor_path(package: &str, enum_name: &str, variant: &str) -> fir::Path {
    if package == "Builtin" || package == "Main" {
        fir::Path::from_idents(vec![enum_name.to_string(), variant.to_string()])
    } else {
        fir::Path::from_idents(vec![
            package.to_string(),
            enum_name.to_string(),
            variant.to_string(),
        ])
    }
}
