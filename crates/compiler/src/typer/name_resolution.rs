use std::collections::{HashMap, HashSet};

use ast::ast;

use crate::builtins;
use crate::hir;
use crate::hir::HirIdent;
use crate::interface;
use crate::intrinsics::CallableBody;
use crate::package_names::{BUILTIN_PACKAGE, is_special_unqualified_package};
use diagnostics::{Diagnostic, Diagnostics, Severity, Stage};
use parser::syntax::MySyntaxNodePtr;

pub type HirTable = hir::HirTable;

#[derive(Default)]
pub struct NameResolution {
    diagnostics: Diagnostics,
}

#[derive(Debug, Clone)]
struct ResolveLocalEnv {
    values: im::Vector<(ast::AstIdent, hir::LocalId)>,
    type_params: HashSet<String>,
}

impl ResolveLocalEnv {
    fn new() -> Self {
        Self {
            values: im::Vector::new(),
            type_params: HashSet::new(),
        }
    }

    fn enter_scope(&self) -> Self {
        self.clone()
    }

    fn add(&mut self, name: &ast::AstIdent, new_name: hir::LocalId) {
        self.values.push_back((name.clone(), new_name));
    }

    fn rfind(&self, key: &ast::AstIdent) -> Option<hir::LocalId> {
        self.values
            .iter()
            .rfind(|(name, _)| name == key)
            .map(|(_, new_name)| *new_name)
    }

    fn set_type_params(&mut self, type_params: HashSet<String>) {
        self.type_params = type_params;
    }

    fn type_params(&self) -> &HashSet<String> {
        &self.type_params
    }
}

struct ResolutionContext<'a> {
    builtin_names: &'a HashMap<String, CallableBody>,
    def_names: &'a HashMap<String, hir::DefId>,
    deps: &'a HashMap<String, interface::PackageInterface>,
    current_package: &'a str,
    imports: &'a HashSet<String>,
    use_aliases: &'a UseAliases,
    constructor_index: &'a ConstructorIndex,
    trait_index: &'a TraitIndex,
}

#[derive(Default)]
struct UseAliases {
    aliases: HashMap<String, Option<Vec<String>>>,
}

fn full_def_name(package: &str, name: &str) -> String {
    if is_special_unqualified_package(package) {
        name.to_string()
    } else {
        format!("{}::{}", package, name)
    }
}

fn package_allowed(package: &str, current_package: &str, imports: &HashSet<String>) -> bool {
    package == current_package || package == BUILTIN_PACKAGE || imports.contains(package)
}

fn internal_package_allowed(package: &str, current_package: &str) -> bool {
    let Some((owner, _)) = package.split_once("::internal::") else {
        return true;
    };
    current_package == owner
        || current_package
            .strip_prefix(owner)
            .is_some_and(|suffix| suffix.starts_with("::"))
}

struct ConstructorIndex {
    enums_by_package: HashMap<String, HashMap<String, HashSet<String>>>,
}

impl ConstructorIndex {
    fn new_with_deps(
        files: &[hir::SourceFileAst],
        deps: &HashMap<String, interface::PackageInterface>,
    ) -> Self {
        let mut index = Self {
            enums_by_package: HashMap::new(),
        };
        index.add_files(files);
        if !files
            .iter()
            .any(|file| file.ast.package.0 == BUILTIN_PACKAGE)
        {
            let builtin_ast = builtins::get_builtin_ast();
            let builtin_file = hir::SourceFileAst::new("<builtin>".into(), builtin_ast);
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

    fn add_interface(&mut self, package: &str, interface: &interface::PackageInterface) {
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

struct TraitIndex {
    traits_by_package: HashMap<String, HashSet<String>>,
}

impl TraitIndex {
    fn new_with_files(files: &[hir::SourceFileAst]) -> Self {
        let mut index = Self {
            traits_by_package: HashMap::new(),
        };
        index.add_files(files);
        if !files
            .iter()
            .any(|file| file.ast.package.0 == BUILTIN_PACKAGE)
        {
            let builtin_ast = builtins::get_builtin_ast();
            let builtin_file = hir::SourceFileAst::new("<builtin>".into(), builtin_ast);
            index.add_files(std::slice::from_ref(&builtin_file));
        }
        index
    }

    fn add_files(&mut self, files: &[hir::SourceFileAst]) {
        for file in files {
            let package = file.ast.package.0.clone();
            let entry = self.traits_by_package.entry(package).or_default();
            for item in &file.ast.toplevels {
                if let ast::Item::TraitDef(def) = item {
                    entry.insert(def.name.0.clone());
                }
            }
        }
    }

    fn has_trait(&self, package: &str, name: &str) -> bool {
        self.traits_by_package
            .get(package)
            .is_some_and(|traits| traits.contains(name))
    }
}

impl ResolutionContext<'_> {
    fn package_allowed(&self, package: &str) -> bool {
        package == self.current_package
            || package == BUILTIN_PACKAGE
            || self.imports.contains(package)
    }
}

fn default_package_alias(
    package: &str,
    deps: &HashMap<String, interface::PackageInterface>,
) -> String {
    deps.get(package)
        .map(|interface| interface.name.as_str())
        .filter(|name| !name.is_empty())
        .unwrap_or_else(|| package.rsplit("::").next().unwrap_or(package))
        .to_string()
}

fn path_segments(path: &ast::Path) -> Vec<String> {
    path.segments()
        .iter()
        .map(|segment| segment.ident.0.clone())
        .collect()
}

fn path_from_segments(segments: Vec<String>) -> hir::Path {
    hir::Path::from_idents(segments)
}

fn qualified_path_from_segments(segments: Vec<String>) -> hir::QualifiedPath {
    if segments.len() <= 1 {
        return hir::QualifiedPath {
            package: None,
            path: path_from_segments(segments),
        };
    }
    hir::QualifiedPath {
        package: Some(hir::PackageName(segments[0].clone())),
        path: path_from_segments(segments[1..].to_vec()),
    }
}

impl NameResolution {
    fn file_imports(
        &mut self,
        file: &hir::SourceFileAst,
        deps: &HashMap<String, interface::PackageInterface>,
        report_internal_error: bool,
    ) -> HashSet<String> {
        let current_package = &file.ast.package.0;
        let mut imports = HashSet::new();
        for use_decl in file.ast.uses.iter() {
            let package = use_decl.path.display();
            if !deps.contains_key(&package) {
                continue;
            }
            if !internal_package_allowed(&package, current_package) {
                if report_internal_error {
                    self.error(format!(
                        "package {} is internal to {}",
                        package,
                        package.split_once("::internal::").unwrap().0
                    ));
                }
                continue;
            }
            imports.insert(package);
        }
        imports
    }

    fn file_use_aliases(
        &mut self,
        file: &ast::File,
        deps: &HashMap<String, interface::PackageInterface>,
        report_conflicts: bool,
    ) -> UseAliases {
        let mut aliases: HashMap<String, Option<Vec<String>>> = HashMap::new();
        let mut imported_packages = HashSet::new();
        for use_decl in file.uses.iter() {
            let target = use_decl.path.display();
            if !deps.contains_key(&target) {
                continue;
            }
            if !internal_package_allowed(&target, &file.package.0) {
                continue;
            }
            if !imported_packages.insert(target.clone()) && report_conflicts {
                self.error(format!("Duplicate package use {}", target));
            }
            let alias = use_decl
                .alias
                .as_ref()
                .map(|alias| alias.0.clone())
                .unwrap_or_else(|| default_package_alias(&target, deps));
            let target = vec![target];
            match aliases.get_mut(&alias) {
                Some(existing) if existing.as_ref().is_some_and(|prev| prev == &target) => {}
                Some(existing) => {
                    if report_conflicts && existing.is_some() {
                        self.error(format!("Ambiguous package use alias {}", alias));
                    }
                    *existing = None;
                }
                None => {
                    aliases.insert(alias, Some(target));
                }
            }
        }
        UseAliases { aliases }
    }

    fn resolve_path_segments_with_aliases(
        &self,
        path: &ast::Path,
        use_aliases: &UseAliases,
    ) -> Vec<String> {
        self.expanded_path_segments(path, use_aliases)
            .unwrap_or_else(|| path_segments(path))
    }

    fn expanded_path_segments(
        &self,
        path: &ast::Path,
        use_aliases: &UseAliases,
    ) -> Option<Vec<String>> {
        let segments = path_segments(path);
        if segments.len() < 2 {
            return None;
        }
        let Some(Some(target)) = use_aliases.aliases.get(&segments[0]) else {
            return None;
        };
        let mut resolved = target.clone();
        resolved.extend(segments[1..].iter().cloned());
        Some(resolved)
    }

    fn resolve_path_segments(&self, path: &ast::Path, ctx: &ResolutionContext) -> Vec<String> {
        self.resolve_path_segments_with_aliases(path, ctx.use_aliases)
    }

    fn resolve_hir_path(&self, path: &ast::Path, ctx: &ResolutionContext) -> hir::Path {
        path_from_segments(self.resolve_path_segments(path, ctx))
    }

    fn resolve_qualified_path(
        &self,
        path: &ast::Path,
        ctx: &ResolutionContext,
    ) -> hir::QualifiedPath {
        qualified_path_from_segments(self.resolve_path_segments(path, ctx))
    }

    fn resolve_qualified_path_with_aliases(
        &self,
        path: &ast::Path,
        use_aliases: &UseAliases,
    ) -> hir::QualifiedPath {
        self.expanded_path_segments(path, use_aliases)
            .map(qualified_path_from_segments)
            .unwrap_or_else(|| path.into())
    }

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
        let segments = self.resolve_path_segments(path, ctx);
        let last = segments.last()?;
        match segments.len() {
            1 => {
                let variant = last;
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
            _ => {
                let variant = last;
                let local_enum = segments[..segments.len() - 1].join("::");
                if ctx
                    .constructor_index
                    .enum_has_variant(ctx.current_package, &local_enum, variant)
                {
                    return Some(constructor_path(ctx.current_package, &local_enum, variant));
                }
                if ctx
                    .constructor_index
                    .enum_has_variant(BUILTIN_PACKAGE, &local_enum, variant)
                {
                    return Some(constructor_path(BUILTIN_PACKAGE, &local_enum, variant));
                }

                if segments.len() >= 3 {
                    let package = segments.first()?;
                    let enum_name = segments[1..segments.len() - 1].join("::");
                    let exists = ctx
                        .constructor_index
                        .enum_has_variant(package, &enum_name, variant);
                    if exists && !ctx.package_allowed(package) {
                        self.error(format!(
                            "package {} not imported in package {}",
                            package, ctx.current_package
                        ));
                        return None;
                    }
                    if exists {
                        return Some(constructor_path(package, &enum_name, variant));
                    }
                }

                None
            }
        }
    }

    fn normalize_constructor_path(
        &mut self,
        path: &ast::Path,
        ctx: &ResolutionContext,
    ) -> hir::Path {
        self.constructor_path_for(path, ctx)
            .unwrap_or_else(|| self.resolve_hir_path(path, ctx))
    }

    pub fn resolve_files_with_env(
        mut self,
        package_id: hir::PackageId,
        files: Vec<hir::SourceFileAst>,
        deps: &HashMap<String, interface::PackageInterface>,
    ) -> (hir::ResolvedHir, HirTable, Diagnostics) {
        let mut hir_table = HirTable::new(package_id);

        let builtin_names = builtins::builtin_callables()
            .iter()
            .map(|(name, body)| (name.clone(), *body))
            .collect::<HashMap<_, _>>();

        let mut def_names = HashMap::new();
        let ctor_index = ConstructorIndex::new_with_deps(&files, deps);
        let trait_index = TraitIndex::new_with_files(&files);
        let mut toplevels = Vec::new();
        let mut per_file_defs = Vec::new();

        for file in files.iter() {
            let package_name = file.ast.package.0.as_str();
            let imports = self.file_imports(file, deps, false);
            let use_aliases = self.file_use_aliases(&file.ast, deps, false);
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
                                predicates: Vec::new(),
                                params: Vec::new(),
                                ret_ty: None,
                                body: hir::Block {
                                    stmts: Vec::new(),
                                    tail: None,
                                },
                            }),
                        );
                        def_names.insert(full_name, id);
                        Some(id)
                    }
                    ast::Item::ExternFn(ext) => {
                        let full_name = full_def_name(package_name, &ext.name.0);
                        let path = full_def_path(package_name, &ext.name.0);
                        let ext_def =
                            self.lower_extern_fn(ext, package_name, &imports, &use_aliases);
                        let id = hir_table.alloc_def_with_path(
                            path,
                            hir::DefKind::ExternFn,
                            hir::Def::ExternFn(ext_def),
                        );
                        def_names.insert(full_name, id);
                        Some(id)
                    }
                    ast::Item::EnumDef(e) => {
                        let full_name = full_def_name(package_name, &e.name.0);
                        let path = full_def_path(package_name, &e.name.0);
                        let enum_def = self.lower_enum_def(e, package_name, &imports, &use_aliases);
                        let id = hir_table.alloc_def_with_path(
                            path,
                            hir::DefKind::EnumDef,
                            hir::Def::EnumDef(enum_def),
                        );
                        def_names.insert(full_name, id);
                        Some(id)
                    }
                    ast::Item::StructDef(s) => {
                        let full_name = full_def_name(package_name, &s.name.0);
                        let path = full_def_path(package_name, &s.name.0);
                        let struct_def =
                            self.lower_struct_def(s, package_name, &imports, &use_aliases);
                        let id = hir_table.alloc_def_with_path(
                            path,
                            hir::DefKind::StructDef,
                            hir::Def::StructDef(struct_def),
                        );
                        def_names.insert(full_name, id);
                        Some(id)
                    }
                    ast::Item::TraitDef(t) => {
                        let full_name = full_def_name(package_name, &t.name.0);
                        let path = full_def_path(package_name, &t.name.0);
                        let trait_def =
                            self.lower_trait_def(t, package_name, &imports, &use_aliases);
                        let id = hir_table.alloc_def_with_path(
                            path,
                            hir::DefKind::TraitDef,
                            hir::Def::TraitDef(trait_def),
                        );
                        def_names.insert(full_name, id);
                        Some(id)
                    }
                    ast::Item::ImplBlock(_i) => Some(hir_table.alloc_def_with_path(
                        full_def_path(package_name, "impl"),
                        hir::DefKind::ImplBlock,
                        hir::Def::ImplBlock(hir::ImplBlock {
                            attrs: Vec::new(),
                            generics: Vec::new(),
                            generic_bounds: Vec::new(),
                            predicates: Vec::new(),
                            associated_types: Vec::new(),
                            trait_ref: None,
                            for_type: hir::TypeExpr::TUnit,
                            methods: Vec::new(),
                        }),
                    )),
                };
                if let Some(def_id) = def_id {
                    toplevels.push(def_id);
                    def_ids.push(def_id);
                }
            }
            per_file_defs.push(def_ids);
        }

        for (file_idx, file) in files.iter().enumerate() {
            let package_name = file.ast.package.0.as_str();
            let imports = self.file_imports(file, deps, true);
            let use_aliases = self.file_use_aliases(&file.ast, deps, true);
            let ctx = ResolutionContext {
                builtin_names: &builtin_names,
                def_names: &def_names,
                deps,
                current_package: package_name,
                imports: &imports,
                use_aliases: &use_aliases,
                constructor_index: &ctor_index,
                trait_index: &trait_index,
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
                        let resolved_fn =
                            self.resolve_fn(func, &ctx, &mut hir_table, full_name, &HashSet::new());
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
                        let tparams = type_param_set(&i.generics);
                        let methods = i
                            .methods
                            .iter()
                            .map(|m| self.resolve_fn_def(m, &ctx, &mut hir_table, &tparams))
                            .collect();
                        let generic_bounds = i
                            .generic_bounds
                            .iter()
                            .map(|(param, traits)| {
                                let traits = traits
                                    .iter()
                                    .map(|trait_ref| {
                                        self.lower_trait_ref(trait_ref, &tparams, &ctx)
                                    })
                                    .collect::<Vec<_>>();
                                (HirIdent::name(&param.0), traits)
                            })
                            .collect();
                        let trait_ref = i
                            .trait_ref
                            .as_ref()
                            .map(|trait_ref| self.lower_trait_ref(trait_ref, &tparams, &ctx));
                        let impl_block = hir::ImplBlock {
                            attrs: i.attrs.iter().map(|a| a.into()).collect(),
                            generics: i.generics.iter().map(|g| HirIdent::name(&g.0)).collect(),
                            generic_bounds,
                            predicates: i
                                .predicates
                                .iter()
                                .map(|predicate| self.lower_predicate(predicate, &tparams, &ctx))
                                .collect(),
                            associated_types: i
                                .associated_types
                                .iter()
                                .map(|(name, ty)| {
                                    (
                                        HirIdent::name(&name.0),
                                        self.lower_type_expr(
                                            ty,
                                            &tparams,
                                            package_name,
                                            &imports,
                                            &use_aliases,
                                        ),
                                    )
                                })
                                .collect(),
                            trait_ref,
                            for_type: self.lower_type_expr(
                                &i.for_type,
                                &tparams,
                                package_name,
                                &imports,
                                &use_aliases,
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
                let path = if is_special_unqualified_package(&package) {
                    file_name.to_string()
                } else {
                    format!("{}/{}", package, file_name)
                };
                let imports = self.file_imports(file, deps, false);
                let use_aliases = self.file_use_aliases(&file.ast, deps, false);
                let mut imports_vec = imports
                    .into_iter()
                    .map(hir::PackageName)
                    .collect::<Vec<_>>();
                imports_vec.sort_by(|a, b| a.0.cmp(&b.0));

                let mut use_traits = Vec::new();
                for use_decl in file.ast.uses.iter() {
                    if deps.contains_key(&use_decl.path.display()) {
                        continue;
                    }
                    if use_decl.alias.is_some() {
                        self.error(format!(
                            "trait use {} cannot declare a package alias",
                            use_decl.path.display()
                        ));
                    }
                    let Some(segments) = self.expanded_path_segments(&use_decl.path, &use_aliases)
                    else {
                        self.error(format!(
                            "use path {} must begin with an imported package alias",
                            use_decl.path.display()
                        ));
                        continue;
                    };
                    let qualified = qualified_path_from_segments(segments);
                    let Some(imported_package) = &qualified.package else {
                        self.ice("use trait is missing package");
                        continue;
                    };
                    if !imports_vec.contains(imported_package) {
                        self.error(format!(
                            "package {} not imported in package {}",
                            imported_package.0, package
                        ));
                        continue;
                    }
                    if qualified.last_ident().is_none() {
                        self.ice("use trait is missing name");
                        continue;
                    }
                    use_traits.push(qualified);
                }
                hir::SourceFileHir {
                    path,
                    package: hir::PackageName(package),
                    imports: imports_vec,
                    use_traits,
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
        inherited_tparams: &HashSet<String>,
    ) -> hir::DefId {
        let def_id = hir_table.alloc_def(
            func.name.0.clone(),
            hir::DefKind::Fn,
            hir::Def::Fn(hir::Fn {
                attrs: Vec::new(),
                name: func.name.0.clone(),
                generics: Vec::new(),
                generic_bounds: Vec::new(),
                predicates: Vec::new(),
                params: Vec::new(),
                ret_ty: None,
                body: hir::Block {
                    stmts: Vec::new(),
                    tail: None,
                },
            }),
        );
        hir_table.set_current_owner(def_id);
        let func = self.resolve_fn(func, ctx, hir_table, func.name.0.clone(), inherited_tparams);
        *hir_table.def_mut(def_id) = hir::Def::Fn(func);
        def_id
    }

    fn resolve_fn(
        &mut self,
        func: &ast::Fn,
        ctx: &ResolutionContext,
        hir_table: &mut HirTable,
        resolved_name: String,
        inherited_tparams: &HashSet<String>,
    ) -> hir::Fn {
        let ast::Fn {
            attrs,
            generics,
            generic_bounds,
            predicates,
            params,
            ret_ty,
            body,
            ..
        } = func;
        let mut env = ResolveLocalEnv::new();
        for param in params {
            env.add(&param.0, self.fresh_name(&param.0.0, hir_table));
        }
        let mut tparams = type_param_set(generics);
        tparams.extend(inherited_tparams.iter().cloned());
        env.set_type_params(tparams.clone());
        let new_params = params
            .iter()
            .map(|param| {
                let local_id = env.rfind(&param.0).unwrap_or_else(|| {
                    self.ice(format!("missing local id for param {}", param.0.0));
                    self.fresh_name(&param.0.0, hir_table)
                });
                (
                    local_id,
                    self.lower_type_expr(
                        &param.1,
                        &tparams,
                        ctx.current_package,
                        ctx.imports,
                        ctx.use_aliases,
                    ),
                )
            })
            .collect();

        let new_generic_bounds = generic_bounds
            .iter()
            .map(|(param, traits)| {
                let traits = traits
                    .iter()
                    .map(|trait_ref| self.lower_trait_ref(trait_ref, &tparams, ctx))
                    .collect::<Vec<_>>();
                (HirIdent::name(&param.0), traits)
            })
            .collect();
        hir::Fn {
            attrs: attrs.iter().map(|a| a.into()).collect(),
            name: resolved_name,
            generics: generics.iter().map(|g| HirIdent::name(&g.0)).collect(),
            generic_bounds: new_generic_bounds,
            predicates: predicates
                .iter()
                .map(|predicate| self.lower_predicate(predicate, &tparams, ctx))
                .collect(),
            params: new_params,
            ret_ty: ret_ty.as_ref().map(|t| {
                self.lower_type_expr(
                    t,
                    &tparams,
                    ctx.current_package,
                    ctx.imports,
                    ctx.use_aliases,
                )
            }),
            body: self.resolve_block(body, &mut env, ctx, hir_table),
        }
    }

    fn alloc_expr_with_ptr(
        &mut self,
        hir_table: &mut HirTable,
        astptr: MySyntaxNodePtr,
        expr: hir::Expr,
    ) -> hir::ExprId {
        let id = hir_table.alloc_expr(expr);
        hir_table.set_expr_ptr(id, Some(astptr));
        id
    }

    fn alloc_pat_with_ptr(
        &mut self,
        hir_table: &mut HirTable,
        astptr: MySyntaxNodePtr,
        pat: hir::Pat,
    ) -> hir::PatId {
        let id = hir_table.alloc_pat(pat);
        hir_table.set_pat_ptr(id, Some(astptr));
        id
    }

    fn resolve_let_stmt(
        &mut self,
        stmt: &ast::LetStmt,
        env: &mut ResolveLocalEnv,
        ctx: &ResolutionContext,
        hir_table: &mut HirTable,
    ) -> hir::LetStmt {
        let new_value = self.resolve_expr(&stmt.value, env, ctx, hir_table);
        let new_pat = self.resolve_pat(&stmt.pat, env, ctx, hir_table);
        if stmt.is_mut {
            if let hir::Pat::PVar { name, .. } = hir_table.pat(new_pat) {
                hir_table.set_local_mutable(*name, true);
            } else {
                self.error("`mut` is only supported on simple variable bindings");
            }
        }
        hir::LetStmt {
            is_mut: stmt.is_mut,
            pat: new_pat,
            annotation: stmt.annotation.as_ref().map(|t| {
                self.lower_type_expr(
                    t,
                    env.type_params(),
                    ctx.current_package,
                    ctx.imports,
                    ctx.use_aliases,
                )
            }),
            value: new_value,
        }
    }

    fn resolve_assign_stmt(
        &mut self,
        stmt: &ast::AssignStmt,
        env: &mut ResolveLocalEnv,
        ctx: &ResolutionContext,
        hir_table: &mut HirTable,
    ) -> hir::AssignStmt {
        let target = self.resolve_expr(&stmt.target, env, ctx, hir_table);
        let value = self.resolve_expr(&stmt.value, env, ctx, hir_table);
        hir::AssignStmt { target, value }
    }

    fn resolve_block(
        &mut self,
        block: &ast::Block,
        env: &mut ResolveLocalEnv,
        ctx: &ResolutionContext,
        hir_table: &mut HirTable,
    ) -> hir::Block {
        let mut stmts = Vec::new();
        for stmt in &block.stmts {
            match stmt {
                ast::Stmt::Let(stmt) => {
                    stmts.push(hir::Stmt::Let(
                        self.resolve_let_stmt(stmt, env, ctx, hir_table),
                    ));
                }
                ast::Stmt::Assign(stmt) => {
                    stmts.push(hir::Stmt::Assign(
                        self.resolve_assign_stmt(stmt, env, ctx, hir_table),
                    ));
                }
                ast::Stmt::Expr(stmt) => {
                    stmts.push(hir::Stmt::Expr(hir::ExprStmt {
                        expr: self.resolve_expr(&stmt.expr, env, ctx, hir_table),
                    }));
                }
            }
        }
        let tail = block
            .tail
            .as_ref()
            .map(|tail| self.resolve_expr(tail, env, ctx, hir_table));
        hir::Block { stmts, tail }
    }

    fn resolve_expr(
        &mut self,
        expr: &ast::Expr,
        env: &mut ResolveLocalEnv,
        ctx: &ResolutionContext,
        hir_table: &mut HirTable,
    ) -> hir::ExprId {
        match expr {
            ast::Expr::EPath {
                path,
                type_args,
                astptr,
            } => {
                if !type_args.is_empty() {
                    let resolved_path = self.resolve_hir_path(path, ctx);
                    let resolved_type_args = type_args
                        .iter()
                        .map(|arg| {
                            self.lower_type_expr(
                                arg,
                                env.type_params(),
                                ctx.current_package,
                                ctx.imports,
                                ctx.use_aliases,
                            )
                        })
                        .collect();
                    return self.alloc_expr_with_ptr(
                        hir_table,
                        *astptr,
                        hir::Expr::EStaticMember {
                            path: resolved_path,
                            type_args: resolved_type_args,
                            astptr: Some(*astptr),
                        },
                    );
                }
                if let Some(constructor) = self.constructor_path_for(path, ctx) {
                    return self.alloc_expr_with_ptr(
                        hir_table,
                        *astptr,
                        hir::Expr::EConstr {
                            constructor: hir::ConstructorRef::Unresolved(constructor),
                            args: Vec::new(),
                        },
                    );
                }
                if path.len() == 1 {
                    let Some(ident) = path.last_ident() else {
                        self.ice("path length 1 missing last ident");
                        return self.alloc_expr_with_ptr(
                            hir_table,
                            *astptr,
                            hir::Expr::ENameRef {
                                res: hir::NameRef::Unresolved(hir::Path::from_ident(
                                    "<error>".to_string(),
                                )),
                                hint: "<error>".to_string(),
                                astptr: Some(*astptr),
                            },
                        );
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
                    self.alloc_expr_with_ptr(
                        hir_table,
                        *astptr,
                        hir::Expr::ENameRef {
                            res,
                            hint,
                            astptr: Some(*astptr),
                        },
                    )
                } else {
                    let resolved_path = self.resolve_hir_path(path, ctx);
                    let full_name = resolved_path.display();
                    let package = resolved_path
                        .segments()
                        .first()
                        .map(|seg| seg.seg().as_str())
                        .unwrap_or_default();
                    if package != ctx.current_package
                        && package != BUILTIN_PACKAGE
                        && ctx.deps.contains_key(package)
                        && !ctx.imports.contains(package)
                    {
                        self.error(format!(
                            "package {} not imported in package {}",
                            package, ctx.current_package
                        ));
                    }

                    let res = if package == ctx.current_package || package == BUILTIN_PACKAGE {
                        ctx.def_names
                            .get(&full_name)
                            .copied()
                            .map(hir::NameRef::Def)
                            .unwrap_or_else(|| hir::NameRef::Unresolved(resolved_path.clone()))
                    } else if ctx.imports.contains(package) {
                        ctx.deps
                            .get(package)
                            .and_then(|pkg_interface| pkg_interface.value_exports.get(&full_name))
                            .copied()
                            .map(|idx| {
                                hir::NameRef::Def(hir::DefId {
                                    pkg: interface::package_id_for_name(package),
                                    idx,
                                })
                            })
                            .unwrap_or_else(|| hir::NameRef::Unresolved(resolved_path.clone()))
                    } else {
                        hir::NameRef::Unresolved(resolved_path.clone())
                    };
                    match res {
                        hir::NameRef::Def(_) => self.alloc_expr_with_ptr(
                            hir_table,
                            *astptr,
                            hir::Expr::ENameRef {
                                res,
                                hint: full_name,
                                astptr: Some(*astptr),
                            },
                        ),
                        hir::NameRef::Unresolved(_)
                            if resolved_path.len() == 2
                                && (package == ctx.current_package
                                    || package == BUILTIN_PACKAGE
                                    || ctx.deps.contains_key(package)) =>
                        {
                            self.alloc_expr_with_ptr(
                                hir_table,
                                *astptr,
                                hir::Expr::ENameRef {
                                    res,
                                    hint: full_name,
                                    astptr: Some(*astptr),
                                },
                            )
                        }
                        hir::NameRef::Unresolved(_) => self.alloc_expr_with_ptr(
                            hir_table,
                            *astptr,
                            hir::Expr::EStaticMember {
                                path: resolved_path,
                                type_args: Vec::new(),
                                astptr: Some(*astptr),
                            },
                        ),
                        _ => self.alloc_expr_with_ptr(
                            hir_table,
                            *astptr,
                            hir::Expr::ENameRef {
                                res,
                                hint: full_name,
                                astptr: Some(*astptr),
                            },
                        ),
                    }
                }
            }
            ast::Expr::EUnit { astptr } => {
                self.alloc_expr_with_ptr(hir_table, *astptr, hir::Expr::EUnit)
            }
            ast::Expr::EBool { value, astptr } => {
                self.alloc_expr_with_ptr(hir_table, *astptr, hir::Expr::EBool { value: *value })
            }
            ast::Expr::EInt { value, astptr } => self.alloc_expr_with_ptr(
                hir_table,
                *astptr,
                hir::Expr::EInt {
                    value: value.clone(),
                },
            ),
            ast::Expr::EInt8 { value, astptr } => self.alloc_expr_with_ptr(
                hir_table,
                *astptr,
                hir::Expr::EInt8 {
                    value: value.clone(),
                },
            ),
            ast::Expr::EInt16 { value, astptr } => self.alloc_expr_with_ptr(
                hir_table,
                *astptr,
                hir::Expr::EInt16 {
                    value: value.clone(),
                },
            ),
            ast::Expr::EInt32 { value, astptr } => self.alloc_expr_with_ptr(
                hir_table,
                *astptr,
                hir::Expr::EInt32 {
                    value: value.clone(),
                },
            ),
            ast::Expr::EInt64 { value, astptr } => self.alloc_expr_with_ptr(
                hir_table,
                *astptr,
                hir::Expr::EInt64 {
                    value: value.clone(),
                },
            ),
            ast::Expr::EUInt8 { value, astptr } => self.alloc_expr_with_ptr(
                hir_table,
                *astptr,
                hir::Expr::EUInt8 {
                    value: value.clone(),
                },
            ),
            ast::Expr::EUInt16 { value, astptr } => self.alloc_expr_with_ptr(
                hir_table,
                *astptr,
                hir::Expr::EUInt16 {
                    value: value.clone(),
                },
            ),
            ast::Expr::EUInt32 { value, astptr } => self.alloc_expr_with_ptr(
                hir_table,
                *astptr,
                hir::Expr::EUInt32 {
                    value: value.clone(),
                },
            ),
            ast::Expr::EUInt64 { value, astptr } => self.alloc_expr_with_ptr(
                hir_table,
                *astptr,
                hir::Expr::EUInt64 {
                    value: value.clone(),
                },
            ),
            ast::Expr::EFloat { value, astptr } => {
                self.alloc_expr_with_ptr(hir_table, *astptr, hir::Expr::EFloat { value: *value })
            }
            ast::Expr::EFloat32 { value, astptr } => self.alloc_expr_with_ptr(
                hir_table,
                *astptr,
                hir::Expr::EFloat32 {
                    value: value.clone(),
                },
            ),
            ast::Expr::EFloat64 { value, astptr } => self.alloc_expr_with_ptr(
                hir_table,
                *astptr,
                hir::Expr::EFloat64 {
                    value: value.clone(),
                },
            ),
            ast::Expr::EString { value, astptr } => self.alloc_expr_with_ptr(
                hir_table,
                *astptr,
                hir::Expr::EString {
                    value: value.clone(),
                },
            ),
            ast::Expr::EChar { value, astptr } => self.alloc_expr_with_ptr(
                hir_table,
                *astptr,
                hir::Expr::EChar {
                    value: value.clone(),
                },
            ),
            ast::Expr::EConstr {
                constructor,
                args,
                astptr,
            } => {
                let new_args = args
                    .iter()
                    .map(|arg| self.resolve_expr(arg, env, ctx, hir_table))
                    .collect();
                let constructor = self.normalize_constructor_path(constructor, ctx);
                self.alloc_expr_with_ptr(
                    hir_table,
                    *astptr,
                    hir::Expr::EConstr {
                        constructor: hir::ConstructorRef::Unresolved(constructor),
                        args: new_args,
                    },
                )
            }
            ast::Expr::EStructLiteral {
                name,
                fields,
                astptr,
            } => {
                let new_fields = fields
                    .iter()
                    .map(|(field_name, expr)| {
                        (
                            HirIdent::name(&field_name.0),
                            self.resolve_expr(expr, env, ctx, hir_table),
                        )
                    })
                    .collect();
                let qualified = self.resolve_qualified_path(name, ctx);
                if let Some(package) = &qualified.package
                    && !ctx.package_allowed(package.as_str())
                {
                    self.error(format!(
                        "package {} not imported in package {}",
                        package.0, ctx.current_package
                    ));
                }
                self.alloc_expr_with_ptr(
                    hir_table,
                    *astptr,
                    hir::Expr::EStructLiteral {
                        name: qualified,
                        fields: new_fields,
                    },
                )
            }
            ast::Expr::ETuple { items, astptr } => {
                let new_items = items
                    .iter()
                    .map(|item| self.resolve_expr(item, env, ctx, hir_table))
                    .collect();
                self.alloc_expr_with_ptr(hir_table, *astptr, hir::Expr::ETuple { items: new_items })
            }
            ast::Expr::EArray { items, astptr } => {
                let new_items = items
                    .iter()
                    .map(|item| self.resolve_expr(item, env, ctx, hir_table))
                    .collect();
                self.alloc_expr_with_ptr(hir_table, *astptr, hir::Expr::EArray { items: new_items })
            }
            ast::Expr::EClosure {
                params,
                body,
                astptr,
            } => {
                let mut closure_env = env.enter_scope();
                let new_params = params
                    .iter()
                    .map(|param| {
                        self.resolve_closure_param(param, &mut closure_env, ctx, hir_table)
                    })
                    .collect();
                let new_body_expr = self.resolve_expr(body, &mut closure_env, ctx, hir_table);

                self.alloc_expr_with_ptr(
                    hir_table,
                    *astptr,
                    hir::Expr::EClosure {
                        params: new_params,
                        body: new_body_expr,
                    },
                )
            }
            ast::Expr::EMatch { expr, arms, astptr } => {
                let new_expr = self.resolve_expr(expr, env, ctx, hir_table);
                let new_arms = arms
                    .iter()
                    .map(|arm| {
                        let mut arm_env = env.enter_scope();
                        let new_pat = self.resolve_pat(&arm.pat, &mut arm_env, ctx, hir_table);
                        let new_body = self.resolve_expr(&arm.body, &mut arm_env, ctx, hir_table);
                        hir::Arm {
                            pat: new_pat,
                            body: new_body,
                        }
                    })
                    .collect();
                self.alloc_expr_with_ptr(
                    hir_table,
                    *astptr,
                    hir::Expr::EMatch {
                        expr: new_expr,
                        arms: new_arms,
                    },
                )
            }
            ast::Expr::EIf {
                cond,
                then_branch,
                else_branch,
                astptr,
            } => {
                let new_cond = self.resolve_expr(cond, env, ctx, hir_table);
                let mut then_env = env.enter_scope();
                let new_then = self.resolve_expr(then_branch, &mut then_env, ctx, hir_table);
                let mut else_env = env.enter_scope();
                let new_else = self.resolve_expr(else_branch, &mut else_env, ctx, hir_table);
                self.alloc_expr_with_ptr(
                    hir_table,
                    *astptr,
                    hir::Expr::EIf {
                        cond: new_cond,
                        then_branch: new_then,
                        else_branch: new_else,
                    },
                )
            }
            ast::Expr::EWhile { cond, body, astptr } => {
                let new_cond = self.resolve_expr(cond, env, ctx, hir_table);
                let mut body_env = env.enter_scope();
                let new_body = self.resolve_expr(body, &mut body_env, ctx, hir_table);
                self.alloc_expr_with_ptr(
                    hir_table,
                    *astptr,
                    hir::Expr::EWhile {
                        cond: new_cond,
                        body: new_body,
                    },
                )
            }
            ast::Expr::EFor {
                pat,
                iterator,
                body,
                astptr,
            } => {
                let new_iterator = self.resolve_expr(iterator, env, ctx, hir_table);
                let mut body_env = env.enter_scope();
                let new_pat = self.resolve_pat(pat, &mut body_env, ctx, hir_table);
                let new_body = self.resolve_expr(body, &mut body_env, ctx, hir_table);
                self.alloc_expr_with_ptr(
                    hir_table,
                    *astptr,
                    hir::Expr::EFor {
                        pat: new_pat,
                        iterator: new_iterator,
                        body: new_body,
                    },
                )
            }
            ast::Expr::EGo { expr, astptr } => {
                let new_expr = self.resolve_expr(expr, env, ctx, hir_table);
                self.alloc_expr_with_ptr(hir_table, *astptr, hir::Expr::EGo { expr: new_expr })
            }
            ast::Expr::EBreak { astptr } => {
                self.alloc_expr_with_ptr(hir_table, *astptr, hir::Expr::EBreak)
            }
            ast::Expr::EContinue { astptr } => {
                self.alloc_expr_with_ptr(hir_table, *astptr, hir::Expr::EContinue)
            }
            ast::Expr::EReturn { expr, astptr } => {
                let expr = expr
                    .as_ref()
                    .map(|expr| self.resolve_expr(expr, env, ctx, hir_table));
                self.alloc_expr_with_ptr(hir_table, *astptr, hir::Expr::EReturn { expr })
            }
            ast::Expr::ECall { func, args, astptr } => {
                if let ast::Expr::EPath { path, .. } = func.as_ref()
                    && let Some(constructor) = self.constructor_path_for(path, ctx)
                {
                    let new_args = args
                        .iter()
                        .map(|arg| self.resolve_expr(arg, env, ctx, hir_table))
                        .collect();
                    return self.alloc_expr_with_ptr(
                        hir_table,
                        *astptr,
                        hir::Expr::EConstr {
                            constructor: hir::ConstructorRef::Unresolved(constructor),
                            args: new_args,
                        },
                    );
                }
                let new_func = self.resolve_expr(func, env, ctx, hir_table);
                let new_args = args
                    .iter()
                    .map(|arg| self.resolve_expr(arg, env, ctx, hir_table))
                    .collect();
                self.alloc_expr_with_ptr(
                    hir_table,
                    *astptr,
                    hir::Expr::ECall {
                        func: new_func,
                        args: new_args,
                    },
                )
            }
            ast::Expr::EUnary { op, expr, astptr } => {
                let new_expr = self.resolve_expr(expr, env, ctx, hir_table);
                self.alloc_expr_with_ptr(
                    hir_table,
                    *astptr,
                    hir::Expr::EUnary {
                        op: *op,
                        expr: new_expr,
                    },
                )
            }
            ast::Expr::ETry { expr, astptr } => {
                let new_expr = self.resolve_expr(expr, env, ctx, hir_table);
                self.alloc_expr_with_ptr(hir_table, *astptr, hir::Expr::ETry { expr: new_expr })
            }
            ast::Expr::EBinary {
                op,
                lhs,
                rhs,
                astptr,
            } => {
                let new_lhs = self.resolve_expr(lhs, env, ctx, hir_table);
                let new_rhs = self.resolve_expr(rhs, env, ctx, hir_table);
                self.alloc_expr_with_ptr(
                    hir_table,
                    *astptr,
                    hir::Expr::EBinary {
                        op: *op,
                        lhs: new_lhs,
                        rhs: new_rhs,
                    },
                )
            }
            ast::Expr::EProj {
                tuple,
                index,
                astptr,
            } => {
                let new_tuple = self.resolve_expr(tuple, env, ctx, hir_table);
                self.alloc_expr_with_ptr(
                    hir_table,
                    *astptr,
                    hir::Expr::EProj {
                        tuple: new_tuple,
                        index: *index,
                    },
                )
            }
            ast::Expr::EField {
                expr,
                field,
                astptr,
            } => {
                let new_expr = self.resolve_expr(expr, env, ctx, hir_table);
                self.alloc_expr_with_ptr(
                    hir_table,
                    *astptr,
                    hir::Expr::EField {
                        expr: new_expr,
                        field: HirIdent::name(&field.0),
                    },
                )
            }
            ast::Expr::EIndex {
                base,
                index,
                astptr,
            } => {
                let base = self.resolve_expr(base, env, ctx, hir_table);
                let index = self.resolve_expr(index, env, ctx, hir_table);
                self.alloc_expr_with_ptr(hir_table, *astptr, hir::Expr::EIndex { base, index })
            }
            ast::Expr::EBlock { block, astptr } => {
                let block = self.resolve_block(block, env, ctx, hir_table);
                self.alloc_expr_with_ptr(hir_table, *astptr, hir::Expr::EBlock { block })
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
                self.alloc_pat_with_ptr(
                    hir_table,
                    *astptr,
                    hir::Pat::PVar {
                        name: newname,
                        astptr: *astptr,
                    },
                )
            }
            ast::Pat::PUnit { astptr } => {
                self.alloc_pat_with_ptr(hir_table, *astptr, hir::Pat::PUnit)
            }
            ast::Pat::PBool { value, astptr } => {
                self.alloc_pat_with_ptr(hir_table, *astptr, hir::Pat::PBool { value: *value })
            }
            ast::Pat::PInt { value, astptr } => self.alloc_pat_with_ptr(
                hir_table,
                *astptr,
                hir::Pat::PInt {
                    value: value.clone(),
                },
            ),
            ast::Pat::PInt8 { value, astptr } => self.alloc_pat_with_ptr(
                hir_table,
                *astptr,
                hir::Pat::PInt8 {
                    value: value.clone(),
                },
            ),
            ast::Pat::PInt16 { value, astptr } => self.alloc_pat_with_ptr(
                hir_table,
                *astptr,
                hir::Pat::PInt16 {
                    value: value.clone(),
                },
            ),
            ast::Pat::PInt32 { value, astptr } => self.alloc_pat_with_ptr(
                hir_table,
                *astptr,
                hir::Pat::PInt32 {
                    value: value.clone(),
                },
            ),
            ast::Pat::PInt64 { value, astptr } => self.alloc_pat_with_ptr(
                hir_table,
                *astptr,
                hir::Pat::PInt64 {
                    value: value.clone(),
                },
            ),
            ast::Pat::PUInt8 { value, astptr } => self.alloc_pat_with_ptr(
                hir_table,
                *astptr,
                hir::Pat::PUInt8 {
                    value: value.clone(),
                },
            ),
            ast::Pat::PUInt16 { value, astptr } => self.alloc_pat_with_ptr(
                hir_table,
                *astptr,
                hir::Pat::PUInt16 {
                    value: value.clone(),
                },
            ),
            ast::Pat::PUInt32 { value, astptr } => self.alloc_pat_with_ptr(
                hir_table,
                *astptr,
                hir::Pat::PUInt32 {
                    value: value.clone(),
                },
            ),
            ast::Pat::PUInt64 { value, astptr } => self.alloc_pat_with_ptr(
                hir_table,
                *astptr,
                hir::Pat::PUInt64 {
                    value: value.clone(),
                },
            ),
            ast::Pat::PFloat { value, astptr } => self.alloc_pat_with_ptr(
                hir_table,
                *astptr,
                hir::Pat::PFloat {
                    value: value.clone(),
                },
            ),
            ast::Pat::PFloat32 { value, astptr } => self.alloc_pat_with_ptr(
                hir_table,
                *astptr,
                hir::Pat::PFloat32 {
                    value: value.clone(),
                },
            ),
            ast::Pat::PFloat64 { value, astptr } => self.alloc_pat_with_ptr(
                hir_table,
                *astptr,
                hir::Pat::PFloat64 {
                    value: value.clone(),
                },
            ),
            ast::Pat::PString { value, astptr } => self.alloc_pat_with_ptr(
                hir_table,
                *astptr,
                hir::Pat::PString {
                    value: value.clone(),
                },
            ),
            ast::Pat::PChar { value, astptr } => self.alloc_pat_with_ptr(
                hir_table,
                *astptr,
                hir::Pat::PChar {
                    value: value.clone(),
                },
            ),
            ast::Pat::PConstr {
                constructor,
                args,
                astptr,
            } => {
                let new_args = args
                    .iter()
                    .map(|arg| self.resolve_pat(arg, env, ctx, hir_table))
                    .collect();
                let constructor = self.normalize_constructor_path(constructor, ctx);
                self.alloc_pat_with_ptr(
                    hir_table,
                    *astptr,
                    hir::Pat::PConstr {
                        constructor: hir::ConstructorRef::Unresolved(constructor),
                        args: new_args,
                    },
                )
            }
            ast::Pat::PStruct {
                name,
                fields,
                astptr,
            } => {
                let new_fields = fields
                    .iter()
                    .map(|(fname, pat)| {
                        (
                            HirIdent::name(&fname.0),
                            self.resolve_pat(pat, env, ctx, hir_table),
                        )
                    })
                    .collect();
                let qualified = self.resolve_qualified_path(name, ctx);
                if let Some(package) = &qualified.package
                    && !ctx.package_allowed(package.as_str())
                {
                    self.error(format!(
                        "package {} not imported in package {}",
                        package.0, ctx.current_package
                    ));
                }
                self.alloc_pat_with_ptr(
                    hir_table,
                    *astptr,
                    hir::Pat::PStruct {
                        name: qualified,
                        fields: new_fields,
                    },
                )
            }
            ast::Pat::PTuple { pats, astptr } => {
                let new_pats = pats
                    .iter()
                    .map(|pat| self.resolve_pat(pat, env, ctx, hir_table))
                    .collect();
                self.alloc_pat_with_ptr(hir_table, *astptr, hir::Pat::PTuple { pats: new_pats })
            }
            ast::Pat::PWild { astptr } => {
                self.alloc_pat_with_ptr(hir_table, *astptr, hir::Pat::PWild)
            }
        }
    }

    fn lower_type_expr(
        &mut self,
        ty: &ast::TypeExpr,
        _tparams: &HashSet<String>,
        current_package: &str,
        imports: &HashSet<String>,
        use_aliases: &UseAliases,
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
            ast::TypeExpr::TChar => hir::TypeExpr::TChar,
            ast::TypeExpr::TTuple { typs } => hir::TypeExpr::TTuple {
                typs: typs
                    .iter()
                    .map(|ty| {
                        self.lower_type_expr(ty, _tparams, current_package, imports, use_aliases)
                    })
                    .collect(),
            },
            ast::TypeExpr::TCon { path } => {
                if path.len() == 2
                    && let (Some(base), Some(assoc)) =
                        (path.segments().first(), path.segments().get(1))
                    && (base.ident.0 == "Self" || _tparams.contains(&base.ident.0))
                {
                    return hir::TypeExpr::TProjection {
                        base: Box::new(hir::TypeExpr::TCon {
                            path: hir::QualifiedPath {
                                package: None,
                                path: hir::Path::new(vec![base.into()]),
                            },
                        }),
                        assoc: HirIdent::name(&assoc.ident.0),
                    };
                }
                let qualified = self.resolve_qualified_path_with_aliases(path, use_aliases);
                if let Some(package) = &qualified.package
                    && !package_allowed(package.as_str(), current_package, imports)
                {
                    self.error(format!(
                        "package {} not imported in package {}",
                        package.0, current_package
                    ));
                }
                hir::TypeExpr::TCon { path: qualified }
            }
            ast::TypeExpr::TDyn { trait_path } => {
                let qualified = self.resolve_qualified_path_with_aliases(trait_path, use_aliases);
                if let Some(package) = &qualified.package
                    && !package_allowed(package.as_str(), current_package, imports)
                {
                    self.error(format!(
                        "package {} not imported in package {}",
                        package.0, current_package
                    ));
                }
                hir::TypeExpr::TDyn {
                    trait_path: qualified,
                }
            }
            ast::TypeExpr::TApp { ty, args } => hir::TypeExpr::TApp {
                ty: Box::new(self.lower_type_expr(
                    ty.as_ref(),
                    _tparams,
                    current_package,
                    imports,
                    use_aliases,
                )),
                args: args
                    .iter()
                    .map(|arg| {
                        self.lower_type_expr(arg, _tparams, current_package, imports, use_aliases)
                    })
                    .collect(),
            },
            ast::TypeExpr::TArray { len, elem } => hir::TypeExpr::TArray {
                len: *len,
                elem: Box::new(self.lower_type_expr(
                    elem.as_ref(),
                    _tparams,
                    current_package,
                    imports,
                    use_aliases,
                )),
            },
            ast::TypeExpr::TFunc { params, ret_ty } => hir::TypeExpr::TFunc {
                params: params
                    .iter()
                    .map(|param| {
                        self.lower_type_expr(param, _tparams, current_package, imports, use_aliases)
                    })
                    .collect(),
                ret_ty: Box::new(self.lower_type_expr(
                    ret_ty.as_ref(),
                    _tparams,
                    current_package,
                    imports,
                    use_aliases,
                )),
            },
        }
    }

    fn lower_enum_def(
        &mut self,
        def: &ast::EnumDef,
        current_package: &str,
        imports: &HashSet<String>,
        use_aliases: &UseAliases,
    ) -> hir::EnumDef {
        let tparams = type_param_set(&def.generics);
        let name = full_def_name(current_package, &def.name.0);
        let variants = def
            .variants
            .iter()
            .map(|(variant_name, tys)| {
                let types = tys
                    .iter()
                    .map(|ty| {
                        self.lower_type_expr(ty, &tparams, current_package, imports, use_aliases)
                    })
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
        use_aliases: &UseAliases,
    ) -> hir::StructDef {
        let tparams = type_param_set(&def.generics);
        let name = full_def_name(current_package, &def.name.0);
        let fields = def
            .fields
            .iter()
            .map(|(field_name, ty)| {
                (
                    HirIdent::name(&field_name.0),
                    self.lower_type_expr(ty, &tparams, current_package, imports, use_aliases),
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
        use_aliases: &UseAliases,
    ) -> hir::TraitDef {
        let tparams = type_param_set(&def.generics);
        let name = full_def_name(current_package, &def.name.0);
        let method_sigs = def
            .method_sigs
            .iter()
            .map(|sig| hir::TraitMethodSignature {
                name: HirIdent::name(&sig.name.0),
                params: sig
                    .params
                    .iter()
                    .map(|ty| {
                        self.lower_type_expr(ty, &tparams, current_package, imports, use_aliases)
                    })
                    .collect(),
                ret_ty: self.lower_type_expr(
                    &sig.ret_ty,
                    &tparams,
                    current_package,
                    imports,
                    use_aliases,
                ),
            })
            .collect();
        let associated_types = def
            .associated_types
            .iter()
            .map(|associated| hir::AssociatedType {
                name: HirIdent::name(&associated.name.0),
                bounds: associated
                    .bounds
                    .iter()
                    .map(|bound| {
                        let qualified =
                            self.resolve_qualified_path_with_aliases(&bound.path, use_aliases);
                        hir::TraitRef {
                            name: HirIdent::name(qualified.display()),
                            args: bound
                                .args
                                .iter()
                                .map(|arg| {
                                    self.lower_type_expr(
                                        arg,
                                        &tparams,
                                        current_package,
                                        imports,
                                        use_aliases,
                                    )
                                })
                                .collect(),
                        }
                    })
                    .collect(),
            })
            .collect();
        hir::TraitDef {
            attrs: def.attrs.iter().map(|a| a.into()).collect(),
            name: HirIdent::name(&name),
            generics: def.generics.iter().map(|g| HirIdent::name(&g.0)).collect(),
            generic_bounds: def
                .generic_bounds
                .iter()
                .map(|(param, bounds)| {
                    (
                        HirIdent::name(&param.0),
                        bounds
                            .iter()
                            .map(|bound| {
                                let qualified = self
                                    .resolve_qualified_path_with_aliases(&bound.path, use_aliases);
                                hir::TraitRef {
                                    name: HirIdent::name(qualified.display()),
                                    args: bound
                                        .args
                                        .iter()
                                        .map(|arg| {
                                            self.lower_type_expr(
                                                arg,
                                                &tparams,
                                                current_package,
                                                imports,
                                                use_aliases,
                                            )
                                        })
                                        .collect(),
                                }
                            })
                            .collect(),
                    )
                })
                .collect(),
            predicates: def
                .predicates
                .iter()
                .map(|predicate| {
                    self.lower_predicate_without_context(
                        predicate,
                        &tparams,
                        current_package,
                        imports,
                        use_aliases,
                    )
                })
                .collect(),
            supertraits: def
                .supertraits
                .iter()
                .map(|bound| {
                    let qualified =
                        self.resolve_qualified_path_with_aliases(&bound.path, use_aliases);
                    hir::TraitRef {
                        name: HirIdent::name(qualified.display()),
                        args: bound
                            .args
                            .iter()
                            .map(|arg| {
                                self.lower_type_expr(
                                    arg,
                                    &tparams,
                                    current_package,
                                    imports,
                                    use_aliases,
                                )
                            })
                            .collect(),
                    }
                })
                .collect(),
            associated_types,
            method_sigs,
        }
    }

    fn lower_trait_ref(
        &mut self,
        trait_ref: &ast::TraitRef,
        tparams: &HashSet<String>,
        ctx: &ResolutionContext,
    ) -> hir::TraitRef {
        hir::TraitRef {
            name: HirIdent::name(self.lower_impl_trait_name(&trait_ref.path, ctx)),
            args: trait_ref
                .args
                .iter()
                .map(|arg| {
                    self.lower_type_expr(
                        arg,
                        tparams,
                        ctx.current_package,
                        ctx.imports,
                        ctx.use_aliases,
                    )
                })
                .collect(),
        }
    }

    fn lower_predicate(
        &mut self,
        predicate: &ast::Predicate,
        tparams: &HashSet<String>,
        ctx: &ResolutionContext,
    ) -> hir::Predicate {
        match predicate {
            ast::Predicate::Trait { ty, trait_ref } => hir::Predicate::Trait {
                ty: self.lower_type_expr(
                    ty,
                    tparams,
                    ctx.current_package,
                    ctx.imports,
                    ctx.use_aliases,
                ),
                trait_ref: self.lower_trait_ref(trait_ref, tparams, ctx),
            },
            ast::Predicate::Equality { lhs, rhs } => hir::Predicate::Equality {
                lhs: self.lower_type_expr(
                    lhs,
                    tparams,
                    ctx.current_package,
                    ctx.imports,
                    ctx.use_aliases,
                ),
                rhs: self.lower_type_expr(
                    rhs,
                    tparams,
                    ctx.current_package,
                    ctx.imports,
                    ctx.use_aliases,
                ),
            },
        }
    }

    fn lower_predicate_without_context(
        &mut self,
        predicate: &ast::Predicate,
        tparams: &HashSet<String>,
        current_package: &str,
        imports: &HashSet<String>,
        use_aliases: &UseAliases,
    ) -> hir::Predicate {
        match predicate {
            ast::Predicate::Trait { ty, trait_ref } => {
                let qualified =
                    self.resolve_qualified_path_with_aliases(&trait_ref.path, use_aliases);
                hir::Predicate::Trait {
                    ty: self.lower_type_expr(ty, tparams, current_package, imports, use_aliases),
                    trait_ref: hir::TraitRef {
                        name: HirIdent::name(qualified.display()),
                        args: trait_ref
                            .args
                            .iter()
                            .map(|arg| {
                                self.lower_type_expr(
                                    arg,
                                    tparams,
                                    current_package,
                                    imports,
                                    use_aliases,
                                )
                            })
                            .collect(),
                    },
                }
            }
            ast::Predicate::Equality { lhs, rhs } => hir::Predicate::Equality {
                lhs: self.lower_type_expr(lhs, tparams, current_package, imports, use_aliases),
                rhs: self.lower_type_expr(rhs, tparams, current_package, imports, use_aliases),
            },
        }
    }

    fn lower_impl_trait_name(&mut self, path: &ast::Path, ctx: &ResolutionContext) -> String {
        if path.len() == 1 {
            let name = match path.last_ident() {
                Some(ident) => ident.0.clone(),
                None => {
                    self.ice("impl trait path length 1 missing last ident");
                    "<error>".to_string()
                }
            };

            let local = full_def_name(ctx.current_package, &name);
            let has_local = ctx.trait_index.has_trait(ctx.current_package, &name);
            let has_builtin = ctx.trait_index.has_trait(BUILTIN_PACKAGE, &name);

            if has_local && has_builtin && local != name {
                self.error(format!(
                    "Ambiguous trait {}. Use {}::{} or {}::{}",
                    name, ctx.current_package, name, BUILTIN_PACKAGE, name
                ));
            }

            if has_local {
                return local;
            }
            if has_builtin {
                return name;
            }
            return local;
        }

        let qualified = self.resolve_qualified_path(path, ctx);
        if let Some(package) = &qualified.package
            && !package_allowed(package.as_str(), ctx.current_package, ctx.imports)
        {
            self.error(format!(
                "package {} not imported in package {}",
                package.0, ctx.current_package
            ));
        }
        qualified.display()
    }

    fn lower_extern_fn(
        &mut self,
        def: &ast::ExternFn,
        current_package: &str,
        imports: &HashSet<String>,
        use_aliases: &UseAliases,
    ) -> hir::ExternFn {
        let name = full_def_name(current_package, &def.name.0);
        let tparams = type_param_set(&def.generics);
        let generic_bounds = def
            .generic_bounds
            .iter()
            .map(|(param, traits)| {
                let traits = traits
                    .iter()
                    .map(|trait_ref| hir::TraitRef {
                        name: HirIdent::name(trait_ref.path.display()),
                        args: trait_ref
                            .args
                            .iter()
                            .map(|arg| {
                                self.lower_type_expr(
                                    arg,
                                    &tparams,
                                    current_package,
                                    imports,
                                    use_aliases,
                                )
                            })
                            .collect(),
                    })
                    .collect::<Vec<_>>();
                (HirIdent::name(&param.0), traits)
            })
            .collect();
        hir::ExternFn {
            attrs: def.attrs.iter().map(|a| a.into()).collect(),
            name: HirIdent::name(&name),
            generics: def.generics.iter().map(|g| HirIdent::name(&g.0)).collect(),
            generic_bounds,
            predicates: def
                .predicates
                .iter()
                .map(|predicate| {
                    self.lower_predicate_without_context(
                        predicate,
                        &tparams,
                        current_package,
                        imports,
                        use_aliases,
                    )
                })
                .collect(),
            params: def
                .params
                .iter()
                .map(|(param, ty)| {
                    (
                        HirIdent::name(&param.0),
                        self.lower_type_expr(ty, &tparams, current_package, imports, use_aliases),
                    )
                })
                .collect(),
            ret_ty: def.ret_ty.as_ref().map(|ty| {
                self.lower_type_expr(ty, &tparams, current_package, imports, use_aliases)
            }),
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
                self.lower_type_expr(
                    t,
                    env.type_params(),
                    ctx.current_package,
                    ctx.imports,
                    ctx.use_aliases,
                )
            }),
            astptr: param.astptr,
        }
    }
}

fn type_param_set(params: &[ast::AstIdent]) -> HashSet<String> {
    params.iter().map(|param| param.0.clone()).collect()
}

fn full_def_path(package: &str, name: &str) -> hir::Path {
    if is_special_unqualified_package(package) {
        hir::Path::from_ident(name.to_string())
    } else {
        hir::Path::from_idents(vec![package.to_string(), name.to_string()])
    }
}

fn constructor_path(package: &str, enum_name: &str, variant: &str) -> hir::Path {
    if is_special_unqualified_package(package) {
        hir::Path::from_idents(vec![enum_name.to_string(), variant.to_string()])
    } else {
        hir::Path::from_idents(vec![
            package.to_string(),
            enum_name.to_string(),
            variant.to_string(),
        ])
    }
}
