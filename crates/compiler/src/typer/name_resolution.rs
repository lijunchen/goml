use std::collections::{HashMap, HashSet};

use ast::ast;

use crate::builtins;
use crate::env;
use crate::hir;
use crate::hir::HirIdent;
use crate::interface;
use crate::namespace_imports::{
    external_import_alias, is_exact_external_namespace_import, resolve_external_import_prefix,
};
use crate::package_names::{BUILTIN_PACKAGE, ROOT_PACKAGE, is_special_unqualified_package};
use crate::pipeline::packages::collect_known_crate_path_imports_from_ast;
use diagnostics::{Diagnostic, Diagnostics, Severity, Stage};
use parser::syntax::MySyntaxNodePtr;

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
    deps: &'a HashMap<String, interface::CrateInterface>,
    current_package: &'a str,
    current_module_path: &'a [String],
    crate_paths_include_package: bool,
    imports: &'a HashSet<String>,
    use_values: &'a HashMap<String, UseValueImport>,
    constructor_index: &'a ConstructorIndex,
    type_index: &'a TypeIndex,
    trait_index: &'a TraitIndex,
}

#[derive(Debug, Clone)]
struct UseValueImport {
    res: hir::NameRef,
    hint: String,
}

fn full_def_name_in_module(package: &str, module_path: &[String], name: &str) -> String {
    let mut segments = full_def_segments(package, module_path, name);
    if segments.len() == 1 {
        segments.pop().unwrap_or_default()
    } else {
        segments.join("::")
    }
}

fn full_def_path_in_module(package: &str, module_path: &[String], name: &str) -> hir::Path {
    hir::Path::from_idents(full_def_segments(package, module_path, name))
}

fn full_def_segments(package: &str, module_path: &[String], name: &str) -> Vec<String> {
    let mut segments = module_prefix_segments(package, module_path);
    segments.push(name.to_string());
    segments
}

fn module_relative_def_name(module_path: &[String], name: &str) -> String {
    if module_path.is_empty() {
        name.to_string()
    } else {
        format!("{}::{name}", module_path.join("::"))
    }
}

fn module_prefix_segments(package: &str, module_path: &[String]) -> Vec<String> {
    if module_path.is_empty() {
        if is_special_unqualified_package(package) {
            Vec::new()
        } else {
            vec![package.to_string()]
        }
    } else {
        let module_name = module_path.join("::");
        if package == module_name || is_special_unqualified_package(package) {
            module_path.to_vec()
        } else {
            std::iter::once(package.to_string())
                .chain(module_path.iter().cloned())
                .collect()
        }
    }
}

fn crate_prefix_segments(
    package: &str,
    module_path: &[String],
    include_package: bool,
) -> Vec<String> {
    let module_name = module_path.join("::");
    if !include_package
        || is_special_unqualified_package(package)
        || (!module_path.is_empty() && package == module_name)
    {
        Vec::new()
    } else {
        vec![package.to_string()]
    }
}

fn parent_module_prefix_segments(package: &str, module_path: &[String]) -> Vec<String> {
    let parent = module_path
        .split_last()
        .map(|(_, parent)| parent)
        .unwrap_or(&[]);
    module_prefix_segments(package, parent)
}

fn namespace_allowed(namespace: &str, current_namespace: &str, imports: &HashSet<String>) -> bool {
    namespace == current_namespace || namespace == BUILTIN_PACKAGE || imports.contains(namespace)
}

struct ConstructorIndex {
    enums_by_namespace: HashMap<String, HashMap<String, HashSet<String>>>,
}

impl ConstructorIndex {
    fn new_with_deps(
        files: &[hir::SourceFileAst],
        deps: &HashMap<String, interface::CrateInterface>,
    ) -> Self {
        let mut index = Self {
            enums_by_namespace: HashMap::new(),
        };
        index.add_files(files);
        if !files.iter().any(|file| file.package == BUILTIN_PACKAGE) {
            let builtin_ast = builtins::get_builtin_ast();
            let builtin_file =
                hir::SourceFileAst::with_package("<builtin>".into(), BUILTIN_PACKAGE, builtin_ast);
            index.add_files(std::slice::from_ref(&builtin_file));
        }
        for (package, interface) in deps {
            index.add_interface(package, interface);
        }
        index
    }

    fn add_files(&mut self, files: &[hir::SourceFileAst]) {
        for file in files {
            let package = file.package.clone();
            let entry = self.enums_by_namespace.entry(package).or_default();
            for item in &file.ast.toplevels {
                if let ast::Item::EnumDef(def) = item {
                    let enum_name = module_relative_def_name(&file.module_path, &def.name.0);
                    let variants = entry.entry(enum_name).or_default();
                    for (variant, _) in &def.variants {
                        variants.insert(variant.0.clone());
                    }
                }
            }
        }
    }

    fn add_interface(&mut self, package: &str, interface: &interface::CrateInterface) {
        let entry = self
            .enums_by_namespace
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
        self.enums_by_namespace
            .get(package)
            .and_then(|enums| enums.get(enum_name))
            .is_some_and(|variants| variants.contains(variant))
    }

    fn unique_enum_for_variant(&self, package: &str, variant: &str) -> Option<String> {
        let enums = self.enums_by_namespace.get(package)?;
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
        self.enums_by_namespace
            .get(package)
            .is_some_and(|enums| enums.values().any(|vars| vars.contains(variant)))
    }
}

struct TypeIndex {
    types_by_namespace: HashMap<String, HashSet<String>>,
}

impl TypeIndex {
    fn new_with_files(files: &[hir::SourceFileAst]) -> Self {
        let mut index = Self {
            types_by_namespace: HashMap::new(),
        };
        index.add_files(files);
        index
    }

    fn add_files(&mut self, files: &[hir::SourceFileAst]) {
        for file in files {
            let package = file.package.clone();
            let entry = self.types_by_namespace.entry(package).or_default();
            for item in &file.ast.toplevels {
                let name = match item {
                    ast::Item::EnumDef(def) => Some(&def.name.0),
                    ast::Item::StructDef(def) => Some(&def.name.0),
                    ast::Item::ExternType(def) => Some(&def.goml_name.0),
                    _ => None,
                };
                if let Some(name) = name {
                    entry.insert(module_relative_def_name(&file.module_path, name));
                }
            }
        }
    }

    fn has_type(&self, package: &str, name: &str) -> bool {
        self.types_by_namespace
            .get(package)
            .is_some_and(|types| types.contains(name))
    }
}

struct TraitIndex {
    traits_by_namespace: HashMap<String, HashSet<String>>,
}

impl TraitIndex {
    fn new_with_files(files: &[hir::SourceFileAst]) -> Self {
        let mut index = Self {
            traits_by_namespace: HashMap::new(),
        };
        index.add_files(files);
        if !files.iter().any(|file| file.package == BUILTIN_PACKAGE) {
            let builtin_ast = builtins::get_builtin_ast();
            let builtin_file =
                hir::SourceFileAst::with_package("<builtin>".into(), BUILTIN_PACKAGE, builtin_ast);
            index.add_files(std::slice::from_ref(&builtin_file));
        }
        index
    }

    fn add_files(&mut self, files: &[hir::SourceFileAst]) {
        for file in files {
            let package = file.package.clone();
            let entry = self.traits_by_namespace.entry(package).or_default();
            for item in &file.ast.toplevels {
                if let ast::Item::TraitDef(def) = item {
                    entry.insert(module_relative_def_name(&file.module_path, &def.name.0));
                }
            }
        }
    }

    fn has_trait(&self, package: &str, name: &str) -> bool {
        self.traits_by_namespace
            .get(package)
            .is_some_and(|traits| traits.contains(name))
    }
}

impl ResolutionContext<'_> {
    fn namespace_allowed(&self, namespace: &str) -> bool {
        namespace == self.current_package
            || namespace == BUILTIN_PACKAGE
            || self.imports.contains(namespace)
    }
}

fn use_path_is_namespace_import(
    path: &ast::Path,
    deps: &HashMap<String, interface::CrateInterface>,
) -> bool {
    is_exact_external_namespace_import(path, deps)
}

fn file_imports(
    file: &ast::File,
    deps: &HashMap<String, interface::CrateInterface>,
) -> HashSet<String> {
    let mut imports = HashSet::new();
    for use_decl in file.uses.iter() {
        if let Some(package) = use_decl_import(&use_decl.path, deps) {
            imports.insert(package);
        }
    }
    for item in file.toplevels.iter() {
        if let ast::Item::Mod(module) = item {
            imports.insert(module.name.0.clone());
        }
    }
    let known_namespaces = deps.keys().cloned().collect::<HashSet<_>>();
    imports.extend(collect_known_crate_path_imports_from_ast(
        file,
        &known_namespaces,
    ));
    imports
}

fn external_import_path_for_alias(
    alias: &str,
    deps: &HashMap<String, interface::CrateInterface>,
) -> Option<String> {
    deps.get(alias)?
        .import_paths
        .iter()
        .find(|path| path.as_str() != alias)
        .cloned()
}

fn external_coordinate_alias_path(
    path: &ast::Path,
    deps: &HashMap<String, interface::CrateInterface>,
) -> Option<(String, String)> {
    let display = path_segments_display(path);
    let mut best = None;
    for (alias, dep) in deps {
        for import_path in dep.import_paths.iter() {
            let Some(import_root) = import_path.split("::").next() else {
                continue;
            };
            if !import_path.contains("::") || deps.contains_key(import_root) {
                continue;
            }
            if display != import_path.as_str()
                && !display
                    .strip_prefix(import_path)
                    .is_some_and(|suffix| suffix.starts_with("::"))
            {
                continue;
            }
            let segment_count = import_path.split("::").count();
            if best
                .as_ref()
                .is_some_and(|(_, _, best_segments)| segment_count >= *best_segments)
            {
                continue;
            }
            let suffix = display
                .strip_prefix(import_path)
                .unwrap_or("")
                .strip_prefix("::")
                .unwrap_or("");
            let alias_path = if suffix.is_empty() {
                alias.clone()
            } else {
                format!("{alias}::{suffix}")
            };
            best = Some((import_path.clone(), alias_path, segment_count));
        }
    }
    best.map(|(import_path, alias_path, _)| (import_path, alias_path))
}

fn first_crate_segment(path: &ast::Path) -> Option<String> {
    path.segments()
        .first()
        .map(|segment| segment.ident.0.clone())
}

fn use_decl_import(
    path: &ast::Path,
    deps: &HashMap<String, interface::CrateInterface>,
) -> Option<String> {
    if let Some(alias) = external_import_alias(path, deps) {
        return Some(alias);
    }
    first_crate_segment(path)
}

fn use_decl_value_import(
    decl: &ast::UseDecl,
    current_package: &str,
    current_module_path: &[String],
    crate_paths_include_package: bool,
    deps: &HashMap<String, interface::CrateInterface>,
    def_names: &HashMap<String, hir::DefId>,
) -> Option<(String, UseValueImport)> {
    path_value_import(
        &decl.path,
        decl.alias.as_ref(),
        current_package,
        current_module_path,
        crate_paths_include_package,
        deps,
        def_names,
    )
}

fn path_value_import(
    path: &ast::Path,
    alias: Option<&ast::AstIdent>,
    current_package: &str,
    current_module_path: &[String],
    crate_paths_include_package: bool,
    deps: &HashMap<String, interface::CrateInterface>,
    def_names: &HashMap<String, hir::DefId>,
) -> Option<(String, UseValueImport)> {
    if path_resolution_segments(path).len() < 2
        && matches!(
            path.root(),
            ast::PathRoot::Relative | ast::PathRoot::Absolute
        )
    {
        return None;
    }
    let full_name = path_resolution_display_in_module(
        path,
        current_package,
        current_module_path,
        crate_paths_include_package,
    );
    let imported_name = alias
        .or_else(|| path.last_ident())
        .map(|ident| ident.0.clone())?;
    let package = known_package_prefix_for_path_in_module(
        path,
        deps,
        current_package,
        current_module_path,
        crate_paths_include_package,
    );
    if package == current_package || package == BUILTIN_PACKAGE {
        let id = def_names.get(&full_name).copied()?;
        return Some((
            imported_name,
            UseValueImport {
                res: hir::NameRef::Def(id),
                hint: full_name,
            },
        ));
    }
    let idx = deps.get(&package)?.value_exports.get(&full_name).copied()?;
    Some((
        imported_name,
        UseValueImport {
            res: hir::NameRef::Def(hir::DefId {
                pkg: interface::crate_id_for_name(&package),
                idx,
            }),
            hint: full_name,
        },
    ))
}

fn file_use_value_imports(
    file: &ast::File,
    current_package: &str,
    current_module_path: &[String],
    crate_paths_include_package: bool,
    deps: &HashMap<String, interface::CrateInterface>,
    def_names: &HashMap<String, hir::DefId>,
) -> HashMap<String, UseValueImport> {
    file.uses
        .iter()
        .filter_map(|decl| {
            use_decl_value_import(
                decl,
                current_package,
                current_module_path,
                crate_paths_include_package,
                deps,
                def_names,
            )
        })
        .collect()
}

fn path_segments_display(path: &ast::Path) -> String {
    path.segments()
        .iter()
        .map(|segment| segment.ident.0.clone())
        .collect::<Vec<_>>()
        .join("::")
}

fn path_resolution_segments(path: &ast::Path) -> &[ast::PathSegment] {
    let segments = path.segments();
    if !matches!(path.root(), ast::PathRoot::Crate)
        && segments
            .first()
            .is_some_and(|segment| segment.ident.0 == "crate")
    {
        &segments[1..]
    } else {
        segments
    }
}

fn path_resolution_segments_in_module(
    path: &ast::Path,
    current_package: &str,
    current_module_path: &[String],
    crate_paths_include_package: bool,
) -> Vec<String> {
    let segments = path.segments();
    let has_crate_segment = !matches!(path.root(), ast::PathRoot::Crate)
        && segments
            .first()
            .is_some_and(|segment| segment.ident.0 == "crate");
    let mut prefix = match path.root() {
        ast::PathRoot::Crate => crate_prefix_segments(
            current_package,
            current_module_path,
            crate_paths_include_package,
        ),
        _ if has_crate_segment => crate_prefix_segments(
            current_package,
            current_module_path,
            crate_paths_include_package,
        ),
        ast::PathRoot::Self_ => module_prefix_segments(current_package, current_module_path),
        ast::PathRoot::Super => parent_module_prefix_segments(current_package, current_module_path),
        ast::PathRoot::Relative | ast::PathRoot::Absolute => Vec::new(),
    };
    let segments = if has_crate_segment {
        &segments[1..]
    } else if matches!(
        path.root(),
        ast::PathRoot::Relative | ast::PathRoot::Absolute
    ) {
        path_resolution_segments(path)
    } else {
        path.segments()
    };
    prefix.extend(segments.iter().map(|segment| segment.ident.0.clone()));
    prefix
}

fn path_resolution_display_in_module(
    path: &ast::Path,
    current_package: &str,
    current_module_path: &[String],
    crate_paths_include_package: bool,
) -> String {
    path_resolution_segments_in_module(
        path,
        current_package,
        current_module_path,
        crate_paths_include_package,
    )
    .join("::")
}

fn known_package_prefix_for_path_in_module(
    path: &ast::Path,
    deps: &HashMap<String, interface::CrateInterface>,
    current_package: &str,
    current_module_path: &[String],
    crate_paths_include_package: bool,
) -> String {
    let segments = path_resolution_segments_in_module(
        path,
        current_package,
        current_module_path,
        crate_paths_include_package,
    );
    if segments.len() < 2 {
        if matches!(
            path.root(),
            ast::PathRoot::Crate | ast::PathRoot::Self_ | ast::PathRoot::Super
        ) {
            return current_package.to_string();
        }
        return segments.join("::");
    }
    let mut best = None;
    for end in 1..segments.len() {
        let package = segments[..end].join("::");
        if package == current_package || package == BUILTIN_PACKAGE || deps.contains_key(&package) {
            best = Some(package);
        }
    }
    best.unwrap_or_else(|| segments.first().cloned().unwrap_or_default())
}

fn lowered_use_trait_path(
    path: &ast::Path,
    deps: &HashMap<String, interface::CrateInterface>,
) -> Option<hir::QualifiedPath> {
    if let Some((package, prefix_len)) = resolve_external_import_prefix(path, deps) {
        let segments = path.segments()[prefix_len..]
            .iter()
            .map(|segment| hir::PathSegment::new(segment.ident.0.clone()))
            .collect();
        return Some(hir::QualifiedPath {
            package: Some(hir::PackageName(package)),
            path: hir::Path::new(segments),
        });
    }
    let segments = path.segments();
    if segments
        .first()
        .is_some_and(|segment| segment.ident.0 == "crate")
    {
        if segments.len() < 3 {
            return None;
        }
        let package = segments[1].ident.0.clone();
        let path_segments = segments[2..]
            .iter()
            .map(|segment| hir::PathSegment::new(segment.ident.0.clone()))
            .collect();
        return Some(hir::QualifiedPath {
            package: Some(hir::PackageName(package)),
            path: hir::Path::new(path_segments),
        });
    }
    if path.len() < 2 {
        return None;
    }
    Some(path.into())
}

fn qualified_path_from_segments(segments: Vec<String>) -> hir::QualifiedPath {
    if segments.len() <= 1 {
        return hir::QualifiedPath {
            package: None,
            path: hir::Path::from_idents(segments),
        };
    }
    hir::QualifiedPath {
        package: Some(hir::PackageName(segments[0].clone())),
        path: hir::Path::from_idents(segments[1..].to_vec()),
    }
}

fn lower_type_path(
    path: &ast::Path,
    tparams: &HashSet<String>,
    current_package: &str,
    current_module_path: &[String],
    crate_paths_include_package: bool,
    type_index: &TypeIndex,
) -> hir::QualifiedPath {
    if path.len() == 1
        && matches!(
            path.root(),
            ast::PathRoot::Relative | ast::PathRoot::Absolute
        )
    {
        let Some(name) = path.last_ident().map(|ident| ident.0.clone()) else {
            return path.into();
        };
        if tparams.contains(&name) {
            return path.into();
        }
        let local = module_relative_def_name(current_module_path, &name);
        if (crate_paths_include_package || !current_module_path.is_empty())
            && type_index.has_type(current_package, &local)
        {
            return qualified_path_from_segments(full_def_segments(
                current_package,
                current_module_path,
                &name,
            ));
        }
        return path.into();
    }

    let has_crate_segment = !matches!(path.root(), ast::PathRoot::Crate)
        && path
            .segments()
            .first()
            .is_some_and(|segment| segment.ident.0 == "crate");
    match path.root() {
        ast::PathRoot::Crate | ast::PathRoot::Self_ | ast::PathRoot::Super => {
            qualified_path_from_segments(path_resolution_segments_in_module(
                path,
                current_package,
                current_module_path,
                crate_paths_include_package,
            ))
        }
        _ if has_crate_segment => qualified_path_from_segments(path_resolution_segments_in_module(
            path,
            current_package,
            current_module_path,
            crate_paths_include_package,
        )),
        ast::PathRoot::Relative | ast::PathRoot::Absolute => path.into(),
    }
}

impl NameResolution {
    fn error(&mut self, message: impl Into<String>) {
        self.diagnostics
            .push(Diagnostic::new(Stage::Typer, Severity::Error, message));
    }

    fn check_path_root(&mut self, path: &ast::Path, ctx: &ResolutionContext) {
        if matches!(path.root(), ast::PathRoot::Super) && ctx.current_module_path.is_empty() {
            self.error("`super` cannot be used at crate root");
        }
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
            _ => {
                let variant = &last.0;
                let local_enum = segments[..segments.len() - 1]
                    .iter()
                    .map(|segment| segment.ident.0.clone())
                    .collect::<Vec<_>>()
                    .join("::");
                let mut local_candidates = vec![local_enum.clone()];
                if matches!(
                    path.root(),
                    ast::PathRoot::Relative | ast::PathRoot::Absolute
                ) {
                    local_candidates.push(module_relative_def_name(
                        ctx.current_module_path,
                        &local_enum,
                    ));
                } else {
                    let resolved_segments = path_resolution_segments_in_module(
                        path,
                        ctx.current_package,
                        ctx.current_module_path,
                        ctx.crate_paths_include_package,
                    );
                    let enum_segments = &resolved_segments[..resolved_segments.len() - 1];
                    let enum_name = if enum_segments
                        .first()
                        .is_some_and(|segment| segment == ctx.current_package)
                    {
                        enum_segments[1..].join("::")
                    } else {
                        enum_segments.join("::")
                    };
                    local_candidates.push(enum_name);
                }
                local_candidates.sort();
                local_candidates.dedup();
                for candidate in local_candidates {
                    if ctx.constructor_index.enum_has_variant(
                        ctx.current_package,
                        &candidate,
                        variant,
                    ) {
                        return Some(constructor_path(ctx.current_package, &candidate, variant));
                    }
                }
                if ctx
                    .constructor_index
                    .enum_has_variant(BUILTIN_PACKAGE, &local_enum, variant)
                {
                    return Some(constructor_path(BUILTIN_PACKAGE, &local_enum, variant));
                }

                if segments.len() >= 3 {
                    let package = segments.first().map(|seg| &seg.ident.0)?;
                    let enum_name = segments[1..segments.len() - 1]
                        .iter()
                        .map(|segment| segment.ident.0.clone())
                        .collect::<Vec<_>>()
                        .join("::");
                    let exists = ctx
                        .constructor_index
                        .enum_has_variant(package, &enum_name, variant);
                    if exists && !ctx.namespace_allowed(package) {
                        self.error(format!(
                            "namespace {} not imported in namespace {}",
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
            .unwrap_or_else(|| path.into())
    }

    pub fn resolve_files(self, files: Vec<ast::File>) -> (hir::ResolvedHir, HirTable, Diagnostics) {
        let deps = HashMap::new();
        let package_name = ROOT_PACKAGE;
        let package_id = match package_name {
            BUILTIN_PACKAGE => hir::PackageId(0),
            ROOT_PACKAGE => hir::PackageId(1),
            _ => hir::PackageId(2),
        };
        let files = files
            .into_iter()
            .enumerate()
            .map(|(idx, ast)| {
                hir::SourceFileAst::with_package(
                    format!("<unknown:{}>", idx).into(),
                    package_name,
                    ast,
                )
            })
            .collect();
        self.resolve_files_with_env(package_id, files, &deps)
    }

    pub fn resolve_files_with_env(
        mut self,
        package_id: hir::PackageId,
        files: Vec<hir::SourceFileAst>,
        deps: &HashMap<String, interface::CrateInterface>,
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
        let type_index = TypeIndex::new_with_files(&files);
        let trait_index = TraitIndex::new_with_files(&files);
        let mut toplevels = Vec::new();
        let mut per_file_defs = Vec::new();
        let packages_with_module_paths = files
            .iter()
            .filter(|file| !file.module_path.is_empty())
            .map(|file| file.package.clone())
            .collect::<HashSet<_>>();

        for file in files.iter() {
            let package_name = file.package.as_str();
            let mut reported_external_coordinates = HashSet::new();
            for use_path in file.ast.uses.iter().map(|use_decl| &use_decl.path) {
                if !reported_external_coordinates.insert(path_segments_display(use_path)) {
                    continue;
                }
                if let Some((registry_path, alias_path)) =
                    external_coordinate_alias_path(use_path, deps)
                {
                    let first = first_crate_segment(use_path).unwrap_or_default();
                    self.error(format!(
                        "unresolved crate `{first}`. Dependency `{registry_path}` is available as crate alias `{}`. Use `{alias_path}` or rename the dependency key in goml.toml.",
                        alias_path.split("::").next().unwrap_or("")
                    ));
                }
            }
            for use_path in file.ast.uses.iter().map(|use_decl| &use_decl.path) {
                let Some(first) = use_path.segments().first() else {
                    continue;
                };
                let Some(full_path) = external_import_path_for_alias(&first.ident.0, deps) else {
                    continue;
                };
                if external_import_alias(use_path, deps).is_none() {
                    let suffix = use_path.segments()[1..]
                        .iter()
                        .map(|segment| segment.ident.0.clone())
                        .collect::<Vec<_>>()
                        .join("::");
                    let suggested = if suffix.is_empty() {
                        full_path
                    } else {
                        format!("{full_path}::{suffix}")
                    };
                    self.error(format!(
                        "external dependency {} must be imported as {}",
                        use_path.display(),
                        suggested
                    ));
                }
            }
            let imports = file_imports(&file.ast, deps);
            let crate_paths_include_package = packages_with_module_paths.contains(package_name);
            let mut def_ids = Vec::new();
            for item in file.ast.toplevels.iter() {
                let def_id = match item {
                    ast::Item::Mod(_) => None,
                    ast::Item::Fn(func) => {
                        let full_name =
                            full_def_name_in_module(package_name, &file.module_path, &func.name.0);
                        let path =
                            full_def_path_in_module(package_name, &file.module_path, &func.name.0);
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
                                body: hir::Block {
                                    stmts: Vec::new(),
                                    tail: None,
                                },
                            }),
                        );
                        def_names.insert(full_name, id);
                        Some(id)
                    }
                    ast::Item::ExternGo(ext) => {
                        let full_name = full_def_name_in_module(
                            package_name,
                            &file.module_path,
                            &ext.goml_name.0,
                        );
                        let path = full_def_path_in_module(
                            package_name,
                            &file.module_path,
                            &ext.goml_name.0,
                        );
                        let ext_def = self.lower_extern_go(
                            ext,
                            package_name,
                            &file.module_path,
                            crate_paths_include_package,
                            &imports,
                            &type_index,
                        );
                        let id = hir_table.alloc_def_with_path(
                            path,
                            hir::DefKind::ExternGo,
                            hir::Def::ExternGo(ext_def),
                        );
                        def_names.insert(full_name, id);
                        Some(id)
                    }
                    ast::Item::ExternBuiltin(ext) => {
                        let full_name =
                            full_def_name_in_module(package_name, &file.module_path, &ext.name.0);
                        let path =
                            full_def_path_in_module(package_name, &file.module_path, &ext.name.0);
                        let ext_def = self.lower_extern_builtin(
                            ext,
                            package_name,
                            &file.module_path,
                            crate_paths_include_package,
                            &imports,
                            &type_index,
                        );
                        let id = hir_table.alloc_def_with_path(
                            path,
                            hir::DefKind::ExternBuiltin,
                            hir::Def::ExternBuiltin(ext_def),
                        );
                        def_names.insert(full_name, id);
                        Some(id)
                    }
                    ast::Item::EnumDef(e) => {
                        let full_name =
                            full_def_name_in_module(package_name, &file.module_path, &e.name.0);
                        let path =
                            full_def_path_in_module(package_name, &file.module_path, &e.name.0);
                        let enum_def = self.lower_enum_def(
                            e,
                            package_name,
                            &file.module_path,
                            crate_paths_include_package,
                            &imports,
                            &type_index,
                        );
                        let id = hir_table.alloc_def_with_path(
                            path,
                            hir::DefKind::EnumDef,
                            hir::Def::EnumDef(enum_def),
                        );
                        def_names.insert(full_name, id);
                        Some(id)
                    }
                    ast::Item::StructDef(s) => {
                        let full_name =
                            full_def_name_in_module(package_name, &file.module_path, &s.name.0);
                        let path =
                            full_def_path_in_module(package_name, &file.module_path, &s.name.0);
                        let struct_def = self.lower_struct_def(
                            s,
                            package_name,
                            &file.module_path,
                            crate_paths_include_package,
                            &imports,
                            &type_index,
                        );
                        let id = hir_table.alloc_def_with_path(
                            path,
                            hir::DefKind::StructDef,
                            hir::Def::StructDef(struct_def),
                        );
                        def_names.insert(full_name, id);
                        Some(id)
                    }
                    ast::Item::TraitDef(t) => {
                        let full_name =
                            full_def_name_in_module(package_name, &file.module_path, &t.name.0);
                        let path =
                            full_def_path_in_module(package_name, &file.module_path, &t.name.0);
                        let trait_def = self.lower_trait_def(
                            t,
                            package_name,
                            &file.module_path,
                            crate_paths_include_package,
                            &imports,
                            &type_index,
                        );
                        let id = hir_table.alloc_def_with_path(
                            path,
                            hir::DefKind::TraitDef,
                            hir::Def::TraitDef(trait_def),
                        );
                        def_names.insert(full_name, id);
                        Some(id)
                    }
                    ast::Item::ImplBlock(_i) => Some(hir_table.alloc_def_with_path(
                        full_def_path_in_module(package_name, &file.module_path, "impl"),
                        hir::DefKind::ImplBlock,
                        hir::Def::ImplBlock(hir::ImplBlock {
                            attrs: Vec::new(),
                            generics: Vec::new(),
                            generic_bounds: Vec::new(),
                            trait_name: None,
                            for_type: hir::TypeExpr::TUnit,
                            methods: Vec::new(),
                        }),
                    )),
                    ast::Item::ExternType(ext) => {
                        let full_name = full_def_name_in_module(
                            package_name,
                            &file.module_path,
                            &ext.goml_name.0,
                        );
                        let path = full_def_path_in_module(
                            package_name,
                            &file.module_path,
                            &ext.goml_name.0,
                        );
                        let ext_def = self.lower_extern_type(ext, package_name, &file.module_path);
                        let id = hir_table.alloc_def_with_path(
                            path,
                            hir::DefKind::ExternType,
                            hir::Def::ExternType(ext_def),
                        );
                        def_names.insert(full_name, id);
                        Some(id)
                    }
                };
                if let Some(def_id) = def_id {
                    toplevels.push(def_id);
                    def_ids.push(def_id);
                }
            }
            per_file_defs.push(def_ids);
        }

        for (file_idx, file) in files.iter().enumerate() {
            let package_name = file.package.as_str();
            let imports = file_imports(&file.ast, deps);
            let crate_paths_include_package = packages_with_module_paths.contains(package_name);
            let use_values = file_use_value_imports(
                &file.ast,
                package_name,
                &file.module_path,
                crate_paths_include_package,
                deps,
                &def_names,
            );
            let ctx = ResolutionContext {
                builtin_names: &builtin_names,
                def_names: &def_names,
                deps,
                current_package: package_name,
                current_module_path: &file.module_path,
                crate_paths_include_package,
                imports: &imports,
                use_values: &use_values,
                constructor_index: &ctor_index,
                type_index: &type_index,
                trait_index: &trait_index,
            };

            let mut toplevel_idx = 0;
            for item in file.ast.toplevels.iter() {
                match item {
                    ast::Item::Mod(_) => {}
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
                        let full_name =
                            full_def_name_in_module(package_name, &file.module_path, &func.name.0);
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
                        let generic_bounds = i
                            .generic_bounds
                            .iter()
                            .map(|(param, traits)| {
                                let traits = traits
                                    .iter()
                                    .map(|path| self.lower_trait_path(path, &ctx))
                                    .collect::<Vec<_>>();
                                (HirIdent::name(&param.0), traits)
                            })
                            .collect();
                        let trait_name = i
                            .trait_name
                            .as_ref()
                            .map(|t| HirIdent::name(self.lower_impl_trait_name(t, &ctx)));
                        let impl_block = hir::ImplBlock {
                            attrs: i.attrs.iter().map(|a| a.into()).collect(),
                            generics: i.generics.iter().map(|g| HirIdent::name(&g.0)).collect(),
                            generic_bounds,
                            trait_name,
                            for_type: self.lower_type_expr(
                                &i.for_type,
                                &tparams,
                                package_name,
                                &file.module_path,
                                ctx.crate_paths_include_package,
                                &imports,
                                &type_index,
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
                let package = file.package.clone();
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
                let imports = file_imports(&file.ast, deps);
                let mut imports_vec = imports
                    .into_iter()
                    .map(hir::PackageName)
                    .collect::<Vec<_>>();
                imports_vec.sort_by(|a, b| a.0.cmp(&b.0));

                let mut use_traits = Vec::new();
                let crate_paths_include_package = packages_with_module_paths.contains(&package);
                for use_decl in file.ast.uses.iter() {
                    if use_decl_value_import(
                        use_decl,
                        &package,
                        &file.module_path,
                        crate_paths_include_package,
                        deps,
                        &def_names,
                    )
                    .is_some()
                    {
                        continue;
                    }
                    if let Some(first) = use_decl.path.segments().first()
                        && external_import_path_for_alias(&first.ident.0, deps).is_some()
                        && external_import_alias(&use_decl.path, deps).is_none()
                    {
                        continue;
                    }
                    if use_path_is_namespace_import(&use_decl.path, deps) {
                        continue;
                    }
                    let Some(qualified) = lowered_use_trait_path(&use_decl.path, deps) else {
                        continue;
                    };
                    let Some(package) = &qualified.package else {
                        self.ice("use trait is missing package");
                        continue;
                    };
                    let Some(trait_name) = qualified.last_ident() else {
                        self.ice("use trait is missing name");
                        continue;
                    };
                    let _ = (package, trait_name);
                    use_traits.push(qualified);
                }
                hir::SourceFileHir {
                    path,
                    module_path: file.module_path.clone(),
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
                body: hir::Block {
                    stmts: Vec::new(),
                    tail: None,
                },
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
                    self.lower_type_expr(
                        &param.1,
                        &tparams,
                        ctx.current_package,
                        ctx.current_module_path,
                        ctx.crate_paths_include_package,
                        ctx.imports,
                        ctx.type_index,
                    ),
                )
            })
            .collect();

        let new_generic_bounds = generic_bounds
            .iter()
            .map(|(param, traits)| {
                let traits = traits
                    .iter()
                    .map(|path| self.lower_trait_path(path, ctx))
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
            ret_ty: ret_ty.as_ref().map(|t| {
                self.lower_type_expr(
                    t,
                    &tparams,
                    ctx.current_package,
                    ctx.current_module_path,
                    ctx.crate_paths_include_package,
                    ctx.imports,
                    ctx.type_index,
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
            annotation: stmt.annotation.as_ref().map(|t| t.into()),
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
            ast::Expr::EPath { path, astptr } => {
                self.check_path_root(path, ctx);
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
                let use_unqualified_resolution = path.len() == 1
                    && !matches!(path.root(), ast::PathRoot::Super)
                    && !(matches!(path.root(), ast::PathRoot::Crate)
                        && ctx.crate_paths_include_package);
                if use_unqualified_resolution {
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
                        let full_name = full_def_name_in_module(
                            ctx.current_package,
                            ctx.current_module_path,
                            name_str,
                        );
                        if let Some(&def_id) = ctx.def_names.get(&full_name) {
                            hir::NameRef::Def(def_id)
                        } else if let Some(&builtin_id) = ctx.builtin_names.get(name_str) {
                            hir::NameRef::Builtin(builtin_id)
                        } else if let Some(imported) = ctx.use_values.get(name_str) {
                            imported.res.clone()
                        } else {
                            hir::NameRef::Unresolved(hir::Path::from_ident(name_str.clone()))
                        }
                    };
                    let hint = match (&res, ctx.use_values.get(name_str)) {
                        (res, Some(imported)) if imported.res == *res => imported.hint.clone(),
                        (hir::NameRef::Def(_), _) => full_def_name_in_module(
                            ctx.current_package,
                            ctx.current_module_path,
                            name_str,
                        ),
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
                    let unresolved_path =
                        hir::Path::from_idents(path_resolution_segments_in_module(
                            path,
                            ctx.current_package,
                            ctx.current_module_path,
                            ctx.crate_paths_include_package,
                        ));
                    let full_name = path_resolution_display_in_module(
                        path,
                        ctx.current_package,
                        ctx.current_module_path,
                        ctx.crate_paths_include_package,
                    );
                    let package = known_package_prefix_for_path_in_module(
                        path,
                        ctx.deps,
                        ctx.current_package,
                        ctx.current_module_path,
                        ctx.crate_paths_include_package,
                    );
                    if package != ctx.current_package
                        && package != BUILTIN_PACKAGE
                        && ctx.deps.contains_key(&package)
                        && !ctx.imports.contains(&package)
                    {
                        self.error(format!(
                            "namespace {} not imported in namespace {}",
                            package, ctx.current_package
                        ));
                    }

                    let res = if package == ctx.current_package || package == BUILTIN_PACKAGE {
                        ctx.def_names
                            .get(&full_name)
                            .copied()
                            .map(hir::NameRef::Def)
                            .unwrap_or_else(|| hir::NameRef::Unresolved(unresolved_path.clone()))
                    } else if ctx.imports.contains(&package) {
                        ctx.deps
                            .get(&package)
                            .and_then(|pkg_interface| pkg_interface.value_exports.get(&full_name))
                            .copied()
                            .map(|idx| {
                                hir::NameRef::Def(hir::DefId {
                                    pkg: interface::crate_id_for_name(&package),
                                    idx,
                                })
                            })
                            .unwrap_or_else(|| hir::NameRef::Unresolved(unresolved_path.clone()))
                    } else {
                        hir::NameRef::Unresolved(unresolved_path.clone())
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
                            if path.len() == 2
                                && (package == ctx.current_package
                                    || package == BUILTIN_PACKAGE
                                    || ctx.deps.contains_key(&package)) =>
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
                                path: unresolved_path,
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
                let qualified = lower_type_path(
                    name,
                    &HashSet::new(),
                    ctx.current_package,
                    ctx.current_module_path,
                    ctx.crate_paths_include_package,
                    ctx.type_index,
                );
                if let Some(package) = &qualified.package
                    && !ctx.namespace_allowed(package.as_str())
                {
                    self.error(format!(
                        "namespace {} not imported in namespace {}",
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
                let qualified = lower_type_path(
                    name,
                    &HashSet::new(),
                    ctx.current_package,
                    ctx.current_module_path,
                    ctx.crate_paths_include_package,
                    ctx.type_index,
                );
                if let Some(package) = &qualified.package
                    && !ctx.namespace_allowed(package.as_str())
                {
                    self.error(format!(
                        "namespace {} not imported in namespace {}",
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
        tparams: &HashSet<String>,
        current_package: &str,
        current_module_path: &[String],
        crate_paths_include_package: bool,
        imports: &HashSet<String>,
        type_index: &TypeIndex,
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
                        self.lower_type_expr(
                            ty,
                            tparams,
                            current_package,
                            current_module_path,
                            crate_paths_include_package,
                            imports,
                            type_index,
                        )
                    })
                    .collect(),
            },
            ast::TypeExpr::TCon { path } => {
                let qualified = lower_type_path(
                    path,
                    tparams,
                    current_package,
                    current_module_path,
                    crate_paths_include_package,
                    type_index,
                );
                if let Some(package) = &qualified.package
                    && !namespace_allowed(package.as_str(), current_package, imports)
                {
                    self.error(format!(
                        "namespace {} not imported in namespace {}",
                        package.0, current_package
                    ));
                }
                hir::TypeExpr::TCon { path: qualified }
            }
            ast::TypeExpr::TDyn { trait_path } => {
                let qualified = lower_type_path(
                    trait_path,
                    &HashSet::new(),
                    current_package,
                    current_module_path,
                    crate_paths_include_package,
                    type_index,
                );
                if let Some(package) = &qualified.package
                    && !namespace_allowed(package.as_str(), current_package, imports)
                {
                    self.error(format!(
                        "namespace {} not imported in namespace {}",
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
                    tparams,
                    current_package,
                    current_module_path,
                    crate_paths_include_package,
                    imports,
                    type_index,
                )),
                args: args
                    .iter()
                    .map(|arg| {
                        self.lower_type_expr(
                            arg,
                            tparams,
                            current_package,
                            current_module_path,
                            crate_paths_include_package,
                            imports,
                            type_index,
                        )
                    })
                    .collect(),
            },
            ast::TypeExpr::TArray { len, elem } => hir::TypeExpr::TArray {
                len: *len,
                elem: Box::new(self.lower_type_expr(
                    elem.as_ref(),
                    tparams,
                    current_package,
                    current_module_path,
                    crate_paths_include_package,
                    imports,
                    type_index,
                )),
            },
            ast::TypeExpr::TFunc { params, ret_ty } => hir::TypeExpr::TFunc {
                params: params
                    .iter()
                    .map(|param| {
                        self.lower_type_expr(
                            param,
                            tparams,
                            current_package,
                            current_module_path,
                            crate_paths_include_package,
                            imports,
                            type_index,
                        )
                    })
                    .collect(),
                ret_ty: Box::new(self.lower_type_expr(
                    ret_ty.as_ref(),
                    tparams,
                    current_package,
                    current_module_path,
                    crate_paths_include_package,
                    imports,
                    type_index,
                )),
            },
        }
    }

    fn lower_enum_def(
        &mut self,
        def: &ast::EnumDef,
        current_package: &str,
        current_module_path: &[String],
        crate_paths_include_package: bool,
        imports: &HashSet<String>,
        type_index: &TypeIndex,
    ) -> hir::EnumDef {
        let tparams = type_param_set(&def.generics);
        let name = full_def_name_in_module(current_package, current_module_path, &def.name.0);
        let variants = def
            .variants
            .iter()
            .map(|(variant_name, tys)| {
                let types = tys
                    .iter()
                    .map(|ty| {
                        self.lower_type_expr(
                            ty,
                            &tparams,
                            current_package,
                            current_module_path,
                            crate_paths_include_package,
                            imports,
                            type_index,
                        )
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
        current_module_path: &[String],
        crate_paths_include_package: bool,
        imports: &HashSet<String>,
        type_index: &TypeIndex,
    ) -> hir::StructDef {
        let tparams = type_param_set(&def.generics);
        let name = full_def_name_in_module(current_package, current_module_path, &def.name.0);
        let fields = def
            .fields
            .iter()
            .map(|(field_name, ty)| {
                (
                    HirIdent::name(&field_name.0),
                    self.lower_type_expr(
                        ty,
                        &tparams,
                        current_package,
                        current_module_path,
                        crate_paths_include_package,
                        imports,
                        type_index,
                    ),
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
        current_module_path: &[String],
        crate_paths_include_package: bool,
        imports: &HashSet<String>,
        type_index: &TypeIndex,
    ) -> hir::TraitDef {
        let name = full_def_name_in_module(current_package, current_module_path, &def.name.0);
        let method_sigs = def
            .method_sigs
            .iter()
            .map(|sig| hir::TraitMethodSignature {
                name: HirIdent::name(&sig.name.0),
                params: sig
                    .params
                    .iter()
                    .map(|ty| {
                        self.lower_type_expr(
                            ty,
                            &HashSet::new(),
                            current_package,
                            current_module_path,
                            crate_paths_include_package,
                            imports,
                            type_index,
                        )
                    })
                    .collect(),
                ret_ty: self.lower_type_expr(
                    &sig.ret_ty,
                    &HashSet::new(),
                    current_package,
                    current_module_path,
                    crate_paths_include_package,
                    imports,
                    type_index,
                ),
            })
            .collect();
        hir::TraitDef {
            attrs: def.attrs.iter().map(|a| a.into()).collect(),
            name: HirIdent::name(&name),
            method_sigs,
        }
    }

    fn lower_trait_path(&mut self, path: &ast::Path, ctx: &ResolutionContext) -> hir::Path {
        let lowered = self.lower_impl_trait_name(path, ctx);
        hir::Path::from_idents(lowered.split("::").map(|s| s.to_string()).collect())
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

            let local =
                full_def_name_in_module(ctx.current_package, ctx.current_module_path, &name);
            let local_key = module_relative_def_name(ctx.current_module_path, &name);
            let has_local = ctx.trait_index.has_trait(ctx.current_package, &local_key);
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

        let qualified = lower_type_path(
            path,
            &HashSet::new(),
            ctx.current_package,
            ctx.current_module_path,
            ctx.crate_paths_include_package,
            ctx.type_index,
        );
        if let Some(package) = &qualified.package
            && !namespace_allowed(package.as_str(), ctx.current_package, ctx.imports)
        {
            self.error(format!(
                "namespace {} not imported in namespace {}",
                package.0, ctx.current_package
            ));
        }
        qualified.display()
    }

    fn lower_extern_go(
        &mut self,
        def: &ast::ExternGo,
        current_package: &str,
        current_module_path: &[String],
        crate_paths_include_package: bool,
        imports: &HashSet<String>,
        type_index: &TypeIndex,
    ) -> hir::ExternGo {
        let name = full_def_name_in_module(current_package, current_module_path, &def.goml_name.0);
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
                        self.lower_type_expr(
                            ty,
                            &HashSet::new(),
                            current_package,
                            current_module_path,
                            crate_paths_include_package,
                            imports,
                            type_index,
                        ),
                    )
                })
                .collect(),
            ret_ty: def.ret_ty.as_ref().map(|ty| {
                self.lower_type_expr(
                    ty,
                    &HashSet::new(),
                    current_package,
                    current_module_path,
                    crate_paths_include_package,
                    imports,
                    type_index,
                )
            }),
        }
    }

    fn lower_extern_type(
        &self,
        def: &ast::ExternType,
        current_package: &str,
        current_module_path: &[String],
    ) -> hir::ExternType {
        let name = full_def_name_in_module(current_package, current_module_path, &def.goml_name.0);
        hir::ExternType {
            attrs: def.attrs.iter().map(|a| a.into()).collect(),
            package_path: def.package_path.clone(),
            go_name: def.go_name.clone(),
            goml_name: HirIdent::name(&name),
            explicit_go_name: def.explicit_go_name,
        }
    }

    fn lower_extern_builtin(
        &mut self,
        def: &ast::ExternBuiltin,
        current_package: &str,
        current_module_path: &[String],
        crate_paths_include_package: bool,
        imports: &HashSet<String>,
        type_index: &TypeIndex,
    ) -> hir::ExternBuiltin {
        let name = full_def_name_in_module(current_package, current_module_path, &def.name.0);
        let generic_bounds = def
            .generic_bounds
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
        hir::ExternBuiltin {
            attrs: def.attrs.iter().map(|a| a.into()).collect(),
            name: HirIdent::name(&name),
            generics: def.generics.iter().map(|g| HirIdent::name(&g.0)).collect(),
            generic_bounds,
            params: def
                .params
                .iter()
                .map(|(param, ty)| {
                    (
                        HirIdent::name(&param.0),
                        self.lower_type_expr(
                            ty,
                            &HashSet::new(),
                            current_package,
                            current_module_path,
                            crate_paths_include_package,
                            imports,
                            type_index,
                        ),
                    )
                })
                .collect(),
            ret_ty: def.ret_ty.as_ref().map(|ty| {
                self.lower_type_expr(
                    ty,
                    &HashSet::new(),
                    current_package,
                    current_module_path,
                    crate_paths_include_package,
                    imports,
                    type_index,
                )
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
                    &HashSet::new(),
                    ctx.current_package,
                    ctx.current_module_path,
                    ctx.crate_paths_include_package,
                    ctx.imports,
                    ctx.type_index,
                )
            }),
            astptr: param.astptr,
        }
    }
}

fn type_param_set(params: &[ast::AstIdent]) -> HashSet<String> {
    params.iter().map(|param| param.0.clone()).collect()
}

fn constructor_path(package: &str, enum_name: &str, variant: &str) -> hir::Path {
    let enum_segments = enum_name
        .split("::")
        .filter(|segment| !segment.is_empty())
        .map(str::to_string)
        .collect::<Vec<_>>();
    let enum_starts_with_package = enum_segments
        .first()
        .is_some_and(|segment| segment == package);
    if is_special_unqualified_package(package) {
        let mut segments = enum_segments;
        segments.push(variant.to_string());
        hir::Path::from_idents(segments)
    } else {
        let mut segments = Vec::new();
        if !enum_starts_with_package {
            segments.push(package.to_string());
        }
        segments.extend(enum_segments);
        segments.push(variant.to_string());
        hir::Path::from_idents(segments)
    }
}
