use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::path::PathBuf;

use diagnostics::Diagnostics;

use crate::artifact::{CoreUnit, CrateExports, InterfaceUnit};
use crate::builtins;
use crate::common::{Constructor, EnumConstructor, StructConstructor};
use crate::core;
use crate::env::{
    EnumDef, ExternFunc, FnConstraint, FnScheme, GlobalTypeEnv, ImplDef, InherentImplKey,
    StructDef, TraitDef, TraitEnv, TypeEnv, ValueEnv,
};
use crate::hir;
use crate::interface;
use crate::package_imports::ExternalImports;
use crate::pipeline::packages::{self, PackageGraph, PackageUnit};
use crate::registry::{
    ModuleCoord, Registry, ResolvedModule, ResolvedModuleGraph, SemVer, cached_registry_dir,
    resolve_dependencies, validate_registry_consistency,
};
use crate::tast::{self, TastIdent, Ty};

#[derive(Debug, Clone)]
pub struct ExternalPackageSource {
    pub logical_name: String,
    pub import_path: String,
    pub dir: PathBuf,
    pub files: Vec<PathBuf>,
}

#[derive(Debug, Clone)]
pub struct ExternalModuleArtifact {
    pub coord: ModuleCoord,
    pub version: SemVer,
    pub interface: InterfaceUnit,
    pub core: CoreUnit,
    pub namespace_interfaces: BTreeMap<String, interface::CrateInterface>,
    pub sources: BTreeMap<String, ExternalPackageSource>,
}

#[derive(Debug, Clone, Default)]
pub struct ExternalDependencyArtifacts {
    pub modules: BTreeMap<String, ExternalModuleArtifact>,
}

impl ExternalDependencyArtifacts {
    pub fn is_empty(&self) -> bool {
        self.modules.is_empty()
    }

    pub fn namespace_interfaces(&self) -> HashMap<String, interface::CrateInterface> {
        let mut interfaces = HashMap::new();
        for module in self.modules.values() {
            for (name, interface) in module.namespace_interfaces.iter() {
                interfaces.insert(name.clone(), interface.clone());
            }
        }
        interfaces
    }

    pub fn namespace_envs(&self) -> HashMap<String, GlobalTypeEnv> {
        let mut envs = HashMap::new();
        for module in self.modules.values() {
            let env = module.interface.exports.to_genv();
            for name in module.namespace_interfaces.keys() {
                envs.insert(name.clone(), env.clone());
            }
        }
        envs
    }

    pub fn namespace_names(&self) -> HashSet<String> {
        self.modules
            .values()
            .flat_map(|module| module.sources.keys().cloned())
            .collect()
    }

    pub fn import_paths(&self) -> HashMap<String, String> {
        let mut import_paths = HashMap::new();
        for module in self.modules.values() {
            for source in module.sources.values() {
                import_paths.insert(source.import_path.clone(), source.logical_name.clone());
            }
        }
        import_paths
    }

    pub fn alias_for_use_path(&self, path: &str) -> Option<String> {
        let import_paths = self.import_paths();
        if let Some(alias) = import_paths.get(path) {
            return Some(alias.clone());
        }

        let namespace_names = self.namespace_names();
        let segments = path.split("::").collect::<Vec<_>>();
        let first = segments.first()?;
        if !namespace_names.contains(*first) {
            return None;
        }

        let mut best = Some((*first).to_string());
        for end in 2..=segments.len() {
            let candidate = segments[1..end].join("::");
            if namespace_names.contains(&candidate) {
                best = Some(candidate);
            }
        }
        best
    }

    pub fn child_namespaces_for_use_namespace(&self, namespace: &str) -> BTreeSet<String> {
        let import_paths = self.import_paths();
        let source_namespace = import_paths
            .iter()
            .find_map(|(source, alias)| (alias == namespace).then_some(source.clone()))
            .or_else(|| {
                let alias = self.alias_for_use_path(namespace)?;
                import_paths.iter().find_map(|(source, source_alias)| {
                    (source_alias == &alias).then_some(source.clone())
                })
            })
            .or_else(|| {
                import_paths
                    .contains_key(namespace)
                    .then_some(namespace.to_string())
            });

        let Some(source_namespace) = source_namespace else {
            return BTreeSet::new();
        };

        let prefix = format!("{source_namespace}::");
        import_paths
            .keys()
            .filter_map(|source| {
                let rest = source.strip_prefix(&prefix)?;
                let child = rest.split("::").next()?;
                (!child.is_empty()).then(|| child.to_string())
            })
            .collect()
    }

    pub fn external_imports(&self) -> ExternalImports {
        ExternalImports::new(self.namespace_names(), self.import_paths())
    }

    pub fn namespace_dirs(&self) -> HashMap<String, PathBuf> {
        let mut dirs = HashMap::new();
        for module in self.modules.values() {
            for (logical_name, source) in module.sources.iter() {
                dirs.insert(logical_name.clone(), source.dir.clone());
            }
        }
        dirs
    }

    pub fn namespace_sources(&self) -> HashMap<String, Vec<PathBuf>> {
        let mut files = HashMap::new();
        for module in self.modules.values() {
            for (logical_name, source) in module.sources.iter() {
                files.insert(logical_name.clone(), source.files.clone());
            }
        }
        files
    }

    pub fn augment_graph(&self, graph: &mut PackageGraph) -> Result<(), String> {
        let mut seen = HashMap::new();
        for module in self.modules.values() {
            for source in module.sources.values() {
                if let Some(existing) = seen.insert(
                    source.logical_name.clone(),
                    (source.import_path.clone(), source.dir.clone()),
                ) {
                    return Err(format!(
                        "external namespace alias {} is ambiguous between {} and {}",
                        source.logical_name, existing.0, source.import_path
                    ));
                }
            }
        }

        for (namespace, (_, dir)) in seen {
            if graph.packages.contains_key(&namespace) {
                return Err(format!(
                    "namespace name {} conflicts with external dependency namespace {}",
                    namespace, namespace
                ));
            }
            graph.add_external_root_namespace(namespace.clone());
            graph.add_external_namespace_dir(namespace, dir);
        }
        Ok(())
    }
}

#[derive(Clone)]
struct CompiledPackage {
    exports: CrateExports,
    interface: InterfaceUnit,
    core: CoreUnit,
}

pub fn resolve_dependency_versions(
    dependencies: &BTreeMap<String, String>,
) -> Result<ExternalDependencyArtifacts, String> {
    if dependencies.is_empty() {
        return Ok(ExternalDependencyArtifacts::default());
    }

    let cache_dir = cached_registry_dir()?;
    if !cache_dir.exists() {
        return Err(format!(
            "registry cache not found at {}; run `goml update` first",
            cache_dir.display()
        ));
    }

    let registry = Registry::load(&cache_dir)?;
    validate_registry_consistency(&registry)?;
    resolve_dependency_versions_with_registry(dependencies, &registry)
}

pub fn resolve_dependency_versions_with_registry(
    dependencies: &BTreeMap<String, String>,
    registry: &Registry,
) -> Result<ExternalDependencyArtifacts, String> {
    if dependencies.is_empty() {
        return Ok(ExternalDependencyArtifacts::default());
    }

    let resolved = resolve_dependencies(registry, dependencies)?;
    let roots = unique_module_roots(&resolved)?;
    let order = topo_sort_modules(&resolved)?;
    let mut compiled = BTreeMap::new();

    for coord in order {
        let module = resolved
            .modules
            .get(&coord)
            .ok_or_else(|| format!("missing resolved module {}", coord.display()))?;
        let root_package = roots
            .get(&coord)
            .cloned()
            .ok_or_else(|| format!("missing module root name for {}", coord.display()))?;
        let artifact = compile_external_module(module, &root_package, &compiled)?;
        ensure_no_external_namespace_conflicts(&compiled, &artifact)?;
        compiled.insert(root_package, artifact);
    }

    Ok(ExternalDependencyArtifacts { modules: compiled })
}

fn unique_module_roots(
    resolved: &ResolvedModuleGraph,
) -> Result<BTreeMap<ModuleCoord, String>, String> {
    let mut roots = BTreeMap::new();
    let mut owners = HashMap::new();

    for coord in resolved.modules.keys() {
        let root = coord.module.clone();
        if let Some(existing) = owners.insert(root.clone(), coord.display()) {
            return Err(format!(
                "external dependency module name {} is ambiguous between {} and {}",
                root,
                existing,
                coord.display()
            ));
        }
        roots.insert(coord.clone(), root);
    }

    Ok(roots)
}

fn topo_sort_modules(resolved: &ResolvedModuleGraph) -> Result<Vec<ModuleCoord>, String> {
    let mut indeg = BTreeMap::<ModuleCoord, usize>::new();
    let mut edges = BTreeMap::<ModuleCoord, Vec<ModuleCoord>>::new();

    for coord in resolved.modules.keys() {
        indeg.entry(coord.clone()).or_insert(0);
        edges.entry(coord.clone()).or_default();
    }

    for (coord, module) in resolved.modules.iter() {
        for dep in module.manifest.dependency_versions().keys() {
            let dep_coord = ModuleCoord::parse(dep)?;
            if !resolved.modules.contains_key(&dep_coord) {
                continue;
            }
            edges
                .entry(dep_coord.clone())
                .or_default()
                .push(coord.clone());
            *indeg.entry(coord.clone()).or_insert(0) += 1;
        }
    }

    let mut ready = indeg
        .iter()
        .filter_map(|(coord, degree)| (*degree == 0).then_some(coord.clone()))
        .collect::<BTreeSet<_>>();
    let mut order = Vec::new();

    while let Some(coord) = ready.pop_first() {
        order.push(coord.clone());
        if let Some(nexts) = edges.get(&coord) {
            for next in nexts {
                if let Some(degree) = indeg.get_mut(next) {
                    *degree -= 1;
                    if *degree == 0 {
                        ready.insert(next.clone());
                    }
                }
            }
        }
    }

    if order.len() != resolved.modules.len() {
        return Err("module dependency cycle detected in registry dependencies".to_string());
    }

    Ok(order)
}

fn compile_external_module(
    module: &ResolvedModule,
    root_package: &str,
    compiled_roots: &BTreeMap<String, ExternalModuleArtifact>,
) -> Result<ExternalModuleArtifact, String> {
    validate_external_module_manifest(module, root_package)?;

    let available_imports = external_imports_from_modules(compiled_roots);
    let mut graph = packages::discover_dependency_crate_packages_with_external_imports(
        &module.root_dir,
        root_package,
        &available_imports,
    )
    .map_err(err_text)?;
    for namespace in available_imports.package_names.iter() {
        if graph.packages.contains_key(namespace) {
            return Err(format!(
                "namespace name {} in {} conflicts with external dependency namespace {}",
                namespace,
                module.root_dir.display(),
                namespace
            ));
        }
        graph.add_external_root_namespace(namespace.clone());
    }

    let order = packages::topo_sort_packages(&graph).map_err(err_text)?;
    let mut compiled_packages = HashMap::<String, CompiledPackage>::new();

    for package_name in order {
        let package = graph.packages.get(&package_name).ok_or_else(|| {
            format!(
                "missing package {} in {}",
                package_name,
                module.coord.display()
            )
        })?;
        let compiled = compile_module_package(package, &compiled_packages, compiled_roots)?;
        compiled_packages.insert(package_name, compiled);
    }

    let logical_names = graph
        .packages
        .keys()
        .map(|package| (package.clone(), logical_package_name(root_package, package)))
        .collect::<HashMap<_, _>>();
    let mut seen_logical_names = HashSet::new();
    for logical_name in logical_names.values() {
        if !seen_logical_names.insert(logical_name.clone()) {
            return Err(format!(
                "external dependency {} defines duplicate package alias {}",
                module.coord.display(),
                logical_name
            ));
        }
    }

    let mut merged_exports = empty_exports();
    let mut public_exports = empty_exports();
    let mut merged_core = core::File {
        toplevels: Vec::new(),
    };
    let mut merged_sources = BTreeSet::new();
    let mut merged_deps = BTreeMap::new();
    let local_packages = logical_names.values().cloned().collect::<HashSet<_>>();
    let mut sources = BTreeMap::new();

    let mut package_names = graph.packages.keys().cloned().collect::<Vec<_>>();
    package_names.sort();
    for package_name in package_names {
        let compiled = compiled_packages
            .get(&package_name)
            .ok_or_else(|| format!("missing compiled package {}", package_name))?;
        let logical_name = logical_names
            .get(&package_name)
            .cloned()
            .ok_or_else(|| format!("missing logical package {}", package_name))?;
        let publicly_visible = graph.namespace_is_publicly_visible(&package_name);
        let transformed_exports = rename_exports(&compiled.exports, &logical_names);
        merge_exports(&mut merged_exports, &transformed_exports);
        if publicly_visible {
            merge_exports(&mut public_exports, &transformed_exports);
        }

        let transformed_core = rename_core_file(&compiled.core.core_ir, &logical_names);
        merged_core.toplevels.extend(transformed_core.toplevels);

        for source in compiled.core.sources.iter() {
            merged_sources.insert(source.clone());
        }
        for (dep, hash) in compiled.interface.deps.iter() {
            let renamed = rename_package_key(dep, &logical_names);
            if local_packages.contains(&renamed) {
                continue;
            }
            match merged_deps.get(&renamed) {
                Some(existing) if existing != hash => {
                    return Err(format!(
                        "dependency hash mismatch while merging {}: {} has both {} and {}",
                        module.coord.display(),
                        renamed,
                        existing,
                        hash
                    ));
                }
                Some(_) => {}
                None => {
                    merged_deps.insert(renamed, hash.clone());
                }
            }
        }

        let package_dir = graph
            .package_dirs
            .get(&package_name)
            .cloned()
            .ok_or_else(|| format!("missing package dir for {}", package_name))?;
        let files = graph
            .packages
            .get(&package_name)
            .ok_or_else(|| format!("missing package files for {}", package_name))?
            .files
            .iter()
            .map(|file| file.path.clone())
            .collect::<Vec<_>>();
        if publicly_visible {
            sources.insert(
                logical_name.clone(),
                ExternalPackageSource {
                    logical_name,
                    import_path: external_import_path(
                        &module.coord.owner,
                        root_package,
                        &package_name,
                    ),
                    dir: package_dir,
                    files,
                },
            );
        }
    }

    let mut namespace_interfaces = BTreeMap::new();
    for source in sources.values() {
        let mut namespace_interface =
            interface::CrateInterface::from_exports(&source.logical_name, &public_exports);
        namespace_interface.packages = std::iter::once(source.import_path.clone()).collect();
        namespace_interfaces.insert(source.logical_name.clone(), namespace_interface);
    }
    let root_interface = namespace_interfaces
        .get(root_package)
        .cloned()
        .ok_or_else(|| format!("missing root namespace interface for {}", root_package))?;

    let interface = InterfaceUnit::new(
        root_package.to_string(),
        public_exports,
        root_interface,
        merged_deps.clone(),
    );
    let core = CoreUnit {
        format_version: crate::artifact::FORMAT_VERSION,
        compiler_abi: crate::artifact::COMPILER_ABI,
        package: root_package.to_string(),
        interface: interface.clone(),
        internal_exports: Some(merged_exports),
        core_ir: merged_core,
        deps: merged_deps,
        sources: merged_sources.into_iter().collect(),
    };

    Ok(ExternalModuleArtifact {
        coord: module.coord.clone(),
        version: module.version.clone(),
        interface,
        core,
        namespace_interfaces,
        sources,
    })
}

fn validate_external_module_manifest(
    module: &ResolvedModule,
    root_package: &str,
) -> Result<(), String> {
    let Some(crate_config) = module.manifest.crate_config.as_ref() else {
        return Err(format!(
            "registry module {}@{} must declare [crate] in {}",
            module.coord.display(),
            module.version.display(),
            module.manifest_path.display()
        ));
    };
    if crate_config.name != root_package {
        return Err(format!(
            "registry module {}@{} must declare crate.name = {:?} in {}",
            module.coord.display(),
            module.version.display(),
            root_package,
            module.manifest_path.display()
        ));
    }
    Ok(())
}

fn compile_module_package(
    package: &PackageUnit,
    local_packages: &HashMap<String, CompiledPackage>,
    external_roots: &BTreeMap<String, ExternalModuleArtifact>,
) -> Result<CompiledPackage, String> {
    let package_id = interface::package_id_for_name(&package.name);
    let mut deps_envs = HashMap::new();
    let mut deps_interfaces = HashMap::new();
    let mut dep_hashes = BTreeMap::new();
    let mut compile_env = builtins::builtin_env();

    if package.name != crate::package_names::BUILTIN_PACKAGE {
        dep_hashes.insert(
            crate::package_names::BUILTIN_PACKAGE.to_string(),
            builtins::builtin_interface_hash(),
        );
    }

    let mut deps = package.imports.iter().cloned().collect::<Vec<_>>();
    deps.sort();
    deps.dedup();

    for dep in deps {
        if dep == crate::package_names::BUILTIN_PACKAGE || dep == package.name {
            continue;
        }
        if let Some(local) = local_packages.get(&dep) {
            deps_envs.insert(dep.clone(), local.interface.exports.to_genv());
            deps_interfaces.insert(dep.clone(), local.interface.interface.clone());
            dep_hashes.insert(dep.clone(), local.interface.interface_hash.clone());
            local.interface.exports.apply_to(&mut compile_env);
            continue;
        }
        if let Some((external, namespace_interface)) = find_external_namespace(external_roots, &dep)
        {
            deps_envs.insert(dep.clone(), external.interface.exports.to_genv());
            deps_interfaces.insert(dep.clone(), namespace_interface.clone());
            dep_hashes.insert(dep.clone(), external.interface.interface_hash.clone());
            external.interface.exports.apply_to(&mut compile_env);
            continue;
        }
        return Err(format!(
            "namespace {} imports missing dependency {}",
            package.name, dep
        ));
    }

    let (hir, hir_table, mut hir_diagnostics) =
        hir::lower_to_hir_files_with_env(package_id, package.files.clone(), &deps_interfaces);
    let (tast, genv, mut diagnostics) = crate::typer::check_file_with_env(
        hir,
        hir_table,
        GlobalTypeEnv::new(),
        builtins::builtin_env(),
        &package.name,
        deps_envs,
    );
    diagnostics.append(&mut hir_diagnostics);
    if diagnostics.has_errors() {
        return Err(diagnostics_text(&diagnostics));
    }

    let full_exports = CrateExports::from_genv(&genv);
    let exports = CrateExports::public_from_package(&package.name, &package.files, &genv);
    let package_interface = interface::CrateInterface::from_exports(&package.name, &exports);
    let interface = InterfaceUnit::new(
        package.name.clone(),
        exports.clone(),
        package_interface,
        dep_hashes,
    );

    full_exports.apply_to(&mut compile_env);
    let gensym = crate::env::Gensym::new();
    let mut compile_diagnostics = Diagnostics::new();
    let core_ir =
        crate::compile_match::compile_file(&compile_env, &gensym, &mut compile_diagnostics, &tast);
    if compile_diagnostics.has_errors() {
        return Err(diagnostics_text(&compile_diagnostics));
    }

    let mut core = CoreUnit::new(package.name.clone(), interface.clone(), core_ir);
    core.sources = package
        .files
        .iter()
        .map(|file| file.path.display().to_string())
        .collect();

    Ok(CompiledPackage {
        exports,
        interface,
        core,
    })
}

fn external_import_path(owner: &str, module: &str, package: &str) -> String {
    if package == module {
        format!("{owner}::{module}")
    } else {
        format!("{owner}::{module}::{package}")
    }
}

fn logical_package_name(root_package: &str, package: &str) -> String {
    if package == root_package {
        root_package.to_string()
    } else {
        package.to_string()
    }
}

fn external_imports_from_modules(
    modules: &BTreeMap<String, ExternalModuleArtifact>,
) -> ExternalImports {
    let mut package_names = HashSet::new();
    let mut import_paths = HashMap::new();
    for module in modules.values() {
        for source in module.sources.values() {
            package_names.insert(source.logical_name.clone());
            import_paths.insert(source.import_path.clone(), source.logical_name.clone());
        }
    }
    ExternalImports::new(package_names, import_paths)
}

fn ensure_no_external_namespace_conflicts(
    compiled: &BTreeMap<String, ExternalModuleArtifact>,
    candidate: &ExternalModuleArtifact,
) -> Result<(), String> {
    for existing in compiled.values() {
        for source in existing.sources.values() {
            if let Some(other) = candidate.sources.get(&source.logical_name) {
                return Err(format!(
                    "external namespace alias {} is ambiguous between {} and {}",
                    source.logical_name, source.import_path, other.import_path
                ));
            }
        }
    }
    Ok(())
}

fn find_external_namespace<'a>(
    external_roots: &'a BTreeMap<String, ExternalModuleArtifact>,
    namespace: &str,
) -> Option<(&'a ExternalModuleArtifact, &'a interface::CrateInterface)> {
    for module in external_roots.values() {
        if let Some(namespace_interface) = module.namespace_interfaces.get(namespace) {
            return Some((module, namespace_interface));
        }
    }
    None
}

fn rename_package_key(name: &str, package_names: &HashMap<String, String>) -> String {
    package_names
        .get(name)
        .cloned()
        .unwrap_or_else(|| name.to_string())
}

fn rename_symbol_name(name: &str, package_names: &HashMap<String, String>) -> String {
    for (old, new) in package_names {
        if name == old {
            return new.clone();
        }
        let prefix = format!("{old}::");
        if let Some(rest) = name.strip_prefix(&prefix) {
            return format!("{new}::{rest}");
        }
    }
    name.to_string()
}

fn rename_global_ref_name(name: &str, package_names: &HashMap<String, String>) -> String {
    if name.contains("::") {
        rename_symbol_name(name, package_names)
    } else {
        name.to_string()
    }
}

fn rename_tast_ident(ident: &TastIdent, package_names: &HashMap<String, String>) -> TastIdent {
    TastIdent(rename_symbol_name(&ident.0, package_names))
}

fn rename_ty(ty: &Ty, package_names: &HashMap<String, String>) -> Ty {
    match ty {
        Ty::TVar(var) => Ty::TVar(*var),
        Ty::TUnit => Ty::TUnit,
        Ty::TBool => Ty::TBool,
        Ty::TInt8 => Ty::TInt8,
        Ty::TInt16 => Ty::TInt16,
        Ty::TInt32 => Ty::TInt32,
        Ty::TInt64 => Ty::TInt64,
        Ty::TUint8 => Ty::TUint8,
        Ty::TUint16 => Ty::TUint16,
        Ty::TUint32 => Ty::TUint32,
        Ty::TUint64 => Ty::TUint64,
        Ty::TFloat32 => Ty::TFloat32,
        Ty::TFloat64 => Ty::TFloat64,
        Ty::TString => Ty::TString,
        Ty::TChar => Ty::TChar,
        Ty::TTuple { typs } => Ty::TTuple {
            typs: typs.iter().map(|ty| rename_ty(ty, package_names)).collect(),
        },
        Ty::TEnum { name } => Ty::TEnum {
            name: rename_symbol_name(name, package_names),
        },
        Ty::TStruct { name } => Ty::TStruct {
            name: rename_symbol_name(name, package_names),
        },
        Ty::TDyn { trait_name } => Ty::TDyn {
            trait_name: rename_symbol_name(trait_name, package_names),
        },
        Ty::TApp { ty, args } => Ty::TApp {
            ty: Box::new(rename_ty(ty, package_names)),
            args: args
                .iter()
                .map(|arg| rename_ty(arg, package_names))
                .collect(),
        },
        Ty::TArray { len, elem } => Ty::TArray {
            len: *len,
            elem: Box::new(rename_ty(elem, package_names)),
        },
        Ty::TSlice { elem } => Ty::TSlice {
            elem: Box::new(rename_ty(elem, package_names)),
        },
        Ty::TVec { elem } => Ty::TVec {
            elem: Box::new(rename_ty(elem, package_names)),
        },
        Ty::TRef { elem } => Ty::TRef {
            elem: Box::new(rename_ty(elem, package_names)),
        },
        Ty::THashMap { key, value } => Ty::THashMap {
            key: Box::new(rename_ty(key, package_names)),
            value: Box::new(rename_ty(value, package_names)),
        },
        Ty::TParam { name } => Ty::TParam { name: name.clone() },
        Ty::TFunc { params, ret_ty } => Ty::TFunc {
            params: params
                .iter()
                .map(|param| rename_ty(param, package_names))
                .collect(),
            ret_ty: Box::new(rename_ty(ret_ty, package_names)),
        },
    }
}

fn rename_constraint(
    constraint: &FnConstraint,
    package_names: &HashMap<String, String>,
) -> FnConstraint {
    FnConstraint {
        type_param: constraint.type_param.clone(),
        trait_name: rename_tast_ident(&constraint.trait_name, package_names),
    }
}

fn rename_fn_scheme(scheme: &FnScheme, package_names: &HashMap<String, String>) -> FnScheme {
    FnScheme {
        type_params: scheme.type_params.clone(),
        constraints: scheme
            .constraints
            .iter()
            .map(|constraint| rename_constraint(constraint, package_names))
            .collect(),
        ty: rename_ty(&scheme.ty, package_names),
        origin: scheme.origin,
    }
}

fn rename_impl_def(def: &ImplDef, package_names: &HashMap<String, String>) -> ImplDef {
    ImplDef {
        params: def.params.clone(),
        methods: def
            .methods
            .iter()
            .map(|(name, scheme)| (name.clone(), rename_fn_scheme(scheme, package_names)))
            .collect(),
    }
}

fn rename_exports(exports: &CrateExports, package_names: &HashMap<String, String>) -> CrateExports {
    let enums = exports
        .type_env
        .enums
        .iter()
        .map(|(name, def)| {
            (
                rename_tast_ident(name, package_names),
                EnumDef {
                    name: rename_tast_ident(&def.name, package_names),
                    generics: def.generics.clone(),
                    variants: def
                        .variants
                        .iter()
                        .map(|(variant, fields)| {
                            (
                                variant.clone(),
                                fields
                                    .iter()
                                    .map(|field| rename_ty(field, package_names))
                                    .collect(),
                            )
                        })
                        .collect(),
                },
            )
        })
        .collect();
    let structs = exports
        .type_env
        .structs
        .iter()
        .map(|(name, def)| {
            (
                rename_tast_ident(name, package_names),
                StructDef {
                    name: rename_tast_ident(&def.name, package_names),
                    generics: def.generics.clone(),
                    fields: def
                        .fields
                        .iter()
                        .map(|(field, ty)| (field.clone(), rename_ty(ty, package_names)))
                        .collect(),
                },
            )
        })
        .collect();
    let extern_types = exports
        .type_env
        .extern_types
        .iter()
        .map(|(name, def)| (rename_symbol_name(name, package_names), def.clone()))
        .collect();
    let trait_defs = exports
        .trait_env
        .trait_defs
        .iter()
        .map(|(name, def)| {
            (
                rename_symbol_name(name, package_names),
                TraitDef {
                    methods: def
                        .methods
                        .iter()
                        .map(|(method, scheme)| {
                            (method.clone(), rename_fn_scheme(scheme, package_names))
                        })
                        .collect(),
                },
            )
        })
        .collect();
    let trait_impls = exports
        .trait_env
        .trait_impls
        .iter()
        .map(|((trait_name, ty), def)| {
            (
                (
                    rename_symbol_name(trait_name, package_names),
                    rename_ty(ty, package_names),
                ),
                rename_impl_def(def, package_names),
            )
        })
        .collect();
    let inherent_impls = exports
        .trait_env
        .inherent_impls
        .iter()
        .map(|(key, def)| {
            let renamed = match key {
                InherentImplKey::Exact(ty) => InherentImplKey::Exact(rename_ty(ty, package_names)),
                InherentImplKey::Constr(name) => {
                    InherentImplKey::Constr(rename_symbol_name(name, package_names))
                }
            };
            (renamed, rename_impl_def(def, package_names))
        })
        .collect();
    let funcs = exports
        .value_env
        .funcs
        .iter()
        .map(|(name, scheme)| {
            (
                rename_symbol_name(name, package_names),
                rename_fn_scheme(scheme, package_names),
            )
        })
        .collect();
    let extern_funcs = exports
        .value_env
        .extern_funcs
        .iter()
        .map(|(name, func)| {
            (
                rename_symbol_name(name, package_names),
                ExternFunc {
                    package_path: func.package_path.clone(),
                    go_name: func.go_name.clone(),
                    ty: rename_ty(&func.ty, package_names),
                    binding_mode: func.binding_mode,
                    return_mode: func.return_mode,
                    variadic_last: func.variadic_last,
                    field_name: func.field_name.clone(),
                },
            )
        })
        .collect();

    CrateExports {
        type_env: TypeEnv {
            enums,
            structs,
            extern_types,
        },
        trait_env: TraitEnv {
            trait_defs,
            trait_impls,
            inherent_impls,
        },
        value_env: ValueEnv {
            funcs,
            extern_funcs,
        },
    }
}

fn rename_constructor(
    constructor: &Constructor,
    package_names: &HashMap<String, String>,
) -> Constructor {
    match constructor {
        Constructor::Enum(constructor) => Constructor::Enum(EnumConstructor {
            type_name: rename_tast_ident(&constructor.type_name, package_names),
            variant: constructor.variant.clone(),
            index: constructor.index,
        }),
        Constructor::Struct(constructor) => Constructor::Struct(StructConstructor {
            type_name: rename_tast_ident(&constructor.type_name, package_names),
        }),
    }
}

fn rename_expr(expr: &core::Expr, package_names: &HashMap<String, String>) -> core::Expr {
    match expr {
        core::Expr::EVar { name, ty } => core::Expr::EVar {
            name: rename_global_ref_name(name, package_names),
            ty: rename_ty(ty, package_names),
        },
        core::Expr::EPrim { value, ty } => core::Expr::EPrim {
            value: value.clone(),
            ty: rename_ty(ty, package_names),
        },
        core::Expr::EConstr {
            constructor,
            args,
            ty,
        } => core::Expr::EConstr {
            constructor: rename_constructor(constructor, package_names),
            args: args
                .iter()
                .map(|arg| rename_expr(arg, package_names))
                .collect(),
            ty: rename_ty(ty, package_names),
        },
        core::Expr::ETuple { items, ty } => core::Expr::ETuple {
            items: items
                .iter()
                .map(|item| rename_expr(item, package_names))
                .collect(),
            ty: rename_ty(ty, package_names),
        },
        core::Expr::EArray { items, ty } => core::Expr::EArray {
            items: items
                .iter()
                .map(|item| rename_expr(item, package_names))
                .collect(),
            ty: rename_ty(ty, package_names),
        },
        core::Expr::EClosure { params, body, ty } => core::Expr::EClosure {
            params: params
                .iter()
                .map(|param| tast::ClosureParam {
                    name: param.name.clone(),
                    ty: rename_ty(&param.ty, package_names),
                    astptr: param.astptr,
                })
                .collect(),
            body: Box::new(rename_expr(body, package_names)),
            ty: rename_ty(ty, package_names),
        },
        core::Expr::EBlock { block, ty } => core::Expr::EBlock {
            block: Box::new(rename_block(block, package_names)),
            ty: rename_ty(ty, package_names),
        },
        core::Expr::EMatch {
            expr,
            arms,
            default,
            ty,
        } => core::Expr::EMatch {
            expr: Box::new(rename_expr(expr, package_names)),
            arms: arms
                .iter()
                .map(|arm| core::Arm {
                    lhs: rename_expr(&arm.lhs, package_names),
                    body: rename_expr(&arm.body, package_names),
                })
                .collect(),
            default: default
                .as_ref()
                .map(|default| Box::new(rename_expr(default, package_names))),
            ty: rename_ty(ty, package_names),
        },
        core::Expr::EIf {
            cond,
            then_branch,
            else_branch,
            ty,
        } => core::Expr::EIf {
            cond: Box::new(rename_expr(cond, package_names)),
            then_branch: Box::new(rename_expr(then_branch, package_names)),
            else_branch: Box::new(rename_expr(else_branch, package_names)),
            ty: rename_ty(ty, package_names),
        },
        core::Expr::EWhile { cond, body, ty } => core::Expr::EWhile {
            cond: Box::new(rename_expr(cond, package_names)),
            body: Box::new(rename_expr(body, package_names)),
            ty: rename_ty(ty, package_names),
        },
        core::Expr::EBreak { ty } => core::Expr::EBreak {
            ty: rename_ty(ty, package_names),
        },
        core::Expr::EContinue { ty } => core::Expr::EContinue {
            ty: rename_ty(ty, package_names),
        },
        core::Expr::EReturn { expr, ty } => core::Expr::EReturn {
            expr: expr
                .as_ref()
                .map(|expr| Box::new(rename_expr(expr, package_names))),
            ty: rename_ty(ty, package_names),
        },
        core::Expr::EGo { expr, ty } => core::Expr::EGo {
            expr: Box::new(rename_expr(expr, package_names)),
            ty: rename_ty(ty, package_names),
        },
        core::Expr::EConstrGet {
            expr,
            constructor,
            field_index,
            ty,
        } => core::Expr::EConstrGet {
            expr: Box::new(rename_expr(expr, package_names)),
            constructor: rename_constructor(constructor, package_names),
            field_index: *field_index,
            ty: rename_ty(ty, package_names),
        },
        core::Expr::EUnary { op, expr, ty } => core::Expr::EUnary {
            op: *op,
            expr: Box::new(rename_expr(expr, package_names)),
            ty: rename_ty(ty, package_names),
        },
        core::Expr::EBinary { op, lhs, rhs, ty } => core::Expr::EBinary {
            op: *op,
            lhs: Box::new(rename_expr(lhs, package_names)),
            rhs: Box::new(rename_expr(rhs, package_names)),
            ty: rename_ty(ty, package_names),
        },
        core::Expr::EAssign {
            name,
            value,
            target_ty,
            ty,
        } => core::Expr::EAssign {
            name: name.clone(),
            value: Box::new(rename_expr(value, package_names)),
            target_ty: rename_ty(target_ty, package_names),
            ty: rename_ty(ty, package_names),
        },
        core::Expr::ECall { func, args, ty } => core::Expr::ECall {
            func: Box::new(rename_expr(func, package_names)),
            args: args
                .iter()
                .map(|arg| rename_expr(arg, package_names))
                .collect(),
            ty: rename_ty(ty, package_names),
        },
        core::Expr::EToDyn {
            trait_name,
            for_ty,
            expr,
            ty,
        } => core::Expr::EToDyn {
            trait_name: rename_tast_ident(trait_name, package_names),
            for_ty: rename_ty(for_ty, package_names),
            expr: Box::new(rename_expr(expr, package_names)),
            ty: rename_ty(ty, package_names),
        },
        core::Expr::EDynCall {
            trait_name,
            method_name,
            receiver,
            args,
            ty,
        } => core::Expr::EDynCall {
            trait_name: rename_tast_ident(trait_name, package_names),
            method_name: method_name.clone(),
            receiver: Box::new(rename_expr(receiver, package_names)),
            args: args
                .iter()
                .map(|arg| rename_expr(arg, package_names))
                .collect(),
            ty: rename_ty(ty, package_names),
        },
        core::Expr::ETraitCall {
            trait_name,
            method_name,
            receiver,
            args,
            ty,
        } => core::Expr::ETraitCall {
            trait_name: rename_tast_ident(trait_name, package_names),
            method_name: method_name.clone(),
            receiver: Box::new(rename_expr(receiver, package_names)),
            args: args
                .iter()
                .map(|arg| rename_expr(arg, package_names))
                .collect(),
            ty: rename_ty(ty, package_names),
        },
        core::Expr::EProj { tuple, index, ty } => core::Expr::EProj {
            tuple: Box::new(rename_expr(tuple, package_names)),
            index: *index,
            ty: rename_ty(ty, package_names),
        },
    }
}

fn rename_block(block: &core::Block, package_names: &HashMap<String, String>) -> core::Block {
    core::Block {
        stmts: block
            .stmts
            .iter()
            .map(|stmt| core::LetStmt {
                name: stmt.name.clone(),
                value: rename_expr(&stmt.value, package_names),
                ty: rename_ty(&stmt.ty, package_names),
            })
            .collect(),
        tail: block
            .tail
            .as_ref()
            .map(|tail| Box::new(rename_expr(tail, package_names))),
    }
}

fn rename_core_file(file: &core::File, package_names: &HashMap<String, String>) -> core::File {
    core::File {
        toplevels: file
            .toplevels
            .iter()
            .map(|func| core::Fn {
                name: rename_symbol_name(&func.name, package_names),
                generics: func.generics.clone(),
                params: func
                    .params
                    .iter()
                    .map(|(name, ty)| (name.clone(), rename_ty(ty, package_names)))
                    .collect(),
                ret_ty: rename_ty(&func.ret_ty, package_names),
                body: rename_block(&func.body, package_names),
            })
            .collect(),
    }
}

fn empty_exports() -> CrateExports {
    CrateExports {
        type_env: TypeEnv::new(),
        trait_env: TraitEnv::new(),
        value_env: ValueEnv::new(),
    }
}

fn merge_exports(into: &mut CrateExports, from: &CrateExports) {
    for (name, def) in from.type_env.enums.iter() {
        into.type_env.enums.insert(name.clone(), def.clone());
    }
    for (name, def) in from.type_env.structs.iter() {
        into.type_env.structs.insert(name.clone(), def.clone());
    }
    for (name, def) in from.type_env.extern_types.iter() {
        into.type_env.extern_types.insert(name.clone(), def.clone());
    }
    for (name, def) in from.trait_env.trait_defs.iter() {
        into.trait_env.trait_defs.insert(name.clone(), def.clone());
    }
    for (key, def) in from.trait_env.trait_impls.iter() {
        into.trait_env.trait_impls.insert(key.clone(), def.clone());
    }
    for (key, def) in from.trait_env.inherent_impls.iter() {
        into.trait_env
            .inherent_impls
            .insert(key.clone(), def.clone());
    }
    for (name, scheme) in from.value_env.funcs.iter() {
        into.value_env.funcs.insert(name.clone(), scheme.clone());
    }
    for (name, func) in from.value_env.extern_funcs.iter() {
        into.value_env
            .extern_funcs
            .insert(name.clone(), func.clone());
    }
}

fn diagnostics_text(diagnostics: &Diagnostics) -> String {
    diagnostics
        .iter()
        .map(|diagnostic| diagnostic.message().to_string())
        .collect::<Vec<_>>()
        .join("\n")
}

fn err_text(err: crate::pipeline::pipeline::CompilationError) -> String {
    diagnostics_text(err.diagnostics())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write_registry_module(root: &std::path::Path, owner: &str, module: &str) {
        let dir = root.join(owner).join(module).join("1.0.0");
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(
            dir.join("goml.toml"),
            format!(
                r#"[crate]
name = "{module}"
kind = "lib"
root = "lib.gom"
"#
            ),
        )
        .unwrap();
    }

    #[test]
    fn rejects_duplicate_external_module_names() {
        let dir = tempfile::tempdir().unwrap();
        write_registry_module(dir.path(), "alice", "util");
        write_registry_module(dir.path(), "bob", "util");
        std::fs::write(
            dir.path().join("index.toml"),
            r#"[modules."alice::util"]
latest = "1.0.0"
versions = ["1.0.0"]

[modules."bob::util"]
latest = "1.0.0"
versions = ["1.0.0"]
"#,
        )
        .unwrap();

        let registry = Registry::load(dir.path()).unwrap();
        let mut dependencies = BTreeMap::new();
        dependencies.insert("alice::util".to_string(), "1.0.0".to_string());
        dependencies.insert("bob::util".to_string(), "1.0.0".to_string());

        let err = resolve_dependency_versions_with_registry(&dependencies, &registry).unwrap_err();
        assert_eq!(
            err,
            "external dependency module name util is ambiguous between alice::util and bob::util"
        );
    }
}
