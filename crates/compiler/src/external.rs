use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::path::PathBuf;

use diagnostics::Diagnostics;

use crate::artifact::{CoreUnit, InterfaceUnit, PackageExports};
use crate::builtins;
use crate::env::GlobalTypeEnv;
use crate::hir;
use crate::interface;
use crate::package_imports::ExternalImports;
use crate::pipeline::packages::{self, PackageGraph, PackageUnit};
use crate::registry::{
    ModuleCoord, Registry, ResolvedModule, ResolvedModuleGraph, SemVer, cached_registry_dir,
    resolve_dependencies, validate_registry_consistency,
};

#[derive(Debug, Clone)]
pub struct ExternalPackageSource {
    pub declared_name: String,
    pub dir: PathBuf,
    pub files: Vec<PathBuf>,
}

#[derive(Debug, Clone)]
pub struct ExternalPackageArtifact {
    pub interface: InterfaceUnit,
    pub core: CoreUnit,
    pub source: ExternalPackageSource,
}

#[derive(Debug, Clone)]
pub struct ExternalModuleArtifact {
    pub coord: ModuleCoord,
    pub version: SemVer,
    pub packages: BTreeMap<String, ExternalPackageArtifact>,
}

#[derive(Debug, Clone, Default)]
pub struct ExternalDependencyArtifacts {
    pub modules: BTreeMap<String, ExternalModuleArtifact>,
}

impl ExternalModuleArtifact {
    pub fn package(&self, package: &str) -> Option<&ExternalPackageArtifact> {
        self.packages.get(package)
    }
}

impl ExternalDependencyArtifacts {
    pub fn is_empty(&self) -> bool {
        self.modules.is_empty()
    }

    pub fn package(&self, package: &str) -> Option<&ExternalPackageArtifact> {
        self.modules
            .values()
            .find_map(|module| module.package(package))
    }

    pub fn package_interfaces(&self) -> HashMap<String, interface::PackageInterface> {
        self.modules
            .values()
            .flat_map(|module| {
                module
                    .packages
                    .iter()
                    .map(|(name, artifact)| (name.clone(), artifact.interface.interface.clone()))
            })
            .collect()
    }

    pub fn package_envs(&self) -> HashMap<String, GlobalTypeEnv> {
        self.modules
            .values()
            .flat_map(|module| {
                module
                    .packages
                    .iter()
                    .map(|(name, artifact)| (name.clone(), artifact.interface.exports.to_genv()))
            })
            .collect()
    }

    pub fn package_names(&self) -> HashSet<String> {
        self.modules
            .values()
            .flat_map(|module| module.packages.keys().cloned())
            .collect()
    }

    pub fn external_imports(&self) -> ExternalImports {
        ExternalImports::new(
            self.modules
                .values()
                .flat_map(|module| {
                    module.packages.iter().map(|(name, artifact)| {
                        (name.clone(), artifact.source.declared_name.clone())
                    })
                })
                .collect(),
        )
    }

    pub fn package_dirs(&self) -> HashMap<String, PathBuf> {
        self.modules
            .values()
            .flat_map(|module| {
                module
                    .packages
                    .iter()
                    .map(|(name, artifact)| (name.clone(), artifact.source.dir.clone()))
            })
            .collect()
    }

    pub fn package_sources(&self) -> HashMap<String, Vec<PathBuf>> {
        self.modules
            .values()
            .flat_map(|module| {
                module
                    .packages
                    .iter()
                    .map(|(name, artifact)| (name.clone(), artifact.source.files.clone()))
            })
            .collect()
    }

    pub fn augment_graph(&self, graph: &mut PackageGraph) -> Result<(), String> {
        for module in self.modules.values() {
            for (package, artifact) in module.packages.iter() {
                if graph.packages.contains_key(package) {
                    return Err(format!(
                        "package {} conflicts with external dependency package {}",
                        package, package
                    ));
                }
                graph.add_external_root_package(package.clone());
                graph.add_external_package(
                    package.clone(),
                    artifact.source.declared_name.clone(),
                    artifact.source.dir.clone(),
                );
            }
        }
        Ok(())
    }

    pub fn reachable_package_names(&self, graph: &PackageGraph) -> Result<HashSet<String>, String> {
        let mut reachable = HashSet::new();
        let mut pending = graph
            .packages
            .values()
            .flat_map(|package| package.imports.iter())
            .filter(|package| self.package(package).is_some())
            .cloned()
            .collect::<Vec<_>>();
        while let Some(package) = pending.pop() {
            if !reachable.insert(package.clone()) {
                continue;
            }
            let artifact = self
                .package(&package)
                .ok_or_else(|| format!("external package {} not found", package))?;
            for dependency in artifact.core.deps.keys() {
                if self.package(dependency).is_some() {
                    pending.push(dependency.clone());
                }
            }
        }
        Ok(reachable)
    }
}

#[derive(Clone)]
struct CompiledPackage {
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
    let order = topo_sort_modules(&resolved)?;
    let mut compiled = BTreeMap::new();

    for coord in order {
        let module = resolved
            .modules
            .get(&coord)
            .ok_or_else(|| format!("missing resolved module {}", coord.display()))?;
        let module_path = coord.display();
        let artifact = compile_external_module(module, &compiled)?;
        ensure_no_external_package_conflicts(&compiled, &artifact)?;
        compiled.insert(module_path, artifact);
    }

    Ok(ExternalDependencyArtifacts { modules: compiled })
}

fn topo_sort_modules(resolved: &ResolvedModuleGraph) -> Result<Vec<ModuleCoord>, String> {
    let mut indeg = BTreeMap::<ModuleCoord, usize>::new();
    let mut edges = BTreeMap::<ModuleCoord, Vec<ModuleCoord>>::new();

    for coord in resolved.modules.keys() {
        indeg.entry(coord.clone()).or_insert(0);
        edges.entry(coord.clone()).or_default();
    }

    for (coord, module) in resolved.modules.iter() {
        for dep in module.manifest.dependencies.keys() {
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

pub fn compile_std_module(root_dir: PathBuf) -> Result<ExternalModuleArtifact, String> {
    let manifest_path = root_dir.join("goml.toml");
    let manifest = crate::config::load_module_manifest(&manifest_path)?;
    if manifest.module.path != "std" {
        return Err(format!(
            "standard library at {} must declare module.path = \"std\"",
            manifest_path.display()
        ));
    }
    let module = ResolvedModule {
        coord: ModuleCoord {
            owner: "std".to_string(),
            module: "std".to_string(),
        },
        version: SemVer {
            major: 0,
            minor: 0,
            patch: 0,
        },
        manifest_path,
        root_dir,
        manifest,
    };
    compile_module_artifact(&module, &BTreeMap::new(), true)
}

fn compile_external_module(
    module: &ResolvedModule,
    compiled_roots: &BTreeMap<String, ExternalModuleArtifact>,
) -> Result<ExternalModuleArtifact, String> {
    validate_external_module_manifest(module)?;
    compile_module_artifact(module, compiled_roots, false)
}

fn compile_module_artifact(
    module: &ResolvedModule,
    compiled_roots: &BTreeMap<String, ExternalModuleArtifact>,
    allow_std_host_externs: bool,
) -> Result<ExternalModuleArtifact, String> {
    let available_imports = external_imports_from_modules(compiled_roots);
    let mut graph = packages::discover_dependency_module_packages_with_external_imports(
        &module.root_dir,
        &available_imports,
    )
    .map_err(err_text)?;
    for package in available_imports.package_names.keys() {
        graph.add_external_root_package(package.clone());
    }

    let imports_std = graph.packages.values().any(|package| {
        package
            .imports
            .iter()
            .any(|dependency| dependency == "std" || dependency.starts_with("std::"))
    });
    let mut dependency_modules = compiled_roots.clone();
    if imports_std && !allow_std_host_externs {
        dependency_modules.insert("std".to_string(), crate::stdlib::stdlib_artifact()?);
    }

    let order = packages::topo_sort_packages(&graph).map_err(err_text)?;
    let mut compiled_packages = HashMap::<String, CompiledPackage>::new();
    let mut package_artifacts = BTreeMap::new();

    for package_name in order {
        let package = graph.packages.get(&package_name).ok_or_else(|| {
            format!(
                "missing package {} in {}",
                package_name,
                module.coord.display()
            )
        })?;
        let compiled = compile_module_package(
            package,
            &compiled_packages,
            &dependency_modules,
            allow_std_host_externs,
        )?;
        let dir = graph
            .package_dirs
            .get(&package_name)
            .cloned()
            .ok_or_else(|| format!("missing package directory for {}", package_name))?;
        let files = package.files.iter().map(|file| file.path.clone()).collect();
        package_artifacts.insert(
            package_name.clone(),
            ExternalPackageArtifact {
                interface: compiled.interface.clone(),
                core: compiled.core.clone(),
                source: ExternalPackageSource {
                    declared_name: package.declared_name.clone(),
                    dir,
                    files,
                },
            },
        );
        compiled_packages.insert(package_name, compiled);
    }

    Ok(ExternalModuleArtifact {
        coord: module.coord.clone(),
        version: module.version.clone(),
        packages: package_artifacts,
    })
}

fn validate_external_module_manifest(module: &ResolvedModule) -> Result<(), String> {
    if module.manifest.module.path != module.coord.display() {
        return Err(format!(
            "registry module {}@{} must declare module.path = {:?} in {}",
            module.coord.display(),
            module.version.display(),
            module.coord.display(),
            module.manifest_path.display()
        ));
    }
    Ok(())
}

fn module_package_dependency_closure(
    package: &PackageUnit,
    local_packages: &HashMap<String, CompiledPackage>,
    external_roots: &BTreeMap<String, ExternalModuleArtifact>,
) -> Result<Vec<String>, String> {
    let mut pending = package.imports.iter().cloned().collect::<BTreeSet<_>>();
    let mut dependencies = BTreeSet::new();

    while let Some(dependency) = pending.pop_first() {
        if dependency == crate::package_names::BUILTIN_PACKAGE || dependency == package.name {
            continue;
        }
        if !dependencies.insert(dependency.clone()) {
            continue;
        }
        if let Some(local) = local_packages.get(&dependency) {
            pending.extend(local.interface.deps.keys().cloned());
            continue;
        }
        if let Some(external) = find_external_package(external_roots, &dependency) {
            pending.extend(external.interface.deps.keys().cloned());
            continue;
        }
        return Err(format!(
            "package {} imports missing dependency {}",
            package.name, dependency
        ));
    }

    Ok(dependencies.into_iter().collect())
}

fn compile_module_package(
    package: &PackageUnit,
    local_packages: &HashMap<String, CompiledPackage>,
    external_roots: &BTreeMap<String, ExternalModuleArtifact>,
    allow_std_host_externs: bool,
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

    let direct_dependencies = package.imports.iter().cloned().collect::<HashSet<_>>();
    let dependencies = module_package_dependency_closure(package, local_packages, external_roots)?;

    for dep in dependencies {
        if let Some(local) = local_packages.get(&dep) {
            deps_envs.insert(dep.clone(), local.interface.exports.to_genv());
            deps_interfaces.insert(dep.clone(), local.interface.interface.clone());
            if direct_dependencies.contains(&dep) {
                dep_hashes.insert(dep.clone(), local.interface.interface_hash.clone());
            }
            local.core.exports.apply_to(&mut compile_env);
            continue;
        }
        if let Some(external) = find_external_package(external_roots, &dep) {
            deps_envs.insert(dep.clone(), external.interface.exports.to_genv());
            deps_interfaces.insert(dep.clone(), external.interface.interface.clone());
            if direct_dependencies.contains(&dep) {
                dep_hashes.insert(dep.clone(), external.interface.interface_hash.clone());
            }
            external.core.exports.apply_to(&mut compile_env);
            continue;
        }
        return Err(format!(
            "package {} imports missing dependency {}",
            package.name, dep
        ));
    }

    let (hir, hir_table, mut hir_diagnostics) =
        hir::lower_to_hir_files_with_env(package_id, package.files.clone(), &deps_interfaces);
    let (tast, genv, mut diagnostics) = if allow_std_host_externs {
        crate::typer::check_file_with_env_allowing_std_host_externs(
            hir,
            hir_table,
            GlobalTypeEnv::new(),
            builtins::builtin_env(),
            &package.name,
            deps_envs,
        )
    } else {
        crate::typer::check_file_with_env(
            hir,
            hir_table,
            GlobalTypeEnv::new(),
            builtins::builtin_env(),
            &package.name,
            deps_envs,
        )
    };
    diagnostics.append(&mut hir_diagnostics);
    if diagnostics.has_errors() {
        return Err(diagnostics_text(&diagnostics));
    }

    let full_exports = PackageExports::from_genv(&genv);
    let exports = PackageExports::public_from_package(&package.name, &package.files, &genv);
    let package_interface =
        interface::PackageInterface::from_package(&package.name, &package.declared_name, &exports);
    let interface =
        InterfaceUnit::new(package.name.clone(), exports, package_interface, dep_hashes);

    full_exports.apply_to(&mut compile_env);
    let gensym = crate::env::Gensym::new();
    let mut compile_diagnostics = Diagnostics::new();
    let core_ir =
        crate::compile_match::compile_file(&compile_env, &gensym, &mut compile_diagnostics, &tast);
    if compile_diagnostics.has_errors() {
        return Err(diagnostics_text(&compile_diagnostics));
    }

    let mut core = CoreUnit::new(
        package.name.clone(),
        interface.clone(),
        full_exports,
        core_ir,
    );
    core.sources = package
        .files
        .iter()
        .map(|file| file.path.display().to_string())
        .collect();

    Ok(CompiledPackage { interface, core })
}

fn external_imports_from_modules(
    modules: &BTreeMap<String, ExternalModuleArtifact>,
) -> ExternalImports {
    let package_names = modules
        .values()
        .flat_map(|module| {
            module
                .packages
                .iter()
                .map(|(name, artifact)| (name.clone(), artifact.source.declared_name.clone()))
        })
        .collect::<HashMap<_, _>>();
    ExternalImports::new(package_names)
}

fn ensure_no_external_package_conflicts(
    compiled: &BTreeMap<String, ExternalModuleArtifact>,
    candidate: &ExternalModuleArtifact,
) -> Result<(), String> {
    for existing in compiled.values() {
        for package in existing.packages.keys() {
            if candidate.packages.contains_key(package) {
                return Err(format!(
                    "external package {} is provided by more than one module",
                    package
                ));
            }
        }
    }
    Ok(())
}

fn find_external_package<'a>(
    external_roots: &'a BTreeMap<String, ExternalModuleArtifact>,
    package: &str,
) -> Option<&'a ExternalPackageArtifact> {
    external_roots
        .values()
        .find_map(|module| module.package(package))
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
