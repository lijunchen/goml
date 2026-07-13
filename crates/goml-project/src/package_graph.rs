use std::collections::{BTreeSet, HashMap, HashSet};
use std::fs;
use std::path::{Component, Path, PathBuf};

use ast::ast;
use cst::cst::{CstNode, File as CstFile};
use parser::syntax::MySyntaxNode;

use crate::STD_PACKAGE;
use crate::config::{load_module_manifest, validate_module_path, validate_project_module_path};

#[derive(Debug, Clone, Default)]
pub struct ExternalImports {
    package_names: HashMap<String, String>,
}

impl ExternalImports {
    pub fn new(package_names: HashMap<String, String>) -> Self {
        Self { package_names }
    }

    pub fn contains_package(&self, package: &str) -> bool {
        self.package_names.contains_key(package)
    }

    pub fn declared_name(&self, package: &str) -> Option<&str> {
        self.package_names.get(package).map(String::as_str)
    }

    pub fn package_names(&self) -> &HashMap<String, String> {
        &self.package_names
    }
}

#[derive(Debug, Clone)]
pub struct PackageUnit {
    pub name: String,
    pub declared_name: String,
    pub files: Vec<PathBuf>,
    pub imports: HashSet<String>,
}

#[derive(Debug, Clone)]
pub struct PackageGraph {
    pub module_dir: PathBuf,
    pub module_name: String,
    pub entry_package: String,
    pub packages: HashMap<String, PackageUnit>,
    pub package_dirs: HashMap<String, PathBuf>,
    pub external_root_packages: HashSet<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProjectPathRole {
    Production,
    InternalTest,
    ExternalTest {
        target_dir: PathBuf,
        suite_dir: PathBuf,
    },
}

#[derive(Debug, Clone)]
pub struct ExternalTestGraph {
    pub target_package: String,
    pub graph: PackageGraph,
}

#[derive(Debug, Clone)]
pub struct ProjectTestPlan {
    pub normal: PackageGraph,
    pub internal: PackageGraph,
    pub external: Vec<ExternalTestGraph>,
}

const TESTS_DIRECTORY: &str = "tests";

struct ParsedSource {
    path: PathBuf,
    ast: ast::File,
}

struct PackageUseCandidate {
    default_alias: String,
    exists: bool,
}

pub fn discover_project_packages(
    module_dir: &Path,
    entry_path: &Path,
    external_imports: &ExternalImports,
) -> Result<PackageGraph, String> {
    let manifest = load_module_manifest(&module_dir.join("goml.toml"))?;
    validate_project_module_path(&manifest.module.path)?;
    let artifact_dir = module_dir.join(&manifest.build.target_dir);
    let target_dir = normalized_parent(entry_path);
    reject_test_only_target(module_dir, target_dir)?;
    discover_reachable_module_packages(
        module_dir,
        &manifest.module.path,
        target_dir,
        &artifact_dir,
        external_imports,
        false,
    )
}

pub fn discover_project_test_packages(
    module_dir: &Path,
    entry_path: &Path,
    external_imports: &ExternalImports,
) -> Result<PackageGraph, String> {
    let manifest = load_module_manifest(&module_dir.join("goml.toml"))?;
    validate_project_module_path(&manifest.module.path)?;
    let artifact_dir = module_dir.join(&manifest.build.target_dir);
    let target_dir = normalized_parent(entry_path);
    reject_test_only_target(module_dir, target_dir)?;
    discover_reachable_module_packages(
        module_dir,
        &manifest.module.path,
        target_dir,
        &artifact_dir,
        external_imports,
        true,
    )
}

pub fn discover_project_test_plan(
    module_dir: &Path,
    entry_path: &Path,
    external_imports: &ExternalImports,
) -> Result<ProjectTestPlan, String> {
    let manifest = load_module_manifest(&module_dir.join("goml.toml"))?;
    validate_project_module_path(&manifest.module.path)?;
    let artifact_dir = module_dir.join(&manifest.build.target_dir);
    let target_dir = normalized_parent(entry_path);
    reject_test_only_target(module_dir, target_dir)?;
    let normal = discover_reachable_module_packages(
        module_dir,
        &manifest.module.path,
        target_dir,
        &artifact_dir,
        external_imports,
        false,
    )?;
    let internal = discover_reachable_module_packages(
        module_dir,
        &manifest.module.path,
        target_dir,
        &artifact_dir,
        external_imports,
        true,
    )?;
    let external = discover_external_test_graphs(
        module_dir,
        &manifest.module.path,
        target_dir,
        &artifact_dir,
        &normal.entry_package,
        external_imports,
    )?;
    Ok(ProjectTestPlan {
        normal,
        internal,
        external,
    })
}

pub fn discover_project_external_test_package(
    module_dir: &Path,
    entry_path: &Path,
    suite_dir: &Path,
    external_imports: &ExternalImports,
) -> Result<ExternalTestGraph, String> {
    let manifest = load_module_manifest(&module_dir.join("goml.toml"))?;
    validate_project_module_path(&manifest.module.path)?;
    let artifact_dir = module_dir.join(&manifest.build.target_dir);
    let target_dir = normalized_parent(entry_path);
    reject_test_only_target(module_dir, target_dir)?;
    let expected_parent = target_dir.join(TESTS_DIRECTORY);
    if suite_dir.parent() != Some(expected_parent.as_path()) {
        return Err(format!(
            "black-box test suite {} must be directly under {}",
            suite_dir.display(),
            expected_parent.display()
        ));
    }
    let target_package = package_import_path(&manifest.module.path, module_dir, target_dir)?;
    let mut graph = discover_reachable_module_packages(
        module_dir,
        &manifest.module.path,
        suite_dir,
        &artifact_dir,
        external_imports,
        true,
    )?;
    let target_graph = discover_reachable_module_packages(
        module_dir,
        &manifest.module.path,
        target_dir,
        &artifact_dir,
        external_imports,
        false,
    )?;
    merge_reachable_graph(&mut graph, target_graph)?;
    Ok(ExternalTestGraph {
        target_package,
        graph,
    })
}

pub fn discover_project_external_test_packages(
    module_dir: &Path,
    entry_path: &Path,
    external_imports: &ExternalImports,
) -> Result<Vec<ExternalTestGraph>, String> {
    let manifest = load_module_manifest(&module_dir.join("goml.toml"))?;
    validate_project_module_path(&manifest.module.path)?;
    let artifact_dir = module_dir.join(&manifest.build.target_dir);
    let target_dir = normalized_parent(entry_path);
    reject_test_only_target(module_dir, target_dir)?;
    let target_package = package_import_path(&manifest.module.path, module_dir, target_dir)?;
    discover_external_test_graphs(
        module_dir,
        &manifest.module.path,
        target_dir,
        &artifact_dir,
        &target_package,
        external_imports,
    )
}

pub fn classify_project_path(module_dir: &Path, path: &Path) -> Result<ProjectPathRole, String> {
    let manifest = load_module_manifest(&module_dir.join("goml.toml"))?;
    let artifact_dir = module_dir.join(&manifest.build.target_dir);
    if path.starts_with(&artifact_dir) {
        return Err(format!(
            "path {} is inside build target directory {}",
            path.display(),
            artifact_dir.display()
        ));
    }
    let relative = path.strip_prefix(module_dir).map_err(|_| {
        format!(
            "path {} is outside module root {}",
            path.display(),
            module_dir.display()
        )
    })?;
    let components = relative
        .components()
        .filter_map(|component| match component {
            Component::Normal(segment) => Some(segment.to_os_string()),
            Component::CurDir => None,
            _ => None,
        })
        .collect::<Vec<_>>();
    if let Some(index) = components
        .iter()
        .position(|component| component == TESTS_DIRECTORY)
    {
        let Some(suite) = components.get(index + 1) else {
            return Err(format!(
                "test path {} must select a suite under a tests directory",
                path.display()
            ));
        };
        let mut target_dir = module_dir.to_path_buf();
        for component in components.iter().take(index) {
            target_dir.push(component);
        }
        let suite_dir = target_dir.join(TESTS_DIRECTORY).join(suite);
        return Ok(ProjectPathRole::ExternalTest {
            target_dir,
            suite_dir,
        });
    }
    if is_internal_test_source(path) {
        Ok(ProjectPathRole::InternalTest)
    } else {
        Ok(ProjectPathRole::Production)
    }
}

pub fn discover_dependency_module_packages(
    module_dir: &Path,
    external_imports: &ExternalImports,
) -> Result<PackageGraph, String> {
    let manifest = load_module_manifest(&module_dir.join("goml.toml"))?;
    let artifact_dir = module_dir.join(&manifest.build.target_dir);
    discover_all_module_packages(
        module_dir,
        &manifest.module.path,
        &artifact_dir,
        external_imports,
    )
}

fn discover_reachable_module_packages(
    module_dir: &Path,
    module_path: &str,
    target_dir: &Path,
    artifact_dir: &Path,
    external_imports: &ExternalImports,
    include_entry_tests: bool,
) -> Result<PackageGraph, String> {
    if target_dir.starts_with(artifact_dir) {
        return Err(format!(
            "package directory {} is inside build target directory {}",
            target_dir.display(),
            artifact_dir.display()
        ));
    }
    let entry_package = package_import_path(module_path, module_dir, target_dir)?;
    let mut entry = load_package(
        target_dir,
        &entry_package,
        module_path,
        external_imports,
        include_entry_tests,
    )?;
    let mut packages = HashMap::new();
    let mut package_dirs = HashMap::new();
    let mut external_root_packages = HashSet::new();
    let mut queue = entry.imports.iter().cloned().collect::<BTreeSet<_>>();
    package_dirs.insert(entry_package.clone(), target_dir.to_path_buf());
    entry.name = entry_package.clone();
    packages.insert(entry_package.clone(), entry);

    while let Some(import_path) = queue.pop_first() {
        if packages.contains_key(&import_path) || external_root_packages.contains(&import_path) {
            continue;
        }
        if let Some(package_dir) = local_package_dir(module_path, module_dir, &import_path) {
            if package_dir.starts_with(artifact_dir) {
                return Err(format!(
                    "package {} is inside build target directory {}",
                    import_path,
                    artifact_dir.display()
                ));
            }
            if !package_dir.is_dir() {
                return Err(format!(
                    "package {} not found at {}",
                    import_path,
                    package_dir.display()
                ));
            }
            let package = load_package(
                &package_dir,
                &import_path,
                module_path,
                external_imports,
                false,
            )?;
            queue.extend(package.imports.iter().cloned());
            package_dirs.insert(import_path.clone(), package_dir);
            packages.insert(import_path, package);
            continue;
        }
        if is_external_or_std(&import_path, external_imports) {
            external_root_packages.insert(import_path);
            continue;
        }
        return Err(format!(
            "package {} is not provided by this module or its dependencies",
            import_path
        ));
    }

    Ok(PackageGraph {
        module_dir: module_dir.to_path_buf(),
        module_name: module_path.to_string(),
        entry_package,
        packages,
        package_dirs,
        external_root_packages,
    })
}

fn discover_external_test_graphs(
    module_dir: &Path,
    module_path: &str,
    target_dir: &Path,
    artifact_dir: &Path,
    target_package: &str,
    external_imports: &ExternalImports,
) -> Result<Vec<ExternalTestGraph>, String> {
    let tests_dir = target_dir.join(TESTS_DIRECTORY);
    if !tests_dir.exists() {
        return Ok(Vec::new());
    }
    if !tests_dir.is_dir() {
        return Err(format!(
            "test path {} must be a directory",
            tests_dir.display()
        ));
    }
    let mut suites = Vec::new();
    for entry in fs::read_dir(&tests_dir)
        .map_err(|err| format!("failed to read {}: {}", tests_dir.display(), err))?
    {
        let entry =
            entry.map_err(|err| format!("failed to read {}: {}", tests_dir.display(), err))?;
        let path = entry.path();
        if path.is_file() && path.extension().is_some_and(|extension| extension == "gom") {
            return Err(format!(
                "black-box test source {} must be placed in a suite directory",
                path.display()
            ));
        }
        if !path.is_dir() || entry.file_name().to_string_lossy().starts_with('.') {
            continue;
        }
        if read_gom_sources(&path, true)?.is_empty() {
            continue;
        }
        suites.push(path);
    }
    suites.sort();
    let mut graphs = Vec::new();
    for suite_dir in suites {
        let mut graph = discover_reachable_module_packages(
            module_dir,
            module_path,
            &suite_dir,
            artifact_dir,
            external_imports,
            true,
        )?;
        let target_graph = discover_reachable_module_packages(
            module_dir,
            module_path,
            target_dir,
            artifact_dir,
            external_imports,
            false,
        )?;
        merge_reachable_graph(&mut graph, target_graph)?;
        graphs.push(ExternalTestGraph {
            target_package: target_package.to_string(),
            graph,
        });
    }
    Ok(graphs)
}

fn merge_reachable_graph(target: &mut PackageGraph, source: PackageGraph) -> Result<(), String> {
    target
        .external_root_packages
        .extend(source.external_root_packages);
    for (package, dir) in source.package_dirs {
        if let Some(existing) = target.package_dirs.get(&package)
            && existing != &dir
        {
            return Err(format!("package {} has multiple directories", package));
        }
        target.package_dirs.insert(package, dir);
    }
    for (package, unit) in source.packages {
        if let Some(existing) = target.packages.get(&package) {
            if existing.declared_name != unit.declared_name
                || existing.files != unit.files
                || existing.imports != unit.imports
            {
                return Err(format!("package {} has conflicting inputs", package));
            }
        } else {
            target.packages.insert(package, unit);
        }
    }
    Ok(())
}

fn reject_test_only_target(module_dir: &Path, target_dir: &Path) -> Result<(), String> {
    if path_has_tests_component(module_dir, target_dir)? {
        return Err(format!(
            "test-only package {} cannot be used as a production target",
            target_dir.display()
        ));
    }
    Ok(())
}

fn path_has_tests_component(module_dir: &Path, path: &Path) -> Result<bool, String> {
    let relative = path.strip_prefix(module_dir).map_err(|_| {
        format!(
            "path {} is outside module root {}",
            path.display(),
            module_dir.display()
        )
    })?;
    Ok(relative.components().any(
        |component| matches!(component, Component::Normal(segment) if segment == TESTS_DIRECTORY),
    ))
}

fn import_path_is_test_only(module_path: &str, import_path: &str) -> bool {
    import_path
        .strip_prefix(module_path)
        .and_then(|suffix| suffix.strip_prefix("::"))
        .is_some_and(|suffix| suffix.split("::").any(|segment| segment == TESTS_DIRECTORY))
}

fn discover_all_module_packages(
    module_dir: &Path,
    module_path: &str,
    artifact_dir: &Path,
    external_imports: &ExternalImports,
) -> Result<PackageGraph, String> {
    let mut dirs = Vec::new();
    collect_package_dirs(module_dir, module_dir, artifact_dir, &mut dirs)?;
    dirs.sort();
    if dirs.is_empty() {
        return Err(format!("module {} has no package directories", module_path));
    }

    let mut packages = HashMap::new();
    let mut package_dirs = HashMap::new();
    for dir in dirs {
        let import_path = package_import_path(module_path, module_dir, &dir)?;
        let package = load_package(&dir, &import_path, module_path, external_imports, false)?;
        package_dirs.insert(import_path.clone(), dir);
        packages.insert(import_path, package);
    }

    let mut external_root_packages = HashSet::new();
    for package in packages.values() {
        for import in package.imports.iter() {
            if packages.contains_key(import) {
                continue;
            }
            if is_external_or_std(import, external_imports) {
                external_root_packages.insert(import.clone());
                continue;
            }
            return Err(format!(
                "package {} uses missing package {}",
                package.name, import
            ));
        }
    }

    let entry_package = if packages.contains_key(module_path) {
        module_path.to_string()
    } else {
        let mut names = packages.keys().cloned().collect::<Vec<_>>();
        names.sort();
        names
            .into_iter()
            .next()
            .ok_or_else(|| format!("module {} has no packages", module_path))?
    };

    Ok(PackageGraph {
        module_dir: module_dir.to_path_buf(),
        module_name: module_path.to_string(),
        entry_package,
        packages,
        package_dirs,
        external_root_packages,
    })
}

fn load_package(
    package_dir: &Path,
    import_path: &str,
    module_path: &str,
    external_imports: &ExternalImports,
    include_tests: bool,
) -> Result<PackageUnit, String> {
    let paths = read_gom_sources(package_dir, include_tests)?;
    if paths.is_empty() {
        return Err(format!(
            "package directory {} has no .gom files",
            package_dir.display()
        ));
    }

    let mut sources = Vec::new();
    let mut declared_name = None::<String>;
    for path in paths {
        let src = fs::read_to_string(&path)
            .map_err(|err| format!("failed to read {}: {}", path.display(), err))?;
        let parsed = parse_ast_file(&path, &src)?;
        if !parsed.package_explicit {
            return Err(format!("{} must declare `package <name>;`", path.display()));
        }
        if let Some(existing) = declared_name.as_deref() {
            if existing != parsed.package.0 {
                return Err(format!(
                    "package mismatch in {}: expected {}, found {}",
                    path.display(),
                    existing,
                    parsed.package.0
                ));
            }
        } else {
            declared_name = Some(parsed.package.0.clone());
        }
        sources.push(ParsedSource { path, ast: parsed });
    }

    let imports = collect_imports(&sources, Some((module_path, package_dir)), external_imports)?;
    let mut files = sources
        .into_iter()
        .map(|source| source.path)
        .collect::<Vec<_>>();
    files.sort();
    Ok(PackageUnit {
        name: import_path.to_string(),
        declared_name: declared_name.unwrap_or_else(|| {
            import_path
                .rsplit("::")
                .next()
                .unwrap_or(import_path)
                .to_string()
        }),
        files,
        imports,
    })
}

fn collect_imports(
    files: &[ParsedSource],
    module: Option<(&str, &Path)>,
    external_imports: &ExternalImports,
) -> Result<HashSet<String>, String> {
    let mut imports = HashSet::new();
    for file in files {
        let mut known_packages = HashSet::new();
        let mut alias_targets = HashMap::<String, HashSet<String>>::new();
        for use_decl in file.ast.uses.iter() {
            let import_path = import_path_text(&use_decl.path)?;
            if let Some(candidate) = package_use_candidate(&import_path, module, external_imports)?
            {
                let alias = use_decl
                    .alias
                    .as_ref()
                    .map(|alias| alias.0.clone())
                    .unwrap_or(candidate.default_alias);
                alias_targets
                    .entry(alias)
                    .or_default()
                    .insert(import_path.clone());
                if candidate.exists {
                    known_packages.insert(import_path);
                }
            }
        }
        for use_decl in file.ast.uses.iter() {
            let import_path = import_path_text(&use_decl.path)?;
            if known_packages.contains(&import_path) {
                imports.insert(import_path);
                continue;
            }
            let first = use_decl
                .path
                .segments()
                .first()
                .map(|segment| segment.ident.0.as_str());
            if first.is_some_and(|first| {
                alias_targets
                    .get(first)
                    .is_some_and(|targets| targets.iter().any(|target| target != &import_path))
            }) {
                continue;
            }
            imports.insert(import_path);
        }
    }
    Ok(imports)
}

fn package_use_candidate(
    import_path: &str,
    module: Option<(&str, &Path)>,
    external_imports: &ExternalImports,
) -> Result<Option<PackageUseCandidate>, String> {
    if let Some(name) = external_imports.declared_name(import_path) {
        return Ok(Some(PackageUseCandidate {
            default_alias: name.to_string(),
            exists: true,
        }));
    }
    if external_imports
        .package_names()
        .keys()
        .any(|package| import_path.starts_with(&format!("{package}::")))
    {
        return Ok(import_path
            .rsplit("::")
            .next()
            .map(|default_alias| PackageUseCandidate {
                default_alias: default_alias.to_string(),
                exists: false,
            }));
    }
    if import_path == STD_PACKAGE || import_path.starts_with("std::") {
        return Ok(import_path
            .rsplit("::")
            .next()
            .map(|default_alias| PackageUseCandidate {
                default_alias: default_alias.to_string(),
                exists: true,
            }));
    }
    let Some((module_path, package_dir)) = module else {
        return Ok(None);
    };
    if import_path_is_test_only(module_path, import_path) {
        return Err(format!(
            "test-only package {} cannot be imported",
            import_path
        ));
    }
    let module_dir = find_module_dir(module_path, package_dir)?;
    let Some(candidate_dir) = local_package_dir(module_path, &module_dir, import_path) else {
        return Ok(None);
    };
    if !candidate_dir.is_dir() {
        return Ok(import_path
            .rsplit("::")
            .next()
            .map(|default_alias| PackageUseCandidate {
                default_alias: default_alias.to_string(),
                exists: false,
            }));
    }
    let Some(path) = read_gom_sources(&candidate_dir, false)?.into_iter().next() else {
        return Ok(import_path
            .rsplit("::")
            .next()
            .map(|default_alias| PackageUseCandidate {
                default_alias: default_alias.to_string(),
                exists: true,
            }));
    };
    let src = fs::read_to_string(&path)
        .map_err(|err| format!("failed to read {}: {}", path.display(), err))?;
    let parsed = parse_ast_file(&path, &src)?;
    Ok(Some(PackageUseCandidate {
        default_alias: parsed.package.0,
        exists: true,
    }))
}

fn find_module_dir(module_path: &str, package_dir: &Path) -> Result<PathBuf, String> {
    let mut current = package_dir.to_path_buf();
    loop {
        let manifest_path = current.join("goml.toml");
        if manifest_path.is_file()
            && load_module_manifest(&manifest_path)?.module.path == module_path
        {
            return Ok(current);
        }
        if !current.pop() {
            return Err(format!("module root for {} not found", module_path));
        }
    }
}

fn import_path_text(path: &ast::Path) -> Result<String, String> {
    if path.is_empty() {
        return Err(format!(
            "package use must name a canonical package path, found `{}`",
            path.display()
        ));
    }
    Ok(path
        .segments()
        .iter()
        .map(|segment| segment.ident.0.clone())
        .collect::<Vec<_>>()
        .join("::"))
}

fn package_import_path(
    module_path: &str,
    module_dir: &Path,
    package_dir: &Path,
) -> Result<String, String> {
    let relative = package_dir.strip_prefix(module_dir).map_err(|_| {
        format!(
            "package directory {} is outside module root {}",
            package_dir.display(),
            module_dir.display()
        )
    })?;
    let mut segments = module_path
        .split("::")
        .map(str::to_string)
        .collect::<Vec<_>>();
    for component in relative.components() {
        match component {
            Component::Normal(segment) => {
                let Some(segment) = segment.to_str() else {
                    return Err(format!(
                        "package path {} is not valid UTF-8",
                        package_dir.display()
                    ));
                };
                segments.push(segment.to_string());
            }
            Component::CurDir => {}
            _ => {
                return Err(format!(
                    "invalid package directory {}",
                    package_dir.display()
                ));
            }
        }
    }
    let import_path = segments.join("::");
    validate_module_path(&import_path)?;
    Ok(import_path)
}

fn local_package_dir(module_path: &str, module_dir: &Path, import_path: &str) -> Option<PathBuf> {
    if import_path == module_path {
        return Some(module_dir.to_path_buf());
    }
    let suffix = import_path.strip_prefix(&format!("{module_path}::"))?;
    let mut dir = module_dir.to_path_buf();
    for segment in suffix.split("::") {
        if segment.is_empty() || segment == "." || segment == ".." {
            return None;
        }
        dir.push(segment);
        if dir.join("goml.toml").is_file() {
            return None;
        }
    }
    Some(dir)
}

fn read_gom_sources(dir: &Path, include_tests: bool) -> Result<Vec<PathBuf>, String> {
    let entries = fs::read_dir(dir).map_err(|err| {
        format!(
            "failed to read package directory {}: {}",
            dir.display(),
            err
        )
    })?;
    let mut files = Vec::new();
    for entry in entries {
        let entry = entry.map_err(|err| {
            format!(
                "failed to read package directory {}: {}",
                dir.display(),
                err
            )
        })?;
        let path = entry.path();
        if path.is_file()
            && path.extension().is_some_and(|ext| ext == "gom")
            && (include_tests || !is_internal_test_source(&path))
        {
            files.push(path);
        }
    }
    files.sort();
    Ok(files)
}

pub fn is_internal_test_source(path: &Path) -> bool {
    path.file_name()
        .and_then(|name| name.to_str())
        .is_some_and(|name| name.ends_with("_test.gom"))
}

fn collect_package_dirs(
    module_dir: &Path,
    dir: &Path,
    artifact_dir: &Path,
    packages: &mut Vec<PathBuf>,
) -> Result<(), String> {
    if dir != module_dir && dir.join("goml.toml").is_file() {
        return Ok(());
    }
    let entries = fs::read_dir(dir)
        .map_err(|err| format!("failed to read module directory {}: {}", dir.display(), err))?;
    let mut children = Vec::new();
    let mut has_source = false;
    for entry in entries {
        let entry = entry
            .map_err(|err| format!("failed to read module directory {}: {}", dir.display(), err))?;
        let path = entry.path();
        if path.is_file()
            && path.extension().is_some_and(|extension| extension == "gom")
            && !is_internal_test_source(&path)
        {
            has_source = true;
        } else if path.is_dir() {
            let name = entry.file_name();
            let name = name.to_string_lossy();
            if !name.starts_with('.') && path != artifact_dir && name != TESTS_DIRECTORY {
                children.push(path);
            }
        }
    }
    if has_source {
        packages.push(dir.to_path_buf());
    }
    children.sort();
    for child in children {
        collect_package_dirs(module_dir, &child, artifact_dir, packages)?;
    }
    Ok(())
}

fn parse_ast_file(path: &Path, src: &str) -> Result<ast::File, String> {
    let parsed = parser::parse(path, src);
    if parsed.has_errors() {
        return Err(parsed.format_errors(src).join("\n"));
    }
    let root = MySyntaxNode::new_root(parsed.green_node);
    let cst = CstFile::cast(root).ok_or_else(|| format!("failed to parse {}", path.display()))?;
    ::ast::lower::lower(cst)
        .into_result()
        .map_err(|diagnostics| diagnostics_text(&diagnostics))
}

fn diagnostics_text(diagnostics: &parser::Diagnostics) -> String {
    diagnostics
        .iter()
        .map(|diagnostic| diagnostic.message().to_string())
        .collect::<Vec<_>>()
        .join("\n")
}

fn normalized_parent(path: &Path) -> &Path {
    path.parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."))
}

fn is_external_or_std(import: &str, external_imports: &ExternalImports) -> bool {
    external_imports.contains_package(import)
        || import == STD_PACKAGE
        || import.starts_with("std::")
}

pub fn topo_sort_packages(graph: &PackageGraph) -> Result<Vec<String>, String> {
    let mut temporary = HashSet::new();
    let mut permanent = HashSet::new();
    let mut stack = Vec::new();
    let mut order = Vec::new();
    let mut names = graph.packages.keys().cloned().collect::<Vec<_>>();
    names.sort();
    for name in names {
        if !permanent.contains(&name) {
            visit_package(
                &name,
                graph,
                &mut temporary,
                &mut permanent,
                &mut stack,
                &mut order,
            )?;
        }
    }
    Ok(order)
}

fn visit_package(
    name: &str,
    graph: &PackageGraph,
    temporary: &mut HashSet<String>,
    permanent: &mut HashSet<String>,
    stack: &mut Vec<String>,
    order: &mut Vec<String>,
) -> Result<(), String> {
    if permanent.contains(name) {
        return Ok(());
    }
    if temporary.contains(name) {
        let position = stack
            .iter()
            .position(|package| package == name)
            .unwrap_or(0);
        let mut cycle = stack[position..].to_vec();
        cycle.push(name.to_string());
        return Err(format!("package use cycle: {}", cycle.join(" -> ")));
    }
    temporary.insert(name.to_string());
    stack.push(name.to_string());
    let package = graph
        .packages
        .get(name)
        .ok_or_else(|| format!("package {} not found", name))?;
    let mut imports = package.imports.iter().cloned().collect::<Vec<_>>();
    imports.sort();
    for import in imports {
        if graph.external_root_packages.contains(&import) {
            continue;
        }
        if !graph.packages.contains_key(&import) {
            return Err(format!("package {} uses missing package {}", name, import));
        }
        visit_package(&import, graph, temporary, permanent, stack, order)?;
    }
    stack.pop();
    temporary.remove(name);
    permanent.insert(name.to_string());
    order.push(name.to_string());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn discovers_reachable_packages_and_trait_scope_uses() {
        let dir = tempfile::tempdir().unwrap();
        fs::create_dir_all(dir.path().join("lib")).unwrap();
        fs::write(dir.path().join("goml.toml"), "[module]\npath = \"demo\"\n").unwrap();
        fs::write(
            dir.path().join("main.gom"),
            "package main;\nuse demo::lib;\nuse lib::Show;\nfn main() -> unit { () }\n",
        )
        .unwrap();
        fs::write(
            dir.path().join("lib/lib.gom"),
            "package lib;\npub trait Show { fn show(Self) -> string; }\n",
        )
        .unwrap();

        let graph = discover_project_packages(
            dir.path(),
            &dir.path().join("main.gom"),
            &ExternalImports::default(),
        )
        .unwrap();
        assert_eq!(graph.packages.len(), 2);
        assert_eq!(
            graph.packages["demo"].imports,
            HashSet::from(["demo::lib".to_string()])
        );
        assert_eq!(
            topo_sort_packages(&graph).unwrap(),
            vec!["demo::lib".to_string(), "demo".to_string()]
        );
    }

    #[test]
    fn discovers_all_dependency_packages() {
        let dir = tempfile::tempdir().unwrap();
        fs::create_dir_all(dir.path().join("unused")).unwrap();
        fs::create_dir_all(dir.path().join("out")).unwrap();
        fs::write(
            dir.path().join("goml.toml"),
            "[module]\npath = \"alice::dep\"\n\n[build]\ntarget-dir = \"out\"\n",
        )
        .unwrap();
        fs::write(
            dir.path().join("dep.gom"),
            "package dep;\npub fn value() -> int32 { 1 }\n",
        )
        .unwrap();
        fs::write(
            dir.path().join("unused/unused.gom"),
            "package unused;\npub fn value() -> int32 { 2 }\n",
        )
        .unwrap();
        fs::write(
            dir.path().join("out/generated.gom"),
            "package generated;\npub fn value() -> int32 { 3 }\n",
        )
        .unwrap();

        let graph =
            discover_dependency_module_packages(dir.path(), &ExternalImports::default()).unwrap();
        assert!(graph.packages.contains_key("alice::dep"));
        assert!(graph.packages.contains_key("alice::dep::unused"));
        assert!(!graph.packages.contains_key("alice::dep::out"));
        assert!(classify_project_path(dir.path(), &dir.path().join("out/generated.gom")).is_err());
    }

    #[test]
    fn test_discovery_only_adds_test_sources_to_the_entry_package() {
        let dir = tempfile::tempdir().unwrap();
        fs::create_dir_all(dir.path().join("dep")).unwrap();
        fs::write(dir.path().join("goml.toml"), "[module]\npath = \"demo\"\n").unwrap();
        fs::write(
            dir.path().join("main.gom"),
            "package app;\nuse demo::dep;\npub fn value() -> int32 { dep::value() }\n",
        )
        .unwrap();
        fs::write(
            dir.path().join("main_test.gom"),
            "package app;\nuse std::testing;\n#[test]\nfn works() -> unit { testing::assert(true) }\n",
        )
        .unwrap();
        fs::write(
            dir.path().join("dep/dep.gom"),
            "package dep;\npub fn value() -> int32 { 1 }\n",
        )
        .unwrap();
        fs::write(
            dir.path().join("dep/dep_test.gom"),
            "package dep;\nuse demo::missing;\n#[test]\nfn hidden() -> unit { () }\n",
        )
        .unwrap();

        let normal = discover_project_packages(
            dir.path(),
            &dir.path().join("main.gom"),
            &ExternalImports::default(),
        )
        .unwrap();
        assert_eq!(normal.packages["demo"].files.len(), 1);
        assert!(!normal.packages["demo"].imports.contains("std::testing"));

        let test = discover_project_test_packages(
            dir.path(),
            &dir.path().join("main.gom"),
            &ExternalImports::default(),
        )
        .unwrap();
        assert_eq!(test.packages["demo"].files.len(), 2);
        assert!(test.packages["demo"].imports.contains("std::testing"));
        assert_eq!(test.packages["demo::dep"].files.len(), 1);
    }

    #[test]
    fn discovers_internal_and_external_test_targets() {
        let dir = tempfile::tempdir().unwrap();
        fs::create_dir_all(dir.path().join("math/tests/api")).unwrap();
        fs::write(dir.path().join("goml.toml"), "[module]\npath = \"demo\"\n").unwrap();
        fs::write(
            dir.path().join("math/math.gom"),
            "package math;\npub fn add(a: int32, b: int32) -> int32 { a + b }\n",
        )
        .unwrap();
        fs::write(
            dir.path().join("math/math_test.gom"),
            "package math;\n#[test]\nfn internal() -> unit { () }\n",
        )
        .unwrap();
        fs::write(
            dir.path().join("math/tests/api/api_test.gom"),
            "package api;\nuse demo::math;\n#[test]\nfn external() -> unit { let _ = math::add(1, 2); () }\n",
        )
        .unwrap();

        let plan = discover_project_test_plan(
            dir.path(),
            &dir.path().join("math/math.gom"),
            &ExternalImports::default(),
        )
        .unwrap();
        assert_eq!(plan.normal.packages["demo::math"].files.len(), 1);
        assert_eq!(plan.internal.packages["demo::math"].files.len(), 2);
        assert_eq!(plan.external.len(), 1);
        assert_eq!(plan.external[0].target_package, "demo::math");
        assert_eq!(
            plan.external[0].graph.entry_package,
            "demo::math::tests::api"
        );
        assert!(plan.external[0].graph.packages.contains_key("demo::math"));
        assert_eq!(
            classify_project_path(dir.path(), &dir.path().join("math/math_test.gom")).unwrap(),
            ProjectPathRole::InternalTest
        );
        assert_eq!(
            classify_project_path(dir.path(), &dir.path().join("math/tests/api/api_test.gom"))
                .unwrap(),
            ProjectPathRole::ExternalTest {
                target_dir: dir.path().join("math"),
                suite_dir: dir.path().join("math/tests/api"),
            }
        );
    }

    #[test]
    fn production_packages_cannot_import_black_box_test_packages() {
        let dir = tempfile::tempdir().unwrap();
        fs::create_dir_all(dir.path().join("math/tests/api")).unwrap();
        fs::write(dir.path().join("goml.toml"), "[module]\npath = \"demo\"\n").unwrap();
        fs::write(
            dir.path().join("main.gom"),
            "package main;\nuse demo::math::tests::api;\nfn main() -> unit { () }\n",
        )
        .unwrap();
        fs::write(
            dir.path().join("math/math.gom"),
            "package math;\npub fn value() -> int32 { 1 }\n",
        )
        .unwrap();
        fs::write(
            dir.path().join("math/tests/api/api.gom"),
            "package api;\npub fn value() -> int32 { 1 }\n",
        )
        .unwrap();

        let error = discover_project_packages(
            dir.path(),
            &dir.path().join("main.gom"),
            &ExternalImports::default(),
        )
        .unwrap_err();
        assert!(error.contains("test-only package demo::math::tests::api cannot be imported"));
    }
}
