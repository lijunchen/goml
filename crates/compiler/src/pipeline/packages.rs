use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fs;
use std::path::{Component, Path, PathBuf};

use ast::ast;

use crate::config::{
    find_module_root, load_module_manifest, validate_module_path, validate_project_module_path,
};
use crate::hir::SourceFileAst;
use crate::package_imports::ExternalImports;
use crate::package_names::ROOT_PACKAGE;
use crate::pipeline::compile_error;
use crate::pipeline::pipeline::{CompilationError, parse_ast_file};

#[derive(Debug)]
pub struct PackageUnit {
    pub name: String,
    pub declared_name: String,
    pub files: Vec<SourceFileAst>,
    pub imports: HashSet<String>,
}

#[derive(Debug)]
pub struct PackageGraph {
    pub module_dir: PathBuf,
    pub module_name: Option<String>,
    pub entry_package: String,
    pub packages: HashMap<String, PackageUnit>,
    pub discovery_order: Vec<String>,
    pub package_dirs: HashMap<String, PathBuf>,
    pub declared_package_names: HashMap<String, String>,
    pub external_root_packages: HashSet<String>,
}

fn read_gom_sources(dir: &Path) -> Result<Vec<PathBuf>, CompilationError> {
    let mut files = Vec::new();
    let entries = fs::read_dir(dir).map_err(|err| {
        compile_error(format!(
            "failed to read package directory {}: {}",
            dir.display(),
            err
        ))
    })?;
    for entry in entries {
        let entry = entry.map_err(|err| {
            compile_error(format!(
                "failed to read package directory {}: {}",
                dir.display(),
                err
            ))
        })?;
        let path = entry.path();
        if path.is_file() && path.extension().is_some_and(|ext| ext == "gom") {
            files.push(path);
        }
    }
    files.sort();
    Ok(files)
}

fn source_override_for_dir<'a>(
    package_dir: &Path,
    source_override: Option<(&'a Path, &'a ast::File)>,
) -> Option<(&'a Path, &'a ast::File)> {
    source_override.filter(|(path, _)| normalized_parent(path) == package_dir)
}

fn normalized_parent(path: &Path) -> &Path {
    path.parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."))
}

fn import_path_text(path: &ast::Path) -> Result<String, CompilationError> {
    if path.is_empty() {
        return Err(compile_error(format!(
            "package use must name a canonical package path, found `{}`",
            path.display()
        )));
    }
    Ok(path
        .segments()
        .iter()
        .map(|segment| segment.ident.0.clone())
        .collect::<Vec<_>>()
        .join("::"))
}

struct PackageUseCandidate {
    default_alias: String,
    exists: bool,
}

fn package_use_candidate(
    import_path: &str,
    module: Option<(&str, &Path)>,
    external_imports: &ExternalImports,
) -> Result<Option<PackageUseCandidate>, CompilationError> {
    if let Some(name) = external_imports.declared_name(import_path) {
        return Ok(Some(PackageUseCandidate {
            default_alias: name.to_string(),
            exists: true,
        }));
    }
    if external_imports
        .package_names
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
    if import_path == crate::package_names::STD_PACKAGE || import_path.starts_with("std::") {
        return Ok(import_path
            .rsplit("::")
            .next()
            .map(|default_alias| PackageUseCandidate {
                default_alias: default_alias.to_string(),
                exists: true,
            }));
    }
    let Some((module_path, module_dir)) = module else {
        return Ok(None);
    };
    let Some(package_dir) = local_package_dir(module_path, module_dir, import_path) else {
        return Ok(None);
    };
    if !package_dir.is_dir() {
        return Ok(import_path
            .rsplit("::")
            .next()
            .map(|default_alias| PackageUseCandidate {
                default_alias: default_alias.to_string(),
                exists: false,
            }));
    }
    let Some(path) = read_gom_sources(&package_dir)?.into_iter().next() else {
        return Ok(import_path
            .rsplit("::")
            .next()
            .map(|default_alias| PackageUseCandidate {
                default_alias: default_alias.to_string(),
                exists: true,
            }));
    };
    let src = fs::read_to_string(&path)
        .map_err(|err| compile_error(format!("failed to read {}: {}", path.display(), err)))?;
    let parsed = parse_ast_file(&path, &src)?;
    let default_alias = if parsed.package_explicit {
        parsed.package.0
    } else {
        import_path
            .rsplit("::")
            .next()
            .unwrap_or(import_path)
            .to_string()
    };
    Ok(Some(PackageUseCandidate {
        default_alias,
        exists: true,
    }))
}

fn collect_imports(
    files: &[SourceFileAst],
    module: Option<(&str, &Path)>,
    external_imports: &ExternalImports,
) -> Result<HashSet<String>, CompilationError> {
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

fn set_package_identity(ast: &mut ast::File, import_path: &str) {
    ast.package = ast::AstIdent::new(import_path);
}

fn load_package(
    package_dir: &Path,
    import_path: &str,
    source_override: Option<(&Path, &ast::File)>,
    allow_implicit_package: bool,
) -> Result<PackageUnit, CompilationError> {
    let source_override = source_override_for_dir(package_dir, source_override);
    let mut paths = read_gom_sources(package_dir)?;
    if let Some((override_path, _)) = source_override
        && !paths.iter().any(|path| path == override_path)
    {
        paths.push(override_path.to_path_buf());
        paths.sort();
    }
    if paths.is_empty() {
        return Err(compile_error(format!(
            "package directory {} has no .gom files",
            package_dir.display()
        )));
    }

    let mut files = Vec::new();
    let mut declared_name = None::<String>;
    for path in paths {
        let mut parsed = if let Some((override_path, override_ast)) = source_override
            && path == override_path
        {
            override_ast.clone()
        } else {
            let src = fs::read_to_string(&path).map_err(|err| {
                compile_error(format!("failed to read {}: {}", path.display(), err))
            })?;
            parse_ast_file(&path, &src)?
        };
        if !parsed.package_explicit && !allow_implicit_package {
            return Err(compile_error(format!(
                "{} must declare `package <name>;`",
                path.display()
            )));
        }
        let file_package = if parsed.package_explicit {
            parsed.package.0.clone()
        } else {
            ROOT_PACKAGE.to_string()
        };
        if let Some(existing) = declared_name.as_deref() {
            if existing != file_package {
                return Err(compile_error(format!(
                    "package mismatch in {}: expected {}, found {}",
                    path.display(),
                    existing,
                    file_package
                )));
            }
        } else {
            declared_name = Some(file_package);
        }
        set_package_identity(&mut parsed, import_path);
        files.push(SourceFileAst::new(path, parsed));
    }
    Ok(PackageUnit {
        name: import_path.to_string(),
        declared_name: declared_name.unwrap_or_else(|| ROOT_PACKAGE.to_string()),
        files,
        imports: HashSet::new(),
    })
}

fn package_import_path(
    module_path: &str,
    module_dir: &Path,
    package_dir: &Path,
) -> Result<String, CompilationError> {
    let relative = package_dir.strip_prefix(module_dir).map_err(|_| {
        compile_error(format!(
            "package directory {} is outside module root {}",
            package_dir.display(),
            module_dir.display()
        ))
    })?;
    let mut segments = module_path
        .split("::")
        .map(str::to_string)
        .collect::<Vec<_>>();
    for component in relative.components() {
        match component {
            Component::Normal(segment) => {
                let Some(segment) = segment.to_str() else {
                    return Err(compile_error(format!(
                        "package path {} is not valid UTF-8",
                        package_dir.display()
                    )));
                };
                segments.push(segment.to_string());
            }
            Component::CurDir => {}
            _ => {
                return Err(compile_error(format!(
                    "invalid package directory {}",
                    package_dir.display()
                )));
            }
        }
    }
    let import_path = segments.join("::");
    validate_module_path(&import_path).map_err(compile_error)?;
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

fn discover_module_packages(
    module_dir: &Path,
    module_path: &str,
    target_dir: &Path,
    source_override: Option<(&Path, &ast::File)>,
    external_imports: &ExternalImports,
) -> Result<PackageGraph, CompilationError> {
    let entry_package = package_import_path(module_path, module_dir, target_dir)?;
    let mut entry = load_package(target_dir, &entry_package, source_override, false)?;
    entry.imports = collect_imports(
        &entry.files,
        Some((module_path, module_dir)),
        external_imports,
    )?;
    let mut packages = HashMap::new();
    let mut discovery_order = Vec::new();
    let mut package_dirs = HashMap::new();
    let mut declared_package_names = HashMap::new();
    let mut external_root_packages = HashSet::new();
    let mut queue = entry.imports.iter().cloned().collect::<BTreeSet<_>>();
    declared_package_names.insert(entry_package.clone(), entry.declared_name.clone());
    packages.insert(entry_package.clone(), entry);
    discovery_order.push(entry_package.clone());
    package_dirs.insert(entry_package.clone(), target_dir.to_path_buf());

    while let Some(import_path) = queue.pop_first() {
        if packages.contains_key(&import_path) || external_root_packages.contains(&import_path) {
            continue;
        }
        if let Some(package_dir) = local_package_dir(module_path, module_dir, &import_path) {
            if !package_dir.is_dir() {
                return Err(compile_error(format!(
                    "package {} not found at {}",
                    import_path,
                    package_dir.display()
                )));
            }
            let mut package = load_package(&package_dir, &import_path, source_override, false)?;
            package.imports = collect_imports(
                &package.files,
                Some((module_path, module_dir)),
                external_imports,
            )?;
            queue.extend(package.imports.iter().cloned());
            package_dirs.insert(import_path.clone(), package_dir);
            declared_package_names.insert(import_path.clone(), package.declared_name.clone());
            discovery_order.push(import_path.clone());
            packages.insert(import_path, package);
            continue;
        }
        if external_imports.contains_package(&import_path)
            || import_path == crate::package_names::STD_PACKAGE
            || import_path.starts_with(&format!("{}::", crate::package_names::STD_PACKAGE))
        {
            external_root_packages.insert(import_path);
            continue;
        }
        return Err(compile_error(format!(
            "package {} is not provided by this module or its dependencies",
            import_path
        )));
    }

    Ok(PackageGraph {
        module_dir: module_dir.to_path_buf(),
        module_name: Some(module_path.to_string()),
        entry_package,
        packages,
        discovery_order,
        package_dirs,
        declared_package_names,
        external_root_packages,
    })
}

fn discover_single_file_packages(
    root_dir: &Path,
    entry_path: &Path,
    entry_ast: ast::File,
    external_imports: &ExternalImports,
) -> Result<PackageGraph, CompilationError> {
    let mut entry = load_package(
        normalized_parent(entry_path),
        ROOT_PACKAGE,
        Some((entry_path, &entry_ast)),
        true,
    )?;
    entry.imports = collect_imports(&entry.files, None, external_imports)?;
    let imports = entry.imports.clone();
    let mut external_root_packages = HashSet::new();
    for import in imports {
        if external_imports.contains_package(&import)
            || import == crate::package_names::STD_PACKAGE
            || import.starts_with("std::")
        {
            external_root_packages.insert(import);
            continue;
        }
        return Err(compile_error(format!(
            "single-file compilation cannot use project package {}",
            import
        )));
    }
    let mut packages = HashMap::new();
    let declared_package_names =
        HashMap::from([(ROOT_PACKAGE.to_string(), entry.declared_name.clone())]);
    packages.insert(ROOT_PACKAGE.to_string(), entry);
    let mut package_dirs = HashMap::new();
    package_dirs.insert(
        ROOT_PACKAGE.to_string(),
        normalized_parent(entry_path).to_path_buf(),
    );
    Ok(PackageGraph {
        module_dir: root_dir.to_path_buf(),
        module_name: None,
        entry_package: ROOT_PACKAGE.to_string(),
        packages,
        discovery_order: vec![ROOT_PACKAGE.to_string()],
        package_dirs,
        declared_package_names,
        external_root_packages,
    })
}

pub fn discover_dependency_versions_from_file(
    file_path: &Path,
) -> Result<(PathBuf, BTreeMap<String, String>), CompilationError> {
    let start_dir = normalized_parent(file_path);
    if let Some((module_dir, _)) = find_module_root(start_dir).map_err(compile_error)? {
        let manifest =
            load_module_manifest(&module_dir.join("goml.toml")).map_err(compile_error)?;
        return Ok((module_dir, manifest.dependencies));
    }
    Ok((start_dir.to_path_buf(), BTreeMap::new()))
}

pub fn discover_packages(
    root_dir: &Path,
    entry_path: Option<&Path>,
    entry_ast: Option<ast::File>,
) -> Result<PackageGraph, CompilationError> {
    discover_packages_with_external_imports(
        root_dir,
        entry_path,
        entry_ast,
        &ExternalImports::default(),
    )
}

pub fn discover_packages_with_external_imports(
    root_dir: &Path,
    entry_path: Option<&Path>,
    entry_ast: Option<ast::File>,
    external_imports: &ExternalImports,
) -> Result<PackageGraph, CompilationError> {
    let manifest = load_module_manifest(&root_dir.join("goml.toml")).map_err(compile_error)?;
    validate_project_module_path(&manifest.module.path).map_err(compile_error)?;
    let target_dir = entry_path.map(normalized_parent).unwrap_or(root_dir);
    let source_override = entry_path.zip(entry_ast.as_ref());
    discover_module_packages(
        root_dir,
        &manifest.module.path,
        target_dir,
        source_override,
        external_imports,
    )
}

pub fn discover_packages_single_file_with_external_imports(
    root_dir: &Path,
    entry_path: &Path,
    entry_ast: ast::File,
    external_imports: &ExternalImports,
) -> Result<PackageGraph, CompilationError> {
    discover_single_file_packages(root_dir, entry_path, entry_ast, external_imports)
}

pub fn discover_dependency_module_packages_with_external_imports(
    module_dir: &Path,
    external_imports: &ExternalImports,
) -> Result<PackageGraph, CompilationError> {
    let manifest = load_module_manifest(&module_dir.join("goml.toml")).map_err(compile_error)?;
    discover_all_module_packages(module_dir, &manifest.module.path, external_imports)
}

fn discover_all_module_packages(
    module_dir: &Path,
    module_path: &str,
    external_imports: &ExternalImports,
) -> Result<PackageGraph, CompilationError> {
    let mut dirs = Vec::new();
    collect_package_dirs(module_dir, module_dir, &mut dirs)?;
    dirs.sort();
    if dirs.is_empty() {
        return Err(compile_error(format!(
            "module {} has no package directories",
            module_path
        )));
    }

    let mut packages = HashMap::new();
    let mut discovery_order = Vec::new();
    let mut package_dirs = HashMap::new();
    let mut declared_package_names = HashMap::new();
    for dir in dirs {
        let import_path = package_import_path(module_path, module_dir, &dir)?;
        let mut package = load_package(&dir, &import_path, None, false)?;
        package.imports = collect_imports(
            &package.files,
            Some((module_path, module_dir)),
            external_imports,
        )?;
        discovery_order.push(import_path.clone());
        package_dirs.insert(import_path.clone(), dir);
        declared_package_names.insert(import_path.clone(), package.declared_name.clone());
        packages.insert(import_path, package);
    }

    let mut external_root_packages = HashSet::new();
    for package in packages.values() {
        for import in package.imports.iter() {
            if packages.contains_key(import) {
                continue;
            }
            if external_imports.contains_package(import)
                || import == crate::package_names::STD_PACKAGE
                || import.starts_with("std::")
            {
                external_root_packages.insert(import.clone());
                continue;
            }
            return Err(compile_error(format!(
                "package {} uses missing package {}",
                package.name, import
            )));
        }
    }

    let entry_package = if packages.contains_key(module_path) {
        module_path.to_string()
    } else {
        discovery_order
            .first()
            .cloned()
            .ok_or_else(|| compile_error(format!("module {} has no packages", module_path)))?
    };

    Ok(PackageGraph {
        module_dir: module_dir.to_path_buf(),
        module_name: Some(module_path.to_string()),
        entry_package,
        packages,
        discovery_order,
        package_dirs,
        declared_package_names,
        external_root_packages,
    })
}

fn collect_package_dirs(
    module_dir: &Path,
    dir: &Path,
    packages: &mut Vec<PathBuf>,
) -> Result<(), CompilationError> {
    if dir != module_dir && dir.join("goml.toml").is_file() {
        return Ok(());
    }
    let entries = fs::read_dir(dir).map_err(|err| {
        compile_error(format!(
            "failed to read module directory {}: {}",
            dir.display(),
            err
        ))
    })?;
    let mut children = Vec::new();
    let mut has_source = false;
    for entry in entries {
        let entry = entry.map_err(|err| {
            compile_error(format!(
                "failed to read module directory {}: {}",
                dir.display(),
                err
            ))
        })?;
        let path = entry.path();
        if path.is_file() && path.extension().is_some_and(|extension| extension == "gom") {
            has_source = true;
            continue;
        }
        if !path.is_dir() {
            continue;
        }
        let name = entry.file_name();
        let name = name.to_string_lossy();
        if name.starts_with('.') || name == "target" {
            continue;
        }
        children.push(path);
    }
    if has_source {
        packages.push(dir.to_path_buf());
    }
    children.sort();
    for child in children {
        collect_package_dirs(module_dir, &child, packages)?;
    }
    Ok(())
}

pub fn topo_sort_packages(graph: &PackageGraph) -> Result<Vec<String>, CompilationError> {
    let mut temp = HashSet::new();
    let mut perm = HashSet::new();
    let mut order = Vec::new();
    let mut stack = Vec::new();
    let mut names = graph.packages.keys().cloned().collect::<Vec<_>>();
    names.sort();
    for name in names {
        if !perm.contains(&name) {
            visit_package(&name, graph, &mut temp, &mut perm, &mut stack, &mut order)?;
        }
    }
    Ok(order)
}

fn visit_package(
    name: &str,
    graph: &PackageGraph,
    temp: &mut HashSet<String>,
    perm: &mut HashSet<String>,
    stack: &mut Vec<String>,
    order: &mut Vec<String>,
) -> Result<(), CompilationError> {
    if perm.contains(name) {
        return Ok(());
    }
    if temp.contains(name) {
        let position = stack
            .iter()
            .position(|package| package == name)
            .unwrap_or(0);
        let mut cycle = stack[position..].to_vec();
        cycle.push(name.to_string());
        return Err(compile_error(format!(
            "package use cycle: {}",
            cycle.join(" -> ")
        )));
    }
    temp.insert(name.to_string());
    stack.push(name.to_string());
    let package = graph
        .packages
        .get(name)
        .ok_or_else(|| compile_error(format!("package {} not found", name)))?;
    let mut imports = package.imports.iter().cloned().collect::<Vec<_>>();
    imports.sort();
    for import in imports {
        if graph.external_root_packages.contains(&import) {
            continue;
        }
        if !graph.packages.contains_key(&import) {
            return Err(compile_error(format!(
                "package {} uses missing package {}",
                name, import
            )));
        }
        visit_package(&import, graph, temp, perm, stack, order)?;
    }
    stack.pop();
    temp.remove(name);
    perm.insert(name.to_string());
    order.push(name.to_string());
    Ok(())
}

impl PackageGraph {
    pub fn add_external_root_package(&mut self, package: impl Into<String>) {
        self.external_root_packages.insert(package.into());
    }

    pub fn add_external_package(
        &mut self,
        package: impl Into<String>,
        declared_name: impl Into<String>,
        dir: PathBuf,
    ) {
        let package = package.into();
        self.package_dirs.insert(package.clone(), dir);
        self.declared_package_names
            .insert(package, declared_name.into());
    }
}
