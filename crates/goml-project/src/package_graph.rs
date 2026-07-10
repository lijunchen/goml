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
    let target_dir = normalized_parent(entry_path);
    discover_reachable_module_packages(
        module_dir,
        &manifest.module.path,
        target_dir,
        external_imports,
    )
}

pub fn discover_dependency_module_packages(
    module_dir: &Path,
    external_imports: &ExternalImports,
) -> Result<PackageGraph, String> {
    let manifest = load_module_manifest(&module_dir.join("goml.toml"))?;
    discover_all_module_packages(module_dir, &manifest.module.path, external_imports)
}

fn discover_reachable_module_packages(
    module_dir: &Path,
    module_path: &str,
    target_dir: &Path,
    external_imports: &ExternalImports,
) -> Result<PackageGraph, String> {
    let entry_package = package_import_path(module_path, module_dir, target_dir)?;
    let mut entry = load_package(target_dir, &entry_package, module_path, external_imports)?;
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
            if !package_dir.is_dir() {
                return Err(format!(
                    "package {} not found at {}",
                    import_path,
                    package_dir.display()
                ));
            }
            let package = load_package(&package_dir, &import_path, module_path, external_imports)?;
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

fn discover_all_module_packages(
    module_dir: &Path,
    module_path: &str,
    external_imports: &ExternalImports,
) -> Result<PackageGraph, String> {
    let mut dirs = Vec::new();
    collect_package_dirs(module_dir, module_dir, &mut dirs)?;
    dirs.sort();
    if dirs.is_empty() {
        return Err(format!("module {} has no package directories", module_path));
    }

    let mut packages = HashMap::new();
    let mut package_dirs = HashMap::new();
    for dir in dirs {
        let import_path = package_import_path(module_path, module_dir, &dir)?;
        let package = load_package(&dir, &import_path, module_path, external_imports)?;
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
) -> Result<PackageUnit, String> {
    let paths = read_gom_sources(package_dir)?;
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
    let Some(path) = read_gom_sources(&candidate_dir)?.into_iter().next() else {
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

fn read_gom_sources(dir: &Path) -> Result<Vec<PathBuf>, String> {
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
        if path.is_file() && path.extension().is_some_and(|ext| ext == "gom") {
            files.push(path);
        }
    }
    files.sort();
    Ok(files)
}

fn collect_package_dirs(
    module_dir: &Path,
    dir: &Path,
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
        if path.is_file() && path.extension().is_some_and(|extension| extension == "gom") {
            has_source = true;
        } else if path.is_dir() {
            let name = entry.file_name();
            let name = name.to_string_lossy();
            if !name.starts_with('.') && name != "target" {
                children.push(path);
            }
        }
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
