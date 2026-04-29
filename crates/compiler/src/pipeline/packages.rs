use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use ast::ast;

use crate::config::GomlConfig;
use crate::hir::SourceFileAst;
use crate::package_imports::ExternalImports;
use crate::package_names::ROOT_PACKAGE;
use crate::pipeline::compile_error;
use crate::pipeline::pipeline::{CompilationError, parse_ast_file};

#[derive(Debug)]
pub struct PackageUnit {
    pub name: String,
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
        if path.extension().is_some_and(|ext| ext == "gom") {
            files.push(path);
        }
    }

    files.sort();
    Ok(files)
}

fn package_dir_is_loadable(dir: &Path) -> bool {
    dir.is_dir()
}

fn package_file_is_loadable(path: &Path) -> bool {
    path.is_file()
}

fn import_from_legacy_use_trait(
    path: &ast::Path,
    external_imports: &ExternalImports,
) -> Option<String> {
    if let Some(alias) = external_imports.alias_for_use_path(path) {
        return Some(alias);
    }
    let segments = path.segments();
    if segments.first().is_some_and(|seg| seg.ident.0 == "crate") {
        return segments.get(1).map(|seg| seg.ident.0.clone());
    }
    segments.first().map(|seg| seg.ident.0.clone())
}

fn import_from_use_decl(path: &ast::Path, external_imports: &ExternalImports) -> Option<String> {
    if let Some(alias) = external_imports.alias_for_use_path(path) {
        return Some(alias);
    }
    path.segments().first().map(|seg| seg.ident.0.clone())
}

fn collect_imports(files: &[SourceFileAst], external_imports: &ExternalImports) -> HashSet<String> {
    files
        .iter()
        .flat_map(|file| {
            let from_imports = file.ast.imports.iter().map(|import| import.0.clone());
            let from_use_decls = file
                .ast
                .uses
                .iter()
                .filter_map(|decl| import_from_use_decl(&decl.path, external_imports));
            let from_use_traits = file
                .ast
                .use_traits
                .iter()
                .filter_map(|path| import_from_legacy_use_trait(path, external_imports));
            let from_mods = file.ast.toplevels.iter().filter_map(|item| match item {
                ast::Item::Mod(module) => Some(module.name.0.clone()),
                _ => None,
            });
            from_imports
                .chain(from_use_decls)
                .chain(from_use_traits)
                .chain(from_mods)
        })
        .collect()
}

fn source_override_for_dir<'a>(
    package_dir: &Path,
    source_override: Option<(&'a Path, &'a ast::File)>,
) -> Option<(&'a Path, &'a ast::File)> {
    source_override.filter(|(path, _)| {
        path.parent()
            .map(|parent| {
                if parent.as_os_str().is_empty() {
                    Path::new(".")
                } else {
                    parent
                }
            })
            .is_some_and(|parent| parent == package_dir)
    })
}

fn ast_with_package(mut ast: ast::File, package: &str) -> ast::File {
    ast.package = ast::AstIdent::new(package);
    ast
}

fn load_package_file(
    package_file: &Path,
    package_name: &str,
    source_override: Option<(&Path, &ast::File)>,
    external_imports: &ExternalImports,
) -> Result<PackageUnit, CompilationError> {
    let ast = if let Some((override_path, override_ast)) = source_override
        && override_path == package_file
    {
        ast_with_package(override_ast.clone(), package_name)
    } else {
        let src = fs::read_to_string(package_file).map_err(|err| {
            compile_error(format!(
                "failed to read {}: {}",
                package_file.display(),
                err
            ))
        })?;
        ast_with_package(parse_ast_file(package_file, &src)?, package_name)
    };
    let files = vec![SourceFileAst::new(package_file.to_path_buf(), ast)];
    let imports = collect_imports(&files, external_imports);
    Ok(PackageUnit {
        name: package_name.to_string(),
        files,
        imports,
    })
}

fn load_package(
    package_dir: &Path,
    expected_package_name: Option<&str>,
    source_override: Option<(&Path, &ast::File)>,
    external_imports: &ExternalImports,
) -> Result<PackageUnit, CompilationError> {
    let mut files = Vec::new();
    let mut package_name = expected_package_name.map(str::to_string);

    let source_override = source_override_for_dir(package_dir, source_override);

    if let Some((path, ast)) = source_override {
        let ast = if let Some(package) = expected_package_name {
            ast_with_package(ast.clone(), package)
        } else {
            ast.clone()
        };
        package_name.get_or_insert_with(|| ast.package.0.clone());
        files.push(SourceFileAst::new(path.to_path_buf(), ast));
    }

    for path in read_gom_sources(package_dir)? {
        if source_override.is_some_and(|(override_path, _)| override_path == path.as_path()) {
            continue;
        }
        let src = fs::read_to_string(&path)
            .map_err(|err| compile_error(format!("failed to read {}: {}", path.display(), err)))?;
        let mut ast = parse_ast_file(&path, &src)?;
        if let Some(expected) = expected_package_name {
            ast = ast_with_package(ast, expected);
        }
        if let Some(existing) = &package_name {
            if &ast.package.0 != existing {
                return Err(compile_error(format!(
                    "package mismatch in {}: expected {}, found {}",
                    path.display(),
                    existing,
                    ast.package.0
                )));
            }
        } else {
            package_name = Some(ast.package.0.clone());
        }
        files.push(SourceFileAst::new(path, ast));
    }

    let Some(name) = package_name else {
        return Err(compile_error(format!(
            "package directory {} has no .gom files",
            package_dir.display()
        )));
    };

    let imports = collect_imports(&files, external_imports);
    Ok(PackageUnit {
        name,
        files,
        imports,
    })
}

fn load_package_from_config(
    package_dir: &Path,
    config: &GomlConfig,
    source_override: Option<(&Path, &ast::File)>,
    external_imports: &ExternalImports,
) -> Result<PackageUnit, CompilationError> {
    let mut files = Vec::new();
    let entry_file_path = package_dir.join(&config.package.entry);
    let expected_package_name = &config.package.name;
    let source_override = source_override_for_dir(package_dir, source_override);

    if let Some((path, ast)) = source_override
        && path == entry_file_path.as_path()
    {
        files.push(SourceFileAst::new(
            path.to_path_buf(),
            ast_with_package(ast.clone(), expected_package_name),
        ));
    } else if entry_file_path.exists() {
        let src = fs::read_to_string(&entry_file_path).map_err(|err| {
            compile_error(format!(
                "failed to read {}: {}",
                entry_file_path.display(),
                err
            ))
        })?;
        let ast = ast_with_package(
            parse_ast_file(&entry_file_path, &src)?,
            expected_package_name,
        );
        files.push(SourceFileAst::new(entry_file_path.clone(), ast));
    }

    for path in read_gom_sources(package_dir)? {
        if path == entry_file_path {
            continue;
        }
        if let Some((override_path, override_ast)) = source_override
            && override_path == path.as_path()
        {
            files.push(SourceFileAst::new(
                override_path.to_path_buf(),
                ast_with_package(override_ast.clone(), expected_package_name),
            ));
            continue;
        }
        let src = fs::read_to_string(&path)
            .map_err(|err| compile_error(format!("failed to read {}: {}", path.display(), err)))?;
        let ast = ast_with_package(parse_ast_file(&path, &src)?, expected_package_name);
        files.push(SourceFileAst::new(path, ast));
    }

    if files.is_empty() {
        return Err(compile_error(format!(
            "package {} at {} has no .gom files",
            expected_package_name,
            package_dir.display()
        )));
    }

    let imports = collect_imports(&files, external_imports);
    Ok(PackageUnit {
        name: expected_package_name.clone(),
        files,
        imports,
    })
}

fn load_single_file_package(
    path: &Path,
    ast: &ast::File,
    external_imports: &ExternalImports,
) -> PackageUnit {
    let files = vec![SourceFileAst::new(path.to_path_buf(), ast.clone())];
    let imports = collect_imports(&files, external_imports);
    PackageUnit {
        name: ast.package.0.clone(),
        files,
        imports,
    }
}

pub fn discover_project_from_file(
    file_path: &Path,
) -> Result<(PathBuf, GomlConfig), CompilationError> {
    let start_dir = file_path
        .parent()
        .filter(|p| !p.as_os_str().is_empty())
        .unwrap_or(Path::new("."));

    GomlConfig::find_module_root(start_dir).ok_or_else(|| {
        compile_error(format!(
            "no goml.toml with [module] section found in ancestors of {}",
            file_path.display()
        ))
    })
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
    discover_packages_inner(
        root_dir,
        entry_path,
        entry_ast,
        false,
        false,
        external_imports,
    )
}

pub fn discover_packages_single_file(
    root_dir: &Path,
    entry_path: &Path,
    entry_ast: ast::File,
) -> Result<PackageGraph, CompilationError> {
    discover_packages_single_file_with_external_imports(
        root_dir,
        entry_path,
        entry_ast,
        &ExternalImports::default(),
    )
}

pub fn discover_packages_single_file_with_external_imports(
    root_dir: &Path,
    entry_path: &Path,
    entry_ast: ast::File,
    external_imports: &ExternalImports,
) -> Result<PackageGraph, CompilationError> {
    discover_packages_inner(
        root_dir,
        Some(entry_path),
        Some(entry_ast),
        true,
        false,
        external_imports,
    )
}

pub fn discover_dependency_packages(
    module_dir: &Path,
    config: &GomlConfig,
) -> Result<PackageGraph, CompilationError> {
    discover_dependency_packages_with_external_imports(
        module_dir,
        config,
        &ExternalImports::default(),
    )
}

pub fn discover_dependency_packages_with_external_imports(
    module_dir: &Path,
    config: &GomlConfig,
    external_imports: &ExternalImports,
) -> Result<PackageGraph, CompilationError> {
    discover_packages_from_config(
        module_dir,
        config,
        None,
        None,
        false,
        true,
        external_imports,
    )
}

fn discover_packages_inner(
    root_dir: &Path,
    entry_path: Option<&Path>,
    entry_ast: Option<ast::File>,
    single_file: bool,
    allow_non_main_module_root: bool,
    external_imports: &ExternalImports,
) -> Result<PackageGraph, CompilationError> {
    if let Some(config) = GomlConfig::find_package_config(root_dir) {
        return discover_packages_from_config(
            root_dir,
            &config,
            entry_path,
            entry_ast,
            single_file,
            allow_non_main_module_root,
            external_imports,
        );
    }

    let source_override = match (entry_path, entry_ast.as_ref()) {
        (Some(path), Some(ast)) => Some((path, ast)),
        _ => None,
    };
    let entry_package = if single_file {
        if let Some((path, ast)) = source_override {
            load_single_file_package(path, ast, external_imports)
        } else {
            load_package(
                root_dir,
                None,
                source_override_for_dir(root_dir, source_override),
                external_imports,
            )?
        }
    } else {
        load_package(
            root_dir,
            None,
            source_override_for_dir(root_dir, source_override),
            external_imports,
        )?
    };
    let entry_name = entry_package.name.clone();

    let mut packages = HashMap::new();
    let mut discovery_order = Vec::new();
    let mut package_dirs = HashMap::new();
    let mut queue: Vec<String> = entry_package.imports.iter().cloned().collect();
    let mut loaded = HashSet::new();

    loaded.insert(entry_name.clone());
    packages.insert(entry_name.clone(), entry_package);
    discovery_order.push(entry_name.clone());
    package_dirs.insert(entry_name.clone(), root_dir.to_path_buf());

    while let Some(package_name) = queue.pop() {
        if loaded.contains(&package_name) {
            continue;
        }
        if external_imports.contains_package(&package_name) {
            loaded.insert(package_name);
            continue;
        }
        let package_dir = root_dir.join(&package_name);
        let package_file = root_dir.join(format!("{package_name}.gom"));
        if !package_dir_is_loadable(&package_dir) && !package_file_is_loadable(&package_file) {
            loaded.insert(package_name);
            continue;
        }
        let package_override = source_override_for_dir(&package_dir, source_override);
        let package = if package_dir_is_loadable(&package_dir) {
            if let Some(config) = GomlConfig::find_package_config(&package_dir) {
                load_package_from_config(&package_dir, &config, package_override, external_imports)?
            } else {
                load_package(
                    &package_dir,
                    Some(&package_name),
                    package_override,
                    external_imports,
                )?
            }
        } else {
            load_package_file(
                &package_file,
                &package_name,
                source_override,
                external_imports,
            )?
        };
        let declared_name = package.name.clone();
        if package.name != package_name {
            return Err(compile_error(format!(
                "package directory {} declares package {}, expected {}",
                package_dir.display(),
                package.name,
                package_name
            )));
        }
        queue.extend(package.imports.iter().cloned());
        loaded.insert(declared_name.clone());
        packages.insert(declared_name.clone(), package);
        discovery_order.push(declared_name.clone());
        package_dirs.insert(declared_name, package_dir);
    }

    Ok(PackageGraph {
        module_dir: root_dir.to_path_buf(),
        module_name: None,
        entry_package: entry_name,
        packages,
        discovery_order,
        package_dirs,
        external_root_packages: HashSet::new(),
    })
}

pub fn discover_packages_from_config(
    module_dir: &Path,
    config: &GomlConfig,
    entry_path: Option<&Path>,
    entry_ast: Option<ast::File>,
    single_file: bool,
    allow_non_main_module_root: bool,
    external_imports: &ExternalImports,
) -> Result<PackageGraph, CompilationError> {
    if config.is_module_root() && !allow_non_main_module_root && config.package.name != ROOT_PACKAGE
    {
        return Err(compile_error(format!(
            "module root package must be `main`, found `{}` in {}",
            config.package.name,
            module_dir.join("goml.toml").display()
        )));
    }

    let source_override = match (entry_path, entry_ast.as_ref()) {
        (Some(path), Some(ast)) => Some((path, ast)),
        _ => None,
    };
    let entry_package = if single_file {
        if let Some((path, ast)) = source_override {
            load_single_file_package(path, ast, external_imports)
        } else {
            load_package_from_config(
                module_dir,
                config,
                source_override_for_dir(module_dir, source_override),
                external_imports,
            )?
        }
    } else {
        load_package_from_config(
            module_dir,
            config,
            source_override_for_dir(module_dir, source_override),
            external_imports,
        )?
    };
    let entry_name = entry_package.name.clone();

    let mut packages = HashMap::new();
    let mut discovery_order = Vec::new();
    let mut package_dirs = HashMap::new();
    let mut queue: Vec<String> = entry_package.imports.iter().cloned().collect();
    let mut loaded = HashSet::new();

    loaded.insert(entry_name.clone());
    packages.insert(entry_name.clone(), entry_package);
    discovery_order.push(entry_name.clone());
    package_dirs.insert(entry_name.clone(), module_dir.to_path_buf());

    while let Some(package_name) = queue.pop() {
        if loaded.contains(&package_name) {
            continue;
        }
        if external_imports.contains_package(&package_name) {
            loaded.insert(package_name);
            continue;
        }
        let package_dir = module_dir.join(&package_name);
        let package_file = module_dir.join(format!("{package_name}.gom"));
        if !package_dir_is_loadable(&package_dir) && !package_file_is_loadable(&package_file) {
            loaded.insert(package_name);
            continue;
        }
        let package_override = source_override_for_dir(&package_dir, source_override);
        let package = if package_dir_is_loadable(&package_dir) {
            if let Some(pkg_config) = GomlConfig::find_package_config(&package_dir) {
                load_package_from_config(
                    &package_dir,
                    &pkg_config,
                    package_override,
                    external_imports,
                )?
            } else {
                load_package(
                    &package_dir,
                    Some(&package_name),
                    package_override,
                    external_imports,
                )?
            }
        } else {
            load_package_file(
                &package_file,
                &package_name,
                source_override,
                external_imports,
            )?
        };
        let declared_name = package.name.clone();
        if package.name != package_name {
            return Err(compile_error(format!(
                "package directory {} declares package {}, expected {}",
                package_dir.display(),
                package.name,
                package_name
            )));
        }
        queue.extend(package.imports.iter().cloned());
        loaded.insert(declared_name.clone());
        packages.insert(declared_name.clone(), package);
        discovery_order.push(declared_name.clone());
        package_dirs.insert(declared_name, package_dir);
    }

    Ok(PackageGraph {
        module_dir: module_dir.to_path_buf(),
        module_name: config.module.as_ref().map(|m| m.name.clone()),
        entry_package: entry_name,
        packages,
        discovery_order,
        package_dirs,
        external_root_packages: HashSet::new(),
    })
}

pub fn topo_sort_packages(graph: &PackageGraph) -> Result<Vec<String>, CompilationError> {
    let mut temp = HashSet::new();
    let mut perm = HashSet::new();
    let mut order = Vec::new();
    let mut stack = Vec::new();

    let mut names: Vec<String> = graph.packages.keys().cloned().collect();
    names.sort();

    for name in names {
        if perm.contains(&name) {
            continue;
        }
        visit_package(&name, graph, &mut temp, &mut perm, &mut stack, &mut order)?;
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
        let mut cycle = Vec::new();
        if let Some(pos) = stack.iter().position(|n| n == name) {
            cycle.extend_from_slice(&stack[pos..]);
        }
        cycle.push(name.to_string());
        let display = cycle
            .iter()
            .map(|pkg| {
                let dir = graph
                    .package_dirs
                    .get(pkg)
                    .map(|dir| dir.display().to_string())
                    .unwrap_or_else(|| graph.module_dir.join(pkg).display().to_string());
                format!("{} ({})", pkg, dir)
            })
            .collect::<Vec<_>>()
            .join(" -> ");
        return Err(compile_error(format!(
            "package dependency cycle detected: {}",
            display
        )));
    }

    temp.insert(name.to_string());
    stack.push(name.to_string());

    let Some(package) = graph.packages.get(name) else {
        return Err(compile_error(format!(
            "package {} not found during dependency walk",
            name
        )));
    };
    let mut deps: Vec<String> = package.imports.iter().cloned().collect();
    deps.sort();

    for dep in deps {
        if !graph.packages.contains_key(&dep) && !graph.external_root_packages.contains(&dep) {
            return Err(compile_error(format!(
                "package {} imports missing package {} in {}",
                name,
                dep,
                graph.module_dir.join(&dep).display()
            )));
        }
        if graph.external_root_packages.contains(&dep) {
            continue;
        }
        visit_package(&dep, graph, temp, perm, stack, order)?;
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

    pub fn add_external_package_dir(&mut self, package: impl Into<String>, dir: PathBuf) {
        self.package_dirs.insert(package.into(), dir);
    }
}
