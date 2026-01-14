use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use ast::ast;

use crate::fir::SourceFileAst;
use crate::pipeline::compile_error;
use crate::pipeline::pipeline::{CompilationError, parse_ast_file};

pub trait PackageLayout {
    fn root_package_name(&self) -> &str;
    fn package_dir(&self, root_dir: &Path, entry_package: &str, package_name: &str) -> PathBuf;
}

pub struct FlatPackageLayout;

impl PackageLayout for FlatPackageLayout {
    fn root_package_name(&self) -> &str {
        "Main"
    }

    fn package_dir(&self, root_dir: &Path, entry_package: &str, package_name: &str) -> PathBuf {
        if package_name == entry_package {
            root_dir.to_path_buf()
        } else {
            root_dir.join(package_name)
        }
    }
}

#[derive(Debug)]
pub struct PackageUnit {
    pub name: String,
    pub files: Vec<SourceFileAst>,
    pub imports: HashSet<String>,
}

#[derive(Debug)]
pub struct PackageGraph {
    pub root_dir: PathBuf,
    pub entry_package: String,
    pub packages: HashMap<String, PackageUnit>,
    pub discovery_order: Vec<String>,
    pub package_dirs: HashMap<String, PathBuf>,
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

fn collect_imports(files: &[SourceFileAst]) -> HashSet<String> {
    files
        .iter()
        .flat_map(|file| file.ast.imports.iter())
        .map(|import| import.0.clone())
        .collect()
}

fn load_package(
    package_dir: &Path,
    entry_path: Option<&Path>,
    entry_ast: Option<ast::File>,
) -> Result<PackageUnit, CompilationError> {
    let mut files = Vec::new();
    let mut package_name = None;

    if let Some(ast) = entry_ast {
        package_name = Some(ast.package.0.clone());
        let path = entry_path.ok_or_else(|| {
            compile_error("entry path missing when entry ast is provided".to_string())
        })?;
        files.push(SourceFileAst {
            path: path.to_path_buf(),
            ast,
        });
    }

    for path in read_gom_sources(package_dir)? {
        if entry_path.is_some_and(|entry| entry == path) {
            continue;
        }
        let src = fs::read_to_string(&path)
            .map_err(|err| compile_error(format!("failed to read {}: {}", path.display(), err)))?;
        let ast = parse_ast_file(&path, &src)?;
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
        files.push(SourceFileAst { path, ast });
    }

    let Some(name) = package_name else {
        return Err(compile_error(format!(
            "package directory {} has no .gom files",
            package_dir.display()
        )));
    };

    let imports = collect_imports(&files);
    Ok(PackageUnit {
        name,
        files,
        imports,
    })
}

pub fn discover_packages(
    root_dir: &Path,
    entry_path: Option<&Path>,
    entry_ast: Option<ast::File>,
) -> Result<PackageGraph, CompilationError> {
    discover_packages_with_layout(&FlatPackageLayout, root_dir, entry_path, entry_ast)
}

pub fn discover_packages_with_layout(
    layout: &impl PackageLayout,
    root_dir: &Path,
    entry_path: Option<&Path>,
    entry_ast: Option<ast::File>,
) -> Result<PackageGraph, CompilationError> {
    let entry_package = load_package(root_dir, entry_path, entry_ast)?;
    let entry_name = entry_package.name.clone();
    if entry_name != layout.root_package_name() {
        return Err(compile_error(format!(
            "root directory {} must declare package {}, found {}",
            root_dir.display(),
            layout.root_package_name(),
            entry_name
        )));
    }
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
        let package_dir = layout.package_dir(root_dir, &entry_name, &package_name);
        let package = load_package(&package_dir, None, None)?;
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
        root_dir: root_dir.to_path_buf(),
        entry_package: entry_name,
        packages,
        discovery_order,
        package_dirs,
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
                    .unwrap_or_else(|| graph.root_dir.join(pkg).display().to_string());
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
        if !graph.packages.contains_key(&dep) {
            return Err(compile_error(format!(
                "package {} imports missing package {} in {}",
                name,
                dep,
                graph.root_dir.join(&dep).display()
            )));
        }
        visit_package(&dep, graph, temp, perm, stack, order)?;
    }

    stack.pop();
    temp.remove(name);
    perm.insert(name.to_string());
    order.push(name.to_string());
    Ok(())
}
