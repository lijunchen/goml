use std::collections::{HashMap, HashSet};

use ast::ast;

use crate::interface;

#[derive(Debug, Clone, Default)]
pub struct ExternalImports {
    pub package_names: HashSet<String>,
    pub import_paths: HashMap<String, String>,
}

impl ExternalImports {
    pub fn new(package_names: HashSet<String>, import_paths: HashMap<String, String>) -> Self {
        Self {
            package_names,
            import_paths,
        }
    }

    pub fn contains_package(&self, package: &str) -> bool {
        self.package_names.contains(package)
    }

    pub fn alias_for_use_path(&self, path: &ast::Path) -> Option<String> {
        resolve_external_import_prefix_with_map(path, &self.import_paths)
            .or_else(|| resolve_alias_root_import_prefix(path, &self.package_names))
            .map(|(alias, _)| alias)
    }

    pub fn is_exact_package_import(&self, path: &ast::Path) -> bool {
        resolve_external_import_prefix_with_map(path, &self.import_paths)
            .or_else(|| resolve_alias_root_import_prefix(path, &self.package_names))
            .is_some_and(|(_, segments)| segments == path.len())
    }
}

pub fn resolve_external_import_prefix_with_map(
    path: &ast::Path,
    import_paths: &HashMap<String, String>,
) -> Option<(String, usize)> {
    let mut best = None;
    for end in 2..=path.len() {
        let prefix = path_prefix(path, end);
        if let Some(alias) = import_paths.get(&prefix) {
            best = Some((alias.clone(), end));
        }
    }
    best
}

pub fn resolve_external_import_prefix(
    path: &ast::Path,
    deps: &HashMap<String, interface::PackageInterface>,
) -> Option<(String, usize)> {
    let mut best = None;
    for end in 2..=path.len() {
        let prefix = path_prefix(path, end);
        for (alias, dep) in deps {
            if dep.packages.contains(&prefix) {
                best = Some((alias.clone(), end));
                break;
            }
        }
    }
    let package_names = deps.keys().cloned().collect();
    if let Some(alias_match) = resolve_alias_root_import_prefix(path, &package_names) {
        best = Some(alias_match);
    }
    best
}

pub fn external_import_alias(
    path: &ast::Path,
    deps: &HashMap<String, interface::PackageInterface>,
) -> Option<String> {
    resolve_external_import_prefix(path, deps).map(|(alias, _)| alias)
}

pub fn is_exact_external_package_import(
    path: &ast::Path,
    deps: &HashMap<String, interface::PackageInterface>,
) -> bool {
    resolve_external_import_prefix(path, deps).is_some_and(|(_, segments)| segments == path.len())
}

fn path_prefix(path: &ast::Path, end: usize) -> String {
    path.segments()[..end]
        .iter()
        .map(|segment| segment.ident.0.clone())
        .collect::<Vec<_>>()
        .join("::")
}

fn resolve_alias_root_import_prefix(
    path: &ast::Path,
    package_names: &HashSet<String>,
) -> Option<(String, usize)> {
    let segments = path.segments();
    let first = segments.first()?.ident.0.as_str();
    if !package_names.contains(first) {
        return None;
    }

    let mut best = Some((first.to_string(), 1));
    for end in 2..=segments.len() {
        let candidate = segments[1..end]
            .iter()
            .map(|segment| segment.ident.0.clone())
            .collect::<Vec<_>>()
            .join("::");
        if package_names.contains(&candidate) {
            best = Some((candidate, end));
        }
    }
    best
}
