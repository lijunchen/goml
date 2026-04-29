use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
};

use cst::cst::CstNode;
use parser::syntax::MySyntaxNode;
use text_size::{TextRange, TextSize};

use super::DefinitionLocation;
use crate::package_names::{BUILTIN_PACKAGE, ROOT_PACKAGE, is_special_unqualified_package};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct MemberLookupKey {
    owner: String,
    member: String,
}

impl MemberLookupKey {
    fn new(owner: impl Into<String>, member: impl Into<String>) -> Self {
        Self {
            owner: owner.into(),
            member: member.into(),
        }
    }

    fn from_refs(owner: &str, member: &str) -> Self {
        Self::new(owner.to_string(), member.to_string())
    }
}

#[derive(Debug, Default)]
pub(crate) struct ProjectSymbolIndex {
    values: HashMap<String, Vec<DefinitionLocation>>,
    types: HashMap<String, Vec<DefinitionLocation>>,
    impl_methods: HashMap<MemberLookupKey, Vec<DefinitionLocation>>,
    trait_methods: HashMap<MemberLookupKey, Vec<DefinitionLocation>>,
    struct_fields: HashMap<MemberLookupKey, Vec<DefinitionLocation>>,
    enum_variants: HashMap<MemberLookupKey, Vec<DefinitionLocation>>,
    namespaces: HashMap<String, PathBuf>,
}

impl ProjectSymbolIndex {
    fn add_value(&mut self, name: String, loc: DefinitionLocation) {
        self.values.entry(name).or_default().push(loc);
    }

    fn add_type(&mut self, name: String, loc: DefinitionLocation) {
        self.types.entry(name).or_default().push(loc);
    }

    fn add_impl_method(&mut self, receiver: String, method: String, loc: DefinitionLocation) {
        self.impl_methods
            .entry(MemberLookupKey::new(receiver, method))
            .or_default()
            .push(loc);
    }

    fn add_trait_method(&mut self, tr: String, method: String, loc: DefinitionLocation) {
        self.trait_methods
            .entry(MemberLookupKey::new(tr, method))
            .or_default()
            .push(loc);
    }

    fn add_struct_field(&mut self, st: String, field: String, loc: DefinitionLocation) {
        self.struct_fields
            .entry(MemberLookupKey::new(st, field))
            .or_default()
            .push(loc);
    }

    fn add_enum_variant(&mut self, en: String, variant: String, loc: DefinitionLocation) {
        self.enum_variants
            .entry(MemberLookupKey::new(en, variant))
            .or_default()
            .push(loc);
    }

    pub(crate) fn find_value(&self, name: &str) -> Vec<DefinitionLocation> {
        self.values.get(name).cloned().unwrap_or_default()
    }

    pub(crate) fn find_type(&self, name: &str) -> Vec<DefinitionLocation> {
        self.types.get(name).cloned().unwrap_or_default()
    }

    pub(crate) fn find_impl_methods(
        &self,
        receiver: &str,
        method: &str,
    ) -> Vec<DefinitionLocation> {
        self.impl_methods
            .get(&MemberLookupKey::from_refs(receiver, method))
            .cloned()
            .unwrap_or_default()
    }

    pub(crate) fn find_trait_methods(&self, tr: &str, method: &str) -> Vec<DefinitionLocation> {
        self.trait_methods
            .get(&MemberLookupKey::from_refs(tr, method))
            .cloned()
            .unwrap_or_default()
    }

    pub(crate) fn find_struct_field(&self, st: &str, field: &str) -> Vec<DefinitionLocation> {
        self.struct_fields
            .get(&MemberLookupKey::from_refs(st, field))
            .cloned()
            .unwrap_or_default()
    }

    pub(crate) fn find_enum_variant(&self, en: &str, variant: &str) -> Vec<DefinitionLocation> {
        self.enum_variants
            .get(&MemberLookupKey::from_refs(en, variant))
            .cloned()
            .unwrap_or_default()
    }

    pub(crate) fn find_namespace(&self, namespace: &str) -> Option<PathBuf> {
        self.namespaces.get(namespace).cloned()
    }

    pub(crate) fn namespace_children(&self, namespace: &str) -> Vec<String> {
        let prefix = format!("{}::", namespace);
        let mut names = self
            .namespaces
            .keys()
            .filter_map(|name| {
                let rest = name.strip_prefix(&prefix)?;
                let child = rest.split("::").next()?;
                if child.is_empty() {
                    None
                } else {
                    Some(child.to_string())
                }
            })
            .collect::<Vec<_>>();
        names.sort();
        names.dedup();
        names
    }
}

pub(crate) struct SymbolLookup {
    pub(crate) graph: Option<crate::pipeline::packages::PackageGraph>,
    pub(crate) index: ProjectSymbolIndex,
}

pub(crate) fn build_symbol_lookup(path: &Path, src: &str) -> SymbolLookup {
    match build_symbol_index(path, src) {
        Ok((graph, index)) => SymbolLookup {
            graph: Some(graph),
            index,
        },
        Err(_) => {
            let mut index = ProjectSymbolIndex::default();
            let _ = index_source_file_symbols(&mut index, &[ROOT_PACKAGE.to_string()], path, src);
            let _ = index_builtin_symbols(&mut index);
            SymbolLookup { graph: None, index }
        }
    }
}

pub(crate) fn build_symbol_index(
    path: &Path,
    src: &str,
) -> Result<(crate::pipeline::packages::PackageGraph, ProjectSymbolIndex), String> {
    if crate::config::find_crate_root(start_dir_for_path(path)).is_some() {
        build_crate_symbol_index(path, src)
    } else {
        Err("namespace discovery fallback is disabled outside manifest crates".to_string())
    }
}

fn build_crate_symbol_index(
    path: &Path,
    src: &str,
) -> Result<(crate::pipeline::packages::PackageGraph, ProjectSymbolIndex), String> {
    let start_dir = start_dir_for_path(path);
    let Some((crate_dir, _)) = crate::config::find_crate_root(start_dir) else {
        return Err("crate root not found".to_string());
    };

    let crate_unit = crate::pipeline::modules::discover_crate_from_dir(&crate_dir)
        .map_err(|err| format!("crate module discovery failed: {:?}", err))?;
    let crate_name = crate_unit.config.name.clone();
    let current_path = canonical_path(path);
    let mut overrides = HashMap::new();
    for module in crate_unit.modules.iter() {
        if canonical_path(&module.file_path) == current_path {
            overrides.insert(module.file_path.clone(), src.to_string());
        }
    }

    let mut graph = crate::pipeline::packages::PackageGraph {
        module_dir: crate_unit.root_dir.clone(),
        module_name: Some(crate_name.clone()),
        entry_package: crate_name.clone(),
        packages: HashMap::new(),
        discovery_order: Vec::new(),
        package_dirs: HashMap::new(),
        package_visibilities: HashMap::new(),
        external_root_packages: HashSet::new(),
    };
    let mut index = ProjectSymbolIndex::default();

    for module in crate_unit.modules.iter() {
        let namespace_names = query_module_namespace_names(module.path.segments(), &crate_name);
        let namespace_name = namespace_names[0].clone();
        let namespace_dir = namespace_dir_for_module_file(&module.file_path);
        let files = vec![crate::hir::SourceFileAst::with_package_module_visibility(
            module.file_path.clone(),
            crate_name.clone(),
            module.path.segments().to_vec(),
            true,
            module.ast.clone(),
        )];
        graph.discovery_order.push(namespace_name.clone());
        graph
            .package_dirs
            .insert(namespace_name.clone(), namespace_dir.clone());
        graph
            .package_visibilities
            .insert(namespace_name.clone(), module.visibility);
        for alias in namespace_names.iter().skip(1) {
            graph
                .package_dirs
                .insert(alias.clone(), namespace_dir.clone());
            graph
                .package_visibilities
                .insert(alias.clone(), module.visibility);
        }
        graph.packages.insert(
            namespace_name.clone(),
            crate::pipeline::packages::PackageUnit {
                name: namespace_name.clone(),
                files,
                imports: HashSet::new(),
            },
        );
        index_namespace_symbols_named(
            &mut index,
            &namespace_name,
            &namespace_names,
            &namespace_dir,
            std::slice::from_ref(&module.file_path),
            &overrides,
        )?;
    }

    if let Ok((_module_dir, dependencies)) =
        crate::pipeline::packages::discover_dependency_versions_from_file(path)
    {
        let external_deps = crate::external::resolve_dependency_versions(&dependencies)
            .map_err(|err| err.to_string())?;
        external_deps
            .augment_graph(&mut graph)
            .map_err(|err| err.to_string())?;
        for (logical_name, files) in external_deps.package_sources() {
            let Some(namespace_dir) = graph.package_dirs.get(&logical_name) else {
                continue;
            };
            index_namespace_symbols_named(
                &mut index,
                &logical_name,
                std::slice::from_ref(&logical_name),
                namespace_dir,
                &files,
                &HashMap::new(),
            )?;
        }
    }

    index_builtin_symbols(&mut index)?;
    Ok((graph, index))
}

fn query_module_namespace_name(module_path: &[String], crate_name: &str) -> String {
    if module_path.is_empty() {
        crate_name.to_string()
    } else {
        module_path.join("::")
    }
}

fn query_module_namespace_names(module_path: &[String], crate_name: &str) -> Vec<String> {
    let primary = query_module_namespace_name(module_path, crate_name);
    let mut names = vec![primary.clone()];
    if !module_path.is_empty() {
        let alias = format!("{}::{}", crate_name, module_path.join("::"));
        if alias != primary {
            names.push(alias);
        }
    }
    names
}

fn namespace_dir_for_module_file(path: &Path) -> PathBuf {
    if path.file_name().is_some_and(|name| name == "mod.gom") {
        path.parent()
            .filter(|parent| !parent.as_os_str().is_empty())
            .unwrap_or_else(|| Path::new("."))
            .to_path_buf()
    } else {
        path.to_path_buf()
    }
}

fn canonical_path(path: &Path) -> PathBuf {
    path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
}

fn start_dir_for_path(path: &Path) -> &Path {
    path.parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."))
}

pub(crate) fn namespace_nav_target_in_dir(dir: &Path) -> Option<PathBuf> {
    if dir.is_file() {
        return Some(dir.to_path_buf());
    }

    let toml = dir.join("goml.toml");
    if toml.exists() {
        return Some(toml);
    }

    let mod_gom = dir.join("mod.gom");
    if mod_gom.exists() {
        return Some(mod_gom);
    }

    None
}

pub(crate) fn lookup_symbol_locations_for_path(
    graph: Option<&crate::pipeline::packages::PackageGraph>,
    index: &ProjectSymbolIndex,
    token_text: &str,
    segments: &[String],
) -> Vec<DefinitionLocation> {
    let segments = normalize_lookup_segments(segments);
    if segments.is_empty() {
        return Vec::new();
    }

    let mut locations = Vec::new();
    let full_name = segments.join("::");

    if segments[0] != BUILTIN_PACKAGE {
        for idx in 0..segments.len() {
            if token_text != segments[idx] {
                continue;
            }
            let namespace = segments[..=idx].join("::");
            if graph.and_then(|g| g.package_dirs.get(&namespace)).is_some()
                && let Some(loc) = index.find_namespace(&namespace)
            {
                locations.push(DefinitionLocation {
                    path: loc,
                    range: TextRange::new(TextSize::from(0), TextSize::from(0)),
                });
                return locations;
            }
        }
    }

    if segments.len() >= 2 {
        let enum_name = segments[..segments.len() - 1].join("::");
        let variant_name = segments[segments.len() - 1].as_str();
        if token_text == variant_name {
            locations.extend(index.find_enum_variant(&enum_name, variant_name));
            if !locations.is_empty() {
                return locations;
            }
        }

        if token_text == segments[segments.len() - 2] {
            locations.extend(index.find_type(&enum_name));
            if !locations.is_empty() {
                return locations;
            }
        }
    }

    locations.extend(index.find_value(&full_name));
    locations.extend(index.find_type(&full_name));
    locations
}

fn normalize_lookup_segments(segments: &[String]) -> &[String] {
    if matches!(
        segments.first().map(String::as_str),
        Some("crate" | "self" | "super")
    ) {
        &segments[1..]
    } else {
        segments
    }
}

fn index_source_file_symbols(
    index: &mut ProjectSymbolIndex,
    namespace_names: &[String],
    file_path: &Path,
    src: &str,
) -> Result<(), String> {
    let result = parser::parse(file_path, src);
    let root = MySyntaxNode::new_root(result.green_node);
    let cst_file =
        cst::cst::File::cast(root).ok_or_else(|| "failed to cast syntax tree".to_string())?;

    for item in cst_file.items() {
        match item {
            cst::nodes::Item::Mod(_) => {}
            cst::nodes::Item::Fn(f) => {
                if let Some(name_tok) = f.lident() {
                    let mut names = Vec::new();
                    add_name_variants(&mut names, namespace_names, &name_tok.to_string());
                    for name in names {
                        index.add_value(
                            name,
                            DefinitionLocation {
                                path: file_path.to_path_buf(),
                                range: name_tok.text_range(),
                            },
                        );
                    }
                }
            }
            cst::nodes::Item::Struct(s) => {
                let Some(type_tok) = s.uident() else {
                    continue;
                };
                let mut type_names = Vec::new();
                add_name_variants(&mut type_names, namespace_names, &type_tok.to_string());
                for tn in type_names.iter() {
                    index.add_type(
                        tn.clone(),
                        DefinitionLocation {
                            path: file_path.to_path_buf(),
                            range: type_tok.text_range(),
                        },
                    );
                }
                if let Some(fields) = s.field_list() {
                    for field in fields.fields() {
                        let Some(field_tok) = field.lident() else {
                            continue;
                        };
                        for tn in type_names.iter() {
                            index.add_struct_field(
                                tn.clone(),
                                field_tok.to_string(),
                                DefinitionLocation {
                                    path: file_path.to_path_buf(),
                                    range: field_tok.text_range(),
                                },
                            );
                        }
                    }
                }
            }
            cst::nodes::Item::Enum(e) => {
                let Some(type_tok) = e.uident() else {
                    continue;
                };
                let mut type_names = Vec::new();
                add_name_variants(&mut type_names, namespace_names, &type_tok.to_string());
                for tn in type_names.iter() {
                    index.add_type(
                        tn.clone(),
                        DefinitionLocation {
                            path: file_path.to_path_buf(),
                            range: type_tok.text_range(),
                        },
                    );
                }
                if let Some(variants) = e.variant_list() {
                    for variant in variants.variants() {
                        let Some(var_tok) = variant.uident() else {
                            continue;
                        };
                        for tn in type_names.iter() {
                            index.add_enum_variant(
                                tn.clone(),
                                var_tok.to_string(),
                                DefinitionLocation {
                                    path: file_path.to_path_buf(),
                                    range: var_tok.text_range(),
                                },
                            );
                        }
                    }
                }
            }
            cst::nodes::Item::Trait(t) => {
                let Some(type_tok) = t.uident() else {
                    continue;
                };
                let mut type_names = Vec::new();
                add_name_variants(&mut type_names, namespace_names, &type_tok.to_string());
                for tn in type_names.iter() {
                    index.add_type(
                        tn.clone(),
                        DefinitionLocation {
                            path: file_path.to_path_buf(),
                            range: type_tok.text_range(),
                        },
                    );
                }
                if let Some(methods) = t.trait_method_list() {
                    for m in methods.methods() {
                        let Some(method_tok) = m.lident() else {
                            continue;
                        };
                        for tn in type_names.iter() {
                            index.add_trait_method(
                                tn.clone(),
                                method_tok.to_string(),
                                DefinitionLocation {
                                    path: file_path.to_path_buf(),
                                    range: method_tok.text_range(),
                                },
                            );
                        }
                    }
                }
            }
            cst::nodes::Item::Impl(i) => {
                let receiver = i
                    .for_type()
                    .and_then(|t| cst_type_path_name(&t))
                    .unwrap_or_default();
                if receiver.is_empty() {
                    continue;
                }
                let receiver_keys = collect_receiver_keys(namespace_names, &receiver);
                for f in i.functions() {
                    let Some(method_tok) = f.lident() else {
                        continue;
                    };
                    for rk in receiver_keys.iter() {
                        index.add_impl_method(
                            rk.clone(),
                            method_tok.to_string(),
                            DefinitionLocation {
                                path: file_path.to_path_buf(),
                                range: method_tok.text_range(),
                            },
                        );
                    }
                }
            }
            cst::nodes::Item::Extern(ex) => {
                if ex.type_keyword().is_some() {
                    let Some(type_tok) = ex.uident() else {
                        continue;
                    };
                    let mut type_names = Vec::new();
                    add_name_variants(&mut type_names, namespace_names, &type_tok.to_string());
                    for tn in type_names.iter() {
                        index.add_type(
                            tn.clone(),
                            DefinitionLocation {
                                path: file_path.to_path_buf(),
                                range: type_tok.text_range(),
                            },
                        );
                    }
                } else if let Some(name_tok) = ex.lident() {
                    let mut names = Vec::new();
                    add_name_variants(&mut names, namespace_names, &name_tok.to_string());
                    for name in names {
                        index.add_value(
                            name,
                            DefinitionLocation {
                                path: file_path.to_path_buf(),
                                range: name_tok.text_range(),
                            },
                        );
                    }
                }
            }
        }
    }

    Ok(())
}

fn ident_tokens_to_segments(path: &cst::nodes::Path) -> Vec<String> {
    path.ident_tokens().map(|token| token.to_string()).collect()
}

fn qualify_name(namespace: &str, name: &str) -> String {
    if is_special_unqualified_package(namespace) {
        name.to_string()
    } else {
        format!("{}::{}", namespace, name)
    }
}

fn add_name_variants(map: &mut Vec<String>, namespaces: &[String], name: &str) {
    map.push(name.to_string());
    for namespace in namespaces {
        let qualified = qualify_name(namespace, name);
        if qualified != name {
            map.push(qualified);
        }
    }
    map.sort();
    map.dedup();
}

fn collect_receiver_keys(namespaces: &[String], receiver: &str) -> Vec<String> {
    let mut keys = Vec::new();
    if receiver.is_empty() {
        return keys;
    }

    keys.push(receiver.to_string());
    if receiver.contains("::") {
        if let Some(last) = receiver.rsplit("::").next()
            && last != receiver
        {
            keys.push(last.to_string());
        }
        return keys;
    }

    for namespace in namespaces {
        let qualified = qualify_name(namespace, receiver);
        if qualified != receiver {
            keys.push(qualified);
        }
    }
    keys.sort();
    keys.dedup();
    keys
}

fn cst_type_path_name(ty: &cst::nodes::Type) -> Option<String> {
    match ty {
        cst::nodes::Type::TAppTy(app) => {
            let path = app.path()?;
            let segments = ident_tokens_to_segments(&path);
            if segments.is_empty() {
                None
            } else {
                Some(segments.join("::"))
            }
        }
        cst::nodes::Type::DynTy(dyn_ty) => {
            let path = dyn_ty.path()?;
            let segments = ident_tokens_to_segments(&path);
            if segments.is_empty() {
                None
            } else {
                Some(segments.join("::"))
            }
        }
        _ => None,
    }
}

fn index_namespace_symbols_named(
    index: &mut ProjectSymbolIndex,
    namespace_name: &str,
    namespace_names: &[String],
    namespace_dir: &Path,
    source_files: &[PathBuf],
    src_overrides: &HashMap<PathBuf, String>,
) -> Result<(), String> {
    let namespace_names = if namespace_names.is_empty() {
        vec![namespace_name.to_string()]
    } else {
        namespace_names.to_vec()
    };
    if let Some(target) = namespace_navigation_target(namespace_dir, source_files) {
        for namespace in &namespace_names {
            index.namespaces.insert(namespace.clone(), target.clone());
        }
    }

    for file in source_files {
        let src = if let Some(override_src) = src_overrides.get(file) {
            override_src.clone()
        } else {
            fs::read_to_string(file)
                .map_err(|e| format!("failed to read {}: {}", file.display(), e))?
        };
        index_source_file_symbols(index, &namespace_names, file, &src)?;
    }

    Ok(())
}

fn namespace_navigation_target(namespace_dir: &Path, source_files: &[PathBuf]) -> Option<PathBuf> {
    if let Some(target) = namespace_nav_target_in_dir(namespace_dir) {
        return Some(target);
    }
    match source_files {
        [file] => Some(file.clone()),
        _ => None,
    }
}

fn index_builtin_symbols(index: &mut ProjectSymbolIndex) -> Result<(), String> {
    let builtin_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src")
        .join("builtin.gom");
    let Ok(src) = fs::read_to_string(&builtin_path) else {
        return Ok(());
    };
    index_source_file_symbols(index, &[BUILTIN_PACKAGE.to_string()], &builtin_path, &src)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn namespace_navigation_target_prefers_mod_file_over_sorted_source() {
        let dir = tempfile::tempdir().unwrap();
        let namespace_dir = dir.path().join("pkg");
        fs::create_dir_all(&namespace_dir).unwrap();
        let first = namespace_dir.join("a.gom");
        let module = namespace_dir.join("mod.gom");
        fs::write(&first, "").unwrap();
        fs::write(&module, "").unwrap();

        let target = namespace_navigation_target(&namespace_dir, &[first, module.clone()]);

        assert_eq!(target, Some(module));
    }

    #[test]
    fn namespace_navigation_target_ignores_ambiguous_multi_file_dir() {
        let dir = tempfile::tempdir().unwrap();
        let namespace_dir = dir.path().join("pkg");
        fs::create_dir_all(&namespace_dir).unwrap();
        let first = namespace_dir.join("a.gom");
        let second = namespace_dir.join("b.gom");
        fs::write(&first, "").unwrap();
        fs::write(&second, "").unwrap();

        let target = namespace_navigation_target(&namespace_dir, &[first, second]);

        assert_eq!(target, None);
    }
}
