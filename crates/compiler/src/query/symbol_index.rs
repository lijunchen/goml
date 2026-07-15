use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};

use cst::cst::CstNode;
use parser::syntax::MySyntaxNode;
use text_size::{TextRange, TextSize};

use super::DefinitionLocation;
use crate::package_names::{
    BUILTIN_PACKAGE, ROOT_PACKAGE, STD_PACKAGE, is_special_unqualified_package,
};

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
    packages: HashMap<String, PathBuf>,
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

    pub(crate) fn find_package(&self, pkg: &str) -> Option<PathBuf> {
        self.packages.get(pkg).cloned()
    }
}

pub(crate) struct SymbolLookup {
    pub(crate) graph: Option<crate::pipeline::packages::PackageGraph>,
    pub(crate) index: ProjectSymbolIndex,
    package_aliases: HashMap<String, String>,
}

impl SymbolLookup {
    pub(crate) fn canonicalize_segments(&self, segments: &[String]) -> Vec<String> {
        let Some((first, rest)) = segments.split_first() else {
            return Vec::new();
        };
        let Some(package) = self.package_aliases.get(first) else {
            return segments.to_vec();
        };
        let mut canonical = Vec::with_capacity(segments.len());
        canonical.push(package.clone());
        canonical.extend(rest.iter().cloned());
        canonical
    }

    pub(crate) fn package_for_alias(&self, alias: &str) -> Option<&str> {
        self.package_aliases.get(alias).map(String::as_str)
    }

    pub(crate) fn default_alias_for_package(&self, package: &str) -> String {
        self.graph
            .as_ref()
            .and_then(|graph| graph.declared_package_names.get(package))
            .cloned()
            .unwrap_or_else(|| package.rsplit("::").next().unwrap_or(package).to_string())
    }
}

pub(crate) fn build_symbol_lookup(path: &Path, src: &str) -> SymbolLookup {
    build_symbol_lookup_with_overrides(path, src, &HashMap::new())
}

pub(crate) fn build_symbol_lookup_with_overrides(
    path: &Path,
    src: &str,
    source_overrides: &HashMap<PathBuf, String>,
) -> SymbolLookup {
    match build_symbol_index_with_overrides(path, src, source_overrides) {
        Ok((graph, index)) => {
            let package_aliases = package_aliases(path, src, &graph);
            SymbolLookup {
                graph: Some(graph),
                index,
                package_aliases,
            }
        }
        Err(_) => {
            let mut index = ProjectSymbolIndex::default();
            let _ = index_source_file_symbols(&mut index, ROOT_PACKAGE, path, src);
            let _ = index_builtin_symbols(&mut index);
            SymbolLookup {
                graph: None,
                index,
                package_aliases: HashMap::new(),
            }
        }
    }
}

fn package_aliases(
    path: &Path,
    src: &str,
    graph: &crate::pipeline::packages::PackageGraph,
) -> HashMap<String, String> {
    let Ok(ast) = parse_ast_for_discovery(path, src) else {
        return HashMap::new();
    };
    let mut candidates: HashMap<String, Option<String>> = HashMap::new();
    for use_decl in ast.uses {
        let package = use_decl.path.display();
        if !graph.package_dirs.contains_key(&package) {
            continue;
        }
        let alias = use_decl
            .alias
            .map(|alias| alias.0)
            .or_else(|| graph.declared_package_names.get(&package).cloned())
            .unwrap_or_else(|| package.rsplit("::").next().unwrap_or(&package).to_string());
        match candidates.get_mut(&alias) {
            Some(existing) if existing.as_ref() == Some(&package) => {}
            Some(existing) => *existing = None,
            None => {
                candidates.insert(alias, Some(package));
            }
        }
    }
    candidates
        .into_iter()
        .filter_map(|(alias, package)| package.map(|package| (alias, package)))
        .collect()
}

pub(crate) fn build_symbol_index_with_overrides(
    path: &Path,
    src: &str,
    source_overrides: &HashMap<PathBuf, String>,
) -> Result<(crate::pipeline::packages::PackageGraph, ProjectSymbolIndex), String> {
    let mut graph = discover_packages_for_query(path, src, source_overrides)?;
    let mut index = ProjectSymbolIndex::default();
    let mut overrides = source_overrides.clone();
    overrides.insert(path.to_path_buf(), src.to_string());

    for (pkg_name, unit) in graph.packages.iter() {
        let Some(pkg_dir) = graph.package_dirs.get(pkg_name) else {
            continue;
        };
        let package_files = unit
            .files
            .iter()
            .map(|file| file.path.clone())
            .collect::<Vec<_>>();
        index_package_symbols_named(&mut index, pkg_name, pkg_dir, &package_files, &overrides)?;
    }

    let mut external_deps = stdlib_deps_for_graph(&graph)?;
    if let Ok((_module_dir, dependencies)) =
        crate::pipeline::packages::discover_dependency_versions_from_file(path)
    {
        let registry_deps = crate::external::resolve_dependency_versions(&dependencies)
            .map_err(|err| err.to_string())?;
        external_deps.modules.extend(registry_deps.modules);
    }

    external_deps
        .augment_graph(&mut graph)
        .map_err(|err| err.to_string())?;
    let reachable_external = external_deps.reachable_package_names(&graph)?;
    for (logical_name, files) in external_deps.package_sources() {
        if !reachable_external.contains(&logical_name) {
            continue;
        }
        let Some(pkg_dir) = graph.package_dirs.get(&logical_name) else {
            continue;
        };
        index_package_symbols_named(&mut index, &logical_name, pkg_dir, &files, &HashMap::new())?;
    }

    index_builtin_symbols(&mut index)?;
    Ok((graph, index))
}

fn graph_imports_package(graph: &crate::pipeline::packages::PackageGraph, package: &str) -> bool {
    graph.packages.values().any(|unit| {
        unit.imports
            .iter()
            .any(|import| import == package || import.starts_with(&format!("{package}::")))
    })
}

fn stdlib_deps_for_graph(
    graph: &crate::pipeline::packages::PackageGraph,
) -> Result<crate::external::ExternalDependencyArtifacts, String> {
    let mut deps = crate::external::ExternalDependencyArtifacts::default();
    if graph_imports_package(graph, STD_PACKAGE) {
        deps.modules.insert(
            STD_PACKAGE.to_string(),
            crate::stdlib::stdlib_artifact().map_err(|err| err.to_string())?,
        );
    }
    Ok(deps)
}

pub(crate) fn lookup_symbol_locations_for_path(
    graph: Option<&crate::pipeline::packages::PackageGraph>,
    index: &ProjectSymbolIndex,
    token_text: &str,
    segments: &[String],
) -> Vec<DefinitionLocation> {
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
            let package = segments[..=idx].join("::");
            if graph.and_then(|g| g.package_dirs.get(&package)).is_some()
                && let Some(loc) = index.find_package(&package)
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

fn index_source_file_symbols(
    index: &mut ProjectSymbolIndex,
    package_name: &str,
    file_path: &Path,
    src: &str,
) -> Result<(), String> {
    let result = parser::parse(file_path, src);
    let root = MySyntaxNode::new_root(result.green_node);
    let cst_file =
        cst::cst::File::cast(root).ok_or_else(|| "failed to cast syntax tree".to_string())?;

    for item in cst_file.items() {
        match item {
            cst::nodes::Item::Fn(f) => {
                if let Some(name_tok) = f.lident() {
                    let mut names = Vec::new();
                    add_name_variants(&mut names, package_name, &name_tok.to_string());
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
                add_name_variants(&mut type_names, package_name, &type_tok.to_string());
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
                add_name_variants(&mut type_names, package_name, &type_tok.to_string());
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
                add_name_variants(&mut type_names, package_name, &type_tok.to_string());
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
                let receiver_keys = collect_receiver_keys(package_name, &receiver);
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
                    add_name_variants(&mut type_names, package_name, &type_tok.to_string());
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
                    add_name_variants(&mut names, package_name, &name_tok.to_string());
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

fn parse_ast_for_discovery(path: &Path, src: &str) -> Result<::ast::ast::File, String> {
    let result = parser::parse(path, src);
    let (green_node, mut diagnostics) = result.into_parts();
    let root = MySyntaxNode::new_root(green_node);
    let cst = cst::cst::File::cast(root).ok_or_else(|| "failed to cast CST".to_string())?;
    let lower = ::ast::lower::lower(cst);
    let (ast, mut lower_diagnostics) = lower.into_parts();
    diagnostics.append(&mut lower_diagnostics);
    let ast = ast.ok_or_else(|| "AST lowering error".to_string())?;
    Ok(ast)
}

fn discover_packages_for_query(
    path: &Path,
    src: &str,
    source_overrides: &HashMap<PathBuf, String>,
) -> Result<crate::pipeline::packages::PackageGraph, String> {
    let entry_ast = parse_ast_for_discovery(path, src)?;
    let start_dir = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    if let Some((module_dir, _)) = crate::config::find_module_root(start_dir)? {
        let (_module_dir, dependencies) =
            crate::pipeline::packages::discover_dependency_versions_from_file(path)
                .map_err(|err| format!("{:?}", err))?;
        let external_deps = crate::external::resolve_dependency_versions(&dependencies)?;
        let external_imports = external_deps.external_imports();
        let mut parsed_overrides = source_overrides
            .iter()
            .map(|(path, src)| {
                (
                    path.clone(),
                    parse_ast_for_discovery(path, src).map_err(|message| {
                        crate::pipeline::compile_error(format!("{}: {}", path.display(), message))
                    }),
                )
            })
            .collect::<crate::pipeline::packages::SourceOverrides>();
        parsed_overrides.insert(path.to_path_buf(), Ok(entry_ast));
        let mode = crate::pipeline::packages::analysis_mode_for_path(&module_dir, path)
            .map_err(|error| format!("{:?}", error))?;
        let graph =
            crate::pipeline::packages::discover_packages_with_overrides_and_external_imports(
                &module_dir,
                Some(path),
                &parsed_overrides,
                mode,
                &external_imports,
            )
            .map_err(|e| format!("{:?}", e))?;
        return Ok(graph);
    }

    let root_dir = start_dir;
    if !path.exists() {
        crate::pipeline::packages::discover_packages_single_file_with_external_imports(
            root_dir,
            path,
            entry_ast,
            &crate::package_imports::ExternalImports::default(),
        )
        .map_err(|e| format!("{:?}", e))
    } else {
        crate::pipeline::packages::discover_packages_with_external_imports(
            root_dir,
            Some(path),
            Some(entry_ast),
            &crate::package_imports::ExternalImports::default(),
        )
        .map_err(|e| format!("{:?}", e))
    }
}

fn ident_tokens_to_segments(path: &cst::nodes::Path) -> Vec<String> {
    path.ident_tokens().map(|token| token.to_string()).collect()
}

fn qualify_name(package: &str, name: &str) -> String {
    if is_special_unqualified_package(package) {
        name.to_string()
    } else {
        format!("{}::{}", package, name)
    }
}

fn add_name_variants(map: &mut Vec<String>, package: &str, name: &str) {
    map.push(name.to_string());
    let qualified = qualify_name(package, name);
    if qualified != name {
        map.push(qualified);
    }
}

fn collect_receiver_keys(package: &str, receiver: &str) -> Vec<String> {
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

    let qualified = qualify_name(package, receiver);
    if qualified != receiver {
        keys.push(qualified);
    }
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

fn index_package_symbols_named(
    index: &mut ProjectSymbolIndex,
    package_name: &str,
    package_dir: &Path,
    package_files: &[PathBuf],
    src_overrides: &HashMap<PathBuf, String>,
) -> Result<(), String> {
    let package_goml = package_dir.join("goml.toml");
    if package_goml.exists() {
        index
            .packages
            .insert(package_name.to_string(), package_goml);
    } else {
        let mut gom_files = package_files.to_vec();
        gom_files.sort();
        if let Some(first) = gom_files.first() {
            index
                .packages
                .insert(package_name.to_string(), first.clone());
        }
    }

    for file in package_files {
        let src = if let Some(override_src) = src_overrides.get(file) {
            override_src.clone()
        } else {
            fs::read_to_string(file)
                .map_err(|e| format!("failed to read {}: {}", file.display(), e))?
        };
        index_source_file_symbols(index, package_name, file, &src)?;
    }

    Ok(())
}

fn index_builtin_symbols(index: &mut ProjectSymbolIndex) -> Result<(), String> {
    for file in crate::builtins::BUILTIN_SOURCE_FILES {
        let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("src")
            .join(file);
        let src = fs::read_to_string(&path)
            .map_err(|error| format!("failed to read {}: {}", path.display(), error))?;
        index_source_file_symbols(index, BUILTIN_PACKAGE, &path, &src)?;
    }
    Ok(())
}
