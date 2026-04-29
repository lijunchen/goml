use std::collections::HashMap;
use std::path::{Path, PathBuf};

use ast::ast;
use cst::cst::{CstNode, File as CstFile};
use diagnostics::{Diagnostic, Diagnostics, Severity, Stage};
use parser::{self, syntax::MySyntaxNode};
use rowan::GreenNode;

use crate::config::{find_crate_root, load_crate_manifest};
use crate::package_names::{BUILTIN_PACKAGE, ENTRY_FUNCTION, ROOT_PACKAGE};
use crate::pipeline::builtin_inherent;
use crate::pipeline::compile_error;
use crate::pipeline::packages;
use crate::{
    anf::{self, GlobalAnfEnv},
    artifact::CrateExports,
    builtins, compile_match, derive,
    env::{Gensym, GlobalTypeEnv},
    external::ExternalDependencyArtifacts,
    go::{self, compile::GlobalGoEnv, goast},
    hir, interface,
    lift::{self, GlobalLiftEnv, LiftFile},
    mono::{self, GlobalMonoEnv},
    tast, typer,
};

#[derive(Debug)]
pub struct Compilation {
    pub green_node: GreenNode,
    pub cst: CstFile,
    pub ast: ast::File,
    pub hir: hir::ProjectHir,
    pub hir_table: hir::ProjectHirTable,
    pub tast: tast::File,
    pub genv: GlobalTypeEnv,
    pub liftenv: GlobalLiftEnv,
    pub monoenv: GlobalMonoEnv,
    pub anfenv: GlobalAnfEnv,
    pub goenv: GlobalGoEnv,
    pub core: crate::core::File,
    pub lambda: LiftFile,
    pub mono: mono::MonoFile,
    pub anf: anf::File,
    pub go: goast::File,
}

#[derive(Debug, Clone)]
pub enum CompilationError {
    Parser { diagnostics: Diagnostics },
    Lower { diagnostics: Diagnostics },
    Typer { diagnostics: Diagnostics },
    Compile { diagnostics: Diagnostics },
}

impl CompilationError {
    pub fn diagnostics(&self) -> &Diagnostics {
        match self {
            CompilationError::Parser { diagnostics }
            | CompilationError::Lower { diagnostics }
            | CompilationError::Typer { diagnostics }
            | CompilationError::Compile { diagnostics } => diagnostics,
        }
    }

    pub fn into_diagnostics(self) -> Diagnostics {
        match self {
            CompilationError::Parser { diagnostics }
            | CompilationError::Lower { diagnostics }
            | CompilationError::Typer { diagnostics }
            | CompilationError::Compile { diagnostics } => diagnostics,
        }
    }
}

#[derive(Debug, Clone)]
struct NamespaceInterface {
    exports: CrateExports,
    crate_interface: interface::CrateInterface,
}

fn nominal_impl_type_name(ty: &tast::Ty) -> Option<&str> {
    match ty {
        tast::Ty::TStruct { name } | tast::Ty::TEnum { name } => Some(name),
        tast::Ty::TApp { ty, .. } => nominal_impl_type_name(ty),
        _ => None,
    }
}

fn exports_define_nominal_type(exports: &CrateExports, ty: &tast::Ty) -> bool {
    let Some(name) = nominal_impl_type_name(ty) else {
        return false;
    };
    let ident = tast::TastIdent::new(name);
    exports.type_env.structs.contains_key(&ident) || exports.type_env.enums.contains_key(&ident)
}

fn genv_defines_nominal_type(genv: &GlobalTypeEnv, ty: &tast::Ty) -> bool {
    let Some(name) = nominal_impl_type_name(ty) else {
        return false;
    };
    let ident = tast::TastIdent::new(name);
    genv.type_env.structs.contains_key(&ident) || genv.type_env.enums.contains_key(&ident)
}

fn duplicate_trait_impl_shadows_builtin(
    genv: &GlobalTypeEnv,
    exports: &CrateExports,
    key: &(String, tast::Ty),
) -> bool {
    builtins::builtin_env()
        .trait_env
        .trait_impls
        .contains_key(key)
        && exports_define_nominal_type(exports, &key.1)
        && !genv_defines_nominal_type(genv, &key.1)
}

pub(super) fn report_duplicate_trait_impls(
    diagnostics: &mut Diagnostics,
    genv: &GlobalTypeEnv,
    exports: &CrateExports,
    namespace_name: &str,
) {
    for (key, _) in exports.trait_env.trait_impls.iter() {
        if genv.trait_env.trait_impls.contains_key(key)
            && !duplicate_trait_impl_shadows_builtin(genv, exports, key)
        {
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Trait {} implementation for {:?} is defined in multiple namespaces (including {})",
                    key.0, key.1, namespace_name
                ),
            ));
        }
    }
}

fn root_namespace_name(namespace_names: &[String]) -> Option<String> {
    if namespace_names.iter().any(|name| name == ROOT_PACKAGE) {
        Some(ROOT_PACKAGE.to_string())
    } else {
        None
    }
}

fn namespace_id_map(namespace_names: &[String]) -> HashMap<String, hir::PackageId> {
    let mut ids = HashMap::new();
    ids.insert(BUILTIN_PACKAGE.to_string(), hir::PackageId(0));

    let root_namespace = root_namespace_name(namespace_names);
    if let Some(root_namespace) = &root_namespace {
        ids.insert(root_namespace.clone(), hir::PackageId(1));
    }

    let mut sorted = namespace_names.to_vec();
    sorted.sort();
    let mut next_id = 2u32;
    for name in sorted {
        if name == BUILTIN_PACKAGE || Some(name.as_str()) == root_namespace.as_deref() {
            continue;
        }
        ids.insert(name, hir::PackageId(next_id));
        next_id += 1;
    }

    ids
}

fn link_core_files(core_files: Vec<crate::core::File>) -> crate::core::File {
    let mut toplevels = Vec::new();
    for core_file in core_files {
        toplevels.extend(core_file.toplevels);
    }
    crate::core::File { toplevels }
}

#[derive(Debug, Clone)]
struct NamespaceArtifact {
    tast: tast::File,
    full_exports: CrateExports,
    interface: NamespaceInterface,
    diagnostics: Diagnostics,
}

#[derive(Debug)]
struct TypecheckNamespacesResult {
    entry_tast: tast::File,
    full_tast: tast::File,
    genv: GlobalTypeEnv,
    diagnostics: Diagnostics,
    graph: packages::PackageGraph,
    artifacts: HashMap<String, NamespaceArtifact>,
    external_deps: ExternalDependencyArtifacts,
}

fn parse_ast_from_source(
    path: &Path,
    src: &str,
) -> Result<(GreenNode, CstFile, ast::File), CompilationError> {
    let parse_result = parser::parse(path, src);
    if parse_result.has_errors() {
        return Err(CompilationError::Parser {
            diagnostics: parse_result.into_diagnostics(),
        });
    }

    let green_node = parse_result.green_node.clone();
    let root = MySyntaxNode::new_root(parse_result.green_node);
    let cst = CstFile::cast(root).expect("failed to cast CST file");
    let lower = ::ast::lower::lower(cst.clone());
    let ast = match lower.into_result() {
        Ok(ast) => ast,
        Err(diagnostics) => {
            return Err(CompilationError::Lower { diagnostics });
        }
    };

    let ast = match derive::expand(ast) {
        Ok(ast) => ast,
        Err(diagnostics) => {
            return Err(CompilationError::Lower { diagnostics });
        }
    };

    Ok((green_node, cst, ast))
}

fn parse_ast_from_source_allow_parse_errors(
    path: &Path,
    src: &str,
) -> Result<(GreenNode, CstFile, ast::File, Diagnostics), CompilationError> {
    let parse_result = parser::parse(path, src);
    let (green_node, mut diagnostics) = parse_result.into_parts();

    let root = MySyntaxNode::new_root(green_node.clone());
    let cst = CstFile::cast(root).expect("failed to cast CST file");
    let lower = ::ast::lower::lower(cst.clone());
    let (ast, mut lower_diagnostics) = lower.into_parts();
    diagnostics.append(&mut lower_diagnostics);
    let Some(ast) = ast else {
        return Err(CompilationError::Lower { diagnostics });
    };

    let original_ast = ast.clone();
    let ast = match derive::expand(ast) {
        Ok(ast) => ast,
        Err(mut derive_diagnostics) => {
            diagnostics.append(&mut derive_diagnostics);
            original_ast
        }
    };

    Ok((green_node, cst, ast, diagnostics))
}

pub fn parse_ast_file(path: &Path, src: &str) -> Result<ast::File, CompilationError> {
    let (_green, _cst, ast) = parse_ast_from_source(path, src)?;
    Ok(ast)
}

fn typecheck_namespace_unit(
    namespace_id: hir::PackageId,
    namespace: &packages::PackageUnit,
    deps_envs: HashMap<String, GlobalTypeEnv>,
    deps_interfaces: &HashMap<String, interface::CrateInterface>,
) -> NamespaceArtifact {
    let (hir, hir_table, mut hir_diagnostics) =
        hir::lower_to_hir_files_with_env(namespace_id, namespace.files.clone(), deps_interfaces);
    let (tast, genv, mut diagnostics) = typer::check_file_with_env(
        hir,
        hir_table,
        GlobalTypeEnv::new(),
        builtins::builtin_env(),
        &namespace.name,
        deps_envs,
    );
    diagnostics.append(&mut hir_diagnostics);
    let full_exports = CrateExports::from_genv(&genv);
    let exports = CrateExports::public_from_namespace(&namespace.name, &namespace.files, &genv);
    let namespace_interface = interface::CrateInterface::from_exports(&namespace.name, &exports);

    NamespaceArtifact {
        tast,
        full_exports,
        interface: NamespaceInterface {
            exports,
            crate_interface: namespace_interface,
        },
        diagnostics,
    }
}

fn typecheck_namespaces_inner(
    path: &Path,
    entry_ast: ast::File,
    single_file: bool,
) -> Result<TypecheckNamespacesResult, CompilationError> {
    let root = discovery_root_for_file(path);
    let external_deps = load_external_dependencies(&root)?;
    let external_imports = external_deps.external_imports();
    let mut graph = if single_file {
        packages::discover_packages_single_file_with_external_imports(
            &root,
            path,
            entry_ast,
            &external_imports,
        )?
    } else {
        packages::discover_packages_with_external_imports(
            &root,
            Some(path),
            Some(entry_ast),
            &external_imports,
        )?
    };
    external_deps
        .augment_graph(&mut graph)
        .map_err(compile_error)?;
    let order = packages::topo_sort_namespaces(&graph)?;

    let mut diagnostics = Diagnostics::new();
    let mut genv = builtins::builtin_env();
    let external_interfaces = external_deps.namespace_interfaces();
    let external_envs = external_deps.namespace_envs();
    for (name, module) in external_deps.modules.iter() {
        report_duplicate_trait_impls(&mut diagnostics, &genv, &module.interface.exports, name);
        module.interface.exports.apply_to(&mut genv);
    }
    let mut artifacts_by_name: HashMap<String, NamespaceArtifact> = HashMap::new();
    let mut namespace_names: Vec<String> = graph.packages.keys().cloned().collect();
    namespace_names.sort();
    let namespace_ids = namespace_id_map(&namespace_names);

    for name in order.iter() {
        let package = graph
            .packages
            .get(name)
            .ok_or_else(|| compile_error(format!("namespace {} not found", name)))?;
        let namespace_id = *namespace_ids
            .get(name)
            .unwrap_or_else(|| panic!("missing namespace id for {}", name));
        let mut deps_envs = HashMap::new();
        let mut deps: Vec<_> = package.imports.iter().cloned().collect();
        deps.sort();
        let mut deps_interfaces = HashMap::new();
        for dep in deps.iter() {
            if let Some(artifact) = artifacts_by_name.get(dep) {
                deps_envs.insert(dep.clone(), artifact.interface.exports.to_genv());
                deps_interfaces.insert(dep.clone(), artifact.interface.crate_interface.clone());
                continue;
            }
            if let Some(interface) = external_interfaces.get(dep) {
                deps_envs.insert(
                    dep.clone(),
                    external_envs.get(dep).cloned().ok_or_else(|| {
                        compile_error(format!("missing namespace env for {}", dep))
                    })?,
                );
                deps_interfaces.insert(dep.clone(), interface.clone());
                continue;
            }
            return Err(compile_error(format!(
                "missing namespace artifact for {}",
                dep
            )));
        }

        let artifact = typecheck_namespace_unit(namespace_id, package, deps_envs, &deps_interfaces);

        let mut package_diagnostics = artifact.diagnostics.clone();
        diagnostics.append(&mut package_diagnostics);
        report_duplicate_trait_impls(&mut diagnostics, &genv, &artifact.interface.exports, name);
        artifact.interface.exports.apply_to(&mut genv);
        artifacts_by_name.insert(name.clone(), artifact);
    }

    let entry_tast = artifacts_by_name
        .get(&graph.entry_package)
        .ok_or_else(|| compile_error("entry namespace not found".to_string()))?
        .tast
        .clone();

    let mut toplevels = Vec::new();
    let mut has_print = false;
    let mut has_println = false;
    for name in graph.discovery_order.iter() {
        let artifact = artifacts_by_name
            .get(name)
            .ok_or_else(|| compile_error(format!("missing namespace artifact for {}", name)))?;
        for item in artifact.tast.toplevels.iter() {
            if let tast::Item::Fn(f) = item {
                if f.name == "print" {
                    has_print = true;
                }
                if f.name == "println" {
                    has_println = true;
                }
            }
        }
        toplevels.extend(artifact.tast.toplevels.clone());
    }

    if !has_print || !has_println {
        let mut extra = Vec::new();
        for item in builtins::builtin_tast().toplevels.iter() {
            let tast::Item::Fn(f) = item else {
                continue;
            };
            if (!has_print && f.name == "print") || (!has_println && f.name == "println") {
                extra.push(item.clone());
            }
        }
        if !extra.is_empty() {
            extra.extend(toplevels);
            toplevels = extra;
        }
    }

    Ok(TypecheckNamespacesResult {
        entry_tast,
        full_tast: tast::File { toplevels },
        genv,
        diagnostics,
        graph,
        artifacts: artifacts_by_name,
        external_deps,
    })
}

fn discovery_root_for_file(path: &Path) -> PathBuf {
    let start_dir = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    if let Some((crate_dir, _)) = find_crate_root(start_dir) {
        return crate_dir;
    }
    start_dir.to_path_buf()
}

fn should_use_single_file_mode(path: &Path) -> bool {
    if path.exists() {
        return false;
    }
    let start_dir = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    find_crate_root(start_dir).is_none()
}

pub fn compile(path: &Path, src: &str) -> Result<Compilation, CompilationError> {
    let single_file = should_use_single_file_mode(path);
    super::with_src_compiler_stack(src, || compile_inner(path, src, single_file, true))
}

pub fn compile_for_analysis(path: &Path, src: &str) -> Result<Compilation, CompilationError> {
    let single_file = should_use_single_file_mode(path);
    super::with_src_compiler_stack(src, || compile_inner(path, src, single_file, false))
}

fn compile_inner(
    path: &Path,
    src: &str,
    single_file: bool,
    validate_entrypoint: bool,
) -> Result<Compilation, CompilationError> {
    let (green_node, cst, entry_ast) = parse_ast_from_source(path, src)?;

    let typecheck = typecheck_namespaces_inner(path, entry_ast.clone(), single_file)?;
    let TypecheckNamespacesResult {
        full_tast,
        genv,
        mut diagnostics,
        graph,
        artifacts,
        external_deps,
        ..
    } = typecheck;
    let mut all_files = Vec::new();
    for name in graph.discovery_order.iter() {
        let package = graph
            .packages
            .get(name)
            .ok_or_else(|| compile_error(format!("namespace {} not found", name)))?;
        all_files.extend(package.files.clone());
    }

    let (hir, hir_table, _hir_diagnostics) = hir::lower_to_project_hir_files(all_files);

    let tast = full_tast;
    if diagnostics.has_errors() {
        return Err(CompilationError::Typer {
            diagnostics: diagnostics.clone(),
        });
    }
    if validate_entrypoint {
        validate_entrypoint_for_compile(&mut diagnostics, &graph, &artifacts);
        if diagnostics.has_errors() {
            return Err(CompilationError::Typer {
                diagnostics: diagnostics.clone(),
            });
        }
    }
    let gensym = Gensym::new();

    let mut core_files = Vec::new();
    let builtin_print_core = compile_match::compile_file(
        &builtins::builtin_env(),
        &gensym,
        &mut diagnostics,
        &builtins::builtin_print_tast(),
    );
    core_files.push(builtin_print_core);
    for module in external_deps.modules.values() {
        core_files.push(module.core.core_ir.clone());
    }
    for name in graph.discovery_order.iter() {
        let package = graph
            .packages
            .get(name)
            .ok_or_else(|| compile_error(format!("namespace {} not found", name)))?;
        let artifact = artifacts
            .get(name)
            .ok_or_else(|| compile_error(format!("missing namespace artifact for {}", name)))?;
        let mut env = builtins::builtin_env();
        let mut deps: Vec<_> = package.imports.iter().cloned().collect();
        deps.sort();
        for dep in deps.iter() {
            if let Some(dep_artifact) = artifacts.get(dep) {
                dep_artifact.interface.exports.apply_to(&mut env);
                continue;
            }
            if let Some(exports) = external_namespace_exports(&external_deps, dep) {
                exports.apply_to(&mut env);
                continue;
            }
            return Err(compile_error(format!(
                "missing namespace artifact for {}",
                dep
            )));
        }
        artifact.full_exports.apply_to(&mut env);
        let core = compile_match::compile_file(&env, &gensym, &mut diagnostics, &artifact.tast);
        core_files.push(core);
    }
    let required_builtin_methods =
        builtin_inherent::collect_required_builtin_collection_methods(&core_files);
    let builtin_collection_core = builtin_inherent::compile_builtin_collection_methods(
        &required_builtin_methods,
        &gensym,
        &mut diagnostics,
    );
    if !builtin_collection_core.toplevels.is_empty() {
        core_files.push(builtin_collection_core);
    }
    let core = link_core_files(core_files);
    if diagnostics.has_errors() {
        return Err(CompilationError::Compile { diagnostics });
    }
    let (mono, monoenv) = mono::mono(genv.clone(), core.clone()).map_err(compile_error)?;
    let (lifted_core, liftenv) = lift::lambda_lift(monoenv.clone(), &gensym, mono.clone());
    let (anf, anfenv) = anf::anf_file(liftenv.clone(), &gensym, lifted_core.clone());
    let (go, goenv) = go::compile::go_file(anfenv.clone(), &gensym, anf.clone());

    Ok(Compilation {
        green_node,
        cst,
        ast: entry_ast,
        hir,
        hir_table,
        tast,
        genv,
        liftenv,
        monoenv,
        anfenv,
        goenv,
        core,
        lambda: lifted_core,
        mono,
        anf,
        go,
    })
}

pub fn compile_single_file(path: &Path, src: &str) -> Result<Compilation, CompilationError> {
    super::with_src_compiler_stack(src, || compile_inner(path, src, true, true))
}

pub fn typecheck_with_namespaces(
    path: &Path,
    src: &str,
) -> Result<(tast::File, GlobalTypeEnv, Diagnostics), CompilationError> {
    let single_file = should_use_single_file_mode(path);
    super::with_src_compiler_stack(src, || {
        let (_green_node, _cst, entry_ast) = parse_ast_from_source(path, src)?;
        let result = typecheck_namespaces_inner(path, entry_ast, single_file)?;
        Ok((result.entry_tast, result.genv, result.diagnostics))
    })
}

pub fn typecheck_with_namespaces_and_results(
    path: &Path,
    src: &str,
) -> Result<
    (
        hir::HirTable,
        typer::results::TypeckResults,
        GlobalTypeEnv,
        Diagnostics,
    ),
    CompilationError,
> {
    let single_file = should_use_single_file_mode(path);
    super::with_src_compiler_stack(src, || {
        let (_green_node, _cst, entry_ast, mut diagnostics) =
            parse_ast_from_source_allow_parse_errors(path, src)?;
        let root = discovery_root_for_file(path);
        let external_deps = load_external_dependencies(&root)?;
        let external_imports = external_deps.external_imports();
        let mut graph = if single_file {
            packages::discover_packages_single_file_with_external_imports(
                &root,
                path,
                entry_ast,
                &external_imports,
            )?
        } else {
            packages::discover_packages_with_external_imports(
                &root,
                Some(path),
                Some(entry_ast),
                &external_imports,
            )?
        };
        external_deps
            .augment_graph(&mut graph)
            .map_err(compile_error)?;
        let order = packages::topo_sort_namespaces(&graph)?;

        let mut genv = builtins::builtin_env();
        let external_interfaces = external_deps.namespace_interfaces();
        let external_envs = external_deps.namespace_envs();
        for (name, module) in external_deps.modules.iter() {
            report_duplicate_trait_impls(&mut diagnostics, &genv, &module.interface.exports, name);
            module.interface.exports.apply_to(&mut genv);
        }
        let mut artifacts_by_name: HashMap<String, NamespaceInterface> = HashMap::new();
        let mut namespace_names: Vec<String> = graph.packages.keys().cloned().collect();
        namespace_names.sort();
        let namespace_ids = namespace_id_map(&namespace_names);

        let mut entry_hir_table = None;
        let mut entry_results = None;

        for name in order.iter() {
            let package = graph
                .packages
                .get(name)
                .ok_or_else(|| compile_error(format!("namespace {} not found", name)))?;
            let namespace_id = *namespace_ids
                .get(name)
                .unwrap_or_else(|| panic!("missing namespace id for {}", name));
            let mut deps_envs = HashMap::new();
            let mut deps: Vec<_> = package.imports.iter().cloned().collect();
            deps.sort();
            let mut deps_interfaces = HashMap::new();
            for dep in deps.iter() {
                if let Some(interface) = artifacts_by_name.get(dep) {
                    deps_envs.insert(dep.clone(), interface.exports.to_genv());
                    deps_interfaces.insert(dep.clone(), interface.crate_interface.clone());
                    continue;
                }
                if let Some(interface) = external_interfaces.get(dep) {
                    deps_envs.insert(
                        dep.clone(),
                        external_envs.get(dep).cloned().ok_or_else(|| {
                            compile_error(format!("missing namespace env for {}", dep))
                        })?,
                    );
                    deps_interfaces.insert(dep.clone(), interface.clone());
                    continue;
                }
                return Err(compile_error(format!(
                    "missing namespace artifact for {}",
                    dep
                )));
            }

            let (hir, hir_table, mut hir_diagnostics) = hir::lower_to_hir_files_with_env(
                namespace_id,
                package.files.clone(),
                &deps_interfaces,
            );
            let (hir_table, results, package_genv, mut package_diagnostics) =
                typer::check_file_with_env_and_results(
                    hir,
                    hir_table,
                    GlobalTypeEnv::new(),
                    builtins::builtin_env(),
                    &package.name,
                    deps_envs,
                );
            package_diagnostics.append(&mut hir_diagnostics);
            diagnostics.append(&mut package_diagnostics);

            let exports = CrateExports::public_from_namespace(name, &package.files, &package_genv);
            report_duplicate_trait_impls(&mut diagnostics, &genv, &exports, name);
            exports.apply_to(&mut genv);
            let crate_interface = interface::CrateInterface::from_exports(name, &exports);

            let interface = NamespaceInterface {
                exports,
                crate_interface,
            };

            if name == &graph.entry_package {
                entry_hir_table = Some(hir_table);
                entry_results = Some(results);
            }

            artifacts_by_name.insert(name.clone(), interface);
        }

        let Some(entry_hir_table) = entry_hir_table else {
            return Err(compile_error("entry namespace not found".to_string()));
        };
        let Some(entry_results) = entry_results else {
            return Err(compile_error("entry namespace not found".to_string()));
        };

        Ok((entry_hir_table, entry_results, genv, diagnostics))
    })
}

fn validate_entrypoint_for_compile(
    diagnostics: &mut Diagnostics,
    graph: &packages::PackageGraph,
    artifacts: &HashMap<String, NamespaceArtifact>,
) {
    let Some(entry_artifact) = artifacts.get(&graph.entry_package) else {
        return;
    };
    if graph.entry_package != ROOT_PACKAGE {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!("entry package must be main, got {}", graph.entry_package),
        ));
        return;
    }
    if !entry_artifact
        .interface
        .exports
        .value_env
        .funcs
        .contains_key(ENTRY_FUNCTION)
    {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            "main function is required".to_string(),
        ));
    }
}

fn external_namespace_exports<'a>(
    external_deps: &'a ExternalDependencyArtifacts,
    namespace: &str,
) -> Option<&'a CrateExports> {
    for module in external_deps.modules.values() {
        if module.namespace_interfaces.contains_key(namespace) {
            return Some(&module.interface.exports);
        }
    }
    None
}

fn load_external_dependencies(
    root: &Path,
) -> Result<ExternalDependencyArtifacts, CompilationError> {
    let manifest = root.join("goml.toml");
    if let Ok(crate_manifest) = load_crate_manifest(&manifest)
        && crate_manifest.crate_config.is_some()
    {
        return crate::external::resolve_dependency_versions(&crate_manifest.dependency_versions())
            .map_err(compile_error);
    }

    Ok(ExternalDependencyArtifacts::default())
}
