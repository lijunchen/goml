use std::collections::{BTreeSet, HashMap};
use std::path::{Path, PathBuf};

use ast::ast;
use cst::cst::{CstNode, File as CstFile};
use diagnostics::{Diagnostic, Diagnostics, Severity, Stage};
use parser::{self, syntax::MySyntaxNode};
use rowan::GreenNode;

use crate::config::{find_module_root, load_module_manifest};
use crate::package_names::{BUILTIN_PACKAGE, ENTRY_FUNCTION, ROOT_PACKAGE, STD_PACKAGE};
use crate::pipeline::compile_error;
use crate::pipeline::packages;
use crate::{
    anf::{self, GlobalAnfEnv},
    artifact::PackageExports,
    builtins, compile_match, derive,
    env::{Gensym, GlobalTypeEnv},
    external::ExternalDependencyArtifacts,
    go::{self, compile::GlobalGoEnv, goast},
    hir, interface,
    lift::{self, GlobalLiftEnv, LiftFile},
    mono::{self, GlobalMonoEnv},
    stdlib, tast, typer,
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
struct PackageInterface {
    exports: PackageExports,
    package_interface: interface::PackageInterface,
}

fn nominal_impl_type_name(ty: &tast::Ty) -> Option<&str> {
    match ty {
        tast::Ty::TStruct { name } | tast::Ty::TEnum { name } => Some(name),
        tast::Ty::TApp { ty, .. } => nominal_impl_type_name(ty),
        _ => None,
    }
}

fn exports_define_nominal_type(exports: &PackageExports, ty: &tast::Ty) -> bool {
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
    exports: &PackageExports,
    key: &crate::env::TraitImplKey,
) -> bool {
    builtins::builtin_env()
        .trait_env
        .trait_impls
        .contains_key(key)
        && exports_define_nominal_type(exports, &key.for_ty)
        && !genv_defines_nominal_type(genv, &key.for_ty)
}

pub(super) fn report_duplicate_trait_impls(
    diagnostics: &mut Diagnostics,
    genv: &GlobalTypeEnv,
    exports: &PackageExports,
    package_name: &str,
) {
    for (key, _) in exports.trait_env.trait_impls.iter() {
        if genv.trait_env.trait_impls.contains_key(key)
            && !duplicate_trait_impl_shadows_builtin(genv, exports, key)
        {
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Trait {} implementation for {:?} is defined in multiple packages (including {})",
                    key.trait_ref.name.0, key.for_ty, package_name
                ),
            ));
        }
    }
}

fn root_package_name(package_names: &[String]) -> Option<String> {
    if package_names.iter().any(|name| name == ROOT_PACKAGE) {
        Some(ROOT_PACKAGE.to_string())
    } else {
        None
    }
}

fn package_id_map(package_names: &[String]) -> HashMap<String, hir::PackageId> {
    let mut ids = HashMap::new();
    ids.insert(BUILTIN_PACKAGE.to_string(), hir::PackageId(0));

    let root_package = root_package_name(package_names);
    if let Some(root_package) = &root_package {
        ids.insert(root_package.clone(), hir::PackageId(1));
    }

    let mut sorted = package_names.to_vec();
    sorted.sort();
    let mut next_id = 2u32;
    for name in sorted {
        if name == BUILTIN_PACKAGE || Some(name.as_str()) == root_package.as_deref() {
            continue;
        }
        ids.insert(name, hir::PackageId(next_id));
        next_id += 1;
    }

    ids
}

fn add_stdlib_if_imported(
    external_deps: &mut ExternalDependencyArtifacts,
    graph: &packages::PackageGraph,
) -> Result<(), CompilationError> {
    let imports_std = graph.packages.values().any(|unit| {
        unit.imports
            .iter()
            .any(|package| package == STD_PACKAGE || package.starts_with("std::"))
    }) || external_deps.modules.values().any(|module| {
        module.packages.values().any(|package| {
            package
                .core
                .deps
                .keys()
                .any(|dependency| dependency == STD_PACKAGE || dependency.starts_with("std::"))
        })
    });
    if !imports_std || external_deps.modules.contains_key(STD_PACKAGE) {
        return Ok(());
    }
    let artifact = stdlib::stdlib_artifact().map_err(compile_error)?;
    external_deps
        .modules
        .insert(STD_PACKAGE.to_string(), artifact);
    Ok(())
}

fn package_dependency_closure(
    package: &packages::PackageUnit,
    graph: &packages::PackageGraph,
    external_deps: &ExternalDependencyArtifacts,
) -> Result<Vec<String>, CompilationError> {
    let mut pending = package.imports.iter().cloned().collect::<BTreeSet<_>>();
    let mut dependencies = BTreeSet::new();

    while let Some(dependency) = pending.pop_first() {
        if dependency == BUILTIN_PACKAGE || dependency == package.name {
            continue;
        }
        if !dependencies.insert(dependency.clone()) {
            continue;
        }
        if let Some(local) = graph.packages.get(&dependency) {
            pending.extend(local.imports.iter().cloned());
            continue;
        }
        if let Some(external) = external_deps.package(&dependency) {
            pending.extend(external.core.deps.keys().cloned());
            continue;
        }
        return Err(compile_error(format!(
            "missing package artifact for {}",
            dependency
        )));
    }

    Ok(dependencies.into_iter().collect())
}

fn link_packages(packages: Vec<crate::core::File>) -> crate::core::File {
    let mut toplevels = Vec::new();
    for package in packages {
        toplevels.extend(package.toplevels);
    }
    crate::core::File { toplevels }
}

#[derive(Debug, Clone)]
struct PackageArtifact {
    tast: tast::File,
    full_exports: PackageExports,
    interface: PackageInterface,
    diagnostics: Diagnostics,
}

#[derive(Debug)]
struct TypecheckPackagesResult {
    entry_tast: tast::File,
    full_tast: tast::File,
    genv: GlobalTypeEnv,
    diagnostics: Diagnostics,
    graph: packages::PackageGraph,
    artifacts: HashMap<String, PackageArtifact>,
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

fn typecheck_package(
    package_id: hir::PackageId,
    package: &packages::PackageUnit,
    deps_envs: HashMap<String, GlobalTypeEnv>,
    deps_interfaces: &HashMap<String, interface::PackageInterface>,
) -> PackageArtifact {
    let (hir, hir_table, mut hir_diagnostics) =
        hir::lower_to_hir_files_with_env(package_id, package.files.clone(), deps_interfaces);
    let (tast, genv, mut diagnostics) = typer::check_file_with_env(
        hir,
        hir_table,
        GlobalTypeEnv::new(),
        builtins::builtin_env(),
        &package.name,
        deps_envs,
    );
    diagnostics.append(&mut hir_diagnostics);
    let full_exports = PackageExports::from_genv(&genv);
    let exports =
        PackageExports::public_from_package(&package.name, &package.files, &genv, &mut diagnostics);
    let package_interface =
        interface::PackageInterface::from_package(&package.name, &package.declared_name, &exports);

    PackageArtifact {
        tast,
        full_exports,
        interface: PackageInterface {
            exports,
            package_interface,
        },
        diagnostics,
    }
}

fn typecheck_packages_inner(
    path: &Path,
    entry_ast: ast::File,
    single_file: bool,
) -> Result<TypecheckPackagesResult, CompilationError> {
    let root = discovery_root_for_file(path)?;
    let mut external_deps = load_external_dependencies(&root)?;
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
    add_stdlib_if_imported(&mut external_deps, &graph)?;
    external_deps
        .augment_graph(&mut graph)
        .map_err(compile_error)?;
    let order = packages::topo_sort_packages(&graph)?;
    let reachable_external = external_deps
        .reachable_package_names(&graph)
        .map_err(compile_error)?;

    let mut diagnostics = Diagnostics::new();
    let mut genv = builtins::builtin_env();
    let external_interfaces = external_deps.package_interfaces();
    let external_envs = external_deps.package_envs();
    for module in external_deps.modules.values() {
        for (name, package) in module.packages.iter() {
            if !reachable_external.contains(name) {
                continue;
            }
            report_duplicate_trait_impls(&mut diagnostics, &genv, &package.interface.exports, name);
            package.interface.exports.apply_to(&mut genv);
        }
    }
    let mut artifacts_by_name: HashMap<String, PackageArtifact> = HashMap::new();
    let mut package_names: Vec<String> = graph.packages.keys().cloned().collect();
    package_names.sort();
    let package_ids = package_id_map(&package_names);

    for name in order.iter() {
        let package = graph
            .packages
            .get(name)
            .ok_or_else(|| compile_error(format!("package {} not found", name)))?;
        let package_id = *package_ids
            .get(name)
            .unwrap_or_else(|| panic!("missing package id for {}", name));
        let mut deps_envs = HashMap::new();
        let mut deps_interfaces = HashMap::new();
        let dependencies = package_dependency_closure(package, &graph, &external_deps)?;
        for dep in dependencies.iter() {
            if let Some(artifact) = artifacts_by_name.get(dep) {
                deps_envs.insert(dep.clone(), artifact.interface.exports.to_genv());
                deps_interfaces.insert(dep.clone(), artifact.interface.package_interface.clone());
                continue;
            }
            if let Some(interface) = external_interfaces.get(dep) {
                deps_envs.insert(
                    dep.clone(),
                    external_envs
                        .get(dep)
                        .cloned()
                        .ok_or_else(|| compile_error(format!("missing package env for {}", dep)))?,
                );
                deps_interfaces.insert(dep.clone(), interface.clone());
                continue;
            }
            return Err(compile_error(format!(
                "missing package artifact for {}",
                dep
            )));
        }

        let artifact = typecheck_package(package_id, package, deps_envs, &deps_interfaces);

        let mut package_diagnostics = artifact.diagnostics.clone();
        diagnostics.append(&mut package_diagnostics);
        report_duplicate_trait_impls(&mut diagnostics, &genv, &artifact.interface.exports, name);
        artifact.interface.exports.apply_to(&mut genv);
        artifacts_by_name.insert(name.clone(), artifact);
    }

    let entry_tast = artifacts_by_name
        .get(&graph.entry_package)
        .ok_or_else(|| compile_error("entry package not found".to_string()))?
        .tast
        .clone();

    let mut toplevels = Vec::new();
    for name in graph.discovery_order.iter() {
        let artifact = artifacts_by_name
            .get(name)
            .ok_or_else(|| compile_error(format!("missing package artifact for {}", name)))?;
        toplevels.extend(artifact.tast.toplevels.clone());
    }

    Ok(TypecheckPackagesResult {
        entry_tast,
        full_tast: tast::File { toplevels },
        genv,
        diagnostics,
        graph,
        artifacts: artifacts_by_name,
        external_deps,
    })
}

fn discovery_root_for_file(path: &Path) -> Result<PathBuf, CompilationError> {
    let start_dir = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    if let Some((module_dir, _)) = find_module_root(start_dir).map_err(compile_error)? {
        return Ok(module_dir);
    }
    Ok(start_dir.to_path_buf())
}

fn should_use_single_file_mode(path: &Path) -> Result<bool, CompilationError> {
    let start_dir = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    Ok(find_module_root(start_dir)
        .map_err(compile_error)?
        .is_none())
}

pub fn compile(path: &Path, src: &str) -> Result<Compilation, CompilationError> {
    let single_file = should_use_single_file_mode(path)?;
    super::with_src_compiler_stack(src, || compile_inner(path, src, single_file, true))
}

pub fn compile_for_analysis(path: &Path, src: &str) -> Result<Compilation, CompilationError> {
    let single_file = should_use_single_file_mode(path)?;
    super::with_src_compiler_stack(src, || compile_inner(path, src, single_file, false))
}

fn compile_inner(
    path: &Path,
    src: &str,
    single_file: bool,
    validate_entrypoint: bool,
) -> Result<Compilation, CompilationError> {
    let (green_node, cst, entry_ast) = parse_ast_from_source(path, src)?;

    let typecheck = typecheck_packages_inner(path, entry_ast.clone(), single_file)?;
    let TypecheckPackagesResult {
        full_tast,
        mut diagnostics,
        graph,
        artifacts,
        external_deps,
        ..
    } = typecheck;
    let reachable_external = external_deps
        .reachable_package_names(&graph)
        .map_err(compile_error)?;
    let mut all_files = Vec::new();
    for name in graph.discovery_order.iter() {
        let package = graph
            .packages
            .get(name)
            .ok_or_else(|| compile_error(format!("package {} not found", name)))?;
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

    let mut package_cores = Vec::new();
    let mut builtin_core = compile_match::compile_file(
        &builtins::builtin_env(),
        &gensym,
        &mut diagnostics,
        &builtins::builtin_tast(),
    );
    for function in builtin_core.toplevels.iter_mut() {
        function.root = false;
    }
    package_cores.push(builtin_core);
    for module in external_deps.modules.values() {
        for (name, package) in module.packages.iter() {
            if !reachable_external.contains(name) {
                continue;
            }
            package_cores.push(package.core.core_ir.clone());
        }
    }
    for name in graph.discovery_order.iter() {
        let package = graph
            .packages
            .get(name)
            .ok_or_else(|| compile_error(format!("package {} not found", name)))?;
        let artifact = artifacts
            .get(name)
            .ok_or_else(|| compile_error(format!("missing package artifact for {}", name)))?;
        let mut env = builtins::builtin_env();
        let dependencies = package_dependency_closure(package, &graph, &external_deps)?;
        for dep in dependencies.iter() {
            if let Some(dep_artifact) = artifacts.get(dep) {
                dep_artifact.full_exports.apply_to(&mut env);
                continue;
            }
            if let Some(package) = external_deps.package(dep) {
                package.core.exports.apply_to(&mut env);
                continue;
            }
            return Err(compile_error(format!(
                "missing package artifact for {}",
                dep
            )));
        }
        artifact.full_exports.apply_to(&mut env);
        let core = compile_match::compile_file(&env, &gensym, &mut diagnostics, &artifact.tast);
        package_cores.push(core);
    }
    let mut core = link_packages(package_cores);
    if validate_entrypoint && graph.entry_package != ROOT_PACKAGE {
        let entry_name = format!("{}::{}", graph.entry_package, ENTRY_FUNCTION);
        let entry_fn = core
            .toplevels
            .iter()
            .find(|function| function.name == entry_name)
            .cloned()
            .ok_or_else(|| compile_error("main function is required".to_string()))?;
        core.toplevels
            .push(super::separate::package_entry_wrapper(&entry_fn));
    }
    if diagnostics.has_errors() {
        return Err(CompilationError::Compile { diagnostics });
    }
    let mut codegen_genv = builtins::builtin_env();
    for module in external_deps.modules.values() {
        for (name, package) in module.packages.iter() {
            if !reachable_external.contains(name) {
                continue;
            }
            package.core.exports.apply_to(&mut codegen_genv);
        }
    }
    for name in graph.discovery_order.iter() {
        let artifact = artifacts
            .get(name)
            .ok_or_else(|| compile_error(format!("missing package artifact for {}", name)))?;
        artifact.full_exports.apply_to(&mut codegen_genv);
    }
    let (mono, monoenv) = mono::mono(codegen_genv.clone(), core.clone()).map_err(compile_error)?;
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
        genv: codegen_genv,
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

pub fn typecheck_with_packages(
    path: &Path,
    src: &str,
) -> Result<(tast::File, GlobalTypeEnv, Diagnostics), CompilationError> {
    let single_file = should_use_single_file_mode(path)?;
    super::with_src_compiler_stack(src, || {
        let (_green_node, _cst, entry_ast) = parse_ast_from_source(path, src)?;
        let result = typecheck_packages_inner(path, entry_ast, single_file)?;
        Ok((result.entry_tast, result.genv, result.diagnostics))
    })
}

pub fn typecheck_with_packages_and_results(
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
    let single_file = should_use_single_file_mode(path)?;
    super::with_src_compiler_stack(src, || {
        let (_green_node, _cst, entry_ast, mut diagnostics) =
            parse_ast_from_source_allow_parse_errors(path, src)?;
        let root = discovery_root_for_file(path)?;
        let mut external_deps = load_external_dependencies(&root)?;
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
        add_stdlib_if_imported(&mut external_deps, &graph)?;
        external_deps
            .augment_graph(&mut graph)
            .map_err(compile_error)?;
        let order = packages::topo_sort_packages(&graph)?;
        let reachable_external = external_deps
            .reachable_package_names(&graph)
            .map_err(compile_error)?;

        let mut genv = builtins::builtin_env();
        let external_interfaces = external_deps.package_interfaces();
        let external_envs = external_deps.package_envs();
        for module in external_deps.modules.values() {
            for (name, package) in module.packages.iter() {
                if !reachable_external.contains(name) {
                    continue;
                }
                report_duplicate_trait_impls(
                    &mut diagnostics,
                    &genv,
                    &package.interface.exports,
                    name,
                );
                package.interface.exports.apply_to(&mut genv);
            }
        }
        let mut artifacts_by_name: HashMap<String, PackageInterface> = HashMap::new();
        let mut package_names: Vec<String> = graph.packages.keys().cloned().collect();
        package_names.sort();
        let package_ids = package_id_map(&package_names);

        let mut entry_hir_table = None;
        let mut entry_results = None;

        for name in order.iter() {
            let package = graph
                .packages
                .get(name)
                .ok_or_else(|| compile_error(format!("package {} not found", name)))?;
            let package_id = *package_ids
                .get(name)
                .unwrap_or_else(|| panic!("missing package id for {}", name));
            let mut deps_envs = HashMap::new();
            let mut deps_interfaces = HashMap::new();
            let dependencies = package_dependency_closure(package, &graph, &external_deps)?;
            for dep in dependencies.iter() {
                if let Some(interface) = artifacts_by_name.get(dep) {
                    deps_envs.insert(dep.clone(), interface.exports.to_genv());
                    deps_interfaces.insert(dep.clone(), interface.package_interface.clone());
                    continue;
                }
                if let Some(interface) = external_interfaces.get(dep) {
                    deps_envs.insert(
                        dep.clone(),
                        external_envs.get(dep).cloned().ok_or_else(|| {
                            compile_error(format!("missing package env for {}", dep))
                        })?,
                    );
                    deps_interfaces.insert(dep.clone(), interface.clone());
                    continue;
                }
                return Err(compile_error(format!(
                    "missing package artifact for {}",
                    dep
                )));
            }

            let (hir, hir_table, mut hir_diagnostics) = hir::lower_to_hir_files_with_env(
                package_id,
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

            let exports = PackageExports::public_from_package(
                name,
                &package.files,
                &package_genv,
                &mut diagnostics,
            );
            report_duplicate_trait_impls(&mut diagnostics, &genv, &exports, name);
            exports.apply_to(&mut genv);
            let package_interface =
                interface::PackageInterface::from_package(name, &package.declared_name, &exports);

            let interface = PackageInterface {
                exports,
                package_interface,
            };

            if name == &graph.entry_package {
                entry_hir_table = Some(hir_table);
                entry_results = Some(results);
            }

            artifacts_by_name.insert(name.clone(), interface);
        }

        let Some(entry_hir_table) = entry_hir_table else {
            return Err(compile_error("entry package not found".to_string()));
        };
        let Some(entry_results) = entry_results else {
            return Err(compile_error("entry package not found".to_string()));
        };

        Ok((entry_hir_table, entry_results, genv, diagnostics))
    })
}

fn validate_entrypoint_for_compile(
    diagnostics: &mut Diagnostics,
    graph: &packages::PackageGraph,
    artifacts: &HashMap<String, PackageArtifact>,
) {
    let Some(entry_package) = graph.packages.get(&graph.entry_package) else {
        return;
    };
    let Some(entry_artifact) = artifacts.get(&graph.entry_package) else {
        return;
    };
    if entry_package.declared_name != ROOT_PACKAGE {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!(
                "entry package {} declares package {}, expected main",
                graph.entry_package, entry_package.declared_name
            ),
        ));
        return;
    }
    super::separate::validate_entrypoint_scheme(
        &graph.entry_package,
        &entry_artifact.full_exports,
        diagnostics,
    );
}

fn load_external_dependencies(
    root: &Path,
) -> Result<ExternalDependencyArtifacts, CompilationError> {
    let manifest = root.join("goml.toml");
    if let Ok(module_manifest) = load_module_manifest(&manifest) {
        return crate::external::resolve_dependency_versions(&module_manifest.dependencies)
            .map_err(compile_error);
    }

    Ok(ExternalDependencyArtifacts::default())
}
