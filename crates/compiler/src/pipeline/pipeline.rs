use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use ast::ast;
use cst::cst::{CstNode, File as CstFile};
use diagnostics::{Diagnostic, Diagnostics, Severity, Stage};
use parser::{self, syntax::MySyntaxNode};
use rowan::GreenNode;

use crate::package_names::{BUILTIN_PACKAGE, ROOT_PACKAGE};
use crate::pipeline::compile_error;
use crate::pipeline::packages;
use crate::{
    anf::{self, GlobalAnfEnv},
    artifact::PackageExports,
    builtins, compile_match, derive,
    env::{Gensym, GlobalTypeEnv},
    go::{self, compile::GlobalGoEnv, goast},
    hir, interface,
    lift::{self, GlobalLiftEnv, LiftFile},
    mono::{self, GlobalMonoEnv},
    names::parse_inherent_method_fn_name,
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
struct PackageInterface {
    exports: PackageExports,
    package_interface: interface::PackageInterface,
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

fn build_package<'a>(
    gensym: &Gensym,
    diagnostics: &mut Diagnostics,
    package: &'a PackageArtifact,
    deps: &[&PackageInterface],
) -> (&'a PackageInterface, crate::core::File) {
    let mut env = builtins::builtin_env();
    for dep in deps {
        dep.exports.apply_to(&mut env);
    }
    package.interface.exports.apply_to(&mut env);

    (
        &package.interface,
        compile_match::compile_file(&env, gensym, diagnostics, &package.tast),
    )
}

fn link_packages(packages: Vec<crate::core::File>) -> crate::core::File {
    let mut toplevels = Vec::new();
    for package in packages {
        toplevels.extend(package.toplevels);
    }
    crate::core::File { toplevels }
}

fn collect_required_builtin_collection_methods(
    files: &[crate::core::File],
) -> HashSet<(String, String)> {
    let mut required = HashSet::new();
    for file in files {
        for func in file.toplevels.iter() {
            collect_required_builtin_collection_methods_from_expr(&func.body, &mut required);
        }
    }
    required
}

fn collect_required_builtin_collection_methods_from_expr(
    expr: &crate::core::Expr,
    required: &mut HashSet<(String, String)>,
) {
    match expr {
        crate::core::Expr::EVar { .. } | crate::core::Expr::EPrim { .. } => {}
        crate::core::Expr::EConstr { args, .. }
        | crate::core::Expr::ETuple { items: args, .. }
        | crate::core::Expr::EArray { items: args, .. } => {
            for arg in args.iter() {
                collect_required_builtin_collection_methods_from_expr(arg, required);
            }
        }
        crate::core::Expr::EClosure { body, .. } => {
            collect_required_builtin_collection_methods_from_expr(body, required);
        }
        crate::core::Expr::ELet { value, body, .. } => {
            collect_required_builtin_collection_methods_from_expr(value, required);
            collect_required_builtin_collection_methods_from_expr(body, required);
        }
        crate::core::Expr::EMatch {
            expr,
            arms,
            default,
            ..
        } => {
            collect_required_builtin_collection_methods_from_expr(expr, required);
            for arm in arms.iter() {
                collect_required_builtin_collection_methods_from_expr(&arm.lhs, required);
                collect_required_builtin_collection_methods_from_expr(&arm.body, required);
            }
            if let Some(default) = default {
                collect_required_builtin_collection_methods_from_expr(default, required);
            }
        }
        crate::core::Expr::EIf {
            cond,
            then_branch,
            else_branch,
            ..
        } => {
            collect_required_builtin_collection_methods_from_expr(cond, required);
            collect_required_builtin_collection_methods_from_expr(then_branch, required);
            collect_required_builtin_collection_methods_from_expr(else_branch, required);
        }
        crate::core::Expr::EWhile { cond, body, .. } => {
            collect_required_builtin_collection_methods_from_expr(cond, required);
            collect_required_builtin_collection_methods_from_expr(body, required);
        }
        crate::core::Expr::EGo { expr, .. }
        | crate::core::Expr::EConstrGet { expr, .. }
        | crate::core::Expr::EUnary { expr, .. }
        | crate::core::Expr::EToDyn { expr, .. }
        | crate::core::Expr::EProj { tuple: expr, .. } => {
            collect_required_builtin_collection_methods_from_expr(expr, required);
        }
        crate::core::Expr::EBinary { lhs, rhs, .. } => {
            collect_required_builtin_collection_methods_from_expr(lhs, required);
            collect_required_builtin_collection_methods_from_expr(rhs, required);
        }
        crate::core::Expr::ECall { func, args, .. } => {
            if let crate::core::Expr::EVar { name, .. } = func.as_ref()
                && let Some((base, method)) = parse_inherent_method_fn_name(name)
                && matches!(base, "Vec" | "Slice" | "HashMap")
            {
                required.insert((base.to_string(), method.to_string()));
            }
            collect_required_builtin_collection_methods_from_expr(func, required);
            for arg in args.iter() {
                collect_required_builtin_collection_methods_from_expr(arg, required);
            }
        }
        crate::core::Expr::EDynCall { receiver, args, .. }
        | crate::core::Expr::ETraitCall { receiver, args, .. } => {
            collect_required_builtin_collection_methods_from_expr(receiver, required);
            for arg in args.iter() {
                collect_required_builtin_collection_methods_from_expr(arg, required);
            }
        }
    }
}

fn compile_builtin_collection_methods(
    required: &HashSet<(String, String)>,
    gensym: &Gensym,
    diagnostics: &mut Diagnostics,
) -> crate::core::File {
    if required.is_empty() {
        return crate::core::File {
            toplevels: Vec::new(),
        };
    }
    let core = compile_match::compile_file(
        &builtins::builtin_env(),
        gensym,
        diagnostics,
        &builtins::builtin_collection_impl_tast(),
    );
    let toplevels = core
        .toplevels
        .into_iter()
        .filter(|f| {
            parse_inherent_method_fn_name(&f.name)
                .map(|(base, method)| required.contains(&(base.to_string(), method.to_string())))
                .unwrap_or(false)
        })
        .collect();
    crate::core::File { toplevels }
}

#[derive(Debug, Clone)]
struct PackageArtifact {
    tast: tast::File,
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
    let exports = PackageExports {
        type_env: genv.type_env.clone(),
        trait_env: genv.trait_env.clone(),
        value_env: genv.value_env.clone(),
    };
    let package_interface = interface::PackageInterface::from_exports(&package.name, &exports);

    PackageArtifact {
        tast,
        interface: PackageInterface {
            exports,
            package_interface,
        },
        diagnostics,
    }
}

fn typecheck_packages(
    path: &Path,
    entry_ast: ast::File,
) -> Result<TypecheckPackagesResult, CompilationError> {
    let graph =
        packages::discover_packages(&discovery_root_for_file(path), Some(path), Some(entry_ast))?;
    let order = packages::topo_sort_packages(&graph)?;

    let mut diagnostics = Diagnostics::new();
    let mut genv = builtins::builtin_env();
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
        let mut deps: Vec<_> = package.imports.iter().cloned().collect();
        deps.sort();
        let mut deps_interfaces = HashMap::new();
        for dep in deps.iter() {
            let artifact = artifacts_by_name
                .get(dep)
                .ok_or_else(|| compile_error(format!("missing package artifact for {}", dep)))?;
            deps_envs.insert(dep.clone(), artifact.interface.exports.to_genv());
            deps_interfaces.insert(dep.clone(), artifact.interface.package_interface.clone());
        }

        let artifact = typecheck_package(package_id, package, deps_envs, &deps_interfaces);

        let mut package_diagnostics = artifact.diagnostics.clone();
        diagnostics.append(&mut package_diagnostics);
        for (key, _) in artifact.interface.exports.trait_env.trait_impls.iter() {
            if genv.trait_env.trait_impls.contains_key(key) {
                diagnostics.push(Diagnostic::new(
                    Stage::Typer,
                    Severity::Error,
                    format!(
                        "Trait {} implementation for {:?} is defined in multiple packages (including {})",
                        key.0, key.1, name
                    ),
                ));
            }
        }
        artifact.interface.exports.apply_to(&mut genv);
        artifacts_by_name.insert(name.clone(), artifact);
    }

    let entry_tast = artifacts_by_name
        .get(&graph.entry_package)
        .ok_or_else(|| compile_error("entry package not found".to_string()))?
        .tast
        .clone();

    let mut toplevels = Vec::new();
    let mut has_print = false;
    let mut has_println = false;
    for name in graph.discovery_order.iter() {
        let artifact = artifacts_by_name
            .get(name)
            .ok_or_else(|| compile_error(format!("missing package artifact for {}", name)))?;
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

    Ok(TypecheckPackagesResult {
        entry_tast,
        full_tast: tast::File { toplevels },
        genv,
        diagnostics,
        graph,
        artifacts: artifacts_by_name,
    })
}

fn discovery_root_for_file(path: &Path) -> PathBuf {
    if let Ok((module_dir, _)) = packages::discover_project_from_file(path) {
        return module_dir;
    }
    path.parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."))
        .to_path_buf()
}

pub fn compile(path: &Path, src: &str) -> Result<Compilation, CompilationError> {
    let (green_node, cst, entry_ast) = parse_ast_from_source(path, src)?;

    let typecheck = typecheck_packages(path, entry_ast.clone())?;
    let TypecheckPackagesResult {
        full_tast,
        genv,
        mut diagnostics,
        graph,
        artifacts,
        ..
    } = typecheck;
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

    let gensym = Gensym::new();

    let mut package_cores = Vec::new();
    let builtin_print_core = compile_match::compile_file(
        &builtins::builtin_env(),
        &gensym,
        &mut diagnostics,
        &builtins::builtin_print_tast(),
    );
    package_cores.push(builtin_print_core);
    for name in graph.discovery_order.iter() {
        let package = graph
            .packages
            .get(name)
            .ok_or_else(|| compile_error(format!("package {} not found", name)))?;
        let artifact = artifacts
            .get(name)
            .ok_or_else(|| compile_error(format!("missing package artifact for {}", name)))?;
        let mut deps: Vec<_> = package.imports.iter().cloned().collect();
        deps.sort();
        let mut dep_interfaces = Vec::new();
        for dep in deps.iter() {
            let dep_artifact = artifacts
                .get(dep)
                .ok_or_else(|| compile_error(format!("missing package artifact for {}", dep)))?;
            dep_interfaces.push(&dep_artifact.interface);
        }
        let (_interface, core) =
            build_package(&gensym, &mut diagnostics, artifact, &dep_interfaces);
        package_cores.push(core);
    }
    let required_builtin_methods = collect_required_builtin_collection_methods(&package_cores);
    let builtin_collection_core =
        compile_builtin_collection_methods(&required_builtin_methods, &gensym, &mut diagnostics);
    if !builtin_collection_core.toplevels.is_empty() {
        package_cores.push(builtin_collection_core);
    }
    let core = link_packages(package_cores);
    if diagnostics.has_errors() {
        return Err(CompilationError::Compile { diagnostics });
    }
    let (mono, monoenv) = mono::mono(genv.clone(), core.clone());
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

pub fn typecheck_with_packages(
    path: &Path,
    src: &str,
) -> Result<(tast::File, GlobalTypeEnv, Diagnostics), CompilationError> {
    let (_green_node, _cst, entry_ast) = parse_ast_from_source(path, src)?;
    let result = typecheck_packages(path, entry_ast)?;
    Ok((result.entry_tast, result.genv, result.diagnostics))
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
    let (_green_node, _cst, entry_ast, mut diagnostics) =
        parse_ast_from_source_allow_parse_errors(path, src)?;
    let graph =
        packages::discover_packages(&discovery_root_for_file(path), Some(path), Some(entry_ast))?;
    let order = packages::topo_sort_packages(&graph)?;

    let mut genv = builtins::builtin_env();
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
        let mut deps: Vec<_> = package.imports.iter().cloned().collect();
        deps.sort();
        let mut deps_interfaces = HashMap::new();
        for dep in deps.iter() {
            let interface = artifacts_by_name
                .get(dep)
                .ok_or_else(|| compile_error(format!("missing package artifact for {}", dep)))?;
            deps_envs.insert(dep.clone(), interface.exports.to_genv());
            deps_interfaces.insert(dep.clone(), interface.package_interface.clone());
        }

        let (hir, hir_table, mut hir_diagnostics) =
            hir::lower_to_hir_files_with_env(package_id, package.files.clone(), &deps_interfaces);
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

        for (key, _) in package_genv.trait_env.trait_impls.iter() {
            if genv.trait_env.trait_impls.contains_key(key) {
                diagnostics.push(Diagnostic::new(
                    Stage::Typer,
                    Severity::Error,
                    format!(
                        "Trait {} implementation for {:?} is defined in multiple packages (including {})",
                        key.0, key.1, name
                    ),
                ));
            }
        }

        let exports = PackageExports {
            type_env: package_genv.type_env.clone(),
            trait_env: package_genv.trait_env.clone(),
            value_env: package_genv.value_env.clone(),
        };
        exports.apply_to(&mut genv);
        let package_interface = interface::PackageInterface::from_exports(name, &exports);

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
}
