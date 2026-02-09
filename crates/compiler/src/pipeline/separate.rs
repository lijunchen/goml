use std::collections::{BTreeMap, HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use crate::artifact::{CoreUnit, InterfaceUnit, PackageExports};
use crate::builtins;
use crate::env::{Gensym, GlobalTypeEnv};
use crate::go::{self, compile::GlobalGoEnv, goast};
use crate::hir;
use crate::interface;
use crate::lift::{self, GlobalLiftEnv, LiftFile};
use crate::mono::{self, GlobalMonoEnv};
use crate::names::parse_inherent_method_fn_name;
use crate::package_names::{BUILTIN_PACKAGE, ENTRY_FUNCTION, ROOT_PACKAGE, is_builtin_package};
use crate::pipeline::compile_error;
use crate::pipeline::pipeline::{CompilationError, parse_ast_file};
use diagnostics::Diagnostics;

pub struct PackageInputs {
    pub package: String,
    pub input_files: Vec<PathBuf>,
    pub interface_files: Vec<PathBuf>,
}

#[derive(Debug)]
pub struct LinkOutput {
    pub go: goast::File,
    pub goenv: GlobalGoEnv,
    pub core: crate::core::File,
    pub genv: GlobalTypeEnv,
    pub mono: mono::MonoFile,
    pub monoenv: GlobalMonoEnv,
    pub lifted: LiftFile,
    pub liftenv: GlobalLiftEnv,
    pub anf: crate::anf::File,
    pub anfenv: crate::anf::GlobalAnfEnv,
}

fn validate_interface_unit(path: &Path, unit: &InterfaceUnit) -> Result<(), CompilationError> {
    if !unit.validate_hash() {
        return Err(compile_error(format!(
            "interface {} has invalid interface_hash",
            path.display()
        )));
    }
    if !is_builtin_package(&unit.package) {
        let expected = builtins::builtin_interface_hash();
        let Some(actual) = unit.deps.get(BUILTIN_PACKAGE) else {
            return Err(compile_error(format!(
                "interface {} is missing implicit builtin dependency (rebuild {})",
                path.display(),
                unit.package
            )));
        };
        if actual != &expected {
            return Err(compile_error(format!(
                "interface {} expects builtin interface_hash {}, but compiler has {} (rebuild {})",
                path.display(),
                actual,
                expected,
                unit.package
            )));
        }
    }
    Ok(())
}

fn load_interface_files(
    interface_files: &[PathBuf],
) -> Result<HashMap<String, (PathBuf, InterfaceUnit)>, CompilationError> {
    let mut units: HashMap<String, (PathBuf, InterfaceUnit)> = HashMap::new();

    for path in interface_files {
        if path.is_dir() {
            return Err(compile_error(format!(
                "interface path {} is a directory; pass a concrete .interface file",
                path.display()
            )));
        }
        let json = fs::read_to_string(path).map_err(|err| {
            compile_error(format!(
                "failed to read interface {}: {}",
                path.display(),
                err
            ))
        })?;
        let unit: InterfaceUnit = serde_json::from_str(&json).map_err(|err| {
            compile_error(format!(
                "failed to parse interface {}: {}",
                path.display(),
                err
            ))
        })?;
        validate_interface_unit(path, &unit)?;
        if let Some((prev_path, _)) = units.get(&unit.package) {
            return Err(compile_error(format!(
                "multiple interface files provided for package {}: {} and {}",
                unit.package,
                prev_path.display(),
                path.display()
            )));
        }
        units.insert(unit.package.clone(), (path.clone(), unit));
    }

    Ok(units)
}

fn load_interface_for_package(
    package: &str,
    interface_files: &[PathBuf],
    units: &HashMap<String, (PathBuf, InterfaceUnit)>,
) -> Result<InterfaceUnit, CompilationError> {
    if let Some((_, unit)) = units.get(package) {
        return Ok(unit.clone());
    }

    Err(compile_error(format!(
        "missing interface file for package {} (provided: {})",
        package,
        if interface_files.is_empty() {
            "<none>".to_string()
        } else {
            interface_files
                .iter()
                .map(|p| p.display().to_string())
                .collect::<Vec<_>>()
                .join(", ")
        }
    )))
}

fn read_source_files(
    package: &str,
    input_files: &[PathBuf],
) -> Result<ReadSourceFilesResult, CompilationError> {
    if input_files.is_empty() {
        return Err(compile_error("no input files provided".to_string()));
    }

    let mut paths = input_files.to_vec();
    paths.sort();
    paths.dedup();

    let mut files = Vec::new();
    let mut imports = HashSet::new();
    let mut source_list = Vec::new();

    for path in paths {
        let src = fs::read_to_string(&path)
            .map_err(|err| compile_error(format!("failed to read {}: {}", path.display(), err)))?;
        let ast = parse_ast_file(&path, &src)?;
        if ast.package.0 != package {
            return Err(compile_error(format!(
                "package mismatch in {}: expected {}, found {}",
                path.display(),
                package,
                ast.package.0
            )));
        }
        for import in ast.imports.iter() {
            imports.insert(import.0.clone());
        }
        for use_trait in ast.use_traits.iter() {
            let Some(first) = use_trait.segments().first() else {
                continue;
            };
            imports.insert(first.ident.0.clone());
        }
        source_list.push(path.display().to_string());
        files.push(hir::SourceFileAst { path, ast });
    }

    Ok((files, imports, source_list))
}

type ReadSourceFilesResult = (Vec<hir::SourceFileAst>, HashSet<String>, Vec<String>);

fn collect_required_builtin_collection_methods_from_core(
    core: &crate::core::File,
) -> HashSet<(String, String)> {
    let mut required = HashSet::new();
    for func in core.toplevels.iter() {
        collect_required_builtin_collection_methods_from_expr(&func.body, &mut required);
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
) -> Result<crate::core::File, CompilationError> {
    if required.is_empty() {
        return Ok(crate::core::File {
            toplevels: Vec::new(),
        });
    }
    let mut diagnostics = Diagnostics::new();
    let core = crate::compile_match::compile_file(
        &builtins::builtin_env(),
        gensym,
        &mut diagnostics,
        &builtins::builtin_collection_impl_tast(),
    );
    if diagnostics.has_errors() {
        return Err(CompilationError::Compile { diagnostics });
    }
    let toplevels = core
        .toplevels
        .into_iter()
        .filter(|f| {
            parse_inherent_method_fn_name(&f.name)
                .map(|(base, method)| required.contains(&(base.to_string(), method.to_string())))
                .unwrap_or(false)
        })
        .collect();
    Ok(crate::core::File { toplevels })
}

fn typecheck_single_package(
    package: &str,
    files: Vec<hir::SourceFileAst>,
    deps_interfaces: &HashMap<String, interface::PackageInterface>,
    deps_envs: HashMap<String, GlobalTypeEnv>,
) -> (
    crate::tast::File,
    PackageExports,
    interface::PackageInterface,
    diagnostics::Diagnostics,
) {
    let package_id = interface::package_id_for_name(package);
    let (hir, hir_table, mut hir_diagnostics) =
        hir::lower_to_hir_files_with_env(package_id, files, deps_interfaces);
    let (tast, genv, mut diagnostics) = crate::typer::check_file_with_env(
        hir,
        hir_table,
        GlobalTypeEnv::new(),
        builtins::builtin_env(),
        package,
        deps_envs,
    );
    diagnostics.append(&mut hir_diagnostics);
    let exports = PackageExports {
        type_env: genv.type_env.clone(),
        trait_env: genv.trait_env.clone(),
        value_env: genv.value_env.clone(),
    };
    let pkg_interface = interface::PackageInterface::from_exports(package, &exports);
    (tast, exports, pkg_interface, diagnostics)
}

pub fn check_package(opts: PackageInputs) -> Result<InterfaceUnit, CompilationError> {
    let (files, imports, _sources) = read_source_files(&opts.package, &opts.input_files)?;
    let interface_units = load_interface_files(&opts.interface_files)?;

    let mut deps: Vec<String> = imports.into_iter().collect();
    deps.sort();
    deps.dedup();

    let mut deps_envs = HashMap::new();
    let mut deps_interfaces = HashMap::new();
    let mut dep_hashes = BTreeMap::new();

    if opts.package != BUILTIN_PACKAGE {
        dep_hashes.insert(
            BUILTIN_PACKAGE.to_string(),
            builtins::builtin_interface_hash(),
        );
    }

    for dep in deps {
        if dep == BUILTIN_PACKAGE || dep == opts.package {
            continue;
        }
        let unit = load_interface_for_package(&dep, &opts.interface_files, &interface_units)?;
        deps_envs.insert(dep.clone(), unit.exports.to_genv());
        deps_interfaces.insert(dep.clone(), unit.interface.clone());
        dep_hashes.insert(dep, unit.interface_hash.clone());
    }

    let (tast, exports, pkg_interface, diagnostics) =
        typecheck_single_package(&opts.package, files, &deps_interfaces, deps_envs);
    drop(tast);

    let interface = InterfaceUnit::new(opts.package.clone(), exports, pkg_interface, dep_hashes);
    if diagnostics.has_errors() {
        return Err(CompilationError::Typer { diagnostics });
    }

    Ok(interface)
}

pub fn build_package(opts: PackageInputs) -> Result<CoreUnit, CompilationError> {
    let (files, imports, sources) = read_source_files(&opts.package, &opts.input_files)?;
    let interface_units = load_interface_files(&opts.interface_files)?;

    let mut deps: Vec<String> = imports.into_iter().collect();
    deps.sort();
    deps.dedup();

    let mut deps_envs = HashMap::new();
    let mut deps_interfaces = HashMap::new();
    let mut dep_hashes = BTreeMap::new();
    let mut dep_units = Vec::new();

    if opts.package != BUILTIN_PACKAGE {
        dep_hashes.insert(
            BUILTIN_PACKAGE.to_string(),
            builtins::builtin_interface_hash(),
        );
    }

    for dep in deps {
        if dep == BUILTIN_PACKAGE || dep == opts.package {
            continue;
        }
        let unit = load_interface_for_package(&dep, &opts.interface_files, &interface_units)?;
        deps_envs.insert(dep.clone(), unit.exports.to_genv());
        deps_interfaces.insert(dep.clone(), unit.interface.clone());
        dep_hashes.insert(dep.clone(), unit.interface_hash.clone());
        dep_units.push(unit);
    }

    let (tast, exports, pkg_interface, diagnostics) =
        typecheck_single_package(&opts.package, files, &deps_interfaces, deps_envs);
    if diagnostics.has_errors() {
        return Err(CompilationError::Typer { diagnostics });
    }

    let interface = InterfaceUnit::new(opts.package.clone(), exports, pkg_interface, dep_hashes);

    let gensym = Gensym::new();
    let mut env = builtins::builtin_env();
    for dep in dep_units.iter() {
        dep.exports.apply_to(&mut env);
    }
    interface.exports.apply_to(&mut env);
    let mut compile_diagnostics = Diagnostics::new();
    let core_ir =
        crate::compile_match::compile_file(&env, &gensym, &mut compile_diagnostics, &tast);
    if compile_diagnostics.has_errors() {
        return Err(CompilationError::Compile {
            diagnostics: compile_diagnostics,
        });
    }

    let mut unit = CoreUnit::new(opts.package.clone(), interface, core_ir);
    unit.sources = sources;

    Ok(unit)
}

pub fn read_core(path: &Path) -> Result<CoreUnit, CompilationError> {
    let json = fs::read_to_string(path)
        .map_err(|err| compile_error(format!("failed to read {}: {}", path.display(), err)))?;
    let unit: CoreUnit = serde_json::from_str(&json)
        .map_err(|err| compile_error(format!("failed to parse {}: {}", path.display(), err)))?;
    if !unit.validate() {
        return Err(compile_error(format!(
            "core {} failed validation",
            path.display()
        )));
    }
    Ok(unit)
}

pub fn link_cores(cores: Vec<CoreUnit>) -> Result<LinkOutput, CompilationError> {
    if cores.is_empty() {
        return Err(compile_error("no core inputs provided".to_string()));
    }

    let mut by_name = HashMap::new();
    for core in cores {
        if by_name.contains_key(&core.package) {
            return Err(compile_error(format!(
                "duplicate core provided for package {}",
                core.package
            )));
        }
        by_name.insert(core.package.clone(), core);
    }

    let Some((main_package, main)) = by_name.get_key_value(ROOT_PACKAGE) else {
        return Err(compile_error("missing main package core".to_string()));
    };
    if !main
        .core_ir
        .toplevels
        .iter()
        .any(|f| f.name == ENTRY_FUNCTION)
    {
        return Err(compile_error(format!(
            "{} package missing main function",
            main_package
        )));
    }

    let builtin_hash = builtins::builtin_interface_hash();
    for (pkg, unit) in by_name.iter() {
        for (dep, expected_hash) in unit.deps.iter() {
            if dep == BUILTIN_PACKAGE {
                if expected_hash != &builtin_hash {
                    return Err(compile_error(format!(
                        "package {} expects builtin interface_hash {}, but compiler has {} (rebuild {})",
                        pkg, expected_hash, builtin_hash, pkg
                    )));
                }
                continue;
            }
            let Some(dep_unit) = by_name.get(dep) else {
                return Err(compile_error(format!(
                    "package {} depends on missing package {}",
                    pkg, dep
                )));
            };
            if &dep_unit.interface.interface_hash != expected_hash {
                return Err(compile_error(format!(
                    "package {} expects interface_hash {} for {}, but got {} (rebuild {})",
                    pkg, expected_hash, dep, dep_unit.interface.interface_hash, pkg
                )));
            }
        }
    }

    let order = topo_sort(&by_name)?;

    let mut genv = builtins::builtin_env();
    let mut diagnostics = Diagnostics::new();
    for pkg in order.iter() {
        let unit = by_name
            .get(pkg)
            .ok_or_else(|| compile_error(format!("missing core for package {}", pkg)))?;
        for (key, _) in unit.interface.exports.trait_env.trait_impls.iter() {
            if genv.trait_env.trait_impls.contains_key(key) {
                diagnostics.push(diagnostics::Diagnostic::new(
                    diagnostics::Stage::Typer,
                    diagnostics::Severity::Error,
                    format!(
                        "Trait {} implementation for {:?} is defined in multiple packages (including {})",
                        key.0, key.1, pkg
                    ),
                ));
            }
        }
        unit.interface.exports.apply_to(&mut genv);
    }
    if diagnostics.has_errors() {
        return Err(CompilationError::Typer { diagnostics });
    }

    let mut linked = crate::core::File {
        toplevels: Vec::new(),
    };
    for pkg in order {
        let unit = by_name
            .get(&pkg)
            .ok_or_else(|| compile_error(format!("missing core for package {}", pkg)))?;
        linked.toplevels.extend(unit.core_ir.toplevels.clone());
    }

    let gensym = Gensym::new();
    let required_builtin_methods = collect_required_builtin_collection_methods_from_core(&linked);
    let builtin_collection_core =
        compile_builtin_collection_methods(&required_builtin_methods, &gensym)?;
    if !builtin_collection_core.toplevels.is_empty() {
        linked.toplevels.extend(builtin_collection_core.toplevels);
    }
    let (mono, monoenv) = mono::mono(genv.clone(), linked.clone());
    let (lifted, liftenv) = lift::lambda_lift(monoenv.clone(), &gensym, mono.clone());
    let (anf, anfenv) = crate::anf::anf_file(liftenv.clone(), &gensym, lifted.clone());
    let (go, goenv) = go::compile::go_file(anfenv.clone(), &gensym, anf.clone());

    Ok(LinkOutput {
        go,
        goenv,
        core: linked,
        genv,
        mono,
        monoenv,
        lifted,
        liftenv,
        anf,
        anfenv,
    })
}

fn topo_sort(cores: &HashMap<String, CoreUnit>) -> Result<Vec<String>, CompilationError> {
    use std::collections::BTreeSet;

    let mut indeg: BTreeMap<String, usize> = BTreeMap::new();
    let mut edges: BTreeMap<String, Vec<String>> = BTreeMap::new();

    for name in cores.keys() {
        indeg.entry(name.clone()).or_insert(0);
        edges.entry(name.clone()).or_default();
    }

    let mut names: Vec<String> = cores.keys().cloned().collect();
    names.sort();
    for name in names {
        let unit = cores
            .get(&name)
            .ok_or_else(|| compile_error(format!("missing core for package {}", name)))?;
        for dep in unit.deps.keys() {
            if !cores.contains_key(dep) {
                continue;
            }
            edges.entry(dep.clone()).or_default().push(name.clone());
            *indeg.entry(name.clone()).or_insert(0) += 1;
        }
    }

    let mut queue: BTreeSet<String> = indeg
        .iter()
        .filter_map(|(k, &v)| (v == 0).then_some(k.clone()))
        .collect();

    let mut out = Vec::new();
    while let Some(n) = queue.pop_first() {
        out.push(n.clone());
        if let Some(nexts) = edges.get(&n) {
            for next in nexts {
                if let Some(v) = indeg.get_mut(next) {
                    *v -= 1;
                    if *v == 0 {
                        queue.insert(next.clone());
                    }
                }
            }
        }
    }

    if out.len() != cores.len() {
        return Err(compile_error(
            "package dependency cycle detected in core inputs".to_string(),
        ));
    }

    Ok(out)
}
