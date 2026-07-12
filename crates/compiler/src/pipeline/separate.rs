use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
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
use crate::package_names::is_special_unqualified_package;
use crate::package_names::{
    BUILTIN_PACKAGE, ENTRY_FUNCTION, ROOT_PACKAGE, STD_PACKAGE, is_builtin_package,
};
use crate::pipeline::pipeline::{CompilationError, parse_ast_file, report_duplicate_trait_impls};
use crate::pipeline::{compile_error, with_compiler_stack};
use crate::stdlib;
use diagnostics::{Diagnostic, Diagnostics, Severity, Stage};
use serde::Deserialize;

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
    if unit.format_version != crate::artifact::FORMAT_VERSION {
        return Err(compile_error(format!(
            "interface {} uses format version {}, expected {}",
            path.display(),
            unit.format_version,
            crate::artifact::FORMAT_VERSION
        )));
    }
    if unit.compiler_abi != crate::artifact::COMPILER_ABI {
        return Err(compile_error(format!(
            "interface {} uses compiler ABI {}, expected {}",
            path.display(),
            unit.compiler_abi,
            crate::artifact::COMPILER_ABI
        )));
    }
    if !unit.validate_hash() {
        return Err(compile_error(format!(
            "interface {} has invalid interface_hash",
            path.display()
        )));
    }
    if !unit.validate() {
        return Err(compile_error(format!(
            "interface {} failed validation",
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
    with_compiler_stack(|| {
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
            let mut deserializer = serde_json::Deserializer::from_str(&json);
            deserializer.disable_recursion_limit();
            let unit = InterfaceUnit::deserialize(&mut deserializer).map_err(|err| {
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
    })
}

fn load_interface_for_package(
    package: &str,
    interface_files: &[PathBuf],
    units: &HashMap<String, (PathBuf, InterfaceUnit)>,
) -> Result<(InterfaceUnit, interface::PackageInterface), CompilationError> {
    if let Some((_, unit)) = units.get(package) {
        return Ok((unit.clone(), unit.interface.clone()));
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

fn dependency_interface_unit(
    package: &str,
    interface_files: &[PathBuf],
    units: &HashMap<String, (PathBuf, InterfaceUnit)>,
) -> Result<InterfaceUnit, CompilationError> {
    if package == STD_PACKAGE || package.starts_with("std::") {
        return stdlib::stdlib_package_interface(package).map_err(compile_error);
    }
    load_interface_for_package(package, interface_files, units).map(|(unit, _)| unit)
}

fn resolve_dependency_interfaces(
    current_package: &str,
    direct_dependencies: &BTreeSet<String>,
    interface_files: &[PathBuf],
    units: &HashMap<String, (PathBuf, InterfaceUnit)>,
) -> Result<Vec<InterfaceUnit>, CompilationError> {
    let mut pending = direct_dependencies
        .iter()
        .cloned()
        .map(|package| (package, None::<(String, String)>))
        .collect::<Vec<_>>();
    let mut resolved = BTreeMap::<String, InterfaceUnit>::new();

    while let Some((package, expected)) = pending.pop() {
        if package == BUILTIN_PACKAGE {
            if let Some((parent, expected_hash)) = expected {
                let actual_hash = builtins::builtin_interface_hash();
                if expected_hash != actual_hash {
                    return Err(compile_error(format!(
                        "interface {} expects builtin interface_hash {}, but compiler has {}",
                        parent, expected_hash, actual_hash
                    )));
                }
            }
            continue;
        }
        if package == current_package {
            return Err(compile_error(format!(
                "package {} cannot use itself",
                current_package
            )));
        }

        let unit = if let Some(unit) = resolved.get(&package) {
            unit.clone()
        } else {
            dependency_interface_unit(&package, interface_files, units)?
        };
        if let Some((parent, expected_hash)) = expected
            && unit.interface_hash != expected_hash
        {
            return Err(compile_error(format!(
                "interface {} expects interface_hash {} for {}, but got {}",
                parent, expected_hash, package, unit.interface_hash
            )));
        }
        if resolved.contains_key(&package) {
            continue;
        }
        for (dependency, expected_hash) in unit.deps.iter() {
            pending.push((
                dependency.clone(),
                Some((unit.package.clone(), expected_hash.clone())),
            ));
        }
        resolved.insert(package, unit);
    }

    Ok(resolved.into_values().collect())
}

fn read_source_files(
    package: &str,
    input_files: &[PathBuf],
    interface_units: &HashMap<String, (PathBuf, InterfaceUnit)>,
) -> Result<ReadSourceFilesResult, CompilationError> {
    crate::config::validate_module_path(package).map_err(compile_error)?;
    if input_files.is_empty() {
        return Err(compile_error("no input files provided".to_string()));
    }

    let mut paths = input_files.to_vec();
    paths.sort();
    paths.dedup();

    let mut files = Vec::new();
    let mut imports = HashSet::new();
    let mut source_list = Vec::new();
    let mut declared_name = None::<String>;

    for path in paths {
        let src = fs::read_to_string(&path)
            .map_err(|err| compile_error(format!("failed to read {}: {}", path.display(), err)))?;
        let mut ast = parse_ast_file(&path, &src)?;
        if !ast.package_explicit {
            return Err(compile_error(format!(
                "{} must declare `package <name>;`",
                path.display()
            )));
        }
        if let Some(existing) = declared_name.as_deref() {
            if existing != ast.package.0 {
                return Err(compile_error(format!(
                    "package mismatch in {}: expected {}, found {}",
                    path.display(),
                    existing,
                    ast.package.0
                )));
            }
        } else {
            declared_name = Some(ast.package.0.clone());
        }
        let mut known_packages = HashSet::new();
        let mut aliases = HashSet::new();
        for use_decl in ast.uses.iter() {
            let target = use_decl.path.display();
            let default_alias = if target == STD_PACKAGE || target.starts_with("std::") {
                target.rsplit("::").next().map(str::to_string)
            } else {
                interface_units
                    .get(&target)
                    .map(|(_, unit)| unit.interface.name.clone())
            };
            if let Some(default_alias) = default_alias {
                known_packages.insert(target);
                aliases.insert(
                    use_decl
                        .alias
                        .as_ref()
                        .map(|alias| alias.0.clone())
                        .unwrap_or(default_alias),
                );
            }
        }
        for use_decl in ast.uses.iter() {
            let target = use_decl.path.display();
            if known_packages.contains(&target) {
                imports.insert(target);
                continue;
            }
            let first = use_decl
                .path
                .segments()
                .first()
                .map(|segment| segment.ident.0.as_str());
            if !first.is_some_and(|first| aliases.contains(first)) {
                imports.insert(target);
            }
        }
        ast.package = ast::ast::AstIdent::new(package);
        source_list.push(path.display().to_string());
        files.push(hir::SourceFileAst::new(path, ast));
    }

    Ok((
        files,
        imports,
        source_list,
        declared_name.unwrap_or_else(|| package.rsplit("::").next().unwrap_or(package).to_string()),
    ))
}

type ReadSourceFilesResult = (
    Vec<hir::SourceFileAst>,
    HashSet<String>,
    Vec<String>,
    String,
);

fn typecheck_single_package(
    package: &str,
    declared_name: &str,
    files: Vec<hir::SourceFileAst>,
    deps_interfaces: &HashMap<String, interface::PackageInterface>,
    deps_envs: HashMap<String, GlobalTypeEnv>,
) -> (
    crate::tast::File,
    PackageExports,
    PackageExports,
    interface::PackageInterface,
    diagnostics::Diagnostics,
) {
    let package_id = interface::package_id_for_name(package);
    let (hir, hir_table, mut hir_diagnostics) =
        hir::lower_to_hir_files_with_env(package_id, files.clone(), deps_interfaces);
    let (tast, genv, mut diagnostics) = crate::typer::check_file_with_env(
        hir,
        hir_table,
        GlobalTypeEnv::new(),
        builtins::builtin_env(),
        package,
        deps_envs,
    );
    diagnostics.append(&mut hir_diagnostics);
    let full_exports = PackageExports::from_genv(&genv);
    let exports = PackageExports::public_from_package(package, &files, &genv, &mut diagnostics);
    let pkg_interface = interface::PackageInterface::from_package(package, declared_name, &exports);
    (tast, full_exports, exports, pkg_interface, diagnostics)
}

pub(crate) fn validate_entrypoint_scheme(
    package: &str,
    exports: &PackageExports,
    diagnostics: &mut Diagnostics,
) {
    let entry_name = if is_special_unqualified_package(package) {
        ENTRY_FUNCTION.to_string()
    } else {
        format!("{package}::{ENTRY_FUNCTION}")
    };
    let Some(scheme) = exports.value_env.funcs.get(&entry_name) else {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            "main function is required".to_string(),
        ));
        return;
    };
    if !scheme.type_params.is_empty() {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            "main function must not have type parameters".to_string(),
        ));
    }
    if let crate::tast::Ty::TFunc { params, .. } = &scheme.ty
        && !params.is_empty()
    {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            "main function must not have parameters".to_string(),
        ));
    }
}

pub fn check_package(opts: PackageInputs) -> Result<InterfaceUnit, CompilationError> {
    with_compiler_stack(|| {
        let interface_units = load_interface_files(&opts.interface_files)?;
        let (files, imports, _sources, declared_name) =
            read_source_files(&opts.package, &opts.input_files, &interface_units)?;

        let direct_dependencies = imports.into_iter().collect::<BTreeSet<_>>();
        let dependency_units = resolve_dependency_interfaces(
            &opts.package,
            &direct_dependencies,
            &opts.interface_files,
            &interface_units,
        )?;

        let mut deps_envs = HashMap::new();
        let mut deps_interfaces = HashMap::new();
        let mut dep_hashes = BTreeMap::new();

        if opts.package != BUILTIN_PACKAGE {
            dep_hashes.insert(
                BUILTIN_PACKAGE.to_string(),
                builtins::builtin_interface_hash(),
            );
        }

        for unit in dependency_units {
            deps_envs.insert(unit.package.clone(), unit.exports.to_genv());
            deps_interfaces.insert(unit.package.clone(), unit.interface.clone());
            if direct_dependencies.contains(&unit.package) {
                dep_hashes.insert(unit.package.clone(), unit.interface_hash.clone());
            }
        }

        let (tast, full_exports, exports, pkg_interface, mut diagnostics) =
            typecheck_single_package(
                &opts.package,
                &declared_name,
                files,
                &deps_interfaces,
                deps_envs,
            );
        drop(tast);

        if declared_name == ROOT_PACKAGE {
            validate_entrypoint_scheme(&opts.package, &full_exports, &mut diagnostics);
        }

        let interface =
            InterfaceUnit::new(opts.package.clone(), exports, pkg_interface, dep_hashes);
        if diagnostics.has_errors() {
            return Err(CompilationError::Typer { diagnostics });
        }

        Ok(interface)
    })
}

pub fn build_package(opts: PackageInputs) -> Result<CoreUnit, CompilationError> {
    with_compiler_stack(|| {
        let interface_units = load_interface_files(&opts.interface_files)?;
        let (files, imports, sources, declared_name) =
            read_source_files(&opts.package, &opts.input_files, &interface_units)?;

        let direct_dependencies = imports.into_iter().collect::<BTreeSet<_>>();
        let dependency_units = resolve_dependency_interfaces(
            &opts.package,
            &direct_dependencies,
            &opts.interface_files,
            &interface_units,
        )?;

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

        for unit in dependency_units {
            deps_envs.insert(unit.package.clone(), unit.exports.to_genv());
            deps_interfaces.insert(unit.package.clone(), unit.interface.clone());
            if direct_dependencies.contains(&unit.package) {
                dep_hashes.insert(unit.package.clone(), unit.interface_hash.clone());
            }
            dep_units.push(unit);
        }

        let (tast, full_exports, exports, pkg_interface, mut diagnostics) =
            typecheck_single_package(
                &opts.package,
                &declared_name,
                files,
                &deps_interfaces,
                deps_envs,
            );
        if declared_name == ROOT_PACKAGE {
            validate_entrypoint_scheme(&opts.package, &full_exports, &mut diagnostics);
        }
        if diagnostics.has_errors() {
            return Err(CompilationError::Typer { diagnostics });
        }

        let interface =
            InterfaceUnit::new(opts.package.clone(), exports, pkg_interface, dep_hashes);

        let gensym = Gensym::new();
        let mut env = builtins::builtin_env();
        for dep in dep_units.iter() {
            dep.exports.apply_to(&mut env);
        }
        full_exports.apply_to(&mut env);
        let mut compile_diagnostics = Diagnostics::new();
        let core_ir =
            crate::compile_match::compile_file(&env, &gensym, &mut compile_diagnostics, &tast);
        if compile_diagnostics.has_errors() {
            return Err(CompilationError::Compile {
                diagnostics: compile_diagnostics,
            });
        }

        let mut unit = CoreUnit::new(opts.package.clone(), interface, full_exports, core_ir);
        unit.sources = sources;

        Ok(unit)
    })
}

pub fn read_core(path: &Path) -> Result<CoreUnit, CompilationError> {
    with_compiler_stack(|| {
        let json = fs::read_to_string(path)
            .map_err(|err| compile_error(format!("failed to read {}: {}", path.display(), err)))?;
        let mut deserializer = serde_json::Deserializer::from_str(&json);
        deserializer.disable_recursion_limit();
        let unit = CoreUnit::deserialize(&mut deserializer)
            .map_err(|err| compile_error(format!("failed to parse {}: {}", path.display(), err)))?;
        if !unit.validate() {
            return Err(compile_error(format!(
                "core {} failed validation",
                path.display()
            )));
        }
        Ok(unit)
    })
}

pub fn link_cores(
    entry_package: &str,
    cores: Vec<CoreUnit>,
) -> Result<LinkOutput, CompilationError> {
    with_compiler_stack(|| {
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

        if !by_name.contains_key(entry_package) {
            return Err(compile_error(format!(
                "missing entry package core for {}",
                entry_package
            )));
        }
        let reachable = reachable_core_packages(entry_package, &by_name)?;
        by_name.retain(|package, _| reachable.contains(package));

        let Some((main_package, main)) = by_name.get_key_value(entry_package) else {
            return Err(compile_error(format!(
                "missing entry package core for {}",
                entry_package
            )));
        };
        if !main.interface.interface.name.is_empty()
            && main.interface.interface.name != ROOT_PACKAGE
        {
            return Err(compile_error(format!(
                "entry package {} declares package {}, expected main",
                entry_package, main.interface.interface.name
            )));
        }
        let entry_name = if is_special_unqualified_package(entry_package) {
            ENTRY_FUNCTION.to_string()
        } else {
            format!("{entry_package}::{ENTRY_FUNCTION}")
        };
        let Some(entry_fn) = main
            .core_ir
            .toplevels
            .iter()
            .find(|function| function.name == entry_name)
            .cloned()
        else {
            return Err(compile_error(format!(
                "{} package missing main function",
                main_package
            )));
        };
        if !entry_fn.params.is_empty() {
            return Err(compile_error(
                "main function must not have parameters".to_string(),
            ));
        }
        if !entry_fn.generics.is_empty() {
            return Err(compile_error(
                "main function must not have type parameters".to_string(),
            ));
        }

        let builtin_hash = builtins::builtin_interface_hash();
        let requested_std_packages = by_name
            .values()
            .flat_map(|unit| unit.deps.keys())
            .filter(|dep| dep.as_str() == STD_PACKAGE || dep.starts_with("std::"))
            .cloned()
            .collect::<HashSet<_>>();
        let mut std_cores = if requested_std_packages.is_empty() {
            HashMap::new()
        } else {
            stdlib::stdlib_cores()
                .map_err(compile_error)?
                .into_iter()
                .map(|unit| (unit.package.clone(), unit))
                .collect::<HashMap<_, _>>()
        };
        let reachable_std =
            reachable_std_packages(requested_std_packages, &std_cores).map_err(compile_error)?;
        std_cores.retain(|package, _| reachable_std.contains(package));
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
                if dep == STD_PACKAGE || dep.starts_with("std::") {
                    let actual_hash = std_cores
                        .get(dep)
                        .map(|unit| unit.interface.interface_hash.as_str());
                    if Some(expected_hash.as_str()) != actual_hash {
                        return Err(compile_error(format!(
                            "package {} expects interface_hash {} for {}, but compiler has {} (rebuild {})",
                            pkg,
                            expected_hash,
                            dep,
                            actual_hash.unwrap_or("<none>"),
                            pkg
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
        let mut std_packages = std_cores.keys().cloned().collect::<Vec<_>>();
        std_packages.sort();
        for package in std_packages.iter() {
            let std_core = std_cores
                .get(package)
                .ok_or_else(|| compile_error(format!("missing std core for {}", package)))?;
            std_core.exports.apply_to(&mut genv);
        }
        let mut diagnostics = Diagnostics::new();
        for pkg in order.iter() {
            let unit = by_name
                .get(pkg)
                .ok_or_else(|| compile_error(format!("missing core for package {}", pkg)))?;
            report_duplicate_trait_impls(&mut diagnostics, &genv, &unit.exports, pkg);
            unit.exports.apply_to(&mut genv);
        }
        if diagnostics.has_errors() {
            return Err(CompilationError::Typer { diagnostics });
        }

        let mut linked = crate::core::File {
            toplevels: Vec::new(),
        };

        let gensym = Gensym::new();
        let mut compile_diagnostics = Diagnostics::new();
        let mut builtin_core = crate::compile_match::compile_file(
            &builtins::builtin_env(),
            &gensym,
            &mut compile_diagnostics,
            &builtins::builtin_tast(),
        );
        for function in builtin_core.toplevels.iter_mut() {
            function.root = false;
        }
        linked.toplevels.extend(builtin_core.toplevels);
        for package in std_packages {
            let std_core = std_cores
                .get(&package)
                .ok_or_else(|| compile_error(format!("missing std core for {}", package)))?;
            linked.toplevels.extend(std_core.core_ir.toplevels.clone());
        }

        for pkg in order {
            let unit = by_name
                .get(&pkg)
                .ok_or_else(|| compile_error(format!("missing core for package {}", pkg)))?;
            linked.toplevels.extend(unit.core_ir.toplevels.clone());
        }
        if entry_name != ENTRY_FUNCTION {
            linked.toplevels.push(package_entry_wrapper(&entry_fn));
        }

        let (mono, monoenv) = mono::mono(genv.clone(), linked.clone()).map_err(compile_error)?;
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
    })
}

fn reachable_core_packages(
    entry_package: &str,
    cores: &HashMap<String, CoreUnit>,
) -> Result<HashSet<String>, CompilationError> {
    let mut reachable = HashSet::new();
    let mut pending = vec![entry_package.to_string()];
    while let Some(package) = pending.pop() {
        if !reachable.insert(package.clone()) {
            continue;
        }
        let unit = cores
            .get(&package)
            .ok_or_else(|| compile_error(format!("missing core for package {}", package)))?;
        for dependency in unit.deps.keys() {
            if dependency == BUILTIN_PACKAGE
                || dependency == STD_PACKAGE
                || dependency.starts_with("std::")
            {
                continue;
            }
            if !cores.contains_key(dependency) {
                return Err(compile_error(format!(
                    "package {} depends on missing package {}",
                    package, dependency
                )));
            }
            pending.push(dependency.clone());
        }
    }
    Ok(reachable)
}

fn reachable_std_packages(
    requested: HashSet<String>,
    cores: &HashMap<String, CoreUnit>,
) -> Result<HashSet<String>, String> {
    let mut reachable = HashSet::new();
    let mut pending = requested.into_iter().collect::<Vec<_>>();
    while let Some(package) = pending.pop() {
        if !reachable.insert(package.clone()) {
            continue;
        }
        let unit = cores
            .get(&package)
            .ok_or_else(|| format!("standard library package {} not found", package))?;
        for dependency in unit.deps.keys() {
            if dependency == STD_PACKAGE || dependency.starts_with("std::") {
                pending.push(dependency.clone());
            }
        }
    }
    Ok(reachable)
}

pub(crate) fn package_entry_wrapper(entry_fn: &crate::core::Fn) -> crate::core::Fn {
    let ret_ty = entry_fn.ret_ty.clone();
    crate::core::Fn {
        name: ENTRY_FUNCTION.to_string(),
        root: true,
        generics: Vec::new(),
        trait_impl: None,
        params: Vec::new(),
        ret_ty: ret_ty.clone(),
        body: crate::core::Block {
            stmts: Vec::new(),
            tail: Some(Box::new(crate::core::Expr::ECall {
                func: Box::new(crate::core::Expr::EVar {
                    name: entry_fn.name.clone(),
                    ty: crate::tast::Ty::TFunc {
                        params: Vec::new(),
                        ret_ty: Box::new(ret_ty.clone()),
                    },
                }),
                args: Vec::new(),
                ty: ret_ty,
            })),
        },
    }
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
