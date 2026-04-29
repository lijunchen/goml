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
use crate::package_names::is_special_unqualified_package;
use crate::package_names::{BUILTIN_PACKAGE, ENTRY_FUNCTION, ROOT_PACKAGE, is_builtin_package};
use crate::pipeline::builtin_inherent;
use crate::pipeline::packages::collect_known_crate_path_imports;
use crate::pipeline::pipeline::{CompilationError, parse_ast_file, report_duplicate_trait_impls};
use crate::pipeline::{compile_error, with_compiler_stack};
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
) -> Result<(InterfaceUnit, interface::PackageInterface), CompilationError> {
    if let Some((_, unit)) = units.get(package) {
        return Ok((unit.clone(), unit.interface.clone()));
    }

    for (_, unit) in units.values() {
        let Some(root_import_path) = external_root_import_path(unit) else {
            continue;
        };
        if !exports_contain_package(package, &unit.exports) {
            continue;
        }
        let mut package_interface =
            interface::PackageInterface::from_exports(package, &unit.exports);
        package_interface.packages =
            std::iter::once(format!("{root_import_path}::{package}")).collect();
        return Ok((unit.clone(), package_interface));
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
    interface_units: &HashMap<String, (PathBuf, InterfaceUnit)>,
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
        let mut ast = parse_ast_file(&path, &src)?;
        ast.package = ast::ast::AstIdent::new(package);
        for use_decl in ast.uses.iter() {
            if let Some(package_import) =
                external_package_import_alias(&use_decl.path, interface_units)
            {
                imports.insert(package_import);
                continue;
            }
            if let Some(first) = use_decl.path.segments().first() {
                imports.insert(first.ident.0.clone());
            }
        }
        for item in ast.toplevels.iter() {
            if let ast::ast::Item::Mod(module) = item {
                imports.insert(child_package_name(package, &module.name.0));
            }
        }
        source_list.push(path.display().to_string());
        files.push(hir::SourceFileAst::new(path, ast));
    }

    let known_packages = interface_units.keys().cloned().collect::<HashSet<_>>();
    imports.extend(collect_known_crate_path_imports(&files, &known_packages));

    Ok((files, imports, source_list))
}

type ReadSourceFilesResult = (Vec<hir::SourceFileAst>, HashSet<String>, Vec<String>);

fn child_package_name(package: &str, child: &str) -> String {
    if package == ROOT_PACKAGE {
        child.to_string()
    } else {
        format!("{package}::{child}")
    }
}

fn external_root_import_path(unit: &InterfaceUnit) -> Option<&str> {
    unit.interface.packages.iter().next().map(String::as_str)
}

fn exports_contain_package(package: &str, exports: &PackageExports) -> bool {
    exports
        .type_env
        .enums
        .keys()
        .any(|name| export_belongs_to_package(package, &name.0))
        || exports
            .type_env
            .structs
            .keys()
            .any(|name| export_belongs_to_package(package, &name.0))
        || exports
            .type_env
            .extern_types
            .keys()
            .any(|name| export_belongs_to_package(package, name))
        || exports
            .trait_env
            .trait_defs
            .keys()
            .any(|name| export_belongs_to_package(package, name))
        || exports
            .value_env
            .funcs
            .keys()
            .any(|name| export_belongs_to_package(package, name))
}

fn export_belongs_to_package(package: &str, name: &str) -> bool {
    if is_special_unqualified_package(package) {
        !name.contains("::")
    } else {
        name.starts_with(&format!("{package}::"))
    }
}

fn external_package_import_alias(
    path: &ast::ast::Path,
    interface_units: &HashMap<String, (PathBuf, InterfaceUnit)>,
) -> Option<String> {
    let display = path.display();
    let segments = path.segments();
    let first = segments.first()?.ident.0.as_str();

    for (_, unit) in interface_units.values() {
        if first == unit.package {
            let mut best = Some(unit.package.clone());
            for end in 2..=segments.len() {
                let package = segments[1..end]
                    .iter()
                    .map(|segment| segment.ident.0.clone())
                    .collect::<Vec<_>>()
                    .join("::");
                if exports_contain_package(&package, &unit.exports) {
                    best = Some(package);
                }
            }
            return best;
        }

        let Some(root_import_path) = external_root_import_path(unit) else {
            continue;
        };
        if display == root_import_path {
            return Some(unit.package.clone());
        }
        if !display.starts_with(root_import_path) {
            continue;
        }
        if !display[root_import_path.len()..].starts_with("::") {
            continue;
        }
        let root_len = root_import_path.split("::").count();
        let mut best = Some(unit.package.clone());
        for end in root_len + 1..=segments.len() {
            let package = segments[root_len..end]
                .iter()
                .map(|segment| segment.ident.0.clone())
                .collect::<Vec<_>>()
                .join("::");
            if exports_contain_package(&package, &unit.exports) {
                best = Some(package);
            }
        }
        return best;
    }

    None
}

fn typecheck_single_package(
    package: &str,
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
    let exports = PackageExports::public_from_package(package, &files, &genv);
    let pkg_interface = interface::PackageInterface::from_exports(package, &exports);
    (tast, full_exports, exports, pkg_interface, diagnostics)
}

pub fn check_package(opts: PackageInputs) -> Result<InterfaceUnit, CompilationError> {
    with_compiler_stack(|| {
        let interface_units = load_interface_files(&opts.interface_files)?;
        let (files, imports, _sources) =
            read_source_files(&opts.package, &opts.input_files, &interface_units)?;

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
            let (unit, package_interface) =
                load_interface_for_package(&dep, &opts.interface_files, &interface_units)?;
            deps_envs.insert(dep.clone(), unit.exports.to_genv());
            deps_interfaces.insert(dep.clone(), package_interface);
            dep_hashes.insert(unit.package.clone(), unit.interface_hash.clone());
        }

        let (tast, _full_exports, exports, pkg_interface, diagnostics) =
            typecheck_single_package(&opts.package, files, &deps_interfaces, deps_envs);
        drop(tast);

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
        let (files, imports, sources) =
            read_source_files(&opts.package, &opts.input_files, &interface_units)?;

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
            let (unit, package_interface) =
                load_interface_for_package(&dep, &opts.interface_files, &interface_units)?;
            deps_envs.insert(dep.clone(), unit.exports.to_genv());
            deps_interfaces.insert(dep.clone(), package_interface);
            dep_hashes.insert(unit.package.clone(), unit.interface_hash.clone());
            dep_units.push(unit);
        }

        let (tast, full_exports, exports, pkg_interface, diagnostics) =
            typecheck_single_package(&opts.package, files, &deps_interfaces, deps_envs);
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

        let mut unit = CoreUnit::new(opts.package.clone(), interface, core_ir);
        unit.sources = sources;

        Ok(unit)
    })
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
            report_duplicate_trait_impls(&mut diagnostics, &genv, &unit.interface.exports, pkg);
            unit.interface.exports.apply_to(&mut genv);
        }
        if diagnostics.has_errors() {
            return Err(CompilationError::Typer { diagnostics });
        }

        let mut linked = crate::core::File {
            toplevels: Vec::new(),
        };

        let gensym = Gensym::new();
        let mut compile_diagnostics = Diagnostics::new();
        let builtin_print_core = crate::compile_match::compile_file(
            &builtins::builtin_env(),
            &gensym,
            &mut compile_diagnostics,
            &builtins::builtin_print_tast(),
        );
        linked.toplevels.extend(builtin_print_core.toplevels);

        for pkg in order {
            let unit = by_name
                .get(&pkg)
                .ok_or_else(|| compile_error(format!("missing core for package {}", pkg)))?;
            linked.toplevels.extend(unit.core_ir.toplevels.clone());
        }

        let required_builtin_methods =
            builtin_inherent::collect_required_builtin_collection_methods(std::slice::from_ref(
                &linked,
            ));
        let builtin_collection_core = builtin_inherent::compile_builtin_collection_methods_checked(
            &required_builtin_methods,
            &gensym,
        )?;
        if !builtin_collection_core.toplevels.is_empty() {
            linked.toplevels.extend(builtin_collection_core.toplevels);
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
