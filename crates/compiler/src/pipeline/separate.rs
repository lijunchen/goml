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
use crate::pipeline::compile_error;
use crate::pipeline::pipeline::{CompilationError, parse_ast_file};
use diagnostics::Diagnostics;

pub struct PackageInputs {
    pub package: String,
    pub input_files: Vec<PathBuf>,
    pub interface_paths: Vec<PathBuf>,
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

fn load_interface_from_paths(
    package: &str,
    interface_paths: &[PathBuf],
) -> Result<InterfaceUnit, CompilationError> {
    for dir in interface_paths {
        let candidate = dir.join(format!("{}.interface", package));
        if !candidate.exists() {
            continue;
        }
        let json = fs::read_to_string(&candidate).map_err(|err| {
            compile_error(format!(
                "failed to read interface {}: {}",
                candidate.display(),
                err
            ))
        })?;
        let unit: InterfaceUnit = serde_json::from_str(&json).map_err(|err| {
            compile_error(format!(
                "failed to parse interface {}: {}",
                candidate.display(),
                err
            ))
        })?;
        if unit.package != package {
            return Err(compile_error(format!(
                "interface {} declares package {}, expected {}",
                candidate.display(),
                unit.package,
                package
            )));
        }
        if !unit.validate_hash() {
            return Err(compile_error(format!(
                "interface {} has invalid interface_hash",
                candidate.display()
            )));
        }
        if unit.package != "Builtin" {
            let expected = builtins::builtin_interface_hash();
            let Some(actual) = unit.deps.get("Builtin") else {
                return Err(compile_error(format!(
                    "interface {} is missing implicit Builtin dependency (rebuild {})",
                    candidate.display(),
                    unit.package
                )));
            };
            if actual != &expected {
                return Err(compile_error(format!(
                    "interface {} expects Builtin interface_hash {}, but compiler has {} (rebuild {})",
                    candidate.display(),
                    actual,
                    expected,
                    unit.package
                )));
            }
        }
        return Ok(unit);
    }

    Err(compile_error(format!(
        "missing interface for package {} (searched: {})",
        package,
        interface_paths
            .iter()
            .map(|p| p.display().to_string())
            .collect::<Vec<_>>()
            .join(", ")
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

    let mut deps: Vec<String> = imports.into_iter().collect();
    deps.sort();
    deps.dedup();

    let mut deps_envs = HashMap::new();
    let mut deps_interfaces = HashMap::new();
    let mut dep_hashes = BTreeMap::new();

    if opts.package != "Builtin" {
        dep_hashes.insert("Builtin".to_string(), builtins::builtin_interface_hash());
    }

    for dep in deps {
        if dep == "Builtin" || dep == opts.package {
            continue;
        }
        let unit = load_interface_from_paths(&dep, &opts.interface_paths)?;
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

    let mut deps: Vec<String> = imports.into_iter().collect();
    deps.sort();
    deps.dedup();

    let mut deps_envs = HashMap::new();
    let mut deps_interfaces = HashMap::new();
    let mut dep_hashes = BTreeMap::new();
    let mut dep_units = Vec::new();

    if opts.package != "Builtin" {
        dep_hashes.insert("Builtin".to_string(), builtins::builtin_interface_hash());
    }

    for dep in deps {
        if dep == "Builtin" || dep == opts.package {
            continue;
        }
        let unit = load_interface_from_paths(&dep, &opts.interface_paths)?;
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

    let Some((main_package, main)) = by_name
        .get_key_value("main")
        .or_else(|| by_name.get_key_value("Main"))
    else {
        return Err(compile_error("missing main package core".to_string()));
    };
    if !main.core_ir.toplevels.iter().any(|f| f.name == "main") {
        return Err(compile_error(format!(
            "{} package missing main function",
            main_package
        )));
    }

    let builtin_hash = builtins::builtin_interface_hash();
    for (pkg, unit) in by_name.iter() {
        for (dep, expected_hash) in unit.deps.iter() {
            if dep == "Builtin" {
                if expected_hash != &builtin_hash {
                    return Err(compile_error(format!(
                        "package {} expects Builtin interface_hash {}, but compiler has {} (rebuild {})",
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
