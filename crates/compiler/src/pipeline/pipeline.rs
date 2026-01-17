use std::collections::HashMap;
use std::path::Path;

use ast::ast;
use cst::cst::{CstNode, File as CstFile};
use diagnostics::{Diagnostic, Diagnostics, Severity, Stage};
use parser::{self, syntax::MySyntaxNode};
use rowan::GreenNode;

use crate::pipeline::compile_error;
use crate::pipeline::packages;
use crate::{
    anf::{self, GlobalAnfEnv},
    compile_match, derive,
    env::{Gensym, GlobalTypeEnv, TraitEnv, TypeEnv, ValueEnv},
    fir,
    go::{self, compile::GlobalGoEnv, goast},
    lift::{self, GlobalLiftEnv, LiftFile},
    mono::{self, GlobalMonoEnv},
    tast, typer,
};

#[derive(Debug)]
pub struct Compilation {
    pub green_node: GreenNode,
    pub cst: CstFile,
    pub ast: ast::File,
    pub fir: fir::ProjectFir,
    pub fir_table: fir::ProjectFirTable,
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
struct PackageExports {
    type_env: TypeEnv,
    trait_env: TraitEnv,
    value_env: ValueEnv,
}

impl PackageExports {
    fn apply_to(&self, genv: &mut GlobalTypeEnv) {
        for (name, def) in self.type_env.enums.iter() {
            genv.type_env.enums.insert(name.clone(), def.clone());
        }
        for (name, def) in self.type_env.structs.iter() {
            genv.type_env.structs.insert(name.clone(), def.clone());
        }
        for (name, def) in self.type_env.extern_types.iter() {
            genv.type_env.extern_types.insert(name.clone(), def.clone());
        }
        for (name, def) in self.trait_env.trait_defs.iter() {
            genv.trait_env.trait_defs.insert(name.clone(), def.clone());
        }
        for (key, def) in self.trait_env.trait_impls.iter() {
            genv.trait_env.trait_impls.insert(key.clone(), def.clone());
        }
        for (key, def) in self.trait_env.inherent_impls.iter() {
            genv.trait_env
                .inherent_impls
                .insert(key.clone(), def.clone());
        }
        for (name, scheme) in self.value_env.funcs.iter() {
            genv.value_env.funcs.insert(name.clone(), scheme.clone());
        }
        for (name, func) in self.value_env.extern_funcs.iter() {
            genv.value_env
                .extern_funcs
                .insert(name.clone(), func.clone());
        }
    }

    fn to_genv(&self) -> GlobalTypeEnv {
        GlobalTypeEnv {
            type_env: self.type_env.clone(),
            trait_env: self.trait_env.clone(),
            value_env: self.value_env.clone(),
        }
    }
}

#[derive(Debug, Clone)]
struct PackageInterface {
    exports: PackageExports,
    fir_interface: fir::PackageInterface,
}

fn build_package<'a>(
    gensym: &Gensym,
    diagnostics: &mut Diagnostics,
    package: &'a PackageArtifact,
    deps: &[&PackageInterface],
) -> (&'a PackageInterface, crate::core::File) {
    let mut env = GlobalTypeEnv::new();
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

pub fn parse_ast_file(path: &Path, src: &str) -> Result<ast::File, CompilationError> {
    let (_green, _cst, ast) = parse_ast_from_source(path, src)?;
    Ok(ast)
}

fn typecheck_package(
    package_id: fir::PackageId,
    package: &packages::PackageUnit,
    deps_envs: HashMap<String, GlobalTypeEnv>,
    deps_interfaces: &HashMap<String, fir::PackageInterface>,
) -> PackageArtifact {
    let (fir, fir_table) =
        fir::lower_to_fir_files_with_env(package_id, package.files.clone(), deps_interfaces);
    let fir_interface = fir::PackageInterface::from_fir(&fir, &fir_table);
    let (tast, genv, diagnostics) = typer::check_file_with_env(
        fir,
        fir_table,
        GlobalTypeEnv::new(),
        &package.name,
        deps_envs,
    );
    let exports = PackageExports {
        type_env: genv.type_env.clone(),
        trait_env: genv.trait_env.clone(),
        value_env: genv.value_env.clone(),
    };

    PackageArtifact {
        tast,
        interface: PackageInterface {
            exports,
            fir_interface,
        },
        diagnostics,
    }
}

fn typecheck_packages(
    path: &Path,
    entry_ast: ast::File,
) -> Result<TypecheckPackagesResult, CompilationError> {
    let root_dir = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    let graph = packages::discover_packages(root_dir, Some(path), Some(entry_ast))?;
    let order = packages::topo_sort_packages(&graph)?;

    let mut diagnostics = Diagnostics::new();
    let mut genv = GlobalTypeEnv::new();
    let mut artifacts_by_name: HashMap<String, PackageArtifact> = HashMap::new();
    let mut package_names: Vec<String> = graph.packages.keys().cloned().collect();
    package_names.sort();
    let mut package_ids = HashMap::new();
    package_ids.insert("Builtin".to_string(), fir::PackageId(0));
    package_ids.insert("Main".to_string(), fir::PackageId(1));
    let mut next_id = 2u32;
    for name in package_names {
        if name == "Builtin" || name == "Main" {
            continue;
        }
        package_ids.insert(name, fir::PackageId(next_id));
        next_id += 1;
    }

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
            deps_interfaces.insert(dep.clone(), artifact.interface.fir_interface.clone());
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
    })
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

    let (fir, fir_table) = fir::lower_to_project_fir_files(all_files);

    let tast = full_tast;
    if diagnostics.has_errors() {
        return Err(CompilationError::Typer {
            diagnostics: diagnostics.clone(),
        });
    }

    let gensym = Gensym::new();

    let mut package_cores = Vec::new();
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
        fir,
        fir_table,
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
