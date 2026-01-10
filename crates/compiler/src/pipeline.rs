use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::path::Path;
use std::sync::{Mutex, OnceLock};

use ast::ast;
use cst::cst::{CstNode, File as CstFile};
use diagnostics::Diagnostics;
use indexmap::IndexMap;
use parser::{self, syntax::MySyntaxNode};
use rowan::GreenNode;

use crate::{
    anf::{self, GlobalAnfEnv},
    compile_match, derive,
    env::{self, FnOrigin, FnScheme, Gensym, GlobalTypeEnv, TraitEnv, TypeEnv, ValueEnv},
    fir,
    go::{self, compile::GlobalGoEnv, goast},
    lift::{self, GlobalLiftEnv, LiftFile},
    mono::{self, GlobalMonoEnv},
    tast,
    typer::{self, name_resolution::FirTable},
};

mod packages;

#[derive(Debug)]
pub struct Compilation {
    pub green_node: GreenNode,
    pub cst: CstFile,
    pub ast: ast::File,
    pub fir: fir::File,
    pub fir_table: FirTable,
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
    fn from_delta(base: &GlobalTypeEnv, full: &GlobalTypeEnv) -> Self {
        let mut type_env = TypeEnv::new();
        for (name, def) in full.type_env.enums.iter() {
            if !base.type_env.enums.contains_key(name) {
                type_env.enums.insert(name.clone(), def.clone());
            }
        }
        for (name, def) in full.type_env.structs.iter() {
            if !base.type_env.structs.contains_key(name) {
                type_env.structs.insert(name.clone(), def.clone());
            }
        }
        for (name, def) in full.type_env.extern_types.iter() {
            if !base.type_env.extern_types.contains_key(name) {
                type_env.extern_types.insert(name.clone(), def.clone());
            }
        }

        let mut trait_env = TraitEnv {
            trait_defs: IndexMap::new(),
            trait_impls: IndexMap::new(),
            inherent_impls: IndexMap::new(),
        };
        for (name, def) in full.trait_env.trait_defs.iter() {
            if !base.trait_env.trait_defs.contains_key(name) {
                trait_env.trait_defs.insert(name.clone(), def.clone());
            }
        }
        for (key, def) in full.trait_env.trait_impls.iter() {
            if !base.trait_env.trait_impls.contains_key(key) {
                trait_env.trait_impls.insert(key.clone(), def.clone());
            }
        }
        for (key, def) in full.trait_env.inherent_impls.iter() {
            if !base.trait_env.inherent_impls.contains_key(key) {
                trait_env.inherent_impls.insert(key.clone(), def.clone());
            }
        }

        let mut value_env = ValueEnv::new();
        for (name, scheme) in full.value_env.funcs.iter() {
            if !base.value_env.funcs.contains_key(name) {
                value_env.funcs.insert(name.clone(), scheme.clone());
            }
        }
        for (name, func) in full.value_env.extern_funcs.iter() {
            if !base.value_env.extern_funcs.contains_key(name) {
                value_env.extern_funcs.insert(name.clone(), func.clone());
            }
        }

        Self {
            type_env,
            trait_env,
            value_env,
        }
    }

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

    fn hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        hash_type_env(&self.type_env, &mut hasher);
        hash_trait_env(&self.trait_env, &mut hasher);
        hash_value_env(&self.value_env, &mut hasher);
        hasher.finish()
    }
}

#[derive(Debug, Clone)]
struct PackageArtifact {
    tast: tast::File,
    exports: PackageExports,
    exports_hash: u64,
    ctor_names: HashSet<String>,
    diagnostics: Diagnostics,
}

#[derive(Debug, Clone)]
struct PackageCacheEntry {
    fingerprint: u64,
    artifact: PackageArtifact,
}

#[derive(Debug, Default)]
struct PackageCache {
    entries: HashMap<String, PackageCacheEntry>,
}

impl PackageCache {
    fn get(&self, name: &str, fingerprint: u64) -> Option<PackageArtifact> {
        self.entries
            .get(name)
            .filter(|entry| entry.fingerprint == fingerprint)
            .map(|entry| entry.artifact.clone())
    }

    fn insert(&mut self, name: String, fingerprint: u64, artifact: PackageArtifact) {
        self.entries.insert(
            name,
            PackageCacheEntry {
                fingerprint,
                artifact,
            },
        );
    }
}

static PACKAGE_CACHE: OnceLock<Mutex<PackageCache>> = OnceLock::new();

#[derive(Debug)]
struct TypecheckPackagesResult {
    entry_tast: tast::File,
    full_tast: tast::File,
    genv: GlobalTypeEnv,
    diagnostics: Diagnostics,
    graph: packages::PackageGraph,
}

fn compile_error(message: String) -> CompilationError {
    let mut diagnostics = Diagnostics::new();
    diagnostics.push(diagnostics::Diagnostic::new(
        diagnostics::Stage::other("compile"),
        diagnostics::Severity::Error,
        message,
    ));
    CompilationError::Compile { diagnostics }
}

fn hash_fn_scheme(scheme: &FnScheme, hasher: &mut DefaultHasher) {
    scheme.type_params.hash(hasher);
    scheme.ty.hash(hasher);
    let origin = match scheme.origin {
        FnOrigin::User => 0u8,
        FnOrigin::Builtin => 1u8,
        FnOrigin::Compiler => 2u8,
    };
    origin.hash(hasher);
}

fn hash_impl_def(def: &env::ImplDef, hasher: &mut DefaultHasher) {
    def.params.hash(hasher);
    let mut keys: Vec<_> = def.methods.keys().collect();
    keys.sort();
    for key in keys {
        key.hash(hasher);
        let scheme = def.methods.get(key).expect("missing impl method");
        hash_fn_scheme(scheme, hasher);
    }
}

fn hash_trait_env(env: &TraitEnv, hasher: &mut DefaultHasher) {
    let mut trait_names: Vec<_> = env.trait_defs.keys().collect();
    trait_names.sort();
    for name in trait_names {
        name.hash(hasher);
        let def = env.trait_defs.get(name).expect("missing trait def");
        let mut method_names: Vec<_> = def.methods.keys().collect();
        method_names.sort();
        for method in method_names {
            method.hash(hasher);
            let scheme = def.methods.get(method).expect("missing trait method");
            hash_fn_scheme(scheme, hasher);
        }
    }

    let mut impl_keys: Vec<_> = env.trait_impls.keys().collect();
    impl_keys.sort();
    for key in impl_keys {
        key.hash(hasher);
        let def = env.trait_impls.get(key).expect("missing trait impl");
        hash_impl_def(def, hasher);
    }

    let mut inherent_keys: Vec<_> = env.inherent_impls.keys().collect();
    inherent_keys.sort();
    for key in inherent_keys {
        key.hash(hasher);
        let def = env.inherent_impls.get(key).expect("missing inherent impl");
        hash_impl_def(def, hasher);
    }
}

fn hash_type_env(env: &TypeEnv, hasher: &mut DefaultHasher) {
    let mut enum_names: Vec<_> = env.enums.keys().collect();
    enum_names.sort_by(|a, b| a.0.cmp(&b.0));
    for name in enum_names {
        name.hash(hasher);
        let def = env.enums.get(name).expect("missing enum def");
        def.name.hash(hasher);
        def.generics.hash(hasher);
        for (variant, tys) in def.variants.iter() {
            variant.hash(hasher);
            tys.hash(hasher);
        }
    }

    let mut struct_names: Vec<_> = env.structs.keys().collect();
    struct_names.sort_by(|a, b| a.0.cmp(&b.0));
    for name in struct_names {
        name.hash(hasher);
        let def = env.structs.get(name).expect("missing struct def");
        def.name.hash(hasher);
        def.generics.hash(hasher);
        for (field, ty) in def.fields.iter() {
            field.hash(hasher);
            ty.hash(hasher);
        }
    }

    let mut extern_names: Vec<_> = env.extern_types.keys().collect();
    extern_names.sort();
    for name in extern_names {
        name.hash(hasher);
        let def = env.extern_types.get(name).expect("missing extern type");
        def.go_name.hash(hasher);
        def.package_path.hash(hasher);
    }
}

fn hash_value_env(env: &ValueEnv, hasher: &mut DefaultHasher) {
    let mut func_names: Vec<_> = env.funcs.keys().collect();
    func_names.sort();
    for name in func_names {
        name.hash(hasher);
        let scheme = env.funcs.get(name).expect("missing func");
        hash_fn_scheme(scheme, hasher);
    }

    let mut extern_names: Vec<_> = env.extern_funcs.keys().collect();
    extern_names.sort();
    for name in extern_names {
        name.hash(hasher);
        let func = env.extern_funcs.get(name).expect("missing extern func");
        func.package_path.hash(hasher);
        func.go_name.hash(hasher);
        func.ty.hash(hasher);
    }
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

fn parse_ast_file(path: &Path, src: &str) -> Result<ast::File, CompilationError> {
    let (_green, _cst, ast) = parse_ast_from_source(path, src)?;
    Ok(ast)
}

fn collect_constructor_names(
    files: &[ast::File],
) -> (HashSet<String>, HashMap<String, HashSet<String>>) {
    let mut global = HashSet::new();
    let mut per_package: HashMap<String, HashSet<String>> = HashMap::new();
    for file in files {
        let entry = per_package.entry(file.package.0.clone()).or_default();
        for item in &file.toplevels {
            if let ast::Item::EnumDef(def) = item {
                for (variant, _) in &def.variants {
                    global.insert(variant.0.clone());
                    entry.insert(variant.0.clone());
                }
            }
        }
    }
    (global, per_package)
}

fn collect_constructor_names_for_files(files: &[ast::File]) -> HashSet<String> {
    let mut names = HashSet::new();
    for file in files {
        for item in &file.toplevels {
            if let ast::Item::EnumDef(def) = item {
                for (variant, _) in &def.variants {
                    names.insert(variant.0.clone());
                }
            }
        }
    }
    names
}

fn promote_constructors_in_file(
    file: ast::File,
    local_ctor_names: &HashSet<String>,
    global_ctor_names: &HashSet<String>,
) -> ast::File {
    let toplevels = file
        .toplevels
        .into_iter()
        .map(|item| promote_constructors_in_item(item, local_ctor_names, global_ctor_names))
        .collect();
    ast::File {
        package: file.package,
        imports: file.imports,
        toplevels,
    }
}

fn promote_constructors_in_item(
    item: ast::Item,
    local_ctor_names: &HashSet<String>,
    global_ctor_names: &HashSet<String>,
) -> ast::Item {
    match item {
        ast::Item::Fn(mut func) => {
            func.body =
                promote_constructors_in_expr(func.body, local_ctor_names, global_ctor_names);
            ast::Item::Fn(func)
        }
        ast::Item::ImplBlock(mut block) => {
            block.methods = block
                .methods
                .into_iter()
                .map(|mut method| {
                    method.body = promote_constructors_in_expr(
                        method.body,
                        local_ctor_names,
                        global_ctor_names,
                    );
                    method
                })
                .collect();
            ast::Item::ImplBlock(block)
        }
        other => other,
    }
}

fn promote_constructors_in_expr(
    expr: ast::Expr,
    local_ctor_names: &HashSet<String>,
    global_ctor_names: &HashSet<String>,
) -> ast::Expr {
    match expr {
        ast::Expr::EPath { path, astptr } => {
            let is_ctor = if let Some(ident) = path.last_ident() {
                if local_ctor_names.contains(&ident.0) {
                    true
                } else {
                    path.len() >= 3 && global_ctor_names.contains(&ident.0)
                }
            } else {
                false
            };
            if is_ctor {
                ast::Expr::EConstr {
                    constructor: path,
                    args: Vec::new(),
                }
            } else {
                ast::Expr::EPath { path, astptr }
            }
        }
        ast::Expr::ECall { func, args } => {
            let func = promote_constructors_in_expr(*func, local_ctor_names, global_ctor_names);
            let args = args
                .into_iter()
                .map(|arg| promote_constructors_in_expr(arg, local_ctor_names, global_ctor_names))
                .collect::<Vec<_>>();
            if let ast::Expr::EPath { path, .. } = &func {
                let is_ctor = if let Some(ident) = path.last_ident() {
                    if local_ctor_names.contains(&ident.0) {
                        true
                    } else {
                        path.len() >= 3 && global_ctor_names.contains(&ident.0)
                    }
                } else {
                    false
                };
                if is_ctor {
                    ast::Expr::EConstr {
                        constructor: path.clone(),
                        args,
                    }
                } else {
                    ast::Expr::ECall {
                        func: Box::new(func),
                        args,
                    }
                }
            } else {
                ast::Expr::ECall {
                    func: Box::new(func),
                    args,
                }
            }
        }
        ast::Expr::EConstr { constructor, args } => ast::Expr::EConstr {
            constructor,
            args: args
                .into_iter()
                .map(|arg| promote_constructors_in_expr(arg, local_ctor_names, global_ctor_names))
                .collect(),
        },
        ast::Expr::EArray { items } => ast::Expr::EArray {
            items: items
                .into_iter()
                .map(|item| promote_constructors_in_expr(item, local_ctor_names, global_ctor_names))
                .collect(),
        },
        ast::Expr::ETuple { items } => ast::Expr::ETuple {
            items: items
                .into_iter()
                .map(|item| promote_constructors_in_expr(item, local_ctor_names, global_ctor_names))
                .collect(),
        },
        ast::Expr::EClosure { params, body } => ast::Expr::EClosure {
            params,
            body: Box::new(promote_constructors_in_expr(
                *body,
                local_ctor_names,
                global_ctor_names,
            )),
        },
        ast::Expr::ELet {
            pat,
            annotation,
            value,
        } => ast::Expr::ELet {
            pat,
            annotation,
            value: Box::new(promote_constructors_in_expr(
                *value,
                local_ctor_names,
                global_ctor_names,
            )),
        },
        ast::Expr::EMatch { expr, arms, astptr } => ast::Expr::EMatch {
            expr: Box::new(promote_constructors_in_expr(
                *expr,
                local_ctor_names,
                global_ctor_names,
            )),
            arms: arms
                .into_iter()
                .map(|arm| ast::Arm {
                    pat: arm.pat,
                    body: promote_constructors_in_expr(
                        arm.body,
                        local_ctor_names,
                        global_ctor_names,
                    ),
                })
                .collect(),
            astptr,
        },
        ast::Expr::EIf {
            cond,
            then_branch,
            else_branch,
        } => ast::Expr::EIf {
            cond: Box::new(promote_constructors_in_expr(
                *cond,
                local_ctor_names,
                global_ctor_names,
            )),
            then_branch: Box::new(promote_constructors_in_expr(
                *then_branch,
                local_ctor_names,
                global_ctor_names,
            )),
            else_branch: Box::new(promote_constructors_in_expr(
                *else_branch,
                local_ctor_names,
                global_ctor_names,
            )),
        },
        ast::Expr::EWhile { cond, body } => ast::Expr::EWhile {
            cond: Box::new(promote_constructors_in_expr(
                *cond,
                local_ctor_names,
                global_ctor_names,
            )),
            body: Box::new(promote_constructors_in_expr(
                *body,
                local_ctor_names,
                global_ctor_names,
            )),
        },
        ast::Expr::EGo { expr } => ast::Expr::EGo {
            expr: Box::new(promote_constructors_in_expr(
                *expr,
                local_ctor_names,
                global_ctor_names,
            )),
        },
        ast::Expr::EUnary { op, expr } => ast::Expr::EUnary {
            op,
            expr: Box::new(promote_constructors_in_expr(
                *expr,
                local_ctor_names,
                global_ctor_names,
            )),
        },
        ast::Expr::EBinary { op, lhs, rhs } => ast::Expr::EBinary {
            op,
            lhs: Box::new(promote_constructors_in_expr(
                *lhs,
                local_ctor_names,
                global_ctor_names,
            )),
            rhs: Box::new(promote_constructors_in_expr(
                *rhs,
                local_ctor_names,
                global_ctor_names,
            )),
        },
        ast::Expr::EProj { tuple, index } => ast::Expr::EProj {
            tuple: Box::new(promote_constructors_in_expr(
                *tuple,
                local_ctor_names,
                global_ctor_names,
            )),
            index,
        },
        ast::Expr::EField {
            expr,
            field,
            astptr,
        } => ast::Expr::EField {
            expr: Box::new(promote_constructors_in_expr(
                *expr,
                local_ctor_names,
                global_ctor_names,
            )),
            field,
            astptr,
        },
        ast::Expr::EBlock { exprs } => ast::Expr::EBlock {
            exprs: exprs
                .into_iter()
                .map(|expr| promote_constructors_in_expr(expr, local_ctor_names, global_ctor_names))
                .collect(),
        },
        other => other,
    }
}

fn typecheck_package(
    package: &packages::PackageUnit,
    base_env: &GlobalTypeEnv,
    deps_ctor_names: &HashSet<String>,
) -> PackageArtifact {
    let local_ctor_names = collect_constructor_names_for_files(&package.files);
    let mut global_ctor_names = deps_ctor_names.clone();
    global_ctor_names.extend(local_ctor_names.iter().cloned());
    let files = package
        .files
        .clone()
        .into_iter()
        .map(|file| promote_constructors_in_file(file, &local_ctor_names, &global_ctor_names))
        .collect();
    let (fir, fir_table) = fir::lower_to_fir_files(files);
    let (tast, genv, diagnostics) = typer::check_file_with_env(fir, fir_table, base_env.clone());
    let exports = PackageExports::from_delta(base_env, &genv);
    let exports_hash = exports.hash();

    PackageArtifact {
        tast,
        exports,
        exports_hash,
        ctor_names: local_ctor_names,
        diagnostics,
    }
}

fn compute_source_hashes(
    graph: &packages::PackageGraph,
    entry_path: &Path,
    entry_src: &str,
) -> Result<HashMap<String, u64>, CompilationError> {
    let mut hashes = HashMap::new();
    for name in graph.packages.keys() {
        let hash = packages::package_hash(graph, name, Some(entry_path), Some(entry_src))?;
        hashes.insert(name.clone(), hash);
    }
    Ok(hashes)
}

fn compute_dependency_sets(
    graph: &packages::PackageGraph,
) -> Result<HashMap<String, HashSet<String>>, CompilationError> {
    fn collect_deps(
        name: &str,
        graph: &packages::PackageGraph,
        memo: &mut HashMap<String, HashSet<String>>,
        visiting: &mut HashSet<String>,
    ) -> Result<HashSet<String>, CompilationError> {
        if let Some(existing) = memo.get(name) {
            return Ok(existing.clone());
        }
        if visiting.contains(name) {
            return Err(compile_error(format!(
                "cycle detected while collecting dependencies for {}",
                name
            )));
        }
        visiting.insert(name.to_string());
        let package = graph
            .packages
            .get(name)
            .ok_or_else(|| compile_error(format!("package {} not found", name)))?;
        let mut deps = HashSet::new();
        for dep in package.imports.iter() {
            let dep_set = collect_deps(dep, graph, memo, visiting)?;
            deps.insert(dep.clone());
            deps.extend(dep_set);
        }
        visiting.remove(name);
        memo.insert(name.to_string(), deps.clone());
        Ok(deps)
    }

    let mut memo = HashMap::new();
    let mut visiting = HashSet::new();
    for name in graph.packages.keys() {
        let _ = collect_deps(name, graph, &mut memo, &mut visiting)?;
    }
    Ok(memo)
}

fn typecheck_packages(
    path: &Path,
    src: &str,
    entry_ast: ast::File,
) -> Result<TypecheckPackagesResult, CompilationError> {
    let root_dir = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    let graph = packages::discover_packages(root_dir, Some(path), Some(entry_ast))?;
    let order = packages::topo_sort_packages(&graph)?;
    let source_hashes = compute_source_hashes(&graph, path, src)?;
    let dependency_sets = compute_dependency_sets(&graph)?;

    let mut diagnostics = Diagnostics::new();
    let mut genv = GlobalTypeEnv::new();
    let mut exports_by_name: HashMap<String, PackageExports> = HashMap::new();
    let mut ctor_names_by_package: HashMap<String, HashSet<String>> = HashMap::new();
    let mut exports_hashes: HashMap<String, u64> = HashMap::new();
    let mut artifacts_by_name: HashMap<String, PackageArtifact> = HashMap::new();
    let cache = PACKAGE_CACHE.get_or_init(|| Mutex::new(PackageCache::default()));

    for name in order.iter() {
        let package = graph
            .packages
            .get(name)
            .ok_or_else(|| compile_error(format!("package {} not found", name)))?;
        let source_hash = *source_hashes
            .get(name)
            .ok_or_else(|| compile_error(format!("hash missing for package {}", name)))?;
        let mut direct_deps: Vec<String> = package.imports.iter().cloned().collect();
        direct_deps.sort();
        let mut hasher = DefaultHasher::new();
        name.hash(&mut hasher);
        source_hash.hash(&mut hasher);
        for dep in direct_deps {
            dep.hash(&mut hasher);
            let dep_hash = exports_hashes
                .get(&dep)
                .ok_or_else(|| compile_error(format!("missing exports for package {}", dep)))?;
            dep_hash.hash(&mut hasher);
        }
        let fingerprint = hasher.finish();

        let cached = { cache.lock().unwrap().get(name, fingerprint) };
        let artifact = if let Some(artifact) = cached {
            artifact
        } else {
            let deps = dependency_sets
                .get(name)
                .ok_or_else(|| compile_error(format!("missing deps for package {}", name)))?;
            let mut base_env = GlobalTypeEnv::new();
            for dep in order.iter() {
                if deps.contains(dep) {
                    let exports = exports_by_name.get(dep).ok_or_else(|| {
                        compile_error(format!("missing exports for package {}", dep))
                    })?;
                    exports.apply_to(&mut base_env);
                }
            }

            let mut deps_ctor_names = HashSet::new();
            for dep in deps.iter() {
                let names = ctor_names_by_package.get(dep).ok_or_else(|| {
                    compile_error(format!("missing constructors for package {}", dep))
                })?;
                deps_ctor_names.extend(names.iter().cloned());
            }

            let artifact = typecheck_package(package, &base_env, &deps_ctor_names);
            cache
                .lock()
                .unwrap()
                .insert(name.clone(), fingerprint, artifact.clone());
            artifact
        };

        let mut package_diagnostics = artifact.diagnostics.clone();
        diagnostics.append(&mut package_diagnostics);
        artifact.exports.apply_to(&mut genv);
        exports_by_name.insert(name.clone(), artifact.exports.clone());
        ctor_names_by_package.insert(name.clone(), artifact.ctor_names.clone());
        exports_hashes.insert(name.clone(), artifact.exports_hash);
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
    })
}

pub fn compile(path: &Path, src: &str) -> Result<Compilation, CompilationError> {
    let (green_node, cst, entry_ast) = parse_ast_from_source(path, src)?;

    let typecheck = typecheck_packages(path, src, entry_ast.clone())?;
    let TypecheckPackagesResult {
        full_tast,
        genv,
        mut diagnostics,
        graph,
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

    let (global_ctor_names, ctor_names_by_package) = collect_constructor_names(&all_files);
    let entry_local = ctor_names_by_package
        .get(&entry_ast.package.0)
        .cloned()
        .unwrap_or_default();
    let entry_ast = promote_constructors_in_file(entry_ast, &entry_local, &global_ctor_names);
    let all_files = all_files
        .into_iter()
        .map(|file| {
            let local = ctor_names_by_package
                .get(&file.package.0)
                .cloned()
                .unwrap_or_default();
            promote_constructors_in_file(file, &local, &global_ctor_names)
        })
        .collect();
    let (fir, fir_table) = fir::lower_to_fir_files(all_files);

    let tast = full_tast;
    if diagnostics.has_errors() {
        return Err(CompilationError::Typer {
            diagnostics: diagnostics.clone(),
        });
    }

    let gensym = Gensym::new();

    let core = compile_match::compile_file(&genv, &gensym, &mut diagnostics, &tast);
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
    let result = typecheck_packages(path, src, entry_ast)?;
    Ok((result.entry_tast, result.genv, result.diagnostics))
}
