use std::collections::{BTreeMap, HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use ast::ast;

use crate::config::{find_crate_root, load_crate_manifest};
use crate::hir::SourceFileAst;
use crate::package_imports::ExternalImports;
use crate::package_names::ROOT_PACKAGE;
use crate::pipeline::compile_error;
use crate::pipeline::modules::CrateUnit;
use crate::pipeline::pipeline::{CompilationError, parse_ast_file};

#[derive(Debug)]
pub struct PackageUnit {
    pub name: String,
    pub files: Vec<SourceFileAst>,
    pub imports: HashSet<String>,
}

#[derive(Debug)]
pub struct PackageGraph {
    pub module_dir: PathBuf,
    pub module_name: Option<String>,
    pub entry_package: String,
    pub packages: HashMap<String, PackageUnit>,
    pub discovery_order: Vec<String>,
    pub package_dirs: HashMap<String, PathBuf>,
    pub package_visibilities: HashMap<String, ast::Visibility>,
    pub external_root_packages: HashSet<String>,
}

fn read_gom_sources(dir: &Path) -> Result<Vec<PathBuf>, CompilationError> {
    let mut files = Vec::new();
    let entries = fs::read_dir(dir).map_err(|err| {
        compile_error(format!(
            "failed to read namespace directory {}: {}",
            dir.display(),
            err
        ))
    })?;

    for entry in entries {
        let entry = entry.map_err(|err| {
            compile_error(format!(
                "failed to read namespace directory {}: {}",
                dir.display(),
                err
            ))
        })?;
        let path = entry.path();
        if path.extension().is_some_and(|ext| ext == "gom") {
            files.push(path);
        }
    }

    files.sort();
    Ok(files)
}

fn namespace_dir_is_loadable(dir: &Path) -> bool {
    dir.is_dir()
}

fn namespace_file_is_loadable(path: &Path) -> bool {
    path.is_file()
}

fn namespace_dir_for_file(path: &Path) -> PathBuf {
    if path.file_name().is_some_and(|name| name == "mod.gom") {
        path.parent()
            .filter(|parent| !parent.as_os_str().is_empty())
            .unwrap_or_else(|| Path::new("."))
            .to_path_buf()
    } else {
        path.to_path_buf()
    }
}

fn import_from_use_decl(path: &ast::Path, external_imports: &ExternalImports) -> Option<String> {
    if let Some(alias) = external_imports.alias_for_use_path(path) {
        return Some(alias);
    }
    path.segments().first().map(|seg| seg.ident.0.clone())
}

fn collect_imports(files: &[SourceFileAst], external_imports: &ExternalImports) -> HashSet<String> {
    files
        .iter()
        .flat_map(|file| {
            let from_use_decls = file
                .ast
                .uses
                .iter()
                .filter_map(|decl| import_from_use_decl(&decl.path, external_imports));
            let from_mods = file.ast.toplevels.iter().filter_map(|item| match item {
                ast::Item::Mod(module) => Some(module.name.0.clone()),
                _ => None,
            });
            from_use_decls.chain(from_mods)
        })
        .collect()
}

pub(crate) fn collect_known_crate_path_imports(
    files: &[SourceFileAst],
    known_packages: &HashSet<String>,
) -> HashSet<String> {
    let mut imports = HashSet::new();
    for file in files {
        imports.extend(collect_known_crate_path_imports_from_ast(
            &file.ast,
            known_packages,
        ));
    }
    imports
}

pub(crate) fn collect_known_crate_path_imports_from_ast(
    file: &ast::File,
    known_packages: &HashSet<String>,
) -> HashSet<String> {
    let mut imports = HashSet::new();
    for use_decl in file.uses.iter() {
        collect_path_crate_import(&use_decl.path, known_packages, &mut imports);
    }
    for item in file.toplevels.iter() {
        collect_item_crate_path_imports(item, known_packages, &mut imports);
    }
    imports
}

fn collect_item_crate_path_imports(
    item: &ast::Item,
    known_packages: &HashSet<String>,
    imports: &mut HashSet<String>,
) {
    match item {
        ast::Item::Mod(_) | ast::Item::ExternType(_) => {}
        ast::Item::EnumDef(def) => {
            for (_, tys) in def.variants.iter() {
                for ty in tys {
                    collect_type_crate_path_imports(ty, known_packages, imports);
                }
            }
        }
        ast::Item::StructDef(def) => {
            for (_, ty) in def.fields.iter() {
                collect_type_crate_path_imports(ty, known_packages, imports);
            }
        }
        ast::Item::TraitDef(def) => {
            for sig in def.method_sigs.iter() {
                for ty in sig.params.iter() {
                    collect_type_crate_path_imports(ty, known_packages, imports);
                }
                collect_type_crate_path_imports(&sig.ret_ty, known_packages, imports);
            }
        }
        ast::Item::ImplBlock(def) => {
            for (_, bounds) in def.generic_bounds.iter() {
                for bound in bounds {
                    collect_path_crate_import(bound, known_packages, imports);
                }
            }
            if let Some(trait_name) = &def.trait_name {
                collect_path_crate_import(trait_name, known_packages, imports);
            }
            collect_type_crate_path_imports(&def.for_type, known_packages, imports);
            for method in def.methods.iter() {
                collect_fn_crate_path_imports(method, known_packages, imports);
            }
        }
        ast::Item::Fn(def) => collect_fn_crate_path_imports(def, known_packages, imports),
        ast::Item::ExternGo(def) => {
            for (_, ty) in def.params.iter() {
                collect_type_crate_path_imports(ty, known_packages, imports);
            }
            if let Some(ty) = &def.ret_ty {
                collect_type_crate_path_imports(ty, known_packages, imports);
            }
        }
        ast::Item::ExternBuiltin(def) => {
            for (_, bounds) in def.generic_bounds.iter() {
                for bound in bounds {
                    collect_path_crate_import(bound, known_packages, imports);
                }
            }
            for (_, ty) in def.params.iter() {
                collect_type_crate_path_imports(ty, known_packages, imports);
            }
            if let Some(ty) = &def.ret_ty {
                collect_type_crate_path_imports(ty, known_packages, imports);
            }
        }
    }
}

fn collect_fn_crate_path_imports(
    def: &ast::Fn,
    known_packages: &HashSet<String>,
    imports: &mut HashSet<String>,
) {
    for (_, bounds) in def.generic_bounds.iter() {
        for bound in bounds {
            collect_path_crate_import(bound, known_packages, imports);
        }
    }
    for (_, ty) in def.params.iter() {
        collect_type_crate_path_imports(ty, known_packages, imports);
    }
    if let Some(ty) = &def.ret_ty {
        collect_type_crate_path_imports(ty, known_packages, imports);
    }
    collect_block_crate_path_imports(&def.body, known_packages, imports);
}

fn collect_type_crate_path_imports(
    ty: &ast::TypeExpr,
    known_packages: &HashSet<String>,
    imports: &mut HashSet<String>,
) {
    match ty {
        ast::TypeExpr::TUnit
        | ast::TypeExpr::TBool
        | ast::TypeExpr::TInt8
        | ast::TypeExpr::TInt16
        | ast::TypeExpr::TInt32
        | ast::TypeExpr::TInt64
        | ast::TypeExpr::TUint8
        | ast::TypeExpr::TUint16
        | ast::TypeExpr::TUint32
        | ast::TypeExpr::TUint64
        | ast::TypeExpr::TFloat32
        | ast::TypeExpr::TFloat64
        | ast::TypeExpr::TString
        | ast::TypeExpr::TChar => {}
        ast::TypeExpr::TTuple { typs } => {
            for ty in typs {
                collect_type_crate_path_imports(ty, known_packages, imports);
            }
        }
        ast::TypeExpr::TCon { path } | ast::TypeExpr::TDyn { trait_path: path } => {
            collect_path_crate_import(path, known_packages, imports);
        }
        ast::TypeExpr::TApp { ty, args } => {
            collect_type_crate_path_imports(ty, known_packages, imports);
            for arg in args {
                collect_type_crate_path_imports(arg, known_packages, imports);
            }
        }
        ast::TypeExpr::TArray { elem, .. } => {
            collect_type_crate_path_imports(elem, known_packages, imports);
        }
        ast::TypeExpr::TFunc { params, ret_ty } => {
            for param in params {
                collect_type_crate_path_imports(param, known_packages, imports);
            }
            collect_type_crate_path_imports(ret_ty, known_packages, imports);
        }
    }
}

fn collect_block_crate_path_imports(
    block: &ast::Block,
    known_packages: &HashSet<String>,
    imports: &mut HashSet<String>,
) {
    for stmt in block.stmts.iter() {
        match stmt {
            ast::Stmt::Let(stmt) => {
                collect_pat_crate_path_imports(&stmt.pat, known_packages, imports);
                if let Some(ty) = &stmt.annotation {
                    collect_type_crate_path_imports(ty, known_packages, imports);
                }
                collect_expr_crate_path_imports(&stmt.value, known_packages, imports);
            }
            ast::Stmt::Assign(stmt) => {
                collect_expr_crate_path_imports(&stmt.target, known_packages, imports);
                collect_expr_crate_path_imports(&stmt.value, known_packages, imports);
            }
            ast::Stmt::Expr(stmt) => {
                collect_expr_crate_path_imports(&stmt.expr, known_packages, imports);
            }
        }
    }
    if let Some(tail) = &block.tail {
        collect_expr_crate_path_imports(tail, known_packages, imports);
    }
}

fn collect_expr_crate_path_imports(
    expr: &ast::Expr,
    known_packages: &HashSet<String>,
    imports: &mut HashSet<String>,
) {
    match expr {
        ast::Expr::EPath { path, .. } => collect_path_crate_import(path, known_packages, imports),
        ast::Expr::EConstr {
            constructor, args, ..
        } => {
            collect_path_crate_import(constructor, known_packages, imports);
            for arg in args {
                collect_expr_crate_path_imports(arg, known_packages, imports);
            }
        }
        ast::Expr::EStructLiteral { name, fields, .. } => {
            collect_path_crate_import(name, known_packages, imports);
            for (_, expr) in fields {
                collect_expr_crate_path_imports(expr, known_packages, imports);
            }
        }
        ast::Expr::ETuple { items, .. } | ast::Expr::EArray { items, .. } => {
            for item in items {
                collect_expr_crate_path_imports(item, known_packages, imports);
            }
        }
        ast::Expr::EClosure { params, body, .. } => {
            for param in params {
                if let Some(ty) = &param.ty {
                    collect_type_crate_path_imports(ty, known_packages, imports);
                }
            }
            collect_expr_crate_path_imports(body, known_packages, imports);
        }
        ast::Expr::EMatch { expr, arms, .. } => {
            collect_expr_crate_path_imports(expr, known_packages, imports);
            for arm in arms {
                collect_pat_crate_path_imports(&arm.pat, known_packages, imports);
                collect_expr_crate_path_imports(&arm.body, known_packages, imports);
            }
        }
        ast::Expr::EIf {
            cond,
            then_branch,
            else_branch,
            ..
        } => {
            collect_expr_crate_path_imports(cond, known_packages, imports);
            collect_expr_crate_path_imports(then_branch, known_packages, imports);
            collect_expr_crate_path_imports(else_branch, known_packages, imports);
        }
        ast::Expr::EWhile { cond, body, .. } => {
            collect_expr_crate_path_imports(cond, known_packages, imports);
            collect_expr_crate_path_imports(body, known_packages, imports);
        }
        ast::Expr::EReturn { expr, .. } => {
            if let Some(expr) = expr {
                collect_expr_crate_path_imports(expr, known_packages, imports);
            }
        }
        ast::Expr::EGo { expr, .. }
        | ast::Expr::EUnary { expr, .. }
        | ast::Expr::ETry { expr, .. } => {
            collect_expr_crate_path_imports(expr, known_packages, imports);
        }
        ast::Expr::ECall { func, args, .. } => {
            collect_expr_crate_path_imports(func, known_packages, imports);
            for arg in args {
                collect_expr_crate_path_imports(arg, known_packages, imports);
            }
        }
        ast::Expr::EBinary { lhs, rhs, .. } => {
            collect_expr_crate_path_imports(lhs, known_packages, imports);
            collect_expr_crate_path_imports(rhs, known_packages, imports);
        }
        ast::Expr::EProj { tuple, .. } => {
            collect_expr_crate_path_imports(tuple, known_packages, imports);
        }
        ast::Expr::EField { expr, .. } => {
            collect_expr_crate_path_imports(expr, known_packages, imports);
        }
        ast::Expr::EIndex { base, index, .. } => {
            collect_expr_crate_path_imports(base, known_packages, imports);
            collect_expr_crate_path_imports(index, known_packages, imports);
        }
        ast::Expr::EBlock { block, .. } => {
            collect_block_crate_path_imports(block, known_packages, imports);
        }
        ast::Expr::EUnit { .. }
        | ast::Expr::EBool { .. }
        | ast::Expr::EInt { .. }
        | ast::Expr::EInt8 { .. }
        | ast::Expr::EInt16 { .. }
        | ast::Expr::EInt32 { .. }
        | ast::Expr::EInt64 { .. }
        | ast::Expr::EUInt8 { .. }
        | ast::Expr::EUInt16 { .. }
        | ast::Expr::EUInt32 { .. }
        | ast::Expr::EUInt64 { .. }
        | ast::Expr::EFloat { .. }
        | ast::Expr::EFloat32 { .. }
        | ast::Expr::EFloat64 { .. }
        | ast::Expr::EString { .. }
        | ast::Expr::EChar { .. }
        | ast::Expr::EBreak { .. }
        | ast::Expr::EContinue { .. } => {}
    }
}

fn collect_pat_crate_path_imports(
    pat: &ast::Pat,
    known_packages: &HashSet<String>,
    imports: &mut HashSet<String>,
) {
    match pat {
        ast::Pat::PConstr {
            constructor, args, ..
        } => {
            collect_path_crate_import(constructor, known_packages, imports);
            for arg in args {
                collect_pat_crate_path_imports(arg, known_packages, imports);
            }
        }
        ast::Pat::PStruct { name, fields, .. } => {
            collect_path_crate_import(name, known_packages, imports);
            for (_, pat) in fields {
                collect_pat_crate_path_imports(pat, known_packages, imports);
            }
        }
        ast::Pat::PTuple { pats, .. } => {
            for pat in pats {
                collect_pat_crate_path_imports(pat, known_packages, imports);
            }
        }
        ast::Pat::PVar { .. }
        | ast::Pat::PUnit { .. }
        | ast::Pat::PBool { .. }
        | ast::Pat::PInt { .. }
        | ast::Pat::PInt8 { .. }
        | ast::Pat::PInt16 { .. }
        | ast::Pat::PInt32 { .. }
        | ast::Pat::PInt64 { .. }
        | ast::Pat::PUInt8 { .. }
        | ast::Pat::PUInt16 { .. }
        | ast::Pat::PUInt32 { .. }
        | ast::Pat::PUInt64 { .. }
        | ast::Pat::PFloat { .. }
        | ast::Pat::PFloat32 { .. }
        | ast::Pat::PFloat64 { .. }
        | ast::Pat::PString { .. }
        | ast::Pat::PChar { .. }
        | ast::Pat::PWild { .. } => {}
    }
}

fn collect_path_crate_import(
    path: &ast::Path,
    known_packages: &HashSet<String>,
    imports: &mut HashSet<String>,
) {
    if !matches!(path.root(), ast::PathRoot::Crate) {
        return;
    }
    if let Some(package) = longest_known_prefix(path.segments(), known_packages) {
        imports.insert(package);
    }
}

fn longest_known_prefix(
    segments: &[ast::PathSegment],
    known_packages: &HashSet<String>,
) -> Option<String> {
    if segments.len() < 2 {
        return None;
    }
    (1..segments.len()).rev().find_map(|len| {
        let package = segments[..len]
            .iter()
            .map(|segment| segment.ident.0.clone())
            .collect::<Vec<_>>()
            .join("::");
        known_packages.contains(&package).then_some(package)
    })
}

fn source_override_for_dir<'a>(
    package_dir: &Path,
    source_override: Option<(&'a Path, &'a ast::File)>,
) -> Option<(&'a Path, &'a ast::File)> {
    source_override.filter(|(path, _)| {
        path.parent()
            .map(|parent| {
                if parent.as_os_str().is_empty() {
                    Path::new(".")
                } else {
                    parent
                }
            })
            .is_some_and(|parent| parent == package_dir)
    })
}

fn derive_ast(ast: ast::File) -> Result<ast::File, CompilationError> {
    crate::derive::expand(ast).map_err(|diagnostics| CompilationError::Lower { diagnostics })
}

fn load_package_file(
    package_file: &Path,
    package_name: &str,
    source_override: Option<(&Path, &ast::File)>,
    external_imports: &ExternalImports,
) -> Result<PackageUnit, CompilationError> {
    let ast = if let Some((override_path, override_ast)) = source_override
        && override_path == package_file
    {
        override_ast.clone()
    } else {
        let src = fs::read_to_string(package_file).map_err(|err| {
            compile_error(format!(
                "failed to read {}: {}",
                package_file.display(),
                err
            ))
        })?;
        parse_ast_file(package_file, &src)?
    };
    let files = vec![SourceFileAst::with_package(
        package_file.to_path_buf(),
        package_name,
        ast,
    )];
    let imports = collect_imports(&files, external_imports);
    Ok(PackageUnit {
        name: package_name.to_string(),
        files,
        imports,
    })
}

fn load_package(
    package_dir: &Path,
    expected_package_name: Option<&str>,
    source_override: Option<(&Path, &ast::File)>,
    external_imports: &ExternalImports,
) -> Result<PackageUnit, CompilationError> {
    let mut files = Vec::new();
    let package_name = expected_package_name
        .map(str::to_string)
        .unwrap_or_else(|| ROOT_PACKAGE.to_string());

    let source_override = source_override_for_dir(package_dir, source_override);

    if let Some((path, ast)) = source_override {
        files.push(SourceFileAst::with_package(
            path.to_path_buf(),
            package_name.clone(),
            ast.clone(),
        ));
    }

    for path in read_gom_sources(package_dir)? {
        if source_override.is_some_and(|(override_path, _)| override_path == path.as_path()) {
            continue;
        }
        let src = fs::read_to_string(&path)
            .map_err(|err| compile_error(format!("failed to read {}: {}", path.display(), err)))?;
        let ast = parse_ast_file(&path, &src)?;
        files.push(SourceFileAst::with_package(path, package_name.clone(), ast));
    }

    if files.is_empty() {
        return Err(compile_error(format!(
            "namespace directory {} has no .gom files",
            package_dir.display()
        )));
    }

    let imports = collect_imports(&files, external_imports);
    Ok(PackageUnit {
        name: package_name,
        files,
        imports,
    })
}

fn load_single_file_package(
    path: &Path,
    ast: &ast::File,
    external_imports: &ExternalImports,
) -> PackageUnit {
    let files = vec![SourceFileAst::with_package(
        path.to_path_buf(),
        ROOT_PACKAGE,
        ast.clone(),
    )];
    let imports = collect_imports(&files, external_imports);
    PackageUnit {
        name: ROOT_PACKAGE.to_string(),
        files,
        imports,
    }
}

fn module_package_name(module_path: &[String], root_package: &str) -> String {
    if module_path.is_empty() {
        root_package.to_string()
    } else {
        module_path.join("::")
    }
}

fn discover_packages_from_crate_unit(
    crate_unit: CrateUnit,
    root_package: &str,
    entry_path: Option<&Path>,
    entry_ast: Option<ast::File>,
    external_imports: &ExternalImports,
) -> Result<PackageGraph, CompilationError> {
    let source_override = match (entry_path, entry_ast.as_ref()) {
        (Some(path), Some(ast)) => Some((path, ast)),
        _ => None,
    };

    let mut packages = HashMap::new();
    let mut discovery_order = Vec::new();
    let mut package_dirs = HashMap::new();
    let mut package_visibilities = HashMap::new();

    for module in crate_unit.modules.iter() {
        let name = module_package_name(module.path.segments(), root_package);
        let ast = if let Some((override_path, override_ast)) = source_override {
            if override_path == module.file_path.as_path() {
                override_ast.clone()
            } else {
                module.ast.clone()
            }
        } else {
            module.ast.clone()
        };
        let ast = derive_ast(ast)?;
        let files = vec![SourceFileAst::with_package_and_module_path(
            module.file_path.clone(),
            name.clone(),
            module.path.segments().to_vec(),
            ast,
        )];
        let mut imports = collect_imports(&files, external_imports);
        for (child_name, child_id) in module.children.iter() {
            if !module.path.segments().is_empty() {
                imports.remove(child_name);
            }
            let child = crate_unit
                .modules
                .get(child_id.0)
                .ok_or_else(|| compile_error(format!("module child {} not found", child_name)))?;
            imports.insert(module_package_name(child.path.segments(), root_package));
        }
        packages.insert(
            name.clone(),
            PackageUnit {
                name: name.clone(),
                files,
                imports,
            },
        );
        discovery_order.push(name.clone());
        package_dirs.insert(name.clone(), namespace_dir_for_file(&module.file_path));
        package_visibilities.insert(name, module.visibility);
    }

    let known_packages = packages.keys().cloned().collect::<HashSet<_>>();
    for (name, package) in packages.iter_mut() {
        let imports = collect_known_crate_path_imports(&package.files, &known_packages);
        package
            .imports
            .extend(imports.into_iter().filter(|dep| dep != name));
    }

    Ok(PackageGraph {
        module_dir: crate_unit.root_dir,
        module_name: Some(crate_unit.config.name),
        entry_package: root_package.to_string(),
        packages,
        discovery_order,
        package_dirs,
        package_visibilities,
        external_root_packages: HashSet::new(),
    })
}

pub fn discover_dependency_versions_from_file(
    file_path: &Path,
) -> Result<(PathBuf, BTreeMap<String, String>), CompilationError> {
    let start_dir = file_path
        .parent()
        .filter(|p| !p.as_os_str().is_empty())
        .unwrap_or(Path::new("."));

    if let Some((crate_dir, _)) = find_crate_root(start_dir) {
        let manifest = load_crate_manifest(&crate_dir.join("goml.toml")).map_err(compile_error)?;
        return Ok((crate_dir, manifest.dependency_versions()));
    }

    Ok((start_dir.to_path_buf(), BTreeMap::new()))
}

pub fn discover_packages(
    root_dir: &Path,
    entry_path: Option<&Path>,
    entry_ast: Option<ast::File>,
) -> Result<PackageGraph, CompilationError> {
    discover_packages_with_external_imports(
        root_dir,
        entry_path,
        entry_ast,
        &ExternalImports::default(),
    )
}

pub fn discover_packages_with_external_imports(
    root_dir: &Path,
    entry_path: Option<&Path>,
    entry_ast: Option<ast::File>,
    external_imports: &ExternalImports,
) -> Result<PackageGraph, CompilationError> {
    discover_packages_inner(root_dir, entry_path, entry_ast, false, external_imports)
}

pub fn discover_packages_single_file(
    root_dir: &Path,
    entry_path: &Path,
    entry_ast: ast::File,
) -> Result<PackageGraph, CompilationError> {
    discover_packages_single_file_with_external_imports(
        root_dir,
        entry_path,
        entry_ast,
        &ExternalImports::default(),
    )
}

pub fn discover_packages_single_file_with_external_imports(
    root_dir: &Path,
    entry_path: &Path,
    entry_ast: ast::File,
    external_imports: &ExternalImports,
) -> Result<PackageGraph, CompilationError> {
    discover_packages_inner(
        root_dir,
        Some(entry_path),
        Some(entry_ast),
        true,
        external_imports,
    )
}

pub fn discover_dependency_crate_packages_with_external_imports(
    module_dir: &Path,
    root_package: &str,
    external_imports: &ExternalImports,
) -> Result<PackageGraph, CompilationError> {
    let crate_unit = crate::pipeline::modules::discover_crate_from_dir(module_dir)
        .map_err(|err| compile_error(format!("crate module discovery failed: {:?}", err)))?;
    discover_packages_from_crate_unit(crate_unit, root_package, None, None, external_imports)
}

fn discover_packages_inner(
    root_dir: &Path,
    entry_path: Option<&Path>,
    entry_ast: Option<ast::File>,
    single_file: bool,
    external_imports: &ExternalImports,
) -> Result<PackageGraph, CompilationError> {
    if root_dir.join("goml.toml").exists() {
        match crate::pipeline::modules::discover_crate_from_dir(root_dir) {
            Ok(crate_unit) => {
                return discover_packages_from_crate_unit(
                    crate_unit,
                    ROOT_PACKAGE,
                    entry_path,
                    entry_ast,
                    external_imports,
                );
            }
            Err(err) => {
                return Err(compile_error(format!(
                    "crate module discovery failed: {:?}",
                    err
                )));
            }
        }
    }

    let source_override = match (entry_path, entry_ast.as_ref()) {
        (Some(path), Some(ast)) => Some((path, ast)),
        _ => None,
    };
    let entry_package = if single_file {
        if let Some((path, ast)) = source_override {
            load_single_file_package(path, ast, external_imports)
        } else {
            load_package(
                root_dir,
                None,
                source_override_for_dir(root_dir, source_override),
                external_imports,
            )?
        }
    } else {
        load_package(
            root_dir,
            None,
            source_override_for_dir(root_dir, source_override),
            external_imports,
        )?
    };
    let entry_name = entry_package.name.clone();

    let mut packages = HashMap::new();
    let mut discovery_order = Vec::new();
    let mut package_dirs = HashMap::new();
    let mut package_visibilities = HashMap::new();
    let mut queue: Vec<String> = entry_package.imports.iter().cloned().collect();
    let mut loaded = HashSet::new();

    loaded.insert(entry_name.clone());
    packages.insert(entry_name.clone(), entry_package);
    discovery_order.push(entry_name.clone());
    package_dirs.insert(entry_name.clone(), root_dir.to_path_buf());
    package_visibilities.insert(entry_name.clone(), ast::Visibility::Public);

    while let Some(package_name) = queue.pop() {
        if loaded.contains(&package_name) {
            continue;
        }
        if external_imports.contains_package(&package_name) {
            loaded.insert(package_name);
            continue;
        }
        let package_dir = root_dir.join(&package_name);
        let package_file = root_dir.join(format!("{package_name}.gom"));
        if !namespace_dir_is_loadable(&package_dir) && !namespace_file_is_loadable(&package_file) {
            loaded.insert(package_name);
            continue;
        }
        let package_override = source_override_for_dir(&package_dir, source_override);
        let package = if namespace_dir_is_loadable(&package_dir) {
            load_package(
                &package_dir,
                Some(&package_name),
                package_override,
                external_imports,
            )?
        } else {
            load_package_file(
                &package_file,
                &package_name,
                source_override,
                external_imports,
            )?
        };
        let declared_name = package.name.clone();
        if package.name != package_name {
            return Err(compile_error(format!(
                "namespace directory {} declares namespace {}, expected {}",
                package_dir.display(),
                package.name,
                package_name
            )));
        }
        queue.extend(package.imports.iter().cloned());
        loaded.insert(declared_name.clone());
        packages.insert(declared_name.clone(), package);
        discovery_order.push(declared_name.clone());
        package_dirs.insert(declared_name, package_dir);
        package_visibilities.insert(package_name, ast::Visibility::Public);
    }

    Ok(PackageGraph {
        module_dir: root_dir.to_path_buf(),
        module_name: None,
        entry_package: entry_name,
        packages,
        discovery_order,
        package_dirs,
        package_visibilities,
        external_root_packages: HashSet::new(),
    })
}

pub fn topo_sort_packages(graph: &PackageGraph) -> Result<Vec<String>, CompilationError> {
    let mut temp = HashSet::new();
    let mut perm = HashSet::new();
    let mut order = Vec::new();
    let mut stack = Vec::new();

    let mut names: Vec<String> = graph.packages.keys().cloned().collect();
    names.sort();

    for name in names {
        if perm.contains(&name) {
            continue;
        }
        visit_package(&name, graph, &mut temp, &mut perm, &mut stack, &mut order)?;
    }

    Ok(order)
}

fn visit_package(
    name: &str,
    graph: &PackageGraph,
    temp: &mut HashSet<String>,
    perm: &mut HashSet<String>,
    stack: &mut Vec<String>,
    order: &mut Vec<String>,
) -> Result<(), CompilationError> {
    if perm.contains(name) {
        return Ok(());
    }
    if temp.contains(name) {
        let mut cycle = Vec::new();
        if let Some(pos) = stack.iter().position(|n| n == name) {
            cycle.extend_from_slice(&stack[pos..]);
        }
        cycle.push(name.to_string());
        let display = cycle
            .iter()
            .map(|pkg| {
                let dir = graph
                    .package_dirs
                    .get(pkg)
                    .map(|dir| dir.display().to_string())
                    .unwrap_or_else(|| graph.module_dir.join(pkg).display().to_string());
                format!("{} ({})", pkg, dir)
            })
            .collect::<Vec<_>>()
            .join(" -> ");
        return Err(compile_error(format!(
            "namespace dependency cycle detected: {}",
            display
        )));
    }

    temp.insert(name.to_string());
    stack.push(name.to_string());

    let Some(package) = graph.packages.get(name) else {
        return Err(compile_error(format!(
            "namespace {} not found during dependency walk",
            name
        )));
    };
    let mut deps: Vec<String> = package.imports.iter().cloned().collect();
    deps.sort();

    for dep in deps {
        if !graph.packages.contains_key(&dep) && !graph.external_root_packages.contains(&dep) {
            return Err(compile_error(format!(
                "namespace {} imports missing namespace {} in {}",
                name,
                dep,
                graph.module_dir.join(&dep).display()
            )));
        }
        if graph.external_root_packages.contains(&dep) {
            continue;
        }
        visit_package(&dep, graph, temp, perm, stack, order)?;
    }

    stack.pop();
    temp.remove(name);
    perm.insert(name.to_string());
    order.push(name.to_string());
    Ok(())
}

impl PackageGraph {
    pub fn namespace_is_publicly_visible(&self, namespace: &str) -> bool {
        if namespace == self.entry_package {
            return true;
        }

        let mut prefix = String::new();
        for segment in namespace.split("::") {
            if prefix.is_empty() {
                prefix.push_str(segment);
            } else {
                prefix.push_str("::");
                prefix.push_str(segment);
            }
            if !matches!(
                self.package_visibilities.get(&prefix),
                Some(ast::Visibility::Public)
            ) {
                return false;
            }
        }
        true
    }

    pub fn add_external_root_namespace(&mut self, namespace: impl Into<String>) {
        self.external_root_packages.insert(namespace.into());
    }

    pub fn add_namespace_dir(&mut self, namespace: impl Into<String>, dir: PathBuf) {
        self.package_dirs.insert(namespace.into(), dir);
    }

    pub fn set_namespace_visibility(
        &mut self,
        namespace: impl Into<String>,
        visibility: ast::Visibility,
    ) {
        self.package_visibilities
            .insert(namespace.into(), visibility);
    }

    pub fn add_external_namespace_dir(&mut self, namespace: impl Into<String>, dir: PathBuf) {
        self.add_namespace_dir(namespace, dir);
    }

    pub fn namespace_dir(&self, namespace: &str) -> Option<&PathBuf> {
        self.package_dirs.get(namespace)
    }
}
