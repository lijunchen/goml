use std::collections::{HashMap, HashSet};
use std::path::Path;

use ast::ast;
use cst::cst::{CstNode, File as CstFile};
use diagnostics::Diagnostics;
use parser::{self, syntax::MySyntaxNode};
use rowan::GreenNode;

use crate::{
    anf::{self, GlobalAnfEnv},
    compile_match, derive,
    env::{Gensym, GlobalTypeEnv},
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

fn compile_error(message: String) -> CompilationError {
    let mut diagnostics = Diagnostics::new();
    diagnostics.push(diagnostics::Diagnostic::new(
        diagnostics::Stage::other("compile"),
        diagnostics::Severity::Error,
        message,
    ));
    CompilationError::Compile { diagnostics }
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

pub fn compile(path: &Path, src: &str) -> Result<Compilation, CompilationError> {
    let (green_node, cst, entry_ast) = parse_ast_from_source(path, src)?;

    let root_dir = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    let packages = packages::load_packages(root_dir, Some(path), Some(entry_ast.clone()))?;
    let all_files: Vec<ast::File> = packages
        .into_iter()
        .flat_map(|package| package.files)
        .collect();

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

    let (tast, genv, mut diagnostics) =
        typer::check_file_with_env(fir.clone(), fir_table.clone(), GlobalTypeEnv::new());
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

    let root_dir = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    let packages = packages::load_packages(root_dir, Some(path), Some(entry_ast.clone()))?;
    let all_files: Vec<ast::File> = packages
        .into_iter()
        .flat_map(|package| package.files)
        .collect();

    let (global_ctor_names, ctor_names_by_package) = collect_constructor_names(&all_files);
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

    let (tast, genv, diagnostics) =
        typer::check_file_with_env(fir, fir_table, GlobalTypeEnv::new());

    Ok((tast, genv, diagnostics))
}
