use std::path::Path;

use ast::ast;
use cst::cst::{CstNode, File as CstFile};
use diagnostics::Diagnostics;
use parser::{self, syntax::MySyntaxNode};
use rowan::GreenNode;

use crate::{
    anf, compile_match,
    env::Env,
    go::{self, goast},
    mono, tast, typer,
};

#[derive(Debug)]
pub struct Compilation {
    pub green_node: GreenNode,
    pub cst: CstFile,
    pub ast: ast::File,
    pub tast: tast::File,
    pub typer_env: Env,
    pub env: Env,
    pub core: crate::core::File,
    pub mono: crate::core::File,
    pub anf: anf::File,
    pub go: goast::File,
}

#[derive(Debug, Clone)]
pub enum CompilationError {
    Parser { diagnostics: Diagnostics },
    Typer { diagnostics: Diagnostics },
}

impl CompilationError {
    pub fn diagnostics(&self) -> &Diagnostics {
        match self {
            CompilationError::Parser { diagnostics } | CompilationError::Typer { diagnostics } => {
                diagnostics
            }
        }
    }

    pub fn into_diagnostics(self) -> Diagnostics {
        match self {
            CompilationError::Parser { diagnostics } | CompilationError::Typer { diagnostics } => {
                diagnostics
            }
        }
    }
}

pub fn compile(path: &Path, src: &str) -> Result<Compilation, CompilationError> {
    let parse_result = parser::parse(path, src);
    if parse_result.has_errors() {
        return Err(CompilationError::Parser {
            diagnostics: parse_result.into_diagnostics(),
        });
    }

    let green_node = parse_result.green_node.clone();
    let root = MySyntaxNode::new_root(parse_result.green_node);
    let cst = CstFile::cast(root).expect("failed to cast CST file");
    let ast = ::ast::lower::lower(cst.clone()).expect("lowering produced no AST");

    let (tast, mut env) = typer::check_file(ast.clone());
    if env.diagnostics.has_errors() {
        return Err(CompilationError::Typer {
            diagnostics: env.diagnostics.clone(),
        });
    }

    let typer_env = env.clone();

    let core = compile_match::compile_file(&env, &tast);
    let mono = mono::mono(&mut env, core.clone());
    let anf = anf::anf_file(&env, mono.clone());
    let go = go::compile::go_file(&env, anf.clone());

    Ok(Compilation {
        green_node,
        cst,
        ast,
        tast,
        typer_env,
        env,
        core,
        mono,
        anf,
        go,
    })
}
