use std::path::Path;

use ast::ast;
use cst::cst::{CstNode, File as CstFile};
use diagnostics::Diagnostics;
use parser::{self, syntax::MySyntaxNode};
use rowan::GreenNode;

use crate::{
    anf, compile_match,
    env::{Gensym, GlobalEnv},
    go::{self, goast},
    lambda_lift, mono, tast, typer,
};

#[derive(Debug)]
pub struct Compilation {
    pub green_node: GreenNode,
    pub cst: CstFile,
    pub ast: ast::File,
    pub tast: tast::File,
    pub typer_env: GlobalEnv,
    pub env: GlobalEnv,
    pub core: crate::core::File,
    pub lambda: crate::core::File,
    pub mono: crate::core::File,
    pub anf: anf::File,
    pub go: goast::File,
}

#[derive(Debug, Clone)]
pub enum CompilationError {
    Parser { diagnostics: Diagnostics },
    Lower { diagnostics: Diagnostics },
    Typer { diagnostics: Diagnostics },
}

impl CompilationError {
    pub fn diagnostics(&self) -> &Diagnostics {
        match self {
            CompilationError::Parser { diagnostics }
            | CompilationError::Lower { diagnostics }
            | CompilationError::Typer { diagnostics } => diagnostics,
        }
    }

    pub fn into_diagnostics(self) -> Diagnostics {
        match self {
            CompilationError::Parser { diagnostics }
            | CompilationError::Lower { diagnostics }
            | CompilationError::Typer { diagnostics } => diagnostics,
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
    let lower = ::ast::lower::lower(cst.clone());
    let ast = match lower.into_result() {
        Ok(ast) => ast,
        Err(diagnostics) => {
            return Err(CompilationError::Lower { diagnostics });
        }
    };

    let (tast, mut env) = typer::check_file(ast.clone());
    if env.diagnostics.has_errors() {
        return Err(CompilationError::Typer {
            diagnostics: env.diagnostics.clone(),
        });
    }

    let typer_env = env.clone();

    let gensym = Gensym::new();

    let core = compile_match::compile_file(&env, &gensym, &tast);
    let lifted_core = lambda_lift::lambda_lift(&mut env, &gensym, core.clone());
    let mono = mono::mono(&mut env, lifted_core.clone());
    let anf = anf::anf_file(&env, &gensym, mono.clone());
    let go = go::compile::go_file(&env, &gensym, anf.clone());

    Ok(Compilation {
        green_node,
        cst,
        ast,
        tast,
        typer_env,
        env,
        core,
        lambda: lifted_core,
        mono,
        anf,
        go,
    })
}
