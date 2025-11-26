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
    go::{self, compile::GlobalGoEnv, goast},
    lift::{self, GlobalLiftEnv},
    mono::{self, GlobalMonoEnv},
    tast, typer,
};

#[derive(Debug)]
pub struct Compilation {
    pub green_node: GreenNode,
    pub cst: CstFile,
    pub ast: ast::File,
    pub tast: tast::File,
    pub genv: GlobalTypeEnv,
    pub liftenv: GlobalLiftEnv,
    pub monoenv: GlobalMonoEnv,
    pub anfenv: GlobalAnfEnv,
    pub goenv: GlobalGoEnv,
    pub core: crate::core::File,
    pub lambda: crate::core::File,
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

    let ast = match derive::expand(ast) {
        Ok(ast) => ast,
        Err(diagnostics) => {
            return Err(CompilationError::Lower { diagnostics });
        }
    };

    let (tast, genv, mut diagnostics) = typer::check_file(ast.clone());
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
    let (lifted_core, liftenv) = lift::lambda_lift(genv.clone(), &gensym, core.clone());
    let (mono, monoenv) = mono::mono(liftenv.clone(), lifted_core.clone());
    let (anf, anfenv) = anf::anf_file(monoenv.clone(), &gensym, mono.clone());
    let (go, goenv) = go::compile::go_file(anfenv.clone(), &gensym, anf.clone());

    Ok(Compilation {
        green_node,
        cst,
        ast,
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
