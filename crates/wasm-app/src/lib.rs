use cst::cst::CstNode;
use diagnostics::Diagnostics;
use parser::{parser::ParseResult, syntax::MySyntaxNode};

use compiler::env::format_typer_diagnostics;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn execute(src: &str) -> String {
    let result = parser::parse(&std::path::PathBuf::from("dummy"), src);
    if result.has_errors() {
        return format_parse_errors(&result, src);
    }
    let root = MySyntaxNode::new_root(result.green_node);
    let cst = cst::cst::File::cast(root).unwrap();
    let lower = ast::lower::lower(cst);
    let ast = match lower.into_result() {
        Ok(ast) => ast,
        Err(diagnostics) => return format_lower_errors(diagnostics),
    };

    let (tast, env) = compiler::typer::check_file(ast);
    let typer_errors = format_typer_diagnostics(&env.diagnostics);
    if !typer_errors.is_empty() {
        return typer_errors
            .into_iter()
            .map(|err| format!("error: {}", err))
            .collect::<Vec<_>>()
            .join("\n");
    }
    let core = compiler::compile_match::compile_file(&env, &tast);
    let _ = core;
    "not support for now".into()
}

#[wasm_bindgen]
pub fn compile_to_core(src: &str) -> String {
    let result = parser::parse(&std::path::PathBuf::from("dummy"), src);
    if result.has_errors() {
        return format_parse_errors(&result, src);
    }
    let root = MySyntaxNode::new_root(result.green_node);
    let cst = cst::cst::File::cast(root).unwrap();
    let lower = ast::lower::lower(cst);
    let ast = match lower.into_result() {
        Ok(ast) => ast,
        Err(diagnostics) => return format_lower_errors(diagnostics),
    };

    let (tast, env) = compiler::typer::check_file(ast);
    let typer_errors = format_typer_diagnostics(&env.diagnostics);
    if !typer_errors.is_empty() {
        return typer_errors
            .into_iter()
            .map(|err| format!("error: {}", err))
            .collect::<Vec<_>>()
            .join("\n");
    }
    let core = compiler::compile_match::compile_file(&env, &tast);

    core.to_pretty(&env, 120)
}

#[wasm_bindgen]
pub fn compile_to_mono(src: &str) -> String {
    let result = parser::parse(&std::path::PathBuf::from("dummy"), src);
    if result.has_errors() {
        return format_parse_errors(&result, src);
    }
    let root = MySyntaxNode::new_root(result.green_node);
    let cst = cst::cst::File::cast(root).unwrap();
    let lower = ast::lower::lower(cst);
    let ast = match lower.into_result() {
        Ok(ast) => ast,
        Err(diagnostics) => return format_lower_errors(diagnostics),
    };

    let (tast, mut env) = compiler::typer::check_file(ast);
    let typer_errors = format_typer_diagnostics(&env.diagnostics);
    if !typer_errors.is_empty() {
        return typer_errors
            .into_iter()
            .map(|err| format!("error: {}", err))
            .collect::<Vec<_>>()
            .join("\n");
    }
    let core = compiler::compile_match::compile_file(&env, &tast);

    let mono = compiler::mono::mono(&mut env, core);
    mono.to_pretty(&env, 120)
}

#[wasm_bindgen]
pub fn compile_to_anf(src: &str) -> String {
    let result = parser::parse(&std::path::PathBuf::from("dummy"), src);
    if result.has_errors() {
        return format_parse_errors(&result, src);
    }
    let root = MySyntaxNode::new_root(result.green_node);
    let cst = cst::cst::File::cast(root).unwrap();
    let lower = ast::lower::lower(cst);
    let ast = match lower.into_result() {
        Ok(ast) => ast,
        Err(diagnostics) => return format_lower_errors(diagnostics),
    };

    let (tast, mut env) = compiler::typer::check_file(ast);
    let typer_errors = format_typer_diagnostics(&env.diagnostics);
    if !typer_errors.is_empty() {
        return typer_errors
            .into_iter()
            .map(|err| format!("error: {}", err))
            .collect::<Vec<_>>()
            .join("\n");
    }
    let core = compiler::compile_match::compile_file(&env, &tast);

    let mono = compiler::mono::mono(&mut env, core);

    let anf = compiler::anf::anf_file(&env, mono);
    anf.to_pretty(&env, 120)
}

#[wasm_bindgen]
pub fn compile_to_go(src: &str) -> String {
    let result = parser::parse(&std::path::PathBuf::from("dummy"), src);
    if result.has_errors() {
        return format_parse_errors(&result, src);
    }
    let root = MySyntaxNode::new_root(result.green_node);
    let cst = cst::cst::File::cast(root).unwrap();
    let lower = ast::lower::lower(cst);
    let ast = match lower.into_result() {
        Ok(ast) => ast,
        Err(diagnostics) => return format_lower_errors(diagnostics),
    };

    let (tast, mut env) = compiler::typer::check_file(ast);
    let typer_errors = format_typer_diagnostics(&env.diagnostics);
    if !typer_errors.is_empty() {
        return typer_errors
            .into_iter()
            .map(|err| format!("error: {}", err))
            .collect::<Vec<_>>()
            .join("\n");
    }
    let core = compiler::compile_match::compile_file(&env, &tast);

    let mono = compiler::mono::mono(&mut env, core);

    let anf = compiler::anf::anf_file(&env, mono);

    let go = compiler::go::compile::go_file(&env, anf);
    go.to_pretty(&env, 120)
}

#[wasm_bindgen]
pub fn get_cst(src: &str) -> String {
    let result = parser::parse(&std::path::PathBuf::from("dummy"), src);
    if result.has_errors() {
        return format_parse_errors(&result, src);
    }
    parser::debug_tree(&result.green_node)
}

#[wasm_bindgen]
pub fn get_ast(src: &str) -> String {
    let result = parser::parse(&std::path::PathBuf::from("dummy"), src);
    if result.has_errors() {
        return format_parse_errors(&result, src);
    }
    let root = MySyntaxNode::new_root(result.green_node);
    let cst = cst::cst::File::cast(root).unwrap();
    let lower = ast::lower::lower(cst);
    let ast = match lower.into_result() {
        Ok(ast) => ast,
        Err(diagnostics) => return format_lower_errors(diagnostics),
    };
    format!("{:#?}", ast)
}

#[wasm_bindgen]
pub fn get_tast(src: &str) -> String {
    let result = parser::parse(&std::path::PathBuf::from("dummy"), src);
    if result.has_errors() {
        return format_parse_errors(&result, src);
    }
    let root = MySyntaxNode::new_root(result.green_node);
    let cst = cst::cst::File::cast(root).unwrap();
    let lower = ast::lower::lower(cst);
    let ast = match lower.into_result() {
        Ok(ast) => ast,
        Err(diagnostics) => return format_lower_errors(diagnostics),
    };

    let (tast, env) = compiler::typer::check_file(ast);
    let typer_errors = format_typer_diagnostics(&env.diagnostics);
    if !typer_errors.is_empty() {
        return typer_errors
            .into_iter()
            .map(|err| format!("error: {}", err))
            .collect::<Vec<_>>()
            .join("\n");
    }
    tast.to_pretty(&env, 120)
}

#[wasm_bindgen]
pub fn hover(src: &str, line: u32, col: u32) -> Option<String> {
    compiler::query::hover_type(src, line, col)
}

fn format_parse_errors(result: &ParseResult, src: &str) -> String {
    result
        .format_errors(src)
        .into_iter()
        .map(|err| format!("error: {}", err))
        .collect::<Vec<_>>()
        .join("\n")
}

fn format_lower_errors(diagnostics: Diagnostics) -> String {
    diagnostics
        .into_iter()
        .map(|diagnostic| {
            format!(
                "error ({}): {}",
                diagnostic.stage().as_str(),
                diagnostic.message()
            )
        })
        .collect::<Vec<_>>()
        .join("\n")
}
