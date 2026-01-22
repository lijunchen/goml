use cst::cst::CstNode;
use diagnostics::Diagnostics;
use parser::{parser::ParseResult, syntax::MySyntaxNode};

use compiler::{
    derive,
    env::{Gensym, format_compile_diagnostics, format_typer_diagnostics},
};
use wasm_bindgen::prelude::*;

fn typecheck_ast(
    ast: ::ast::ast::File,
) -> Result<
    (
        compiler::tast::File,
        compiler::env::GlobalTypeEnv,
        Diagnostics,
    ),
    String,
> {
    if !ast.imports.is_empty() {
        return Err("error: package imports are not supported in webapp".to_string());
    }
    let (hir, hir_table, mut hir_diagnostics) = compiler::hir::lower_to_hir(ast);
    let (tast, genv, mut diagnostics) = compiler::typer::check_file(hir, hir_table);
    diagnostics.append(&mut hir_diagnostics);
    Ok((tast, genv, diagnostics))
}

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

    let ast = match derive::expand(ast) {
        Ok(ast) => ast,
        Err(diagnostics) => return format_derive_errors(diagnostics),
    };

    let (tast, genv, diagnostics) = match typecheck_ast(ast) {
        Ok(result) => result,
        Err(message) => return message,
    };
    let typer_errors = format_typer_diagnostics(&diagnostics);
    if !typer_errors.is_empty() {
        return typer_errors
            .into_iter()
            .map(|err| format!("error: {}", err))
            .collect::<Vec<_>>()
            .join("\n");
    }
    let gensym = Gensym::new();
    let mut compile_diagnostics = Diagnostics::new();
    let core =
        compiler::compile_match::compile_file(&genv, &gensym, &mut compile_diagnostics, &tast);
    if compile_diagnostics.has_errors() {
        return format_compile_diagnostics(&compile_diagnostics, src)
            .into_iter()
            .map(|message| format!("error (compile): {}", message))
            .collect::<Vec<_>>()
            .join("\n");
    }
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

    let ast = match derive::expand(ast) {
        Ok(ast) => ast,
        Err(diagnostics) => return format_derive_errors(diagnostics),
    };

    let (tast, genv, diagnostics) = match typecheck_ast(ast) {
        Ok(result) => result,
        Err(message) => return message,
    };
    let typer_errors = format_typer_diagnostics(&diagnostics);
    if !typer_errors.is_empty() {
        return typer_errors
            .into_iter()
            .map(|err| format!("error: {}", err))
            .collect::<Vec<_>>()
            .join("\n");
    }
    let gensym = Gensym::new();
    let mut compile_diagnostics = Diagnostics::new();
    let core =
        compiler::compile_match::compile_file(&genv, &gensym, &mut compile_diagnostics, &tast);
    if compile_diagnostics.has_errors() {
        return format_compile_diagnostics(&compile_diagnostics, src)
            .into_iter()
            .map(|message| format!("error (compile): {}", message))
            .collect::<Vec<_>>()
            .join("\n");
    }

    core.to_pretty(&genv, 120)
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
    let ast = match derive::expand(ast) {
        Ok(ast) => ast,
        Err(diagnostics) => return format_derive_errors(diagnostics),
    };

    let (tast, genv, diagnostics) = match typecheck_ast(ast) {
        Ok(result) => result,
        Err(message) => return message,
    };
    let typer_errors = format_typer_diagnostics(&diagnostics);
    if !typer_errors.is_empty() {
        return typer_errors
            .into_iter()
            .map(|err| format!("error: {}", err))
            .collect::<Vec<_>>()
            .join("\n");
    }
    let gensym = Gensym::new();
    let mut compile_diagnostics = Diagnostics::new();
    let core =
        compiler::compile_match::compile_file(&genv, &gensym, &mut compile_diagnostics, &tast);
    if compile_diagnostics.has_errors() {
        return format_compile_diagnostics(&compile_diagnostics, src)
            .into_iter()
            .map(|message| format!("error (compile): {}", message))
            .collect::<Vec<_>>()
            .join("\n");
    }
    let (mono, monoenv) = compiler::mono::mono(genv, core);

    let (lifted, liftenv) = compiler::lift::lambda_lift(monoenv, &gensym, mono);
    lifted.to_pretty(&liftenv, 120)
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
    let ast = match derive::expand(ast) {
        Ok(ast) => ast,
        Err(diagnostics) => return format_derive_errors(diagnostics),
    };

    let (tast, genv, diagnostics) = match typecheck_ast(ast) {
        Ok(result) => result,
        Err(message) => return message,
    };
    let typer_errors = format_typer_diagnostics(&diagnostics);
    if !typer_errors.is_empty() {
        return typer_errors
            .into_iter()
            .map(|err| format!("error: {}", err))
            .collect::<Vec<_>>()
            .join("\n");
    }
    let gensym = Gensym::new();
    let mut compile_diagnostics = Diagnostics::new();
    let core =
        compiler::compile_match::compile_file(&genv, &gensym, &mut compile_diagnostics, &tast);
    if compile_diagnostics.has_errors() {
        return format_compile_diagnostics(&compile_diagnostics, src)
            .into_iter()
            .map(|message| format!("error (compile): {}", message))
            .collect::<Vec<_>>()
            .join("\n");
    }
    let (mono, monoenv) = compiler::mono::mono(genv, core);

    let (lifted, liftenv) = compiler::lift::lambda_lift(monoenv, &gensym, mono);

    let (anf, anfenv) = compiler::anf::anf_file(liftenv, &gensym, lifted);
    anf.to_pretty(&anfenv, 120)
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
    let ast = match derive::expand(ast) {
        Ok(ast) => ast,
        Err(diagnostics) => return format_derive_errors(diagnostics),
    };

    let (tast, genv, diagnostics) = match typecheck_ast(ast) {
        Ok(result) => result,
        Err(message) => return message,
    };
    let typer_errors = format_typer_diagnostics(&diagnostics);
    if !typer_errors.is_empty() {
        return typer_errors
            .into_iter()
            .map(|err| format!("error: {}", err))
            .collect::<Vec<_>>()
            .join("\n");
    }
    let gensym = Gensym::new();
    let mut compile_diagnostics = Diagnostics::new();
    let core =
        compiler::compile_match::compile_file(&genv, &gensym, &mut compile_diagnostics, &tast);
    if compile_diagnostics.has_errors() {
        return format_compile_diagnostics(&compile_diagnostics, src)
            .into_iter()
            .map(|message| format!("error (compile): {}", message))
            .collect::<Vec<_>>()
            .join("\n");
    }
    let (mono, monoenv) = compiler::mono::mono(genv, core);

    let (lifted, liftenv) = compiler::lift::lambda_lift(monoenv, &gensym, mono);

    let (anf, anfenv) = compiler::anf::anf_file(liftenv, &gensym, lifted);
    let (go, goenv) = compiler::go::compile::go_file(anfenv, &gensym, anf);
    go.to_pretty(&goenv, 120)
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
    let ast = match derive::expand(ast) {
        Ok(ast) => ast,
        Err(diagnostics) => return format_derive_errors(diagnostics),
    };

    let (tast, genv, diagnostics) = match typecheck_ast(ast) {
        Ok(result) => result,
        Err(message) => return message,
    };
    let typer_errors = format_typer_diagnostics(&diagnostics);
    if !typer_errors.is_empty() {
        return typer_errors
            .into_iter()
            .map(|err| format!("error: {}", err))
            .collect::<Vec<_>>()
            .join("\n");
    }
    tast.to_pretty(&genv, 120)
}

#[wasm_bindgen]
pub fn hover(src: &str, line: u32, col: u32) -> String {
    if has_imports(src) {
        return "error: package imports are not supported in webapp".to_string();
    }
    match compiler::query::hover_type(std::path::Path::new("dummy"), src, line, col) {
        Ok(result) => result,
        Err(e) => format!("error: {}", e),
    }
}

#[wasm_bindgen]
pub fn dot_completions(src: &str, line: u32, col: u32) -> String {
    if has_imports(src) {
        return "[]".to_string();
    }
    let items = compiler::query::dot_completions(std::path::Path::new("dummy"), src, line, col)
        .unwrap_or_default();
    let mut parts = Vec::with_capacity(items.len());

    for item in items {
        let compiler::query::DotCompletionItem { name, kind, detail } = item;

        let kind_str = match kind {
            compiler::query::DotCompletionKind::Field => "field",
            compiler::query::DotCompletionKind::Method => "method",
        };

        let mut entry = format!(
            "{{\"name\":\"{}\",\"kind\":\"{}\"",
            json_escape(&name),
            kind_str,
        );

        if let Some(detail) = detail {
            entry.push_str(&format!(",\"detail\":\"{}\"", json_escape(&detail)));
        }

        entry.push('}');
        parts.push(entry);
    }

    format!("[{}]", parts.join(","))
}

fn json_escape(input: &str) -> String {
    let mut escaped = String::with_capacity(input.len());

    for c in input.chars() {
        match c {
            '"' => escaped.push_str("\\\""),
            '\\' => escaped.push_str("\\\\"),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            c if c.is_control() => escaped.push_str(&format!("\\u{:04x}", c as u32)),
            c => escaped.push(c),
        }
    }

    escaped
}

fn has_imports(src: &str) -> bool {
    if !src.contains("import") {
        return false;
    }
    let result = parser::parse(&std::path::PathBuf::from("dummy"), src);
    if result.has_errors() {
        return false;
    }
    let root = MySyntaxNode::new_root(result.green_node);
    let cst = cst::cst::File::cast(root).unwrap();
    cst.import_decls().next().is_some()
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

fn format_derive_errors(diagnostics: Diagnostics) -> String {
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
