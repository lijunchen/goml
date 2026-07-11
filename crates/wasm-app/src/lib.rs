use std::path::Path;

use cst::cst::CstNode;
use diagnostics::Diagnostics;
use parser::{parser::ParseResult, syntax::MySyntaxNode};

use compiler::{
    derive,
    env::{Gensym, GlobalTypeEnv, format_compile_diagnostics, format_typer_diagnostics},
};
use wasm_bindgen::prelude::*;

const SOURCE_PATH: &str = "dummy";
const PRETTY_WIDTH: usize = 120;

struct CompilationStage<Ir, Env> {
    ir: Ir,
    env: Env,
    gensym: Gensym,
}

type CoreStage = CompilationStage<compiler::core::File, GlobalTypeEnv>;
type MonoStage = CompilationStage<compiler::mono::MonoFile, compiler::mono::GlobalMonoEnv>;
type LiftStage = CompilationStage<compiler::lift::LiftFile, compiler::lift::GlobalLiftEnv>;
type AnfStage = CompilationStage<compiler::anf::File, compiler::anf::GlobalAnfEnv>;

fn parse_source(src: &str) -> Result<ParseResult, String> {
    let result = parser::parse(Path::new(SOURCE_PATH), src);
    if result.has_errors() {
        return Err(format_parse_errors(&result, src));
    }
    Ok(result)
}

fn lower_source(src: &str) -> Result<::ast::ast::File, String> {
    let result = parse_source(src)?;
    let root = MySyntaxNode::new_root(result.green_node);
    let cst = cst::cst::File::cast(root).unwrap();
    ast::lower::lower(cst)
        .into_result()
        .map_err(format_stage_errors)
}

fn expand_source(src: &str) -> Result<::ast::ast::File, String> {
    derive::expand(lower_source(src)?).map_err(format_stage_errors)
}

fn typecheck_source(src: &str) -> Result<(compiler::tast::File, GlobalTypeEnv), String> {
    let ast = expand_source(src)?;
    if !ast.uses.is_empty() {
        return Err("error: package uses are not supported in webapp".to_string());
    }
    let (hir, hir_table, mut hir_diagnostics) = compiler::hir::lower_to_hir(ast);
    let (tast, genv, mut diagnostics) = compiler::typer::check_file(hir, hir_table);
    diagnostics.append(&mut hir_diagnostics);
    let typer_errors = format_typer_diagnostics(&diagnostics, src);
    if !typer_errors.is_empty() {
        return Err(typer_errors
            .into_iter()
            .map(|error| format!("error: {error}"))
            .collect::<Vec<_>>()
            .join("\n"));
    }
    Ok((tast, compiler::builtins::merge_with_builtin_env(&genv)))
}

fn compile_core_source(src: &str) -> Result<CoreStage, String> {
    let (tast, env) = typecheck_source(src)?;
    let gensym = Gensym::new();
    let mut diagnostics = Diagnostics::new();
    let ir = compiler::compile_match::compile_file(&env, &gensym, &mut diagnostics, &tast);
    if diagnostics.has_errors() {
        return Err(format_compile_diagnostics(&diagnostics, src)
            .into_iter()
            .map(|message| format!("error (compile): {message}"))
            .collect::<Vec<_>>()
            .join("\n"));
    }
    Ok(CompilationStage { ir, env, gensym })
}

fn compile_mono_source(src: &str) -> Result<MonoStage, String> {
    let CoreStage { ir, env, gensym } = compile_core_source(src)?;
    let (ir, env) =
        compiler::mono::mono(env, ir).map_err(|message| format!("error (compile): {message}"))?;
    Ok(CompilationStage { ir, env, gensym })
}

fn compile_lift_source(src: &str) -> Result<LiftStage, String> {
    let MonoStage { ir, env, gensym } = compile_mono_source(src)?;
    let (ir, env) = compiler::lift::lambda_lift(env, &gensym, ir);
    Ok(CompilationStage { ir, env, gensym })
}

fn compile_anf_source(src: &str) -> Result<AnfStage, String> {
    let LiftStage { ir, env, gensym } = compile_lift_source(src)?;
    let (ir, env) = compiler::anf::anf_file(env, &gensym, ir);
    Ok(CompilationStage { ir, env, gensym })
}

fn render(result: Result<String, String>) -> String {
    result.unwrap_or_else(|error| error)
}

#[wasm_bindgen]
pub fn execute(src: &str) -> String {
    render(compile_core_source(src).map(|_| "not support for now".to_string()))
}

#[wasm_bindgen]
pub fn compile_to_core(src: &str) -> String {
    render(compile_core_source(src).map(|stage| stage.ir.to_pretty(&stage.env, PRETTY_WIDTH)))
}

#[wasm_bindgen]
pub fn compile_to_mono(src: &str) -> String {
    render(compile_mono_source(src).map(|stage| stage.ir.to_pretty(&stage.env, PRETTY_WIDTH)))
}

#[wasm_bindgen]
pub fn compile_to_anf(src: &str) -> String {
    render(compile_anf_source(src).map(|stage| stage.ir.to_pretty(&stage.env, PRETTY_WIDTH)))
}

#[wasm_bindgen]
pub fn compile_to_go(src: &str) -> String {
    render(compile_anf_source(src).map(|stage| {
        let (go, goenv) = compiler::go::compile::go_file(stage.env, &stage.gensym, stage.ir);
        go.to_pretty(&goenv, PRETTY_WIDTH)
    }))
}

#[wasm_bindgen]
pub fn get_cst(src: &str) -> String {
    render(parse_source(src).map(|result| parser::debug_tree(&result.green_node)))
}

#[wasm_bindgen]
pub fn get_ast(src: &str) -> String {
    render(lower_source(src).map(|ast| format!("{ast:#?}")))
}

#[wasm_bindgen]
pub fn get_tast(src: &str) -> String {
    render(typecheck_source(src).map(|(tast, env)| tast.to_pretty(&env, PRETTY_WIDTH)))
}

#[wasm_bindgen]
pub fn hover(src: &str, line: u32, col: u32) -> String {
    if has_uses(src) {
        return "error: package uses are not supported in webapp".to_string();
    }
    match compiler::query::hover_type(Path::new(SOURCE_PATH), src, line, col) {
        Ok(result) => result,
        Err(e) => format!("error: {}", e),
    }
}

#[wasm_bindgen]
pub fn dot_completions(src: &str, line: u32, col: u32) -> String {
    if has_uses(src) {
        return "[]".to_string();
    }
    let items = compiler::query::dot_completions(Path::new(SOURCE_PATH), src, line, col)
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

#[wasm_bindgen]
pub fn colon_colon_completions(src: &str, line: u32, col: u32) -> String {
    if has_uses(src) {
        return "[]".to_string();
    }
    let items = compiler::query::colon_colon_completions(Path::new(SOURCE_PATH), src, line, col)
        .unwrap_or_default();
    let mut parts = Vec::with_capacity(items.len());

    for item in items {
        let compiler::query::ColonColonCompletionItem { name, kind, detail } = item;

        let kind_str = match kind {
            compiler::query::ColonColonCompletionKind::Package => "package",
            compiler::query::ColonColonCompletionKind::Type => "type",
            compiler::query::ColonColonCompletionKind::Value => "value",
            compiler::query::ColonColonCompletionKind::Trait => "trait",
            compiler::query::ColonColonCompletionKind::Variant => "variant",
            compiler::query::ColonColonCompletionKind::Method => "method",
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

#[wasm_bindgen]
pub fn value_completions(src: &str, line: u32, col: u32) -> String {
    if has_uses(src) {
        return "[]".to_string();
    }
    let items = compiler::query::value_completions(Path::new(SOURCE_PATH), src, line, col)
        .unwrap_or_default();
    let mut parts = Vec::with_capacity(items.len());

    for item in items {
        let compiler::query::ValueCompletionItem { name, kind, detail } = item;

        let kind_str = match kind {
            compiler::query::ValueCompletionKind::Variable => "variable",
            compiler::query::ValueCompletionKind::Package => "package",
            compiler::query::ValueCompletionKind::Function => "function",
            compiler::query::ValueCompletionKind::Keyword => "keyword",
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

fn has_uses(src: &str) -> bool {
    if !src.contains("use") {
        return false;
    }
    let result = parser::parse(Path::new(SOURCE_PATH), src);
    if result.has_errors() {
        return false;
    }
    let root = MySyntaxNode::new_root(result.green_node);
    let cst = cst::cst::File::cast(root).unwrap();
    cst.use_decls().next().is_some()
}

fn format_parse_errors(result: &ParseResult, src: &str) -> String {
    result
        .format_errors(src)
        .into_iter()
        .map(|err| format!("error: {}", err))
        .collect::<Vec<_>>()
        .join("\n")
}

fn format_stage_errors(diagnostics: Diagnostics) -> String {
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

#[cfg(test)]
mod tests {
    use super::{compile_to_core, compile_to_mono};

    #[test]
    fn compile_to_core_handles_builtin_result_match() {
        let src = r#"


fn unwrap_or_zero(x: Result[int32, string]) -> int32 {
    match x {
        Result::Ok(v) => v,
        Result::Err(_) => 0,
    }
}

fn main() {
    println(unwrap_or_zero(Result::Ok(1)));
}
"#;

        let output = compile_to_core(src);
        assert!(!output.contains("Internal compiler error"));
        assert!(!output.contains("enum Result not found during match compilation"));
    }

    #[test]
    fn compile_to_mono_preserves_closures() {
        let src = r#"
fn main() {
    let identity = |value: int32| value;
    let _ = identity(1);
    ()
}
"#;

        let output = compile_to_mono(src);
        assert!(output.contains(" = |"), "{output}");
    }
}
