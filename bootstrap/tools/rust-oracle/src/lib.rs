use std::fmt::Write;
use std::path::Path;

mod ast_encode;

pub use ast_encode::encode_ast;

pub fn encode_lift(path: &Path, source: &str) -> String {
    let path = path.to_owned();
    let source = source.to_owned();
    std::thread::Builder::new()
        .stack_size(64 * 1024 * 1024)
        .spawn(move || encode_lift_inner(&path, &source))
        .unwrap()
        .join()
        .unwrap()
}

fn encode_lift_inner(path: &Path, source: &str) -> String {
    use cst::cst::CstNode;
    use parser::syntax::MySyntaxNode;

    let parsed = parser::parse(path, source);
    if !parsed.diagnostics.is_empty() {
        return String::new();
    }
    let root = MySyntaxNode::new_root(parsed.green_node);
    let file = cst::cst::File::cast(root).unwrap();
    let (file, _) = ast::lower::lower(file).into_parts();
    let Some(file) = file else {
        return String::new();
    };
    let original = file.clone();
    let file = compiler::derive::expand(file).unwrap_or(original);
    let source_file = compiler::hir::SourceFileAst::new(path.to_owned(), file);
    let (hir, table, diagnostics) = compiler::hir::lower_to_hir_files(vec![source_file]);
    if diagnostics.has_errors() {
        return String::new();
    }
    let (tast, genv, mut diagnostics) = compiler::typer::check_file(hir, table);
    if diagnostics.has_errors() {
        return String::new();
    }
    let genv = compiler::builtins::merge_with_builtin_env(&genv);
    let gensym = compiler::env::Gensym::new();
    let core = compiler::compile_match::compile_file(&genv, &gensym, &mut diagnostics, &tast);
    if diagnostics.has_errors() {
        return String::new();
    }
    let Ok((mono, monoenv)) = compiler::mono::mono(genv, core) else {
        return String::new();
    };
    let (lift, liftenv) = compiler::lift::lambda_lift(monoenv, &gensym, mono);
    lift.to_pretty(&liftenv, 120)
}

pub fn encode_mono(path: &Path, source: &str) -> String {
    let path = path.to_owned();
    let source = source.to_owned();
    std::thread::Builder::new()
        .stack_size(64 * 1024 * 1024)
        .spawn(move || encode_mono_inner(&path, &source))
        .unwrap()
        .join()
        .unwrap()
}

fn encode_mono_inner(path: &Path, source: &str) -> String {
    use cst::cst::CstNode;
    use parser::syntax::MySyntaxNode;

    let parsed = parser::parse(path, source);
    if !parsed.diagnostics.is_empty() {
        return String::new();
    }
    let root = MySyntaxNode::new_root(parsed.green_node);
    let file = cst::cst::File::cast(root).unwrap();
    let (file, _) = ast::lower::lower(file).into_parts();
    let Some(file) = file else {
        return String::new();
    };
    let original = file.clone();
    let file = compiler::derive::expand(file).unwrap_or(original);
    let source_file = compiler::hir::SourceFileAst::new(path.to_owned(), file);
    let (hir, table, diagnostics) = compiler::hir::lower_to_hir_files(vec![source_file]);
    if diagnostics.has_errors() {
        return String::new();
    }
    let (tast, genv, mut diagnostics) = compiler::typer::check_file(hir, table);
    if diagnostics.has_errors() {
        return String::new();
    }
    let genv = compiler::builtins::merge_with_builtin_env(&genv);
    let core = compiler::compile_match::compile_file(
        &genv,
        &compiler::env::Gensym::new(),
        &mut diagnostics,
        &tast,
    );
    if diagnostics.has_errors() {
        return String::new();
    }
    let Ok((mono, monoenv)) = compiler::mono::mono(genv, core) else {
        return String::new();
    };
    mono.to_pretty(&monoenv, 120)
}

pub fn encode_core(path: &Path, source: &str) -> String {
    let path = path.to_owned();
    let source = source.to_owned();
    std::thread::Builder::new()
        .stack_size(64 * 1024 * 1024)
        .spawn(move || encode_core_inner(&path, &source))
        .unwrap()
        .join()
        .unwrap()
}

fn encode_core_inner(path: &Path, source: &str) -> String {
    use cst::cst::CstNode;
    use parser::syntax::MySyntaxNode;

    let parsed = parser::parse(path, source);
    if !parsed.diagnostics.is_empty() {
        return String::new();
    }
    let root = MySyntaxNode::new_root(parsed.green_node);
    let file = cst::cst::File::cast(root).unwrap();
    let (file, _) = ast::lower::lower(file).into_parts();
    let Some(file) = file else {
        return String::new();
    };
    let original = file.clone();
    let file = compiler::derive::expand(file).unwrap_or(original);
    let source_file = compiler::hir::SourceFileAst::new(path.to_owned(), file);
    let (hir, table, diagnostics) = compiler::hir::lower_to_hir_files(vec![source_file]);
    if diagnostics.has_errors() {
        return String::new();
    }
    let (tast, genv, mut diagnostics) = compiler::typer::check_file(hir, table);
    if diagnostics.has_errors() {
        return String::new();
    }
    let genv = compiler::builtins::merge_with_builtin_env(&genv);
    let core = compiler::compile_match::compile_file(
        &genv,
        &compiler::env::Gensym::new(),
        &mut diagnostics,
        &tast,
    );
    if diagnostics.has_errors() {
        return String::new();
    }
    core.to_pretty(&genv, 120)
}

pub fn encode_tast(path: &Path, source: &str) -> String {
    let path = path.to_owned();
    let source = source.to_owned();
    std::thread::Builder::new()
        .stack_size(64 * 1024 * 1024)
        .spawn(move || encode_tast_inner(&path, &source))
        .unwrap()
        .join()
        .unwrap()
}

fn encode_tast_inner(path: &Path, source: &str) -> String {
    use cst::cst::CstNode;
    use parser::syntax::MySyntaxNode;

    let parsed = parser::parse(path, source);
    if !parsed.diagnostics.is_empty() {
        return String::new();
    }
    let root = MySyntaxNode::new_root(parsed.green_node);
    let file = cst::cst::File::cast(root).unwrap();
    let (file, _) = ast::lower::lower(file).into_parts();
    let Some(file) = file else {
        return String::new();
    };
    let original = file.clone();
    let file = compiler::derive::expand(file).unwrap_or(original);
    let source_file = compiler::hir::SourceFileAst::new(path.to_owned(), file);
    let (hir, table, diagnostics) = compiler::hir::lower_to_hir_files(vec![source_file]);
    if diagnostics.has_errors() {
        return String::new();
    }
    let (tast, genv, diagnostics) = compiler::typer::check_file(hir, table);
    if diagnostics.has_errors() {
        return String::new();
    }
    tast.to_pretty(&genv, 120)
}

pub fn encode_hir(path: &Path, source: &str) -> String {
    let path = path.to_owned();
    let source = source.to_owned();
    std::thread::Builder::new()
        .stack_size(64 * 1024 * 1024)
        .spawn(move || encode_hir_inner(&path, &source))
        .unwrap()
        .join()
        .unwrap()
}

fn encode_hir_inner(path: &Path, source: &str) -> String {
    use cst::cst::CstNode;
    use parser::syntax::MySyntaxNode;

    let parsed = parser::parse(path, source);
    if !parsed.diagnostics.is_empty() {
        return String::new();
    }
    let root = MySyntaxNode::new_root(parsed.green_node);
    let file = cst::cst::File::cast(root).unwrap();
    let (file, _) = ast::lower::lower(file).into_parts();
    let Some(file) = file else {
        return String::new();
    };
    let original = file.clone();
    let file = compiler::derive::expand(file).unwrap_or(original);
    let source_file = compiler::hir::SourceFileAst::new(path.to_owned(), file);
    let (package, table, _) = compiler::hir::lower_to_hir_files(vec![source_file]);
    let mut project_table = compiler::hir::ProjectHirTable::new();
    project_table.insert(package.id, table);
    let context = compiler::pprint::hir_pprint::HirPrintCtx::new(&project_table);
    package.to_pretty(&context, 120)
}

pub fn encode(source: &str) -> String {
    let mut output = String::new();
    for token in lexer::lex(source) {
        let start = u32::from(token.range.start());
        let end = u32::from(token.range.end());
        write!(output, "{:?}\t{start}\t{end}\t", token.kind).unwrap();
        for byte in token.text.bytes() {
            write!(output, "{byte:02x}").unwrap();
        }
        output.push('\n');
    }
    output
}

fn encode_hex(output: &mut String, value: &str) {
    for byte in value.bytes() {
        write!(output, "{byte:02x}").unwrap();
    }
}

fn encode_node(output: &mut String, node: &parser::syntax::MySyntaxNode) {
    let range = node.text_range();
    writeln!(
        output,
        "N\t{:?}\t{}\t{}",
        node.kind(),
        u32::from(range.start()),
        u32::from(range.end())
    )
    .unwrap();
    for child in node.children_with_tokens() {
        if let Some(child_node) = child.as_node() {
            encode_node(output, child_node);
        } else if let Some(token) = child.as_token() {
            let range = token.text_range();
            write!(
                output,
                "T\t{:?}\t{}\t{}\t",
                token.kind(),
                u32::from(range.start()),
                u32::from(range.end())
            )
            .unwrap();
            encode_hex(output, token.text());
            output.push('\n');
        }
    }
    output.push_str("E\n");
}

pub fn encode_parse(path: &Path, source: &str) -> String {
    let result = parser::parse(path, source);
    let root = parser::syntax::MySyntaxNode::new_root(result.green_node);
    let mut output = String::new();
    encode_node(&mut output, &root);
    for diagnostic in &result.diagnostics {
        let range = diagnostic.range().unwrap_or_default();
        write!(
            output,
            "D\t{}\t{}\t",
            u32::from(range.start()),
            u32::from(range.end())
        )
        .unwrap();
        encode_hex(&mut output, diagnostic.message());
        output.push('\n');
    }
    output
}
