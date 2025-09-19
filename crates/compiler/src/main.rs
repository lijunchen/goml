use cst::cst::CstNode;
use parser::syntax::MySyntaxNode;

fn main() {
    let file_arg = std::env::args().nth(1).unwrap_or_else(|| {
        eprintln!("Usage: {} <file_path>", std::env::args().next().unwrap());
        std::process::exit(1);
    });
    let file_path = std::path::PathBuf::from(&file_arg);
    let content = std::fs::read_to_string(&file_path).unwrap_or_else(|_| {
        eprintln!("Error reading file: {}", file_path.display());
        std::process::exit(1);
    });
    let src = content;
    let parse_result = parser::parse(&file_path, &src);
    let (green_node, diagnostics) = parse_result.into_parts();
    if diagnostics.has_errors() {
        for error in parser::format_parser_diagnostics(&diagnostics, &src) {
            eprintln!("error: {}: {}", file_path.display(), error);
        }
        std::process::exit(1);
    }
    // Future compilation stages can extend `diagnostics` with their own errors.
    let root = MySyntaxNode::new_root(green_node);
    let cst = cst::cst::File::cast(root).unwrap();
    let ast = ast::lower::lower(cst).unwrap();

    let (tast, mut env) = compiler::typer::check_file(ast);
    // dbg!(&tast);
    let core = compiler::compile_match::compile_file(&env, &tast);
    let core = compiler::mono::mono(&mut env, core);
    // dbg!(&core);
    let anf = compiler::anf::anf_file(&env, core);
    // dbg!(&anf);
    // println!("{}", anf.to_pretty(&env, 120));
    let go = compiler::go::compile::go_file(&env, anf);
    // dbg!(&go);
    println!("{}", go.to_pretty(&env, 120));
}
