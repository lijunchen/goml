use std::{env, fs, process};

fn main() {
    let mut args = env::args_os();
    let _ = args.next();
    let Some(mode) = args.next() else {
        eprintln!(
            "usage: gomlang-parser-rust-oracle <lex|cst|ast|hir|tast|core|mono|lift|anf|go> <file>"
        );
        process::exit(2);
    };
    let Some(path) = args.next() else {
        eprintln!(
            "usage: gomlang-parser-rust-oracle <lex|cst|ast|hir|tast|core|mono|lift|anf|go> <file>"
        );
        process::exit(2);
    };
    let source = match fs::read_to_string(&path) {
        Ok(source) => source,
        Err(error) => {
            eprintln!("failed to read {}: {error}", path.to_string_lossy());
            process::exit(1);
        }
    };
    if mode == "lex" {
        print!("{}", gomlang_parser_rust_oracle::encode(&source));
    } else if mode == "cst" {
        print!(
            "{}",
            gomlang_parser_rust_oracle::encode_parse(path.as_ref(), &source)
        );
    } else if mode == "ast" {
        print!(
            "{}",
            gomlang_parser_rust_oracle::encode_ast(path.as_ref(), &source)
        );
    } else if mode == "hir" {
        print!(
            "{}",
            gomlang_parser_rust_oracle::encode_hir(path.as_ref(), &source)
        );
    } else if mode == "tast" {
        print!(
            "{}",
            gomlang_parser_rust_oracle::encode_tast(path.as_ref(), &source)
        );
    } else if mode == "core" {
        print!(
            "{}",
            gomlang_parser_rust_oracle::encode_core(path.as_ref(), &source)
        );
    } else if mode == "mono" {
        print!(
            "{}",
            gomlang_parser_rust_oracle::encode_mono(path.as_ref(), &source)
        );
    } else if mode == "lift" {
        print!(
            "{}",
            gomlang_parser_rust_oracle::encode_lift(path.as_ref(), &source)
        );
    } else if mode == "anf" {
        print!(
            "{}",
            gomlang_parser_rust_oracle::encode_anf(path.as_ref(), &source)
        );
    } else if mode == "go" {
        print!(
            "{}",
            gomlang_parser_rust_oracle::encode_go(path.as_ref(), &source)
        );
    } else {
        eprintln!("unknown mode: {}", mode.to_string_lossy());
        process::exit(2);
    }
}
