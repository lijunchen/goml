use std::path::{Path, PathBuf};

use anyhow::Context;
use cst::cst::CstNode;
use parser::{debug_tree, syntax::MySyntaxNode};

mod query_test;

#[test]
fn test_cases() -> anyhow::Result<()> {
    let root_dir = std::path::PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let cases_dir = root_dir.join("src/tests/cases");
    run_test_cases(&cases_dir)
}

#[test]
fn test_examples() -> anyhow::Result<()> {
    let root_dir = std::path::PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let examples_dir = root_dir.join("src/tests/examples");
    run_test_cases(&examples_dir)
}

fn go_bin() -> PathBuf {
    if std::env::consts::OS == "linux" {
        let p = PathBuf::from("/usr/lib/go-1.25/bin/go");
        if p.exists() { p } else { PathBuf::from("go") }
    } else {
        PathBuf::from("go")
    }
}

fn execute_single_go_file(input: &Path) -> anyhow::Result<String> {
    use std::io::Write;
    use std::process::{Command, Stdio};
    dbg!(&input);

    let dir = tempfile::tempdir().with_context(|| "Failed to create temporary directory")?;

    // copy go_file into tempdir, and rename extension name to .go
    let main_go_file = dir.path().join("main.go");
    std::fs::copy(&input, &main_go_file).with_context(|| {
        format!(
            "Failed to copy go file from {} to {}",
            input.display(),
            main_go_file.display()
        )
    })?;

    let go = go_bin();
    let mut child = Command::new(&go)
        .arg("run")
        .arg("main.go")
        .current_dir(dir.path())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    {
        let stdin = child.stdin.as_mut().unwrap();
        stdin
            .write_all(b"")
            .with_context(|| "Failed to write to stdin of Go program")?;
    }
    let output = child.wait_with_output()?;
    if !output.status.success() {
        Ok(String::from_utf8_lossy(&output.stderr).to_string())
    } else {
        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    }
}

fn run_test_cases(dir: &Path) -> anyhow::Result<()> {
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        if entry.file_type()?.is_file()
            && entry.path().extension().and_then(std::ffi::OsStr::to_str) == Some("src")
        {
            let p = entry.path();
            println!("Testing file: {}", p.display());
            let filename = p.file_name().unwrap().to_str().unwrap();
            if filename == "016.src" || filename == "018.src" || filename == "002_overloading.src" {
                println!("Skip {}, monomorphization not implemented yet", filename);
                continue;
            }
            let cst_filename = p.with_file_name(format!("{}.cst", filename));
            let ast_filename = p.with_file_name(format!("{}.ast", filename));
            let tast_filename = p.with_file_name(format!("{}.tast", filename));
            let core_filename = p.with_file_name(format!("{}.core", filename));
            let anf_filename = p.with_file_name(format!("{}.anf", filename));
            let go_filename = p.with_file_name(format!("{}.gom", filename));
            let result_filename = p.with_file_name(format!("{}.out", filename));

            let input = std::fs::read_to_string(entry.path())?;

            {
                let result = parser::parse(&p, &input);
                expect_test::expect_file![cst_filename].assert_eq(&debug_tree(&result.green_node));
            }

            let result = parser::parse(&p, &input);
            let root = MySyntaxNode::new_root(result.green_node);
            let cst = cst::cst::File::cast(root).unwrap();
            let ast = ast::lower::lower(cst).unwrap();

            expect_test::expect_file![ast_filename].assert_eq(&ast.to_pretty(120));
            let (tast, env) = crate::typer::check_file(ast);
            expect_test::expect_file![tast_filename].assert_eq(&tast.to_pretty(&env, 120));
            let core = crate::compile_match::compile_file(&env, &tast);
            expect_test::expect_file![core_filename].assert_eq(&core.to_pretty(&env, 120));

            let mut buf = String::new();
            let eval_env = im::HashMap::new();
            let interpreter_result = crate::interpreter::eval_file(&eval_env, &mut buf, &core);

            let anf = crate::anf::anf_file(&env, core);
            expect_test::expect_file![anf_filename].assert_eq(&anf.to_pretty(&env, 120));

            let go = crate::go::compile::go_file(&env, anf);
            expect_test::expect_file![&go_filename].assert_eq(&go.to_pretty(&env, 120));

            let go_output = execute_single_go_file(&go_filename).unwrap();

            expect_test::expect_file![result_filename].assert_eq(&format!(
                "{:?}\n====\n{}\n====\n{}",
                interpreter_result, buf, go_output
            ));
        }
    }
    Ok(())
}
