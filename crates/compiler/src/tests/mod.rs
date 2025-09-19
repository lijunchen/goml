use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, OnceLock};

use anyhow::Context;
use anyhow::bail;
use cst::cst::CstNode;
use parser::{debug_tree, syntax::MySyntaxNode};

mod query_test;
mod struct_type_test;
mod trait_impl_test;

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

#[test]
fn test_parse_error_cases() -> anyhow::Result<()> {
    let root_dir = std::path::PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let diagnostics_dir = root_dir.join("src/tests/diagnostics");
    run_parse_error_cases(&diagnostics_dir)
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
    use std::process::{Command, Stdio};

    dbg!(&input);

    let go = go_bin();
    let go_version = go_version(&go)?;
    let go_source = std::fs::read(input)
        .with_context(|| format!("Failed to read go source from {}", input.display()))?;

    let cache_dir = go_cache_dir(&go_version, &go_source, input)?;
    let main_go_file = cache_dir.join("main.go");
    let binary_path = cache_dir.join(if cfg!(windows) { "main.exe" } else { "main" });

    if !binary_path.exists() {
        std::fs::create_dir_all(&cache_dir)
            .with_context(|| format!("Failed to create {}", cache_dir.display()))?;
        std::fs::write(&main_go_file, &go_source)
            .with_context(|| format!("Failed to write go source to {}", main_go_file.display()))?;

        let output = Command::new(&go)
            .arg("build")
            .arg("-o")
            .arg(&binary_path)
            .arg("main.go")
            .current_dir(&cache_dir)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .with_context(|| format!("Failed to build go binary at {}", cache_dir.display()))?;

        if !output.status.success() {
            let _ = std::fs::remove_file(&binary_path);
            let stderr = if output.stderr.is_empty() {
                &output.stdout
            } else {
                &output.stderr
            };
            return Ok(String::from_utf8_lossy(stderr).to_string());
        }
    }

    let output = Command::new(&binary_path)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .with_context(|| format!("Failed to run go binary at {}", binary_path.display()))?;

    if !output.status.success() {
        Ok(String::from_utf8_lossy(&output.stderr).to_string())
    } else {
        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    }
}

fn go_version(go: &Path) -> anyhow::Result<String> {
    static VERSION: OnceLock<String> = OnceLock::new();
    if let Some(version) = VERSION.get() {
        return Ok(version.clone());
    }

    let output = std::process::Command::new(go)
        .arg("version")
        .output()
        .with_context(|| format!("Failed to query Go toolchain at {}", go.display()))?;
    if !output.status.success() {
        bail!("`go version` exited with status {}", output.status);
    }
    let version = String::from_utf8_lossy(&output.stdout).trim().to_owned();
    let _ = VERSION.set(version.clone());
    Ok(version)
}

fn go_cache_dir(go_version: &str, source: &[u8], input: &Path) -> anyhow::Result<PathBuf> {
    let base = std::env::var_os("CARGO_TARGET_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| workspace_root().join("target"));
    let root = base.join("go-run-cache");
    std::fs::create_dir_all(&root)?;

    let mut hasher = Fnv1aHasher::new();
    hasher.update(go_version.as_bytes());
    hasher.update(source);
    let path_string = input.to_string_lossy();
    hasher.update(path_string.as_bytes());
    let key = hasher.finish_hex();

    Ok(root.join(key))
}

fn workspace_root() -> PathBuf {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    for ancestor in manifest_dir.ancestors() {
        if ancestor.join("Cargo.lock").exists() {
            return ancestor.to_path_buf();
        }
    }
    manifest_dir
}

struct Fnv1aHasher {
    hash: u64,
}

impl Fnv1aHasher {
    fn new() -> Self {
        Self {
            hash: 0xcbf29ce484222325,
        }
    }

    fn update(&mut self, bytes: &[u8]) {
        for &byte in bytes {
            self.hash ^= u64::from(byte);
            self.hash = self.hash.wrapping_mul(0x100000001b3);
        }
    }

    fn finish_hex(&self) -> String {
        format!("{:016x}", self.hash)
    }
}

fn run_test_cases(dir: &Path) -> anyhow::Result<()> {
    let mut case_paths = Vec::new();
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        if entry.file_type()?.is_file()
            && entry.path().extension().and_then(std::ffi::OsStr::to_str) == Some("src")
        {
            case_paths.push(entry.path());
        }
    }

    if case_paths.is_empty() {
        return Ok(());
    }

    let available = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1);
    let worker_count = std::cmp::min(available, case_paths.len());
    let cases: Arc<Mutex<Vec<PathBuf>>> = Arc::new(Mutex::new(case_paths));

    let mut handles = Vec::with_capacity(worker_count);
    for _ in 0..worker_count {
        let cases = Arc::clone(&cases);
        handles.push(std::thread::spawn(move || -> anyhow::Result<()> {
            loop {
                let case = {
                    let mut cases = cases.lock().unwrap();
                    cases.pop()
                };

                match case {
                    Some(case) => run_single_test_case(case)?,
                    None => break,
                }
            }

            Ok(())
        }));
    }

    for handle in handles {
        match handle.join() {
            Ok(result) => result?,
            Err(panic) => std::panic::resume_unwind(panic),
        }
    }

    Ok(())
}

fn run_single_test_case(p: PathBuf) -> anyhow::Result<()> {
    println!("Testing file: {}", p.display());
    let filename = p.file_name().unwrap().to_str().unwrap();
    let cst_filename = p.with_file_name(format!("{}.cst", filename));
    let ast_filename = p.with_file_name(format!("{}.ast", filename));
    let tast_filename = p.with_file_name(format!("{}.tast", filename));
    let core_filename = p.with_file_name(format!("{}.core", filename));
    let mono_filename = p.with_file_name(format!("{}.mono", filename));
    let anf_filename = p.with_file_name(format!("{}.anf", filename));
    let go_filename = p.with_file_name(format!("{}.gom", filename));
    let result_filename = p.with_file_name(format!("{}.out", filename));

    let input = std::fs::read_to_string(&p)?;

    let result = parser::parse(&p, &input);
    if result.has_errors() {
        panic!(
            "Parse errors in {}:\n{}",
            p.display(),
            result.format_errors(&input).join("\n")
        );
    }

    let cst_debug = debug_tree(&result.green_node);
    expect_test::expect_file![cst_filename].assert_eq(&cst_debug);

    let root = MySyntaxNode::new_root(result.green_node);
    let cst = cst::cst::File::cast(root).unwrap();
    let ast = ast::lower::lower(cst).unwrap();

    expect_test::expect_file![ast_filename].assert_eq(&ast.to_pretty(120));
    let (tast, mut env) = crate::typer::check_file(ast);
    expect_test::expect_file![tast_filename].assert_eq(&tast.to_pretty(&env, 120));
    let core = crate::compile_match::compile_file(&env, &tast);
    expect_test::expect_file![core_filename].assert_eq(&core.to_pretty(&env, 120));

    let mono = crate::mono::mono(&mut env, core);
    expect_test::expect_file![mono_filename].assert_eq(&mono.to_pretty(&env, 120));

    let anf = crate::anf::anf_file(&env, mono);
    expect_test::expect_file![anf_filename].assert_eq(&anf.to_pretty(&env, 120));

    let go = crate::go::compile::go_file(&env, anf);
    expect_test::expect_file![&go_filename].assert_eq(&go.to_pretty(&env, 120));

    let go_output = execute_single_go_file(&go_filename)?;

    expect_test::expect_file![result_filename].assert_eq(&go_output);

    Ok(())
}

fn run_parse_error_cases(dir: &Path) -> anyhow::Result<()> {
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        if entry.file_type()?.is_file()
            && entry.path().extension().and_then(std::ffi::OsStr::to_str) == Some("src")
        {
            let p = entry.path();
            println!("Testing diagnostics: {}", p.display());
            let filename = p.file_name().unwrap().to_str().unwrap();
            let diag_filename = p.with_file_name(format!("{}.diag", filename));

            let input = std::fs::read_to_string(&p)?;
            let result = parser::parse(&p, &input);

            if !result.has_errors() {
                bail!(
                    "Expected parse errors in {}, but none were reported",
                    p.display()
                );
            }

            let diagnostics = result.format_errors(&input);
            if diagnostics.is_empty() {
                bail!(
                    "Parser reported errors but no diagnostics were produced for {}",
                    p.display()
                );
            }

            let mut formatted = diagnostics.join("\n");
            if !formatted.is_empty() {
                formatted.push('\n');
            }

            expect_test::expect_file![diag_filename].assert_eq(&formatted);
        }
    }
    Ok(())
}
