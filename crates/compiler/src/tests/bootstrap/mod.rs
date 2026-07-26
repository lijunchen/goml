use std::env;
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Mutex, MutexGuard, OnceLock};

mod ast_encode;
mod generated;
mod oracle;

static PREPARED: OnceLock<()> = OnceLock::new();
static SERIAL: Mutex<()> = Mutex::new(());
static NEXT_TEMP_DIR: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone)]
struct Repository {
    root: PathBuf,
    bootstrap: PathBuf,
    goml: PathBuf,
    rust_gomlc: PathBuf,
    bootstrap_gomlc: PathBuf,
}

impl Repository {
    fn discover() -> Self {
        let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let default_root = manifest
            .parent()
            .and_then(Path::parent)
            .unwrap()
            .to_path_buf();
        let root = env::var_os("GOML_REPO")
            .map(PathBuf::from)
            .unwrap_or(default_root);
        let bootstrap = root.join("bootstrap");
        let goml = env_path("GOML_BIN", root.join("target/debug/goml"));
        let rust_gomlc = env_path("RUST_GOMLC_BIN", root.join("target/debug/gomlc"));
        let bootstrap_gomlc = env_path(
            "BOOTSTRAP_GOMLC_BIN",
            bootstrap.join("_artifact/bin/cmd/gomlc/gomlc"),
        );
        Self {
            root,
            bootstrap,
            goml,
            rust_gomlc,
            bootstrap_gomlc,
        }
    }

    fn tests(&self) -> PathBuf {
        self.root.join("crates/compiler/src/tests")
    }
}

struct TempDir {
    path: PathBuf,
}

impl TempDir {
    fn new(label: &str) -> Self {
        let index = NEXT_TEMP_DIR.fetch_add(1, Ordering::Relaxed);
        let path = env::temp_dir().join(format!(
            "gomlc-bootstrap-{label}-{}-{index}",
            std::process::id()
        ));
        if path.exists() {
            fs::remove_dir_all(&path).unwrap();
        }
        fs::create_dir_all(&path).unwrap();
        Self { path }
    }

    fn path(&self) -> &Path {
        &self.path
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.path);
    }
}

#[derive(Clone, Copy)]
enum Stage {
    Lex,
    Cst,
    Ast,
    Hir,
    Tast,
    Core,
    Mono,
    Lift,
    Anf,
    Go,
}

impl Stage {
    fn name(self) -> &'static str {
        match self {
            Self::Lex => "lex",
            Self::Cst => "cst",
            Self::Ast => "ast",
            Self::Hir => "hir",
            Self::Tast => "tast",
            Self::Core => "core",
            Self::Mono => "mono",
            Self::Lift => "lift",
            Self::Anf => "anf",
            Self::Go => "go",
        }
    }

    fn expected(self, path: &Path, source: &str) -> String {
        match self {
            Self::Lex => oracle::encode(source),
            Self::Cst => oracle::encode_parse(path, source),
            Self::Ast => oracle::encode_ast(path, source),
            Self::Hir => oracle::encode_hir(path, source),
            Self::Tast => oracle::encode_tast(path, source),
            Self::Core => oracle::encode_core(path, source),
            Self::Mono => oracle::encode_mono(path, source),
            Self::Lift => oracle::encode_lift(path, source),
            Self::Anf => oracle::encode_anf(path, source),
            Self::Go => oracle::encode_go(path, source),
        }
    }

    fn skips_empty_oracle(self) -> bool {
        matches!(
            self,
            Self::Hir | Self::Tast | Self::Core | Self::Mono | Self::Lift | Self::Anf | Self::Go
        )
    }
}

fn env_path(name: &str, default: PathBuf) -> PathBuf {
    env::var_os(name).map(PathBuf::from).unwrap_or(default)
}

fn serial() -> MutexGuard<'static, ()> {
    SERIAL.lock().unwrap_or_else(|error| error.into_inner())
}

fn checked_output(command: &mut Command, label: &str) -> Output {
    let output = command
        .output()
        .unwrap_or_else(|error| panic!("failed to run {label}: {error}"));
    if !output.status.success() {
        panic!(
            "{label} failed with {}\nstdout:\n{}\nstderr:\n{}",
            output.status,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }
    output
}

fn prepare() -> Repository {
    let repository = Repository::discover();
    PREPARED.get_or_init(|| {
        if env::var_os("BOOTSTRAP_GOMLC_SKIP_BUILD").as_deref() == Some(OsStr::new("1")) {
            return;
        }
        let mut rust_build = Command::new(env::var_os("CARGO").unwrap_or_else(|| "cargo".into()));
        rust_build
            .arg("build")
            .arg("--manifest-path")
            .arg(repository.root.join("Cargo.toml"))
            .arg("-p")
            .arg("goml")
            .arg("-p")
            .arg("gomlc");
        checked_output(&mut rust_build, "Rust compiler build");
        let mut bootstrap_build = Command::new(&repository.goml);
        bootstrap_build
            .arg("build")
            .arg(repository.bootstrap.join("cmd/gomlc"));
        checked_output(&mut bootstrap_build, "bootstrap compiler build");
    });
    assert!(
        repository.goml.is_file(),
        "missing {}",
        repository.goml.display()
    );
    assert!(
        repository.rust_gomlc.is_file(),
        "missing {}",
        repository.rust_gomlc.display()
    );
    assert!(
        repository.bootstrap_gomlc.is_file(),
        "missing {}",
        repository.bootstrap_gomlc.display()
    );
    repository
}

fn different_line(expected: &[u8], actual: &[u8]) -> String {
    let expected = String::from_utf8_lossy(expected);
    let actual = String::from_utf8_lossy(actual);
    let expected_lines: Vec<&str> = expected.lines().collect();
    let actual_lines: Vec<&str> = actual.lines().collect();
    let count = expected_lines.len().max(actual_lines.len());
    for index in 0..count {
        let expected = expected_lines.get(index).copied().unwrap_or("<missing>");
        let actual = actual_lines.get(index).copied().unwrap_or("<missing>");
        if expected != actual {
            return format!(
                "first difference at line {}\nexpected: {expected}\nactual:   {actual}",
                index + 1
            );
        }
    }
    format!(
        "byte mismatch after {} text lines: expected {} bytes, actual {} bytes",
        count,
        expected.len(),
        actual.len()
    )
}

fn assert_bytes(label: &str, expected: &[u8], actual: &[u8]) {
    if expected != actual {
        panic!("{label}: {}", different_line(expected, actual));
    }
}

fn gom_files(roots: &[PathBuf]) -> Vec<PathBuf> {
    fn collect(path: &Path, files: &mut Vec<PathBuf>) {
        if path.is_dir() {
            if matches!(
                path.file_name().and_then(OsStr::to_str),
                Some("_artifact" | "artifact" | "target")
            ) {
                return;
            }
            let mut entries: Vec<PathBuf> = fs::read_dir(path)
                .unwrap()
                .map(|entry| entry.unwrap().path())
                .collect();
            entries.sort();
            for entry in entries {
                collect(&entry, files);
            }
        } else if path.extension().and_then(OsStr::to_str) == Some("gom") {
            files.push(path.to_path_buf());
        }
    }

    let mut files = Vec::new();
    for root in roots {
        collect(root, &mut files);
    }
    files.sort();
    files
}

fn pipeline_sources(root: &Path) -> Vec<PathBuf> {
    let mut files: Vec<PathBuf> = fs::read_dir(root)
        .unwrap()
        .map(|entry| entry.unwrap().path().join("main.gom"))
        .filter(|path| path.is_file())
        .collect();
    files.sort();
    files
}

fn bootstrap_stage(repository: &Repository, stage: Stage, source: &Path) -> Output {
    let mut command = Command::new(&repository.bootstrap_gomlc);
    if matches!(stage, Stage::Lex) {
        command.arg("lex").arg(source);
    } else {
        command
            .arg("__canonical-stage")
            .arg(stage.name())
            .arg(source);
    }
    checked_output(
        &mut command,
        &format!("bootstrap {} for {}", stage.name(), source.display()),
    )
}

fn compare_corpus_stage(stage: Stage) {
    let _guard = serial();
    let repository = prepare();
    let roots = [
        repository.tests(),
        repository.root.join("crates/lexer"),
        repository.root.join("crates/parser"),
        repository
            .root
            .join("crates/compiler/src/builtin_contract.gom"),
        repository
            .root
            .join("crates/compiler/src/builtin_prelude.gom"),
        repository.root.join("stdlib"),
        repository.bootstrap.clone(),
    ];
    let sources = gom_files(&roots);
    assert!(!sources.is_empty());
    let mut matched = 0;
    for source_path in sources {
        let source = fs::read_to_string(&source_path).unwrap();
        let expected = stage.expected(&source_path, &source);
        if expected.is_empty() && stage.skips_empty_oracle() {
            continue;
        }
        let actual = bootstrap_stage(&repository, stage, &source_path);
        assert_bytes(
            &format!(
                "{} corpus mismatch for {}",
                stage.name(),
                source_path.display()
            ),
            expected.as_bytes(),
            &actual.stdout,
        );
        matched += 1;
    }
    assert!(matched > 0);
}

#[test]
fn generated_sources_match() {
    let _guard = serial();
    let repository = prepare();
    let temporary = TempDir::new("generated");
    generated::compare_lexer(
        &repository.bootstrap_gomlc,
        &temporary.path().join("lexer.gom"),
        4096,
    );
    for stage in ["cst", "ast", "hir", "tast"] {
        generated::compare_parser(
            &repository.bootstrap_gomlc,
            &temporary.path().join(format!("{stage}.gom")),
            stage,
            2048,
        );
    }
    generated::compare_codegen(
        &repository.bootstrap_gomlc,
        &temporary.path().join("go.gom"),
        72,
    );
}

#[test]
fn compiler_version_protocols_match() {
    let _guard = serial();
    let repository = prepare();
    let mut rust = Command::new(&repository.rust_gomlc);
    rust.args(["version", "--format", "json"]);
    let rust = checked_output(&mut rust, "Rust compiler version");
    let mut bootstrap = Command::new(&repository.bootstrap_gomlc);
    bootstrap.args(["version", "--format", "json"]);
    let bootstrap = checked_output(&mut bootstrap, "bootstrap compiler version");
    let rust: serde_json::Value = serde_json::from_slice(&rust.stdout).unwrap();
    let bootstrap: serde_json::Value = serde_json::from_slice(&bootstrap.stdout).unwrap();
    for field in [
        "tool",
        "version",
        "driver_protocol",
        "artifact_format",
        "compiler_abi",
    ] {
        assert_eq!(rust[field], bootstrap[field], "version field {field}");
    }
    assert!(bootstrap["git_hash"].is_null());
    assert!(bootstrap["git_date"].is_null());
}

#[test]
fn cli_subcommand_suggestions_match() {
    let _guard = serial();
    let repository = prepare();
    for typo in ["chec", "buid", "versio", "run-singe", "lnik"] {
        let rust = Command::new(&repository.rust_gomlc)
            .arg(typo)
            .output()
            .unwrap();
        let bootstrap = Command::new(&repository.bootstrap_gomlc)
            .arg(typo)
            .output()
            .unwrap();
        assert_eq!(rust.status.code(), bootstrap.status.code(), "{typo}");
        assert_bytes(
            &format!("subcommand suggestion mismatch for {typo}"),
            &rust.stderr,
            &bootstrap.stderr,
        );
    }
}

#[test]
fn lexer_corpus_matches() {
    compare_corpus_stage(Stage::Lex);
}

#[test]
fn cst_corpus_matches() {
    compare_corpus_stage(Stage::Cst);
}

#[test]
fn ast_corpus_matches() {
    compare_corpus_stage(Stage::Ast);
}

#[test]
fn hir_corpus_matches() {
    compare_corpus_stage(Stage::Hir);
}

#[test]
fn tast_corpus_matches() {
    compare_corpus_stage(Stage::Tast);
}

#[test]
fn core_corpus_matches() {
    compare_corpus_stage(Stage::Core);
}

#[test]
fn mono_corpus_matches() {
    compare_corpus_stage(Stage::Mono);
}

#[test]
fn lift_corpus_matches() {
    compare_corpus_stage(Stage::Lift);
}

#[test]
fn anf_corpus_matches() {
    compare_corpus_stage(Stage::Anf);
}

#[test]
fn go_corpus_matches() {
    compare_corpus_stage(Stage::Go);
}

#[test]
fn run_single_matches() {
    let _guard = serial();
    let repository = prepare();
    let source = env_path(
        "GOMLANG_RUN_SINGLE_SOURCE",
        repository.tests().join("pipeline/001/main.gom"),
    );
    let flags = [
        "--dump-ast",
        "--dump-hir",
        "--dump-tast",
        "--dump-core",
        "--dump-mono",
        "--dump-lift",
        "--dump-anf",
        "--dump-go",
    ];
    let mut rust = Command::new(&repository.rust_gomlc);
    rust.arg("run-single").args(flags).arg(&source);
    let rust = checked_output(&mut rust, "Rust run-single");
    let mut bootstrap = Command::new(&repository.bootstrap_gomlc);
    bootstrap.arg("run-single").args(flags).arg(&source);
    let bootstrap = checked_output(&mut bootstrap, "bootstrap run-single");
    assert_bytes(
        "run-single stdout mismatch",
        &rust.stdout,
        &bootstrap.stdout,
    );
}

#[test]
fn pipeline_snapshots_match() {
    let _guard = serial();
    let repository = prepare();
    let pipeline = repository.tests().join("pipeline");
    let sources = pipeline_sources(&pipeline);
    assert!(!sources.is_empty());
    let stages = [
        "cst", "ast", "hir", "tast", "core", "mono", "lift", "anf", "go",
    ];
    for stage in stages {
        for source in &sources {
            let mut command = Command::new(&repository.bootstrap_gomlc);
            command.arg(stage).arg(source);
            let actual = checked_output(
                &mut command,
                &format!("bootstrap {stage} for {}", source.display()),
            );
            let expected_path = PathBuf::from(format!("{}.{stage}", source.display()));
            let expected = fs::read(&expected_path).unwrap();
            assert_bytes(
                &format!("pipeline snapshot mismatch for {}", expected_path.display()),
                &expected,
                &actual.stdout,
            );
        }
    }
    for source in &sources {
        let mut command = Command::new(&repository.bootstrap_gomlc);
        command.arg("run-single").arg(source);
        let actual = checked_output(
            &mut command,
            &format!("bootstrap execution for {}", source.display()),
        );
        let expected_path = PathBuf::from(format!("{}.out", source.display()));
        let expected = if expected_path.is_file() {
            fs::read(&expected_path).unwrap()
        } else {
            Vec::new()
        };
        assert_bytes(
            &format!("pipeline runtime mismatch for {}", source.display()),
            &expected,
            &actual.stdout,
        );
    }
}

fn bootstrap_diagnostics(repository: &Repository, source: &Path) -> Vec<u8> {
    let mut command = Command::new(&repository.bootstrap_gomlc);
    command.arg("__test-diagnostics").arg(source);
    checked_output(
        &mut command,
        &format!("bootstrap diagnostics for {}", source.display()),
    )
    .stdout
}

fn compare_diagnostic_suite(repository: &Repository, suite: &str) -> usize {
    let sources = gom_files(&[repository.tests().join(suite)]);
    assert!(!sources.is_empty());
    for source in &sources {
        let expected_path = PathBuf::from(format!("{}.diag", source.display()));
        if env::var_os("UPDATE_EXPECT").is_some() {
            let text = fs::read_to_string(source).unwrap();
            fs::write(&expected_path, oracle::encode_diagnostics(source, &text)).unwrap();
            continue;
        }
        let expected = fs::read(expected_path).unwrap();
        let actual = bootstrap_diagnostics(repository, source);
        assert_bytes(
            &format!("{suite} mismatch for {}", source.display()),
            &expected,
            &actual,
        );
    }
    sources.len()
}

fn compare_e2e(repository: &Repository) -> usize {
    let sources = gom_files(&[repository.tests().join("e2e")]);
    let sources: Vec<PathBuf> = sources
        .into_iter()
        .filter(|path| path.file_name().and_then(OsStr::to_str) == Some("main.gom"))
        .filter(|path| PathBuf::from(format!("{}.out", path.display())).is_file())
        .collect();
    assert!(!sources.is_empty());
    for source in &sources {
        let expected = fs::read(PathBuf::from(format!("{}.out", source.display()))).unwrap();
        let diagnostics = bootstrap_diagnostics(repository, source);
        let actual = if diagnostics.is_empty() {
            let mut command = Command::new(&repository.bootstrap_gomlc);
            command.arg("run-single").arg(source);
            checked_output(
                &mut command,
                &format!("bootstrap e2e execution for {}", source.display()),
            )
            .stdout
        } else {
            diagnostics
        };
        assert_bytes(
            &format!("e2e mismatch for {}", source.display()),
            &expected,
            &actual,
        );
    }
    sources.len()
}

fn compare_modules(repository: &Repository, temporary: &TempDir) -> usize {
    let mut outputs: Vec<PathBuf> = gom_files(&[repository.tests().join("module")])
        .into_iter()
        .filter_map(|path| {
            let output = PathBuf::from(format!("{}.out", path.display()));
            (path.file_name().and_then(OsStr::to_str) == Some("main.gom") && output.is_file())
                .then_some(output)
        })
        .collect();
    outputs.sort();
    assert!(!outputs.is_empty());
    for (index, expected_path) in outputs.iter().enumerate() {
        let project = expected_path.parent().unwrap();
        let mut command = Command::new(&repository.goml);
        command
            .current_dir(temporary.path())
            .arg("run")
            .arg("--compiler")
            .arg(&repository.bootstrap_gomlc)
            .arg("--target-dir")
            .arg(temporary.path().join(format!("module-artifact-{index}")))
            .arg(project);
        let actual = checked_output(
            &mut command,
            &format!("bootstrap module execution for {}", project.display()),
        );
        let expected = fs::read(expected_path).unwrap();
        assert_bytes(
            &format!("module mismatch for {}", project.display()),
            &expected,
            &actual.stdout,
        );
    }
    outputs.len()
}

fn compare_module_diagnostics(repository: &Repository, temporary: &TempDir) -> usize {
    let root = repository.tests().join("module_diagnostics");
    let mut projects: Vec<PathBuf> = fs::read_dir(&root)
        .unwrap()
        .map(|entry| entry.unwrap().path())
        .filter(|path| path.is_dir())
        .filter(|path| {
            path.join("main.gom.expect").is_file()
                || path.join("main.gom.ok").is_file()
                || path.join("main.gom.build").is_file()
        })
        .collect();
    projects.sort();
    assert!(!projects.is_empty());
    let mut mismatches = Vec::new();
    for (index, project) in projects.iter().enumerate() {
        let command = if project.join("main.gom.build").is_file() {
            "build"
        } else {
            "check"
        };
        let rust_target = temporary
            .path()
            .join(format!("module-diagnostic-rust-{index}"));
        let bootstrap_target = temporary
            .path()
            .join(format!("module-diagnostic-bootstrap-{index}"));
        let mut rust = Command::new(&repository.goml);
        rust.current_dir(temporary.path())
            .arg(command)
            .arg("--compiler")
            .arg(&repository.rust_gomlc)
            .arg("--target-dir")
            .arg(&rust_target)
            .arg(project);
        let rust = rust.output().unwrap();
        let mut bootstrap = Command::new(&repository.goml);
        bootstrap
            .current_dir(temporary.path())
            .arg(command)
            .arg("--compiler")
            .arg(&repository.bootstrap_gomlc)
            .arg("--target-dir")
            .arg(&bootstrap_target)
            .arg(project);
        let bootstrap = bootstrap.output().unwrap();
        if rust.status.code() != bootstrap.status.code() {
            mismatches.push(format!(
                "module diagnostic status mismatch for {}\nRust: {:?}\nBootstrap: {:?}",
                project.display(),
                rust.status.code(),
                bootstrap.status.code()
            ));
        }
        let rust_stdout = String::from_utf8(rust.stdout)
            .unwrap()
            .replace(rust_target.to_str().unwrap(), "<TARGET>");
        let bootstrap_stdout = String::from_utf8(bootstrap.stdout)
            .unwrap()
            .replace(bootstrap_target.to_str().unwrap(), "<TARGET>");
        if rust_stdout != bootstrap_stdout {
            mismatches.push(format!(
                "module diagnostic stdout mismatch for {}\nRust:\n{rust_stdout}\nBootstrap:\n{bootstrap_stdout}",
                project.display()
            ));
        }
        let rust_stderr = String::from_utf8(rust.stderr)
            .unwrap()
            .replace(rust_target.to_str().unwrap(), "<TARGET>");
        let bootstrap_stderr = String::from_utf8(bootstrap.stderr)
            .unwrap()
            .replace(bootstrap_target.to_str().unwrap(), "<TARGET>");
        if rust_stderr != bootstrap_stderr {
            mismatches.push(format!(
                "module diagnostic stderr mismatch for {}\nRust:\n{rust_stderr}\nBootstrap:\n{bootstrap_stderr}",
                project.display()
            ));
        }
    }
    assert!(mismatches.is_empty(), "{}", mismatches.join("\n\n"));
    projects.len()
}

fn compare_crashers(repository: &Repository, temporary: &TempDir) -> usize {
    let mut sources: Vec<PathBuf> = gom_files(&[repository.tests().join("crashers")])
        .into_iter()
        .filter(|path| path.file_name().and_then(OsStr::to_str) == Some("main.gom"))
        .filter(|path| !path.parent().unwrap().join("goml.toml").is_file())
        .collect();
    sources.sort();
    assert!(!sources.is_empty());
    let mut matched = 0;
    for source in sources {
        let text = fs::read_to_string(&source).unwrap();
        let expected = oracle::encode_diagnostics(&source, &text);
        if env::var_os("UPDATE_EXPECT").is_some() && !expected.is_empty() {
            fs::write(
                PathBuf::from(format!("{}.diag", source.display())),
                &expected,
            )
            .unwrap();
        }
        let actual = bootstrap_diagnostics(repository, &source);
        assert_bytes(
            &format!("crasher diagnostics mismatch for {}", source.display()),
            expected.as_bytes(),
            &actual,
        );
        matched += 1;
        if !expected.is_empty() {
            continue;
        }
        let mut rust = Command::new(&repository.rust_gomlc);
        rust.arg("run-single").arg(&source);
        let rust = rust.output().unwrap();
        let mut bootstrap = Command::new(&repository.bootstrap_gomlc);
        bootstrap.arg("run-single").arg(&source);
        let bootstrap = bootstrap.output().unwrap();
        assert_eq!(
            rust.status.code(),
            bootstrap.status.code(),
            "crasher execution status mismatch for {}",
            source.display()
        );
        assert_bytes(
            &format!("crasher runtime mismatch for {}", source.display()),
            &rust.stdout,
            &bootstrap.stdout,
        );
        matched += 1;
    }
    let host_shadow = repository
        .tests()
        .join("crashers/local_std_host_extern_shadow");
    let mut command = Command::new(&repository.goml);
    command
        .current_dir(temporary.path())
        .arg("check")
        .arg("--compiler")
        .arg(&repository.bootstrap_gomlc)
        .arg("--target-dir")
        .arg(temporary.path().join("host-shadow-artifact"))
        .arg(&host_shadow);
    let output = command.output().unwrap();
    assert!(
        !output.status.success(),
        "bootstrap unexpectedly accepted {}",
        host_shadow.display()
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains("extern args_raw is not permitted in this source"),
        "unexpected host-shadow diagnostic: {combined}"
    );
    matched + 1
}

#[test]
fn compiler_test_suites_match() {
    let _guard = serial();
    let repository = prepare();
    let temporary = TempDir::new("compiler-suites");
    let mut matched = compare_diagnostic_suite(&repository, "diagnostics");
    matched += compare_diagnostic_suite(&repository, "typer");
    matched += compare_diagnostic_suite(&repository, "trait_impl");
    matched += compare_diagnostic_suite(&repository, "struct_type");
    matched += compare_e2e(&repository);
    matched += compare_modules(&repository, &temporary);
    matched += compare_module_diagnostics(&repository, &temporary);
    matched += compare_crashers(&repository, &temporary);
    assert!(matched > 0);
}
