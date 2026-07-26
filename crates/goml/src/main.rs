use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::ffi::OsString;
use std::fs;
use std::io::{Read, Write};
use std::path::{Component, Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::Mutex;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::thread;
use std::time::{Duration, Instant};

use anyhow::{Context, anyhow, bail};
use clap::{Args, Parser, Subcommand, ValueEnum};
use goml_project::ENTRY_FUNCTION;
use goml_project::config::{
    DEFAULT_TARGET_DIR, ensure_goml_home_layout, find_module_root, goml_bin_dir, goml_cache_dir,
    goml_home_dir, goml_lib_dir, goml_std_dir, load_module_manifest, validate_manifest_target_dir,
};
use goml_project::registry::{
    ModuleCoord, ModuleRequirement, Registry, cached_registry_dir, default_registry_url,
    load_or_create_user_config, user_config_path, validate_registry_consistency,
};
use toml_edit::{DocumentMut, Item, Table, value};

mod build_cache;
mod gomlc;

const DEFAULT_LIB_PACKAGE: &str = "lib";

#[derive(Parser, Debug)]
#[command(name = "goml", arg_required_else_help = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    New(NewArgs),
    Check(CheckCommandArgs),
    Build(ProjectCommandArgs),
    Run(RunCommandArgs),
    Test(TestCommandArgs),
    Update(RegistryCommandArgs),
    Add(AddArgs),
    Remove(RemoveArgs),
    Home,
    Version,
    #[command(hide = true)]
    Compiler(CompilerCompatArgs),
}

#[derive(Args, Debug)]
struct RegistryCommandArgs {
    #[arg(long = "local-registry")]
    local_registry: Option<PathBuf>,
}

#[derive(Args, Debug)]
struct AddArgs {
    dependency: String,
    #[arg(long = "local-registry")]
    local_registry: Option<PathBuf>,
}

#[derive(Args, Debug)]
struct RemoveArgs {
    dependency: String,
    #[arg(long = "local-registry")]
    local_registry: Option<PathBuf>,
}

#[derive(Args, Debug, Clone)]
struct ProjectCommandArgs {
    #[arg(long = "target-dir")]
    target_dir: Option<PathBuf>,
    #[arg(long = "dry-run")]
    dry_run: bool,
    #[arg(long = "compiler")]
    compiler: Option<PathBuf>,
}

#[derive(Args, Debug, Clone)]
struct CheckCommandArgs {
    #[command(flatten)]
    project: ProjectCommandArgs,
    #[arg(long)]
    tests: bool,
}

#[derive(Args, Debug, Clone)]
struct RunCommandArgs {
    #[command(flatten)]
    project: ProjectCommandArgs,
    #[arg(default_value = ".")]
    target: PathBuf,
    #[arg(last = true, allow_hyphen_values = true, value_name = "ARGS")]
    args: Vec<OsString>,
}

#[derive(Args, Debug, Clone)]
struct TestCommandArgs {
    #[arg(value_name = "FILTER")]
    filter: Option<String>,
    #[arg(long = "target-dir")]
    target_dir: Option<PathBuf>,
    #[arg(long = "dry-run")]
    dry_run: bool,
    #[arg(long = "compiler")]
    compiler: Option<PathBuf>,
    #[arg(long)]
    list: bool,
    #[arg(long, conflicts_with = "include_ignored")]
    ignored: bool,
    #[arg(long)]
    include_ignored: bool,
    #[arg(long)]
    nocapture: bool,
    #[arg(long, value_enum, default_value_t = TestOutputFormat::Text)]
    format: TestOutputFormat,
    #[arg(long, default_value = "30s", value_parser = parse_duration)]
    timeout: Duration,
    #[arg(long, default_value_t = 1, value_parser = parse_positive_usize)]
    jobs: usize,
    #[arg(long, value_enum, default_value_t = TestKind::All)]
    kind: TestKind,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum TestOutputFormat {
    Text,
    Json,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum TestKind {
    Internal,
    External,
    All,
}

#[derive(Args, Debug)]
struct NewArgs {
    project_name: String,
    #[arg(long, default_value = ".")]
    path: PathBuf,
}

#[derive(Args, Debug)]
#[command(trailing_var_arg = true)]
struct CompilerCompatArgs {
    #[arg(required = true, allow_hyphen_values = true)]
    args: Vec<OsString>,
}

struct ProjectContext {
    module_dir: PathBuf,
    module_path: String,
    entry_path: Option<PathBuf>,
    target_role: Option<ProjectTargetRole>,
    dependencies: BTreeMap<String, String>,
    artifacts: ArtifactLayout,
}

impl ProjectContext {
    fn entry_path(&self) -> anyhow::Result<&Path> {
        self.entry_path
            .as_deref()
            .ok_or_else(|| anyhow!("project command requires a package target"))
    }

    fn target_role(&self) -> anyhow::Result<&ProjectTargetRole> {
        self.target_role
            .as_ref()
            .ok_or_else(|| anyhow!("project command requires a package target"))
    }
}

#[derive(Clone)]
struct ArtifactLayout {
    root: PathBuf,
}

impl ArtifactLayout {
    fn new(root: PathBuf) -> Self {
        Self { root }
    }

    fn check_root(&self) -> PathBuf {
        self.root.join("check")
    }

    fn build_root(&self) -> PathBuf {
        self.root.join("build")
    }

    fn test_internal_root(&self) -> PathBuf {
        self.root.join("test").join("internal")
    }

    fn test_external_root(&self) -> PathBuf {
        self.root.join("test").join("external")
    }

    fn binary(&self, module_path: &str, entry_package: &str) -> anyhow::Result<PathBuf> {
        let name = entry_package.rsplit("::").next().unwrap_or(entry_package);
        let mut output = self.root.join("bin");
        if entry_package != module_path {
            let relative_package = entry_package
                .strip_prefix(module_path)
                .and_then(|value| value.strip_prefix("::"))
                .ok_or_else(|| {
                    anyhow!("entry package {entry_package} is outside module {module_path}")
                })?;
            output.extend(relative_package.split("::"));
        }
        Ok(output.join(format!("{name}{}", std::env::consts::EXE_SUFFIX)))
    }

    fn package_go(&self, root: &Path, package: &str) -> PathBuf {
        local_artifact_base(root, package).with_extension("go")
    }

    fn test_manifest(&self, root: &Path, package: &str) -> PathBuf {
        local_artifact_base(root, package)
            .parent()
            .expect("package artifact must have a parent")
            .join("tests.json")
    }

    fn test_runner(&self, root: &Path, package: &str) -> PathBuf {
        local_artifact_base(root, package)
            .parent()
            .expect("package artifact must have a parent")
            .join(format!("runner{}", std::env::consts::EXE_SUFFIX))
    }
}

enum ProjectTargetRole {
    Production,
    InternalTest,
    ExternalTest,
}

struct PackageCompilerCommand {
    package: String,
    input_files: Vec<PathBuf>,
    interface_files: Vec<PathBuf>,
    output: PathBuf,
}

struct LinkCompilerCommand {
    input_cores: Vec<PathBuf>,
    entry_package: String,
    output: PathBuf,
}

struct TestLinkCompilerCommand {
    input_cores: Vec<PathBuf>,
    packages: Vec<String>,
    output: PathBuf,
    manifest: PathBuf,
}

struct ProjectCommandPlan {
    commands: Vec<PlannedCompilerCommand>,
}

struct ProjectBuildCommandPlan {
    compiler: ProjectCommandPlan,
    go: Vec<GoBuildCommand>,
}

struct GoBuildCommand {
    input: PathBuf,
    output: PathBuf,
}

impl GoBuildCommand {
    fn display(&self) -> String {
        format!(
            "go build -o {} {}",
            shell_escape(&self.output.to_string_lossy()),
            shell_escape(&self.input.to_string_lossy())
        )
    }
}

struct ProjectTestCommandPlan {
    commands: Vec<PlannedCompilerCommand>,
    groups: Vec<TestRunGroup>,
}

#[derive(Clone)]
struct TestRunGroup {
    kind: TestKind,
    go_output: PathBuf,
    manifest: PathBuf,
    runner: PathBuf,
}

#[derive(Clone)]
struct BuildPackage {
    input_files: Vec<PathBuf>,
    imports: HashSet<String>,
    output: PathBuf,
}

#[derive(Clone, Default)]
struct ExternalPackagesPlan {
    packages: HashMap<String, BuildPackage>,
    order: Vec<String>,
    declared_names: HashMap<String, String>,
}

#[derive(Clone, Copy)]
enum ProjectStage {
    Check,
    TestCheck,
    Build,
    Test,
}

impl ProjectStage {
    fn output_root(self, artifacts: &ArtifactLayout) -> PathBuf {
        match self {
            Self::Check | Self::TestCheck => artifacts.check_root(),
            Self::Build => artifacts.build_root(),
            Self::Test => artifacts.test_internal_root(),
        }
    }

    fn label(self) -> &'static str {
        match self {
            Self::Check => "project check",
            Self::TestCheck => "project test check",
            Self::Build => "project build",
            Self::Test => "project test",
        }
    }
}

enum PlannedCompilerCommand {
    Check(PackageCompilerCommand),
    TestCheck(PackageCompilerCommand),
    Build(PackageCompilerCommand),
    TestBuild(PackageCompilerCommand),
    Link(LinkCompilerCommand),
    TestLink(TestLinkCompilerCommand),
}

impl PlannedCompilerCommand {
    fn to_args(&self) -> Vec<OsString> {
        match self {
            PlannedCompilerCommand::Check(cmd) => package_command_args("check", cmd),
            PlannedCompilerCommand::TestCheck(cmd) => package_command_args("test-check", cmd),
            PlannedCompilerCommand::Build(cmd) => package_command_args("build", cmd),
            PlannedCompilerCommand::TestBuild(cmd) => package_command_args("test-build", cmd),
            PlannedCompilerCommand::Link(cmd) => {
                let mut args = vec![OsString::from("link"), OsString::from("--input")];
                args.extend(
                    cmd.input_cores
                        .iter()
                        .map(|path| path.clone().into_os_string()),
                );
                args.push(OsString::from("--output"));
                args.push(cmd.output.clone().into_os_string());
                args.push(OsString::from("--entry"));
                args.push(OsString::from(&cmd.entry_package));
                args
            }
            PlannedCompilerCommand::TestLink(cmd) => {
                let mut args = vec![OsString::from("test-link"), OsString::from("--input")];
                args.extend(
                    cmd.input_cores
                        .iter()
                        .map(|path| path.clone().into_os_string()),
                );
                args.push(OsString::from("--output"));
                args.push(cmd.output.clone().into_os_string());
                args.push(OsString::from("--manifest"));
                args.push(cmd.manifest.clone().into_os_string());
                for package in &cmd.packages {
                    args.push(OsString::from("--package"));
                    args.push(OsString::from(package));
                }
                args
            }
        }
    }

    fn display(&self) -> String {
        let args = self.to_args();
        let mut parts = Vec::with_capacity(args.len() + 1);
        parts.push("gomlc".to_string());
        parts.extend(args.iter().map(|arg| shell_escape(&arg.to_string_lossy())));
        parts.join(" ")
    }

    fn cache_inputs(&self) -> Vec<build_cache::CacheInput<'_>> {
        match self {
            Self::Check(command)
            | Self::TestCheck(command)
            | Self::Build(command)
            | Self::TestBuild(command) => command
                .input_files
                .iter()
                .map(|path| build_cache::CacheInput::new("source", path))
                .chain(
                    command
                        .interface_files
                        .iter()
                        .map(|path| build_cache::CacheInput::new("interface", path)),
                )
                .collect(),
            Self::Link(command) => command
                .input_cores
                .iter()
                .map(|path| build_cache::CacheInput::new("core", path))
                .collect(),
            Self::TestLink(command) => command
                .input_cores
                .iter()
                .map(|path| build_cache::CacheInput::new("core", path))
                .collect(),
        }
    }

    fn cache_kind(&self) -> &'static str {
        match self {
            Self::Check(_) => "check",
            Self::TestCheck(_) => "test-check",
            Self::Build(_) => "build",
            Self::TestBuild(_) => "test-build",
            Self::Link(_) => "link",
            Self::TestLink(_) => "test-link",
        }
    }

    fn cache_outputs(&self) -> Vec<PathBuf> {
        match self {
            Self::Check(command) | Self::TestCheck(command) => {
                vec![command.output.with_extension("interface")]
            }
            Self::Build(command) | Self::TestBuild(command) => vec![
                command.output.with_extension("interface"),
                command.output.with_extension("core"),
            ],
            Self::Link(command) => vec![command.output.clone()],
            Self::TestLink(command) => vec![command.output.clone(), command.manifest.clone()],
        }
    }

    fn cache_anchor(&self) -> &Path {
        match self {
            Self::Check(command)
            | Self::TestCheck(command)
            | Self::Build(command)
            | Self::TestBuild(command) => &command.output,
            Self::Link(command) => &command.output,
            Self::TestLink(command) => &command.output,
        }
    }
}

fn package_command_args(kind: &str, cmd: &PackageCompilerCommand) -> Vec<OsString> {
    let mut args = vec![
        OsString::from(kind),
        OsString::from("--package"),
        OsString::from(&cmd.package),
    ];
    for input in cmd.input_files.iter() {
        args.push(OsString::from("--input"));
        args.push(input.clone().into_os_string());
    }
    for interface in cmd.interface_files.iter() {
        args.push(OsString::from("--interface-path"));
        args.push(interface.clone().into_os_string());
    }
    args.push(OsString::from("--output"));
    args.push(cmd.output.clone().into_os_string());
    args
}

fn main() {
    if let Err(err) = run_cli() {
        eprintln!("{err}");
        std::process::exit(1);
    }
}

fn run_cli() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::New(args) => execute_new(args),
        Commands::Check(args) => execute_project_check(args),
        Commands::Build(args) => execute_project_build(args),
        Commands::Run(args) => execute_project_run(args),
        Commands::Test(args) => execute_project_test(args),
        Commands::Update(args) => execute_update(args),
        Commands::Add(args) => execute_add(args),
        Commands::Remove(args) => execute_remove(args),
        Commands::Home => execute_home(),
        Commands::Version => execute_version(),
        Commands::Compiler(args) => execute_compiler_compat(args),
    }
}

fn execute_home() -> anyhow::Result<()> {
    ensure_goml_home_layout().map_err(anyhow::Error::msg)?;
    println!(
        "GOML_HOME={}",
        goml_home_dir().map_err(anyhow::Error::msg)?.display()
    );
    println!(
        "bin={}",
        goml_bin_dir().map_err(anyhow::Error::msg)?.display()
    );
    println!(
        "lib={}",
        goml_lib_dir().map_err(anyhow::Error::msg)?.display()
    );
    println!(
        "std={}",
        goml_std_dir().map_err(anyhow::Error::msg)?.display()
    );
    println!(
        "cache={}",
        goml_cache_dir().map_err(anyhow::Error::msg)?.display()
    );
    Ok(())
}

fn execute_version() -> anyhow::Result<()> {
    match (option_env!("GOML_GIT_HASH"), option_env!("GOML_GIT_DATE")) {
        (Some(hash), Some(date)) => println!("goml {} ({hash} {date})", env!("CARGO_PKG_VERSION")),
        _ => println!("goml {}", env!("CARGO_PKG_VERSION")),
    }
    Ok(())
}

fn execute_new(args: NewArgs) -> anyhow::Result<()> {
    if !is_valid_identifier(&args.project_name) {
        bail!(
            "invalid project name `{}`: expected identifier [A-Za-z_][A-Za-z0-9_]*",
            args.project_name
        );
    }
    goml_project::config::validate_project_module_path(&args.project_name)
        .map_err(anyhow::Error::msg)?;

    let project_dir = args.path.join(&args.project_name);
    ensure_project_dir_ready(&project_dir)?;
    let lib_dir = project_dir.join(DEFAULT_LIB_PACKAGE);
    fs::create_dir_all(&lib_dir)
        .with_context(|| format!("failed to create directory {}", lib_dir.display()))?;

    write_file_with_dirs(
        &project_dir.join("goml.toml"),
        &render_root_goml_toml(&args.project_name),
    )?;
    write_file_with_dirs(&project_dir.join(".gitignore"), &render_gitignore())?;
    write_file_with_dirs(
        &project_dir.join("main.gom"),
        &render_main_gom(&args.project_name),
    )?;
    write_file_with_dirs(&lib_dir.join("lib.gom"), &render_lib_gom())?;

    println!("Created project at {}", project_dir.display());
    println!("Next steps:");
    println!("  cd {}", project_dir.display());
    println!("  goml check");
    println!("  goml build");

    Ok(())
}

fn execute_update(args: RegistryCommandArgs) -> anyhow::Result<()> {
    ensure_goml_home_layout().map_err(anyhow::Error::msg)?;
    let source = registry_source(args.local_registry.as_deref())?;
    let cache_dir = cached_registry_dir().map_err(anyhow::Error::msg)?;
    if let Some(parent) = cache_dir.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create directory {}", parent.display()))?;
    }

    if cache_dir.exists() {
        let git_dir = cache_dir.join(".git");
        if !git_dir.exists() {
            bail!(
                "cached registry path {} exists but is not a git repository",
                cache_dir.display()
            );
        }
        run_git_command([
            "-C",
            cache_dir.to_string_lossy().as_ref(),
            "remote",
            "set-url",
            "origin",
            source.as_str(),
        ])?;
        run_git_command([
            "-C",
            cache_dir.to_string_lossy().as_ref(),
            "pull",
            "--ff-only",
        ])?;
    } else {
        run_git_command([
            "clone",
            source.as_str(),
            cache_dir.to_string_lossy().as_ref(),
        ])?;
    }

    let registry = Registry::load(&cache_dir).map_err(anyhow::Error::msg)?;
    validate_registry_consistency(&registry).map_err(anyhow::Error::msg)?;
    println!("updated registry cache at {}", cache_dir.display());
    Ok(())
}

fn execute_add(args: AddArgs) -> anyhow::Result<()> {
    let module_dir = locate_module_root_from_cwd()?;
    let manifest_path = module_dir.join("goml.toml");
    let registry = load_registry_for_command(args.local_registry.as_deref())?;
    let (coord, requested_version) = parse_dependency_spec(&args.dependency)?;
    let version = if let Some(version) = requested_version {
        let requirement = ModuleRequirement {
            coord: coord.clone(),
            min_version: version,
        };
        registry
            .select_minimum_version(&requirement)
            .map_err(anyhow::Error::msg)?
    } else {
        registry
            .latest_version(&coord)
            .map_err(anyhow::Error::msg)?
    };
    upsert_dependency(&manifest_path, &coord, &version.display())?;
    println!("added {} = {}", coord.display(), version.display());
    Ok(())
}

fn execute_remove(args: RemoveArgs) -> anyhow::Result<()> {
    let module_dir = locate_module_root_from_cwd()?;
    let manifest_path = module_dir.join("goml.toml");
    let coord = ModuleCoord::parse(args.dependency.trim()).map_err(anyhow::Error::msg)?;
    remove_dependency(&manifest_path, &coord)?;
    println!("removed {}", coord.display());
    Ok(())
}

fn is_valid_identifier(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !(first == '_' || first.is_ascii_alphabetic()) {
        return false;
    }
    chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
}

fn parse_dependency_spec(
    input: &str,
) -> anyhow::Result<(ModuleCoord, Option<goml_project::registry::SemVer>)> {
    let trimmed = input.trim();
    let (coord, version) = match trimmed.split_once('@') {
        Some((coord, version)) => (coord, Some(version)),
        None => (trimmed, None),
    };
    let coord = ModuleCoord::parse(coord).map_err(anyhow::Error::msg)?;
    let version = version
        .map(goml_project::registry::SemVer::parse)
        .transpose()
        .map_err(anyhow::Error::msg)?;
    Ok((coord, version))
}

fn registry_source(local_registry: Option<&Path>) -> anyhow::Result<String> {
    if let Some(path) = local_registry {
        return Ok(path.to_string_lossy().into_owned());
    }
    if !user_config_path().map_err(anyhow::Error::msg)?.exists() {
        let _ = load_or_create_user_config().map_err(anyhow::Error::msg)?;
    }
    default_registry_url().map_err(anyhow::Error::msg)
}

fn load_registry_for_command(local_registry: Option<&Path>) -> anyhow::Result<Registry> {
    if let Some(path) = local_registry {
        let registry = Registry::load(path).map_err(anyhow::Error::msg)?;
        validate_registry_consistency(&registry).map_err(anyhow::Error::msg)?;
        return Ok(registry);
    }

    let cache_dir = cached_registry_dir().map_err(anyhow::Error::msg)?;
    if !cache_dir.exists() {
        bail!(
            "registry cache not found at {}; run `goml update` or use --local-registry",
            cache_dir.display()
        );
    }
    let registry = Registry::load(&cache_dir).map_err(anyhow::Error::msg)?;
    validate_registry_consistency(&registry).map_err(anyhow::Error::msg)?;
    Ok(registry)
}

fn locate_module_root_from_cwd() -> anyhow::Result<PathBuf> {
    let cwd = std::env::current_dir().context("failed to read current directory")?;
    if let Some((module_dir, _config)) = find_module_root(&cwd).map_err(anyhow::Error::msg)? {
        return Ok(module_dir);
    }
    bail!(
        "no goml.toml with [module] section found in ancestors of {}",
        cwd.display()
    )
}

fn load_manifest_document(path: &Path) -> anyhow::Result<DocumentMut> {
    let text =
        fs::read_to_string(path).with_context(|| format!("failed to read {}", path.display()))?;
    text.parse::<DocumentMut>()
        .map_err(|err| anyhow!("failed to parse {}: {}", path.display(), err))
}

fn upsert_dependency(path: &Path, coord: &ModuleCoord, version: &str) -> anyhow::Result<()> {
    let mut doc = load_manifest_document(path)?;
    ensure_dependencies_table(&mut doc).insert(&coord.display(), value(version));
    fs::write(path, doc.to_string()).with_context(|| format!("failed to write {}", path.display()))
}

fn remove_dependency(path: &Path, coord: &ModuleCoord) -> anyhow::Result<()> {
    let mut doc = load_manifest_document(path)?;
    let dependencies_item = &mut doc["dependencies"];
    if let Some(table) = dependencies_item.as_table_like_mut() {
        table.remove(coord.display().as_str());
        if table.is_empty() {
            *dependencies_item = Item::None;
        }
    }
    fs::write(path, doc.to_string()).with_context(|| format!("failed to write {}", path.display()))
}

fn ensure_dependencies_table(doc: &mut DocumentMut) -> &mut Table {
    if !doc.as_table().contains_key("dependencies") || !doc["dependencies"].is_table() {
        doc["dependencies"] = Item::Table(Table::new());
    }
    doc["dependencies"]
        .as_table_mut()
        .expect("dependencies must be a table")
}

fn run_git_command<const N: usize>(args: [&str; N]) -> anyhow::Result<()> {
    let status = Command::new("git")
        .args(args)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .context("failed to execute git")?;
    if !status.success() {
        bail!("git command failed");
    }
    Ok(())
}

fn ensure_project_dir_ready(path: &Path) -> anyhow::Result<()> {
    if !path.exists() {
        fs::create_dir_all(path)
            .with_context(|| format!("failed to create directory {}", path.display()))?;
        return Ok(());
    }

    let mut entries = fs::read_dir(path)
        .with_context(|| format!("failed to read directory {}", path.display()))?;
    if entries.next().is_some() {
        bail!(
            "target directory {} already exists and is not empty",
            path.display()
        );
    }
    Ok(())
}

fn write_file_with_dirs(path: &Path, content: &str) -> anyhow::Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create directory {}", parent.display()))?;
    }
    fs::write(path, content).with_context(|| format!("failed to write {}", path.display()))?;
    Ok(())
}

fn render_root_goml_toml(project_name: &str) -> String {
    format!("[module]\npath = \"{project_name}\"\n")
}

fn render_gitignore() -> String {
    format!("/{DEFAULT_TARGET_DIR}/\n")
}

fn render_main_gom(project_name: &str) -> String {
    format!(
        r#"package main;

use {project_name}::{DEFAULT_LIB_PACKAGE};

fn {ENTRY_FUNCTION}() -> unit {{
    println({DEFAULT_LIB_PACKAGE}::message())
}}
"#
    )
}

fn render_lib_gom() -> String {
    r#"package lib;

pub fn message() -> string {
    "hello from lib"
}
"#
    .to_string()
}

fn execute_compiler_compat(args: CompilerCompatArgs) -> anyhow::Result<()> {
    eprintln!("warning: `goml compiler` is deprecated; invoke `gomlc` directly");
    let executable = gomlc::resolve(None)?;
    gomlc::verify(&executable)?;
    let status = gomlc::execute(&executable, &args.args, None)?;
    if !status.success() {
        bail!("gomlc failed with status {status}");
    }
    Ok(())
}

fn execute_project_check(args: CheckCommandArgs) -> anyhow::Result<()> {
    let project = load_current_project(args.project.target_dir.as_deref())?;
    let plan = build_module_check_plan(&project, args.tests)?;
    execute_planned_commands(
        &project.module_dir,
        &plan.commands,
        args.project.dry_run,
        args.project.compiler.as_deref(),
    )
}

fn execute_project_build(args: ProjectCommandArgs) -> anyhow::Result<()> {
    let project = load_current_project(args.target_dir.as_deref())?;
    let plan = build_module_build_plan(&project)?;
    execute_project_build_plan(
        &project.module_dir,
        &plan,
        args.compiler.as_deref(),
        args.dry_run,
    )
}

fn execute_project_run(args: RunCommandArgs) -> anyhow::Result<()> {
    let project = load_project(&args.target, args.project.target_dir.as_deref())?;
    if !matches!(project.target_role()?, ProjectTargetRole::Production) {
        bail!("test-only targets cannot be run directly; use `goml test`");
    }
    let plan = build_project_build_plan(&project)?;
    execute_project_build_plan(
        &project.module_dir,
        &plan,
        args.project.compiler.as_deref(),
        args.project.dry_run,
    )?;
    let executable = plan
        .go
        .first()
        .ok_or_else(|| anyhow!("run build plan omitted executable"))?;
    if args.project.dry_run {
        println!(
            "{}",
            display_command_path_and_args(&executable.output, &args.args)
        );
        return Ok(());
    }
    let executable = absolute_from_module(&project.module_dir, &executable.output);
    let status = Command::new(&executable)
        .args(&args.args)
        .status()
        .with_context(|| format!("failed to execute {}", executable.display()))?;
    if !status.success() {
        std::process::exit(status.code().unwrap_or(1));
    }
    Ok(())
}

fn execute_project_test(args: TestCommandArgs) -> anyhow::Result<()> {
    let project = load_current_project(args.target_dir.as_deref())?;
    let plan = build_module_test_plan(&project, args.kind)?;
    execute_planned_commands(
        &project.module_dir,
        &plan.commands,
        args.dry_run,
        args.compiler.as_deref(),
    )?;
    if args.dry_run {
        return Ok(());
    }
    execute_test_runner(&project.module_dir, &plan.groups, &args)
}

fn build_module_graph_plan(
    project: &ProjectContext,
    stage: ProjectStage,
) -> anyhow::Result<(
    ProjectCommandPlan,
    goml_project::package_graph::PackageGraph,
)> {
    let output_root = stage.output_root(&project.artifacts);
    let external = build_external_packages_plan(project, &output_root)?;
    let external_imports =
        goml_project::package_graph::ExternalImports::new(external.declared_names.clone());
    let graph = goml_project::package_graph::discover_all_project_packages(
        &project.module_dir,
        &external_imports,
    )
    .map_err(|err| anyhow!("{} failed: {}", stage.label(), err))?;
    let plan = build_graph_plan(project, stage, external, graph.clone())?;
    Ok((plan, graph))
}

fn build_module_check_plan(
    project: &ProjectContext,
    include_all_tests: bool,
) -> anyhow::Result<ProjectCommandPlan> {
    let (mut plan, _) = build_module_graph_plan(project, ProjectStage::Check)?;
    if include_all_tests {
        plan.commands
            .extend(build_all_module_test_check_commands(project)?);
    }
    Ok(plan)
}

fn build_all_module_test_check_commands(
    project: &ProjectContext,
) -> anyhow::Result<Vec<PlannedCompilerCommand>> {
    let check_root = project.artifacts.check_root();
    let external = build_external_packages_plan(project, &check_root)?;
    let external_imports =
        goml_project::package_graph::ExternalImports::new(external.declared_names.clone());
    let tests = goml_project::package_graph::discover_all_project_test_plan(
        &project.module_dir,
        &external_imports,
    )
    .map_err(|err| anyhow!("project test check failed: {}", err))?;
    let mut commands = Vec::new();
    for graph in tests.internal {
        commands.extend(
            build_graph_plan(project, ProjectStage::TestCheck, external.clone(), graph)?.commands,
        );
    }
    for test in tests.external {
        commands.extend(
            build_graph_plan(
                project,
                ProjectStage::TestCheck,
                external.clone(),
                test.graph,
            )?
            .commands,
        );
    }
    Ok(commands)
}

fn build_module_build_plan(project: &ProjectContext) -> anyhow::Result<ProjectBuildCommandPlan> {
    let (mut compiler, graph) = build_module_graph_plan(project, ProjectStage::Build)?;
    let link = compiler
        .commands
        .iter()
        .find_map(|command| match command {
            PlannedCompilerCommand::Link(command) => Some(command.input_cores.clone()),
            _ => None,
        })
        .ok_or_else(|| anyhow!("project build plan is missing core outputs"))?;
    compiler
        .commands
        .retain(|command| !matches!(command, PlannedCompilerCommand::Link(_)));
    let mut entries = graph
        .packages
        .values()
        .filter(|package| package.declared_name == "main")
        .map(|package| package.name.clone())
        .collect::<Vec<_>>();
    entries.sort();
    let mut go = Vec::new();
    for entry_package in entries {
        let input = project
            .artifacts
            .package_go(&project.artifacts.build_root(), &entry_package);
        compiler
            .commands
            .push(PlannedCompilerCommand::Link(LinkCompilerCommand {
                input_cores: link.clone(),
                entry_package: entry_package.clone(),
                output: input.clone(),
            }));
        go.push(GoBuildCommand {
            input,
            output: project
                .artifacts
                .binary(&project.module_path, &entry_package)?,
        });
    }
    Ok(ProjectBuildCommandPlan { compiler, go })
}

fn build_module_test_plan(
    project: &ProjectContext,
    requested_kind: TestKind,
) -> anyhow::Result<ProjectTestCommandPlan> {
    let mut commands = build_module_graph_plan(project, ProjectStage::Check)?
        .0
        .commands;
    let mut groups = Vec::new();
    if matches!(requested_kind, TestKind::Internal | TestKind::All) {
        let root = project.artifacts.test_internal_root();
        let external = build_external_packages_plan(project, &root)?;
        let imports =
            goml_project::package_graph::ExternalImports::new(external.declared_names.clone());
        let tests = goml_project::package_graph::discover_all_project_test_plan(
            &project.module_dir,
            &imports,
        )
        .map_err(|err| anyhow!("project test failed: {}", err))?;
        for graph in tests.internal {
            let package = graph.entry_package.clone();
            commands.extend(
                build_graph_plan(project, ProjectStage::Test, external.clone(), graph)?.commands,
            );
            groups.push(internal_test_run_group(&project.artifacts, &package));
        }
    }
    if matches!(requested_kind, TestKind::External | TestKind::All) {
        let root = project.artifacts.test_external_root();
        let external = build_external_packages_plan(project, &root)?;
        let imports =
            goml_project::package_graph::ExternalImports::new(external.declared_names.clone());
        let tests = goml_project::package_graph::discover_all_project_test_plan(
            &project.module_dir,
            &imports,
        )
        .map_err(|err| anyhow!("project test failed: {}", err))?;
        for test in tests.external {
            let package = test.graph.entry_package.clone();
            let test_packages = HashSet::from([package.clone()]);
            let link_packages = vec![package.clone(), test.target_package];
            let plan = build_external_test_graph_plan(
                project,
                external.clone(),
                test.graph,
                test_packages,
                link_packages,
            )?;
            commands.extend(plan.commands);
            groups.extend(plan.groups);
        }
    }
    Ok(ProjectTestCommandPlan { commands, groups })
}

fn build_project_build_plan(project: &ProjectContext) -> anyhow::Result<ProjectBuildCommandPlan> {
    let compiler = build_project_plan(project, ProjectStage::Build)?;
    let entry_package = compiler
        .commands
        .iter()
        .find_map(|command| match command {
            PlannedCompilerCommand::Link(command) => Some(command.entry_package.as_str()),
            _ => None,
        })
        .ok_or_else(|| anyhow!("project build plan is missing a link command"))?;
    let go = GoBuildCommand {
        input: project
            .artifacts
            .package_go(&project.artifacts.build_root(), entry_package),
        output: project
            .artifacts
            .binary(&project.module_path, entry_package)?,
    };
    Ok(ProjectBuildCommandPlan {
        compiler,
        go: vec![go],
    })
}

fn build_external_test_graph_plan(
    project: &ProjectContext,
    external: ExternalPackagesPlan,
    graph: goml_project::package_graph::PackageGraph,
    test_packages: HashSet<String>,
    link_packages: Vec<String>,
) -> anyhow::Result<ProjectTestCommandPlan> {
    let entry_package = graph.entry_package.clone();
    let output_root = project.artifacts.test_external_root();
    let order = goml_project::package_graph::topo_sort_packages(&graph)
        .map_err(|err| anyhow!("project test failed: {}", err))?;
    let mut packages = external.packages;
    let mut build_order = external.order;
    for package_name in order {
        if packages.contains_key(&package_name) {
            bail!(
                "project test failed: package {} conflicts with an external dependency",
                package_name
            );
        }
        let package = graph
            .packages
            .get(&package_name)
            .ok_or_else(|| anyhow!("project test failed: missing package {}", package_name))?;
        packages.insert(
            package_name.clone(),
            BuildPackage {
                input_files: sorted_relative_inputs(&project.module_dir, &package.files),
                imports: package.imports.clone(),
                output: local_artifact_base(&output_root, &package_name),
            },
        );
        build_order.push(package_name);
    }
    let mut commands = Vec::new();
    let mut interface_outputs = HashMap::new();
    let mut core_outputs = Vec::new();
    for package_name in build_order {
        let package = packages
            .get(&package_name)
            .ok_or_else(|| anyhow!("project test failed: missing package {}", package_name))?;
        let command = PackageCompilerCommand {
            package: package_name.clone(),
            input_files: package.input_files.clone(),
            interface_files: package_interface_inputs(
                &package_name,
                &package.imports,
                &packages,
                &interface_outputs,
                "project test",
            )?,
            output: package.output.clone(),
        };
        if test_packages.contains(&package_name) {
            commands.push(PlannedCompilerCommand::TestBuild(command));
        } else {
            commands.push(PlannedCompilerCommand::Build(command));
        }
        interface_outputs.insert(package_name, package.output.with_extension("interface"));
        core_outputs.push(package.output.with_extension("core"));
    }
    commands.push(PlannedCompilerCommand::TestLink(TestLinkCompilerCommand {
        input_cores: core_outputs,
        packages: link_packages,
        output: project
            .artifacts
            .package_go(&project.artifacts.test_external_root(), &entry_package),
        manifest: project
            .artifacts
            .test_manifest(&project.artifacts.test_external_root(), &entry_package),
    }));
    Ok(ProjectTestCommandPlan {
        commands,
        groups: vec![external_test_run_group(&project.artifacts, &entry_package)],
    })
}

fn internal_test_run_group(artifacts: &ArtifactLayout, package: &str) -> TestRunGroup {
    TestRunGroup {
        kind: TestKind::Internal,
        go_output: artifacts.package_go(&artifacts.test_internal_root(), package),
        manifest: artifacts.test_manifest(&artifacts.test_internal_root(), package),
        runner: artifacts.test_runner(&artifacts.test_internal_root(), package),
    }
}

fn external_test_run_group(artifacts: &ArtifactLayout, package: &str) -> TestRunGroup {
    TestRunGroup {
        kind: TestKind::External,
        go_output: artifacts.package_go(&artifacts.test_external_root(), package),
        manifest: artifacts.test_manifest(&artifacts.test_external_root(), package),
        runner: artifacts.test_runner(&artifacts.test_external_root(), package),
    }
}

fn build_project_plan(
    project: &ProjectContext,
    stage: ProjectStage,
) -> anyhow::Result<ProjectCommandPlan> {
    let output_root = stage.output_root(&project.artifacts);
    let external = build_external_packages_plan(project, &output_root)?;
    let external_imports =
        goml_project::package_graph::ExternalImports::new(external.declared_names.clone());
    let graph = match stage {
        ProjectStage::Test => goml_project::package_graph::discover_project_test_packages(
            &project.module_dir,
            project.entry_path()?,
            &external_imports,
        ),
        ProjectStage::TestCheck => goml_project::package_graph::discover_project_test_packages(
            &project.module_dir,
            project.entry_path()?,
            &external_imports,
        ),
        ProjectStage::Check | ProjectStage::Build => {
            goml_project::package_graph::discover_project_packages(
                &project.module_dir,
                project.entry_path()?,
                &external_imports,
            )
        }
    }
    .map_err(|err| anyhow!("{} failed: {}", stage.label(), err))?;
    build_graph_plan(project, stage, external, graph)
}

fn build_graph_plan(
    project: &ProjectContext,
    stage: ProjectStage,
    external: ExternalPackagesPlan,
    graph: goml_project::package_graph::PackageGraph,
) -> anyhow::Result<ProjectCommandPlan> {
    let output_root = stage.output_root(&project.artifacts);
    let local_order = goml_project::package_graph::topo_sort_packages(&graph)
        .map_err(|err| anyhow!("{} failed: {}", stage.label(), err))?;

    let mut packages = external.packages;
    let mut order = external.order;
    for package_name in local_order {
        if packages.contains_key(&package_name) {
            bail!(
                "{} failed: package {} conflicts with an external dependency",
                stage.label(),
                package_name
            );
        }
        let package = graph
            .packages
            .get(&package_name)
            .ok_or_else(|| anyhow!("{} failed: missing package {}", stage.label(), package_name))?;
        packages.insert(
            package_name.clone(),
            BuildPackage {
                input_files: sorted_relative_inputs(&project.module_dir, &package.files),
                imports: package.imports.clone(),
                output: local_artifact_base(&output_root, &package_name),
            },
        );
        order.push(package_name);
    }

    let mut commands = Vec::new();
    let mut interface_outputs = HashMap::new();
    let mut core_outputs = Vec::new();
    for package_name in order {
        let package = packages
            .get(&package_name)
            .ok_or_else(|| anyhow!("{} failed: missing package {}", stage.label(), package_name))?;
        let interface_files = package_interface_inputs(
            &package_name,
            &package.imports,
            &packages,
            &interface_outputs,
            stage.label(),
        )?;
        let command = PackageCompilerCommand {
            package: package_name.clone(),
            input_files: package.input_files.clone(),
            interface_files,
            output: package.output.clone(),
        };
        commands.push(match stage {
            ProjectStage::Check => PlannedCompilerCommand::Check(command),
            ProjectStage::TestCheck if package_name == graph.entry_package => {
                PlannedCompilerCommand::TestCheck(command)
            }
            ProjectStage::TestCheck => PlannedCompilerCommand::Check(command),
            ProjectStage::Build => PlannedCompilerCommand::Build(command),
            ProjectStage::Test if package_name == graph.entry_package => {
                PlannedCompilerCommand::TestBuild(command)
            }
            ProjectStage::Test => PlannedCompilerCommand::Build(command),
        });
        interface_outputs.insert(package_name, package.output.with_extension("interface"));
        if matches!(stage, ProjectStage::Build | ProjectStage::Test) {
            core_outputs.push(package.output.with_extension("core"));
        }
    }

    if matches!(stage, ProjectStage::Build) {
        let entry_package = graph.entry_package;
        commands.push(PlannedCompilerCommand::Link(LinkCompilerCommand {
            input_cores: core_outputs,
            entry_package: entry_package.clone(),
            output: project
                .artifacts
                .package_go(&project.artifacts.build_root(), &entry_package),
        }));
    } else if matches!(stage, ProjectStage::Test) {
        commands.push(PlannedCompilerCommand::TestLink(TestLinkCompilerCommand {
            input_cores: core_outputs,
            packages: vec![graph.entry_package.clone()],
            output: project.artifacts.package_go(
                &project.artifacts.test_internal_root(),
                &graph.entry_package,
            ),
            manifest: project.artifacts.test_manifest(
                &project.artifacts.test_internal_root(),
                &graph.entry_package,
            ),
        }));
    }
    Ok(ProjectCommandPlan { commands })
}

fn build_external_packages_plan(
    project: &ProjectContext,
    output_root: &Path,
) -> anyhow::Result<ExternalPackagesPlan> {
    if project.dependencies.is_empty() {
        return Ok(ExternalPackagesPlan::default());
    }
    let cache_dir = cached_registry_dir().map_err(anyhow::Error::msg)?;
    if !cache_dir.exists() {
        bail!(
            "registry cache not found at {}; run `goml update` first",
            cache_dir.display()
        );
    }
    let registry = Registry::load(&cache_dir).map_err(anyhow::Error::msg)?;
    validate_registry_consistency(&registry).map_err(anyhow::Error::msg)?;
    let resolved = goml_project::registry::resolve_dependencies(&registry, &project.dependencies)
        .map_err(anyhow::Error::msg)?;
    let module_order =
        goml_project::registry::topo_sort_modules(&resolved).map_err(anyhow::Error::msg)?;

    let mut plan = ExternalPackagesPlan::default();
    for coord in module_order {
        let module = resolved
            .modules
            .get(&coord)
            .ok_or_else(|| anyhow!("missing resolved module {}", coord.display()))?;
        let available =
            goml_project::package_graph::ExternalImports::new(plan.declared_names.clone());
        let graph = goml_project::package_graph::discover_dependency_module_packages(
            &module.root_dir,
            &available,
        )
        .map_err(|err| anyhow!("dependency {} failed: {}", coord.display(), err))?;
        let package_order = goml_project::package_graph::topo_sort_packages(&graph)
            .map_err(|err| anyhow!("dependency {} failed: {}", coord.display(), err))?;

        for package_name in graph.packages.keys() {
            if plan.declared_names.contains_key(package_name) {
                bail!(
                    "external package {} is provided by more than one module",
                    package_name
                );
            }
        }
        for package_name in package_order {
            let package = graph.packages.get(&package_name).ok_or_else(|| {
                anyhow!(
                    "dependency {} is missing package {}",
                    coord.display(),
                    package_name
                )
            })?;
            plan.declared_names
                .insert(package_name.clone(), package.declared_name.clone());
            plan.packages.insert(
                package_name.clone(),
                BuildPackage {
                    input_files: package.files.clone(),
                    imports: package.imports.clone(),
                    output: external_artifact_base(output_root, module, &package_name),
                },
            );
            plan.order.push(package_name);
        }
    }
    Ok(plan)
}

fn external_artifact_base(
    output_root: &Path,
    module: &goml_project::registry::ResolvedModule,
    package: &str,
) -> PathBuf {
    let mut path = output_root
        .join("deps")
        .join(&module.coord.owner)
        .join(&module.coord.module)
        .join(module.version.display())
        .join("pkg");
    for segment in package.split("::") {
        path.push(segment);
    }
    path.join(package.rsplit("::").next().unwrap_or(package))
}

fn local_artifact_base(output_root: &Path, package: &str) -> PathBuf {
    let mut path = output_root.join("pkg");
    for segment in package.split("::") {
        path.push(segment);
    }
    path.join(package.rsplit("::").next().unwrap_or(package))
}

fn package_interface_inputs(
    package_name: &str,
    imports: &HashSet<String>,
    packages: &HashMap<String, BuildPackage>,
    interface_outputs: &HashMap<String, PathBuf>,
    stage: &str,
) -> anyhow::Result<Vec<PathBuf>> {
    let mut pending = imports.iter().cloned().collect::<BTreeSet<_>>();
    let mut visited = HashSet::new();
    let mut outputs = Vec::new();
    let mut seen = HashSet::new();
    while let Some(dep) = pending.pop_first() {
        if dep == goml_project::BUILTIN_PACKAGE
            || dep == goml_project::STD_PACKAGE
            || dep.starts_with("std::")
            || dep == package_name
        {
            continue;
        }
        if !visited.insert(dep.clone()) {
            continue;
        }
        if let Some(dep_interface) = interface_outputs.get(&dep) {
            if seen.insert(dep_interface.clone()) {
                outputs.push(dep_interface.clone());
            }
            if let Some(package) = packages.get(&dep) {
                pending.extend(package.imports.iter().cloned());
            }
            continue;
        }
        if packages.contains_key(&dep) {
            return Err(anyhow!(
                "{} failed: dependency {} of package {} is ordered after its consumer",
                stage,
                dep,
                package_name
            ));
        }
        return Err(anyhow!(
            "{} failed: package {} imports missing dependency {}",
            stage,
            package_name,
            dep
        ));
    }
    Ok(outputs)
}

fn sorted_relative_inputs(module_dir: &Path, files: &[PathBuf]) -> Vec<PathBuf> {
    let mut inputs = files
        .iter()
        .map(|file| relative_to_module(module_dir, file))
        .collect::<Vec<_>>();
    inputs.sort();
    inputs
}

fn relative_to_module(module_dir: &Path, path: &Path) -> PathBuf {
    path.strip_prefix(module_dir)
        .map(Path::to_path_buf)
        .unwrap_or_else(|_| path.to_path_buf())
}

#[derive(Debug, Clone, serde::Deserialize)]
struct TestManifestEntry {
    id: String,
    display_name: String,
    source_path: String,
    ignored: bool,
    ignore_reason: Option<String>,
}

#[derive(Clone)]
struct ScheduledTest {
    test: TestManifestEntry,
    kind: TestKind,
    runner: PathBuf,
}

enum TestExecutionStatus {
    Passed,
    Failed(Option<i32>),
    TimedOut,
    Ignored,
}

struct TestExecution {
    test: TestManifestEntry,
    kind: TestKind,
    status: TestExecutionStatus,
    stdout: Vec<u8>,
    stderr: Vec<u8>,
    duration: Duration,
}

fn execute_test_runner(
    module_dir: &Path,
    groups: &[TestRunGroup],
    args: &TestCommandArgs,
) -> anyhow::Result<()> {
    if matches!(args.format, TestOutputFormat::Json) && args.nocapture {
        bail!("--nocapture cannot be combined with --format json");
    }
    let mut tests = Vec::new();
    for group in groups {
        let manifest_path = module_dir.join(&group.manifest);
        let manifest = fs::read_to_string(&manifest_path)
            .with_context(|| format!("failed to read {}", manifest_path.display()))?;
        let entries: Vec<TestManifestEntry> = serde_json::from_str(&manifest)
            .with_context(|| format!("failed to parse {}", manifest_path.display()))?;
        tests.extend(entries.into_iter().map(|test| ScheduledTest {
            test,
            kind: group.kind,
            runner: module_dir.join(&group.runner),
        }));
    }
    tests.retain(|test| {
        args.filter
            .as_ref()
            .is_none_or(|filter| test.test.display_name.contains(filter))
    });
    if args.ignored {
        tests.retain(|test| test.test.ignored);
    }
    tests.sort_by(|left, right| left.test.display_name.cmp(&right.test.display_name));

    if args.list {
        for test in tests {
            match args.format {
                TestOutputFormat::Text => {
                    if test.test.ignored {
                        println!("{}: ignored", test.test.display_name);
                    } else {
                        println!("{}", test.test.display_name);
                    }
                }
                TestOutputFormat::Json => println!(
                    "{}",
                    serde_json::json!({
                        "event": "test",
                        "name": test.test.display_name,
                        "kind": test_kind_name(test.kind),
                        "ignored": test.test.ignored,
                        "reason": test.test.ignore_reason,
                    })
                ),
            }
        }
        return Ok(());
    }

    let runnable = tests
        .iter()
        .any(|test| !test.test.ignored || args.ignored || args.include_ignored);
    if runnable {
        for group in groups {
            if tests.iter().any(|test| {
                test.kind == group.kind
                    && (!test.test.ignored || args.ignored || args.include_ignored)
            }) {
                build_test_runner(module_dir, group)?;
            }
        }
    }
    if matches!(args.format, TestOutputFormat::Text) {
        println!("running {} tests\n", tests.len());
        std::io::stdout().flush()?;
    }
    let executions = execute_tests(module_dir, tests, args)?;
    report_test_executions(&executions, args.format)?;
    let failures = executions
        .iter()
        .filter(|execution| {
            matches!(
                execution.status,
                TestExecutionStatus::Failed(_) | TestExecutionStatus::TimedOut
            )
        })
        .count();
    if failures > 0 {
        bail!("{failures} test(s) failed");
    }
    Ok(())
}

fn build_test_runner(module_dir: &Path, group: &TestRunGroup) -> anyhow::Result<()> {
    let runner = module_dir.join(&group.runner);
    let status = Command::new("go")
        .args(["build", "-o"])
        .arg(&runner)
        .arg(&group.go_output)
        .current_dir(module_dir)
        .env("GOWORK", "off")
        .env("GO111MODULE", "off")
        .stdin(Stdio::null())
        .status()
        .context("failed to execute go build for test runner")?;
    if !status.success() {
        bail!("go build failed for test runner with status {status}");
    }
    Ok(())
}

fn execute_tests(
    module_dir: &Path,
    tests: Vec<ScheduledTest>,
    args: &TestCommandArgs,
) -> anyhow::Result<Vec<TestExecution>> {
    let count = tests.len();
    let next = AtomicUsize::new(0);
    let results = Mutex::new(
        (0..count)
            .map(|_| None)
            .collect::<Vec<Option<anyhow::Result<TestExecution>>>>(),
    );
    let worker_count = args.jobs.min(count.max(1));
    thread::scope(|scope| {
        for _ in 0..worker_count {
            scope.spawn(|| {
                loop {
                    let index = next.fetch_add(1, Ordering::Relaxed);
                    let Some(scheduled) = tests.get(index).cloned() else {
                        break;
                    };
                    let execution =
                        if scheduled.test.ignored && !args.ignored && !args.include_ignored {
                            Ok(TestExecution {
                                test: scheduled.test,
                                kind: scheduled.kind,
                                status: TestExecutionStatus::Ignored,
                                stdout: Vec::new(),
                                stderr: Vec::new(),
                                duration: Duration::ZERO,
                            })
                        } else {
                            run_test_case(module_dir, scheduled, args.timeout, args.nocapture)
                        };
                    if let Ok(mut results) = results.lock() {
                        results[index] = Some(execution);
                    }
                }
            });
        }
    });
    let results = results
        .into_inner()
        .map_err(|_| anyhow!("test worker results were poisoned"))?;
    results
        .into_iter()
        .map(|result| result.ok_or_else(|| anyhow!("test worker did not produce a result"))?)
        .collect()
}

fn run_test_case(
    module_dir: &Path,
    scheduled: ScheduledTest,
    timeout: Duration,
    nocapture: bool,
) -> anyhow::Result<TestExecution> {
    let mut command = Command::new(&scheduled.runner);
    command
        .arg(&scheduled.test.id)
        .current_dir(module_dir)
        .env("TZ", "UTC")
        .stdin(Stdio::null());
    if nocapture {
        command.stdout(Stdio::inherit()).stderr(Stdio::inherit());
    } else {
        command.stdout(Stdio::piped()).stderr(Stdio::piped());
    }
    let start = Instant::now();
    let mut child = command
        .spawn()
        .with_context(|| format!("failed to run test {}", scheduled.test.display_name))?;
    let stdout_reader = child.stdout.take().map(|mut stdout| {
        thread::spawn(move || {
            let mut bytes = Vec::new();
            let _ = stdout.read_to_end(&mut bytes);
            bytes
        })
    });
    let stderr_reader = child.stderr.take().map(|mut stderr| {
        thread::spawn(move || {
            let mut bytes = Vec::new();
            let _ = stderr.read_to_end(&mut bytes);
            bytes
        })
    });
    let mut timed_out = false;
    let exit_status = loop {
        if let Some(status) = child.try_wait().context("failed to poll test process")? {
            break status;
        }
        if start.elapsed() >= timeout {
            timed_out = true;
            child.kill().context("failed to terminate timed out test")?;
            break child.wait().context("failed to reap timed out test")?;
        }
        thread::sleep(Duration::from_millis(10));
    };
    let stdout = join_output_reader(stdout_reader)?;
    let stderr = join_output_reader(stderr_reader)?;
    let status = if timed_out {
        TestExecutionStatus::TimedOut
    } else if exit_status.success() {
        TestExecutionStatus::Passed
    } else {
        TestExecutionStatus::Failed(exit_status.code())
    };
    Ok(TestExecution {
        test: scheduled.test,
        kind: scheduled.kind,
        status,
        stdout,
        stderr,
        duration: start.elapsed(),
    })
}

fn join_output_reader(reader: Option<thread::JoinHandle<Vec<u8>>>) -> anyhow::Result<Vec<u8>> {
    match reader {
        Some(reader) => reader
            .join()
            .map_err(|_| anyhow!("test output reader panicked")),
        None => Ok(Vec::new()),
    }
}

fn report_test_executions(
    executions: &[TestExecution],
    format: TestOutputFormat,
) -> anyhow::Result<()> {
    let passed = executions
        .iter()
        .filter(|execution| matches!(execution.status, TestExecutionStatus::Passed))
        .count();
    let failed = executions
        .iter()
        .filter(|execution| {
            matches!(
                execution.status,
                TestExecutionStatus::Failed(_) | TestExecutionStatus::TimedOut
            )
        })
        .count();
    let ignored = executions
        .iter()
        .filter(|execution| matches!(execution.status, TestExecutionStatus::Ignored))
        .count();
    match format {
        TestOutputFormat::Text => {
            for execution in executions {
                report_text_test_execution(execution);
            }
            println!(
                "\nresult: {}. {} passed; {} failed; {} ignored",
                if failed == 0 { "ok" } else { "FAILED" },
                passed,
                failed,
                ignored
            );
        }
        TestOutputFormat::Json => {
            for execution in executions {
                println!(
                    "{}",
                    serde_json::json!({
                        "event": "result",
                        "id": execution.test.id,
                        "name": execution.test.display_name,
                        "kind": test_kind_name(execution.kind),
                        "status": test_status_name(&execution.status),
                        "reason": execution.test.ignore_reason,
                        "exit_code": test_exit_code(&execution.status),
                        "duration_ms": execution.duration.as_secs_f64() * 1000.0,
                        "stdout": String::from_utf8_lossy(&execution.stdout),
                        "stderr": String::from_utf8_lossy(&execution.stderr),
                        "source": execution.test.source_path,
                    })
                );
            }
            println!(
                "{}",
                serde_json::json!({
                    "event": "summary",
                    "passed": passed,
                    "failed": failed,
                    "ignored": ignored,
                })
            );
        }
    }
    Ok(())
}

fn report_text_test_execution(execution: &TestExecution) {
    let status = match execution.status {
        TestExecutionStatus::Passed => "ok".to_string(),
        TestExecutionStatus::Ignored => execution.test.ignore_reason.as_ref().map_or_else(
            || "ignored".to_string(),
            |reason| format!("ignored: {reason}"),
        ),
        TestExecutionStatus::TimedOut => "FAILED (timed out)".to_string(),
        TestExecutionStatus::Failed(Some(code)) => format!("FAILED (exit code {code})"),
        TestExecutionStatus::Failed(None) => "FAILED (terminated by signal)".to_string(),
    };
    println!("test {} ... {}", execution.test.display_name, status);
    if matches!(
        execution.status,
        TestExecutionStatus::Failed(_) | TestExecutionStatus::TimedOut
    ) {
        if !execution.stdout.is_empty() {
            println!("---- stdout ----");
            print!("{}", String::from_utf8_lossy(&execution.stdout));
        }
        if !execution.stderr.is_empty() {
            println!("---- stderr ----");
            print!("{}", String::from_utf8_lossy(&execution.stderr));
        }
        println!("at {}", execution.test.source_path);
    }
}

fn test_status_name(status: &TestExecutionStatus) -> &'static str {
    match status {
        TestExecutionStatus::Passed => "passed",
        TestExecutionStatus::Failed(_) => "failed",
        TestExecutionStatus::TimedOut => "timed_out",
        TestExecutionStatus::Ignored => "ignored",
    }
}

fn test_exit_code(status: &TestExecutionStatus) -> Option<i32> {
    match status {
        TestExecutionStatus::Failed(code) => *code,
        _ => None,
    }
}

fn test_kind_name(kind: TestKind) -> &'static str {
    match kind {
        TestKind::Internal => "internal",
        TestKind::External => "external",
        TestKind::All => "all",
    }
}

fn parse_duration(value: &str) -> Result<Duration, String> {
    let (number, multiplier) = if let Some(value) = value.strip_suffix("ms") {
        (value, 1u64)
    } else if let Some(value) = value.strip_suffix('s') {
        (value, 1_000)
    } else if let Some(value) = value.strip_suffix('m') {
        (value, 60_000)
    } else {
        return Err("duration must end in ms, s, or m".to_string());
    };
    let number = number
        .parse::<u64>()
        .map_err(|_| format!("invalid duration `{value}`"))?;
    if number == 0 {
        return Err("duration must be greater than zero".to_string());
    }
    number
        .checked_mul(multiplier)
        .map(Duration::from_millis)
        .ok_or_else(|| "duration is too large".to_string())
}

fn parse_positive_usize(value: &str) -> Result<usize, String> {
    let value = value
        .parse::<usize>()
        .map_err(|_| format!("invalid positive integer `{value}`"))?;
    if value == 0 {
        return Err("value must be greater than zero".to_string());
    }
    Ok(value)
}

fn execute_planned_commands(
    module_dir: &Path,
    commands: &[PlannedCompilerCommand],
    dry_run: bool,
    compiler: Option<&Path>,
) -> anyhow::Result<()> {
    if dry_run {
        for command in commands {
            println!("{}", command.display());
        }
        return Ok(());
    }

    let executable = gomlc::resolve(compiler)?;
    let executable = executable
        .canonicalize()
        .with_context(|| format!("failed to resolve compiler {}", executable.display()))?;
    let mut compiler_identity = build_cache::CompilerIdentity::read(&executable, module_dir)?;
    gomlc::verify(&executable)?;
    compiler_identity.ensure_unchanged(&executable, module_dir)?;
    for command in commands {
        compiler_identity.ensure_unchanged(&executable, module_dir)?;
        let display = command.display();
        let args = command.to_args();
        let cache = build_cache::CommandCache::new(
            module_dir,
            &compiler_identity,
            command.cache_kind(),
            &args,
            command.cache_inputs(),
            command.cache_outputs(),
            command.cache_anchor(),
        );
        let fingerprint = cache.fingerprint()?;
        if cache.is_fresh(&fingerprint)? {
            continue;
        }
        cache.prepare_for_execution()?;
        let status = gomlc::execute(&executable, &args, Some(module_dir))
            .with_context(|| format!("failed to execute {display}"))?;
        if !status.success() {
            bail!("subcommand failed: {}", display);
        }
        if let Err(error) = compiler_identity.ensure_unchanged(&executable, module_dir) {
            cache.prepare_for_execution()?;
            return Err(error);
        }
        cache.store_if_unchanged(&fingerprint)?;
    }
    Ok(())
}

fn execute_project_build_plan(
    module_dir: &Path,
    plan: &ProjectBuildCommandPlan,
    compiler: Option<&Path>,
    dry_run: bool,
) -> anyhow::Result<()> {
    execute_planned_commands(module_dir, &plan.compiler.commands, dry_run, compiler)?;
    if dry_run {
        for command in &plan.go {
            println!("{}", command.display());
        }
        return Ok(());
    }
    for command in &plan.go {
        let output = absolute_from_module(module_dir, &command.output);
        let output_dir = output
            .parent()
            .ok_or_else(|| anyhow!("binary output {} has no parent directory", output.display()))?;
        fs::create_dir_all(output_dir)
            .with_context(|| format!("failed to create {}", output_dir.display()))?;
        let status = Command::new("go")
            .args(["build", "-o"])
            .arg(&command.output)
            .arg(&command.input)
            .current_dir(module_dir)
            .env("GOWORK", "off")
            .env("GO111MODULE", "off")
            .status()
            .context("failed to execute go build")?;
        if !status.success() {
            bail!("go build failed with status {status}");
        }
    }
    Ok(())
}

fn absolute_from_module(module_dir: &Path, path: &Path) -> PathBuf {
    if path.is_absolute() {
        path.to_path_buf()
    } else {
        module_dir.join(path)
    }
}

fn display_command_path_and_args(path: &Path, args: &[OsString]) -> String {
    let mut parts = Vec::with_capacity(args.len() + 1);
    parts.push(shell_escape(&path.to_string_lossy()));
    parts.extend(args.iter().map(|arg| shell_escape(&arg.to_string_lossy())));
    parts.join(" ")
}

fn shell_escape(arg: &str) -> String {
    if arg
        .chars()
        .all(|ch| ch.is_ascii_alphanumeric() || matches!(ch, '/' | '.' | '_' | '-' | ':' | '='))
    {
        arg.to_string()
    } else {
        format!("{arg:?}")
    }
}

fn load_project(
    target: &Path,
    target_dir_override: Option<&Path>,
) -> anyhow::Result<ProjectContext> {
    let cwd = std::env::current_dir().context("failed to read current directory")?;
    let target = if target.is_absolute() {
        target.to_path_buf()
    } else {
        cwd.join(target)
    };
    if !target.exists() {
        bail!("target {} does not exist", target.display());
    }
    let target = target
        .canonicalize()
        .with_context(|| format!("failed to resolve target {}", target.display()))?;
    let search_dir = if target.is_file() {
        target.parent().unwrap_or_else(|| Path::new("."))
    } else {
        target.as_path()
    };
    let (module_dir, _) = find_module_root(search_dir)
        .map_err(anyhow::Error::msg)?
        .ok_or_else(|| {
            anyhow!(
                "no goml.toml with [module] section found in ancestors of {}",
                search_dir.display()
            )
        })?;
    let manifest =
        load_module_manifest(&module_dir.join("goml.toml")).map_err(anyhow::Error::msg)?;
    let artifact_root =
        resolve_artifact_root(&module_dir, &manifest.build.target_dir, target_dir_override)?;
    let absolute_artifact_root = if artifact_root.is_absolute() {
        artifact_root.clone()
    } else {
        module_dir.join(&artifact_root)
    };
    if target.starts_with(&absolute_artifact_root) {
        bail!(
            "target {} is inside build target directory {}",
            target.display(),
            absolute_artifact_root.display()
        );
    }
    let (target_dir, target_role) =
        match goml_project::package_graph::classify_project_path(&module_dir, &target)
            .map_err(anyhow::Error::msg)?
        {
            goml_project::package_graph::ProjectPathRole::Production => {
                let target_dir = if target.is_file() {
                    target
                        .parent()
                        .unwrap_or_else(|| Path::new("."))
                        .to_path_buf()
                } else {
                    target.clone()
                };
                (target_dir, ProjectTargetRole::Production)
            }
            goml_project::package_graph::ProjectPathRole::InternalTest => (
                target
                    .parent()
                    .unwrap_or_else(|| Path::new("."))
                    .to_path_buf(),
                ProjectTargetRole::InternalTest,
            ),
            goml_project::package_graph::ProjectPathRole::ExternalTest { target_dir, .. } => {
                (target_dir, ProjectTargetRole::ExternalTest)
            }
        };
    let entry_path = if target.is_file()
        && matches!(&target_role, ProjectTargetRole::Production)
        && !goml_project::package_graph::is_internal_test_source(&target)
    {
        target
    } else {
        first_production_source(&target_dir)?
    };
    Ok(ProjectContext {
        module_dir,
        module_path: manifest.module.path,
        entry_path: Some(entry_path),
        target_role: Some(target_role),
        dependencies: manifest.dependencies,
        artifacts: ArtifactLayout::new(artifact_root),
    })
}

fn load_current_project(target_dir_override: Option<&Path>) -> anyhow::Result<ProjectContext> {
    let cwd = std::env::current_dir().context("failed to read current directory")?;
    let (module_dir, _) = find_module_root(&cwd)
        .map_err(anyhow::Error::msg)?
        .ok_or_else(|| {
            anyhow!(
                "no goml.toml with [module] section found in ancestors of {}",
                cwd.display()
            )
        })?;
    let manifest =
        load_module_manifest(&module_dir.join("goml.toml")).map_err(anyhow::Error::msg)?;
    let artifact_root =
        resolve_artifact_root(&module_dir, &manifest.build.target_dir, target_dir_override)?;
    Ok(ProjectContext {
        module_dir,
        module_path: manifest.module.path,
        entry_path: None,
        target_role: None,
        dependencies: manifest.dependencies,
        artifacts: ArtifactLayout::new(artifact_root),
    })
}

fn resolve_artifact_root(
    module_dir: &Path,
    configured: &Path,
    target_dir_override: Option<&Path>,
) -> anyhow::Result<PathBuf> {
    let target_dir = target_dir_override.unwrap_or(configured);
    if target_dir.is_absolute() {
        let mut has_segment = false;
        for component in target_dir.components() {
            match component {
                Component::Normal(_) => has_segment = true,
                Component::ParentDir => {
                    bail!("target-dir must not contain parent directory components")
                }
                Component::CurDir | Component::RootDir | Component::Prefix(_) => {}
            }
        }
        if !has_segment {
            bail!("target-dir must not be a filesystem root");
        }
        if target_dir == module_dir {
            bail!("target-dir must not be the module root");
        }
        return Ok(target_dir.to_path_buf());
    }
    validate_manifest_target_dir(target_dir).map_err(anyhow::Error::msg)?;
    Ok(target_dir.to_path_buf())
}

fn first_production_source(package_dir: &Path) -> anyhow::Result<PathBuf> {
    let mut sources = fs::read_dir(package_dir)
        .with_context(|| format!("failed to read package directory {}", package_dir.display()))?
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|path| {
            path.is_file()
                && path.extension().is_some_and(|extension| extension == "gom")
                && !goml_project::package_graph::is_internal_test_source(path)
        })
        .collect::<Vec<_>>();
    sources.sort();
    sources.into_iter().next().ok_or_else(|| {
        anyhow!(
            "package directory {} has no .gom files",
            package_dir.display()
        )
    })
}
