use std::collections::{HashMap, HashSet};
use std::ffi::OsString;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use anyhow::{Context, anyhow, bail};
use clap::{Args, Parser, Subcommand};
use compiler::config::GomlConfig;
use compiler::env::{format_compile_diagnostics, format_typer_diagnostics};
use compiler::external::ExternalDependencyArtifacts;
use compiler::package_names::{ENTRY_FUNCTION, ROOT_PACKAGE};
use compiler::pipeline::{
    pipeline::Compilation, pipeline::CompilationError, pipeline::compile_single_file,
};
use compiler::registry::{
    ModuleCoord, ModuleRequirement, Registry, cached_registry_dir, default_registry_url,
    load_or_create_user_config, user_config_path, validate_registry_consistency,
};
use parser::format_parser_diagnostics;
use tempfile::tempdir;
use toml_edit::{DocumentMut, Item, Table, value};

const PRETTY_WIDTH: usize = 120;
const PROJECT_GO_OUTPUT: &str = "target/goml/main.go";
const PROJECT_CHECK_OUTPUT_DIR: &str = "target/goml/check";
const PROJECT_BUILD_OUTPUT_DIR: &str = "target/goml/build";
const DEFAULT_LIB_PACKAGE: &str = "lib";
const DEFAULT_ENTRY_FILE: &str = "main.gom";

#[derive(Parser, Debug)]
#[command(name = "goml", arg_required_else_help = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    New(NewArgs),
    Check(ProjectCommandArgs),
    Build(ProjectCommandArgs),
    Update(RegistryCommandArgs),
    Add(AddArgs),
    Remove(RemoveArgs),
    Version,
    Compiler(CompilerArgs),
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

#[derive(Args, Debug, Clone, Copy)]
struct ProjectCommandArgs {
    #[arg(long = "dry-run")]
    dry_run: bool,
}

#[derive(Args, Debug)]
struct NewArgs {
    project_name: String,
    #[arg(long, default_value = ".")]
    path: PathBuf,
}

#[derive(Args, Debug)]
struct CompilerArgs {
    #[command(subcommand)]
    command: CompilerCommands,
}

#[derive(Subcommand, Debug)]
enum CompilerCommands {
    Check(PackageCommandArgs),
    Build(PackageCommandArgs),
    Link(LinkArgs),
    RunSingle(RunArgs),
}

#[derive(Args, Debug)]
struct RunArgs {
    #[arg(long = "dump-ast")]
    dump_ast: bool,
    #[arg(long = "dump-hir")]
    dump_hir: bool,
    #[arg(long = "dump-tast")]
    dump_tast: bool,
    #[arg(long = "dump-core")]
    dump_core: bool,
    #[arg(long = "dump-mono")]
    dump_mono: bool,
    #[arg(long = "dump-lift")]
    dump_lift: bool,
    #[arg(long = "dump-anf")]
    dump_anf: bool,
    #[arg(long = "dump-go")]
    dump_go: bool,
    file: PathBuf,
}

#[derive(Args, Debug)]
struct PackageCommandArgs {
    #[arg(long)]
    package: String,
    #[arg(long, required = true, num_args = 1..)]
    input: Vec<PathBuf>,
    #[arg(long = "interface-path", value_name = "INTERFACE_FILE")]
    interface_path: Vec<PathBuf>,
    #[arg(long)]
    output: PathBuf,
}

#[derive(Args, Debug)]
struct LinkArgs {
    #[arg(long, required = true, num_args = 1..)]
    input: Vec<PathBuf>,
    #[arg(long)]
    output: PathBuf,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum DumpStage {
    Ast,
    Hir,
    Tast,
    Core,
    Mono,
    Lift,
    Anf,
    Go,
}

impl DumpStage {
    fn label(&self) -> &'static str {
        match self {
            DumpStage::Ast => "AST",
            DumpStage::Hir => "HIR",
            DumpStage::Tast => "Typed AST",
            DumpStage::Core => "Core",
            DumpStage::Mono => "Mono",
            DumpStage::Lift => "Lifted",
            DumpStage::Anf => "ANF",
            DumpStage::Go => "Go",
        }
    }

    fn order(&self) -> usize {
        match self {
            DumpStage::Ast => 0,
            DumpStage::Hir => 1,
            DumpStage::Tast => 2,
            DumpStage::Core => 3,
            DumpStage::Mono => 4,
            DumpStage::Lift => 5,
            DumpStage::Anf => 6,
            DumpStage::Go => 7,
        }
    }
}

struct RunOptions {
    file_path: PathBuf,
    dumps: Vec<DumpStage>,
}

struct PackageCommandOptions {
    package: String,
    input_files: Vec<PathBuf>,
    interface_files: Vec<PathBuf>,
    output: PathBuf,
}

struct LinkOptions {
    input_cores: Vec<PathBuf>,
    output: PathBuf,
}

struct ProjectContext {
    module_dir: PathBuf,
    entry_path: PathBuf,
    config: GomlConfig,
}

struct PackageCompilerCommand {
    package: String,
    input_files: Vec<PathBuf>,
    interface_files: Vec<PathBuf>,
    output: PathBuf,
}

struct LinkCompilerCommand {
    input_cores: Vec<PathBuf>,
    output: PathBuf,
}

struct ProjectCommandPlan {
    commands: Vec<PlannedCompilerCommand>,
    external: ExternalArtifactsPlan,
}

struct ExternalArtifactsPlan {
    artifacts: ExternalDependencyArtifacts,
    interface_outputs: HashMap<String, PathBuf>,
    core_outputs: HashMap<String, PathBuf>,
}

enum PlannedCompilerCommand {
    Check(PackageCompilerCommand),
    Build(PackageCompilerCommand),
    Link(LinkCompilerCommand),
}

impl PlannedCompilerCommand {
    fn to_args(&self) -> Vec<OsString> {
        match self {
            PlannedCompilerCommand::Check(cmd) => package_command_args("check", cmd),
            PlannedCompilerCommand::Build(cmd) => package_command_args("build", cmd),
            PlannedCompilerCommand::Link(cmd) => {
                let mut args = vec![
                    OsString::from("compiler"),
                    OsString::from("link"),
                    OsString::from("--input"),
                ];
                args.extend(
                    cmd.input_cores
                        .iter()
                        .map(|path| path.clone().into_os_string()),
                );
                args.push(OsString::from("--output"));
                args.push(cmd.output.clone().into_os_string());
                args
            }
        }
    }

    fn display(&self) -> String {
        let args = self.to_args();
        let mut parts = Vec::with_capacity(args.len() + 1);
        parts.push("goml".to_string());
        parts.extend(args.iter().map(|arg| shell_escape(&arg.to_string_lossy())));
        parts.join(" ")
    }
}

fn package_command_args(kind: &str, cmd: &PackageCompilerCommand) -> Vec<OsString> {
    let mut args = vec![
        OsString::from("compiler"),
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
        Commands::Update(args) => execute_update(args),
        Commands::Add(args) => execute_add(args),
        Commands::Remove(args) => execute_remove(args),
        Commands::Version => execute_version(),
        Commands::Compiler(args) => execute_compiler_command(args.command),
    }
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

    let project_dir = args.path.join(&args.project_name);
    ensure_project_dir_ready(&project_dir)?;
    let lib_dir = project_dir.join(DEFAULT_LIB_PACKAGE);
    fs::create_dir_all(&lib_dir)
        .with_context(|| format!("failed to create directory {}", lib_dir.display()))?;

    write_file_with_dirs(
        &project_dir.join("goml.toml"),
        &render_root_goml_toml(&args.project_name),
    )?;
    write_file_with_dirs(&project_dir.join("main.gom"), &render_main_gom())?;
    write_file_with_dirs(&lib_dir.join("goml.toml"), &render_lib_goml_toml())?;
    write_file_with_dirs(&lib_dir.join("lib.gom"), &render_lib_gom())?;

    println!("Created project at {}", project_dir.display());
    println!("Next steps:");
    println!("  cd {}", project_dir.display());
    println!("  goml check");
    println!("  goml build");

    Ok(())
}

fn execute_update(args: RegistryCommandArgs) -> anyhow::Result<()> {
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
) -> anyhow::Result<(ModuleCoord, Option<compiler::registry::SemVer>)> {
    let trimmed = input.trim();
    let (coord, version) = match trimmed.split_once('@') {
        Some((coord, version)) => (coord, Some(version)),
        None => (trimmed, None),
    };
    let coord = ModuleCoord::parse(coord).map_err(anyhow::Error::msg)?;
    let version = version
        .map(compiler::registry::SemVer::parse)
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
    let Some((module_dir, _config)) = GomlConfig::find_module_root(&cwd) else {
        bail!(
            "no goml.toml with [module] section found in ancestors of {}",
            cwd.display()
        );
    };
    Ok(module_dir)
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
    format!(
        r#"[module]
name = "{project_name}"

[package]
name = "{ROOT_PACKAGE}"
entry = "{DEFAULT_ENTRY_FILE}"
"#
    )
}

fn render_main_gom() -> String {
    format!(
        r#"package {ROOT_PACKAGE};

use {DEFAULT_LIB_PACKAGE};

fn {ENTRY_FUNCTION}() -> unit {{
    string_println({DEFAULT_LIB_PACKAGE}::message())
}}
"#
    )
}

fn render_lib_goml_toml() -> String {
    format!(
        r#"[package]
name = "{DEFAULT_LIB_PACKAGE}"
"#
    )
}

fn render_lib_gom() -> String {
    format!(
        r#"package {DEFAULT_LIB_PACKAGE};

fn message() -> string {{
    "hello from lib"
}}
"#
    )
}

fn execute_compiler_command(command: CompilerCommands) -> anyhow::Result<()> {
    match command {
        CompilerCommands::RunSingle(args) => {
            let dumps = run_dumps(&args);
            let options = RunOptions {
                file_path: args.file,
                dumps,
            };
            execute_run_single(options)
        }
        CompilerCommands::Check(args) => {
            let options = PackageCommandOptions {
                package: args.package,
                input_files: args.input,
                interface_files: args.interface_path,
                output: args.output,
            };
            execute_compiler_check(options)
        }
        CompilerCommands::Build(args) => {
            let options = PackageCommandOptions {
                package: args.package,
                input_files: args.input,
                interface_files: args.interface_path,
                output: args.output,
            };
            execute_compiler_build(options)
        }
        CompilerCommands::Link(args) => {
            let options = LinkOptions {
                input_cores: args.input,
                output: args.output,
            };
            execute_compiler_link(options)
        }
    }
}

fn run_dumps(args: &RunArgs) -> Vec<DumpStage> {
    let mut dumps = Vec::new();

    if args.dump_ast {
        dumps.push(DumpStage::Ast);
    }
    if args.dump_hir {
        dumps.push(DumpStage::Hir);
    }
    if args.dump_tast {
        dumps.push(DumpStage::Tast);
    }
    if args.dump_core {
        dumps.push(DumpStage::Core);
    }
    if args.dump_mono {
        dumps.push(DumpStage::Mono);
    }
    if args.dump_lift {
        dumps.push(DumpStage::Lift);
    }
    if args.dump_anf {
        dumps.push(DumpStage::Anf);
    }
    if args.dump_go {
        dumps.push(DumpStage::Go);
    }

    dumps.sort_by_key(|stage| stage.order());
    dumps.dedup();

    dumps
}

fn execute_run_single(options: RunOptions) -> anyhow::Result<()> {
    let src = fs::read_to_string(&options.file_path)
        .with_context(|| format!("error reading goml file: {}", options.file_path.display()))?;

    let compilation = match compile_single_file(&options.file_path, &src) {
        Ok(compilation) => compilation,
        Err(err) => {
            report_compilation_error(&options.file_path, &src, err);
            std::process::exit(1);
        }
    };

    if !options.dumps.is_empty() {
        print_dumps(&compilation, &options.dumps);
    }

    let go_source = compilation.go.to_pretty(&compilation.goenv, PRETTY_WIDTH);
    let output = execute_go_source(&go_source)?;
    print!("{output}");

    Ok(())
}

fn execute_compiler_check(options: PackageCommandOptions) -> anyhow::Result<()> {
    let unit =
        compiler::pipeline::separate::check_package(compiler::pipeline::separate::PackageInputs {
            package: options.package,
            input_files: options.input_files,
            interface_files: options.interface_files,
        })
        .map_err(|err| anyhow!("check failed: {:?}", err))?;

    let out = options.output.with_extension("interface");
    let json = serde_json::to_string_pretty(&unit)?;
    if let Some(parent) = out.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create directory {}", parent.display()))?;
    }
    fs::write(&out, json).with_context(|| format!("failed to write {}", out.display()))?;
    Ok(())
}

fn execute_compiler_build(options: PackageCommandOptions) -> anyhow::Result<()> {
    let unit =
        compiler::pipeline::separate::build_package(compiler::pipeline::separate::PackageInputs {
            package: options.package,
            input_files: options.input_files,
            interface_files: options.interface_files,
        })
        .map_err(|err| anyhow!("build failed: {:?}", err))?;

    let interface_path = options.output.with_extension("interface");
    let core_path = options.output.with_extension("core");

    let interface_json = serde_json::to_string_pretty(&unit.interface)?;
    if let Some(parent) = interface_path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create directory {}", parent.display()))?;
    }
    fs::write(&interface_path, interface_json)
        .with_context(|| format!("failed to write {}", interface_path.display()))?;

    let core_json = serde_json::to_string_pretty(&unit)?;
    if let Some(parent) = core_path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create directory {}", parent.display()))?;
    }
    fs::write(&core_path, core_json)
        .with_context(|| format!("failed to write {}", core_path.display()))?;

    Ok(())
}

fn execute_compiler_link(options: LinkOptions) -> anyhow::Result<()> {
    let mut units = Vec::new();
    for path in options.input_cores {
        let unit = compiler::pipeline::separate::read_core(&path)
            .map_err(|err| anyhow!("link failed: {:?} ({})", err, path.display()))?;
        units.push(unit);
    }

    let linked = compiler::pipeline::separate::link_cores(units)
        .map_err(|err| anyhow!("link failed: {:?}", err))?;
    let go_source = linked.go.to_pretty(&linked.goenv, PRETTY_WIDTH);
    if let Some(parent) = options.output.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create directory {}", parent.display()))?;
    }
    fs::write(&options.output, go_source)
        .with_context(|| format!("failed to write {}", options.output.display()))?;
    Ok(())
}

fn execute_project_check(args: ProjectCommandArgs) -> anyhow::Result<()> {
    let project = load_project_from_cwd()?;
    let plan = build_project_check_plan(&project)?;
    if !args.dry_run {
        materialize_external_artifacts(&plan.external, false)?;
    }
    execute_planned_commands(&project.module_dir, plan.commands, args.dry_run)
}

fn execute_project_build(args: ProjectCommandArgs) -> anyhow::Result<()> {
    let project = load_project_from_cwd()?;
    let plan = build_project_build_plan(&project)?;
    if !args.dry_run {
        materialize_external_artifacts(&plan.external, true)?;
    }
    execute_planned_commands(&project.module_dir, plan.commands, args.dry_run)
}

fn build_project_check_plan(project: &ProjectContext) -> anyhow::Result<ProjectCommandPlan> {
    let external = build_external_artifacts_plan(project, PROJECT_CHECK_OUTPUT_DIR)?;
    let external_imports = external.artifacts.external_imports();
    let mut graph = compiler::pipeline::packages::discover_packages_with_external_imports(
        &project.module_dir,
        Some(&project.entry_path),
        None,
        &external_imports,
    )
    .map_err(|err| anyhow!("project check failed: {:?}", err))?;
    external
        .artifacts
        .augment_graph(&mut graph)
        .map_err(anyhow::Error::msg)?;
    let order = compiler::pipeline::packages::topo_sort_packages(&graph)
        .map_err(|err| anyhow!("project check failed: {:?}", err))?;

    let artifact_outputs =
        package_artifact_outputs(&graph, &project.module_dir, PROJECT_CHECK_OUTPUT_DIR)?;
    let mut commands = Vec::new();
    let mut interface_outputs = HashMap::new();

    for package_name in order.iter() {
        let package = graph
            .packages
            .get(package_name)
            .ok_or_else(|| anyhow!("project check failed: missing package {}", package_name))?;
        let output_base = artifact_outputs.get(package_name).cloned().ok_or_else(|| {
            anyhow!(
                "project check failed: missing artifact output for package {}",
                package_name
            )
        })?;
        let interface_files = package_interface_inputs(
            &graph,
            package_name,
            &package.imports,
            &interface_outputs,
            &external.interface_outputs,
            "project check",
        )?;
        commands.push(PlannedCompilerCommand::Check(PackageCompilerCommand {
            package: package_name.clone(),
            input_files: sorted_package_inputs(&project.module_dir, package),
            interface_files,
            output: output_base.clone(),
        }));
        interface_outputs.insert(
            package_name.clone(),
            output_base.with_extension("interface"),
        );
    }

    Ok(ProjectCommandPlan { commands, external })
}

fn build_project_build_plan(project: &ProjectContext) -> anyhow::Result<ProjectCommandPlan> {
    let external = build_external_artifacts_plan(project, PROJECT_BUILD_OUTPUT_DIR)?;
    let external_imports = external.artifacts.external_imports();
    let mut graph = compiler::pipeline::packages::discover_packages_with_external_imports(
        &project.module_dir,
        Some(&project.entry_path),
        None,
        &external_imports,
    )
    .map_err(|err| anyhow!("project build failed: {:?}", err))?;
    external
        .artifacts
        .augment_graph(&mut graph)
        .map_err(anyhow::Error::msg)?;
    let order = compiler::pipeline::packages::topo_sort_packages(&graph)
        .map_err(|err| anyhow!("project build failed: {:?}", err))?;

    let artifact_outputs =
        package_artifact_outputs(&graph, &project.module_dir, PROJECT_BUILD_OUTPUT_DIR)?;
    let mut commands = Vec::new();
    let mut interface_outputs = HashMap::new();
    let mut core_outputs = Vec::new();

    for package_name in order.iter() {
        let package = graph
            .packages
            .get(package_name)
            .ok_or_else(|| anyhow!("project build failed: missing package {}", package_name))?;
        let output_base = artifact_outputs.get(package_name).cloned().ok_or_else(|| {
            anyhow!(
                "project build failed: missing artifact output for package {}",
                package_name
            )
        })?;
        let interface_files = package_interface_inputs(
            &graph,
            package_name,
            &package.imports,
            &interface_outputs,
            &external.interface_outputs,
            "project build",
        )?;
        commands.push(PlannedCompilerCommand::Build(PackageCompilerCommand {
            package: package_name.clone(),
            input_files: sorted_package_inputs(&project.module_dir, package),
            interface_files,
            output: output_base.clone(),
        }));
        interface_outputs.insert(
            package_name.clone(),
            output_base.with_extension("interface"),
        );
        core_outputs.push(output_base.with_extension("core"));
    }

    let mut external_cores = external.core_outputs.values().cloned().collect::<Vec<_>>();
    external_cores.sort();
    core_outputs.splice(0..0, external_cores);

    commands.push(PlannedCompilerCommand::Link(LinkCompilerCommand {
        input_cores: core_outputs,
        output: PathBuf::from(PROJECT_GO_OUTPUT),
    }));

    Ok(ProjectCommandPlan { commands, external })
}

fn build_external_artifacts_plan(
    project: &ProjectContext,
    output_root: &str,
) -> anyhow::Result<ExternalArtifactsPlan> {
    let artifacts =
        compiler::external::resolve_project_dependencies(&project.module_dir, &project.config)
            .map_err(anyhow::Error::msg)?;
    let mut interface_outputs = HashMap::new();
    let mut core_outputs = HashMap::new();

    for (root, module) in artifacts.modules.iter() {
        let base = external_artifact_base(output_root, module, root);
        let interface_output = base.with_extension("interface");
        interface_outputs.insert(root.clone(), interface_output.clone());
        for source in module.sources.values() {
            interface_outputs.insert(source.logical_name.clone(), interface_output.clone());
        }
        core_outputs.insert(root.clone(), base.with_extension("core"));
    }

    Ok(ExternalArtifactsPlan {
        artifacts,
        interface_outputs,
        core_outputs,
    })
}

fn external_artifact_base(
    output_root: &str,
    module: &compiler::external::ExternalModuleArtifact,
    root: &str,
) -> PathBuf {
    PathBuf::from(output_root)
        .join("deps")
        .join(&module.coord.owner)
        .join(&module.coord.module)
        .join(module.version.display())
        .join(root)
}

fn materialize_external_artifacts(
    plan: &ExternalArtifactsPlan,
    include_core: bool,
) -> anyhow::Result<()> {
    for (root, module) in plan.artifacts.modules.iter() {
        let interface_path = plan
            .interface_outputs
            .get(root)
            .ok_or_else(|| anyhow!("missing external interface output for dependency {}", root))?;
        write_json(
            interface_path,
            serde_json::to_string_pretty(&module.interface)?,
        )?;

        if include_core {
            let core_path = plan
                .core_outputs
                .get(root)
                .ok_or_else(|| anyhow!("missing external core output for dependency {}", root))?;
            write_json(core_path, serde_json::to_string_pretty(&module.core)?)?;
        }
    }
    Ok(())
}

fn write_json(path: &Path, json: String) -> anyhow::Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create directory {}", parent.display()))?;
    }
    fs::write(path, json).with_context(|| format!("failed to write {}", path.display()))?;
    Ok(())
}

fn package_artifact_outputs(
    graph: &compiler::pipeline::packages::PackageGraph,
    module_dir: &Path,
    output_root: &str,
) -> anyhow::Result<HashMap<String, PathBuf>> {
    let mut names: Vec<String> = graph.packages.keys().cloned().collect();
    names.sort();

    let mut outputs = HashMap::new();
    let mut seen = HashMap::new();

    for package_name in names {
        let base = package_artifact_base(graph, module_dir, &package_name)?;
        let output = PathBuf::from(output_root).join(base);

        if let Some(existing) = seen.insert(output.clone(), package_name.clone()) {
            bail!(
                "artifact path conflict in {}: packages {} and {} both map to {}",
                output_root,
                existing,
                package_name,
                output.display()
            );
        }

        outputs.insert(package_name, output);
    }

    Ok(outputs)
}

fn package_artifact_base(
    graph: &compiler::pipeline::packages::PackageGraph,
    module_dir: &Path,
    package_name: &str,
) -> anyhow::Result<PathBuf> {
    let package_dir = graph
        .package_dirs
        .get(package_name)
        .ok_or_else(|| anyhow!("missing package dir for {}", package_name))?;
    let package = graph
        .packages
        .get(package_name)
        .ok_or_else(|| anyhow!("missing package {}", package_name))?;

    let base = relative_to_module(module_dir, package_dir);
    if !base.as_os_str().is_empty() {
        return Ok(base.join(&package.name));
    }

    let entry_relative = package_entry_relative_path(package, package_dir)?;
    let stem = entry_relative
        .file_stem()
        .ok_or_else(|| anyhow!("invalid root package entry {}", entry_relative.display()))?;
    let base = PathBuf::from(stem);

    if base.as_os_str().is_empty() {
        bail!(
            "package {} resolved to empty artifact path from {}",
            package_name,
            entry_relative.display()
        );
    }

    Ok(base)
}

fn package_entry_relative_path(
    package: &compiler::pipeline::packages::PackageUnit,
    package_dir: &Path,
) -> anyhow::Result<PathBuf> {
    if let Some(config) = GomlConfig::find_package_config(package_dir) {
        return Ok(PathBuf::from(config.package.entry));
    }

    let default_entry = package_dir.join("lib.gom");
    if default_entry.exists() {
        return Ok(PathBuf::from("lib.gom"));
    }

    let mut files: Vec<PathBuf> = package.files.iter().map(|file| file.path.clone()).collect();
    files.sort();

    if files.len() == 1 {
        return files[0]
            .strip_prefix(package_dir)
            .map(Path::to_path_buf)
            .map_err(|_| {
                anyhow!(
                    "failed to compute entry path for package {} from {}",
                    package.name,
                    files[0].display()
                )
            });
    }

    bail!(
        "package {} in {} has multiple .gom files and no goml.toml entry",
        package.name,
        package_dir.display()
    )
}

fn package_interface_inputs(
    graph: &compiler::pipeline::packages::PackageGraph,
    package_name: &str,
    imports: &HashSet<String>,
    interface_outputs: &HashMap<String, PathBuf>,
    external_interfaces: &HashMap<String, PathBuf>,
    stage: &str,
) -> anyhow::Result<Vec<PathBuf>> {
    let mut deps: Vec<String> = imports.iter().cloned().collect();
    deps.sort();
    deps.dedup();

    let mut outputs = Vec::new();
    let mut seen = HashSet::new();
    for dep in deps {
        if dep == compiler::package_names::BUILTIN_PACKAGE || dep == package_name {
            continue;
        }
        if let Some(dep_interface) = interface_outputs.get(&dep) {
            if seen.insert(dep_interface.clone()) {
                outputs.push(dep_interface.clone());
            }
            continue;
        }
        if let Some(dep_interface) = external_interfaces.get(&dep) {
            if seen.insert(dep_interface.clone()) {
                outputs.push(dep_interface.clone());
            }
            continue;
        }
        if graph.external_root_packages.contains(&dep) {
            return Err(anyhow!(
                "{} failed: missing external interface artifact for dependency {} of package {}",
                stage,
                dep,
                package_name
            ));
        }
        if !graph.packages.contains_key(&dep) {
            continue;
        }
        return Err(anyhow!(
            "{} failed: missing interface artifact for dependency {} of package {}",
            stage,
            dep,
            package_name
        ));
    }
    Ok(outputs)
}

fn sorted_package_inputs(
    module_dir: &Path,
    package: &compiler::pipeline::packages::PackageUnit,
) -> Vec<PathBuf> {
    let mut inputs: Vec<PathBuf> = package
        .files
        .iter()
        .map(|file| relative_to_module(module_dir, &file.path))
        .collect();
    inputs.sort();
    inputs
}

fn relative_to_module(module_dir: &Path, path: &Path) -> PathBuf {
    path.strip_prefix(module_dir)
        .map(Path::to_path_buf)
        .unwrap_or_else(|_| path.to_path_buf())
}

fn execute_planned_commands(
    module_dir: &Path,
    commands: Vec<PlannedCompilerCommand>,
    dry_run: bool,
) -> anyhow::Result<()> {
    if dry_run {
        for command in commands.iter() {
            println!("{}", command.display());
        }
        return Ok(());
    }

    let executable =
        std::env::current_exe().context("failed to resolve current executable for subcommands")?;
    for command in commands {
        let display = command.display();
        let args = command.to_args();
        let status = Command::new(&executable)
            .args(args)
            .current_dir(module_dir)
            .status()
            .with_context(|| format!("failed to execute {}", display))?;
        if !status.success() {
            bail!("subcommand failed: {}", display);
        }
    }
    Ok(())
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

fn load_project_from_cwd() -> anyhow::Result<ProjectContext> {
    let cwd = std::env::current_dir().context("failed to read current directory")?;
    let Some((module_dir, config)) = GomlConfig::find_module_root(&cwd) else {
        bail!(
            "no goml.toml with [module] section found in ancestors of {}",
            cwd.display()
        );
    };

    Ok(ProjectContext {
        entry_path: module_dir.join(&config.package.entry),
        module_dir,
        config,
    })
}

fn print_dumps(compilation: &Compilation, dumps: &[DumpStage]) {
    for (idx, stage) in dumps.iter().enumerate() {
        if idx > 0 {
            println!();
        }
        print_dump(compilation, *stage);
    }
}

fn print_dump(compilation: &Compilation, stage: DumpStage) {
    let content = match stage {
        DumpStage::Ast => compilation.ast.to_pretty(PRETTY_WIDTH),
        DumpStage::Hir => {
            let ctx = compiler::pprint::hir_pprint::HirPrintCtx::new(&compilation.hir_table);
            compilation.hir.to_pretty(&ctx, PRETTY_WIDTH)
        }
        DumpStage::Tast => compilation.tast.to_pretty(&compilation.genv, PRETTY_WIDTH),
        DumpStage::Core => compilation.core.to_pretty(&compilation.genv, PRETTY_WIDTH),
        DumpStage::Mono => compilation
            .mono
            .to_pretty(&compilation.monoenv, PRETTY_WIDTH),
        DumpStage::Lift => compilation
            .lambda
            .to_pretty(&compilation.liftenv, PRETTY_WIDTH),
        DumpStage::Anf => compilation.anf.to_pretty(&compilation.anfenv, PRETTY_WIDTH),
        DumpStage::Go => compilation.go.to_pretty(&compilation.goenv, PRETTY_WIDTH),
    };

    println!("== {} ==", stage.label());
    println!("{content}");
}

fn report_compilation_error(file_path: &Path, src: &str, err: CompilationError) {
    match &err {
        CompilationError::Parser { diagnostics } => {
            for error in format_parser_diagnostics(diagnostics, src) {
                eprintln!("error: {}: {}", file_path.display(), error);
            }
        }
        CompilationError::Lower { diagnostics } => {
            for diagnostic in diagnostics.iter() {
                eprintln!(
                    "error (lower): {}: {}",
                    file_path.display(),
                    diagnostic.message()
                );
            }
        }
        CompilationError::Typer { diagnostics } => {
            for error in format_typer_diagnostics(diagnostics, src) {
                eprintln!("error (typer): {}: {}", file_path.display(), error);
            }
        }
        CompilationError::Compile { diagnostics } => {
            for error in format_compile_diagnostics(diagnostics, src) {
                eprintln!("error (compile): {}: {}", file_path.display(), error);
            }
        }
    }
}

fn execute_go_source(source: &str) -> anyhow::Result<String> {
    let dir = tempdir().context("failed to create temporary directory for Go output")?;
    let main_go_file = dir.path().join("main.go");
    fs::write(&main_go_file, source).with_context(|| {
        format!(
            "failed to write generated Go source to {}",
            main_go_file.display()
        )
    })?;

    if let Some(output) = try_execute_with_yaegi(dir.path(), &main_go_file)? {
        return Ok(output);
    }

    execute_with_go_run(dir.path(), &main_go_file)
}

fn try_execute_with_yaegi(dir: &Path, file: &Path) -> anyhow::Result<Option<String>> {
    let status = Command::new("yaegi")
        .arg("help")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status();

    if status.is_err() || !status.unwrap().success() {
        return Ok(None);
    }

    let output = Command::new("yaegi")
        .arg("run")
        .arg(file)
        .current_dir(dir)
        .env("TZ", "UTC")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .with_context(|| "failed to execute yaegi")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!("yaegi failed: {}", stderr.trim());
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    Ok(Some(stdout.to_string()))
}

fn execute_with_go_run(dir: &Path, file: &Path) -> anyhow::Result<String> {
    let output = Command::new("go")
        .arg("run")
        .arg(file)
        .current_dir(dir)
        .env("TZ", "UTC")
        .env("GOWORK", "off")
        .env("GO111MODULE", "off")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .with_context(|| "failed to execute go run")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!("go run failed: {}", stderr.trim());
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    Ok(stdout.to_string())
}
