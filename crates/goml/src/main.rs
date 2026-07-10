use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::ffi::OsString;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use anyhow::{Context, anyhow, bail};
use clap::{Args, Parser, Subcommand};
use goml_project::ENTRY_FUNCTION;
use goml_project::config::{
    ensure_goml_home_layout, find_module_root, goml_bin_dir, goml_cache_dir, goml_home_dir,
    goml_lib_dir, goml_std_dir, load_module_manifest,
};
use goml_project::registry::{
    ModuleCoord, ModuleRequirement, Registry, cached_registry_dir, default_registry_url,
    load_or_create_user_config, user_config_path, validate_registry_consistency,
};
use toml_edit::{DocumentMut, Item, Table, value};

mod gomlc;

const PROJECT_GO_OUTPUT: &str = "target/goml/main.go";
const PROJECT_CHECK_OUTPUT_DIR: &str = "target/goml/check";
const PROJECT_BUILD_OUTPUT_DIR: &str = "target/goml/build";
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
    Check(ProjectCommandArgs),
    Build(ProjectCommandArgs),
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
    #[arg(default_value = ".")]
    target: PathBuf,
    #[arg(long = "dry-run")]
    dry_run: bool,
    #[arg(long = "compiler")]
    compiler: Option<PathBuf>,
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
    entry_path: PathBuf,
    dependencies: BTreeMap<String, String>,
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

struct ProjectCommandPlan {
    commands: Vec<PlannedCompilerCommand>,
}

#[derive(Clone)]
struct BuildPackage {
    input_files: Vec<PathBuf>,
    imports: HashSet<String>,
    output: PathBuf,
}

#[derive(Default)]
struct ExternalPackagesPlan {
    packages: HashMap<String, BuildPackage>,
    order: Vec<String>,
    declared_names: HashMap<String, String>,
}

#[derive(Clone, Copy)]
enum ProjectStage {
    Check,
    Build,
}

impl ProjectStage {
    fn output_root(self) -> &'static str {
        match self {
            Self::Check => PROJECT_CHECK_OUTPUT_DIR,
            Self::Build => PROJECT_BUILD_OUTPUT_DIR,
        }
    }

    fn label(self) -> &'static str {
        match self {
            Self::Check => "project check",
            Self::Build => "project build",
        }
    }
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
        }
    }

    fn display(&self) -> String {
        let args = self.to_args();
        let mut parts = Vec::with_capacity(args.len() + 1);
        parts.push("gomlc".to_string());
        parts.extend(args.iter().map(|arg| shell_escape(&arg.to_string_lossy())));
        parts.join(" ")
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

fn execute_project_check(args: ProjectCommandArgs) -> anyhow::Result<()> {
    let project = load_project(&args.target)?;
    let plan = build_project_check_plan(&project)?;
    execute_planned_commands(
        &project.module_dir,
        plan.commands,
        args.dry_run,
        args.compiler.as_deref(),
    )
}

fn execute_project_build(args: ProjectCommandArgs) -> anyhow::Result<()> {
    let project = load_project(&args.target)?;
    let plan = build_project_build_plan(&project)?;
    execute_planned_commands(
        &project.module_dir,
        plan.commands,
        args.dry_run,
        args.compiler.as_deref(),
    )
}

fn build_project_check_plan(project: &ProjectContext) -> anyhow::Result<ProjectCommandPlan> {
    build_project_plan(project, ProjectStage::Check)
}

fn build_project_build_plan(project: &ProjectContext) -> anyhow::Result<ProjectCommandPlan> {
    build_project_plan(project, ProjectStage::Build)
}

fn build_project_plan(
    project: &ProjectContext,
    stage: ProjectStage,
) -> anyhow::Result<ProjectCommandPlan> {
    let external = build_external_packages_plan(project, stage.output_root())?;
    let external_imports =
        goml_project::package_graph::ExternalImports::new(external.declared_names.clone());
    let graph = goml_project::package_graph::discover_project_packages(
        &project.module_dir,
        &project.entry_path,
        &external_imports,
    )
    .map_err(|err| anyhow!("{} failed: {}", stage.label(), err))?;
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
                output: local_artifact_base(stage.output_root(), &package_name),
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
            ProjectStage::Build => PlannedCompilerCommand::Build(command),
        });
        interface_outputs.insert(package_name, package.output.with_extension("interface"));
        if matches!(stage, ProjectStage::Build) {
            core_outputs.push(package.output.with_extension("core"));
        }
    }

    if matches!(stage, ProjectStage::Build) {
        commands.push(PlannedCompilerCommand::Link(LinkCompilerCommand {
            input_cores: core_outputs,
            entry_package: graph.entry_package,
            output: PathBuf::from(PROJECT_GO_OUTPUT),
        }));
    }
    Ok(ProjectCommandPlan { commands })
}

fn build_external_packages_plan(
    project: &ProjectContext,
    output_root: &str,
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
    output_root: &str,
    module: &goml_project::registry::ResolvedModule,
    package: &str,
) -> PathBuf {
    let mut path = PathBuf::from(output_root)
        .join("deps")
        .join(&module.coord.owner)
        .join(&module.coord.module)
        .join(module.version.display())
        .join("pkg");
    for segment in package.split("::") {
        path.push(segment);
    }
    path.join("package")
}

fn local_artifact_base(output_root: &str, package: &str) -> PathBuf {
    let mut path = PathBuf::from(output_root).join("pkg");
    for segment in package.split("::") {
        path.push(segment);
    }
    path.join("package")
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

fn execute_planned_commands(
    module_dir: &Path,
    commands: Vec<PlannedCompilerCommand>,
    dry_run: bool,
    compiler: Option<&Path>,
) -> anyhow::Result<()> {
    if dry_run {
        for command in commands.iter() {
            println!("{}", command.display());
        }
        return Ok(());
    }

    let executable = gomlc::resolve(compiler)?;
    gomlc::verify(&executable)?;
    for command in commands {
        let display = command.display();
        let args = command.to_args();
        let status = gomlc::execute(&executable, &args, Some(module_dir))
            .with_context(|| format!("failed to execute {display}"))?;
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

fn load_project(target: &Path) -> anyhow::Result<ProjectContext> {
    let cwd = std::env::current_dir().context("failed to read current directory")?;
    let target = if target.is_absolute() {
        target.to_path_buf()
    } else {
        cwd.join(target)
    };
    let (target_dir, entry_path) = if target.is_file() {
        let dir = target
            .parent()
            .filter(|path| !path.as_os_str().is_empty())
            .unwrap_or_else(|| Path::new("."))
            .to_path_buf();
        (dir, target)
    } else if target.is_dir() {
        let mut sources = fs::read_dir(&target)
            .with_context(|| format!("failed to read package directory {}", target.display()))?
            .filter_map(Result::ok)
            .map(|entry| entry.path())
            .filter(|path| path.is_file() && path.extension().is_some_and(|ext| ext == "gom"))
            .collect::<Vec<_>>();
        sources.sort();
        let entry = sources
            .first()
            .cloned()
            .ok_or_else(|| anyhow!("package directory {} has no .gom files", target.display()))?;
        (target, entry)
    } else {
        bail!("build target {} does not exist", target.display());
    };
    let (module_dir, _) = find_module_root(&target_dir)
        .map_err(anyhow::Error::msg)?
        .ok_or_else(|| {
            anyhow!(
                "no goml.toml with [module] section found in ancestors of {}",
                target_dir.display()
            )
        })?;
    let manifest =
        load_module_manifest(&module_dir.join("goml.toml")).map_err(anyhow::Error::msg)?;
    Ok(ProjectContext {
        module_dir,
        entry_path,
        dependencies: manifest.dependencies,
    })
}
