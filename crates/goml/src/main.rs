use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use anyhow::{Context, anyhow, bail};
use clap::{Args, Parser, Subcommand};
use compiler::config::GomlConfig;
use compiler::env::{format_compile_diagnostics, format_typer_diagnostics};
use compiler::pipeline::{pipeline::Compilation, pipeline::CompilationError, pipeline::compile};
use parser::format_parser_diagnostics;
use tempfile::tempdir;

const PRETTY_WIDTH: usize = 120;
const PROJECT_GO_OUTPUT: &str = "target/goml/main.go";

#[derive(Parser, Debug)]
#[command(name = "goml", arg_required_else_help = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    Check,
    Build,
    Compiler(CompilerArgs),
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
    #[arg(long = "interface-path")]
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
    interface_paths: Vec<PathBuf>,
    output: PathBuf,
}

struct LinkOptions {
    input_cores: Vec<PathBuf>,
    output: PathBuf,
}

struct ProjectContext {
    module_dir: PathBuf,
    entry_path: PathBuf,
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
        Commands::Check => execute_project_check(),
        Commands::Build => execute_project_build(),
        Commands::Compiler(args) => execute_compiler_command(args.command),
    }
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
                interface_paths: args.interface_path,
                output: args.output,
            };
            execute_compiler_check(options)
        }
        CompilerCommands::Build(args) => {
            let options = PackageCommandOptions {
                package: args.package,
                input_files: args.input,
                interface_paths: args.interface_path,
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

    let compilation = match compile(&options.file_path, &src) {
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
            interface_paths: options.interface_paths,
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
            interface_paths: options.interface_paths,
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

fn execute_project_check() -> anyhow::Result<()> {
    let project = load_project_from_cwd()?;
    let src = fs::read_to_string(&project.entry_path)
        .with_context(|| format!("error reading goml file: {}", project.entry_path.display()))?;

    match compiler::pipeline::pipeline::typecheck_with_packages(&project.entry_path, &src) {
        Ok((_tast, _genv, _diagnostics)) => Ok(()),
        Err(err) => {
            report_compilation_error(&project.entry_path, &src, err);
            std::process::exit(1);
        }
    }
}

fn execute_project_build() -> anyhow::Result<()> {
    let project = load_project_from_cwd()?;
    let src = fs::read_to_string(&project.entry_path)
        .with_context(|| format!("error reading goml file: {}", project.entry_path.display()))?;

    let compilation = match compile(&project.entry_path, &src) {
        Ok(compilation) => compilation,
        Err(err) => {
            report_compilation_error(&project.entry_path, &src, err);
            std::process::exit(1);
        }
    };

    let output = project.module_dir.join(PROJECT_GO_OUTPUT);
    if let Some(parent) = output.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create directory {}", parent.display()))?;
    }

    let go_source = compilation.go.to_pretty(&compilation.goenv, PRETTY_WIDTH);
    fs::write(&output, go_source)
        .with_context(|| format!("failed to write {}", output.display()))?;
    Ok(())
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
        entry_path: module_dir.join(config.package.entry),
        module_dir,
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
