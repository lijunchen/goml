use std::path::PathBuf;

use clap::{Args, Parser, Subcommand, ValueEnum};

#[derive(Parser, Debug)]
#[command(name = "gomlc", arg_required_else_help = true)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    Check(PackageCommandArgs),
    Build(PackageCommandArgs),
    Link(LinkArgs),
    RunSingle(RunArgs),
    Version(VersionArgs),
}

#[derive(Args, Debug)]
pub struct RunArgs {
    #[arg(long = "dump-ast")]
    pub dump_ast: bool,
    #[arg(long = "dump-hir")]
    pub dump_hir: bool,
    #[arg(long = "dump-tast")]
    pub dump_tast: bool,
    #[arg(long = "dump-core")]
    pub dump_core: bool,
    #[arg(long = "dump-mono")]
    pub dump_mono: bool,
    #[arg(long = "dump-lift")]
    pub dump_lift: bool,
    #[arg(long = "dump-anf")]
    pub dump_anf: bool,
    #[arg(long = "dump-go")]
    pub dump_go: bool,
    pub file: PathBuf,
}

#[derive(Args, Debug)]
pub struct PackageCommandArgs {
    #[arg(long)]
    pub package: String,
    #[arg(long, required = true, num_args = 1..)]
    pub input: Vec<PathBuf>,
    #[arg(long = "interface-path", value_name = "INTERFACE_FILE")]
    pub interface_path: Vec<PathBuf>,
    #[arg(long)]
    pub output: PathBuf,
}

#[derive(Args, Debug)]
pub struct LinkArgs {
    #[arg(long, required = true, num_args = 1..)]
    pub input: Vec<PathBuf>,
    #[arg(long)]
    pub entry: String,
    #[arg(long)]
    pub output: PathBuf,
}

#[derive(Args, Debug)]
pub struct VersionArgs {
    #[arg(long, value_enum, default_value_t = VersionFormat::Text)]
    pub format: VersionFormat,
}

#[derive(Clone, Copy, Debug, ValueEnum)]
pub enum VersionFormat {
    Text,
    Json,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum DumpStage {
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
    pub fn label(self) -> &'static str {
        match self {
            Self::Ast => "AST",
            Self::Hir => "HIR",
            Self::Tast => "Typed AST",
            Self::Core => "Core",
            Self::Mono => "Mono",
            Self::Lift => "Lifted",
            Self::Anf => "ANF",
            Self::Go => "Go",
        }
    }

    fn order(self) -> usize {
        match self {
            Self::Ast => 0,
            Self::Hir => 1,
            Self::Tast => 2,
            Self::Core => 3,
            Self::Mono => 4,
            Self::Lift => 5,
            Self::Anf => 6,
            Self::Go => 7,
        }
    }
}

impl RunArgs {
    pub fn dumps(&self) -> Vec<DumpStage> {
        let mut dumps = Vec::new();
        if self.dump_ast {
            dumps.push(DumpStage::Ast);
        }
        if self.dump_hir {
            dumps.push(DumpStage::Hir);
        }
        if self.dump_tast {
            dumps.push(DumpStage::Tast);
        }
        if self.dump_core {
            dumps.push(DumpStage::Core);
        }
        if self.dump_mono {
            dumps.push(DumpStage::Mono);
        }
        if self.dump_lift {
            dumps.push(DumpStage::Lift);
        }
        if self.dump_anf {
            dumps.push(DumpStage::Anf);
        }
        if self.dump_go {
            dumps.push(DumpStage::Go);
        }
        dumps.sort_by_key(|stage| stage.order());
        dumps.dedup();
        dumps
    }
}
