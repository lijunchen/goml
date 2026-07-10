mod package;
mod run_single;
mod version;

use anyhow::Result;

use crate::cli::Commands;

pub(crate) fn execute(command: Commands) -> Result<()> {
    match command {
        Commands::Check(args) => package::check(args),
        Commands::Build(args) => package::build(args),
        Commands::Link(args) => package::link(args),
        Commands::RunSingle(args) => run_single::execute(args),
        Commands::Version(args) => version::execute(args),
    }
}
