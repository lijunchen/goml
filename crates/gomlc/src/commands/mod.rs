mod diagnostics;
mod package;
mod run_single;
mod version;

use anyhow::Result;

use crate::cli::Commands;

pub(crate) fn execute(command: Commands) -> Result<()> {
    match command {
        Commands::Check(args) => package::check(args),
        Commands::TestCheck(args) => package::test_check(args),
        Commands::Build(args) => package::build(args),
        Commands::TestBuild(args) => package::test_build(args),
        Commands::Link(args) => package::link(args),
        Commands::TestLink(args) => package::test_link(args),
        Commands::PackageInfo(args) => package::package_info(args),
        Commands::RunSingle(args) => run_single::execute(args),
        Commands::Version(args) => version::execute(args),
    }
}
