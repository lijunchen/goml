mod cli;
mod commands;

use clap::Parser;

fn main() {
    if let Err(err) = commands::execute(cli::Cli::parse().command) {
        eprintln!("{err:#}");
        std::process::exit(1);
    }
}
