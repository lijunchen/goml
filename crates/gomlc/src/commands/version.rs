use anyhow::Result;

use crate::cli::{VersionArgs, VersionFormat};

pub(crate) fn execute(args: VersionArgs) -> Result<()> {
    match args.format {
        VersionFormat::Text => match (option_env!("GOML_GIT_HASH"), option_env!("GOML_GIT_DATE")) {
            (Some(hash), Some(date)) => {
                println!("gomlc {} ({hash} {date})", env!("CARGO_PKG_VERSION"))
            }
            _ => println!("gomlc {}", env!("CARGO_PKG_VERSION")),
        },
        VersionFormat::Json => {
            let value = serde_json::json!({
                "tool": "gomlc",
                "version": env!("CARGO_PKG_VERSION"),
                "driver_protocol": goml_project::DRIVER_PROTOCOL,
                "artifact_format": compiler::artifact::FORMAT_VERSION,
                "compiler_abi": compiler::artifact::COMPILER_ABI,
                "git_hash": option_env!("GOML_GIT_HASH"),
                "git_date": option_env!("GOML_GIT_DATE"),
            });
            println!("{}", serde_json::to_string(&value)?);
        }
    }
    Ok(())
}
