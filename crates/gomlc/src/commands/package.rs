use std::fs;
use std::path::Path;

use anyhow::{Context, Result, anyhow};
use compiler::pipeline::with_compiler_stack;

use crate::cli::{LinkArgs, PackageCommandArgs};

const PRETTY_WIDTH: usize = 120;

pub fn check(args: PackageCommandArgs) -> Result<()> {
    let unit =
        compiler::pipeline::separate::check_package(compiler::pipeline::separate::PackageInputs {
            package: args.package,
            input_files: args.input,
            interface_files: args.interface_path,
        })
        .map_err(|err| anyhow!("check failed: {:?}", err))?;

    write_json(
        &args.output.with_extension("interface"),
        with_compiler_stack(|| serde_json::to_string_pretty(&unit))?,
    )
}

pub fn build(args: PackageCommandArgs) -> Result<()> {
    let unit =
        compiler::pipeline::separate::build_package(compiler::pipeline::separate::PackageInputs {
            package: args.package,
            input_files: args.input,
            interface_files: args.interface_path,
        })
        .map_err(|err| anyhow!("build failed: {:?}", err))?;

    write_json(
        &args.output.with_extension("interface"),
        with_compiler_stack(|| serde_json::to_string_pretty(&unit.interface))?,
    )?;
    write_json(
        &args.output.with_extension("core"),
        with_compiler_stack(|| serde_json::to_string_pretty(&unit))?,
    )
}

pub fn link(args: LinkArgs) -> Result<()> {
    let mut units = Vec::new();
    for path in args.input {
        let unit = compiler::pipeline::separate::read_core(&path)
            .map_err(|err| anyhow!("link failed: {:?} ({})", err, path.display()))?;
        units.push(unit);
    }

    let linked = compiler::pipeline::separate::link_cores(&args.entry, units)
        .map_err(|err| anyhow!("link failed: {:?}", err))?;
    let go_source = with_compiler_stack(|| linked.go.to_pretty(&linked.goenv, PRETTY_WIDTH));
    if let Some(parent) = args.output.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create directory {}", parent.display()))?;
    }
    fs::write(&args.output, go_source)
        .with_context(|| format!("failed to write {}", args.output.display()))?;
    Ok(())
}

fn write_json(path: &Path, json: String) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create directory {}", parent.display()))?;
    }
    fs::write(path, json).with_context(|| format!("failed to write {}", path.display()))?;
    Ok(())
}
