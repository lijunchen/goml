use std::fs;
use std::path::Path;

use anyhow::{Context, Result, anyhow};
use compiler::pipeline::with_compiler_stack;

use crate::cli::{LinkArgs, PackageCommandArgs, TestLinkArgs};

const PRETTY_WIDTH: usize = 120;

pub(crate) fn check(args: PackageCommandArgs) -> Result<()> {
    write_check(args, false)
}

pub(crate) fn test_check(args: PackageCommandArgs) -> Result<()> {
    write_check(args, true)
}

fn write_check(args: PackageCommandArgs, test: bool) -> Result<()> {
    let inputs = compiler::pipeline::separate::PackageInputs {
        package: args.package,
        input_files: args.input,
        interface_files: args.interface_path,
    };
    let unit = if test {
        compiler::pipeline::separate::check_test_package(inputs)
    } else {
        compiler::pipeline::separate::check_package(inputs)
    }
    .map_err(|err| anyhow!("check failed: {:?}", err))?;

    write_json(
        &args.output.with_extension("interface"),
        with_compiler_stack(|| serde_json::to_string_pretty(&unit))?,
    )
}

pub(crate) fn build(args: PackageCommandArgs) -> Result<()> {
    write_build(args, false)
}

pub(crate) fn test_build(args: PackageCommandArgs) -> Result<()> {
    write_build(args, true)
}

fn write_build(args: PackageCommandArgs, test: bool) -> Result<()> {
    let inputs = compiler::pipeline::separate::PackageInputs {
        package: args.package,
        input_files: args.input,
        interface_files: args.interface_path,
    };
    let unit = if test {
        compiler::pipeline::separate::build_test_package(inputs)
    } else {
        compiler::pipeline::separate::build_package(inputs)
    }
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

pub(crate) fn test_link(args: TestLinkArgs) -> Result<()> {
    let mut units = Vec::new();
    for path in args.input {
        let unit = compiler::pipeline::separate::read_core(&path)
            .map_err(|err| anyhow!("test link failed: {:?} ({})", err, path.display()))?;
        units.push(unit);
    }

    let output = compiler::pipeline::separate::link_test_cores_multi(&args.package, units)
        .map_err(|err| anyhow!("test link failed: {:?}", err))?;
    let go_source =
        with_compiler_stack(|| output.link.go.to_pretty(&output.link.goenv, PRETTY_WIDTH));
    if let Some(parent) = args.output.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create directory {}", parent.display()))?;
    }
    fs::write(&args.output, go_source)
        .with_context(|| format!("failed to write {}", args.output.display()))?;
    write_json(
        &args.manifest,
        with_compiler_stack(|| serde_json::to_string_pretty(&output.tests))?,
    )
}

pub(crate) fn link(args: LinkArgs) -> Result<()> {
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
