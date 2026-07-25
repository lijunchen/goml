use std::fs;
use std::path::Path;

use anyhow::{Context, Result};
use compiler::pipeline::with_compiler_stack;

use crate::cli::{LinkArgs, PackageCommandArgs, PackageInfoArgs, TestLinkArgs};

use super::diagnostics::{compilation_error, source_map_from_paths};

const PRETTY_WIDTH: usize = 120;

pub(crate) fn package_info(mut args: PackageInfoArgs) -> Result<()> {
    args.input.sort();
    args.input.dedup();
    let source_map = source_map_from_paths(&args.input);
    let mut files = Vec::new();
    for path in args.input {
        let source = fs::read_to_string(&path)
            .with_context(|| format!("failed to read {}", path.display()))?;
        let ast = compiler::pipeline::pipeline::parse_ast_file(&path, &source)
            .map_err(|error| compilation_error(error, source_map.clone()))?;
        let uses = ast
            .uses
            .into_iter()
            .map(|use_decl| {
                serde_json::json!({
                    "path": use_decl.path.display(),
                    "alias": use_decl.alias.map(|alias| alias.0),
                })
            })
            .collect::<Vec<_>>();
        files.push(serde_json::json!({
            "path": path.to_string_lossy(),
            "package": ast.package.0,
            "package_explicit": ast.package_explicit,
            "uses": uses,
        }));
    }
    let output = serde_json::json!({
        "protocol_version": 1,
        "files": files,
    });
    println!("{}", serde_json::to_string(&output)?);
    Ok(())
}

pub(crate) fn check(args: PackageCommandArgs) -> Result<()> {
    write_check(args, false)
}

pub(crate) fn test_check(args: PackageCommandArgs) -> Result<()> {
    write_check(args, true)
}

fn write_check(args: PackageCommandArgs, test: bool) -> Result<()> {
    let source_map = source_map_from_paths(&args.input);
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
    .map_err(|error| compilation_error(error, source_map))?;

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
    let source_map = source_map_from_paths(&args.input);
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
    .map_err(|error| compilation_error(error, source_map))?;

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
            .map_err(|error| compilation_error(error, Default::default()))
            .with_context(|| format!("test link failed to read {}", path.display()))?;
        units.push(unit);
    }

    let output = compiler::pipeline::separate::link_test_cores_multi_to_go(&args.package, units)
        .map_err(|error| compilation_error(error, Default::default()))
        .context("test link failed")?;
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
            .map_err(|error| compilation_error(error, Default::default()))
            .with_context(|| format!("link failed to read {}", path.display()))?;
        units.push(unit);
    }

    let linked = compiler::pipeline::separate::link_cores_to_go(&args.entry, units)
        .map_err(|error| compilation_error(error, Default::default()))
        .context("link failed")?;
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
