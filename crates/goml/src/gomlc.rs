use std::ffi::{OsStr, OsString};
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, bail};
use goml_project::config::goml_bin_dir;

pub fn resolve(explicit: Option<&Path>) -> Result<PathBuf> {
    if let Some(path) = explicit {
        return resolve_required(path, "--compiler");
    }

    if let Some(path) = std::env::var_os("GOMLC")
        && !path.is_empty()
    {
        return resolve_required(Path::new(&path), "GOMLC");
    }

    let mut searched = Vec::new();
    if let Ok(current) = std::env::current_exe()
        && let Some(parent) = current.parent()
    {
        let sibling = parent.join(executable_name());
        searched.push(sibling.clone());
        if sibling.is_file() {
            return Ok(sibling);
        }
    }

    if let Ok(bin_dir) = goml_bin_dir() {
        let installed = bin_dir.join(executable_name());
        searched.push(installed.clone());
        if installed.is_file() {
            return Ok(installed);
        }
    }

    if let Some(path) = find_in_path(OsStr::new("gomlc")) {
        return Ok(path);
    }

    let searched = searched
        .iter()
        .map(|path| path.display().to_string())
        .collect::<Vec<_>>()
        .join(", ");
    bail!(
        "gomlc not found; set GOMLC, pass --compiler, or install gomlc{}",
        if searched.is_empty() {
            String::new()
        } else {
            format!(" (searched: {searched}, PATH)")
        }
    )
}

fn resolve_required(path: &Path, source: &str) -> Result<PathBuf> {
    if path.components().count() == 1
        && let Some(found) = find_in_path(path.as_os_str())
    {
        return Ok(found);
    }
    if path.is_file() {
        return Ok(path.to_path_buf());
    }
    bail!(
        "{source} points to missing gomlc executable {}",
        path.display()
    )
}

fn find_in_path(name: &OsStr) -> Option<PathBuf> {
    let path = std::env::var_os("PATH")?;
    for dir in std::env::split_paths(&path) {
        let candidate = dir.join(with_executable_suffix(name));
        if candidate.is_file() {
            return Some(candidate);
        }
    }
    None
}

fn executable_name() -> OsString {
    with_executable_suffix(OsStr::new("gomlc"))
}

fn with_executable_suffix(name: &OsStr) -> OsString {
    let suffix = std::env::consts::EXE_SUFFIX;
    if suffix.is_empty()
        || Path::new(name)
            .extension()
            .is_some_and(|ext| ext == &suffix[1..])
    {
        return name.to_os_string();
    }
    let mut executable = name.to_os_string();
    executable.push(suffix);
    executable
}

pub fn execute(
    executable: &Path,
    args: &[OsString],
    current_dir: Option<&Path>,
) -> Result<std::process::ExitStatus> {
    let mut command = std::process::Command::new(executable);
    command.args(args);
    if let Some(current_dir) = current_dir {
        command.current_dir(current_dir);
    }
    command
        .status()
        .with_context(|| format!("failed to execute {}", executable.display()))
}

pub fn verify(executable: &Path) -> Result<()> {
    let output = std::process::Command::new(executable)
        .args(["version", "--format", "json"])
        .output()
        .with_context(|| format!("failed to query {} version", executable.display()))?;
    if !output.status.success() {
        bail!(
            "{} does not support the gomlc driver protocol",
            executable.display()
        );
    }
    verify_version_data(executable, &output.stdout)
}

fn verify_version_data(executable: &Path, data: &[u8]) -> Result<()> {
    let version: serde_json::Value = serde_json::from_slice(data)
        .with_context(|| format!("{} returned invalid version data", executable.display()))?;
    let protocol = version
        .get("driver_protocol")
        .and_then(serde_json::Value::as_u64)
        .ok_or_else(|| anyhow::anyhow!("{} omitted driver_protocol", executable.display()))?;
    if protocol != goml_project::DRIVER_PROTOCOL {
        bail!(
            "incompatible gomlc driver protocol {protocol}; goml requires {}",
            goml_project::DRIVER_PROTOCOL
        );
    }
    Ok(())
}
