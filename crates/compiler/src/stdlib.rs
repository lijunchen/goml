use std::{path::PathBuf, sync::OnceLock};

use crate::{
    artifact::{CoreUnit, InterfaceUnit},
    config::goml_std_dir,
    external::{self, ExternalModuleArtifact},
};

static STDLIB_ARTIFACT: OnceLock<Result<ExternalModuleArtifact, String>> = OnceLock::new();

fn source_tree_stdlib_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../stdlib/std")
}

fn validate_stdlib_root(path: PathBuf) -> Result<PathBuf, String> {
    if path.join("goml.toml").is_file() {
        return Ok(path);
    }
    Err(format!(
        "std library not found at {}; install std into GOML_HOME/lib/std or set GOML_STD_PATH",
        path.display()
    ))
}

fn stdlib_root() -> Result<PathBuf, String> {
    if let Some(path) = std::env::var_os("GOML_STD_PATH")
        && !path.is_empty()
    {
        return validate_stdlib_root(PathBuf::from(path));
    }

    let home_std = goml_std_dir()?;
    if home_std.join("goml.toml").is_file() {
        return Ok(home_std);
    }

    let dev_std = source_tree_stdlib_root();
    if dev_std.join("goml.toml").is_file() {
        return Ok(dev_std);
    }

    validate_stdlib_root(home_std)
}

pub fn stdlib_artifact() -> Result<ExternalModuleArtifact, String> {
    STDLIB_ARTIFACT
        .get_or_init(|| stdlib_root().and_then(external::compile_std_module))
        .clone()
}

pub fn stdlib_package_interface(package: &str) -> Result<InterfaceUnit, String> {
    stdlib_artifact()?
        .packages
        .get(package)
        .map(|artifact| artifact.interface.clone())
        .ok_or_else(|| format!("standard library package {} not found", package))
}

pub fn stdlib_cores() -> Result<Vec<CoreUnit>, String> {
    Ok(stdlib_artifact()?
        .packages
        .values()
        .map(|artifact| artifact.core.clone())
        .collect())
}
