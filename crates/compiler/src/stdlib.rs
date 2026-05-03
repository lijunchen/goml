use std::{path::PathBuf, sync::OnceLock};

use crate::{
    artifact::{CoreUnit, InterfaceUnit},
    external::{self, ExternalModuleArtifact},
};

static STDLIB_ARTIFACT: OnceLock<Result<ExternalModuleArtifact, String>> = OnceLock::new();

fn stdlib_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../stdlib/std")
}

pub fn stdlib_artifact() -> Result<ExternalModuleArtifact, String> {
    STDLIB_ARTIFACT
        .get_or_init(|| external::compile_std_module(stdlib_root()))
        .clone()
}

pub fn stdlib_interface() -> Result<InterfaceUnit, String> {
    Ok(stdlib_artifact()?.interface)
}

pub fn stdlib_core() -> Result<CoreUnit, String> {
    Ok(stdlib_artifact()?.core)
}
