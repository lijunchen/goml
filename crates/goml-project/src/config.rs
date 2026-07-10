use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct ModuleConfig {
    pub path: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleManifest {
    pub module: ModuleConfig,
    pub dependencies: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
struct RawModuleManifest {
    module: ModuleConfig,
    #[serde(default)]
    dependencies: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct UserConfig {
    #[serde(default)]
    pub registry: UserRegistryConfig,
}

#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct UserRegistryConfig {
    #[serde(default)]
    pub default: String,
}

pub fn load_module_manifest(path: &Path) -> Result<ModuleManifest, String> {
    let content = std::fs::read_to_string(path)
        .map_err(|err| format!("failed to read {}: {}", path.display(), err))?;
    let manifest: RawModuleManifest = toml::from_str(&content)
        .map_err(|err| format!("failed to parse {}: {}", path.display(), err))?;
    validate_module_path(&manifest.module.path)?;
    for (path, version) in manifest.dependencies.iter() {
        crate::registry::ModuleCoord::parse(path)?;
        crate::registry::SemVer::parse(version)?;
    }
    Ok(ModuleManifest {
        module: manifest.module,
        dependencies: manifest.dependencies,
    })
}

pub fn find_module_root(start_dir: &Path) -> Result<Option<(PathBuf, ModuleConfig)>, String> {
    let mut current = start_dir.to_path_buf();
    loop {
        let config_path = current.join("goml.toml");
        if config_path.exists() {
            let manifest = load_module_manifest(&config_path)?;
            return Ok(Some((current, manifest.module)));
        }
        if !current.pop() {
            break;
        }
    }
    Ok(None)
}

pub fn validate_module_path(path: &str) -> Result<(), String> {
    let mut segments = path.split("::");
    let Some(first) = segments.next() else {
        return Err("module path must not be empty".to_string());
    };
    if !valid_path_segment(first) || segments.any(|segment| !valid_path_segment(segment)) {
        return Err(format!("invalid module path `{path}`"));
    }
    Ok(())
}

pub fn validate_project_module_path(path: &str) -> Result<(), String> {
    validate_module_path(path)?;
    if path == "main" || path == BUILTIN_PACKAGE || path == STD_PACKAGE || path.starts_with("std::")
    {
        return Err(format!("module path `{path}` is reserved"));
    }
    Ok(())
}

pub fn validate_package_name(name: &str) -> Result<(), String> {
    if valid_path_segment(name) {
        Ok(())
    } else {
        Err(format!("invalid package name `{name}`"))
    }
}

fn valid_path_segment(segment: &str) -> bool {
    let mut chars = segment.chars();
    chars
        .next()
        .is_some_and(|ch| ch == '_' || ch.is_ascii_alphabetic())
        && chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
}

impl FromStr for UserConfig {
    type Err = toml::de::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        toml::from_str(s)
    }
}

impl UserConfig {
    pub fn load(path: &Path) -> Result<Self, String> {
        let content = std::fs::read_to_string(path)
            .map_err(|err| format!("failed to read {}: {}", path.display(), err))?;
        content
            .parse()
            .map_err(|err| format!("failed to parse {}: {}", path.display(), err))
    }

    pub fn default_contents() -> String {
        toml::to_string_pretty(&Self::default())
            .expect("default user config must serialize to TOML")
    }
}

pub fn goml_home_dir() -> Result<PathBuf, String> {
    if let Some(home) = std::env::var_os("GOML_HOME")
        && !home.is_empty()
    {
        return Ok(PathBuf::from(home));
    }
    if let Some(home) = std::env::var_os("HOME")
        && !home.is_empty()
    {
        return Ok(PathBuf::from(home).join(".goml"));
    }
    if let Some(home) = std::env::var_os("USERPROFILE")
        && !home.is_empty()
    {
        return Ok(PathBuf::from(home).join(".goml"));
    }
    Err("failed to determine home directory".to_string())
}

pub fn goml_bin_dir() -> Result<PathBuf, String> {
    Ok(goml_home_dir()?.join("bin"))
}

pub fn goml_lib_dir() -> Result<PathBuf, String> {
    Ok(goml_home_dir()?.join("lib"))
}

pub fn goml_std_dir() -> Result<PathBuf, String> {
    Ok(goml_lib_dir()?.join("std"))
}

pub fn goml_cache_dir() -> Result<PathBuf, String> {
    Ok(goml_home_dir()?.join("cache"))
}

pub fn ensure_goml_home_layout() -> Result<(), String> {
    for dir in [
        goml_home_dir()?,
        goml_bin_dir()?,
        goml_lib_dir()?,
        goml_cache_dir()?,
    ] {
        std::fs::create_dir_all(&dir)
            .map_err(|err| format!("failed to create {}: {}", dir.display(), err))?;
    }
    Ok(())
}

const BUILTIN_PACKAGE: &str = crate::BUILTIN_PACKAGE;
const STD_PACKAGE: &str = crate::STD_PACKAGE;
