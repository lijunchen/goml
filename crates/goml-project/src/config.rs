use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use serde::{Deserialize, Serialize};

pub const DEFAULT_TARGET_DIR: &str = "artifact";

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct ModuleConfig {
    pub path: String,
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct BuildConfig {
    #[serde(default = "default_target_dir", rename = "target-dir")]
    pub target_dir: PathBuf,
}

impl Default for BuildConfig {
    fn default() -> Self {
        Self {
            target_dir: default_target_dir(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleManifest {
    pub module: ModuleConfig,
    pub build: BuildConfig,
    pub dependencies: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
struct RawModuleManifest {
    module: ModuleConfig,
    #[serde(default)]
    build: BuildConfig,
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
    validate_manifest_target_dir(&manifest.build.target_dir)?;
    for (path, version) in manifest.dependencies.iter() {
        crate::registry::ModuleCoord::parse(path)?;
        crate::registry::SemVer::parse(version)?;
    }
    Ok(ModuleManifest {
        module: manifest.module,
        build: manifest.build,
        dependencies: manifest.dependencies,
    })
}

fn default_target_dir() -> PathBuf {
    PathBuf::from(DEFAULT_TARGET_DIR)
}

pub fn validate_manifest_target_dir(path: &Path) -> Result<(), String> {
    if path.as_os_str().is_empty() {
        return Err("build target-dir must not be empty".to_string());
    }
    if path.is_absolute() {
        return Err("build target-dir must be relative to the module root".to_string());
    }
    let mut has_segment = false;
    for component in path.components() {
        match component {
            std::path::Component::Normal(_) => has_segment = true,
            std::path::Component::CurDir => {}
            std::path::Component::ParentDir
            | std::path::Component::RootDir
            | std::path::Component::Prefix(_) => {
                return Err("build target-dir must not escape the module root".to_string());
            }
        }
    }
    if !has_segment {
        return Err("build target-dir must not be the module root".to_string());
    }
    Ok(())
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
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_module_manifest() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("goml.toml");
        std::fs::write(
            &path,
            r#"[module]
path = "acme::hello"

[dependencies]
"alice::http" = "1.2.3"
"#,
        )
        .unwrap();
        let manifest = load_module_manifest(&path).unwrap();
        assert_eq!(manifest.module.path, "acme::hello");
        assert_eq!(manifest.build.target_dir, PathBuf::from("artifact"));
        assert_eq!(
            manifest.dependencies.get("alice::http"),
            Some(&"1.2.3".to_string())
        );
    }

    #[test]
    fn parses_custom_target_directory() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("goml.toml");
        std::fs::write(
            &path,
            r#"[module]
path = "acme::hello"

[build]
target-dir = "out/generated"
"#,
        )
        .unwrap();
        let manifest = load_module_manifest(&path).unwrap();
        assert_eq!(manifest.build.target_dir, PathBuf::from("out/generated"));
    }

    #[test]
    fn rejects_unsafe_target_directories() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("goml.toml");
        for target_dir in ["", ".", "../out"] {
            std::fs::write(
                &path,
                format!(
                    "[module]\npath = \"acme::hello\"\n\n[build]\ntarget-dir = {target_dir:?}\n"
                ),
            )
            .unwrap();
            assert!(load_module_manifest(&path).is_err(), "{target_dir}");
        }
        let absolute = dir.path().join("out");
        std::fs::write(
            &path,
            format!(
                "[module]\npath = \"acme::hello\"\n\n[build]\ntarget-dir = {:?}\n",
                absolute.to_string_lossy()
            ),
        )
        .unwrap();
        assert!(load_module_manifest(&path).is_err());
    }

    #[test]
    fn rejects_crate_manifest() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("goml.toml");
        std::fs::write(&path, "[crate]\nname = \"hello\"\n").unwrap();
        assert!(load_module_manifest(&path).is_err());
    }

    #[test]
    fn rejects_module_path_that_cannot_be_imported() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("goml.toml");
        std::fs::write(&path, "[module]\npath = \"acme::not-importable\"\n").unwrap();
        assert!(load_module_manifest(&path).is_err());
    }

    #[test]
    fn rejects_legacy_module_fields() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("goml.toml");
        std::fs::write(
            &path,
            "[module]\npath = \"acme::hello\"\nkind = \"bin\"\nroot = \"main.gom\"\n",
        )
        .unwrap();
        assert!(load_module_manifest(&path).is_err());
    }

    #[test]
    fn rejects_reserved_project_module_paths() {
        for path in ["main", "builtin", "std", "std::internal"] {
            assert!(validate_project_module_path(path).is_err());
        }
    }

    #[test]
    fn finds_module_root_from_descendant() {
        let dir = tempfile::tempdir().unwrap();
        let nested = dir.path().join("pkg").join("api");
        std::fs::create_dir_all(&nested).unwrap();
        std::fs::write(
            dir.path().join("goml.toml"),
            "[module]\npath = \"acme::hello\"\n",
        )
        .unwrap();
        let (root, module) = find_module_root(&nested).unwrap().unwrap();
        assert_eq!(root, dir.path());
        assert_eq!(module.path, "acme::hello");
    }

    #[test]
    fn malformed_nearest_manifest_is_not_skipped() {
        let dir = tempfile::tempdir().unwrap();
        let nested = dir.path().join("nested");
        std::fs::create_dir_all(&nested).unwrap();
        std::fs::write(
            dir.path().join("goml.toml"),
            "[module]\npath = \"acme::hello\"\n",
        )
        .unwrap();
        std::fs::write(nested.join("goml.toml"), "[crate]\nname = \"old\"\n").unwrap();
        assert!(find_module_root(&nested).is_err());
    }

    #[test]
    fn serialize_default_user_config() {
        let config = UserConfig::default();
        let text = toml::to_string_pretty(&config).unwrap();
        assert_eq!(text, "[registry]\ndefault = \"\"\n");
    }
}
