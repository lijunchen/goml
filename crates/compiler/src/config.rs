use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
pub struct CrateConfig {
    pub name: String,
    #[serde(default)]
    pub kind: Option<CrateKind>,
    #[serde(default)]
    pub root: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CrateManifest {
    pub crate_config: Option<CrateConfig>,
    pub dependencies: BTreeMap<String, CrateDependency>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CrateDependency {
    pub package: Option<String>,
    pub version: String,
}

#[derive(Debug, Clone, Deserialize)]
struct RawCrateManifest {
    #[serde(rename = "crate", default)]
    crate_config: Option<CrateConfig>,
    #[serde(default)]
    dependencies: BTreeMap<String, RawCrateDependency>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
enum RawCrateDependency {
    Version(String),
    Detailed {
        #[serde(default)]
        package: Option<String>,
        version: String,
    },
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum CrateKind {
    Bin,
    Lib,
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

pub fn load_crate_config(path: &Path) -> Result<Option<CrateConfig>, String> {
    load_crate_manifest(path).map(|manifest| manifest.crate_config)
}

pub fn load_crate_manifest(path: &Path) -> Result<CrateManifest, String> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| format!("failed to read {}: {}", path.display(), e))?;
    let manifest: RawCrateManifest = toml::from_str(&content)
        .map_err(|e| format!("failed to parse {}: {}", path.display(), e))?;
    Ok(manifest.into())
}

pub fn find_crate_root(start_dir: &Path) -> Option<(PathBuf, CrateConfig)> {
    let mut current = start_dir.to_path_buf();
    loop {
        let config_path = current.join("goml.toml");
        if config_path.exists()
            && let Ok(Some(config)) = load_crate_config(&config_path)
        {
            return Some((current, config));
        }
        if !current.pop() {
            break;
        }
    }
    None
}

impl From<RawCrateManifest> for CrateManifest {
    fn from(value: RawCrateManifest) -> Self {
        let dependencies = value
            .dependencies
            .into_iter()
            .map(|(alias, dep)| (alias, dep.into()))
            .collect();
        Self {
            crate_config: value.crate_config,
            dependencies,
        }
    }
}

impl CrateManifest {
    pub fn dependency_versions(&self) -> BTreeMap<String, String> {
        self.dependencies
            .iter()
            .map(|(alias, dep)| {
                (
                    dep.package.clone().unwrap_or_else(|| alias.clone()),
                    dep.version.clone(),
                )
            })
            .collect()
    }
}

impl From<RawCrateDependency> for CrateDependency {
    fn from(value: RawCrateDependency) -> Self {
        match value {
            RawCrateDependency::Version(version) => Self {
                package: None,
                version,
            },
            RawCrateDependency::Detailed { package, version } => Self { package, version },
        }
    }
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
            .map_err(|e| format!("failed to read {}: {}", path.display(), e))?;
        content
            .parse()
            .map_err(|e| format!("failed to parse {}: {}", path.display(), e))
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_crate_config() {
        let content = r#"
[crate]
name = "hello"
kind = "bin"
root = "src/main.gom"
"#;
        let manifest: RawCrateManifest = toml::from_str(content).unwrap();
        let crate_config = manifest.crate_config.as_ref().unwrap();
        assert_eq!(crate_config.name, "hello");
        assert_eq!(crate_config.kind, Some(CrateKind::Bin));
        assert_eq!(crate_config.root.as_deref(), Some("src/main.gom"));
    }

    #[test]
    fn load_crate_config_without_package_section() {
        let dir = tempfile::tempdir().unwrap();
        let manifest = dir.path().join("goml.toml");
        std::fs::write(
            &manifest,
            r#"[crate]
name = "hello"
kind = "lib"
"#,
        )
        .unwrap();

        let crate_config = load_crate_config(&manifest).unwrap().unwrap();
        assert_eq!(crate_config.name, "hello");
        assert_eq!(crate_config.kind, Some(CrateKind::Lib));
        assert_eq!(crate_config.root, None);
    }

    #[test]
    fn load_crate_manifest_parses_alias_dependencies() {
        let dir = tempfile::tempdir().unwrap();
        let manifest = dir.path().join("goml.toml");
        std::fs::write(
            &manifest,
            r#"[crate]
name = "hello"

[dependencies]
http = { package = "alice::http", version = "1.2.3" }
json = "0.4.0"
"#,
        )
        .unwrap();

        let manifest = load_crate_manifest(&manifest).unwrap();
        assert_eq!(manifest.crate_config.as_ref().unwrap().name, "hello");
        assert_eq!(
            manifest.dependencies.get("http"),
            Some(&CrateDependency {
                package: Some("alice::http".to_string()),
                version: "1.2.3".to_string()
            })
        );
        assert_eq!(
            manifest.dependencies.get("json"),
            Some(&CrateDependency {
                package: None,
                version: "0.4.0".to_string()
            })
        );
        assert_eq!(
            manifest.dependency_versions().get("alice::http"),
            Some(&"1.2.3".to_string())
        );
        assert_eq!(
            manifest.dependency_versions().get("json"),
            Some(&"0.4.0".to_string())
        );
    }

    #[test]
    fn find_crate_root_from_descendant() {
        let dir = tempfile::tempdir().unwrap();
        let nested = dir.path().join("src").join("api");
        std::fs::create_dir_all(&nested).unwrap();
        std::fs::write(
            dir.path().join("goml.toml"),
            r#"[crate]
name = "hello"
root = "src/main.gom"
"#,
        )
        .unwrap();

        let (root, crate_config) = find_crate_root(&nested).unwrap();
        assert_eq!(root, dir.path());
        assert_eq!(crate_config.name, "hello");
        assert_eq!(crate_config.root.as_deref(), Some("src/main.gom"));
    }

    #[test]
    fn serialize_default_user_config() {
        let config = UserConfig::default();
        let text = toml::to_string_pretty(&config).unwrap();
        assert_eq!(text, "[registry]\ndefault = \"\"\n");
    }
}
