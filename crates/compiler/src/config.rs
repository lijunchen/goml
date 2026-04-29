use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct GomlConfig {
    #[serde(rename = "crate", default)]
    pub crate_config: Option<CrateConfig>,
    pub module: Option<ModuleConfig>,
    pub package: PackageConfig,
    #[serde(default)]
    pub dependencies: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct CrateConfig {
    pub name: String,
    #[serde(default)]
    pub kind: Option<CrateKind>,
    #[serde(default)]
    pub root: Option<String>,
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum CrateKind {
    Bin,
    Lib,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ModuleConfig {
    pub name: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct PackageConfig {
    pub name: String,
    #[serde(default = "default_entry")]
    pub entry: String,
}

#[derive(Debug, Clone, Deserialize)]
struct CrateRootConfig {
    #[serde(rename = "crate", default)]
    crate_config: Option<CrateConfig>,
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

fn default_entry() -> String {
    "lib.gom".to_string()
}

impl FromStr for GomlConfig {
    type Err = toml::de::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        toml::from_str(s)
    }
}

impl GomlConfig {
    pub fn load(path: &Path) -> Result<Self, String> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| format!("failed to read {}: {}", path.display(), e))?;
        content
            .parse()
            .map_err(|e| format!("failed to parse {}: {}", path.display(), e))
    }

    pub fn is_module_root(&self) -> bool {
        self.module.is_some()
    }

    pub fn is_crate_root(&self) -> bool {
        self.crate_config.is_some()
    }

    pub fn load_crate_config(path: &Path) -> Result<Option<CrateConfig>, String> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| format!("failed to read {}: {}", path.display(), e))?;
        let root_config: CrateRootConfig = toml::from_str(&content)
            .map_err(|e| format!("failed to parse {}: {}", path.display(), e))?;
        Ok(root_config.crate_config)
    }

    pub fn find_module_root(start_dir: &Path) -> Option<(std::path::PathBuf, Self)> {
        let mut current = start_dir.to_path_buf();
        loop {
            let config_path = current.join("goml.toml");
            if config_path.exists()
                && let Ok(config) = Self::load(&config_path)
                && config.is_module_root()
            {
                return Some((current, config));
            }
            if !current.pop() {
                break;
            }
        }
        None
    }

    pub fn find_crate_root(start_dir: &Path) -> Option<(std::path::PathBuf, CrateConfig)> {
        let mut current = start_dir.to_path_buf();
        loop {
            let config_path = current.join("goml.toml");
            if config_path.exists()
                && let Ok(Some(config)) = Self::load_crate_config(&config_path)
            {
                return Some((current, config));
            }
            if !current.pop() {
                break;
            }
        }
        None
    }

    pub fn find_package_config(dir: &Path) -> Option<Self> {
        let config_path = dir.join("goml.toml");
        if config_path.exists() {
            Self::load(&config_path).ok()
        } else {
            None
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
    fn parse_module_root_config() {
        let content = r#"
[module]
name = "myapp"

[package]
name = "main"
entry = "main.gom"
"#;
        let config: GomlConfig = content.parse().unwrap();
        assert!(config.is_module_root());
        assert!(!config.is_crate_root());
        assert_eq!(config.module.as_ref().unwrap().name, "myapp");
        assert_eq!(config.package.name, "main");
        assert_eq!(config.package.entry, "main.gom");
        assert!(config.dependencies.is_empty());
    }

    #[test]
    fn parse_sub_package_config() {
        let content = r#"
[package]
name = "utils"
"#;
        let config: GomlConfig = content.parse().unwrap();
        assert!(!config.is_module_root());
        assert!(!config.is_crate_root());
        assert_eq!(config.package.name, "utils");
        assert_eq!(config.package.entry, "lib.gom");
        assert!(config.dependencies.is_empty());
    }

    #[test]
    fn parse_lowercase_package_name() {
        let content = r#"
[package]
name = "mypackage"
"#;
        let config: GomlConfig = content.parse().unwrap();
        assert_eq!(config.package.name, "mypackage");
    }

    #[test]
    fn parse_crate_config_with_legacy_package() {
        let content = r#"
[crate]
name = "hello"
kind = "bin"
root = "src/main.gom"

[package]
name = "main"
"#;
        let config: GomlConfig = content.parse().unwrap();
        let crate_config = config.crate_config.as_ref().unwrap();
        assert!(config.is_crate_root());
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

        let crate_config = GomlConfig::load_crate_config(&manifest).unwrap().unwrap();
        assert_eq!(crate_config.name, "hello");
        assert_eq!(crate_config.kind, Some(CrateKind::Lib));
        assert_eq!(crate_config.root, None);
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

        let (root, crate_config) = GomlConfig::find_crate_root(&nested).unwrap();
        assert_eq!(root, dir.path());
        assert_eq!(crate_config.name, "hello");
        assert_eq!(crate_config.root.as_deref(), Some("src/main.gom"));
    }

    #[test]
    fn parse_dependencies() {
        let content = r#"
[module]
name = "myapp"

[package]
name = "main"
entry = "main.gom"

[dependencies]
"alice::http" = "1.2.3"
"bob::json" = "0.4.0"
"#;
        let config: GomlConfig = content.parse().unwrap();
        assert_eq!(
            config.dependencies.get("alice::http"),
            Some(&"1.2.3".to_string())
        );
        assert_eq!(
            config.dependencies.get("bob::json"),
            Some(&"0.4.0".to_string())
        );
    }

    #[test]
    fn serialize_default_user_config() {
        let config = UserConfig::default();
        let text = toml::to_string_pretty(&config).unwrap();
        assert_eq!(text, "[registry]\ndefault = \"\"\n");
    }
}
