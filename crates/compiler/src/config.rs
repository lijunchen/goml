use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct GomlConfig {
    pub module: Option<ModuleConfig>,
    pub package: PackageConfig,
    #[serde(default)]
    pub dependencies: BTreeMap<String, String>,
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
