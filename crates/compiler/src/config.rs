use std::path::Path;
use std::str::FromStr;

use serde::Deserialize;

#[derive(Debug, Clone, Deserialize)]
pub struct GomlConfig {
    pub module: Option<ModuleConfig>,
    pub package: PackageConfig,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ModuleConfig {
    pub name: String,
}

#[derive(Debug, Clone, Deserialize)]
pub struct PackageConfig {
    pub name: String,
    #[serde(default = "default_entry")]
    pub entry: String,
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
}
