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
    pub package: String,
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
struct RawCrateDependency {
    #[serde(default)]
    package: Option<String>,
    version: String,
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
    let value = toml::from_str::<toml::Value>(&content)
        .map_err(|e| format!("failed to parse {}: {}", path.display(), e))?;
    validate_manifest_shape(&value)
        .map_err(|e| format!("failed to parse {}: {}", path.display(), e))?;
    let manifest: RawCrateManifest = toml::from_str(&content)
        .map_err(|e| format!("failed to parse {}: {}", path.display(), e))?;
    manifest
        .into_manifest()
        .map_err(|e| format!("failed to parse {}: {}", path.display(), e))
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

impl RawCrateManifest {
    fn into_manifest(self) -> Result<CrateManifest, String> {
        let dependencies = self
            .dependencies
            .into_iter()
            .map(|(alias, dep)| dependency_from_raw(alias, dep))
            .collect::<Result<_, _>>()?;
        Ok(CrateManifest {
            crate_config: self.crate_config,
            dependencies,
        })
    }
}

impl CrateManifest {
    pub fn dependency_versions(&self) -> BTreeMap<String, String> {
        self.dependencies
            .iter()
            .map(|(_alias, dep)| (dep.package.clone(), dep.version.clone()))
            .collect()
    }
}

fn validate_manifest_shape(value: &toml::Value) -> Result<(), String> {
    if value.get("module").is_some() {
        return Err("old [module] manifests are no longer supported; use [crate]".to_string());
    }
    if value.get("package").is_some() {
        return Err("old [package] manifests are no longer supported; use [crate]".to_string());
    }
    let Some(dependencies) = value.get("dependencies") else {
        return Ok(());
    };
    let Some(dependencies) = dependencies.as_table() else {
        return Err("[dependencies] must be a table".to_string());
    };
    for (alias, dependency) in dependencies {
        validate_dependency_alias(alias)?;
        if !dependency.is_table() {
            return Err(format!(
                "dependency `{alias}` must use `{alias} = {{ package = \"owner::module\", version = \"...\" }}`"
            ));
        }
    }
    Ok(())
}

fn dependency_from_raw(
    alias: String,
    value: RawCrateDependency,
) -> Result<(String, CrateDependency), String> {
    validate_dependency_alias(&alias)?;
    let RawCrateDependency { package, version } = value;
    let Some(package) = package else {
        return Err(format!(
            "dependency `{alias}` is missing `package`; use `{alias} = {{ package = \"owner::module\", version = \"{version}\" }}`"
        ));
    };
    if !is_valid_registry_package(&package) {
        return Err(format!(
            "dependency `{alias}` package `{package}` is invalid: expected owner::module"
        ));
    }
    Ok((alias, CrateDependency { package, version }))
}

fn validate_dependency_alias(alias: &str) -> Result<(), String> {
    if let Some((_, module)) = alias.split_once("::") {
        return Err(format!(
            "dependency key `{alias}` is a registry coordinate; use `{module} = {{ package = \"{alias}\", version = \"...\" }}`"
        ));
    }
    if !is_valid_identifier(&alias) {
        return Err(format!(
            "dependency alias `{alias}` is invalid: expected identifier [A-Za-z_][A-Za-z0-9_]*"
        ));
    }
    Ok(())
}

fn is_valid_registry_package(package: &str) -> bool {
    let Some((owner, module)) = package.split_once("::") else {
        return false;
    };
    package.matches("::").count() == 1 && is_valid_identifier(owner) && is_valid_identifier(module)
}

fn is_valid_identifier(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !(first == '_' || first.is_ascii_alphabetic()) {
        return false;
    }
    chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
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
json = { package = "bob::json", version = "0.4.0" }
"#,
        )
        .unwrap();

        let manifest = load_crate_manifest(&manifest).unwrap();
        assert_eq!(manifest.crate_config.as_ref().unwrap().name, "hello");
        assert_eq!(
            manifest.dependencies.get("http"),
            Some(&CrateDependency {
                package: "alice::http".to_string(),
                version: "1.2.3".to_string()
            })
        );
        assert_eq!(
            manifest.dependencies.get("json"),
            Some(&CrateDependency {
                package: "bob::json".to_string(),
                version: "0.4.0".to_string()
            })
        );
        assert_eq!(
            manifest.dependency_versions().get("alice::http"),
            Some(&"1.2.3".to_string())
        );
        assert_eq!(
            manifest.dependency_versions().get("bob::json"),
            Some(&"0.4.0".to_string())
        );
    }

    #[test]
    fn load_crate_manifest_rejects_legacy_sections() {
        let dir = tempfile::tempdir().unwrap();
        let manifest = dir.path().join("goml.toml");

        std::fs::write(&manifest, "[module]\nname = \"hello\"\n").unwrap();
        let err = load_crate_manifest(&manifest).unwrap_err();
        assert!(err.contains("old [module] manifests are no longer supported"));

        std::fs::write(&manifest, "[package]\nname = \"hello\"\n").unwrap();
        let err = load_crate_manifest(&manifest).unwrap_err();
        assert!(err.contains("old [package] manifests are no longer supported"));
    }

    #[test]
    fn load_crate_manifest_rejects_legacy_dependency_forms() {
        let dir = tempfile::tempdir().unwrap();
        let manifest = dir.path().join("goml.toml");

        std::fs::write(
            &manifest,
            r#"[crate]
name = "hello"

[dependencies]
"alice::http" = "1.2.3"
"#,
        )
        .unwrap();
        let err = load_crate_manifest(&manifest).unwrap_err();
        assert!(err.contains("dependency key `alice::http` is a registry coordinate"));

        std::fs::write(
            &manifest,
            r#"[crate]
name = "hello"

[dependencies]
http = "1.2.3"
"#,
        )
        .unwrap();
        let err = load_crate_manifest(&manifest).unwrap_err();
        assert!(err.contains("dependency `http` must use"));
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
