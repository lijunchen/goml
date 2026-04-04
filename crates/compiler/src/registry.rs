use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::path::{Path, PathBuf};

use serde::Deserialize;

use crate::config::{GomlConfig, UserConfig, goml_home_dir};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SemVer {
    pub major: u64,
    pub minor: u64,
    pub patch: u64,
}

impl SemVer {
    pub fn parse(input: &str) -> Result<Self, String> {
        let mut parts = input.split('.');
        let major = parse_semver_part(parts.next(), input)?;
        let minor = parse_semver_part(parts.next(), input)?;
        let patch = parse_semver_part(parts.next(), input)?;
        if parts.next().is_some() {
            return Err(format!("invalid semver `{input}`"));
        }
        Ok(Self {
            major,
            minor,
            patch,
        })
    }

    pub fn display(&self) -> String {
        format!("{}.{}.{}", self.major, self.minor, self.patch)
    }
}

fn parse_semver_part(part: Option<&str>, input: &str) -> Result<u64, String> {
    let Some(part) = part else {
        return Err(format!("invalid semver `{input}`"));
    };
    if part.is_empty() || !part.chars().all(|ch| ch.is_ascii_digit()) {
        return Err(format!("invalid semver `{input}`"));
    }
    part.parse::<u64>()
        .map_err(|_| format!("invalid semver `{input}`"))
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleCoord {
    pub owner: String,
    pub module: String,
}

impl ModuleCoord {
    pub fn parse(input: &str) -> Result<Self, String> {
        let Some((owner, module)) = input.split_once('/') else {
            return Err(format!(
                "invalid module coordinate `{input}`: expected owner/module"
            ));
        };
        if !is_valid_ident(owner) || !is_valid_ident(module) {
            return Err(format!(
                "invalid module coordinate `{input}`: expected owner/module"
            ));
        }
        Ok(Self {
            owner: owner.to_string(),
            module: module.to_string(),
        })
    }

    pub fn display(&self) -> String {
        format!("{}/{}", self.owner, self.module)
    }
}

fn is_valid_ident(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !(first == '_' || first.is_ascii_alphabetic()) {
        return false;
    }
    chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleRequirement {
    pub coord: ModuleCoord,
    pub min_version: SemVer,
}

impl ModuleRequirement {
    pub fn parse(coord: &str, version: &str) -> Result<Self, String> {
        Ok(Self {
            coord: ModuleCoord::parse(coord)?,
            min_version: SemVer::parse(version)?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedModule {
    pub coord: ModuleCoord,
    pub version: SemVer,
    pub manifest_path: PathBuf,
    pub root_dir: PathBuf,
    pub config: GomlConfig,
}

#[derive(Debug, Clone)]
pub struct ResolvedModuleGraph {
    pub modules: BTreeMap<ModuleCoord, ResolvedModule>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct RegistryIndex {
    #[serde(default)]
    pub modules: BTreeMap<String, RegistryIndexModule>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct RegistryIndexModule {
    pub latest: String,
    #[serde(default)]
    pub versions: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Registry {
    root: PathBuf,
    index: RegistryIndex,
}

impl Registry {
    pub fn load(root: &Path) -> Result<Self, String> {
        let index_path = root.join("index.toml");
        let content = std::fs::read_to_string(&index_path)
            .map_err(|err| format!("failed to read {}: {}", index_path.display(), err))?;
        let index = toml::from_str::<RegistryIndex>(&content)
            .map_err(|err| format!("failed to parse {}: {}", index_path.display(), err))?;
        Ok(Self {
            root: root.to_path_buf(),
            index,
        })
    }

    pub fn root(&self) -> &Path {
        &self.root
    }

    pub fn index(&self) -> &RegistryIndex {
        &self.index
    }

    pub fn latest_version(&self, coord: &ModuleCoord) -> Result<SemVer, String> {
        let entry = self.module_entry(coord)?;
        SemVer::parse(&entry.latest)
    }

    pub fn available_versions(&self, coord: &ModuleCoord) -> Result<Vec<SemVer>, String> {
        let entry = self.module_entry(coord)?;
        let mut versions = entry
            .versions
            .iter()
            .map(|version| SemVer::parse(version))
            .collect::<Result<Vec<_>, _>>()?;
        versions.sort();
        versions.dedup();
        Ok(versions)
    }

    pub fn select_minimum_version(
        &self,
        requirement: &ModuleRequirement,
    ) -> Result<SemVer, String> {
        let versions = self.available_versions(&requirement.coord)?;
        let selected = versions
            .into_iter()
            .find(|version| version >= &requirement.min_version)
            .ok_or_else(|| {
                format!(
                    "registry has no version for {} satisfying >= {}",
                    requirement.coord.display(),
                    requirement.min_version.display()
                )
            })?;
        Ok(selected)
    }

    pub fn load_module(
        &self,
        coord: &ModuleCoord,
        version: &SemVer,
    ) -> Result<ResolvedModule, String> {
        let root_dir = self
            .root
            .join(&coord.owner)
            .join(&coord.module)
            .join(version.display());
        if !root_dir.exists() {
            return Err(format!(
                "registry entry for {}@{} is missing at {}",
                coord.display(),
                version.display(),
                root_dir.display()
            ));
        }
        let manifest_path = root_dir.join("goml.toml");
        let config = GomlConfig::load(&manifest_path)?;
        Ok(ResolvedModule {
            coord: coord.clone(),
            version: version.clone(),
            manifest_path,
            root_dir,
            config,
        })
    }

    fn module_entry(&self, coord: &ModuleCoord) -> Result<&RegistryIndexModule, String> {
        let key = coord.display();
        self.index
            .modules
            .get(&key)
            .ok_or_else(|| format!("module {} not found in registry index", key))
    }
}

pub fn resolve_dependencies(
    registry: &Registry,
    dependencies: &BTreeMap<String, String>,
) -> Result<ResolvedModuleGraph, String> {
    let mut selected = BTreeMap::<ModuleCoord, SemVer>::new();
    let mut queue = VecDeque::new();

    for (coord, version) in dependencies {
        queue.push_back(ModuleRequirement::parse(coord, version)?);
    }

    while let Some(requirement) = queue.pop_front() {
        let chosen = registry.select_minimum_version(&requirement)?;
        let needs_update = match selected.get(&requirement.coord) {
            Some(existing) => chosen > *existing,
            None => true,
        };
        if !needs_update {
            continue;
        }
        selected.insert(requirement.coord.clone(), chosen.clone());

        let module = registry.load_module(&requirement.coord, &chosen)?;
        for (dep_coord, dep_version) in module.config.dependencies.iter() {
            queue.push_back(ModuleRequirement::parse(dep_coord, dep_version)?);
        }
    }

    let mut modules = BTreeMap::new();
    for (coord, version) in selected {
        let module = registry.load_module(&coord, &version)?;
        modules.insert(coord, module);
    }

    Ok(ResolvedModuleGraph { modules })
}

pub fn goml_cache_dir() -> Result<PathBuf, String> {
    Ok(goml_home_dir()?.join("cache"))
}

pub fn cached_registry_dir() -> Result<PathBuf, String> {
    Ok(goml_cache_dir()?.join("registry"))
}

pub fn user_config_path() -> Result<PathBuf, String> {
    Ok(goml_home_dir()?.join("config.toml"))
}

pub fn load_or_create_user_config() -> Result<UserConfig, String> {
    let path = user_config_path()?;
    if path.exists() {
        return UserConfig::load(&path);
    }
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)
            .map_err(|err| format!("failed to create {}: {}", parent.display(), err))?;
    }
    std::fs::write(&path, UserConfig::default_contents())
        .map_err(|err| format!("failed to write {}: {}", path.display(), err))?;
    Ok(UserConfig::default())
}

pub fn default_registry_url() -> Result<String, String> {
    let config = load_or_create_user_config()?;
    if config.registry.default.trim().is_empty() {
        return Err(format!(
            "registry.default is not configured in {}; set it or use --local-registry",
            user_config_path()?.display()
        ));
    }
    Ok(config.registry.default)
}

pub fn validate_registry_consistency(registry: &Registry) -> Result<(), String> {
    let mut missing = Vec::new();
    for (coord_text, entry) in registry.index.modules.iter() {
        let coord = ModuleCoord::parse(coord_text)?;
        let mut versions = BTreeSet::new();
        for version in entry.versions.iter() {
            let version = SemVer::parse(version)?;
            let path = registry
                .root
                .join(&coord.owner)
                .join(&coord.module)
                .join(version.display());
            if !path.exists() {
                missing.push(format!("{}@{}", coord.display(), version.display()));
            }
            versions.insert(version);
        }
        let latest = SemVer::parse(&entry.latest)?;
        if !versions.contains(&latest) {
            return Err(format!(
                "registry index latest version {} for {} is not listed in versions",
                latest.display(),
                coord.display()
            ));
        }
    }
    if !missing.is_empty() {
        missing.sort();
        return Err(format!(
            "registry index references missing module directories: {}",
            missing.join(", ")
        ));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_registry(root: &Path) {
        std::fs::create_dir_all(root.join("alice/http/1.0.0")).unwrap();
        std::fs::create_dir_all(root.join("alice/http/1.2.0")).unwrap();
        std::fs::create_dir_all(root.join("alice/net/0.1.0")).unwrap();
        std::fs::write(
            root.join("index.toml"),
            r#"
[modules."alice/http"]
latest = "1.2.0"
versions = ["1.0.0", "1.2.0"]

[modules."alice/net"]
latest = "0.1.0"
versions = ["0.1.0"]
"#,
        )
        .unwrap();
        std::fs::write(
            root.join("alice/http/1.0.0/goml.toml"),
            r#"
[package]
name = "http"
"#,
        )
        .unwrap();
        std::fs::write(
            root.join("alice/http/1.2.0/goml.toml"),
            r#"
[package]
name = "http"

[dependencies]
"alice/net" = "0.1.0"
"#,
        )
        .unwrap();
        std::fs::write(
            root.join("alice/net/0.1.0/goml.toml"),
            r#"
[package]
name = "net"
"#,
        )
        .unwrap();
    }

    #[test]
    fn parses_semver() {
        assert_eq!(
            SemVer::parse("1.2.3").unwrap(),
            SemVer {
                major: 1,
                minor: 2,
                patch: 3,
            }
        );
        assert!(SemVer::parse("1.2").is_err());
        assert!(SemVer::parse("1.2.3-beta").is_err());
    }

    #[test]
    fn resolves_mvs_graph() {
        let dir = tempfile::tempdir().unwrap();
        sample_registry(dir.path());
        let registry = Registry::load(dir.path()).unwrap();
        let mut deps = BTreeMap::new();
        deps.insert("alice/http".to_string(), "1.0.0".to_string());
        let resolved = resolve_dependencies(&registry, &deps).unwrap();
        assert_eq!(resolved.modules.len(), 1);
        assert_eq!(
            resolved
                .modules
                .get(&ModuleCoord::parse("alice/http").unwrap())
                .unwrap()
                .version
                .display(),
            "1.0.0"
        );

        deps.insert("alice/http".to_string(), "1.2.0".to_string());
        let resolved = resolve_dependencies(&registry, &deps).unwrap();
        assert_eq!(
            resolved
                .modules
                .get(&ModuleCoord::parse("alice/http").unwrap())
                .unwrap()
                .version
                .display(),
            "1.2.0"
        );
        assert!(
            resolved
                .modules
                .contains_key(&ModuleCoord::parse("alice/net").unwrap())
        );
    }

    #[test]
    fn validates_index_consistency() {
        let dir = tempfile::tempdir().unwrap();
        sample_registry(dir.path());
        let registry = Registry::load(dir.path()).unwrap();
        validate_registry_consistency(&registry).unwrap();
    }
}
