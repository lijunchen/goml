use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::path::{Path, PathBuf};

use serde::Deserialize;

use crate::config::{
    ModuleManifest, UserConfig, ensure_goml_home_layout, goml_cache_dir, goml_home_dir,
    load_module_manifest,
};

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
        let Some((owner, module)) = input.split_once("::") else {
            return Err(format!(
                "invalid module coordinate `{input}`: expected owner::module"
            ));
        };
        if !is_valid_ident(owner) || !is_valid_ident(module) {
            return Err(format!(
                "invalid module coordinate `{input}`: expected owner::module"
            ));
        }
        if owner == "std" {
            return Err(format!(
                "invalid module coordinate `{input}`: owner std is reserved"
            ));
        }
        Ok(Self {
            owner: owner.to_string(),
            module: module.to_string(),
        })
    }

    pub fn display(&self) -> String {
        format!("{}::{}", self.owner, self.module)
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
    pub manifest: ModuleManifest,
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
        SemVer::parse(&self.module_entry(coord)?.latest)
    }

    pub fn available_versions(&self, coord: &ModuleCoord) -> Result<Vec<SemVer>, String> {
        let mut versions = self
            .module_entry(coord)?
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
        self.available_versions(&requirement.coord)?
            .into_iter()
            .find(|version| version >= &requirement.min_version)
            .ok_or_else(|| {
                format!(
                    "registry has no version for {} satisfying >= {}",
                    requirement.coord.display(),
                    requirement.min_version.display()
                )
            })
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
        let manifest = load_module_manifest(&manifest_path)?;
        if manifest.module.path != coord.display() {
            return Err(format!(
                "registry module {}@{} declares module path `{}` in {}",
                coord.display(),
                version.display(),
                manifest.module.path,
                manifest_path.display()
            ));
        }
        Ok(ResolvedModule {
            coord: coord.clone(),
            version: version.clone(),
            manifest_path,
            root_dir,
            manifest,
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
        for (dep_coord, dep_version) in module.manifest.dependencies.iter() {
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

pub fn topo_sort_modules(graph: &ResolvedModuleGraph) -> Result<Vec<ModuleCoord>, String> {
    let mut indeg = BTreeMap::<ModuleCoord, usize>::new();
    let mut edges = BTreeMap::<ModuleCoord, Vec<ModuleCoord>>::new();
    for coord in graph.modules.keys() {
        indeg.entry(coord.clone()).or_insert(0);
        edges.entry(coord.clone()).or_default();
    }
    for (coord, module) in graph.modules.iter() {
        for dependency in module.manifest.dependencies.keys() {
            let dependency = ModuleCoord::parse(dependency)?;
            if !graph.modules.contains_key(&dependency) {
                continue;
            }
            edges.entry(dependency).or_default().push(coord.clone());
            *indeg.entry(coord.clone()).or_insert(0) += 1;
        }
    }

    let mut ready = indeg
        .iter()
        .filter_map(|(coord, degree)| (*degree == 0).then_some(coord.clone()))
        .collect::<BTreeSet<_>>();
    let mut order = Vec::new();
    while let Some(coord) = ready.pop_first() {
        order.push(coord.clone());
        if let Some(nexts) = edges.get(&coord) {
            for next in nexts {
                if let Some(degree) = indeg.get_mut(next) {
                    *degree -= 1;
                    if *degree == 0 {
                        ready.insert(next.clone());
                    }
                }
            }
        }
    }
    if order.len() != graph.modules.len() {
        return Err("module dependency cycle detected in registry dependencies".to_string());
    }
    Ok(order)
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
    ensure_goml_home_layout()?;
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
