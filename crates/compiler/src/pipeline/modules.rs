use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use ::ast::{ast, lower};
use cst::cst::{CstNode, File as CstFile};
use parser::syntax::MySyntaxNode;

use crate::config::{CrateConfig, CrateDependency, CrateKind, load_crate_manifest};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModulePath(Vec<String>);

impl ModulePath {
    pub fn root() -> Self {
        Self(Vec::new())
    }

    pub fn child(&self, name: &str) -> Self {
        let mut segments = self.0.clone();
        segments.push(name.to_string());
        Self(segments)
    }

    pub fn segments(&self) -> &[String] {
        &self.0
    }

    pub fn display(&self) -> String {
        if self.0.is_empty() {
            "crate".to_string()
        } else {
            format!("crate::{}", self.0.join("::"))
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleId(pub usize);

#[derive(Debug, Clone)]
pub struct ModuleUnit {
    pub id: ModuleId,
    pub path: ModulePath,
    pub file_path: PathBuf,
    pub child_dir: PathBuf,
    pub visibility: ast::Visibility,
    pub ast: ast::File,
    pub children: BTreeMap<String, ModuleId>,
}

#[derive(Debug, Clone)]
pub struct CrateUnit {
    pub config: CrateConfig,
    pub dependencies: BTreeMap<String, CrateDependency>,
    pub root_dir: PathBuf,
    pub root_file: PathBuf,
    pub root_mod: ModuleId,
    pub modules: Vec<ModuleUnit>,
}

impl CrateUnit {
    pub fn source_files(&self) -> Vec<crate::hir::SourceFileAst> {
        self.modules
            .iter()
            .map(|module| {
                crate::hir::SourceFileAst::with_module_path(
                    module.file_path.clone(),
                    module.path.segments().to_vec(),
                    module.ast.clone(),
                )
            })
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DiscoveryError {
    MissingCrateManifest {
        dir: PathBuf,
    },
    MissingCrateRoot {
        root_dir: PathBuf,
        candidates: Vec<PathBuf>,
    },
    AmbiguousCrateRoot {
        root_dir: PathBuf,
        candidates: Vec<PathBuf>,
    },
    MissingModule {
        parent: ModulePath,
        name: String,
        candidates: Vec<PathBuf>,
    },
    AmbiguousModule {
        parent: ModulePath,
        name: String,
        candidates: Vec<PathBuf>,
    },
    DuplicateModule {
        parent: ModulePath,
        name: String,
    },
    ModuleCycle {
        first: ModulePath,
        second: ModulePath,
        file_path: PathBuf,
    },
    Parse {
        file_path: PathBuf,
        message: String,
    },
}

pub fn discover_crate_from_dir(crate_dir: &Path) -> Result<CrateUnit, DiscoveryError> {
    let manifest = crate_dir.join("goml.toml");
    let manifest_config =
        load_crate_manifest(&manifest).map_err(|message| DiscoveryError::Parse {
            file_path: manifest.clone(),
            message,
        })?;
    let Some(config) = manifest_config.crate_config else {
        return Err(DiscoveryError::MissingCrateManifest {
            dir: crate_dir.to_path_buf(),
        });
    };
    discover_crate(crate_dir, config, manifest_config.dependencies)
}

pub fn discover_crate_from_file(entry: &Path) -> Result<CrateUnit, DiscoveryError> {
    let root_dir = entry
        .parent()
        .filter(|path| !path.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."))
        .to_path_buf();
    let name = entry
        .file_stem()
        .and_then(|stem| stem.to_str())
        .filter(|stem| !stem.is_empty())
        .unwrap_or("main")
        .to_string();
    let config = CrateConfig {
        name,
        kind: None,
        root: Some(
            entry
                .strip_prefix(&root_dir)
                .unwrap_or(entry)
                .to_string_lossy()
                .to_string(),
        ),
    };
    discover_crate(&root_dir, config, BTreeMap::new())
}

fn discover_crate(
    root_dir: &Path,
    config: CrateConfig,
    dependencies: BTreeMap<String, CrateDependency>,
) -> Result<CrateUnit, DiscoveryError> {
    let root_file = resolve_crate_root(root_dir, &config)?;
    let mut state = DiscoveryState {
        modules: Vec::new(),
        seen_files: BTreeMap::new(),
    };
    let root_mod = state.load_module(
        ModulePath::root(),
        root_file.clone(),
        ast::Visibility::Public,
    )?;
    Ok(CrateUnit {
        config,
        dependencies,
        root_dir: root_dir.to_path_buf(),
        root_file,
        root_mod,
        modules: state.modules,
    })
}

fn resolve_crate_root(root_dir: &Path, config: &CrateConfig) -> Result<PathBuf, DiscoveryError> {
    if let Some(root) = &config.root {
        let root_file = root_dir.join(root);
        if root_file.exists() {
            return Ok(root_file);
        }
        return Err(DiscoveryError::MissingCrateRoot {
            root_dir: root_dir.to_path_buf(),
            candidates: vec![root_file],
        });
    }

    let candidates = crate_root_candidates(root_dir, config.kind);
    let existing = candidates
        .iter()
        .filter(|candidate| candidate.exists())
        .cloned()
        .collect::<Vec<_>>();

    match existing.as_slice() {
        [root] => Ok(root.clone()),
        [] => Err(DiscoveryError::MissingCrateRoot {
            root_dir: root_dir.to_path_buf(),
            candidates,
        }),
        _ => Err(DiscoveryError::AmbiguousCrateRoot {
            root_dir: root_dir.to_path_buf(),
            candidates: existing,
        }),
    }
}

fn crate_root_candidates(root_dir: &Path, kind: Option<CrateKind>) -> Vec<PathBuf> {
    match kind {
        Some(CrateKind::Bin) => vec![root_dir.join("src/main.gom"), root_dir.join("main.gom")],
        Some(CrateKind::Lib) => vec![root_dir.join("src/lib.gom"), root_dir.join("lib.gom")],
        None => vec![
            root_dir.join("src/main.gom"),
            root_dir.join("src/lib.gom"),
            root_dir.join("main.gom"),
            root_dir.join("lib.gom"),
        ],
    }
}

struct DiscoveryState {
    modules: Vec<ModuleUnit>,
    seen_files: BTreeMap<PathBuf, ModulePath>,
}

impl DiscoveryState {
    fn load_module(
        &mut self,
        module_path: ModulePath,
        file_path: PathBuf,
        visibility: ast::Visibility,
    ) -> Result<ModuleId, DiscoveryError> {
        let canonical = canonical_file_key(&file_path);
        if let Some(first) = self.seen_files.get(&canonical) {
            return Err(DiscoveryError::ModuleCycle {
                first: first.clone(),
                second: module_path,
                file_path,
            });
        }
        self.seen_files.insert(canonical, module_path.clone());

        let source = std::fs::read_to_string(&file_path).map_err(|err| DiscoveryError::Parse {
            file_path: file_path.clone(),
            message: format!("failed to read {}: {}", file_path.display(), err),
        })?;
        let parsed = parser::parse(&file_path, &source);
        if parsed.has_errors() {
            return Err(DiscoveryError::Parse {
                file_path,
                message: parsed.format_errors(&source).join("\n"),
            });
        }
        let root = MySyntaxNode::new_root(parsed.green_node.clone());
        let cst = CstFile::cast(root).ok_or_else(|| DiscoveryError::Parse {
            file_path: file_path.clone(),
            message: "failed to cast CST root".to_string(),
        })?;
        let mod_names = collect_mod_names(&module_path, &cst)?;
        let lowered = lower::lower(cst);
        if lowered.has_errors() {
            return Err(DiscoveryError::Parse {
                file_path: file_path.clone(),
                message: lowered
                    .diagnostics()
                    .iter()
                    .map(|diagnostic| diagnostic.message().to_string())
                    .collect::<Vec<_>>()
                    .join("\n"),
            });
        }
        let Some(ast) = lowered.into_ast() else {
            return Err(DiscoveryError::Parse {
                file_path: file_path.clone(),
                message: "failed to lower AST".to_string(),
            });
        };

        let id = ModuleId(self.modules.len());
        let child_dir = module_child_dir(&module_path, &file_path);
        self.modules.push(ModuleUnit {
            id,
            path: module_path.clone(),
            file_path: file_path.clone(),
            child_dir: child_dir.clone(),
            visibility,
            ast,
            children: BTreeMap::new(),
        });

        for (name, visibility) in mod_names {
            let child_file = resolve_mod_file(&module_path, &child_dir, &name)?;
            let child_id = self.load_module(module_path.child(&name), child_file, visibility)?;
            self.modules[id.0].children.insert(name, child_id);
        }

        Ok(id)
    }
}

fn collect_mod_names(
    module_path: &ModulePath,
    cst: &CstFile,
) -> Result<Vec<(String, ast::Visibility)>, DiscoveryError> {
    let mut names = Vec::new();
    let mut seen = BTreeSet::new();
    for item in cst.items() {
        let cst::nodes::Item::Mod(module) = item else {
            continue;
        };
        let Some(name) = module.name_token().map(|token| token.to_string()) else {
            continue;
        };
        if !seen.insert(name.clone()) {
            return Err(DiscoveryError::DuplicateModule {
                parent: module_path.clone(),
                name,
            });
        }
        let visibility = mod_visibility(&module);
        names.push((name, visibility));
    }
    Ok(names)
}

fn mod_visibility(module: &cst::nodes::Mod) -> ast::Visibility {
    if module
        .syntax()
        .children_with_tokens()
        .filter_map(|it| it.into_token())
        .any(|token| token.kind() == parser::syntax::MySyntaxKind::PubKeyword)
    {
        ast::Visibility::Public
    } else {
        ast::Visibility::Private
    }
}

fn resolve_mod_file(
    parent: &ModulePath,
    child_dir: &Path,
    name: &str,
) -> Result<PathBuf, DiscoveryError> {
    let flat = child_dir.join(format!("{name}.gom"));
    let nested = child_dir.join(name).join("mod.gom");
    match (flat.exists(), nested.exists()) {
        (true, false) => Ok(flat),
        (false, true) => Ok(nested),
        (false, false) => Err(DiscoveryError::MissingModule {
            parent: parent.clone(),
            name: name.to_string(),
            candidates: vec![flat, nested],
        }),
        (true, true) => Err(DiscoveryError::AmbiguousModule {
            parent: parent.clone(),
            name: name.to_string(),
            candidates: vec![flat, nested],
        }),
    }
}

fn module_child_dir(module_path: &ModulePath, file_path: &Path) -> PathBuf {
    let parent = file_path
        .parent()
        .filter(|path| !path.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    if module_path.segments().is_empty()
        || file_path.file_name().is_some_and(|name| name == "mod.gom")
    {
        parent.to_path_buf()
    } else {
        parent.join(
            module_path
                .segments()
                .last()
                .expect("module path has last segment"),
        )
    }
}

fn canonical_file_key(path: &Path) -> PathBuf {
    path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn discovers_explicit_root_and_flat_child_module() {
        let dir = tempfile::tempdir().unwrap();
        write(
            dir.path().join("goml.toml"),
            r#"[crate]
name = "hello"
root = "src/main.gom"

[dependencies]
http = { package = "alice::http", version = "1.2.3" }
"#,
        );
        write(dir.path().join("src/main.gom"), "mod math;\nfn main() {}\n");
        write(dir.path().join("src/math.gom"), "fn add() -> int32 { 1 }\n");

        let unit = discover_crate_from_dir(dir.path()).unwrap();
        assert_eq!(unit.config.name, "hello");
        assert_eq!(
            unit.dependencies
                .get("http")
                .map(|dep| dep.version.as_str()),
            Some("1.2.3")
        );
        assert_eq!(unit.modules.len(), 2);
        assert_eq!(unit.modules[0].path.display(), "crate");
        assert_eq!(unit.modules[0].visibility, ast::Visibility::Public);
        assert_eq!(unit.modules[1].path.display(), "crate::math");
        assert_eq!(unit.modules[1].visibility, ast::Visibility::Private);
        assert_eq!(unit.modules[0].children.get("math"), Some(&ModuleId(1)));
    }

    #[test]
    fn records_public_child_module_visibility() {
        let dir = tempfile::tempdir().unwrap();
        write(
            dir.path().join("goml.toml"),
            r#"[crate]
name = "hello"
root = "src/main.gom"
"#,
        );
        write(
            dir.path().join("src/main.gom"),
            "pub mod api;\nmod internal;\nfn main() {}\n",
        );
        write(dir.path().join("src/api.gom"), "fn api() -> int32 { 1 }\n");
        write(
            dir.path().join("src/internal.gom"),
            "fn internal() -> int32 { 2 }\n",
        );

        let unit = discover_crate_from_dir(dir.path()).unwrap();
        assert_eq!(unit.modules[1].path.display(), "crate::api");
        assert_eq!(unit.modules[1].visibility, ast::Visibility::Public);
        assert_eq!(unit.modules[2].path.display(), "crate::internal");
        assert_eq!(unit.modules[2].visibility, ast::Visibility::Private);
    }

    #[test]
    fn discovers_nested_modules_under_flat_parent_file() {
        let dir = tempfile::tempdir().unwrap();
        write(
            dir.path().join("goml.toml"),
            r#"[crate]
name = "hello"
kind = "bin"
"#,
        );
        write(dir.path().join("src/main.gom"), "mod foo;\nfn main() {}\n");
        write(dir.path().join("src/foo.gom"), "mod bar;\nfn foo() {}\n");
        write(dir.path().join("src/foo/bar.gom"), "fn bar() {}\n");

        let unit = discover_crate_from_dir(dir.path()).unwrap();
        let paths = unit
            .modules
            .iter()
            .map(|module| module.path.display())
            .collect::<Vec<_>>();
        assert_eq!(paths, vec!["crate", "crate::foo", "crate::foo::bar"]);
        let source_module_paths = unit
            .source_files()
            .into_iter()
            .map(|file| file.module_path)
            .collect::<Vec<_>>();
        assert_eq!(
            source_module_paths,
            vec![
                Vec::<String>::new(),
                vec!["foo".to_string()],
                vec!["foo".to_string(), "bar".to_string()]
            ]
        );
    }

    #[test]
    fn discovers_nested_modules_under_mod_file_parent() {
        let dir = tempfile::tempdir().unwrap();
        write(
            dir.path().join("goml.toml"),
            r#"[crate]
name = "hello"
kind = "bin"
"#,
        );
        write(dir.path().join("src/main.gom"), "mod foo;\nfn main() {}\n");
        write(
            dir.path().join("src/foo/mod.gom"),
            "mod bar;\nfn foo() {}\n",
        );
        write(dir.path().join("src/foo/bar.gom"), "fn bar() {}\n");

        let unit = discover_crate_from_dir(dir.path()).unwrap();
        let paths = unit
            .modules
            .iter()
            .map(|module| module.path.display())
            .collect::<Vec<_>>();
        assert_eq!(paths, vec!["crate", "crate::foo", "crate::foo::bar"]);
    }

    #[test]
    fn ignores_undeclared_gom_files() {
        let dir = tempfile::tempdir().unwrap();
        write(
            dir.path().join("goml.toml"),
            r#"[crate]
name = "hello"
root = "src/main.gom"
"#,
        );
        write(dir.path().join("src/main.gom"), "fn main() {}\n");
        write(dir.path().join("src/broken.gom"), "fn broken( {\n");

        let unit = discover_crate_from_dir(dir.path()).unwrap();
        assert_eq!(unit.modules.len(), 1);
    }

    #[test]
    fn use_paths_do_not_discover_modules() {
        let dir = tempfile::tempdir().unwrap();
        write(
            dir.path().join("goml.toml"),
            r#"[crate]
name = "hello"
root = "src/main.gom"
"#,
        );
        write(
            dir.path().join("src/main.gom"),
            "use crate::math::add;\nfn main() {}\n",
        );
        write(
            dir.path().join("src/math.gom"),
            "pub fn add() -> int32 { 1 }\n",
        );

        let unit = discover_crate_from_dir(dir.path()).unwrap();
        let paths = unit
            .modules
            .iter()
            .map(|module| module.path.display())
            .collect::<Vec<_>>();
        assert_eq!(paths, vec!["crate"]);
    }

    #[test]
    fn discovers_from_file_path_without_manifest() {
        let dir = tempfile::tempdir().unwrap();
        let entry = dir.path().join("src/main.gom");
        write(entry.clone(), "mod helper;\nfn main() {}\n");
        write(dir.path().join("src/helper.gom"), "fn helper() {}\n");

        let unit = discover_crate_from_file(&entry).unwrap();
        assert_eq!(unit.root_dir, dir.path().join("src"));
        assert_eq!(unit.modules.len(), 2);
        assert_eq!(unit.modules[1].path.display(), "crate::helper");
    }

    #[test]
    fn reports_missing_module_file() {
        let dir = tempfile::tempdir().unwrap();
        write(
            dir.path().join("goml.toml"),
            r#"[crate]
name = "hello"
root = "main.gom"
"#,
        );
        write(dir.path().join("main.gom"), "mod missing;\nfn main() {}\n");

        let err = discover_crate_from_dir(dir.path()).unwrap_err();
        assert!(matches!(
            err,
            DiscoveryError::MissingModule { ref name, .. } if name == "missing"
        ));
    }

    #[test]
    fn reports_ambiguous_module_file() {
        let dir = tempfile::tempdir().unwrap();
        write(
            dir.path().join("goml.toml"),
            r#"[crate]
name = "hello"
root = "main.gom"
"#,
        );
        write(dir.path().join("main.gom"), "mod foo;\nfn main() {}\n");
        write(dir.path().join("foo.gom"), "fn foo() {}\n");
        write(dir.path().join("foo/mod.gom"), "fn foo() {}\n");

        let err = discover_crate_from_dir(dir.path()).unwrap_err();
        assert!(matches!(
            err,
            DiscoveryError::AmbiguousModule { ref name, .. } if name == "foo"
        ));
    }

    #[test]
    fn reports_duplicate_module_declaration() {
        let dir = tempfile::tempdir().unwrap();
        write(
            dir.path().join("goml.toml"),
            r#"[crate]
name = "hello"
root = "main.gom"
"#,
        );
        write(
            dir.path().join("main.gom"),
            "mod foo;\nmod foo;\nfn main() {}\n",
        );
        write(dir.path().join("foo.gom"), "fn foo() {}\n");

        let err = discover_crate_from_dir(dir.path()).unwrap_err();
        assert!(matches!(
            err,
            DiscoveryError::DuplicateModule { ref name, .. } if name == "foo"
        ));
    }

    #[test]
    fn reports_ambiguous_inferred_crate_root() {
        let dir = tempfile::tempdir().unwrap();
        write(
            dir.path().join("goml.toml"),
            r#"[crate]
name = "hello"
"#,
        );
        write(dir.path().join("src/main.gom"), "fn main() {}\n");
        write(dir.path().join("main.gom"), "fn main() {}\n");

        let err = discover_crate_from_dir(dir.path()).unwrap_err();
        assert!(matches!(err, DiscoveryError::AmbiguousCrateRoot { .. }));
    }

    #[test]
    fn reports_missing_configured_crate_root() {
        let dir = tempfile::tempdir().unwrap();
        write(
            dir.path().join("goml.toml"),
            r#"[crate]
name = "hello"
root = "src/main.gom"
"#,
        );

        let err = discover_crate_from_dir(dir.path()).unwrap_err();
        assert!(matches!(err, DiscoveryError::MissingCrateRoot { .. }));
    }

    fn write(path: PathBuf, contents: &str) {
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).unwrap();
        }
        std::fs::write(path, contents).unwrap();
    }
}
