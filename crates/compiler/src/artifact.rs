use std::collections::{BTreeMap, HashSet};

use ast::ast;
use sha2::Digest;

use crate::env::{GlobalTypeEnv, TraitEnv, TypeEnv, ValueEnv};
use crate::hir::SourceFileAst;
use crate::package_names::{BUILTIN_PACKAGE, ROOT_PACKAGE, is_special_unqualified_package};
use crate::tast::TastIdent;

pub const FORMAT_VERSION: u32 = 4;
pub const COMPILER_ABI: u32 = 1;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct CrateExports {
    pub type_env: TypeEnv,
    pub trait_env: TraitEnv,
    pub value_env: ValueEnv,
}

impl CrateExports {
    pub fn from_genv(genv: &GlobalTypeEnv) -> Self {
        Self {
            type_env: genv.type_env.clone(),
            trait_env: genv.trait_env.clone(),
            value_env: genv.value_env.clone(),
        }
    }

    pub fn public_from_package(
        package: &str,
        files: &[SourceFileAst],
        genv: &GlobalTypeEnv,
    ) -> Self {
        if package == BUILTIN_PACKAGE || package == ROOT_PACKAGE {
            return Self::from_genv(genv);
        }

        Self::public_filtered(package, files, genv)
    }

    pub fn public_from_crate(
        crate_name: &str,
        files: &[SourceFileAst],
        genv: &GlobalTypeEnv,
    ) -> Self {
        if crate_name == BUILTIN_PACKAGE {
            return Self::from_genv(genv);
        }

        Self::public_filtered(crate_name, files, genv)
    }

    fn public_filtered(namespace: &str, files: &[SourceFileAst], genv: &GlobalTypeEnv) -> Self {
        let public_names = public_export_names(namespace, files);
        let mut exports = Self::from_genv(genv);
        exports
            .type_env
            .enums
            .retain(|name, _| public_names.contains(&name.0));
        exports
            .type_env
            .structs
            .retain(|name, _| public_names.contains(&name.0));
        exports
            .type_env
            .extern_types
            .retain(|name, _| public_names.contains(name));
        exports
            .trait_env
            .trait_defs
            .retain(|name, _| public_names.contains(name));
        exports
            .value_env
            .funcs
            .retain(|name, _| public_names.contains(name));
        exports
            .value_env
            .extern_funcs
            .retain(|name, _| public_names.contains(name));
        exports
    }

    pub fn apply_to(&self, genv: &mut GlobalTypeEnv) {
        for (name, def) in self.type_env.enums.iter() {
            genv.type_env.structs.shift_remove(name);
            genv.type_env.extern_types.shift_remove(&name.0);
            genv.type_env.enums.insert(name.clone(), def.clone());
        }
        for (name, def) in self.type_env.structs.iter() {
            genv.type_env.enums.shift_remove(name);
            genv.type_env.extern_types.shift_remove(&name.0);
            genv.type_env.structs.insert(name.clone(), def.clone());
        }
        for (name, def) in self.type_env.extern_types.iter() {
            let ident = crate::tast::TastIdent::new(name);
            genv.type_env.enums.shift_remove(&ident);
            genv.type_env.structs.shift_remove(&ident);
            genv.type_env.extern_types.insert(name.clone(), def.clone());
        }
        for (name, def) in self.trait_env.trait_defs.iter() {
            genv.trait_env.trait_defs.insert(name.clone(), def.clone());
        }
        for (key, def) in self.trait_env.trait_impls.iter() {
            genv.trait_env.trait_impls.insert(key.clone(), def.clone());
        }
        for (key, def) in self.trait_env.inherent_impls.iter() {
            genv.trait_env
                .inherent_impls
                .insert(key.clone(), def.clone());
        }
        for (name, scheme) in self.value_env.funcs.iter() {
            genv.value_env.funcs.insert(name.clone(), scheme.clone());
        }
        for (name, func) in self.value_env.extern_funcs.iter() {
            genv.value_env
                .extern_funcs
                .insert(name.clone(), func.clone());
        }
    }

    pub fn to_genv(&self) -> GlobalTypeEnv {
        GlobalTypeEnv {
            type_env: self.type_env.clone(),
            trait_env: self.trait_env.clone(),
            value_env: self.value_env.clone(),
        }
    }
}

fn public_export_names(namespace: &str, files: &[SourceFileAst]) -> HashSet<String> {
    let mut names = HashSet::new();
    for file in files {
        if !file.module_visible {
            continue;
        }
        for item in file.ast.toplevels.iter() {
            let name = match item {
                ast::Item::EnumDef(def) if def.visibility == ast::Visibility::Public => {
                    Some(&def.name.0)
                }
                ast::Item::StructDef(def) if def.visibility == ast::Visibility::Public => {
                    Some(&def.name.0)
                }
                ast::Item::TraitDef(def) if def.visibility == ast::Visibility::Public => {
                    Some(&def.name.0)
                }
                ast::Item::Fn(def) if def.visibility == ast::Visibility::Public => {
                    Some(&def.name.0)
                }
                ast::Item::ExternGo(def) if def.visibility == ast::Visibility::Public => {
                    Some(&def.goml_name.0)
                }
                ast::Item::ExternType(def) if def.visibility == ast::Visibility::Public => {
                    Some(&def.goml_name.0)
                }
                ast::Item::ExternBuiltin(def) if def.visibility == ast::Visibility::Public => {
                    Some(&def.name.0)
                }
                _ => None,
            };
            if let Some(name) = name {
                names.insert(export_name_in_module(namespace, &file.module_path, name));
            }
        }
    }
    names
}

fn export_name_in_module(namespace: &str, module_path: &[String], name: &str) -> String {
    let mut segments = if module_path.is_empty() {
        if is_special_unqualified_package(namespace) {
            Vec::new()
        } else {
            vec![namespace.to_string()]
        }
    } else {
        let module_name = module_path.join("::");
        if namespace == module_name || is_special_unqualified_package(namespace) {
            module_path.to_vec()
        } else {
            std::iter::once(namespace.to_string())
                .chain(module_path.iter().cloned())
                .collect()
        }
    };
    segments.push(name.to_string());
    if segments.len() == 1 {
        segments.pop().unwrap_or_default()
    } else {
        TastIdent::new(&segments.join("::")).0
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct InterfaceUnit {
    pub format_version: u32,
    pub compiler_abi: u32,
    pub package: String,
    pub exports: CrateExports,
    pub interface: crate::interface::CrateInterface,
    pub deps: BTreeMap<String, String>,
    pub interface_hash: String,
}

#[derive(serde::Serialize)]
struct InterfaceHashView<'a> {
    format_version: u32,
    compiler_abi: u32,
    package: &'a str,
    exports: &'a CrateExports,
    interface: &'a crate::interface::CrateInterface,
    deps: &'a BTreeMap<String, String>,
}

impl InterfaceUnit {
    pub fn new(
        package: String,
        exports: CrateExports,
        interface: crate::interface::CrateInterface,
        deps: BTreeMap<String, String>,
    ) -> Self {
        let mut unit = Self {
            format_version: FORMAT_VERSION,
            compiler_abi: COMPILER_ABI,
            package,
            exports,
            interface,
            deps,
            interface_hash: String::new(),
        };
        unit.interface_hash = unit.compute_hash();
        unit
    }

    pub fn compute_hash(&self) -> String {
        let view = InterfaceHashView {
            format_version: self.format_version,
            compiler_abi: self.compiler_abi,
            package: &self.package,
            exports: &self.exports,
            interface: &self.interface,
            deps: &self.deps,
        };
        let bytes = serde_json::to_vec(&view).expect("InterfaceUnit hash view must serialize");
        let digest = sha2::Sha256::digest(bytes);
        hex::encode(digest)
    }

    pub fn validate_hash(&self) -> bool {
        self.interface_hash == self.compute_hash()
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct CoreUnit {
    pub format_version: u32,
    pub compiler_abi: u32,
    pub package: String,
    pub interface: InterfaceUnit,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub internal_exports: Option<CrateExports>,
    pub core_ir: crate::core::File,
    pub deps: BTreeMap<String, String>,
    pub sources: Vec<String>,
}

impl CoreUnit {
    pub fn new(package: String, interface: InterfaceUnit, core_ir: crate::core::File) -> Self {
        let deps = interface.deps.clone();
        Self {
            format_version: FORMAT_VERSION,
            compiler_abi: COMPILER_ABI,
            package,
            interface,
            internal_exports: None,
            core_ir,
            deps,
            sources: Vec::new(),
        }
    }

    pub fn validate(&self) -> bool {
        self.format_version == FORMAT_VERSION
            && self.compiler_abi == COMPILER_ABI
            && self.package == self.interface.package
            && self.interface.validate_hash()
            && self.deps == self.interface.deps
    }
}
