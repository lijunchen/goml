use std::collections::BTreeMap;

use sha2::Digest;

use crate::env::{GlobalTypeEnv, TraitEnv, TypeEnv, ValueEnv};

pub const FORMAT_VERSION: u32 = 1;
pub const COMPILER_ABI: u32 = 1;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct PackageExports {
    pub type_env: TypeEnv,
    pub trait_env: TraitEnv,
    pub value_env: ValueEnv,
}

impl PackageExports {
    pub fn apply_to(&self, genv: &mut GlobalTypeEnv) {
        for (name, def) in self.type_env.enums.iter() {
            genv.type_env.enums.insert(name.clone(), def.clone());
        }
        for (name, def) in self.type_env.structs.iter() {
            genv.type_env.structs.insert(name.clone(), def.clone());
        }
        for (name, def) in self.type_env.extern_types.iter() {
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

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct InterfaceUnit {
    pub format_version: u32,
    pub compiler_abi: u32,
    pub package: String,
    pub exports: PackageExports,
    pub hir_interface: crate::hir::PackageInterface,
    pub deps: BTreeMap<String, String>,
    pub interface_hash: String,
}

#[derive(serde::Serialize)]
struct InterfaceHashView<'a> {
    format_version: u32,
    compiler_abi: u32,
    package: &'a str,
    exports: &'a PackageExports,
    hir_interface: &'a crate::hir::PackageInterface,
    deps: &'a BTreeMap<String, String>,
}

impl InterfaceUnit {
    pub fn new(
        package: String,
        exports: PackageExports,
        hir_interface: crate::hir::PackageInterface,
        deps: BTreeMap<String, String>,
    ) -> Self {
        let mut unit = Self {
            format_version: FORMAT_VERSION,
            compiler_abi: COMPILER_ABI,
            package,
            exports,
            hir_interface,
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
            hir_interface: &self.hir_interface,
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
