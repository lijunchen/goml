use std::collections::{BTreeMap, BTreeSet};

use sha2::Digest;

use crate::artifact::CrateExports;
use crate::hir;
use crate::package_names::{BUILTIN_PACKAGE, ROOT_PACKAGE, is_special_unqualified_package};

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct CrateInterface {
    pub package: String,
    #[serde(default)]
    pub packages: BTreeSet<String>,
    pub value_exports: BTreeMap<String, u32>,
    pub enum_variants: BTreeMap<String, Vec<String>>,
}

impl CrateInterface {
    pub fn from_exports(package: &str, exports: &CrateExports) -> Self {
        let mut value_names: Vec<String> = exports
            .value_env
            .funcs
            .keys()
            .filter(|name| belongs_to_package(package, name))
            .cloned()
            .collect();
        value_names.sort();
        value_names.dedup();

        let value_exports = value_names
            .into_iter()
            .enumerate()
            .map(|(idx, name)| (name, idx as u32))
            .collect();

        let mut enum_variants = BTreeMap::new();
        for (name, def) in exports.type_env.enums.iter() {
            let full = name.0.as_str();
            if !belongs_to_package(package, full) {
                continue;
            }
            let key = relative_export_name(package, full);
            let mut variants: Vec<String> = def.variants.iter().map(|(v, _)| v.0.clone()).collect();
            variants.sort();
            variants.dedup();
            enum_variants.insert(key, variants);
        }

        Self {
            package: package.to_string(),
            packages: std::iter::once(package.to_string()).collect(),
            value_exports,
            enum_variants,
        }
    }
}

fn belongs_to_package(package: &str, name: &str) -> bool {
    if is_special_unqualified_package(package) {
        !name.contains("::")
    } else {
        name.starts_with(&format!("{}::", package))
    }
}

fn relative_export_name(package: &str, name: &str) -> String {
    if is_special_unqualified_package(package) {
        return name.to_string();
    }
    let prefix = format!("{}::", package);
    name.strip_prefix(&prefix).unwrap_or(name).to_string()
}

pub fn crate_id_for_name(name: &str) -> hir::PackageId {
    match name {
        BUILTIN_PACKAGE => hir::PackageId(0),
        ROOT_PACKAGE => hir::PackageId(1),
        other => {
            let digest = sha2::Sha256::digest(other.as_bytes());
            let mut bytes = [0u8; 4];
            bytes.copy_from_slice(&digest[..4]);
            let mut id = u32::from_be_bytes(bytes);
            if id <= 1 {
                id = id.wrapping_add(2);
            }
            hir::PackageId(id)
        }
    }
}

pub fn package_id_for_name(name: &str) -> hir::PackageId {
    crate_id_for_name(name)
}
