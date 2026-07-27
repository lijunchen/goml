use std::collections::{BTreeMap, HashSet};

use ast::ast;
use diagnostics::{Diagnostic, Diagnostics, Severity, Stage};
use sha2::Digest;

use crate::env::{GlobalTypeEnv, TraitEnv, TypeEnv, ValueEnv};
use crate::hir::SourceFileAst;
use crate::package_names::{BUILTIN_PACKAGE, ROOT_PACKAGE, is_special_unqualified_package};
use crate::tast::TastIdent;

pub const FORMAT_VERSION: u32 = 16;
pub const COMPILER_ABI: u32 = 3;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct PackageExports {
    pub type_env: TypeEnv,
    pub trait_env: TraitEnv,
    pub value_env: ValueEnv,
}

impl PackageExports {
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
        diagnostics: &mut Diagnostics,
    ) -> Self {
        if package == BUILTIN_PACKAGE || package == ROOT_PACKAGE {
            return Self::from_genv(genv);
        }

        let public_names = public_export_names(package, files);
        let mut exports = Self::from_genv(genv);
        exports
            .type_env
            .enums
            .retain(|name, _| public_names.contains(&name.0));
        exports
            .type_env
            .structs
            .retain(|name, _| public_names.contains(&name.0));
        let public_struct_fields = exports.type_env.public_struct_fields.clone();
        for (name, definition) in exports.type_env.structs.iter_mut() {
            let visible = public_struct_fields.get(name).cloned().unwrap_or_default();
            let original_len = definition.fields.len();
            definition
                .fields
                .retain(|(field, _)| visible.contains(&field.0));
            definition.has_hidden_fields |= definition.fields.len() != original_len;
            for (field, ty) in &definition.fields {
                let mut private_names = HashSet::new();
                collect_private_local_names(package, &public_names, ty, &mut private_names);
                if !private_names.is_empty() {
                    let mut private_names = private_names.into_iter().collect::<Vec<_>>();
                    private_names.sort();
                    diagnostics.push(Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Public field {}.{} exposes private type {}",
                            name.0,
                            field.0,
                            private_names.join(", ")
                        ),
                    ));
                }
            }
        }
        exports.type_env.public_struct_fields.clear();
        exports
            .trait_env
            .trait_defs
            .retain(|name, _| public_names.contains(name));
        exports.trait_env.trait_impls.retain(|key, definition| {
            if !definition.valid
                || !local_name_is_public(package, &public_names, &key.trait_ref.name.0)
                || !ty_has_public_local_names(package, &public_names, &key.for_ty)
            {
                return false;
            }
            let mut private_names = HashSet::new();
            for arg in &key.trait_ref.args {
                collect_private_local_names(package, &public_names, arg, &mut private_names);
            }
            for ty in definition.associated_types.values() {
                collect_private_local_names(package, &public_names, ty, &mut private_names);
            }
            for predicate in &definition.constraints {
                collect_predicate_private_local_names(
                    package,
                    &public_names,
                    predicate,
                    &mut private_names,
                );
            }
            for scheme in definition.methods.values() {
                collect_private_local_names(package, &public_names, &scheme.ty, &mut private_names);
                for predicate in &scheme.constraints {
                    collect_predicate_private_local_names(
                        package,
                        &public_names,
                        predicate,
                        &mut private_names,
                    );
                }
            }
            if private_names.is_empty() {
                return true;
            }
            let mut private_names = private_names.into_iter().collect::<Vec<_>>();
            private_names.sort();
            diagnostics.push(
                Diagnostic::new(
                    Stage::Typer,
                    Severity::Error,
                    format!(
                        "Public trait implementation {} exposes private type {}",
                        key.trait_ref.name.0,
                        private_names.join(", ")
                    ),
                )
                .with_range(definition.origin),
            );
            false
        });
        let public_inherent_methods = exports.trait_env.public_inherent_methods.clone();
        exports.trait_env.inherent_impls.retain(|key, definition| {
            let key_is_public = match key {
                crate::env::InherentImplKey::Exact(ty) => {
                    ty_has_public_local_names(package, &public_names, ty)
                }
                crate::env::InherentImplKey::Constr(name) => {
                    local_name_is_public(package, &public_names, name)
                }
            };
            if !key_is_public {
                return false;
            }
            let visible = public_inherent_methods
                .get(key)
                .cloned()
                .unwrap_or_default();
            definition.methods.retain(|name, _| visible.contains(name));
            definition.methods.retain(|name, scheme| {
                let mut private_names = HashSet::new();
                collect_private_local_names(package, &public_names, &scheme.ty, &mut private_names);
                for predicate in &scheme.constraints {
                    collect_predicate_private_local_names(
                        package,
                        &public_names,
                        predicate,
                        &mut private_names,
                    );
                }
                if private_names.is_empty() {
                    return true;
                }
                let mut private_names = private_names.into_iter().collect::<Vec<_>>();
                private_names.sort();
                diagnostics.push(Diagnostic::new(
                    Stage::Typer,
                    Severity::Error,
                    format!(
                        "Public inherent method {} exposes private type {}",
                        name,
                        private_names.join(", ")
                    ),
                ));
                false
            });
            !definition.methods.is_empty()
        });
        exports.trait_env.public_inherent_methods.clear();
        exports
            .value_env
            .funcs
            .retain(|name, _| public_names.contains(name));
        exports
    }

    pub fn apply_to(&self, genv: &mut GlobalTypeEnv) {
        for (name, def) in self.type_env.enums.iter() {
            genv.type_env.structs.shift_remove(name);
            genv.type_env.enums.insert(name.clone(), def.clone());
        }
        for (name, def) in self.type_env.structs.iter() {
            genv.type_env.enums.shift_remove(name);
            genv.type_env.structs.insert(name.clone(), def.clone());
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
    }

    pub fn to_genv(&self) -> GlobalTypeEnv {
        GlobalTypeEnv {
            type_env: self.type_env.clone(),
            trait_env: self.trait_env.clone(),
            value_env: self.value_env.clone(),
            lang_items: Default::default(),
        }
    }
}

fn local_name_is_public(package: &str, public_names: &HashSet<String>, name: &str) -> bool {
    let Some(rest) = name.strip_prefix(&format!("{package}::")) else {
        return true;
    };
    rest.contains("::") || public_names.contains(name)
}

fn ty_has_public_local_names(
    package: &str,
    public_names: &HashSet<String>,
    ty: &crate::tast::Ty,
) -> bool {
    let mut private_names = HashSet::new();
    collect_private_local_names(package, public_names, ty, &mut private_names);
    private_names.is_empty()
}

fn collect_predicate_private_local_names(
    package: &str,
    public_names: &HashSet<String>,
    predicate: &crate::env::TypePredicate,
    private_names: &mut HashSet<String>,
) {
    match predicate {
        crate::env::TypePredicate::Trait { for_ty, trait_ref } => {
            collect_private_local_names(package, public_names, for_ty, private_names);
            if !local_name_is_public(package, public_names, &trait_ref.name.0) {
                private_names.insert(trait_ref.name.0.clone());
            }
            for arg in &trait_ref.args {
                collect_private_local_names(package, public_names, arg, private_names);
            }
        }
        crate::env::TypePredicate::Equality { lhs, rhs } => {
            collect_private_local_names(package, public_names, lhs, private_names);
            collect_private_local_names(package, public_names, rhs, private_names);
        }
    }
}

fn collect_private_local_names(
    package: &str,
    public_names: &HashSet<String>,
    ty: &crate::tast::Ty,
    private_names: &mut HashSet<String>,
) {
    match ty {
        crate::tast::Ty::TEnum { name } | crate::tast::Ty::TStruct { name } => {
            if !local_name_is_public(package, public_names, name) {
                private_names.insert(name.clone());
            }
        }
        crate::tast::Ty::TDyn { trait_name } => {
            if !local_name_is_public(package, public_names, trait_name) {
                private_names.insert(trait_name.clone());
            }
        }
        crate::tast::Ty::TTuple { typs } => {
            for ty in typs {
                collect_private_local_names(package, public_names, ty, private_names);
            }
        }
        crate::tast::Ty::TProjection {
            trait_ref, for_ty, ..
        } => {
            collect_private_local_names(package, public_names, for_ty, private_names);
            if let Some(trait_ref) = trait_ref {
                if !local_name_is_public(package, public_names, &trait_ref.name.0) {
                    private_names.insert(trait_ref.name.0.clone());
                }
                for arg in &trait_ref.args {
                    collect_private_local_names(package, public_names, arg, private_names);
                }
            }
        }
        crate::tast::Ty::TApp { ty, args } => {
            collect_private_local_names(package, public_names, ty, private_names);
            for arg in args {
                collect_private_local_names(package, public_names, arg, private_names);
            }
        }
        crate::tast::Ty::TArray { elem, .. }
        | crate::tast::Ty::TSlice { elem }
        | crate::tast::Ty::TVec { elem }
        | crate::tast::Ty::TRef { elem }
        | crate::tast::Ty::TChannel { elem } => {
            collect_private_local_names(package, public_names, elem, private_names);
        }
        crate::tast::Ty::THashMap { key, value } => {
            collect_private_local_names(package, public_names, key, private_names);
            collect_private_local_names(package, public_names, value, private_names);
        }
        crate::tast::Ty::TFunc { params, ret_ty } => {
            for param in params {
                collect_private_local_names(package, public_names, param, private_names);
            }
            collect_private_local_names(package, public_names, ret_ty, private_names);
        }
        crate::tast::Ty::TVar(_)
        | crate::tast::Ty::TParam { .. }
        | crate::tast::Ty::TUnit
        | crate::tast::Ty::TNever
        | crate::tast::Ty::TBool
        | crate::tast::Ty::TInt
        | crate::tast::Ty::TInt8
        | crate::tast::Ty::TInt16
        | crate::tast::Ty::TInt32
        | crate::tast::Ty::TInt64
        | crate::tast::Ty::TUint
        | crate::tast::Ty::TUint8
        | crate::tast::Ty::TUint16
        | crate::tast::Ty::TUint32
        | crate::tast::Ty::TUint64
        | crate::tast::Ty::TFloat32
        | crate::tast::Ty::TFloat64
        | crate::tast::Ty::TString
        | crate::tast::Ty::TChar => {}
    }
}

fn public_export_names(package: &str, files: &[SourceFileAst]) -> HashSet<String> {
    let mut names = HashSet::new();
    for file in files {
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
                ast::Item::ExternFn(def) if def.visibility == ast::Visibility::Public => {
                    Some(&def.name.0)
                }
                _ => None,
            };
            if let Some(name) = name {
                names.insert(export_name(package, name));
            }
        }
    }
    names
}

fn export_name(package: &str, name: &str) -> String {
    if is_special_unqualified_package(package) {
        name.to_string()
    } else {
        TastIdent::new(&format!("{package}::{name}")).0
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct InterfaceUnit {
    pub format_version: u32,
    pub compiler_abi: u32,
    pub package: String,
    pub exports: PackageExports,
    pub interface: crate::interface::PackageInterface,
    pub deps: BTreeMap<String, String>,
    pub interface_hash: String,
}

#[derive(serde::Serialize)]
struct InterfaceHashView<'a> {
    format_version: u32,
    compiler_abi: u32,
    package: &'a str,
    exports: &'a PackageExports,
    interface: &'a crate::interface::PackageInterface,
    deps: &'a BTreeMap<String, String>,
}

impl InterfaceUnit {
    pub fn new(
        package: String,
        exports: PackageExports,
        interface: crate::interface::PackageInterface,
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

    pub fn validate(&self) -> bool {
        self.format_version == FORMAT_VERSION
            && self.compiler_abi == COMPILER_ABI
            && crate::config::validate_module_path(&self.package).is_ok()
            && self.package == self.interface.package
            && crate::config::validate_package_name(&self.interface.name).is_ok()
            && self.validate_hash()
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TestDescriptor {
    pub id: String,
    pub package: String,
    pub symbol: String,
    pub display_name: String,
    pub source_path: String,
    pub start: u32,
    pub end: u32,
    pub ignored: bool,
    pub ignore_reason: Option<String>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct CoreUnit {
    pub format_version: u32,
    pub compiler_abi: u32,
    pub package: String,
    pub interface: InterfaceUnit,
    pub exports: PackageExports,
    pub core_ir: crate::core::File,
    pub deps: BTreeMap<String, String>,
    pub sources: Vec<String>,
    #[serde(default)]
    pub tests: Vec<TestDescriptor>,
}

impl CoreUnit {
    pub fn new(
        package: String,
        interface: InterfaceUnit,
        exports: PackageExports,
        core_ir: crate::core::File,
    ) -> Self {
        let deps = interface.deps.clone();
        Self {
            format_version: FORMAT_VERSION,
            compiler_abi: COMPILER_ABI,
            package,
            interface,
            exports,
            core_ir,
            deps,
            sources: Vec::new(),
            tests: Vec::new(),
        }
    }

    pub fn validate(&self) -> bool {
        let mut test_ids = HashSet::new();
        self.format_version == FORMAT_VERSION
            && self.compiler_abi == COMPILER_ABI
            && crate::config::validate_module_path(&self.package).is_ok()
            && self.package == self.interface.package
            && self.interface.validate()
            && self.deps == self.interface.deps
            && self.tests.iter().all(|test| {
                test.package == self.package
                    && !test.id.is_empty()
                    && !test.symbol.is_empty()
                    && !test.display_name.is_empty()
                    && !test.source_path.is_empty()
                    && test.start <= test.end
                    && (test.ignored || test.ignore_reason.is_none())
                    && test_ids.insert(test.id.as_str())
            })
    }
}
