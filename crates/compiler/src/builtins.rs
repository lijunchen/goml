use std::{
    collections::{HashMap, HashSet},
    path::Path,
    sync::OnceLock,
};

use crate::derive;
use ::ast::{ast, lower};
use cst::cst::{CstNode, File as CstFile};
use indexmap::{IndexMap, IndexSet};
use parser::{self, syntax::MySyntaxNode};

use crate::{
    artifact::{InterfaceUnit, PackageExports},
    env::{FnOrigin, FnScheme, GlobalTypeEnv, TraitEnv, TypeEnv, ValueEnv},
    hir, interface,
    package_names::BUILTIN_PACKAGE,
    tast, typer,
};

/// The embedded builtin.gom source code
const BUILTIN_GOM: &str = include_str!("builtin.gom");

static BUILTIN_AST: OnceLock<ast::File> = OnceLock::new();
static BUILTIN_ARTIFACTS: OnceLock<BuiltinArtifacts> = OnceLock::new();
static BUILTIN_COLLECTION_METHOD_KEYS: OnceLock<HashSet<(String, String)>> = OnceLock::new();

#[derive(Debug, Clone)]
struct BuiltinArtifacts {
    genv: GlobalTypeEnv,
    tast: tast::File,
}

fn parse_builtin_ast() -> ast::File {
    let path = Path::new("builtin.gom");
    let parse_result = parser::parse(path, BUILTIN_GOM);
    if parse_result.has_errors() {
        panic!(
            "Failed to parse builtin.gom: {:?}",
            parse_result.into_diagnostics()
        );
    }

    let root = MySyntaxNode::new_root(parse_result.green_node);
    let cst = CstFile::cast(root).expect("failed to cast CST file");
    let lower_result = lower::lower(cst);
    let mut ast_file = lower_result
        .into_result()
        .expect("failed to lower builtin.gom AST");
    ast_file.package = ast::AstIdent::new(BUILTIN_PACKAGE);

    match derive::expand(ast_file) {
        Ok(ast) => ast,
        Err(diags) => panic!("Failed to expand builtin.gom: {:?}", diags),
    }
}

fn builtin_ast() -> ast::File {
    BUILTIN_AST.get_or_init(parse_builtin_ast).clone()
}

/// Get the builtin AST for use in HIR lowering or other passes
pub fn get_builtin_ast() -> ast::File {
    builtin_ast()
}

fn build_builtin_artifacts() -> BuiltinArtifacts {
    let ast = builtin_ast();

    let mut base_env = GlobalTypeEnv {
        type_env: TypeEnv::new(),
        trait_env: TraitEnv::new(),
        value_env: ValueEnv {
            funcs: IndexMap::new(),
        },
    };

    add_array_builtins(&mut base_env.value_env.funcs);

    let (hir, hir_table, mut hir_diagnostics) = hir::lower_to_hir(ast);

    let (tast, mut genv, mut diagnostics) = typer::check_file_with_env(
        hir,
        hir_table,
        base_env,
        GlobalTypeEnv::new_empty(),
        BUILTIN_PACKAGE,
        HashMap::new(),
    );
    diagnostics.append(&mut hir_diagnostics);
    if diagnostics.has_errors() {
        panic!("Failed to typecheck builtin.gom: {:?}", diagnostics);
    }

    genv.trait_env
        .inherent_impls
        .extend(builtin_inherent_methods());

    BuiltinArtifacts { genv, tast }
}

pub(crate) fn builtin_env() -> GlobalTypeEnv {
    BUILTIN_ARTIFACTS
        .get_or_init(build_builtin_artifacts)
        .genv
        .clone()
}

pub fn merge_with_builtin_env(genv: &GlobalTypeEnv) -> GlobalTypeEnv {
    let exports = PackageExports {
        type_env: genv.type_env.clone(),
        trait_env: genv.trait_env.clone(),
        value_env: genv.value_env.clone(),
    };
    let mut full_env = builtin_env();
    exports.apply_to(&mut full_env);
    full_env
}

pub(crate) fn builtin_tast() -> tast::File {
    BUILTIN_ARTIFACTS
        .get_or_init(build_builtin_artifacts)
        .tast
        .clone()
}

pub(crate) fn builtin_print_tast() -> tast::File {
    let toplevels = builtin_tast()
        .toplevels
        .into_iter()
        .filter(|item| match item {
            tast::Item::Fn(f) => f.name == "print" || f.name == "println",
            _ => false,
        })
        .collect();
    tast::File { toplevels }
}

pub(crate) fn builtin_collection_impl_tast() -> tast::File {
    let toplevels = builtin_tast()
        .toplevels
        .into_iter()
        .filter(|item| match item {
            tast::Item::ImplBlock(impl_block) => matches!(
                &impl_block.for_type,
                tast::Ty::TVec { .. } | tast::Ty::TSlice { .. } | tast::Ty::THashMap { .. }
            ),
            _ => false,
        })
        .collect();
    tast::File { toplevels }
}

fn build_builtin_collection_method_keys() -> HashSet<(String, String)> {
    let mut keys = HashSet::new();
    for item in builtin_collection_impl_tast().toplevels {
        let tast::Item::ImplBlock(impl_block) = item else {
            continue;
        };
        let base = impl_block.for_type.get_constr_name_unsafe();
        for method in impl_block.methods {
            keys.insert((base.clone(), method.name));
        }
    }
    keys
}

pub(crate) fn builtin_collection_method_keys() -> &'static HashSet<(String, String)> {
    BUILTIN_COLLECTION_METHOD_KEYS.get_or_init(build_builtin_collection_method_keys)
}

pub fn builtin_interface_hash() -> String {
    let genv = builtin_env();
    let exports = PackageExports {
        type_env: genv.type_env.clone(),
        trait_env: genv.trait_env.clone(),
        value_env: genv.value_env.clone(),
    };
    let iface = interface::PackageInterface::from_exports(BUILTIN_PACKAGE, &exports);
    InterfaceUnit::new(
        BUILTIN_PACKAGE.to_string(),
        exports,
        iface,
        Default::default(),
    )
    .interface_hash
}

fn make_fn_scheme(params: Vec<tast::Ty>, ret: tast::Ty) -> FnScheme {
    FnScheme {
        type_params: vec![],
        constraints: vec![],
        ty: tast::Ty::TFunc {
            params,
            ret_ty: Box::new(ret),
        },
        origin: FnOrigin::Builtin,
    }
}

fn add_array_builtins(funcs: &mut IndexMap<String, FnScheme>) {
    let array_elem_param = tast::Ty::TParam {
        name: "T".to_string(),
    };
    let array_ty = tast::Ty::TArray {
        len: tast::ARRAY_WILDCARD_LEN,
        elem: Box::new(array_elem_param.clone()),
    };
    funcs.insert(
        "array_get".to_string(),
        make_fn_scheme(
            vec![array_ty.clone(), tast::Ty::TInt32],
            array_elem_param.clone(),
        ),
    );
    funcs.insert(
        "array_set".to_string(),
        make_fn_scheme(
            vec![array_ty.clone(), tast::Ty::TInt32, array_elem_param.clone()],
            array_ty,
        ),
    );
}

pub(super) fn builtin_inherent_methods()
-> IndexMap<crate::env::InherentImplKey, crate::env::ImplDef> {
    let mut impls = IndexMap::new();

    let int32_ty = tast::Ty::TInt32;
    let method_ty = tast::Ty::TFunc {
        params: vec![int32_ty.clone()],
        ret_ty: Box::new(tast::Ty::TString),
    };

    let mut int32_impl = crate::env::ImplDef::default();
    int32_impl.methods.insert(
        "to_string".to_string(),
        crate::env::FnScheme {
            type_params: vec![],
            constraints: vec![],
            ty: method_ty,
            origin: FnOrigin::Builtin,
        },
    );
    impls.insert(crate::env::InherentImplKey::Exact(int32_ty), int32_impl);

    let char_ty = tast::Ty::TChar;
    let char_method_ty = tast::Ty::TFunc {
        params: vec![char_ty.clone()],
        ret_ty: Box::new(tast::Ty::TString),
    };
    let mut char_impl = crate::env::ImplDef::default();
    char_impl.methods.insert(
        "to_string".to_string(),
        crate::env::FnScheme {
            type_params: vec![],
            constraints: vec![],
            ty: char_method_ty,
            origin: FnOrigin::Builtin,
        },
    );
    impls.insert(crate::env::InherentImplKey::Exact(char_ty), char_impl);

    let string_ty = tast::Ty::TString;
    let mut string_impl = crate::env::ImplDef::default();
    string_impl.methods.insert(
        "len".to_string(),
        crate::env::FnScheme {
            type_params: vec![],
            constraints: vec![],
            ty: tast::Ty::TFunc {
                params: vec![string_ty.clone()],
                ret_ty: Box::new(tast::Ty::TInt32),
            },
            origin: FnOrigin::Builtin,
        },
    );
    string_impl.methods.insert(
        "get".to_string(),
        crate::env::FnScheme {
            type_params: vec![],
            constraints: vec![],
            ty: tast::Ty::TFunc {
                params: vec![string_ty.clone(), tast::Ty::TInt32],
                ret_ty: Box::new(tast::Ty::TChar),
            },
            origin: FnOrigin::Builtin,
        },
    );
    impls.insert(crate::env::InherentImplKey::Exact(string_ty), string_impl);

    impls
}

pub fn builtin_function_names() -> Vec<String> {
    let mut names: IndexSet<String> = IndexSet::new();

    for item in builtin_ast().toplevels.iter() {
        if let ast::Item::ExternBuiltin(ext) = item {
            names.insert(ext.name.0.clone());
        }
    }

    for name in ["array_get", "array_set"] {
        names.insert(name.to_string());
    }

    names.into_iter().collect()
}
