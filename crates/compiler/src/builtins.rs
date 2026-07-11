use std::{collections::HashMap, path::Path, sync::OnceLock};

use crate::derive;
use ::ast::{ast, lower};
use cst::cst::{CstNode, File as CstFile};
use indexmap::IndexMap;
use parser::{self, syntax::MySyntaxNode};

use crate::{
    artifact::{InterfaceUnit, PackageExports},
    env::{FnConstraint, FnScheme, GlobalTypeEnv, TraitEnv, TypeEnv, ValueEnv},
    hir, interface,
    intrinsics::{CallableBody, ExternCapability, IntrinsicId, LangItemId, LangItemTable},
    package_names::BUILTIN_PACKAGE,
    tast, typer,
};

pub const BUILTIN_SOURCE_FILES: [&str; 2] = ["builtin_contract.gom", "builtin_prelude.gom"];

const BUILTIN_CONTRACT_GOM: &str = include_str!("builtin_contract.gom");
const BUILTIN_PRELUDE_GOM: &str = include_str!("builtin_prelude.gom");

static BUILTIN_AST: OnceLock<ast::File> = OnceLock::new();
static BUILTIN_ARTIFACTS: OnceLock<BuiltinArtifacts> = OnceLock::new();
static BUILTIN_CALLABLES: OnceLock<IndexMap<String, CallableBody>> = OnceLock::new();

#[derive(Debug, Clone)]
struct BuiltinArtifacts {
    genv: GlobalTypeEnv,
    tast: tast::File,
}

fn parse_builtin_ast() -> ast::File {
    let path = Path::new("builtin_contract.gom");
    let source = format!("{BUILTIN_CONTRACT_GOM}\n{BUILTIN_PRELUDE_GOM}");
    let parse_result = parser::parse(path, &source);
    if parse_result.has_errors() {
        panic!(
            "Failed to parse builtin sources: {:?}",
            parse_result.into_diagnostics()
        );
    }

    let root = MySyntaxNode::new_root(parse_result.green_node);
    let cst = CstFile::cast(root).expect("failed to cast CST file");
    let lower_result = lower::lower(cst);
    let mut ast_file = lower_result
        .into_result()
        .expect("failed to lower builtin AST");
    ast_file.package = ast::AstIdent::new(BUILTIN_PACKAGE);

    match derive::expand(ast_file) {
        Ok(ast) => ast,
        Err(diags) => panic!("Failed to expand builtin sources: {:?}", diags),
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
        lang_items: LangItemTable::with_builtin_types(),
    };

    add_array_builtins(&mut base_env.value_env.funcs);

    let (hir, hir_table, mut hir_diagnostics) = hir::lower_to_hir(ast);

    let (tast, genv, mut diagnostics) = typer::check_file_with_env_capability(
        hir,
        hir_table,
        base_env,
        GlobalTypeEnv::new_empty(),
        BUILTIN_PACKAGE,
        HashMap::new(),
        ExternCapability::Core,
    );
    diagnostics.append(&mut hir_diagnostics);
    if diagnostics.has_errors() {
        panic!("Failed to typecheck builtin sources: {:?}", diagnostics);
    }
    for item in LangItemId::ALL {
        if genv.lang_item(item).is_none() {
            panic!("missing lang item {}", item.key());
        }
    }

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

fn make_fn_scheme(intrinsic: IntrinsicId) -> FnScheme {
    let signature = intrinsic.signature();
    FnScheme {
        type_params: signature.type_params,
        constraints: signature
            .constraints
            .into_iter()
            .map(|(type_param, trait_name)| FnConstraint {
                type_param,
                trait_ref: tast::TraitRef::without_args(tast::TastIdent::new(&trait_name)),
            })
            .collect(),
        ty: signature.ty,
        body: CallableBody::Intrinsic(intrinsic),
    }
}

fn add_array_builtins(funcs: &mut IndexMap<String, FnScheme>) {
    funcs.insert(
        IntrinsicId::ArrayGet.source_name().to_string(),
        make_fn_scheme(IntrinsicId::ArrayGet),
    );
    funcs.insert(
        IntrinsicId::ArraySet.source_name().to_string(),
        make_fn_scheme(IntrinsicId::ArraySet),
    );
}

fn build_builtin_callables() -> IndexMap<String, CallableBody> {
    let mut callables = IndexMap::new();
    for item in builtin_ast().toplevels.iter() {
        match item {
            ast::Item::Fn(func) => {
                callables.insert(func.name.0.clone(), CallableBody::Goml);
            }
            ast::Item::ExternFn(ext) => {
                let body = crate::intrinsics::callable_body_from_attributes(
                    ext.attrs.iter().map(|attr| attr.text.as_str()),
                )
                .expect("invalid builtin callable declaration");
                callables.insert(ext.name.0.clone(), body);
            }
            ast::Item::EnumDef(_)
            | ast::Item::StructDef(_)
            | ast::Item::TraitDef(_)
            | ast::Item::ImplBlock(_) => {}
        }
    }
    callables.insert(
        IntrinsicId::ArrayGet.source_name().to_string(),
        CallableBody::Intrinsic(IntrinsicId::ArrayGet),
    );
    callables.insert(
        IntrinsicId::ArraySet.source_name().to_string(),
        CallableBody::Intrinsic(IntrinsicId::ArraySet),
    );
    callables
}

pub fn builtin_callables() -> &'static IndexMap<String, CallableBody> {
    BUILTIN_CALLABLES.get_or_init(build_builtin_callables)
}

pub fn builtin_function_names() -> Vec<String> {
    builtin_callables().keys().cloned().collect()
}
