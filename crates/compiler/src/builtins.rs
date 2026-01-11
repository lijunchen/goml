use std::{collections::HashMap, path::Path, sync::OnceLock};

use crate::derive;
use ::ast::{ast, lower};
use cst::cst::{CstNode, File as CstFile};
use indexmap::{IndexMap, IndexSet};
use parser::{self, syntax::MySyntaxNode};

use crate::{
    env::{FnOrigin, FnScheme, GlobalTypeEnv, TraitEnv, TypeEnv, ValueEnv},
    fir,
    mangle::encode_ty,
    tast, typer,
};

/// The embedded builtin.gom source code
const BUILTIN_GOM: &str = include_str!("builtin.gom");

static BUILTIN_AST: OnceLock<ast::File> = OnceLock::new();
static BUILTIN_GENV: OnceLock<GlobalTypeEnv> = OnceLock::new();

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
    ast_file.package = ast::AstIdent::new("Builtin");

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

fn build_builtin_env() -> GlobalTypeEnv {
    let ast = builtin_ast();

    let base_env = GlobalTypeEnv {
        type_env: TypeEnv::new(),
        trait_env: TraitEnv::new(),
        value_env: ValueEnv {
            funcs: IndexMap::new(),
            extern_funcs: IndexMap::new(),
        },
    };

    let (fir, fir_table) = fir::lower_to_fir(ast);

    let (_tast, mut genv, diagnostics) = typer::check_file_with_env(
        fir,
        fir_table,
        base_env,
        "Builtin",
        HashMap::new(),
    );
    if diagnostics.has_errors() {
        panic!("Failed to typecheck builtin.gom: {:?}", diagnostics);
    }

    add_array_builtins(&mut genv.value_env.funcs);
    add_ref_builtins(&mut genv.value_env.funcs);
    add_vec_builtins(&mut genv.value_env.funcs);

    genv
}

pub(crate) fn builtin_env() -> GlobalTypeEnv {
    BUILTIN_GENV.get_or_init(build_builtin_env).clone()
}

fn make_fn_scheme(params: Vec<tast::Ty>, ret: tast::Ty) -> FnScheme {
    FnScheme {
        type_params: vec![],
        constraints: (),
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

fn add_ref_builtins(funcs: &mut IndexMap<String, FnScheme>) {
    let ref_elem_param = tast::Ty::TParam {
        name: "T".to_string(),
    };
    let ref_ty = tast::Ty::TRef {
        elem: Box::new(ref_elem_param.clone()),
    };
    funcs.insert(
        "ref".to_string(),
        make_fn_scheme(
            vec![ref_elem_param.clone()],
            tast::Ty::TRef {
                elem: Box::new(ref_elem_param.clone()),
            },
        ),
    );
    funcs.insert(
        "ref_get".to_string(),
        make_fn_scheme(vec![ref_ty.clone()], ref_elem_param.clone()),
    );
    funcs.insert(
        "ref_set".to_string(),
        make_fn_scheme(
            vec![ref_ty.clone(), ref_elem_param.clone()],
            tast::Ty::TUnit,
        ),
    );
}

fn add_vec_builtins(funcs: &mut IndexMap<String, FnScheme>) {
    let vec_elem_param = tast::Ty::TParam {
        name: "T".to_string(),
    };
    // Vec[T] is now a built-in type TVec { elem: T }
    let vec_ty = tast::Ty::TVec {
        elem: Box::new(vec_elem_param.clone()),
    };

    // vec_new() -> Vec[T]
    funcs.insert(
        "vec_new".to_string(),
        make_fn_scheme(vec![], vec_ty.clone()),
    );

    // vec_push(vec: Vec[T], elem: T) -> Vec[T]
    funcs.insert(
        "vec_push".to_string(),
        make_fn_scheme(vec![vec_ty.clone(), vec_elem_param.clone()], vec_ty.clone()),
    );

    // vec_get(vec: Vec[T], index: int32) -> T
    funcs.insert(
        "vec_get".to_string(),
        make_fn_scheme(
            vec![vec_ty.clone(), tast::Ty::TInt32],
            vec_elem_param.clone(),
        ),
    );

    // vec_len(vec: Vec[T]) -> int32
    funcs.insert(
        "vec_len".to_string(),
        make_fn_scheme(vec![vec_ty.clone()], tast::Ty::TInt32),
    );
}

pub(super) fn builtin_inherent_methods() -> IndexMap<String, crate::env::ImplDef> {
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
            constraints: (),
            ty: method_ty,
            origin: FnOrigin::Builtin,
        },
    );
    impls.insert(encode_ty(&int32_ty), int32_impl);

    impls
}

pub fn builtin_function_names() -> Vec<String> {
    let mut names: IndexSet<String> = IndexSet::new();

    for item in builtin_ast().toplevels.iter() {
        if let ast::Item::ExternBuiltin(ext) = item {
            names.insert(ext.name.0.clone());
        }
    }

    for name in [
        "array_get",
        "array_set",
        "ref",
        "ref_get",
        "ref_set",
        "vec_new",
        "vec_push",
        "vec_get",
        "vec_len",
    ] {
        names.insert(name.to_string());
    }

    names.into_iter().collect()
}
