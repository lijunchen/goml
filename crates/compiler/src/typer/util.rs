use std::collections::HashSet;

use crate::{
    env::Env,
    tast::{self},
};
use ast::ast;

pub(crate) fn validate_ty(env: &mut Env, ty: &tast::Ty, tparams: &HashSet<String>) {
    match ty {
        tast::Ty::TVar(_) => {}
        tast::Ty::TUnit | tast::Ty::TBool | tast::Ty::TInt | tast::Ty::TString => {}
        tast::Ty::TTuple { typs } => {
            for ty in typs.iter() {
                validate_ty(env, ty, tparams);
            }
        }
        tast::Ty::TFunc { params, ret_ty } => {
            for param in params.iter() {
                validate_ty(env, param, tparams);
            }
            validate_ty(env, ret_ty.as_ref(), tparams);
        }
        tast::Ty::TParam { name } => {
            if !tparams.contains(name) {
                env.report_typer_error(format!("Unbound type parameter {}", name));
            }
        }
        tast::Ty::TCon { name } => {
            if name == "Self" {
                return;
            }
            let ident = ast::Uident::new(name);
            if let Some(enum_def) = env.enums.get(&ident) {
                if !enum_def.generics.is_empty() {
                    env.report_typer_error(format!(
                        "Type {} expects {} type arguments, but got 0",
                        name,
                        enum_def.generics.len()
                    ));
                }
            } else if let Some(struct_def) = env.structs.get(&ident) {
                if !struct_def.generics.is_empty() {
                    env.report_typer_error(format!(
                        "Type {} expects {} type arguments, but got 0",
                        name,
                        struct_def.generics.len()
                    ));
                }
            } else {
                env.report_typer_error(format!("Unknown type constructor {}", name));
            }
        }
        tast::Ty::TApp { ty, args } => {
            for arg in args.iter() {
                validate_ty(env, arg, tparams);
            }

            let base_name = ty.get_constr_name_unsafe();
            let ident = ast::Uident::new(&base_name);

            if let Some(enum_def) = env.enums.get(&ident) {
                let expected = enum_def.generics.len();
                let actual = args.len();
                if expected != actual {
                    env.report_typer_error(format!(
                        "Type {} expects {} type arguments, but got {}",
                        base_name, expected, actual
                    ));
                }
            } else if let Some(struct_def) = env.structs.get(&ident) {
                let expected = struct_def.generics.len();
                let actual = args.len();
                if expected != actual {
                    env.report_typer_error(format!(
                        "Type {} expects {} type arguments, but got {}",
                        base_name, expected, actual
                    ));
                }
            } else {
                env.report_typer_error(format!("Unknown type constructor {}", base_name));
            }
        }
        tast::Ty::TArray { elem, .. } => {
            validate_ty(env, elem, tparams);
        }
        tast::Ty::TRef { elem } => {
            validate_ty(env, elem, tparams);
        }
    }
}

pub(crate) fn ast_ty_to_tast_ty_with_tparams_env(
    ast_ty: &ast::Ty,
    tparams_env: &[ast::Uident],
) -> tast::Ty {
    match ast_ty {
        ast::Ty::TUnit => tast::Ty::TUnit,
        ast::Ty::TBool => tast::Ty::TBool,
        ast::Ty::TInt => tast::Ty::TInt,
        ast::Ty::TString => tast::Ty::TString,
        ast::Ty::TTuple { typs } => {
            let typs = typs
                .iter()
                .map(|ty| ast_ty_to_tast_ty_with_tparams_env(ty, tparams_env))
                .collect();
            tast::Ty::TTuple { typs }
        }
        ast::Ty::TCon { name } => {
            if tparams_env.iter().any(|param| param.0 == *name) {
                tast::Ty::TParam { name: name.clone() }
            } else {
                tast::Ty::TCon { name: name.clone() }
            }
        }
        ast::Ty::TApp { ty, args } => {
            let ty = ast_ty_to_tast_ty_with_tparams_env(ty, tparams_env);
            let args = args
                .iter()
                .map(|ty| ast_ty_to_tast_ty_with_tparams_env(ty, tparams_env))
                .collect();
            tast::Ty::TApp {
                ty: Box::new(ty),
                args,
            }
        }
        ast::Ty::TArray { len, elem } => tast::Ty::TArray {
            len: *len,
            elem: Box::new(ast_ty_to_tast_ty_with_tparams_env(elem, tparams_env)),
        },
        ast::Ty::TFunc { params, ret_ty } => tast::Ty::TFunc {
            params: params
                .iter()
                .map(|ty| ast_ty_to_tast_ty_with_tparams_env(ty, tparams_env))
                .collect(),
            ret_ty: Box::new(ast_ty_to_tast_ty_with_tparams_env(ret_ty, tparams_env)),
        },
    }
}

pub(crate) fn type_param_name_set(tparams: &[ast::Uident]) -> HashSet<String> {
    tparams.iter().map(|param| param.0.clone()).collect()
}

pub(crate) fn binary_supports_builtin(op: ast::BinaryOp, lhs: &tast::Ty, rhs: &tast::Ty) -> bool {
    match op {
        ast::BinaryOp::Add => matches!(
            (lhs, rhs),
            (tast::Ty::TInt, tast::Ty::TInt) | (tast::Ty::TString, tast::Ty::TString)
        ),
        ast::BinaryOp::Sub | ast::BinaryOp::Mul | ast::BinaryOp::Div => {
            matches!((lhs, rhs), (tast::Ty::TInt, tast::Ty::TInt))
        }
        ast::BinaryOp::And | ast::BinaryOp::Or => {
            matches!((lhs, rhs), (tast::Ty::TBool, tast::Ty::TBool))
        }
        ast::BinaryOp::Assign => matches!(lhs, tast::Ty::TRef { .. }),
    }
}
