use std::collections::HashSet;

use crate::{
    env::GlobalTypeEnv,
    tast::{self},
};
use ast::ast;

pub(crate) fn validate_ty(genv: &mut GlobalTypeEnv, ty: &tast::Ty, tparams: &HashSet<String>) {
    match ty {
        tast::Ty::TVar(_) => {}
        tast::Ty::TUnit
        | tast::Ty::TBool
        | tast::Ty::TInt
        | tast::Ty::TInt8
        | tast::Ty::TInt16
        | tast::Ty::TInt32
        | tast::Ty::TInt64
        | tast::Ty::TUint8
        | tast::Ty::TUint16
        | tast::Ty::TUint32
        | tast::Ty::TUint64
        | tast::Ty::TFloat32
        | tast::Ty::TFloat64
        | tast::Ty::TString => {}
        tast::Ty::TTuple { typs } => {
            for ty in typs.iter() {
                validate_ty(genv, ty, tparams);
            }
        }
        tast::Ty::TFunc { params, ret_ty } => {
            for param in params.iter() {
                validate_ty(genv, param, tparams);
            }
            validate_ty(genv, ret_ty.as_ref(), tparams);
        }
        tast::Ty::TParam { name } => {
            if !tparams.contains(name) {
                genv.report_typer_error(format!("Unbound type parameter {}", name));
            }
        }
        tast::Ty::TCon { name } => {
            if name == "Self" {
                return;
            }
            let ident = ast::Uident::new(name);
            if let Some(enum_def) = genv.enums.get(&ident) {
                if !enum_def.generics.is_empty() {
                    genv.report_typer_error(format!(
                        "Type {} expects {} type arguments, but got 0",
                        name,
                        enum_def.generics.len()
                    ));
                }
            } else if let Some(struct_def) = genv.structs.get(&ident) {
                if !struct_def.generics.is_empty() {
                    genv.report_typer_error(format!(
                        "Type {} expects {} type arguments, but got 0",
                        name,
                        struct_def.generics.len()
                    ));
                }
            } else if genv.extern_types.contains_key(name) {
                // Extern types do not have generics.
            } else {
                genv.report_typer_error(format!("Unknown type constructor {}", name));
            }
        }
        tast::Ty::TApp { ty, args } => {
            for arg in args.iter() {
                validate_ty(genv, arg, tparams);
            }

            let base_name = ty.get_constr_name_unsafe();
            let ident = ast::Uident::new(&base_name);

            if let Some(enum_def) = genv.enums.get(&ident) {
                let expected = enum_def.generics.len();
                let actual = args.len();
                if expected != actual {
                    genv.report_typer_error(format!(
                        "Type {} expects {} type arguments, but got {}",
                        base_name, expected, actual
                    ));
                }
            } else if let Some(struct_def) = genv.structs.get(&ident) {
                let expected = struct_def.generics.len();
                let actual = args.len();
                if expected != actual {
                    genv.report_typer_error(format!(
                        "Type {} expects {} type arguments, but got {}",
                        base_name, expected, actual
                    ));
                }
            } else if genv.extern_types.contains_key(&base_name) {
                if !args.is_empty() {
                    genv.report_typer_error(format!(
                        "Type {} does not accept type arguments, but got {}",
                        base_name,
                        args.len()
                    ));
                }
            } else {
                genv.report_typer_error(format!("Unknown type constructor {}", base_name));
            }
        }
        tast::Ty::TArray { elem, .. } => {
            validate_ty(genv, elem, tparams);
        }
        tast::Ty::TRef { elem } => {
            validate_ty(genv, elem, tparams);
        }
    }
}

impl tast::Ty {
    pub(crate) fn from_ast_with_tparams_env(ast_ty: &ast::Ty, tparams_env: &[ast::Uident]) -> Self {
        match ast_ty {
            ast::Ty::TUnit => Self::TUnit,
            ast::Ty::TBool => Self::TBool,
            ast::Ty::TInt => Self::TInt,
            ast::Ty::TInt8 => Self::TInt8,
            ast::Ty::TInt16 => Self::TInt16,
            ast::Ty::TInt32 => Self::TInt32,
            ast::Ty::TInt64 => Self::TInt64,
            ast::Ty::TUint8 => Self::TUint8,
            ast::Ty::TUint16 => Self::TUint16,
            ast::Ty::TUint32 => Self::TUint32,
            ast::Ty::TUint64 => Self::TUint64,
            ast::Ty::TFloat32 => Self::TFloat32,
            ast::Ty::TFloat64 => Self::TFloat64,
            ast::Ty::TString => Self::TString,
            ast::Ty::TTuple { typs } => Self::TTuple {
                typs: typs
                    .iter()
                    .map(|ty| Self::from_ast_with_tparams_env(ty, tparams_env))
                    .collect(),
            },
            ast::Ty::TCon { name } => {
                if tparams_env.iter().any(|param| param.0 == *name) {
                    Self::TParam { name: name.clone() }
                } else {
                    Self::TCon { name: name.clone() }
                }
            }
            ast::Ty::TApp { ty, args } => Self::TApp {
                ty: Box::new(Self::from_ast_with_tparams_env(ty, tparams_env)),
                args: args
                    .iter()
                    .map(|ty| Self::from_ast_with_tparams_env(ty, tparams_env))
                    .collect(),
            },
            ast::Ty::TArray { len, elem } => Self::TArray {
                len: *len,
                elem: Box::new(Self::from_ast_with_tparams_env(elem, tparams_env)),
            },
            ast::Ty::TRef { elem } => Self::TRef {
                elem: Box::new(Self::from_ast_with_tparams_env(elem, tparams_env)),
            },
            ast::Ty::TFunc { params, ret_ty } => Self::TFunc {
                params: params
                    .iter()
                    .map(|ty| Self::from_ast_with_tparams_env(ty, tparams_env))
                    .collect(),
                ret_ty: Box::new(Self::from_ast_with_tparams_env(ret_ty, tparams_env)),
            },
        }
    }
}

pub(crate) fn type_param_name_set(tparams: &[ast::Uident]) -> HashSet<String> {
    tparams.iter().map(|param| param.0.clone()).collect()
}

pub(crate) fn binary_supports_builtin(op: ast::BinaryOp, lhs: &tast::Ty, rhs: &tast::Ty) -> bool {
    match op {
        ast::BinaryOp::Add => {
            if matches!((lhs, rhs), (tast::Ty::TString, tast::Ty::TString)) {
                return true;
            }
            numeric_binary_same_width(lhs, rhs)
        }
        ast::BinaryOp::Sub | ast::BinaryOp::Mul | ast::BinaryOp::Div => {
            numeric_binary_same_width(lhs, rhs)
        }
        ast::BinaryOp::And | ast::BinaryOp::Or => {
            matches!((lhs, rhs), (tast::Ty::TBool, tast::Ty::TBool))
        }
    }
}

fn numeric_binary_same_width(lhs: &tast::Ty, rhs: &tast::Ty) -> bool {
    matches!(
        (lhs, rhs),
        (tast::Ty::TInt, tast::Ty::TInt)
            | (tast::Ty::TInt8, tast::Ty::TInt8)
            | (tast::Ty::TInt16, tast::Ty::TInt16)
            | (tast::Ty::TInt32, tast::Ty::TInt32)
            | (tast::Ty::TInt64, tast::Ty::TInt64)
            | (tast::Ty::TUint8, tast::Ty::TUint8)
            | (tast::Ty::TUint16, tast::Ty::TUint16)
            | (tast::Ty::TUint32, tast::Ty::TUint32)
            | (tast::Ty::TUint64, tast::Ty::TUint64)
            | (tast::Ty::TFloat32, tast::Ty::TFloat32)
            | (tast::Ty::TFloat64, tast::Ty::TFloat64)
    )
}
