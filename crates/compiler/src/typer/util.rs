use std::collections::HashSet;

use crate::{
    env::GlobalTypeEnv,
    tast::{self},
};
use ast::ast;
use diagnostics::{Severity, Stage};
use parser::{Diagnostic, Diagnostics};

pub(crate) fn validate_ty(
    genv: &mut GlobalTypeEnv,
    diagnostics: &mut Diagnostics,
    ty: &tast::Ty,
    tparams: &HashSet<String>,
) {
    match ty {
        tast::Ty::TVar(_) => {}
        tast::Ty::TUnit
        | tast::Ty::TBool
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
                validate_ty(genv, diagnostics, ty, tparams);
            }
        }
        tast::Ty::TFunc { params, ret_ty } => {
            for param in params.iter() {
                validate_ty(genv, diagnostics, param, tparams);
            }
            validate_ty(genv, diagnostics, ret_ty.as_ref(), tparams);
        }
        tast::Ty::TParam { name } => {
            if !tparams.contains(name) {
                diagnostics.push(Diagnostic::new(
                    Stage::Typer,
                    Severity::Error,
                    format!("Unknown type parameter {}", name),
                ));
            }
        }
        tast::Ty::TEnum { name } | tast::Ty::TStruct { name } => {
            if name == "Self" {
                return;
            }
            let ident = ast::Ident::new(name);
            if let Some(enum_def) = genv.enums().get(&ident) {
                if !enum_def.generics.is_empty() {
                    diagnostics.push(Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Type {} expects {} type arguments, but got 0",
                            name,
                            enum_def.generics.len()
                        ),
                    ));
                }
            } else if let Some(struct_def) = genv.structs().get(&ident) {
                if !struct_def.generics.is_empty() {
                    diagnostics.push(Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Type {} expects {} type arguments, but got 0",
                            name,
                            struct_def.generics.len()
                        ),
                    ));
                }
            } else if genv.extern_types.contains_key(name) {
                // Extern types do not have generics.
            } else {
                diagnostics.push(Diagnostic::new(
                    Stage::Typer,
                    Severity::Error,
                    format!("Unknown type constructor {}", name),
                ));
            }
        }
        tast::Ty::TApp { ty, args } => {
            for arg in args.iter() {
                validate_ty(genv, diagnostics, arg, tparams);
            }

            let base_name = ty.get_constr_name_unsafe();
            let ident = ast::Ident::new(&base_name);

            if let Some(enum_def) = genv.enums().get(&ident) {
                let expected = enum_def.generics.len();
                let actual = args.len();
                if expected != actual {
                    diagnostics.push(Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Type {} expects {} type arguments, but got {}",
                            base_name, expected, actual
                        ),
                    ));
                }
            } else if let Some(struct_def) = genv.structs().get(&ident) {
                let expected = struct_def.generics.len();
                let actual = args.len();
                if expected != actual {
                    diagnostics.push(Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Type {} expects {} type arguments, but got {}",
                            base_name, expected, actual
                        ),
                    ));
                }
            } else if genv.extern_types.contains_key(&base_name) {
                if !args.is_empty() {
                    diagnostics.push(Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Type {} does not accept type arguments, but got {}",
                            base_name,
                            args.len()
                        ),
                    ));
                }
            } else {
                diagnostics.push(Diagnostic::new(
                    Stage::Typer,
                    Severity::Error,
                    format!("Unknown type constructor {}", base_name),
                ));
            }
        }
        tast::Ty::TArray { elem, .. } => {
            validate_ty(genv, diagnostics, elem, tparams);
        }
        tast::Ty::TRef { elem } => {
            validate_ty(genv, diagnostics, elem, tparams);
        }
    }
}

impl tast::Ty {
    pub(crate) fn from_ast(
        genv: &GlobalTypeEnv,
        ast_ty: &ast::TypeExpr,
        tparams_env: &[ast::Ident],
    ) -> Self {
        match ast_ty {
            ast::TypeExpr::TUnit => Self::TUnit,
            ast::TypeExpr::TBool => Self::TBool,
            ast::TypeExpr::TInt8 => Self::TInt8,
            ast::TypeExpr::TInt16 => Self::TInt16,
            ast::TypeExpr::TInt32 => Self::TInt32,
            ast::TypeExpr::TInt64 => Self::TInt64,
            ast::TypeExpr::TUint8 => Self::TUint8,
            ast::TypeExpr::TUint16 => Self::TUint16,
            ast::TypeExpr::TUint32 => Self::TUint32,
            ast::TypeExpr::TUint64 => Self::TUint64,
            ast::TypeExpr::TFloat32 => Self::TFloat32,
            ast::TypeExpr::TFloat64 => Self::TFloat64,
            ast::TypeExpr::TString => Self::TString,
            ast::TypeExpr::TTuple { typs } => Self::TTuple {
                typs: typs
                    .iter()
                    .map(|ty| Self::from_ast(genv, ty, tparams_env))
                    .collect(),
            },
            ast::TypeExpr::TCon { name } => {
                if tparams_env.iter().any(|param| param.0 == *name) {
                    Self::TParam { name: name.clone() }
                } else {
                    let ident = ast::Ident::new(name);
                    if genv.enums().contains_key(&ident) {
                        Self::TEnum { name: name.clone() }
                    } else if genv.structs().contains_key(&ident)
                        || genv.extern_types.contains_key(name)
                    {
                        Self::TStruct { name: name.clone() }
                    } else {
                        // Unknown types (e.g., `Self`, unbound type parameters, or truly
                        // unknown types) are represented as TStruct here. Validation in
                        // `validate_ty` will report appropriate diagnostics for invalid uses.
                        Self::TStruct { name: name.clone() }
                    }
                }
            }
            ast::TypeExpr::TApp { ty, args } => {
                // Special handling for Ref[T] - convert to TRef
                if let ast::TypeExpr::TCon { name } = ty.as_ref()
                    && name == "Ref"
                    && args.len() == 1
                {
                    return Self::TRef {
                        elem: Box::new(Self::from_ast(genv, &args[0], tparams_env)),
                    };
                }
                Self::TApp {
                    ty: Box::new(Self::from_ast(genv, ty, tparams_env)),
                    args: args
                        .iter()
                        .map(|ty| Self::from_ast(genv, ty, tparams_env))
                        .collect(),
                }
            }
            ast::TypeExpr::TArray { len, elem } => Self::TArray {
                len: *len,
                elem: Box::new(Self::from_ast(genv, elem, tparams_env)),
            },
            ast::TypeExpr::TFunc { params, ret_ty } => Self::TFunc {
                params: params
                    .iter()
                    .map(|ty| Self::from_ast(genv, ty, tparams_env))
                    .collect(),
                ret_ty: Box::new(Self::from_ast(genv, ret_ty, tparams_env)),
            },
        }
    }
}

pub(crate) fn type_param_name_set(tparams: &[ast::Ident]) -> HashSet<String> {
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
        (tast::Ty::TInt8, tast::Ty::TInt8)
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
