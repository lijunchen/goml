use std::collections::HashMap;

use parser::Diagnostics;

use crate::{env::PackageTypeEnv, tast};

use super::{
    ArithmeticKind, Typer,
    type_ops::{contains_tvar, decompose_struct_type, same_or_unresolved_ty, substitute_ty_params},
    util::{format_ty_for_diag, push_error_with_range, resolve_type_name},
};

pub(crate) fn integer_literal_target(expected: &tast::Ty) -> Option<tast::Ty> {
    is_integer_ty(expected).then(|| expected.clone())
}

pub(crate) fn is_integer_ty(ty: &tast::Ty) -> bool {
    matches!(
        ty,
        tast::Ty::TInt8
            | tast::Ty::TInt16
            | tast::Ty::TInt32
            | tast::Ty::TInt64
            | tast::Ty::TUint8
            | tast::Ty::TUint16
            | tast::Ty::TUint32
            | tast::Ty::TUint64
    )
}

pub(crate) fn is_float_ty(ty: &tast::Ty) -> bool {
    matches!(ty, tast::Ty::TFloat32 | tast::Ty::TFloat64)
}

pub(crate) fn is_numeric_ty(ty: &tast::Ty) -> bool {
    is_integer_ty(ty) || is_float_ty(ty)
}

fn is_order_comparable_ty(ty: &tast::Ty) -> bool {
    is_numeric_ty(ty) || matches!(ty, tast::Ty::TString | tast::Ty::TChar)
}

pub(crate) fn comparison_operand_is_valid(
    genv: &PackageTypeEnv,
    op: common_defs::BinaryOp,
    ty: &tast::Ty,
) -> bool {
    match op {
        common_defs::BinaryOp::Eq | common_defs::BinaryOp::NotEq => {
            is_equality_comparable_ty(genv, ty)
        }
        common_defs::BinaryOp::Less
        | common_defs::BinaryOp::Greater
        | common_defs::BinaryOp::LessEq
        | common_defs::BinaryOp::GreaterEq => is_order_comparable_ty(ty),
        common_defs::BinaryOp::Add
        | common_defs::BinaryOp::Sub
        | common_defs::BinaryOp::Mul
        | common_defs::BinaryOp::Div
        | common_defs::BinaryOp::And
        | common_defs::BinaryOp::Or => true,
    }
}

fn is_equality_comparable_ty(genv: &PackageTypeEnv, ty: &tast::Ty) -> bool {
    match ty {
        tast::Ty::TUnit
        | tast::Ty::TBool
        | tast::Ty::TString
        | tast::Ty::TChar
        | tast::Ty::TInt8
        | tast::Ty::TInt16
        | tast::Ty::TInt32
        | tast::Ty::TInt64
        | tast::Ty::TUint8
        | tast::Ty::TUint16
        | tast::Ty::TUint32
        | tast::Ty::TUint64
        | tast::Ty::TFloat32
        | tast::Ty::TFloat64 => true,
        tast::Ty::TTuple { typs } => typs
            .iter()
            .all(|item| is_equality_comparable_ty(genv, item)),
        tast::Ty::TArray { elem, .. } => is_equality_comparable_ty(genv, elem),
        _ => is_struct_equality_comparable_ty(genv, ty),
    }
}

fn is_struct_equality_comparable_ty(genv: &PackageTypeEnv, ty: &tast::Ty) -> bool {
    let Some((type_name, type_args)) = decompose_struct_type(ty) else {
        return false;
    };
    let (resolved, env) = resolve_type_name(genv, &type_name);
    let Some(struct_def) = env.structs().get(&tast::TastIdent::new(&resolved)) else {
        return false;
    };
    if struct_def.generics.len() != type_args.len() {
        return false;
    }

    let subst = struct_def
        .generics
        .iter()
        .zip(type_args.iter())
        .map(|(param, arg)| (param.0.clone(), arg.clone()))
        .collect::<HashMap<_, _>>();

    struct_def.fields.iter().all(|(_, field_ty)| {
        let field_ty = substitute_ty_params(field_ty, &subst);
        is_equality_comparable_ty(genv, &field_ty)
    })
}

pub(crate) fn comparison_operator_text(op: common_defs::BinaryOp) -> &'static str {
    match op {
        common_defs::BinaryOp::Less => "<",
        common_defs::BinaryOp::Greater => ">",
        common_defs::BinaryOp::LessEq => "<=",
        common_defs::BinaryOp::GreaterEq => ">=",
        common_defs::BinaryOp::Eq => "==",
        common_defs::BinaryOp::NotEq => "!=",
        common_defs::BinaryOp::Add => "+",
        common_defs::BinaryOp::Sub => "-",
        common_defs::BinaryOp::Mul => "*",
        common_defs::BinaryOp::Div => "/",
        common_defs::BinaryOp::And => "&&",
        common_defs::BinaryOp::Or => "||",
    }
}

pub(crate) fn is_signed_numeric_ty(ty: &tast::Ty) -> bool {
    matches!(
        ty,
        tast::Ty::TInt8
            | tast::Ty::TInt16
            | tast::Ty::TInt32
            | tast::Ty::TInt64
            | tast::Ty::TFloat32
            | tast::Ty::TFloat64
    )
}

impl Typer {
    pub(crate) fn validate_deferred_comparison_checks(
        &mut self,
        genv: &PackageTypeEnv,
        diagnostics: &mut Diagnostics,
    ) {
        let checks = std::mem::take(&mut self.deferred_comparison_checks);
        for check in checks {
            let lhs_norm = self.norm(&check.lhs_ty);
            let rhs_norm = self.norm(&check.rhs_ty);
            if contains_tvar(&lhs_norm)
                || contains_tvar(&rhs_norm)
                || !same_or_unresolved_ty(&lhs_norm, &rhs_norm)
            {
                continue;
            }
            if comparison_operand_is_valid(genv, check.op, &lhs_norm) {
                continue;
            }
            push_error_with_range(
                diagnostics,
                format!(
                    "Operator {} is not defined for type {}",
                    comparison_operator_text(check.op),
                    format_ty_for_diag(&lhs_norm)
                ),
                check.origin,
            );
        }
    }

    pub(crate) fn validate_deferred_arithmetic_checks(&mut self, diagnostics: &mut Diagnostics) {
        let checks = std::mem::take(&mut self.deferred_arithmetic_checks);
        for check in checks {
            let norm_ty = self.norm(&check.ty);
            let valid = match check.kind {
                ArithmeticKind::NumericOrString => {
                    is_numeric_ty(&norm_ty) || matches!(norm_ty, tast::Ty::TString)
                }
                ArithmeticKind::Numeric => is_numeric_ty(&norm_ty),
            };
            if !valid && !matches!(norm_ty, tast::Ty::TVar(..)) {
                push_error_with_range(
                    diagnostics,
                    format!(
                        "Operator {} is not defined for type {}",
                        check.op,
                        format_ty_for_diag(&norm_ty)
                    ),
                    check.origin,
                );
            }
        }
    }
}
