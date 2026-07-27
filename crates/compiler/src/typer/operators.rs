use std::collections::HashMap;

use crate::{env::PackageTypeEnv, tast};

use super::{
    type_ops::{decompose_struct_type, substitute_ty_params},
    util::resolve_type_name,
};

pub(crate) fn is_integer_ty(ty: &tast::Ty) -> bool {
    matches!(
        ty,
        tast::Ty::TInt
            | tast::Ty::TInt8
            | tast::Ty::TInt16
            | tast::Ty::TInt32
            | tast::Ty::TInt64
            | tast::Ty::TUint
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
        | common_defs::BinaryOp::Rem
        | common_defs::BinaryOp::BitAnd
        | common_defs::BinaryOp::BitOr
        | common_defs::BinaryOp::BitXor
        | common_defs::BinaryOp::Shl
        | common_defs::BinaryOp::Shr
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
        | tast::Ty::TInt
        | tast::Ty::TInt8
        | tast::Ty::TInt16
        | tast::Ty::TInt32
        | tast::Ty::TInt64
        | tast::Ty::TUint
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
        common_defs::BinaryOp::Rem => "%",
        common_defs::BinaryOp::BitAnd => "&",
        common_defs::BinaryOp::BitOr => "|",
        common_defs::BinaryOp::BitXor => "^",
        common_defs::BinaryOp::Shl => "<<",
        common_defs::BinaryOp::Shr => ">>",
        common_defs::BinaryOp::And => "&&",
        common_defs::BinaryOp::Or => "||",
    }
}

pub(crate) fn is_signed_numeric_ty(ty: &tast::Ty) -> bool {
    matches!(
        ty,
        tast::Ty::TInt
            | tast::Ty::TInt8
            | tast::Ty::TInt16
            | tast::Ty::TInt32
            | tast::Ty::TInt64
            | tast::Ty::TFloat32
            | tast::Ty::TFloat64
    )
}
