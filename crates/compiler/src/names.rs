use crate::tast;

pub fn ty_compact(ty: &tast::Ty) -> String {
    ty.to_pretty(10000)
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect()
}

pub fn trait_impl_fn_name(
    trait_name: &tast::TastIdent,
    for_ty: &tast::Ty,
    method_name: &str,
) -> String {
    format!(
        "trait_impl#{}#{}#{}",
        trait_name.0,
        ty_compact(for_ty),
        method_name
    )
}

pub fn inherent_method_fn_name(receiver_ty: &tast::Ty, method_name: &str) -> String {
    if is_primitive(receiver_ty) {
        return format!("{}_{}", inherent_base(receiver_ty), method_name);
    }
    let base = inherent_base(receiver_ty);
    format!(
        "inherent#{}#{}#{}",
        base,
        ty_compact(receiver_ty),
        method_name
    )
}

pub fn parse_inherent_method_fn_name(name: &str) -> Option<(&str, &str)> {
    let mut parts = name.split('#');
    if parts.next()? != "inherent" {
        return None;
    }
    let base = parts.next()?;
    let _ty = parts.next()?;
    let method = parts.next()?;
    if parts.next().is_some() {
        return None;
    }
    Some((base, method))
}

fn inherent_base(receiver_ty: &tast::Ty) -> String {
    match receiver_ty {
        tast::Ty::TUnit => "unit".to_string(),
        tast::Ty::TBool => "bool".to_string(),
        tast::Ty::TInt8 => "int8".to_string(),
        tast::Ty::TInt16 => "int16".to_string(),
        tast::Ty::TInt32 => "int32".to_string(),
        tast::Ty::TInt64 => "int64".to_string(),
        tast::Ty::TUint8 => "uint8".to_string(),
        tast::Ty::TUint16 => "uint16".to_string(),
        tast::Ty::TUint32 => "uint32".to_string(),
        tast::Ty::TUint64 => "uint64".to_string(),
        tast::Ty::TFloat32 => "float32".to_string(),
        tast::Ty::TFloat64 => "float64".to_string(),
        tast::Ty::TString => "string".to_string(),
        tast::Ty::TEnum { .. }
        | tast::Ty::TStruct { .. }
        | tast::Ty::TApp { .. }
        | tast::Ty::TVec { .. }
        | tast::Ty::TRef { .. } => receiver_ty.get_constr_name_unsafe(),
        other => ty_compact(other),
    }
}

fn is_primitive(ty: &tast::Ty) -> bool {
    matches!(
        ty,
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
            | tast::Ty::TString
    )
}
