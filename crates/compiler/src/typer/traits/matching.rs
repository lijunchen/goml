use std::collections::HashMap;

use crate::tast;

fn match_ty_with_mode(
    template: &tast::Ty,
    actual: &tast::Ty,
    subst: &mut HashMap<String, tast::Ty>,
    erase_projections: bool,
) -> bool {
    match template {
        tast::Ty::TParam { name } => match subst.get(name) {
            Some(bound) => bound == actual,
            None => {
                subst.insert(name.clone(), actual.clone());
                true
            }
        },
        tast::Ty::TVar(_)
        | tast::Ty::TUnit
        | tast::Ty::TNever
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
        | tast::Ty::TChar => template == actual,
        tast::Ty::TTuple { typs } => match actual {
            tast::Ty::TTuple { typs: actual_typs } if typs.len() == actual_typs.len() => typs
                .iter()
                .zip(actual_typs.iter())
                .all(|(template, actual)| {
                    match_ty_with_mode(template, actual, subst, erase_projections)
                }),
            _ => false,
        },
        tast::Ty::TEnum { name } => {
            matches!(actual, tast::Ty::TEnum { name: actual } if actual == name)
        }
        tast::Ty::TStruct { name } => {
            matches!(actual, tast::Ty::TStruct { name: actual } if actual == name)
        }
        tast::Ty::TDyn { trait_name } => {
            matches!(actual, tast::Ty::TDyn { trait_name: actual } if actual == trait_name)
        }
        tast::Ty::TProjection { .. } if erase_projections => true,
        tast::Ty::TProjection {
            trait_ref,
            for_ty,
            name,
        } => match actual {
            tast::Ty::TProjection {
                trait_ref: actual_trait_ref,
                for_ty: actual_for_ty,
                name: actual_name,
            } if name == actual_name
                && trait_ref.as_ref().map(|reference| &reference.name)
                    == actual_trait_ref.as_ref().map(|reference| &reference.name) =>
            {
                let args_match = match (trait_ref, actual_trait_ref) {
                    (Some(expected), Some(actual)) if expected.args.len() == actual.args.len() => {
                        expected
                            .args
                            .iter()
                            .zip(actual.args.iter())
                            .all(|(expected, actual)| {
                                match_ty_with_mode(expected, actual, subst, erase_projections)
                            })
                    }
                    (None, None) => true,
                    _ => false,
                };
                args_match && match_ty_with_mode(for_ty, actual_for_ty, subst, erase_projections)
            }
            _ => false,
        },
        tast::Ty::TApp { ty, args } => match actual {
            tast::Ty::TApp {
                ty: actual_ty,
                args: actual_args,
            } if args.len() == actual_args.len() => {
                match_ty_with_mode(ty, actual_ty, subst, erase_projections)
                    && args
                        .iter()
                        .zip(actual_args.iter())
                        .all(|(template, actual)| {
                            match_ty_with_mode(template, actual, subst, erase_projections)
                        })
            }
            _ => false,
        },
        tast::Ty::TArray { len, elem } => match actual {
            tast::Ty::TArray {
                len: actual_len,
                elem: actual_elem,
            } => {
                len == actual_len && match_ty_with_mode(elem, actual_elem, subst, erase_projections)
            }
            _ => false,
        },
        tast::Ty::TSlice { elem } => match actual {
            tast::Ty::TSlice { elem: actual_elem } => {
                match_ty_with_mode(elem, actual_elem, subst, erase_projections)
            }
            _ => false,
        },
        tast::Ty::TVec { elem } => match actual {
            tast::Ty::TVec { elem: actual_elem } => {
                match_ty_with_mode(elem, actual_elem, subst, erase_projections)
            }
            _ => false,
        },
        tast::Ty::TRef { elem } => match actual {
            tast::Ty::TRef { elem: actual_elem } => {
                match_ty_with_mode(elem, actual_elem, subst, erase_projections)
            }
            _ => false,
        },
        tast::Ty::THashMap { key, value } => match actual {
            tast::Ty::THashMap {
                key: actual_key,
                value: actual_value,
            } => {
                match_ty_with_mode(key, actual_key, subst, erase_projections)
                    && match_ty_with_mode(value, actual_value, subst, erase_projections)
            }
            _ => false,
        },
        tast::Ty::TFunc { params, ret_ty } => match actual {
            tast::Ty::TFunc {
                params: actual_params,
                ret_ty: actual_ret,
            } if params.len() == actual_params.len() => {
                params
                    .iter()
                    .zip(actual_params.iter())
                    .all(|(template, actual)| {
                        match_ty_with_mode(template, actual, subst, erase_projections)
                    })
                    && match_ty_with_mode(ret_ty, actual_ret, subst, erase_projections)
            }
            _ => false,
        },
    }
}

fn match_ty(template: &tast::Ty, actual: &tast::Ty, subst: &mut HashMap<String, tast::Ty>) -> bool {
    match_ty_with_mode(template, actual, subst, false)
}

pub(crate) fn impl_self_subst(
    template_self: &tast::Ty,
    actual_self: &tast::Ty,
) -> Option<HashMap<String, tast::Ty>> {
    let mut substitution = HashMap::new();
    match_ty(template_self, actual_self, &mut substitution).then_some(substitution)
}

pub(crate) fn trait_impl_subst(
    template_trait_ref: &tast::TraitRef,
    template_self: &tast::Ty,
    actual_trait_ref: &tast::TraitRef,
    actual_self: &tast::Ty,
) -> Option<HashMap<String, tast::Ty>> {
    if template_trait_ref.name != actual_trait_ref.name
        || template_trait_ref.args.len() != actual_trait_ref.args.len()
    {
        return None;
    }
    let mut substitution = HashMap::new();
    let self_matches = match_ty_with_mode(template_self, actual_self, &mut substitution, true);
    let trait_args_match = template_trait_ref
        .args
        .iter()
        .zip(actual_trait_ref.args.iter())
        .all(|(template, actual)| match_ty_with_mode(template, actual, &mut substitution, true));
    (self_matches && trait_args_match).then_some(substitution)
}
