use std::collections::HashMap;

use crate::tast;

pub(crate) fn trait_impl_subst(
    template: &tast::Ty,
    actual: &tast::Ty,
) -> Option<HashMap<String, tast::Ty>> {
    fn go(template: &tast::Ty, actual: &tast::Ty, subst: &mut HashMap<String, tast::Ty>) -> bool {
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
                    .all(|(template, actual)| go(template, actual, subst)),
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
            tast::Ty::TApp { ty, args } => match actual {
                tast::Ty::TApp {
                    ty: actual_ty,
                    args: actual_args,
                } if args.len() == actual_args.len() => {
                    go(ty, actual_ty, subst)
                        && args
                            .iter()
                            .zip(actual_args.iter())
                            .all(|(template, actual)| go(template, actual, subst))
                }
                _ => false,
            },
            tast::Ty::TArray { len, elem } => match actual {
                tast::Ty::TArray {
                    len: actual_len,
                    elem: actual_elem,
                } => len == actual_len && go(elem, actual_elem, subst),
                _ => false,
            },
            tast::Ty::TSlice { elem } => match actual {
                tast::Ty::TSlice { elem: actual_elem } => go(elem, actual_elem, subst),
                _ => false,
            },
            tast::Ty::TVec { elem } => match actual {
                tast::Ty::TVec { elem: actual_elem } => go(elem, actual_elem, subst),
                _ => false,
            },
            tast::Ty::TRef { elem } => match actual {
                tast::Ty::TRef { elem: actual_elem } => go(elem, actual_elem, subst),
                _ => false,
            },
            tast::Ty::THashMap { key, value } => match actual {
                tast::Ty::THashMap {
                    key: actual_key,
                    value: actual_value,
                } => go(key, actual_key, subst) && go(value, actual_value, subst),
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
                        .all(|(template, actual)| go(template, actual, subst))
                        && go(ret_ty, actual_ret, subst)
                }
                _ => false,
            },
        }
    }

    let mut substitution = HashMap::new();
    go(template, actual, &mut substitution).then_some(substitution)
}
