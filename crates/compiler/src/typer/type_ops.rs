use std::{
    collections::{HashMap, HashSet},
    ops::ControlFlow,
};

use crate::tast;

fn rewrite_ty(ty: &tast::Ty, rewrite: &mut impl FnMut(&tast::Ty) -> Option<tast::Ty>) -> tast::Ty {
    if let Some(rewritten) = rewrite(ty) {
        return rewritten;
    }

    match ty {
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
        | tast::Ty::TChar
        | tast::Ty::TEnum { .. }
        | tast::Ty::TStruct { .. }
        | tast::Ty::TDyn { .. }
        | tast::Ty::TParam { .. } => ty.clone(),
        tast::Ty::TTuple { typs } => tast::Ty::TTuple {
            typs: typs.iter().map(|ty| rewrite_ty(ty, rewrite)).collect(),
        },
        tast::Ty::TApp { ty, args } => tast::Ty::TApp {
            ty: Box::new(rewrite_ty(ty, rewrite)),
            args: args.iter().map(|ty| rewrite_ty(ty, rewrite)).collect(),
        },
        tast::Ty::TArray { len, elem } => tast::Ty::TArray {
            len: *len,
            elem: Box::new(rewrite_ty(elem, rewrite)),
        },
        tast::Ty::TSlice { elem } => tast::Ty::TSlice {
            elem: Box::new(rewrite_ty(elem, rewrite)),
        },
        tast::Ty::TVec { elem } => tast::Ty::TVec {
            elem: Box::new(rewrite_ty(elem, rewrite)),
        },
        tast::Ty::TRef { elem } => tast::Ty::TRef {
            elem: Box::new(rewrite_ty(elem, rewrite)),
        },
        tast::Ty::THashMap { key, value } => tast::Ty::THashMap {
            key: Box::new(rewrite_ty(key, rewrite)),
            value: Box::new(rewrite_ty(value, rewrite)),
        },
        tast::Ty::TFunc { params, ret_ty } => tast::Ty::TFunc {
            params: params.iter().map(|ty| rewrite_ty(ty, rewrite)).collect(),
            ret_ty: Box::new(rewrite_ty(ret_ty, rewrite)),
        },
    }
}

pub(crate) fn substitute_ty_params(ty: &tast::Ty, subst: &HashMap<String, tast::Ty>) -> tast::Ty {
    rewrite_ty(ty, &mut |ty| match ty {
        tast::Ty::TParam { name } => subst.get(name).cloned(),
        _ => None,
    })
}

pub(crate) fn instantiate_self_ty(ty: &tast::Ty, self_ty: &tast::Ty) -> tast::Ty {
    rewrite_ty(ty, &mut |ty| match ty {
        tast::Ty::TStruct { name } if name == "Self" => Some(self_ty.clone()),
        _ => None,
    })
}

pub(crate) fn decompose_struct_type(ty: &tast::Ty) -> Option<(String, Vec<tast::Ty>)> {
    match ty {
        tast::Ty::TStruct { name } => Some((name.clone(), Vec::new())),
        tast::Ty::TApp { ty, args } => {
            let (name, mut collected) = decompose_struct_type(ty)?;
            collected.extend(args.iter().cloned());
            Some((name, collected))
        }
        _ => None,
    }
}

fn visit_ty<B>(
    ty: &tast::Ty,
    visitor: &mut impl FnMut(&tast::Ty) -> ControlFlow<B>,
) -> ControlFlow<B> {
    visitor(ty)?;

    match ty {
        tast::Ty::TTuple { typs } => {
            for ty in typs {
                visit_ty(ty, visitor)?;
            }
        }
        tast::Ty::TApp { ty, args } => {
            visit_ty(ty, visitor)?;
            for ty in args {
                visit_ty(ty, visitor)?;
            }
        }
        tast::Ty::TArray { elem, .. }
        | tast::Ty::TSlice { elem }
        | tast::Ty::TVec { elem }
        | tast::Ty::TRef { elem } => visit_ty(elem, visitor)?,
        tast::Ty::THashMap { key, value } => {
            visit_ty(key, visitor)?;
            visit_ty(value, visitor)?;
        }
        tast::Ty::TFunc { params, ret_ty } => {
            for ty in params {
                visit_ty(ty, visitor)?;
            }
            visit_ty(ret_ty, visitor)?;
        }
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
        | tast::Ty::TChar
        | tast::Ty::TEnum { .. }
        | tast::Ty::TStruct { .. }
        | tast::Ty::TDyn { .. }
        | tast::Ty::TParam { .. } => {}
    }

    ControlFlow::Continue(())
}

pub(crate) fn contains_tvar(ty: &tast::Ty) -> bool {
    visit_ty(ty, &mut |ty| match ty {
        tast::Ty::TVar(_) => ControlFlow::Break(()),
        _ => ControlFlow::Continue(()),
    })
    .is_break()
}

pub(crate) fn contains_tparam(ty: &tast::Ty) -> bool {
    visit_ty(ty, &mut |ty| match ty {
        tast::Ty::TParam { .. } => ControlFlow::Break(()),
        _ => ControlFlow::Continue(()),
    })
    .is_break()
}

pub(crate) fn same_or_unresolved_ty(lhs: &tast::Ty, rhs: &tast::Ty) -> bool {
    lhs == rhs || contains_tvar(lhs) || contains_tvar(rhs)
}

pub(crate) fn is_concrete_ty(ty: &tast::Ty) -> bool {
    !contains_tvar(ty) && !contains_tparam(ty)
}

pub(crate) fn collect_type_param_substitution(
    template: &tast::Ty,
    actual: &tast::Ty,
    subst: &mut HashMap<String, tast::Ty>,
) {
    match (template, actual) {
        (tast::Ty::TParam { name }, _) => {
            subst.entry(name.clone()).or_insert_with(|| actual.clone());
        }
        (tast::Ty::TTuple { typs }, tast::Ty::TTuple { typs: actual_typs }) => {
            for (template, actual) in typs.iter().zip(actual_typs.iter()) {
                collect_type_param_substitution(template, actual, subst);
            }
        }
        (
            tast::Ty::TApp { ty, args },
            tast::Ty::TApp {
                ty: actual_ty,
                args: actual_args,
            },
        ) => {
            collect_type_param_substitution(ty, actual_ty, subst);
            for (template, actual) in args.iter().zip(actual_args.iter()) {
                collect_type_param_substitution(template, actual, subst);
            }
        }
        (tast::Ty::TArray { elem, .. }, tast::Ty::TArray { elem: actual, .. })
        | (tast::Ty::TSlice { elem }, tast::Ty::TSlice { elem: actual })
        | (tast::Ty::TVec { elem }, tast::Ty::TVec { elem: actual })
        | (tast::Ty::TRef { elem }, tast::Ty::TRef { elem: actual }) => {
            collect_type_param_substitution(elem, actual, subst);
        }
        (
            tast::Ty::THashMap { key, value },
            tast::Ty::THashMap {
                key: actual_key,
                value: actual_value,
            },
        ) => {
            collect_type_param_substitution(key, actual_key, subst);
            collect_type_param_substitution(value, actual_value, subst);
        }
        (
            tast::Ty::TFunc { params, ret_ty },
            tast::Ty::TFunc {
                params: actual_params,
                ret_ty: actual_ret_ty,
            },
        ) => {
            for (template, actual) in params.iter().zip(actual_params.iter()) {
                collect_type_param_substitution(template, actual, subst);
            }
            collect_type_param_substitution(ret_ty, actual_ret_ty, subst);
        }
        _ => {}
    }
}

fn collect_tvars(ty: &tast::Ty) -> HashSet<tast::TypeVar> {
    let mut vars = HashSet::new();
    let _ = visit_ty(ty, &mut |ty| {
        if let tast::Ty::TVar(var) = ty {
            vars.insert(*var);
        }
        ControlFlow::<()>::Continue(())
    });
    vars
}

pub(crate) fn fn_ret_depends_on_params(ty: &tast::Ty) -> bool {
    let tast::Ty::TFunc { params, ret_ty } = ty else {
        return false;
    };
    let param_vars = params
        .iter()
        .flat_map(collect_tvars)
        .collect::<HashSet<_>>();
    !param_vars.is_empty()
        && collect_tvars(ret_ty)
            .iter()
            .any(|var| param_vars.contains(var))
}
