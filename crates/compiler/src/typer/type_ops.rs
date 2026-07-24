use std::{
    collections::{HashMap, HashSet},
    ops::ControlFlow,
};

use crate::{env, tast};

pub(crate) fn rewrite_ty(
    ty: &tast::Ty,
    rewrite: &mut impl FnMut(&tast::Ty) -> Option<tast::Ty>,
) -> tast::Ty {
    if let Some(rewritten) = rewrite(ty) {
        return rewritten;
    }

    match ty {
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
        | tast::Ty::TChar
        | tast::Ty::TEnum { .. }
        | tast::Ty::TStruct { .. }
        | tast::Ty::TDyn { .. }
        | tast::Ty::TParam { .. } => ty.clone(),
        tast::Ty::TProjection {
            trait_ref,
            for_ty,
            name,
        } => tast::Ty::TProjection {
            trait_ref: trait_ref.as_ref().map(|trait_ref| tast::TraitRef {
                name: trait_ref.name.clone(),
                args: trait_ref
                    .args
                    .iter()
                    .map(|arg| rewrite_ty(arg, rewrite))
                    .collect(),
            }),
            for_ty: Box::new(rewrite_ty(for_ty, rewrite)),
            name: name.clone(),
        },
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

pub(crate) fn substitute_trait_ref(
    trait_ref: &tast::TraitRef,
    subst: &HashMap<String, tast::Ty>,
) -> tast::TraitRef {
    tast::TraitRef {
        name: trait_ref.name.clone(),
        args: trait_ref
            .args
            .iter()
            .map(|arg| substitute_ty_params(arg, subst))
            .collect(),
    }
}

pub(crate) fn substitute_predicate(
    predicate: &env::TypePredicate,
    subst: &HashMap<String, tast::Ty>,
) -> env::TypePredicate {
    match predicate {
        env::TypePredicate::Trait { for_ty, trait_ref } => env::TypePredicate::Trait {
            for_ty: substitute_ty_params(for_ty, subst),
            trait_ref: substitute_trait_ref(trait_ref, subst),
        },
        env::TypePredicate::Equality { lhs, rhs } => env::TypePredicate::Equality {
            lhs: substitute_ty_params(lhs, subst),
            rhs: substitute_ty_params(rhs, subst),
        },
    }
}

pub(crate) fn instantiate_self_ty(ty: &tast::Ty, self_ty: &tast::Ty) -> tast::Ty {
    rewrite_ty(ty, &mut |ty| match ty {
        tast::Ty::TStruct { name } if name == "Self" => Some(self_ty.clone()),
        _ => None,
    })
}

pub(crate) fn rename_type_params(ty: &tast::Ty, prefix: &str) -> tast::Ty {
    rewrite_ty(ty, &mut |ty| match ty {
        tast::Ty::TParam { name } => Some(tast::Ty::TParam {
            name: format!("{prefix}::{name}"),
        }),
        _ => None,
    })
}

pub(crate) fn rename_trait_params(trait_ref: &tast::TraitRef, prefix: &str) -> tast::TraitRef {
    tast::TraitRef {
        name: trait_ref.name.clone(),
        args: trait_ref
            .args
            .iter()
            .map(|arg| rename_type_params(arg, prefix))
            .collect(),
    }
}

pub(crate) fn rename_predicate_params(
    predicate: &env::TypePredicate,
    prefix: &str,
) -> env::TypePredicate {
    match predicate {
        env::TypePredicate::Trait { for_ty, trait_ref } => env::TypePredicate::Trait {
            for_ty: rename_type_params(for_ty, prefix),
            trait_ref: rename_trait_params(trait_ref, prefix),
        },
        env::TypePredicate::Equality { lhs, rhs } => env::TypePredicate::Equality {
            lhs: rename_type_params(lhs, prefix),
            rhs: rename_type_params(rhs, prefix),
        },
    }
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
        tast::Ty::TProjection {
            trait_ref, for_ty, ..
        } => {
            visit_ty(for_ty, visitor)?;
            if let Some(trait_ref) = trait_ref {
                for arg in &trait_ref.args {
                    visit_ty(arg, visitor)?;
                }
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

pub(crate) fn type_vars(ty: &tast::Ty) -> HashSet<tast::TypeVar> {
    let mut variables = HashSet::new();
    let _ = visit_ty(ty, &mut |ty| {
        if let tast::Ty::TVar(variable) = ty {
            variables.insert(*variable);
        }
        ControlFlow::<()>::Continue(())
    });
    variables
}

pub(crate) fn trait_ref_type_vars(trait_ref: &tast::TraitRef) -> HashSet<tast::TypeVar> {
    trait_ref.args.iter().flat_map(type_vars).collect()
}

pub(crate) fn contains_tparam(ty: &tast::Ty) -> bool {
    visit_ty(ty, &mut |ty| match ty {
        tast::Ty::TParam { .. } => ControlFlow::Break(()),
        _ => ControlFlow::Continue(()),
    })
    .is_break()
}

pub(crate) fn trait_ref_contains_tparam(trait_ref: &tast::TraitRef) -> bool {
    trait_ref.args.iter().any(contains_tparam)
}

pub(crate) fn type_params(ty: &tast::Ty) -> HashSet<String> {
    let mut parameters = HashSet::new();
    let _ = visit_ty(ty, &mut |ty| {
        if let tast::Ty::TParam { name } = ty {
            parameters.insert(name.clone());
        }
        ControlFlow::<()>::Continue(())
    });
    parameters
}

pub(crate) fn injective_type_params(ty: &tast::Ty) -> HashSet<String> {
    fn collect(ty: &tast::Ty, parameters: &mut HashSet<String>) {
        match ty {
            tast::Ty::TParam { name } => {
                parameters.insert(name.clone());
            }
            tast::Ty::TTuple { typs } => {
                for ty in typs {
                    collect(ty, parameters);
                }
            }
            tast::Ty::TApp { ty, args } => {
                collect(ty, parameters);
                for arg in args {
                    collect(arg, parameters);
                }
            }
            tast::Ty::TArray { elem, .. }
            | tast::Ty::TSlice { elem }
            | tast::Ty::TVec { elem }
            | tast::Ty::TRef { elem } => collect(elem, parameters),
            tast::Ty::THashMap { key, value } => {
                collect(key, parameters);
                collect(value, parameters);
            }
            tast::Ty::TFunc { params, ret_ty } => {
                for param in params {
                    collect(param, parameters);
                }
                collect(ret_ty, parameters);
            }
            tast::Ty::TProjection { .. }
            | tast::Ty::TVar(_)
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
            | tast::Ty::TChar
            | tast::Ty::TEnum { .. }
            | tast::Ty::TStruct { .. }
            | tast::Ty::TDyn { .. } => {}
        }
    }

    let mut parameters = HashSet::new();
    collect(ty, &mut parameters);
    parameters
}

pub(crate) fn same_or_unresolved_ty(lhs: &tast::Ty, rhs: &tast::Ty) -> bool {
    lhs == rhs || contains_tvar(lhs) || contains_tvar(rhs)
}

pub(crate) fn fn_ret_depends_on_params(ty: &tast::Ty) -> bool {
    let tast::Ty::TFunc { params, ret_ty } = ty else {
        return false;
    };
    let param_vars = params.iter().flat_map(type_vars).collect::<HashSet<_>>();
    !param_vars.is_empty() && type_vars(ret_ty).iter().any(|var| param_vars.contains(var))
}
