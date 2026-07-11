use std::collections::HashMap;

use crate::{
    env::{ImplDef, PackageTypeEnv},
    tast,
    typer::{
        obligations::{ParamEnv, TraitGoal},
        traits::solver::{SelectionResult, TraitSolver},
        type_ops::{
            contains_tparam, rename_predicate_params, rename_trait_params, rename_type_params,
            substitute_ty_params, trait_ref_contains_tparam,
        },
    },
};

pub(crate) fn impls_overlap(
    env: &PackageTypeEnv,
    left_trait_ref: &tast::TraitRef,
    left_self: &tast::Ty,
    left_def: &ImplDef,
    right_trait_ref: &tast::TraitRef,
    right_self: &tast::Ty,
    right_def: &ImplDef,
) -> bool {
    if left_trait_ref.name != right_trait_ref.name
        || left_trait_ref.args.len() != right_trait_ref.args.len()
    {
        return false;
    }
    let left_trait_ref = rename_trait_params(left_trait_ref, "left");
    let right_trait_ref = rename_trait_params(right_trait_ref, "right");
    let left_self = rename_type_params(left_self, "left");
    let right_self = rename_type_params(right_self, "right");
    let mut subst = HashMap::new();
    if !left_trait_ref
        .args
        .iter()
        .zip(right_trait_ref.args.iter())
        .all(|(left, right)| unify(left, right, &mut subst))
        || !unify(&left_self, &right_self, &mut subst)
    {
        return false;
    }

    let param_env = ParamEnv::default();
    let mut solver = TraitSolver::new(env, &param_env);
    left_def
        .constraints
        .iter()
        .map(|constraint| ("left", constraint))
        .chain(
            right_def
                .constraints
                .iter()
                .map(|constraint| ("right", constraint)),
        )
        .all(
            |(prefix, predicate)| match rename_predicate_params(predicate, prefix) {
                crate::env::TypePredicate::Trait { for_ty, trait_ref } => {
                    let for_ty = resolve(&for_ty, &subst);
                    let trait_ref = tast::TraitRef {
                        name: trait_ref.name,
                        args: trait_ref
                            .args
                            .iter()
                            .map(|arg| resolve(arg, &subst))
                            .collect(),
                    };
                    contains_tparam(&for_ty)
                        || trait_ref_contains_tparam(&trait_ref)
                        || matches!(
                            solver.select_ground(TraitGoal { trait_ref, for_ty }),
                            SelectionResult::Unique(_)
                        )
                }
                crate::env::TypePredicate::Equality { lhs, rhs } => {
                    let lhs = resolve(&lhs, &subst);
                    let rhs = resolve(&rhs, &subst);
                    contains_tparam(&lhs) || contains_tparam(&rhs) || lhs == rhs
                }
            },
        )
}

fn resolve(ty: &tast::Ty, subst: &HashMap<String, tast::Ty>) -> tast::Ty {
    let mut current = ty.clone();
    loop {
        let next = substitute_ty_params(&current, subst);
        if next == current {
            return current;
        }
        current = next;
    }
}

fn contains_param(ty: &tast::Ty, param: &str, subst: &HashMap<String, tast::Ty>) -> bool {
    let ty = resolve(ty, subst);
    match &ty {
        tast::Ty::TParam { name } => name == param,
        tast::Ty::TTuple { typs } => typs.iter().any(|ty| contains_param(ty, param, subst)),
        tast::Ty::TApp { ty, args } => {
            contains_param(ty, param, subst)
                || args.iter().any(|ty| contains_param(ty, param, subst))
        }
        tast::Ty::TProjection {
            trait_ref, for_ty, ..
        } => {
            contains_param(for_ty, param, subst)
                || trait_ref.as_ref().is_some_and(|trait_ref| {
                    trait_ref
                        .args
                        .iter()
                        .any(|ty| contains_param(ty, param, subst))
                })
        }
        tast::Ty::TArray { elem, .. }
        | tast::Ty::TSlice { elem }
        | tast::Ty::TVec { elem }
        | tast::Ty::TRef { elem } => contains_param(elem, param, subst),
        tast::Ty::THashMap { key, value } => {
            contains_param(key, param, subst) || contains_param(value, param, subst)
        }
        tast::Ty::TFunc { params, ret_ty } => {
            params.iter().any(|ty| contains_param(ty, param, subst))
                || contains_param(ret_ty, param, subst)
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
        | tast::Ty::TDyn { .. } => false,
    }
}

fn bind(name: &str, ty: &tast::Ty, subst: &mut HashMap<String, tast::Ty>) -> bool {
    let ty = resolve(ty, subst);
    if matches!(&ty, tast::Ty::TParam { name: other } if other == name) {
        return true;
    }
    if contains_param(&ty, name, subst) {
        return false;
    }
    subst.insert(name.to_string(), ty);
    true
}

pub(crate) fn unify(
    left: &tast::Ty,
    right: &tast::Ty,
    subst: &mut HashMap<String, tast::Ty>,
) -> bool {
    let left = resolve(left, subst);
    let right = resolve(right, subst);
    match (&left, &right) {
        (tast::Ty::TParam { name }, ty) => bind(name, ty, subst),
        (ty, tast::Ty::TParam { name }) => bind(name, ty, subst),
        (tast::Ty::TTuple { typs: left }, tast::Ty::TTuple { typs: right }) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| unify(left, right, subst))
        }
        (
            tast::Ty::TApp {
                ty: left_ty,
                args: left_args,
            },
            tast::Ty::TApp {
                ty: right_ty,
                args: right_args,
            },
        ) => {
            left_args.len() == right_args.len()
                && unify(left_ty, right_ty, subst)
                && left_args
                    .iter()
                    .zip(right_args.iter())
                    .all(|(left, right)| unify(left, right, subst))
        }
        (
            tast::Ty::TProjection {
                trait_ref: left_trait,
                for_ty: left_self,
                name: left_name,
            },
            tast::Ty::TProjection {
                trait_ref: right_trait,
                for_ty: right_self,
                name: right_name,
            },
        ) => {
            left_name == right_name
                && match (left_trait, right_trait) {
                    (Some(left), Some(right)) => {
                        left.name == right.name
                            && left.args.len() == right.args.len()
                            && left
                                .args
                                .iter()
                                .zip(right.args.iter())
                                .all(|(left, right)| unify(left, right, subst))
                    }
                    (None, None) => true,
                    _ => false,
                }
                && unify(left_self, right_self, subst)
        }
        (
            tast::Ty::TArray {
                len: left_len,
                elem: left,
            },
            tast::Ty::TArray {
                len: right_len,
                elem: right,
            },
        ) => left_len == right_len && unify(left, right, subst),
        (tast::Ty::TSlice { elem: left }, tast::Ty::TSlice { elem: right })
        | (tast::Ty::TVec { elem: left }, tast::Ty::TVec { elem: right })
        | (tast::Ty::TRef { elem: left }, tast::Ty::TRef { elem: right }) => {
            unify(left, right, subst)
        }
        (
            tast::Ty::THashMap {
                key: left_key,
                value: left_value,
            },
            tast::Ty::THashMap {
                key: right_key,
                value: right_value,
            },
        ) => unify(left_key, right_key, subst) && unify(left_value, right_value, subst),
        (
            tast::Ty::TFunc {
                params: left_params,
                ret_ty: left_ret,
            },
            tast::Ty::TFunc {
                params: right_params,
                ret_ty: right_ret,
            },
        ) => {
            left_params.len() == right_params.len()
                && left_params
                    .iter()
                    .zip(right_params.iter())
                    .all(|(left, right)| unify(left, right, subst))
                && unify(left_ret, right_ret, subst)
        }
        _ => left == right,
    }
}
