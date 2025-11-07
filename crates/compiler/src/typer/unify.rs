use std::collections::HashMap;

use crate::{
    env::{Constraint, Env},
    tast::{self, TypeVar},
    typer::TypeInference,
};
use ast::ast::{Lident, Uident};

fn occurs(env: &mut Env, var: TypeVar, ty: &tast::Ty) -> bool {
    match ty {
        tast::Ty::TVar(v) => {
            if var == *v {
                env.report_typer_error(format!(
                    "occurs check failed: {:?} occurs in {:?}",
                    var, ty
                ));
                return false;
            }
        }
        tast::Ty::TUnit
        | tast::Ty::TBool
        | tast::Ty::TInt
        | tast::Ty::TInt8
        | tast::Ty::TInt16
        | tast::Ty::TInt32
        | tast::Ty::TInt64
        | tast::Ty::TString
        | tast::Ty::TParam { .. } => {}
        tast::Ty::TTuple { typs } => {
            for ty in typs.iter() {
                if !occurs(env, var, ty) {
                    return false;
                }
            }
        }
        tast::Ty::TCon { .. } => {}
        tast::Ty::TApp { ty, args } => {
            if !occurs(env, var, ty.as_ref()) {
                return false;
            }
            for arg in args.iter() {
                if !occurs(env, var, arg) {
                    return false;
                }
            }
        }
        tast::Ty::TArray { elem, .. } => {
            if !occurs(env, var, elem) {
                return false;
            }
        }
        tast::Ty::TRef { elem } => {
            if !occurs(env, var, elem) {
                return false;
            }
        }
        tast::Ty::TFunc { params, ret_ty } => {
            for param in params.iter() {
                if !occurs(env, var, param) {
                    return false;
                }
            }
            if !occurs(env, var, ret_ty) {
                return false;
            }
        }
    }

    true
}

fn substitute_ty_params(ty: &tast::Ty, subst: &HashMap<String, tast::Ty>) -> tast::Ty {
    match ty {
        tast::Ty::TVar(_)
        | tast::Ty::TUnit
        | tast::Ty::TBool
        | tast::Ty::TInt
        | tast::Ty::TInt8
        | tast::Ty::TInt16
        | tast::Ty::TInt32
        | tast::Ty::TInt64
        | tast::Ty::TString => ty.clone(),
        tast::Ty::TTuple { typs } => tast::Ty::TTuple {
            typs: typs
                .iter()
                .map(|t| substitute_ty_params(t, subst))
                .collect(),
        },
        tast::Ty::TCon { name } => tast::Ty::TCon { name: name.clone() },
        tast::Ty::TApp { ty, args } => tast::Ty::TApp {
            ty: Box::new(substitute_ty_params(ty, subst)),
            args: args
                .iter()
                .map(|t| substitute_ty_params(t, subst))
                .collect(),
        },
        tast::Ty::TParam { name } => subst
            .get(name)
            .cloned()
            .unwrap_or_else(|| tast::Ty::TParam { name: name.clone() }),
        tast::Ty::TArray { len, elem } => tast::Ty::TArray {
            len: *len,
            elem: Box::new(substitute_ty_params(elem, subst)),
        },
        tast::Ty::TRef { elem } => tast::Ty::TRef {
            elem: Box::new(substitute_ty_params(elem, subst)),
        },
        tast::Ty::TFunc { params, ret_ty } => tast::Ty::TFunc {
            params: params
                .iter()
                .map(|t| substitute_ty_params(t, subst))
                .collect(),
            ret_ty: Box::new(substitute_ty_params(ret_ty, subst)),
        },
    }
}

fn instantiate_struct_field_ty(
    struct_def: &crate::env::StructDef,
    type_args: &[tast::Ty],
    field: &Lident,
) -> tast::Ty {
    if struct_def.generics.len() != type_args.len() {
        panic!(
            "Struct {} expects {} type arguments, but got {}",
            struct_def.name.0,
            struct_def.generics.len(),
            type_args.len()
        );
    }

    let mut subst = HashMap::new();
    for (param, arg) in struct_def.generics.iter().zip(type_args.iter()) {
        subst.insert(param.0.clone(), arg.clone());
    }

    struct_def
        .fields
        .iter()
        .find(|(fname, _)| fname == field)
        .map(|(_, ty)| substitute_ty_params(ty, &subst))
        .unwrap_or_else(|| panic!("Struct {} has no field {}", struct_def.name.0, field.0))
}

fn decompose_struct_type(ty: &tast::Ty) -> Option<(Uident, Vec<tast::Ty>)> {
    match ty {
        tast::Ty::TCon { name } => Some((Uident::new(name), Vec::new())),
        tast::Ty::TApp { ty: base, args } => {
            let (type_name, mut collected) = decompose_struct_type(base)?;
            collected.extend(args.iter().cloned());
            Some((type_name, collected))
        }
        _ => None,
    }
}

impl TypeInference {
    pub fn solve(&mut self, env: &mut Env) {
        let mut constraints = env.constraints.clone();
        let mut changed = true;

        fn is_concrete(norm_ty: &tast::Ty) -> bool {
            match norm_ty {
                tast::Ty::TVar(..) => false,
                tast::Ty::TUnit
                | tast::Ty::TBool
                | tast::Ty::TInt
                | tast::Ty::TInt8
                | tast::Ty::TInt16
                | tast::Ty::TInt32
                | tast::Ty::TInt64
                | tast::Ty::TString
                | tast::Ty::TParam { .. } => true, // TParam is treated as concrete here
                tast::Ty::TTuple { typs } => typs.iter().all(is_concrete),
                tast::Ty::TCon { .. } => true,
                tast::Ty::TApp { ty, args } => {
                    is_concrete(ty.as_ref()) && args.iter().all(is_concrete)
                }
                tast::Ty::TArray { elem, .. } => is_concrete(elem),
                tast::Ty::TRef { elem } => is_concrete(elem),
                tast::Ty::TFunc { params, ret_ty } => {
                    params.iter().all(is_concrete) && is_concrete(ret_ty)
                }
            }
        }

        while changed {
            changed = false;
            let mut still_pending = Vec::new();
            for constraint in constraints.drain(..) {
                match constraint {
                    Constraint::TypeEqual(l, r) => {
                        if self.unify(env, &l, &r) {
                            changed = true;
                        }
                    }
                    Constraint::Overloaded {
                        op,
                        trait_name,
                        call_site_type,
                    } => {
                        let norm_call_site_type = self.norm(&call_site_type);
                        if let tast::Ty::TFunc {
                            params: norm_arg_types,
                            ret_ty: norm_ret_ty,
                        } = norm_call_site_type
                        {
                            if let Some(self_ty) = norm_arg_types.first() {
                                match self_ty {
                                    ty if is_concrete(ty) => {
                                        match env.get_trait_impl(&trait_name, self_ty, &op) {
                                            Some(impl_scheme) => {
                                                let impl_fun_ty = self.inst_ty(&impl_scheme);

                                                let call_fun_ty = tast::Ty::TFunc {
                                                    params: norm_arg_types.clone(),
                                                    ret_ty: norm_ret_ty.clone(),
                                                };

                                                still_pending.push(Constraint::TypeEqual(
                                                    call_fun_ty,
                                                    impl_fun_ty,
                                                ));

                                                // Made progress!
                                                changed = true;
                                            }
                                            None => {
                                                env.report_typer_error(format!(
                                                    "No instance found for trait {}<{:?}> for operator {}",
                                                    trait_name.0, ty, op.0
                                                ));
                                            }
                                        }
                                    }
                                    tast::Ty::TVar(_) => {
                                        // We cannot resolve this yet. Defer it.
                                        still_pending.push(Constraint::Overloaded {
                                            op,
                                            trait_name,
                                            call_site_type, // Push original back
                                        });
                                    }
                                    _ => {
                                        env.report_typer_error(format!(
                                            "Overload resolution failed for non-concrete, non-variable type {:?}",
                                            self_ty
                                        ));
                                    }
                                }
                            } else {
                                env.report_typer_error(format!(
                                    "Overloaded operator {} called with no arguments?",
                                    op.0
                                ));
                            }
                        } else {
                            env.report_typer_error(format!(
                                "Overloaded constraint does not involve a function type: {:?}",
                                norm_call_site_type
                            ));
                        }
                    }
                    Constraint::StructFieldAccess {
                        expr_ty,
                        field,
                        result_ty,
                    } => {
                        let norm_expr_ty = self.norm(&expr_ty);
                        if let Some((type_name, type_args)) = decompose_struct_type(&norm_expr_ty) {
                            let struct_def = env.structs.get(&type_name).unwrap_or_else(|| {
                                panic!(
                                    "Struct {} not found when accessing field {}",
                                    type_name.0, field.0
                                )
                            });
                            let field_ty =
                                instantiate_struct_field_ty(struct_def, &type_args, &field);
                            if self.unify(env, &result_ty, &field_ty) {
                                changed = true;
                            }
                        } else {
                            still_pending.push(Constraint::StructFieldAccess {
                                expr_ty: norm_expr_ty,
                                field,
                                result_ty,
                            });
                        }
                    }
                }
            }
            constraints.extend(still_pending);

            if !changed && !constraints.is_empty() {
                env.report_typer_error(format!(
                    "Could not solve all constraints: {:?}",
                    constraints
                ));
                break;
            }
        }
        if !constraints.is_empty() {
            env.report_typer_error(format!(
                "Type inference failed, remaining constraints: {:?}",
                constraints
            ));
        }
    }

    fn norm(&mut self, ty: &tast::Ty) -> tast::Ty {
        match ty {
            tast::Ty::TVar(v) => {
                if let Some(value) = self.uni.probe_value(*v) {
                    self.norm(&value)
                } else {
                    tast::Ty::TVar(self.uni.find(*v))
                }
            }
            tast::Ty::TUnit => tast::Ty::TUnit,
            tast::Ty::TBool => tast::Ty::TBool,
            tast::Ty::TInt => tast::Ty::TInt,
            tast::Ty::TInt8 => tast::Ty::TInt8,
            tast::Ty::TInt16 => tast::Ty::TInt16,
            tast::Ty::TInt32 => tast::Ty::TInt32,
            tast::Ty::TInt64 => tast::Ty::TInt64,
            tast::Ty::TString => tast::Ty::TString,
            tast::Ty::TTuple { typs } => {
                let typs = typs.iter().map(|ty| self.norm(ty)).collect();
                tast::Ty::TTuple { typs }
            }
            tast::Ty::TCon { name } => tast::Ty::TCon { name: name.clone() },
            tast::Ty::TApp { ty, args } => tast::Ty::TApp {
                ty: Box::new(self.norm(ty)),
                args: args.iter().map(|ty| self.norm(ty)).collect(),
            },
            tast::Ty::TArray { len, elem } => tast::Ty::TArray {
                len: *len,
                elem: Box::new(self.norm(elem)),
            },
            tast::Ty::TRef { elem } => tast::Ty::TRef {
                elem: Box::new(self.norm(elem)),
            },
            tast::Ty::TFunc { params, ret_ty } => {
                let params = params.iter().map(|ty| self.norm(ty)).collect();
                let ret_ty = Box::new(self.norm(ret_ty));
                tast::Ty::TFunc { params, ret_ty }
            }
            tast::Ty::TParam { name } => tast::Ty::TParam { name: name.clone() },
        }
    }

    fn unify(&mut self, env: &mut Env, l: &tast::Ty, r: &tast::Ty) -> bool {
        let l_norm = self.norm(l);
        let r_norm = self.norm(r);
        match (&l_norm, &r_norm) {
            (tast::Ty::TVar(a), tast::Ty::TVar(b)) => {
                if self.uni.unify_var_var(*a, *b).is_err() {
                    env.report_typer_error(format!(
                        "Failed to unify type variables {:?} and {:?}",
                        a, b
                    ));
                    return false;
                }
            }
            (tast::Ty::TVar(a), t) | (t, tast::Ty::TVar(a)) => {
                if !occurs(env, *a, t) {
                    return false;
                }
                if self.uni.unify_var_value(*a, Some(t.clone())).is_err() {
                    env.report_typer_error(format!(
                        "Failed to unify type variable {:?} with {:?}",
                        a, t
                    ));
                    return false;
                }
            }

            (tast::Ty::TUnit, tast::Ty::TUnit) => {}
            (tast::Ty::TBool, tast::Ty::TBool) => {}
            (tast::Ty::TInt, tast::Ty::TInt) => {}
            (tast::Ty::TInt8, tast::Ty::TInt8) => {}
            (tast::Ty::TInt16, tast::Ty::TInt16) => {}
            (tast::Ty::TInt32, tast::Ty::TInt32) => {}
            (tast::Ty::TInt64, tast::Ty::TInt64) => {}
            (tast::Ty::TString, tast::Ty::TString) => {}
            (tast::Ty::TTuple { typs: typs1 }, tast::Ty::TTuple { typs: typs2 }) => {
                if typs1.len() != typs2.len() {
                    env.report_typer_error(format!(
                        "Tuple types have different lengths: {:?} and {:?}",
                        l, r
                    ));
                    return false;
                }
                for (ty1, ty2) in typs1.iter().zip(typs2.iter()) {
                    if !self.unify(env, ty1, ty2) {
                        return false;
                    }
                }
            }
            (
                tast::Ty::TArray {
                    len: len1,
                    elem: elem1,
                },
                tast::Ty::TArray {
                    len: len2,
                    elem: elem2,
                },
            ) => {
                let wildcard = tast::ARRAY_WILDCARD_LEN;
                if len1 != len2 && *len1 != wildcard && *len2 != wildcard {
                    env.report_typer_error(format!(
                        "Array types have different lengths: {:?} and {:?}",
                        l, r
                    ));
                    return false;
                }
                if !self.unify(env, elem1, elem2) {
                    return false;
                }
            }
            (tast::Ty::TRef { elem: elem1 }, tast::Ty::TRef { elem: elem2 }) => {
                if !self.unify(env, elem1, elem2) {
                    return false;
                }
            }
            (
                tast::Ty::TFunc {
                    params: param1,
                    ret_ty: ret_ty1,
                },
                tast::Ty::TFunc {
                    params: param2,
                    ret_ty: ret_ty2,
                },
            ) => {
                if param1.len() != param2.len() {
                    env.report_typer_error(format!(
                        "Function types have different parameter lengths: {:?} and {:?}",
                        l, r
                    ));
                    return false;
                }
                for (p1, p2) in param1.iter().zip(param2.iter()) {
                    if !self.unify(env, p1, p2) {
                        return false;
                    }
                }
                if !self.unify(env, ret_ty1, ret_ty2) {
                    return false;
                }
            }
            (tast::Ty::TCon { name: n1 }, tast::Ty::TCon { name: n2 }) => {
                if n1 != n2 {
                    env.report_typer_error(format!(
                        "Constructor types are different: {:?} and {:?}",
                        l, r
                    ));
                    return false;
                }
            }
            (
                tast::Ty::TApp {
                    ty: ty1,
                    args: args1,
                },
                tast::Ty::TApp {
                    ty: ty2,
                    args: args2,
                },
            ) => {
                if args1.len() != args2.len() {
                    env.report_typer_error(format!(
                        "Constructor types have different argument lengths: {:?} and {:?}",
                        l, r
                    ));
                    return false;
                }
                if !self.unify(env, ty1.as_ref(), ty2.as_ref()) {
                    return false;
                }
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    if !self.unify(env, arg1, arg2) {
                        return false;
                    }
                }
            }
            (tast::Ty::TParam { name }, tast::Ty::TParam { name: name2 }) => {
                if name != name2 {
                    env.report_typer_error(format!(
                        "Type parameters are different: {:?} and {:?}",
                        l, r
                    ));
                    return false;
                }
            }
            _ => {
                env.report_typer_error(format!("type not equal {:?} and {:?}", l_norm, r_norm));
                return false;
            }
        }
        true
    }

    pub(crate) fn fresh_ty_var(&mut self) -> tast::Ty {
        tast::Ty::TVar(self.uni.new_key(None))
    }

    pub(crate) fn inst_ty(&mut self, ty: &tast::Ty) -> tast::Ty {
        let mut subst: HashMap<String, tast::Ty> = HashMap::new();
        self._go_inst_ty(ty, &mut subst)
    }

    fn _go_inst_ty(&mut self, ty: &tast::Ty, subst: &mut HashMap<String, tast::Ty>) -> tast::Ty {
        match ty {
            tast::Ty::TVar(_) => ty.clone(),
            tast::Ty::TUnit => ty.clone(),
            tast::Ty::TBool => ty.clone(),
            tast::Ty::TInt => ty.clone(),
            tast::Ty::TInt8 => ty.clone(),
            tast::Ty::TInt16 => ty.clone(),
            tast::Ty::TInt32 => ty.clone(),
            tast::Ty::TInt64 => ty.clone(),
            tast::Ty::TString => ty.clone(),
            tast::Ty::TTuple { typs } => {
                let typs = typs
                    .iter()
                    .map(|ty| self._go_inst_ty(ty, subst))
                    .collect::<Vec<_>>();
                tast::Ty::TTuple { typs }
            }
            tast::Ty::TCon { name } => tast::Ty::TCon { name: name.clone() },
            tast::Ty::TApp { ty, args } => {
                let ty = self._go_inst_ty(ty, subst);
                let args = args
                    .iter()
                    .map(|arg| self._go_inst_ty(arg, subst))
                    .collect::<Vec<_>>();
                tast::Ty::TApp {
                    ty: Box::new(ty),
                    args,
                }
            }
            tast::Ty::TArray { len, elem } => tast::Ty::TArray {
                len: *len,
                elem: Box::new(self._go_inst_ty(elem, subst)),
            },
            tast::Ty::TRef { elem } => tast::Ty::TRef {
                elem: Box::new(self._go_inst_ty(elem, subst)),
            },
            tast::Ty::TParam { name } => {
                if subst.contains_key(name) {
                    let ty = subst.get(name).unwrap();
                    ty.clone()
                } else {
                    let new_ty = self.fresh_ty_var();
                    subst.insert(name.clone(), new_ty.clone());
                    new_ty
                }
            }
            tast::Ty::TFunc { params, ret_ty } => {
                let params = params
                    .iter()
                    .map(|ty| self._go_inst_ty(ty, subst))
                    .collect::<Vec<_>>();
                let ret_ty = Box::new(self._go_inst_ty(ret_ty, subst));
                tast::Ty::TFunc { params, ret_ty }
            }
        }
    }

    fn subst_ty(&mut self, env: &mut Env, ty: &tast::Ty) -> tast::Ty {
        match ty {
            tast::Ty::TVar(v) => {
                if let Some(value) = self.uni.probe_value(*v) {
                    self.subst_ty(env, &value)
                } else {
                    env.report_typer_error(format!("Type variable {:?} not resolved", v));
                    tast::Ty::TVar(*v)
                }
            }
            tast::Ty::TUnit => tast::Ty::TUnit,
            tast::Ty::TBool => tast::Ty::TBool,
            tast::Ty::TInt => tast::Ty::TInt,
            tast::Ty::TInt8 => tast::Ty::TInt8,
            tast::Ty::TInt16 => tast::Ty::TInt16,
            tast::Ty::TInt32 => tast::Ty::TInt32,
            tast::Ty::TInt64 => tast::Ty::TInt64,
            tast::Ty::TString => tast::Ty::TString,
            tast::Ty::TTuple { typs } => {
                let typs = typs.iter().map(|ty| self.subst_ty(env, ty)).collect();
                tast::Ty::TTuple { typs }
            }
            tast::Ty::TCon { name } => tast::Ty::TCon { name: name.clone() },
            tast::Ty::TApp { ty, args } => tast::Ty::TApp {
                ty: Box::new(self.subst_ty(env, ty)),
                args: args.iter().map(|arg| self.subst_ty(env, arg)).collect(),
            },
            tast::Ty::TArray { len, elem } => tast::Ty::TArray {
                len: *len,
                elem: Box::new(self.subst_ty(env, elem)),
            },
            tast::Ty::TRef { elem } => tast::Ty::TRef {
                elem: Box::new(self.subst_ty(env, elem)),
            },
            tast::Ty::TFunc { params, ret_ty } => {
                let params = params.iter().map(|ty| self.subst_ty(env, ty)).collect();
                let ret_ty = Box::new(self.subst_ty(env, ret_ty));
                tast::Ty::TFunc { params, ret_ty }
            }
            tast::Ty::TParam { name } => tast::Ty::TParam { name: name.clone() },
        }
    }

    fn subst_pat(&mut self, env: &mut Env, p: tast::Pat) -> tast::Pat {
        match p {
            tast::Pat::PVar { name, ty, astptr } => {
                let ty = self.subst_ty(env, &ty);
                tast::Pat::PVar {
                    name: name.clone(),
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Pat::PUnit { ty } => {
                let ty = self.subst_ty(env, &ty);
                tast::Pat::PUnit { ty: ty.clone() }
            }
            tast::Pat::PBool { value, ty } => {
                let ty = self.subst_ty(env, &ty);
                tast::Pat::PBool {
                    value,
                    ty: ty.clone(),
                }
            }
            tast::Pat::PInt { value, ty } => {
                let ty = self.subst_ty(env, &ty);
                tast::Pat::PInt {
                    value,
                    ty: ty.clone(),
                }
            }
            tast::Pat::PString { value, ty } => {
                let ty = self.subst_ty(env, &ty);
                tast::Pat::PString {
                    value,
                    ty: ty.clone(),
                }
            }
            tast::Pat::PConstr {
                constructor,
                args,
                ty,
            } => {
                let ty = self.subst_ty(env, &ty);
                let args = args
                    .into_iter()
                    .map(|arg| self.subst_pat(env, arg))
                    .collect::<Vec<_>>();
                tast::Pat::PConstr {
                    constructor,
                    args,
                    ty: ty.clone(),
                }
            }
            tast::Pat::PTuple { items, ty } => {
                let ty = self.subst_ty(env, &ty);
                let items = items
                    .into_iter()
                    .map(|item| self.subst_pat(env, item))
                    .collect::<Vec<_>>();
                tast::Pat::PTuple {
                    items,
                    ty: ty.clone(),
                }
            }
            tast::Pat::PWild { ty } => {
                let ty = self.subst_ty(env, &ty);
                tast::Pat::PWild { ty: ty.clone() }
            }
        }
    }

    pub fn subst(&mut self, env: &mut Env, e: tast::Expr) -> tast::Expr {
        match e {
            tast::Expr::EVar { name, ty, astptr } => {
                let ty = self.subst_ty(env, &ty);
                tast::Expr::EVar {
                    name,
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Expr::EUnit { ty } => {
                let ty = self.subst_ty(env, &ty);
                tast::Expr::EUnit { ty: ty.clone() }
            }
            tast::Expr::EBool { value, ty } => {
                let ty = self.subst_ty(env, &ty);
                tast::Expr::EBool {
                    value,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EInt { value, ty } => {
                let ty = self.subst_ty(env, &ty);
                tast::Expr::EInt {
                    value,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EString { value, ty } => {
                let ty = self.subst_ty(env, &ty);
                tast::Expr::EString {
                    value: value.clone(),
                    ty: ty.clone(),
                }
            }
            tast::Expr::EConstr {
                constructor,
                args,
                ty,
            } => {
                let ty = self.subst_ty(env, &ty);
                let args = args
                    .into_iter()
                    .map(|arg| self.subst(env, arg))
                    .collect::<Vec<_>>();
                tast::Expr::EConstr {
                    constructor,
                    args,
                    ty,
                }
            }
            tast::Expr::ETuple { items, ty } => {
                let ty = self.subst_ty(env, &ty);
                let items = items
                    .into_iter()
                    .map(|item| self.subst(env, item))
                    .collect::<Vec<_>>();
                tast::Expr::ETuple {
                    items,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EArray { items, ty } => {
                let ty = self.subst_ty(env, &ty);
                let items = items
                    .into_iter()
                    .map(|item| self.subst(env, item))
                    .collect::<Vec<_>>();
                tast::Expr::EArray {
                    items,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EClosure { params, body, ty } => {
                let ty = self.subst_ty(env, &ty);
                let params = params
                    .into_iter()
                    .map(|param| tast::ClosureParam {
                        name: param.name,
                        ty: self.subst_ty(env, &param.ty),
                        astptr: param.astptr,
                    })
                    .collect();
                let body = Box::new(self.subst(env, *body));
                tast::Expr::EClosure {
                    params,
                    body,
                    ty: ty.clone(),
                }
            }
            tast::Expr::ELet {
                pat,
                value,
                body,
                ty,
            } => {
                let ty = self.subst_ty(env, &ty);
                let pat = self.subst_pat(env, pat);
                let value = Box::new(self.subst(env, *value));
                let body = Box::new(self.subst(env, *body));
                tast::Expr::ELet {
                    pat,
                    value,
                    body,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EMatch { expr, arms, ty } => {
                let ty = self.subst_ty(env, &ty);
                let expr = Box::new(self.subst(env, *expr));
                let arms = arms
                    .into_iter()
                    .map(|arm| tast::Arm {
                        pat: self.subst_pat(env, arm.pat),
                        body: self.subst(env, arm.body),
                    })
                    .collect::<Vec<_>>();
                tast::Expr::EMatch {
                    expr,
                    arms,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EIf {
                cond,
                then_branch,
                else_branch,
                ty,
            } => {
                let ty = self.subst_ty(env, &ty);
                let cond = Box::new(self.subst(env, *cond));
                let then_branch = Box::new(self.subst(env, *then_branch));
                let else_branch = Box::new(self.subst(env, *else_branch));
                tast::Expr::EIf {
                    cond,
                    then_branch,
                    else_branch,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EWhile { cond, body, ty } => {
                let ty = self.subst_ty(env, &ty);
                let cond = Box::new(self.subst(env, *cond));
                let body = Box::new(self.subst(env, *body));
                tast::Expr::EWhile {
                    cond,
                    body,
                    ty: ty.clone(),
                }
            }
            tast::Expr::ECall { func, args, ty } => {
                let ty = self.subst_ty(env, &ty);
                let func = Box::new(self.subst(env, *func));
                let args = args
                    .into_iter()
                    .map(|arg| self.subst(env, arg))
                    .collect::<Vec<_>>();
                tast::Expr::ECall {
                    func,
                    args,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EUnary {
                op,
                expr,
                ty,
                resolution,
            } => {
                let ty = self.subst_ty(env, &ty);
                let expr = Box::new(self.subst(env, *expr));
                tast::Expr::EUnary {
                    op,
                    expr,
                    ty: ty.clone(),
                    resolution,
                }
            }
            tast::Expr::EBinary {
                op,
                lhs,
                rhs,
                ty,
                resolution,
            } => {
                let ty = self.subst_ty(env, &ty);
                let lhs = Box::new(self.subst(env, *lhs));
                let rhs = Box::new(self.subst(env, *rhs));
                tast::Expr::EBinary {
                    op,
                    lhs,
                    rhs,
                    ty: ty.clone(),
                    resolution,
                }
            }
            tast::Expr::EProj { tuple, index, ty } => {
                let ty = self.subst_ty(env, &ty);
                let tuple = Box::new(self.subst(env, *tuple));
                tast::Expr::EProj {
                    tuple,
                    index,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EField {
                expr,
                field_name,
                ty,
            } => {
                let ty = self.subst_ty(env, &ty);
                let expr = Box::new(self.subst(env, *expr));
                tast::Expr::EField {
                    expr,
                    field_name,
                    ty: ty.clone(),
                }
            }
        }
    }
}
