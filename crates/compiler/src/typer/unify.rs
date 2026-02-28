use std::collections::HashMap;

use crate::{
    env::{Constraint, PackageTypeEnv},
    tast::{self, TastIdent, TypeVar},
    typer::Typer,
};
use diagnostics::{Severity, Stage};
use parser::{Diagnostic, Diagnostics};
use text_size::TextRange;

fn pat_origin(pat: &tast::Pat) -> Option<TextRange> {
    match pat {
        tast::Pat::PVar { astptr, .. }
        | tast::Pat::PPrim { astptr, .. }
        | tast::Pat::PConstr { astptr, .. }
        | tast::Pat::PTuple { astptr, .. }
        | tast::Pat::PWild { astptr, .. } => astptr.as_ref().map(|ptr| ptr.text_range()),
    }
}

fn expr_origin(expr: &tast::Expr) -> Option<TextRange> {
    match expr {
        tast::Expr::EVar { astptr, .. } => astptr.as_ref().map(|ptr| ptr.text_range()),
        tast::Expr::EMatch { astptr, expr, .. } => astptr
            .as_ref()
            .map(|ptr| ptr.text_range())
            .or_else(|| expr_origin(expr.as_ref())),
        tast::Expr::EField { astptr, expr, .. } => astptr
            .as_ref()
            .map(|ptr| ptr.text_range())
            .or_else(|| expr_origin(expr.as_ref())),
        tast::Expr::ETraitMethod { astptr, .. } => astptr.as_ref().map(|ptr| ptr.text_range()),
        tast::Expr::EDynTraitMethod { astptr, .. } => astptr.as_ref().map(|ptr| ptr.text_range()),
        tast::Expr::EInherentMethod { astptr, .. } => astptr.as_ref().map(|ptr| ptr.text_range()),
        tast::Expr::EToDyn { astptr, expr, .. } => astptr
            .as_ref()
            .map(|ptr| ptr.text_range())
            .or_else(|| expr_origin(expr.as_ref())),
        tast::Expr::ECall { func, args, .. } => expr_origin(func)
            .or_else(|| args.first().and_then(expr_origin))
            .or_else(|| args.last().and_then(expr_origin)),
        tast::Expr::EBinary { lhs, rhs, .. } => expr_origin(lhs).or_else(|| expr_origin(rhs)),
        tast::Expr::EUnary { expr, .. } => expr_origin(expr),
        tast::Expr::EBlock { block, .. } => block
            .tail
            .as_ref()
            .and_then(|tail| expr_origin(tail.as_ref()))
            .or_else(|| {
                block.stmts.iter().find_map(|stmt| match stmt {
                    tast::Stmt::Let(stmt) => {
                        pat_origin(&stmt.pat).or_else(|| expr_origin(stmt.value.as_ref()))
                    }
                    tast::Stmt::Expr(stmt) => expr_origin(&stmt.expr),
                })
            }),
        tast::Expr::EIf {
            cond,
            then_branch,
            else_branch,
            ..
        } => expr_origin(cond)
            .or_else(|| expr_origin(then_branch))
            .or_else(|| expr_origin(else_branch)),
        tast::Expr::EWhile { cond, body, .. } => expr_origin(cond).or_else(|| expr_origin(body)),
        tast::Expr::EBreak { .. } | tast::Expr::EContinue { .. } => None,
        tast::Expr::EGo { expr, .. } => expr_origin(expr),
        tast::Expr::EProj { tuple, .. } => expr_origin(tuple),
        tast::Expr::EConstr { args, .. } => args.first().and_then(expr_origin),
        tast::Expr::ETuple { items, .. } => items.first().and_then(expr_origin),
        tast::Expr::EArray { items, .. } => items.first().and_then(expr_origin),
        tast::Expr::EClosure { body, .. } => expr_origin(body),
        tast::Expr::EPrim { .. } => None,
    }
}

fn occurs(
    diagnostics: &mut Diagnostics,
    origin: Option<TextRange>,
    var: TypeVar,
    ty: &tast::Ty,
) -> bool {
    match ty {
        tast::Ty::TVar(v) => {
            if var == *v {
                diagnostics.push(
                    Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Type inference failed: recursive type involving {}",
                            super::util::format_ty_for_diag(ty)
                        ),
                    )
                    .with_range(origin),
                );
                return false;
            }
        }
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
        | tast::Ty::TChar
        | tast::Ty::TParam { .. } => {}
        tast::Ty::TTuple { typs } => {
            for ty in typs.iter() {
                if !occurs(diagnostics, origin, var, ty) {
                    return false;
                }
            }
        }
        tast::Ty::TEnum { .. } | tast::Ty::TStruct { .. } => {}
        tast::Ty::TDyn { .. } => {}
        tast::Ty::TApp { ty, args } => {
            if !occurs(diagnostics, origin, var, ty.as_ref()) {
                return false;
            }
            for arg in args.iter() {
                if !occurs(diagnostics, origin, var, arg) {
                    return false;
                }
            }
        }
        tast::Ty::TArray { elem, .. } => {
            if !occurs(diagnostics, origin, var, elem) {
                return false;
            }
        }
        tast::Ty::TSlice { elem } => {
            if !occurs(diagnostics, origin, var, elem) {
                return false;
            }
        }
        tast::Ty::TVec { elem } => {
            if !occurs(diagnostics, origin, var, elem) {
                return false;
            }
        }
        tast::Ty::TRef { elem } => {
            if !occurs(diagnostics, origin, var, elem) {
                return false;
            }
        }
        tast::Ty::THashMap { key, value } => {
            if !occurs(diagnostics, origin, var, key) {
                return false;
            }
            if !occurs(diagnostics, origin, var, value) {
                return false;
            }
        }
        tast::Ty::TFunc { params, ret_ty } => {
            for param in params.iter() {
                if !occurs(diagnostics, origin, var, param) {
                    return false;
                }
            }
            if !occurs(diagnostics, origin, var, ret_ty) {
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
        | tast::Ty::TChar => ty.clone(),
        tast::Ty::TTuple { typs } => tast::Ty::TTuple {
            typs: typs
                .iter()
                .map(|t| substitute_ty_params(t, subst))
                .collect(),
        },
        tast::Ty::TEnum { name } => tast::Ty::TEnum { name: name.clone() },
        tast::Ty::TStruct { name } => tast::Ty::TStruct { name: name.clone() },
        tast::Ty::TDyn { trait_name } => tast::Ty::TDyn {
            trait_name: trait_name.clone(),
        },
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
        tast::Ty::TSlice { elem } => tast::Ty::TSlice {
            elem: Box::new(substitute_ty_params(elem, subst)),
        },
        tast::Ty::TVec { elem } => tast::Ty::TVec {
            elem: Box::new(substitute_ty_params(elem, subst)),
        },
        tast::Ty::TRef { elem } => tast::Ty::TRef {
            elem: Box::new(substitute_ty_params(elem, subst)),
        },
        tast::Ty::THashMap { key, value } => tast::Ty::THashMap {
            key: Box::new(substitute_ty_params(key, subst)),
            value: Box::new(substitute_ty_params(value, subst)),
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
    diagnostics: &mut Diagnostics,
    struct_def: &crate::env::StructDef,
    type_args: &[tast::Ty],
    field: &TastIdent,
) -> Option<tast::Ty> {
    const COMPLETION_PLACEHOLDER: &str = "completion_placeholder";
    if struct_def.generics.len() != type_args.len() {
        super::util::push_error(
            diagnostics,
            format!(
                "Struct {} expects {} type arguments, but got {}",
                struct_def.name.0,
                struct_def.generics.len(),
                type_args.len()
            ),
        );
        return None;
    }

    let mut subst = HashMap::new();
    for (param, arg) in struct_def.generics.iter().zip(type_args.iter()) {
        subst.insert(param.0.clone(), arg.clone());
    }

    if let Some((_, ty)) = struct_def.fields.iter().find(|(fname, _)| fname == field) {
        Some(substitute_ty_params(ty, &subst))
    } else if field.0 == COMPLETION_PLACEHOLDER {
        Some(tast::Ty::TUnit)
    } else {
        super::util::push_error(
            diagnostics,
            format!("Struct {} has no field {}", struct_def.name.0, field.0),
        );
        None
    }
}

fn decompose_struct_type(ty: &tast::Ty) -> Option<(TastIdent, Vec<tast::Ty>)> {
    match ty {
        tast::Ty::TStruct { name } => Some((TastIdent::new(name), Vec::new())),
        tast::Ty::TApp { ty: base, args } => {
            let (type_name, mut collected) = decompose_struct_type(base)?;
            collected.extend(args.iter().cloned());
            Some((type_name, collected))
        }
        _ => None,
    }
}

impl Typer {
    fn constraint_origin(constraint: &Constraint) -> Option<TextRange> {
        match constraint {
            Constraint::TypeEqual(_, _, origin) => *origin,
            Constraint::Overloaded { origin, .. } => *origin,
            Constraint::Implements { origin, .. } => *origin,
            Constraint::StructFieldAccess { origin, .. } => *origin,
        }
    }

    fn first_constraint_origin(constraints: &[Constraint]) -> Option<TextRange> {
        constraints.iter().find_map(Self::constraint_origin)
    }

    fn ty_mentions_var(ty: &tast::Ty, var: TypeVar) -> bool {
        match ty {
            tast::Ty::TVar(v) => *v == var,
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
            | tast::Ty::TChar
            | tast::Ty::TEnum { .. }
            | tast::Ty::TStruct { .. }
            | tast::Ty::TDyn { .. }
            | tast::Ty::TParam { .. } => false,
            tast::Ty::TTuple { typs } => typs.iter().any(|t| Self::ty_mentions_var(t, var)),
            tast::Ty::TApp { ty, args } => {
                Self::ty_mentions_var(ty, var) || args.iter().any(|t| Self::ty_mentions_var(t, var))
            }
            tast::Ty::TArray { elem, .. } => Self::ty_mentions_var(elem, var),
            tast::Ty::TSlice { elem } => Self::ty_mentions_var(elem, var),
            tast::Ty::TVec { elem } => Self::ty_mentions_var(elem, var),
            tast::Ty::TRef { elem } => Self::ty_mentions_var(elem, var),
            tast::Ty::THashMap { key, value } => {
                Self::ty_mentions_var(key, var) || Self::ty_mentions_var(value, var)
            }
            tast::Ty::TFunc { params, ret_ty } => {
                params.iter().any(|t| Self::ty_mentions_var(t, var))
                    || Self::ty_mentions_var(ret_ty, var)
            }
        }
    }

    fn origin_for_unresolved_type_var(&self, var: TypeVar) -> Option<TextRange> {
        self.constraints
            .iter()
            .find_map(|constraint| match constraint {
                Constraint::TypeEqual(l, r, origin) => {
                    if Self::ty_mentions_var(l, var) || Self::ty_mentions_var(r, var) {
                        *origin
                    } else {
                        None
                    }
                }
                Constraint::Overloaded {
                    call_site_type,
                    origin,
                    ..
                } => {
                    if Self::ty_mentions_var(call_site_type, var) {
                        *origin
                    } else {
                        None
                    }
                }
                Constraint::Implements { for_ty, origin, .. } => {
                    if Self::ty_mentions_var(for_ty, var) {
                        *origin
                    } else {
                        None
                    }
                }
                Constraint::StructFieldAccess {
                    expr_ty,
                    result_ty,
                    origin,
                    ..
                } => {
                    if Self::ty_mentions_var(expr_ty, var) || Self::ty_mentions_var(result_ty, var)
                    {
                        *origin
                    } else {
                        None
                    }
                }
            })
    }

    pub fn solve(&mut self, genv: &PackageTypeEnv, diagnostics: &mut Diagnostics) {
        let mut constraints = std::mem::take(&mut self.constraints);
        self.reported_unresolved_type_vars.clear();
        let mut changed = true;

        fn is_concrete(norm_ty: &tast::Ty) -> bool {
            match norm_ty {
                tast::Ty::TVar(..) => false,
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
                | tast::Ty::TChar
                | tast::Ty::TParam { .. } => true, // TParam is treated as concrete here
                tast::Ty::TTuple { typs } => typs.iter().all(is_concrete),
                tast::Ty::TEnum { .. } | tast::Ty::TStruct { .. } | tast::Ty::TDyn { .. } => true,
                tast::Ty::TApp { ty, args } => {
                    is_concrete(ty.as_ref()) && args.iter().all(is_concrete)
                }
                tast::Ty::TArray { elem, .. } => is_concrete(elem),
                tast::Ty::TSlice { elem } => is_concrete(elem),
                tast::Ty::TVec { elem } => is_concrete(elem),
                tast::Ty::TRef { elem } => is_concrete(elem),
                tast::Ty::THashMap { key, value } => is_concrete(key) && is_concrete(value),
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
                    Constraint::TypeEqual(l, r, origin) => {
                        let l_norm = self.norm(&l);
                        let r_norm = self.norm(&r);
                        let dyn_coercion_ok = match (&l_norm, &r_norm) {
                            (tast::Ty::TDyn { trait_name }, concrete)
                            | (concrete, tast::Ty::TDyn { trait_name })
                                if !matches!(concrete, tast::Ty::TVar(_) | tast::Ty::TDyn { .. }) =>
                            {
                                genv.has_trait_impl_visible(trait_name, concrete)
                            }
                            _ => false,
                        };
                        if dyn_coercion_ok || self.unify(diagnostics, &l, &r, origin) {
                            changed = true;
                        }
                    }
                    Constraint::Overloaded {
                        op,
                        trait_name,
                        call_site_type,
                        origin,
                    } => {
                        let norm_call_site_type = self.norm(&call_site_type);
                        if let tast::Ty::TFunc {
                            params: norm_arg_types,
                            ret_ty: norm_ret_ty,
                        } = norm_call_site_type
                        {
                            if let Some(self_ty) = norm_arg_types.first() {
                                match self_ty {
                                    tast::Ty::TParam { name }
                                        if self
                                            .tparam_trait_bounds
                                            .get(name)
                                            .is_some_and(|bounds| {
                                                let (resolved, _) =
                                                    super::util::normalize_trait_name(
                                                        genv,
                                                        &trait_name.0,
                                                    );
                                                bounds.contains(&resolved)
                                            }) =>
                                    {
                                        let (resolved, env) =
                                            super::util::normalize_trait_name(genv, &trait_name.0);
                                        let trait_ident = TastIdent(resolved);
                                        if let Some(method_scheme) =
                                            env.lookup_trait_method_scheme(&trait_ident, &op)
                                        {
                                            let method_ty = self.inst_ty(&method_scheme.ty);
                                            let method_ty = super::check::instantiate_self_ty(
                                                &method_ty,
                                                self_ty,
                                            );
                                            let call_fun_ty = tast::Ty::TFunc {
                                                params: norm_arg_types.clone(),
                                                ret_ty: norm_ret_ty.clone(),
                                            };
                                            still_pending.push(Constraint::TypeEqual(
                                                call_fun_ty,
                                                method_ty,
                                                origin,
                                            ));
                                            changed = true;
                                        } else {
                                            diagnostics.push(
                                                Diagnostic::new(
                                                    Stage::Typer,
                                                    Severity::Error,
                                                    format!(
                                                        "Method {} not found in trait {}",
                                                        op.0, trait_name.0
                                                    ),
                                                )
                                                .with_range(origin),
                                            );
                                        }
                                    }
                                    tast::Ty::TDyn {
                                        trait_name: dyn_trait,
                                    } if {
                                        let (resolved, _) =
                                            super::util::normalize_trait_name(genv, &trait_name.0);
                                        *dyn_trait == resolved
                                    } =>
                                    {
                                        let (resolved, env) =
                                            super::util::normalize_trait_name(genv, &trait_name.0);
                                        let trait_ident = TastIdent(resolved);
                                        if let Some(method_scheme) =
                                            env.lookup_trait_method_scheme(&trait_ident, &op)
                                        {
                                            let method_ty = self.inst_ty(&method_scheme.ty);
                                            let method_ty = super::check::instantiate_self_ty(
                                                &method_ty,
                                                self_ty,
                                            );
                                            let call_fun_ty = tast::Ty::TFunc {
                                                params: norm_arg_types.clone(),
                                                ret_ty: norm_ret_ty.clone(),
                                            };
                                            still_pending.push(Constraint::TypeEqual(
                                                call_fun_ty,
                                                method_ty,
                                                origin,
                                            ));
                                            changed = true;
                                        } else {
                                            diagnostics.push(
                                                Diagnostic::new(
                                                    Stage::Typer,
                                                    Severity::Error,
                                                    format!(
                                                        "Method {} not found in trait {}",
                                                        op.0, trait_name.0
                                                    ),
                                                )
                                                .with_range(origin),
                                            );
                                        }
                                    }
                                    ty if is_concrete(ty) => {
                                        let (resolved, _env) =
                                            super::util::normalize_trait_name(genv, &trait_name.0);
                                        let trait_ident = TastIdent(resolved);
                                        let impls = genv.collect_visible_trait_impl_schemes(
                                            &trait_ident,
                                            self_ty,
                                            &op,
                                        );
                                        match impls.as_slice() {
                                            [impl_scheme] => {
                                                let impl_fun_ty = self.inst_ty(&impl_scheme.ty);

                                                let call_fun_ty = tast::Ty::TFunc {
                                                    params: norm_arg_types.clone(),
                                                    ret_ty: norm_ret_ty.clone(),
                                                };

                                                still_pending.push(Constraint::TypeEqual(
                                                    call_fun_ty,
                                                    impl_fun_ty,
                                                    origin,
                                                ));

                                                changed = true;
                                            }
                                            [] => {
                                                diagnostics.push(Diagnostic::new(
                                                    Stage::Typer,
                                                    Severity::Error,
                                                    format!(
                                                        "No instance found for trait {}<{}> for operator {}",
                                                        trait_name.0,
                                                        super::util::format_ty_for_diag(ty),
                                                        op.0
                                                    ),
                                                ).with_range(origin))
                                            }
                                            _ => {
                                                diagnostics.push(Diagnostic::new(
                                                    Stage::Typer,
                                                    Severity::Error,
                                                    format!(
                                                        "Multiple instances found for trait {}<{}> for operator {}",
                                                        trait_name.0,
                                                        super::util::format_ty_for_diag(ty),
                                                        op.0
                                                    ),
                                                ).with_range(origin))
                                            }
                                        }
                                    }
                                    tast::Ty::TVar(_) => {
                                        // We cannot resolve this yet. Defer it.
                                        still_pending.push(Constraint::Overloaded {
                                            op,
                                            trait_name,
                                            call_site_type, // Push original back
                                            origin,
                                        });
                                    }
                                    _ => {
                                        diagnostics.push(Diagnostic::new(
                                            Stage::Typer,
                                            Severity::Error,
                                            format!(
                                                "Overload resolution failed for non-concrete type {}",
                                                super::util::format_ty_for_diag(self_ty)
                                            ),
                                        ).with_range(origin));
                                    }
                                }
                            } else {
                                diagnostics.push(
                                    Diagnostic::new(
                                        Stage::Typer,
                                        Severity::Error,
                                        format!(
                                            "Overloaded operator {} called with no arguments?",
                                            op.0
                                        ),
                                    )
                                    .with_range(origin),
                                );
                            }
                        } else {
                            diagnostics.push(Diagnostic::new(
                                Stage::Typer,
                                Severity::Error,
                                format!(
                                    "Overloaded constraint does not involve a function type: {}",
                                    super::util::format_ty_for_diag(&norm_call_site_type)
                                ),
                            ).with_range(origin));
                        }
                    }
                    Constraint::Implements {
                        trait_name,
                        for_ty,
                        origin,
                    } => {
                        let norm_for_ty = self.norm(&for_ty);
                        let dyn_satisfied = matches!(
                            &norm_for_ty,
                            tast::Ty::TDyn {
                                trait_name: dyn_trait_name
                            } if dyn_trait_name == &trait_name.0
                        );
                        let impl_found = genv.has_trait_impl_visible(&trait_name.0, &norm_for_ty);
                        let tparam_satisfied = matches!(
                            &norm_for_ty,
                            tast::Ty::TParam { name }
                            if self.tparam_trait_bounds
                                .get(name)
                                .is_some_and(|bounds| bounds.contains(&trait_name.0))
                        );
                        if dyn_satisfied || impl_found || tparam_satisfied {
                            changed = true;
                        } else if matches!(norm_for_ty, tast::Ty::TVar(_))
                            || !is_concrete(&norm_for_ty)
                        {
                            still_pending.push(Constraint::Implements {
                                trait_name,
                                for_ty: norm_for_ty,
                                origin,
                            });
                        } else {
                            diagnostics.push(
                                Diagnostic::new(
                                    Stage::Typer,
                                    Severity::Error,
                                    format!(
                                        "No instance found for trait {}<{}>",
                                        trait_name.0,
                                        super::util::format_ty_for_diag(&norm_for_ty)
                                    ),
                                )
                                .with_range(origin),
                            );
                        }
                    }
                    Constraint::StructFieldAccess {
                        expr_ty,
                        field,
                        result_ty,
                        origin,
                    } => {
                        let norm_expr_ty = self.norm(&expr_ty);
                        if let Some((type_name, type_args)) = decompose_struct_type(&norm_expr_ty) {
                            let (resolved, env) =
                                super::util::resolve_type_name(genv, &type_name.0);
                            let struct_def = env.structs().get(&TastIdent(resolved));
                            let Some(struct_def) = struct_def else {
                                super::util::push_error_with_range(
                                    diagnostics,
                                    format!(
                                        "Struct {} not found when accessing field {}",
                                        type_name.0, field.0
                                    ),
                                    origin,
                                );
                                continue;
                            };
                            if let Some(field_ty) = instantiate_struct_field_ty(
                                diagnostics,
                                struct_def,
                                &type_args,
                                &field,
                            ) && self.unify(diagnostics, &result_ty, &field_ty, origin)
                            {
                                changed = true;
                            }
                        } else {
                            still_pending.push(Constraint::StructFieldAccess {
                                expr_ty: norm_expr_ty,
                                field,
                                result_ty,
                                origin,
                            });
                        }
                    }
                }
            }
            constraints.extend(still_pending);

            let deferred = std::mem::take(&mut self.deferred_dyn_coercions);
            let mut still_deferred = Vec::new();
            for coercion in deferred {
                let expected_norm = self.norm(&coercion.expected_ty);
                match &expected_norm {
                    tast::Ty::TVar(_) => {
                        still_deferred.push(coercion);
                    }
                    tast::Ty::TDyn { trait_name } => {
                        let concrete_norm = self.norm(&coercion.concrete_ty);
                        if matches!(concrete_norm, tast::Ty::TDyn { .. }) {
                            changed = true;
                        } else if genv.has_trait_impl_visible(trait_name, &concrete_norm) {
                            self.results.push_coercion(
                                coercion.expr_id,
                                crate::typer::results::Coercion::ToDyn {
                                    trait_name: tast::TastIdent(trait_name.clone()),
                                    for_ty: concrete_norm,
                                    ty: expected_norm,
                                    astptr: None,
                                },
                            );
                            changed = true;
                        } else {
                            constraints.push(Constraint::TypeEqual(
                                coercion.concrete_ty,
                                coercion.expected_ty,
                                None,
                            ));
                        }
                    }
                    _ => {
                        constraints.push(Constraint::TypeEqual(
                            coercion.concrete_ty,
                            coercion.expected_ty,
                            None,
                        ));
                        changed = true;
                    }
                }
            }
            self.deferred_dyn_coercions = still_deferred;

            if !changed && !self.deferred_dyn_coercions.is_empty() {
                let remaining = std::mem::take(&mut self.deferred_dyn_coercions);
                for coercion in remaining {
                    constraints.push(Constraint::TypeEqual(
                        coercion.concrete_ty,
                        coercion.expected_ty,
                        None,
                    ));
                }
                changed = true;
                continue;
            }

            if !changed && !constraints.is_empty() {
                diagnostics.push(
                    Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        "Could not solve all type constraints".to_string(),
                    )
                    .with_range(Self::first_constraint_origin(&constraints)),
                );
                break;
            }
        }
        if !constraints.is_empty() {
            diagnostics.push(
                Diagnostic::new(
                    Stage::Typer,
                    Severity::Error,
                    "Type inference failed due to unresolved constraints".to_string(),
                )
                .with_range(Self::first_constraint_origin(&constraints)),
            );
        }
        self.constraints = constraints;
    }

    pub(crate) fn norm(&mut self, ty: &tast::Ty) -> tast::Ty {
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
            tast::Ty::TInt8 => tast::Ty::TInt8,
            tast::Ty::TInt16 => tast::Ty::TInt16,
            tast::Ty::TInt32 => tast::Ty::TInt32,
            tast::Ty::TInt64 => tast::Ty::TInt64,
            tast::Ty::TUint8 => tast::Ty::TUint8,
            tast::Ty::TUint16 => tast::Ty::TUint16,
            tast::Ty::TUint32 => tast::Ty::TUint32,
            tast::Ty::TUint64 => tast::Ty::TUint64,
            tast::Ty::TFloat32 => tast::Ty::TFloat32,
            tast::Ty::TFloat64 => tast::Ty::TFloat64,
            tast::Ty::TString => tast::Ty::TString,
            tast::Ty::TChar => tast::Ty::TChar,
            tast::Ty::TTuple { typs } => {
                let typs = typs.iter().map(|ty| self.norm(ty)).collect();
                tast::Ty::TTuple { typs }
            }
            tast::Ty::TEnum { name } => tast::Ty::TEnum { name: name.clone() },
            tast::Ty::TStruct { name } => tast::Ty::TStruct { name: name.clone() },
            tast::Ty::TDyn { trait_name } => tast::Ty::TDyn {
                trait_name: trait_name.clone(),
            },
            tast::Ty::TApp { ty, args } => tast::Ty::TApp {
                ty: Box::new(self.norm(ty)),
                args: args.iter().map(|ty| self.norm(ty)).collect(),
            },
            tast::Ty::TArray { len, elem } => tast::Ty::TArray {
                len: *len,
                elem: Box::new(self.norm(elem)),
            },
            tast::Ty::TSlice { elem } => tast::Ty::TSlice {
                elem: Box::new(self.norm(elem)),
            },
            tast::Ty::TVec { elem } => tast::Ty::TVec {
                elem: Box::new(self.norm(elem)),
            },
            tast::Ty::TRef { elem } => tast::Ty::TRef {
                elem: Box::new(self.norm(elem)),
            },
            tast::Ty::THashMap { key, value } => tast::Ty::THashMap {
                key: Box::new(self.norm(key)),
                value: Box::new(self.norm(value)),
            },
            tast::Ty::TFunc { params, ret_ty } => {
                let params = params.iter().map(|ty| self.norm(ty)).collect();
                let ret_ty = Box::new(self.norm(ret_ty));
                tast::Ty::TFunc { params, ret_ty }
            }
            tast::Ty::TParam { name } => tast::Ty::TParam { name: name.clone() },
        }
    }

    fn unify(
        &mut self,
        diagnostics: &mut Diagnostics,
        l: &tast::Ty,
        r: &tast::Ty,
        origin: Option<TextRange>,
    ) -> bool {
        let l_norm = self.norm(l);
        let r_norm = self.norm(r);
        match (&l_norm, &r_norm) {
            (tast::Ty::TVar(a), tast::Ty::TVar(b)) => {
                if self.uni.unify_var_var(*a, *b).is_err() {
                    diagnostics.push(
                        Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            "Type inference failed while unifying unknown types".to_string(),
                        )
                        .with_range(origin),
                    );

                    return false;
                }
            }
            (tast::Ty::TVar(a), t) | (t, tast::Ty::TVar(a)) => {
                if !occurs(diagnostics, origin, *a, t) {
                    return false;
                }
                if self.uni.unify_var_value(*a, Some(t.clone())).is_err() {
                    diagnostics.push(
                        Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            format!(
                                "Type inference failed while unifying unknown type with {}",
                                super::util::format_ty_for_diag(t)
                            ),
                        )
                        .with_range(origin),
                    );
                    return false;
                }
            }

            (tast::Ty::TUnit, tast::Ty::TUnit) => {}
            (tast::Ty::TBool, tast::Ty::TBool) => {}
            (tast::Ty::TInt32, tast::Ty::TInt32) => {}
            (tast::Ty::TInt8, tast::Ty::TInt8) => {}
            (tast::Ty::TInt16, tast::Ty::TInt16) => {}
            (tast::Ty::TInt64, tast::Ty::TInt64) => {}
            (tast::Ty::TUint8, tast::Ty::TUint8) => {}
            (tast::Ty::TUint16, tast::Ty::TUint16) => {}
            (tast::Ty::TUint32, tast::Ty::TUint32) => {}
            (tast::Ty::TUint64, tast::Ty::TUint64) => {}
            (tast::Ty::TFloat32, tast::Ty::TFloat32) => {}
            (tast::Ty::TFloat64, tast::Ty::TFloat64) => {}
            (tast::Ty::TString, tast::Ty::TString) => {}
            (tast::Ty::TChar, tast::Ty::TChar) => {}
            (tast::Ty::TTuple { typs: typs1 }, tast::Ty::TTuple { typs: typs2 }) => {
                if typs1.len() != typs2.len() {
                    diagnostics.push(
                        Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            format!(
                                "Tuple length mismatch: expected {}, found {}",
                                super::util::format_ty_for_diag(&l_norm),
                                super::util::format_ty_for_diag(&r_norm)
                            ),
                        )
                        .with_range(origin),
                    );
                    return false;
                }
                for (ty1, ty2) in typs1.iter().zip(typs2.iter()) {
                    if !self.unify(diagnostics, ty1, ty2, origin) {
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
                    diagnostics.push(
                        Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            format!(
                                "Array length mismatch: expected {}, found {}",
                                super::util::format_ty_for_diag(&l_norm),
                                super::util::format_ty_for_diag(&r_norm)
                            ),
                        )
                        .with_range(origin),
                    );
                    return false;
                }
                if !self.unify(diagnostics, elem1, elem2, origin) {
                    return false;
                }
            }
            (tast::Ty::TRef { elem: elem1 }, tast::Ty::TRef { elem: elem2 }) => {
                if !self.unify(diagnostics, elem1, elem2, origin) {
                    return false;
                }
            }
            (tast::Ty::TSlice { elem: elem1 }, tast::Ty::TSlice { elem: elem2 }) => {
                if !self.unify(diagnostics, elem1, elem2, origin) {
                    return false;
                }
            }
            (tast::Ty::TVec { elem: elem1 }, tast::Ty::TVec { elem: elem2 }) => {
                if !self.unify(diagnostics, elem1, elem2, origin) {
                    return false;
                }
            }
            (
                tast::Ty::THashMap {
                    key: key1,
                    value: value1,
                },
                tast::Ty::THashMap {
                    key: key2,
                    value: value2,
                },
            ) => {
                if !self.unify(diagnostics, key1, key2, origin) {
                    return false;
                }
                if !self.unify(diagnostics, value1, value2, origin) {
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
                    diagnostics.push(
                        Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            format!(
                                "Function arity mismatch: expected {}, found {}",
                                super::util::format_ty_for_diag(&l_norm),
                                super::util::format_ty_for_diag(&r_norm)
                            ),
                        )
                        .with_range(origin),
                    );
                    return false;
                }
                for (p1, p2) in param1.iter().zip(param2.iter()) {
                    if !self.unify(diagnostics, p1, p2, origin) {
                        return false;
                    }
                }
                if !self.unify(diagnostics, ret_ty1, ret_ty2, origin) {
                    return false;
                }
            }
            (tast::Ty::TEnum { name: n1 }, tast::Ty::TEnum { name: n2 })
            | (tast::Ty::TStruct { name: n1 }, tast::Ty::TStruct { name: n2 }) => {
                if n1 != n2 {
                    diagnostics.push(Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Type mismatch: expected {}, found {}",
                            super::util::format_ty_for_diag(&l_norm),
                            super::util::format_ty_for_diag(&r_norm)
                        ),
                    ));
                    return false;
                }
            }
            (tast::Ty::TDyn { trait_name: t1 }, tast::Ty::TDyn { trait_name: t2 }) => {
                if t1 != t2 {
                    diagnostics.push(Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Type mismatch: expected {}, found {}",
                            super::util::format_ty_for_diag(&l_norm),
                            super::util::format_ty_for_diag(&r_norm)
                        ),
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
                    diagnostics.push(
                        Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            format!(
                                "Type argument arity mismatch: expected {}, found {}",
                                super::util::format_ty_for_diag(&l_norm),
                                super::util::format_ty_for_diag(&r_norm)
                            ),
                        )
                        .with_range(origin),
                    );
                    return false;
                }
                if !self.unify(diagnostics, ty1.as_ref(), ty2.as_ref(), origin) {
                    return false;
                }
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    if !self.unify(diagnostics, arg1, arg2, origin) {
                        return false;
                    }
                }
            }
            (tast::Ty::TParam { name }, tast::Ty::TParam { name: name2 }) => {
                if name != name2 {
                    diagnostics.push(
                        Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            format!(
                                "Type mismatch: expected {}, found {}",
                                super::util::format_ty_for_diag(&l_norm),
                                super::util::format_ty_for_diag(&r_norm)
                            ),
                        )
                        .with_range(origin),
                    );
                    return false;
                }
            }
            (tast::Ty::TParam { name }, ty) | (ty, tast::Ty::TParam { name }) => {
                diagnostics.push(
                    Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Type mismatch: expected {}, found {}",
                            name,
                            super::util::format_ty_for_diag(ty)
                        ),
                    )
                    .with_range(origin),
                );
                return false;
            }
            _ => {
                diagnostics.push(
                    Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Type mismatch: expected {}, found {}",
                            super::util::format_ty_for_diag(&l_norm),
                            super::util::format_ty_for_diag(&r_norm)
                        ),
                    )
                    .with_range(origin),
                );
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
            tast::Ty::TInt8 => ty.clone(),
            tast::Ty::TInt16 => ty.clone(),
            tast::Ty::TInt32 => ty.clone(),
            tast::Ty::TInt64 => ty.clone(),
            tast::Ty::TUint8 => ty.clone(),
            tast::Ty::TUint16 => ty.clone(),
            tast::Ty::TUint32 => ty.clone(),
            tast::Ty::TUint64 => ty.clone(),
            tast::Ty::TFloat32 => ty.clone(),
            tast::Ty::TFloat64 => ty.clone(),
            tast::Ty::TString => ty.clone(),
            tast::Ty::TChar => ty.clone(),
            tast::Ty::TTuple { typs } => {
                let typs = typs
                    .iter()
                    .map(|ty| self._go_inst_ty(ty, subst))
                    .collect::<Vec<_>>();
                tast::Ty::TTuple { typs }
            }
            tast::Ty::TEnum { name } => tast::Ty::TEnum { name: name.clone() },
            tast::Ty::TStruct { name } => tast::Ty::TStruct { name: name.clone() },
            tast::Ty::TDyn { trait_name } => tast::Ty::TDyn {
                trait_name: trait_name.clone(),
            },
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
            tast::Ty::TSlice { elem } => tast::Ty::TSlice {
                elem: Box::new(self._go_inst_ty(elem, subst)),
            },
            tast::Ty::TVec { elem } => tast::Ty::TVec {
                elem: Box::new(self._go_inst_ty(elem, subst)),
            },
            tast::Ty::TRef { elem } => tast::Ty::TRef {
                elem: Box::new(self._go_inst_ty(elem, subst)),
            },
            tast::Ty::THashMap { key, value } => tast::Ty::THashMap {
                key: Box::new(self._go_inst_ty(key, subst)),
                value: Box::new(self._go_inst_ty(value, subst)),
            },
            tast::Ty::TParam { name } => {
                if let Some(ty) = subst.get(name) {
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

    pub(crate) fn subst_ty(
        &mut self,
        diagnostics: &mut Diagnostics,
        ty: &tast::Ty,
        origin: Option<TextRange>,
    ) -> tast::Ty {
        match ty {
            tast::Ty::TVar(v) => {
                if let Some(value) = self.uni.probe_value(*v) {
                    self.subst_ty(diagnostics, &value, origin)
                } else {
                    if self.reported_unresolved_type_vars.insert(*v) {
                        diagnostics.push(
                            Diagnostic::new(
                                Stage::Typer,
                                Severity::Error,
                                "Could not infer type".to_string(),
                            )
                            .with_range(
                                origin
                                    .or_else(|| self.origin_for_unresolved_type_var(*v))
                                    .or_else(|| {
                                        diagnostics.iter().filter_map(|d| d.range()).last()
                                    }),
                            ),
                        );
                    }
                    tast::Ty::TVar(*v)
                }
            }
            tast::Ty::TUnit => tast::Ty::TUnit,
            tast::Ty::TBool => tast::Ty::TBool,
            tast::Ty::TInt8 => tast::Ty::TInt8,
            tast::Ty::TInt16 => tast::Ty::TInt16,
            tast::Ty::TInt32 => tast::Ty::TInt32,
            tast::Ty::TInt64 => tast::Ty::TInt64,
            tast::Ty::TUint8 => tast::Ty::TUint8,
            tast::Ty::TUint16 => tast::Ty::TUint16,
            tast::Ty::TUint32 => tast::Ty::TUint32,
            tast::Ty::TUint64 => tast::Ty::TUint64,
            tast::Ty::TFloat32 => tast::Ty::TFloat32,
            tast::Ty::TFloat64 => tast::Ty::TFloat64,
            tast::Ty::TString => tast::Ty::TString,
            tast::Ty::TChar => tast::Ty::TChar,
            tast::Ty::TTuple { typs } => {
                let typs = typs
                    .iter()
                    .map(|ty| self.subst_ty(diagnostics, ty, origin))
                    .collect();
                tast::Ty::TTuple { typs }
            }
            tast::Ty::TEnum { name } => tast::Ty::TEnum { name: name.clone() },
            tast::Ty::TStruct { name } => tast::Ty::TStruct { name: name.clone() },
            tast::Ty::TDyn { trait_name } => tast::Ty::TDyn {
                trait_name: trait_name.clone(),
            },
            tast::Ty::TApp { ty, args } => tast::Ty::TApp {
                ty: Box::new(self.subst_ty(diagnostics, ty, origin)),
                args: args
                    .iter()
                    .map(|arg| self.subst_ty(diagnostics, arg, origin))
                    .collect(),
            },
            tast::Ty::TArray { len, elem } => tast::Ty::TArray {
                len: *len,
                elem: Box::new(self.subst_ty(diagnostics, elem, origin)),
            },
            tast::Ty::TSlice { elem } => tast::Ty::TSlice {
                elem: Box::new(self.subst_ty(diagnostics, elem, origin)),
            },
            tast::Ty::TVec { elem } => tast::Ty::TVec {
                elem: Box::new(self.subst_ty(diagnostics, elem, origin)),
            },
            tast::Ty::TRef { elem } => tast::Ty::TRef {
                elem: Box::new(self.subst_ty(diagnostics, elem, origin)),
            },
            tast::Ty::THashMap { key, value } => tast::Ty::THashMap {
                key: Box::new(self.subst_ty(diagnostics, key, origin)),
                value: Box::new(self.subst_ty(diagnostics, value, origin)),
            },
            tast::Ty::TFunc { params, ret_ty } => {
                let params = params
                    .iter()
                    .map(|ty| self.subst_ty(diagnostics, ty, origin))
                    .collect();
                let ret_ty = Box::new(self.subst_ty(diagnostics, ret_ty, origin));
                tast::Ty::TFunc { params, ret_ty }
            }
            tast::Ty::TParam { name } => tast::Ty::TParam { name: name.clone() },
        }
    }

    pub(crate) fn subst_ty_silent(&mut self, ty: &tast::Ty) -> tast::Ty {
        match ty {
            tast::Ty::TVar(v) => self
                .uni
                .probe_value(*v)
                .as_ref()
                .map(|value| self.subst_ty_silent(value))
                .unwrap_or(tast::Ty::TVar(*v)),
            tast::Ty::TUnit => tast::Ty::TUnit,
            tast::Ty::TBool => tast::Ty::TBool,
            tast::Ty::TInt8 => tast::Ty::TInt8,
            tast::Ty::TInt16 => tast::Ty::TInt16,
            tast::Ty::TInt32 => tast::Ty::TInt32,
            tast::Ty::TInt64 => tast::Ty::TInt64,
            tast::Ty::TUint8 => tast::Ty::TUint8,
            tast::Ty::TUint16 => tast::Ty::TUint16,
            tast::Ty::TUint32 => tast::Ty::TUint32,
            tast::Ty::TUint64 => tast::Ty::TUint64,
            tast::Ty::TFloat32 => tast::Ty::TFloat32,
            tast::Ty::TFloat64 => tast::Ty::TFloat64,
            tast::Ty::TString => tast::Ty::TString,
            tast::Ty::TChar => tast::Ty::TChar,
            tast::Ty::TTuple { typs } => tast::Ty::TTuple {
                typs: typs.iter().map(|ty| self.subst_ty_silent(ty)).collect(),
            },
            tast::Ty::TEnum { name } => tast::Ty::TEnum { name: name.clone() },
            tast::Ty::TStruct { name } => tast::Ty::TStruct { name: name.clone() },
            tast::Ty::TDyn { trait_name } => tast::Ty::TDyn {
                trait_name: trait_name.clone(),
            },
            tast::Ty::TApp { ty, args } => tast::Ty::TApp {
                ty: Box::new(self.subst_ty_silent(ty)),
                args: args.iter().map(|arg| self.subst_ty_silent(arg)).collect(),
            },
            tast::Ty::TArray { len, elem } => tast::Ty::TArray {
                len: *len,
                elem: Box::new(self.subst_ty_silent(elem)),
            },
            tast::Ty::TSlice { elem } => tast::Ty::TSlice {
                elem: Box::new(self.subst_ty_silent(elem)),
            },
            tast::Ty::TVec { elem } => tast::Ty::TVec {
                elem: Box::new(self.subst_ty_silent(elem)),
            },
            tast::Ty::TRef { elem } => tast::Ty::TRef {
                elem: Box::new(self.subst_ty_silent(elem)),
            },
            tast::Ty::THashMap { key, value } => tast::Ty::THashMap {
                key: Box::new(self.subst_ty_silent(key)),
                value: Box::new(self.subst_ty_silent(value)),
            },
            tast::Ty::TFunc { params, ret_ty } => tast::Ty::TFunc {
                params: params.iter().map(|ty| self.subst_ty_silent(ty)).collect(),
                ret_ty: Box::new(self.subst_ty_silent(ret_ty)),
            },
            tast::Ty::TParam { name } => tast::Ty::TParam { name: name.clone() },
        }
    }

    fn subst_pat(&mut self, diagnostics: &mut Diagnostics, p: tast::Pat) -> tast::Pat {
        match p {
            tast::Pat::PVar { name, ty, astptr } => {
                let origin = astptr.as_ref().map(|ptr| ptr.text_range());
                let ty = self.subst_ty(diagnostics, &ty, origin);
                tast::Pat::PVar {
                    name: name.clone(),
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Pat::PPrim { value, ty, astptr } => {
                let origin = astptr.as_ref().map(|ptr| ptr.text_range());
                let ty = self.subst_ty(diagnostics, &ty, origin);
                tast::Pat::PPrim {
                    value,
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Pat::PConstr {
                constructor,
                args,
                ty,
                astptr,
            } => {
                let origin = astptr
                    .as_ref()
                    .map(|ptr| ptr.text_range())
                    .or_else(|| args.first().and_then(pat_origin));
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let args = args
                    .into_iter()
                    .map(|arg| self.subst_pat(diagnostics, arg))
                    .collect::<Vec<_>>();
                tast::Pat::PConstr {
                    constructor,
                    args,
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Pat::PTuple { items, ty, astptr } => {
                let origin = astptr
                    .as_ref()
                    .map(|ptr| ptr.text_range())
                    .or_else(|| items.first().and_then(pat_origin));
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let items = items
                    .into_iter()
                    .map(|item| self.subst_pat(diagnostics, item))
                    .collect::<Vec<_>>();
                tast::Pat::PTuple {
                    items,
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Pat::PWild { ty, astptr } => {
                let origin = astptr.as_ref().map(|ptr| ptr.text_range());
                let ty = self.subst_ty(diagnostics, &ty, origin);
                tast::Pat::PWild {
                    ty: ty.clone(),
                    astptr,
                }
            }
        }
    }

    pub fn subst_block(
        &mut self,
        diagnostics: &mut Diagnostics,
        block: tast::Block,
    ) -> tast::Block {
        let stmts = block
            .stmts
            .into_iter()
            .map(|stmt| match stmt {
                tast::Stmt::Let(stmt) => tast::Stmt::Let(tast::LetStmt {
                    pat: self.subst_pat(diagnostics, stmt.pat),
                    value: Box::new(self.subst(diagnostics, *stmt.value)),
                }),
                tast::Stmt::Expr(stmt) => tast::Stmt::Expr(tast::ExprStmt {
                    expr: self.subst(diagnostics, stmt.expr),
                }),
            })
            .collect();
        let tail = block
            .tail
            .map(|tail| Box::new(self.subst(diagnostics, *tail)));
        tast::Block { stmts, tail }
    }

    pub fn subst(&mut self, diagnostics: &mut Diagnostics, e: tast::Expr) -> tast::Expr {
        match e {
            tast::Expr::EVar { name, ty, astptr } => {
                let origin = astptr.as_ref().map(|ptr| ptr.text_range());
                let ty = self.subst_ty(diagnostics, &ty, origin);
                tast::Expr::EVar {
                    name,
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Expr::EPrim { value, ty } => {
                let ty = self.subst_ty(diagnostics, &ty, None);
                tast::Expr::EPrim { value, ty }
            }
            tast::Expr::EConstr {
                constructor,
                args,
                ty,
            } => {
                let origin = args.first().and_then(expr_origin);
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let args = args
                    .into_iter()
                    .map(|arg| self.subst(diagnostics, arg))
                    .collect::<Vec<_>>();
                tast::Expr::EConstr {
                    constructor,
                    args,
                    ty,
                }
            }
            tast::Expr::ETuple { items, ty } => {
                let origin = items.first().and_then(expr_origin);
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let items = items
                    .into_iter()
                    .map(|item| self.subst(diagnostics, item))
                    .collect::<Vec<_>>();
                tast::Expr::ETuple {
                    items,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EArray { items, ty } => {
                let origin = items.first().and_then(expr_origin);
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let items = items
                    .into_iter()
                    .map(|item| self.subst(diagnostics, item))
                    .collect::<Vec<_>>();
                tast::Expr::EArray {
                    items,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EClosure {
                params,
                body,
                ty,
                captures,
            } => {
                let outer_origin = expr_origin(body.as_ref());
                let ty = self.subst_ty(diagnostics, &ty, outer_origin);
                let params = params
                    .into_iter()
                    .map(|param| tast::ClosureParam {
                        name: param.name,
                        ty: self.subst_ty(
                            diagnostics,
                            &param.ty,
                            param
                                .astptr
                                .as_ref()
                                .map(|ptr| ptr.text_range())
                                .or(outer_origin),
                        ),
                        astptr: param.astptr,
                    })
                    .collect();
                let body = Box::new(self.subst(diagnostics, *body));
                let captures = captures
                    .into_iter()
                    .map(|(name, cap_ty)| (name, self.subst_ty(diagnostics, &cap_ty, outer_origin)))
                    .collect();
                tast::Expr::EClosure {
                    params,
                    body,
                    ty: ty.clone(),
                    captures,
                }
            }
            tast::Expr::EBlock { block, ty } => {
                let origin = block
                    .tail
                    .as_ref()
                    .and_then(|tail| expr_origin(tail.as_ref()));
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let block = Box::new(self.subst_block(diagnostics, *block));
                tast::Expr::EBlock {
                    block,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EMatch {
                expr,
                arms,
                ty,
                astptr,
            } => {
                let origin = astptr
                    .as_ref()
                    .map(|ptr| ptr.text_range())
                    .or_else(|| expr_origin(expr.as_ref()));
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let expr = Box::new(self.subst(diagnostics, *expr));
                let arms = arms
                    .into_iter()
                    .map(|arm| tast::Arm {
                        pat: self.subst_pat(diagnostics, arm.pat),
                        body: self.subst(diagnostics, arm.body),
                    })
                    .collect::<Vec<_>>();
                tast::Expr::EMatch {
                    expr,
                    arms,
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Expr::EIf {
                cond,
                then_branch,
                else_branch,
                ty,
            } => {
                let origin = expr_origin(cond.as_ref())
                    .or_else(|| expr_origin(then_branch.as_ref()))
                    .or_else(|| expr_origin(else_branch.as_ref()));
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let cond = Box::new(self.subst(diagnostics, *cond));
                let then_branch = Box::new(self.subst(diagnostics, *then_branch));
                let else_branch = Box::new(self.subst(diagnostics, *else_branch));
                tast::Expr::EIf {
                    cond,
                    then_branch,
                    else_branch,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EWhile { cond, body, ty } => {
                let origin = expr_origin(cond.as_ref()).or_else(|| expr_origin(body.as_ref()));
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let cond = Box::new(self.subst(diagnostics, *cond));
                let body = Box::new(self.subst(diagnostics, *body));
                tast::Expr::EWhile {
                    cond,
                    body,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EBreak { ty } => {
                let ty = self.subst_ty(diagnostics, &ty, None);
                tast::Expr::EBreak { ty }
            }
            tast::Expr::EContinue { ty } => {
                let ty = self.subst_ty(diagnostics, &ty, None);
                tast::Expr::EContinue { ty }
            }
            tast::Expr::EGo { expr, ty } => {
                let origin = expr_origin(expr.as_ref());
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let expr = Box::new(self.subst(diagnostics, *expr));
                tast::Expr::EGo {
                    expr,
                    ty: ty.clone(),
                }
            }
            tast::Expr::ECall { func, args, ty } => {
                let origin = expr_origin(func.as_ref())
                    .or_else(|| args.first().and_then(expr_origin))
                    .or_else(|| args.last().and_then(expr_origin));
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let func = Box::new(self.subst(diagnostics, *func));
                let args = args
                    .into_iter()
                    .map(|arg| self.subst(diagnostics, arg))
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
                let origin = expr_origin(expr.as_ref());
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let expr = Box::new(self.subst(diagnostics, *expr));
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
                let origin = expr_origin(lhs.as_ref()).or_else(|| expr_origin(rhs.as_ref()));
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let lhs = Box::new(self.subst(diagnostics, *lhs));
                let rhs = Box::new(self.subst(diagnostics, *rhs));
                tast::Expr::EBinary {
                    op,
                    lhs,
                    rhs,
                    ty: ty.clone(),
                    resolution,
                }
            }
            tast::Expr::EProj { tuple, index, ty } => {
                let origin = expr_origin(tuple.as_ref());
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let tuple = Box::new(self.subst(diagnostics, *tuple));
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
                astptr,
            } => {
                let origin = astptr
                    .as_ref()
                    .map(|ptr| ptr.text_range())
                    .or_else(|| expr_origin(expr.as_ref()));
                let ty = self.subst_ty(diagnostics, &ty, origin);
                let expr = Box::new(self.subst(diagnostics, *expr));
                tast::Expr::EField {
                    expr,
                    field_name,
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Expr::ETraitMethod {
                trait_name,
                method_name,
                ty,
                astptr,
            } => {
                let origin = astptr.as_ref().map(|ptr| ptr.text_range());
                let ty = self.subst_ty(diagnostics, &ty, origin);
                tast::Expr::ETraitMethod {
                    trait_name,
                    method_name,
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Expr::EDynTraitMethod {
                trait_name,
                method_name,
                ty,
                astptr,
            } => {
                let origin = astptr.as_ref().map(|ptr| ptr.text_range());
                let ty = self.subst_ty(diagnostics, &ty, origin);
                tast::Expr::EDynTraitMethod {
                    trait_name,
                    method_name,
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Expr::EInherentMethod {
                receiver_ty,
                method_name,
                ty,
                astptr,
            } => {
                let origin = astptr.as_ref().map(|ptr| ptr.text_range());
                let receiver_ty = self.subst_ty(diagnostics, &receiver_ty, origin);
                let ty = self.subst_ty(diagnostics, &ty, origin);
                tast::Expr::EInherentMethod {
                    receiver_ty,
                    method_name,
                    ty,
                    astptr,
                }
            }
            tast::Expr::EToDyn {
                trait_name,
                for_ty,
                expr,
                ty,
                astptr,
            } => {
                let origin = astptr
                    .as_ref()
                    .map(|ptr| ptr.text_range())
                    .or_else(|| expr_origin(expr.as_ref()));
                let for_ty = self.subst_ty(diagnostics, &for_ty, origin);
                let expr = Box::new(self.subst(diagnostics, *expr));
                let ty = self.subst_ty(diagnostics, &ty, origin);
                if let (
                    tast::Ty::TDyn {
                        trait_name: from_trait_name,
                    },
                    tast::Ty::TDyn {
                        trait_name: to_trait_name,
                    },
                ) = (&for_ty, &ty)
                    && from_trait_name == to_trait_name
                    && to_trait_name == &trait_name.0
                {
                    return *expr;
                }
                tast::Expr::EToDyn {
                    trait_name,
                    for_ty,
                    expr,
                    ty,
                    astptr,
                }
            }
        }
    }
}
