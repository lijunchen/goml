use super::*;
use crate::typer::util;

pub(super) struct StaticMemberCall<'a> {
    pub(super) call_expr_id: hir::ExprId,
    pub(super) func_expr_id: hir::ExprId,
    pub(super) path: &'a hir::Path,
    pub(super) astptr: Option<MySyntaxNodePtr>,
    pub(super) args: &'a [hir::ExprId],
}

struct StaticCallSite<'a> {
    call_expr_id: hir::ExprId,
    func_expr_id: hir::ExprId,
    astptr: Option<MySyntaxNodePtr>,
    args: &'a [hir::ExprId],
}

struct StaticTraitMethodCall<'a> {
    site: StaticCallSite<'a>,
    trait_name: tast::TastIdent,
    method_name: tast::TastIdent,
    method_ty: tast::Ty,
}

struct DynTraitMethodCall<'a> {
    call_expr_id: hir::ExprId,
    func_expr_id: hir::ExprId,
    astptr: Option<MySyntaxNodePtr>,
    args: &'a [hir::ExprId],
    receiver: tast::Expr,
    trait_name: &'a tast::TastIdent,
    method_name: &'a tast::TastIdent,
    params: &'a [tast::Ty],
    ret_ty: &'a tast::Ty,
}

struct StaticInherentMethodCall<'a> {
    site: StaticCallSite<'a>,
    type_name: String,
    member_name: String,
    member_ident: tast::TastIdent,
}

impl Typer {
    pub(super) fn infer_static_member_call_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        call: StaticMemberCall<'_>,
    ) -> tast::Expr {
        let StaticMemberCall {
            call_expr_id,
            func_expr_id,
            path,
            astptr,
            args,
        } = call;
        if path.len() < 2 {
            util::push_ice(
                diagnostics,
                format!(
                    "static member call callee must have at least 2 segments: {}",
                    path.display()
                ),
            );
            return self.error_expr(None);
        }

        let type_name = path
            .namespace_segments()
            .iter()
            .map(|seg| seg.seg().clone())
            .collect::<Vec<_>>()
            .join("::");
        let Some(member_name) = path.last_ident().cloned() else {
            util::push_ice(diagnostics, "callee path missing final segment");
            return self.error_expr(None);
        };
        let member_ident = tast::TastIdent(member_name.clone());
        let site = StaticCallSite {
            call_expr_id,
            func_expr_id,
            astptr,
            args,
        };

        if let Some((trait_name, method_ty)) =
            lookup_trait_method_from_type_name(genv, &type_name, &member_ident)
        {
            self.infer_static_trait_method_call(
                genv,
                local_env,
                diagnostics,
                StaticTraitMethodCall {
                    site,
                    trait_name,
                    method_name: member_ident,
                    method_ty,
                },
            )
        } else {
            self.infer_static_inherent_method_call(
                genv,
                local_env,
                diagnostics,
                StaticInherentMethodCall {
                    site,
                    type_name,
                    member_name,
                    member_ident,
                },
            )
        }
    }

    fn infer_static_trait_method_call(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        call: StaticTraitMethodCall<'_>,
    ) -> tast::Expr {
        let StaticTraitMethodCall {
            site,
            trait_name: type_ident,
            method_name: member_ident,
            method_ty,
        } = call;
        let StaticCallSite {
            call_expr_id,
            func_expr_id,
            astptr,
            args,
        } = site;
        let inst_method_ty = self.inst_ty(&method_ty);

        if let tast::Ty::TFunc { params, ret_ty } = &inst_method_ty
            && !args.is_empty()
        {
            let Some(receiver_arg) = args.first() else {
                util::push_ice(diagnostics, "callee args missing receiver");
                return self.error_expr(None);
            };
            let receiver_tast = self.infer_expr(genv, local_env, diagnostics, *receiver_arg);
            if let tast::Ty::TDyn {
                trait_name: recv_trait,
            } = receiver_tast.get_ty()
                && recv_trait == type_ident.0
            {
                return self.infer_dyn_trait_method_call(
                    genv,
                    local_env,
                    diagnostics,
                    DynTraitMethodCall {
                        call_expr_id,
                        func_expr_id,
                        astptr,
                        args,
                        receiver: receiver_tast,
                        trait_name: &type_ident,
                        method_name: &member_ident,
                        params,
                        ret_ty,
                    },
                );
            }
        }

        let mut args_tast = Vec::new();
        let mut arg_types = Vec::new();
        for arg in args.iter() {
            let arg_tast = self.infer_expr(genv, local_env, diagnostics, *arg);
            arg_types.push(arg_tast.get_ty());
            args_tast.push(arg_tast);
        }

        let receiver_ty = args_tast
            .first()
            .map(|arg| arg.get_ty())
            .unwrap_or(tast::Ty::TUnit);
        let inst_method_ty_for_call = instantiate_self_ty(&inst_method_ty, &receiver_ty);
        let ret_ty_for_call = match &inst_method_ty_for_call {
            tast::Ty::TFunc { ret_ty, .. } => (**ret_ty).clone(),
            _ => self.fresh_ty_var(),
        };
        let call_site_func_ty = tast::Ty::TFunc {
            params: arg_types,
            ret_ty: Box::new(ret_ty_for_call.clone()),
        };

        if let tast::Ty::TParam { name } = &receiver_ty {
            let in_bounds = local_env
                .tparam_trait_bounds(name)
                .is_some_and(|bounds| bounds.iter().any(|t| t.0 == type_ident.0));
            if !in_bounds {
                diagnostics.push(
                    Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Type parameter {} is not constrained by trait {}",
                            name, type_ident.0
                        ),
                    )
                    .with_range(self.expr_range(call_expr_id)),
                );
                return self.error_expr(None);
            }
            self.push_constraint(Constraint::TypeEqual(
                call_site_func_ty.clone(),
                inst_method_ty_for_call.clone(),
                self.expr_range(call_expr_id),
            ));
        } else {
            self.push_constraint(Constraint::Overloaded {
                op: member_ident.clone(),
                trait_name: type_ident.clone(),
                call_site_type: call_site_func_ty.clone(),
                origin: self.expr_range(call_expr_id),
            });
        }

        self.results.record_call_elab(
            call_expr_id,
            CallElab {
                callee: CalleeElab::TraitMethod {
                    trait_name: type_ident.clone(),
                    method_name: member_ident.clone(),
                    ty: inst_method_ty_for_call.clone(),
                    astptr: None,
                },
                args: args.to_vec(),
            },
        );
        self.results
            .record_expr_ty(func_expr_id, inst_method_ty_for_call.clone());
        self.results.record_name_ref_elab(
            func_expr_id,
            NameRefElab::TraitMethod {
                trait_name: type_ident.clone(),
                method_name: member_ident.clone(),
                ty: inst_method_ty_for_call.clone(),
                astptr,
            },
        );
        tast::Expr::ECall {
            func: Box::new(tast::Expr::ETraitMethod {
                trait_name: type_ident.clone(),
                method_name: member_ident.clone(),
                ty: inst_method_ty_for_call,
                astptr: None,
            }),
            args: args_tast,
            ty: ret_ty_for_call,
        }
    }

    fn infer_dyn_trait_method_call(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        call: DynTraitMethodCall<'_>,
    ) -> tast::Expr {
        let DynTraitMethodCall {
            call_expr_id,
            func_expr_id,
            astptr,
            args,
            receiver,
            trait_name,
            method_name,
            params,
            ret_ty,
        } = call;
        if params.len() != args.len() {
            util::push_error_with_range(
                diagnostics,
                format!(
                    "Trait method {}::{} expects {} arguments but got {}",
                    trait_name.0,
                    method_name.0,
                    params.len(),
                    args.len()
                ),
                self.expr_range(call_expr_id),
            );
            return self.error_expr(None);
        }

        let mut args_tast = Vec::with_capacity(args.len());
        args_tast.push(receiver);
        for (arg, expected_ty) in args.iter().skip(1).zip(params.iter().skip(1)) {
            args_tast.push(self.check_expr(genv, local_env, diagnostics, *arg, expected_ty));
        }

        let mut dyn_params = params.to_vec();
        if let Some(first) = dyn_params.get_mut(0) {
            *first = tast::Ty::TDyn {
                trait_name: trait_name.0.clone(),
            };
        } else {
            util::push_ice(diagnostics, "dyn method params missing receiver");
        }
        let dyn_method_ty = tast::Ty::TFunc {
            params: dyn_params,
            ret_ty: Box::new(ret_ty.clone()),
        };

        self.results
            .record_expr_ty(func_expr_id, dyn_method_ty.clone());
        self.results.record_name_ref_elab(
            func_expr_id,
            NameRefElab::DynTraitMethod {
                trait_name: trait_name.clone(),
                method_name: method_name.clone(),
                ty: dyn_method_ty.clone(),
                astptr,
            },
        );
        self.results.record_call_elab(
            call_expr_id,
            CallElab {
                callee: CalleeElab::DynTraitMethod {
                    trait_name: trait_name.clone(),
                    method_name: method_name.clone(),
                    ty: dyn_method_ty.clone(),
                    astptr: None,
                },
                args: args.to_vec(),
            },
        );
        tast::Expr::ECall {
            func: Box::new(tast::Expr::EDynTraitMethod {
                trait_name: trait_name.clone(),
                method_name: method_name.clone(),
                ty: dyn_method_ty,
                astptr: None,
            }),
            args: args_tast,
            ty: ret_ty.clone(),
        }
    }

    fn infer_static_inherent_method_call(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        call: StaticInherentMethodCall<'_>,
    ) -> tast::Expr {
        let StaticInherentMethodCall {
            site,
            type_name,
            member_name: member,
            member_ident,
        } = call;
        let StaticCallSite {
            call_expr_id,
            func_expr_id,
            astptr,
            args,
        } = site;
        let (resolved_type_name, type_env) = util::resolve_type_name(genv, &type_name);
        let type_ident = tast::TastIdent(resolved_type_name.clone());
        let receiver_ty = if type_env.enums().contains_key(&type_ident) {
            Some(tast::Ty::TEnum {
                name: resolved_type_name.clone(),
            })
        } else if type_env.structs().contains_key(&type_ident) {
            Some(tast::Ty::TStruct {
                name: resolved_type_name.clone(),
            })
        } else {
            None
        };
        let has_constr_impl =
            type_env
                .trait_env
                .inherent_impls
                .contains_key(&crate::env::InherentImplKey::Constr(
                    resolved_type_name.clone(),
                ));
        if receiver_ty.is_none() && !has_constr_impl {
            util::push_error_with_range(
                diagnostics,
                format!(
                    "Type or trait {} not found for member access",
                    resolved_type_name
                ),
                self.expr_range(call_expr_id),
            );
            return self.error_expr(None);
        }

        let method_scheme = if let Some(receiver_ty) = receiver_ty.as_ref() {
            type_env.lookup_inherent_method_scheme(receiver_ty, &member_ident)
        } else {
            type_env.lookup_inherent_method_by_constr(&resolved_type_name, &member_ident)
        };
        let Some(method_scheme) = method_scheme else {
            util::push_error_with_range(
                diagnostics,
                format!(
                    "Method {} not found for type {}",
                    member, resolved_type_name
                ),
                self.expr_range(call_expr_id),
            );
            return self.error_expr(None);
        };

        let inst_method_ty = self.inst_ty(&method_scheme.ty);
        let tast::Ty::TFunc { params, ret_ty } = inst_method_ty.clone() else {
            util::push_ice(
                diagnostics,
                format!("Type member {}::{} is not callable", type_name, member),
            );
            return self.error_expr(None);
        };
        let receiver_ty_for_record = receiver_ty.clone().unwrap_or_else(|| {
            params
                .first()
                .filter(|param| {
                    util::try_constr_name(param).is_some_and(|name| name == resolved_type_name)
                })
                .cloned()
                .unwrap_or_else(|| (*ret_ty).clone())
        });
        if params.len() != args.len() {
            util::push_error_with_range(
                diagnostics,
                format!(
                    "Method {} expects {} arguments but got {}",
                    member,
                    params.len(),
                    args.len()
                ),
                self.expr_range(call_expr_id),
            );
            return self.error_expr(None);
        }

        let mut args_tast = Vec::with_capacity(args.len());
        for (arg, expected_ty) in args.iter().zip(params.iter()) {
            let arg_tast = self.check_expr(genv, local_env, diagnostics, *arg, expected_ty);
            args_tast.push(arg_tast);
        }
        if member == "get"
            && let Some(receiver_arg) = args_tast.first()
        {
            validate_hashmap_get_option_for_map_ty(
                genv,
                diagnostics,
                &receiver_arg.get_ty(),
                self.expr_range(call_expr_id),
            );
        }
        let call_site_ty = tast::Ty::TFunc {
            params: args_tast.iter().map(|arg| arg.get_ty()).collect(),
            ret_ty: Box::new((*ret_ty).clone()),
        };
        if !self.apply_fn_scheme_constraints(
            genv,
            local_env,
            diagnostics,
            FnSchemeApplication {
                scheme: &method_scheme,
                template_call_ty: &method_scheme.ty,
                actual_call_ty: &call_site_ty,
                range: self.expr_range(call_expr_id),
            },
        ) {
            return self.error_expr(None);
        }

        self.results.record_call_elab(
            call_expr_id,
            CallElab {
                callee: CalleeElab::InherentMethod {
                    receiver_ty: receiver_ty_for_record.clone(),
                    method_name: member_ident.clone(),
                    ty: inst_method_ty.clone(),
                    astptr: None,
                },
                args: args.to_vec(),
            },
        );
        self.results
            .record_expr_ty(func_expr_id, inst_method_ty.clone());
        self.results.record_name_ref_elab(
            func_expr_id,
            NameRefElab::InherentMethod {
                receiver_ty: receiver_ty_for_record.clone(),
                method_name: member_ident.clone(),
                ty: inst_method_ty.clone(),
                astptr,
            },
        );
        tast::Expr::ECall {
            func: Box::new(tast::Expr::EInherentMethod {
                receiver_ty: receiver_ty_for_record,
                method_name: member_ident.clone(),
                ty: inst_method_ty,
                astptr: None,
            }),
            args: args_tast,
            ty: (*ret_ty).clone(),
        }
    }
}
