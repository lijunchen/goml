use super::*;
use crate::typer::util;

struct FieldValueCall<'a> {
    call_expr_id: hir::ExprId,
    func_expr_id: hir::ExprId,
    receiver: tast::Expr,
    field_name: &'a tast::TastIdent,
    field_ty: tast::Ty,
    args: &'a [hir::ExprId],
}

pub(super) struct MethodCallRequest<'a> {
    pub(super) call_expr_id: hir::ExprId,
    pub(super) func_expr_id: hir::ExprId,
    pub(super) receiver_expr: hir::ExprId,
    pub(super) field: hir::HirIdent,
    pub(super) args: &'a [hir::ExprId],
    pub(super) hint_ret_ty: Option<&'a tast::Ty>,
}

struct ReceiverCall<'a> {
    call_expr_id: hir::ExprId,
    func_expr_id: hir::ExprId,
    receiver_expr_id: hir::ExprId,
    receiver: tast::Expr,
    receiver_ty: tast::Ty,
    method_name: tast::TastIdent,
    args: &'a [hir::ExprId],
    hint_ret_ty: Option<&'a tast::Ty>,
}

struct TraitMethodCall<'a> {
    call_expr_id: hir::ExprId,
    func_expr_id: hir::ExprId,
    receiver_expr_id: hir::ExprId,
    receiver: tast::Expr,
    receiver_ty: tast::Ty,
    trait_ref: &'a tast::TraitRef,
    method_name: &'a tast::TastIdent,
    method_scheme: &'a crate::env::FnScheme,
    args: &'a [hir::ExprId],
}

struct DeferredMethodCall<'a> {
    call_expr_id: hir::ExprId,
    func_expr_id: hir::ExprId,
    receiver_expr_id: hir::ExprId,
    receiver: tast::Expr,
    receiver_ty: tast::Ty,
    method_name: &'a tast::TastIdent,
    args: &'a [hir::ExprId],
}

impl Typer {
    fn check_method_arguments(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        args: &[hir::ExprId],
        params: &[tast::Ty],
    ) -> InferredArguments {
        let mut exprs = Vec::with_capacity(args.len());
        let mut types = Vec::with_capacity(args.len());
        for (index, arg) in args.iter().enumerate() {
            let expected_ty = params.get(index + 1);
            let expr = match expected_ty {
                Some(expected_ty) => {
                    self.check_expr(genv, local_env, diagnostics, *arg, expected_ty)
                }
                None => self.infer_expr(genv, local_env, diagnostics, *arg),
            };
            types.push(expected_ty.cloned().unwrap_or_else(|| expr.get_ty()));
            exprs.push(expr);
        }
        InferredArguments { exprs, types }
    }

    fn infer_field_value_call(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        call: FieldValueCall<'_>,
    ) -> tast::Expr {
        let arguments = self.infer_call_arguments(genv, local_env, diagnostics, call.args);
        let ret_ty = self.fresh_ty_var();
        let call_site_ty = tast::Ty::TFunc {
            params: arguments.types,
            ret_ty: Box::new(ret_ty.clone()),
        };
        self.equate(
            diagnostics,
            &call.field_ty,
            &call_site_ty,
            self.expr_range(call.call_expr_id),
        );
        self.results.record_call_elab(
            call.call_expr_id,
            CallElab {
                callee: CalleeElab::Expr(call.func_expr_id),
                args: call.args.to_vec(),
            },
        );
        tast::Expr::ECall {
            func: Box::new(tast::Expr::EField {
                expr: Box::new(call.receiver),
                field_name: call.field_name.0.clone(),
                ty: call.field_ty,
                astptr: None,
            }),
            args: arguments.exprs,
            ty: ret_ty,
        }
    }

    pub(super) fn infer_method_call(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        request: MethodCallRequest<'_>,
    ) -> tast::Expr {
        let MethodCallRequest {
            call_expr_id,
            func_expr_id,
            receiver_expr,
            field,
            args,
            hint_ret_ty,
        } = request;
        let receiver = self.infer_expr(genv, local_env, diagnostics, receiver_expr);
        let receiver_ty = receiver.get_ty();
        let method_name = tast::TastIdent(field.to_ident_name());
        let method_scheme = genv.lookup_visible_inherent_method_scheme(&receiver_ty, &method_name);
        let call = ReceiverCall {
            call_expr_id,
            func_expr_id,
            receiver_expr_id: receiver_expr,
            receiver,
            receiver_ty,
            method_name,
            args,
            hint_ret_ty,
        };
        match method_scheme {
            Some(method_scheme) => {
                self.infer_inherent_method_call(genv, local_env, diagnostics, call, method_scheme)
            }
            None => self.infer_trait_or_field_call(genv, local_env, diagnostics, call),
        }
    }

    fn infer_inherent_method_call(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        call: ReceiverCall<'_>,
        method_scheme: crate::env::FnScheme,
    ) -> tast::Expr {
        let ReceiverCall {
            call_expr_id,
            func_expr_id: func,
            receiver_expr_id: receiver_expr,
            receiver: receiver_tast,
            receiver_ty,
            method_name,
            args,
            hint_ret_ty: _,
        } = call;
        let method_name_str = method_name.0;
        let range = self.expr_range(call_expr_id);
        let instantiated = self.instantiate_scheme(
            &method_scheme,
            ObligationCause::new(range, ObligationCauseKind::MethodCall),
        );
        self.register_scheme_obligations(&instantiated);
        let mut inst_method_ty = instantiated.ty;
        if let tast::Ty::TFunc { params, .. } = &inst_method_ty
            && let Some(receiver_param_ty) = params.first()
        {
            self.unify(
                diagnostics,
                receiver_param_ty,
                &receiver_ty,
                self.expr_range(call_expr_id),
            );
            inst_method_ty = self.norm(&inst_method_ty);
        }
        let method_params = match &inst_method_ty {
            tast::Ty::TFunc { params, .. } => params.clone(),
            _ => Vec::new(),
        };
        let mut arguments =
            self.check_method_arguments(genv, local_env, diagnostics, args, &method_params);
        arguments.types.insert(0, receiver_ty.clone());
        arguments.exprs.insert(0, receiver_tast);
        let ret_ty = match &inst_method_ty {
            tast::Ty::TFunc { ret_ty, .. } => (**ret_ty).clone(),
            _ => {
                util::push_ice(
                    diagnostics,
                    format!(
                        "Expected inherent method {} to have a function type",
                        method_name_str
                    ),
                );
                return self.error_expr(None);
            }
        };
        let call_site_ty = tast::Ty::TFunc {
            params: arguments.types,
            ret_ty: Box::new(ret_ty.clone()),
        };
        self.equate(
            diagnostics,
            &inst_method_ty,
            &call_site_ty,
            self.expr_range(call_expr_id),
        );

        self.results.record_expr_ty(func, inst_method_ty.clone());
        self.results.record_name_ref_elab(
            func,
            NameRefElab::InherentMethod {
                receiver_ty: receiver_ty.clone(),
                method_name: tast::TastIdent(method_name_str.clone()),
                ty: inst_method_ty.clone(),
                astptr: None,
            },
        );
        self.results.record_call_elab(
            call_expr_id,
            CallElab {
                callee: CalleeElab::InherentMethod {
                    receiver_ty: receiver_ty.clone(),
                    method_name: tast::TastIdent(method_name_str.clone()),
                    ty: inst_method_ty.clone(),
                    astptr: None,
                },
                args: std::iter::once(receiver_expr)
                    .chain(args.iter().copied())
                    .collect(),
            },
        );
        if method_name_str == "get" {
            validate_hashmap_get_option_for_map_ty(
                genv,
                diagnostics,
                &receiver_ty,
                self.expr_range(call_expr_id),
            );
        }
        tast::Expr::ECall {
            func: Box::new(tast::Expr::EInherentMethod {
                receiver_ty: receiver_ty.clone(),
                method_name: tast::TastIdent(method_name_str),
                ty: inst_method_ty,
                astptr: None,
            }),
            args: arguments.exprs,
            ty: ret_ty,
        }
    }

    fn infer_trait_or_field_call(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        call: ReceiverCall<'_>,
    ) -> tast::Expr {
        let ReceiverCall {
            call_expr_id,
            func_expr_id: func,
            receiver_expr_id: receiver_expr,
            receiver: receiver_tast,
            receiver_ty,
            method_name,
            args,
            hint_ret_ty,
        } = call;
        let field_ty = resolve_field_ty_eager(genv, &receiver_ty, &method_name);
        if contains_tvar(&receiver_ty) || contains_tparam(&receiver_ty) {
            return self.infer_deferred_method_call(
                genv,
                local_env,
                diagnostics,
                DeferredMethodCall {
                    call_expr_id,
                    func_expr_id: func,
                    receiver_expr_id: receiver_expr,
                    receiver: receiver_tast,
                    receiver_ty,
                    method_name: &method_name,
                    args,
                },
            );
        }
        let mut lookup =
            lookup_trait_method_candidates(self, genv, local_env, &receiver_ty, &method_name);
        if lookup.candidates.len() > 1
            && let Some(expected) = hint_ret_ty
        {
            lookup.candidates.retain(|(_, scheme)| {
                let method_ty = instantiate_self_ty(&scheme.ty, &receiver_ty);
                let tast::Ty::TFunc { params, ret_ty } = method_ty else {
                    return false;
                };
                if params.len() != args.len() + 1 {
                    return false;
                }
                let snapshot = self.snapshot_inference();
                let mut candidate_diagnostics = Diagnostics::new();
                let matches = self.unify(
                    &mut candidate_diagnostics,
                    &ret_ty,
                    expected,
                    self.expr_range(call_expr_id),
                );
                self.rollback_inference(snapshot);
                matches
            });
        }
        match lookup.candidates.as_slice() {
            [(trait_ref, method_scheme)] => {
                if let Some(field_ty) = field_ty.clone() {
                    return self.infer_field_value_call(
                        genv,
                        local_env,
                        diagnostics,
                        FieldValueCall {
                            call_expr_id,
                            func_expr_id: func,
                            receiver: receiver_tast,
                            field_name: &method_name,
                            field_ty,
                            args,
                        },
                    );
                }
                self.infer_trait_method_call(
                    genv,
                    local_env,
                    diagnostics,
                    TraitMethodCall {
                        call_expr_id,
                        func_expr_id: func,
                        receiver_expr_id: receiver_expr,
                        receiver: receiver_tast,
                        receiver_ty: receiver_ty.clone(),
                        trait_ref,
                        method_name: &method_name,
                        method_scheme,
                        args,
                    },
                )
            }
            [] => {
                if let Some(field_ty) = field_ty {
                    self.infer_field_value_call(
                        genv,
                        local_env,
                        diagnostics,
                        FieldValueCall {
                            call_expr_id,
                            func_expr_id: func,
                            receiver: receiver_tast,
                            field_name: &method_name,
                            field_ty,
                            args,
                        },
                    )
                } else {
                    report_method_not_found(
                        diagnostics,
                        &method_name,
                        &lookup.receiver,
                        self.expr_range(call_expr_id),
                    );
                    tast::Expr::EVar {
                        name: "<error>".to_string(),
                        ty: self.fresh_ty_var(),
                        astptr: None,
                    }
                }
            }
            _ => {
                if let Some(field_ty) = field_ty {
                    return self.infer_field_value_call(
                        genv,
                        local_env,
                        diagnostics,
                        FieldValueCall {
                            call_expr_id,
                            func_expr_id: func,
                            receiver: receiver_tast,
                            field_name: &method_name,
                            field_ty,
                            args,
                        },
                    );
                }
                report_ambiguous_method(
                    diagnostics,
                    &method_name,
                    &lookup.receiver,
                    &lookup.candidates,
                    self.expr_range(call_expr_id),
                );
                tast::Expr::EVar {
                    name: "<error>".to_string(),
                    ty: self.fresh_ty_var(),
                    astptr: None,
                }
            }
        }
    }

    fn infer_deferred_method_call(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        call: DeferredMethodCall<'_>,
    ) -> tast::Expr {
        let DeferredMethodCall {
            call_expr_id,
            func_expr_id,
            receiver_expr_id,
            receiver,
            receiver_ty,
            method_name,
            args,
        } = call;
        let mut arguments = self.infer_call_arguments(genv, local_env, diagnostics, args);
        arguments.types.insert(0, receiver_ty.clone());
        arguments.exprs.insert(0, receiver);
        let ret_ty = self.fresh_ty_var();
        let call_site_ty = tast::Ty::TFunc {
            params: arguments.types,
            ret_ty: Box::new(ret_ty.clone()),
        };
        let mut candidate_traits = local_env.in_scope_traits().to_vec();
        if let tast::Ty::TParam { name } = &receiver_ty
            && let Some(bounds) = local_env.tparam_trait_bounds(name)
        {
            for bound in bounds {
                if !candidate_traits.contains(&bound.name) {
                    candidate_traits.push(bound.name.clone());
                }
            }
        }
        self.push_obligation(
            Predicate::Method(MethodGoal {
                call_expr_id,
                func_expr_id,
                receiver_expr_id,
                receiver_ty: receiver_ty.clone(),
                method: method_name.clone(),
                call_site_type: call_site_ty.clone(),
                args: args.to_vec(),
                in_scope_traits: candidate_traits,
            }),
            ObligationCause::new(
                self.expr_range(call_expr_id),
                ObligationCauseKind::MethodCall,
            ),
        );

        self.results
            .record_expr_ty(func_expr_id, call_site_ty.clone());
        self.results.record_name_ref_elab(
            func_expr_id,
            NameRefElab::InherentMethod {
                receiver_ty: receiver_ty.clone(),
                method_name: method_name.clone(),
                ty: call_site_ty.clone(),
                astptr: None,
            },
        );
        self.results.record_call_elab(
            call_expr_id,
            CallElab {
                callee: CalleeElab::InherentMethod {
                    receiver_ty: receiver_ty.clone(),
                    method_name: method_name.clone(),
                    ty: call_site_ty.clone(),
                    astptr: None,
                },
                args: std::iter::once(receiver_expr_id)
                    .chain(args.iter().copied())
                    .collect(),
            },
        );
        tast::Expr::ECall {
            func: Box::new(tast::Expr::EInherentMethod {
                receiver_ty,
                method_name: method_name.clone(),
                ty: call_site_ty,
                astptr: None,
            }),
            args: arguments.exprs,
            ty: ret_ty,
        }
    }

    fn infer_trait_method_call(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        call: TraitMethodCall<'_>,
    ) -> tast::Expr {
        let TraitMethodCall {
            call_expr_id,
            func_expr_id,
            receiver_expr_id,
            receiver,
            receiver_ty,
            trait_ref,
            method_name,
            method_scheme,
            args,
        } = call;
        let range = self.expr_range(call_expr_id);
        let parent = self.push_obligation(
            Predicate::Trait(TraitGoal {
                trait_ref: trait_ref.clone(),
                for_ty: receiver_ty.clone(),
            }),
            ObligationCause::new(range, ObligationCauseKind::MethodCall),
        );
        let instantiated = self.instantiate_scheme(
            method_scheme,
            ObligationCause::new(range, ObligationCauseKind::FunctionBound).with_parent(parent),
        );
        self.register_scheme_obligations(&instantiated);
        let inst_method_ty = instantiated.ty;
        let inst_method_ty_for_call = instantiate_self_ty(&inst_method_ty, &receiver_ty);

        let (params, ret_ty) = match &inst_method_ty_for_call {
            tast::Ty::TFunc { params, ret_ty } => (params.clone(), (**ret_ty).clone()),
            _ => {
                util::push_ice(
                    diagnostics,
                    format!(
                        "Expected trait method {}::{} to have a function type",
                        trait_ref.name.0, method_name.0
                    ),
                );
                return self.error_expr(None);
            }
        };

        if params.len() != args.len() + 1 {
            util::push_error_with_range(
                diagnostics,
                format!(
                    "Trait method {}::{} expects {} arguments but got {}",
                    trait_ref.name.0,
                    method_name.0,
                    params.len(),
                    args.len() + 1
                ),
                self.expr_range(call_expr_id),
            );
            return self.error_expr(None);
        }

        let mut args_tast = Vec::with_capacity(args.len() + 1);
        args_tast.push(receiver);
        for (arg, expected_ty) in args.iter().zip(params.iter().skip(1)) {
            args_tast.push(self.check_expr(genv, local_env, diagnostics, *arg, expected_ty));
        }

        let receiver_param_ty = params.first().cloned().unwrap_or_else(|| {
            util::push_ice(
                diagnostics,
                format!(
                    "trait method {}::{} missing receiver parameter",
                    trait_ref.name.0, method_name.0
                ),
            );
            self.fresh_ty_var()
        });
        self.equate(
            diagnostics,
            &receiver_ty,
            &receiver_param_ty,
            self.expr_range(call_expr_id),
        );

        self.results.record_call_elab(
            call_expr_id,
            CallElab {
                callee: CalleeElab::TraitMethod {
                    trait_ref: trait_ref.clone(),
                    method_name: method_name.clone(),
                    ty: inst_method_ty_for_call.clone(),
                    astptr: None,
                },
                args: std::iter::once(receiver_expr_id)
                    .chain(args.iter().copied())
                    .collect(),
            },
        );
        self.results
            .record_expr_ty(func_expr_id, inst_method_ty_for_call.clone());
        self.results.record_name_ref_elab(
            func_expr_id,
            NameRefElab::TraitMethod {
                trait_ref: trait_ref.clone(),
                method_name: method_name.clone(),
                ty: inst_method_ty_for_call.clone(),
                astptr: None,
            },
        );
        tast::Expr::ECall {
            func: Box::new(tast::Expr::ETraitMethod {
                trait_ref: trait_ref.clone(),
                method_name: method_name.clone(),
                ty: inst_method_ty_for_call,
                astptr: None,
            }),
            args: args_tast,
            ty: ret_ty,
        }
    }
}
