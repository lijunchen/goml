mod method;
mod static_member;

use method::MethodCallRequest;
use static_member::StaticMemberCall;

use super::*;
use crate::typer::util;

struct InferredArguments {
    exprs: Vec<tast::Expr>,
    types: Vec<tast::Ty>,
}

pub(super) struct CallRequest<'a> {
    pub call_expr_id: hir::ExprId,
    pub func: hir::ExprId,
    pub args: &'a [hir::ExprId],
    pub hint_ret_ty: Option<&'a tast::Ty>,
}

impl Typer {
    pub(super) fn infer_call_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        request: CallRequest<'_>,
    ) -> tast::Expr {
        let CallRequest {
            call_expr_id,
            func,
            args,
            hint_ret_ty,
        } = request;
        let func_expr = self.hir_table.expr(func).clone();
        match func_expr {
            hir::Expr::ENameRef {
                res: hir::NameRef::Local(name),
                astptr: func_astptr,
                ..
            } => {
                let name_str = self.hir_table.local_ident_name(name);
                if let Some(var_ty) = local_env.lookup_var(name) {
                    let norm_var_ty = self.norm(&var_ty);
                    if !matches!(norm_var_ty, tast::Ty::TFunc { .. } | tast::Ty::TVar(_)) {
                        let _ = self.infer_call_arguments(genv, local_env, diagnostics, args);
                        util::push_error_with_range(
                            diagnostics,
                            format!(
                                "Cannot call non-function type {}",
                                util::format_ty_for_diag(&norm_var_ty)
                            ),
                            self.expr_range(call_expr_id),
                        );
                        return self.error_expr_with_ty(func_astptr, tast::Ty::TUnit);
                    }
                    let arguments = if let tast::Ty::TFunc { params, .. } = &norm_var_ty {
                        self.check_call_arguments(genv, local_env, diagnostics, args, params)
                    } else {
                        self.infer_call_arguments(genv, local_env, diagnostics, args)
                    };

                    self.results.record_expr_ty(func, var_ty.clone());
                    self.results.record_name_ref_elab(
                        func,
                        NameRefElab::Var {
                            name: name_str.clone(),
                            ty: var_ty.clone(),
                            astptr: func_astptr,
                        },
                    );
                    let ret_ty = self.fresh_ty_var();
                    let call_site_func_ty = tast::Ty::TFunc {
                        params: arguments.types,
                        ret_ty: Box::new(ret_ty.clone()),
                    };
                    self.equate(
                        diagnostics,
                        &var_ty,
                        &call_site_func_ty,
                        self.expr_range(call_expr_id),
                    );

                    self.results.record_call_elab(
                        call_expr_id,
                        CallElab {
                            callee: CalleeElab::Expr(func),
                            args: args.to_vec(),
                        },
                    );
                    tast::Expr::ECall {
                        func: Box::new(tast::Expr::EVar {
                            name: name_str,
                            ty: var_ty.clone(),
                            astptr: func_astptr,
                        }),
                        args: arguments.exprs,
                        ty: ret_ty,
                    }
                } else {
                    let _ = self.infer_call_arguments(genv, local_env, diagnostics, args);
                    util::push_error_with_range(
                        diagnostics,
                        format!("Unknown variable {}", name_str),
                        func_astptr.map(|ptr| ptr.text_range()),
                    );
                    self.error_expr(func_astptr)
                }
            }
            hir::Expr::ENameRef {
                res: hir::NameRef::Def(_) | hir::NameRef::Builtin(_),
                hint,
                astptr,
                ..
            } => {
                let name = &hint;
                if let Some(func_scheme) = genv.get_function_scheme(name.as_str()) {
                    let call_range = self.expr_range(call_expr_id);
                    let instantiated = self.instantiate_scheme(
                        &func_scheme,
                        ObligationCause::new(call_range, ObligationCauseKind::FunctionBound),
                    );
                    self.register_scheme_obligations(&instantiated);
                    let inst_ty = instantiated.ty;
                    let needs_early_call_site_unify = fn_ret_depends_on_params(&inst_ty);
                    if let (Some(hint), tast::Ty::TFunc { ret_ty: fn_ret, .. }) =
                        (hint_ret_ty, &inst_ty)
                    {
                        self.unify(diagnostics, fn_ret, hint, self.expr_range(call_expr_id));
                    }
                    let arguments = if let tast::Ty::TFunc { params, .. } = &inst_ty {
                        self.check_scheme_call_arguments(genv, local_env, diagnostics, args, params)
                    } else {
                        self.infer_call_arguments(genv, local_env, diagnostics, args)
                    };

                    let ret_ty = self.fresh_ty_var();

                    let call_site_func_ty = tast::Ty::TFunc {
                        params: arguments.types,
                        ret_ty: Box::new(ret_ty.clone()),
                    };
                    if needs_early_call_site_unify {
                        self.try_unify_silent(&inst_ty, &call_site_func_ty);
                    }
                    self.equate(diagnostics, &inst_ty, &call_site_func_ty, call_range);
                    self.results.record_expr_ty(func, inst_ty.clone());
                    let callee = match func_scheme.body {
                        crate::intrinsics::CallableBody::Goml => {
                            self.results.record_name_ref_elab(
                                func,
                                NameRefElab::Var {
                                    name: name.clone(),
                                    ty: inst_ty.clone(),
                                    astptr,
                                },
                            );
                            CalleeElab::Var {
                                name: name.clone(),
                                ty: inst_ty.clone(),
                                astptr: None,
                            }
                        }
                        body => {
                            self.results.record_name_ref_elab(
                                func,
                                NameRefElab::Callable {
                                    name: name.clone(),
                                    body,
                                    ty: inst_ty.clone(),
                                    astptr,
                                },
                            );
                            CalleeElab::Callable {
                                name: name.clone(),
                                body,
                                ty: inst_ty.clone(),
                                astptr: None,
                            }
                        }
                    };
                    self.results.record_call_elab(
                        call_expr_id,
                        CallElab {
                            callee,
                            args: args.to_vec(),
                        },
                    );
                    if matches!(
                        func_scheme.body,
                        crate::intrinsics::CallableBody::Intrinsic(
                            crate::intrinsics::IntrinsicId::HashMapGet
                        )
                    ) && let Some(map_arg) = arguments.exprs.first()
                    {
                        validate_hashmap_get_option_for_map_ty(
                            genv,
                            diagnostics,
                            &map_arg.get_ty(),
                            call_range,
                        );
                    }
                    let func = match func_scheme.body {
                        crate::intrinsics::CallableBody::Goml => tast::Expr::EVar {
                            name: name.clone(),
                            ty: inst_ty,
                            astptr: None,
                        },
                        body => tast::Expr::ECallable {
                            name: name.clone(),
                            body,
                            ty: inst_ty,
                            astptr: None,
                        },
                    };
                    tast::Expr::ECall {
                        func: Box::new(func),
                        args: arguments.exprs,
                        ty: ret_ty,
                    }
                } else {
                    util::push_ice(
                        diagnostics,
                        format!("Function {} not found in environment", name),
                    );
                    self.error_expr(None)
                }
            }
            hir::Expr::ENameRef {
                res: hir::NameRef::Unresolved(path),
                ..
            } => {
                util::push_error_with_range(
                    diagnostics,
                    format!("Unresolved callee {}", path.display()),
                    self.expr_range(call_expr_id),
                );
                self.error_expr(None)
            }
            hir::Expr::EStaticMember {
                path,
                type_args,
                astptr,
            } => self.infer_static_member_call_expr(
                genv,
                local_env,
                diagnostics,
                StaticMemberCall {
                    call_expr_id,
                    func_expr_id: func,
                    path: &path,
                    type_args: &type_args,
                    astptr,
                    args,
                },
            ),
            hir::Expr::EField {
                expr: receiver_expr,
                field,
            } => self.infer_method_call(
                genv,
                local_env,
                diagnostics,
                MethodCallRequest {
                    call_expr_id,
                    func_expr_id: func,
                    receiver_expr,
                    field,
                    args,
                    hint_ret_ty,
                },
            ),
            _ => {
                let arguments = self.infer_call_arguments(genv, local_env, diagnostics, args);
                let ret_ty = self.fresh_ty_var();
                let call_site_func_ty = tast::Ty::TFunc {
                    params: arguments.types,
                    ret_ty: Box::new(ret_ty.clone()),
                };
                let func_tast = self.infer_expr(genv, local_env, diagnostics, func);
                self.equate(
                    diagnostics,
                    &func_tast.get_ty(),
                    &call_site_func_ty,
                    self.expr_range(call_expr_id),
                );

                self.results.record_call_elab(
                    call_expr_id,
                    CallElab {
                        callee: CalleeElab::Expr(func),
                        args: args.to_vec(),
                    },
                );
                tast::Expr::ECall {
                    func: Box::new(func_tast),
                    args: arguments.exprs,
                    ty: ret_ty,
                }
            }
        }
    }

    fn infer_call_arguments(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        args: &[hir::ExprId],
    ) -> InferredArguments {
        let mut exprs = Vec::with_capacity(args.len());
        let mut types = Vec::with_capacity(args.len());
        for arg in args {
            let expr = self.infer_expr(genv, local_env, diagnostics, *arg);
            types.push(expr.get_ty());
            exprs.push(expr);
        }
        InferredArguments { exprs, types }
    }

    fn check_call_arguments(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        args: &[hir::ExprId],
        params: &[tast::Ty],
    ) -> InferredArguments {
        if params.len() != args.len() || params.is_empty() {
            return self.infer_call_arguments(genv, local_env, diagnostics, args);
        }

        let mut exprs = Vec::with_capacity(args.len());
        for (arg, expected_ty) in args.iter().zip(params) {
            exprs.push(self.check_expr(genv, local_env, diagnostics, *arg, expected_ty));
        }
        InferredArguments {
            exprs,
            types: params.to_vec(),
        }
    }

    fn check_scheme_call_arguments(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        args: &[hir::ExprId],
        params: &[tast::Ty],
    ) -> InferredArguments {
        if params.len() != args.len() || params.is_empty() {
            return self.infer_call_arguments(genv, local_env, diagnostics, args);
        }

        let mut exprs = Vec::with_capacity(args.len());
        let mut types = Vec::with_capacity(args.len());
        for (arg, param_ty) in args.iter().zip(params) {
            let expected_ty = self.norm(param_ty);
            let expr = self.check_expr(genv, local_env, diagnostics, *arg, &expected_ty);
            types.push(expected_ty);
            exprs.push(expr);
        }
        InferredArguments { exprs, types }
    }
}
