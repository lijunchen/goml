use std::{collections::HashMap, num::IntErrorKind};

use diagnostics::{Severity, Stage};
use parser::{Diagnostic, Diagnostics, syntax::MySyntaxNodePtr};
use text_size::TextRange;

use crate::common::{self, Prim};
use crate::hir::{self};
use crate::package_names::BUILTIN_PACKAGE;
use crate::typer::localenv::LocalTypeEnv;
use crate::typer::results::{
    CallElab, CalleeElab, Coercion, NameRefElab, StructLitArgElab, StructLitElab, StructPatArgElab,
    StructPatElab, TryElab, TryKind,
};
use crate::{
    env::{Constraint, PackageTypeEnv},
    tast::{self},
    typer::Typer,
};

#[derive(Clone, Copy)]
enum ArrayAssignRoot {
    Local(hir::LocalId),
    Ref,
}

impl Typer {
    fn expr_range(&self, expr_id: hir::ExprId) -> Option<TextRange> {
        self.hir_table.expr_ptr(expr_id).map(|ptr| ptr.text_range())
    }

    fn pat_range(&self, pat_id: hir::PatId) -> Option<TextRange> {
        self.hir_table.pat_ptr(pat_id).map(|ptr| ptr.text_range())
    }

    fn pat_astptr(&self, pat_id: hir::PatId) -> Option<MySyntaxNodePtr> {
        self.hir_table.pat_ptr(pat_id)
    }

    fn error_expr(&mut self, astptr: Option<MySyntaxNodePtr>) -> tast::Expr {
        tast::Expr::EVar {
            name: "<error>".to_string(),
            ty: self.fresh_ty_var(),
            astptr,
        }
    }

    fn error_expr_with_ty(&mut self, astptr: Option<MySyntaxNodePtr>, ty: tast::Ty) -> tast::Expr {
        tast::Expr::EVar {
            name: "<error>".to_string(),
            ty,
            astptr,
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn apply_fn_scheme_constraints(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        scheme: &crate::env::FnScheme,
        template_call_ty: &tast::Ty,
        actual_call_ty: &tast::Ty,
        range: Option<TextRange>,
    ) -> bool {
        let mut subst = HashMap::new();
        collect_type_param_substitution(template_call_ty, actual_call_ty, &mut subst);
        for constraint in scheme.constraints.iter() {
            let Some(actual_ty) = subst.get(&constraint.type_param) else {
                continue;
            };
            if !self.apply_trait_requirement(
                genv,
                local_env,
                diagnostics,
                actual_ty,
                &constraint.trait_name,
                range,
            ) {
                return false;
            }
        }
        true
    }

    fn apply_trait_requirement(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        target_ty: &tast::Ty,
        required_trait: &tast::TastIdent,
        range: Option<TextRange>,
    ) -> bool {
        let Some(resolved_trait) =
            resolve_required_trait_name_or_report(genv, diagnostics, required_trait, range)
        else {
            return false;
        };

        match target_ty {
            tast::Ty::TDyn { trait_name } if trait_name == &resolved_trait => true,
            tast::Ty::TParam { name } => {
                let in_bounds = tparam_has_trait_bound(local_env, name, &resolved_trait);
                if in_bounds {
                    true
                } else {
                    diagnostics.push(
                        Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            format!(
                                "Type parameter {} is not constrained by trait {}",
                                name, resolved_trait
                            ),
                        )
                        .with_range(range),
                    );
                    false
                }
            }
            _ => {
                self.push_constraint(Constraint::Implements {
                    trait_name: tast::TastIdent(resolved_trait),
                    for_ty: target_ty.clone(),
                    origin: range,
                });
                true
            }
        }
    }

    fn record_expr_result(&mut self, expr_id: hir::ExprId, expr: &tast::Expr) {
        self.results.record_expr_ty(expr_id, expr.get_ty());
        match expr {
            tast::Expr::EUnary { resolution, .. } => {
                self.results
                    .record_unary_resolution(expr_id, resolution.clone());
            }
            tast::Expr::EBinary { resolution, .. } => {
                self.results
                    .record_binary_resolution(expr_id, resolution.clone());
            }
            tast::Expr::EClosure { captures, .. } => {
                self.results
                    .record_closure_captures(expr_id, captures.clone());
            }
            _ => {}
        }
    }

    fn record_name_ref_elab(&mut self, expr_id: hir::ExprId, expr: &tast::Expr) {
        match expr {
            tast::Expr::EVar { name, ty, astptr } => {
                self.results.record_name_ref_elab(
                    expr_id,
                    NameRefElab::Var {
                        name: name.clone(),
                        ty: ty.clone(),
                        astptr: *astptr,
                    },
                );
            }
            tast::Expr::ETraitMethod {
                trait_name,
                method_name,
                ty,
                astptr,
            } => {
                self.results.record_name_ref_elab(
                    expr_id,
                    NameRefElab::TraitMethod {
                        trait_name: trait_name.clone(),
                        method_name: method_name.clone(),
                        ty: ty.clone(),
                        astptr: *astptr,
                    },
                );
            }
            tast::Expr::EDynTraitMethod {
                trait_name,
                method_name,
                ty,
                astptr,
            } => {
                self.results.record_name_ref_elab(
                    expr_id,
                    NameRefElab::DynTraitMethod {
                        trait_name: trait_name.clone(),
                        method_name: method_name.clone(),
                        ty: ty.clone(),
                        astptr: *astptr,
                    },
                );
            }
            tast::Expr::EInherentMethod {
                receiver_ty,
                method_name,
                ty,
                astptr,
            } => {
                self.results.record_name_ref_elab(
                    expr_id,
                    NameRefElab::InherentMethod {
                        receiver_ty: receiver_ty.clone(),
                        method_name: method_name.clone(),
                        ty: ty.clone(),
                        astptr: *astptr,
                    },
                );
            }
            _ => {}
        }
    }

    pub fn infer_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        e: hir::ExprId,
    ) -> tast::Expr {
        let expr = self.hir_table.expr(e).clone();
        let out = match expr.clone() {
            hir::Expr::ENameRef { res, hint, astptr } => {
                self.infer_res_expr(genv, local_env, diagnostics, &res, &hint, astptr)
            }
            hir::Expr::EStaticMember { path, astptr } => {
                self.infer_static_member_expr(genv, diagnostics, &path, astptr)
            }
            hir::Expr::EUnit => tast::Expr::EPrim {
                value: Prim::unit(),
                ty: tast::Ty::TUnit,
            },
            hir::Expr::EBool { value } => tast::Expr::EPrim {
                value: Prim::boolean(value),
                ty: tast::Ty::TBool,
            },
            hir::Expr::EInt { value } => {
                let ty = tast::Ty::TInt32;
                let range = self.expr_range(e);
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty, range)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EInt8 { value } => {
                let ty = tast::Ty::TInt8;
                let range = self.expr_range(e);
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty, range)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EInt16 { value } => {
                let ty = tast::Ty::TInt16;
                let range = self.expr_range(e);
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty, range)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EInt32 { value } => {
                let ty = tast::Ty::TInt32;
                let range = self.expr_range(e);
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty, range)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EInt64 { value } => {
                let ty = tast::Ty::TInt64;
                let range = self.expr_range(e);
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty, range)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EUInt8 { value } => {
                let ty = tast::Ty::TUint8;
                let range = self.expr_range(e);
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty, range)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EUInt16 { value } => {
                let ty = tast::Ty::TUint16;
                let range = self.expr_range(e);
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty, range)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EUInt32 { value } => {
                let ty = tast::Ty::TUint32;
                let range = self.expr_range(e);
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty, range)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EUInt64 { value } => {
                let ty = tast::Ty::TUint64;
                let range = self.expr_range(e);
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty, range)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EFloat { value } => {
                let range = self.expr_range(e);
                self.ensure_float_literal_fits(diagnostics, value, &tast::Ty::TFloat64, range);
                let ty = tast::Ty::TFloat64;
                tast::Expr::EPrim {
                    value: Prim::from_float_literal(value, &ty),
                    ty,
                }
            }
            hir::Expr::EFloat32 { value } => {
                let ty = tast::Ty::TFloat32;
                let range = self.expr_range(e);
                let prim = self
                    .parse_float_literal_with_ty(diagnostics, &value, &ty, range)
                    .unwrap_or_else(|| Prim::from_float_literal(0.0, &ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EFloat64 { value } => {
                let ty = tast::Ty::TFloat64;
                let range = self.expr_range(e);
                let prim = self
                    .parse_float_literal_with_ty(diagnostics, &value, &ty, range)
                    .unwrap_or_else(|| Prim::from_float_literal(0.0, &ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EString { value } => tast::Expr::EPrim {
                value: Prim::string(value),
                ty: tast::Ty::TString,
            },
            hir::Expr::EChar { value } => {
                let ty = tast::Ty::TChar;
                let range = self.expr_range(e);
                let ch = self
                    .parse_char_literal(diagnostics, &value, range)
                    .unwrap_or('\0');
                tast::Expr::EPrim {
                    value: Prim::character(ch),
                    ty,
                }
            }
            hir::Expr::EConstr { constructor, args } => self.infer_constructor_expr(
                genv,
                local_env,
                diagnostics,
                e,
                &constructor,
                &args,
                None,
            ),
            hir::Expr::EStructLiteral { name, fields } => self.infer_struct_literal_expr(
                genv,
                local_env,
                diagnostics,
                e,
                &name,
                &fields,
                None,
            ),
            hir::Expr::ETuple { items } => {
                self.infer_tuple_expr(genv, local_env, diagnostics, &items)
            }
            hir::Expr::EArray { items } => {
                self.infer_array_expr(genv, local_env, diagnostics, &items)
            }
            hir::Expr::EClosure { params, body } => {
                self.infer_closure_expr(genv, local_env, diagnostics, &params, body)
            }
            hir::Expr::EBlock { block } => {
                self.infer_block_expr(genv, local_env, diagnostics, &block)
            }
            hir::Expr::EMatch { expr, arms } => self.infer_match_expr(
                genv,
                local_env,
                diagnostics,
                expr,
                &arms,
                self.hir_table.expr_ptr(e),
            ),
            hir::Expr::EIf {
                cond,
                then_branch,
                else_branch,
            } => self.infer_if_expr(genv, local_env, diagnostics, cond, then_branch, else_branch),
            hir::Expr::EWhile { cond, body } => {
                self.infer_while_expr(genv, local_env, diagnostics, cond, body)
            }
            hir::Expr::EBreak => self.infer_break_expr(diagnostics, e),
            hir::Expr::EContinue => self.infer_continue_expr(diagnostics, e),
            hir::Expr::EReturn { expr } => {
                self.infer_return_expr(genv, local_env, diagnostics, e, expr)
            }
            hir::Expr::ETry { expr } => self.infer_try_expr(genv, local_env, diagnostics, e, expr),
            hir::Expr::EGo { expr } => self.infer_go_expr(genv, local_env, diagnostics, expr),
            hir::Expr::ECall { func, args } => {
                self.infer_call_expr(genv, local_env, diagnostics, e, func, &args, None)
            }
            hir::Expr::EUnary { op, expr } => {
                self.infer_unary_expr(genv, local_env, diagnostics, op, expr)
            }
            hir::Expr::EBinary { op, lhs, rhs } => {
                self.infer_binary_expr(genv, local_env, diagnostics, op, lhs, rhs)
            }
            hir::Expr::EProj { tuple, index } => {
                self.infer_proj_expr(genv, local_env, diagnostics, e, tuple, index)
            }
            hir::Expr::EIndex { base, index } => {
                self.infer_index_expr(genv, local_env, diagnostics, e, base, index)
            }
            hir::Expr::EField { expr, field } => self.infer_field_expr(
                genv,
                local_env,
                diagnostics,
                expr,
                &field,
                self.hir_table.expr_ptr(e),
            ),
        };

        self.record_expr_result(e, &out);
        if matches!(
            expr,
            hir::Expr::ENameRef { .. } | hir::Expr::EStaticMember { .. }
        ) {
            self.record_name_ref_elab(e, &out);
        }
        out
    }

    pub fn check_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        e: hir::ExprId,
        expected: &tast::Ty,
    ) -> tast::Expr {
        let expr = self.hir_table.expr(e).clone();
        let expr_tast = match expr {
            hir::Expr::EUnary {
                op: common_defs::UnaryOp::Neg,
                expr: inner,
            } if is_signed_numeric_ty(expected) => {
                let inner_expr = self.hir_table.expr(inner).clone();
                if let hir::Expr::EInt { ref value } = inner_expr
                    && is_integer_ty(expected)
                {
                    let negated = format!("-{}", value);
                    let range = self.expr_range(e);
                    let prim = self
                        .parse_integer_literal_with_ty(diagnostics, &negated, expected, range)
                        .unwrap_or_else(|| Prim::zero_for_int_ty(expected));
                    self.record_expr_result(
                        inner,
                        &tast::Expr::EPrim {
                            value: prim.clone(),
                            ty: expected.clone(),
                        },
                    );
                    tast::Expr::EPrim {
                        value: prim,
                        ty: expected.clone(),
                    }
                } else {
                    let operand = self.check_expr(genv, local_env, diagnostics, inner, expected);
                    tast::Expr::EUnary {
                        op: common_defs::UnaryOp::Neg,
                        expr: Box::new(operand),
                        ty: expected.clone(),
                        resolution: tast::UnaryResolution::Builtin,
                    }
                }
            }
            hir::Expr::EInt { ref value } if is_integer_ty(expected) => {
                let range = self.expr_range(e);
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, value, expected, range)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(expected));
                tast::Expr::EPrim {
                    value: prim,
                    ty: expected.clone(),
                }
            }
            hir::Expr::EFloat { value } if is_float_ty(expected) => {
                let range = self.expr_range(e);
                self.ensure_float_literal_fits(diagnostics, value, expected, range);
                tast::Expr::EPrim {
                    value: Prim::from_float_literal(value, expected),
                    ty: expected.clone(),
                }
            }
            hir::Expr::EBinary { op, lhs, rhs }
                if is_numeric_ty(expected)
                    && matches!(
                        op,
                        common_defs::BinaryOp::Add
                            | common_defs::BinaryOp::Sub
                            | common_defs::BinaryOp::Mul
                            | common_defs::BinaryOp::Div
                    ) =>
            {
                let lhs_tast = self.check_expr(genv, local_env, diagnostics, lhs, expected);
                let rhs_tast = self.check_expr(genv, local_env, diagnostics, rhs, expected);
                tast::Expr::EBinary {
                    op,
                    lhs: Box::new(lhs_tast),
                    rhs: Box::new(rhs_tast),
                    ty: expected.clone(),
                    resolution: tast::BinaryResolution::Builtin,
                }
            }
            hir::Expr::EClosure { params, body } => {
                self.check_closure_expr(genv, local_env, diagnostics, &params, body, expected)
            }
            hir::Expr::EBlock { block } => {
                self.check_block_expr(genv, local_env, diagnostics, &block, expected)
            }
            hir::Expr::ETuple { items } if matches!(expected, tast::Ty::TTuple { typs } if typs.len() == items.len()) =>
            {
                let expected_elem_tys = match expected {
                    tast::Ty::TTuple { typs } => typs.clone(),
                    _ => {
                        super::util::push_ice(
                            diagnostics,
                            "tuple check reached with non-tuple expected type",
                        );
                        (0..items.len()).map(|_| self.fresh_ty_var()).collect()
                    }
                };
                let mut checked_items = Vec::with_capacity(items.len());
                let mut elem_tys = Vec::with_capacity(items.len());
                for (item_expr, expected_ty) in items.iter().zip(expected_elem_tys.iter()) {
                    let item_tast =
                        self.check_expr(genv, local_env, diagnostics, *item_expr, expected_ty);
                    elem_tys.push(item_tast.get_ty());
                    checked_items.push(item_tast);
                }
                tast::Expr::ETuple {
                    items: checked_items,
                    ty: tast::Ty::TTuple { typs: elem_tys },
                }
            }
            hir::Expr::EArray { items } if matches!(expected, tast::Ty::TArray { .. }) => {
                let expected_elem_ty = match expected {
                    tast::Ty::TArray { elem, .. } => (**elem).clone(),
                    _ => self.fresh_ty_var(),
                };
                let len = items.len();
                let mut checked_items = Vec::with_capacity(len);
                for item_expr in items.iter() {
                    let item_tast = self.check_expr(
                        genv,
                        local_env,
                        diagnostics,
                        *item_expr,
                        &expected_elem_ty,
                    );
                    checked_items.push(item_tast);
                }
                tast::Expr::EArray {
                    items: checked_items,
                    ty: tast::Ty::TArray {
                        len,
                        elem: Box::new(expected_elem_ty),
                    },
                }
            }
            hir::Expr::EIf {
                cond,
                then_branch,
                else_branch,
            } => {
                let cond_tast =
                    self.check_expr(genv, local_env, diagnostics, cond, &tast::Ty::TBool);
                let then_tast =
                    self.check_expr(genv, local_env, diagnostics, then_branch, expected);
                let else_tast =
                    self.check_expr(genv, local_env, diagnostics, else_branch, expected);
                tast::Expr::EIf {
                    cond: Box::new(cond_tast),
                    then_branch: Box::new(then_tast),
                    else_branch: Box::new(else_tast),
                    ty: expected.clone(),
                }
            }
            hir::Expr::EMatch { expr, arms } => {
                let expr_tast = self.infer_expr(genv, local_env, diagnostics, expr);
                let expr_ty = expr_tast.get_ty();

                let mut arms_tast = Vec::new();
                for arm in arms.iter() {
                    local_env.push_scope();
                    let arm_tast = self.check_pat(genv, local_env, diagnostics, arm.pat, &expr_ty);
                    let arm_body_tast =
                        self.check_expr(genv, local_env, diagnostics, arm.body, expected);
                    local_env.pop_scope(diagnostics);

                    arms_tast.push(tast::Arm {
                        pat: arm_tast,
                        body: arm_body_tast,
                    });
                }
                tast::Expr::EMatch {
                    expr: Box::new(expr_tast),
                    arms: arms_tast,
                    ty: expected.clone(),
                    astptr: self.hir_table.expr_ptr(e),
                }
            }
            hir::Expr::EConstr { constructor, args } => self.infer_constructor_expr(
                genv,
                local_env,
                diagnostics,
                e,
                &constructor,
                &args,
                Some(expected),
            ),
            hir::Expr::ECall { func, args } => {
                self.infer_call_expr(genv, local_env, diagnostics, e, func, &args, Some(expected))
            }
            hir::Expr::EStructLiteral { name, fields }
                if !matches!(expected, tast::Ty::TDyn { .. }) =>
            {
                self.infer_struct_literal_expr(
                    genv,
                    local_env,
                    diagnostics,
                    e,
                    &name,
                    &fields,
                    Some(expected),
                )
            }
            _ => self.infer_expr(genv, local_env, diagnostics, e),
        };

        self.record_expr_result(e, &expr_tast);
        if self.expr_always_exits_loop_control(e) {
            return expr_tast;
        }
        let (expr_tast, deferred_dyn) =
            self.coerce_to_expected_dyn(genv, local_env, diagnostics, e, expr_tast, expected);
        if !deferred_dyn {
            self.push_constraint(Constraint::TypeEqual(
                expr_tast.get_ty(),
                expected.clone(),
                self.expr_range(e),
            ));
        }
        expr_tast
    }

    fn check_expr_with_deferred_dyn(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        expr_id: hir::ExprId,
        expected: &tast::Ty,
    ) -> (tast::Expr, bool) {
        let deferred_start = self.deferred_dyn_coercions.len();
        let expr = self.check_expr(genv, local_env, diagnostics, expr_id, expected);
        let deferred_dyn = self.deferred_dyn_coercions[deferred_start..]
            .iter()
            .any(|coercion| coercion.expr_id == expr_id);
        (expr, deferred_dyn)
    }

    fn coerce_to_expected_dyn(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        expr_id: hir::ExprId,
        expr: tast::Expr,
        expected: &tast::Ty,
    ) -> (tast::Expr, bool) {
        let expected_norm = self.norm(expected);
        match &expected_norm {
            tast::Ty::TVar(_) => {
                let for_ty = expr.get_ty();
                if !matches!(for_ty, tast::Ty::TDyn { .. }) {
                    self.deferred_dyn_coercions
                        .push(super::DeferredDynCoercion {
                            expr_id,
                            concrete_ty: for_ty.clone(),
                            expected_ty: expected_norm,
                            origin: self.expr_range(expr_id),
                        });
                    return (expr, true);
                }
                return (expr, false);
            }
            tast::Ty::TDyn { .. } => {}
            _ => return (expr, false),
        }

        let tast::Ty::TDyn { trait_name } = &expected_norm else {
            unreachable!()
        };

        if matches!(expr.get_ty(), tast::Ty::TDyn { .. }) {
            return (expr, false);
        }

        let range = self.expr_range(expr_id);
        let Some(resolved_trait) =
            resolve_trait_name_or_report(genv, diagnostics, trait_name, range)
        else {
            return (expr, false);
        };

        let for_ty = expr.get_ty();
        match &for_ty {
            tast::Ty::TParam { name } => {
                if !tparam_has_trait_bound(local_env, name, &resolved_trait) {
                    diagnostics.push(
                        Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            format!(
                                "Type parameter {} is not constrained by trait {}",
                                name, resolved_trait
                            ),
                        )
                        .with_range(range),
                    );
                    return (expr, false);
                }
            }
            tast::Ty::TVar(_) => {
                self.push_constraint(Constraint::Implements {
                    trait_name: tast::TastIdent(resolved_trait.clone()),
                    for_ty: for_ty.clone(),
                    origin: range,
                });
            }
            _ if contains_tvar(&for_ty) => {
                self.push_constraint(Constraint::Implements {
                    trait_name: tast::TastIdent(resolved_trait.clone()),
                    for_ty: for_ty.clone(),
                    origin: range,
                });
            }
            _ if !is_concrete_dyn_target(&for_ty) => {}
            _ => {
                if !genv.has_trait_impl_visible(&resolved_trait, &for_ty) {
                    diagnostics.push(
                        Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            format!(
                                "Type {} does not implement trait {}",
                                super::util::format_ty_for_diag(&for_ty),
                                resolved_trait
                            ),
                        )
                        .with_range(range),
                    );
                    return (expr, false);
                }
            }
        }

        self.results.push_coercion(
            expr_id,
            Coercion::ToDyn {
                trait_name: tast::TastIdent(resolved_trait.clone()),
                for_ty: for_ty.clone(),
                ty: expected_norm.clone(),
                astptr: None,
            },
        );
        (
            tast::Expr::EToDyn {
                trait_name: tast::TastIdent(resolved_trait.clone()),
                for_ty,
                expr: Box::new(expr),
                ty: expected_norm.clone(),
                astptr: None,
            },
            false,
        )
    }

    fn infer_res_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        res: &hir::NameRef,
        hint: &str,
        astptr: Option<MySyntaxNodePtr>,
    ) -> tast::Expr {
        match res {
            hir::NameRef::Local(local_id) => {
                let name_str = self.hir_table.local_ident_name(*local_id);
                if let Some(ty) = local_env.lookup_var(*local_id) {
                    tast::Expr::EVar {
                        name: name_str,
                        ty: ty.clone(),
                        astptr,
                    }
                } else {
                    super::util::push_error_with_range(
                        diagnostics,
                        format!("Unknown variable {}", name_str),
                        astptr.map(|ptr| ptr.text_range()),
                    );
                    self.error_expr(astptr)
                }
            }
            hir::NameRef::Def(_def_id) => {
                let Some(func_scheme) = lookup_function_scheme_by_hint(genv, hint) else {
                    super::util::push_ice(
                        diagnostics,
                        format!("Function {} not found in environment", hint),
                    );
                    return self.error_expr(astptr);
                };
                let inst_ty = self.inst_ty(&func_scheme.ty);
                tast::Expr::EVar {
                    name: hint.to_string(),
                    ty: inst_ty,
                    astptr,
                }
            }
            hir::NameRef::Builtin(_builtin_id) => {
                let Some(func_scheme) = lookup_function_scheme_by_hint(genv, hint) else {
                    super::util::push_ice(
                        diagnostics,
                        format!("Builtin {} not found in environment", hint),
                    );
                    return self.error_expr(astptr);
                };
                let inst_ty = self.inst_ty(&func_scheme.ty);
                tast::Expr::EVar {
                    name: hint.to_string(),
                    ty: inst_ty,
                    astptr,
                }
            }
            hir::NameRef::Unresolved(path) => {
                if path.len() == 1
                    && let Some(name) = path.last_ident()
                    && let Some(func_scheme) = genv.get_function_scheme_unqualified(name.as_str())
                {
                    let inst_ty = self.inst_ty(&func_scheme.ty);
                    return tast::Expr::EVar {
                        name: name.clone(),
                        ty: inst_ty,
                        astptr,
                    };
                }
                super::util::push_error_with_range(
                    diagnostics,
                    format!("Unresolved name {}", path.display()),
                    astptr.map(|p| p.text_range()),
                );
                self.error_expr(astptr)
            }
        }
    }

    fn infer_static_member_expr(
        &mut self,
        genv: &PackageTypeEnv,
        diagnostics: &mut Diagnostics,
        path: &hir::Path,
        astptr: Option<MySyntaxNodePtr>,
    ) -> tast::Expr {
        if path.len() < 2 {
            super::util::push_ice(
                diagnostics,
                format!(
                    "static member path must have at least 2 segments: {}",
                    path.display()
                ),
            );
            return self.error_expr(astptr);
        }
        let namespace = path.namespace_segments();
        let type_name = namespace
            .iter()
            .map(|seg| seg.seg().clone())
            .collect::<Vec<_>>()
            .join("::");
        let Some(member) = path.last_ident() else {
            super::util::push_ice(diagnostics, "static member path missing final segment");
            return self.error_expr(astptr);
        };
        self.infer_type_member_expr(genv, diagnostics, &type_name, member, astptr)
    }

    fn infer_type_member_expr(
        &mut self,
        genv: &PackageTypeEnv,
        diagnostics: &mut Diagnostics,
        type_name: &str,
        member: &str,
        astptr: Option<MySyntaxNodePtr>,
    ) -> tast::Expr {
        let (resolved_type_name, type_env) = super::util::resolve_type_name(genv, type_name);
        let type_ident = tast::TastIdent(resolved_type_name.clone());
        let member_ident = tast::TastIdent(member.to_string());
        if let Some((trait_ident, method_ty)) =
            lookup_trait_method_from_type_name(genv, type_name, &member_ident)
        {
            let inst_ty = self.inst_ty(&method_ty);
            return tast::Expr::ETraitMethod {
                trait_name: trait_ident,
                method_name: member_ident.clone(),
                ty: inst_ty,
                astptr,
            };
        }

        let receiver_ty = if type_env.enums().contains_key(&type_ident) {
            Some(tast::Ty::TEnum {
                name: resolved_type_name.to_string(),
            })
        } else if type_env.structs().contains_key(&type_ident) {
            Some(tast::Ty::TStruct {
                name: resolved_type_name.to_string(),
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
            super::util::push_error_with_range(
                diagnostics,
                format!(
                    "Type or trait {} not found for member access",
                    resolved_type_name
                ),
                astptr.map(|p| p.text_range()),
            );
            return self.error_expr(astptr);
        }

        let method_scheme = if let Some(receiver_ty) = receiver_ty.as_ref() {
            type_env.lookup_inherent_method_scheme(receiver_ty, &member_ident)
        } else {
            type_env.lookup_inherent_method_by_constr(&resolved_type_name, &member_ident)
        };
        if let Some(method_scheme) = method_scheme {
            let inst_ty = self.inst_ty(&method_scheme.ty);
            let receiver_ty_for_record = if let Some(receiver_ty) = receiver_ty.clone() {
                receiver_ty
            } else {
                match &inst_ty {
                    tast::Ty::TFunc { params, ret_ty } => params
                        .first()
                        .cloned()
                        .unwrap_or_else(|| (**ret_ty).clone()),
                    _ => self.fresh_ty_var(),
                }
            };
            tast::Expr::EInherentMethod {
                receiver_ty: receiver_ty_for_record,
                method_name: member_ident,
                ty: inst_ty,
                astptr,
            }
        } else {
            if let Some(enum_def) = type_env.enums().get(&type_ident) {
                if let Some((_, fields)) = enum_def
                    .variants
                    .iter()
                    .find(|(name, _)| name == &member_ident)
                {
                    super::util::push_error_with_range(
                        diagnostics,
                        format!(
                            "Variant {} of enum {} expects {} arguments, but got 0",
                            member,
                            resolved_type_name,
                            fields.len()
                        ),
                        astptr.map(|p| p.text_range()),
                    );
                    return self.error_expr(astptr);
                }
                super::util::push_error_with_range(
                    diagnostics,
                    format!(
                        "Variant {} not found for enum {}",
                        member, resolved_type_name
                    ),
                    astptr.map(|p| p.text_range()),
                );
                return self.error_expr(astptr);
            }
            super::util::push_error_with_range(
                diagnostics,
                format!(
                    "Method {} not found for type {}",
                    member, resolved_type_name
                ),
                astptr.map(|p| p.text_range()),
            );
            self.error_expr(astptr)
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn infer_static_member_call_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        call_expr_id: hir::ExprId,
        func_expr_id: hir::ExprId,
        path: &hir::Path,
        astptr: Option<MySyntaxNodePtr>,
        args: &[hir::ExprId],
    ) -> tast::Expr {
        if path.len() < 2 {
            super::util::push_ice(
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
        let Some(member) = path.last_ident() else {
            super::util::push_ice(diagnostics, "callee path missing final segment");
            return self.error_expr(None);
        };
        let member_ident = tast::TastIdent(member.clone());

        if let Some((type_ident, method_ty)) =
            lookup_trait_method_from_type_name(genv, &type_name, &member_ident)
        {
            let inst_method_ty = self.inst_ty(&method_ty);

            if let tast::Ty::TFunc { params, ret_ty } = &inst_method_ty
                && !args.is_empty()
            {
                let Some(receiver_arg) = args.first() else {
                    super::util::push_ice(diagnostics, "callee args missing receiver");
                    return self.error_expr(None);
                };
                let receiver_tast = self.infer_expr(genv, local_env, diagnostics, *receiver_arg);
                if let tast::Ty::TDyn {
                    trait_name: recv_trait,
                } = receiver_tast.get_ty()
                    && recv_trait == type_ident.0
                {
                    if params.len() != args.len() {
                        super::util::push_error_with_range(
                            diagnostics,
                            format!(
                                "Trait method {}::{} expects {} arguments but got {}",
                                type_ident.0,
                                member_ident.0,
                                params.len(),
                                args.len()
                            ),
                            self.expr_range(call_expr_id),
                        );
                        return self.error_expr(None);
                    }

                    let mut args_tast = Vec::with_capacity(args.len());
                    args_tast.push(receiver_tast);
                    for (arg, expected_ty) in args.iter().skip(1).zip(params.iter().skip(1)) {
                        args_tast.push(self.check_expr(
                            genv,
                            local_env,
                            diagnostics,
                            *arg,
                            expected_ty,
                        ));
                    }

                    let mut dyn_params = params.clone();
                    if let Some(first) = dyn_params.get_mut(0) {
                        *first = tast::Ty::TDyn {
                            trait_name: type_ident.0.clone(),
                        };
                    } else {
                        super::util::push_ice(diagnostics, "dyn method params missing receiver");
                    }
                    let dyn_method_ty = tast::Ty::TFunc {
                        params: dyn_params,
                        ret_ty: ret_ty.clone(),
                    };

                    self.results
                        .record_expr_ty(func_expr_id, dyn_method_ty.clone());
                    self.results.record_name_ref_elab(
                        func_expr_id,
                        NameRefElab::DynTraitMethod {
                            trait_name: type_ident.clone(),
                            method_name: member_ident.clone(),
                            ty: dyn_method_ty.clone(),
                            astptr,
                        },
                    );
                    self.results.record_call_elab(
                        call_expr_id,
                        CallElab {
                            callee: CalleeElab::DynTraitMethod {
                                trait_name: type_ident.clone(),
                                method_name: member_ident.clone(),
                                ty: dyn_method_ty.clone(),
                                astptr: None,
                            },
                            args: args.to_vec(),
                        },
                    );
                    return tast::Expr::ECall {
                        func: Box::new(tast::Expr::EDynTraitMethod {
                            trait_name: type_ident.clone(),
                            method_name: member_ident.clone(),
                            ty: dyn_method_ty,
                            astptr: None,
                        }),
                        args: args_tast,
                        ty: (**ret_ty).clone(),
                    };
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
            return tast::Expr::ECall {
                func: Box::new(tast::Expr::ETraitMethod {
                    trait_name: type_ident.clone(),
                    method_name: member_ident.clone(),
                    ty: inst_method_ty_for_call,
                    astptr: None,
                }),
                args: args_tast,
                ty: ret_ty_for_call,
            };
        }

        let (resolved_type_name, type_env) = super::util::resolve_type_name(genv, &type_name);
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
            super::util::push_error_with_range(
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
            super::util::push_error_with_range(
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
            super::util::push_ice(
                diagnostics,
                format!("Type member {}::{} is not callable", type_name, member),
            );
            return self.error_expr(None);
        };
        let receiver_ty_for_record = receiver_ty
            .clone()
            .unwrap_or_else(|| params.first().cloned().unwrap_or_else(|| (*ret_ty).clone()));
        if params.len() != args.len() {
            super::util::push_error_with_range(
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
        let call_site_ty = tast::Ty::TFunc {
            params: args_tast.iter().map(|arg| arg.get_ty()).collect(),
            ret_ty: Box::new((*ret_ty).clone()),
        };
        if !self.apply_fn_scheme_constraints(
            genv,
            local_env,
            diagnostics,
            &method_scheme,
            &method_scheme.ty,
            &call_site_ty,
            self.expr_range(call_expr_id),
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

    fn constructor_path_from_id(
        &self,
        diagnostics: &mut Diagnostics,
        ctor_id: &hir::ConstructorId,
    ) -> hir::Path {
        match ctor_id {
            hir::ConstructorId::EnumVariant {
                enum_def,
                variant_idx,
            } => {
                if let hir::Def::EnumDef(enum_def_data) = self.hir_table.def(*enum_def) {
                    let Some(variant_idx) = usize::try_from(*variant_idx).ok() else {
                        super::util::push_ice(
                            diagnostics,
                            format!("invalid enum variant index {}", variant_idx),
                        );
                        return hir::Path::from_ident("<error>".to_string());
                    };
                    let Some((variant_ident, _)) = enum_def_data.variants.get(variant_idx) else {
                        super::util::push_ice(
                            diagnostics,
                            format!(
                                "Enum {} variant index {} out of bounds",
                                self.hir_table.def_path(*enum_def).display(),
                                variant_idx
                            ),
                        );
                        return hir::Path::from_ident("<error>".to_string());
                    };
                    let variant_name = variant_ident.to_ident_name();
                    let mut segments = self.hir_table.def_path(*enum_def).segments.clone();
                    segments.push(hir::PathSegment::new(variant_name));
                    hir::Path::new(segments)
                } else {
                    super::util::push_ice(diagnostics, "Constructor points to non-enum DefId");
                    hir::Path::from_ident("<error>".to_string())
                }
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn infer_constructor_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        expr_id: hir::ExprId,
        constructor_ref: &hir::ConstructorRef,
        args: &[hir::ExprId],
        hint_ret_ty: Option<&tast::Ty>,
    ) -> tast::Expr {
        let constructor_path = match constructor_ref {
            hir::ConstructorRef::Resolved(ctor_id) => {
                self.constructor_path_from_id(diagnostics, ctor_id)
            }
            hir::ConstructorRef::Unresolved(path) => path.clone(),
            hir::ConstructorRef::Ambiguous { path, .. } => {
                super::util::push_error_with_range(
                    diagnostics,
                    format!("Ambiguous constructor {}", path.display()),
                    self.expr_range(expr_id),
                );
                return self.error_expr(None);
            }
        };

        let Some(variant_ident) = constructor_path.last_ident() else {
            super::util::push_ice(diagnostics, "Constructor path missing final segment");
            return self.error_expr(None);
        };
        let namespace = constructor_path.namespace_segments();
        let (enum_ident, enum_env) = if namespace.is_empty() {
            (None, genv.current())
        } else {
            let name = namespace
                .iter()
                .map(|seg| seg.seg().clone())
                .collect::<Vec<_>>()
                .join("::");
            let (resolved, env) = super::util::resolve_type_name(genv, &name);
            (Some(tast::TastIdent(resolved)), env)
        };
        let variant_name = tast::TastIdent(variant_ident.clone());
        let ctor = enum_env.lookup_constructor_with_namespace(enum_ident.as_ref(), &variant_name);
        let Some((constructor, constr_ty)) = ctor else {
            super::util::push_error_with_range(
                diagnostics,
                format!(
                    "Constructor {} not found in environment",
                    constructor_path.display()
                ),
                self.expr_range(expr_id),
            );
            return self.error_expr(None);
        };

        let expected_arity = match &constructor {
            common::Constructor::Enum(enum_constructor) => {
                let def = enum_env.enums().get(&enum_constructor.type_name);
                let Some(def) = def else {
                    super::util::push_ice(
                        diagnostics,
                        format!(
                            "Enum {} not found when checking constructor {}",
                            enum_constructor.type_name.0,
                            constructor.name().0
                        ),
                    );
                    return self.error_expr(None);
                };
                let variant = def.variants.get(enum_constructor.index);
                let Some((_, tys)) = variant else {
                    super::util::push_ice(
                        diagnostics,
                        format!(
                            "Enum {} variant index {} out of bounds",
                            enum_constructor.type_name.0, enum_constructor.index
                        ),
                    );
                    return self.error_expr(None);
                };
                tys.len()
            }
            common::Constructor::Struct(struct_constructor) => {
                let def = enum_env.structs().get(&struct_constructor.type_name);
                let Some(def) = def else {
                    super::util::push_ice(
                        diagnostics,
                        format!(
                            "Struct {} not found when checking constructor {}",
                            struct_constructor.type_name.0,
                            constructor.name().0
                        ),
                    );
                    return self.error_expr(None);
                };
                def.fields.len()
            }
        };

        if expected_arity != args.len() {
            super::util::push_error_with_range(
                diagnostics,
                format!(
                    "Constructor {} expects {} arguments, but got {}",
                    constructor.name().0,
                    expected_arity,
                    args.len()
                ),
                self.expr_range(expr_id),
            );
            return self.error_expr(None);
        }

        let inst_constr_ty = self.inst_ty(&constr_ty);
        let param_tys = match &inst_constr_ty {
            tast::Ty::TFunc { params, .. } => params.clone(),
            _ => Vec::new(),
        };

        let ret_ty = match &inst_constr_ty {
            tast::Ty::TFunc { ret_ty, .. } => *ret_ty.clone(),
            _ => inst_constr_ty.clone(),
        };

        if let Some(hint) = hint_ret_ty {
            self.unify(diagnostics, &ret_ty, hint, self.expr_range(expr_id));
        }

        let mut args_tast = Vec::new();
        if param_tys.is_empty() {
            for arg in args.iter() {
                args_tast.push(self.infer_expr(genv, local_env, diagnostics, *arg));
            }
        } else {
            for (arg, param_ty) in args.iter().zip(param_tys.iter()) {
                let expected_ty = self.norm(param_ty);
                let (arg_tast, deferred_dyn) = self.check_expr_with_deferred_dyn(
                    genv,
                    local_env,
                    diagnostics,
                    *arg,
                    &expected_ty,
                );
                if contains_tvar(&expected_ty) && !deferred_dyn {
                    self.unify(
                        diagnostics,
                        &arg_tast.get_ty(),
                        &expected_ty,
                        self.expr_range(*arg),
                    );
                }
                args_tast.push(arg_tast);
            }
        }

        if !args_tast.is_empty() {
            let actual_params: Vec<tast::Ty> = if param_tys.is_empty() {
                args_tast.iter().map(|arg| arg.get_ty()).collect()
            } else {
                param_tys
            };
            let actual_ty = tast::Ty::TFunc {
                params: actual_params,
                ret_ty: Box::new(ret_ty.clone()),
            };
            self.push_constraint(Constraint::TypeEqual(
                inst_constr_ty,
                actual_ty,
                self.expr_range(expr_id),
            ));
        } else {
            self.push_constraint(Constraint::TypeEqual(
                inst_constr_ty,
                ret_ty.clone(),
                self.expr_range(expr_id),
            ));
        }

        self.results
            .record_constructor_expr(expr_id, constructor.clone());

        tast::Expr::EConstr {
            constructor,
            args: args_tast,
            ty: ret_ty,
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn infer_struct_literal_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        expr_id: hir::ExprId,
        name: &hir::QualifiedPath,
        fields: &[(hir::HirIdent, hir::ExprId)],
        hint_ret_ty: Option<&tast::Ty>,
    ) -> tast::Expr {
        let name_display = name.display();
        let (resolved_name, type_env) = super::util::resolve_type_name(genv, &name_display);
        let ctor = type_env.lookup_constructor(&tast::TastIdent(resolved_name.clone()));
        let Some((constructor, constr_ty)) = ctor else {
            super::util::push_error_with_range(
                diagnostics,
                format!("Constructor {} not found in environment", resolved_name),
                self.expr_range(expr_id),
            );
            return self.error_expr(None);
        };

        let struct_fields = match &constructor {
            common::Constructor::Struct(struct_constructor) => {
                let type_name = &struct_constructor.type_name;
                let struct_def = type_env.structs().get(type_name);
                let Some(struct_def) = struct_def else {
                    super::util::push_ice(
                        diagnostics,
                        format!(
                            "Struct {} not found when checking literal {}",
                            type_name.0, resolved_name
                        ),
                    );
                    return self.error_expr(None);
                };
                struct_def.fields.clone()
            }
            common::Constructor::Enum { .. } => {
                super::util::push_error_with_range(
                    diagnostics,
                    format!(
                        "Constructor {} refers to an enum, but a struct literal was used",
                        name_display
                    ),
                    self.expr_range(expr_id),
                );
                return self.error_expr(None);
            }
        };

        let inst_constr_ty = self.inst_ty(&constr_ty);
        if let Some(hint) = hint_ret_ty {
            let ret_ty = match &inst_constr_ty {
                tast::Ty::TFunc { ret_ty, .. } => *ret_ty.clone(),
                _ => inst_constr_ty.clone(),
            };
            self.unify(diagnostics, &ret_ty, hint, self.expr_range(expr_id));
        }
        let param_tys: Vec<tast::Ty> = match &inst_constr_ty {
            tast::Ty::TFunc { params, .. } => params.iter().map(|p| self.norm(p)).collect(),
            _ => Vec::new(),
        };

        if !param_tys.is_empty() && param_tys.len() != struct_fields.len() {
            super::util::push_ice(
                diagnostics,
                format!(
                    "Constructor {} expects {} fields, but got {}",
                    resolved_name,
                    param_tys.len(),
                    struct_fields.len()
                ),
            );
        }

        let mut field_positions: HashMap<tast::TastIdent, usize> = HashMap::new();
        for (idx, (fname, _)) in struct_fields.iter().enumerate() {
            field_positions.insert(fname.clone(), idx);
        }

        let mut ordered_args: Vec<Option<tast::Expr>> = vec![None; struct_fields.len()];
        let mut ordered_hir_args: Vec<Option<hir::ExprId>> = vec![None; struct_fields.len()];
        for (field_name, expr) in fields.iter() {
            let idx = field_positions.get(&tast::TastIdent(field_name.to_ident_name()));
            let Some(&idx) = idx else {
                super::util::push_error_with_range(
                    diagnostics,
                    format!(
                        "Unknown field {} on struct literal {}",
                        field_name.to_ident_name(),
                        resolved_name
                    ),
                    self.expr_range(*expr),
                );
                self.infer_expr(genv, local_env, diagnostics, *expr);
                continue;
            };
            let slot = ordered_args.get_mut(idx);
            let Some(slot) = slot else {
                super::util::push_ice(
                    diagnostics,
                    format!("struct literal field index {} out of bounds", idx),
                );
                continue;
            };
            if slot.is_some() {
                super::util::push_error_with_range(
                    diagnostics,
                    format!(
                        "Duplicate field {} in struct literal {}",
                        field_name.to_ident_name(),
                        name_display
                    ),
                    self.expr_range(*expr),
                );
                continue;
            }
            let hir_slot = ordered_hir_args.get_mut(idx);
            if let Some(hir_slot) = hir_slot {
                *hir_slot = Some(*expr);
            }
            let field_expr = if let Some(expected_ty) = param_tys.get(idx) {
                self.check_expr(genv, local_env, diagnostics, *expr, expected_ty)
            } else {
                self.infer_expr(genv, local_env, diagnostics, *expr)
            };
            *slot = Some(field_expr);
        }

        let mut elab_args = Vec::with_capacity(struct_fields.len());
        for (idx, slot) in ordered_args.iter_mut().enumerate() {
            if slot.is_none() {
                let missing = struct_fields.get(idx).map(|(name, _)| name.0.as_str());
                let Some(missing) = missing else {
                    super::util::push_ice(
                        diagnostics,
                        format!("struct literal field index {} out of bounds", idx),
                    );
                    let placeholder = self.error_expr(None);
                    elab_args.push(StructLitArgElab::Missing {
                        expected_ty: placeholder.get_ty(),
                    });
                    *slot = Some(placeholder);
                    continue;
                };
                super::util::push_error_with_range(
                    diagnostics,
                    format!(
                        "Missing field {} in struct literal {}",
                        missing, name_display
                    ),
                    self.expr_range(expr_id),
                );
                let placeholder = param_tys
                    .get(idx)
                    .cloned()
                    .map(|ty| self.error_expr_with_ty(None, ty))
                    .unwrap_or_else(|| self.error_expr(None));
                elab_args.push(StructLitArgElab::Missing {
                    expected_ty: placeholder.get_ty(),
                });
                *slot = Some(placeholder);
            } else if let Some(hir_arg) = ordered_hir_args.get(idx).and_then(|x| *x) {
                elab_args.push(StructLitArgElab::Expr(hir_arg));
            } else {
                let placeholder_ty = slot
                    .as_ref()
                    .map(tast::Expr::get_ty)
                    .unwrap_or(tast::Ty::TUnit);
                elab_args.push(StructLitArgElab::Missing {
                    expected_ty: placeholder_ty,
                });
            }
        }

        let args_tast: Vec<tast::Expr> = ordered_args
            .into_iter()
            .map(|arg| arg.unwrap_or_else(|| self.error_expr(None)))
            .collect();
        let ret_ty = match &inst_constr_ty {
            tast::Ty::TFunc { ret_ty, .. } => *ret_ty.clone(),
            _ => inst_constr_ty.clone(),
        };

        if !args_tast.is_empty() {
            let actual_params: Vec<_> = args_tast
                .iter()
                .enumerate()
                .map(|(i, arg)| param_tys.get(i).cloned().unwrap_or_else(|| arg.get_ty()))
                .collect();
            let actual_ty = tast::Ty::TFunc {
                params: actual_params,
                ret_ty: Box::new(ret_ty.clone()),
            };
            self.push_constraint(Constraint::TypeEqual(
                inst_constr_ty,
                actual_ty,
                self.expr_range(expr_id),
            ));
        } else {
            self.push_constraint(Constraint::TypeEqual(
                inst_constr_ty,
                ret_ty.clone(),
                self.expr_range(expr_id),
            ));
        }

        self.results.record_struct_lit_elab(
            expr_id,
            StructLitElab {
                constructor: constructor.clone(),
                args: elab_args,
            },
        );

        tast::Expr::EConstr {
            constructor,
            args: args_tast,
            ty: ret_ty,
        }
    }

    fn infer_tuple_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        items: &[hir::ExprId],
    ) -> tast::Expr {
        let mut typs = Vec::new();
        let mut items_tast = Vec::new();
        for item in items.iter() {
            let item_tast = self.infer_expr(genv, local_env, diagnostics, *item);
            typs.push(item_tast.get_ty());
            items_tast.push(item_tast);
        }
        tast::Expr::ETuple {
            items: items_tast,
            ty: tast::Ty::TTuple { typs },
        }
    }

    fn infer_array_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        items: &[hir::ExprId],
    ) -> tast::Expr {
        let len = items.len();
        let elem_ty = self.fresh_ty_var();
        let mut items_tast = Vec::with_capacity(len);
        for item in items.iter() {
            let item_tast = self.infer_expr(genv, local_env, diagnostics, *item);
            self.push_constraint(Constraint::TypeEqual(
                item_tast.get_ty(),
                elem_ty.clone(),
                self.expr_range(*item),
            ));
            items_tast.push(item_tast);
        }

        tast::Expr::EArray {
            items: items_tast,
            ty: tast::Ty::TArray {
                len,
                elem: Box::new(elem_ty),
            },
        }
    }

    fn infer_closure_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        params: &[hir::ClosureParam],
        body: hir::ExprId,
    ) -> tast::Expr {
        local_env.begin_closure();
        let mut params_tast = Vec::new();
        let mut param_tys = Vec::new();
        let current_tparams_env = local_env.current_tparams_env();

        for param in params.iter() {
            let name_str = self.hir_table.local_ident_name(param.name);
            let param_ty = match &param.ty {
                Some(ty) => tast::Ty::from_hir(genv, ty, &current_tparams_env),
                None => self.fresh_ty_var(),
            };
            local_env.insert_var(param.name, param_ty.clone());
            self.results.record_local_ty(param.name, param_ty.clone());
            param_tys.push(param_ty.clone());
            params_tast.push(tast::ClosureParam {
                name: name_str,
                ty: param_ty,
                astptr: Some(param.astptr),
            });
        }

        let body_ty = self.fresh_ty_var();
        self.return_ty_stack.push(body_ty.clone());
        let saved_while_depth = self.while_depth;
        self.while_depth = 0;
        let body_tast = self.infer_expr(genv, local_env, diagnostics, body);
        self.while_depth = saved_while_depth;
        let _ = self.return_ty_stack.pop();
        self.push_constraint(Constraint::TypeEqual(
            body_tast.get_ty(),
            body_ty.clone(),
            self.expr_range(body),
        ));
        let captures = local_env.end_closure(diagnostics, &self.hir_table);

        let closure_ty = tast::Ty::TFunc {
            params: param_tys,
            ret_ty: Box::new(body_ty.clone()),
        };

        tast::Expr::EClosure {
            params: params_tast,
            body: Box::new(body_tast),
            ty: closure_ty,
            captures,
        }
    }

    fn check_closure_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        params: &[hir::ClosureParam],
        body: hir::ExprId,
        expected: &tast::Ty,
    ) -> tast::Expr {
        match expected {
            tast::Ty::TFunc {
                params: expected_params,
                ret_ty: expected_ret,
            } if expected_params.len() == params.len() => {
                local_env.begin_closure();
                let mut params_tast = Vec::new();
                let mut param_tys = Vec::new();
                let current_tparams_env = local_env.current_tparams_env();

                for (param, expected_param_ty) in params.iter().zip(expected_params.iter()) {
                    let name_str = self.hir_table.local_ident_name(param.name);
                    let annotated_ty = param
                        .ty
                        .as_ref()
                        .map(|ty| tast::Ty::from_hir(genv, ty, &current_tparams_env));

                    let param_ty = match annotated_ty {
                        Some(ann_ty) => {
                            self.push_constraint(Constraint::TypeEqual(
                                ann_ty.clone(),
                                expected_param_ty.clone(),
                                None,
                            ));
                            ann_ty
                        }
                        None => expected_param_ty.clone(),
                    };

                    local_env.insert_var(param.name, param_ty.clone());
                    self.results.record_local_ty(param.name, param_ty.clone());
                    param_tys.push(param_ty.clone());
                    params_tast.push(tast::ClosureParam {
                        name: name_str,
                        ty: param_ty,
                        astptr: Some(param.astptr),
                    });
                }

                self.return_ty_stack.push(expected_ret.as_ref().clone());
                let saved_while_depth = self.while_depth;
                self.while_depth = 0;
                let body_tast =
                    self.check_expr(genv, local_env, diagnostics, body, expected_ret.as_ref());
                self.while_depth = saved_while_depth;
                let _ = self.return_ty_stack.pop();
                let body_ty = body_tast.get_ty();
                let captures = local_env.end_closure(diagnostics, &self.hir_table);

                tast::Expr::EClosure {
                    params: params_tast,
                    body: Box::new(body_tast),
                    ty: tast::Ty::TFunc {
                        params: param_tys,
                        ret_ty: Box::new(body_ty),
                    },
                    captures,
                }
            }
            _ => self.infer_closure_expr(genv, local_env, diagnostics, params, body),
        }
    }

    fn infer_let_stmt(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        stmt: &hir::LetStmt,
    ) -> tast::LetStmt {
        let current_tparams_env = local_env.current_tparams_env();
        let annotated_ty = stmt
            .annotation
            .as_ref()
            .map(|ty| tast::Ty::from_hir(genv, ty, &current_tparams_env));

        let (value_tast, value_ty) = if let Some(ann_ty) = &annotated_ty {
            (
                self.check_expr(genv, local_env, diagnostics, stmt.value, ann_ty),
                ann_ty.clone(),
            )
        } else {
            let tast = self.infer_expr(genv, local_env, diagnostics, stmt.value);
            let ty = tast.get_ty();
            (tast, ty)
        };

        let pat_tast = self.check_pat(genv, local_env, diagnostics, stmt.pat, &value_ty);
        self.check_irrefutable_let_pattern(diagnostics, stmt.pat);
        tast::LetStmt {
            is_mut: stmt.is_mut,
            pat: pat_tast,
            value: Box::new(value_tast),
        }
    }

    fn infer_assign_stmt(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        stmt: &hir::AssignStmt,
    ) -> tast::AssignStmt {
        let (target, value_ty) =
            self.infer_assign_target(genv, local_env, diagnostics, stmt.target);
        let value = self.check_expr(genv, local_env, diagnostics, stmt.value, &value_ty);
        tast::AssignStmt {
            target: Box::new(target),
            value: Box::new(value),
        }
    }

    fn infer_block(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        block: &hir::Block,
    ) -> tast::Block {
        let mut stmts = Vec::new();
        for stmt in &block.stmts {
            match stmt {
                hir::Stmt::Let(stmt) => {
                    stmts.push(tast::Stmt::Let(self.infer_let_stmt(
                        genv,
                        local_env,
                        diagnostics,
                        stmt,
                    )));
                }
                hir::Stmt::Assign(stmt) => {
                    stmts.push(tast::Stmt::Assign(self.infer_assign_stmt(
                        genv,
                        local_env,
                        diagnostics,
                        stmt,
                    )));
                }
                hir::Stmt::Expr(stmt) => {
                    stmts.push(tast::Stmt::Expr(tast::ExprStmt {
                        expr: self.infer_expr(genv, local_env, diagnostics, stmt.expr),
                    }));
                }
            }
        }
        let tail = block
            .tail
            .map(|tail| Box::new(self.infer_expr(genv, local_env, diagnostics, tail)));
        tast::Block { stmts, tail }
    }

    pub fn check_block(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        block: &hir::Block,
        expected: &tast::Ty,
    ) -> tast::Block {
        let mut stmts = Vec::new();
        for stmt in &block.stmts {
            match stmt {
                hir::Stmt::Let(stmt) => {
                    stmts.push(tast::Stmt::Let(self.infer_let_stmt(
                        genv,
                        local_env,
                        diagnostics,
                        stmt,
                    )));
                }
                hir::Stmt::Assign(stmt) => {
                    stmts.push(tast::Stmt::Assign(self.infer_assign_stmt(
                        genv,
                        local_env,
                        diagnostics,
                        stmt,
                    )));
                }
                hir::Stmt::Expr(stmt) => {
                    stmts.push(tast::Stmt::Expr(tast::ExprStmt {
                        expr: self.infer_expr(genv, local_env, diagnostics, stmt.expr),
                    }));
                }
            }
        }
        let tail = if let Some(tail) = block.tail {
            Some(Box::new(self.check_expr(
                genv,
                local_env,
                diagnostics,
                tail,
                expected,
            )))
        } else {
            if !self.block_always_returns(block) {
                self.push_constraint(Constraint::TypeEqual(
                    tast::Ty::TUnit,
                    expected.clone(),
                    None,
                ));
            }
            None
        };
        tast::Block { stmts, tail }
    }

    fn infer_block_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        block: &hir::Block,
    ) -> tast::Expr {
        local_env.push_scope();
        let block = self.infer_block(genv, local_env, diagnostics, block);
        local_env.pop_scope(diagnostics);
        let ty = block
            .tail
            .as_ref()
            .map(|expr| expr.get_ty())
            .unwrap_or(tast::Ty::TUnit);
        tast::Expr::EBlock {
            block: Box::new(block),
            ty,
        }
    }

    fn check_block_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        block: &hir::Block,
        expected: &tast::Ty,
    ) -> tast::Expr {
        local_env.push_scope();
        let block = self.check_block(genv, local_env, diagnostics, block, expected);
        local_env.pop_scope(diagnostics);
        let ty = block
            .tail
            .as_ref()
            .map(|expr| expr.get_ty())
            .unwrap_or(tast::Ty::TUnit);
        tast::Expr::EBlock {
            block: Box::new(block),
            ty,
        }
    }

    fn block_always_returns(&self, block: &hir::Block) -> bool {
        for stmt in &block.stmts {
            if self.stmt_always_returns(stmt) {
                return true;
            }
        }
        if let Some(tail) = block.tail {
            return self.expr_always_returns(tail);
        }
        false
    }

    fn stmt_always_returns(&self, stmt: &hir::Stmt) -> bool {
        match stmt {
            hir::Stmt::Expr(stmt) => self.expr_always_returns(stmt.expr),
            hir::Stmt::Let(_) | hir::Stmt::Assign(_) => false,
        }
    }

    fn expr_always_returns(&self, expr_id: hir::ExprId) -> bool {
        match self.hir_table.expr(expr_id) {
            hir::Expr::EReturn { .. } => true,
            hir::Expr::EBlock { block } => self.block_always_returns(block),
            hir::Expr::EIf {
                then_branch,
                else_branch,
                ..
            } => self.expr_always_returns(*then_branch) && self.expr_always_returns(*else_branch),
            hir::Expr::EMatch { arms, .. } => {
                let has_catch_all = arms.iter().any(|arm| {
                    matches!(
                        self.hir_table.pat(arm.pat),
                        hir::Pat::PWild | hir::Pat::PVar { .. }
                    )
                });
                has_catch_all && arms.iter().all(|arm| self.expr_always_returns(arm.body))
            }
            _ => false,
        }
    }

    fn block_always_exits_loop_control(&self, block: &hir::Block) -> bool {
        for stmt in &block.stmts {
            if self.stmt_always_exits_loop_control(stmt) {
                return true;
            }
        }
        if let Some(tail) = block.tail {
            return self.expr_always_exits_loop_control(tail);
        }
        false
    }

    fn stmt_always_exits_loop_control(&self, stmt: &hir::Stmt) -> bool {
        match stmt {
            hir::Stmt::Expr(stmt) => self.expr_always_exits_loop_control(stmt.expr),
            hir::Stmt::Let(_) | hir::Stmt::Assign(_) => false,
        }
    }

    fn expr_always_exits_loop_control(&self, expr_id: hir::ExprId) -> bool {
        match self.hir_table.expr(expr_id) {
            hir::Expr::EBreak | hir::Expr::EContinue | hir::Expr::EReturn { .. } => true,
            hir::Expr::EConstr { args, .. } => args
                .iter()
                .any(|arg| self.expr_always_exits_loop_control(*arg)),
            hir::Expr::EStructLiteral { fields, .. } => fields
                .iter()
                .any(|(_, expr)| self.expr_always_exits_loop_control(*expr)),
            hir::Expr::ETuple { items } | hir::Expr::EArray { items } => items
                .iter()
                .any(|item| self.expr_always_exits_loop_control(*item)),
            hir::Expr::ECall { func, args } => {
                self.expr_always_exits_loop_control(*func)
                    || args
                        .iter()
                        .any(|arg| self.expr_always_exits_loop_control(*arg))
            }
            hir::Expr::EUnary { expr, .. }
            | hir::Expr::ETry { expr }
            | hir::Expr::EGo { expr }
            | hir::Expr::EField { expr, .. } => self.expr_always_exits_loop_control(*expr),
            hir::Expr::EBinary { op, lhs, rhs } => {
                self.expr_always_exits_loop_control(*lhs)
                    || match op {
                        common_defs::BinaryOp::And | common_defs::BinaryOp::Or => false,
                        _ => self.expr_always_exits_loop_control(*rhs),
                    }
            }
            hir::Expr::EProj { tuple, .. } => self.expr_always_exits_loop_control(*tuple),
            hir::Expr::EIndex { base, index } => {
                self.expr_always_exits_loop_control(*base)
                    || self.expr_always_exits_loop_control(*index)
            }
            hir::Expr::EBlock { block } => self.block_always_exits_loop_control(block),
            hir::Expr::EIf {
                then_branch,
                else_branch,
                ..
            } => {
                self.expr_always_exits_loop_control(*then_branch)
                    && self.expr_always_exits_loop_control(*else_branch)
            }
            hir::Expr::EMatch { arms, .. } => {
                let has_catch_all = arms.iter().any(|arm| {
                    matches!(
                        self.hir_table.pat(arm.pat),
                        hir::Pat::PWild | hir::Pat::PVar { .. }
                    )
                });
                has_catch_all
                    && arms
                        .iter()
                        .all(|arm| self.expr_always_exits_loop_control(arm.body))
            }
            hir::Expr::EWhile { cond, .. } => self.expr_always_exits_loop_control(*cond),
            _ => false,
        }
    }

    fn infer_match_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        expr: hir::ExprId,
        arms: &[hir::Arm],
        astptr: Option<MySyntaxNodePtr>,
    ) -> tast::Expr {
        let expr_tast = self.infer_expr(genv, local_env, diagnostics, expr);
        let expr_ty = expr_tast.get_ty();

        let mut arms_tast = Vec::new();
        let arm_ty = self.fresh_ty_var();
        let mut has_value_arm = false;
        for arm in arms.iter() {
            local_env.push_scope();
            let arm_tast = self.check_pat(genv, local_env, diagnostics, arm.pat, &expr_ty);
            let arm_body_tast = self.infer_expr(genv, local_env, diagnostics, arm.body);
            local_env.pop_scope(diagnostics);
            if !self.expr_always_exits_loop_control(arm.body) {
                has_value_arm = true;
                self.push_constraint(Constraint::TypeEqual(
                    arm_body_tast.get_ty(),
                    arm_ty.clone(),
                    self.expr_range(arm.body),
                ));
            }

            arms_tast.push(tast::Arm {
                pat: arm_tast,
                body: arm_body_tast,
            });
        }
        tast::Expr::EMatch {
            expr: Box::new(expr_tast),
            arms: arms_tast,
            ty: if has_value_arm {
                arm_ty
            } else {
                tast::Ty::TUnit
            },
            astptr,
        }
    }

    fn infer_if_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        cond: hir::ExprId,
        then_branch: hir::ExprId,
        else_branch: hir::ExprId,
    ) -> tast::Expr {
        let cond_tast = self.infer_expr(genv, local_env, diagnostics, cond);
        self.push_constraint(Constraint::TypeEqual(
            cond_tast.get_ty(),
            tast::Ty::TBool,
            self.expr_range(cond),
        ));

        let then_tast = self.infer_expr(genv, local_env, diagnostics, then_branch);
        let else_tast = self.infer_expr(genv, local_env, diagnostics, else_branch);
        let result_ty = self.fresh_ty_var();
        let then_exits = self.expr_always_exits_loop_control(then_branch);
        let else_exits = self.expr_always_exits_loop_control(else_branch);

        if !then_exits {
            self.push_constraint(Constraint::TypeEqual(
                then_tast.get_ty(),
                result_ty.clone(),
                self.expr_range(then_branch),
            ));
        }
        if !else_exits {
            self.push_constraint(Constraint::TypeEqual(
                else_tast.get_ty(),
                result_ty.clone(),
                self.expr_range(else_branch),
            ));
        }

        tast::Expr::EIf {
            cond: Box::new(cond_tast),
            then_branch: Box::new(then_tast),
            else_branch: Box::new(else_tast),
            ty: if then_exits && else_exits {
                tast::Ty::TUnit
            } else {
                result_ty
            },
        }
    }

    fn infer_while_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        cond: hir::ExprId,
        body: hir::ExprId,
    ) -> tast::Expr {
        self.while_depth += 1;
        let cond_tast = self.infer_expr(genv, local_env, diagnostics, cond);
        let body_tast = self.infer_expr(genv, local_env, diagnostics, body);
        self.while_depth -= 1;

        if !self.expr_always_exits_loop_control(cond) {
            self.push_constraint(Constraint::TypeEqual(
                cond_tast.get_ty(),
                tast::Ty::TBool,
                self.expr_range(cond),
            ));
        }
        self.push_constraint(Constraint::TypeEqual(
            body_tast.get_ty(),
            tast::Ty::TUnit,
            self.expr_range(body),
        ));

        tast::Expr::EWhile {
            cond: Box::new(cond_tast),
            body: Box::new(body_tast),
            ty: tast::Ty::TUnit,
        }
    }

    fn infer_break_expr(&mut self, diagnostics: &mut Diagnostics, e: hir::ExprId) -> tast::Expr {
        if self.while_depth == 0 {
            super::util::push_error_with_range(
                diagnostics,
                "`break` outside of a while loop",
                self.expr_range(e),
            );
        }
        tast::Expr::EBreak {
            ty: tast::Ty::TUnit,
        }
    }

    fn infer_continue_expr(&mut self, diagnostics: &mut Diagnostics, e: hir::ExprId) -> tast::Expr {
        if self.while_depth == 0 {
            super::util::push_error_with_range(
                diagnostics,
                "`continue` outside of a while loop",
                self.expr_range(e),
            );
        }
        tast::Expr::EContinue {
            ty: tast::Ty::TUnit,
        }
    }

    fn infer_return_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        e: hir::ExprId,
        expr: Option<hir::ExprId>,
    ) -> tast::Expr {
        let Some(expected_ret_ty) = self.return_ty_stack.last().cloned() else {
            super::util::push_error_with_range(
                diagnostics,
                "`return` outside of a function or closure",
                self.expr_range(e),
            );
            let expr = expr
                .map(|expr_id| Box::new(self.infer_expr(genv, local_env, diagnostics, expr_id)));
            return tast::Expr::EReturn {
                expr,
                ty: tast::Ty::TUnit,
            };
        };

        let expr = if let Some(expr_id) = expr {
            Some(Box::new(self.check_expr(
                genv,
                local_env,
                diagnostics,
                expr_id,
                &expected_ret_ty,
            )))
        } else {
            self.push_constraint(Constraint::TypeEqual(
                tast::Ty::TUnit,
                expected_ret_ty,
                self.expr_range(e),
            ));
            None
        };

        tast::Expr::EReturn {
            expr,
            ty: tast::Ty::TUnit,
        }
    }

    fn infer_try_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        e: hir::ExprId,
        expr: hir::ExprId,
    ) -> tast::Expr {
        let inner_expr = self.infer_expr(genv, local_env, diagnostics, expr);
        let inner_ty_raw = inner_expr.get_ty();
        let inner_ty = self.recover_try_container_ty(&inner_expr, &inner_ty_raw);

        let Some(outer_ret_ty_raw) = self.return_ty_stack.last().cloned() else {
            super::util::push_error_with_range(
                diagnostics,
                "`?` outside of a function or closure",
                self.expr_range(e),
            );
            return tast::Expr::EVar {
                name: "<try>".to_string(),
                ty: self.fresh_ty_var(),
                astptr: self.hir_table.expr_ptr(e),
            };
        };
        let outer_ret_ty = self.subst_ty_silent(&outer_ret_ty_raw);
        let range = self.expr_range(e);

        let (kind, ok_ty, container_name) = match (
            try_result_parts(&inner_ty),
            try_option_parts(&inner_ty),
            try_result_parts(&outer_ret_ty),
            try_option_parts(&outer_ret_ty),
            matches!(inner_ty, tast::Ty::TVar(_)),
            matches!(outer_ret_ty, tast::Ty::TVar(_)),
        ) {
            (
                Some((inner_name, ok_ty, err_ty)),
                _,
                Some((outer_name, _, outer_err_ty)),
                _,
                _,
                _,
            ) if inner_name == outer_name => {
                if !same_or_unresolved_ty(outer_err_ty, err_ty) {
                    super::util::push_error_with_range(
                        diagnostics,
                        "`?` on Result[T, E] requires the enclosing function or closure to return Result[_, E]",
                        range,
                    );
                    return tast::Expr::EVar {
                        name: "<try>".to_string(),
                        ty: ok_ty.clone(),
                        astptr: self.hir_table.expr_ptr(e),
                    };
                }
                let outer_ok_ty = self.fresh_ty_var();
                self.push_constraint(Constraint::TypeEqual(
                    outer_ret_ty_raw.clone(),
                    result_ty(inner_name, outer_ok_ty, err_ty.clone()),
                    range,
                ));
                (TryKind::Result, ok_ty.clone(), inner_name.to_string())
            }
            (Some((inner_name, ok_ty, err_ty)), _, _, _, _, true) => {
                let outer_ok_ty = self.fresh_ty_var();
                self.push_constraint(Constraint::TypeEqual(
                    outer_ret_ty_raw.clone(),
                    result_ty(inner_name, outer_ok_ty, err_ty.clone()),
                    range,
                ));
                (TryKind::Result, ok_ty.clone(), inner_name.to_string())
            }
            (_, Some((inner_name, ok_ty)), _, Some((outer_name, _)), _, _)
                if inner_name == outer_name =>
            {
                let outer_ok_ty = self.fresh_ty_var();
                self.push_constraint(Constraint::TypeEqual(
                    outer_ret_ty_raw.clone(),
                    option_ty(inner_name, outer_ok_ty),
                    range,
                ));
                (TryKind::Option, ok_ty.clone(), inner_name.to_string())
            }
            (_, Some((inner_name, ok_ty)), _, _, _, true) => {
                let outer_ok_ty = self.fresh_ty_var();
                self.push_constraint(Constraint::TypeEqual(
                    outer_ret_ty_raw.clone(),
                    option_ty(inner_name, outer_ok_ty),
                    range,
                ));
                (TryKind::Option, ok_ty.clone(), inner_name.to_string())
            }
            (_, _, Some((outer_name, _, err_ty)), _, true, _) => {
                let ok_ty = self.fresh_ty_var();
                self.push_constraint(Constraint::TypeEqual(
                    inner_ty_raw.clone(),
                    result_ty(outer_name, ok_ty.clone(), err_ty.clone()),
                    range,
                ));
                (TryKind::Result, ok_ty, outer_name.to_string())
            }
            (_, _, _, Some((outer_name, _)), true, _) => {
                let ok_ty = self.fresh_ty_var();
                self.push_constraint(Constraint::TypeEqual(
                    inner_ty_raw.clone(),
                    option_ty(outer_name, ok_ty.clone()),
                    range,
                ));
                (TryKind::Option, ok_ty, outer_name.to_string())
            }
            (Some((_, ok_ty, _)), _, _, _, _, _) => {
                super::util::push_error_with_range(
                    diagnostics,
                    "`?` on Result[T, E] requires the enclosing function or closure to return Result[_, E]",
                    range,
                );
                return tast::Expr::EVar {
                    name: "<try>".to_string(),
                    ty: ok_ty.clone(),
                    astptr: self.hir_table.expr_ptr(e),
                };
            }
            (_, Some((_, ok_ty)), _, _, _, _) => {
                super::util::push_error_with_range(
                    diagnostics,
                    "`?` on Option[T] requires the enclosing function or closure to return Option[_]",
                    range,
                );
                return tast::Expr::EVar {
                    name: "<try>".to_string(),
                    ty: ok_ty.clone(),
                    astptr: self.hir_table.expr_ptr(e),
                };
            }
            _ => {
                super::util::push_error_with_range(
                    diagnostics,
                    "`?` can only be used on values of type Option[T] or Result[T, E]",
                    range,
                );
                return tast::Expr::EVar {
                    name: "<try>".to_string(),
                    ty: self.fresh_ty_var(),
                    astptr: self.hir_table.expr_ptr(e),
                };
            }
        };

        let Some((success_index, residual_index)) =
            try_variant_indices(genv, &container_name, &kind, diagnostics, range)
        else {
            return tast::Expr::EVar {
                name: "<try>".to_string(),
                ty: ok_ty,
                astptr: self.hir_table.expr_ptr(e),
            };
        };

        self.results.record_try_elab(
            e,
            TryElab {
                kind,
                outer_ret_ty: outer_ret_ty_raw,
                success_index,
                residual_index,
            },
        );

        tast::Expr::EVar {
            name: "<try>".to_string(),
            ty: ok_ty,
            astptr: self.hir_table.expr_ptr(e),
        }
    }

    fn recover_try_container_ty(&mut self, expr: &tast::Expr, fallback: &tast::Ty) -> tast::Ty {
        match expr {
            tast::Expr::ECall { func, .. } => {
                let func_ty = self.norm(&func.get_ty());
                if let tast::Ty::TFunc { ret_ty, .. } = func_ty {
                    let ret_ty = self.norm(ret_ty.as_ref());
                    if try_result_parts(&ret_ty).is_some() || try_option_parts(&ret_ty).is_some() {
                        return ret_ty;
                    }
                }
            }
            tast::Expr::EBlock { block, .. } => {
                if let Some(tail) = block.tail.as_deref() {
                    let tail_ty = self.recover_try_container_ty(tail, &tail.get_ty());
                    if try_result_parts(&tail_ty).is_some() || try_option_parts(&tail_ty).is_some()
                    {
                        return tail_ty;
                    }
                }
            }
            _ => {}
        }
        self.subst_ty_silent(fallback)
    }

    fn infer_go_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        expr: hir::ExprId,
    ) -> tast::Expr {
        let expr_tast = self.infer_expr(genv, local_env, diagnostics, expr);
        if self.expr_always_exits_loop_control(expr) {
            return tast::Expr::EGo {
                expr: Box::new(expr_tast),
                ty: tast::Ty::TUnit,
            };
        }
        // go expression expects a closure () -> unit
        let closure_ty = tast::Ty::TFunc {
            params: vec![],
            ret_ty: Box::new(tast::Ty::TUnit),
        };
        self.push_constraint(Constraint::TypeEqual(
            expr_tast.get_ty(),
            closure_ty,
            self.expr_range(expr),
        ));

        tast::Expr::EGo {
            expr: Box::new(expr_tast),
            ty: tast::Ty::TUnit,
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn infer_call_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        call_expr_id: hir::ExprId,
        func: hir::ExprId,
        args: &[hir::ExprId],
        hint_ret_ty: Option<&tast::Ty>,
    ) -> tast::Expr {
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
                        for arg in args.iter() {
                            let _ = self.infer_expr(genv, local_env, diagnostics, *arg);
                        }
                        super::util::push_error_with_range(
                            diagnostics,
                            format!(
                                "Cannot call non-function type {}",
                                super::util::format_ty_for_diag(&norm_var_ty)
                            ),
                            self.expr_range(call_expr_id),
                        );
                        return self.error_expr_with_ty(func_astptr, tast::Ty::TUnit);
                    }
                    let mut args_tast = Vec::new();
                    let mut arg_types = Vec::new();
                    if let tast::Ty::TFunc { params, .. } = &norm_var_ty
                        && params.len() == args.len()
                        && !params.is_empty()
                    {
                        for (arg, expected_ty) in args.iter().zip(params.iter()) {
                            let arg_tast =
                                self.check_expr(genv, local_env, diagnostics, *arg, expected_ty);
                            arg_types.push(expected_ty.clone());
                            args_tast.push(arg_tast);
                        }
                    } else {
                        for arg in args.iter() {
                            let arg_tast = self.infer_expr(genv, local_env, diagnostics, *arg);
                            arg_types.push(arg_tast.get_ty());
                            args_tast.push(arg_tast);
                        }
                    }

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
                        params: arg_types,
                        ret_ty: Box::new(ret_ty.clone()),
                    };
                    self.push_constraint(Constraint::TypeEqual(
                        var_ty.clone(),
                        call_site_func_ty.clone(),
                        self.expr_range(call_expr_id),
                    ));

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
                        args: args_tast,
                        ty: ret_ty,
                    }
                } else {
                    let mut args_tast = Vec::new();
                    for arg in args.iter() {
                        let arg_tast = self.infer_expr(genv, local_env, diagnostics, *arg);
                        args_tast.push(arg_tast);
                    }
                    super::util::push_error_with_range(
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
                if let Some(func_scheme) = lookup_function_scheme_by_hint(genv, name.as_str()) {
                    let inst_ty = self.inst_ty(&func_scheme.ty);
                    if let (Some(hint), tast::Ty::TFunc { ret_ty: fn_ret, .. }) =
                        (hint_ret_ty, &inst_ty)
                    {
                        self.unify(diagnostics, fn_ret, hint, self.expr_range(call_expr_id));
                    }
                    let mut args_tast = Vec::new();
                    let mut arg_types = Vec::new();
                    if let tast::Ty::TFunc { params, .. } = &inst_ty
                        && params.len() == args.len()
                        && !params.is_empty()
                    {
                        for (arg, param_ty) in args.iter().zip(params.iter()) {
                            let expected_ty = self.norm(param_ty);
                            let (arg_tast, deferred_dyn) = self.check_expr_with_deferred_dyn(
                                genv,
                                local_env,
                                diagnostics,
                                *arg,
                                &expected_ty,
                            );
                            if contains_tvar(&expected_ty) && !deferred_dyn {
                                self.unify(
                                    diagnostics,
                                    &arg_tast.get_ty(),
                                    &expected_ty,
                                    self.expr_range(*arg),
                                );
                            }
                            arg_types.push(expected_ty);
                            args_tast.push(arg_tast);
                        }
                    } else {
                        for arg in args.iter() {
                            let arg_tast = self.infer_expr(genv, local_env, diagnostics, *arg);
                            arg_types.push(arg_tast.get_ty());
                            args_tast.push(arg_tast);
                        }
                    }

                    let ret_ty = self.fresh_ty_var();

                    let call_range = self.expr_range(call_expr_id);
                    let call_site_func_ty = tast::Ty::TFunc {
                        params: arg_types,
                        ret_ty: Box::new(ret_ty.clone()),
                    };
                    if !self.apply_fn_scheme_constraints(
                        genv,
                        local_env,
                        diagnostics,
                        &func_scheme,
                        &func_scheme.ty,
                        &inst_ty,
                        call_range,
                    ) {
                        return self.error_expr(astptr);
                    }
                    self.push_constraint(Constraint::TypeEqual(
                        inst_ty.clone(),
                        call_site_func_ty.clone(),
                        call_range,
                    ));
                    self.results.record_expr_ty(func, inst_ty.clone());
                    self.results.record_name_ref_elab(
                        func,
                        NameRefElab::Var {
                            name: name.clone(),
                            ty: inst_ty.clone(),
                            astptr,
                        },
                    );
                    self.results.record_call_elab(
                        call_expr_id,
                        CallElab {
                            callee: CalleeElab::Var {
                                name: name.clone(),
                                ty: inst_ty.clone(),
                                astptr: None,
                            },
                            args: args.to_vec(),
                        },
                    );
                    tast::Expr::ECall {
                        func: Box::new(tast::Expr::EVar {
                            name: name.clone(),
                            ty: inst_ty,
                            astptr: None,
                        }),
                        args: args_tast,
                        ty: ret_ty,
                    }
                } else {
                    super::util::push_ice(
                        diagnostics,
                        format!("Function {} not found in environment", name),
                    );
                    self.error_expr(None)
                }
            }
            hir::Expr::ENameRef {
                res: hir::NameRef::Unresolved(path),
                astptr,
                ..
            } => {
                if path.len() == 1
                    && let Some(name) = path.last_ident()
                    && let Some(func_scheme) = genv.get_function_scheme_unqualified(name.as_str())
                {
                    let inst_ty = self.inst_ty(&func_scheme.ty);
                    if let (Some(hint), tast::Ty::TFunc { ret_ty: fn_ret, .. }) =
                        (hint_ret_ty, &inst_ty)
                    {
                        self.unify(diagnostics, fn_ret, hint, self.expr_range(call_expr_id));
                    }
                    let mut args_tast = Vec::new();
                    let mut arg_types = Vec::new();
                    if let tast::Ty::TFunc { params, .. } = &inst_ty
                        && params.len() == args.len()
                        && !params.is_empty()
                    {
                        for (arg, param_ty) in args.iter().zip(params.iter()) {
                            let expected_ty = self.norm(param_ty);
                            let (arg_tast, deferred_dyn) = self.check_expr_with_deferred_dyn(
                                genv,
                                local_env,
                                diagnostics,
                                *arg,
                                &expected_ty,
                            );
                            if contains_tvar(&expected_ty) && !deferred_dyn {
                                self.unify(
                                    diagnostics,
                                    &arg_tast.get_ty(),
                                    &expected_ty,
                                    self.expr_range(*arg),
                                );
                            }
                            arg_types.push(arg_tast.get_ty());
                            args_tast.push(arg_tast);
                        }
                    } else {
                        for arg in args.iter() {
                            let arg_tast = self.infer_expr(genv, local_env, diagnostics, *arg);
                            arg_types.push(arg_tast.get_ty());
                            args_tast.push(arg_tast);
                        }
                    }

                    let ret_ty = self.fresh_ty_var();

                    let call_range = self.expr_range(call_expr_id);
                    let call_site_func_ty = tast::Ty::TFunc {
                        params: arg_types,
                        ret_ty: Box::new(ret_ty.clone()),
                    };
                    if !self.apply_fn_scheme_constraints(
                        genv,
                        local_env,
                        diagnostics,
                        &func_scheme,
                        &func_scheme.ty,
                        &inst_ty,
                        call_range,
                    ) {
                        return self.error_expr(astptr);
                    }
                    self.push_constraint(Constraint::TypeEqual(
                        inst_ty.clone(),
                        call_site_func_ty.clone(),
                        call_range,
                    ));
                    self.results.record_expr_ty(func, inst_ty.clone());
                    self.results.record_name_ref_elab(
                        func,
                        NameRefElab::Var {
                            name: name.clone(),
                            ty: inst_ty.clone(),
                            astptr,
                        },
                    );
                    self.results.record_call_elab(
                        call_expr_id,
                        CallElab {
                            callee: CalleeElab::Var {
                                name: name.clone(),
                                ty: inst_ty.clone(),
                                astptr: None,
                            },
                            args: args.to_vec(),
                        },
                    );
                    return tast::Expr::ECall {
                        func: Box::new(tast::Expr::EVar {
                            name: name.clone(),
                            ty: inst_ty,
                            astptr: None,
                        }),
                        args: args_tast,
                        ty: ret_ty,
                    };
                }
                super::util::push_error_with_range(
                    diagnostics,
                    format!("Unresolved callee {}", path.display()),
                    self.expr_range(call_expr_id),
                );
                self.error_expr(None)
            }
            hir::Expr::EStaticMember { path, astptr } => self.infer_static_member_call_expr(
                genv,
                local_env,
                diagnostics,
                call_expr_id,
                func,
                &path,
                astptr,
                args,
            ),
            hir::Expr::EField {
                expr: receiver_expr,
                field,
            } => {
                let receiver_tast = self.infer_expr(genv, local_env, diagnostics, receiver_expr);
                let receiver_ty = receiver_tast.get_ty();
                let method_name_str = field.to_ident_name();
                let inherent_lookup = lookup_inherent_method_for_ty(
                    genv,
                    &receiver_ty,
                    &tast::TastIdent(method_name_str.clone()),
                );
                if let Some(method_scheme) = inherent_lookup {
                    let inst_method_ty = self.inst_ty(&method_scheme.ty);
                    let method_params = match &inst_method_ty {
                        tast::Ty::TFunc { params, .. } => params.clone(),
                        _ => Vec::new(),
                    };
                    let mut args_tast = Vec::with_capacity(args.len() + 1);
                    let mut arg_types = Vec::with_capacity(args.len() + 1);
                    arg_types.push(receiver_ty.clone());
                    args_tast.push(receiver_tast);
                    for (i, arg) in args.iter().enumerate() {
                        let expected_param_ty = method_params.get(i + 1);
                        let arg_tast = if let Some(expected_param_ty) = expected_param_ty {
                            self.check_expr(genv, local_env, diagnostics, *arg, expected_param_ty)
                        } else {
                            self.infer_expr(genv, local_env, diagnostics, *arg)
                        };
                        let ty_for_call_site = expected_param_ty
                            .cloned()
                            .unwrap_or_else(|| arg_tast.get_ty());
                        arg_types.push(ty_for_call_site);
                        args_tast.push(arg_tast);
                    }
                    let ret_ty = match &inst_method_ty {
                        tast::Ty::TFunc { ret_ty, .. } => (**ret_ty).clone(),
                        _ => {
                            super::util::push_ice(
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
                        params: arg_types,
                        ret_ty: Box::new(ret_ty.clone()),
                    };
                    if !self.apply_fn_scheme_constraints(
                        genv,
                        local_env,
                        diagnostics,
                        &method_scheme,
                        &method_scheme.ty,
                        &inst_method_ty,
                        self.expr_range(call_expr_id),
                    ) {
                        return self.error_expr(None);
                    }
                    self.push_constraint(Constraint::TypeEqual(
                        inst_method_ty.clone(),
                        call_site_ty,
                        self.expr_range(call_expr_id),
                    ));

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
                    tast::Expr::ECall {
                        func: Box::new(tast::Expr::EInherentMethod {
                            receiver_ty: receiver_ty.clone(),
                            method_name: tast::TastIdent(method_name_str),
                            ty: inst_method_ty,
                            astptr: None,
                        }),
                        args: args_tast,
                        ty: ret_ty,
                    }
                } else {
                    let method_name = tast::TastIdent(field.to_ident_name());
                    let field_ty = resolve_field_ty_eager(genv, &receiver_ty, &method_name);
                    let lookup =
                        lookup_trait_method_candidates(genv, local_env, &receiver_ty, &method_name);
                    let is_deferred = matches!(lookup.receiver, MethodLookupReceiver::Deferred(_));
                    match lookup.candidates.as_slice() {
                        [(trait_name, method_ty)] => {
                            if let Some(field_ty) = field_ty.clone() {
                                let mut args_tast = Vec::new();
                                let mut arg_types = Vec::new();
                                for arg in args.iter() {
                                    let arg_tast =
                                        self.infer_expr(genv, local_env, diagnostics, *arg);
                                    arg_types.push(arg_tast.get_ty());
                                    args_tast.push(arg_tast);
                                }
                                let ret_ty = self.fresh_ty_var();
                                let call_site_func_ty = tast::Ty::TFunc {
                                    params: arg_types,
                                    ret_ty: Box::new(ret_ty.clone()),
                                };
                                self.push_constraint(Constraint::TypeEqual(
                                    field_ty.clone(),
                                    call_site_func_ty.clone(),
                                    self.expr_range(call_expr_id),
                                ));
                                let field_tast = tast::Expr::EField {
                                    expr: Box::new(receiver_tast),
                                    field_name: method_name.0.clone(),
                                    ty: field_ty,
                                    astptr: None,
                                };
                                self.results.record_call_elab(
                                    call_expr_id,
                                    CallElab {
                                        callee: CalleeElab::Expr(func),
                                        args: args.to_vec(),
                                    },
                                );
                                return tast::Expr::ECall {
                                    func: Box::new(field_tast),
                                    args: args_tast,
                                    ty: ret_ty,
                                };
                            }
                            let result = self.build_trait_method_call_expr(
                                genv,
                                local_env,
                                diagnostics,
                                call_expr_id,
                                func,
                                receiver_expr,
                                receiver_tast,
                                &receiver_ty,
                                trait_name,
                                &method_name,
                                method_ty,
                                args,
                            );
                            if is_deferred {
                                let call_site_type = if let tast::Expr::ECall {
                                    func: _,
                                    ref args,
                                    ref ty,
                                } = result
                                {
                                    let mut params = Vec::with_capacity(args.len());
                                    for arg in args.iter() {
                                        params.push(arg.get_ty());
                                    }
                                    tast::Ty::TFunc {
                                        params,
                                        ret_ty: Box::new(ty.clone()),
                                    }
                                } else {
                                    self.fresh_ty_var()
                                };
                                self.push_constraint(Constraint::Overloaded {
                                    op: method_name.clone(),
                                    trait_name: trait_name.clone(),
                                    call_site_type,
                                    origin: self.expr_range(call_expr_id),
                                });
                            }
                            result
                        }
                        [] if contains_tvar(&receiver_ty) => {
                            if let Some(field_ty) = field_ty.clone() {
                                let mut args_tast = Vec::new();
                                let mut arg_types = Vec::new();
                                for arg in args.iter() {
                                    let arg_tast =
                                        self.infer_expr(genv, local_env, diagnostics, *arg);
                                    arg_types.push(arg_tast.get_ty());
                                    args_tast.push(arg_tast);
                                }
                                let ret_ty = self.fresh_ty_var();
                                let call_site_func_ty = tast::Ty::TFunc {
                                    params: arg_types,
                                    ret_ty: Box::new(ret_ty.clone()),
                                };
                                self.push_constraint(Constraint::TypeEqual(
                                    field_ty.clone(),
                                    call_site_func_ty.clone(),
                                    self.expr_range(call_expr_id),
                                ));
                                let field_tast = tast::Expr::EField {
                                    expr: Box::new(receiver_tast),
                                    field_name: method_name.0.clone(),
                                    ty: field_ty,
                                    astptr: None,
                                };
                                self.results.record_call_elab(
                                    call_expr_id,
                                    CallElab {
                                        callee: CalleeElab::Expr(func),
                                        args: args.to_vec(),
                                    },
                                );
                                tast::Expr::ECall {
                                    func: Box::new(field_tast),
                                    args: args_tast,
                                    ty: ret_ty,
                                }
                            } else {
                                let mut args_tast = Vec::with_capacity(args.len() + 1);
                                let mut arg_types = Vec::with_capacity(args.len() + 1);
                                arg_types.push(receiver_ty.clone());
                                args_tast.push(receiver_tast);
                                for arg in args.iter() {
                                    let arg_tast =
                                        self.infer_expr(genv, local_env, diagnostics, *arg);
                                    arg_types.push(arg_tast.get_ty());
                                    args_tast.push(arg_tast);
                                }
                                let ret_ty = self.fresh_ty_var();
                                let call_site_ty = tast::Ty::TFunc {
                                    params: arg_types,
                                    ret_ty: Box::new(ret_ty.clone()),
                                };
                                self.push_constraint(Constraint::InherentMethodCall {
                                    receiver_ty: receiver_ty.clone(),
                                    method: method_name.clone(),
                                    call_site_type: call_site_ty.clone(),
                                    origin: self.expr_range(call_expr_id),
                                });

                                self.results.record_expr_ty(func, call_site_ty.clone());
                                self.results.record_name_ref_elab(
                                    func,
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
                                        args: std::iter::once(receiver_expr)
                                            .chain(args.iter().copied())
                                            .collect(),
                                    },
                                );
                                tast::Expr::ECall {
                                    func: Box::new(tast::Expr::EInherentMethod {
                                        receiver_ty: receiver_ty.clone(),
                                        method_name: method_name.clone(),
                                        ty: call_site_ty,
                                        astptr: None,
                                    }),
                                    args: args_tast,
                                    ty: ret_ty,
                                }
                            }
                        }
                        [] => {
                            if let Some(field_ty) = field_ty {
                                let mut args_tast = Vec::new();
                                let mut arg_types = Vec::new();
                                for arg in args.iter() {
                                    let arg_tast =
                                        self.infer_expr(genv, local_env, diagnostics, *arg);
                                    arg_types.push(arg_tast.get_ty());
                                    args_tast.push(arg_tast);
                                }
                                let ret_ty = self.fresh_ty_var();
                                let call_site_func_ty = tast::Ty::TFunc {
                                    params: arg_types,
                                    ret_ty: Box::new(ret_ty.clone()),
                                };
                                self.push_constraint(Constraint::TypeEqual(
                                    field_ty.clone(),
                                    call_site_func_ty.clone(),
                                    self.expr_range(call_expr_id),
                                ));
                                let field_tast = tast::Expr::EField {
                                    expr: Box::new(receiver_tast),
                                    field_name: method_name.0.clone(),
                                    ty: field_ty,
                                    astptr: None,
                                };
                                self.results.record_call_elab(
                                    call_expr_id,
                                    CallElab {
                                        callee: CalleeElab::Expr(func),
                                        args: args.to_vec(),
                                    },
                                );
                                tast::Expr::ECall {
                                    func: Box::new(field_tast),
                                    args: args_tast,
                                    ty: ret_ty,
                                }
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
                                let mut args_tast = Vec::new();
                                let mut arg_types = Vec::new();
                                for arg in args.iter() {
                                    let arg_tast =
                                        self.infer_expr(genv, local_env, diagnostics, *arg);
                                    arg_types.push(arg_tast.get_ty());
                                    args_tast.push(arg_tast);
                                }
                                let ret_ty = self.fresh_ty_var();
                                let call_site_func_ty = tast::Ty::TFunc {
                                    params: arg_types,
                                    ret_ty: Box::new(ret_ty.clone()),
                                };
                                self.push_constraint(Constraint::TypeEqual(
                                    field_ty.clone(),
                                    call_site_func_ty.clone(),
                                    self.expr_range(call_expr_id),
                                ));
                                let field_tast = tast::Expr::EField {
                                    expr: Box::new(receiver_tast),
                                    field_name: method_name.0.clone(),
                                    ty: field_ty,
                                    astptr: None,
                                };
                                self.results.record_call_elab(
                                    call_expr_id,
                                    CallElab {
                                        callee: CalleeElab::Expr(func),
                                        args: args.to_vec(),
                                    },
                                );
                                return tast::Expr::ECall {
                                    func: Box::new(field_tast),
                                    args: args_tast,
                                    ty: ret_ty,
                                };
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
            }
            _ => {
                let mut args_tast = Vec::new();
                let mut arg_types = Vec::new();
                for arg in args.iter() {
                    let arg_tast = self.infer_expr(genv, local_env, diagnostics, *arg);
                    arg_types.push(arg_tast.get_ty());
                    args_tast.push(arg_tast);
                }
                let ret_ty = self.fresh_ty_var();
                let call_site_func_ty = tast::Ty::TFunc {
                    params: arg_types,
                    ret_ty: Box::new(ret_ty.clone()),
                };
                let func_tast = self.infer_expr(genv, local_env, diagnostics, func);
                self.push_constraint(Constraint::TypeEqual(
                    func_tast.get_ty(),
                    call_site_func_ty.clone(),
                    self.expr_range(call_expr_id),
                ));

                self.results.record_call_elab(
                    call_expr_id,
                    CallElab {
                        callee: CalleeElab::Expr(func),
                        args: args.to_vec(),
                    },
                );
                tast::Expr::ECall {
                    func: Box::new(func_tast),
                    args: args_tast,
                    ty: ret_ty,
                }
            }
        }
    }

    fn infer_unary_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        op: common_defs::UnaryOp,
        expr: hir::ExprId,
    ) -> tast::Expr {
        let expr_tast = self.infer_expr(genv, local_env, diagnostics, expr);
        let expr_ty = expr_tast.get_ty();
        match op {
            common_defs::UnaryOp::Not => {
                self.push_constraint(Constraint::TypeEqual(
                    expr_ty.clone(),
                    tast::Ty::TBool,
                    self.expr_range(expr),
                ));
                tast::Expr::EUnary {
                    op,
                    expr: Box::new(expr_tast),
                    ty: tast::Ty::TBool,
                    resolution: tast::UnaryResolution::Builtin,
                }
            }
            common_defs::UnaryOp::Neg => {
                self.push_constraint(Constraint::TypeEqual(
                    expr_ty.clone(),
                    expr_ty.clone(),
                    self.expr_range(expr),
                ));
                tast::Expr::EUnary {
                    op,
                    expr: Box::new(expr_tast),
                    ty: expr_ty,
                    resolution: tast::UnaryResolution::Builtin,
                }
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn build_trait_method_call_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        call_expr_id: hir::ExprId,
        func_expr_id: hir::ExprId,
        receiver_expr_id: hir::ExprId,
        receiver_tast: tast::Expr,
        receiver_ty: &tast::Ty,
        trait_name: &tast::TastIdent,
        method_name: &tast::TastIdent,
        method_ty: &tast::Ty,
        args: &[hir::ExprId],
    ) -> tast::Expr {
        let inst_method_ty = self.inst_ty(method_ty);
        let inst_method_ty_for_call = instantiate_self_ty(&inst_method_ty, receiver_ty);

        let (params, ret_ty) = match &inst_method_ty_for_call {
            tast::Ty::TFunc { params, ret_ty } => (params.clone(), (**ret_ty).clone()),
            _ => {
                super::util::push_ice(
                    diagnostics,
                    format!(
                        "Expected trait method {}::{} to have a function type",
                        trait_name.0, method_name.0
                    ),
                );
                return self.error_expr(None);
            }
        };

        if params.len() != args.len() + 1 {
            super::util::push_error_with_range(
                diagnostics,
                format!(
                    "Trait method {}::{} expects {} arguments but got {}",
                    trait_name.0,
                    method_name.0,
                    params.len(),
                    args.len() + 1
                ),
                self.expr_range(call_expr_id),
            );
            return self.error_expr(None);
        }

        let mut args_tast = Vec::with_capacity(args.len() + 1);
        args_tast.push(receiver_tast);
        for (arg, expected_ty) in args.iter().zip(params.iter().skip(1)) {
            args_tast.push(self.check_expr(genv, local_env, diagnostics, *arg, expected_ty));
        }

        let receiver_param_ty = params.first().cloned().unwrap_or_else(|| {
            super::util::push_ice(
                diagnostics,
                format!(
                    "trait method {}::{} missing receiver parameter",
                    trait_name.0, method_name.0
                ),
            );
            self.fresh_ty_var()
        });
        self.push_constraint(Constraint::TypeEqual(
            receiver_ty.clone(),
            receiver_param_ty,
            self.expr_range(call_expr_id),
        ));

        self.results.record_call_elab(
            call_expr_id,
            CallElab {
                callee: CalleeElab::TraitMethod {
                    trait_name: trait_name.clone(),
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
                trait_name: trait_name.clone(),
                method_name: method_name.clone(),
                ty: inst_method_ty_for_call.clone(),
                astptr: None,
            },
        );
        tast::Expr::ECall {
            func: Box::new(tast::Expr::ETraitMethod {
                trait_name: trait_name.clone(),
                method_name: method_name.clone(),
                ty: inst_method_ty_for_call,
                astptr: None,
            }),
            args: args_tast,
            ty: ret_ty,
        }
    }

    fn infer_binary_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        op: common_defs::BinaryOp,
        lhs: hir::ExprId,
        rhs: hir::ExprId,
    ) -> tast::Expr {
        let lhs_tast = self.infer_expr(genv, local_env, diagnostics, lhs);
        let rhs_tast = self.infer_expr(genv, local_env, diagnostics, rhs);
        let lhs_ty = lhs_tast.get_ty();
        let rhs_ty = rhs_tast.get_ty();

        let ret_ty = match op {
            common_defs::BinaryOp::And
            | common_defs::BinaryOp::Or
            | common_defs::BinaryOp::Less
            | common_defs::BinaryOp::Greater
            | common_defs::BinaryOp::LessEq
            | common_defs::BinaryOp::GreaterEq
            | common_defs::BinaryOp::Eq
            | common_defs::BinaryOp::NotEq => tast::Ty::TBool,
            common_defs::BinaryOp::Add => {
                let norm_lhs = self.norm(&lhs_ty);
                if is_numeric_ty(&norm_lhs) || matches!(norm_lhs, tast::Ty::TString) {
                    norm_lhs
                } else if matches!(norm_lhs, tast::Ty::TVar(..)) {
                    let tv = self.fresh_ty_var();
                    self.deferred_arithmetic_checks
                        .push(super::DeferredArithmeticCheck {
                            kind: super::ArithmeticKind::NumericOrString,
                            ty: tv.clone(),
                            op: "+",
                            origin: self.expr_range(lhs),
                        });
                    tv
                } else {
                    super::util::push_error_with_range(
                        diagnostics,
                        format!(
                            "Operator + is not defined for type {}",
                            super::util::format_ty_for_diag(&norm_lhs)
                        ),
                        self.expr_range(lhs),
                    );
                    norm_lhs
                }
            }
            common_defs::BinaryOp::Sub
            | common_defs::BinaryOp::Mul
            | common_defs::BinaryOp::Div => {
                let norm_lhs = self.norm(&lhs_ty);
                if is_numeric_ty(&norm_lhs) {
                    norm_lhs
                } else if matches!(norm_lhs, tast::Ty::TVar(..)) {
                    let tv = self.fresh_ty_var();
                    let op_str = match op {
                        common_defs::BinaryOp::Sub => "-",
                        common_defs::BinaryOp::Mul => "*",
                        common_defs::BinaryOp::Div => "/",
                        _ => unreachable!(),
                    };
                    self.deferred_arithmetic_checks
                        .push(super::DeferredArithmeticCheck {
                            kind: super::ArithmeticKind::Numeric,
                            ty: tv.clone(),
                            op: op_str,
                            origin: self.expr_range(lhs),
                        });
                    tv
                } else {
                    let op_str = match op {
                        common_defs::BinaryOp::Sub => "-",
                        common_defs::BinaryOp::Mul => "*",
                        common_defs::BinaryOp::Div => "/",
                        _ => unreachable!(),
                    };
                    super::util::push_error_with_range(
                        diagnostics,
                        format!(
                            "Operator {} is not defined for type {}",
                            op_str,
                            super::util::format_ty_for_diag(&norm_lhs)
                        ),
                        self.expr_range(lhs),
                    );
                    norm_lhs
                }
            }
        };

        match op {
            common_defs::BinaryOp::Add
            | common_defs::BinaryOp::Sub
            | common_defs::BinaryOp::Mul
            | common_defs::BinaryOp::Div => {
                self.push_constraint(Constraint::TypeEqual(
                    lhs_ty.clone(),
                    ret_ty.clone(),
                    self.expr_range(lhs),
                ));
                self.push_constraint(Constraint::TypeEqual(
                    rhs_ty.clone(),
                    ret_ty.clone(),
                    self.expr_range(rhs),
                ));
            }
            common_defs::BinaryOp::And | common_defs::BinaryOp::Or => {
                self.push_constraint(Constraint::TypeEqual(
                    lhs_ty.clone(),
                    tast::Ty::TBool,
                    self.expr_range(lhs),
                ));
                self.push_constraint(Constraint::TypeEqual(
                    rhs_ty.clone(),
                    tast::Ty::TBool,
                    self.expr_range(rhs),
                ));
            }
            common_defs::BinaryOp::Less
            | common_defs::BinaryOp::Greater
            | common_defs::BinaryOp::LessEq
            | common_defs::BinaryOp::GreaterEq
            | common_defs::BinaryOp::Eq
            | common_defs::BinaryOp::NotEq => {
                // Comparison operators: lhs and rhs must have same type (numeric types)
                self.push_constraint(Constraint::TypeEqual(
                    lhs_ty.clone(),
                    rhs_ty.clone(),
                    self.expr_range(lhs),
                ));
            }
        }

        tast::Expr::EBinary {
            op,
            lhs: Box::new(lhs_tast),
            rhs: Box::new(rhs_tast),
            ty: ret_ty.clone(),
            resolution: tast::BinaryResolution::Builtin,
        }
    }

    fn infer_proj_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        proj_expr_id: hir::ExprId,
        tuple: hir::ExprId,
        index: usize,
    ) -> tast::Expr {
        let tuple_tast = self.infer_expr(genv, local_env, diagnostics, tuple);
        if self.expr_always_exits_loop_control(tuple) {
            return tast::Expr::EProj {
                tuple: Box::new(tuple_tast),
                index,
                ty: tast::Ty::TUnit,
            };
        }
        let tuple_ty = tuple_tast.get_ty();
        let range = self.expr_range(proj_expr_id);
        match &tuple_ty {
            tast::Ty::TTuple { typs } => {
                let field_ty = typs.get(index).cloned().unwrap_or_else(|| {
                    diagnostics.push(
                        Diagnostic::new(
                            diagnostics::Stage::Typer,
                            diagnostics::Severity::Error,
                            format!(
                                "Tuple index {} out of bounds for type {}",
                                index,
                                super::util::format_ty_for_diag(&tuple_ty)
                            ),
                        )
                        .with_range(range),
                    );
                    self.fresh_ty_var()
                });
                tast::Expr::EProj {
                    tuple: Box::new(tuple_tast),
                    index,
                    ty: field_ty,
                }
            }
            _ => {
                diagnostics.push(
                    Diagnostic::new(
                        diagnostics::Stage::Typer,
                        diagnostics::Severity::Error,
                        format!(
                            "Cannot project field {} on non-tuple type {}",
                            index,
                            super::util::format_ty_for_diag(&tuple_ty)
                        ),
                    )
                    .with_range(range),
                );
                let ret_ty = self.fresh_ty_var();
                tast::Expr::EProj {
                    tuple: Box::new(tuple_tast),
                    index,
                    ty: ret_ty,
                }
            }
        }
    }

    fn infer_index_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        index_expr_id: hir::ExprId,
        base: hir::ExprId,
        index: hir::ExprId,
    ) -> tast::Expr {
        let base_tast = self.infer_expr(genv, local_env, diagnostics, base);
        if self.expr_always_exits_loop_control(base) {
            return tast::Expr::EIndex {
                base: Box::new(base_tast),
                index: Box::new(tast::Expr::EPrim {
                    value: Prim::unit(),
                    ty: tast::Ty::TUnit,
                }),
                ty: tast::Ty::TUnit,
                astptr: self.hir_table.expr_ptr(index_expr_id),
            };
        }
        if self.expr_always_exits_loop_control(index) {
            let index_tast = self.infer_expr(genv, local_env, diagnostics, index);
            return tast::Expr::EIndex {
                base: Box::new(base_tast),
                index: Box::new(index_tast),
                ty: tast::Ty::TUnit,
                astptr: self.hir_table.expr_ptr(index_expr_id),
            };
        }
        let base_ty = self.resolve_index_base_ty(diagnostics, base, &base_tast);
        let range = self.expr_range(index_expr_id);
        let (index_tast, result_ty) = match &base_ty {
            tast::Ty::TArray { elem, .. } | tast::Ty::TVec { elem } | tast::Ty::TSlice { elem } => {
                (
                    self.check_expr(genv, local_env, diagnostics, index, &tast::Ty::TInt32),
                    elem.as_ref().clone(),
                )
            }
            tast::Ty::THashMap { key, value } => (
                self.check_expr(genv, local_env, diagnostics, index, key.as_ref()),
                option_ty("Option", value.as_ref().clone()),
            ),
            tast::Ty::TVar(_) => {
                super::util::push_error_with_range(
                    diagnostics,
                    "cannot infer indexed container type; add a type annotation",
                    range,
                );
                (
                    self.infer_expr(genv, local_env, diagnostics, index),
                    self.fresh_ty_var(),
                )
            }
            _ => {
                super::util::push_error_with_range(
                    diagnostics,
                    format!(
                        "type {} does not support indexing",
                        super::util::format_ty_for_diag(&base_ty)
                    ),
                    range,
                );
                (
                    self.infer_expr(genv, local_env, diagnostics, index),
                    self.fresh_ty_var(),
                )
            }
        };

        tast::Expr::EIndex {
            base: Box::new(base_tast),
            index: Box::new(index_tast),
            ty: result_ty,
            astptr: self.hir_table.expr_ptr(index_expr_id),
        }
    }

    fn infer_assign_target(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        target: hir::ExprId,
    ) -> (tast::Expr, tast::Ty) {
        match self.hir_table.expr(target).clone() {
            hir::Expr::ENameRef { res, hint, astptr } => match res {
                hir::NameRef::Local(local_id) => {
                    let target_ty = local_env
                        .lookup_var(local_id)
                        .unwrap_or_else(|| self.fresh_ty_var());
                    if !self.hir_table.local_is_mutable(local_id) {
                        diagnostics.push(
                            Diagnostic::new(
                                Stage::Typer,
                                Severity::Error,
                                format!("cannot assign to immutable binding `{}`", hint),
                            )
                            .with_range(astptr.map(|ptr| ptr.text_range())),
                        );
                    }
                    let target_expr = tast::Expr::EVar {
                        name: self.hir_table.local_ident_name(local_id),
                        ty: target_ty.clone(),
                        astptr,
                    };
                    self.record_expr_result(target, &target_expr);
                    self.record_name_ref_elab(target, &target_expr);
                    (target_expr, target_ty)
                }
                _ => {
                    diagnostics.push(
                        Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            format!(
                                "assignment target `{}` is not a mutable local binding",
                                hint
                            ),
                        )
                        .with_range(astptr.map(|ptr| ptr.text_range())),
                    );
                    (
                        self.infer_expr(genv, local_env, diagnostics, target),
                        self.fresh_ty_var(),
                    )
                }
            },
            hir::Expr::EIndex { base, index } => {
                self.infer_assign_index_target(genv, local_env, diagnostics, target, base, index)
            }
            _ => {
                let target_tast = self.infer_expr(genv, local_env, diagnostics, target);
                diagnostics.push(
                    Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        "unsupported assignment target",
                    )
                    .with_range(self.expr_range(target)),
                );
                let value_ty = target_tast.get_ty();
                (target_tast, value_ty)
            }
        }
    }

    fn infer_assign_index_target(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        target_expr_id: hir::ExprId,
        base: hir::ExprId,
        index: hir::ExprId,
    ) -> (tast::Expr, tast::Ty) {
        let base_tast = self.infer_expr(genv, local_env, diagnostics, base);
        let base_ty = self.resolve_index_base_ty(diagnostics, base, &base_tast);
        let range = self.expr_range(target_expr_id);
        let (index_tast, read_ty, value_ty) = match &base_ty {
            tast::Ty::TArray { elem, .. } => {
                self.validate_array_assignment_target(local_env, diagnostics, target_expr_id);
                let elem_ty = elem.as_ref().clone();
                (
                    self.check_expr(genv, local_env, diagnostics, index, &tast::Ty::TInt32),
                    elem_ty.clone(),
                    elem_ty,
                )
            }
            tast::Ty::TVec { elem } => {
                let elem_ty = elem.as_ref().clone();
                (
                    self.check_expr(genv, local_env, diagnostics, index, &tast::Ty::TInt32),
                    elem_ty.clone(),
                    elem_ty,
                )
            }
            tast::Ty::TSlice { elem } => {
                super::util::push_error_with_range(
                    diagnostics,
                    "cannot assign through Slice indexing; Slice is read-only",
                    range,
                );
                let elem_ty = elem.as_ref().clone();
                (
                    self.check_expr(genv, local_env, diagnostics, index, &tast::Ty::TInt32),
                    elem_ty.clone(),
                    elem_ty,
                )
            }
            tast::Ty::THashMap { key, value } => {
                let value_ty = value.as_ref().clone();
                (
                    self.check_expr(genv, local_env, diagnostics, index, key.as_ref()),
                    option_ty("Option", value_ty.clone()),
                    value_ty,
                )
            }
            tast::Ty::TVar(_) => {
                super::util::push_error_with_range(
                    diagnostics,
                    "cannot infer indexed container type for assignment; add a type annotation",
                    range,
                );
                let index_tast = self.infer_expr(genv, local_env, diagnostics, index);
                let value_ty = self.fresh_ty_var();
                (index_tast, value_ty.clone(), value_ty)
            }
            _ => {
                super::util::push_error_with_range(
                    diagnostics,
                    format!(
                        "type {} does not support indexed assignment",
                        super::util::format_ty_for_diag(&base_ty)
                    ),
                    range,
                );
                let index_tast = self.infer_expr(genv, local_env, diagnostics, index);
                let value_ty = self.fresh_ty_var();
                (index_tast, value_ty.clone(), value_ty)
            }
        };

        (
            tast::Expr::EIndex {
                base: Box::new(base_tast),
                index: Box::new(index_tast),
                ty: read_ty,
                astptr: self.hir_table.expr_ptr(target_expr_id),
            },
            value_ty,
        )
    }

    fn validate_array_assignment_target(
        &mut self,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        expr_id: hir::ExprId,
    ) {
        match self.classify_array_assign_root(expr_id) {
            Some(ArrayAssignRoot::Local(local_id)) => {
                if !self.hir_table.local_is_mutable(local_id) {
                    diagnostics.push(
                        Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            format!(
                                "cannot assign through array indexing on immutable binding `{}`",
                                self.hir_table.local_ident_name(local_id)
                            ),
                        )
                        .with_range(self.expr_range(expr_id)),
                    );
                }
                let _ = local_env.lookup_var(local_id);
            }
            Some(ArrayAssignRoot::Ref) => {}
            None => {
                diagnostics.push(
                    Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        "array indexed assignment requires a writable root such as a mutable local or `ref_get(...)`",
                    )
                    .with_range(self.expr_range(expr_id)),
                );
            }
        }
    }

    fn resolve_index_base_ty(
        &mut self,
        diagnostics: &mut Diagnostics,
        base_expr_id: hir::ExprId,
        base_tast: &tast::Expr,
    ) -> tast::Ty {
        let base_ty = self.norm(&base_tast.get_ty());
        if !matches!(base_ty, tast::Ty::TVar(_)) {
            return base_ty;
        }

        if let tast::Expr::ECall { func, .. } = base_tast {
            let func_ty = self.norm(&func.get_ty());
            if let tast::Ty::TFunc { ret_ty, .. } = func_ty
                && !matches!(ret_ty.as_ref(), tast::Ty::TVar(_))
            {
                let inferred = ret_ty.as_ref().clone();
                let _ = self.unify(
                    diagnostics,
                    &base_tast.get_ty(),
                    &inferred,
                    self.expr_range(base_expr_id),
                );
                return self.norm(&inferred);
            }
        }

        base_ty
    }

    fn classify_array_assign_root(&self, expr_id: hir::ExprId) -> Option<ArrayAssignRoot> {
        match self.hir_table.expr(expr_id) {
            hir::Expr::ENameRef {
                res: hir::NameRef::Local(local_id),
                ..
            } => Some(ArrayAssignRoot::Local(*local_id)),
            hir::Expr::EProj { tuple, .. } | hir::Expr::EField { expr: tuple, .. } => {
                self.classify_array_assign_root(*tuple)
            }
            hir::Expr::EIndex { base, .. } => self.classify_array_assign_root(*base),
            hir::Expr::ECall { func, args } => {
                if args.len() == 1
                    && let hir::Expr::ENameRef {
                        res: hir::NameRef::Builtin(hir::BuiltinId::RefGet),
                        ..
                    } = self.hir_table.expr(*func)
                {
                    Some(ArrayAssignRoot::Ref)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn infer_field_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        expr: hir::ExprId,
        field: &hir::HirIdent,
        astptr: Option<MySyntaxNodePtr>,
    ) -> tast::Expr {
        let base_tast = self.infer_expr(genv, local_env, diagnostics, expr);
        if self.expr_always_exits_loop_control(expr) {
            return tast::Expr::EField {
                expr: Box::new(base_tast),
                field_name: field.to_ident_name(),
                ty: tast::Ty::TUnit,
                astptr,
            };
        }
        let base_ty = base_tast.get_ty();
        let field_ident = tast::TastIdent(field.to_ident_name());
        let result_ty = if let Some(ty) = resolve_field_ty_eager(genv, &base_ty, &field_ident) {
            ty
        } else if !contains_tvar(&base_ty) && decompose_struct_type(&base_ty).is_none() {
            let message = if matches!(base_ty, tast::Ty::TTuple { .. }) {
                format!(
                    "Tuple type {} has no named field {}; use tuple indexing like .0",
                    super::util::format_ty_for_diag(&base_ty),
                    field_ident.0
                )
            } else {
                format!(
                    "Field {} not found on type {}",
                    field_ident.0,
                    super::util::format_ty_for_diag(&base_ty)
                )
            };
            super::util::push_error_with_range(
                diagnostics,
                message,
                astptr.map(|p| p.text_range()),
            );
            tast::Ty::TUnit
        } else {
            let result_ty = self.fresh_ty_var();
            self.push_constraint(Constraint::StructFieldAccess {
                expr_ty: base_ty.clone(),
                field: field_ident.clone(),
                result_ty: result_ty.clone(),
                origin: astptr.map(|p| p.text_range()),
            });
            result_ty
        };

        tast::Expr::EField {
            expr: Box::new(base_tast),
            field_name: field_ident.0,
            ty: result_ty,
            astptr,
        }
    }

    fn check_pat(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        pat: hir::PatId,
        ty: &tast::Ty,
    ) -> tast::Pat {
        let pat_node = self.hir_table.pat(pat).clone();
        let range = self.pat_range(pat);
        let astptr = self.pat_astptr(pat);
        let out = match pat_node {
            hir::Pat::PVar { name, astptr } => {
                self.check_pat_var(local_env, diagnostics, name, Some(astptr), ty)
            }
            hir::Pat::PUnit => self.check_pat_unit(astptr),
            hir::Pat::PBool { value } => self.check_pat_bool(value, astptr),
            hir::Pat::PInt { value } => self.check_pat_int(diagnostics, &value, ty, range, astptr),
            hir::Pat::PInt8 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TInt8, ty, range, astptr)
            }
            hir::Pat::PInt16 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TInt16, ty, range, astptr)
            }
            hir::Pat::PInt32 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TInt32, ty, range, astptr)
            }
            hir::Pat::PInt64 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TInt64, ty, range, astptr)
            }
            hir::Pat::PUInt8 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TUint8, ty, range, astptr)
            }
            hir::Pat::PUInt16 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TUint16, ty, range, astptr)
            }
            hir::Pat::PUInt32 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TUint32, ty, range, astptr)
            }
            hir::Pat::PUInt64 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TUint64, ty, range, astptr)
            }
            hir::Pat::PFloat { value } => {
                self.check_pat_float(diagnostics, &value, ty, range, astptr)
            }
            hir::Pat::PFloat32 { value } => self.check_pat_typed_float(
                diagnostics,
                &value,
                &tast::Ty::TFloat32,
                ty,
                range,
                astptr,
            ),
            hir::Pat::PFloat64 { value } => self.check_pat_typed_float(
                diagnostics,
                &value,
                &tast::Ty::TFloat64,
                ty,
                range,
                astptr,
            ),
            hir::Pat::PString { value } => self.check_pat_string(&value, ty, range, astptr),
            hir::Pat::PChar { value } => {
                self.check_pat_char(diagnostics, value.as_str(), ty, range, astptr)
            }
            hir::Pat::PConstr { .. } => {
                self.check_pat_constructor(genv, local_env, diagnostics, pat, ty)
            }
            hir::Pat::PStruct { .. } => {
                self.check_pat_constructor(genv, local_env, diagnostics, pat, ty)
            }
            hir::Pat::PTuple { pats } => {
                self.check_pat_tuple(genv, local_env, diagnostics, pat, &pats, ty)
            }
            hir::Pat::PWild => self.check_pat_wild(ty, astptr),
        };
        self.results.record_pat_ty(pat, out.get_ty());
        out
    }

    fn check_pat_var(
        &mut self,
        local_env: &mut LocalTypeEnv,
        _diagnostics: &mut Diagnostics,
        name: hir::LocalId,
        astptr: Option<MySyntaxNodePtr>,
        ty: &tast::Ty,
    ) -> tast::Pat {
        local_env.insert_var(name, ty.clone());
        self.results.record_local_ty(name, ty.clone());
        let name_str = self.hir_table.local_ident_name(name);
        tast::Pat::PVar {
            name: name_str,
            ty: ty.clone(),
            astptr,
        }
    }

    fn check_pat_unit(&self, astptr: Option<MySyntaxNodePtr>) -> tast::Pat {
        tast::Pat::PPrim {
            value: Prim::Unit { value: () },
            ty: tast::Ty::TUnit,
            astptr,
        }
    }

    fn check_pat_bool(&self, value: bool, astptr: Option<MySyntaxNodePtr>) -> tast::Pat {
        tast::Pat::PPrim {
            value: Prim::boolean(value),
            ty: tast::Ty::TBool,
            astptr,
        }
    }

    fn check_pat_int(
        &mut self,
        diagnostics: &mut Diagnostics,
        value: &str,
        ty: &tast::Ty,
        range: Option<TextRange>,
        astptr: Option<MySyntaxNodePtr>,
    ) -> tast::Pat {
        let target_ty = integer_literal_target(ty).unwrap_or(tast::Ty::TInt32);
        let prim = self
            .parse_integer_literal_with_ty(diagnostics, value, &target_ty, range)
            .unwrap_or_else(|| Prim::zero_for_int_ty(&target_ty));
        self.push_constraint(Constraint::TypeEqual(target_ty.clone(), ty.clone(), range));
        tast::Pat::PPrim {
            value: prim,
            ty: ty.clone(),
            astptr,
        }
    }

    fn check_pat_typed_int(
        &mut self,
        diagnostics: &mut Diagnostics,
        value: &str,
        literal_ty: &tast::Ty,
        expected_ty: &tast::Ty,
        range: Option<TextRange>,
        astptr: Option<MySyntaxNodePtr>,
    ) -> tast::Pat {
        let prim = self
            .parse_integer_literal_with_ty(diagnostics, value, literal_ty, range)
            .unwrap_or_else(|| Prim::zero_for_int_ty(literal_ty));
        self.push_constraint(Constraint::TypeEqual(
            literal_ty.clone(),
            expected_ty.clone(),
            range,
        ));
        tast::Pat::PPrim {
            value: prim,
            ty: literal_ty.clone(),
            astptr,
        }
    }

    fn check_pat_float(
        &mut self,
        diagnostics: &mut Diagnostics,
        value: &str,
        ty: &tast::Ty,
        range: Option<TextRange>,
        astptr: Option<MySyntaxNodePtr>,
    ) -> tast::Pat {
        let target_ty = if is_float_ty(ty) {
            ty.clone()
        } else {
            tast::Ty::TFloat64
        };
        let prim = self
            .parse_float_literal_with_ty(diagnostics, value, &target_ty, range)
            .unwrap_or(Prim::Float64 { value: 0.0 });
        self.push_constraint(Constraint::TypeEqual(target_ty.clone(), ty.clone(), range));
        tast::Pat::PPrim {
            value: prim,
            ty: ty.clone(),
            astptr,
        }
    }

    fn check_pat_typed_float(
        &mut self,
        diagnostics: &mut Diagnostics,
        value: &str,
        literal_ty: &tast::Ty,
        expected_ty: &tast::Ty,
        range: Option<TextRange>,
        astptr: Option<MySyntaxNodePtr>,
    ) -> tast::Pat {
        let prim = self
            .parse_float_literal_with_ty(diagnostics, value, literal_ty, range)
            .unwrap_or(Prim::Float64 { value: 0.0 });
        self.push_constraint(Constraint::TypeEqual(
            literal_ty.clone(),
            expected_ty.clone(),
            range,
        ));
        tast::Pat::PPrim {
            value: prim,
            ty: literal_ty.clone(),
            astptr,
        }
    }

    fn check_pat_string(
        &mut self,
        value: &String,
        ty: &tast::Ty,
        range: Option<TextRange>,
        astptr: Option<MySyntaxNodePtr>,
    ) -> tast::Pat {
        self.push_constraint(Constraint::TypeEqual(tast::Ty::TString, ty.clone(), range));
        tast::Pat::PPrim {
            value: Prim::string(value.to_owned()),
            ty: tast::Ty::TString,
            astptr,
        }
    }

    fn check_pat_char(
        &mut self,
        diagnostics: &mut Diagnostics,
        value: &str,
        ty: &tast::Ty,
        range: Option<TextRange>,
        astptr: Option<MySyntaxNodePtr>,
    ) -> tast::Pat {
        self.push_constraint(Constraint::TypeEqual(tast::Ty::TChar, ty.clone(), range));
        let ch = self
            .parse_char_literal(diagnostics, value, range)
            .unwrap_or('\0');
        tast::Pat::PPrim {
            value: Prim::character(ch),
            ty: tast::Ty::TChar,
            astptr,
        }
    }

    fn check_pat_constructor(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        pat: hir::PatId,
        ty: &tast::Ty,
    ) -> tast::Pat {
        let pat_node = self.hir_table.pat(pat);
        let kind = pat_node.clone();
        match kind {
            hir::Pat::PConstr {
                constructor: constructor_ref,
                args,
            } => {
                let constructor_path = match &constructor_ref {
                    hir::ConstructorRef::Resolved(ctor_id) => {
                        self.constructor_path_from_id(diagnostics, ctor_id)
                    }
                    hir::ConstructorRef::Unresolved(path) => path.clone(),
                    hir::ConstructorRef::Ambiguous { path, .. } => {
                        super::util::push_error_with_range(
                            diagnostics,
                            format!("Ambiguous constructor {}", path.display()),
                            self.pat_range(pat),
                        );
                        return self.check_pat_wild(ty, self.pat_astptr(pat));
                    }
                };

                let Some(variant_ident) = constructor_path.last_ident() else {
                    super::util::push_ice(diagnostics, "Constructor path missing final segment");
                    return self.check_pat_wild(ty, self.pat_astptr(pat));
                };
                let variant_name = tast::TastIdent(variant_ident.clone());
                let namespace = constructor_path.namespace_segments();
                let (enum_ident, enum_env) = if namespace.is_empty() {
                    (None, genv.current())
                } else {
                    let name = namespace
                        .iter()
                        .map(|seg| seg.seg().clone())
                        .collect::<Vec<_>>()
                        .join("::");
                    let (resolved, env) = super::util::resolve_type_name(genv, &name);
                    (Some(tast::TastIdent(resolved)), env)
                };
                let ctor =
                    enum_env.lookup_constructor_with_namespace(enum_ident.as_ref(), &variant_name);
                let Some((constructor, constr_ty)) = ctor else {
                    super::util::push_error_with_range(
                        diagnostics,
                        format!(
                            "Constructor {} not found in environment",
                            constructor_path.display()
                        ),
                        self.pat_range(pat),
                    );
                    return self.check_pat_wild(ty, self.pat_astptr(pat));
                };

                let expected_arity = match &constructor {
                    common::Constructor::Enum(enum_constructor) => {
                        let def = enum_env.enums().get(&enum_constructor.type_name);
                        let Some(def) = def else {
                            super::util::push_ice(
                                diagnostics,
                                format!(
                                    "Enum {} not found when checking constructor {}",
                                    enum_constructor.type_name.0,
                                    constructor.name().0
                                ),
                            );
                            return self.check_pat_wild(ty, self.pat_astptr(pat));
                        };
                        let variant = def.variants.get(enum_constructor.index);
                        let Some((_, tys)) = variant else {
                            super::util::push_ice(
                                diagnostics,
                                format!(
                                    "Enum {} variant index {} out of bounds",
                                    enum_constructor.type_name.0, enum_constructor.index
                                ),
                            );
                            return self.check_pat_wild(ty, self.pat_astptr(pat));
                        };
                        tys.len()
                    }
                    common::Constructor::Struct(_) => {
                        super::util::push_error_with_range(
                            diagnostics,
                            format!(
                                "Struct {} patterns must use field syntax",
                                constructor.name().0
                            ),
                            self.pat_range(pat),
                        );
                        return self.check_pat_wild(ty, self.pat_astptr(pat));
                    }
                };

                if expected_arity != args.len() {
                    super::util::push_error_with_range(
                        diagnostics,
                        format!(
                            "Constructor {} expects {} arguments, but got {}",
                            constructor.name().0,
                            expected_arity,
                            args.len()
                        ),
                        self.pat_range(pat),
                    );
                    return self.check_pat_wild(ty, self.pat_astptr(pat));
                }

                let inst_constr_ty = self.inst_ty(&constr_ty);
                let (param_tys, ret_ty) = match &inst_constr_ty {
                    tast::Ty::TFunc { params, ret_ty } => (params.clone(), ret_ty.as_ref().clone()),
                    ty => (Vec::new(), ty.clone()),
                };

                let mut args_tast = Vec::with_capacity(args.len());
                for (idx, arg_ast) in args.iter().enumerate() {
                    let expected_ty = param_tys
                        .get(idx)
                        .cloned()
                        .unwrap_or_else(|| self.fresh_ty_var());
                    let arg_tast =
                        self.check_pat(genv, local_env, diagnostics, *arg_ast, &expected_ty);
                    args_tast.push(arg_tast);
                }

                self.push_constraint(Constraint::TypeEqual(
                    ret_ty.clone(),
                    ty.clone(),
                    self.pat_range(pat),
                ));

                self.results
                    .record_constructor_pat(pat, constructor.clone());
                tast::Pat::PConstr {
                    constructor,
                    args: args_tast,
                    ty: ret_ty,
                    astptr: self.pat_astptr(pat),
                }
            }
            hir::Pat::PStruct { name, fields } => {
                let name_display = name.display();
                let (type_name, type_env) = super::util::resolve_type_name(genv, &name_display);
                let struct_def = type_env.structs().get(&tast::TastIdent(type_name.clone()));
                let Some(struct_def) = struct_def else {
                    super::util::push_error_with_range(
                        diagnostics,
                        format!("Struct {} not found when checking pattern", type_name),
                        self.pat_range(pat),
                    );
                    return self.check_pat_wild(ty, self.pat_astptr(pat));
                };
                let struct_fields = struct_def.fields.clone();
                if struct_fields.len() != fields.len() {
                    super::util::push_error_with_range(
                        diagnostics,
                        format!(
                            "Struct pattern {} expects {} fields, but got {}",
                            type_name,
                            struct_fields.len(),
                            fields.len()
                        ),
                        self.pat_range(pat),
                    );
                }

                let mut field_map: HashMap<String, hir::PatId> = HashMap::new();
                for (fname, pat_id) in fields.iter() {
                    if field_map.insert(fname.to_ident_name(), *pat_id).is_some() {
                        super::util::push_error_with_range(
                            diagnostics,
                            format!(
                                "Struct pattern {} has duplicate field {}",
                                type_name,
                                fname.to_ident_name()
                            ),
                            self.pat_range(pat),
                        );
                    }
                }

                let ctor = type_env.lookup_constructor(&tast::TastIdent(type_name.clone()));
                let Some((constructor, constr_ty)) = ctor else {
                    super::util::push_ice(
                        diagnostics,
                        format!("Struct {} not found when checking constructor", type_name),
                    );
                    return self.check_pat_wild(ty, self.pat_astptr(pat));
                };

                let inst_constr_ty = self.inst_ty(&constr_ty);
                let (param_tys, ret_ty) = match &inst_constr_ty {
                    tast::Ty::TFunc { params, ret_ty } => (params.clone(), ret_ty.as_ref().clone()),
                    ty => (Vec::new(), ty.clone()),
                };

                if !param_tys.is_empty() && param_tys.len() != struct_fields.len() {
                    super::util::push_ice(
                        diagnostics,
                        format!(
                            "Constructor {} expects {} fields, but got {}",
                            type_name,
                            param_tys.len(),
                            struct_fields.len()
                        ),
                    );
                }

                let mut args_tast = Vec::with_capacity(struct_fields.len());
                let mut elab_args = Vec::with_capacity(struct_fields.len());
                for (idx, (field_name, _)) in struct_fields.iter().enumerate() {
                    let expected_ty = param_tys
                        .get(idx)
                        .cloned()
                        .unwrap_or_else(|| self.fresh_ty_var());
                    let pat_id = field_map.remove(&field_name.0);
                    let pat_tast = match pat_id {
                        Some(pat_id) => {
                            elab_args.push(StructPatArgElab::Pat(pat_id));
                            self.check_pat(genv, local_env, diagnostics, pat_id, &expected_ty)
                        }
                        None => {
                            super::util::push_error_with_range(
                                diagnostics,
                                format!(
                                    "Struct pattern {} missing field {}",
                                    name_display, field_name.0
                                ),
                                self.pat_range(pat),
                            );
                            let wild = self.check_pat_wild(&expected_ty, None);
                            elab_args.push(StructPatArgElab::MissingWild {
                                expected_ty: wild.get_ty(),
                            });
                            wild
                        }
                    };
                    args_tast.push(pat_tast);
                }

                if !field_map.is_empty() {
                    let extra = field_map.keys().cloned().collect::<Vec<_>>().join(", ");
                    super::util::push_error_with_range(
                        diagnostics,
                        format!(
                            "Struct pattern {} has unknown fields: {}",
                            name_display, extra
                        ),
                        self.pat_range(pat),
                    );
                }

                self.push_constraint(Constraint::TypeEqual(
                    ret_ty.clone(),
                    ty.clone(),
                    self.pat_range(pat),
                ));

                self.results.record_struct_pat_elab(
                    pat,
                    StructPatElab {
                        constructor: constructor.clone(),
                        args: elab_args,
                    },
                );
                tast::Pat::PConstr {
                    constructor,
                    args: args_tast,
                    ty: ret_ty,
                    astptr: self.pat_astptr(pat),
                }
            }
            _ => {
                super::util::push_ice(diagnostics, "Expected constructor pattern");
                self.check_pat_wild(ty, self.pat_astptr(pat))
            }
        }
    }

    fn check_irrefutable_let_pattern(&mut self, diagnostics: &mut Diagnostics, pat_id: hir::PatId) {
        if self.is_irrefutable_pat(pat_id) {
            return;
        }
        diagnostics.push(
            Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                "Refutable pattern is not allowed in let binding; use match instead".to_string(),
            )
            .with_range(self.pat_range(pat_id)),
        );
    }

    fn is_irrefutable_pat(&self, pat_id: hir::PatId) -> bool {
        match self.hir_table.pat(pat_id) {
            hir::Pat::PVar { .. } | hir::Pat::PWild | hir::Pat::PUnit => true,
            hir::Pat::PTuple { pats } => pats.iter().all(|pat| self.is_irrefutable_pat(*pat)),
            hir::Pat::PStruct { fields, .. } => {
                fields.iter().all(|(_, pat)| self.is_irrefutable_pat(*pat))
            }
            hir::Pat::PBool { .. }
            | hir::Pat::PInt { .. }
            | hir::Pat::PInt8 { .. }
            | hir::Pat::PInt16 { .. }
            | hir::Pat::PInt32 { .. }
            | hir::Pat::PInt64 { .. }
            | hir::Pat::PUInt8 { .. }
            | hir::Pat::PUInt16 { .. }
            | hir::Pat::PUInt32 { .. }
            | hir::Pat::PUInt64 { .. }
            | hir::Pat::PFloat { .. }
            | hir::Pat::PFloat32 { .. }
            | hir::Pat::PFloat64 { .. }
            | hir::Pat::PString { .. }
            | hir::Pat::PChar { .. }
            | hir::Pat::PConstr { .. } => false,
        }
    }

    fn check_pat_tuple(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        pat_id: hir::PatId,
        pats: &[hir::PatId],
        ty: &tast::Ty,
    ) -> tast::Pat {
        let expected_elem_tys: Vec<tast::Ty> = match ty {
            tast::Ty::TTuple { typs } if typs.len() == pats.len() => typs.clone(),
            _ => (0..pats.len()).map(|_| self.fresh_ty_var()).collect(),
        };

        let mut pats_tast = Vec::new();
        let mut pat_typs = Vec::new();
        for (pat, expected_ty) in pats.iter().zip(expected_elem_tys.iter()) {
            let pat_tast = self.check_pat(genv, local_env, diagnostics, *pat, expected_ty);
            pat_typs.push(pat_tast.get_ty());
            pats_tast.push(pat_tast);
        }
        let pat_ty = tast::Ty::TTuple { typs: pat_typs };
        self.push_constraint(Constraint::TypeEqual(pat_ty.clone(), ty.clone(), None));
        tast::Pat::PTuple {
            items: pats_tast,
            ty: pat_ty,
            astptr: self.pat_astptr(pat_id),
        }
    }

    fn check_pat_wild(&mut self, ty: &tast::Ty, astptr: Option<MySyntaxNodePtr>) -> tast::Pat {
        let pat_ty = self.fresh_ty_var();
        self.push_constraint(Constraint::TypeEqual(pat_ty.clone(), ty.clone(), None));
        tast::Pat::PWild { ty: pat_ty, astptr }
    }
}

fn is_concrete_dyn_target(ty: &tast::Ty) -> bool {
    match ty {
        tast::Ty::TVar(_) | tast::Ty::TParam { .. } => false,
        tast::Ty::TTuple { typs } => typs.iter().all(is_concrete_dyn_target),
        tast::Ty::TApp { ty, args } => {
            is_concrete_dyn_target(ty) && args.iter().all(is_concrete_dyn_target)
        }
        tast::Ty::TArray { elem, .. } => is_concrete_dyn_target(elem),
        tast::Ty::TSlice { elem } => is_concrete_dyn_target(elem),
        tast::Ty::TVec { elem } => is_concrete_dyn_target(elem),
        tast::Ty::TRef { elem } => is_concrete_dyn_target(elem),
        tast::Ty::THashMap { key, value } => {
            is_concrete_dyn_target(key) && is_concrete_dyn_target(value)
        }
        tast::Ty::TFunc { params, ret_ty } => {
            params.iter().all(is_concrete_dyn_target) && is_concrete_dyn_target(ret_ty)
        }
        tast::Ty::TEnum { .. }
        | tast::Ty::TStruct { .. }
        | tast::Ty::TDyn { .. }
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
        | tast::Ty::TChar => true,
    }
}

fn tparam_has_trait_bound(local_env: &LocalTypeEnv, tparam_name: &str, trait_name: &str) -> bool {
    local_env
        .tparam_trait_bounds(tparam_name)
        .is_some_and(|bounds| bounds.iter().any(|bound| bound.0 == trait_name))
}

fn is_concrete_trait_impl_target(ty: &tast::Ty) -> bool {
    !matches!(ty, tast::Ty::TDyn { .. }) && is_concrete_dyn_target(ty)
}

fn resolve_trait_name_or_report(
    genv: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    trait_name: &str,
    range: Option<TextRange>,
) -> Option<String> {
    let Some((resolved, _env)) = super::util::resolve_trait_name(genv, trait_name) else {
        diagnostics.push(
            Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!("Unknown trait {}", trait_name),
            )
            .with_range(range),
        );
        return None;
    };
    Some(resolved)
}

fn resolve_required_trait_name_or_report(
    genv: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    required_trait: &tast::TastIdent,
    range: Option<TextRange>,
) -> Option<String> {
    if let Some((resolved, _env)) = super::util::resolve_trait_name(genv, &required_trait.0) {
        return Some(resolved);
    }
    if required_trait.0.contains("::") {
        return Some(required_trait.0.clone());
    }
    diagnostics.push(
        Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!("Unknown trait {}", required_trait.0),
        )
        .with_range(range),
    );
    None
}

fn integer_literal_target(expected: &tast::Ty) -> Option<tast::Ty> {
    if is_integer_ty(expected) {
        Some(expected.clone())
    } else {
        None
    }
}

fn is_integer_ty(ty: &tast::Ty) -> bool {
    matches!(
        ty,
        tast::Ty::TInt8
            | tast::Ty::TInt16
            | tast::Ty::TInt32
            | tast::Ty::TInt64
            | tast::Ty::TUint8
            | tast::Ty::TUint16
            | tast::Ty::TUint32
            | tast::Ty::TUint64
    )
}

fn is_float_ty(ty: &tast::Ty) -> bool {
    matches!(ty, tast::Ty::TFloat32 | tast::Ty::TFloat64)
}

fn is_numeric_ty(ty: &tast::Ty) -> bool {
    is_integer_ty(ty) || is_float_ty(ty)
}

fn is_signed_numeric_ty(ty: &tast::Ty) -> bool {
    matches!(
        ty,
        tast::Ty::TInt8
            | tast::Ty::TInt16
            | tast::Ty::TInt32
            | tast::Ty::TInt64
            | tast::Ty::TFloat32
            | tast::Ty::TFloat64
    )
}

impl Typer {
    pub(crate) fn validate_deferred_arithmetic_checks(&mut self, diagnostics: &mut Diagnostics) {
        let checks = std::mem::take(&mut self.deferred_arithmetic_checks);
        for check in checks {
            let norm_ty = self.norm(&check.ty);
            let valid = match check.kind {
                super::ArithmeticKind::NumericOrString => {
                    is_numeric_ty(&norm_ty) || matches!(norm_ty, tast::Ty::TString)
                }
                super::ArithmeticKind::Numeric => is_numeric_ty(&norm_ty),
            };
            if !valid && !matches!(norm_ty, tast::Ty::TVar(..)) {
                super::util::push_error_with_range(
                    diagnostics,
                    format!(
                        "Operator {} is not defined for type {}",
                        check.op,
                        super::util::format_ty_for_diag(&norm_ty)
                    ),
                    check.origin,
                );
            }
        }
    }
}

impl Typer {
    fn parse_integer_literal_with_ty(
        &mut self,
        diagnostics: &mut Diagnostics,
        literal: &str,
        ty: &tast::Ty,
        range: Option<TextRange>,
    ) -> Option<Prim> {
        match ty {
            tast::Ty::TInt8 => self
                .parse_signed_integer(diagnostics, literal, "int8", range)
                .map(|value| Prim::Int8 { value }),
            tast::Ty::TInt16 => self
                .parse_signed_integer(diagnostics, literal, "int16", range)
                .map(|value| Prim::Int16 { value }),
            tast::Ty::TInt32 => self
                .parse_signed_integer(diagnostics, literal, "int32", range)
                .map(|value| Prim::Int32 { value }),
            tast::Ty::TInt64 => self
                .parse_signed_integer(diagnostics, literal, "int64", range)
                .map(|value| Prim::Int64 { value }),
            tast::Ty::TUint8 => self
                .parse_unsigned_integer(diagnostics, literal, "uint8", range)
                .map(|value| Prim::UInt8 { value }),
            tast::Ty::TUint16 => self
                .parse_unsigned_integer(diagnostics, literal, "uint16", range)
                .map(|value| Prim::UInt16 { value }),
            tast::Ty::TUint32 => self
                .parse_unsigned_integer(diagnostics, literal, "uint32", range)
                .map(|value| Prim::UInt32 { value }),
            tast::Ty::TUint64 => self
                .parse_unsigned_integer(diagnostics, literal, "uint64", range)
                .map(|value| Prim::UInt64 { value }),
            _ => None,
        }
    }

    fn parse_char_literal(
        &mut self,
        diagnostics: &mut Diagnostics,
        literal: &str,
        range: Option<TextRange>,
    ) -> Option<char> {
        if literal.is_empty() {
            diagnostics.push(
                Diagnostic::new(
                    diagnostics::Stage::Typer,
                    diagnostics::Severity::Error,
                    "Char literal cannot be empty".to_string(),
                )
                .with_range(range),
            );
            return None;
        }

        if let Some(rest) = literal.strip_prefix('\\') {
            let mut chars = rest.chars();
            let Some(tag) = chars.next() else {
                self.report_invalid_char_literal(diagnostics, literal, range);
                return None;
            };

            let out = match tag {
                '\'' => Some('\''),
                '"' => Some('"'),
                '\\' => Some('\\'),
                '/' => Some('/'),
                'b' => Some('\u{0008}'),
                'f' => Some('\u{000C}'),
                'n' => Some('\n'),
                'r' => Some('\r'),
                't' => Some('\t'),
                'u' => {
                    let hex: String = chars.by_ref().take(4).collect();
                    if hex.chars().count() != 4 || chars.next().is_some() {
                        None
                    } else if let Ok(code) = u32::from_str_radix(&hex, 16) {
                        char::from_u32(code)
                    } else {
                        None
                    }
                }
                _ => None,
            };

            if let Some(ch) = out
                && chars.next().is_none()
            {
                return Some(ch);
            }

            self.report_invalid_char_literal(diagnostics, literal, range);
            return None;
        }

        let mut iter = literal.chars();
        let first = iter.next();
        let second = iter.next();
        match (first, second) {
            (Some(ch), None) => Some(ch),
            _ => {
                self.report_invalid_char_literal(diagnostics, literal, range);
                None
            }
        }
    }

    fn parse_signed_integer<T>(
        &mut self,
        diagnostics: &mut Diagnostics,
        literal: &str,
        ty_name: &str,
        range: Option<TextRange>,
    ) -> Option<T>
    where
        T: std::str::FromStr<Err = std::num::ParseIntError>,
    {
        match literal.parse::<T>() {
            Ok(value) => Some(value),
            Err(err) => {
                match err.kind() {
                    IntErrorKind::Empty | IntErrorKind::InvalidDigit => {
                        self.report_invalid_integer_literal(diagnostics, literal, range);
                    }
                    _ => {
                        diagnostics.push(
                            Diagnostic::new(
                                diagnostics::Stage::Typer,
                                diagnostics::Severity::Error,
                                format!("Integer literal {} does not fit in {}", literal, ty_name),
                            )
                            .with_range(range),
                        );
                    }
                }
                None
            }
        }
    }

    fn parse_unsigned_integer<T>(
        &mut self,
        diagnostics: &mut Diagnostics,
        literal: &str,
        ty_name: &str,
        range: Option<TextRange>,
    ) -> Option<T>
    where
        T: std::str::FromStr<Err = std::num::ParseIntError>,
    {
        if literal.starts_with('-') {
            diagnostics.push(
                Diagnostic::new(
                    diagnostics::Stage::Typer,
                    diagnostics::Severity::Error,
                    format!("Integer literal {} does not fit in {}", literal, ty_name),
                )
                .with_range(range),
            );
            return None;
        }

        match literal.parse::<T>() {
            Ok(value) => Some(value),
            Err(err) => {
                match err.kind() {
                    IntErrorKind::Empty | IntErrorKind::InvalidDigit => {
                        self.report_invalid_integer_literal(diagnostics, literal, range);
                    }
                    _ => {
                        diagnostics.push(
                            Diagnostic::new(
                                diagnostics::Stage::Typer,
                                diagnostics::Severity::Error,
                                format!("Integer literal {} does not fit in {}", literal, ty_name),
                            )
                            .with_range(range),
                        );
                    }
                }
                None
            }
        }
    }

    fn report_invalid_integer_literal(
        &mut self,
        diagnostics: &mut Diagnostics,
        literal: &str,
        range: Option<TextRange>,
    ) {
        diagnostics.push(
            Diagnostic::new(
                diagnostics::Stage::Typer,
                diagnostics::Severity::Error,
                format!("Invalid integer literal: {}", literal),
            )
            .with_range(range),
        );
    }

    fn report_invalid_char_literal(
        &mut self,
        diagnostics: &mut Diagnostics,
        literal: &str,
        range: Option<TextRange>,
    ) {
        diagnostics.push(
            Diagnostic::new(
                diagnostics::Stage::Typer,
                diagnostics::Severity::Error,
                format!("Invalid char literal: {}", literal),
            )
            .with_range(range),
        );
    }

    fn ensure_float_literal_fits(
        &mut self,
        diagnostics: &mut Diagnostics,
        value: f64,
        ty: &tast::Ty,
        range: Option<TextRange>,
    ) {
        if !value.is_finite() {
            diagnostics.push(
                Diagnostic::new(
                    diagnostics::Stage::Typer,
                    diagnostics::Severity::Error,
                    "Float literal must be finite".to_string(),
                )
                .with_range(range),
            );
            return;
        }

        match ty {
            tast::Ty::TFloat32 => {
                if value < f32::MIN as f64 || value > f32::MAX as f64 {
                    diagnostics.push(
                        Diagnostic::new(
                            diagnostics::Stage::Typer,
                            diagnostics::Severity::Error,
                            format!("Float literal {} does not fit in float32", value),
                        )
                        .with_range(range),
                    );
                }
            }
            tast::Ty::TFloat64 => {
                // Any finite f64 literal fits.
            }
            _ => {}
        }
    }

    fn parse_float_literal_with_ty(
        &mut self,
        diagnostics: &mut Diagnostics,
        literal: &str,
        ty: &tast::Ty,
        range: Option<TextRange>,
    ) -> Option<Prim> {
        match literal.parse::<f64>() {
            Ok(value) => {
                self.ensure_float_literal_fits(diagnostics, value, ty, range);
                match ty {
                    tast::Ty::TFloat32 => Some(Prim::Float32 {
                        value: value as f32,
                    }),
                    tast::Ty::TFloat64 => Some(Prim::Float64 { value }),
                    _ => None,
                }
            }
            Err(_) => {
                diagnostics.push(
                    Diagnostic::new(
                        diagnostics::Stage::Typer,
                        diagnostics::Severity::Error,
                        format!("Invalid float literal: {}", literal),
                    )
                    .with_range(range),
                );
                None
            }
        }
    }
}

fn lookup_function_path(
    genv: &PackageTypeEnv,
    path: &hir::Path,
) -> Option<(String, crate::env::FnScheme)> {
    if path.len() == 1 {
        let name = path.last_ident()?.clone();
        return genv
            .get_function_scheme_unqualified(name.as_str())
            .map(|scheme| (name, scheme));
    }

    let full_name = path.display();
    let package = path.segments().first()?.seg().as_str();
    if package == BUILTIN_PACKAGE {
        let name = path.last_ident()?.clone();
        return genv
            .builtins()
            .get_function_scheme(name.as_str())
            .map(|scheme| (name, scheme));
    }
    if package == genv.package {
        return genv
            .current()
            .get_function_scheme(&full_name)
            .map(|scheme| (full_name, scheme));
    }
    if let Some(env) = genv.deps.get(package) {
        env.get_function_scheme(&full_name)
            .map(|scheme| (full_name, scheme))
    } else {
        eprintln!(
            "lookup_function_path: package '{}' not found when resolving function '{}'",
            package, full_name
        );
        None
    }
}

fn lookup_function_scheme_by_hint(
    genv: &PackageTypeEnv,
    hint: &str,
) -> Option<crate::env::FnScheme> {
    if let Some(func_scheme) = genv
        .current()
        .get_function_scheme(hint)
        .or_else(|| genv.builtins().get_function_scheme(hint))
    {
        return Some(func_scheme);
    }
    let segments = hint.split("::").map(|seg| seg.to_string()).collect();
    let path = hir::Path::from_idents(segments);
    lookup_function_path(genv, &path).map(|(_, scheme)| scheme)
}

fn lookup_inherent_method_for_ty(
    genv: &PackageTypeEnv,
    receiver_ty: &tast::Ty,
    method: &tast::TastIdent,
) -> Option<crate::env::FnScheme> {
    genv.lookup_visible_inherent_method_scheme(receiver_ty, method)
}

fn collect_type_param_substitution(
    template: &tast::Ty,
    actual: &tast::Ty,
    subst: &mut HashMap<String, tast::Ty>,
) {
    match template {
        tast::Ty::TParam { name } => {
            subst.entry(name.clone()).or_insert_with(|| actual.clone());
        }
        tast::Ty::TTuple { typs } => {
            if let tast::Ty::TTuple { typs: actual_typs } = actual {
                for (template_item, actual_item) in typs.iter().zip(actual_typs.iter()) {
                    collect_type_param_substitution(template_item, actual_item, subst);
                }
            }
        }
        tast::Ty::TApp { ty, args } => {
            if let tast::Ty::TApp {
                ty: actual_ty,
                args: actual_args,
            } = actual
            {
                collect_type_param_substitution(ty, actual_ty, subst);
                for (template_arg, actual_arg) in args.iter().zip(actual_args.iter()) {
                    collect_type_param_substitution(template_arg, actual_arg, subst);
                }
            }
        }
        tast::Ty::TArray { elem, .. } => {
            if let tast::Ty::TArray {
                elem: actual_elem, ..
            } = actual
            {
                collect_type_param_substitution(elem, actual_elem, subst);
            }
        }
        tast::Ty::TSlice { elem } => {
            if let tast::Ty::TSlice { elem: actual_elem } = actual {
                collect_type_param_substitution(elem, actual_elem, subst);
            }
        }
        tast::Ty::TVec { elem } => {
            if let tast::Ty::TVec { elem: actual_elem } = actual {
                collect_type_param_substitution(elem, actual_elem, subst);
            }
        }
        tast::Ty::TRef { elem } => {
            if let tast::Ty::TRef { elem: actual_elem } = actual {
                collect_type_param_substitution(elem, actual_elem, subst);
            }
        }
        tast::Ty::THashMap { key, value } => {
            if let tast::Ty::THashMap {
                key: actual_key,
                value: actual_value,
            } = actual
            {
                collect_type_param_substitution(key, actual_key, subst);
                collect_type_param_substitution(value, actual_value, subst);
            }
        }
        tast::Ty::TFunc { params, ret_ty } => {
            if let tast::Ty::TFunc {
                params: actual_params,
                ret_ty: actual_ret_ty,
            } = actual
            {
                for (template_param, actual_param) in params.iter().zip(actual_params.iter()) {
                    collect_type_param_substitution(template_param, actual_param, subst);
                }
                collect_type_param_substitution(ret_ty, actual_ret_ty, subst);
            }
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
        | tast::Ty::TDyn { .. } => {}
    }
}

pub(crate) fn resolve_field_ty_eager(
    genv: &PackageTypeEnv,
    base_ty: &tast::Ty,
    field: &tast::TastIdent,
) -> Option<tast::Ty> {
    let (type_name, type_args) = decompose_struct_type(base_ty)?;
    let (resolved, env) = super::util::resolve_type_name(genv, &type_name);
    let struct_def = env.structs().get(&tast::TastIdent::new(&resolved))?;
    if struct_def.generics.len() != type_args.len() {
        return None;
    }

    let mut subst = HashMap::new();
    for (param, arg) in struct_def.generics.iter().zip(type_args.iter()) {
        subst.insert(param.0.clone(), arg.clone());
    }

    let (_, ty) = struct_def.fields.iter().find(|(fname, _)| fname == field)?;
    Some(substitute_ty_params(ty, &subst))
}

fn decompose_struct_type(ty: &tast::Ty) -> Option<(String, Vec<tast::Ty>)> {
    match ty {
        tast::Ty::TStruct { name } => Some((name.clone(), Vec::new())),
        tast::Ty::TApp { ty: base, args } => {
            let (type_name, mut collected) = decompose_struct_type(base.as_ref())?;
            collected.extend(args.iter().cloned());
            Some((type_name, collected))
        }
        _ => None,
    }
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
                .map(|item| substitute_ty_params(item, subst))
                .collect(),
        },
        tast::Ty::TEnum { name } => tast::Ty::TEnum { name: name.clone() },
        tast::Ty::TStruct { name } => tast::Ty::TStruct { name: name.clone() },
        tast::Ty::TDyn { trait_name } => tast::Ty::TDyn {
            trait_name: trait_name.clone(),
        },
        tast::Ty::TApp { ty, args } => tast::Ty::TApp {
            ty: Box::new(substitute_ty_params(ty.as_ref(), subst)),
            args: args
                .iter()
                .map(|item| substitute_ty_params(item, subst))
                .collect(),
        },
        tast::Ty::TArray { len, elem } => tast::Ty::TArray {
            len: *len,
            elem: Box::new(substitute_ty_params(elem.as_ref(), subst)),
        },
        tast::Ty::TSlice { elem } => tast::Ty::TSlice {
            elem: Box::new(substitute_ty_params(elem.as_ref(), subst)),
        },
        tast::Ty::TVec { elem } => tast::Ty::TVec {
            elem: Box::new(substitute_ty_params(elem.as_ref(), subst)),
        },
        tast::Ty::TRef { elem } => tast::Ty::TRef {
            elem: Box::new(substitute_ty_params(elem.as_ref(), subst)),
        },
        tast::Ty::THashMap { key, value } => tast::Ty::THashMap {
            key: Box::new(substitute_ty_params(key.as_ref(), subst)),
            value: Box::new(substitute_ty_params(value.as_ref(), subst)),
        },
        tast::Ty::TParam { name } => subst
            .get(name)
            .cloned()
            .unwrap_or_else(|| tast::Ty::TParam { name: name.clone() }),
        tast::Ty::TFunc { params, ret_ty } => tast::Ty::TFunc {
            params: params
                .iter()
                .map(|item| substitute_ty_params(item, subst))
                .collect(),
            ret_ty: Box::new(substitute_ty_params(ret_ty.as_ref(), subst)),
        },
    }
}

pub(crate) fn instantiate_self_ty(ty: &tast::Ty, self_ty: &tast::Ty) -> tast::Ty {
    match ty {
        tast::Ty::TVar(var) => tast::Ty::TVar(*var),
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
            typs: typs
                .iter()
                .map(|ty| instantiate_self_ty(ty, self_ty))
                .collect(),
        },
        tast::Ty::TEnum { name } => tast::Ty::TEnum { name: name.clone() },
        tast::Ty::TStruct { name } => {
            if name == "Self" {
                self_ty.clone()
            } else {
                tast::Ty::TStruct { name: name.clone() }
            }
        }
        tast::Ty::TDyn { trait_name } => tast::Ty::TDyn {
            trait_name: trait_name.clone(),
        },
        tast::Ty::TApp { ty, args } => tast::Ty::TApp {
            ty: Box::new(instantiate_self_ty(ty, self_ty)),
            args: args
                .iter()
                .map(|ty| instantiate_self_ty(ty, self_ty))
                .collect(),
        },
        tast::Ty::TArray { len, elem } => tast::Ty::TArray {
            len: *len,
            elem: Box::new(instantiate_self_ty(elem, self_ty)),
        },
        tast::Ty::TSlice { elem } => tast::Ty::TSlice {
            elem: Box::new(instantiate_self_ty(elem, self_ty)),
        },
        tast::Ty::TVec { elem } => tast::Ty::TVec {
            elem: Box::new(instantiate_self_ty(elem, self_ty)),
        },
        tast::Ty::TRef { elem } => tast::Ty::TRef {
            elem: Box::new(instantiate_self_ty(elem, self_ty)),
        },
        tast::Ty::THashMap { key, value } => tast::Ty::THashMap {
            key: Box::new(instantiate_self_ty(key, self_ty)),
            value: Box::new(instantiate_self_ty(value, self_ty)),
        },
        tast::Ty::TParam { name } => tast::Ty::TParam { name: name.clone() },
        tast::Ty::TFunc { params, ret_ty } => tast::Ty::TFunc {
            params: params
                .iter()
                .map(|param| instantiate_self_ty(param, self_ty))
                .collect(),
            ret_ty: Box::new(instantiate_self_ty(ret_ty, self_ty)),
        },
    }
}

fn lookup_bound_trait_methods(
    genv: &PackageTypeEnv,
    bounds: &[tast::TastIdent],
    method: &tast::TastIdent,
) -> Vec<(tast::TastIdent, tast::Ty)> {
    lookup_trait_methods(genv, bounds, method, None)
}

fn lookup_in_scope_trait_methods(
    genv: &PackageTypeEnv,
    in_scope_traits: &[tast::TastIdent],
    receiver_ty: &tast::Ty,
    method: &tast::TastIdent,
) -> Vec<(tast::TastIdent, tast::Ty)> {
    lookup_trait_methods(genv, in_scope_traits, method, Some(receiver_ty))
}

fn lookup_trait_methods(
    genv: &PackageTypeEnv,
    trait_names: &[tast::TastIdent],
    method: &tast::TastIdent,
    receiver_ty: Option<&tast::Ty>,
) -> Vec<(tast::TastIdent, tast::Ty)> {
    let mut result = Vec::new();
    for trait_name in trait_names.iter() {
        let Some((resolved_trait, trait_env)) =
            super::util::resolve_trait_name(genv, &trait_name.0)
        else {
            continue;
        };
        let resolved_ident = tast::TastIdent(resolved_trait);
        if let Some(ty) = receiver_ty
            && (!is_concrete_trait_impl_target(ty)
                || !genv.has_trait_impl_visible(&resolved_ident.0, ty))
        {
            continue;
        }
        if let Some(method_ty) = trait_env.lookup_trait_method(&resolved_ident, method) {
            result.push((resolved_ident, method_ty));
        }
    }
    result
}

fn lookup_trait_method_from_type_name(
    genv: &PackageTypeEnv,
    type_name: &str,
    method: &tast::TastIdent,
) -> Option<(tast::TastIdent, tast::Ty)> {
    let (trait_name, trait_env) = super::util::resolve_trait_name(genv, type_name)?;
    let trait_ident = tast::TastIdent(trait_name);
    let method_ty = trait_env.lookup_trait_method(&trait_ident, method)?;
    Some((trait_ident, method_ty))
}

struct TraitMethodLookup {
    receiver: MethodLookupReceiver,
    candidates: Vec<(tast::TastIdent, tast::Ty)>,
}

enum MethodLookupReceiver {
    TypeParam(String),
    Concrete(tast::Ty),
    Deferred(tast::Ty),
}

pub(crate) fn contains_tvar(ty: &tast::Ty) -> bool {
    match ty {
        tast::Ty::TVar(_) => true,
        tast::Ty::TTuple { typs } => typs.iter().any(contains_tvar),
        tast::Ty::TApp { ty, args } => contains_tvar(ty) || args.iter().any(contains_tvar),
        tast::Ty::TArray { elem, .. }
        | tast::Ty::TSlice { elem }
        | tast::Ty::TVec { elem }
        | tast::Ty::TRef { elem } => contains_tvar(elem),
        tast::Ty::THashMap { key, value } => contains_tvar(key) || contains_tvar(value),
        tast::Ty::TFunc { params, ret_ty } => {
            params.iter().any(contains_tvar) || contains_tvar(ret_ty)
        }
        _ => false,
    }
}

fn try_result_parts(ty: &tast::Ty) -> Option<(&str, &tast::Ty, &tast::Ty)> {
    let tast::Ty::TApp { ty, args } = ty else {
        return None;
    };
    let tast::Ty::TEnum { name } = ty.as_ref() else {
        return None;
    };
    if name != "Result" && !name.ends_with("::Result") {
        return None;
    }
    if args.len() != 2 {
        return None;
    }
    Some((name.as_str(), &args[0], &args[1]))
}

fn try_option_parts(ty: &tast::Ty) -> Option<(&str, &tast::Ty)> {
    let tast::Ty::TApp { ty, args } = ty else {
        return None;
    };
    let tast::Ty::TEnum { name } = ty.as_ref() else {
        return None;
    };
    if name != "Option" && !name.ends_with("::Option") {
        return None;
    }
    if args.len() != 1 {
        return None;
    }
    Some((name.as_str(), &args[0]))
}

fn try_variant_indices(
    genv: &PackageTypeEnv,
    enum_name: &str,
    kind: &TryKind,
    diagnostics: &mut Diagnostics,
    range: Option<TextRange>,
) -> Option<(usize, usize)> {
    let (success_name, success_arity, residual_name, residual_arity, message) = match kind {
        TryKind::Result => (
            "Ok",
            1,
            "Err",
            1,
            "`?` on Result[T, E] requires the enum to define `Ok(T)` and `Err(E)` variants",
        ),
        TryKind::Option => (
            "Some",
            1,
            "None",
            0,
            "`?` on Option[T] requires the enum to define `Some(T)` and `None` variants",
        ),
    };

    let (resolved, env) = super::util::resolve_type_name(genv, enum_name);
    let ident = tast::TastIdent::new(&resolved);
    let Some(enum_def) = env.enums().get(&ident) else {
        super::util::push_ice(
            diagnostics,
            format!("enum {} not found when lowering `?`", enum_name),
        );
        return None;
    };

    let success_index = enum_def
        .variants
        .iter()
        .position(|(name, fields)| name.0 == success_name && fields.len() == success_arity);
    let residual_index = enum_def
        .variants
        .iter()
        .position(|(name, fields)| name.0 == residual_name && fields.len() == residual_arity);

    match (success_index, residual_index) {
        (Some(success_index), Some(residual_index)) => Some((success_index, residual_index)),
        _ => {
            super::util::push_error_with_range(diagnostics, message, range);
            None
        }
    }
}

fn result_ty(name: &str, ok_ty: tast::Ty, err_ty: tast::Ty) -> tast::Ty {
    tast::Ty::TApp {
        ty: Box::new(tast::Ty::TEnum {
            name: name.to_string(),
        }),
        args: vec![ok_ty, err_ty],
    }
}

fn option_ty(name: &str, ok_ty: tast::Ty) -> tast::Ty {
    tast::Ty::TApp {
        ty: Box::new(tast::Ty::TEnum {
            name: name.to_string(),
        }),
        args: vec![ok_ty],
    }
}

fn same_or_unresolved_ty(lhs: &tast::Ty, rhs: &tast::Ty) -> bool {
    lhs == rhs || contains_tvar(lhs) || contains_tvar(rhs)
}

pub(crate) fn contains_tparam(ty: &tast::Ty) -> bool {
    match ty {
        tast::Ty::TParam { .. } => true,
        tast::Ty::TTuple { typs } => typs.iter().any(contains_tparam),
        tast::Ty::TApp { ty, args } => contains_tparam(ty) || args.iter().any(contains_tparam),
        tast::Ty::TArray { elem, .. }
        | tast::Ty::TSlice { elem }
        | tast::Ty::TVec { elem }
        | tast::Ty::TRef { elem } => contains_tparam(elem),
        tast::Ty::THashMap { key, value } => contains_tparam(key) || contains_tparam(value),
        tast::Ty::TFunc { params, ret_ty } => {
            params.iter().any(contains_tparam) || contains_tparam(ret_ty)
        }
        _ => false,
    }
}

fn lookup_trait_method_candidates(
    genv: &PackageTypeEnv,
    local_env: &LocalTypeEnv,
    receiver_ty: &tast::Ty,
    method: &tast::TastIdent,
) -> TraitMethodLookup {
    if let tast::Ty::TParam { name } = receiver_ty {
        let bounds = local_env.tparam_trait_bounds(name).unwrap_or(&[]);
        return TraitMethodLookup {
            receiver: MethodLookupReceiver::TypeParam(name.clone()),
            candidates: lookup_bound_trait_methods(genv, bounds, method),
        };
    }
    if contains_tvar(receiver_ty) {
        return TraitMethodLookup {
            receiver: MethodLookupReceiver::Deferred(receiver_ty.clone()),
            candidates: lookup_trait_methods(genv, local_env.in_scope_traits(), method, None),
        };
    }
    TraitMethodLookup {
        receiver: MethodLookupReceiver::Concrete(receiver_ty.clone()),
        candidates: lookup_in_scope_trait_methods(
            genv,
            local_env.in_scope_traits(),
            receiver_ty,
            method,
        ),
    }
}

fn report_method_not_found(
    diagnostics: &mut Diagnostics,
    method_name: &tast::TastIdent,
    receiver: &MethodLookupReceiver,
    range: Option<TextRange>,
) {
    match receiver {
        MethodLookupReceiver::TypeParam(name) => diagnostics.push(
            Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Method {} is not available for type parameter {}",
                    method_name.0, name
                ),
            )
            .with_range(range),
        ),
        MethodLookupReceiver::Concrete(ty) | MethodLookupReceiver::Deferred(ty) => {
            super::util::push_error_with_range(
                diagnostics,
                format!(
                    "Method {} not found for type {}",
                    method_name.0,
                    super::util::format_ty_for_diag(ty)
                ),
                range,
            )
        }
    }
}

fn report_ambiguous_method(
    diagnostics: &mut Diagnostics,
    method_name: &tast::TastIdent,
    receiver: &MethodLookupReceiver,
    candidates: &[(tast::TastIdent, tast::Ty)],
    range: Option<TextRange>,
) {
    let trait_names = candidates
        .iter()
        .map(|(t, _)| t.0.clone())
        .collect::<Vec<_>>()
        .join(", ");
    let receiver_label = match receiver {
        MethodLookupReceiver::TypeParam(name) => format!("type parameter {}", name),
        MethodLookupReceiver::Concrete(ty) | MethodLookupReceiver::Deferred(ty) => {
            format!("type {}", super::util::format_ty_for_diag(ty))
        }
    };
    diagnostics.push(
        Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!(
                "Ambiguous method {} for {} (candidates: {}). Use UFCS like Trait::{}(...) to disambiguate",
                method_name.0, receiver_label, trait_names, method_name.0
            ),
        )
        .with_range(range),
    );
}
