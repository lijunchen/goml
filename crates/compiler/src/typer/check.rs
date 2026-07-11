use std::collections::HashMap;

mod call;
mod coercion;
mod constraints;
mod constructors;
mod patterns;

use call::CallRequest;
use constraints::FnSchemeApplication;
use constructors::{ConstructorRequest, StructLiteralRequest};

use diagnostics::{Severity, Stage};
use parser::{Diagnostic, Diagnostics, syntax::MySyntaxNodePtr};
use text_size::TextRange;

use crate::common::{self, Prim};
use crate::hir::{self};
use crate::typer::literals::{
    ensure_float_literal_fits, parse_char_literal, parse_float_literal_with_ty,
    parse_integer_literal_with_ty,
};
use crate::typer::localenv::LocalTypeEnv;
use crate::typer::member_lookup::{
    MethodLookupReceiver, lookup_trait_method_candidates, lookup_trait_method_from_type_name,
    report_ambiguous_method, report_method_not_found, resolve_field_ty_eager,
};
use crate::typer::operators::{
    comparison_operand_is_valid, comparison_operator_text, integer_literal_target, is_float_ty,
    is_integer_ty, is_numeric_ty, is_signed_numeric_ty,
};
use crate::typer::results::{
    CallElab, CalleeElab, Coercion, NameRefElab, StructLitArgElab, StructLitElab, StructPatArgElab,
    StructPatElab, TryElab, TryKind,
};
use crate::typer::type_ops::{
    collect_type_param_substitution, contains_tvar, decompose_struct_type,
    fn_ret_depends_on_params, instantiate_self_ty, is_concrete_ty, same_or_unresolved_ty,
    substitute_ty_params,
};
use crate::{
    env::{Constraint, PackageTypeEnv},
    tast::{self},
    typer::{LoopControlContext, Typer},
};

#[derive(Clone, Copy)]
enum ArrayAssignRoot {
    Local(hir::LocalId),
    Ref,
}

impl Typer {
    fn with_expr_ty(&self, expr: tast::Expr, ty: tast::Ty) -> tast::Expr {
        match expr {
            tast::Expr::EVar { name, astptr, .. } => tast::Expr::EVar { name, ty, astptr },
            tast::Expr::EPrim { value, .. } => tast::Expr::EPrim { value, ty },
            tast::Expr::EConstr {
                constructor, args, ..
            } => tast::Expr::EConstr {
                constructor,
                args,
                ty,
            },
            tast::Expr::ETuple { items, .. } => tast::Expr::ETuple { items, ty },
            tast::Expr::EArray { items, .. } => tast::Expr::EArray { items, ty },
            tast::Expr::EClosure {
                params,
                body,
                captures,
                ..
            } => tast::Expr::EClosure {
                params,
                body,
                ty,
                captures,
            },
            tast::Expr::EBlock { block, .. } => tast::Expr::EBlock { block, ty },
            tast::Expr::EMatch {
                expr, arms, astptr, ..
            } => tast::Expr::EMatch {
                expr,
                arms,
                ty,
                astptr,
            },
            tast::Expr::EIf {
                cond,
                then_branch,
                else_branch,
                ..
            } => tast::Expr::EIf {
                cond,
                then_branch,
                else_branch,
                ty,
            },
            tast::Expr::EWhile { cond, body, .. } => tast::Expr::EWhile { cond, body, ty },
            tast::Expr::EBreak { .. } => tast::Expr::EBreak { ty },
            tast::Expr::EContinue { .. } => tast::Expr::EContinue { ty },
            tast::Expr::EReturn { expr, .. } => tast::Expr::EReturn { expr, ty },
            tast::Expr::EGo { expr, .. } => tast::Expr::EGo { expr, ty },
            tast::Expr::ECall { func, args, .. } => tast::Expr::ECall { func, args, ty },
            tast::Expr::EUnary {
                op,
                expr,
                resolution,
                ..
            } => tast::Expr::EUnary {
                op,
                expr,
                ty,
                resolution,
            },
            tast::Expr::EProj { tuple, index, .. } => tast::Expr::EProj { tuple, index, ty },
            tast::Expr::EField {
                expr,
                field_name,
                astptr,
                ..
            } => tast::Expr::EField {
                expr,
                field_name,
                ty,
                astptr,
            },
            tast::Expr::EIndex {
                base,
                index,
                astptr,
                ..
            } => tast::Expr::EIndex {
                base,
                index,
                ty,
                astptr,
            },
            tast::Expr::EBinary {
                op,
                lhs,
                rhs,
                resolution,
                ..
            } => tast::Expr::EBinary {
                op,
                lhs,
                rhs,
                ty,
                resolution,
            },
            tast::Expr::ETraitMethod {
                trait_name,
                method_name,
                astptr,
                ..
            } => tast::Expr::ETraitMethod {
                trait_name,
                method_name,
                ty,
                astptr,
            },
            tast::Expr::EDynTraitMethod {
                trait_name,
                method_name,
                astptr,
                ..
            } => tast::Expr::EDynTraitMethod {
                trait_name,
                method_name,
                ty,
                astptr,
            },
            tast::Expr::EInherentMethod {
                receiver_ty,
                method_name,
                astptr,
                ..
            } => tast::Expr::EInherentMethod {
                receiver_ty,
                method_name,
                ty,
                astptr,
            },
            tast::Expr::EToDyn {
                trait_name,
                for_ty,
                expr,
                astptr,
                ..
            } => tast::Expr::EToDyn {
                trait_name,
                for_ty,
                expr,
                ty,
                astptr,
            },
        }
    }

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
                let prim = parse_integer_literal_with_ty(diagnostics, &value, &ty, range)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EInt8 { value } => {
                let ty = tast::Ty::TInt8;
                let range = self.expr_range(e);
                let prim = parse_integer_literal_with_ty(diagnostics, &value, &ty, range)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EInt16 { value } => {
                let ty = tast::Ty::TInt16;
                let range = self.expr_range(e);
                let prim = parse_integer_literal_with_ty(diagnostics, &value, &ty, range)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EInt32 { value } => {
                let ty = tast::Ty::TInt32;
                let range = self.expr_range(e);
                let prim = parse_integer_literal_with_ty(diagnostics, &value, &ty, range)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EInt64 { value } => {
                let ty = tast::Ty::TInt64;
                let range = self.expr_range(e);
                let prim = parse_integer_literal_with_ty(diagnostics, &value, &ty, range)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EUInt8 { value } => {
                let ty = tast::Ty::TUint8;
                let range = self.expr_range(e);
                let prim = parse_integer_literal_with_ty(diagnostics, &value, &ty, range)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EUInt16 { value } => {
                let ty = tast::Ty::TUint16;
                let range = self.expr_range(e);
                let prim = parse_integer_literal_with_ty(diagnostics, &value, &ty, range)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EUInt32 { value } => {
                let ty = tast::Ty::TUint32;
                let range = self.expr_range(e);
                let prim = parse_integer_literal_with_ty(diagnostics, &value, &ty, range)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EUInt64 { value } => {
                let ty = tast::Ty::TUint64;
                let range = self.expr_range(e);
                let prim = parse_integer_literal_with_ty(diagnostics, &value, &ty, range)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EFloat { value } => {
                let range = self.expr_range(e);
                ensure_float_literal_fits(diagnostics, value, &tast::Ty::TFloat64, range);
                let ty = tast::Ty::TFloat64;
                tast::Expr::EPrim {
                    value: Prim::from_float_literal(value, &ty),
                    ty,
                }
            }
            hir::Expr::EFloat32 { value } => {
                let ty = tast::Ty::TFloat32;
                let range = self.expr_range(e);
                let prim = parse_float_literal_with_ty(diagnostics, &value, &ty, range)
                    .unwrap_or_else(|| Prim::from_float_literal(0.0, &ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EFloat64 { value } => {
                let ty = tast::Ty::TFloat64;
                let range = self.expr_range(e);
                let prim = parse_float_literal_with_ty(diagnostics, &value, &ty, range)
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
                let ch = parse_char_literal(diagnostics, &value, range).unwrap_or('\0');
                tast::Expr::EPrim {
                    value: Prim::character(ch),
                    ty,
                }
            }
            hir::Expr::EConstr { constructor, args } => self.infer_constructor_expr(
                genv,
                local_env,
                diagnostics,
                ConstructorRequest {
                    expr_id: e,
                    constructor_ref: &constructor,
                    args: &args,
                    hint_ret_ty: None,
                },
            ),
            hir::Expr::EStructLiteral { name, fields } => self.infer_struct_literal_expr(
                genv,
                local_env,
                diagnostics,
                StructLiteralRequest {
                    expr_id: e,
                    name: &name,
                    fields: &fields,
                    hint_ret_ty: None,
                },
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
            hir::Expr::ECall { func, args } => self.infer_call_expr(
                genv,
                local_env,
                diagnostics,
                CallRequest {
                    call_expr_id: e,
                    func,
                    args: &args,
                    hint_ret_ty: None,
                },
            ),
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
                    let prim =
                        parse_integer_literal_with_ty(diagnostics, &negated, expected, range)
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
                let prim = parse_integer_literal_with_ty(diagnostics, value, expected, range)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(expected));
                tast::Expr::EPrim {
                    value: prim,
                    ty: expected.clone(),
                }
            }
            hir::Expr::EFloat { value } if is_float_ty(expected) => {
                let range = self.expr_range(e);
                ensure_float_literal_fits(diagnostics, value, expected, range);
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
                let mut expr_tast = self.infer_expr(genv, local_env, diagnostics, expr);
                if self.expr_always_exits_loop_control(expr) {
                    let scrut_ty = self.fresh_ty_var();
                    expr_tast = self.with_expr_ty(expr_tast, scrut_ty.clone());
                    self.record_expr_result(expr, &expr_tast);
                }
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
                ConstructorRequest {
                    expr_id: e,
                    constructor_ref: &constructor,
                    args: &args,
                    hint_ret_ty: if matches!(expected, tast::Ty::TDyn { .. }) {
                        None
                    } else {
                        Some(expected)
                    },
                },
            ),
            hir::Expr::ECall { func, args } => self.infer_call_expr(
                genv,
                local_env,
                diagnostics,
                CallRequest {
                    call_expr_id: e,
                    func,
                    args: &args,
                    hint_ret_ty: if matches!(expected, tast::Ty::TDyn { .. }) {
                        None
                    } else {
                        Some(expected)
                    },
                },
            ),
            hir::Expr::EStructLiteral { name, fields }
                if !matches!(expected, tast::Ty::TDyn { .. }) =>
            {
                self.infer_struct_literal_expr(
                    genv,
                    local_env,
                    diagnostics,
                    StructLiteralRequest {
                        expr_id: e,
                        name: &name,
                        fields: &fields,
                        hint_ret_ty: Some(expected),
                    },
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
                let Some(func_scheme) = genv.get_function_scheme(hint) else {
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
                let Some(func_scheme) = genv.get_function_scheme(hint) else {
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
                        .filter(|param| {
                            super::util::try_constr_name(param)
                                .is_some_and(|name| name == resolved_type_name)
                        })
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
                Some(ty) => {
                    let param_ty = tast::Ty::from_hir(genv, ty, &current_tparams_env);
                    super::util::validate_dyn_object_safety_in_ty(genv, diagnostics, &param_ty);
                    param_ty
                }
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
        let saved_loop_control_context = self.loop_control_context;
        self.loop_control_context = LoopControlContext::Disallowed;
        let body_tast = self.infer_expr(genv, local_env, diagnostics, body);
        self.loop_control_context = saved_loop_control_context;
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
                    let annotated_ty = param.ty.as_ref().map(|ty| {
                        let ann_ty = tast::Ty::from_hir(genv, ty, &current_tparams_env);
                        super::util::validate_dyn_object_safety_in_ty(genv, diagnostics, &ann_ty);
                        ann_ty
                    });

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
                let saved_loop_control_context = self.loop_control_context;
                self.loop_control_context = LoopControlContext::Disallowed;
                let body_tast =
                    self.check_expr(genv, local_env, diagnostics, body, expected_ret.as_ref());
                self.loop_control_context = saved_loop_control_context;
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
        let annotated_ty = stmt.annotation.as_ref().map(|ty| {
            let ann_ty = tast::Ty::from_hir(genv, ty, &current_tparams_env);
            super::util::validate_dyn_object_safety_in_ty(genv, diagnostics, &ann_ty);
            ann_ty
        });

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
        let mut expr_tast = self.infer_expr(genv, local_env, diagnostics, expr);
        if self.expr_always_exits_loop_control(expr) {
            let scrut_ty = self.fresh_ty_var();
            expr_tast = self.with_expr_ty(expr_tast, scrut_ty.clone());
            self.record_expr_result(expr, &expr_tast);
        }
        let expr_ty = expr_tast.get_ty();

        let mut arms_tast = Vec::new();
        let arm_ty = self.fresh_ty_var();
        let mut has_value_arm = false;
        for arm in arms.iter() {
            local_env.push_scope();
            let arm_tast = self.check_pat(genv, local_env, diagnostics, arm.pat, &expr_ty);
            let arm_exits = self.expr_always_exits_loop_control(arm.body);
            let mut arm_body_tast = self.infer_expr(genv, local_env, diagnostics, arm.body);
            local_env.pop_scope(diagnostics);
            if !arm_exits {
                has_value_arm = true;
                self.push_constraint(Constraint::TypeEqual(
                    arm_body_tast.get_ty(),
                    arm_ty.clone(),
                    self.expr_range(arm.body),
                ));
            } else {
                arm_body_tast = self.with_expr_ty(arm_body_tast, arm_ty.clone());
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
        let mut then_tast = then_tast;
        let mut else_tast = else_tast;

        if !then_exits {
            self.push_constraint(Constraint::TypeEqual(
                then_tast.get_ty(),
                result_ty.clone(),
                self.expr_range(then_branch),
            ));
        } else {
            then_tast = self.with_expr_ty(then_tast, result_ty.clone());
        }
        if !else_exits {
            self.push_constraint(Constraint::TypeEqual(
                else_tast.get_ty(),
                result_ty.clone(),
                self.expr_range(else_branch),
            ));
        } else {
            else_tast = self.with_expr_ty(else_tast, result_ty.clone());
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
        let cond_always_exits = self.expr_always_exits_loop_control(cond);
        let saved_loop_control_context = self.loop_control_context;
        self.loop_control_context = LoopControlContext::WhileCondition;
        let mut cond_tast = self.infer_expr(genv, local_env, diagnostics, cond);
        self.loop_control_context = LoopControlContext::Allowed;
        let body_tast = self.infer_expr(genv, local_env, diagnostics, body);
        self.loop_control_context = saved_loop_control_context;

        if cond_always_exits {
            cond_tast = self.with_expr_ty(cond_tast, tast::Ty::TBool);
            self.record_expr_result(cond, &cond_tast);
        } else {
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
        match self.loop_control_context {
            LoopControlContext::Allowed => {}
            LoopControlContext::WhileCondition => {
                super::util::push_error_with_range(
                    diagnostics,
                    "`break` is not allowed in a while condition",
                    self.expr_range(e),
                );
            }
            LoopControlContext::Disallowed => {
                super::util::push_error_with_range(
                    diagnostics,
                    "`break` outside of a while loop",
                    self.expr_range(e),
                );
            }
        }
        tast::Expr::EBreak {
            ty: tast::Ty::TUnit,
        }
    }

    fn infer_continue_expr(&mut self, diagnostics: &mut Diagnostics, e: hir::ExprId) -> tast::Expr {
        match self.loop_control_context {
            LoopControlContext::Allowed => {}
            LoopControlContext::WhileCondition => {
                super::util::push_error_with_range(
                    diagnostics,
                    "`continue` is not allowed in a while condition",
                    self.expr_range(e),
                );
            }
            LoopControlContext::Disallowed => {
                super::util::push_error_with_range(
                    diagnostics,
                    "`continue` outside of a while loop",
                    self.expr_range(e),
                );
            }
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

        let (kind, ok_ty, residual_ty, container_name) = match (
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
                (
                    TryKind::Result,
                    ok_ty.clone(),
                    Some(err_ty.clone()),
                    inner_name.to_string(),
                )
            }
            (Some((inner_name, ok_ty, err_ty)), _, _, _, _, true) => {
                let outer_ok_ty = self.fresh_ty_var();
                self.push_constraint(Constraint::TypeEqual(
                    outer_ret_ty_raw.clone(),
                    result_ty(inner_name, outer_ok_ty, err_ty.clone()),
                    range,
                ));
                (
                    TryKind::Result,
                    ok_ty.clone(),
                    Some(err_ty.clone()),
                    inner_name.to_string(),
                )
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
                (TryKind::Option, ok_ty.clone(), None, inner_name.to_string())
            }
            (_, Some((inner_name, ok_ty)), _, _, _, true) => {
                let outer_ok_ty = self.fresh_ty_var();
                self.push_constraint(Constraint::TypeEqual(
                    outer_ret_ty_raw.clone(),
                    option_ty(inner_name, outer_ok_ty),
                    range,
                ));
                (TryKind::Option, ok_ty.clone(), None, inner_name.to_string())
            }
            (_, _, Some((outer_name, _, err_ty)), _, true, _) => {
                let ok_ty = self.fresh_ty_var();
                self.push_constraint(Constraint::TypeEqual(
                    inner_ty_raw.clone(),
                    result_ty(outer_name, ok_ty.clone(), err_ty.clone()),
                    range,
                ));
                (
                    TryKind::Result,
                    ok_ty,
                    Some(err_ty.clone()),
                    outer_name.to_string(),
                )
            }
            (_, _, _, Some((outer_name, _)), true, _) => {
                let ok_ty = self.fresh_ty_var();
                self.push_constraint(Constraint::TypeEqual(
                    inner_ty_raw.clone(),
                    option_ty(outer_name, ok_ty.clone()),
                    range,
                ));
                (TryKind::Option, ok_ty, None, outer_name.to_string())
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

        let Some((success_index, residual_index)) = try_variant_indices(
            genv,
            &container_name,
            &kind,
            &ok_ty,
            residual_ty.as_ref(),
            diagnostics,
            range,
        ) else {
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
                self.push_constraint(Constraint::TypeEqual(
                    lhs_ty.clone(),
                    rhs_ty.clone(),
                    self.expr_range(lhs),
                ));
                self.validate_comparison_operand(
                    genv,
                    diagnostics,
                    op,
                    &lhs_ty,
                    &rhs_ty,
                    self.expr_range(lhs),
                );
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

    fn validate_comparison_operand(
        &mut self,
        genv: &PackageTypeEnv,
        diagnostics: &mut Diagnostics,
        op: common_defs::BinaryOp,
        lhs_ty: &tast::Ty,
        rhs_ty: &tast::Ty,
        range: Option<TextRange>,
    ) {
        let lhs_norm = self.norm(lhs_ty);
        let rhs_norm = self.norm(rhs_ty);
        if contains_tvar(&lhs_norm) || contains_tvar(&rhs_norm) {
            self.deferred_comparison_checks
                .push(super::DeferredComparisonCheck {
                    op,
                    lhs_ty: lhs_ty.clone(),
                    rhs_ty: rhs_ty.clone(),
                    origin: range,
                });
            return;
        }
        if !same_or_unresolved_ty(&lhs_norm, &rhs_norm) {
            return;
        }

        let valid = comparison_operand_is_valid(genv, op, &lhs_norm);

        if !valid {
            super::util::push_error_with_range(
                diagnostics,
                format!(
                    "Operator {} is not defined for type {}",
                    comparison_operator_text(op),
                    super::util::format_ty_for_diag(&lhs_norm)
                ),
                range,
            );
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
            _ if contains_tvar(&tuple_ty) => {
                let result_ty = self.fresh_ty_var();
                self.push_constraint(Constraint::TupleProjectionAccess {
                    tuple_ty: tuple_ty.clone(),
                    index,
                    result_ty: result_ty.clone(),
                    origin: range,
                });
                tast::Expr::EProj {
                    tuple: Box::new(tuple_tast),
                    index,
                    ty: result_ty,
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
            tast::Ty::THashMap { key, value } => {
                validate_hashmap_get_option_ty(
                    genv,
                    diagnostics,
                    value.as_ref(),
                    self.expr_range(index_expr_id),
                );
                (
                    self.check_expr(genv, local_env, diagnostics, index, key.as_ref()),
                    option_ty("Option", value.as_ref().clone()),
                )
            }
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
                self.validate_array_assignment_target(
                    local_env,
                    diagnostics,
                    target_expr_id,
                    &base_tast,
                );
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
        base_tast: &tast::Expr,
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
                if Self::tast_array_assign_root_is_ref(base_tast) {
                    return;
                }
                diagnostics.push(
                    Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        "array indexed assignment requires a writable root such as a mutable local or `ref.get()`",
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

    fn tast_array_assign_root_is_ref(expr: &tast::Expr) -> bool {
        match expr {
            tast::Expr::EProj { tuple, .. } | tast::Expr::EField { expr: tuple, .. } => {
                Self::tast_array_assign_root_is_ref(tuple)
            }
            tast::Expr::EIndex { base, .. } => Self::tast_array_assign_root_is_ref(base),
            tast::Expr::ECall { func, args, .. } if args.len() == 1 => match func.as_ref() {
                tast::Expr::EInherentMethod {
                    receiver_ty,
                    method_name,
                    ..
                } => method_name.0 == "get" && matches!(receiver_ty, tast::Ty::TRef { .. }),
                _ => false,
            },
            _ => false,
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
    ok_ty: &tast::Ty,
    residual_ty: Option<&tast::Ty>,
    diagnostics: &mut Diagnostics,
    range: Option<TextRange>,
) -> Option<(usize, usize)> {
    let (success_name, residual_name, message) = match kind {
        TryKind::Result => (
            "Ok",
            "Err",
            "`?` on Result[T, E] requires the enum to define `Ok(T)` and `Err(E)` variants",
        ),
        TryKind::Option => (
            "Some",
            "None",
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

    let (success_index, residual_index) = match kind {
        TryKind::Result => {
            let Some(residual_ty) = residual_ty else {
                super::util::push_error_with_range(diagnostics, message, range);
                return None;
            };
            if enum_def.generics.len() != 2 {
                super::util::push_error_with_range(diagnostics, message, range);
                return None;
            }
            let mut subst = HashMap::new();
            subst.insert(enum_def.generics[0].0.clone(), ok_ty.clone());
            subst.insert(enum_def.generics[1].0.clone(), residual_ty.clone());
            (
                try_payload_variant_index(enum_def, success_name, ok_ty, &subst),
                try_payload_variant_index(enum_def, residual_name, residual_ty, &subst),
            )
        }
        TryKind::Option => {
            if enum_def.generics.len() != 1 {
                super::util::push_error_with_range(diagnostics, message, range);
                return None;
            }
            let mut subst = HashMap::new();
            subst.insert(enum_def.generics[0].0.clone(), ok_ty.clone());
            (
                try_payload_variant_index(enum_def, success_name, ok_ty, &subst),
                enum_def
                    .variants
                    .iter()
                    .position(|(name, fields)| name.0 == residual_name && fields.is_empty()),
            )
        }
    };

    match (success_index, residual_index) {
        (Some(success_index), Some(residual_index)) => Some((success_index, residual_index)),
        _ => {
            super::util::push_error_with_range(diagnostics, message, range);
            None
        }
    }
}

fn try_payload_variant_index(
    enum_def: &crate::env::EnumDef,
    variant_name: &str,
    expected_ty: &tast::Ty,
    subst: &HashMap<String, tast::Ty>,
) -> Option<usize> {
    enum_def.variants.iter().position(|(name, fields)| {
        if name.0 != variant_name {
            return false;
        }
        let [field_ty] = fields.as_slice() else {
            return false;
        };
        let field_ty = substitute_ty_params(field_ty, subst);
        same_or_unresolved_ty(&field_ty, expected_ty)
    })
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

fn validate_hashmap_get_option_for_map_ty(
    genv: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    map_ty: &tast::Ty,
    range: Option<TextRange>,
) {
    if let tast::Ty::THashMap { value, .. } = map_ty {
        validate_hashmap_get_option_ty(genv, diagnostics, value, range);
    }
}

fn validate_hashmap_get_option_ty(
    genv: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    value_ty: &tast::Ty,
    range: Option<TextRange>,
) {
    let (resolved, env) = super::util::resolve_type_name(genv, "Option");
    let ident = tast::TastIdent::new(&resolved);
    let Some(enum_def) = env.enums().get(&ident) else {
        super::util::push_ice(
            diagnostics,
            "enum Option not found when checking HashMap::get",
        );
        return;
    };

    let valid = if enum_def.generics.len() == 1 {
        let param = enum_def.generics[0].0.clone();
        let mut empty_variants = 0usize;
        let mut payload_variants = 0usize;
        let mut matching_payload_variants = 0usize;
        for (_, fields) in &enum_def.variants {
            match fields.as_slice() {
                [] => empty_variants += 1,
                [field_ty] => {
                    payload_variants += 1;
                    let mut subst = HashMap::new();
                    subst.insert(param.clone(), value_ty.clone());
                    let field_ty = substitute_ty_params(field_ty, &subst);
                    if same_or_unresolved_ty(&field_ty, value_ty) {
                        matching_payload_variants += 1;
                    }
                }
                _ => {}
            }
        }
        empty_variants == 1 && payload_variants == 1 && matching_payload_variants == 1
    } else {
        false
    };

    if !valid {
        super::util::push_error_with_range(
            diagnostics,
            "HashMap::get requires Option[T] to define exactly one empty variant and one payload variant containing T",
            range,
        );
    }
}
