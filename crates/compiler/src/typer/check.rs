use std::{collections::HashMap, num::IntErrorKind};

use diagnostics::{Severity, Stage};
use parser::{Diagnostic, Diagnostics, syntax::MySyntaxNodePtr};

use crate::common::{self, Prim};
use crate::hir::{self};
use crate::typer::localenv::LocalTypeEnv;
use crate::typer::results::{
    CalleeElab, CallElab, Coercion, NameRefElab, StructLitArgElab, StructLitElab, StructPatArgElab,
    StructPatElab,
};
use crate::{
    env::{Constraint, GlobalTypeEnv, PackageTypeEnv},
    tast::{self},
    typer::Typer,
};

impl Typer {
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
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EInt8 { value } => {
                let ty = tast::Ty::TInt8;
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EInt16 { value } => {
                let ty = tast::Ty::TInt16;
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EInt32 { value } => {
                let ty = tast::Ty::TInt32;
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EInt64 { value } => {
                let ty = tast::Ty::TInt64;
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EUInt8 { value } => {
                let ty = tast::Ty::TUint8;
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EUInt16 { value } => {
                let ty = tast::Ty::TUint16;
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EUInt32 { value } => {
                let ty = tast::Ty::TUint32;
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EUInt64 { value } => {
                let ty = tast::Ty::TUint64;
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EFloat { value } => {
                self.ensure_float_literal_fits(diagnostics, value, &tast::Ty::TFloat64);
                let ty = tast::Ty::TFloat64;
                tast::Expr::EPrim {
                    value: Prim::from_float_literal(value, &ty),
                    ty,
                }
            }
            hir::Expr::EFloat32 { value } => {
                let ty = tast::Ty::TFloat32;
                let prim = self
                    .parse_float_literal_with_ty(diagnostics, &value, &ty)
                    .unwrap_or_else(|| Prim::from_float_literal(0.0, &ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EFloat64 { value } => {
                let ty = tast::Ty::TFloat64;
                let prim = self
                    .parse_float_literal_with_ty(diagnostics, &value, &ty)
                    .unwrap_or_else(|| Prim::from_float_literal(0.0, &ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            hir::Expr::EString { value } => tast::Expr::EPrim {
                value: Prim::string(value),
                ty: tast::Ty::TString,
            },
            hir::Expr::EConstr { constructor, args } => {
                self.infer_constructor_expr(
                    genv,
                    local_env,
                    diagnostics,
                    e,
                    &constructor,
                    &args,
                )
            }
            hir::Expr::EStructLiteral { name, fields } => {
                self.infer_struct_literal_expr(genv, local_env, diagnostics, e, &name, &fields)
            }
            hir::Expr::ETuple { items } => {
                self.infer_tuple_expr(genv, local_env, diagnostics, &items)
            }
            hir::Expr::EArray { items } => {
                self.infer_array_expr(genv, local_env, diagnostics, &items)
            }
            hir::Expr::EClosure { params, body } => {
                self.infer_closure_expr(genv, local_env, diagnostics, &params, body)
            }
            hir::Expr::ELet {
                pat,
                annotation,
                value,
            } => self.infer_let_expr(genv, local_env, diagnostics, pat, &annotation, value),
            hir::Expr::EBlock { exprs } => {
                self.infer_block_expr(genv, local_env, diagnostics, &exprs)
            }
            hir::Expr::EMatch { expr, arms } => {
                self.infer_match_expr(genv, local_env, diagnostics, expr, &arms, None)
            }
            hir::Expr::EIf {
                cond,
                then_branch,
                else_branch,
            } => self.infer_if_expr(genv, local_env, diagnostics, cond, then_branch, else_branch),
            hir::Expr::EWhile { cond, body } => {
                self.infer_while_expr(genv, local_env, diagnostics, cond, body)
            }
            hir::Expr::EGo { expr } => self.infer_go_expr(genv, local_env, diagnostics, expr),
            hir::Expr::ECall { func, args } => {
                self.infer_call_expr(genv, local_env, diagnostics, e, func, &args)
            }
            hir::Expr::EUnary { op, expr } => {
                self.infer_unary_expr(genv, local_env, diagnostics, op, expr)
            }
            hir::Expr::EBinary { op, lhs, rhs } => {
                self.infer_binary_expr(genv, local_env, diagnostics, op, lhs, rhs)
            }
            hir::Expr::EProj { tuple, index } => {
                self.infer_proj_expr(genv, local_env, diagnostics, tuple, index)
            }
            hir::Expr::EField { expr, field } => {
                self.infer_field_expr(genv, local_env, diagnostics, expr, &field, None)
            }
        };

        self.record_expr_result(e, &out);
        if matches!(expr, hir::Expr::ENameRef { .. }) {
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
            } if is_numeric_ty(expected) => {
                let operand = self.check_expr(genv, local_env, diagnostics, inner, expected);
                tast::Expr::EUnary {
                    op: common_defs::UnaryOp::Neg,
                    expr: Box::new(operand),
                    ty: expected.clone(),
                    resolution: tast::UnaryResolution::Builtin,
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
            hir::Expr::ELet {
                pat,
                annotation,
                value,
            } => self.check_let_expr(
                genv,
                local_env,
                diagnostics,
                pat,
                &annotation,
                value,
                expected,
            ),
            hir::Expr::EBlock { exprs } => {
                self.check_block_expr(genv, local_env, diagnostics, &exprs, expected)
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
                    astptr: None,
                }
            }
            _ => self.infer_expr(genv, local_env, diagnostics, e),
        };

        let expr_tast = self.coerce_to_expected_dyn(genv, diagnostics, e, expr_tast, expected);
        self.push_constraint(Constraint::TypeEqual(expr_tast.get_ty(), expected.clone()));
        self.record_expr_result(e, &expr_tast);
        expr_tast
    }

    fn coerce_to_expected_dyn(
        &mut self,
        genv: &PackageTypeEnv,
        diagnostics: &mut Diagnostics,
        expr_id: hir::ExprId,
        expr: tast::Expr,
        expected: &tast::Ty,
    ) -> tast::Expr {
        let tast::Ty::TDyn { trait_name } = expected else {
            return expr;
        };

        if matches!(expr.get_ty(), tast::Ty::TDyn { .. }) {
            return expr;
        }

        let Some((resolved_trait, _env)) = super::util::resolve_trait_name(genv, trait_name) else {
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!("Unknown trait {}", trait_name),
            ));
            return expr;
        };

        let for_ty = expr.get_ty();
        if !is_concrete_dyn_target(&for_ty) {
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Cannot convert non-concrete type {:?} to dyn {}",
                    for_ty, resolved_trait
                ),
            ));
            return expr;
        }

        if !has_visible_trait_impl(genv, &resolved_trait, &for_ty) {
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Type {:?} does not implement trait {}",
                    for_ty, resolved_trait
                ),
            ));
            return expr;
        }

        self.results.push_coercion(
            expr_id,
            Coercion::ToDyn {
                trait_name: tast::TastIdent(resolved_trait.clone()),
                for_ty: for_ty.clone(),
                ty: expected.clone(),
                astptr: None,
            },
        );
        tast::Expr::EToDyn {
            trait_name: tast::TastIdent(resolved_trait.clone()),
            for_ty,
            expr: Box::new(expr),
            ty: expected.clone(),
            astptr: None,
        }
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
                    super::util::push_ice(
                        diagnostics,
                        format!("Variable {} not found in environment", name_str),
                    );
                    self.error_expr(astptr)
                }
            }
            hir::NameRef::Def(_def_id) => {
                let Some(func_ty) = lookup_function_type_by_hint(genv, hint) else {
                    super::util::push_ice(
                        diagnostics,
                        format!("Function {} not found in environment", hint),
                    );
                    return self.error_expr(astptr);
                };
                let inst_ty = self.inst_ty(&func_ty);
                tast::Expr::EVar {
                    name: hint.to_string(),
                    ty: inst_ty,
                    astptr,
                }
            }
            hir::NameRef::Builtin(_builtin_id) => {
                let Some(func_ty) = lookup_function_type_by_hint(genv, hint) else {
                    super::util::push_ice(
                        diagnostics,
                        format!("Builtin {} not found in environment", hint),
                    );
                    return self.error_expr(astptr);
                };
                let inst_ty = self.inst_ty(&func_ty);
                tast::Expr::EVar {
                    name: hint.to_string(),
                    ty: inst_ty,
                    astptr,
                }
            }
            hir::NameRef::Unresolved(path) => {
                self.infer_unresolved_path_expr(genv, diagnostics, path, astptr)
            }
        }
    }

    fn infer_unresolved_path_expr(
        &mut self,
        genv: &PackageTypeEnv,
        diagnostics: &mut Diagnostics,
        path: &hir::Path,
        astptr: Option<MySyntaxNodePtr>,
    ) -> tast::Expr {
        if path.len() == 1 {
            let Some(name) = path.last_ident() else {
                super::util::push_ice(diagnostics, "unresolved path missing last segment");
                return self.error_expr(astptr);
            };
            if let Some(func_ty) = genv.current().get_type_of_function(name.as_str()) {
                let inst_ty = self.inst_ty(&func_ty);
                return tast::Expr::EVar {
                    name: name.clone(),
                    ty: inst_ty,
                    astptr,
                };
            }
            super::util::push_error(diagnostics, format!("Unresolved name {}", name));
            return self.error_expr(astptr);
        }
        if let Some((name, func_ty)) = lookup_function_path(genv, path) {
            let inst_ty = self.inst_ty(&func_ty);
            return tast::Expr::EVar {
                name,
                ty: inst_ty,
                astptr,
            };
        }
        let namespace = path.namespace_segments();
        let type_name = namespace
            .iter()
            .map(|seg| seg.seg().clone())
            .collect::<Vec<_>>()
            .join("::");
        let Some(member) = path.last_ident() else {
            super::util::push_ice(diagnostics, "unresolved member path missing final segment");
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
        // First check if type_name is a trait
        if let Some((trait_name, trait_env)) = super::util::resolve_trait_name(genv, type_name)
            && let Some(method_ty) =
                trait_env.lookup_trait_method(&tast::TastIdent(trait_name.clone()), &member_ident)
        {
            let inst_ty = self.inst_ty(&method_ty);
            return tast::Expr::ETraitMethod {
                trait_name: tast::TastIdent(trait_name),
                method_name: member_ident.clone(),
                ty: inst_ty,
                astptr,
            };
        }

        // Check if type_name is an enum or struct for inherent method lookup
        let receiver_ty = if type_env.enums().contains_key(&type_ident) {
            tast::Ty::TEnum {
                name: resolved_type_name.to_string(),
            }
        } else if type_env.structs().contains_key(&type_ident) {
            tast::Ty::TStruct {
                name: resolved_type_name.to_string(),
            }
        } else {
            super::util::push_error(
                diagnostics,
                format!(
                    "Type or trait {} not found for member access",
                    resolved_type_name
                ),
            );
            return self.error_expr(astptr);
        };
        if let Some(method_ty) = type_env.lookup_inherent_method(&receiver_ty, &member_ident) {
            let inst_ty = self.inst_ty(&method_ty);
            tast::Expr::EInherentMethod {
                receiver_ty: receiver_ty.clone(),
                method_name: member_ident,
                ty: inst_ty,
                astptr,
            }
        } else {
            super::util::push_error(
                diagnostics,
                format!("Method {} not found for type {}", member, type_name),
            );
            self.error_expr(astptr)
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
                                "enum variant index {} out of bounds for {:?}",
                                variant_idx, enum_def
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

    fn infer_constructor_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        expr_id: hir::ExprId,
        constructor_ref: &hir::ConstructorRef,
        args: &[hir::ExprId],
    ) -> tast::Expr {
        let constructor_path = match constructor_ref {
            hir::ConstructorRef::Resolved(ctor_id) => {
                self.constructor_path_from_id(diagnostics, ctor_id)
            }
            hir::ConstructorRef::Unresolved(path) => path.clone(),
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
            super::util::push_error(
                diagnostics,
                format!(
                    "Constructor {} not found in environment",
                    constructor_path.display()
                ),
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
            super::util::push_error(
                diagnostics,
                format!(
                    "Constructor {} expects {} arguments, but got {}",
                    constructor.name().0,
                    expected_arity,
                    args.len()
                ),
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

        let mut args_tast = Vec::new();
        if param_tys.is_empty() {
            for arg in args.iter() {
                args_tast.push(self.infer_expr(genv, local_env, diagnostics, *arg));
            }
        } else {
            for (arg, expected_ty) in args.iter().zip(param_tys.iter()) {
                args_tast.push(self.check_expr(genv, local_env, diagnostics, *arg, expected_ty));
            }
        }

        if !args_tast.is_empty() {
            let actual_ty = tast::Ty::TFunc {
                params: args_tast.iter().map(|arg| arg.get_ty()).collect(),
                ret_ty: Box::new(ret_ty.clone()),
            };
            self.push_constraint(Constraint::TypeEqual(inst_constr_ty, actual_ty));
        } else {
            self.push_constraint(Constraint::TypeEqual(inst_constr_ty, ret_ty.clone()));
        }

        self.results
            .record_constructor_expr(expr_id, constructor.clone());

        tast::Expr::EConstr {
            constructor,
            args: args_tast,
            ty: ret_ty,
        }
    }

    fn infer_struct_literal_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        expr_id: hir::ExprId,
        name: &hir::QualifiedPath,
        fields: &[(hir::HirIdent, hir::ExprId)],
    ) -> tast::Expr {
        let name_display = name.display();
        let (resolved_name, type_env) = super::util::resolve_type_name(genv, &name_display);
        let ctor = type_env.lookup_constructor(&tast::TastIdent(resolved_name.clone()));
        let Some((constructor, constr_ty)) = ctor else {
            super::util::push_error(
                diagnostics,
                format!("Constructor {} not found in environment", resolved_name),
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
                super::util::push_error(
                    diagnostics,
                    format!(
                        "Constructor {} refers to an enum, but a struct literal was used",
                        name_display
                    ),
                );
                return self.error_expr(None);
            }
        };

        let inst_constr_ty = self.inst_ty(&constr_ty);
        let param_tys = match &inst_constr_ty {
            tast::Ty::TFunc { params, .. } => params.clone(),
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
                super::util::push_error(
                    diagnostics,
                    format!(
                        "Unknown field {} on struct literal {}",
                        field_name.to_ident_name(),
                        resolved_name
                    ),
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
                super::util::push_error(
                    diagnostics,
                    format!(
                        "Duplicate field {} in struct literal {}",
                        field_name.to_ident_name(),
                        name_display
                    ),
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
                super::util::push_error(
                    diagnostics,
                    format!(
                        "Missing field {} in struct literal {}",
                        missing, name_display
                    ),
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
            let actual_ty = tast::Ty::TFunc {
                params: args_tast.iter().map(|arg| arg.get_ty()).collect(),
                ret_ty: Box::new(ret_ty.clone()),
            };
            self.push_constraint(Constraint::TypeEqual(inst_constr_ty, actual_ty));
        } else {
            self.push_constraint(Constraint::TypeEqual(inst_constr_ty, ret_ty.clone()));
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
            self.push_constraint(Constraint::TypeEqual(item_tast.get_ty(), elem_ty.clone()));
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

        let body_tast = self.infer_expr(genv, local_env, diagnostics, body);
        let body_ty = body_tast.get_ty();
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

                let body_tast =
                    self.check_expr(genv, local_env, diagnostics, body, expected_ret.as_ref());
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

    #[allow(clippy::too_many_arguments)]
    fn infer_let_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        pat: hir::PatId,
        annotation: &Option<hir::TypeExpr>,
        value: hir::ExprId,
    ) -> tast::Expr {
        let current_tparams_env = local_env.current_tparams_env();
        let annotated_ty = annotation
            .as_ref()
            .map(|ty| tast::Ty::from_hir(genv, ty, &current_tparams_env));

        let (value_tast, value_ty) = if let Some(ann_ty) = &annotated_ty {
            (
                self.check_expr(genv, local_env, diagnostics, value, ann_ty),
                ann_ty.clone(),
            )
        } else {
            let tast = self.infer_expr(genv, local_env, diagnostics, value);
            let ty = tast.get_ty();
            (tast, ty)
        };

        let pat_tast = self.check_pat(genv, local_env, diagnostics, pat, &value_ty);
        // ELet without body returns unit type
        tast::Expr::ELet {
            pat: pat_tast,
            value: Box::new(value_tast),
            ty: tast::Ty::TUnit,
        }
    }

    fn infer_block_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        exprs: &[hir::ExprId],
    ) -> tast::Expr {
        if exprs.is_empty() {
            return tast::Expr::EPrim {
                value: Prim::unit(),
                ty: tast::Ty::TUnit,
            };
        }

        local_env.push_scope();
        let result = self.infer_block_exprs(genv, local_env, diagnostics, exprs);
        local_env.pop_scope(diagnostics);
        result
    }

    fn infer_block_exprs(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        exprs: &[hir::ExprId],
    ) -> tast::Expr {
        if exprs.is_empty() {
            return tast::Expr::EPrim {
                value: Prim::unit(),
                ty: tast::Ty::TUnit,
            };
        }

        let mut tast_exprs = Vec::new();
        for expr in exprs {
            let tast_expr = self.infer_expr(genv, local_env, diagnostics, *expr);
            tast_exprs.push(tast_expr);
        }

        let ty = tast_exprs
            .last()
            .map(|e| e.get_ty())
            .unwrap_or(tast::Ty::TUnit);
        tast::Expr::EBlock {
            exprs: tast_exprs,
            ty,
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn check_let_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        pat: hir::PatId,
        annotation: &Option<hir::TypeExpr>,
        value: hir::ExprId,
        _expected: &tast::Ty,
    ) -> tast::Expr {
        let current_tparams_env = local_env.current_tparams_env();
        let annotated_ty = annotation
            .as_ref()
            .map(|ty| tast::Ty::from_hir(genv, ty, &current_tparams_env));

        let (value_tast, value_ty) = if let Some(ann_ty) = &annotated_ty {
            (
                self.check_expr(genv, local_env, diagnostics, value, ann_ty),
                ann_ty.clone(),
            )
        } else {
            let tast = self.infer_expr(genv, local_env, diagnostics, value);
            let ty = tast.get_ty();
            (tast, ty)
        };

        let pat_tast = self.check_pat(genv, local_env, diagnostics, pat, &value_ty);
        // Standalone ELet returns unit

        tast::Expr::ELet {
            pat: pat_tast,
            value: Box::new(value_tast),
            ty: tast::Ty::TUnit,
        }
    }

    fn check_block_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        exprs: &[hir::ExprId],
        expected: &tast::Ty,
    ) -> tast::Expr {
        if exprs.is_empty() {
            return tast::Expr::EPrim {
                value: Prim::unit(),
                ty: tast::Ty::TUnit,
            };
        }

        local_env.push_scope();
        let result = self.check_block_exprs(genv, local_env, diagnostics, exprs, expected);
        local_env.pop_scope(diagnostics);
        result
    }

    fn check_block_exprs(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        exprs: &[hir::ExprId],
        expected: &tast::Ty,
    ) -> tast::Expr {
        if exprs.is_empty() {
            return tast::Expr::EPrim {
                value: Prim::unit(),
                ty: tast::Ty::TUnit,
            };
        }

        // For check mode, we infer all expressions except the last one
        // which we check against expected
        let mut tast_exprs = Vec::new();
        let len = exprs.len();
        for (i, expr) in exprs.iter().enumerate() {
            let tast_expr = if i == len - 1 {
                // Last expression: check against expected type
                self.check_expr(genv, local_env, diagnostics, *expr, expected)
            } else {
                // Not last: just infer
                self.infer_expr(genv, local_env, diagnostics, *expr)
            };
            tast_exprs.push(tast_expr);
        }

        let ty = tast_exprs
            .last()
            .map(|e| e.get_ty())
            .unwrap_or(tast::Ty::TUnit);
        tast::Expr::EBlock {
            exprs: tast_exprs,
            ty,
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
        for arm in arms.iter() {
            local_env.push_scope();
            let arm_tast = self.check_pat(genv, local_env, diagnostics, arm.pat, &expr_ty);
            let arm_body_tast = self.infer_expr(genv, local_env, diagnostics, arm.body);
            local_env.pop_scope(diagnostics);
            self.push_constraint(Constraint::TypeEqual(
                arm_body_tast.get_ty(),
                arm_ty.clone(),
            ));

            arms_tast.push(tast::Arm {
                pat: arm_tast,
                body: arm_body_tast,
            });
        }
        tast::Expr::EMatch {
            expr: Box::new(expr_tast),
            arms: arms_tast,
            ty: arm_ty,
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
        self.push_constraint(Constraint::TypeEqual(cond_tast.get_ty(), tast::Ty::TBool));

        let then_tast = self.infer_expr(genv, local_env, diagnostics, then_branch);
        let else_tast = self.infer_expr(genv, local_env, diagnostics, else_branch);
        let result_ty = self.fresh_ty_var();

        self.push_constraint(Constraint::TypeEqual(then_tast.get_ty(), result_ty.clone()));
        self.push_constraint(Constraint::TypeEqual(else_tast.get_ty(), result_ty.clone()));

        tast::Expr::EIf {
            cond: Box::new(cond_tast),
            then_branch: Box::new(then_tast),
            else_branch: Box::new(else_tast),
            ty: result_ty,
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
        let cond_tast = self.infer_expr(genv, local_env, diagnostics, cond);
        self.push_constraint(Constraint::TypeEqual(cond_tast.get_ty(), tast::Ty::TBool));

        let body_tast = self.infer_expr(genv, local_env, diagnostics, body);
        self.push_constraint(Constraint::TypeEqual(body_tast.get_ty(), tast::Ty::TUnit));

        tast::Expr::EWhile {
            cond: Box::new(cond_tast),
            body: Box::new(body_tast),
            ty: tast::Ty::TUnit,
        }
    }

    fn infer_go_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        expr: hir::ExprId,
    ) -> tast::Expr {
        let expr_tast = self.infer_expr(genv, local_env, diagnostics, expr);
        // go expression expects a closure () -> unit
        let closure_ty = tast::Ty::TFunc {
            params: vec![],
            ret_ty: Box::new(tast::Ty::TUnit),
        };
        self.push_constraint(Constraint::TypeEqual(expr_tast.get_ty(), closure_ty));

        tast::Expr::EGo {
            expr: Box::new(expr_tast),
            ty: tast::Ty::TUnit,
        }
    }

    fn infer_call_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        call_expr_id: hir::ExprId,
        func: hir::ExprId,
        args: &[hir::ExprId],
    ) -> tast::Expr {
        let func_expr = self.hir_table.expr(func).clone();
        match func_expr {
            hir::Expr::ENameRef {
                res: hir::NameRef::Local(name),
                astptr: func_astptr,
                ..
            } => {
                let mut args_tast = Vec::new();
                let mut arg_types = Vec::new();
                for arg in args.iter() {
                    let arg_tast = self.infer_expr(genv, local_env, diagnostics, *arg);
                    arg_types.push(arg_tast.get_ty());
                    args_tast.push(arg_tast);
                }

                let name_str = self.hir_table.local_ident_name(name);
                if let Some(var_ty) = local_env.lookup_var(name) {
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
                    super::util::push_ice(
                        diagnostics,
                        format!("Variable {} not found in environment", name_str),
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
                let mut args_tast = Vec::new();
                let mut arg_types = Vec::new();
                for arg in args.iter() {
                    let arg_tast = self.infer_expr(genv, local_env, diagnostics, *arg);
                    arg_types.push(arg_tast.get_ty());
                    args_tast.push(arg_tast);
                }
                if let Some(func_ty) = lookup_function_type_by_hint(genv, name.as_str()) {
                    let inst_ty = self.inst_ty(&func_ty);
                    if let tast::Ty::TFunc { params, .. } = &inst_ty
                        && params.len() == args.len()
                        && !params.is_empty()
                    {
                        args_tast.clear();
                        arg_types.clear();
                        for (arg, expected_ty) in args.iter().zip(params.iter()) {
                            let arg_tast =
                                self.check_expr(genv, local_env, diagnostics, *arg, expected_ty);
                            arg_types.push(arg_tast.get_ty());
                            args_tast.push(arg_tast);
                        }
                    }

                    let ret_ty = if name.as_str() == "ref" && args_tast.len() == 1 {
                        let elem_ty =
                            args_tast
                                .first()
                                .map(|arg| arg.get_ty())
                                .unwrap_or_else(|| {
                                    super::util::push_ice(
                                        diagnostics,
                                        "ref call expected one argument but none found",
                                    );
                                    self.fresh_ty_var()
                                });
                        tast::Ty::TRef {
                            elem: Box::new(elem_ty),
                        }
                    } else {
                        self.fresh_ty_var()
                    };
                    let call_site_func_ty = tast::Ty::TFunc {
                        params: arg_types,
                        ret_ty: Box::new(ret_ty.clone()),
                    };
                    self.push_constraint(Constraint::TypeEqual(
                        inst_ty.clone(),
                        call_site_func_ty.clone(),
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
                if let Some((full_name, func_ty)) = lookup_function_path(genv, &path) {
                    let mut args_tast = Vec::new();
                    let mut arg_types = Vec::new();
                    for arg in args.iter() {
                        let arg_tast = self.infer_expr(genv, local_env, diagnostics, *arg);
                        arg_types.push(arg_tast.get_ty());
                        args_tast.push(arg_tast);
                    }

                    let inst_ty = self.inst_ty(&func_ty);
                    if let tast::Ty::TFunc { params, .. } = &inst_ty
                        && params.len() == args.len()
                        && !params.is_empty()
                    {
                        args_tast.clear();
                        arg_types.clear();
                        for (arg, expected_ty) in args.iter().zip(params.iter()) {
                            let arg_tast =
                                self.check_expr(genv, local_env, diagnostics, *arg, expected_ty);
                            arg_types.push(arg_tast.get_ty());
                            args_tast.push(arg_tast);
                        }
                    }

                    let ret_ty = match &inst_ty {
                        tast::Ty::TFunc { ret_ty, .. } => (**ret_ty).clone(),
                        _ => inst_ty.clone(),
                    };

                    self.results.record_expr_ty(func, inst_ty.clone());
                    self.results.record_name_ref_elab(
                        func,
                        NameRefElab::Var {
                            name: full_name.clone(),
                            ty: inst_ty.clone(),
                            astptr,
                        },
                    );
                    self.results.record_call_elab(
                        call_expr_id,
                        CallElab {
                            callee: CalleeElab::Var {
                                name: full_name.clone(),
                                ty: inst_ty.clone(),
                                astptr: None,
                            },
                            args: args.to_vec(),
                        },
                    );
                    return tast::Expr::ECall {
                        func: Box::new(tast::Expr::EVar {
                            name: full_name,
                            ty: inst_ty,
                            astptr: None,
                        }),
                        args: args_tast,
                        ty: ret_ty,
                    };
                }
                if path.len() == 1 {
                    super::util::push_error(
                        diagnostics,
                        format!("Unresolved callee {}", path.display()),
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
                if let Some((trait_name, trait_env)) =
                    super::util::resolve_trait_name(genv, &type_name)
                {
                    let type_ident = tast::TastIdent(trait_name.clone());
                    if let Some(method_ty) =
                        trait_env.lookup_trait_method(&type_ident, &member_ident)
                    {
                        let inst_method_ty = self.inst_ty(&method_ty);
                        if let tast::Ty::TFunc { params, ret_ty } = &inst_method_ty
                            && !args.is_empty()
                        {
                            let Some(receiver_arg) = args.first() else {
                                super::util::push_ice(
                                    diagnostics,
                                    "callee args empty after non-empty check",
                                );
                                return self.error_expr(None);
                            };
                            let receiver_tast =
                                self.infer_expr(genv, local_env, diagnostics, *receiver_arg);
                            if let tast::Ty::TDyn {
                                trait_name: recv_trait,
                            } = receiver_tast.get_ty()
                                && recv_trait == type_ident.0
                            {
                                if params.len() != args.len() {
                                    super::util::push_error(
                                        diagnostics,
                                        format!(
                                            "Trait method {}::{} expects {} arguments but got {}",
                                            type_ident.0,
                                            member_ident.0,
                                            params.len(),
                                            args.len()
                                        ),
                                    );
                                    return self.error_expr(None);
                                }

                                let mut args_tast = Vec::with_capacity(args.len());
                                args_tast.push(receiver_tast);
                                for (arg, expected_ty) in
                                    args.iter().skip(1).zip(params.iter().skip(1))
                                {
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
                                    super::util::push_ice(
                                        diagnostics,
                                        "dyn method params missing receiver",
                                    );
                                }
                                let dyn_method_ty = tast::Ty::TFunc {
                                    params: dyn_params,
                                    ret_ty: ret_ty.clone(),
                                };

                                self.results.record_expr_ty(func, dyn_method_ty.clone());
                                self.results.record_name_ref_elab(
                                    func,
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
                        let inst_method_ty_for_call =
                            instantiate_self_ty(&inst_method_ty, &receiver_ty);
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
                                diagnostics.push(Diagnostic::new(
                                    Stage::Typer,
                                    Severity::Error,
                                    format!(
                                        "Type parameter {} is not constrained by trait {}",
                                        name, type_ident.0
                                    ),
                                ));
                                return tast::Expr::EVar {
                                    name: "<error>".to_string(),
                                    ty: self.fresh_ty_var(),
                                    astptr: None,
                                };
                            }
                            self.push_constraint(Constraint::TypeEqual(
                                call_site_func_ty.clone(),
                                inst_method_ty_for_call.clone(),
                            ));
                        } else {
                            self.push_constraint(Constraint::Overloaded {
                                op: member_ident.clone(),
                                trait_name: type_ident.clone(),
                                call_site_type: call_site_func_ty.clone(),
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
                        self.results.record_expr_ty(func, inst_method_ty_for_call.clone());
                        self.results.record_name_ref_elab(
                            func,
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
                }
                let (resolved_type_name, type_env) =
                    super::util::resolve_type_name(genv, &type_name);
                let type_ident = tast::TastIdent(resolved_type_name.clone());
                let receiver_ty = if type_env.enums().contains_key(&type_ident) {
                    tast::Ty::TEnum {
                        name: resolved_type_name.clone(),
                    }
                } else if type_env.structs().contains_key(&type_ident) {
                    tast::Ty::TStruct {
                        name: resolved_type_name.clone(),
                    }
                } else {
                    super::util::push_error(
                        diagnostics,
                        format!(
                            "Type or trait {} not found for member access",
                            resolved_type_name
                        ),
                    );
                    return self.error_expr(None);
                };
                if let Some(method_ty) =
                    type_env.lookup_inherent_method(&receiver_ty, &member_ident)
                {
                    let inst_method_ty = self.inst_ty(&method_ty);
                    if let tast::Ty::TFunc { params, ret_ty } = inst_method_ty.clone() {
                        if params.len() != args.len() {
                            super::util::push_error(
                                diagnostics,
                                format!(
                                    "Method {} expects {} arguments but got {}",
                                    member,
                                    params.len(),
                                    args.len()
                                ),
                            );
                            return self.error_expr(None);
                        }

                        let mut args_tast = Vec::with_capacity(args.len());
                        for (arg, expected_ty) in args.iter().zip(params.iter()) {
                            let arg_tast =
                                self.check_expr(genv, local_env, diagnostics, *arg, expected_ty);
                            args_tast.push(arg_tast);
                        }

                        self.results.record_call_elab(
                            call_expr_id,
                            CallElab {
                                callee: CalleeElab::InherentMethod {
                                    receiver_ty: receiver_ty.clone(),
                                    method_name: member_ident.clone(),
                                    ty: inst_method_ty.clone(),
                                    astptr: None,
                                },
                                args: args.to_vec(),
                            },
                        );
                        self.results.record_expr_ty(func, inst_method_ty.clone());
                        self.results.record_name_ref_elab(
                            func,
                            NameRefElab::InherentMethod {
                                receiver_ty: receiver_ty.clone(),
                                method_name: member_ident.clone(),
                                ty: inst_method_ty.clone(),
                                astptr,
                            },
                        );
                        tast::Expr::ECall {
                            func: Box::new(tast::Expr::EInherentMethod {
                                receiver_ty: receiver_ty.clone(),
                                method_name: member_ident.clone(),
                                ty: inst_method_ty,
                                astptr: None,
                            }),
                            args: args_tast,
                            ty: (*ret_ty).clone(),
                        }
                    } else {
                        super::util::push_ice(
                            diagnostics,
                            format!("Type member {}::{} is not callable", type_name, member),
                        );
                        self.error_expr(None)
                    }
                } else {
                    super::util::push_error(
                        diagnostics,
                        format!("Method {} not found for type {}", member, type_name),
                    );
                    self.error_expr(None)
                }
            }
            hir::Expr::EField {
                expr: receiver_expr,
                field,
            } => {
                let receiver_tast = self.infer_expr(genv, local_env, diagnostics, receiver_expr);
                let receiver_ty = receiver_tast.get_ty();
                if let Some(method_ty) = lookup_inherent_method_for_ty(
                    genv,
                    &receiver_ty,
                    &tast::TastIdent(field.to_ident_name()),
                ) {
                    let mut args_tast = Vec::with_capacity(args.len() + 1);
                    let mut arg_types = Vec::with_capacity(args.len() + 1);
                    arg_types.push(receiver_ty.clone());
                    args_tast.push(receiver_tast);
                    for arg in args.iter() {
                        let arg_tast = self.infer_expr(genv, local_env, diagnostics, *arg);
                        arg_types.push(arg_tast.get_ty());
                        args_tast.push(arg_tast);
                    }

                    let inst_method_ty = self.inst_ty(&method_ty);
                    let ret_ty = self.fresh_ty_var();
                    let call_site_ty = tast::Ty::TFunc {
                        params: arg_types,
                        ret_ty: Box::new(ret_ty.clone()),
                    };
                    self.push_constraint(Constraint::TypeEqual(
                        inst_method_ty.clone(),
                        call_site_ty,
                    ));

                    self.results.record_expr_ty(func, inst_method_ty.clone());
                    self.results.record_name_ref_elab(
                        func,
                        NameRefElab::InherentMethod {
                            receiver_ty: receiver_ty.clone(),
                            method_name: tast::TastIdent(field.to_ident_name()),
                            ty: inst_method_ty.clone(),
                            astptr: None,
                        },
                    );
                    self.results.record_call_elab(
                        call_expr_id,
                        CallElab {
                            callee: CalleeElab::InherentMethod {
                                receiver_ty: receiver_ty.clone(),
                                method_name: tast::TastIdent(field.to_ident_name()),
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
                            method_name: tast::TastIdent(field.to_ident_name()),
                            ty: inst_method_ty,
                            astptr: None,
                        }),
                        args: args_tast,
                        ty: ret_ty,
                    }
                } else if let tast::Ty::TParam { name } = &receiver_ty {
                    let method_name = tast::TastIdent(field.to_ident_name());
                    let bounds = local_env.tparam_trait_bounds(name).unwrap_or(&[]);
                    let candidates = lookup_bound_trait_methods(genv, bounds, &method_name);
                    match candidates.as_slice() {
                        [(trait_name, method_ty)] => {
                            let inst_method_ty = self.inst_ty(method_ty);
                            let inst_method_ty_for_call =
                                instantiate_self_ty(&inst_method_ty, &receiver_ty);

                            let (params, ret_ty) = match &inst_method_ty_for_call {
                                tast::Ty::TFunc { params, ret_ty } => {
                                    (params.clone(), (**ret_ty).clone())
                                }
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
                                super::util::push_error(
                                    diagnostics,
                                    format!(
                                        "Trait method {}::{} expects {} arguments but got {}",
                                        trait_name.0,
                                        method_name.0,
                                        params.len(),
                                        args.len() + 1
                                    ),
                                );
                                return self.error_expr(None);
                            }

                            let mut args_tast = Vec::with_capacity(args.len() + 1);
                            args_tast.push(receiver_tast);
                            for (arg, expected_ty) in args.iter().zip(params.iter().skip(1)) {
                                args_tast.push(self.check_expr(
                                    genv,
                                    local_env,
                                    diagnostics,
                                    *arg,
                                    expected_ty,
                                ));
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
                                    args: std::iter::once(receiver_expr)
                                        .chain(args.iter().copied())
                                        .collect(),
                                },
                            );
                            self.results.record_expr_ty(func, inst_method_ty_for_call.clone());
                            self.results.record_name_ref_elab(
                                func,
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
                        [] => {
                            diagnostics.push(Diagnostic::new(
                                Stage::Typer,
                                Severity::Error,
                                format!(
                                    "Method {} is not available for type parameter {}",
                                    method_name.0, name
                                ),
                            ));
                            tast::Expr::EVar {
                                name: "<error>".to_string(),
                                ty: self.fresh_ty_var(),
                                astptr: None,
                            }
                        }
                        _ => {
                            let trait_names = candidates
                                .iter()
                                .map(|(t, _)| t.0.clone())
                                .collect::<Vec<_>>()
                                .join(", ");
                            diagnostics.push(Diagnostic::new(
                                Stage::Typer,
                                Severity::Error,
                                format!(
                                    "Ambiguous method {} for type parameter {} (candidates: {}). Use UFCS like Trait::{}(...) to disambiguate",
                                    method_name.0, name, trait_names, method_name.0
                                ),
                            ));
                            tast::Expr::EVar {
                                name: "<error>".to_string(),
                                ty: self.fresh_ty_var(),
                                astptr: None,
                            }
                        }
                    }
                } else {
                    super::util::push_error(
                        diagnostics,
                        format!(
                            "Method {} not found for type {:?}",
                            field.to_ident_name(),
                            receiver_expr
                        ),
                    );
                    self.error_expr(None)
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
                self.push_constraint(Constraint::TypeEqual(expr_ty.clone(), tast::Ty::TBool));
                tast::Expr::EUnary {
                    op,
                    expr: Box::new(expr_tast),
                    ty: tast::Ty::TBool,
                    resolution: tast::UnaryResolution::Builtin,
                }
            }
            common_defs::UnaryOp::Neg => {
                self.push_constraint(Constraint::TypeEqual(expr_ty.clone(), expr_ty.clone()));
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
            _ => self.fresh_ty_var(),
        };

        match op {
            common_defs::BinaryOp::Add => {
                self.push_constraint(Constraint::TypeEqual(lhs_ty.clone(), ret_ty.clone()));
                self.push_constraint(Constraint::TypeEqual(rhs_ty.clone(), ret_ty.clone()));
            }
            common_defs::BinaryOp::Sub
            | common_defs::BinaryOp::Mul
            | common_defs::BinaryOp::Div => {
                self.push_constraint(Constraint::TypeEqual(lhs_ty.clone(), ret_ty.clone()));
                self.push_constraint(Constraint::TypeEqual(rhs_ty.clone(), ret_ty.clone()));
            }
            common_defs::BinaryOp::And | common_defs::BinaryOp::Or => {
                self.push_constraint(Constraint::TypeEqual(lhs_ty.clone(), tast::Ty::TBool));
                self.push_constraint(Constraint::TypeEqual(rhs_ty.clone(), tast::Ty::TBool));
            }
            common_defs::BinaryOp::Less
            | common_defs::BinaryOp::Greater
            | common_defs::BinaryOp::LessEq
            | common_defs::BinaryOp::GreaterEq
            | common_defs::BinaryOp::Eq
            | common_defs::BinaryOp::NotEq => {
                // Comparison operators: lhs and rhs must have same type (numeric types)
                self.push_constraint(Constraint::TypeEqual(lhs_ty.clone(), rhs_ty.clone()));
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
        tuple: hir::ExprId,
        index: usize,
    ) -> tast::Expr {
        let tuple_tast = self.infer_expr(genv, local_env, diagnostics, tuple);
        let tuple_ty = tuple_tast.get_ty();
        match &tuple_ty {
            tast::Ty::TTuple { typs } => {
                let field_ty = typs.get(index).cloned().unwrap_or_else(|| {
                    super::util::push_error(
                        diagnostics,
                        format!(
                            "Tuple index {} out of bounds for type {:?}",
                            index, tuple_ty
                        ),
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
                diagnostics.push(Diagnostic::new(
                    diagnostics::Stage::Typer,
                    diagnostics::Severity::Error,
                    format!(
                        "Cannot project field {} on non-tuple type {:?}",
                        index, tuple_ty
                    ),
                ));
                let ret_ty = self.fresh_ty_var();
                tast::Expr::EProj {
                    tuple: Box::new(tuple_tast),
                    index,
                    ty: ret_ty,
                }
            }
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
        let base_ty = base_tast.get_ty();
        let result_ty = self.fresh_ty_var();
        self.push_constraint(Constraint::StructFieldAccess {
            expr_ty: base_ty.clone(),
            field: tast::TastIdent(field.to_ident_name()),
            result_ty: result_ty.clone(),
        });

        tast::Expr::EField {
            expr: Box::new(base_tast),
            field_name: field.to_ident_name(),
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
        let out = match pat_node {
            hir::Pat::PVar { name, astptr } => {
                self.check_pat_var(local_env, diagnostics, name, Some(astptr), ty)
            }
            hir::Pat::PUnit => self.check_pat_unit(),
            hir::Pat::PBool { value } => self.check_pat_bool(value),
            hir::Pat::PInt { value } => self.check_pat_int(diagnostics, &value, ty),
            hir::Pat::PInt8 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TInt8, ty)
            }
            hir::Pat::PInt16 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TInt16, ty)
            }
            hir::Pat::PInt32 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TInt32, ty)
            }
            hir::Pat::PInt64 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TInt64, ty)
            }
            hir::Pat::PUInt8 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TUint8, ty)
            }
            hir::Pat::PUInt16 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TUint16, ty)
            }
            hir::Pat::PUInt32 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TUint32, ty)
            }
            hir::Pat::PUInt64 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TUint64, ty)
            }
            hir::Pat::PString { value } => self.check_pat_string(&value, ty),
            hir::Pat::PConstr { .. } => {
                self.check_pat_constructor(genv, local_env, diagnostics, pat, ty)
            }
            hir::Pat::PStruct { .. } => {
                self.check_pat_constructor(genv, local_env, diagnostics, pat, ty)
            }
            hir::Pat::PTuple { pats } => {
                self.check_pat_tuple(genv, local_env, diagnostics, &pats, ty)
            }
            hir::Pat::PWild => self.check_pat_wild(ty),
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

    fn check_pat_unit(&self) -> tast::Pat {
        tast::Pat::PPrim {
            value: Prim::Unit { value: () },
            ty: tast::Ty::TUnit,
        }
    }

    fn check_pat_bool(&self, value: bool) -> tast::Pat {
        tast::Pat::PPrim {
            value: Prim::boolean(value),
            ty: tast::Ty::TBool,
        }
    }

    fn check_pat_int(
        &mut self,
        diagnostics: &mut Diagnostics,
        value: &str,
        ty: &tast::Ty,
    ) -> tast::Pat {
        let target_ty = integer_literal_target(ty).unwrap_or(tast::Ty::TInt32);
        let prim = self
            .parse_integer_literal_with_ty(diagnostics, value, &target_ty)
            .unwrap_or_else(|| Prim::zero_for_int_ty(&target_ty));
        self.push_constraint(Constraint::TypeEqual(target_ty.clone(), ty.clone()));
        tast::Pat::PPrim {
            value: prim,
            ty: ty.clone(),
        }
    }

    fn check_pat_typed_int(
        &mut self,
        diagnostics: &mut Diagnostics,
        value: &str,
        literal_ty: &tast::Ty,
        expected_ty: &tast::Ty,
    ) -> tast::Pat {
        let prim = self
            .parse_integer_literal_with_ty(diagnostics, value, literal_ty)
            .unwrap_or_else(|| Prim::zero_for_int_ty(literal_ty));
        self.push_constraint(Constraint::TypeEqual(
            literal_ty.clone(),
            expected_ty.clone(),
        ));
        tast::Pat::PPrim {
            value: prim,
            ty: literal_ty.clone(),
        }
    }

    fn check_pat_string(&mut self, value: &String, ty: &tast::Ty) -> tast::Pat {
        self.push_constraint(Constraint::TypeEqual(tast::Ty::TString, ty.clone()));
        tast::Pat::PPrim {
            value: Prim::string(value.to_owned()),
            ty: tast::Ty::TString,
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
                };

                let Some(variant_ident) = constructor_path.last_ident() else {
                    super::util::push_ice(diagnostics, "Constructor path missing final segment");
                    return self.check_pat_wild(ty);
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
                    super::util::push_error(
                        diagnostics,
                        format!(
                            "Constructor {} not found in environment",
                            constructor_path.display()
                        ),
                    );
                    return self.check_pat_wild(ty);
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
                            return self.check_pat_wild(ty);
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
                            return self.check_pat_wild(ty);
                        };
                        tys.len()
                    }
                    common::Constructor::Struct(_) => {
                        super::util::push_error(
                            diagnostics,
                            format!(
                                "Struct {} patterns must use field syntax",
                                constructor.name().0
                            ),
                        );
                        return self.check_pat_wild(ty);
                    }
                };

                if expected_arity != args.len() {
                    super::util::push_error(
                        diagnostics,
                        format!(
                            "Constructor {} expects {} arguments, but got {}",
                            constructor.name().0,
                            expected_arity,
                            args.len()
                        ),
                    );
                    return self.check_pat_wild(ty);
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

                self.push_constraint(Constraint::TypeEqual(ret_ty.clone(), ty.clone()));

                self.results
                    .record_constructor_pat(pat, constructor.clone());
                tast::Pat::PConstr {
                    constructor,
                    args: args_tast,
                    ty: ret_ty,
                }
            }
            hir::Pat::PStruct { name, fields } => {
                let name_display = name.display();
                let (type_name, type_env) = super::util::resolve_type_name(genv, &name_display);
                let struct_def = type_env.structs().get(&tast::TastIdent(type_name.clone()));
                let Some(struct_def) = struct_def else {
                    super::util::push_error(
                        diagnostics,
                        format!("Struct {} not found when checking pattern", type_name),
                    );
                    return self.check_pat_wild(ty);
                };
                let struct_fields = struct_def.fields.clone();
                if struct_fields.len() != fields.len() {
                    super::util::push_error(
                        diagnostics,
                        format!(
                            "Struct pattern {} expects {} fields, but got {}",
                            type_name,
                            struct_fields.len(),
                            fields.len()
                        ),
                    );
                }

                let mut field_map: HashMap<String, hir::PatId> = HashMap::new();
                for (fname, pat_id) in fields.iter() {
                    if field_map.insert(fname.to_ident_name(), *pat_id).is_some() {
                        super::util::push_error(
                            diagnostics,
                            format!(
                                "Struct pattern {} has duplicate field {}",
                                type_name,
                                fname.to_ident_name()
                            ),
                        );
                    }
                }

                let ctor = type_env.lookup_constructor(&tast::TastIdent(type_name.clone()));
                let Some((constructor, constr_ty)) = ctor else {
                    super::util::push_ice(
                        diagnostics,
                        format!("Struct {} not found when checking constructor", type_name),
                    );
                    return self.check_pat_wild(ty);
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
                            super::util::push_error(
                                diagnostics,
                                format!(
                                    "Struct pattern {} missing field {}",
                                    name_display, field_name.0
                                ),
                            );
                            let wild = self.check_pat_wild(&expected_ty);
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
                    super::util::push_error(
                        diagnostics,
                        format!(
                            "Struct pattern {} has unknown fields: {}",
                            name_display, extra
                        ),
                    );
                }

                self.push_constraint(Constraint::TypeEqual(ret_ty.clone(), ty.clone()));

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
                }
            }
            _ => {
                super::util::push_ice(diagnostics, "Expected constructor pattern");
                self.check_pat_wild(ty)
            }
        }
    }

    fn check_pat_tuple(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
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
        self.push_constraint(Constraint::TypeEqual(pat_ty.clone(), ty.clone()));
        tast::Pat::PTuple {
            items: pats_tast,
            ty: pat_ty,
        }
    }

    fn check_pat_wild(&mut self, ty: &tast::Ty) -> tast::Pat {
        let pat_ty = self.fresh_ty_var();
        self.push_constraint(Constraint::TypeEqual(pat_ty.clone(), ty.clone()));
        tast::Pat::PWild { ty: pat_ty }
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
        tast::Ty::TVec { elem } => is_concrete_dyn_target(elem),
        tast::Ty::TRef { elem } => is_concrete_dyn_target(elem),
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
        | tast::Ty::TString => true,
    }
}

fn has_visible_trait_impl(genv: &PackageTypeEnv, trait_name: &str, for_ty: &tast::Ty) -> bool {
    let key = (trait_name.to_string(), for_ty.clone());
    if genv.current().trait_env.trait_impls.contains_key(&key) {
        return true;
    }
    genv.deps
        .values()
        .any(|env| env.trait_env.trait_impls.contains_key(&key))
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

impl Typer {
    fn parse_integer_literal_with_ty(
        &mut self,
        diagnostics: &mut Diagnostics,
        literal: &str,
        ty: &tast::Ty,
    ) -> Option<Prim> {
        match ty {
            tast::Ty::TInt8 => self
                .parse_signed_integer(diagnostics, literal, "int8")
                .map(|value| Prim::Int8 { value }),
            tast::Ty::TInt16 => self
                .parse_signed_integer(diagnostics, literal, "int16")
                .map(|value| Prim::Int16 { value }),
            tast::Ty::TInt32 => self
                .parse_signed_integer(diagnostics, literal, "int32")
                .map(|value| Prim::Int32 { value }),
            tast::Ty::TInt64 => self
                .parse_signed_integer(diagnostics, literal, "int64")
                .map(|value| Prim::Int64 { value }),
            tast::Ty::TUint8 => self
                .parse_unsigned_integer(diagnostics, literal, "uint8")
                .map(|value| Prim::UInt8 { value }),
            tast::Ty::TUint16 => self
                .parse_unsigned_integer(diagnostics, literal, "uint16")
                .map(|value| Prim::UInt16 { value }),
            tast::Ty::TUint32 => self
                .parse_unsigned_integer(diagnostics, literal, "uint32")
                .map(|value| Prim::UInt32 { value }),
            tast::Ty::TUint64 => self
                .parse_unsigned_integer(diagnostics, literal, "uint64")
                .map(|value| Prim::UInt64 { value }),
            _ => None,
        }
    }

    fn parse_signed_integer<T>(
        &mut self,
        diagnostics: &mut Diagnostics,
        literal: &str,
        ty_name: &str,
    ) -> Option<T>
    where
        T: std::str::FromStr<Err = std::num::ParseIntError>,
    {
        match literal.parse::<T>() {
            Ok(value) => Some(value),
            Err(err) => {
                match err.kind() {
                    IntErrorKind::Empty | IntErrorKind::InvalidDigit => {
                        self.report_invalid_integer_literal(diagnostics, literal);
                    }
                    _ => {
                        diagnostics.push(Diagnostic::new(
                            diagnostics::Stage::Typer,
                            diagnostics::Severity::Error,
                            format!("Integer literal {} does not fit in {}", literal, ty_name),
                        ));
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
    ) -> Option<T>
    where
        T: std::str::FromStr<Err = std::num::ParseIntError>,
    {
        if literal.starts_with('-') {
            diagnostics.push(Diagnostic::new(
                diagnostics::Stage::Typer,
                diagnostics::Severity::Error,
                format!("Integer literal {} does not fit in {}", literal, ty_name),
            ));
            return None;
        }

        match literal.parse::<T>() {
            Ok(value) => Some(value),
            Err(err) => {
                match err.kind() {
                    IntErrorKind::Empty | IntErrorKind::InvalidDigit => {
                        self.report_invalid_integer_literal(diagnostics, literal);
                    }
                    _ => {
                        diagnostics.push(Diagnostic::new(
                            diagnostics::Stage::Typer,
                            diagnostics::Severity::Error,
                            format!("Integer literal {} does not fit in {}", literal, ty_name),
                        ));
                    }
                }
                None
            }
        }
    }

    fn report_invalid_integer_literal(&mut self, diagnostics: &mut Diagnostics, literal: &str) {
        diagnostics.push(Diagnostic::new(
            diagnostics::Stage::Typer,
            diagnostics::Severity::Error,
            format!("Invalid integer literal: {}", literal),
        ));
    }

    fn ensure_float_literal_fits(
        &mut self,
        diagnostics: &mut Diagnostics,
        value: f64,
        ty: &tast::Ty,
    ) {
        if !value.is_finite() {
            diagnostics.push(Diagnostic::new(
                diagnostics::Stage::Typer,
                diagnostics::Severity::Error,
                "Float literal must be finite".to_string(),
            ));
            return;
        }

        match ty {
            tast::Ty::TFloat32 => {
                if value < f32::MIN as f64 || value > f32::MAX as f64 {
                    diagnostics.push(Diagnostic::new(
                        diagnostics::Stage::Typer,
                        diagnostics::Severity::Error,
                        format!("Float literal {} does not fit in float32", value),
                    ));
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
    ) -> Option<Prim> {
        match literal.parse::<f64>() {
            Ok(value) => {
                self.ensure_float_literal_fits(diagnostics, value, ty);
                match ty {
                    tast::Ty::TFloat32 => Some(Prim::Float32 {
                        value: value as f32,
                    }),
                    tast::Ty::TFloat64 => Some(Prim::Float64 { value }),
                    _ => None,
                }
            }
            Err(_) => {
                diagnostics.push(Diagnostic::new(
                    diagnostics::Stage::Typer,
                    diagnostics::Severity::Error,
                    format!("Invalid float literal: {}", literal),
                ));
                None
            }
        }
    }
}

fn lookup_function_path(genv: &PackageTypeEnv, path: &hir::Path) -> Option<(String, tast::Ty)> {
    if path.len() == 1 {
        let name = path.last_ident()?.clone();
        return genv
            .current()
            .get_type_of_function(name.as_str())
            .map(|ty| (name, ty));
    }

    let full_name = path.display();
    let package = path.segments().first()?.seg().as_str();
    if package == "Builtin" {
        let name = path.last_ident()?.clone();
        return genv
            .current()
            .get_type_of_function(name.as_str())
            .map(|ty| (name, ty));
    }
    if package == genv.package {
        return genv
            .current()
            .get_type_of_function(&full_name)
            .map(|ty| (full_name, ty));
    }
    if let Some(env) = genv.deps.get(package) {
        env.get_type_of_function(&full_name)
            .map(|ty| (full_name, ty))
    } else {
        eprintln!(
            "lookup_function_path: package '{}' not found when resolving function '{}'",
            package, full_name
        );
        None
    }
}

fn lookup_function_type_by_hint(genv: &PackageTypeEnv, hint: &str) -> Option<tast::Ty> {
    if let Some(func_ty) = genv.current().get_type_of_function(hint) {
        return Some(func_ty);
    }
    let segments = hint.split("::").map(|seg| seg.to_string()).collect();
    let path = hir::Path::from_idents(segments);
    lookup_function_path(genv, &path).map(|(_, ty)| ty)
}

fn lookup_inherent_method_for_ty(
    genv: &PackageTypeEnv,
    receiver_ty: &tast::Ty,
    method: &tast::TastIdent,
) -> Option<tast::Ty> {
    let env = env_for_receiver_ty(genv, receiver_ty);
    env.lookup_inherent_method(receiver_ty, method)
}

fn env_for_receiver_ty<'a>(genv: &'a PackageTypeEnv, receiver_ty: &tast::Ty) -> &'a GlobalTypeEnv {
    match receiver_ty {
        tast::Ty::TEnum { name } | tast::Ty::TStruct { name } => {
            let (_resolved, env) = super::util::resolve_type_name(genv, name);
            env
        }
        tast::Ty::TApp { ty, .. } => env_for_receiver_ty(genv, ty),
        tast::Ty::TRef { .. } | tast::Ty::TVec { .. } => genv.current(),
        _ => genv.current(),
    }
}

fn instantiate_self_ty(ty: &tast::Ty, self_ty: &tast::Ty) -> tast::Ty {
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
        tast::Ty::TVec { elem } => tast::Ty::TVec {
            elem: Box::new(instantiate_self_ty(elem, self_ty)),
        },
        tast::Ty::TRef { elem } => tast::Ty::TRef {
            elem: Box::new(instantiate_self_ty(elem, self_ty)),
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
    let mut result = Vec::new();
    for trait_name in bounds.iter() {
        let Some((resolved_trait, trait_env)) =
            super::util::resolve_trait_name(genv, &trait_name.0)
        else {
            continue;
        };
        let resolved_ident = tast::TastIdent(resolved_trait);
        if let Some(method_ty) = trait_env.lookup_trait_method(&resolved_ident, method) {
            result.push((resolved_ident, method_ty));
        }
    }
    result
}
