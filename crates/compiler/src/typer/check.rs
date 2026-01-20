use std::{collections::HashMap, num::IntErrorKind};

use diagnostics::{Severity, Stage};
use parser::{Diagnostic, Diagnostics, syntax::MySyntaxNodePtr};

use crate::common::{self, Prim};
use crate::fir::{self};
use crate::typer::localenv::LocalTypeEnv;
use crate::{
    env::{Constraint, GlobalTypeEnv, PackageTypeEnv},
    tast::{self},
    typer::Typer,
};

impl Typer {
    pub fn infer_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        e: fir::ExprId,
    ) -> tast::Expr {
        let expr = self.fir_table.expr(e).clone();
        match expr {
            fir::Expr::ENameRef { res, hint, astptr } => {
                self.infer_res_expr(genv, local_env, diagnostics, &res, &hint, astptr)
            }
            fir::Expr::EUnit => tast::Expr::EPrim {
                value: Prim::unit(),
                ty: tast::Ty::TUnit,
            },
            fir::Expr::EBool { value } => tast::Expr::EPrim {
                value: Prim::boolean(value),
                ty: tast::Ty::TBool,
            },
            fir::Expr::EInt { value } => {
                let ty = tast::Ty::TInt32;
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            fir::Expr::EInt8 { value } => {
                let ty = tast::Ty::TInt8;
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            fir::Expr::EInt16 { value } => {
                let ty = tast::Ty::TInt16;
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            fir::Expr::EInt32 { value } => {
                let ty = tast::Ty::TInt32;
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            fir::Expr::EInt64 { value } => {
                let ty = tast::Ty::TInt64;
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            fir::Expr::EUInt8 { value } => {
                let ty = tast::Ty::TUint8;
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            fir::Expr::EUInt16 { value } => {
                let ty = tast::Ty::TUint16;
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            fir::Expr::EUInt32 { value } => {
                let ty = tast::Ty::TUint32;
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            fir::Expr::EUInt64 { value } => {
                let ty = tast::Ty::TUint64;
                let prim = self
                    .parse_integer_literal_with_ty(diagnostics, &value, &ty)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            fir::Expr::EFloat { value } => {
                self.ensure_float_literal_fits(diagnostics, value, &tast::Ty::TFloat64);
                let ty = tast::Ty::TFloat64;
                tast::Expr::EPrim {
                    value: Prim::from_float_literal(value, &ty),
                    ty,
                }
            }
            fir::Expr::EFloat32 { value } => {
                let ty = tast::Ty::TFloat32;
                let prim = self
                    .parse_float_literal_with_ty(diagnostics, &value, &ty)
                    .unwrap_or_else(|| Prim::from_float_literal(0.0, &ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            fir::Expr::EFloat64 { value } => {
                let ty = tast::Ty::TFloat64;
                let prim = self
                    .parse_float_literal_with_ty(diagnostics, &value, &ty)
                    .unwrap_or_else(|| Prim::from_float_literal(0.0, &ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            fir::Expr::EString { value } => tast::Expr::EPrim {
                value: Prim::string(value),
                ty: tast::Ty::TString,
            },
            fir::Expr::EConstr { constructor, args } => {
                self.infer_constructor_expr(genv, local_env, diagnostics, &constructor, &args)
            }
            fir::Expr::EStructLiteral { name, fields } => {
                self.infer_struct_literal_expr(genv, local_env, diagnostics, &name, &fields)
            }
            fir::Expr::ETuple { items } => {
                self.infer_tuple_expr(genv, local_env, diagnostics, &items)
            }
            fir::Expr::EArray { items } => {
                self.infer_array_expr(genv, local_env, diagnostics, &items)
            }
            fir::Expr::EClosure { params, body } => {
                self.infer_closure_expr(genv, local_env, diagnostics, &params, body)
            }
            fir::Expr::ELet {
                pat,
                annotation,
                value,
            } => self.infer_let_expr(genv, local_env, diagnostics, pat, &annotation, value),
            fir::Expr::EBlock { exprs } => {
                self.infer_block_expr(genv, local_env, diagnostics, &exprs)
            }
            fir::Expr::EMatch { expr, arms } => {
                self.infer_match_expr(genv, local_env, diagnostics, expr, &arms, None)
            }
            fir::Expr::EIf {
                cond,
                then_branch,
                else_branch,
            } => self.infer_if_expr(genv, local_env, diagnostics, cond, then_branch, else_branch),
            fir::Expr::EWhile { cond, body } => {
                self.infer_while_expr(genv, local_env, diagnostics, cond, body)
            }
            fir::Expr::EGo { expr } => self.infer_go_expr(genv, local_env, diagnostics, expr),
            fir::Expr::ECall { func, args } => {
                self.infer_call_expr(genv, local_env, diagnostics, func, &args)
            }
            fir::Expr::EUnary { op, expr } => {
                self.infer_unary_expr(genv, local_env, diagnostics, op, expr)
            }
            fir::Expr::EBinary { op, lhs, rhs } => {
                self.infer_binary_expr(genv, local_env, diagnostics, op, lhs, rhs)
            }
            fir::Expr::EProj { tuple, index } => {
                self.infer_proj_expr(genv, local_env, diagnostics, tuple, index)
            }
            fir::Expr::EField { expr, field } => {
                self.infer_field_expr(genv, local_env, diagnostics, expr, &field, None)
            }
        }
    }

    pub fn check_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        e: fir::ExprId,
        expected: &tast::Ty,
    ) -> tast::Expr {
        let expr = self.fir_table.expr(e).clone();
        let expr_tast = match expr {
            fir::Expr::EUnary {
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
            fir::Expr::EBinary { op, lhs, rhs }
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
            fir::Expr::EClosure { params, body } => {
                self.check_closure_expr(genv, local_env, diagnostics, &params, body, expected)
            }
            fir::Expr::ELet {
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
            fir::Expr::EBlock { exprs } => {
                self.check_block_expr(genv, local_env, diagnostics, &exprs, expected)
            }
            fir::Expr::ETuple { items } if matches!(expected, tast::Ty::TTuple { typs } if typs.len() == items.len()) =>
            {
                let expected_elem_tys = match expected {
                    tast::Ty::TTuple { typs } => typs.clone(),
                    _ => unreachable!("Tuple guard ensures tuple type"),
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
            fir::Expr::EIf {
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
            fir::Expr::EMatch { expr, arms } => {
                let expr_tast = self.infer_expr(genv, local_env, diagnostics, expr);
                let expr_ty = expr_tast.get_ty();

                let mut arms_tast = Vec::new();
                for arm in arms.iter() {
                    local_env.push_scope();
                    let arm_tast = self.check_pat(genv, local_env, diagnostics, arm.pat, &expr_ty);
                    let arm_body_tast =
                        self.check_expr(genv, local_env, diagnostics, arm.body, expected);
                    local_env.pop_scope();

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

        let expr_tast = self.coerce_to_expected_dyn(genv, diagnostics, expr_tast, expected);
        self.push_constraint(Constraint::TypeEqual(expr_tast.get_ty(), expected.clone()));
        expr_tast
    }

    fn coerce_to_expected_dyn(
        &mut self,
        genv: &PackageTypeEnv,
        diagnostics: &mut Diagnostics,
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
        _diagnostics: &mut Diagnostics,
        res: &fir::NameRef,
        hint: &str,
        astptr: Option<MySyntaxNodePtr>,
    ) -> tast::Expr {
        match res {
            fir::NameRef::Local(local_id) => {
                let name_str = self.fir_table.local_ident_name(*local_id);
                if let Some(ty) = local_env.lookup_var(*local_id) {
                    tast::Expr::EVar {
                        name: name_str,
                        ty: ty.clone(),
                        astptr,
                    }
                } else {
                    panic!("Variable {} not found in environment", name_str);
                }
            }
            fir::NameRef::Def(_def_id) => {
                let func_ty = lookup_function_type_by_hint(genv, hint)
                    .unwrap_or_else(|| panic!("Function {} not found in environment", hint));
                let inst_ty = self.inst_ty(&func_ty);
                tast::Expr::EVar {
                    name: hint.to_string(),
                    ty: inst_ty,
                    astptr,
                }
            }
            fir::NameRef::Builtin(_builtin_id) => {
                let func_ty = lookup_function_type_by_hint(genv, hint)
                    .unwrap_or_else(|| panic!("Builtin {} not found in environment", hint));
                let inst_ty = self.inst_ty(&func_ty);
                tast::Expr::EVar {
                    name: hint.to_string(),
                    ty: inst_ty,
                    astptr,
                }
            }
            fir::NameRef::Unresolved(path) => self.infer_unresolved_path_expr(genv, path, astptr),
        }
    }

    fn infer_unresolved_path_expr(
        &mut self,
        genv: &PackageTypeEnv,
        path: &fir::Path,
        astptr: Option<MySyntaxNodePtr>,
    ) -> tast::Expr {
        if path.len() == 1 {
            let name = path.last_ident().unwrap();
            if let Some(func_ty) = genv.current().get_type_of_function(name.as_str()) {
                let inst_ty = self.inst_ty(&func_ty);
                return tast::Expr::EVar {
                    name: name.clone(),
                    ty: inst_ty,
                    astptr,
                };
            }
            panic!("Variable {} not found in environment", name);
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
        let member = path.last_ident().unwrap();
        self.infer_type_member_expr(genv, &type_name, member, astptr)
    }

    fn infer_type_member_expr(
        &mut self,
        genv: &PackageTypeEnv,
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
            panic!(
                "Type or trait {} not found for member access",
                resolved_type_name
            );
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
            panic!("Method {} not found for type {}", member, type_name);
        }
    }

    fn constructor_path_from_id(&self, ctor_id: &fir::ConstructorId) -> fir::Path {
        match ctor_id {
            fir::ConstructorId::EnumVariant {
                enum_def,
                variant_idx,
            } => {
                if let fir::Def::EnumDef(enum_def_data) = self.fir_table.def(*enum_def) {
                    let variant_name = enum_def_data.variants[*variant_idx as usize]
                        .0
                        .to_ident_name();
                    let mut segments = self.fir_table.def_path(*enum_def).segments.clone();
                    segments.push(fir::PathSegment::new(variant_name));
                    fir::Path::new(segments)
                } else {
                    panic!("Constructor points to non-enum DefId");
                }
            }
        }
    }

    fn infer_constructor_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        constructor_ref: &fir::ConstructorRef,
        args: &[fir::ExprId],
    ) -> tast::Expr {
        let constructor_path = match constructor_ref {
            fir::ConstructorRef::Resolved(ctor_id) => self.constructor_path_from_id(ctor_id),
            fir::ConstructorRef::Unresolved(path) => path.clone(),
        };

        let variant_ident = constructor_path
            .last_ident()
            .unwrap_or_else(|| panic!("Constructor path missing final segment"));
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
        let (constructor, constr_ty) = enum_env
            .lookup_constructor_with_namespace(enum_ident.as_ref(), &variant_name)
            .unwrap_or_else(|| {
                panic!(
                    "Constructor {} not found in environment",
                    constructor_path.display()
                )
            });

        let expected_arity = match &constructor {
            common::Constructor::Enum(enum_constructor) => enum_env
                .enums()
                .get(&enum_constructor.type_name)
                .map(|def| def.variants[enum_constructor.index].1.len())
                .unwrap_or_else(|| {
                    panic!(
                        "Enum {} not found when checking constructor {}",
                        enum_constructor.type_name.0,
                        constructor.name().0
                    )
                }),
            common::Constructor::Struct(struct_constructor) => enum_env
                .structs()
                .get(&struct_constructor.type_name)
                .map(|def| def.fields.len())
                .unwrap_or_else(|| {
                    panic!(
                        "Struct {} not found when checking constructor {}",
                        struct_constructor.type_name.0,
                        constructor.name().0
                    )
                }),
        };

        if expected_arity != args.len() {
            panic!(
                "Constructor {} expects {} arguments, but got {}",
                constructor.name().0,
                expected_arity,
                args.len()
            );
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
        name: &fir::QualifiedPath,
        fields: &[(fir::FirIdent, fir::ExprId)],
    ) -> tast::Expr {
        let name_display = name.display();
        let (resolved_name, type_env) = super::util::resolve_type_name(genv, &name_display);
        let (constructor, constr_ty) = type_env
            .lookup_constructor(&tast::TastIdent(resolved_name.clone()))
            .unwrap_or_else(|| panic!("Constructor {} not found in environment", resolved_name));

        let struct_fields = match &constructor {
            common::Constructor::Struct(struct_constructor) => {
                let type_name = &struct_constructor.type_name;
                let struct_def = type_env.structs().get(type_name).unwrap_or_else(|| {
                    panic!(
                        "Struct {} not found when checking literal {}",
                        type_name.0, resolved_name
                    )
                });
                struct_def.fields.clone()
            }
            common::Constructor::Enum { .. } => {
                panic!(
                    "Constructor {} refers to an enum, but a struct literal was used",
                    name_display
                )
            }
        };

        let inst_constr_ty = self.inst_ty(&constr_ty);
        let param_tys = match &inst_constr_ty {
            tast::Ty::TFunc { params, .. } => params.clone(),
            _ => Vec::new(),
        };

        if !param_tys.is_empty() && param_tys.len() != struct_fields.len() {
            panic!(
                "Constructor {} expects {} fields, but got {}",
                resolved_name,
                param_tys.len(),
                struct_fields.len()
            );
        }

        let mut field_positions: HashMap<tast::TastIdent, usize> = HashMap::new();
        for (idx, (fname, _)) in struct_fields.iter().enumerate() {
            field_positions.insert(fname.clone(), idx);
        }

        let mut ordered_args: Vec<Option<tast::Expr>> = vec![None; struct_fields.len()];
        for (field_name, expr) in fields.iter() {
            let idx = field_positions
                .get(&tast::TastIdent(field_name.to_ident_name()))
                .unwrap_or_else(|| {
                    panic!(
                        "Unknown field {} on struct literal {}",
                        field_name.to_ident_name(),
                        resolved_name
                    )
                });
            if ordered_args[*idx].is_some() {
                panic!(
                    "Duplicate field {} in struct literal {}",
                    field_name.to_ident_name(),
                    name_display
                );
            }
            let field_expr = if let Some(expected_ty) = param_tys.get(*idx) {
                self.check_expr(genv, local_env, diagnostics, *expr, expected_ty)
            } else {
                self.infer_expr(genv, local_env, diagnostics, *expr)
            };
            ordered_args[*idx] = Some(field_expr);
        }

        for (idx, slot) in ordered_args.iter().enumerate() {
            if slot.is_none() {
                let missing = &struct_fields[idx].0;
                panic!(
                    "Missing field {} in struct literal {}",
                    missing.0, name_display
                );
            }
        }

        let args_tast: Vec<tast::Expr> = ordered_args
            .into_iter()
            .map(|arg| arg.expect("field checked to exist"))
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
        items: &[fir::ExprId],
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
        items: &[fir::ExprId],
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
        params: &[fir::ClosureParam],
        body: fir::ExprId,
    ) -> tast::Expr {
        local_env.begin_closure();
        let mut params_tast = Vec::new();
        let mut param_tys = Vec::new();
        let current_tparams_env = local_env.current_tparams_env();

        for param in params.iter() {
            let name_str = self.fir_table.local_ident_name(param.name);
            let param_ty = match &param.ty {
                Some(ty) => tast::Ty::from_fir(genv, ty, &current_tparams_env),
                None => self.fresh_ty_var(),
            };
            local_env.insert_var(param.name, param_ty.clone());
            param_tys.push(param_ty.clone());
            params_tast.push(tast::ClosureParam {
                name: name_str,
                ty: param_ty,
                astptr: Some(param.astptr),
            });
        }

        let body_tast = self.infer_expr(genv, local_env, diagnostics, body);
        let body_ty = body_tast.get_ty();
        let captures = local_env.end_closure(&self.fir_table);

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
        params: &[fir::ClosureParam],
        body: fir::ExprId,
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
                    let name_str = self.fir_table.local_ident_name(param.name);
                    let annotated_ty = param
                        .ty
                        .as_ref()
                        .map(|ty| tast::Ty::from_fir(genv, ty, &current_tparams_env));

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
                let captures = local_env.end_closure(&self.fir_table);

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
        pat: fir::PatId,
        annotation: &Option<fir::TypeExpr>,
        value: fir::ExprId,
    ) -> tast::Expr {
        let current_tparams_env = local_env.current_tparams_env();
        let annotated_ty = annotation
            .as_ref()
            .map(|ty| tast::Ty::from_fir(genv, ty, &current_tparams_env));

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
        exprs: &[fir::ExprId],
    ) -> tast::Expr {
        if exprs.is_empty() {
            return tast::Expr::EPrim {
                value: Prim::unit(),
                ty: tast::Ty::TUnit,
            };
        }

        local_env.push_scope();
        let result = self.infer_block_exprs(genv, local_env, diagnostics, exprs);
        local_env.pop_scope();
        result
    }

    fn infer_block_exprs(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        exprs: &[fir::ExprId],
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
        pat: fir::PatId,
        annotation: &Option<fir::TypeExpr>,
        value: fir::ExprId,
        _expected: &tast::Ty,
    ) -> tast::Expr {
        let current_tparams_env = local_env.current_tparams_env();
        let annotated_ty = annotation
            .as_ref()
            .map(|ty| tast::Ty::from_fir(genv, ty, &current_tparams_env));

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
        exprs: &[fir::ExprId],
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
        local_env.pop_scope();
        result
    }

    fn check_block_exprs(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        exprs: &[fir::ExprId],
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
        expr: fir::ExprId,
        arms: &[fir::Arm],
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
            local_env.pop_scope();
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
        cond: fir::ExprId,
        then_branch: fir::ExprId,
        else_branch: fir::ExprId,
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
        cond: fir::ExprId,
        body: fir::ExprId,
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
        expr: fir::ExprId,
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
        func: fir::ExprId,
        args: &[fir::ExprId],
    ) -> tast::Expr {
        let func_expr = self.fir_table.expr(func).clone();
        match func_expr {
            fir::Expr::ENameRef {
                res: fir::NameRef::Local(name),
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

                let name_str = self.fir_table.local_ident_name(name);
                if let Some(var_ty) = local_env.lookup_var(name) {
                    let ret_ty = self.fresh_ty_var();
                    let call_site_func_ty = tast::Ty::TFunc {
                        params: arg_types,
                        ret_ty: Box::new(ret_ty.clone()),
                    };
                    self.push_constraint(Constraint::TypeEqual(
                        var_ty.clone(),
                        call_site_func_ty.clone(),
                    ));

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
                    panic!("Variable {} not found in environment", name_str);
                }
            }
            fir::Expr::ENameRef {
                res: fir::NameRef::Def(_) | fir::NameRef::Builtin(_),
                hint,
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
                        tast::Ty::TRef {
                            elem: Box::new(args_tast[0].get_ty()),
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
                    panic!("Variable {} not found in environment", name);
                }
            }
            fir::Expr::ENameRef {
                res: fir::NameRef::Unresolved(path),
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
                    panic!("Unresolved callee {}", path.display());
                }
                let type_name = path
                    .namespace_segments()
                    .iter()
                    .map(|seg| seg.seg().clone())
                    .collect::<Vec<_>>()
                    .join("::");
                let member = path.last_ident().unwrap();
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
                            let receiver_tast =
                                self.infer_expr(genv, local_env, diagnostics, args[0]);
                            if let tast::Ty::TDyn {
                                trait_name: recv_trait,
                            } = receiver_tast.get_ty()
                                && recv_trait == type_ident.0
                            {
                                if params.len() != args.len() {
                                    panic!(
                                        "Trait method {}::{} expects {} arguments but got {}",
                                        type_ident.0,
                                        member_ident.0,
                                        params.len(),
                                        args.len()
                                    );
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
                                dyn_params[0] = tast::Ty::TDyn {
                                    trait_name: type_ident.0.clone(),
                                };
                                let dyn_method_ty = tast::Ty::TFunc {
                                    params: dyn_params,
                                    ret_ty: ret_ty.clone(),
                                };

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

                        let ret_ty = self.fresh_ty_var();
                        let call_site_func_ty = tast::Ty::TFunc {
                            params: arg_types,
                            ret_ty: Box::new(ret_ty.clone()),
                        };

                        let receiver_ty = args_tast
                            .first()
                            .map(|arg| arg.get_ty())
                            .unwrap_or(tast::Ty::TUnit);
                        let inst_method_ty_for_call =
                            instantiate_self_ty(&inst_method_ty, &receiver_ty);

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

                        return tast::Expr::ECall {
                            func: Box::new(tast::Expr::ETraitMethod {
                                trait_name: type_ident.clone(),
                                method_name: member_ident.clone(),
                                ty: inst_method_ty_for_call,
                                astptr: None,
                            }),
                            args: args_tast,
                            ty: ret_ty,
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
                    panic!(
                        "Type or trait {} not found for member access",
                        resolved_type_name
                    );
                };
                if let Some(method_ty) =
                    type_env.lookup_inherent_method(&receiver_ty, &member_ident)
                {
                    let inst_method_ty = self.inst_ty(&method_ty);
                    if let tast::Ty::TFunc { params, ret_ty } = inst_method_ty.clone() {
                        if params.len() != args.len() {
                            panic!(
                                "Method {} expects {} arguments but got {}",
                                member,
                                params.len(),
                                args.len()
                            );
                        }

                        let mut args_tast = Vec::with_capacity(args.len());
                        for (arg, expected_ty) in args.iter().zip(params.iter()) {
                            let arg_tast =
                                self.check_expr(genv, local_env, diagnostics, *arg, expected_ty);
                            args_tast.push(arg_tast);
                        }

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
                        panic!("Type member {}::{} is not callable", type_name, member);
                    }
                } else {
                    panic!("Method {} not found for type {}", member, type_name);
                }
            }
            fir::Expr::EField {
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
                            let mut args_tast = Vec::with_capacity(args.len() + 1);
                            let mut arg_types = Vec::with_capacity(args.len() + 1);
                            arg_types.push(receiver_ty.clone());
                            args_tast.push(receiver_tast);
                            for arg in args.iter() {
                                let arg_tast = self.infer_expr(genv, local_env, diagnostics, *arg);
                                arg_types.push(arg_tast.get_ty());
                                args_tast.push(arg_tast);
                            }

                            let inst_method_ty = self.inst_ty(method_ty);
                            let inst_method_ty_for_call =
                                instantiate_self_ty(&inst_method_ty, &receiver_ty);
                            let ret_ty = self.fresh_ty_var();
                            let call_site_ty = tast::Ty::TFunc {
                                params: arg_types,
                                ret_ty: Box::new(ret_ty.clone()),
                            };
                            self.push_constraint(Constraint::TypeEqual(
                                inst_method_ty_for_call.clone(),
                                call_site_ty,
                            ));

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
                    panic!(
                        "Method {} not found for type {:?}",
                        field.to_ident_name(),
                        receiver_expr
                    );
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
        expr: fir::ExprId,
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
        lhs: fir::ExprId,
        rhs: fir::ExprId,
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
        tuple: fir::ExprId,
        index: usize,
    ) -> tast::Expr {
        let tuple_tast = self.infer_expr(genv, local_env, diagnostics, tuple);
        let tuple_ty = tuple_tast.get_ty();
        match &tuple_ty {
            tast::Ty::TTuple { typs } => {
                let field_ty = typs.get(index).cloned().unwrap_or_else(|| {
                    panic!(
                        "Tuple index {} out of bounds for type {:?}",
                        index, tuple_ty
                    )
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
        expr: fir::ExprId,
        field: &fir::FirIdent,
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
        pat: fir::PatId,
        ty: &tast::Ty,
    ) -> tast::Pat {
        let pat_node = self.fir_table.pat(pat).clone();
        match pat_node {
            fir::Pat::PVar { name, astptr } => {
                self.check_pat_var(local_env, diagnostics, name, Some(astptr), ty)
            }
            fir::Pat::PUnit => self.check_pat_unit(),
            fir::Pat::PBool { value } => self.check_pat_bool(value),
            fir::Pat::PInt { value } => self.check_pat_int(diagnostics, &value, ty),
            fir::Pat::PInt8 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TInt8, ty)
            }
            fir::Pat::PInt16 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TInt16, ty)
            }
            fir::Pat::PInt32 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TInt32, ty)
            }
            fir::Pat::PInt64 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TInt64, ty)
            }
            fir::Pat::PUInt8 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TUint8, ty)
            }
            fir::Pat::PUInt16 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TUint16, ty)
            }
            fir::Pat::PUInt32 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TUint32, ty)
            }
            fir::Pat::PUInt64 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TUint64, ty)
            }
            fir::Pat::PString { value } => self.check_pat_string(&value, ty),
            fir::Pat::PConstr { .. } => {
                self.check_pat_constructor(genv, local_env, diagnostics, pat, ty)
            }
            fir::Pat::PStruct { .. } => {
                self.check_pat_constructor(genv, local_env, diagnostics, pat, ty)
            }
            fir::Pat::PTuple { pats } => {
                self.check_pat_tuple(genv, local_env, diagnostics, &pats, ty)
            }
            fir::Pat::PWild => self.check_pat_wild(ty),
        }
    }

    fn check_pat_var(
        &mut self,
        local_env: &mut LocalTypeEnv,
        _diagnostics: &mut Diagnostics,
        name: fir::LocalId,
        astptr: Option<MySyntaxNodePtr>,
        ty: &tast::Ty,
    ) -> tast::Pat {
        local_env.insert_var(name, ty.clone());
        let name_str = self.fir_table.local_ident_name(name);
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
        pat: fir::PatId,
        ty: &tast::Ty,
    ) -> tast::Pat {
        let pat_node = self.fir_table.pat(pat);
        let kind = pat_node.clone();
        match kind {
            fir::Pat::PConstr {
                constructor: constructor_ref,
                args,
            } => {
                let constructor_path = match &constructor_ref {
                    fir::ConstructorRef::Resolved(ctor_id) => {
                        self.constructor_path_from_id(ctor_id)
                    }
                    fir::ConstructorRef::Unresolved(path) => path.clone(),
                };

                let variant_ident = constructor_path
                    .last_ident()
                    .unwrap_or_else(|| panic!("Constructor path missing final segment"));
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
                let (constructor, constr_ty) = enum_env
                    .lookup_constructor_with_namespace(enum_ident.as_ref(), &variant_name)
                    .unwrap_or_else(|| {
                        panic!(
                            "Constructor {} not found in environment",
                            constructor_path.display()
                        )
                    });

                let expected_arity = match &constructor {
                    common::Constructor::Enum(enum_constructor) => enum_env
                        .enums()
                        .get(&enum_constructor.type_name)
                        .map(|def| def.variants[enum_constructor.index].1.len())
                        .unwrap_or_else(|| {
                            panic!(
                                "Enum {} not found when checking constructor {}",
                                enum_constructor.type_name.0,
                                constructor.name().0
                            )
                        }),
                    common::Constructor::Struct(_) => {
                        panic!(
                            "Struct {} patterns must use field syntax",
                            constructor.name().0
                        )
                    }
                };

                if expected_arity != args.len() {
                    panic!(
                        "Constructor {} expects {} arguments, but got {}",
                        constructor.name().0,
                        expected_arity,
                        args.len()
                    );
                }

                let inst_constr_ty = self.inst_ty(&constr_ty);
                let (param_tys, ret_ty) = match &inst_constr_ty {
                    tast::Ty::TFunc { params, ret_ty } => (params.clone(), ret_ty.as_ref().clone()),
                    ty => (Vec::new(), ty.clone()),
                };

                let mut args_tast = Vec::new();
                for (arg_ast, expected_ty) in args.iter().zip(param_tys.iter()) {
                    let arg_tast =
                        self.check_pat(genv, local_env, diagnostics, *arg_ast, expected_ty);
                    args_tast.push(arg_tast);
                }

                self.push_constraint(Constraint::TypeEqual(ret_ty.clone(), ty.clone()));

                tast::Pat::PConstr {
                    constructor,
                    args: args_tast,
                    ty: ret_ty,
                }
            }
            fir::Pat::PStruct { name, fields } => {
                let name_display = name.display();
                let (type_name, type_env) = super::util::resolve_type_name(genv, &name_display);
                let struct_fields = {
                    let struct_def = type_env
                        .structs()
                        .get(&tast::TastIdent(type_name.clone()))
                        .unwrap_or_else(|| {
                            panic!("Struct {} not found when checking pattern", type_name)
                        });
                    let expected_len = struct_def.fields.len();
                    if expected_len != fields.len() {
                        panic!(
                            "Struct pattern {} expects {} fields, but got {}",
                            type_name,
                            expected_len,
                            fields.len()
                        );
                    }
                    struct_def.fields.clone()
                };

                let mut field_map: HashMap<String, fir::PatId> = HashMap::new();
                for (fname, pat_id) in fields.iter() {
                    if field_map.insert(fname.to_ident_name(), *pat_id).is_some() {
                        panic!(
                            "Struct pattern {} has duplicate field {}",
                            type_name,
                            fname.to_ident_name()
                        );
                    }
                }

                let (constructor, constr_ty) = type_env
                    .lookup_constructor(&tast::TastIdent(type_name.clone()))
                    .unwrap_or_else(|| {
                        panic!("Struct {} not found when checking constructor", type_name)
                    });

                let inst_constr_ty = self.inst_ty(&constr_ty);
                let (param_tys, ret_ty) = match &inst_constr_ty {
                    tast::Ty::TFunc { params, ret_ty } => (params.clone(), ret_ty.as_ref().clone()),
                    ty => (Vec::new(), ty.clone()),
                };

                if param_tys.len() != struct_fields.len() {
                    panic!(
                        "Constructor {} expects {} fields, but got {}",
                        type_name,
                        param_tys.len(),
                        struct_fields.len()
                    );
                }

                let mut args_tast = Vec::new();
                for (idx, (field_name, _)) in struct_fields.iter().enumerate() {
                    let pat_id = field_map.remove(&field_name.0).unwrap_or_else(|| {
                        panic!(
                            "Struct pattern {} missing field {}",
                            name_display, field_name.0
                        )
                    });
                    let expected_ty = param_tys.get(idx).unwrap_or_else(|| {
                        panic!("Missing instantiated type for field {}", field_name.0)
                    });
                    let pat_tast =
                        self.check_pat(genv, local_env, diagnostics, pat_id, expected_ty);
                    args_tast.push(pat_tast);
                }

                if !field_map.is_empty() {
                    let extra = field_map.keys().cloned().collect::<Vec<_>>().join(", ");
                    panic!(
                        "Struct pattern {} has unknown fields: {}",
                        name_display, extra
                    );
                }

                self.push_constraint(Constraint::TypeEqual(ret_ty.clone(), ty.clone()));

                tast::Pat::PConstr {
                    constructor,
                    args: args_tast,
                    ty: ret_ty,
                }
            }
            _ => unreachable!("Expected constructor pattern"),
        }
    }

    fn check_pat_tuple(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        pats: &[fir::PatId],
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

fn lookup_function_path(genv: &PackageTypeEnv, path: &fir::Path) -> Option<(String, tast::Ty)> {
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
    let path = fir::Path::from_idents(segments);
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
