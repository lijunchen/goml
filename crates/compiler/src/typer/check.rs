use std::{collections::HashMap, num::IntErrorKind};

use parser::{Diagnostic, Diagnostics, syntax::MySyntaxNodePtr};

use crate::common::{self, Prim};
use crate::fir::{self};
use crate::typer::localenv::LocalTypeEnv;
use crate::{
    env::{Constraint, GlobalTypeEnv},
    tast::{self},
    typer::Typer,
};

impl Typer {
    pub fn infer_expr(
        &mut self,
        genv: &GlobalTypeEnv,
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
        genv: &GlobalTypeEnv,
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

        self.push_constraint(Constraint::TypeEqual(expr_tast.get_ty(), expected.clone()));
        expr_tast
    }

    fn infer_res_expr(
        &mut self,
        genv: &GlobalTypeEnv,
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
                if let Some(func_ty) = genv.get_type_of_function(hint) {
                    let inst_ty = self.inst_ty(&func_ty);
                    tast::Expr::EVar {
                        name: hint.to_string(),
                        ty: inst_ty,
                        astptr,
                    }
                } else {
                    panic!("Function {} not found in environment", hint);
                }
            }
            fir::NameRef::Builtin(_builtin_id) => {
                if let Some(func_ty) = genv.get_type_of_function(hint) {
                    let inst_ty = self.inst_ty(&func_ty);
                    tast::Expr::EVar {
                        name: hint.to_string(),
                        ty: inst_ty,
                        astptr,
                    }
                } else {
                    panic!("Builtin {} not found in environment", hint);
                }
            }
            fir::NameRef::Unresolved(path) => {
                self.infer_path_expr(genv, local_env, _diagnostics, path, astptr)
            }
        }
    }

    fn infer_path_expr(
        &mut self,
        genv: &GlobalTypeEnv,
        local_env: &mut LocalTypeEnv,
        _diagnostics: &mut Diagnostics,
        path: &fir::Path,
        astptr: Option<MySyntaxNodePtr>,
    ) -> tast::Expr {
        let _ = local_env;
        if path.len() == 1 {
            let name = path.last_ident().unwrap();
            if let Some(func_ty) = genv.get_type_of_function(name.as_str()) {
                let inst_ty = self.inst_ty(&func_ty);
                tast::Expr::EVar {
                    name: name.clone(),
                    ty: inst_ty,
                    astptr,
                }
            } else {
                panic!("Variable {} not found in environment", name);
            }
        } else {
            // Multi-segment path: Type::member
            let type_name = path.parent_ident().unwrap();
            let member = path.last_ident().unwrap();
            self.infer_type_member_expr(genv, type_name, member, astptr)
        }
    }

    fn infer_type_member_expr(
        &mut self,
        genv: &GlobalTypeEnv,
        type_name: &str,
        member: &str,
        astptr: Option<MySyntaxNodePtr>,
    ) -> tast::Expr {
        let type_ident = tast::TastIdent(type_name.to_string());
        let member_ident = tast::TastIdent(member.to_string());
        // First check if type_name is a trait
        if let Some(method_ty) = genv.lookup_trait_method(&type_ident, &member_ident) {
            let inst_ty = self.inst_ty(&method_ty);
            return tast::Expr::ETraitMethod {
                trait_name: type_ident.clone(),
                method_name: member_ident.clone(),
                ty: inst_ty,
                astptr,
            };
        }

        // Check if type_name is an enum or struct for inherent method lookup
        let receiver_ty = if genv.enums().contains_key(&type_ident) {
            tast::Ty::TEnum {
                name: type_name.to_string(),
            }
        } else if genv.structs().contains_key(&type_ident) {
            tast::Ty::TStruct {
                name: type_name.to_string(),
            }
        } else {
            panic!("Type or trait {} not found for member access", type_name);
        };
        if let Some(method_ty) = genv.lookup_inherent_method(&receiver_ty, &member_ident) {
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

    fn infer_constructor_expr(
        &mut self,
        genv: &GlobalTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        constructor_ref: &fir::ConstructorRef,
        args: &[fir::ExprId],
    ) -> tast::Expr {
        let constructor_path = match constructor_ref {
            fir::ConstructorRef::Resolved(ctor_id) => match ctor_id {
                fir::ConstructorId::EnumVariant {
                    enum_def,
                    variant_idx,
                } => {
                    if let fir::Def::EnumDef(enum_def_data) = self.fir_table.def(*enum_def) {
                        let enum_name = enum_def_data.name.to_ident_name();
                        let variant_name = enum_def_data.variants[*variant_idx as usize]
                            .0
                            .to_ident_name();
                        fir::Path::from_idents(vec![enum_name, variant_name])
                    } else {
                        panic!("Constructor points to non-enum DefId");
                    }
                }
            },
            fir::ConstructorRef::Unresolved(path) => path.clone(),
        };

        let variant_ident = constructor_path
            .last_ident()
            .unwrap_or_else(|| panic!("Constructor path missing final segment"));
        let enum_name = constructor_path.parent_ident();
        let variant_name = tast::TastIdent(variant_ident.clone());
        let enum_ident = enum_name.map(|name| tast::TastIdent(name.clone()));
        let (constructor, constr_ty) = genv
            .lookup_constructor_with_namespace(enum_ident.as_ref(), &variant_name)
            .unwrap_or_else(|| {
                panic!(
                    "Constructor {} not found in environment",
                    constructor_path.display()
                )
            });

        let expected_arity = match &constructor {
            common::Constructor::Enum(enum_constructor) => genv
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
            common::Constructor::Struct(struct_constructor) => genv
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
        genv: &GlobalTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        name: &fir::FirIdent,
        fields: &[(fir::FirIdent, fir::ExprId)],
    ) -> tast::Expr {
        let (constructor, constr_ty) = genv
            .lookup_constructor(&tast::TastIdent(name.to_ident_name()))
            .unwrap_or_else(|| {
                panic!(
                    "Constructor {} not found in environment",
                    name.to_ident_name()
                )
            });

        let struct_fields = match &constructor {
            common::Constructor::Struct(struct_constructor) => {
                let type_name = &struct_constructor.type_name;
                let struct_def = genv.structs().get(type_name).unwrap_or_else(|| {
                    panic!(
                        "Struct {} not found when checking literal {}",
                        type_name.0,
                        name.to_ident_name()
                    )
                });
                struct_def.fields.clone()
            }
            common::Constructor::Enum { .. } => {
                panic!(
                    "Constructor {} refers to an enum, but a struct literal was used",
                    name.to_ident_name()
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
                name.to_ident_name(),
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
                        name.to_ident_name()
                    )
                });
            if ordered_args[*idx].is_some() {
                panic!(
                    "Duplicate field {} in struct literal {}",
                    field_name.to_ident_name(),
                    name.to_ident_name()
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
                    missing.0,
                    name.to_ident_name()
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
        genv: &GlobalTypeEnv,
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
        genv: &GlobalTypeEnv,
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
        genv: &GlobalTypeEnv,
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
        genv: &GlobalTypeEnv,
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
        genv: &GlobalTypeEnv,
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
        genv: &GlobalTypeEnv,
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
        genv: &GlobalTypeEnv,
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
        genv: &GlobalTypeEnv,
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
        genv: &GlobalTypeEnv,
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
        genv: &GlobalTypeEnv,
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
        genv: &GlobalTypeEnv,
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
        genv: &GlobalTypeEnv,
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
        genv: &GlobalTypeEnv,
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
        genv: &GlobalTypeEnv,
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
        genv: &GlobalTypeEnv,
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
                if let Some(func_ty) = genv.get_type_of_function(name.as_str()) {
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
                if path.len() == 1 {
                    let name = path.last_ident().unwrap();

                    if let Some(func_ty) = genv.get_type_of_function(name) {
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
                                let arg_tast = self.check_expr(
                                    genv,
                                    local_env,
                                    diagnostics,
                                    *arg,
                                    expected_ty,
                                );
                                arg_types.push(arg_tast.get_ty());
                                args_tast.push(arg_tast);
                            }
                        }

                        let ret_ty = match &inst_ty {
                            tast::Ty::TFunc { ret_ty, .. } => (**ret_ty).clone(),
                            _ => inst_ty.clone(),
                        };

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
                        self.infer_constructor_expr(
                            genv,
                            local_env,
                            diagnostics,
                            &fir::ConstructorRef::Unresolved(path.clone()),
                            args,
                        )
                    }
                } else {
                    let type_name = path.parent_ident().unwrap();
                    let member = path.last_ident().unwrap();
                    let type_ident = tast::TastIdent(type_name.clone());
                    let member_ident = tast::TastIdent(member.clone());
                    if let Some(method_ty) = genv.lookup_trait_method(&type_ident, &member_ident) {
                        let inst_method_ty = self.inst_ty(&method_ty);

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

                        self.push_constraint(Constraint::Overloaded {
                            op: member_ident.clone(),
                            trait_name: type_ident.clone(),
                            call_site_type: call_site_func_ty.clone(),
                        });

                        return tast::Expr::ECall {
                            func: Box::new(tast::Expr::ETraitMethod {
                                trait_name: type_ident.clone(),
                                method_name: member_ident.clone(),
                                ty: inst_method_ty,
                                astptr: None,
                            }),
                            args: args_tast,
                            ty: ret_ty,
                        };
                    }

                    let receiver_ty = if genv.enums().contains_key(&type_ident) {
                        tast::Ty::TEnum {
                            name: type_name.clone(),
                        }
                    } else if genv.structs().contains_key(&type_ident) {
                        tast::Ty::TStruct {
                            name: type_name.clone(),
                        }
                    } else {
                        panic!("Type or trait {} not found for member access", type_name);
                    };
                    if let Some(method_ty) =
                        genv.lookup_inherent_method(&receiver_ty, &member_ident)
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
                                let arg_tast = self.check_expr(
                                    genv,
                                    local_env,
                                    diagnostics,
                                    *arg,
                                    expected_ty,
                                );
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
            }
            fir::Expr::EField {
                expr: receiver_expr,
                field,
            } => {
                if let Some((receiver_tast, receiver_ty, method_ty)) = {
                    let receiver_tast =
                        self.infer_expr(genv, local_env, diagnostics, receiver_expr);
                    let receiver_ty = receiver_tast.get_ty();
                    genv.lookup_inherent_method(
                        &receiver_ty,
                        &tast::TastIdent(field.to_ident_name()),
                    )
                    .map(|ty| (receiver_tast, receiver_ty, ty))
                } {
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
        genv: &GlobalTypeEnv,
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
        genv: &GlobalTypeEnv,
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
        genv: &GlobalTypeEnv,
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
        genv: &GlobalTypeEnv,
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
        genv: &GlobalTypeEnv,
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
        genv: &GlobalTypeEnv,
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
                    fir::ConstructorRef::Resolved(ctor_id) => match ctor_id {
                        fir::ConstructorId::EnumVariant {
                            enum_def,
                            variant_idx,
                        } => {
                            if let fir::Def::EnumDef(enum_def_data) = self.fir_table.def(*enum_def)
                            {
                                let enum_name = enum_def_data.name.to_ident_name();
                                let variant_name = enum_def_data.variants[*variant_idx as usize]
                                    .0
                                    .to_ident_name();
                                fir::Path::from_idents(vec![enum_name, variant_name])
                            } else {
                                panic!("Constructor points to non-enum DefId");
                            }
                        }
                    },
                    fir::ConstructorRef::Unresolved(path) => path.clone(),
                };

                let variant_ident = constructor_path
                    .last_ident()
                    .unwrap_or_else(|| panic!("Constructor path missing final segment"));
                let enum_name = constructor_path.parent_ident();
                let variant_name = tast::TastIdent(variant_ident.clone());
                let enum_ident = enum_name.map(|name| tast::TastIdent(name.clone()));
                let (constructor, constr_ty) = genv
                    .lookup_constructor_with_namespace(enum_ident.as_ref(), &variant_name)
                    .unwrap_or_else(|| {
                        panic!(
                            "Constructor {} not found in environment",
                            constructor_path.display()
                        )
                    });

                let expected_arity = match &constructor {
                    common::Constructor::Enum(enum_constructor) => genv
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
                let struct_fields = {
                    let struct_def = genv
                        .structs()
                        .get(&tast::TastIdent(name.to_ident_name()))
                        .unwrap_or_else(|| {
                            panic!(
                                "Struct {} not found when checking pattern",
                                name.to_ident_name()
                            )
                        });
                    let expected_len = struct_def.fields.len();
                    if expected_len != fields.len() {
                        panic!(
                            "Struct pattern {} expects {} fields, but got {}",
                            name.to_ident_name(),
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
                            name.to_ident_name(),
                            fname.to_ident_name()
                        );
                    }
                }

                let (constructor, constr_ty) = genv
                    .lookup_constructor(&tast::TastIdent(name.to_ident_name()))
                    .unwrap_or_else(|| {
                        panic!(
                            "Struct {} not found when checking constructor",
                            name.to_ident_name()
                        )
                    });

                let inst_constr_ty = self.inst_ty(&constr_ty);
                let (param_tys, ret_ty) = match &inst_constr_ty {
                    tast::Ty::TFunc { params, ret_ty } => (params.clone(), ret_ty.as_ref().clone()),
                    ty => (Vec::new(), ty.clone()),
                };

                if param_tys.len() != struct_fields.len() {
                    panic!(
                        "Constructor {} expects {} fields, but got {}",
                        name.to_ident_name(),
                        param_tys.len(),
                        struct_fields.len()
                    );
                }

                let mut args_tast = Vec::new();
                for (idx, (field_name, _)) in struct_fields.iter().enumerate() {
                    let pat_id = field_map.remove(&field_name.0).unwrap_or_else(|| {
                        panic!(
                            "Struct pattern {} missing field {}",
                            name.to_ident_name(),
                            field_name.0
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
                        name.to_ident_name(),
                        extra
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
        genv: &GlobalTypeEnv,
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
