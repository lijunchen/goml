use std::{collections::HashMap, num::IntErrorKind};

use ::ast::ast::Ident;
use ast::ast;
use parser::syntax::MySyntaxNodePtr;

use crate::{
    env::{Constraint, GlobalTypeEnv, LocalTypeEnv},
    tast::{self, Prim},
    typer::{Typer, util::binary_supports_builtin},
};

impl Typer {
    pub fn infer_expr(
        &mut self,
        genv: &GlobalTypeEnv,
        local_env: &mut LocalTypeEnv,
        e: &ast::Expr,
    ) -> tast::Expr {
        match e {
            ast::Expr::EVar { name, astptr } => self.infer_var_expr(genv, local_env, name, astptr),
            ast::Expr::EUnit => tast::Expr::EPrim {
                value: Prim::unit(),
                ty: tast::Ty::TUnit,
            },
            ast::Expr::EBool { value } => tast::Expr::EPrim {
                value: Prim::boolean(*value),
                ty: tast::Ty::TBool,
            },
            ast::Expr::EInt { value } => {
                let ty = tast::Ty::TInt32;
                let prim = self
                    .parse_integer_literal_with_ty(value, &ty)
                    .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                tast::Expr::EPrim { value: prim, ty }
            }
            ast::Expr::EFloat { value } => {
                self.ensure_float_literal_fits(*value, &tast::Ty::TFloat64);
                let ty = tast::Ty::TFloat64;
                tast::Expr::EPrim {
                    value: Prim::from_float_literal(*value, &ty),
                    ty,
                }
            }
            ast::Expr::EString { value } => tast::Expr::EPrim {
                value: Prim::string(value.clone()),
                ty: tast::Ty::TString,
            },
            ast::Expr::EConstr { constructor, args } => {
                self.infer_constructor_expr(genv, local_env, constructor, args)
            }
            ast::Expr::EStructLiteral { name, fields } => {
                self.infer_struct_literal_expr(genv, local_env, name, fields)
            }
            ast::Expr::ETuple { items } => self.infer_tuple_expr(genv, local_env, items),
            ast::Expr::EArray { items } => self.infer_array_expr(genv, local_env, items),
            ast::Expr::EClosure { params, body } => {
                self.infer_closure_expr(genv, local_env, params, body)
            }
            ast::Expr::ELet {
                pat,
                annotation,
                value,
                body,
            } => self.infer_let_expr(genv, local_env, pat, annotation, value, body),
            ast::Expr::EMatch { expr, arms, astptr } => {
                self.infer_match_expr(genv, local_env, expr, arms, astptr)
            }
            ast::Expr::EIf {
                cond,
                then_branch,
                else_branch,
            } => self.infer_if_expr(genv, local_env, cond, then_branch, else_branch),
            ast::Expr::EWhile { cond, body } => self.infer_while_expr(genv, local_env, cond, body),
            ast::Expr::ECall { func, args } => self.infer_call_expr(genv, local_env, func, args),
            ast::Expr::EUnary { op, expr } => self.infer_unary_expr(genv, local_env, *op, expr),
            ast::Expr::EBinary { op, lhs, rhs } => {
                self.infer_binary_expr(genv, local_env, *op, lhs, rhs)
            }
            ast::Expr::EProj { tuple, index } => {
                self.infer_proj_expr(genv, local_env, tuple, *index)
            }
            ast::Expr::EField {
                expr,
                field,
                astptr,
            } => self.infer_field_expr(genv, local_env, expr, field, astptr),
            ast::Expr::ETypeMember {
                type_name,
                member,
                astptr,
            } => self.infer_type_member_expr(genv, type_name, member, astptr),
        }
    }

    pub fn check_expr(
        &mut self,
        genv: &GlobalTypeEnv,
        local_env: &mut LocalTypeEnv,
        e: &ast::Expr,
        expected: &tast::Ty,
    ) -> tast::Expr {
        let expr_tast = match e {
            ast::Expr::EInt { value } => {
                if let Some(target_ty) = integer_literal_target(expected) {
                    let prim = self
                        .parse_integer_literal_with_ty(value, &target_ty)
                        .unwrap_or_else(|| Prim::zero_for_int_ty(&target_ty));
                    tast::Expr::EPrim {
                        value: prim,
                        ty: target_ty,
                    }
                } else {
                    let ty = tast::Ty::TInt32;
                    let prim = self
                        .parse_integer_literal_with_ty(value, &ty)
                        .unwrap_or_else(|| Prim::zero_for_int_ty(&ty));
                    tast::Expr::EPrim { value: prim, ty }
                }
            }
            ast::Expr::EFloat { value } => {
                if let Some(target_ty) = float_literal_target(expected) {
                    self.ensure_float_literal_fits(*value, &target_ty);
                    tast::Expr::EPrim {
                        value: Prim::from_float_literal(*value, &target_ty),
                        ty: target_ty,
                    }
                } else {
                    self.infer_expr(genv, local_env, e)
                }
            }
            ast::Expr::EUnary {
                op: ast::UnaryOp::Neg,
                expr: inner,
            } if is_numeric_ty(expected) => {
                let operand = self.check_expr(genv, local_env, inner, expected);
                tast::Expr::EUnary {
                    op: ast::UnaryOp::Neg,
                    expr: Box::new(operand),
                    ty: expected.clone(),
                    resolution: tast::UnaryResolution::Builtin,
                }
            }
            ast::Expr::EBinary { op, lhs, rhs }
                if is_numeric_ty(expected)
                    && matches!(
                        op,
                        ast::BinaryOp::Add
                            | ast::BinaryOp::Sub
                            | ast::BinaryOp::Mul
                            | ast::BinaryOp::Div
                    ) =>
            {
                let lhs_tast = self.check_expr(genv, local_env, lhs, expected);
                let rhs_tast = self.check_expr(genv, local_env, rhs, expected);
                tast::Expr::EBinary {
                    op: *op,
                    lhs: Box::new(lhs_tast),
                    rhs: Box::new(rhs_tast),
                    ty: expected.clone(),
                    resolution: tast::BinaryResolution::Builtin,
                }
            }
            ast::Expr::EClosure { params, body } => {
                self.check_closure_expr(genv, local_env, params, body, expected)
            }
            ast::Expr::ELet {
                pat,
                annotation,
                value,
                body,
            } => self.check_let_expr(genv, local_env, pat, annotation, value, body, expected),
            ast::Expr::ETuple { items } if matches!(expected, tast::Ty::TTuple { typs } if typs.len() == items.len()) =>
            {
                let expected_elem_tys = match expected {
                    tast::Ty::TTuple { typs } => typs.clone(),
                    _ => unreachable!("Tuple guard ensures tuple type"),
                };
                let mut checked_items = Vec::with_capacity(items.len());
                let mut elem_tys = Vec::with_capacity(items.len());
                for (item_expr, expected_ty) in items.iter().zip(expected_elem_tys.iter()) {
                    let item_tast = self.check_expr(genv, local_env, item_expr, expected_ty);
                    elem_tys.push(item_tast.get_ty());
                    checked_items.push(item_tast);
                }
                tast::Expr::ETuple {
                    items: checked_items,
                    ty: tast::Ty::TTuple { typs: elem_tys },
                }
            }
            _ => self.infer_expr(genv, local_env, e),
        };

        self.push_constraint(Constraint::TypeEqual(expr_tast.get_ty(), expected.clone()));
        expr_tast
    }

    fn infer_var_expr(
        &mut self,
        genv: &GlobalTypeEnv,
        local_env: &mut LocalTypeEnv,
        name: &Ident,
        astptr: &MySyntaxNodePtr,
    ) -> tast::Expr {
        if let Some(ty) = local_env.lookup_var(name) {
            tast::Expr::EVar {
                name: name.0.clone(),
                ty: ty.clone(),
                astptr: Some(astptr.clone()),
            }
        } else if let Some(func_ty) = genv.get_type_of_function(&name.0) {
            let inst_ty = self.inst_ty(&func_ty);
            tast::Expr::EVar {
                name: name.0.clone(),
                ty: inst_ty,
                astptr: Some(astptr.clone()),
            }
        } else {
            panic!("Variable {} not found in environment", name.0);
        }
    }

    fn infer_type_member_expr(
        &mut self,
        genv: &GlobalTypeEnv,
        type_name: &Ident,
        member: &Ident,
        astptr: &MySyntaxNodePtr,
    ) -> tast::Expr {
        let receiver_ty = tast::Ty::TCon {
            name: type_name.0.clone(),
        };
        if let Some((mangled_name, method_ty)) = genv.lookup_inherent_method(&receiver_ty, member) {
            let inst_ty = self.inst_ty(&method_ty);
            tast::Expr::EVar {
                name: mangled_name,
                ty: inst_ty,
                astptr: Some(astptr.clone()),
            }
        } else {
            panic!("Method {} not found for type {}", member.0, type_name.0);
        }
    }

    fn infer_constructor_expr(
        &mut self,
        genv: &GlobalTypeEnv,
        local_env: &mut LocalTypeEnv,
        constructor_path: &ast::Path,
        args: &[ast::Expr],
    ) -> tast::Expr {
        let variant_ident = constructor_path
            .last_ident()
            .unwrap_or_else(|| panic!("Constructor path missing final segment"));
        let enum_name = constructor_path.parent_ident();
        let (constructor, constr_ty) = genv
            .lookup_constructor_with_namespace(enum_name, variant_ident)
            .unwrap_or_else(|| {
                panic!(
                    "Constructor {} not found in environment",
                    constructor_path.display()
                )
            });

        let expected_arity = match &constructor {
            tast::Constructor::Enum(enum_constructor) => genv
                .enums
                .get(&enum_constructor.type_name)
                .map(|def| def.variants[enum_constructor.index].1.len())
                .unwrap_or_else(|| {
                    panic!(
                        "Enum {} not found when checking constructor {}",
                        enum_constructor.type_name.0,
                        constructor.name().0
                    )
                }),
            tast::Constructor::Struct(struct_constructor) => genv
                .structs
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
                args_tast.push(self.infer_expr(genv, local_env, arg));
            }
        } else {
            for (arg, expected_ty) in args.iter().zip(param_tys.iter()) {
                args_tast.push(self.check_expr(genv, local_env, arg, expected_ty));
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
        name: &ast::Ident,
        fields: &[(ast::Ident, ast::Expr)],
    ) -> tast::Expr {
        let (constructor, constr_ty) = genv
            .lookup_constructor(name)
            .unwrap_or_else(|| panic!("Constructor {} not found in environment", name.0));

        let struct_fields = match &constructor {
            tast::Constructor::Struct(struct_constructor) => {
                let type_name = &struct_constructor.type_name;
                let struct_def = genv.structs.get(type_name).unwrap_or_else(|| {
                    panic!(
                        "Struct {} not found when checking literal {}",
                        type_name.0, name.0
                    )
                });
                struct_def.fields.clone()
            }
            tast::Constructor::Enum { .. } => {
                panic!(
                    "Constructor {} refers to an enum, but a struct literal was used",
                    name.0
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
                name.0,
                param_tys.len(),
                struct_fields.len()
            );
        }

        let mut field_positions: HashMap<Ident, usize> = HashMap::new();
        for (idx, (fname, _)) in struct_fields.iter().enumerate() {
            field_positions.insert(fname.clone(), idx);
        }

        let mut ordered_args: Vec<Option<tast::Expr>> = vec![None; struct_fields.len()];
        for (field_name, expr) in fields.iter() {
            let idx = field_positions.get(field_name).unwrap_or_else(|| {
                panic!(
                    "Unknown field {} on struct literal {}",
                    field_name.0, name.0
                )
            });
            if ordered_args[*idx].is_some() {
                panic!(
                    "Duplicate field {} in struct literal {}",
                    field_name.0, name.0
                );
            }
            let field_expr = if let Some(expected_ty) = param_tys.get(*idx) {
                self.check_expr(genv, local_env, expr, expected_ty)
            } else {
                self.infer_expr(genv, local_env, expr)
            };
            ordered_args[*idx] = Some(field_expr);
        }

        for (idx, slot) in ordered_args.iter().enumerate() {
            if slot.is_none() {
                let missing = &struct_fields[idx].0;
                panic!("Missing field {} in struct literal {}", missing.0, name.0);
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
        items: &[ast::Expr],
    ) -> tast::Expr {
        let mut typs = Vec::new();
        let mut items_tast = Vec::new();
        for item in items.iter() {
            let item_tast = self.infer_expr(genv, local_env, item);
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
        items: &[ast::Expr],
    ) -> tast::Expr {
        let len = items.len();
        let elem_ty = self.fresh_ty_var();
        let mut items_tast = Vec::with_capacity(len);
        for item in items.iter() {
            let item_tast = self.infer_expr(genv, local_env, item);
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
        params: &[ast::ClosureParam],
        body: &ast::Expr,
    ) -> tast::Expr {
        local_env.begin_closure();
        let mut params_tast = Vec::new();
        let mut param_tys = Vec::new();
        let current_tparams_env = local_env.current_tparams_env();

        for param in params.iter() {
            let param_ty = match &param.ty {
                Some(ty) => tast::Ty::from_ast(ty, &current_tparams_env),
                None => self.fresh_ty_var(),
            };
            local_env.insert_var(&param.name, param_ty.clone());
            param_tys.push(param_ty.clone());
            params_tast.push(tast::ClosureParam {
                name: param.name.0.clone(),
                ty: param_ty,
                astptr: Some(param.astptr),
            });
        }

        let body_tast = self.infer_expr(genv, local_env, body);
        let body_ty = body_tast.get_ty();
        let captures = local_env.end_closure();

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
        params: &[ast::ClosureParam],
        body: &ast::Expr,
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
                    let annotated_ty = param
                        .ty
                        .as_ref()
                        .map(|ty| tast::Ty::from_ast(ty, &current_tparams_env));

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

                    local_env.insert_var(&param.name, param_ty.clone());
                    param_tys.push(param_ty.clone());
                    params_tast.push(tast::ClosureParam {
                        name: param.name.0.clone(),
                        ty: param_ty,
                        astptr: Some(param.astptr),
                    });
                }

                let body_tast = self.check_expr(genv, local_env, body, expected_ret.as_ref());
                let body_ty = body_tast.get_ty();
                let captures = local_env.end_closure();

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
            _ => self.infer_closure_expr(genv, local_env, params, body),
        }
    }

    fn infer_let_expr(
        &mut self,
        genv: &GlobalTypeEnv,
        local_env: &mut LocalTypeEnv,
        pat: &ast::Pat,
        annotation: &Option<ast::Ty>,
        value: &ast::Expr,
        body: &ast::Expr,
    ) -> tast::Expr {
        let current_tparams_env = local_env.current_tparams_env();
        let annotated_ty = annotation
            .as_ref()
            .map(|ty| tast::Ty::from_ast(ty, &current_tparams_env));

        let (value_tast, value_ty) = if let Some(ann_ty) = &annotated_ty {
            (
                self.check_expr(genv, local_env, value, ann_ty),
                ann_ty.clone(),
            )
        } else {
            let tast = self.infer_expr(genv, local_env, value);
            let ty = tast.get_ty();
            (tast, ty)
        };

        local_env.push_scope();
        let pat_tast = self.check_pat(genv, local_env, pat, &value_ty);
        let body_tast = self.infer_expr(genv, local_env, body);
        local_env.pop_scope();
        let body_ty = body_tast.get_ty();
        tast::Expr::ELet {
            pat: pat_tast,
            value: Box::new(value_tast),
            body: Box::new(body_tast),
            ty: body_ty.clone(),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn check_let_expr(
        &mut self,
        genv: &GlobalTypeEnv,
        local_env: &mut LocalTypeEnv,
        pat: &ast::Pat,
        annotation: &Option<ast::Ty>,
        value: &ast::Expr,
        body: &ast::Expr,
        expected: &tast::Ty,
    ) -> tast::Expr {
        let current_tparams_env = local_env.current_tparams_env();
        let annotated_ty = annotation
            .as_ref()
            .map(|ty| tast::Ty::from_ast(ty, &current_tparams_env));

        let (value_tast, value_ty) = if let Some(ann_ty) = &annotated_ty {
            (
                self.check_expr(genv, local_env, value, ann_ty),
                ann_ty.clone(),
            )
        } else {
            let tast = self.infer_expr(genv, local_env, value);
            let ty = tast.get_ty();
            (tast, ty)
        };

        local_env.push_scope();
        let pat_tast = self.check_pat(genv, local_env, pat, &value_ty);
        let body_tast = self.check_expr(genv, local_env, body, expected);
        local_env.pop_scope();
        let body_ty = body_tast.get_ty();

        tast::Expr::ELet {
            pat: pat_tast,
            value: Box::new(value_tast),
            body: Box::new(body_tast),
            ty: body_ty,
        }
    }

    fn infer_match_expr(
        &mut self,
        genv: &GlobalTypeEnv,
        local_env: &mut LocalTypeEnv,
        expr: &ast::Expr,
        arms: &[ast::Arm],
        astptr: &MySyntaxNodePtr,
    ) -> tast::Expr {
        let expr_tast = self.infer_expr(genv, local_env, expr);
        let expr_ty = expr_tast.get_ty();

        let mut arms_tast = Vec::new();
        let arm_ty = self.fresh_ty_var();
        for arm in arms.iter() {
            local_env.push_scope();
            let arm_tast = self.check_pat(genv, local_env, &arm.pat, &expr_ty);
            let arm_body_tast = self.infer_expr(genv, local_env, &arm.body);
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
            astptr: Some(astptr.clone()),
        }
    }

    fn infer_if_expr(
        &mut self,
        genv: &GlobalTypeEnv,
        local_env: &mut LocalTypeEnv,
        cond: &ast::Expr,
        then_branch: &ast::Expr,
        else_branch: &ast::Expr,
    ) -> tast::Expr {
        let cond_tast = self.infer_expr(genv, local_env, cond);
        self.push_constraint(Constraint::TypeEqual(cond_tast.get_ty(), tast::Ty::TBool));

        let then_tast = self.infer_expr(genv, local_env, then_branch);
        let else_tast = self.infer_expr(genv, local_env, else_branch);
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
        cond: &ast::Expr,
        body: &ast::Expr,
    ) -> tast::Expr {
        let cond_tast = self.infer_expr(genv, local_env, cond);
        self.push_constraint(Constraint::TypeEqual(cond_tast.get_ty(), tast::Ty::TBool));

        let body_tast = self.infer_expr(genv, local_env, body);
        self.push_constraint(Constraint::TypeEqual(body_tast.get_ty(), tast::Ty::TUnit));

        tast::Expr::EWhile {
            cond: Box::new(cond_tast),
            body: Box::new(body_tast),
            ty: tast::Ty::TUnit,
        }
    }

    fn infer_call_expr(
        &mut self,
        genv: &GlobalTypeEnv,
        local_env: &mut LocalTypeEnv,
        func: &ast::Expr,
        args: &[ast::Expr],
    ) -> tast::Expr {
        match func {
            ast::Expr::EVar { name, astptr } => {
                let mut args_tast = Vec::new();
                let mut arg_types = Vec::new();
                for arg in args.iter() {
                    let arg_tast = self.infer_expr(genv, local_env, arg);
                    arg_types.push(arg_tast.get_ty());
                    args_tast.push(arg_tast);
                }
                if let Some(func_ty) = genv.get_type_of_function(&name.0) {
                    let inst_ty = self.inst_ty(&func_ty);
                    if let tast::Ty::TFunc { params, .. } = &inst_ty
                        && params.len() == args.len()
                        && !params.is_empty()
                    {
                        args_tast.clear();
                        arg_types.clear();
                        for (arg, expected_ty) in args.iter().zip(params.iter()) {
                            let arg_tast = self.check_expr(genv, local_env, arg, expected_ty);
                            arg_types.push(arg_tast.get_ty());
                            args_tast.push(arg_tast);
                        }
                    }

                    let ret_ty = if name.0 == "ref" && args_tast.len() == 1 {
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
                            name: name.0.clone(),
                            ty: inst_ty,
                            astptr: Some(astptr.clone()),
                        }),
                        args: args_tast,
                        ty: ret_ty,
                    }
                } else if let Some(trait_name) =
                    genv.overloaded_funcs_to_trait_name.get(&name.0).cloned()
                {
                    let ret_ty = self.fresh_ty_var();
                    let call_site_func_ty = tast::Ty::TFunc {
                        params: arg_types,
                        ret_ty: Box::new(ret_ty.clone()),
                    };
                    self.push_constraint(Constraint::Overloaded {
                        op: name.clone(),
                        trait_name,
                        call_site_type: call_site_func_ty.clone(),
                    });

                    tast::Expr::ECall {
                        func: Box::new(tast::Expr::EVar {
                            name: name.0.clone(),
                            ty: call_site_func_ty.clone(),
                            astptr: Some(astptr.clone()),
                        }),
                        args: args_tast,
                        ty: ret_ty,
                    }
                } else if let Some(var_ty) = local_env.lookup_var(name) {
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
                            name: name.0.clone(),
                            ty: var_ty.clone(),
                            astptr: Some(astptr.clone()),
                        }),
                        args: args_tast,
                        ty: ret_ty,
                    }
                } else {
                    panic!("Variable {} not found in environment", name.0);
                }
            }

            ast::Expr::EField {
                expr: receiver_expr,
                field,
                ..
            } => {
                if let Some((receiver_tast, receiver_ty, mangled_name, method_ty)) = {
                    let receiver_tast = self.infer_expr(genv, local_env, receiver_expr);
                    let receiver_ty = receiver_tast.get_ty();
                    genv.lookup_inherent_method(&receiver_ty, field)
                        .map(|(name, ty)| (receiver_tast, receiver_ty, name, ty))
                } {
                    let mut args_tast = Vec::with_capacity(args.len() + 1);
                    let mut arg_types = Vec::with_capacity(args.len() + 1);
                    arg_types.push(receiver_ty.clone());
                    args_tast.push(receiver_tast);
                    for arg in args.iter() {
                        let arg_tast = self.infer_expr(genv, local_env, arg);
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
                        func: Box::new(tast::Expr::EVar {
                            name: mangled_name,
                            ty: inst_method_ty,
                            astptr: None,
                        }),
                        args: args_tast,
                        ty: ret_ty,
                    }
                } else {
                    panic!("Method {} not found for type {:?}", field.0, receiver_expr);
                }
            }
            ast::Expr::ETypeMember {
                type_name,
                member,
                astptr,
            } => {
                let receiver_ty = tast::Ty::TCon {
                    name: type_name.0.clone(),
                };
                if let Some((mangled_name, method_ty)) =
                    genv.lookup_inherent_method(&receiver_ty, member)
                {
                    let inst_method_ty = self.inst_ty(&method_ty);
                    if let tast::Ty::TFunc { params, ret_ty } = inst_method_ty.clone() {
                        if params.len() != args.len() {
                            panic!(
                                "Method {} expects {} arguments but got {}",
                                member.0,
                                params.len(),
                                args.len()
                            );
                        }

                        let mut args_tast = Vec::with_capacity(args.len());
                        for (arg, expected_ty) in args.iter().zip(params.iter()) {
                            let arg_tast = self.check_expr(genv, local_env, arg, expected_ty);
                            args_tast.push(arg_tast);
                        }

                        tast::Expr::ECall {
                            func: Box::new(tast::Expr::EVar {
                                name: mangled_name,
                                ty: inst_method_ty,
                                astptr: Some(astptr.clone()),
                            }),
                            args: args_tast,
                            ty: (*ret_ty).clone(),
                        }
                    } else {
                        panic!(
                            "Type member {}::{} is not callable",
                            type_name.0, member.0
                        );
                    }
                } else {
                    panic!(
                        "Method {} not found for type {}",
                        member.0, type_name.0
                    );
                }
            }
            _ => {
                let mut args_tast = Vec::new();
                let mut arg_types = Vec::new();
                for arg in args.iter() {
                    let arg_tast = self.infer_expr(genv, local_env, arg);
                    arg_types.push(arg_tast.get_ty());
                    args_tast.push(arg_tast);
                }
                let ret_ty = self.fresh_ty_var();
                let call_site_func_ty = tast::Ty::TFunc {
                    params: arg_types,
                    ret_ty: Box::new(ret_ty.clone()),
                };
                let func_tast = self.infer_expr(genv, local_env, func);
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
        op: ast::UnaryOp,
        expr: &ast::Expr,
    ) -> tast::Expr {
        let expr_tast = self.infer_expr(genv, local_env, expr);
        let expr_ty = expr_tast.get_ty();
        let method_name = op.method_name();
        let builtin_expr = match op {
            ast::UnaryOp::Not => {
                self.push_constraint(Constraint::TypeEqual(expr_ty.clone(), tast::Ty::TBool));
                Some(tast::Expr::EUnary {
                    op,
                    expr: Box::new(expr_tast.clone()),
                    ty: tast::Ty::TBool,
                    resolution: tast::UnaryResolution::Builtin,
                })
            }
            ast::UnaryOp::Neg => {
                let target_ty = if is_numeric_ty(&expr_ty) {
                    expr_ty.clone()
                } else {
                    tast::Ty::TInt32
                };
                self.push_constraint(Constraint::TypeEqual(expr_ty.clone(), target_ty.clone()));
                Some(tast::Expr::EUnary {
                    op,
                    expr: Box::new(expr_tast.clone()),
                    ty: target_ty,
                    resolution: tast::UnaryResolution::Builtin,
                })
            }
        };

        if let Some(expr) = builtin_expr {
            return expr;
        }

        if let Some(trait_name) = genv
            .overloaded_funcs_to_trait_name
            .get(method_name)
            .cloned()
        {
            let ret_ty = self.fresh_ty_var();
            let call_site_type = tast::Ty::TFunc {
                params: vec![expr_ty.clone()],
                ret_ty: Box::new(ret_ty.clone()),
            };
            self.push_constraint(Constraint::Overloaded {
                op: ast::Ident(method_name.to_string()),
                trait_name: trait_name.clone(),
                call_site_type,
            });
            tast::Expr::EUnary {
                op,
                expr: Box::new(expr_tast),
                ty: ret_ty,
                resolution: tast::UnaryResolution::Overloaded { trait_name },
            }
        } else {
            panic!("Unsupported unary operator {:?}", op);
        }
    }

    fn infer_binary_expr(
        &mut self,
        genv: &GlobalTypeEnv,
        local_env: &mut LocalTypeEnv,
        op: ast::BinaryOp,
        lhs: &ast::Expr,
        rhs: &ast::Expr,
    ) -> tast::Expr {
        let lhs_tast = self.infer_expr(genv, local_env, lhs);
        let rhs_tast = self.infer_expr(genv, local_env, rhs);
        let lhs_ty = lhs_tast.get_ty();
        let rhs_ty = rhs_tast.get_ty();
        let method_name = op.method_name();

        if let Some(trait_name) = genv
            .overloaded_funcs_to_trait_name
            .get(method_name)
            .cloned()
            && !binary_supports_builtin(op, &lhs_ty, &rhs_ty)
        {
            let ret_ty = self.fresh_ty_var();
            let call_site_type = tast::Ty::TFunc {
                params: vec![lhs_ty.clone(), rhs_ty.clone()],
                ret_ty: Box::new(ret_ty.clone()),
            };
            self.push_constraint(Constraint::Overloaded {
                op: ast::Ident(method_name.to_string()),
                trait_name: trait_name.clone(),
                call_site_type,
            });
            return tast::Expr::EBinary {
                op,
                lhs: Box::new(lhs_tast),
                rhs: Box::new(rhs_tast),
                ty: ret_ty,
                resolution: tast::BinaryResolution::Overloaded { trait_name },
            };
        }

        let ret_ty = match op {
            ast::BinaryOp::And | ast::BinaryOp::Or => tast::Ty::TBool,
            _ => self.fresh_ty_var(),
        };

        match op {
            ast::BinaryOp::Add => {
                self.push_constraint(Constraint::TypeEqual(lhs_ty.clone(), ret_ty.clone()));
                self.push_constraint(Constraint::TypeEqual(rhs_ty.clone(), ret_ty.clone()));
            }
            ast::BinaryOp::Sub | ast::BinaryOp::Mul | ast::BinaryOp::Div => {
                self.push_constraint(Constraint::TypeEqual(lhs_ty.clone(), ret_ty.clone()));
                self.push_constraint(Constraint::TypeEqual(rhs_ty.clone(), ret_ty.clone()));
            }
            ast::BinaryOp::And | ast::BinaryOp::Or => {
                self.push_constraint(Constraint::TypeEqual(lhs_ty.clone(), tast::Ty::TBool));
                self.push_constraint(Constraint::TypeEqual(rhs_ty.clone(), tast::Ty::TBool));
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
        tuple: &ast::Expr,
        index: usize,
    ) -> tast::Expr {
        let tuple_tast = self.infer_expr(genv, local_env, tuple);
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
                self.report_error(format!(
                    "Cannot project field {} on non-tuple type {:?}",
                    index, tuple_ty
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
        expr: &ast::Expr,
        field: &ast::Ident,
        astptr: &MySyntaxNodePtr,
    ) -> tast::Expr {
        let base_tast = self.infer_expr(genv, local_env, expr);
        let base_ty = base_tast.get_ty();
        let result_ty = self.fresh_ty_var();
        self.push_constraint(Constraint::StructFieldAccess {
            expr_ty: base_ty.clone(),
            field: field.clone(),
            result_ty: result_ty.clone(),
        });

        tast::Expr::EField {
            expr: Box::new(base_tast),
            field_name: field.0.clone(),
            ty: result_ty,
            astptr: Some(astptr.clone()),
        }
    }

    fn check_pat(
        &mut self,
        genv: &GlobalTypeEnv,
        local_env: &mut LocalTypeEnv,
        pat: &ast::Pat,
        ty: &tast::Ty,
    ) -> tast::Pat {
        match pat {
            ast::Pat::PVar { name, astptr } => self.check_pat_var(local_env, name, astptr, ty),
            ast::Pat::PUnit => self.check_pat_unit(),
            ast::Pat::PBool { value } => self.check_pat_bool(*value),
            ast::Pat::PInt { value } => self.check_pat_int(value, ty),
            ast::Pat::PString { value } => self.check_pat_string(value, ty),
            ast::Pat::PConstr { .. } => self.check_pat_constructor(genv, local_env, pat, ty),
            ast::Pat::PStruct { .. } => self.check_pat_constructor(genv, local_env, pat, ty),
            ast::Pat::PTuple { pats } => self.check_pat_tuple(genv, local_env, pats, ty),
            ast::Pat::PWild => self.check_pat_wild(ty),
        }
    }

    fn check_pat_var(
        &mut self,
        local_env: &mut LocalTypeEnv,
        name: &Ident,
        astptr: &MySyntaxNodePtr,
        ty: &tast::Ty,
    ) -> tast::Pat {
        local_env.insert_var(name, ty.clone());
        tast::Pat::PVar {
            name: name.0.clone(),
            ty: ty.clone(),
            astptr: Some(astptr.clone()),
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

    fn check_pat_int(&mut self, value: &str, ty: &tast::Ty) -> tast::Pat {
        let target_ty = integer_literal_target(ty).unwrap_or(tast::Ty::TInt32);
        let prim = self
            .parse_integer_literal_with_ty(value, &target_ty)
            .unwrap_or_else(|| Prim::zero_for_int_ty(&target_ty));
        self.push_constraint(Constraint::TypeEqual(target_ty.clone(), ty.clone()));
        tast::Pat::PPrim {
            value: prim,
            ty: ty.clone(),
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
        pat: &ast::Pat,
        ty: &tast::Ty,
    ) -> tast::Pat {
        match pat {
            ast::Pat::PConstr {
                constructor: constructor_path,
                args,
            } => {
                let variant_ident = constructor_path
                    .last_ident()
                    .unwrap_or_else(|| panic!("Constructor path missing final segment"));
                let enum_name = constructor_path.parent_ident();
                let (constructor, constr_ty) = genv
                    .lookup_constructor_with_namespace(enum_name, variant_ident)
                    .unwrap_or_else(|| {
                        panic!(
                            "Constructor {} not found in environment",
                            constructor_path.display()
                        )
                    });

                let expected_arity = match &constructor {
                    tast::Constructor::Enum(enum_constructor) => genv
                        .enums
                        .get(&enum_constructor.type_name)
                        .map(|def| def.variants[enum_constructor.index].1.len())
                        .unwrap_or_else(|| {
                            panic!(
                                "Enum {} not found when checking constructor {}",
                                enum_constructor.type_name.0,
                                constructor.name().0
                            )
                        }),
                    tast::Constructor::Struct(_) => {
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
                    let arg_tast = self.check_pat(genv, local_env, arg_ast, expected_ty);
                    args_tast.push(arg_tast);
                }

                self.push_constraint(Constraint::TypeEqual(ret_ty.clone(), ty.clone()));

                tast::Pat::PConstr {
                    constructor,
                    args: args_tast,
                    ty: ret_ty,
                }
            }
            ast::Pat::PStruct { name, fields } => {
                let struct_fields = {
                    let struct_def = genv.structs.get(name).unwrap_or_else(|| {
                        panic!("Struct {} not found when checking pattern", name.0)
                    });
                    let expected_len = struct_def.fields.len();
                    if expected_len != fields.len() {
                        panic!(
                            "Struct pattern {} expects {} fields, but got {}",
                            name.0,
                            expected_len,
                            fields.len()
                        );
                    }
                    struct_def.fields.clone()
                };

                let mut field_map: HashMap<String, &ast::Pat> = HashMap::new();
                for (fname, pat) in fields.iter() {
                    if field_map.insert(fname.0.clone(), pat).is_some() {
                        panic!("Struct pattern {} has duplicate field {}", name.0, fname.0);
                    }
                }

                let (constructor, constr_ty) = genv.lookup_constructor(name).unwrap_or_else(|| {
                    panic!("Struct {} not found when checking constructor", name.0)
                });

                let inst_constr_ty = self.inst_ty(&constr_ty);
                let (param_tys, ret_ty) = match &inst_constr_ty {
                    tast::Ty::TFunc { params, ret_ty } => (params.clone(), ret_ty.as_ref().clone()),
                    ty => (Vec::new(), ty.clone()),
                };

                if param_tys.len() != struct_fields.len() {
                    panic!(
                        "Constructor {} expects {} fields, but got {}",
                        name.0,
                        param_tys.len(),
                        struct_fields.len()
                    );
                }

                let mut args_tast = Vec::new();
                for (idx, (field_name, _)) in struct_fields.iter().enumerate() {
                    let pat_ast = field_map.remove(&field_name.0).unwrap_or_else(|| {
                        panic!("Struct pattern {} missing field {}", name.0, field_name.0)
                    });
                    let expected_ty = param_tys.get(idx).unwrap_or_else(|| {
                        panic!("Missing instantiated type for field {}", field_name.0)
                    });
                    let pat_tast = self.check_pat(genv, local_env, pat_ast, expected_ty);
                    args_tast.push(pat_tast);
                }

                if !field_map.is_empty() {
                    let extra = field_map.keys().cloned().collect::<Vec<_>>().join(", ");
                    panic!("Struct pattern {} has unknown fields: {}", name.0, extra);
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
        pats: &[ast::Pat],
        ty: &tast::Ty,
    ) -> tast::Pat {
        let expected_elem_tys: Vec<tast::Ty> = match ty {
            tast::Ty::TTuple { typs } if typs.len() == pats.len() => typs.clone(),
            _ => (0..pats.len()).map(|_| self.fresh_ty_var()).collect(),
        };

        let mut pats_tast = Vec::new();
        let mut pat_typs = Vec::new();
        for (pat, expected_ty) in pats.iter().zip(expected_elem_tys.iter()) {
            let pat_tast = self.check_pat(genv, local_env, pat, expected_ty);
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

fn float_literal_target(expected: &tast::Ty) -> Option<tast::Ty> {
    if is_float_ty(expected) {
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
    fn parse_integer_literal_with_ty(&mut self, literal: &str, ty: &tast::Ty) -> Option<Prim> {
        match ty {
            tast::Ty::TInt8 => self
                .parse_signed_integer::<i8>(literal, "int8")
                .map(|value| Prim::Int8 { value }),
            tast::Ty::TInt16 => self
                .parse_signed_integer::<i16>(literal, "int16")
                .map(|value| Prim::Int16 { value }),
            tast::Ty::TInt32 => self
                .parse_signed_integer::<i32>(literal, "int32")
                .map(|value| Prim::Int32 { value }),
            tast::Ty::TInt64 => self
                .parse_signed_integer::<i64>(literal, "int64")
                .map(|value| Prim::Int64 { value }),
            tast::Ty::TUint8 => self
                .parse_unsigned_integer::<u8>(literal, "uint8")
                .map(|value| Prim::UInt8 { value }),
            tast::Ty::TUint16 => self
                .parse_unsigned_integer::<u16>(literal, "uint16")
                .map(|value| Prim::UInt16 { value }),
            tast::Ty::TUint32 => self
                .parse_unsigned_integer::<u32>(literal, "uint32")
                .map(|value| Prim::UInt32 { value }),
            tast::Ty::TUint64 => self
                .parse_unsigned_integer::<u64>(literal, "uint64")
                .map(|value| Prim::UInt64 { value }),
            _ => None,
        }
    }

    fn parse_signed_integer<T>(&mut self, literal: &str, ty_name: &str) -> Option<T>
    where
        T: std::str::FromStr<Err = std::num::ParseIntError>,
    {
        match literal.parse::<T>() {
            Ok(value) => Some(value),
            Err(err) => {
                match err.kind() {
                    IntErrorKind::Empty | IntErrorKind::InvalidDigit => {
                        self.report_invalid_integer_literal(literal);
                    }
                    _ => {
                        self.report_error(format!(
                            "Integer literal {} does not fit in {}",
                            literal, ty_name
                        ));
                    }
                }
                None
            }
        }
    }

    fn parse_unsigned_integer<T>(&mut self, literal: &str, ty_name: &str) -> Option<T>
    where
        T: std::str::FromStr<Err = std::num::ParseIntError>,
    {
        if literal.starts_with('-') {
            self.report_error(format!(
                "Integer literal {} does not fit in {}",
                literal, ty_name
            ));
            return None;
        }

        match literal.parse::<T>() {
            Ok(value) => Some(value),
            Err(err) => {
                match err.kind() {
                    IntErrorKind::Empty | IntErrorKind::InvalidDigit => {
                        self.report_invalid_integer_literal(literal);
                    }
                    _ => {
                        self.report_error(format!(
                            "Integer literal {} does not fit in {}",
                            literal, ty_name
                        ));
                    }
                }
                None
            }
        }
    }

    fn report_invalid_integer_literal(&mut self, literal: &str) {
        self.report_error(format!("Invalid integer literal: {}", literal));
    }

    fn ensure_float_literal_fits(&mut self, value: f64, ty: &tast::Ty) {
        if !value.is_finite() {
            self.report_error("Float literal must be finite".to_string());
            return;
        }

        match ty {
            tast::Ty::TFloat32 => {
                if value < f32::MIN as f64 || value > f32::MAX as f64 {
                    self.report_error(format!("Float literal {} does not fit in float32", value));
                }
            }
            tast::Ty::TFloat64 => {
                // Any finite f64 literal fits.
            }
            _ => {}
        }
    }
}
