use std::collections::HashMap;

use ::ast::ast::Lident;
use ast::ast;
use parser::syntax::MySyntaxNodePtr;

use crate::{
    env::{Constraint, Env},
    tast::{self},
    typer::{
        TypeInference,
        util::{ast_ty_to_tast_ty_with_tparams_env, binary_supports_builtin},
    },
};

impl TypeInference {
    pub fn infer_expr(
        &mut self,
        env: &mut Env,
        vars: &im::HashMap<Lident, tast::Ty>,
        e: &ast::Expr,
    ) -> tast::Expr {
        match e {
            ast::Expr::EVar { name, astptr } => self.infer_var_expr(env, vars, name, astptr),
            ast::Expr::EUnit => tast::Expr::EUnit {
                ty: tast::Ty::TUnit,
            },
            ast::Expr::EBool { value } => tast::Expr::EBool {
                value: *value,
                ty: tast::Ty::TBool,
            },
            ast::Expr::EInt { value } => tast::Expr::EInt {
                value: *value,
                ty: tast::Ty::TInt,
            },
            ast::Expr::EString { value } => tast::Expr::EString {
                value: value.clone(),
                ty: tast::Ty::TString,
            },
            ast::Expr::EConstr { vcon, args } => self.infer_constructor_expr(env, vars, vcon, args),
            ast::Expr::EStructLiteral { name, fields } => {
                self.infer_struct_literal_expr(env, vars, name, fields)
            }
            ast::Expr::ETuple { items } => self.infer_tuple_expr(env, vars, items),
            ast::Expr::EArray { items } => self.infer_array_expr(env, vars, items),
            ast::Expr::EClosure { params, body } => {
                self.infer_closure_expr(env, vars, params, body)
            }
            ast::Expr::ELet { pat, value, body } => {
                self.infer_let_expr(env, vars, pat, value, body)
            }
            ast::Expr::EMatch { expr, arms } => self.infer_match_expr(env, vars, expr, arms),
            ast::Expr::EIf {
                cond,
                then_branch,
                else_branch,
            } => self.infer_if_expr(env, vars, cond, then_branch, else_branch),
            ast::Expr::ECall { func, args } => self.infer_call_expr(env, vars, func, args),
            ast::Expr::EUnary { op, expr } => self.infer_unary_expr(env, vars, *op, expr),
            ast::Expr::EBinary { op, lhs, rhs } => self.infer_binary_expr(env, vars, *op, lhs, rhs),
            ast::Expr::EProj { tuple, index } => self.infer_proj_expr(env, vars, tuple, *index),
        }
    }

    pub fn check_expr(
        &mut self,
        env: &mut Env,
        vars: &im::HashMap<Lident, tast::Ty>,
        e: &ast::Expr,
        expected: &tast::Ty,
    ) -> tast::Expr {
        let expr_tast = match e {
            ast::Expr::EClosure { params, body } => {
                self.check_closure_expr(env, vars, params, body, expected)
            }
            ast::Expr::ELet { pat, value, body } => {
                self.check_let_expr(env, vars, pat, value, body, expected)
            }
            _ => self.infer_expr(env, vars, e),
        };

        env.constraints
            .push(Constraint::TypeEqual(expr_tast.get_ty(), expected.clone()));
        expr_tast
    }

    fn infer_var_expr(
        &mut self,
        env: &mut Env,
        vars: &im::HashMap<Lident, tast::Ty>,
        name: &Lident,
        astptr: &MySyntaxNodePtr,
    ) -> tast::Expr {
        if let Some(ty) = vars.get(name) {
            tast::Expr::EVar {
                name: name.0.clone(),
                ty: ty.clone(),
                astptr: Some(*astptr),
            }
        } else if let Some(func_ty) = env.get_type_of_function(&name.0) {
            let inst_ty = self.inst_ty(&func_ty);
            tast::Expr::EVar {
                name: name.0.clone(),
                ty: inst_ty,
                astptr: Some(*astptr),
            }
        } else {
            panic!("Variable {} not found in environment", name.0);
        }
    }

    fn infer_constructor_expr(
        &mut self,
        env: &mut Env,
        vars: &im::HashMap<Lident, tast::Ty>,
        vcon: &ast::Uident,
        args: &[ast::Expr],
    ) -> tast::Expr {
        let (constructor, constr_ty) = env
            .lookup_constructor(vcon)
            .unwrap_or_else(|| panic!("Constructor {} not found in environment", vcon.0));

        let expected_arity = match &constructor {
            tast::Constructor::Enum(enum_constructor) => env
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
            tast::Constructor::Struct(struct_constructor) => env
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
        let ret_ty = self.fresh_ty_var();

        let mut args_tast = Vec::new();
        for arg in args.iter() {
            let arg_tast = self.infer_expr(env, vars, arg);
            args_tast.push(arg_tast);
        }

        if !args_tast.is_empty() {
            let actual_ty = tast::Ty::TFunc {
                params: args_tast.iter().map(|arg| arg.get_ty()).collect(),
                ret_ty: Box::new(ret_ty.clone()),
            };
            env.constraints
                .push(Constraint::TypeEqual(inst_constr_ty, actual_ty));
        } else {
            env.constraints
                .push(Constraint::TypeEqual(inst_constr_ty, ret_ty.clone()));
        }

        tast::Expr::EConstr {
            constructor,
            args: args_tast,
            ty: ret_ty,
        }
    }

    fn infer_struct_literal_expr(
        &mut self,
        env: &mut Env,
        vars: &im::HashMap<Lident, tast::Ty>,
        name: &ast::Uident,
        fields: &[(ast::Lident, ast::Expr)],
    ) -> tast::Expr {
        let (constructor, constr_ty) = env
            .lookup_constructor(name)
            .unwrap_or_else(|| panic!("Constructor {} not found in environment", name.0));

        let struct_fields = match &constructor {
            tast::Constructor::Struct(struct_constructor) => {
                let type_name = &struct_constructor.type_name;
                let struct_def = env.structs.get(type_name).unwrap_or_else(|| {
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

        let mut field_positions: HashMap<Lident, usize> = HashMap::new();
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
            let field_expr = self.infer_expr(env, vars, expr);
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

        let inst_constr_ty = self.inst_ty(&constr_ty);
        let ret_ty = self.fresh_ty_var();

        if !args_tast.is_empty() {
            let actual_ty = tast::Ty::TFunc {
                params: args_tast.iter().map(|arg| arg.get_ty()).collect(),
                ret_ty: Box::new(ret_ty.clone()),
            };
            env.constraints
                .push(Constraint::TypeEqual(inst_constr_ty, actual_ty));
        } else {
            env.constraints
                .push(Constraint::TypeEqual(inst_constr_ty, ret_ty.clone()));
        }

        tast::Expr::EConstr {
            constructor,
            args: args_tast,
            ty: ret_ty,
        }
    }

    fn infer_tuple_expr(
        &mut self,
        env: &mut Env,
        vars: &im::HashMap<Lident, tast::Ty>,
        items: &[ast::Expr],
    ) -> tast::Expr {
        let mut typs = Vec::new();
        let mut items_tast = Vec::new();
        for item in items.iter() {
            let item_tast = self.infer_expr(env, vars, item);
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
        env: &mut Env,
        vars: &im::HashMap<Lident, tast::Ty>,
        items: &[ast::Expr],
    ) -> tast::Expr {
        let len = items.len();
        let elem_ty = self.fresh_ty_var();
        let mut items_tast = Vec::with_capacity(len);
        for item in items.iter() {
            let item_tast = self.infer_expr(env, vars, item);
            env.constraints
                .push(Constraint::TypeEqual(item_tast.get_ty(), elem_ty.clone()));
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
        env: &mut Env,
        vars: &im::HashMap<Lident, tast::Ty>,
        params: &[ast::ClosureParam],
        body: &ast::Expr,
    ) -> tast::Expr {
        let mut closure_vars = vars.clone();
        let mut params_tast = Vec::new();
        let mut param_tys = Vec::new();
        let current_tparams_env = self.current_tparams_env();

        for param in params.iter() {
            let param_ty = match &param.ty {
                Some(ty) => ast_ty_to_tast_ty_with_tparams_env(ty, &current_tparams_env),
                None => self.fresh_ty_var(),
            };
            closure_vars.insert(param.name.clone(), param_ty.clone());
            param_tys.push(param_ty.clone());
            params_tast.push(tast::ClosureParam {
                name: param.name.0.clone(),
                ty: param_ty,
                astptr: Some(param.astptr),
            });
        }

        let body_tast = self.infer_expr(env, &closure_vars, body);
        let body_ty = body_tast.get_ty();

        let closure_ty = tast::Ty::TFunc {
            params: param_tys,
            ret_ty: Box::new(body_ty.clone()),
        };

        tast::Expr::EClosure {
            params: params_tast,
            body: Box::new(body_tast),
            ty: closure_ty,
        }
    }

    fn check_closure_expr(
        &mut self,
        env: &mut Env,
        vars: &im::HashMap<Lident, tast::Ty>,
        params: &[ast::ClosureParam],
        body: &ast::Expr,
        expected: &tast::Ty,
    ) -> tast::Expr {
        match expected {
            tast::Ty::TFunc {
                params: expected_params,
                ret_ty: expected_ret,
            } if expected_params.len() == params.len() => {
                let mut closure_vars = vars.clone();
                let mut params_tast = Vec::new();
                let mut param_tys = Vec::new();
                let current_tparams_env = self.current_tparams_env();

                for (param, expected_param_ty) in params.iter().zip(expected_params.iter()) {
                    let annotated_ty = param
                        .ty
                        .as_ref()
                        .map(|ty| ast_ty_to_tast_ty_with_tparams_env(ty, &current_tparams_env));

                    let param_ty = match annotated_ty {
                        Some(ann_ty) => {
                            env.constraints.push(Constraint::TypeEqual(
                                ann_ty.clone(),
                                expected_param_ty.clone(),
                            ));
                            ann_ty
                        }
                        None => expected_param_ty.clone(),
                    };

                    closure_vars.insert(param.name.clone(), param_ty.clone());
                    param_tys.push(param_ty.clone());
                    params_tast.push(tast::ClosureParam {
                        name: param.name.0.clone(),
                        ty: param_ty,
                        astptr: Some(param.astptr),
                    });
                }

                let body_tast = self.check_expr(env, &closure_vars, body, expected_ret.as_ref());
                let body_ty = body_tast.get_ty();

                tast::Expr::EClosure {
                    params: params_tast,
                    body: Box::new(body_tast),
                    ty: tast::Ty::TFunc {
                        params: param_tys,
                        ret_ty: Box::new(body_ty),
                    },
                }
            }
            _ => self.infer_closure_expr(env, vars, params, body),
        }
    }

    fn infer_let_expr(
        &mut self,
        env: &mut Env,
        vars: &im::HashMap<Lident, tast::Ty>,
        pat: &ast::Pat,
        value: &ast::Expr,
        body: &ast::Expr,
    ) -> tast::Expr {
        let value_tast = self.infer_expr(env, vars, value);
        let value_ty = value_tast.get_ty();

        let mut new_vars = vars.clone();
        let pat_tast = self.check_pat(env, &mut new_vars, pat, &value_ty);

        let body_tast = self.infer_expr(env, &new_vars, body);
        let body_ty = body_tast.get_ty();
        tast::Expr::ELet {
            pat: pat_tast,
            value: Box::new(value_tast),
            body: Box::new(body_tast),
            ty: body_ty.clone(),
        }
    }

    fn check_let_expr(
        &mut self,
        env: &mut Env,
        vars: &im::HashMap<Lident, tast::Ty>,
        pat: &ast::Pat,
        value: &ast::Expr,
        body: &ast::Expr,
        expected: &tast::Ty,
    ) -> tast::Expr {
        let value_tast = self.infer_expr(env, vars, value);
        let value_ty = value_tast.get_ty();

        let mut new_vars = vars.clone();
        let pat_tast = self.check_pat(env, &mut new_vars, pat, &value_ty);

        let body_tast = self.check_expr(env, &new_vars, body, expected);
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
        env: &mut Env,
        vars: &im::HashMap<Lident, tast::Ty>,
        expr: &ast::Expr,
        arms: &[ast::Arm],
    ) -> tast::Expr {
        let expr_tast = self.infer_expr(env, vars, expr);
        let expr_ty = expr_tast.get_ty();

        let mut arms_tast = Vec::new();
        let arm_ty = self.fresh_ty_var();
        for arm in arms.iter() {
            let mut new_vars = vars.clone();
            let arm_tast = self.check_pat(env, &mut new_vars, &arm.pat, &expr_ty);
            let arm_body_tast = self.infer_expr(env, &new_vars, &arm.body);
            env.constraints.push(Constraint::TypeEqual(
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
        }
    }

    fn infer_if_expr(
        &mut self,
        env: &mut Env,
        vars: &im::HashMap<Lident, tast::Ty>,
        cond: &ast::Expr,
        then_branch: &ast::Expr,
        else_branch: &ast::Expr,
    ) -> tast::Expr {
        let cond_tast = self.infer_expr(env, vars, cond);
        env.constraints
            .push(Constraint::TypeEqual(cond_tast.get_ty(), tast::Ty::TBool));

        let then_tast = self.infer_expr(env, vars, then_branch);
        let else_tast = self.infer_expr(env, vars, else_branch);
        let result_ty = self.fresh_ty_var();

        env.constraints
            .push(Constraint::TypeEqual(then_tast.get_ty(), result_ty.clone()));
        env.constraints
            .push(Constraint::TypeEqual(else_tast.get_ty(), result_ty.clone()));

        tast::Expr::EIf {
            cond: Box::new(cond_tast),
            then_branch: Box::new(then_tast),
            else_branch: Box::new(else_tast),
            ty: result_ty,
        }
    }

    fn infer_call_expr(
        &mut self,
        env: &mut Env,
        vars: &im::HashMap<Lident, tast::Ty>,
        func: &ast::Expr,
        args: &[ast::Expr],
    ) -> tast::Expr {
        let mut args_tast = Vec::new();
        let mut arg_types = Vec::new();
        for arg in args.iter() {
            let arg_tast = self.infer_expr(env, vars, arg);
            arg_types.push(arg_tast.get_ty());
            args_tast.push(arg_tast);
        }

        match func {
            ast::Expr::EVar { name, astptr } => {
                if let Some(func_ty) = env.get_type_of_function(&name.0) {
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
                    let inst_ty = self.inst_ty(&func_ty);
                    env.constraints.push(Constraint::TypeEqual(
                        inst_ty.clone(),
                        call_site_func_ty.clone(),
                    ));
                    tast::Expr::ECall {
                        func: Box::new(tast::Expr::EVar {
                            name: name.0.clone(),
                            ty: inst_ty,
                            astptr: Some(*astptr),
                        }),
                        args: args_tast,
                        ty: ret_ty,
                    }
                } else if let Some(trait_name) =
                    env.overloaded_funcs_to_trait_name.get(&name.0).cloned()
                {
                    let ret_ty = self.fresh_ty_var();
                    let call_site_func_ty = tast::Ty::TFunc {
                        params: arg_types,
                        ret_ty: Box::new(ret_ty.clone()),
                    };
                    env.constraints.push(Constraint::Overloaded {
                        op: name.clone(),
                        trait_name,
                        call_site_type: call_site_func_ty.clone(),
                    });

                    tast::Expr::ECall {
                        func: Box::new(tast::Expr::EVar {
                            name: name.0.clone(),
                            ty: call_site_func_ty.clone(),
                            astptr: Some(*astptr),
                        }),
                        args: args_tast,
                        ty: ret_ty,
                    }
                } else if let Some(var_ty) = vars.get(name) {
                    let ret_ty = self.fresh_ty_var();
                    let call_site_func_ty = tast::Ty::TFunc {
                        params: arg_types,
                        ret_ty: Box::new(ret_ty.clone()),
                    };
                    env.constraints.push(Constraint::TypeEqual(
                        var_ty.clone(),
                        call_site_func_ty.clone(),
                    ));

                    tast::Expr::ECall {
                        func: Box::new(tast::Expr::EVar {
                            name: name.0.clone(),
                            ty: var_ty.clone(),
                            astptr: Some(*astptr),
                        }),
                        args: args_tast,
                        ty: ret_ty,
                    }
                } else {
                    let ret_ty = self.fresh_ty_var();
                    let call_site_func_ty = tast::Ty::TFunc {
                        params: arg_types,
                        ret_ty: Box::new(ret_ty.clone()),
                    };
                    let func_tast = self.infer_expr(env, vars, func);
                    env.constraints.push(Constraint::TypeEqual(
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
            _ => {
                let ret_ty = self.fresh_ty_var();
                let call_site_func_ty = tast::Ty::TFunc {
                    params: arg_types,
                    ret_ty: Box::new(ret_ty.clone()),
                };
                let func_tast = self.infer_expr(env, vars, func);
                env.constraints.push(Constraint::TypeEqual(
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
        env: &mut Env,
        vars: &im::HashMap<Lident, tast::Ty>,
        op: ast::UnaryOp,
        expr: &ast::Expr,
    ) -> tast::Expr {
        let expr_tast = self.infer_expr(env, vars, expr);
        let expr_ty = expr_tast.get_ty();
        let method_name = op.method_name();
        let builtin_expr = match op {
            ast::UnaryOp::Not => {
                let ref_inner = match &expr_ty {
                    tast::Ty::TRef { elem } => Some(elem.as_ref().clone()),
                    _ => None,
                };

                if let Some(inner_ty) = ref_inner {
                    env.constraints.push(Constraint::TypeEqual(
                        expr_ty.clone(),
                        tast::Ty::TRef {
                            elem: Box::new(inner_ty.clone()),
                        },
                    ));
                    Some(tast::Expr::EUnary {
                        op,
                        expr: Box::new(expr_tast.clone()),
                        ty: inner_ty,
                        resolution: tast::UnaryResolution::Builtin,
                    })
                } else {
                    env.constraints
                        .push(Constraint::TypeEqual(expr_ty.clone(), tast::Ty::TBool));
                    Some(tast::Expr::EUnary {
                        op,
                        expr: Box::new(expr_tast.clone()),
                        ty: tast::Ty::TBool,
                        resolution: tast::UnaryResolution::Builtin,
                    })
                }
            }
            ast::UnaryOp::Neg => {
                env.constraints
                    .push(Constraint::TypeEqual(expr_ty.clone(), tast::Ty::TInt));
                Some(tast::Expr::EUnary {
                    op,
                    expr: Box::new(expr_tast.clone()),
                    ty: tast::Ty::TInt,
                    resolution: tast::UnaryResolution::Builtin,
                })
            }
        };

        if let Some(expr) = builtin_expr {
            return expr;
        }

        if let Some(trait_name) = env.overloaded_funcs_to_trait_name.get(method_name).cloned() {
            let ret_ty = self.fresh_ty_var();
            let call_site_type = tast::Ty::TFunc {
                params: vec![expr_ty.clone()],
                ret_ty: Box::new(ret_ty.clone()),
            };
            env.constraints.push(Constraint::Overloaded {
                op: ast::Lident(method_name.to_string()),
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
        env: &mut Env,
        vars: &im::HashMap<Lident, tast::Ty>,
        op: ast::BinaryOp,
        lhs: &ast::Expr,
        rhs: &ast::Expr,
    ) -> tast::Expr {
        let lhs_tast = self.infer_expr(env, vars, lhs);
        let rhs_tast = self.infer_expr(env, vars, rhs);
        let lhs_ty = lhs_tast.get_ty();
        let rhs_ty = rhs_tast.get_ty();
        let method_name = op.method_name();

        if op == ast::BinaryOp::Assign {
            env.constraints.push(Constraint::TypeEqual(
                lhs_ty.clone(),
                tast::Ty::TRef {
                    elem: Box::new(rhs_ty.clone()),
                },
            ));
            return tast::Expr::EBinary {
                op,
                lhs: Box::new(lhs_tast),
                rhs: Box::new(rhs_tast),
                ty: tast::Ty::TUnit,
                resolution: tast::BinaryResolution::Builtin,
            };
        }

        if let Some(trait_name) = env.overloaded_funcs_to_trait_name.get(method_name).cloned()
            && !binary_supports_builtin(op, &lhs_ty, &rhs_ty)
        {
            let ret_ty = self.fresh_ty_var();
            let call_site_type = tast::Ty::TFunc {
                params: vec![lhs_ty.clone(), rhs_ty.clone()],
                ret_ty: Box::new(ret_ty.clone()),
            };
            env.constraints.push(Constraint::Overloaded {
                op: ast::Lident(method_name.to_string()),
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
            ast::BinaryOp::Add => self.fresh_ty_var(),
            ast::BinaryOp::Sub | ast::BinaryOp::Mul | ast::BinaryOp::Div => tast::Ty::TInt,
            ast::BinaryOp::And | ast::BinaryOp::Or => tast::Ty::TBool,
            ast::BinaryOp::Assign => unreachable!(),
        };

        match op {
            ast::BinaryOp::Add => {
                env.constraints
                    .push(Constraint::TypeEqual(lhs_ty.clone(), ret_ty.clone()));
                env.constraints
                    .push(Constraint::TypeEqual(rhs_ty.clone(), ret_ty.clone()));
            }
            ast::BinaryOp::Sub | ast::BinaryOp::Mul | ast::BinaryOp::Div => {
                env.constraints
                    .push(Constraint::TypeEqual(lhs_ty.clone(), tast::Ty::TInt));
                env.constraints
                    .push(Constraint::TypeEqual(rhs_ty.clone(), tast::Ty::TInt));
            }
            ast::BinaryOp::And | ast::BinaryOp::Or => {
                env.constraints
                    .push(Constraint::TypeEqual(lhs_ty.clone(), tast::Ty::TBool));
                env.constraints
                    .push(Constraint::TypeEqual(rhs_ty.clone(), tast::Ty::TBool));
            }
            ast::BinaryOp::Assign => unreachable!(),
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
        env: &mut Env,
        vars: &im::HashMap<Lident, tast::Ty>,
        tuple: &ast::Expr,
        index: usize,
    ) -> tast::Expr {
        let tuple_tast = self.infer_expr(env, vars, tuple);
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
                env.report_typer_error(format!(
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

    fn check_pat(
        &mut self,
        env: &mut Env,
        vars: &mut im::HashMap<Lident, tast::Ty>,
        pat: &ast::Pat,
        ty: &tast::Ty,
    ) -> tast::Pat {
        match pat {
            ast::Pat::PVar { name, astptr } => self.check_pat_var(vars, name, astptr, ty),
            ast::Pat::PUnit => self.check_pat_unit(),
            ast::Pat::PBool { value } => self.check_pat_bool(*value),
            ast::Pat::PInt { value } => self.check_pat_int(env, *value, ty),
            ast::Pat::PString { value } => self.check_pat_string(env, value, ty),
            ast::Pat::PConstr { .. } => self.check_pat_constructor(env, vars, pat, ty),
            ast::Pat::PStruct { .. } => self.check_pat_constructor(env, vars, pat, ty),
            ast::Pat::PTuple { pats } => self.check_pat_tuple(env, vars, pats, ty),
            ast::Pat::PWild => self.check_pat_wild(env, ty),
        }
    }

    fn check_pat_var(
        &mut self,
        vars: &mut im::HashMap<Lident, tast::Ty>,
        name: &Lident,
        astptr: &MySyntaxNodePtr,
        ty: &tast::Ty,
    ) -> tast::Pat {
        vars.insert(name.clone(), ty.clone());
        tast::Pat::PVar {
            name: name.0.clone(),
            ty: ty.clone(),
            astptr: Some(*astptr),
        }
    }

    fn check_pat_unit(&self) -> tast::Pat {
        tast::Pat::PUnit {
            ty: tast::Ty::TUnit,
        }
    }

    fn check_pat_bool(&self, value: bool) -> tast::Pat {
        tast::Pat::PBool {
            value,
            ty: tast::Ty::TBool,
        }
    }

    fn check_pat_int(&mut self, env: &mut Env, value: i32, ty: &tast::Ty) -> tast::Pat {
        env.constraints
            .push(Constraint::TypeEqual(tast::Ty::TInt, ty.clone()));
        tast::Pat::PInt {
            value,
            ty: tast::Ty::TInt,
        }
    }

    fn check_pat_string(&mut self, env: &mut Env, value: &String, ty: &tast::Ty) -> tast::Pat {
        env.constraints
            .push(Constraint::TypeEqual(tast::Ty::TString, ty.clone()));
        tast::Pat::PString {
            value: value.to_owned(),
            ty: tast::Ty::TString,
        }
    }

    fn check_pat_constructor(
        &mut self,
        env: &mut Env,
        vars: &mut im::HashMap<Lident, tast::Ty>,
        pat: &ast::Pat,
        ty: &tast::Ty,
    ) -> tast::Pat {
        let tast = self.infer_pat(env, vars, pat);
        env.constraints
            .push(Constraint::TypeEqual(tast.get_ty(), ty.clone()));
        tast
    }

    fn check_pat_tuple(
        &mut self,
        env: &mut Env,
        vars: &mut im::HashMap<Lident, tast::Ty>,
        pats: &[ast::Pat],
        ty: &tast::Ty,
    ) -> tast::Pat {
        let mut pats_tast = Vec::new();
        let mut pat_typs = Vec::new();
        for pat in pats.iter() {
            let pat_tast = self.infer_pat(env, vars, pat);
            pat_typs.push(pat_tast.get_ty());
            pats_tast.push(pat_tast);
        }
        let pat_ty = tast::Ty::TTuple { typs: pat_typs };
        env.constraints
            .push(Constraint::TypeEqual(pat_ty.clone(), ty.clone()));
        tast::Pat::PTuple {
            items: pats_tast,
            ty: pat_ty,
        }
    }

    fn check_pat_wild(&mut self, env: &mut Env, ty: &tast::Ty) -> tast::Pat {
        let pat_ty = self.fresh_ty_var();
        env.constraints
            .push(Constraint::TypeEqual(pat_ty.clone(), ty.clone()));
        tast::Pat::PWild { ty: pat_ty }
    }

    fn infer_pat(
        &mut self,
        env: &mut Env,
        vars: &mut im::HashMap<Lident, tast::Ty>,
        pat: &ast::Pat,
    ) -> tast::Pat {
        match pat {
            ast::Pat::PVar { name, astptr } => {
                let pat_ty = match vars.get(name) {
                    Some(ty) => ty.clone(),
                    None => {
                        let newvar = self.fresh_ty_var();
                        vars.insert(name.clone(), newvar.clone());
                        newvar
                    }
                };
                tast::Pat::PVar {
                    name: name.0.clone(),
                    ty: pat_ty.clone(),
                    astptr: Some(*astptr),
                }
            }
            ast::Pat::PConstr { vcon, args } => {
                let (constructor, constr_ty) = env
                    .lookup_constructor(vcon)
                    .unwrap_or_else(|| panic!("Constructor {} not found in environment", vcon.0));

                let expected_arity = match &constructor {
                    tast::Constructor::Enum(enum_constructor) => env
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

                let ret_ty = self.fresh_ty_var();
                let mut args_tast = Vec::new();
                let mut args_ty = Vec::new();
                for arg in args.iter() {
                    let arg_tast = self.infer_pat(env, vars, arg);
                    args_ty.push(arg_tast.get_ty());
                    args_tast.push(arg_tast);
                }

                if !args_ty.is_empty() {
                    let actual_ty = tast::Ty::TFunc {
                        params: args_ty,
                        ret_ty: Box::new(ret_ty.clone()),
                    };
                    env.constraints
                        .push(Constraint::TypeEqual(inst_constr_ty.clone(), actual_ty));
                } else {
                    env.constraints.push(Constraint::TypeEqual(
                        inst_constr_ty.clone(),
                        ret_ty.clone(),
                    ));
                }

                tast::Pat::PConstr {
                    constructor,
                    args: args_tast,
                    ty: ret_ty,
                }
            }
            ast::Pat::PStruct { name, fields } => {
                let struct_fields = {
                    let struct_def = env.structs.get(name).unwrap_or_else(|| {
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

                let (constructor, constr_ty) = env.lookup_constructor(name).unwrap_or_else(|| {
                    panic!("Struct {} not found when checking constructor", name.0)
                });

                let mut args_tast = Vec::new();
                let mut args_ty = Vec::new();

                for (field_name, _) in struct_fields.iter() {
                    let pat_ast = field_map.remove(&field_name.0).unwrap_or_else(|| {
                        panic!("Struct pattern {} missing field {}", name.0, field_name.0)
                    });
                    let pat_tast = self.infer_pat(env, vars, pat_ast);
                    args_ty.push(pat_tast.get_ty());
                    args_tast.push(pat_tast);
                }

                if !field_map.is_empty() {
                    let extra = field_map.keys().cloned().collect::<Vec<_>>().join(", ");
                    panic!("Struct pattern {} has unknown fields: {}", name.0, extra);
                }

                let inst_constr_ty = self.inst_ty(&constr_ty);
                let ret_ty = self.fresh_ty_var();

                if !args_ty.is_empty() {
                    let actual_ty = tast::Ty::TFunc {
                        params: args_ty,
                        ret_ty: Box::new(ret_ty.clone()),
                    };
                    env.constraints
                        .push(Constraint::TypeEqual(inst_constr_ty.clone(), actual_ty));
                } else {
                    env.constraints.push(Constraint::TypeEqual(
                        inst_constr_ty.clone(),
                        ret_ty.clone(),
                    ));
                }

                tast::Pat::PConstr {
                    constructor,
                    args: args_tast,
                    ty: ret_ty,
                }
            }
            ast::Pat::PTuple { pats } => {
                let mut pats_tast = Vec::new();
                let mut pat_typs = Vec::new();
                for pat in pats.iter() {
                    let pat_tast = self.infer_pat(env, vars, pat);
                    pat_typs.push(pat_tast.get_ty());
                    pats_tast.push(pat_tast);
                }
                tast::Pat::PTuple {
                    items: pats_tast,
                    ty: tast::Ty::TTuple { typs: pat_typs },
                }
            }
            ast::Pat::PUnit => tast::Pat::PUnit {
                ty: tast::Ty::TUnit,
            },
            ast::Pat::PBool { value } => tast::Pat::PBool {
                value: *value,
                ty: tast::Ty::TBool,
            },
            ast::Pat::PInt { value } => tast::Pat::PInt {
                value: *value,
                ty: tast::Ty::TInt,
            },
            ast::Pat::PString { value } => tast::Pat::PString {
                value: value.clone(),
                ty: tast::Ty::TString,
            },
            ast::Pat::PWild => {
                let pat_ty = self.fresh_ty_var();
                tast::Pat::PWild { ty: pat_ty }
            }
        }
    }
}
