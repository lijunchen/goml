use ast::ast::Uident;

use crate::{
    anf::{self, AExpr},
    env::Env,
    go::goast::{self, go_type_name_for, tast_ty_to_go_type},
    tast,
    tast::Constructor,
};

use indexmap::IndexSet;
use std::collections::HashMap;

use super::goty;
use super::runtime;

fn compile_imm(env: &Env, imm: &anf::ImmExpr) -> goast::Expr {
    match imm {
        anf::ImmExpr::ImmVar { name, ty: _ } => goast::Expr::Var {
            name: name.clone(),
            ty: tast_ty_to_go_type(&imm_ty(imm)),
        },
        anf::ImmExpr::ImmUnit { ty: _ } => goast::Expr::Unit {
            ty: goty::GoType::TUnit,
        },
        anf::ImmExpr::ImmBool { value, ty: _ } => goast::Expr::Bool {
            value: *value,
            ty: goty::GoType::TBool,
        },
        anf::ImmExpr::ImmInt { value, ty: _ } => goast::Expr::Int {
            value: *value,
            ty: goty::GoType::TInt,
        },
        anf::ImmExpr::ImmString { value, ty: _ } => goast::Expr::String {
            value: value.clone(),
            ty: goty::GoType::TString,
        },
        anf::ImmExpr::ImmTag { index, ty } => goast::Expr::StructLiteral {
            fields: vec![],
            ty: variant_ty_by_index(env, ty, *index),
        },
    }
}

fn imm_ty(imm: &anf::ImmExpr) -> tast::Ty {
    match imm {
        anf::ImmExpr::ImmVar { ty, .. }
        | anf::ImmExpr::ImmUnit { ty }
        | anf::ImmExpr::ImmBool { ty, .. }
        | anf::ImmExpr::ImmInt { ty, .. }
        | anf::ImmExpr::ImmString { ty, .. }
        | anf::ImmExpr::ImmTag { ty, .. } => ty.clone(),
    }
}

fn cexpr_ty(env: &Env, e: &anf::CExpr) -> goty::GoType {
    let t = match e {
        anf::CExpr::CImm { imm } => imm_ty(imm),
        anf::CExpr::EConstr { ty, .. }
        | anf::CExpr::ETuple { ty, .. }
        | anf::CExpr::EArray { ty, .. }
        | anf::CExpr::EMatch { ty, .. }
        | anf::CExpr::EIf { ty, .. }
        | anf::CExpr::EWhile { ty, .. }
        | anf::CExpr::ECall { ty, .. }
        | anf::CExpr::EProj { ty, .. } => ty.clone(),
        // For EConstrGet, compute field type from the scrutinee's data constructor
        anf::CExpr::EConstrGet {
            expr,
            constructor,
            field_index,
            ty: _,
        } => match constructor {
            Constructor::Enum(enum_constructor) => {
                let def = env
                    .enums
                    .get(&enum_constructor.type_name)
                    .expect("unknown enum in EConstrGet");
                def.variants[enum_constructor.index].1[*field_index].clone()
            }
            Constructor::Struct(struct_constructor) => {
                let scrut_ty = imm_ty(expr);
                let (ty_name, type_args) = match scrut_ty {
                    tast::Ty::TCon { name } => (name, Vec::new()),
                    tast::Ty::TApp { ty, args } => {
                        let base_name = ty.get_constr_name_unsafe();
                        (base_name, args)
                    }
                    other => panic!(
                        "EConstrGet on non-struct type {:?} for constructor {}",
                        other, struct_constructor.type_name.0
                    ),
                };
                let struct_name = &struct_constructor.type_name.0;
                assert_eq!(
                    ty_name, *struct_name,
                    "struct constructor type mismatch: expected {}, got {}",
                    struct_name, ty_name
                );
                let fields =
                    instantiate_struct_fields(env, &struct_constructor.type_name, &type_args);
                fields[*field_index].1.clone()
            }
        },
    };
    tast_ty_to_go_type(&t)
}

fn variant_struct_name(env: &Env, enum_name: &str, variant_name: &str) -> String {
    // Count how many enums define a variant with this name.
    let mut count = 0;
    for (_ename, edef) in env.enums.iter() {
        if edef
            .variants
            .iter()
            .any(|(v, _)| v.0.as_str() == variant_name)
        {
            count += 1;
            if count > 1 {
                break;
            }
        }
    }
    if count > 1 {
        format!("{}_{}", enum_name, variant_name)
    } else {
        variant_name.to_string()
    }
}

fn lookup_variant_name(env: &Env, ty: &tast::Ty, index: usize) -> String {
    let name = ty.get_constr_name_unsafe();
    if let Some(def) = env.enums.get(&Uident::new(&name)) {
        let (vname, _fields) = &def.variants[index];
        return variant_struct_name(env, &name, &vname.0);
    }
    panic!(
        "Cannot resolve variant name for ty {:?} index {}",
        ty, index
    );
}

fn variant_ty_by_index(env: &Env, ty: &tast::Ty, index: usize) -> goty::GoType {
    let vname = lookup_variant_name(env, ty, index);
    let ty = tast::Ty::TCon { name: vname };
    tast_ty_to_go_type(&ty)
}

fn go_package_alias(package_path: &str) -> String {
    let last_segment = package_path.rsplit('/').next().unwrap_or(package_path);
    let mut alias = String::new();
    for ch in last_segment.chars() {
        if ch.is_ascii_alphanumeric() {
            alias.push(ch);
        } else {
            alias.push('_');
        }
    }
    if alias.is_empty() {
        return "pkg".to_string();
    }
    if alias.chars().next().is_some_and(|c| c.is_ascii_digit()) {
        alias.insert(0, '_');
    }
    alias
}

fn substitute_ty_params(ty: &tast::Ty, subst: &HashMap<String, tast::Ty>) -> tast::Ty {
    match ty {
        tast::Ty::TVar(_)
        | tast::Ty::TUnit
        | tast::Ty::TBool
        | tast::Ty::TInt
        | tast::Ty::TString => ty.clone(),
        tast::Ty::TTuple { typs } => tast::Ty::TTuple {
            typs: typs
                .iter()
                .map(|t| substitute_ty_params(t, subst))
                .collect(),
        },
        tast::Ty::TCon { name } => tast::Ty::TCon { name: name.clone() },
        tast::Ty::TApp { ty, args } => tast::Ty::TApp {
            ty: Box::new(substitute_ty_params(ty, subst)),
            args: args
                .iter()
                .map(|t| substitute_ty_params(t, subst))
                .collect(),
        },
        tast::Ty::TParam { name } => subst
            .get(name)
            .cloned()
            .unwrap_or_else(|| tast::Ty::TParam { name: name.clone() }),
        tast::Ty::TArray { len, elem } => tast::Ty::TArray {
            len: *len,
            elem: Box::new(substitute_ty_params(elem, subst)),
        },
        tast::Ty::TRef { elem } => tast::Ty::TRef {
            elem: Box::new(substitute_ty_params(elem, subst)),
        },
        tast::Ty::TFunc { params, ret_ty } => tast::Ty::TFunc {
            params: params
                .iter()
                .map(|t| substitute_ty_params(t, subst))
                .collect(),
            ret_ty: Box::new(substitute_ty_params(ret_ty, subst)),
        },
    }
}

fn instantiate_struct_fields(
    env: &Env,
    type_name: &Uident,
    type_args: &[tast::Ty],
) -> Vec<(String, tast::Ty)> {
    let struct_def = env
        .structs
        .get(type_name)
        .unwrap_or_else(|| panic!("Unknown struct {}", type_name.0));

    if struct_def.generics.len() != type_args.len() {
        panic!(
            "Struct {} expects {} type arguments, but got {}",
            struct_def.name.0,
            struct_def.generics.len(),
            type_args.len()
        );
    }

    let mut subst = HashMap::new();
    for (param, arg) in struct_def.generics.iter().zip(type_args.iter()) {
        subst.insert(param.0.clone(), arg.clone());
    }

    struct_def
        .fields
        .iter()
        .map(|(fname, fty)| (fname.0.clone(), substitute_ty_params(fty, &subst)))
        .collect()
}

fn tuple_to_go_struct_type(_env: &Env, ty: &tast::Ty) -> goty::GoType {
    if let tast::Ty::TTuple { typs } = ty {
        let name = go_type_name_for(ty);
        goty::GoType::TStruct {
            name,
            fields: typs
                .iter()
                .enumerate()
                .map(|(i, t)| (format!("_{}", i), tast_ty_to_go_type(t)))
                .collect(),
        }
    } else {
        panic!("expected tuple type, got {:?}", ty);
    }
}

fn compile_cexpr(env: &Env, e: &anf::CExpr) -> goast::Expr {
    match e {
        anf::CExpr::CImm { imm } => compile_imm(env, imm),
        anf::CExpr::EConstr {
            constructor,
            args,
            ty,
        } => match constructor {
            Constructor::Enum(enum_constructor) => {
                let variant_ty = variant_ty_by_index(env, ty, enum_constructor.index);
                let fields = args
                    .iter()
                    .enumerate()
                    .map(|(i, a)| (format!("_{}", i), compile_imm(env, a)))
                    .collect();
                goast::Expr::StructLiteral {
                    ty: variant_ty,
                    fields,
                }
            }
            Constructor::Struct(struct_constructor) => {
                let go_ty = tast_ty_to_go_type(ty);
                let struct_def = env
                    .structs
                    .get(&struct_constructor.type_name)
                    .unwrap_or_else(|| panic!("unknown struct {}", struct_constructor.type_name.0));
                if struct_def.fields.len() != args.len() {
                    panic!(
                        "struct constructor {} expects {} args, got {}",
                        struct_constructor.type_name.0,
                        struct_def.fields.len(),
                        args.len()
                    );
                }
                let fields = struct_def
                    .fields
                    .iter()
                    .zip(args.iter())
                    .map(|((fname, _), arg)| (fname.0.clone(), compile_imm(env, arg)))
                    .collect();
                goast::Expr::StructLiteral { ty: go_ty, fields }
            }
        },
        anf::CExpr::ETuple { items, ty } => {
            let fields = items
                .iter()
                .enumerate()
                .map(|(i, a)| (format!("_{}", i), compile_imm(env, a)))
                .collect();
            goast::Expr::StructLiteral {
                ty: tuple_to_go_struct_type(env, ty),
                fields,
            }
        }
        anf::CExpr::EArray { items, ty } => {
            let elems = items.iter().map(|item| compile_imm(env, item)).collect();
            goast::Expr::ArrayLiteral {
                elems,
                ty: tast_ty_to_go_type(ty),
            }
        }
        anf::CExpr::EMatch { expr, .. } => match imm_ty(expr) {
            // Boolean matches are handled as statements (not expressions) in Go.
            tast::Ty::TBool => {
                panic!("boolean match should be lowered to  goast::Stmt::If in compile_aexpr")
            }
            _ => {
                panic!("EMatch should be lowered in compile_aexpr/compile_aexpr_assign")
            }
        },
        anf::CExpr::EIf {
            cond: _,
            then: _,
            else_: _,
            ty: _,
        } => panic!("EIf should be lowered to  goast::Stmt::If in compile_aexpr"),
        anf::CExpr::EWhile { .. } => {
            panic!("EWhile should be lowered to goast::Stmt::Loop in compile_aexpr")
        }
        anf::CExpr::EConstrGet {
            expr,
            constructor,
            field_index,
            ty: _,
        } => {
            let obj = compile_imm(env, expr);
            match constructor {
                Constructor::Enum(enum_constructor) => {
                    let def = env
                        .enums
                        .get(&enum_constructor.type_name)
                        .expect("unknown enum in EConstrGet");
                    let field_ty = def.variants[enum_constructor.index].1[*field_index].clone();
                    goast::Expr::FieldAccess {
                        obj: Box::new(obj),
                        field: format!("_{}", field_index),
                        ty: tast_ty_to_go_type(&field_ty),
                    }
                }
                Constructor::Struct(struct_constructor) => {
                    let scrut_ty = imm_ty(expr);
                    let (ty_name, type_args) = match scrut_ty {
                        tast::Ty::TCon { name } => (name, Vec::new()),
                        tast::Ty::TApp { ty, args } => {
                            let base_name = ty.get_constr_name_unsafe();
                            (base_name, args)
                        }
                        other => panic!(
                            "EConstrGet on non-struct type {:?} for constructor {}",
                            other, struct_constructor.type_name.0
                        ),
                    };
                    let struct_name = &struct_constructor.type_name.0;
                    assert_eq!(
                        ty_name, *struct_name,
                        "struct constructor type mismatch: expected {}, got {}",
                        struct_name, ty_name
                    );
                    let fields =
                        instantiate_struct_fields(env, &struct_constructor.type_name, &type_args);
                    let (field_name, field_ty) = &fields[*field_index];
                    goast::Expr::FieldAccess {
                        obj: Box::new(obj),
                        field: field_name.clone(),
                        ty: tast_ty_to_go_type(field_ty),
                    }
                }
            }
        }
        anf::CExpr::ECall { func, args, ty } => {
            let compiled_args: Vec<_> = args.iter().map(|arg| compile_imm(env, arg)).collect();
            let func_ty = tast_ty_to_go_type(&imm_ty(func));

            if let anf::ImmExpr::ImmVar { name, .. } = &func
                && (*name == "array_get" || *name == "array_set")
            {
                let helper = runtime::array_helper_fn_name(name, &imm_ty(&args[0]));
                goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: helper,
                        ty: func_ty,
                    }),
                    args: compiled_args,
                    ty: tast_ty_to_go_type(ty),
                }
            } else if let anf::ImmExpr::ImmVar { name, .. } = &func
                && (*name == "ref" || *name == "ref_get" || *name == "ref_set")
            {
                let (helper, helper_ty) = if name == "ref" {
                    let tast::Ty::TRef { elem } = ty else {
                        panic!("ref return type must be reference, got {:?}", ty);
                    };
                    let elem_go_ty = tast_ty_to_go_type(elem);
                    let ref_go_ty = tast_ty_to_go_type(ty);
                    (
                        runtime::ref_helper_fn_name("ref", ty),
                        goty::GoType::TFunc {
                            params: vec![elem_go_ty],
                            ret_ty: Box::new(ref_go_ty),
                        },
                    )
                } else {
                    let ref_ty = imm_ty(&args[0]);
                    let tast::Ty::TRef { elem } = &ref_ty else {
                        panic!("{} expects reference argument, got {:?}", name, ref_ty);
                    };
                    let ref_go_ty = tast_ty_to_go_type(&ref_ty);
                    let elem_go_ty = tast_ty_to_go_type(elem);
                    let ret_ty = if name == "ref_get" {
                        elem_go_ty.clone()
                    } else {
                        goty::GoType::TUnit
                    };
                    (
                        runtime::ref_helper_fn_name(name, &ref_ty),
                        goty::GoType::TFunc {
                            params: if name == "ref_get" {
                                vec![ref_go_ty.clone()]
                            } else {
                                vec![ref_go_ty.clone(), elem_go_ty.clone()]
                            },
                            ret_ty: Box::new(ret_ty),
                        },
                    )
                };
                goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: helper,
                        ty: helper_ty,
                    }),
                    args: compiled_args,
                    ty: tast_ty_to_go_type(ty),
                }
            } else if let anf::ImmExpr::ImmVar { name, .. } = &func
                && let Some(extern_fn) = env.extern_funcs.get(name)
            {
                let alias = go_package_alias(&extern_fn.package_path);
                goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: format!("{}.{}", alias, extern_fn.go_name),
                        ty: func_ty,
                    }),
                    args: compiled_args,
                    ty: tast_ty_to_go_type(ty),
                }
            } else {
                goast::Expr::Call {
                    func: Box::new(compile_imm(env, func)),
                    args: compiled_args,
                    ty: tast_ty_to_go_type(ty),
                }
            }
        }
        anf::CExpr::EProj { tuple, index, ty } => {
            let obj = compile_imm(env, tuple);
            goast::Expr::FieldAccess {
                obj: Box::new(obj),
                field: format!("_{}", index),
                ty: tast_ty_to_go_type(ty),
            }
        }
    }
}

fn compile_match_branches<F>(
    env: &Env,
    scrutinee: &anf::ImmExpr,
    arms: &[anf::Arm],
    default: &Option<Box<anf::AExpr>>,
    mut build_branch: F,
) -> Vec<goast::Stmt>
where
    F: FnMut(anf::AExpr) -> Vec<goast::Stmt>,
{
    match imm_ty(scrutinee) {
        tast::Ty::TUnit => {
            if let Some(first) = arms.first() {
                return build_branch(first.body.clone());
            }
            if let Some(default_arm) = default.as_ref() {
                return build_branch((**default_arm).clone());
            }
            Vec::new()
        }
        tast::Ty::TBool => {
            let mut cases = Vec::new();
            for arm in arms {
                if let anf::ImmExpr::ImmBool { value, .. } = &arm.lhs {
                    cases.push((
                        goast::Expr::Bool {
                            value: *value,
                            ty: goty::GoType::TBool,
                        },
                        goast::Block {
                            stmts: build_branch(arm.body.clone()),
                        },
                    ));
                } else {
                    panic!("expected ImmBool in boolean match arm");
                }
            }
            let default_block = default.as_ref().map(|d| goast::Block {
                stmts: build_branch((**d).clone()),
            });
            vec![goast::Stmt::SwitchExpr {
                expr: compile_imm(env, scrutinee),
                cases,
                default: default_block,
            }]
        }
        tast::Ty::TInt => {
            let mut cases = Vec::new();
            for arm in arms {
                if let anf::ImmExpr::ImmInt { value, .. } = &arm.lhs {
                    cases.push((
                        goast::Expr::Int {
                            value: *value,
                            ty: goty::GoType::TInt,
                        },
                        goast::Block {
                            stmts: build_branch(arm.body.clone()),
                        },
                    ));
                } else {
                    panic!("expected ImmInt in integer match arm");
                }
            }
            let default_block = default.as_ref().map(|d| goast::Block {
                stmts: build_branch((**d).clone()),
            });
            vec![goast::Stmt::SwitchExpr {
                expr: compile_imm(env, scrutinee),
                cases,
                default: default_block,
            }]
        }
        tast::Ty::TString => {
            let mut cases = Vec::new();
            for arm in arms {
                if let anf::ImmExpr::ImmString { value, .. } = &arm.lhs {
                    cases.push((
                        goast::Expr::String {
                            value: value.clone(),
                            ty: goty::GoType::TString,
                        },
                        goast::Block {
                            stmts: build_branch(arm.body.clone()),
                        },
                    ));
                } else {
                    panic!("expected ImmString in string match arm");
                }
            }
            let default_block = default.as_ref().map(|d| goast::Block {
                stmts: build_branch((**d).clone()),
            });
            vec![goast::Stmt::SwitchExpr {
                expr: compile_imm(env, scrutinee),
                cases,
                default: default_block,
            }]
        }
        ty @ (tast::Ty::TCon { .. } | tast::Ty::TApp { .. }) => {
            let type_name = ty.get_constr_name_unsafe();
            if !env.enums.contains_key(&Uident::new(&type_name)) {
                panic!(
                    "unsupported scrutinee type {:?} for match in Go backend",
                    ty
                );
            }
            let scrutinee_name = match scrutinee {
                anf::ImmExpr::ImmVar { name, .. } => name.clone(),
                _ => {
                    unreachable!("expected scrutinee to be a variable after ANF lowering")
                }
            };
            let mut cases = Vec::new();
            for arm in arms {
                if let anf::ImmExpr::ImmTag { index, ty } = &arm.lhs {
                    let vty = variant_ty_by_index(env, ty, *index);
                    cases.push((
                        vty,
                        goast::Block {
                            stmts: build_branch(arm.body.clone()),
                        },
                    ));
                } else {
                    panic!("expected ImmTag in enum match arm");
                }
            }
            let default_block = default.as_ref().map(|d| goast::Block {
                stmts: build_branch((**d).clone()),
            });
            vec![goast::Stmt::SwitchType {
                bind: Some(scrutinee_name),
                expr: compile_imm(env, scrutinee),
                cases,
                default: default_block,
            }]
        }
        _ => panic!("unsupported scrutinee type for match in Go backend"),
    }
}

fn compile_cexpr_effect(env: &Env, expr: &anf::CExpr) -> Vec<goast::Stmt> {
    match expr {
        anf::CExpr::CImm { .. }
        | anf::CExpr::EConstr { .. }
        | anf::CExpr::ETuple { .. }
        | anf::CExpr::EArray { .. }
        | anf::CExpr::EConstrGet { .. }
        | anf::CExpr::EProj { .. } => Vec::new(),
        anf::CExpr::ECall { func, args, .. } => {
            if let Some(spawn) = compile_spawn_call(env, func, args) {
                vec![spawn.stmt]
            } else {
                vec![goast::Stmt::Expr(compile_cexpr(env, expr))]
            }
        }
        anf::CExpr::EMatch { .. } | anf::CExpr::EIf { .. } | anf::CExpr::EWhile { .. } => {
            panic!("control-flow expressions should be handled before compile_cexpr_effect")
        }
    }
}

struct ClosureApplyFn {
    name: String,
    ty: tast::Ty,
    ret_ty: tast::Ty,
}

struct SpawnCompilation {
    stmt: goast::Stmt,
    result_ty: goty::GoType,
    result_expr: goast::Expr,
}

fn compile_spawn_call(
    env: &Env,
    func: &anf::ImmExpr,
    args: &[anf::ImmExpr],
) -> Option<SpawnCompilation> {
    let anf::ImmExpr::ImmVar { name, .. } = func else {
        return None;
    };
    if name != "spawn" {
        return None;
    }

    if args.len() != 1 {
        return None;
    }
    let closure_env = &args[0];

    let closure_ty = imm_ty(closure_env);
    let apply = find_closure_apply_fn(env, &closure_ty)?;

    let apply_call = anf::CExpr::ECall {
        func: anf::ImmExpr::ImmVar {
            name: apply.name.clone(),
            ty: apply.ty.clone(),
        },
        args: vec![closure_env.clone()],
        ty: apply.ret_ty.clone(),
    };

    let call_expr = compile_cexpr(env, &apply_call);
    Some(SpawnCompilation {
        stmt: goast::Stmt::Go { call: call_expr },
        result_ty: goty::GoType::TUnit,
        result_expr: goast::Expr::Unit {
            ty: goty::GoType::TUnit,
        },
    })
}

fn find_closure_apply_fn(env: &Env, closure_ty: &tast::Ty) -> Option<ClosureApplyFn> {
    let tast::Ty::TCon { name } = closure_ty else {
        return None;
    };

    let apply_name = env.closure_apply_fn(name)?;
    let fn_ty = env.funcs.get(apply_name)?;
    let tast::Ty::TFunc { params, ret_ty } = fn_ty else {
        return None;
    };

    if params.first()? != closure_ty {
        return None;
    }

    Some(ClosureApplyFn {
        name: apply_name.to_string(),
        ty: fn_ty.clone(),
        ret_ty: (**ret_ty).clone(),
    })
}

fn compile_aexpr_effect(env: &Env, e: anf::AExpr) -> Vec<goast::Stmt> {
    match e {
        AExpr::ACExpr { expr } => match expr {
            anf::CExpr::EIf {
                cond, then, else_, ..
            } => {
                let cond_e = compile_imm(env, &cond);
                let then_block = goast::Block {
                    stmts: compile_aexpr_effect(env, *then),
                };
                let else_block = goast::Block {
                    stmts: compile_aexpr_effect(env, *else_),
                };
                vec![goast::Stmt::If {
                    cond: cond_e,
                    then: then_block,
                    else_: Some(else_block),
                }]
            }
            anf::CExpr::EMatch {
                expr: scrutinee,
                arms,
                default,
                ty: _,
            } => compile_match_branches(env, scrutinee.as_ref(), &arms, &default, |branch| {
                compile_aexpr_effect(env, branch)
            }),
            anf::CExpr::EWhile { cond, body, .. } => compile_while(env, *cond, *body),
            other => compile_cexpr_effect(env, &other),
        },
        AExpr::ALet {
            name,
            value,
            body,
            ty: _,
        } => {
            let mut out = Vec::new();
            let value_expr = *value;

            if let anf::CExpr::ECall {
                ref func, ref args, ..
            } = value_expr
            {
                if let Some(spawn) = compile_spawn_call(env, func, args) {
                    out.push(spawn.stmt);
                    out.push(goast::Stmt::VarDecl {
                        name: name.clone(),
                        ty: spawn.result_ty.clone(),
                        value: Some(spawn.result_expr),
                    });
                    out.extend(compile_aexpr_effect(env, *body));
                    return out;
                }
            }

            match value_expr {
                complex @ (anf::CExpr::EIf { .. }
                | anf::CExpr::EMatch { .. }
                | anf::CExpr::EWhile { .. }) => {
                    out.push(goast::Stmt::VarDecl {
                        name: name.clone(),
                        ty: cexpr_ty(env, &complex),
                        value: None,
                    });
                    out.extend(compile_aexpr_assign(
                        env,
                        &name,
                        AExpr::ACExpr { expr: complex },
                    ));
                }
                simple => {
                    out.push(goast::Stmt::VarDecl {
                        name: name.clone(),
                        ty: cexpr_ty(env, &simple),
                        value: Some(compile_cexpr(env, &simple)),
                    });
                }
            }
            out.extend(compile_aexpr_effect(env, *body));
            out
        }
    }
}

fn compile_while(env: &Env, cond: anf::AExpr, body: anf::AExpr) -> Vec<goast::Stmt> {
    let cond_ty = cond.get_ty();
    if cond_ty != tast::Ty::TBool {
        panic!("while condition must have type bool, got {:?}", cond_ty);
    }

    let cond_var = env.gensym("cond");
    let mut stmts = Vec::new();
    stmts.push(goast::Stmt::VarDecl {
        name: cond_var.clone(),
        ty: goty::GoType::TBool,
        value: None,
    });

    let mut loop_body = compile_aexpr_assign(env, &cond_var, cond);
    let not_cond = goast::Expr::UnaryOp {
        op: goast::UnaryOp::Not,
        expr: Box::new(goast::Expr::Var {
            name: cond_var.clone(),
            ty: goty::GoType::TBool,
        }),
        ty: goty::GoType::TBool,
    };
    loop_body.push(goast::Stmt::If {
        cond: not_cond,
        then: goast::Block {
            stmts: vec![goast::Stmt::Break],
        },
        else_: None,
    });
    loop_body.extend(compile_aexpr_effect(env, body));

    stmts.push(goast::Stmt::Loop {
        body: goast::Block { stmts: loop_body },
    });
    stmts
}

fn compile_aexpr_assign(env: &Env, target: &str, e: anf::AExpr) -> Vec<goast::Stmt> {
    match e {
        AExpr::ACExpr { expr } => match expr {
            anf::CExpr::EIf {
                cond, then, else_, ..
            } => {
                let cond_e = compile_imm(env, &cond);
                let then_stmts = compile_aexpr_assign(env, target, *then);
                let else_stmts = compile_aexpr_assign(env, target, *else_);
                vec![goast::Stmt::If {
                    cond: cond_e,
                    then: goast::Block { stmts: then_stmts },
                    else_: Some(goast::Block { stmts: else_stmts }),
                }]
            }
            anf::CExpr::EMatch {
                expr: scrutinee,
                arms,
                default,
                ty: _,
            } => compile_match_branches(env, scrutinee.as_ref(), &arms, &default, |branch| {
                compile_aexpr_assign(env, target, branch)
            }),
            anf::CExpr::EWhile { cond, body, .. } => {
                let mut stmts = compile_while(env, *cond, *body);
                stmts.push(goast::Stmt::Assignment {
                    name: target.to_string(),
                    value: goast::Expr::Unit {
                        ty: goty::GoType::TUnit,
                    },
                });
                stmts
            }
            other @ (anf::CExpr::CImm { .. }
            | anf::CExpr::EConstr { .. }
            | anf::CExpr::EConstrGet { .. }
            | anf::CExpr::EProj { .. }
            | anf::CExpr::ETuple { .. }
            | anf::CExpr::EArray { .. }) => vec![goast::Stmt::Assignment {
                name: target.to_string(),
                value: compile_cexpr(env, &other),
            }],
            anf::CExpr::ECall { func, args, ty } => {
                if let Some(spawn) = compile_spawn_call(env, &func, &args) {
                    let mut stmts = Vec::new();
                    stmts.push(spawn.stmt);
                    stmts.push(goast::Stmt::Assignment {
                        name: target.to_string(),
                        value: spawn.result_expr,
                    });
                    stmts
                } else {
                    vec![goast::Stmt::Assignment {
                        name: target.to_string(),
                        value: compile_cexpr(env, &anf::CExpr::ECall { func, args, ty }),
                    }]
                }
            }
        },
        AExpr::ALet {
            name,
            value,
            body,
            ty: _,
        } => {
            let mut out = Vec::new();

            if let anf::CExpr::ECall {
                ref func, ref args, ..
            } = *value.clone()
            {
                if let Some(spawn) = compile_spawn_call(env, func, args) {
                    out.push(spawn.stmt);
                    out.push(goast::Stmt::VarDecl {
                        name: name.clone(),
                        ty: spawn.result_ty.clone(),
                        value: Some(spawn.result_expr),
                    });
                    out.extend(compile_aexpr_assign(env, target, *body));
                    return out;
                }
            }

            match &*value {
                // If RHS needs statements (if/match), first declare the variable,
                // then lower the RHS into assignments targeting that variable.
                anf::CExpr::EIf { .. } | anf::CExpr::EMatch { .. } | anf::CExpr::EWhile { .. } => {
                    out.push(goast::Stmt::VarDecl {
                        name: name.clone(),
                        ty: cexpr_ty(env, &value),
                        value: None,
                    });
                    out.extend(compile_aexpr_assign(
                        env,
                        &name,
                        AExpr::ACExpr {
                            expr: *value.clone(),
                        },
                    ));
                }
                _ => {
                    out.push(goast::Stmt::VarDecl {
                        name: name.clone(),
                        ty: cexpr_ty(env, &value),
                        value: Some(compile_cexpr(env, &value)),
                    });
                }
            }

            // Continue with body to finally assign into the target
            out.extend(compile_aexpr_assign(env, target, *body));
            out
        }
    }
}

fn compile_aexpr(env: &Env, e: anf::AExpr) -> Vec<goast::Stmt> {
    let mut stmts = Vec::new();
    match e {
        AExpr::ACExpr { expr } => match expr {
            // Lower conditional expressions to if-statements with returns in branches
            anf::CExpr::EIf {
                cond, then, else_, ..
            } => {
                let cond_e = compile_imm(env, &cond);
                let then_block = goast::Block {
                    stmts: compile_aexpr(env, *then),
                };
                let else_block = goast::Block {
                    stmts: compile_aexpr(env, *else_),
                };
                stmts.push(goast::Stmt::If {
                    cond: cond_e,
                    then: then_block,
                    else_: Some(else_block),
                });
            }
            anf::CExpr::EMatch {
                expr: scrutinee,
                arms,
                default,
                ty: _,
            } => {
                stmts.extend(compile_match_branches(
                    env,
                    scrutinee.as_ref(),
                    &arms,
                    &default,
                    |branch| compile_aexpr(env, branch),
                ));
            }
            anf::CExpr::EWhile { cond, body, .. } => {
                stmts.extend(compile_while(env, *cond, *body));
                stmts.push(goast::Stmt::Return {
                    expr: Some(goast::Expr::Unit {
                        ty: goty::GoType::TUnit,
                    }),
                });
            }
            _ => {
                let e = compile_cexpr(env, &expr);
                match e.get_ty() {
                    goty::GoType::TVoid => {}
                    _ => {
                        stmts.push(goast::Stmt::Return { expr: Some(e) });
                    }
                }
            }
        },
        AExpr::ALet {
            name,
            value,
            body,
            ty: _,
        } => {
            if let anf::CExpr::ECall {
                ref func, ref args, ..
            } = *value.clone()
            {
                if let Some(spawn) = compile_spawn_call(env, func, args) {
                    stmts.push(spawn.stmt);
                    stmts.push(goast::Stmt::VarDecl {
                        name: name.clone(),
                        ty: spawn.result_ty.clone(),
                        value: Some(spawn.result_expr),
                    });
                    stmts.extend(compile_aexpr(env, *body));
                    return stmts;
                }
            }

            match &*value {
                // If RHS needs statements, declare then fill via if-lowering
                anf::CExpr::EIf { .. } | anf::CExpr::EMatch { .. } | anf::CExpr::EWhile { .. } => {
                    stmts.push(goast::Stmt::VarDecl {
                        name: name.clone(),
                        ty: cexpr_ty(env, &value),
                        value: None,
                    });
                    stmts.extend(compile_aexpr_assign(
                        env,
                        &name,
                        AExpr::ACExpr {
                            expr: *value.clone(),
                        },
                    ));
                }
                anf::CExpr::ETuple { .. } => {
                    stmts.push(goast::Stmt::VarDecl {
                        name: name.clone(),
                        ty: cexpr_ty(env, &value),
                        value: Some(compile_cexpr(env, &value)),
                    });
                }
                _ => {
                    stmts.push(goast::Stmt::VarDecl {
                        name: name.clone(),
                        ty: cexpr_ty(env, &value),
                        value: Some(compile_cexpr(env, &value)),
                    });
                }
            }
            stmts.extend(compile_aexpr(env, *body));
        }
    }
    stmts
}

fn compile_fn(env: &Env, f: anf::Fn) -> goast::Fn {
    let mut params = Vec::new();
    for (name, ty) in f.params {
        params.push((name, tast_ty_to_go_type(&ty)));
    }

    let go_ret_ty = tast_ty_to_go_type(&f.ret_ty);

    let patched_name = if f.name == "main" {
        "main0".to_string()
    } else {
        f.name.clone()
    };

    let body = f.body;

    let (ret_ty, body_stmts) = match go_ret_ty {
        goty::GoType::TVoid => (None, compile_aexpr(env, body)),
        _ => {
            let ret_name = env.gensym("ret");
            let mut stmts = Vec::new();

            stmts.push(goast::Stmt::VarDecl {
                name: ret_name.clone(),
                ty: go_ret_ty.clone(),
                value: None,
            });

            stmts.extend(compile_aexpr_assign(env, &ret_name, body));

            stmts.push(goast::Stmt::Return {
                expr: Some(goast::Expr::Var {
                    name: ret_name,
                    ty: go_ret_ty.clone(),
                }),
            });

            (Some(go_ret_ty), stmts)
        }
    };

    goast::Fn {
        name: patched_name,
        params,
        ret_ty,
        body: goast::Block { stmts: body_stmts },
    }
}

pub fn go_file(env: &Env, file: anf::File) -> goast::File {
    let mut all = Vec::new();

    all.extend(runtime::make_runtime());
    all.extend(runtime::make_array_runtime(&env.array_types));
    all.extend(runtime::make_ref_runtime(&env.ref_types));

    if !env.extern_funcs.is_empty() {
        let mut existing_imports: IndexSet<String> = IndexSet::new();
        for item in &all {
            if let goast::Item::Import(import_decl) = item {
                for spec in &import_decl.specs {
                    existing_imports.insert(spec.path.clone());
                }
            }
        }

        let mut extra_specs = Vec::new();
        for extern_fn in env.extern_funcs.values() {
            if existing_imports.insert(extern_fn.package_path.clone()) {
                extra_specs.push(goast::ImportSpec {
                    alias: None,
                    path: extern_fn.package_path.clone(),
                });
            }
        }

        if !extra_specs.is_empty() {
            if let Some(import_decl) = all.iter_mut().find_map(|item| {
                if let goast::Item::Import(import_decl) = item {
                    Some(import_decl)
                } else {
                    None
                }
            }) {
                import_decl.specs.extend(extra_specs);
            } else {
                let insert_pos = if matches!(all.first(), Some(goast::Item::Package(_))) {
                    1
                } else {
                    0
                };
                all.insert(
                    insert_pos,
                    goast::Item::Import(goast::ImportDecl { specs: extra_specs }),
                );
            }
        }
    }

    for ty in env.tuple_types.iter() {
        match tuple_to_go_struct_type(env, ty) {
            goty::GoType::TStruct { name, fields } => {
                let fields = fields
                    .into_iter()
                    .map(|(field_name, field_ty)| goast::Field {
                        name: field_name,
                        ty: field_ty,
                    })
                    .collect();
                all.push(goast::Item::Struct(goast::Struct {
                    name,
                    fields,
                    methods: vec![],
                }));
            }
            other => panic!("expected struct type, got {:?}", other),
        }
    }

    let file = anf::anf_renamer::rename(file);

    let mut toplevels = gen_type_definition(env);
    for item in file.toplevels {
        let gof = compile_fn(env, item);
        toplevels.push(goast::Item::Fn(gof));
    }
    all.extend(toplevels);
    all.push(goast::Item::Fn(goast::Fn {
        name: "main".to_string(),
        params: vec![],
        ret_ty: None,
        body: goast::Block {
            stmts: vec![goast::Stmt::Expr(goast::Expr::Call {
                func: Box::new(goast::Expr::Var {
                    name: "main0".to_string(),
                    ty: goty::GoType::TFunc {
                        params: vec![],
                        ret_ty: Box::new(goty::GoType::TVoid),
                    },
                }),
                args: vec![],
                ty: goty::GoType::TVoid,
            })],
        },
    }));
    // Run a simple DCE pass to drop unused local variables for Go
    let file = goast::File { toplevels: all };
    crate::go::dce::eliminate_dead_vars(file)
}

fn gen_type_definition(env: &Env) -> Vec<goast::Item> {
    let mut defs = Vec::new();
    for (name, def) in env.structs.iter() {
        let has_type_param = name.0.contains("TParam")
            || !def.generics.is_empty()
            || def
                .fields
                .iter()
                .any(|(_, ty)| matches!(ty, tast::Ty::TParam { .. }));
        if has_type_param {
            continue;
        }

        let fields = def
            .fields
            .iter()
            .map(|(fname, fty)| goast::Field {
                name: fname.0.clone(),
                ty: tast_ty_to_go_type(fty),
            })
            .collect();
        defs.push(goast::Item::Struct(goast::Struct {
            name: name.0.clone(),
            fields,
            methods: vec![],
        }));
    }

    for (name, def) in env.enums.iter() {
        // Skip generating Go types for generic-specialized enums whose fields still contain type parameters
        let has_type_param = name.0.contains("TParam")
            || def
                .variants
                .iter()
                .any(|(_, fields)| fields.iter().any(|f| matches!(f, tast::Ty::TParam { .. })));
        if has_type_param {
            continue;
        }
        let type_identifier_method = format!("is{}", name.0);

        defs.push(goast::Item::Interface(goast::Interface {
            name: name.0.clone(),
            methods: vec![goast::MethodElem {
                name: type_identifier_method.clone(),
                params: vec![],
                ret: None,
            }],
        }));
        for (variant_name, variant_fields) in def.variants.iter() {
            let variant_name = variant_struct_name(env, &name.0, &variant_name.0);
            let mut fields = Vec::new();
            for (i, field) in variant_fields.iter().enumerate() {
                fields.push(goast::Field {
                    name: format!("_{}", i),
                    ty: tast_ty_to_go_type(field),
                });
            }

            let methods = vec![goast::Method {
                receiver: goast::Receiver {
                    name: "_".to_string(),
                    ty: goty::GoType::TName {
                        name: variant_name.clone(),
                    },
                },
                name: type_identifier_method.clone(),
                params: vec![],
                body: goast::Block { stmts: vec![] },
            }];

            defs.push(goast::Item::Struct(goast::Struct {
                name: variant_name,
                fields,
                methods,
            }));
        }
    }
    defs
}

#[test]
fn test_type_gen() {
    use crate::env::EnumDef;
    use expect_test::expect;

    let mut env = Env::new();
    env.enums.insert(
        Uident::new("Tree"),
        EnumDef {
            name: Uident::new("Tree"),
            generics: vec![],
            variants: vec![
                (Uident::new("Empty"), vec![]),
                (Uident::new("Leaf"), vec![tast::Ty::TInt]),
                (
                    Uident::new("Node"),
                    vec![
                        tast::Ty::TCon {
                            name: "Tree".to_string(),
                        },
                        tast::Ty::TCon {
                            name: "Tree".to_string(),
                        },
                    ],
                ),
            ],
        },
    );

    let item = gen_type_definition(&env);
    let dummy_file = goast::File { toplevels: item };
    expect![[r#"
        type Tree interface {
            isTree()
        }

        type Empty struct {}

        func (_ Empty) isTree() {}

        type Leaf struct {
            _0 int
        }

        func (_ Leaf) isTree() {}

        type Node struct {
            _0 Tree
            _1 Tree
        }

        func (_ Node) isTree() {}
    "#]]
    .assert_eq(&dummy_file.to_pretty(&env, 120));
}
