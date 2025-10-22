use std::collections::{HashMap, HashSet};

use ::ast::ast::{Lident, StructDef, TraitMethodSignature};
use ast::ast;
use ena::unify::InPlaceUnificationTable;

use crate::{
    env::{self, Constraint, Env},
    rename,
    tast::{self, TypeVar},
    type_encoding::encode_ty,
};

fn binary_supports_builtin(op: ast::BinaryOp, lhs: &tast::Ty, rhs: &tast::Ty) -> bool {
    match op {
        ast::BinaryOp::Add => matches!(
            (lhs, rhs),
            (tast::Ty::TInt, tast::Ty::TInt) | (tast::Ty::TString, tast::Ty::TString)
        ),
        ast::BinaryOp::Sub | ast::BinaryOp::Mul | ast::BinaryOp::Div => {
            matches!((lhs, rhs), (tast::Ty::TInt, tast::Ty::TInt))
        }
        ast::BinaryOp::And | ast::BinaryOp::Or => {
            matches!((lhs, rhs), (tast::Ty::TBool, tast::Ty::TBool))
        }
    }
}

fn go_symbol_name(name: &str) -> String {
    let mut result = String::new();
    let mut uppercase_next = true;
    for ch in name.chars() {
        if ch == '_' {
            uppercase_next = true;
            continue;
        }
        if uppercase_next {
            for upper in ch.to_uppercase() {
                result.push(upper);
            }
            uppercase_next = false;
        } else {
            result.push(ch);
        }
    }
    if result.is_empty() {
        name.to_string()
    } else {
        result
    }
}

pub fn check_file(ast: ast::File) -> (tast::File, env::Env) {
    let mut env = env::Env::new();
    let ast = rename::Rename::default().rename_file(ast);
    collect_typedefs(&mut env, &ast);
    let mut typer = TypeInference::new();
    let mut typed_toplevel_tasts = vec![];
    for item in ast.toplevels.iter() {
        match item {
            ast::Item::EnumDef(..) => (),
            ast::Item::StructDef(..) => (),
            ast::Item::TraitDef(..) => (),
            ast::Item::ImplBlock(impl_blcok) => {
                let for_ty = ast_ty_to_tast_ty_with_tparams_env(&impl_blcok.for_type, &[]);
                let mut typed_methods = Vec::new();
                for f in impl_blcok.methods.iter() {
                    let mut vars = im::HashMap::<Lident, tast::Ty>::new();
                    for (name, ty) in f.params.iter() {
                        let ty = ast_ty_to_tast_ty_with_tparams_env(ty, &f.generics);
                        vars.insert(name.clone(), ty);
                    }
                    let new_params = f
                        .params
                        .iter()
                        .map(|(name, ty)| {
                            (
                                name.0.clone(),
                                ast_ty_to_tast_ty_with_tparams_env(ty, &f.generics),
                            )
                        })
                        .collect::<Vec<_>>();

                    let ret_ty = match &f.ret_ty {
                        Some(ty) => ast_ty_to_tast_ty_with_tparams_env(ty, &f.generics),
                        None => tast::Ty::TUnit,
                    };

                    let typed_body = typer.with_tparams_env(&f.generics, |typer| {
                        typer.infer(&mut env, &vars, &f.body)
                    });
                    typer.solve(&mut env);
                    let typed_body = typer.subst(&mut env, typed_body);
                    typed_methods.push(tast::Fn {
                        name: f.name.0.clone(),
                        params: new_params,
                        ret_ty,
                        body: typed_body,
                    });
                }
                let trait_name = impl_blcok.trait_name.clone();
                let trait_impl = tast::ImplBlock {
                    trait_name,
                    for_type: for_ty,
                    methods: typed_methods,
                };
                typed_toplevel_tasts.push(tast::Item::ImplBlock(trait_impl));
            }
            ast::Item::Fn(f) => {
                let mut vars = im::HashMap::<Lident, tast::Ty>::new();
                for (name, ty) in f.params.iter() {
                    let ty = ast_ty_to_tast_ty_with_tparams_env(ty, &f.generics);
                    vars.insert(name.clone(), ty);
                }
                let new_params = f
                    .params
                    .iter()
                    .map(|(name, ty)| {
                        (
                            name.0.clone(),
                            ast_ty_to_tast_ty_with_tparams_env(ty, &f.generics),
                        )
                    })
                    .collect::<Vec<_>>();

                let ret_ty = match &f.ret_ty {
                    Some(ty) => ast_ty_to_tast_ty_with_tparams_env(ty, &f.generics),
                    None => tast::Ty::TUnit,
                };

                let typed_body = typer
                    .with_tparams_env(&f.generics, |typer| typer.infer(&mut env, &vars, &f.body));
                typer.solve(&mut env);
                let typed_body = typer.subst(&mut env, typed_body);
                typed_toplevel_tasts.push(tast::Item::Fn(tast::Fn {
                    name: f.name.0.clone(),
                    params: new_params,
                    ret_ty,
                    body: typed_body,
                }));
            }
            ast::Item::ExternGo(ext) => {
                let params = ext
                    .params
                    .iter()
                    .map(|(name, ty)| (name.0.clone(), ast_ty_to_tast_ty_with_tparams_env(ty, &[])))
                    .collect::<Vec<_>>();
                let ret_ty = match &ext.ret_ty {
                    Some(ty) => ast_ty_to_tast_ty_with_tparams_env(ty, &[]),
                    None => tast::Ty::TUnit,
                };
                typed_toplevel_tasts.push(tast::Item::ExternGo(tast::ExternGo {
                    goml_name: ext.goml_name.0.clone(),
                    go_name: go_symbol_name(&ext.go_symbol),
                    package_path: ext.package_path.clone(),
                    params,
                    ret_ty,
                }));
            }
        }
    }

    (
        tast::File {
            toplevels: typed_toplevel_tasts,
        },
        env,
    )
}

fn type_param_name_set(tparams: &[ast::Uident]) -> HashSet<String> {
    tparams.iter().map(|param| param.0.clone()).collect()
}

fn validate_ty(env: &mut Env, ty: &tast::Ty, tparams: &HashSet<String>) {
    match ty {
        tast::Ty::TVar(_) => {}
        tast::Ty::TUnit | tast::Ty::TBool | tast::Ty::TInt | tast::Ty::TString => {}
        tast::Ty::TTuple { typs } => {
            for ty in typs.iter() {
                validate_ty(env, ty, tparams);
            }
        }
        tast::Ty::TFunc { params, ret_ty } => {
            for param in params.iter() {
                validate_ty(env, param, tparams);
            }
            validate_ty(env, ret_ty.as_ref(), tparams);
        }
        tast::Ty::TParam { name } => {
            if !tparams.contains(name) {
                env.report_typer_error(format!("Unbound type parameter {}", name));
            }
        }
        tast::Ty::TCon { name } => {
            if name == "Self" {
                return;
            }
            let ident = ast::Uident::new(name);
            if let Some(enum_def) = env.enums.get(&ident) {
                if !enum_def.generics.is_empty() {
                    env.report_typer_error(format!(
                        "Type {} expects {} type arguments, but got 0",
                        name,
                        enum_def.generics.len()
                    ));
                }
            } else if let Some(struct_def) = env.structs.get(&ident) {
                if !struct_def.generics.is_empty() {
                    env.report_typer_error(format!(
                        "Type {} expects {} type arguments, but got 0",
                        name,
                        struct_def.generics.len()
                    ));
                }
            } else {
                env.report_typer_error(format!("Unknown type constructor {}", name));
            }
        }
        tast::Ty::TApp { ty, args } => {
            for arg in args.iter() {
                validate_ty(env, arg, tparams);
            }

            let base_name = ty.get_constr_name_unsafe();
            let ident = ast::Uident::new(&base_name);

            if let Some(enum_def) = env.enums.get(&ident) {
                let expected = enum_def.generics.len();
                let actual = args.len();
                if expected != actual {
                    env.report_typer_error(format!(
                        "Type {} expects {} type arguments, but got {}",
                        base_name, expected, actual
                    ));
                }
            } else if let Some(struct_def) = env.structs.get(&ident) {
                let expected = struct_def.generics.len();
                let actual = args.len();
                if expected != actual {
                    env.report_typer_error(format!(
                        "Type {} expects {} type arguments, but got {}",
                        base_name, expected, actual
                    ));
                }
            } else {
                env.report_typer_error(format!("Unknown type constructor {}", base_name));
            }
        }
        tast::Ty::TArray { elem, .. } => {
            validate_ty(env, elem, tparams);
        }
    }
}

fn predeclare_types(env: &mut Env, ast: &ast::File) {
    for item in ast.toplevels.iter() {
        match item {
            ast::Item::EnumDef(enum_def) => {
                env.enums
                    .entry(enum_def.name.clone())
                    .or_insert_with(|| env::EnumDef {
                        name: enum_def.name.clone(),
                        generics: enum_def.generics.clone(),
                        variants: Vec::new(), // empty body
                    });
            }
            ast::Item::StructDef(StructDef { name, generics, .. }) => {
                env.structs
                    .entry(name.clone())
                    .or_insert_with(|| env::StructDef {
                        name: name.clone(),
                        generics: generics.clone(),
                        fields: Vec::new(), // empty body
                    });
            }
            _ => {}
        }
    }
}

fn define_enum(env: &mut Env, enum_def: &ast::EnumDef) {
    let params_env = &enum_def.generics;
    let tparam_names = type_param_name_set(params_env);

    let variants = enum_def
        .variants
        .iter()
        .map(|(vcon, typs)| {
            let typs = typs
                .iter()
                .map(|ast_ty| ast_ty_to_tast_ty_with_tparams_env(ast_ty, params_env))
                .collect::<Vec<_>>();
            for ty in typs.iter() {
                validate_ty(env, ty, &tparam_names);
            }
            (vcon.clone(), typs)
        })
        .collect();
    env.enums.insert(
        enum_def.name.clone(),
        env::EnumDef {
            name: enum_def.name.clone(),
            generics: enum_def.generics.clone(),
            variants,
        },
    );
}

fn define_struct(env: &mut Env, struct_def: &ast::StructDef) {
    let tparam_names = type_param_name_set(&struct_def.generics);
    let fields = struct_def
        .fields
        .iter()
        .map(|(fname, ast_ty)| {
            let ty = ast_ty_to_tast_ty_with_tparams_env(ast_ty, &struct_def.generics);
            validate_ty(env, &ty, &tparam_names);
            (fname.clone(), ty)
        })
        .collect();

    env.structs.insert(
        struct_def.name.clone(),
        env::StructDef {
            name: struct_def.name.clone(),
            generics: struct_def.generics.clone(),
            fields,
        },
    );
}

fn define_trait(env: &mut Env, trait_def: &ast::TraitDef) {
    for TraitMethodSignature {
        name: method_name,
        params,
        ret_ty,
    } in trait_def.method_sigs.iter()
    {
        env.overloaded_funcs_to_trait_name
            .insert(method_name.0.clone(), trait_def.name.clone());

        let param_tys = params
            .iter()
            .map(|ast_ty| ast_ty_to_tast_ty_with_tparams_env(ast_ty, &[]))
            .collect::<Vec<_>>();
        let ret_ty = ast_ty_to_tast_ty_with_tparams_env(ret_ty, &[]);
        let fn_ty = tast::Ty::TFunc {
            params: param_tys,
            ret_ty: Box::new(ret_ty),
        };

        env.trait_defs
            .insert((trait_def.name.0.clone(), method_name.0.clone()), fn_ty);
    }
}

fn define_impl(env: &mut Env, impl_block: &ast::ImplBlock) {
    let empty_tparams = HashSet::new();
    let for_ty = ast_ty_to_tast_ty_with_tparams_env(&impl_block.for_type, &[]);
    validate_ty(env, &for_ty, &empty_tparams);
    let trait_name_str = impl_block.trait_name.0.clone();
    let trait_method_names: HashSet<String> = env
        .trait_defs
        .keys()
        .filter_map(|(t_name, method_name)| {
            if t_name == &trait_name_str {
                Some(method_name.clone())
            } else {
                None
            }
        })
        .collect();

    if trait_method_names.is_empty() {
        env.report_typer_error(format!(
            "Trait {} is not defined, cannot implement it for {:?}",
            trait_name_str, for_ty
        ));
        return;
    }

    let mut implemented_methods: HashSet<String> = HashSet::new();

    for m in impl_block.methods.iter() {
        let method_name = m.name.clone();
        let method_name_str = method_name.0.clone();

        if !trait_method_names.contains(&method_name_str) {
            env.report_typer_error(format!(
                "Method {} is not declared in trait {}",
                method_name_str, trait_name_str
            ));
            continue;
        }

        if !implemented_methods.insert(method_name_str.clone()) {
            env.report_typer_error(format!(
                "Method {} implemented multiple times in impl of trait {}",
                method_name_str, trait_name_str
            ));
            continue;
        }

        let trait_sig = env
            .trait_defs
            .get(&(trait_name_str.clone(), method_name_str.clone()))
            .cloned()
            .expect("trait method signature to exist");

        let tparam_names = type_param_name_set(&m.generics);
        let params = m
            .params
            .iter()
            .map(|(_, ty)| {
                let ty = ast_ty_to_tast_ty_with_tparams_env(ty, &m.generics);
                validate_ty(env, &ty, &tparam_names);
                ty
            })
            .collect::<Vec<_>>();
        let ret = match &m.ret_ty {
            Some(ty) => {
                let ret = ast_ty_to_tast_ty_with_tparams_env(ty, &m.generics);
                validate_ty(env, &ret, &tparam_names);
                ret
            }
            None => tast::Ty::TUnit,
        };

        let impl_method_ty = tast::Ty::TFunc {
            params: params.clone(),
            ret_ty: Box::new(ret.clone()),
        };

        let expected_method_ty = instantiate_trait_method_ty(&trait_sig, &for_ty);

        let mut method_ok = true;
        match (&expected_method_ty, &impl_method_ty) {
            (
                tast::Ty::TFunc {
                    params: expected_params,
                    ret_ty: expected_ret,
                },
                tast::Ty::TFunc {
                    params: impl_params,
                    ret_ty: impl_ret,
                },
            ) => {
                if expected_params.len() != impl_params.len() {
                    env.report_typer_error(format!(
                        "Trait {}::{} expects {} parameters but impl has {}",
                        trait_name_str,
                        method_name_str,
                        expected_params.len(),
                        impl_params.len()
                    ));
                    method_ok = false;
                }

                for (idx, (expected, actual)) in
                    expected_params.iter().zip(impl_params.iter()).enumerate()
                {
                    if expected != actual {
                        env.report_typer_error(format!(
                            "Trait {}::{} parameter {} expected type {:?} but found {:?}",
                            trait_name_str, method_name_str, idx, expected, actual
                        ));
                        method_ok = false;
                    }
                }

                if **expected_ret != **impl_ret {
                    env.report_typer_error(format!(
                        "Trait {}::{} expected return type {:?} but found {:?}",
                        trait_name_str, method_name_str, expected_ret, impl_ret
                    ));
                    method_ok = false;
                }
            }
            _ => {
                env.report_typer_error(format!(
                    "Trait {}::{} does not have a function type signature",
                    trait_name_str, method_name_str
                ));
                method_ok = false;
            }
        }

        if method_ok {
            env.trait_impls.insert(
                (trait_name_str.clone(), encode_ty(&for_ty), method_name),
                impl_method_ty,
            );
        }
    }

    for method_name in trait_method_names.iter() {
        if !implemented_methods.contains(method_name) {
            env.report_typer_error(format!(
                "Trait {} implementation for {:?} is missing method {}",
                trait_name_str, for_ty, method_name
            ));
        }
    }
}

fn define_function(env: &mut Env, func: &ast::Fn) {
    let name = func.name.clone();
    let tparam_names = type_param_name_set(&func.generics);
    let params = func
        .params
        .iter()
        .map(|(_, ty)| {
            let ty = ast_ty_to_tast_ty_with_tparams_env(ty, &func.generics);
            validate_ty(env, &ty, &tparam_names);
            ty
        })
        .collect::<Vec<_>>();
    let ret = match &func.ret_ty {
        Some(ty) => {
            let ret = ast_ty_to_tast_ty_with_tparams_env(ty, &func.generics);
            validate_ty(env, &ret, &tparam_names);
            ret
        }
        None => tast::Ty::TUnit,
    };
    env.funcs.insert(
        name.0.clone(),
        tast::Ty::TFunc {
            params,
            ret_ty: Box::new(ret),
        },
    );
}

fn define_extern_go(env: &mut Env, ext: &ast::ExternGo) {
    let params = ext
        .params
        .iter()
        .map(|(_, ty)| {
            let ty = ast_ty_to_tast_ty_with_tparams_env(ty, &[]);
            validate_ty(env, &ty, &HashSet::new());
            ty
        })
        .collect::<Vec<_>>();
    let ret = match &ext.ret_ty {
        Some(ty) => {
            let ret = ast_ty_to_tast_ty_with_tparams_env(ty, &[]);
            validate_ty(env, &ret, &HashSet::new());
            ret
        }
        None => tast::Ty::TUnit,
    };

    let fn_ty = tast::Ty::TFunc {
        params: params.clone(),
        ret_ty: Box::new(ret.clone()),
    };
    let go_name = go_symbol_name(&ext.go_symbol);
    env.register_extern_function(
        ext.goml_name.0.clone(),
        ext.package_path.clone(),
        go_name,
        fn_ty,
    );
}

fn collect_typedefs(env: &mut Env, ast: &ast::File) {
    predeclare_types(env, ast);

    for item in ast.toplevels.iter() {
        match item {
            ast::Item::EnumDef(enum_def) => define_enum(env, enum_def),
            ast::Item::StructDef(struct_def) => define_struct(env, struct_def),
            ast::Item::TraitDef(trait_def) => define_trait(env, trait_def),
            ast::Item::ImplBlock(impl_block) => define_impl(env, impl_block),
            ast::Item::Fn(func) => define_function(env, func),
            ast::Item::ExternGo(ext) => define_extern_go(env, ext),
        }
    }
}

fn instantiate_trait_method_ty(ty: &tast::Ty, self_ty: &tast::Ty) -> tast::Ty {
    match ty {
        tast::Ty::TVar(var) => tast::Ty::TVar(*var),
        tast::Ty::TUnit => tast::Ty::TUnit,
        tast::Ty::TBool => tast::Ty::TBool,
        tast::Ty::TInt => tast::Ty::TInt,
        tast::Ty::TString => tast::Ty::TString,
        tast::Ty::TTuple { typs } => tast::Ty::TTuple {
            typs: typs
                .iter()
                .map(|ty| instantiate_trait_method_ty(ty, self_ty))
                .collect(),
        },
        tast::Ty::TCon { name } => {
            if name == "Self" {
                self_ty.clone()
            } else {
                tast::Ty::TCon { name: name.clone() }
            }
        }
        tast::Ty::TApp { ty, args } => tast::Ty::TApp {
            ty: Box::new(instantiate_trait_method_ty(ty, self_ty)),
            args: args
                .iter()
                .map(|ty| instantiate_trait_method_ty(ty, self_ty))
                .collect(),
        },
        tast::Ty::TArray { len, elem } => tast::Ty::TArray {
            len: *len,
            elem: Box::new(instantiate_trait_method_ty(elem, self_ty)),
        },
        tast::Ty::TParam { name } => tast::Ty::TParam { name: name.clone() },
        tast::Ty::TFunc { params, ret_ty } => tast::Ty::TFunc {
            params: params
                .iter()
                .map(|param| instantiate_trait_method_ty(param, self_ty))
                .collect(),
            ret_ty: Box::new(instantiate_trait_method_ty(ret_ty, self_ty)),
        },
    }
}

pub fn ast_ty_to_tast_ty_with_tparams_env(
    ast_ty: &ast::Ty,
    tparams_env: &[ast::Uident],
) -> tast::Ty {
    match ast_ty {
        ast::Ty::TUnit => tast::Ty::TUnit,
        ast::Ty::TBool => tast::Ty::TBool,
        ast::Ty::TInt => tast::Ty::TInt,
        ast::Ty::TString => tast::Ty::TString,
        ast::Ty::TTuple { typs } => {
            let typs = typs
                .iter()
                .map(|ty| ast_ty_to_tast_ty_with_tparams_env(ty, tparams_env))
                .collect();
            tast::Ty::TTuple { typs }
        }
        ast::Ty::TCon { name } => {
            if tparams_env.iter().any(|param| param.0 == *name) {
                tast::Ty::TParam { name: name.clone() }
            } else {
                tast::Ty::TCon { name: name.clone() }
            }
        }
        ast::Ty::TApp { ty, args } => {
            let ty = ast_ty_to_tast_ty_with_tparams_env(ty, tparams_env);
            let args = args
                .iter()
                .map(|ty| ast_ty_to_tast_ty_with_tparams_env(ty, tparams_env))
                .collect();
            tast::Ty::TApp {
                ty: Box::new(ty),
                args,
            }
        }
        ast::Ty::TArray { len, elem } => tast::Ty::TArray {
            len: *len,
            elem: Box::new(ast_ty_to_tast_ty_with_tparams_env(elem, tparams_env)),
        },
        ast::Ty::TFunc { params, ret_ty } => tast::Ty::TFunc {
            params: params
                .iter()
                .map(|ty| ast_ty_to_tast_ty_with_tparams_env(ty, tparams_env))
                .collect(),
            ret_ty: Box::new(ast_ty_to_tast_ty_with_tparams_env(ret_ty, tparams_env)),
        },
    }
}

pub struct TypeInference {
    pub uni: InPlaceUnificationTable<TypeVar>,
    tparams_env_stack: Vec<Vec<ast::Uident>>,
}

impl Default for TypeInference {
    fn default() -> Self {
        Self::new()
    }
}

fn occurs(env: &mut Env, var: TypeVar, ty: &tast::Ty) -> bool {
    match ty {
        tast::Ty::TVar(v) => {
            if var == *v {
                env.report_typer_error(format!(
                    "occurs check failed: {:?} occurs in {:?}",
                    var, ty
                ));
                return false;
            }
        }
        tast::Ty::TUnit
        | tast::Ty::TBool
        | tast::Ty::TInt
        | tast::Ty::TString
        | tast::Ty::TParam { .. } => {}
        tast::Ty::TTuple { typs } => {
            for ty in typs.iter() {
                if !occurs(env, var, ty) {
                    return false;
                }
            }
        }
        tast::Ty::TCon { .. } => {}
        tast::Ty::TApp { ty, args } => {
            if !occurs(env, var, ty.as_ref()) {
                return false;
            }
            for arg in args.iter() {
                if !occurs(env, var, arg) {
                    return false;
                }
            }
        }
        tast::Ty::TArray { elem, .. } => {
            if !occurs(env, var, elem) {
                return false;
            }
        }
        tast::Ty::TFunc { params, ret_ty } => {
            for param in params.iter() {
                if !occurs(env, var, param) {
                    return false;
                }
            }
            if !occurs(env, var, ret_ty) {
                return false;
            }
        }
    }

    true
}

impl TypeInference {
    pub fn solve(&mut self, env: &mut Env) {
        let mut constraints = env.constraints.clone();
        let mut changed = true;

        fn is_concrete(norm_ty: &tast::Ty) -> bool {
            match norm_ty {
                tast::Ty::TVar(..) => false,
                tast::Ty::TUnit
                | tast::Ty::TBool
                | tast::Ty::TInt
                | tast::Ty::TString
                | tast::Ty::TParam { .. } => true, // TParam is treated as concrete here
                tast::Ty::TTuple { typs } => typs.iter().all(is_concrete),
                tast::Ty::TCon { .. } => true,
                tast::Ty::TApp { ty, args } => {
                    is_concrete(ty.as_ref()) && args.iter().all(is_concrete)
                }
                tast::Ty::TArray { elem, .. } => is_concrete(elem),
                tast::Ty::TFunc { params, ret_ty } => {
                    params.iter().all(is_concrete) && is_concrete(ret_ty)
                }
            }
        }

        while changed {
            changed = false;
            let mut still_pending = Vec::new();
            for constraint in constraints.drain(..) {
                match constraint {
                    Constraint::TypeEqual(l, r) => {
                        if self.unify(env, &l, &r) {
                            changed = true;
                        }
                    }
                    Constraint::Overloaded {
                        op,
                        trait_name,
                        call_site_type,
                    } => {
                        let norm_call_site_type = self.norm(&call_site_type);
                        if let tast::Ty::TFunc {
                            params: norm_arg_types,
                            ret_ty: norm_ret_ty,
                        } = norm_call_site_type
                        {
                            if let Some(self_ty) = norm_arg_types.first() {
                                match self_ty {
                                    ty if is_concrete(ty) => {
                                        match env.get_trait_impl(&trait_name, self_ty, &op) {
                                            Some(impl_scheme) => {
                                                let impl_fun_ty = self.inst_ty(&impl_scheme);

                                                let call_fun_ty = tast::Ty::TFunc {
                                                    params: norm_arg_types.clone(),
                                                    ret_ty: norm_ret_ty.clone(),
                                                };

                                                still_pending.push(Constraint::TypeEqual(
                                                    call_fun_ty,
                                                    impl_fun_ty,
                                                ));

                                                // Made progress!
                                                changed = true;
                                            }
                                            None => {
                                                env.report_typer_error(format!(
                                                    "No instance found for trait {}<{:?}> for operator {}",
                                                    trait_name.0, ty, op.0
                                                ));
                                            }
                                        }
                                    }
                                    tast::Ty::TVar(_) => {
                                        // We cannot resolve this yet. Defer it.
                                        still_pending.push(Constraint::Overloaded {
                                            op,
                                            trait_name,
                                            call_site_type, // Push original back
                                        });
                                    }
                                    _ => {
                                        env.report_typer_error(format!(
                                            "Overload resolution failed for non-concrete, non-variable type {:?}",
                                            self_ty
                                        ));
                                    }
                                }
                            } else {
                                env.report_typer_error(format!(
                                    "Overloaded operator {} called with no arguments?",
                                    op.0
                                ));
                            }
                        } else {
                            env.report_typer_error(format!(
                                "Overloaded constraint does not involve a function type: {:?}",
                                norm_call_site_type
                            ));
                        }
                    }
                }
            }
            constraints.extend(still_pending);
            if !changed && !constraints.is_empty() {
                env.report_typer_error(format!(
                    "Could not solve all constraints: {:?}",
                    constraints
                ));
                break;
            }
        }
        if !constraints.is_empty() {
            env.report_typer_error(format!(
                "Type inference failed, remaining constraints: {:?}",
                constraints
            ));
        }
    }

    fn norm(&mut self, ty: &tast::Ty) -> tast::Ty {
        match ty {
            tast::Ty::TVar(v) => {
                if let Some(value) = self.uni.probe_value(*v) {
                    self.norm(&value)
                } else {
                    tast::Ty::TVar(self.uni.find(*v))
                }
            }
            tast::Ty::TUnit => tast::Ty::TUnit,
            tast::Ty::TBool => tast::Ty::TBool,
            tast::Ty::TInt => tast::Ty::TInt,
            tast::Ty::TString => tast::Ty::TString,
            tast::Ty::TTuple { typs } => {
                let typs = typs.iter().map(|ty| self.norm(ty)).collect();
                tast::Ty::TTuple { typs }
            }
            tast::Ty::TCon { name } => tast::Ty::TCon { name: name.clone() },
            tast::Ty::TApp { ty, args } => tast::Ty::TApp {
                ty: Box::new(self.norm(ty)),
                args: args.iter().map(|ty| self.norm(ty)).collect(),
            },
            tast::Ty::TArray { len, elem } => tast::Ty::TArray {
                len: *len,
                elem: Box::new(self.norm(elem)),
            },
            tast::Ty::TFunc { params, ret_ty } => {
                let params = params.iter().map(|ty| self.norm(ty)).collect();
                let ret_ty = Box::new(self.norm(ret_ty));
                tast::Ty::TFunc { params, ret_ty }
            }
            tast::Ty::TParam { name } => tast::Ty::TParam { name: name.clone() },
        }
    }

    fn unify(&mut self, env: &mut Env, l: &tast::Ty, r: &tast::Ty) -> bool {
        let l_norm = self.norm(l);
        let r_norm = self.norm(r);
        match (&l_norm, &r_norm) {
            (tast::Ty::TVar(a), tast::Ty::TVar(b)) => {
                if self.uni.unify_var_var(*a, *b).is_err() {
                    env.report_typer_error(format!(
                        "Failed to unify type variables {:?} and {:?}",
                        a, b
                    ));
                    return false;
                }
            }
            (tast::Ty::TVar(a), t) | (t, tast::Ty::TVar(a)) => {
                if !occurs(env, *a, t) {
                    return false;
                }
                if self.uni.unify_var_value(*a, Some(t.clone())).is_err() {
                    env.report_typer_error(format!(
                        "Failed to unify type variable {:?} with {:?}",
                        a, t
                    ));
                    return false;
                }
            }

            (tast::Ty::TUnit, tast::Ty::TUnit) => {}
            (tast::Ty::TBool, tast::Ty::TBool) => {}
            (tast::Ty::TInt, tast::Ty::TInt) => {}
            (tast::Ty::TString, tast::Ty::TString) => {}
            (tast::Ty::TTuple { typs: typs1 }, tast::Ty::TTuple { typs: typs2 }) => {
                if typs1.len() != typs2.len() {
                    env.report_typer_error(format!(
                        "Tuple types have different lengths: {:?} and {:?}",
                        l, r
                    ));
                    return false;
                }
                for (ty1, ty2) in typs1.iter().zip(typs2.iter()) {
                    if !self.unify(env, ty1, ty2) {
                        return false;
                    }
                }
            }
            (
                tast::Ty::TArray {
                    len: len1,
                    elem: elem1,
                },
                tast::Ty::TArray {
                    len: len2,
                    elem: elem2,
                },
            ) => {
                let wildcard = tast::ARRAY_WILDCARD_LEN;
                if len1 != len2 && *len1 != wildcard && *len2 != wildcard {
                    env.report_typer_error(format!(
                        "Array types have different lengths: {:?} and {:?}",
                        l, r
                    ));
                    return false;
                }
                if !self.unify(env, elem1, elem2) {
                    return false;
                }
            }
            (
                tast::Ty::TFunc {
                    params: param1,
                    ret_ty: ret_ty1,
                },
                tast::Ty::TFunc {
                    params: param2,
                    ret_ty: ret_ty2,
                },
            ) => {
                if param1.len() != param2.len() {
                    env.report_typer_error(format!(
                        "Function types have different parameter lengths: {:?} and {:?}",
                        l, r
                    ));
                    return false;
                }
                for (p1, p2) in param1.iter().zip(param2.iter()) {
                    if !self.unify(env, p1, p2) {
                        return false;
                    }
                }
                if !self.unify(env, ret_ty1, ret_ty2) {
                    return false;
                }
            }
            (tast::Ty::TCon { name: n1 }, tast::Ty::TCon { name: n2 }) => {
                if n1 != n2 {
                    env.report_typer_error(format!(
                        "Constructor types are different: {:?} and {:?}",
                        l, r
                    ));
                    return false;
                }
            }
            (
                tast::Ty::TApp {
                    ty: ty1,
                    args: args1,
                },
                tast::Ty::TApp {
                    ty: ty2,
                    args: args2,
                },
            ) => {
                if args1.len() != args2.len() {
                    env.report_typer_error(format!(
                        "Constructor types have different argument lengths: {:?} and {:?}",
                        l, r
                    ));
                    return false;
                }
                if !self.unify(env, ty1.as_ref(), ty2.as_ref()) {
                    return false;
                }
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    if !self.unify(env, arg1, arg2) {
                        return false;
                    }
                }
            }
            (tast::Ty::TParam { name }, tast::Ty::TParam { name: name2 }) => {
                if name != name2 {
                    env.report_typer_error(format!(
                        "Type parameters are different: {:?} and {:?}",
                        l, r
                    ));
                    return false;
                }
            }
            _ => {
                env.report_typer_error(format!("type not equal {:?} and {:?}", l_norm, r_norm));
                return false;
            }
        }
        true
    }
}

impl TypeInference {
    pub fn new() -> Self {
        Self {
            uni: InPlaceUnificationTable::new(),
            tparams_env_stack: Vec::new(),
        }
    }

    fn push_tparams_env(&mut self, tparams: &[ast::Uident]) {
        self.tparams_env_stack.push(tparams.to_vec());
    }

    fn pop_tparams_env(&mut self) {
        self.tparams_env_stack.pop();
    }

    fn current_tparams_env(&self) -> Vec<ast::Uident> {
        self.tparams_env_stack
            .iter()
            .flat_map(|env| env.iter().cloned())
            .collect()
    }

    fn with_tparams_env<F, R>(&mut self, tparams: &[ast::Uident], f: F) -> R
    where
        F: FnOnce(&mut TypeInference) -> R,
    {
        self.push_tparams_env(tparams);
        let result = f(self);
        self.pop_tparams_env();
        result
    }

    fn fresh_ty_var(&mut self) -> tast::Ty {
        tast::Ty::TVar(self.uni.new_key(None))
    }

    fn inst_ty(&mut self, ty: &tast::Ty) -> tast::Ty {
        let mut subst: HashMap<String, tast::Ty> = HashMap::new();
        self._go_inst_ty(ty, &mut subst)
    }

    fn _go_inst_ty(&mut self, ty: &tast::Ty, subst: &mut HashMap<String, tast::Ty>) -> tast::Ty {
        match ty {
            tast::Ty::TVar(_) => ty.clone(),
            tast::Ty::TUnit => ty.clone(),
            tast::Ty::TBool => ty.clone(),
            tast::Ty::TInt => ty.clone(),
            tast::Ty::TString => ty.clone(),
            tast::Ty::TTuple { typs } => {
                let typs = typs
                    .iter()
                    .map(|ty| self._go_inst_ty(ty, subst))
                    .collect::<Vec<_>>();
                tast::Ty::TTuple { typs }
            }
            tast::Ty::TCon { name } => tast::Ty::TCon { name: name.clone() },
            tast::Ty::TApp { ty, args } => {
                let ty = self._go_inst_ty(ty, subst);
                let args = args
                    .iter()
                    .map(|arg| self._go_inst_ty(arg, subst))
                    .collect::<Vec<_>>();
                tast::Ty::TApp {
                    ty: Box::new(ty),
                    args,
                }
            }
            tast::Ty::TArray { len, elem } => tast::Ty::TArray {
                len: *len,
                elem: Box::new(self._go_inst_ty(elem, subst)),
            },
            tast::Ty::TParam { name } => {
                if subst.contains_key(name) {
                    let ty = subst.get(name).unwrap();
                    ty.clone()
                } else {
                    let new_ty = self.fresh_ty_var();
                    subst.insert(name.clone(), new_ty.clone());
                    new_ty
                }
            }
            tast::Ty::TFunc { params, ret_ty } => {
                let params = params
                    .iter()
                    .map(|ty| self._go_inst_ty(ty, subst))
                    .collect::<Vec<_>>();
                let ret_ty = Box::new(self._go_inst_ty(ret_ty, subst));
                tast::Ty::TFunc { params, ret_ty }
            }
        }
    }

    fn subst_ty(&mut self, env: &mut Env, ty: &tast::Ty) -> tast::Ty {
        match ty {
            tast::Ty::TVar(v) => {
                if let Some(value) = self.uni.probe_value(*v) {
                    self.subst_ty(env, &value)
                } else {
                    env.report_typer_error(format!("Type variable {:?} not resolved", v));
                    tast::Ty::TVar(*v)
                }
            }
            tast::Ty::TUnit => tast::Ty::TUnit,
            tast::Ty::TBool => tast::Ty::TBool,
            tast::Ty::TInt => tast::Ty::TInt,
            tast::Ty::TString => tast::Ty::TString,
            tast::Ty::TTuple { typs } => {
                let typs = typs.iter().map(|ty| self.subst_ty(env, ty)).collect();
                tast::Ty::TTuple { typs }
            }
            tast::Ty::TCon { name } => tast::Ty::TCon { name: name.clone() },
            tast::Ty::TApp { ty, args } => tast::Ty::TApp {
                ty: Box::new(self.subst_ty(env, ty)),
                args: args.iter().map(|arg| self.subst_ty(env, arg)).collect(),
            },
            tast::Ty::TArray { len, elem } => tast::Ty::TArray {
                len: *len,
                elem: Box::new(self.subst_ty(env, elem)),
            },
            tast::Ty::TFunc { params, ret_ty } => {
                let params = params.iter().map(|ty| self.subst_ty(env, ty)).collect();
                let ret_ty = Box::new(self.subst_ty(env, ret_ty));
                tast::Ty::TFunc { params, ret_ty }
            }
            tast::Ty::TParam { name } => tast::Ty::TParam { name: name.clone() },
        }
    }

    fn subst_pat(&mut self, env: &mut Env, p: tast::Pat) -> tast::Pat {
        match p {
            tast::Pat::PVar { name, ty, astptr } => {
                let ty = self.subst_ty(env, &ty);
                tast::Pat::PVar {
                    name: name.clone(),
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Pat::PUnit { ty } => {
                let ty = self.subst_ty(env, &ty);
                tast::Pat::PUnit { ty: ty.clone() }
            }
            tast::Pat::PBool { value, ty } => {
                let ty = self.subst_ty(env, &ty);
                tast::Pat::PBool {
                    value,
                    ty: ty.clone(),
                }
            }
            tast::Pat::PInt { value, ty } => {
                let ty = self.subst_ty(env, &ty);
                tast::Pat::PInt {
                    value,
                    ty: ty.clone(),
                }
            }
            tast::Pat::PString { value, ty } => {
                let ty = self.subst_ty(env, &ty);
                tast::Pat::PString {
                    value,
                    ty: ty.clone(),
                }
            }
            tast::Pat::PConstr {
                constructor,
                args,
                ty,
            } => {
                let ty = self.subst_ty(env, &ty);
                let args = args
                    .into_iter()
                    .map(|arg| self.subst_pat(env, arg))
                    .collect::<Vec<_>>();
                tast::Pat::PConstr {
                    constructor,
                    args,
                    ty: ty.clone(),
                }
            }
            tast::Pat::PTuple { items, ty } => {
                let ty = self.subst_ty(env, &ty);
                let items = items
                    .into_iter()
                    .map(|item| self.subst_pat(env, item))
                    .collect::<Vec<_>>();
                tast::Pat::PTuple {
                    items,
                    ty: ty.clone(),
                }
            }
            tast::Pat::PWild { ty } => {
                let ty = self.subst_ty(env, &ty);
                tast::Pat::PWild { ty: ty.clone() }
            }
        }
    }

    pub fn subst(&mut self, env: &mut Env, e: tast::Expr) -> tast::Expr {
        match e {
            tast::Expr::EVar { name, ty, astptr } => {
                let ty = self.subst_ty(env, &ty);
                tast::Expr::EVar {
                    name,
                    ty: ty.clone(),
                    astptr,
                }
            }
            tast::Expr::EUnit { ty } => {
                let ty = self.subst_ty(env, &ty);
                tast::Expr::EUnit { ty: ty.clone() }
            }
            tast::Expr::EBool { value, ty } => {
                let ty = self.subst_ty(env, &ty);
                tast::Expr::EBool {
                    value,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EInt { value, ty } => {
                let ty = self.subst_ty(env, &ty);
                tast::Expr::EInt {
                    value,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EString { value, ty } => {
                let ty = self.subst_ty(env, &ty);
                tast::Expr::EString {
                    value: value.clone(),
                    ty: ty.clone(),
                }
            }
            tast::Expr::EConstr {
                constructor,
                args,
                ty,
            } => {
                let ty = self.subst_ty(env, &ty);
                let args = args
                    .into_iter()
                    .map(|arg| self.subst(env, arg))
                    .collect::<Vec<_>>();
                tast::Expr::EConstr {
                    constructor,
                    args,
                    ty,
                }
            }
            tast::Expr::ETuple { items, ty } => {
                let ty = self.subst_ty(env, &ty);
                let items = items
                    .into_iter()
                    .map(|item| self.subst(env, item))
                    .collect::<Vec<_>>();
                tast::Expr::ETuple {
                    items,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EArray { items, ty } => {
                let ty = self.subst_ty(env, &ty);
                let items = items
                    .into_iter()
                    .map(|item| self.subst(env, item))
                    .collect::<Vec<_>>();
                tast::Expr::EArray {
                    items,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EClosure { params, body, ty } => {
                let ty = self.subst_ty(env, &ty);
                let params = params
                    .into_iter()
                    .map(|param| tast::ClosureParam {
                        name: param.name,
                        ty: self.subst_ty(env, &param.ty),
                        astptr: param.astptr,
                    })
                    .collect();
                let body = Box::new(self.subst(env, *body));
                tast::Expr::EClosure {
                    params,
                    body,
                    ty: ty.clone(),
                }
            }
            tast::Expr::ELet {
                pat,
                value,
                body,
                ty,
            } => {
                let ty = self.subst_ty(env, &ty);
                let pat = self.subst_pat(env, pat);
                let value = Box::new(self.subst(env, *value));
                let body = Box::new(self.subst(env, *body));
                tast::Expr::ELet {
                    pat,
                    value,
                    body,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EMatch { expr, arms, ty } => {
                let ty = self.subst_ty(env, &ty);
                let expr = Box::new(self.subst(env, *expr));
                let arms = arms
                    .into_iter()
                    .map(|arm| tast::Arm {
                        pat: self.subst_pat(env, arm.pat),
                        body: self.subst(env, arm.body),
                    })
                    .collect::<Vec<_>>();
                tast::Expr::EMatch {
                    expr,
                    arms,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EIf {
                cond,
                then_branch,
                else_branch,
                ty,
            } => {
                let ty = self.subst_ty(env, &ty);
                let cond = Box::new(self.subst(env, *cond));
                let then_branch = Box::new(self.subst(env, *then_branch));
                let else_branch = Box::new(self.subst(env, *else_branch));
                tast::Expr::EIf {
                    cond,
                    then_branch,
                    else_branch,
                    ty: ty.clone(),
                }
            }
            tast::Expr::ECall { func, args, ty } => {
                let ty = self.subst_ty(env, &ty);
                let func = Box::new(self.subst(env, *func));
                let args = args
                    .into_iter()
                    .map(|arg| self.subst(env, arg))
                    .collect::<Vec<_>>();
                tast::Expr::ECall {
                    func,
                    args,
                    ty: ty.clone(),
                }
            }
            tast::Expr::EUnary {
                op,
                expr,
                ty,
                resolution,
            } => {
                let ty = self.subst_ty(env, &ty);
                let expr = Box::new(self.subst(env, *expr));
                tast::Expr::EUnary {
                    op,
                    expr,
                    ty: ty.clone(),
                    resolution,
                }
            }
            tast::Expr::EBinary {
                op,
                lhs,
                rhs,
                ty,
                resolution,
            } => {
                let ty = self.subst_ty(env, &ty);
                let lhs = Box::new(self.subst(env, *lhs));
                let rhs = Box::new(self.subst(env, *rhs));
                tast::Expr::EBinary {
                    op,
                    lhs,
                    rhs,
                    ty: ty.clone(),
                    resolution,
                }
            }
            tast::Expr::EProj { tuple, index, ty } => {
                let ty = self.subst_ty(env, &ty);
                let tuple = Box::new(self.subst(env, *tuple));
                tast::Expr::EProj {
                    tuple,
                    index,
                    ty: ty.clone(),
                }
            }
        }
    }

    pub fn infer(
        &mut self,
        env: &mut Env,
        vars: &im::HashMap<Lident, tast::Ty>,
        e: &ast::Expr,
    ) -> tast::Expr {
        let current_tparams_env = self.current_tparams_env();
        match e {
            ast::Expr::EVar { name, astptr } => {
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
            ast::Expr::EConstr { vcon, args } => {
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
                    let arg_tast = self.infer(env, vars, arg);
                    args_tast.push(arg_tast.clone());
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
            ast::Expr::EStructLiteral { name, fields } => {
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
                    let field_expr = self.infer(env, vars, expr);
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
            ast::Expr::ETuple { items } => {
                let mut typs = Vec::new();
                let mut items_tast = Vec::new();
                for item in items.iter() {
                    let item_tast = self.infer(env, vars, item);
                    items_tast.push(item_tast.clone());
                    typs.push(item_tast.get_ty());
                }
                tast::Expr::ETuple {
                    items: items_tast,
                    ty: tast::Ty::TTuple { typs },
                }
            }
            ast::Expr::EArray { items } => {
                let len = items.len();
                let elem_ty = self.fresh_ty_var();
                let mut items_tast = Vec::with_capacity(len);
                for item in items.iter() {
                    let item_tast = self.infer(env, vars, item);
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
            ast::Expr::EClosure { params, body } => {
                let mut closure_vars = vars.clone();
                let mut params_tast = Vec::new();
                let mut param_tys = Vec::new();

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

                let body_tast = self.infer(env, &closure_vars, body);
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
            ast::Expr::ELet { pat, value, body } => {
                let value_tast = self.infer(env, vars, value);
                let value_ty = value_tast.get_ty();

                let mut new_vars = vars.clone();
                let pat_tast = self.check_pat(env, &mut new_vars, pat, &value_ty);

                let body_tast = self.infer(env, &new_vars, body);
                let body_ty = body_tast.get_ty();
                tast::Expr::ELet {
                    pat: pat_tast,
                    value: Box::new(value_tast),
                    body: Box::new(body_tast),
                    ty: body_ty.clone(),
                }
            }
            ast::Expr::EMatch { expr, arms } => {
                let expr_tast = self.infer(env, vars, expr);
                let expr_ty = expr_tast.get_ty();

                let mut arms_tast = Vec::new();
                let arm_ty = self.fresh_ty_var();
                for arm in arms.iter() {
                    let mut new_vars = vars.clone();
                    let arm_tast = self.check_pat(env, &mut new_vars, &arm.pat, &expr_ty);
                    let arm_body_tast = self.infer(env, &new_vars, &arm.body);
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
            ast::Expr::EIf {
                cond,
                then_branch,
                else_branch,
            } => {
                let cond_tast = self.infer(env, vars, cond);
                env.constraints
                    .push(Constraint::TypeEqual(cond_tast.get_ty(), tast::Ty::TBool));

                let then_tast = self.infer(env, vars, then_branch);
                let else_tast = self.infer(env, vars, else_branch);
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
            ast::Expr::ECall { func, args } => {
                let mut args_tast = Vec::new();
                let mut arg_types = Vec::new();
                for arg in args.iter() {
                    let arg_tast = self.infer(env, vars, arg);
                    arg_types.push(arg_tast.get_ty());
                    args_tast.push(arg_tast);
                }

                let ret_ty = self.fresh_ty_var();
                let call_site_func_ty = tast::Ty::TFunc {
                    params: arg_types,
                    ret_ty: Box::new(ret_ty.clone()),
                };

                match func.as_ref() {
                    ast::Expr::EVar { name, astptr } => {
                        if let Some(func_ty) = env.get_type_of_function(&name.0) {
                            let inst_func_ty = self.inst_ty(&func_ty);
                            env.constraints.push(Constraint::TypeEqual(
                                inst_func_ty.clone(),
                                call_site_func_ty.clone(),
                            ));

                            tast::Expr::ECall {
                                func: Box::new(tast::Expr::EVar {
                                    name: name.0.clone(),
                                    ty: inst_func_ty,
                                    astptr: Some(*astptr),
                                }),
                                args: args_tast,
                                ty: ret_ty,
                            }
                        } else if let Some(trait_name) =
                            env.overloaded_funcs_to_trait_name.get(&name.0).cloned()
                        {
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
                            let func_tast = self.infer(env, vars, func);
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
                        let func_tast = self.infer(env, vars, func);
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
            ast::Expr::EUnary { op, expr } => {
                let expr_tast = self.infer(env, vars, expr);
                let expr_ty = expr_tast.get_ty();
                let method_name = op.method_name();

                if let Some(trait_name) =
                    env.overloaded_funcs_to_trait_name.get(method_name).cloned()
                {
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
                        op: *op,
                        expr: Box::new(expr_tast),
                        ty: ret_ty,
                        resolution: tast::UnaryResolution::Overloaded { trait_name },
                    }
                } else {
                    match op {
                        ast::UnaryOp::Neg => {
                            env.constraints
                                .push(Constraint::TypeEqual(expr_ty.clone(), tast::Ty::TInt));
                            tast::Expr::EUnary {
                                op: *op,
                                expr: Box::new(expr_tast),
                                ty: tast::Ty::TInt,
                                resolution: tast::UnaryResolution::Builtin,
                            }
                        }
                        ast::UnaryOp::Not => {
                            env.constraints
                                .push(Constraint::TypeEqual(expr_ty.clone(), tast::Ty::TBool));
                            tast::Expr::EUnary {
                                op: *op,
                                expr: Box::new(expr_tast),
                                ty: tast::Ty::TBool,
                                resolution: tast::UnaryResolution::Builtin,
                            }
                        }
                    }
                }
            }
            ast::Expr::EBinary { op, lhs, rhs } => {
                let lhs_tast = self.infer(env, vars, lhs);
                let rhs_tast = self.infer(env, vars, rhs);
                let lhs_ty = lhs_tast.get_ty();
                let rhs_ty = rhs_tast.get_ty();
                let method_name = op.method_name();

                if let Some(trait_name) =
                    env.overloaded_funcs_to_trait_name.get(method_name).cloned()
                    && !binary_supports_builtin(*op, &lhs_ty, &rhs_ty)
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
                        op: *op,
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
                }

                tast::Expr::EBinary {
                    op: *op,
                    lhs: Box::new(lhs_tast),
                    rhs: Box::new(rhs_tast),
                    ty: ret_ty.clone(),
                    resolution: tast::BinaryResolution::Builtin,
                }
            }
            ast::Expr::EProj { tuple, index } => {
                let tuple_tast = self.infer(env, vars, tuple);
                let tuple_ty = tuple_tast.get_ty();
                match &tuple_ty {
                    tast::Ty::TTuple { typs } => {
                        let field_ty = typs.get(*index).cloned().unwrap_or_else(|| {
                            panic!(
                                "Tuple index {} out of bounds for type {:?}",
                                index, tuple_ty
                            )
                        });
                        tast::Expr::EProj {
                            tuple: Box::new(tuple_tast),
                            index: *index,
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
                            index: *index,
                            ty: ret_ty,
                        }
                    }
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
            ast::Pat::PVar { name, astptr } => {
                vars.insert(name.clone(), ty.clone());
                tast::Pat::PVar {
                    name: name.0.clone(),
                    ty: ty.clone(),
                    astptr: Some(*astptr),
                }
            }
            ast::Pat::PUnit => tast::Pat::PUnit {
                ty: tast::Ty::TUnit,
            },
            ast::Pat::PBool { value } => tast::Pat::PBool {
                value: *value,
                ty: tast::Ty::TBool,
            },
            ast::Pat::PInt { value } => {
                env.constraints
                    .push(Constraint::TypeEqual(tast::Ty::TInt, ty.clone()));
                tast::Pat::PInt {
                    value: *value,
                    ty: tast::Ty::TInt,
                }
            }
            ast::Pat::PString { value } => {
                env.constraints
                    .push(Constraint::TypeEqual(tast::Ty::TString, ty.clone()));
                tast::Pat::PString {
                    value: value.clone(),
                    ty: tast::Ty::TString,
                }
            }
            ast::Pat::PConstr { .. } => {
                let tast = self.infer_pat(env, vars, pat);
                env.constraints
                    .push(Constraint::TypeEqual(tast.get_ty(), ty.clone()));
                tast
            }
            ast::Pat::PStruct { .. } => {
                let tast = self.infer_pat(env, vars, pat);
                env.constraints
                    .push(Constraint::TypeEqual(tast.get_ty(), ty.clone()));
                tast
            }
            ast::Pat::PTuple { pats } => {
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
            ast::Pat::PWild => {
                let pat_ty = self.fresh_ty_var();
                env.constraints
                    .push(Constraint::TypeEqual(pat_ty.clone(), ty.clone()));
                tast::Pat::PWild { ty: pat_ty }
            }
        }
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
