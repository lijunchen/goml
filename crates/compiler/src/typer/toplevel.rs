use std::collections::HashSet;

use ::ast::ast::{Lident, StructDef, TraitMethodSignature};
use ast::ast;

use crate::{
    env::{self, Env},
    rename,
    tast::{self},
    type_encoding::encode_ty,
    typer::{
        TypeInference,
        util::{ast_ty_to_tast_ty_with_tparams_env, type_param_name_set, validate_ty},
    },
};

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

pub fn go_symbol_name(name: &str) -> String {
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

fn define_extern_type(env: &mut Env, ext: &ast::ExternType) {
    env.register_extern_type(ext.goml_name.0.clone());
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
        tast::Ty::TRef { elem } => tast::Ty::TRef {
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

pub fn collect_typedefs(env: &mut Env, ast: &ast::File) {
    predeclare_types(env, ast);

    for item in ast.toplevels.iter() {
        match item {
            ast::Item::EnumDef(enum_def) => define_enum(env, enum_def),
            ast::Item::StructDef(struct_def) => define_struct(env, struct_def),
            ast::Item::TraitDef(trait_def) => define_trait(env, trait_def),
            ast::Item::ImplBlock(impl_block) => define_impl(env, impl_block),
            ast::Item::Fn(func) => define_function(env, func),
            ast::Item::ExternGo(ext) => define_extern_go(env, ext),
            ast::Item::ExternType(ext) => define_extern_type(env, ext),
        }
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
            ast::Item::ImplBlock(impl_block) => {
                typed_toplevel_tasts.push(tast::Item::ImplBlock(check_impl_block(
                    &mut env, &mut typer, impl_block,
                )));
            }
            ast::Item::Fn(f) => {
                typed_toplevel_tasts.push(tast::Item::Fn(check_fn(&mut env, &mut typer, f)));
            }
            ast::Item::ExternGo(ext) => {
                typed_toplevel_tasts.push(tast::Item::ExternGo(check_extern_go(
                    &mut env, &mut typer, ext,
                )));
            }
            ast::Item::ExternType(ext) => {
                typed_toplevel_tasts.push(tast::Item::ExternType(check_extern_type(&mut env, ext)));
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

fn check_fn(env: &mut Env, typer: &mut TypeInference, f: &ast::Fn) -> tast::Fn {
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
        typer.check_expr(env, &vars, &f.body, &ret_ty)
    });
    typer.solve(env);
    let typed_body = typer.subst(env, typed_body);
    tast::Fn {
        name: f.name.0.clone(),
        params: new_params,
        ret_ty,
        body: typed_body,
    }
}

fn check_impl_block(
    env: &mut Env,
    typer: &mut TypeInference,
    impl_block: &ast::ImplBlock,
) -> tast::ImplBlock {
    let for_ty = ast_ty_to_tast_ty_with_tparams_env(&impl_block.for_type, &[]);
    let mut typed_methods = Vec::new();
    for f in impl_block.methods.iter() {
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
            typer.check_expr(env, &vars, &f.body, &ret_ty)
        });
        typer.solve(env);
        let typed_body = typer.subst(env, typed_body);
        typed_methods.push(tast::Fn {
            name: f.name.0.clone(),
            params: new_params,
            ret_ty,
            body: typed_body,
        });
    }
    let trait_name = impl_block.trait_name.clone();
    tast::ImplBlock {
        trait_name,
        for_type: for_ty,
        methods: typed_methods,
    }
}

fn check_extern_go(
    _env: &mut Env,
    _typer: &mut TypeInference,
    ext: &ast::ExternGo,
) -> tast::ExternGo {
    let params = ext
        .params
        .iter()
        .map(|(name, ty)| (name.0.clone(), ast_ty_to_tast_ty_with_tparams_env(ty, &[])))
        .collect::<Vec<_>>();
    let ret_ty = match &ext.ret_ty {
        Some(ty) => ast_ty_to_tast_ty_with_tparams_env(ty, &[]),
        None => tast::Ty::TUnit,
    };
    tast::ExternGo {
        goml_name: ext.goml_name.0.clone(),
        go_name: go_symbol_name(&ext.go_symbol),
        package_path: ext.package_path.clone(),
        params,
        ret_ty,
    }
}

fn check_extern_type(env: &mut Env, ext: &ast::ExternType) -> tast::ExternType {
    env.register_extern_type(ext.goml_name.0.clone());
    tast::ExternType {
        goml_name: ext.goml_name.0.clone(),
    }
}
