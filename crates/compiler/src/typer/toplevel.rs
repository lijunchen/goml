use std::collections::HashSet;

use ::ast::ast::{Ident, StructDef, TraitMethodSignature};
use ast::ast;
use diagnostics::{Severity, Stage};
use parser::{Diagnostic, Diagnostics};

use crate::{
    env::{self, GlobalTypeEnv, LocalTypeEnv},
    mangle::encode_ty,
    mangle::mangle_inherent_name,
    rename,
    tast::{self},
    typer::{
        Typer,
        util::{type_param_name_set, validate_ty},
    },
};

fn predeclare_types(genv: &mut GlobalTypeEnv, ast: &ast::File) {
    for item in ast.toplevels.iter() {
        match item {
            ast::Item::EnumDef(enum_def) => {
                genv.ensure_enum_placeholder(enum_def.name.clone(), enum_def.generics.clone());
            }
            ast::Item::StructDef(StructDef { name, generics, .. }) => {
                genv.ensure_struct_placeholder(name.clone(), generics.clone());
            }
            _ => {}
        }
    }
}

fn define_enum(genv: &mut GlobalTypeEnv, diagnostics: &mut Diagnostics, enum_def: &ast::EnumDef) {
    let params_env = &enum_def.generics;
    let tparam_names = type_param_name_set(params_env);

    let variants = enum_def
        .variants
        .iter()
        .map(|(vcon, typs)| {
            let typs = typs
                .iter()
                .map(|ast_ty| tast::Ty::from_ast(genv, ast_ty, params_env))
                .collect::<Vec<_>>();
            for ty in typs.iter() {
                validate_ty(genv, diagnostics, ty, &tparam_names);
            }
            (vcon.clone(), typs)
        })
        .collect();
    genv.insert_enum(env::EnumDef {
        name: enum_def.name.clone(),
        generics: enum_def.generics.clone(),
        variants,
    });
}

fn define_struct(
    genv: &mut GlobalTypeEnv,
    diagnostics: &mut Diagnostics,
    struct_def: &ast::StructDef,
) {
    let tparam_names = type_param_name_set(&struct_def.generics);
    let fields = struct_def
        .fields
        .iter()
        .map(|(fname, ast_ty)| {
            let ty = tast::Ty::from_ast(genv, ast_ty, &struct_def.generics);
            validate_ty(genv, diagnostics, &ty, &tparam_names);
            (fname.clone(), ty)
        })
        .collect();

    genv.insert_struct(env::StructDef {
        name: struct_def.name.clone(),
        generics: struct_def.generics.clone(),
        fields,
    });
}

fn define_trait(genv: &mut GlobalTypeEnv, trait_def: &ast::TraitDef) {
    for TraitMethodSignature {
        name: method_name,
        params,
        ret_ty,
    } in trait_def.method_sigs.iter()
    {
        genv.overloaded_funcs_to_trait_name
            .insert(method_name.0.clone(), trait_def.name.clone());

        let param_tys = params
            .iter()
            .map(|ast_ty| tast::Ty::from_ast(genv, ast_ty, &[]))
            .collect::<Vec<_>>();
        let ret_ty = tast::Ty::from_ast(genv, ret_ty, &[]);
        let fn_ty = tast::Ty::TFunc {
            params: param_tys,
            ret_ty: Box::new(ret_ty),
        };

        genv.trait_defs
            .insert((trait_def.name.0.clone(), method_name.0.clone()), fn_ty);
    }
}

fn define_trait_impl(
    genv: &mut GlobalTypeEnv,
    diagnostics: &mut Diagnostics,
    impl_block: &ast::ImplBlock,
    trait_name: &Ident,
) {
    let empty_tparams = HashSet::new();
    let for_ty = tast::Ty::from_ast(genv, &impl_block.for_type, &[]);
    validate_ty(genv, diagnostics, &for_ty, &empty_tparams);
    let trait_name_str = trait_name.0.clone();
    let trait_method_names: HashSet<String> = genv
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
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!(
                "Trait {} is not defined, cannot implement it for {:?}",
                trait_name_str, for_ty
            ),
        ));
        return;
    }

    let mut implemented_methods: HashSet<String> = HashSet::new();

    for m in impl_block.methods.iter() {
        let method_name = m.name.clone();
        let method_name_str = method_name.0.clone();

        if !trait_method_names.contains(&method_name_str) {
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Method {} is not declared in trait {}",
                    method_name_str, trait_name_str
                ),
            ));
            continue;
        }

        if !implemented_methods.insert(method_name_str.clone()) {
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Method {} implemented multiple times in impl of trait {}",
                    method_name_str, trait_name_str
                ),
            ));
            continue;
        }

        let trait_sig = genv
            .trait_defs
            .get(&(trait_name_str.clone(), method_name_str.clone()))
            .cloned()
            .expect("trait method signature to exist");

        let tparam_names = type_param_name_set(&m.generics);
        let params = m
            .params
            .iter()
            .map(|(_, ty)| {
                let ty = tast::Ty::from_ast(genv, ty, &m.generics);
                validate_ty(genv, diagnostics, &ty, &tparam_names);
                instantiate_self_ty(&ty, &for_ty)
            })
            .collect::<Vec<_>>();
        let ret = match &m.ret_ty {
            Some(ty) => {
                let ret = tast::Ty::from_ast(genv, ty, &m.generics);
                validate_ty(genv, diagnostics, &ret, &tparam_names);
                instantiate_self_ty(&ret, &for_ty)
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
                    diagnostics.push(Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Trait {}::{} expects {} parameters but impl has {}",
                            trait_name_str,
                            method_name_str,
                            expected_params.len(),
                            impl_params.len()
                        ),
                    ));
                    method_ok = false;
                }

                for (idx, (expected, actual)) in
                    expected_params.iter().zip(impl_params.iter()).enumerate()
                {
                    if expected != actual {
                        diagnostics.push(Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            format!(
                                "Trait {}::{} parameter {} expected type {:?} but found {:?}",
                                trait_name_str, method_name_str, idx, expected, actual
                            ),
                        ));
                        method_ok = false;
                    }
                }

                if **expected_ret != **impl_ret {
                    diagnostics.push(Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Trait {}::{} expected return type {:?} but found {:?}",
                            trait_name_str, method_name_str, expected_ret, impl_ret
                        ),
                    ));
                    method_ok = false;
                }
            }
            _ => {
                diagnostics.push(Diagnostic::new(
                    Stage::Typer,
                    Severity::Error,
                    format!(
                        "Trait {}::{} does not have a function type signature",
                        trait_name_str, method_name_str
                    ),
                ));
                method_ok = false;
            }
        }

        if method_ok {
            genv.trait_impls.insert(
                (trait_name_str.clone(), encode_ty(&for_ty), method_name),
                impl_method_ty,
            );
        }
    }

    for method_name in trait_method_names.iter() {
        if !implemented_methods.contains(method_name) {
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Trait {} implementation for {:?} is missing method {}",
                    trait_name_str, for_ty, method_name
                ),
            ));
        }
    }
}

fn define_inherent_impl(
    genv: &mut GlobalTypeEnv,
    diagnostics: &mut Diagnostics,
    impl_block: &ast::ImplBlock,
) {
    let empty_tparams = HashSet::new();
    let for_ty = tast::Ty::from_ast(genv, &impl_block.for_type, &[]);
    validate_ty(genv, diagnostics, &for_ty, &empty_tparams);

    let mut implemented_methods: HashSet<String> = HashSet::new();
    for m in impl_block.methods.iter() {
        let method_name = m.name.clone();
        let method_name_str = method_name.0.clone();

        if !implemented_methods.insert(method_name_str.clone()) {
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Method {} implemented multiple times in impl for {:?}",
                    method_name_str, for_ty
                ),
            ));
            continue;
        }

        let tparam_names = type_param_name_set(&m.generics);
        let params = m
            .params
            .iter()
            .map(|(_, ty)| {
                let ty = tast::Ty::from_ast(genv, ty, &m.generics);
                validate_ty(genv, diagnostics, &ty, &tparam_names);
                instantiate_self_ty(&ty, &for_ty)
            })
            .collect::<Vec<_>>();
        let ret = match &m.ret_ty {
            Some(ty) => {
                let ret = tast::Ty::from_ast(genv, ty, &m.generics);
                validate_ty(genv, diagnostics, &ret, &tparam_names);
                instantiate_self_ty(&ret, &for_ty)
            }
            None => tast::Ty::TUnit,
        };

        let impl_method_ty = tast::Ty::TFunc {
            params: params.clone(),
            ret_ty: Box::new(ret.clone()),
        };

        let mangled_name = mangle_inherent_name(&for_ty, &method_name_str);
        let key = (encode_ty(&for_ty), method_name.clone());
        genv.funcs
            .insert(mangled_name.clone(), impl_method_ty.clone());
        genv.inherent_impls
            .insert(key, (mangled_name, impl_method_ty));
    }
}

fn define_function(genv: &mut GlobalTypeEnv, diagnostics: &mut Diagnostics, func: &ast::Fn) {
    let name = func.name.clone();
    let tparam_names = type_param_name_set(&func.generics);
    let params = func
        .params
        .iter()
        .map(|(_, ty)| {
            let ty = tast::Ty::from_ast(genv, ty, &func.generics);
            validate_ty(genv, diagnostics, &ty, &tparam_names);
            ty
        })
        .collect::<Vec<_>>();
    let ret = match &func.ret_ty {
        Some(ty) => {
            let ret = tast::Ty::from_ast(genv, ty, &func.generics);
            validate_ty(genv, diagnostics, &ret, &tparam_names);
            ret
        }
        None => tast::Ty::TUnit,
    };
    genv.funcs.insert(
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

fn define_extern_go(genv: &mut GlobalTypeEnv, diagnostics: &mut Diagnostics, ext: &ast::ExternGo) {
    let params = ext
        .params
        .iter()
        .map(|(_, ty)| {
            let ty = tast::Ty::from_ast(genv, ty, &[]);
            validate_ty(genv, diagnostics, &ty, &HashSet::new());
            ty
        })
        .collect::<Vec<_>>();
    let ret = match &ext.ret_ty {
        Some(ty) => {
            let ret = tast::Ty::from_ast(genv, ty, &[]);
            validate_ty(genv, diagnostics, &ret, &HashSet::new());
            ret
        }
        None => tast::Ty::TUnit,
    };

    let fn_ty = tast::Ty::TFunc {
        params: params.clone(),
        ret_ty: Box::new(ret.clone()),
    };
    let go_name = go_symbol_name(&ext.go_symbol);
    genv.register_extern_function(
        ext.goml_name.0.clone(),
        ext.package_path.clone(),
        go_name,
        fn_ty,
    );
}

fn define_extern_type(
    genv: &mut GlobalTypeEnv,
    _diagnostics: &mut Diagnostics,
    ext: &ast::ExternType,
) {
    genv.register_extern_type(ext.goml_name.0.clone());
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

fn instantiate_trait_method_ty(ty: &tast::Ty, self_ty: &tast::Ty) -> tast::Ty {
    instantiate_self_ty(ty, self_ty)
}

pub fn collect_typedefs(genv: &mut GlobalTypeEnv, diagnostics: &mut Diagnostics, ast: &ast::File) {
    predeclare_types(genv, ast);

    for item in ast.toplevels.iter() {
        match item {
            ast::Item::EnumDef(enum_def) => define_enum(genv, diagnostics, enum_def),
            ast::Item::StructDef(struct_def) => define_struct(genv, diagnostics, struct_def),
            ast::Item::TraitDef(trait_def) => define_trait(genv, trait_def),
            ast::Item::ImplBlock(impl_block) => {
                if let Some(trait_name) = &impl_block.trait_name {
                    define_trait_impl(genv, diagnostics, impl_block, trait_name);
                } else {
                    define_inherent_impl(genv, diagnostics, impl_block);
                }
            }
            ast::Item::Fn(func) => define_function(genv, diagnostics, func),
            ast::Item::ExternGo(ext) => define_extern_go(genv, diagnostics, ext),
            ast::Item::ExternType(ext) => define_extern_type(genv, diagnostics, ext),
        }
    }
}

pub fn check_file(ast: ast::File) -> (tast::File, env::GlobalTypeEnv, Diagnostics) {
    let mut genv = env::GlobalTypeEnv::new();
    let ast = rename::Rename::default().rename_file(ast);
    let mut diagnostics = Diagnostics::new();
    collect_typedefs(&mut genv, &mut diagnostics, &ast);
    let mut typer = Typer::new();
    let mut typed_toplevel_tasts = vec![];
    for item in ast.toplevels.iter() {
        match item {
            ast::Item::EnumDef(..) => (),
            ast::Item::StructDef(..) => (),
            ast::Item::TraitDef(..) => (),
            ast::Item::ImplBlock(impl_block) => {
                typed_toplevel_tasts.push(tast::Item::ImplBlock(check_impl_block(
                    &genv,
                    &mut typer,
                    &mut diagnostics,
                    impl_block,
                )));
            }
            ast::Item::Fn(f) => {
                typed_toplevel_tasts.push(tast::Item::Fn(check_fn(
                    &genv,
                    &mut typer,
                    &mut diagnostics,
                    f,
                )));
            }
            ast::Item::ExternGo(ext) => {
                typed_toplevel_tasts.push(tast::Item::ExternGo(check_extern_go(
                    &genv,
                    &mut typer,
                    &mut diagnostics,
                    ext,
                )));
            }
            ast::Item::ExternType(ext) => {
                typed_toplevel_tasts.push(tast::Item::ExternType(check_extern_type(
                    &mut genv,
                    &mut diagnostics,
                    ext,
                )));
            }
        }
    }

    (
        tast::File {
            toplevels: typed_toplevel_tasts,
        },
        genv,
        diagnostics,
    )
}

fn check_fn(
    genv: &GlobalTypeEnv,
    typer: &mut Typer,
    diagnostics: &mut Diagnostics,
    f: &ast::Fn,
) -> tast::Fn {
    let mut local_env = LocalTypeEnv::new();
    let param_types: Vec<(Ident, tast::Ty)> = f
        .params
        .iter()
        .map(|(name, ty)| (name.clone(), tast::Ty::from_ast(genv, ty, &f.generics)))
        .collect();
    let new_params = param_types
        .iter()
        .map(|(name, ty)| (name.0.clone(), ty.clone()))
        .collect::<Vec<_>>();

    let ret_ty = match &f.ret_ty {
        Some(ty) => tast::Ty::from_ast(genv, ty, &f.generics),
        None => tast::Ty::TUnit,
    };

    local_env.set_tparams_env(&f.generics);
    local_env.push_scope();
    for (name, ty) in param_types.iter() {
        local_env.insert_var(name, ty.clone());
    }
    let typed_body = typer.check_expr(genv, &mut local_env, diagnostics, &f.body, &ret_ty);
    local_env.pop_scope();
    local_env.clear_tparams_env();
    typer.solve(genv, diagnostics);
    let typed_body = typer.subst(diagnostics, typed_body);
    tast::Fn {
        name: f.name.0.clone(),
        params: new_params,
        ret_ty,
        body: typed_body,
    }
}

fn check_impl_block(
    genv: &GlobalTypeEnv,
    typer: &mut Typer,
    diagnostics: &mut Diagnostics,
    impl_block: &ast::ImplBlock,
) -> tast::ImplBlock {
    let for_ty = tast::Ty::from_ast(genv, &impl_block.for_type, &[]);
    let mut typed_methods = Vec::new();
    for f in impl_block.methods.iter() {
        let mut local_env = LocalTypeEnv::new();
        let param_types: Vec<(Ident, tast::Ty)> = f
            .params
            .iter()
            .map(|(name, ty)| {
                let ty = tast::Ty::from_ast(genv, ty, &f.generics);
                let ty = instantiate_self_ty(&ty, &for_ty);
                (name.clone(), ty)
            })
            .collect();
        let new_params = param_types
            .iter()
            .map(|(name, ty)| (name.0.clone(), ty.clone()))
            .collect::<Vec<_>>();

        let ret_ty = match &f.ret_ty {
            Some(ty) => {
                let ty = tast::Ty::from_ast(genv, ty, &f.generics);
                instantiate_self_ty(&ty, &for_ty)
            }
            None => tast::Ty::TUnit,
        };

        local_env.set_tparams_env(&f.generics);
        local_env.push_scope();
        for (name, ty) in param_types.iter() {
            local_env.insert_var(name, ty.clone());
        }
        let typed_body = typer.check_expr(genv, &mut local_env, diagnostics, &f.body, &ret_ty);
        local_env.pop_scope();
        local_env.clear_tparams_env();
        typer.solve(genv, diagnostics);
        let typed_body = typer.subst(diagnostics, typed_body);
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
    genv: &GlobalTypeEnv,
    _typer: &mut Typer,
    _diagnostics: &mut Diagnostics,
    ext: &ast::ExternGo,
) -> tast::ExternGo {
    let params = ext
        .params
        .iter()
        .map(|(name, ty)| (name.0.clone(), tast::Ty::from_ast(genv, ty, &[])))
        .collect::<Vec<_>>();
    let ret_ty = match &ext.ret_ty {
        Some(ty) => tast::Ty::from_ast(genv, ty, &[]),
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

fn check_extern_type(
    genv: &mut GlobalTypeEnv,
    _diagnostics: &mut Diagnostics,
    ext: &ast::ExternType,
) -> tast::ExternType {
    genv.register_extern_type(ext.goml_name.0.clone());
    tast::ExternType {
        goml_name: ext.goml_name.0.clone(),
    }
}
