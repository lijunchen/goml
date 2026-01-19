use std::collections::{HashMap, HashSet};

use diagnostics::{Severity, Stage};
use indexmap::IndexMap;
use parser::{Diagnostic, Diagnostics};

use crate::{
    env::{self, FnOrigin, FnScheme, GlobalTypeEnv, PackageTypeEnv},
    fir::{self},
    tast::{self},
    typer::{
        Typer,
        localenv::LocalTypeEnv,
        name_resolution,
        util::{type_param_name_set, validate_ty},
    },
};

fn predeclare_types(genv: &mut GlobalTypeEnv, fir: &fir::PackageFir, fir_table: &fir::FirTable) {
    for item in fir.toplevels.iter() {
        match fir_table.def(*item) {
            fir::Def::EnumDef(enum_def) => {
                genv.ensure_enum_placeholder(
                    tast::TastIdent(enum_def.name.to_ident_name()),
                    enum_def
                        .generics
                        .iter()
                        .map(|i| tast::TastIdent(i.to_ident_name()))
                        .collect(),
                );
            }
            fir::Def::StructDef(fir::StructDef { name, generics, .. }) => {
                genv.ensure_struct_placeholder(
                    tast::TastIdent(name.to_ident_name()),
                    generics
                        .iter()
                        .map(|i| tast::TastIdent(i.to_ident_name()))
                        .collect(),
                );
            }
            _ => {}
        }
    }
}

fn define_enum(env: &mut PackageTypeEnv, diagnostics: &mut Diagnostics, enum_def: &fir::EnumDef) {
    let params_env: Vec<tast::TastIdent> = enum_def
        .generics
        .iter()
        .map(|i| tast::TastIdent(i.to_ident_name()))
        .collect();
    let tparam_names = type_param_name_set(&enum_def.generics);

    let variants = enum_def
        .variants
        .iter()
        .map(|(vcon, typs)| {
            let typs = typs
                .iter()
                .map(|ast_ty| tast::Ty::from_fir(env, ast_ty, &params_env))
                .collect::<Vec<_>>();
            for ty in typs.iter() {
                validate_ty(env, diagnostics, ty, &tparam_names);
            }
            (tast::TastIdent(vcon.to_ident_name()), typs)
        })
        .collect();
    env.current_mut().insert_enum(env::EnumDef {
        name: tast::TastIdent(enum_def.name.to_ident_name()),
        generics: enum_def
            .generics
            .iter()
            .map(|i| tast::TastIdent(i.to_ident_name()))
            .collect(),
        variants,
    });
}

fn define_struct(
    env: &mut PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    struct_def: &fir::StructDef,
) {
    let params_env: Vec<tast::TastIdent> = struct_def
        .generics
        .iter()
        .map(|i| tast::TastIdent(i.to_ident_name()))
        .collect();
    let tparam_names = type_param_name_set(&struct_def.generics);
    let fields = struct_def
        .fields
        .iter()
        .map(|(fname, ast_ty)| {
            let ty = tast::Ty::from_fir(env, ast_ty, &params_env);
            validate_ty(env, diagnostics, &ty, &tparam_names);
            (tast::TastIdent(fname.to_ident_name()), ty)
        })
        .collect();

    env.current_mut().insert_struct(env::StructDef {
        name: tast::TastIdent(struct_def.name.to_ident_name()),
        generics: struct_def
            .generics
            .iter()
            .map(|i| tast::TastIdent(i.to_ident_name()))
            .collect(),
        fields,
    });
}

fn define_trait(env: &mut PackageTypeEnv, trait_def: &fir::TraitDef) {
    let mut methods = IndexMap::new();

    for fir::TraitMethodSignature {
        name: method_name,
        params,
        ret_ty,
    } in trait_def.method_sigs.iter()
    {
        let param_tys = params
            .iter()
            .map(|ast_ty| tast::Ty::from_fir(env, ast_ty, &[]))
            .collect::<Vec<_>>();
        let ret_ty = tast::Ty::from_fir(env, ret_ty, &[]);
        let fn_ty = tast::Ty::TFunc {
            params: param_tys,
            ret_ty: Box::new(ret_ty),
        };

        methods.insert(
            method_name.to_ident_name(),
            FnScheme {
                type_params: vec![],
                constraints: (),
                ty: fn_ty,
                origin: FnOrigin::User,
            },
        );
    }

    env.current_mut()
        .trait_env
        .trait_defs
        .insert(trait_def.name.to_ident_name(), env::TraitDef { methods });
}

fn is_local_name(current_package: &str, name: &str) -> bool {
    if let Some((package, _)) = name.split_once("::") {
        package == current_package
    } else {
        current_package == "Main" || current_package == "Builtin"
    }
}

fn is_local_nominal_type(current_package: &str, ty: &tast::Ty) -> bool {
    match ty {
        tast::Ty::TStruct { name } | tast::Ty::TEnum { name } => {
            is_local_name(current_package, name)
        }
        tast::Ty::TApp { ty, .. } => is_local_nominal_type(current_package, ty),
        _ => false,
    }
}

fn define_trait_impl(
    env: &mut PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    impl_block: &fir::ImplBlock,
    trait_name: &fir::FirIdent,
    fir_table: &fir::FirTable,
) {
    let empty_tparams = HashSet::new();
    let for_ty = tast::Ty::from_fir(env, &impl_block.for_type, &[]);
    validate_ty(env, diagnostics, &for_ty, &empty_tparams);
    let trait_name_raw = trait_name.to_ident_name();
    let Some((trait_name_str, trait_env)) = super::util::resolve_trait_name(env, &trait_name_raw)
    else {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!(
                "Trait {} is not defined, cannot implement it for {:?}",
                trait_name_raw, for_ty
            ),
        ));
        return;
    };
    let trait_def = trait_env
        .trait_env
        .trait_defs
        .get(&trait_name_str)
        .cloned()
        .expect("trait def to exist");
    let trait_local = is_local_name(&env.package, &trait_name_str);
    let type_local = is_local_nominal_type(&env.package, &for_ty);
    if !trait_local && !type_local {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!(
                "Impl violates orphan rule: trait {} and type {:?} are not local to package {}",
                trait_name_str, for_ty, env.package
            ),
        ));
        return;
    }

    let key = (trait_name_str.clone(), for_ty.clone());
    if env.current().trait_env.trait_impls.contains_key(&key) {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!(
                "Trait {} implementation for {:?} is already defined",
                trait_name_str, for_ty
            ),
        ));
        return;
    }

    let trait_method_names: HashSet<String> = trait_def.methods.keys().cloned().collect();

    let mut implemented_methods: HashSet<String> = HashSet::new();
    let mut impl_methods: IndexMap<String, env::FnScheme> = IndexMap::new();

    for m in impl_block.methods.iter() {
        let m = match fir_table.def(*m) {
            fir::Def::Fn(func) => func,
            _ => continue,
        };
        let method_name_str = m.name.clone();

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

        let trait_sig = trait_def
            .methods
            .get(&method_name_str)
            .map(|scheme| scheme.ty.clone())
            .expect("trait method signature to exist");

        let tparam_names = type_param_name_set(&m.generics);
        let generics_tast: Vec<tast::TastIdent> = m
            .generics
            .iter()
            .map(|g| tast::TastIdent(g.to_ident_name()))
            .collect();
        let params = m
            .params
            .iter()
            .map(|(_, ty)| {
                let ty = tast::Ty::from_fir(env, ty, &generics_tast);
                validate_ty(env, diagnostics, &ty, &tparam_names);
                instantiate_self_ty(&ty, &for_ty)
            })
            .collect::<Vec<_>>();
        let ret = match &m.ret_ty {
            Some(ty) => {
                let ret = tast::Ty::from_fir(env, ty, &generics_tast);
                validate_ty(env, diagnostics, &ret, &tparam_names);
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
            impl_methods.insert(
                method_name_str.clone(),
                env::FnScheme {
                    type_params: vec![],
                    constraints: (),
                    ty: impl_method_ty,
                    origin: FnOrigin::User,
                },
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

    // Insert the impl block
    env.current_mut().trait_env.trait_impls.insert(
        key,
        env::ImplDef {
            params: vec![],
            methods: impl_methods,
        },
    );
}

fn define_inherent_impl(
    env: &mut PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    impl_block: &fir::ImplBlock,
    fir_table: &fir::FirTable,
) {
    // Combine impl generics with method generics for type parameter validation
    let impl_tparams = type_param_name_set(&impl_block.generics);
    let impl_generics_tast: Vec<tast::TastIdent> = impl_block
        .generics
        .iter()
        .map(|g| tast::TastIdent(g.to_ident_name()))
        .collect();
    let for_ty = tast::Ty::from_fir(env, &impl_block.for_type, &impl_generics_tast);
    validate_ty(env, diagnostics, &for_ty, &impl_tparams);
    if !is_local_nominal_type(&env.package, &for_ty) {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!(
                "Inherent impl for non-local type {:?} is not allowed",
                for_ty
            ),
        ));
        return;
    }

    let key = if !impl_block.generics.is_empty() {
        env::InherentImplKey::Constr(for_ty.get_constr_name_unsafe())
    } else {
        env::InherentImplKey::Exact(for_ty.clone())
    };
    let mut methods_to_add: IndexMap<String, env::FnScheme> = IndexMap::new();

    let mut implemented_methods: HashSet<String> = HashSet::new();
    for m in impl_block.methods.iter() {
        let m = match fir_table.def(*m) {
            fir::Def::Fn(func) => func,
            _ => continue,
        };
        let method_name_str = m.name.clone();

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

        // Combine impl generics and method generics
        let mut all_generics = impl_block.generics.clone();
        all_generics.extend(m.generics.clone());
        let tparam_names = type_param_name_set(&all_generics);
        let all_generics_tast: Vec<tast::TastIdent> = all_generics
            .iter()
            .map(|g| tast::TastIdent(g.to_ident_name()))
            .collect();

        let params = m
            .params
            .iter()
            .map(|(_, ty)| {
                let ty = tast::Ty::from_fir(env, ty, &all_generics_tast);
                validate_ty(env, diagnostics, &ty, &tparam_names);
                instantiate_self_ty(&ty, &for_ty)
            })
            .collect::<Vec<_>>();
        let ret = match &m.ret_ty {
            Some(ty) => {
                let ret = tast::Ty::from_fir(env, ty, &all_generics_tast);
                validate_ty(env, diagnostics, &ret, &tparam_names);
                instantiate_self_ty(&ret, &for_ty)
            }
            None => tast::Ty::TUnit,
        };

        let impl_method_ty = tast::Ty::TFunc {
            params: params.clone(),
            ret_ty: Box::new(ret.clone()),
        };

        // Store impl generics as type parameters for the method scheme
        let type_params: Vec<String> = impl_block
            .generics
            .iter()
            .map(|g| g.to_ident_name())
            .collect();

        methods_to_add.insert(
            method_name_str,
            env::FnScheme {
                type_params,
                constraints: (),
                ty: impl_method_ty,
                origin: FnOrigin::User,
            },
        );
    }

    // Insert or extend the impl def
    let impl_def = env
        .current_mut()
        .trait_env
        .inherent_impls
        .entry(key)
        .or_default();
    impl_def.methods.extend(methods_to_add);
}

fn define_function(env: &mut PackageTypeEnv, diagnostics: &mut Diagnostics, func: &fir::Fn) {
    let name = func.name.clone();
    let tparam_names = type_param_name_set(&func.generics);
    let generics_tast: Vec<tast::TastIdent> = func
        .generics
        .iter()
        .map(|g| tast::TastIdent(g.to_ident_name()))
        .collect();
    let params = func
        .params
        .iter()
        .map(|(_, ty)| {
            let ty = tast::Ty::from_fir(env, ty, &generics_tast);
            validate_ty(env, diagnostics, &ty, &tparam_names);
            ty
        })
        .collect::<Vec<_>>();
    let ret = match &func.ret_ty {
        Some(ty) => {
            let ret = tast::Ty::from_fir(env, ty, &generics_tast);
            validate_ty(env, diagnostics, &ret, &tparam_names);
            ret
        }
        None => tast::Ty::TUnit,
    };
    env.current_mut().value_env.funcs.insert(
        name,
        FnScheme {
            type_params: vec![],
            constraints: (),
            ty: tast::Ty::TFunc {
                params,
                ret_ty: Box::new(ret),
            },
            origin: FnOrigin::User,
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

fn define_extern_go(env: &mut PackageTypeEnv, diagnostics: &mut Diagnostics, ext: &fir::ExternGo) {
    let params = ext
        .params
        .iter()
        .map(|(_, ty)| {
            let ty = tast::Ty::from_fir(env, ty, &[]);
            validate_ty(env, diagnostics, &ty, &HashSet::new());
            ty
        })
        .collect::<Vec<_>>();
    let ret = match &ext.ret_ty {
        Some(ty) => {
            let ret = tast::Ty::from_fir(env, ty, &[]);
            validate_ty(env, diagnostics, &ret, &HashSet::new());
            ret
        }
        None => tast::Ty::TUnit,
    };

    let fn_ty = tast::Ty::TFunc {
        params: params.clone(),
        ret_ty: Box::new(ret.clone()),
    };
    let go_name = go_symbol_name(&ext.go_symbol);
    env.current_mut().register_extern_function(
        ext.goml_name.to_ident_name(),
        ext.package_path.clone(),
        go_name,
        fn_ty,
    );
}

fn define_extern_type(
    env: &mut PackageTypeEnv,
    _diagnostics: &mut Diagnostics,
    ext: &fir::ExternType,
) {
    env.current_mut()
        .register_extern_type(ext.goml_name.to_ident_name());
}

fn define_extern_builtin(
    env: &mut PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    ext: &fir::ExternBuiltin,
) {
    let params = ext
        .params
        .iter()
        .map(|(_, ty)| {
            let ty = tast::Ty::from_fir(env, ty, &[]);
            validate_ty(env, diagnostics, &ty, &HashSet::new());
            ty
        })
        .collect::<Vec<_>>();
    let ret_ty = match &ext.ret_ty {
        Some(ty) => {
            let ty = tast::Ty::from_fir(env, ty, &[]);
            validate_ty(env, diagnostics, &ty, &HashSet::new());
            ty
        }
        None => tast::Ty::TUnit,
    };

    env.current_mut().value_env.funcs.insert(
        ext.name.to_ident_name(),
        FnScheme {
            type_params: vec![],
            constraints: (),
            ty: tast::Ty::TFunc {
                params,
                ret_ty: Box::new(ret_ty),
            },
            origin: FnOrigin::Builtin,
        },
    );
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

fn instantiate_trait_method_ty(ty: &tast::Ty, self_ty: &tast::Ty) -> tast::Ty {
    instantiate_self_ty(ty, self_ty)
}

pub fn collect_typedefs(
    env: &mut PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    fir: &fir::PackageFir,
    fir_table: &fir::FirTable,
) {
    predeclare_types(env.current_mut(), fir, fir_table);

    for item in fir.toplevels.iter() {
        match fir_table.def(*item) {
            fir::Def::EnumDef(enum_def) => define_enum(env, diagnostics, enum_def),
            fir::Def::StructDef(struct_def) => define_struct(env, diagnostics, struct_def),
            fir::Def::TraitDef(trait_def) => define_trait(env, trait_def),
            fir::Def::ImplBlock(impl_block) => {
                if let Some(trait_name) = &impl_block.trait_name {
                    define_trait_impl(env, diagnostics, impl_block, trait_name, fir_table);
                } else {
                    define_inherent_impl(env, diagnostics, impl_block, fir_table);
                }
            }
            fir::Def::Fn(func) => define_function(env, diagnostics, func),
            fir::Def::ExternGo(ext) => define_extern_go(env, diagnostics, ext),
            fir::Def::ExternType(ext) => define_extern_type(env, diagnostics, ext),
            fir::Def::ExternBuiltin(ext) => define_extern_builtin(env, diagnostics, ext),
        }
    }
}

pub fn check_file(
    fir: fir::PackageFir,
    fir_table: name_resolution::FirTable,
) -> (tast::File, env::GlobalTypeEnv, Diagnostics) {
    check_file_with_env(
        fir,
        fir_table,
        env::GlobalTypeEnv::new(),
        "Main",
        HashMap::new(),
    )
}

pub fn check_file_with_env(
    fir: fir::PackageFir,
    fir_table: name_resolution::FirTable,
    genv: env::GlobalTypeEnv,
    package: &str,
    deps: HashMap<String, env::GlobalTypeEnv>,
) -> (tast::File, env::GlobalTypeEnv, Diagnostics) {
    let mut genv = env::PackageTypeEnv::new(package.to_string(), genv, deps);
    let mut typer = Typer::new(fir_table);
    let mut diagnostics = Diagnostics::new();
    collect_typedefs(&mut genv, &mut diagnostics, &fir, &typer.fir_table);
    let mut typed_toplevel_tasts = vec![];
    for item in fir.toplevels.iter() {
        match typer.fir_table.def(*item).clone() {
            fir::Def::EnumDef(..) => (),
            fir::Def::StructDef(..) => (),
            fir::Def::TraitDef(..) => (),
            fir::Def::ImplBlock(impl_block) => {
                typed_toplevel_tasts.push(tast::Item::ImplBlock(check_impl_block(
                    &genv,
                    &mut typer,
                    &mut diagnostics,
                    &impl_block,
                )));
            }
            fir::Def::Fn(f) => {
                typed_toplevel_tasts.push(tast::Item::Fn(check_fn(
                    &genv,
                    &mut typer,
                    &mut diagnostics,
                    &f,
                )));
            }
            fir::Def::ExternGo(ext) => {
                typed_toplevel_tasts.push(tast::Item::ExternGo(check_extern_go(
                    &genv,
                    &mut typer,
                    &mut diagnostics,
                    &ext,
                )));
            }
            fir::Def::ExternType(ext) => {
                typed_toplevel_tasts.push(tast::Item::ExternType(check_extern_type(
                    &mut genv,
                    &mut diagnostics,
                    &ext,
                )));
            }
            // ExternBuiltin is handled during GlobalTypeEnv initialization
            fir::Def::ExternBuiltin(_) => (),
        }
    }

    (
        tast::File {
            toplevels: typed_toplevel_tasts,
        },
        genv.current,
        diagnostics,
    )
}

fn check_fn(
    genv: &PackageTypeEnv,
    typer: &mut Typer,
    diagnostics: &mut Diagnostics,
    f: &fir::Fn,
) -> tast::Fn {
    let mut local_env = LocalTypeEnv::new();
    let tparams: Vec<tast::TastIdent> = f
        .generics
        .iter()
        .map(|g| tast::TastIdent(g.to_ident_name()))
        .collect();
    let param_types: Vec<(fir::LocalId, String, tast::Ty)> = f
        .params
        .iter()
        .map(|(name, ty)| {
            let name_str = typer.fir_table.local_ident_name(*name);
            let ty = tast::Ty::from_fir(genv, ty, &tparams);
            (*name, name_str, ty)
        })
        .collect();
    let new_params = param_types
        .iter()
        .map(|(_, name_str, ty)| (name_str.clone(), ty.clone()))
        .collect::<Vec<_>>();

    let ret_ty = match &f.ret_ty {
        Some(ty) => tast::Ty::from_fir(genv, ty, &tparams),
        None => tast::Ty::TUnit,
    };

    local_env.set_tparams_env(&tparams);
    local_env.push_scope();
    for (id, _, ty) in param_types.iter() {
        local_env.insert_var(*id, ty.clone());
    }
    let typed_body = typer.check_expr(genv, &mut local_env, diagnostics, f.body, &ret_ty);
    local_env.pop_scope();
    local_env.clear_tparams_env();
    typer.solve(genv, diagnostics);
    let typed_body = typer.subst(diagnostics, typed_body);
    tast::Fn {
        name: f.name.clone(),
        params: new_params,
        ret_ty,
        body: typed_body,
    }
}

fn check_impl_block(
    genv: &PackageTypeEnv,
    typer: &mut Typer,
    diagnostics: &mut Diagnostics,
    impl_block: &fir::ImplBlock,
) -> tast::ImplBlock {
    let impl_generics_tast: Vec<tast::TastIdent> = impl_block
        .generics
        .iter()
        .map(|g| tast::TastIdent(g.to_ident_name()))
        .collect();
    let for_ty = tast::Ty::from_fir(genv, &impl_block.for_type, &impl_generics_tast);
    let mut typed_methods = Vec::new();
    for f in impl_block.methods.iter() {
        let f = match typer.fir_table.def(*f).clone() {
            fir::Def::Fn(func) => func,
            _ => continue,
        };
        let mut local_env = LocalTypeEnv::new();

        // Combine impl generics and method generics
        let mut all_generics = impl_block.generics.clone();
        all_generics.extend(f.generics.clone());
        let all_generics_tast: Vec<tast::TastIdent> = all_generics
            .iter()
            .map(|g| tast::TastIdent(g.to_ident_name()))
            .collect();

        let param_types: Vec<(fir::LocalId, String, tast::Ty)> = f
            .params
            .iter()
            .map(|(name, ty)| {
                let ty = tast::Ty::from_fir(genv, ty, &all_generics_tast);
                let ty = instantiate_self_ty(&ty, &for_ty);
                let name_str = typer.fir_table.local_ident_name(*name);
                (*name, name_str, ty)
            })
            .collect();
        let new_params = param_types
            .iter()
            .map(|(_, name_str, ty)| (name_str.clone(), ty.clone()))
            .collect::<Vec<_>>();

        let ret_ty = match &f.ret_ty {
            Some(ty) => {
                let ty = tast::Ty::from_fir(genv, ty, &all_generics_tast);
                instantiate_self_ty(&ty, &for_ty)
            }
            None => tast::Ty::TUnit,
        };

        let tparams: Vec<tast::TastIdent> = all_generics
            .iter()
            .map(|g| tast::TastIdent(g.to_ident_name()))
            .collect();
        local_env.set_tparams_env(&tparams);
        local_env.push_scope();
        for (id, _, ty) in param_types.iter() {
            local_env.insert_var(*id, ty.clone());
        }
        let typed_body = typer.check_expr(genv, &mut local_env, diagnostics, f.body, &ret_ty);
        local_env.pop_scope();
        local_env.clear_tparams_env();
        typer.solve(genv, diagnostics);
        let typed_body = typer.subst(diagnostics, typed_body);
        typed_methods.push(tast::Fn {
            name: f.name.clone(),
            params: new_params,
            ret_ty,
            body: typed_body,
        });
    }
    let trait_name = impl_block.trait_name.as_ref().map(|t| {
        let name = t.to_ident_name();
        super::util::resolve_trait_name(genv, &name)
            .map(|(resolved, _)| tast::TastIdent(resolved))
            .unwrap_or_else(|| tast::TastIdent(name))
    });
    let generics: Vec<String> = impl_block
        .generics
        .iter()
        .map(|g| g.to_ident_name())
        .collect();
    tast::ImplBlock {
        generics,
        trait_name,
        for_type: for_ty,
        methods: typed_methods,
    }
}

fn check_extern_go(
    genv: &PackageTypeEnv,
    _typer: &mut Typer,
    _diagnostics: &mut Diagnostics,
    ext: &fir::ExternGo,
) -> tast::ExternGo {
    let params = ext
        .params
        .iter()
        .map(|(name, ty)| (name.to_ident_name(), tast::Ty::from_fir(genv, ty, &[])))
        .collect::<Vec<_>>();
    let ret_ty = match &ext.ret_ty {
        Some(ty) => tast::Ty::from_fir(genv, ty, &[]),
        None => tast::Ty::TUnit,
    };
    tast::ExternGo {
        goml_name: ext.goml_name.to_ident_name(),
        go_name: go_symbol_name(&ext.go_symbol),
        package_path: ext.package_path.clone(),
        params,
        ret_ty,
    }
}

fn check_extern_type(
    genv: &mut PackageTypeEnv,
    _diagnostics: &mut Diagnostics,
    ext: &fir::ExternType,
) -> tast::ExternType {
    genv.current_mut()
        .register_extern_type(ext.goml_name.to_ident_name());
    tast::ExternType {
        goml_name: ext.goml_name.to_ident_name(),
    }
}
