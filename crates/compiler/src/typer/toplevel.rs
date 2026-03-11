use std::collections::{HashMap, HashSet};

use diagnostics::{Severity, Stage};
use indexmap::IndexMap;
use parser::{Diagnostic, Diagnostics};

use crate::{
    builtins,
    env::{
        self, ExternBindingMode, ExternReturnMode, ExternTypeBindingConflict, FnOrigin, FnScheme,
        GlobalTypeEnv, PackageTypeEnv,
    },
    hir::{self},
    package_names::{BUILTIN_PACKAGE, ROOT_PACKAGE, is_special_unqualified_package},
    tast::{self},
    typer::{
        Typer,
        localenv::LocalTypeEnv,
        name_resolution,
        util::{type_expr_range, type_param_name_set, validate_ty},
    },
};

fn predeclare_types(
    genv: &mut GlobalTypeEnv,
    diagnostics: &mut Diagnostics,
    hir: &hir::PackageHir,
    hir_table: &hir::HirTable,
) {
    for item in hir.toplevels.iter() {
        match hir_table.def(*item) {
            hir::Def::EnumDef(enum_def) => {
                genv.ensure_enum_placeholder(
                    tast::TastIdent(enum_def.name.to_ident_name()),
                    enum_def
                        .generics
                        .iter()
                        .map(|i| tast::TastIdent(i.to_ident_name()))
                        .collect(),
                );
            }
            hir::Def::StructDef(hir::StructDef { name, generics, .. }) => {
                genv.ensure_struct_placeholder(
                    tast::TastIdent(name.to_ident_name()),
                    generics
                        .iter()
                        .map(|i| tast::TastIdent(i.to_ident_name()))
                        .collect(),
                );
            }
            hir::Def::ExternType(ext) => {
                if let Some(conflict) = genv.register_extern_type(
                    ext.goml_name.to_ident_name(),
                    ext.package_path.clone(),
                    ext.go_name.clone(),
                    ext.package_path.is_some(),
                ) {
                    push_extern_type_binding_conflict(diagnostics, conflict);
                }
            }
            _ => {}
        }
    }
}

fn push_extern_type_binding_conflict(
    diagnostics: &mut Diagnostics,
    conflict: ExternTypeBindingConflict,
) {
    let existing = match conflict.existing_package_path {
        Some(package_path) => format!("\"{}\" \"{}\"", package_path, conflict.existing_go_name),
        None => format!("unbound Go type \"{}\"", conflict.existing_go_name),
    };
    let new = match conflict.new_package_path {
        Some(package_path) => format!("\"{}\" \"{}\"", package_path, conflict.new_go_name),
        None => format!("unbound Go type \"{}\"", conflict.new_go_name),
    };
    super::util::push_error(
        diagnostics,
        format!(
            "Extern type {} has conflicting Go bindings: {} vs {}",
            conflict.type_name, existing, new
        ),
    );
}

fn attribute_name(attr: &hir::Attribute) -> Option<&str> {
    let trimmed = attr.text.trim();
    let inner = trimmed.strip_prefix("#[")?.strip_suffix(']')?.trim();
    let name_part = match inner.find('(') {
        Some(idx) => inner[..idx].trim(),
        None => inner,
    };
    if name_part.is_empty() {
        None
    } else {
        Some(name_part)
    }
}

fn extern_return_mode(attrs: &[hir::Attribute], diagnostics: &mut Diagnostics) -> ExternReturnMode {
    let mut error_only = false;
    let mut error_last = false;
    let mut option_last = false;
    for attr in attrs {
        match attribute_name(attr) {
            Some("go_error") => error_only = true,
            Some("go_error_last") => error_last = true,
            Some("go_option_last") => option_last = true,
            _ => {}
        }
    }

    let mode_count = error_only as u8 + error_last as u8 + option_last as u8;
    if mode_count > 1 {
        super::util::push_error(
            diagnostics,
            "extern \"go\" function can use at most one of #[go_error], #[go_error_last], #[go_option_last]",
        );
        return ExternReturnMode::Plain;
    }

    if error_last {
        ExternReturnMode::ErrorLast
    } else if error_only {
        ExternReturnMode::ErrorOnly
    } else if option_last {
        ExternReturnMode::OptionLast
    } else {
        ExternReturnMode::Plain
    }
}

fn extern_binding_mode(
    attrs: &[hir::Attribute],
    return_mode: ExternReturnMode,
    diagnostics: &mut Diagnostics,
) -> ExternBindingMode {
    let mut go_value = false;
    for attr in attrs {
        if matches!(attribute_name(attr), Some("go_value")) {
            go_value = true;
        }
    }

    if !go_value {
        return ExternBindingMode::Call;
    }

    if return_mode != ExternReturnMode::Plain {
        super::util::push_error(
            diagnostics,
            "extern \"go\" function #[go_value] cannot be combined with #[go_error], #[go_error_last], or #[go_option_last]",
        );
        return ExternBindingMode::Call;
    }

    ExternBindingMode::Value
}

fn extract_result_tys(ty: &tast::Ty) -> Option<(&tast::Ty, &tast::Ty)> {
    let tast::Ty::TApp { ty, args } = ty else {
        return None;
    };
    let tast::Ty::TEnum { name } = ty.as_ref() else {
        return None;
    };
    if name != "Result" || args.len() != 2 {
        return None;
    }
    Some((&args[0], &args[1]))
}

fn is_go_error_ty(ty: &tast::Ty) -> bool {
    matches!(ty, tast::Ty::TStruct { name } if name == "GoError")
}

fn extract_option_ty(ty: &tast::Ty) -> Option<&tast::Ty> {
    let tast::Ty::TApp { ty, args } = ty else {
        return None;
    };
    let tast::Ty::TEnum { name } = ty.as_ref() else {
        return None;
    };
    if name != "Option" || args.len() != 1 {
        return None;
    }
    Some(&args[0])
}

fn is_go_method_symbol(name: &str) -> bool {
    if let Some(rest) = name.strip_prefix("(*") {
        return rest.contains(").");
    }

    if let Some(rest) = name.strip_prefix('(') {
        return rest.contains(").");
    }

    let parts = name.split('.').collect::<Vec<_>>();
    matches!(
        parts.as_slice(),
        [receiver_ty, _method_name]
            if receiver_ty
                .chars()
                .next()
                .is_some_and(|ch| ch.is_ascii_uppercase())
    ) || matches!(
        parts.as_slice(),
        [package_name, receiver_ty, _method_name]
            if package_name
                .chars()
                .next()
                .is_some_and(|ch| ch.is_ascii_lowercase())
                && receiver_ty
                    .chars()
                    .next()
                    .is_some_and(|ch| ch.is_ascii_uppercase())
    )
}

fn validate_extern_return_mode(
    diagnostics: &mut Diagnostics,
    extern_name: &str,
    return_mode: ExternReturnMode,
    ret_ty: &tast::Ty,
) {
    match return_mode {
        ExternReturnMode::Plain => {}
        ExternReturnMode::ErrorOnly => {
            let Some((ok_ty, err_ty)) = extract_result_tys(ret_ty) else {
                super::util::push_error(
                    diagnostics,
                    format!(
                        "extern \"go\" function {} with #[go_error] must return Result[unit, GoError]",
                        extern_name
                    ),
                );
                return;
            };
            if !matches!(ok_ty, tast::Ty::TUnit) || !is_go_error_ty(err_ty) {
                super::util::push_error(
                    diagnostics,
                    format!(
                        "extern \"go\" function {} with #[go_error] must return Result[unit, GoError]",
                        extern_name
                    ),
                );
            }
        }
        ExternReturnMode::ErrorLast => {
            let Some((_ok_ty, err_ty)) = extract_result_tys(ret_ty) else {
                super::util::push_error(
                    diagnostics,
                    format!(
                        "extern \"go\" function {} with #[go_error_last] must return Result[T, GoError]",
                        extern_name
                    ),
                );
                return;
            };
            if !is_go_error_ty(err_ty) {
                super::util::push_error(
                    diagnostics,
                    format!(
                        "extern \"go\" function {} with #[go_error_last] must return Result[T, GoError]",
                        extern_name
                    ),
                );
            }
        }
        ExternReturnMode::OptionLast => {
            if extract_option_ty(ret_ty).is_none() {
                super::util::push_error(
                    diagnostics,
                    format!(
                        "extern \"go\" function {} with #[go_option_last] must return Option[T]",
                        extern_name
                    ),
                );
            }
        }
    }
}

fn validate_extern_binding_mode(
    diagnostics: &mut Diagnostics,
    extern_name: &str,
    binding_mode: ExternBindingMode,
    params: &[tast::Ty],
) -> ExternBindingMode {
    if binding_mode == ExternBindingMode::Value && !params.is_empty() {
        super::util::push_error(
            diagnostics,
            format!(
                "extern \"go\" function {} with #[go_value] must have no parameters",
                extern_name
            ),
        );
        ExternBindingMode::Call
    } else {
        binding_mode
    }
}

fn define_enum(env: &mut PackageTypeEnv, diagnostics: &mut Diagnostics, enum_def: &hir::EnumDef) {
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
                .map(|ast_ty| {
                    let ty = tast::Ty::from_hir(env, ast_ty, &params_env);
                    validate_ty(
                        env,
                        diagnostics,
                        &ty,
                        type_expr_range(ast_ty),
                        &tparam_names,
                    );
                    ty
                })
                .collect::<Vec<_>>();
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
    struct_def: &hir::StructDef,
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
            let ty = tast::Ty::from_hir(env, ast_ty, &params_env);
            validate_ty(
                env,
                diagnostics,
                &ty,
                type_expr_range(ast_ty),
                &tparam_names,
            );
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

fn define_trait(env: &mut PackageTypeEnv, trait_def: &hir::TraitDef) {
    let mut methods = IndexMap::new();

    for hir::TraitMethodSignature {
        name: method_name,
        params,
        ret_ty,
    } in trait_def.method_sigs.iter()
    {
        let param_tys = params
            .iter()
            .map(|ast_ty| tast::Ty::from_hir(env, ast_ty, &[]))
            .collect::<Vec<_>>();
        let ret_ty = tast::Ty::from_hir(env, ret_ty, &[]);
        let fn_ty = tast::Ty::TFunc {
            params: param_tys,
            ret_ty: Box::new(ret_ty),
        };

        methods.insert(
            method_name.to_ident_name(),
            FnScheme {
                type_params: vec![],
                constraints: vec![],
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

fn add_fn_constraints_from_bounds(
    env: &PackageTypeEnv,
    known_type_params: &HashSet<String>,
    bounds: &[(hir::HirIdent, Vec<hir::Path>)],
    constraints: &mut Vec<env::FnConstraint>,
) {
    for (param, traits) in bounds.iter() {
        let param_name = param.to_ident_name();
        if !known_type_params.contains(&param_name) {
            continue;
        }
        for trait_path in traits.iter() {
            let raw_trait_name = trait_path.display();
            let Some((trait_name, _trait_env)) =
                super::util::resolve_trait_name(env, &raw_trait_name)
            else {
                continue;
            };
            let constraint = env::FnConstraint {
                type_param: param_name.clone(),
                trait_name: tast::TastIdent(trait_name),
            };
            if !constraints.contains(&constraint) {
                constraints.push(constraint);
            }
        }
    }
}

fn build_fn_constraints(
    env: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    generics: &[hir::HirIdent],
    bounds: &[(hir::HirIdent, Vec<hir::Path>)],
) -> Vec<env::FnConstraint> {
    let known_type_params = generics
        .iter()
        .map(|param| param.to_ident_name())
        .collect::<HashSet<_>>();
    let mut constraints = Vec::new();
    let _ = diagnostics;
    add_fn_constraints_from_bounds(env, &known_type_params, bounds, &mut constraints);
    constraints
}

fn build_method_constraints(
    env: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    all_generics: &[hir::HirIdent],
    impl_bounds: &[(hir::HirIdent, Vec<hir::Path>)],
    method_bounds: &[(hir::HirIdent, Vec<hir::Path>)],
) -> Vec<env::FnConstraint> {
    let known_type_params = all_generics
        .iter()
        .map(|param| param.to_ident_name())
        .collect::<HashSet<_>>();
    let mut constraints = Vec::new();
    let _ = diagnostics;
    add_fn_constraints_from_bounds(env, &known_type_params, impl_bounds, &mut constraints);
    add_fn_constraints_from_bounds(env, &known_type_params, method_bounds, &mut constraints);
    constraints
}

fn is_local_name(current_package: &str, name: &str) -> bool {
    if let Some((package, _)) = name.split_once("::") {
        package == current_package
    } else {
        is_special_unqualified_package(current_package)
    }
}

fn is_local_nominal_type(current_package: &str, ty: &tast::Ty) -> bool {
    match ty {
        tast::Ty::TStruct { name } | tast::Ty::TEnum { name } => {
            is_local_name(current_package, name)
        }
        tast::Ty::TApp { ty, .. } => is_local_nominal_type(current_package, ty),
        tast::Ty::TSlice { .. }
        | tast::Ty::TVec { .. }
        | tast::Ty::TRef { .. }
        | tast::Ty::THashMap { .. } => current_package == BUILTIN_PACKAGE,
        _ => false,
    }
}

fn define_trait_impl(
    env: &mut PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    impl_block: &hir::ImplBlock,
    trait_name: &hir::HirIdent,
    hir_table: &hir::HirTable,
) {
    let impl_tparams = type_param_name_set(&impl_block.generics);
    let impl_generics_tast: Vec<tast::TastIdent> = impl_block
        .generics
        .iter()
        .map(|g| tast::TastIdent(g.to_ident_name()))
        .collect();
    let for_ty = tast::Ty::from_hir(env, &impl_block.for_type, &impl_generics_tast);
    validate_ty(
        env,
        diagnostics,
        &for_ty,
        type_expr_range(&impl_block.for_type),
        &impl_tparams,
    );
    let trait_name_raw = trait_name.to_ident_name();
    let Some((trait_name_str, trait_env)) = super::util::resolve_trait_name(env, &trait_name_raw)
    else {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!(
                "Trait {} is not defined, cannot implement it for {}",
                trait_name_raw,
                super::util::format_ty_for_diag(&for_ty)
            ),
        ));
        return;
    };
    let trait_def = trait_env.trait_env.trait_defs.get(&trait_name_str).cloned();
    let Some(trait_def) = trait_def else {
        super::util::push_ice(
            diagnostics,
            format!("trait def missing after resolution: {}", trait_name_str),
        );
        return;
    };
    let trait_local = is_local_name(&env.package, &trait_name_str);
    let type_local = is_local_nominal_type(&env.package, &for_ty);
    if !trait_local && !type_local {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!(
                "Impl violates orphan rule: trait {} and type {} are not local to package {}",
                trait_name_str,
                super::util::format_ty_for_diag(&for_ty),
                env.package
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
                "Trait {} implementation for {} is already defined",
                trait_name_str,
                super::util::format_ty_for_diag(&for_ty)
            ),
        ));
        return;
    }

    let trait_method_names: HashSet<String> = trait_def.methods.keys().cloned().collect();

    let mut implemented_methods: HashSet<String> = HashSet::new();
    let mut impl_methods: IndexMap<String, env::FnScheme> = IndexMap::new();

    for m in impl_block.methods.iter() {
        let m = match hir_table.def(*m) {
            hir::Def::Fn(func) => func,
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
            .map(|scheme| scheme.ty.clone());
        let Some(trait_sig) = trait_sig else {
            super::util::push_ice(
                diagnostics,
                format!(
                    "trait method signature missing: {}::{}",
                    trait_name_str, method_name_str
                ),
            );
            continue;
        };

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
            .map(|(_, hir_ty)| {
                let ty = tast::Ty::from_hir(env, hir_ty, &all_generics_tast);
                validate_ty(
                    env,
                    diagnostics,
                    &ty,
                    type_expr_range(hir_ty),
                    &tparam_names,
                );
                instantiate_self_ty(&ty, &for_ty)
            })
            .collect::<Vec<_>>();
        let ret = match &m.ret_ty {
            Some(hir_ty) => {
                let ret = tast::Ty::from_hir(env, hir_ty, &all_generics_tast);
                validate_ty(
                    env,
                    diagnostics,
                    &ret,
                    type_expr_range(hir_ty),
                    &tparam_names,
                );
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
                                "Trait {}::{} parameter {} expected type {} but found {}",
                                trait_name_str,
                                method_name_str,
                                idx,
                                super::util::format_ty_for_diag(expected),
                                super::util::format_ty_for_diag(actual)
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
                            "Trait {}::{} expected return type {} but found {}",
                            trait_name_str,
                            method_name_str,
                            super::util::format_ty_for_diag(expected_ret),
                            super::util::format_ty_for_diag(impl_ret)
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
            let type_params: Vec<String> = all_generics.iter().map(|g| g.to_ident_name()).collect();
            let constraints = build_method_constraints(
                env,
                diagnostics,
                &all_generics,
                &impl_block.generic_bounds,
                &m.generic_bounds,
            );
            impl_methods.insert(
                method_name_str.clone(),
                env::FnScheme {
                    type_params,
                    constraints,
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
                    "Trait {} implementation for {} is missing method {}",
                    trait_name_str,
                    super::util::format_ty_for_diag(&for_ty),
                    method_name
                ),
            ));
        }
    }

    // Insert the impl block
    env.current_mut().trait_env.trait_impls.insert(
        key,
        env::ImplDef {
            params: impl_generics_tast,
            methods: impl_methods,
        },
    );
}

fn define_inherent_impl(
    env: &mut PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    impl_block: &hir::ImplBlock,
    hir_table: &hir::HirTable,
) {
    // Combine impl generics with method generics for type parameter validation
    let impl_tparams = type_param_name_set(&impl_block.generics);
    let impl_generics_tast: Vec<tast::TastIdent> = impl_block
        .generics
        .iter()
        .map(|g| tast::TastIdent(g.to_ident_name()))
        .collect();
    let for_ty = tast::Ty::from_hir(env, &impl_block.for_type, &impl_generics_tast);
    validate_ty(
        env,
        diagnostics,
        &for_ty,
        type_expr_range(&impl_block.for_type),
        &impl_tparams,
    );
    if !is_local_nominal_type(&env.package, &for_ty) {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!(
                "Inherent impl for non-local type {} is not allowed",
                super::util::format_ty_for_diag(&for_ty)
            ),
        ));
        return;
    }

    let key = if !impl_block.generics.is_empty() {
        let Some(constr_name) = super::util::try_constr_name(&for_ty) else {
            super::util::push_ice(
                diagnostics,
                format!(
                    "Expected constructor type in inherent impl, got {}",
                    super::util::format_ty_for_diag(&for_ty)
                ),
            );
            return;
        };
        env::InherentImplKey::Constr(constr_name)
    } else {
        env::InherentImplKey::Exact(for_ty.clone())
    };
    let mut methods_to_add: IndexMap<String, env::FnScheme> = IndexMap::new();

    let mut implemented_methods: HashSet<String> = HashSet::new();
    for m in impl_block.methods.iter() {
        let m = match hir_table.def(*m) {
            hir::Def::Fn(func) => func,
            _ => continue,
        };
        let method_name_str = m.name.clone();

        if !implemented_methods.insert(method_name_str.clone()) {
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Method {} implemented multiple times in impl for {}",
                    method_name_str,
                    super::util::format_ty_for_diag(&for_ty)
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
            .map(|(_, hir_ty)| {
                let ty = tast::Ty::from_hir(env, hir_ty, &all_generics_tast);
                validate_ty(
                    env,
                    diagnostics,
                    &ty,
                    type_expr_range(hir_ty),
                    &tparam_names,
                );
                instantiate_self_ty(&ty, &for_ty)
            })
            .collect::<Vec<_>>();
        let ret = match &m.ret_ty {
            Some(hir_ty) => {
                let ret = tast::Ty::from_hir(env, hir_ty, &all_generics_tast);
                validate_ty(
                    env,
                    diagnostics,
                    &ret,
                    type_expr_range(hir_ty),
                    &tparam_names,
                );
                instantiate_self_ty(&ret, &for_ty)
            }
            None => tast::Ty::TUnit,
        };

        let impl_method_ty = tast::Ty::TFunc {
            params: params.clone(),
            ret_ty: Box::new(ret.clone()),
        };

        let type_params: Vec<String> = all_generics.iter().map(|g| g.to_ident_name()).collect();
        let constraints = build_method_constraints(
            env,
            diagnostics,
            &all_generics,
            &impl_block.generic_bounds,
            &m.generic_bounds,
        );

        methods_to_add.insert(
            method_name_str,
            env::FnScheme {
                type_params,
                constraints,
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

fn define_function(env: &mut PackageTypeEnv, diagnostics: &mut Diagnostics, func: &hir::Fn) {
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
        .map(|(_, hir_ty)| {
            let ty = tast::Ty::from_hir(env, hir_ty, &generics_tast);
            validate_ty(
                env,
                diagnostics,
                &ty,
                type_expr_range(hir_ty),
                &tparam_names,
            );
            ty
        })
        .collect::<Vec<_>>();
    let ret = match &func.ret_ty {
        Some(hir_ty) => {
            let ret = tast::Ty::from_hir(env, hir_ty, &generics_tast);
            validate_ty(
                env,
                diagnostics,
                &ret,
                type_expr_range(hir_ty),
                &tparam_names,
            );
            ret
        }
        None => tast::Ty::TUnit,
    };
    let fn_constraints =
        build_fn_constraints(env, diagnostics, &func.generics, &func.generic_bounds);
    env.current_mut().value_env.funcs.insert(
        name,
        FnScheme {
            type_params: func.generics.iter().map(|g| g.to_ident_name()).collect(),
            constraints: fn_constraints,
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

fn define_extern_go(env: &mut PackageTypeEnv, diagnostics: &mut Diagnostics, ext: &hir::ExternGo) {
    let params = ext
        .params
        .iter()
        .map(|(_, hir_ty)| {
            let ty = tast::Ty::from_hir(env, hir_ty, &[]);
            validate_ty(
                env,
                diagnostics,
                &ty,
                type_expr_range(hir_ty),
                &HashSet::new(),
            );
            ty
        })
        .collect::<Vec<_>>();
    let ret = match &ext.ret_ty {
        Some(hir_ty) => {
            let ret = tast::Ty::from_hir(env, hir_ty, &[]);
            validate_ty(
                env,
                diagnostics,
                &ret,
                type_expr_range(hir_ty),
                &HashSet::new(),
            );
            ret
        }
        None => tast::Ty::TUnit,
    };

    let fn_ty = tast::Ty::TFunc {
        params: params.clone(),
        ret_ty: Box::new(ret.clone()),
    };
    let return_mode = extern_return_mode(&ext.attrs, diagnostics);
    let binding_mode = extern_binding_mode(&ext.attrs, return_mode, diagnostics);
    let binding_mode = validate_extern_binding_mode(
        diagnostics,
        &ext.goml_name.to_ident_name(),
        binding_mode,
        &params,
    );
    validate_extern_return_mode(
        diagnostics,
        &ext.goml_name.to_ident_name(),
        return_mode,
        &ret,
    );
    if ext.explicit_go_symbol && is_go_method_symbol(&ext.go_symbol) && params.is_empty() {
        super::util::push_error(
            diagnostics,
            format!(
                "extern \"go\" method binding {} requires a receiver parameter",
                ext.goml_name.to_ident_name()
            ),
        );
    }
    let go_name = if ext.explicit_go_symbol {
        ext.go_symbol.clone()
    } else {
        go_symbol_name(&ext.go_symbol)
    };
    env.current_mut().register_extern_function(
        ext.goml_name.to_ident_name(),
        ext.package_path.clone(),
        go_name,
        fn_ty,
        binding_mode,
        return_mode,
    );
}

fn define_extern_type(
    _env: &mut PackageTypeEnv,
    _diagnostics: &mut Diagnostics,
    _ext: &hir::ExternType,
) {
}

fn define_extern_builtin(
    env: &mut PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    ext: &hir::ExternBuiltin,
) {
    let tparams: Vec<tast::TastIdent> = ext
        .generics
        .iter()
        .map(|g| tast::TastIdent(g.to_ident_name()))
        .collect();
    let tparam_names = type_param_name_set(&ext.generics);
    let params = ext
        .params
        .iter()
        .map(|(_, hir_ty)| {
            let ty = tast::Ty::from_hir(env, hir_ty, &tparams);
            validate_ty(
                env,
                diagnostics,
                &ty,
                type_expr_range(hir_ty),
                &tparam_names,
            );
            ty
        })
        .collect::<Vec<_>>();
    let ret_ty = match &ext.ret_ty {
        Some(hir_ty) => {
            let ty = tast::Ty::from_hir(env, hir_ty, &tparams);
            validate_ty(
                env,
                diagnostics,
                &ty,
                type_expr_range(hir_ty),
                &tparam_names,
            );
            ty
        }
        None => tast::Ty::TUnit,
    };
    let fn_constraints = build_fn_constraints(env, diagnostics, &ext.generics, &ext.generic_bounds);

    env.current_mut().value_env.funcs.insert(
        ext.name.to_ident_name(),
        FnScheme {
            type_params: ext.generics.iter().map(|g| g.to_ident_name()).collect(),
            constraints: fn_constraints,
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
        tast::Ty::TChar => tast::Ty::TChar,
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
        tast::Ty::TSlice { elem } => tast::Ty::TSlice {
            elem: Box::new(instantiate_self_ty(elem, self_ty)),
        },
        tast::Ty::TVec { elem } => tast::Ty::TVec {
            elem: Box::new(instantiate_self_ty(elem, self_ty)),
        },
        tast::Ty::TRef { elem } => tast::Ty::TRef {
            elem: Box::new(instantiate_self_ty(elem, self_ty)),
        },
        tast::Ty::THashMap { key, value } => tast::Ty::THashMap {
            key: Box::new(instantiate_self_ty(key, self_ty)),
            value: Box::new(instantiate_self_ty(value, self_ty)),
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
    hir: &hir::PackageHir,
    hir_table: &hir::HirTable,
) {
    predeclare_types(env.current_mut(), diagnostics, hir, hir_table);

    for item in hir.toplevels.iter() {
        match hir_table.def(*item) {
            hir::Def::EnumDef(enum_def) => define_enum(env, diagnostics, enum_def),
            hir::Def::StructDef(struct_def) => define_struct(env, diagnostics, struct_def),
            hir::Def::TraitDef(trait_def) => define_trait(env, trait_def),
            hir::Def::ImplBlock(impl_block) => {
                if let Some(trait_name) = &impl_block.trait_name {
                    define_trait_impl(env, diagnostics, impl_block, trait_name, hir_table);
                } else {
                    define_inherent_impl(env, diagnostics, impl_block, hir_table);
                }
            }
            hir::Def::Fn(func) => define_function(env, diagnostics, func),
            hir::Def::ExternGo(ext) => define_extern_go(env, diagnostics, ext),
            hir::Def::ExternType(ext) => define_extern_type(env, diagnostics, ext),
            hir::Def::ExternBuiltin(ext) => define_extern_builtin(env, diagnostics, ext),
        }
    }
    validate_no_infinite_size_structs(env, diagnostics);
}

fn validate_no_infinite_size_structs(env: &PackageTypeEnv, diagnostics: &mut Diagnostics) {
    let structs = env.current().structs();
    for (name, _) in structs.iter() {
        let mut visited = HashSet::new();
        if has_infinite_size(structs, &name.0, &mut visited) {
            diagnostics.push(
                Diagnostic::new(
                    Stage::Typer,
                    Severity::Error,
                    format!(
                        "Struct {} has infinite size due to recursive field; use Ref[{}] for indirection",
                        name.0, name.0
                    ),
                )
            );
        }
    }
}

fn has_infinite_size(
    structs: &IndexMap<tast::TastIdent, env::StructDef>,
    target: &str,
    visited: &mut HashSet<String>,
) -> bool {
    if !visited.insert(target.to_string()) {
        return true;
    }
    let Some(def) = structs.get(&tast::TastIdent(target.to_string())) else {
        visited.remove(target);
        return false;
    };
    for (_, field_ty) in &def.fields {
        if ty_contains_inline_struct(structs, field_ty, visited) {
            visited.remove(target);
            return true;
        }
    }
    visited.remove(target);
    false
}

fn ty_contains_inline_struct(
    structs: &IndexMap<tast::TastIdent, env::StructDef>,
    ty: &tast::Ty,
    visited: &mut HashSet<String>,
) -> bool {
    match ty {
        tast::Ty::TStruct { name, .. } => has_infinite_size(structs, name, visited),
        tast::Ty::TTuple { typs } => typs
            .iter()
            .any(|t| ty_contains_inline_struct(structs, t, visited)),
        tast::Ty::TArray { elem, .. } => ty_contains_inline_struct(structs, elem, visited),
        _ => false,
    }
}

pub fn check_file(
    hir: hir::PackageHir,
    hir_table: name_resolution::HirTable,
) -> (tast::File, env::GlobalTypeEnv, Diagnostics) {
    check_file_with_env(
        hir,
        hir_table,
        env::GlobalTypeEnv::new(),
        builtins::builtin_env(),
        ROOT_PACKAGE,
        HashMap::new(),
    )
}

pub fn check_file_with_env(
    hir: hir::PackageHir,
    hir_table: name_resolution::HirTable,
    genv: env::GlobalTypeEnv,
    builtins: env::GlobalTypeEnv,
    package: &str,
    deps: HashMap<String, env::GlobalTypeEnv>,
) -> (tast::File, env::GlobalTypeEnv, Diagnostics) {
    let mut genv = env::PackageTypeEnv::new(package.to_string(), builtins, genv, deps);
    let mut typer = Typer::new(hir_table);
    let mut diagnostics = Diagnostics::new();
    collect_typedefs(&mut genv, &mut diagnostics, &hir, &typer.hir_table);
    let in_scope_traits = build_in_scope_traits(&genv, &hir, &mut diagnostics);
    for item in hir.toplevels.iter() {
        match typer.hir_table.def(*item).clone() {
            hir::Def::ImplBlock(impl_block) => typecheck_impl_block(
                &genv,
                &mut typer,
                &mut diagnostics,
                &impl_block,
                &in_scope_traits,
            ),
            hir::Def::Fn(func) => {
                typecheck_fn(&genv, &mut typer, &mut diagnostics, &func, &in_scope_traits)
            }
            hir::Def::EnumDef(..)
            | hir::Def::StructDef(..)
            | hir::Def::TraitDef(..)
            | hir::Def::ExternGo(..)
            | hir::Def::ExternType(..)
            | hir::Def::ExternBuiltin(..) => {}
        }
    }
    let mut results = std::mem::replace(
        &mut typer.results,
        crate::typer::results::TypeckResultsBuilder::new(&typer.hir_table),
    );
    results.finalize_types(&mut typer);
    typer.results = results;
    let file = crate::typer::tast_builder::build_file(
        &genv,
        &hir,
        &typer.hir_table,
        typer.results.results(),
    );
    let file = subst_file(&mut typer, &mut diagnostics, file);

    (file, genv.current, diagnostics)
}

pub fn check_file_with_env_and_results(
    hir: hir::PackageHir,
    hir_table: name_resolution::HirTable,
    genv: env::GlobalTypeEnv,
    builtins: env::GlobalTypeEnv,
    package: &str,
    deps: HashMap<String, env::GlobalTypeEnv>,
) -> (
    name_resolution::HirTable,
    crate::typer::results::TypeckResults,
    env::GlobalTypeEnv,
    Diagnostics,
) {
    let mut genv = env::PackageTypeEnv::new(package.to_string(), builtins, genv, deps);
    let mut typer = Typer::new(hir_table);
    let mut diagnostics = Diagnostics::new();
    collect_typedefs(&mut genv, &mut diagnostics, &hir, &typer.hir_table);
    let in_scope_traits = build_in_scope_traits(&genv, &hir, &mut diagnostics);
    for item in hir.toplevels.iter() {
        match typer.hir_table.def(*item).clone() {
            hir::Def::ImplBlock(impl_block) => typecheck_impl_block(
                &genv,
                &mut typer,
                &mut diagnostics,
                &impl_block,
                &in_scope_traits,
            ),
            hir::Def::Fn(func) => {
                typecheck_fn(&genv, &mut typer, &mut diagnostics, &func, &in_scope_traits)
            }
            hir::Def::EnumDef(..)
            | hir::Def::StructDef(..)
            | hir::Def::TraitDef(..)
            | hir::Def::ExternGo(..)
            | hir::Def::ExternType(..)
            | hir::Def::ExternBuiltin(..) => {}
        }
    }
    let mut results = std::mem::replace(
        &mut typer.results,
        crate::typer::results::TypeckResultsBuilder::new(&typer.hir_table),
    );
    results.finalize_types(&mut typer);
    let results = results.finish();

    (typer.hir_table, results, genv.current, diagnostics)
}

fn subst_file(typer: &mut Typer, diagnostics: &mut Diagnostics, file: tast::File) -> tast::File {
    let toplevels = file
        .toplevels
        .into_iter()
        .map(|item| match item {
            tast::Item::Fn(func) => tast::Item::Fn(tast::Fn {
                body: typer.subst_block(diagnostics, func.body),
                ..func
            }),
            tast::Item::ImplBlock(impl_block) => tast::Item::ImplBlock(tast::ImplBlock {
                methods: impl_block
                    .methods
                    .into_iter()
                    .map(|method| tast::Fn {
                        body: typer.subst_block(diagnostics, method.body),
                        ..method
                    })
                    .collect(),
                ..impl_block
            }),
            other => other,
        })
        .collect();
    tast::File { toplevels }
}

fn build_in_scope_traits(
    genv: &PackageTypeEnv,
    hir: &hir::PackageHir,
    diagnostics: &mut Diagnostics,
) -> Vec<tast::TastIdent> {
    let mut traits = genv
        .builtins()
        .trait_env
        .trait_defs
        .keys()
        .cloned()
        .map(tast::TastIdent)
        .collect::<Vec<_>>();
    for trait_name in genv.current().trait_env.trait_defs.keys() {
        traits.push(tast::TastIdent(trait_name.clone()));
    }
    for use_trait in hir.use_traits.iter() {
        let name = use_trait.display();
        if let Some(resolved) = resolve_trait_ident_or_report(genv, diagnostics, &name) {
            traits.push(resolved);
        }
    }
    traits.sort_by(|a, b| a.0.cmp(&b.0));
    traits.dedup_by(|a, b| a.0 == b.0);
    traits
}

fn init_trait_bounds(
    tparams: &[tast::TastIdent],
) -> indexmap::IndexMap<String, Vec<tast::TastIdent>> {
    let mut bounds = indexmap::IndexMap::new();
    for param in tparams.iter() {
        bounds.insert(param.0.clone(), Vec::new());
    }
    bounds
}

fn extend_trait_bounds(
    genv: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    bounds: &mut indexmap::IndexMap<String, Vec<tast::TastIdent>>,
    generic_bounds: &[(hir::HirIdent, Vec<hir::Path>)],
) {
    for (param, traits) in generic_bounds.iter() {
        let param_name = param.to_ident_name();
        let Some(out) = bounds.get_mut(&param_name) else {
            continue;
        };
        for trait_path in traits.iter() {
            let name = trait_path.display();
            if let Some(resolved) = resolve_trait_ident_or_report(genv, diagnostics, &name) {
                out.push(resolved);
            }
        }
    }
}

fn resolve_trait_ident_or_report(
    genv: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    name: &str,
) -> Option<tast::TastIdent> {
    if let Some((resolved, _env)) = super::util::resolve_trait_name(genv, name) {
        return Some(tast::TastIdent(resolved));
    }
    diagnostics.push(Diagnostic::new(
        Stage::Typer,
        Severity::Error,
        format!("Unknown trait {}", name),
    ));
    None
}

fn normalize_trait_bounds(bounds: &mut indexmap::IndexMap<String, Vec<tast::TastIdent>>) {
    for traits in bounds.values_mut() {
        traits.sort_by(|a, b| a.0.cmp(&b.0));
        traits.dedup_by(|a, b| a.0 == b.0);
    }
}

fn typecheck_fn(
    genv: &PackageTypeEnv,
    typer: &mut Typer,
    diagnostics: &mut Diagnostics,
    f: &hir::Fn,
    in_scope_traits: &[tast::TastIdent],
) {
    let mut local_env = LocalTypeEnv::new();
    local_env.set_in_scope_traits(in_scope_traits.to_vec());
    let tparams: Vec<tast::TastIdent> = f
        .generics
        .iter()
        .map(|g| tast::TastIdent(g.to_ident_name()))
        .collect();
    let mut bounds = init_trait_bounds(&tparams);
    extend_trait_bounds(genv, diagnostics, &mut bounds, &f.generic_bounds);
    normalize_trait_bounds(&mut bounds);
    local_env.set_tparam_trait_bounds(bounds);
    let param_types: Vec<(hir::LocalId, tast::Ty)> = f
        .params
        .iter()
        .map(|(name, ty)| {
            let ty = tast::Ty::from_hir(genv, ty, &tparams);
            (*name, ty)
        })
        .collect();

    let ret_ty = match &f.ret_ty {
        Some(ty) => tast::Ty::from_hir(genv, ty, &tparams),
        None => tast::Ty::TUnit,
    };

    local_env.set_tparams_env(&tparams);
    local_env.push_scope();
    for (id, ty) in param_types.iter() {
        local_env.insert_var(*id, ty.clone());
        typer.results.record_local_ty(*id, ty.clone());
    }
    typer.return_ty_stack.push(ret_ty.clone());
    let _typed_body = typer.check_block(genv, &mut local_env, diagnostics, &f.body, &ret_ty);
    let _ = typer.return_ty_stack.pop();
    local_env.pop_scope(diagnostics);
    local_env.clear_tparams_env();
    typer.tparam_trait_bounds = local_env
        .tparam_trait_bounds_map()
        .iter()
        .map(|(k, v)| (k.clone(), v.iter().map(|t| t.0.clone()).collect()))
        .collect();
    local_env.clear_tparam_trait_bounds();
    typer.solve(genv, diagnostics);
    typer.tparam_trait_bounds.clear();
    typer.validate_deferred_arithmetic_checks(diagnostics);
}

fn typecheck_impl_block(
    genv: &PackageTypeEnv,
    typer: &mut Typer,
    diagnostics: &mut Diagnostics,
    impl_block: &hir::ImplBlock,
    in_scope_traits: &[tast::TastIdent],
) {
    let impl_generics_tast: Vec<tast::TastIdent> = impl_block
        .generics
        .iter()
        .map(|g| tast::TastIdent(g.to_ident_name()))
        .collect();
    let for_ty = tast::Ty::from_hir(genv, &impl_block.for_type, &impl_generics_tast);
    for f in impl_block.methods.iter() {
        let f = match typer.hir_table.def(*f).clone() {
            hir::Def::Fn(func) => func,
            _ => continue,
        };
        let mut local_env = LocalTypeEnv::new();
        local_env.set_in_scope_traits(in_scope_traits.to_vec());

        // Combine impl generics and method generics
        let mut all_generics = impl_block.generics.clone();
        all_generics.extend(f.generics.clone());
        let all_generics_tast: Vec<tast::TastIdent> = all_generics
            .iter()
            .map(|g| tast::TastIdent(g.to_ident_name()))
            .collect();

        let mut bounds = init_trait_bounds(&all_generics_tast);
        extend_trait_bounds(genv, diagnostics, &mut bounds, &impl_block.generic_bounds);
        extend_trait_bounds(genv, diagnostics, &mut bounds, &f.generic_bounds);
        normalize_trait_bounds(&mut bounds);
        local_env.set_tparam_trait_bounds(bounds);

        let param_types: Vec<(hir::LocalId, tast::Ty)> = f
            .params
            .iter()
            .map(|(name, ty)| {
                let ty = tast::Ty::from_hir(genv, ty, &all_generics_tast);
                let ty = instantiate_self_ty(&ty, &for_ty);
                (*name, ty)
            })
            .collect();

        let ret_ty = match &f.ret_ty {
            Some(ty) => {
                let ty = tast::Ty::from_hir(genv, ty, &all_generics_tast);
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
        for (id, ty) in param_types.iter() {
            local_env.insert_var(*id, ty.clone());
            typer.results.record_local_ty(*id, ty.clone());
        }
        typer.return_ty_stack.push(ret_ty.clone());
        let _typed_body = typer.check_block(genv, &mut local_env, diagnostics, &f.body, &ret_ty);
        let _ = typer.return_ty_stack.pop();
        local_env.pop_scope(diagnostics);
        local_env.clear_tparams_env();
        typer.tparam_trait_bounds = local_env
            .tparam_trait_bounds_map()
            .iter()
            .map(|(k, v)| (k.clone(), v.iter().map(|t| t.0.clone()).collect()))
            .collect();
        local_env.clear_tparam_trait_bounds();
        typer.solve(genv, diagnostics);
        typer.tparam_trait_bounds.clear();
        typer.validate_deferred_arithmetic_checks(diagnostics);
    }
}
