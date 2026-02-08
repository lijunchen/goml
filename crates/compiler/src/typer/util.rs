use std::collections::HashSet;

use crate::{
    env::{GlobalTypeEnv, PackageTypeEnv},
    hir,
    package_names::{BUILTIN_PACKAGE, is_special_unqualified_package},
    tast::{self},
};
use diagnostics::{Severity, Stage};
use parser::{Diagnostic, Diagnostics};
use text_size::TextRange;

pub(crate) fn push_error(diagnostics: &mut Diagnostics, message: impl Into<String>) {
    diagnostics.push(Diagnostic::new(Stage::Typer, Severity::Error, message));
}

pub(crate) fn push_error_with_range(
    diagnostics: &mut Diagnostics,
    message: impl Into<String>,
    range: Option<TextRange>,
) {
    diagnostics.push(Diagnostic::new(Stage::Typer, Severity::Error, message).with_range(range));
}

pub(crate) fn push_ice(diagnostics: &mut Diagnostics, message: impl Into<String>) {
    push_error(diagnostics, format!("Internal error: {}", message.into()));
}

pub(crate) fn format_ty_for_diag(ty: &tast::Ty) -> String {
    match ty {
        tast::Ty::TVar(_) => "unknown".to_string(),
        tast::Ty::TUnit => "unit".to_string(),
        tast::Ty::TBool => "bool".to_string(),
        tast::Ty::TInt8 => "int8".to_string(),
        tast::Ty::TInt16 => "int16".to_string(),
        tast::Ty::TInt32 => "int32".to_string(),
        tast::Ty::TInt64 => "int64".to_string(),
        tast::Ty::TUint8 => "uint8".to_string(),
        tast::Ty::TUint16 => "uint16".to_string(),
        tast::Ty::TUint32 => "uint32".to_string(),
        tast::Ty::TUint64 => "uint64".to_string(),
        tast::Ty::TFloat32 => "float32".to_string(),
        tast::Ty::TFloat64 => "float64".to_string(),
        tast::Ty::TString => "string".to_string(),
        tast::Ty::TChar => "char".to_string(),
        tast::Ty::TTuple { typs } => {
            let items = typs
                .iter()
                .map(format_ty_for_diag)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({})", items)
        }
        tast::Ty::TEnum { name } | tast::Ty::TStruct { name } => name.clone(),
        tast::Ty::TDyn { trait_name } => format!("dyn {}", trait_name),
        tast::Ty::TApp { ty, args } => {
            let base = format_ty_for_diag(ty.as_ref());
            if args.is_empty() {
                base
            } else {
                let args = args
                    .iter()
                    .map(format_ty_for_diag)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}[{}]", base, args)
            }
        }
        tast::Ty::TArray { len, elem } => {
            let rendered_len = if *len == tast::ARRAY_WILDCARD_LEN {
                "_".to_string()
            } else {
                len.to_string()
            };
            format!("[{}; {}]", format_ty_for_diag(elem.as_ref()), rendered_len)
        }
        tast::Ty::TSlice { elem } => format!("Slice[{}]", format_ty_for_diag(elem.as_ref())),
        tast::Ty::TVec { elem } => format!("Vec[{}]", format_ty_for_diag(elem.as_ref())),
        tast::Ty::TRef { elem } => format!("Ref[{}]", format_ty_for_diag(elem.as_ref())),
        tast::Ty::THashMap { key, value } => format!(
            "HashMap[{}, {}]",
            format_ty_for_diag(key.as_ref()),
            format_ty_for_diag(value.as_ref())
        ),
        tast::Ty::TParam { name } => name.clone(),
        tast::Ty::TFunc { params, ret_ty } => {
            let params = params
                .iter()
                .map(format_ty_for_diag)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({}) -> {}", params, format_ty_for_diag(ret_ty.as_ref()))
        }
    }
}

pub(crate) fn try_constr_name(ty: &tast::Ty) -> Option<String> {
    match ty {
        tast::Ty::TEnum { name } | tast::Ty::TStruct { name } => Some(name.clone()),
        tast::Ty::TApp { ty, .. } => try_constr_name(ty),
        tast::Ty::TSlice { .. } => Some("Slice".to_string()),
        tast::Ty::TVec { .. } => Some("Vec".to_string()),
        tast::Ty::TRef { .. } => Some("Ref".to_string()),
        tast::Ty::THashMap { .. } => Some("HashMap".to_string()),
        _ => None,
    }
}

pub(crate) fn validate_ty(
    genv: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    ty: &tast::Ty,
    range: Option<TextRange>,
    tparams: &HashSet<String>,
) {
    match ty {
        tast::Ty::TVar(_) => {}
        tast::Ty::TUnit
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
        | tast::Ty::TString
        | tast::Ty::TChar => {}
        tast::Ty::TTuple { typs } => {
            for ty in typs.iter() {
                validate_ty(genv, diagnostics, ty, range, tparams);
            }
        }
        tast::Ty::TFunc { params, ret_ty } => {
            for param in params.iter() {
                validate_ty(genv, diagnostics, param, range, tparams);
            }
            validate_ty(genv, diagnostics, ret_ty.as_ref(), range, tparams);
        }
        tast::Ty::TSlice { elem } => {
            validate_ty(genv, diagnostics, elem, range, tparams);
        }
        tast::Ty::TVec { elem } => {
            validate_ty(genv, diagnostics, elem, range, tparams);
        }
        tast::Ty::THashMap { key, value } => {
            validate_ty(genv, diagnostics, key, range, tparams);
            validate_ty(genv, diagnostics, value, range, tparams);
        }
        tast::Ty::TParam { name } => {
            if !tparams.contains(name) {
                push_error_with_range(
                    diagnostics,
                    format!("Unknown type parameter {}", name),
                    range,
                );
            }
        }
        tast::Ty::TDyn { trait_name } => {
            validate_dyn_trait(genv, diagnostics, trait_name);
        }
        tast::Ty::TEnum { name } | tast::Ty::TStruct { name } => {
            if name == "Self" {
                return;
            }
            let (resolved, env) = resolve_type_name(genv, name);
            let ident = tast::TastIdent::new(&resolved);
            if let Some(enum_def) = env.enums().get(&ident) {
                if !enum_def.generics.is_empty() {
                    push_error_with_range(
                        diagnostics,
                        format!(
                            "Type {} expects {} type arguments, but got 0",
                            name,
                            enum_def.generics.len()
                        ),
                        range,
                    );
                }
            } else if let Some(struct_def) = env.structs().get(&ident) {
                if !struct_def.generics.is_empty() {
                    push_error_with_range(
                        diagnostics,
                        format!(
                            "Type {} expects {} type arguments, but got 0",
                            name,
                            struct_def.generics.len()
                        ),
                        range,
                    );
                }
            } else if env.type_env.extern_types.contains_key(&resolved) {
                // Extern types do not have generics.
            } else {
                push_error_with_range(
                    diagnostics,
                    format!("Unknown type constructor {}", name),
                    range,
                );
            }
        }
        tast::Ty::TApp { ty, args } => {
            for arg in args.iter() {
                validate_ty(genv, diagnostics, arg, range, tparams);
            }

            let Some(base_name) = try_constr_name(ty.as_ref()) else {
                push_error_with_range(
                    diagnostics,
                    format!(
                        "Expected a type constructor, got {}",
                        format_ty_for_diag(ty.as_ref())
                    ),
                    range,
                );
                return;
            };
            let (resolved, env) = resolve_type_name(genv, &base_name);
            let ident = tast::TastIdent::new(&resolved);

            if let Some(enum_def) = env.enums().get(&ident) {
                let expected = enum_def.generics.len();
                let actual = args.len();
                if expected != actual {
                    push_error_with_range(
                        diagnostics,
                        format!(
                            "Type {} expects {} type arguments, but got {}",
                            base_name, expected, actual
                        ),
                        range,
                    );
                }
            } else if let Some(struct_def) = env.structs().get(&ident) {
                let expected = struct_def.generics.len();
                let actual = args.len();
                if expected != actual {
                    push_error_with_range(
                        diagnostics,
                        format!(
                            "Type {} expects {} type arguments, but got {}",
                            base_name, expected, actual
                        ),
                        range,
                    );
                }
            } else if env.type_env.extern_types.contains_key(&resolved) {
                if !args.is_empty() {
                    push_error_with_range(
                        diagnostics,
                        format!(
                            "Type {} does not accept type arguments, but got {}",
                            base_name,
                            args.len()
                        ),
                        range,
                    );
                }
            } else {
                push_error_with_range(
                    diagnostics,
                    format!("Unknown type constructor {}", base_name),
                    range,
                );
            }
        }
        tast::Ty::TArray { elem, .. } => {
            validate_ty(genv, diagnostics, elem, range, tparams);
        }
        tast::Ty::TRef { elem } => {
            validate_ty(genv, diagnostics, elem, range, tparams);
        }
    }
}

fn validate_dyn_trait(genv: &PackageTypeEnv, diagnostics: &mut Diagnostics, trait_name: &str) {
    let Some((resolved, trait_env)) = resolve_trait_name(genv, trait_name) else {
        push_error(diagnostics, format!("Unknown trait {}", trait_name));
        return;
    };

    let Some(trait_def) = trait_env.trait_env.trait_defs.get(&resolved) else {
        push_error(diagnostics, format!("Unknown trait {}", trait_name));
        return;
    };

    for (method_name, scheme) in trait_def.methods.iter() {
        let tast::Ty::TFunc { params, ret_ty } = &scheme.ty else {
            push_error(
                diagnostics,
                format!("Trait {}::{} is not a function", resolved, method_name),
            );
            continue;
        };

        if params.is_empty() {
            push_error(
                diagnostics,
                format!(
                    "Trait {}::{} is not dyn-safe: missing receiver parameter",
                    resolved, method_name
                ),
            );
            continue;
        }

        if let Some(self_param) = params.first()
            && !is_self_ty(self_param)
        {
            push_error(
                diagnostics,
                format!(
                    "Trait {}::{} is not dyn-safe: receiver must be Self",
                    resolved, method_name
                ),
            );
        }

        for param in params.iter().skip(1) {
            if ty_contains_self(param) {
                push_error(
                    diagnostics,
                    format!(
                        "Trait {}::{} is not dyn-safe: Self is not allowed in non-receiver parameters",
                        resolved, method_name
                    ),
                );
                break;
            }
        }

        if ty_contains_self(ret_ty.as_ref()) {
            push_error(
                diagnostics,
                format!(
                    "Trait {}::{} is not dyn-safe: Self is not allowed in return type",
                    resolved, method_name
                ),
            );
        }
    }
}

fn is_self_ty(ty: &tast::Ty) -> bool {
    matches!(ty, tast::Ty::TStruct { name } if name == "Self")
}

fn ty_contains_self(ty: &tast::Ty) -> bool {
    match ty {
        tast::Ty::TStruct { name } => name == "Self",
        tast::Ty::TTuple { typs } => typs.iter().any(ty_contains_self),
        tast::Ty::TApp { ty, args } => ty_contains_self(ty) || args.iter().any(ty_contains_self),
        tast::Ty::TArray { elem, .. } => ty_contains_self(elem),
        tast::Ty::TSlice { elem } => ty_contains_self(elem),
        tast::Ty::TVec { elem } => ty_contains_self(elem),
        tast::Ty::TRef { elem } => ty_contains_self(elem),
        tast::Ty::THashMap { key, value } => ty_contains_self(key) || ty_contains_self(value),
        tast::Ty::TFunc { params, ret_ty } => {
            params.iter().any(ty_contains_self) || ty_contains_self(ret_ty)
        }
        tast::Ty::TEnum { .. }
        | tast::Ty::TDyn { .. }
        | tast::Ty::TVar(_)
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
        | tast::Ty::TString
        | tast::Ty::TChar
        | tast::Ty::TParam { .. } => false,
    }
}

impl tast::Ty {
    pub(crate) fn from_hir(
        genv: &PackageTypeEnv,
        hir_ty: &hir::TypeExpr,
        tparams_env: &[tast::TastIdent],
    ) -> Self {
        match hir_ty {
            hir::TypeExpr::TUnit => Self::TUnit,
            hir::TypeExpr::TBool => Self::TBool,
            hir::TypeExpr::TInt8 => Self::TInt8,
            hir::TypeExpr::TInt16 => Self::TInt16,
            hir::TypeExpr::TInt32 => Self::TInt32,
            hir::TypeExpr::TInt64 => Self::TInt64,
            hir::TypeExpr::TUint8 => Self::TUint8,
            hir::TypeExpr::TUint16 => Self::TUint16,
            hir::TypeExpr::TUint32 => Self::TUint32,
            hir::TypeExpr::TUint64 => Self::TUint64,
            hir::TypeExpr::TFloat32 => Self::TFloat32,
            hir::TypeExpr::TFloat64 => Self::TFloat64,
            hir::TypeExpr::TString => Self::TString,
            hir::TypeExpr::TChar => Self::TChar,
            hir::TypeExpr::TTuple { typs } => Self::TTuple {
                typs: typs
                    .iter()
                    .map(|ty| Self::from_hir(genv, ty, tparams_env))
                    .collect(),
            },
            hir::TypeExpr::TCon { path } => {
                let name = path.display();
                if path.package.is_none()
                    && path.len() == 1
                    && tparams_env.iter().any(|param| param.0 == name)
                {
                    Self::TParam { name }
                } else {
                    let (resolved, env) = resolve_type_name(genv, &name);
                    let ident = tast::TastIdent::new(&resolved);
                    if env.enums().contains_key(&ident) {
                        Self::TEnum { name: resolved }
                    } else if env.structs().contains_key(&ident)
                        || env.type_env.extern_types.contains_key(&resolved)
                    {
                        Self::TStruct { name: resolved }
                    } else {
                        // FIXME
                        Self::TStruct { name: resolved }
                    }
                }
            }
            hir::TypeExpr::TDyn { trait_path } => {
                let name = trait_path.display();
                let (resolved, _env) = normalize_trait_name(genv, &name);
                Self::TDyn {
                    trait_name: resolved,
                }
            }
            hir::TypeExpr::TApp { ty, args } => {
                if let hir::TypeExpr::TCon { path } = ty.as_ref()
                    && path.package.is_none()
                    && path.len() == 1
                    && path.last_ident().is_some_and(|name| name == "Slice")
                    && args.len() == 1
                    && let Some(arg0) = args.first()
                {
                    return Self::TSlice {
                        elem: Box::new(Self::from_hir(genv, arg0, tparams_env)),
                    };
                }
                if let hir::TypeExpr::TCon { path } = ty.as_ref()
                    && path.package.is_none()
                    && path.len() == 1
                    && path.last_ident().is_some_and(|name| name == "Ref")
                    && args.len() == 1
                    && let Some(arg0) = args.first()
                {
                    return Self::TRef {
                        elem: Box::new(Self::from_hir(genv, arg0, tparams_env)),
                    };
                }
                if let hir::TypeExpr::TCon { path } = ty.as_ref()
                    && path.package.is_none()
                    && path.len() == 1
                    && path.last_ident().is_some_and(|name| name == "Vec")
                    && args.len() == 1
                    && let Some(arg0) = args.first()
                {
                    return Self::TVec {
                        elem: Box::new(Self::from_hir(genv, arg0, tparams_env)),
                    };
                }
                if let hir::TypeExpr::TCon { path } = ty.as_ref()
                    && path.package.is_none()
                    && path.len() == 1
                    && path.last_ident().is_some_and(|name| name == "HashMap")
                    && args.len() == 2
                {
                    let mut it = args.iter();
                    let key = it.next().unwrap();
                    let value = it.next().unwrap();
                    return Self::THashMap {
                        key: Box::new(Self::from_hir(genv, key, tparams_env)),
                        value: Box::new(Self::from_hir(genv, value, tparams_env)),
                    };
                }
                Self::TApp {
                    ty: Box::new(Self::from_hir(genv, ty, tparams_env)),
                    args: args
                        .iter()
                        .map(|ty| Self::from_hir(genv, ty, tparams_env))
                        .collect(),
                }
            }
            hir::TypeExpr::TArray { len, elem } => Self::TArray {
                len: *len,
                elem: Box::new(Self::from_hir(genv, elem, tparams_env)),
            },
            hir::TypeExpr::TFunc { params, ret_ty } => Self::TFunc {
                params: params
                    .iter()
                    .map(|ty| Self::from_hir(genv, ty, tparams_env))
                    .collect(),
                ret_ty: Box::new(Self::from_hir(genv, ret_ty, tparams_env)),
            },
        }
    }
}

pub(crate) fn type_expr_range(ty: &hir::TypeExpr) -> Option<TextRange> {
    match ty {
        hir::TypeExpr::TCon { path } => path
            .path
            .last()
            .and_then(|segment| segment.range())
            .or_else(|| {
                path.path
                    .segments()
                    .iter()
                    .find_map(|segment| segment.range())
            }),
        hir::TypeExpr::TDyn { trait_path } => trait_path
            .path
            .last()
            .and_then(|segment| segment.range())
            .or_else(|| {
                trait_path
                    .path
                    .segments()
                    .iter()
                    .find_map(|segment| segment.range())
            }),
        hir::TypeExpr::TApp { ty, args } => {
            type_expr_range(ty).or_else(|| args.iter().find_map(type_expr_range))
        }
        hir::TypeExpr::TTuple { typs } => typs.iter().find_map(type_expr_range),
        hir::TypeExpr::TArray { elem, .. } => type_expr_range(elem),
        hir::TypeExpr::TFunc { params, ret_ty } => params
            .iter()
            .find_map(type_expr_range)
            .or_else(|| type_expr_range(ret_ty)),
        hir::TypeExpr::TUnit
        | hir::TypeExpr::TBool
        | hir::TypeExpr::TInt8
        | hir::TypeExpr::TInt16
        | hir::TypeExpr::TInt32
        | hir::TypeExpr::TInt64
        | hir::TypeExpr::TUint8
        | hir::TypeExpr::TUint16
        | hir::TypeExpr::TUint32
        | hir::TypeExpr::TUint64
        | hir::TypeExpr::TFloat32
        | hir::TypeExpr::TFloat64
        | hir::TypeExpr::TString
        | hir::TypeExpr::TChar => None,
    }
}

pub(crate) fn resolve_type_name<'a>(
    genv: &'a PackageTypeEnv,
    name: &str,
) -> (String, &'a GlobalTypeEnv) {
    if name == "Self" {
        return (name.to_string(), genv.current());
    }

    if let Some((package, rest)) = name.split_once("::") {
        if package == BUILTIN_PACKAGE {
            return (rest.to_string(), genv.builtins());
        }
        if is_special_unqualified_package(package) && is_special_unqualified_package(&genv.package)
        {
            return (rest.to_string(), genv.current());
        }
        if package == genv.package {
            return (name.to_string(), genv.current());
        }
        if let Some(dep) = genv.deps.get(package) {
            return (name.to_string(), dep);
        }
        return (name.to_string(), genv.current());
    }

    let current_name = if is_special_unqualified_package(&genv.package) {
        name.to_string()
    } else {
        format!("{}::{}", genv.package, name)
    };

    let current_env = genv.current();
    if type_constructor_exists(current_env, &current_name) {
        return (current_name, current_env);
    }

    let builtin_env = genv.builtins();
    if type_constructor_exists(builtin_env, name) {
        return (name.to_string(), builtin_env);
    }

    (current_name, current_env)
}

fn type_constructor_exists(env: &GlobalTypeEnv, name: &str) -> bool {
    let ident = tast::TastIdent::new(name);
    env.enums().contains_key(&ident)
        || env.structs().contains_key(&ident)
        || env.type_env.extern_types.contains_key(name)
        || env
            .trait_env
            .inherent_impls
            .contains_key(&crate::env::InherentImplKey::Constr(name.to_string()))
}

pub(crate) fn resolve_trait_name<'a>(
    genv: &'a PackageTypeEnv,
    name: &str,
) -> Option<(String, &'a GlobalTypeEnv)> {
    let (resolved, env) = normalize_trait_name(genv, name);
    env.trait_env
        .trait_defs
        .contains_key(&resolved)
        .then_some((resolved, env))
}

pub(crate) fn normalize_trait_name<'a>(
    genv: &'a PackageTypeEnv,
    name: &str,
) -> (String, &'a GlobalTypeEnv) {
    if name == "Self" {
        return (name.to_string(), genv.current());
    }

    if let Some((package, rest)) = name.split_once("::") {
        if package == BUILTIN_PACKAGE {
            return (rest.to_string(), genv.builtins());
        }
        if is_special_unqualified_package(package) && is_special_unqualified_package(&genv.package)
        {
            return (rest.to_string(), genv.current());
        }
        if package == genv.package {
            return (name.to_string(), genv.current());
        }
        if let Some(dep) = genv.deps.get(package) {
            return (name.to_string(), dep);
        }
        return (name.to_string(), genv.current());
    }

    let current_name = if is_special_unqualified_package(&genv.package) {
        name.to_string()
    } else {
        format!("{}::{}", genv.package, name)
    };

    let current_env = genv.current();
    if current_env.trait_env.trait_defs.contains_key(&current_name) {
        return (current_name, current_env);
    }

    let builtin_env = genv.builtins();
    if builtin_env.trait_env.trait_defs.contains_key(name) {
        return (name.to_string(), builtin_env);
    }

    (current_name, current_env)
}

pub(crate) fn type_param_name_set(tparams: &[hir::HirIdent]) -> HashSet<String> {
    tparams.iter().map(|param| param.to_ident_name()).collect()
}
