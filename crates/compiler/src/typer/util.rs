use std::collections::HashSet;

use crate::{
    env::{GlobalTypeEnv, PackageTypeEnv},
    fir,
    tast::{self},
};
use diagnostics::{Severity, Stage};
use parser::{Diagnostic, Diagnostics};

pub(crate) fn validate_ty(
    genv: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    ty: &tast::Ty,
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
        | tast::Ty::TString => {}
        tast::Ty::TTuple { typs } => {
            for ty in typs.iter() {
                validate_ty(genv, diagnostics, ty, tparams);
            }
        }
        tast::Ty::TFunc { params, ret_ty } => {
            for param in params.iter() {
                validate_ty(genv, diagnostics, param, tparams);
            }
            validate_ty(genv, diagnostics, ret_ty.as_ref(), tparams);
        }
        tast::Ty::TVec { elem } => {
            validate_ty(genv, diagnostics, elem, tparams);
        }
        tast::Ty::TParam { name } => {
            if !tparams.contains(name) {
                diagnostics.push(Diagnostic::new(
                    Stage::Typer,
                    Severity::Error,
                    format!("Unknown type parameter {}", name),
                ));
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
                    diagnostics.push(Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Type {} expects {} type arguments, but got 0",
                            name,
                            enum_def.generics.len()
                        ),
                    ));
                }
            } else if let Some(struct_def) = env.structs().get(&ident) {
                if !struct_def.generics.is_empty() {
                    diagnostics.push(Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Type {} expects {} type arguments, but got 0",
                            name,
                            struct_def.generics.len()
                        ),
                    ));
                }
            } else if env.type_env.extern_types.contains_key(&resolved) {
                // Extern types do not have generics.
            } else {
                diagnostics.push(Diagnostic::new(
                    Stage::Typer,
                    Severity::Error,
                    format!("Unknown type constructor {}", name),
                ));
            }
        }
        tast::Ty::TApp { ty, args } => {
            for arg in args.iter() {
                validate_ty(genv, diagnostics, arg, tparams);
            }

            let base_name = ty.get_constr_name_unsafe();
            let (resolved, env) = resolve_type_name(genv, &base_name);
            let ident = tast::TastIdent::new(&resolved);

            if let Some(enum_def) = env.enums().get(&ident) {
                let expected = enum_def.generics.len();
                let actual = args.len();
                if expected != actual {
                    diagnostics.push(Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Type {} expects {} type arguments, but got {}",
                            base_name, expected, actual
                        ),
                    ));
                }
            } else if let Some(struct_def) = env.structs().get(&ident) {
                let expected = struct_def.generics.len();
                let actual = args.len();
                if expected != actual {
                    diagnostics.push(Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Type {} expects {} type arguments, but got {}",
                            base_name, expected, actual
                        ),
                    ));
                }
            } else if env.type_env.extern_types.contains_key(&resolved) {
                if !args.is_empty() {
                    diagnostics.push(Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Type {} does not accept type arguments, but got {}",
                            base_name,
                            args.len()
                        ),
                    ));
                }
            } else {
                diagnostics.push(Diagnostic::new(
                    Stage::Typer,
                    Severity::Error,
                    format!("Unknown type constructor {}", base_name),
                ));
            }
        }
        tast::Ty::TArray { elem, .. } => {
            validate_ty(genv, diagnostics, elem, tparams);
        }
        tast::Ty::TRef { elem } => {
            validate_ty(genv, diagnostics, elem, tparams);
        }
    }
}

fn validate_dyn_trait(genv: &PackageTypeEnv, diagnostics: &mut Diagnostics, trait_name: &str) {
    let Some((resolved, trait_env)) = resolve_trait_name(genv, trait_name) else {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!("Unknown trait {}", trait_name),
        ));
        return;
    };

    let Some(trait_def) = trait_env.trait_env.trait_defs.get(&resolved) else {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!("Unknown trait {}", trait_name),
        ));
        return;
    };

    for (method_name, scheme) in trait_def.methods.iter() {
        let tast::Ty::TFunc { params, ret_ty } = &scheme.ty else {
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!("Trait {}::{} is not a function", resolved, method_name),
            ));
            continue;
        };

        if params.is_empty() {
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Trait {}::{} is not dyn-safe: missing receiver parameter",
                    resolved, method_name
                ),
            ));
            continue;
        }

        if !is_self_ty(&params[0]) {
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Trait {}::{} is not dyn-safe: receiver must be Self",
                    resolved, method_name
                ),
            ));
        }

        for param in params.iter().skip(1) {
            if ty_contains_self(param) {
                diagnostics.push(Diagnostic::new(
                    Stage::Typer,
                    Severity::Error,
                    format!(
                        "Trait {}::{} is not dyn-safe: Self is not allowed in non-receiver parameters",
                        resolved, method_name
                    ),
                ));
                break;
            }
        }

        if ty_contains_self(ret_ty.as_ref()) {
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Trait {}::{} is not dyn-safe: Self is not allowed in return type",
                    resolved, method_name
                ),
            ));
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
        tast::Ty::TVec { elem } => ty_contains_self(elem),
        tast::Ty::TRef { elem } => ty_contains_self(elem),
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
        | tast::Ty::TParam { .. } => false,
    }
}

impl tast::Ty {
    pub(crate) fn from_fir(
        genv: &PackageTypeEnv,
        fir_ty: &fir::TypeExpr,
        tparams_env: &[tast::TastIdent],
    ) -> Self {
        match fir_ty {
            fir::TypeExpr::TUnit => Self::TUnit,
            fir::TypeExpr::TBool => Self::TBool,
            fir::TypeExpr::TInt8 => Self::TInt8,
            fir::TypeExpr::TInt16 => Self::TInt16,
            fir::TypeExpr::TInt32 => Self::TInt32,
            fir::TypeExpr::TInt64 => Self::TInt64,
            fir::TypeExpr::TUint8 => Self::TUint8,
            fir::TypeExpr::TUint16 => Self::TUint16,
            fir::TypeExpr::TUint32 => Self::TUint32,
            fir::TypeExpr::TUint64 => Self::TUint64,
            fir::TypeExpr::TFloat32 => Self::TFloat32,
            fir::TypeExpr::TFloat64 => Self::TFloat64,
            fir::TypeExpr::TString => Self::TString,
            fir::TypeExpr::TTuple { typs } => Self::TTuple {
                typs: typs
                    .iter()
                    .map(|ty| Self::from_fir(genv, ty, tparams_env))
                    .collect(),
            },
            fir::TypeExpr::TCon { path } => {
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
            fir::TypeExpr::TDyn { trait_path } => {
                let name = trait_path.display();
                let (resolved, _env) = resolve_type_name(genv, &name);
                Self::TDyn { trait_name: resolved }
            }
            fir::TypeExpr::TApp { ty, args } => {
                if let fir::TypeExpr::TCon { path } = ty.as_ref()
                    && path.package.is_none()
                    && path.len() == 1
                    && path.last_ident().is_some_and(|name| name == "Ref")
                    && args.len() == 1
                {
                    return Self::TRef {
                        elem: Box::new(Self::from_fir(genv, &args[0], tparams_env)),
                    };
                }
                if let fir::TypeExpr::TCon { path } = ty.as_ref()
                    && path.package.is_none()
                    && path.len() == 1
                    && path.last_ident().is_some_and(|name| name == "Vec")
                    && args.len() == 1
                {
                    return Self::TVec {
                        elem: Box::new(Self::from_fir(genv, &args[0], tparams_env)),
                    };
                }
                Self::TApp {
                    ty: Box::new(Self::from_fir(genv, ty, tparams_env)),
                    args: args
                        .iter()
                        .map(|ty| Self::from_fir(genv, ty, tparams_env))
                        .collect(),
                }
            }
            fir::TypeExpr::TArray { len, elem } => Self::TArray {
                len: *len,
                elem: Box::new(Self::from_fir(genv, elem, tparams_env)),
            },
            fir::TypeExpr::TFunc { params, ret_ty } => Self::TFunc {
                params: params
                    .iter()
                    .map(|ty| Self::from_fir(genv, ty, tparams_env))
                    .collect(),
                ret_ty: Box::new(Self::from_fir(genv, ret_ty, tparams_env)),
            },
        }
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
        if package == "Builtin" {
            return (rest.to_string(), genv.current());
        }
        if package == "Main" && genv.package == "Main" {
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

    if genv.package == "Main" || genv.package == "Builtin" {
        (name.to_string(), genv.current())
    } else {
        (format!("{}::{}", genv.package, name), genv.current())
    }
}

pub(crate) fn resolve_trait_name<'a>(
    genv: &'a PackageTypeEnv,
    name: &str,
) -> Option<(String, &'a GlobalTypeEnv)> {
    let (resolved, env) = resolve_type_name(genv, name);
    if env.trait_env.trait_defs.contains_key(&resolved) {
        Some((resolved, env))
    } else {
        None
    }
}

pub(crate) fn type_param_name_set(tparams: &[fir::FirIdent]) -> HashSet<String> {
    tparams.iter().map(|param| param.to_ident_name()).collect()
}
