use crate::{
    anf::{self, GlobalAnfEnv},
    common::{Constructor, Prim},
    env::{EnumDef, FnOrigin, Gensym, GlobalTypeEnv, InherentImplKey, StructDef},
    go::goast::{self, go_type_name_for, tast_ty_to_go_type},
    go::mangle::{encode_ty, go_dyn_struct_name, go_generated_ident, go_ident, go_user_type_name},
    lift::{GlobalLiftEnv, is_closure_env_struct},
    names::{inherent_method_fn_name, parse_trait_impl_fn_name, trait_impl_fn_name, ty_compact},
    package_names::{ENTRY_FUNCTION, ENTRY_WRAPPER_FUNCTION},
    tast::{self, TastIdent},
};

use indexmap::IndexSet;
use std::collections::{HashMap, HashSet};

use super::goty;
use super::runtime;

#[derive(Debug, Clone)]
pub struct GlobalGoEnv {
    pub genv: GlobalTypeEnv,
    pub liftenv: GlobalLiftEnv,
    pub toplevel_funcs: HashSet<String>,
    pub colliding_callable_idents: HashSet<String>,
    pub callable_go_names: HashMap<String, String>,
    pub callable_go_name_set: HashSet<String>,
    pub dyn_vtable_ctor_go_names: HashMap<(String, String), String>,
    pub dyn_wrap_go_names: HashMap<(String, String, String), String>,
}

impl Default for GlobalGoEnv {
    fn default() -> Self {
        let genv = crate::builtins::builtin_env();
        let monoenv = crate::mono::GlobalMonoEnv::from_genv(genv.clone());
        let liftenv = crate::lift::GlobalLiftEnv::from_monoenv(monoenv);
        Self {
            genv,
            liftenv,
            toplevel_funcs: HashSet::new(),
            colliding_callable_idents: HashSet::new(),
            callable_go_names: HashMap::new(),
            callable_go_name_set: HashSet::new(),
            dyn_vtable_ctor_go_names: HashMap::new(),
            dyn_wrap_go_names: HashMap::new(),
        }
    }
}

impl GlobalGoEnv {
    pub fn from_anf_env(anfenv: GlobalAnfEnv) -> Self {
        let liftenv = anfenv.liftenv.clone();
        let genv = liftenv.monoenv.genv.clone();
        Self {
            genv,
            liftenv,
            toplevel_funcs: HashSet::new(),
            colliding_callable_idents: HashSet::new(),
            callable_go_names: HashMap::new(),
            callable_go_name_set: HashSet::new(),
            dyn_vtable_ctor_go_names: HashMap::new(),
            dyn_wrap_go_names: HashMap::new(),
        }
    }

    pub fn enums(&self) -> impl Iterator<Item = (&TastIdent, &EnumDef)> {
        self.liftenv.enums()
    }

    pub fn structs(&self) -> impl Iterator<Item = (&TastIdent, &StructDef)> {
        self.liftenv.structs()
    }

    /// Lookup the apply method type for a closure environment struct via inherent_impls.
    pub fn closure_apply_method(&self, closure_ty: &tast::Ty) -> Option<tast::Ty> {
        let tast::Ty::TStruct { name } = closure_ty else {
            return None;
        };
        if !is_closure_env_struct(name) {
            unreachable!(
                "closure_apply_method called on non-closure environment struct {:?}",
                closure_ty
            );
        }
        self.liftenv
            .inherent_impls()
            .find(|(k, _)| matches!(k, InherentImplKey::Exact(ty) if ty == closure_ty))
            .and_then(|(_, impl_def)| impl_def.methods.get("apply"))
            .map(|scheme| scheme.ty.clone())
    }

    pub fn get_enum(&self, name: &TastIdent) -> Option<&EnumDef> {
        self.enums().find(|(k, _)| *k == name).map(|(_, v)| v)
    }

    pub fn get_struct(&self, name: &TastIdent) -> Option<&StructDef> {
        self.liftenv.get_struct(name)
    }
}

fn runtime_builtin_available(goenv: &GlobalGoEnv, name: &str) -> bool {
    if goenv.toplevel_funcs.contains(name) {
        return false;
    }

    goenv
        .genv
        .value_env
        .funcs
        .get(name)
        .map(|scheme| scheme.origin)
        == Some(FnOrigin::Builtin)
}

fn runtime_generated_function_name(name: &str) -> bool {
    matches!(
        name,
        "unit_to_string"
            | "bool_to_string"
            | "string_len"
            | "string_get"
            | "char_to_string"
            | "int8_to_string"
            | "int16_to_string"
            | "int32_to_string"
            | "int64_to_string"
            | "uint8_to_string"
            | "uint16_to_string"
            | "uint32_to_string"
            | "uint64_to_string"
            | "float32_to_string"
            | "float64_to_string"
            | "int8_hash"
            | "int16_hash"
            | "int32_hash"
            | "int64_hash"
            | "uint8_hash"
            | "uint16_hash"
            | "uint32_hash"
            | "float32_hash"
            | "float64_hash"
            | "char_hash"
            | "string_hash"
            | "string_print"
            | "string_println"
            | "missing"
    )
}

fn go_toplevel_func_name(goenv: &GlobalGoEnv, name: &str) -> String {
    if let Some(go_name) = goenv.callable_go_names.get(name) {
        return go_name.clone();
    }
    resolve_toplevel_func_name(goenv, name)
}

fn resolve_toplevel_func_name(goenv: &GlobalGoEnv, name: &str) -> String {
    let ident = go_ident(name);
    let is_callable = goenv.toplevel_funcs.contains(name);
    if is_callable && go_callable_name_collides(goenv, &ident) {
        go_unique_toplevel_func_name(name)
    } else if is_callable
        && (ident == "init"
            || runtime_generated_function_name(name)
            || is_generated_tuple_type_name(&ident)
            || go_toplevel_func_name_collides_with_type(goenv, &ident))
    {
        format!("_goml_user_{}", ident)
    } else {
        ident
    }
}

fn collect_callable_go_names(goenv: &GlobalGoEnv) -> HashMap<String, String> {
    goenv
        .toplevel_funcs
        .iter()
        .map(|name| (name.clone(), resolve_toplevel_func_name(goenv, name)))
        .collect()
}

fn go_callable_name_collides(goenv: &GlobalGoEnv, ident: &str) -> bool {
    goenv.colliding_callable_idents.contains(ident)
}

fn collect_colliding_callable_idents(goenv: &GlobalGoEnv) -> HashSet<String> {
    let mut counts = HashMap::new();
    let mut colliding = HashSet::new();
    for name in goenv.toplevel_funcs.iter() {
        let ident = go_ident(name);
        let count = counts.entry(ident.clone()).or_insert(0usize);
        *count += 1;
        if *count > 1 {
            colliding.insert(ident);
        }
    }
    colliding
}

fn go_unique_toplevel_func_name(name: &str) -> String {
    let mut out = String::from("_goml_fn");
    for byte in name.as_bytes() {
        use std::fmt::Write;
        write!(&mut out, "_{:02x}", byte).unwrap();
    }
    out
}

fn go_toplevel_func_name_collides_with_type(goenv: &GlobalGoEnv, name: &str) -> bool {
    goenv
        .structs()
        .any(|(struct_name, _)| go_user_type_name(&struct_name.0) == name)
        || goenv
            .enums()
            .any(|(enum_name, _)| go_user_type_name(&enum_name.0) == name)
}

fn go_value_name(goenv: &GlobalGoEnv, name: &str) -> String {
    if goenv.toplevel_funcs.contains(name) {
        go_toplevel_func_name(goenv, name)
    } else {
        let ident = go_ident(name);
        if is_generated_tuple_type_name(&ident)
            && go_toplevel_func_name_collides_with_type(goenv, &ident)
        {
            format!("_goml_user_{}", ident)
        } else {
            ident
        }
    }
}

fn go_literal_from_primitive(value: &Prim, ty: &tast::Ty) -> goast::Expr {
    if matches!(value, Prim::Unit { .. }) {
        return goast::Expr::Unit {
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(bool_value) = value.as_bool() {
        return goast::Expr::Bool {
            value: bool_value,
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(str_value) = value.as_str() {
        return goast::Expr::String {
            value: str_value.to_string(),
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(ch) = value.as_char() {
        return goast::Expr::Int {
            value: (ch as u32).to_string(),
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(v) = value.as_int8() {
        return goast::Expr::Int {
            value: v.to_string(),
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(v) = value.as_int16() {
        return goast::Expr::Int {
            value: v.to_string(),
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(v) = value.as_int32() {
        return goast::Expr::Int {
            value: v.to_string(),
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(v) = value.as_int64() {
        return goast::Expr::Int {
            value: v.to_string(),
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(v) = value.as_uint8() {
        return goast::Expr::Int {
            value: v.to_string(),
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(v) = value.as_uint16() {
        return goast::Expr::Int {
            value: v.to_string(),
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(v) = value.as_uint32() {
        return goast::Expr::Int {
            value: v.to_string(),
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(v) = value.as_uint64() {
        return goast::Expr::Int {
            value: v.to_string(),
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(v) = value.as_float32() {
        return goast::Expr::Float {
            value: v as f64,
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let Some(v) = value.as_float64() {
        return goast::Expr::Float {
            value: v,
            ty: tast_ty_to_go_type(ty),
        };
    }

    panic!(
        "Unsupported primitive literal {:?} for Go code generation with type {:?}",
        value, ty
    );
}

fn compile_imm(goenv: &GlobalGoEnv, imm: &anf::ImmExpr) -> goast::Expr {
    match imm {
        anf::ImmExpr::Var { id, .. } => goast::Expr::Var {
            name: go_value_name(goenv, &id.0),
            ty: tast_ty_to_go_type(&imm_ty(imm)),
        },
        anf::ImmExpr::Prim { value, .. } => {
            let ty = imm_ty(imm);
            go_literal_from_primitive(value, &ty)
        }
        anf::ImmExpr::Tag { index, ty } => {
            if enum_is_tag_only(goenv, ty) {
                return variant_const_expr_by_index(goenv, ty, *index);
            }
            goast::Expr::StructLiteral {
                fields: vec![],
                ty: variant_ty_by_index(goenv, ty, *index),
            }
        }
    }
}

fn imm_ty(imm: &anf::ImmExpr) -> tast::Ty {
    match imm {
        anf::ImmExpr::Var { ty, .. }
        | anf::ImmExpr::Prim { ty, .. }
        | anf::ImmExpr::Tag { ty, .. } => ty.clone(),
    }
}

#[allow(dead_code)]
fn value_expr_ty(expr: &anf::ValueExpr) -> tast::Ty {
    match expr {
        anf::ValueExpr::Imm(imm) => imm_ty(imm),
        anf::ValueExpr::Constr { ty, .. }
        | anf::ValueExpr::Tuple { ty, .. }
        | anf::ValueExpr::Array { ty, .. }
        | anf::ValueExpr::ConstrGet { ty, .. }
        | anf::ValueExpr::Unary { ty, .. }
        | anf::ValueExpr::Binary { ty, .. }
        | anf::ValueExpr::Assign { ty, .. }
        | anf::ValueExpr::Call { ty, .. }
        | anf::ValueExpr::ToDyn { ty, .. }
        | anf::ValueExpr::DynCall { ty, .. }
        | anf::ValueExpr::Go { ty, .. }
        | anf::ValueExpr::Proj { ty, .. } => ty.clone(),
    }
}

pub(crate) fn variant_symbol_name(
    goenv: &GlobalGoEnv,
    enum_name: &str,
    variant_name: &str,
) -> String {
    variant_symbol_name_for_go_enum(goenv, &go_user_type_name(enum_name), variant_name)
}

fn variant_symbol_name_for_go_enum(
    goenv: &GlobalGoEnv,
    enum_go_name: &str,
    variant_name: &str,
) -> String {
    // Count how many enums define a variant with this name.
    let mut count = 0;
    for (_ename, edef) in goenv.enums() {
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
    let candidate = if count > 1 {
        format!("{}_{}", enum_go_name, go_ident(variant_name))
    } else {
        go_ident(variant_name)
    };
    unique_variant_symbol_name(goenv, candidate)
}

fn unique_variant_symbol_name(goenv: &GlobalGoEnv, base: String) -> String {
    if !go_toplevel_name_is_reserved(goenv, &base) {
        return base;
    }

    if is_generated_tuple_type_name(&base) {
        let protected_base = format!("_goml_user_{}", base);
        if !go_toplevel_name_is_reserved(goenv, &protected_base) {
            return protected_base;
        }
        let mut index = 1usize;
        loop {
            let candidate = format!("{}__variant{}", protected_base, index);
            if !go_toplevel_name_is_reserved(goenv, &candidate) {
                return candidate;
            }
            index += 1;
        }
    }

    let mut index = 1usize;
    loop {
        let candidate = format!("{}__variant{}", base, index);
        if !go_toplevel_name_is_reserved(goenv, &candidate) {
            return candidate;
        }
        index += 1;
    }
}

fn is_generated_tuple_type_name(name: &str) -> bool {
    let Some(rest) = name.strip_prefix("Tuple") else {
        return false;
    };
    let digit_count = rest.chars().take_while(|ch| ch.is_ascii_digit()).count();
    digit_count > 0 && (rest[digit_count..].is_empty() || rest[digit_count..].starts_with('_'))
}

fn go_toplevel_name_is_reserved(goenv: &GlobalGoEnv, name: &str) -> bool {
    name == "init"
        || is_generated_tuple_type_name(name)
        || runtime_generated_function_name(name)
        || goenv
            .structs()
            .any(|(struct_name, _)| go_user_type_name(&struct_name.0) == name)
        || goenv
            .enums()
            .any(|(enum_name, _)| go_user_type_name(&enum_name.0) == name)
        || goenv.callable_go_name_set.contains(name)
}

fn enum_def_for_ty<'a>(goenv: &'a GlobalGoEnv, ty: &tast::Ty) -> Option<&'a EnumDef> {
    let base_name = match ty {
        tast::Ty::TEnum { name } => Some(name.clone()),
        tast::Ty::TApp { ty: base, .. } => match base.as_ref() {
            tast::Ty::TEnum { name } => Some(name.clone()),
            _ => None,
        },
        _ => None,
    }?;

    let specialized_name = match tast_ty_to_go_type(ty) {
        goty::GoType::TName { name } => Some(name),
        _ => None,
    };

    if let Some(name) = specialized_name.as_deref()
        && let Some(def) = goenv.get_enum(&TastIdent::new(name))
    {
        return Some(def);
    }

    if let Some(def) = goenv.get_enum(&TastIdent::new(&base_name)) {
        return Some(def);
    }

    goenv.genv.type_env.enums.get(&TastIdent::new(&base_name))
}

fn is_tag_only_enum_def(def: &EnumDef) -> bool {
    def.variants.iter().all(|(_, fields)| fields.is_empty())
}

fn enum_is_tag_only(goenv: &GlobalGoEnv, ty: &tast::Ty) -> bool {
    enum_def_for_ty(goenv, ty).is_some_and(is_tag_only_enum_def)
}

fn lookup_variant_symbol_name(goenv: &GlobalGoEnv, ty: &tast::Ty, index: usize) -> String {
    let base_name = ty.get_constr_name_unsafe();
    let specialized_name = match tast_ty_to_go_type(ty) {
        goty::GoType::TName { name } => Some(name),
        _ => None,
    };

    if let Some(name) = specialized_name.as_deref()
        && let Some(def) = goenv.get_enum(&TastIdent::new(name))
    {
        let (vname, _fields) = &def.variants[index];
        return variant_symbol_name_for_go_enum(goenv, name, &vname.0);
    }

    if let Some(def) = goenv.get_enum(&TastIdent::new(&base_name)) {
        let (vname, _fields) = &def.variants[index];
        if let Some(enum_name) = specialized_name {
            return variant_symbol_name_for_go_enum(goenv, &enum_name, &vname.0);
        }
        return variant_symbol_name(goenv, &base_name, &vname.0);
    }
    if let Some(enum_name) = specialized_name.as_deref() {
        if extract_result_tys(ty).is_some() {
            let variant_name = match index {
                0 => "Ok",
                1 => "Err",
                _ => panic!("invalid Result variant index {}", index),
            };
            return variant_symbol_name_for_go_enum(goenv, enum_name, variant_name);
        }
        if extract_option_ty(ty).is_some() {
            let variant_name = match index {
                0 => "None",
                1 => "Some",
                _ => panic!("invalid Option variant index {}", index),
            };
            return variant_symbol_name_for_go_enum(goenv, enum_name, variant_name);
        }
    }
    panic!(
        "Cannot resolve variant name for ty {:?} index {}",
        ty, index
    );
}

fn variant_ty_by_index(goenv: &GlobalGoEnv, ty: &tast::Ty, index: usize) -> goty::GoType {
    assert!(
        !enum_is_tag_only(goenv, ty),
        "tag-only enum does not have variant struct types: {:?}",
        ty
    );
    goty::GoType::TName {
        name: lookup_variant_symbol_name(goenv, ty, index),
    }
}

fn variant_const_expr_by_index(goenv: &GlobalGoEnv, ty: &tast::Ty, index: usize) -> goast::Expr {
    goast::Expr::Var {
        name: lookup_variant_symbol_name(goenv, ty, index),
        ty: tast_ty_to_go_type(ty),
    }
}

fn extract_result_tys(ty: &tast::Ty) -> Option<(&tast::Ty, &tast::Ty)> {
    let tast::Ty::TApp { ty, args } = ty else {
        return None;
    };
    let tast::Ty::TEnum { name } = ty.as_ref() else {
        return None;
    };
    if !(name == "Result"
        || name.ends_with("::Result")
        || name.starts_with("Result__")
        || name.contains("_x3a__x3a_Result"))
        || args.len() != 2
    {
        return None;
    }
    Some((&args[0], &args[1]))
}

fn extract_option_ty(ty: &tast::Ty) -> Option<&tast::Ty> {
    let tast::Ty::TApp { ty, args } = ty else {
        return None;
    };
    let tast::Ty::TEnum { name } = ty.as_ref() else {
        return None;
    };
    if !(name == "Option"
        || name.ends_with("::Option")
        || name.starts_with("Option__")
        || name.contains("_x3a__x3a_Option"))
        || args.len() != 1
    {
        return None;
    }
    Some(&args[0])
}

fn substitute_ty_params(ty: &tast::Ty, subst: &HashMap<String, tast::Ty>) -> tast::Ty {
    match ty {
        tast::Ty::TVar(_)
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
        | tast::Ty::TChar => ty.clone(),
        tast::Ty::TTuple { typs } => tast::Ty::TTuple {
            typs: typs
                .iter()
                .map(|t| substitute_ty_params(t, subst))
                .collect(),
        },
        tast::Ty::TEnum { name } => tast::Ty::TEnum { name: name.clone() },
        tast::Ty::TStruct { name } => tast::Ty::TStruct { name: name.clone() },
        tast::Ty::TDyn { trait_name } => tast::Ty::TDyn {
            trait_name: trait_name.clone(),
        },
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
        tast::Ty::TSlice { elem } => tast::Ty::TSlice {
            elem: Box::new(substitute_ty_params(elem, subst)),
        },
        tast::Ty::TVec { elem } => tast::Ty::TVec {
            elem: Box::new(substitute_ty_params(elem, subst)),
        },
        tast::Ty::TRef { elem } => tast::Ty::TRef {
            elem: Box::new(substitute_ty_params(elem, subst)),
        },
        tast::Ty::THashMap { key, value } => tast::Ty::THashMap {
            key: Box::new(substitute_ty_params(key, subst)),
            value: Box::new(substitute_ty_params(value, subst)),
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
    goenv: &GlobalGoEnv,
    type_name: &TastIdent,
    type_args: &[tast::Ty],
) -> Vec<(String, tast::Ty)> {
    let struct_def = goenv
        .get_struct(type_name)
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

type RuntimeTypeSets = (
    IndexSet<tast::Ty>,
    IndexSet<tast::Ty>,
    IndexSet<tast::Ty>,
    IndexSet<tast::Ty>,
    IndexSet<tast::Ty>,
    IndexSet<tast::Ty>,
);

fn ty_contains_type_param(ty: &tast::Ty) -> bool {
    match ty {
        tast::Ty::TParam { .. } => true,
        tast::Ty::TArray { elem, .. }
        | tast::Ty::TSlice { elem }
        | tast::Ty::TVec { elem }
        | tast::Ty::TRef { elem } => ty_contains_type_param(elem),
        tast::Ty::THashMap { key, value } => {
            ty_contains_type_param(key) || ty_contains_type_param(value)
        }
        tast::Ty::TTuple { typs } => typs.iter().any(ty_contains_type_param),
        tast::Ty::TApp { ty, args } => {
            ty_contains_type_param(ty) || args.iter().any(ty_contains_type_param)
        }
        tast::Ty::TFunc { params, ret_ty } => {
            params.iter().any(ty_contains_type_param) || ty_contains_type_param(ret_ty)
        }
        _ => false,
    }
}

fn collect_runtime_types(goenv: &GlobalGoEnv, file: &anf::File) -> RuntimeTypeSets {
    struct Collector {
        tuples: IndexSet<tast::Ty>,
        arrays: IndexSet<tast::Ty>,
        vecs: IndexSet<tast::Ty>,
        refs: IndexSet<tast::Ty>,
        hashmaps: IndexSet<tast::Ty>,
        missings: IndexSet<tast::Ty>,
    }

    impl Collector {
        fn collect_file(mut self, goenv: &GlobalGoEnv, file: &anf::File) -> RuntimeTypeSets {
            for item in &file.toplevels {
                self.collect_fn(item);
            }
            self.collect_go_types(goenv);
            (
                self.tuples,
                self.arrays,
                self.vecs,
                self.refs,
                self.hashmaps,
                self.missings,
            )
        }

        fn collect_go_types(&mut self, goenv: &GlobalGoEnv) {
            for (name, def) in goenv.structs() {
                if name.0.contains("TParam")
                    || def.fields.iter().any(|(_, ty)| ty_contains_type_param(ty))
                {
                    continue;
                }
                for (_, ty) in &def.fields {
                    self.collect_type(ty);
                }
            }

            for (name, def) in goenv.enums() {
                if name.0.contains("TParam")
                    || def
                        .variants
                        .iter()
                        .flat_map(|(_, fields)| fields.iter())
                        .any(ty_contains_type_param)
                {
                    continue;
                }
                for (_, fields) in &def.variants {
                    for ty in fields {
                        self.collect_type(ty);
                    }
                }
            }
        }

        fn collect_fn(&mut self, item: &anf::Fn) {
            for (_, ty) in &item.params {
                self.collect_type(ty);
            }
            self.collect_type(&item.ret_ty);
            self.collect_block(&item.body);
        }

        fn collect_block(&mut self, block: &anf::Block) {
            for bind in &block.binds {
                self.collect_bind(bind);
            }
            self.collect_term(&block.term);
        }

        fn collect_bind(&mut self, bind: &anf::Bind) {
            match bind {
                anf::Bind::Let(let_bind) => {
                    self.collect_value_expr(&let_bind.value);
                    self.collect_type(&let_bind.ty);
                }
                anf::Bind::Join(join_bind) => {
                    for (_, ty) in &join_bind.params {
                        self.collect_type(ty);
                    }
                    self.collect_type(&join_bind.ret_ty);
                    self.collect_block(&join_bind.body);
                }
                anf::Bind::JoinRec(group) => {
                    for join_bind in group {
                        for (_, ty) in &join_bind.params {
                            self.collect_type(ty);
                        }
                        self.collect_type(&join_bind.ret_ty);
                        self.collect_block(&join_bind.body);
                    }
                }
            }
        }

        fn collect_term(&mut self, term: &anf::Term) {
            match term {
                anf::Term::Return(imm) => self.collect_imm(imm),
                anf::Term::Jump { args, ret_ty, .. } => {
                    for a in args {
                        self.collect_imm(a);
                    }
                    self.collect_type(ret_ty);
                }
                anf::Term::If {
                    cond,
                    then_,
                    else_,
                    ret_ty,
                } => {
                    self.collect_imm(cond);
                    self.collect_block(then_);
                    self.collect_block(else_);
                    self.collect_type(ret_ty);
                }
                anf::Term::Match {
                    scrut,
                    arms,
                    default,
                    ret_ty,
                } => {
                    self.collect_imm(scrut);
                    for arm in arms {
                        self.collect_imm(&arm.lhs);
                        self.collect_block(&arm.body);
                    }
                    if let Some(default) = default {
                        self.collect_block(default);
                    }
                    self.collect_type(ret_ty);
                }
                anf::Term::Unreachable { ty } => self.collect_type(ty),
            }
        }

        fn collect_value_expr(&mut self, expr: &anf::ValueExpr) {
            match expr {
                anf::ValueExpr::Imm(imm) => self.collect_imm(imm),
                anf::ValueExpr::Constr { args, ty, .. }
                | anf::ValueExpr::Tuple { items: args, ty }
                | anf::ValueExpr::Array { items: args, ty } => {
                    for arg in args {
                        self.collect_imm(arg);
                    }
                    self.collect_type(ty);
                }
                anf::ValueExpr::ConstrGet { expr, ty, .. } => {
                    self.collect_imm(expr);
                    self.collect_type(ty);
                }
                anf::ValueExpr::Unary { expr, ty, .. } => {
                    self.collect_imm(expr);
                    self.collect_type(ty);
                }
                anf::ValueExpr::Binary { lhs, rhs, ty, .. } => {
                    self.collect_imm(lhs);
                    self.collect_imm(rhs);
                    self.collect_type(ty);
                }
                anf::ValueExpr::Assign {
                    value,
                    target_ty,
                    ty,
                    ..
                } => {
                    self.collect_imm(value);
                    self.collect_type(target_ty);
                    self.collect_type(ty);
                }
                anf::ValueExpr::Call { func, args, ty } => {
                    self.collect_imm(func);
                    for arg in args {
                        self.collect_imm(arg);
                    }
                    if let anf::ImmExpr::Var { id, .. } = func
                        && id.0 == "missing"
                    {
                        self.missings.insert(ty.clone());
                    }
                    self.collect_type(ty);
                }
                anf::ValueExpr::ToDyn {
                    for_ty, expr, ty, ..
                } => {
                    self.collect_type(for_ty);
                    self.collect_imm(expr);
                    self.collect_type(ty);
                }
                anf::ValueExpr::DynCall {
                    receiver, args, ty, ..
                } => {
                    self.collect_imm(receiver);
                    for arg in args {
                        self.collect_imm(arg);
                    }
                    self.collect_type(ty);
                }
                anf::ValueExpr::Go { closure, ty } => {
                    self.collect_imm(closure);
                    self.collect_type(ty);
                }
                anf::ValueExpr::Proj { tuple, ty, .. } => {
                    self.collect_imm(tuple);
                    self.collect_type(ty);
                }
            }
        }

        fn collect_imm(&mut self, imm: &anf::ImmExpr) {
            match imm {
                anf::ImmExpr::Var { ty, .. }
                | anf::ImmExpr::Prim { ty, .. }
                | anf::ImmExpr::Tag { ty, .. } => {
                    self.collect_type(ty);
                }
            }
        }

        fn collect_type(&mut self, ty: &tast::Ty) {
            match ty {
                tast::Ty::TTuple { typs } => {
                    if self.tuples.insert(ty.clone()) {
                        for inner in typs {
                            self.collect_type(inner);
                        }
                    }
                }
                tast::Ty::TArray { elem, .. } => {
                    if self.arrays.insert(ty.clone()) {
                        self.collect_type(elem);
                    }
                }
                tast::Ty::TSlice { elem } => {
                    self.collect_type(elem);
                }
                tast::Ty::TVec { elem } => {
                    if self.vecs.insert(ty.clone()) {
                        self.collect_type(elem);
                    }
                }
                tast::Ty::TRef { elem } => {
                    if self.refs.insert(ty.clone()) {
                        self.collect_type(elem);
                    }
                }
                tast::Ty::THashMap { key, value } => {
                    if self.hashmaps.insert(ty.clone()) {
                        self.collect_type(key);
                        self.collect_type(value);
                    }
                }
                tast::Ty::TStruct { name: _ } => {}
                tast::Ty::TApp { ty, args } => {
                    self.collect_type(ty);
                    for arg in args {
                        self.collect_type(arg);
                    }
                }
                tast::Ty::TFunc { params, ret_ty } => {
                    for param in params {
                        self.collect_type(param);
                    }
                    self.collect_type(ret_ty);
                }
                _ => {}
            }
        }
    }

    Collector {
        tuples: IndexSet::new(),
        arrays: IndexSet::new(),
        vecs: IndexSet::new(),
        refs: IndexSet::new(),
        hashmaps: IndexSet::new(),
        missings: IndexSet::new(),
    }
    .collect_file(goenv, file)
}

#[derive(Default)]
struct DynRequirements {
    traits: IndexSet<String>,
    vtables: IndexSet<(String, tast::Ty)>,
}

fn any_go_type() -> goty::GoType {
    goty::GoType::TName {
        name: "any".to_string(),
    }
}

fn dyn_struct_go_name(trait_name: &str) -> String {
    go_dyn_struct_name(trait_name)
}

fn dyn_vtable_struct_go_name(trait_name: &str) -> String {
    go_generated_ident(&format!("dyn__{}_vtable", trait_name))
}

fn dyn_vtable_ctor_go_name_raw(trait_name: &str, for_ty: &tast::Ty) -> String {
    go_generated_ident(&format!(
        "dyn__{}__vtable__{}",
        trait_name,
        encode_ty(for_ty)
    ))
}

fn dyn_vtable_ctor_go_name(goenv: &GlobalGoEnv, trait_name: &str, for_ty: &tast::Ty) -> String {
    goenv
        .dyn_vtable_ctor_go_names
        .get(&(trait_name.to_string(), dyn_ty_key(for_ty)))
        .cloned()
        .unwrap_or_else(|| dyn_vtable_ctor_go_name_raw(trait_name, for_ty))
}

fn dyn_wrap_go_name_raw(trait_name: &str, for_ty: &tast::Ty, method_name: &str) -> String {
    go_generated_ident(&format!(
        "dyn__{}__wrap__{}__{}",
        trait_name,
        encode_ty(for_ty),
        method_name
    ))
}

fn dyn_wrap_go_name(
    goenv: &GlobalGoEnv,
    trait_name: &str,
    for_ty: &tast::Ty,
    method_name: &str,
) -> String {
    goenv
        .dyn_wrap_go_names
        .get(&(
            trait_name.to_string(),
            dyn_ty_key(for_ty),
            method_name.to_string(),
        ))
        .cloned()
        .unwrap_or_else(|| dyn_wrap_go_name_raw(trait_name, for_ty, method_name))
}

fn dyn_ty_key(ty: &tast::Ty) -> String {
    ty_compact(ty)
}

type DynVtableCtorGoNames = HashMap<(String, String), String>;
type DynWrapGoNames = HashMap<(String, String, String), String>;

fn collect_dyn_helper_go_names(
    goenv: &GlobalGoEnv,
    req: &DynRequirements,
) -> (DynVtableCtorGoNames, DynWrapGoNames) {
    let mut ctor_counts = HashMap::new();
    let mut ctor_items = Vec::new();
    let mut wrap_counts = HashMap::new();
    let mut wrap_items = Vec::new();

    for (trait_name, for_ty) in req.vtables.iter() {
        let ty_key = dyn_ty_key(for_ty);
        let ctor_raw = dyn_vtable_ctor_go_name_raw(trait_name, for_ty);
        *ctor_counts.entry(ctor_raw.clone()).or_insert(0usize) += 1;
        ctor_items.push(((trait_name.clone(), ty_key.clone()), ctor_raw));

        for (method_name, _, _) in trait_method_sigs(goenv, trait_name) {
            let wrap_raw = dyn_wrap_go_name_raw(trait_name, for_ty, &method_name);
            *wrap_counts.entry(wrap_raw.clone()).or_insert(0usize) += 1;
            wrap_items.push(((trait_name.clone(), ty_key.clone(), method_name), wrap_raw));
        }
    }

    let mut ctor_names = HashMap::new();
    for (key, raw) in ctor_items {
        if ctor_counts.get(&raw).copied().unwrap_or(0) > 1 {
            ctor_names.insert(
                key.clone(),
                go_unique_toplevel_func_name(&format!("dyn_vtable#{}#{}", key.0, key.1)),
            );
        }
    }

    let mut wrap_names = HashMap::new();
    for (key, raw) in wrap_items {
        if wrap_counts.get(&raw).copied().unwrap_or(0) > 1 {
            wrap_names.insert(
                key.clone(),
                go_unique_toplevel_func_name(&format!("dyn_wrap#{}#{}#{}", key.0, key.1, key.2)),
            );
        }
    }

    (ctor_names, wrap_names)
}

fn collect_dyn_requirements(goenv: &GlobalGoEnv, file: &anf::File) -> DynRequirements {
    fn collect_ty(req: &mut DynRequirements, ty: &tast::Ty) {
        match ty {
            tast::Ty::TDyn { trait_name } => {
                req.traits.insert(trait_name.clone());
            }
            tast::Ty::TTuple { typs } => {
                for t in typs {
                    collect_ty(req, t);
                }
            }
            tast::Ty::TApp { ty, args } => {
                collect_ty(req, ty);
                for a in args {
                    collect_ty(req, a);
                }
            }
            tast::Ty::TArray { elem, .. } => collect_ty(req, elem),
            tast::Ty::TSlice { elem } => collect_ty(req, elem),
            tast::Ty::TVec { elem } => collect_ty(req, elem),
            tast::Ty::TRef { elem } => collect_ty(req, elem),
            tast::Ty::THashMap { key, value } => {
                collect_ty(req, key);
                collect_ty(req, value);
            }
            tast::Ty::TFunc { params, ret_ty } => {
                for p in params {
                    collect_ty(req, p);
                }
                collect_ty(req, ret_ty);
            }
            _ => {}
        }
    }

    fn collect_imm(req: &mut DynRequirements, imm: &anf::ImmExpr) {
        match imm {
            anf::ImmExpr::Var { ty, .. }
            | anf::ImmExpr::Prim { ty, .. }
            | anf::ImmExpr::Tag { ty, .. } => collect_ty(req, ty),
        }
    }

    fn collect_value_expr(req: &mut DynRequirements, expr: &anf::ValueExpr) {
        match expr {
            anf::ValueExpr::Imm(imm) => collect_imm(req, imm),
            anf::ValueExpr::Constr { args, ty, .. } => {
                for a in args {
                    collect_imm(req, a);
                }
                collect_ty(req, ty);
            }
            anf::ValueExpr::Tuple { items, ty } | anf::ValueExpr::Array { items, ty } => {
                for a in items {
                    collect_imm(req, a);
                }
                collect_ty(req, ty);
            }
            anf::ValueExpr::ConstrGet { expr, ty, .. } => {
                collect_imm(req, expr);
                collect_ty(req, ty);
            }
            anf::ValueExpr::Unary { expr, ty, .. } => {
                collect_imm(req, expr);
                collect_ty(req, ty);
            }
            anf::ValueExpr::Binary { lhs, rhs, ty, .. } => {
                collect_imm(req, lhs);
                collect_imm(req, rhs);
                collect_ty(req, ty);
            }
            anf::ValueExpr::Assign {
                value,
                target_ty,
                ty,
                ..
            } => {
                collect_imm(req, value);
                collect_ty(req, target_ty);
                collect_ty(req, ty);
            }
            anf::ValueExpr::Call { func, args, ty } => {
                collect_imm(req, func);
                for a in args {
                    collect_imm(req, a);
                }
                collect_ty(req, ty);
            }
            anf::ValueExpr::ToDyn {
                trait_name,
                for_ty,
                expr,
                ty,
            } => {
                req.traits.insert(trait_name.0.clone());
                req.vtables.insert((trait_name.0.clone(), for_ty.clone()));
                collect_ty(req, for_ty);
                collect_imm(req, expr);
                collect_ty(req, ty);
            }
            anf::ValueExpr::DynCall {
                trait_name,
                receiver,
                args,
                ty,
                ..
            } => {
                req.traits.insert(trait_name.0.clone());
                collect_imm(req, receiver);
                for a in args {
                    collect_imm(req, a);
                }
                collect_ty(req, ty);
            }
            anf::ValueExpr::Go { closure, ty } => {
                collect_imm(req, closure);
                collect_ty(req, ty);
            }
            anf::ValueExpr::Proj { tuple, ty, .. } => {
                collect_imm(req, tuple);
                collect_ty(req, ty);
            }
        }
    }

    fn collect_term(req: &mut DynRequirements, term: &anf::Term) {
        match term {
            anf::Term::Return(imm) => collect_imm(req, imm),
            anf::Term::Jump { args, ret_ty, .. } => {
                for a in args {
                    collect_imm(req, a);
                }
                collect_ty(req, ret_ty);
            }
            anf::Term::If {
                cond,
                then_,
                else_,
                ret_ty,
            } => {
                collect_imm(req, cond);
                collect_block(req, then_);
                collect_block(req, else_);
                collect_ty(req, ret_ty);
            }
            anf::Term::Match {
                scrut,
                arms,
                default,
                ret_ty,
            } => {
                collect_imm(req, scrut);
                for arm in arms {
                    collect_imm(req, &arm.lhs);
                    collect_block(req, &arm.body);
                }
                if let Some(default) = default {
                    collect_block(req, default);
                }
                collect_ty(req, ret_ty);
            }
            anf::Term::Unreachable { ty } => collect_ty(req, ty),
        }
    }

    fn collect_bind(req: &mut DynRequirements, bind: &anf::Bind) {
        match bind {
            anf::Bind::Let(let_bind) => {
                collect_value_expr(req, &let_bind.value);
                collect_ty(req, &let_bind.ty);
            }
            anf::Bind::Join(join_bind) => {
                for (_, ty) in &join_bind.params {
                    collect_ty(req, ty);
                }
                collect_ty(req, &join_bind.ret_ty);
                collect_block(req, &join_bind.body);
            }
            anf::Bind::JoinRec(group) => {
                for join_bind in group {
                    for (_, ty) in &join_bind.params {
                        collect_ty(req, ty);
                    }
                    collect_ty(req, &join_bind.ret_ty);
                    collect_block(req, &join_bind.body);
                }
            }
        }
    }

    fn collect_block(req: &mut DynRequirements, block: &anf::Block) {
        for bind in &block.binds {
            collect_bind(req, bind);
        }
        collect_term(req, &block.term);
    }

    let mut req = DynRequirements::default();
    for (_, def) in goenv.structs() {
        for (_, ty) in &def.fields {
            collect_ty(&mut req, ty);
        }
    }
    for (_, def) in goenv.enums() {
        for (_, fields) in &def.variants {
            for ty in fields {
                collect_ty(&mut req, ty);
            }
        }
    }
    for f in &file.toplevels {
        for (_, ty) in &f.params {
            collect_ty(&mut req, ty);
        }
        collect_ty(&mut req, &f.ret_ty);
        collect_block(&mut req, &f.body);
    }
    let mut i = 0;
    while let Some(trait_name) = req.traits.get_index(i).cloned() {
        for (_, params, ret_ty) in trait_method_sigs(goenv, &trait_name) {
            for param_ty in params {
                collect_ty(&mut req, &param_ty);
            }
            collect_ty(&mut req, &ret_ty);
        }
        i += 1;
    }
    req
}

fn trait_method_sigs(
    goenv: &GlobalGoEnv,
    trait_name: &str,
) -> Vec<(String, Vec<tast::Ty>, tast::Ty)> {
    let trait_def = goenv
        .genv
        .trait_env
        .trait_defs
        .get(trait_name)
        .unwrap_or_else(|| panic!("missing trait def {}", trait_name));
    trait_def
        .methods
        .iter()
        .map(|(name, scheme)| {
            let tast::Ty::TFunc { params, ret_ty } = &scheme.ty else {
                panic!("trait {}::{} is not a function", trait_name, name);
            };
            let rest = params.iter().skip(1).cloned().collect::<Vec<_>>();
            (name.clone(), rest, (**ret_ty).clone())
        })
        .collect()
}

fn gen_dyn_type_definitions(goenv: &GlobalGoEnv, req: &DynRequirements) -> Vec<goast::Item> {
    let mut traits: Vec<String> = req.traits.iter().cloned().collect();
    traits.sort();

    let mut items = Vec::new();
    for trait_name in traits {
        let vtable_struct_name = dyn_vtable_struct_go_name(&trait_name);
        let mut vtable_fields = Vec::new();
        for (method_name, params, ret_ty) in trait_method_sigs(goenv, &trait_name) {
            let mut go_params = Vec::with_capacity(params.len() + 1);
            go_params.push(any_go_type());
            go_params.extend(params.iter().map(tast_ty_to_go_type));
            let go_ret = tast_ty_to_go_type(&ret_ty);
            vtable_fields.push(goast::Field {
                name: go_ident(&method_name),
                ty: goty::GoType::TFunc {
                    params: go_params,
                    ret_ty: Box::new(go_ret),
                },
            });
        }
        items.push(goast::Item::Struct(goast::Struct {
            name: vtable_struct_name.clone(),
            fields: vtable_fields,
            methods: vec![],
        }));

        items.push(goast::Item::Struct(goast::Struct {
            name: dyn_struct_go_name(&trait_name),
            fields: vec![
                goast::Field {
                    name: "data".to_string(),
                    ty: any_go_type(),
                },
                goast::Field {
                    name: "vtable".to_string(),
                    ty: goty::GoType::TPointer {
                        elem: Box::new(goty::GoType::TName {
                            name: vtable_struct_name,
                        }),
                    },
                },
            ],
            methods: vec![],
        }));
    }
    items
}

fn build_trait_impl_name_map(
    goenv: &GlobalGoEnv,
    toplevels: &[anf::Fn],
) -> std::collections::HashMap<(String, String, String), String> {
    let mut map = std::collections::HashMap::new();
    for tl in toplevels {
        if let Some((trait_name, for_ty_str, method_name)) = parse_trait_impl_fn_name(&tl.name) {
            let go_name = go_toplevel_func_name(goenv, &tl.name);
            map.insert(
                (
                    trait_name.to_string(),
                    for_ty_str.to_string(),
                    method_name.to_string(),
                ),
                go_name,
            );
        }
    }
    map
}

fn gen_dyn_helper_fns(
    goenv: &GlobalGoEnv,
    req: &DynRequirements,
    impl_name_map: &std::collections::HashMap<(String, String, String), String>,
) -> Vec<goast::Item> {
    let mut vtables: Vec<(String, tast::Ty)> = req.vtables.iter().cloned().collect();
    vtables.sort_by(|(t1, ty1), (t2, ty2)| {
        let k1 = (t1.clone(), encode_ty(ty1));
        let k2 = (t2.clone(), encode_ty(ty2));
        k1.cmp(&k2)
    });

    let mut items = Vec::new();
    for (trait_name, for_ty) in vtables {
        let methods = trait_method_sigs(goenv, &trait_name);

        for (method_name, params, ret_ty) in &methods {
            items.push(goast::Item::Fn(gen_dyn_wrap_fn(
                goenv,
                &trait_name,
                &for_ty,
                method_name,
                params,
                ret_ty,
                impl_name_map,
            )));
        }

        items.push(goast::Item::Fn(gen_dyn_vtable_ctor_fn(
            goenv,
            &trait_name,
            &for_ty,
            &methods,
        )));
    }
    items
}

fn gen_dyn_wrap_fn(
    goenv: &GlobalGoEnv,
    trait_name: &str,
    for_ty: &tast::Ty,
    method_name: &str,
    params: &[tast::Ty],
    ret_ty: &tast::Ty,
    impl_name_map: &std::collections::HashMap<(String, String, String), String>,
) -> goast::Fn {
    let fn_name = dyn_wrap_go_name(goenv, trait_name, for_ty, method_name);

    let mut go_params = Vec::with_capacity(params.len() + 1);
    go_params.push(("self".to_string(), any_go_type()));
    for (i, pty) in params.iter().enumerate() {
        go_params.push((format!("p{}", i), tast_ty_to_go_type(pty)));
    }

    let trait_ident = TastIdent(trait_name.to_string());
    let for_ty_compact = ty_compact(for_ty);
    let impl_go_name = impl_name_map
        .get(&(
            trait_name.to_string(),
            for_ty_compact,
            method_name.to_string(),
        ))
        .cloned()
        .unwrap_or_else(|| {
            let impl_name = trait_impl_fn_name(&trait_ident, for_ty, method_name);
            go_toplevel_func_name(goenv, &impl_name)
        });

    let receiver_go_ty = tast_ty_to_go_type(for_ty);
    let ret_go_ty = tast_ty_to_go_type(ret_ty);

    if trait_name == "ToString"
        && method_name == "to_string"
        && params.is_empty()
        && let Some(expr) = builtin_tostring_expr(
            goenv,
            goast::Expr::Cast {
                expr: Box::new(goast::Expr::Var {
                    name: "self".to_string(),
                    ty: any_go_type(),
                }),
                ty: receiver_go_ty.clone(),
            },
            for_ty,
        )
    {
        return goast::Fn {
            name: fn_name,
            params: go_params,
            ret_ty: Some(ret_go_ty),
            body: goast::Block {
                stmts: vec![goast::Stmt::Return { expr: Some(expr) }],
            },
        };
    }

    let mut impl_param_tys = Vec::with_capacity(params.len() + 1);
    impl_param_tys.push(receiver_go_ty.clone());
    impl_param_tys.extend(params.iter().map(tast_ty_to_go_type));

    let enum_name = match for_ty {
        tast::Ty::TEnum { name } => Some(name.clone()),
        _ => None,
    };
    let is_enum = enum_name
        .as_ref()
        .and_then(|n| goenv.get_enum(&TastIdent::new(n)))
        .is_some();

    if is_enum {
        let mut args = Vec::with_capacity(params.len() + 1);
        args.push(goast::Expr::Var {
            name: "v".to_string(),
            ty: receiver_go_ty.clone(),
        });
        for (i, pty) in params.iter().enumerate() {
            args.push(goast::Expr::Var {
                name: format!("p{}", i),
                ty: tast_ty_to_go_type(pty),
            });
        }
        let call = goast::Expr::Call {
            func: Box::new(goast::Expr::Var {
                name: impl_go_name,
                ty: goty::GoType::TFunc {
                    params: impl_param_tys,
                    ret_ty: Box::new(ret_go_ty.clone()),
                },
            }),
            args,
            ty: ret_go_ty.clone(),
        };
        let switch_stmt = goast::Stmt::SwitchType {
            bind: Some("v".to_string()),
            expr: goast::Expr::Var {
                name: "self".to_string(),
                ty: any_go_type(),
            },
            cases: vec![(
                receiver_go_ty,
                goast::Block {
                    stmts: vec![goast::Stmt::Return { expr: Some(call) }],
                },
            )],
            default: Some(goast::Block {
                stmts: vec![goast::Stmt::Expr(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "panic".to_string(),
                        ty: goty::GoType::TUnit,
                    }),
                    args: vec![goast::Expr::String {
                        value: "unexpected type".to_string(),
                        ty: goty::GoType::TString,
                    }],
                    ty: goty::GoType::TUnit,
                })],
            }),
        };
        goast::Fn {
            name: fn_name,
            params: go_params,
            ret_ty: Some(ret_go_ty),
            body: goast::Block {
                stmts: vec![switch_stmt],
            },
        }
    } else {
        let asserted_self = goast::Expr::Cast {
            expr: Box::new(goast::Expr::Var {
                name: "self".to_string(),
                ty: any_go_type(),
            }),
            ty: receiver_go_ty.clone(),
        };

        let mut args = Vec::with_capacity(params.len() + 1);
        args.push(asserted_self);
        for (i, pty) in params.iter().enumerate() {
            args.push(goast::Expr::Var {
                name: format!("p{}", i),
                ty: tast_ty_to_go_type(pty),
            });
        }

        let call = goast::Expr::Call {
            func: Box::new(goast::Expr::Var {
                name: impl_go_name,
                ty: goty::GoType::TFunc {
                    params: impl_param_tys,
                    ret_ty: Box::new(ret_go_ty.clone()),
                },
            }),
            args,
            ty: ret_go_ty.clone(),
        };

        goast::Fn {
            name: fn_name,
            params: go_params,
            ret_ty: Some(ret_go_ty),
            body: goast::Block {
                stmts: vec![goast::Stmt::Return { expr: Some(call) }],
            },
        }
    }
}

fn builtin_tostring_expr(
    goenv: &GlobalGoEnv,
    value: goast::Expr,
    ty: &tast::Ty,
) -> Option<goast::Expr> {
    match ty {
        tast::Ty::TString => Some(value),
        tast::Ty::TUnit => Some(unary_tostring_call("unit_to_string", value, ty)),
        tast::Ty::TBool => Some(unary_tostring_call("bool_to_string", value, ty)),
        tast::Ty::TChar => Some(unary_tostring_call("char_to_string", value, ty)),
        tast::Ty::TInt8 => Some(unary_tostring_call("int8_to_string", value, ty)),
        tast::Ty::TInt16 => Some(unary_tostring_call("int16_to_string", value, ty)),
        tast::Ty::TInt32 => Some(unary_tostring_call("int32_to_string", value, ty)),
        tast::Ty::TInt64 => Some(unary_tostring_call("int64_to_string", value, ty)),
        tast::Ty::TUint8 => Some(unary_tostring_call("uint8_to_string", value, ty)),
        tast::Ty::TUint16 => Some(unary_tostring_call("uint16_to_string", value, ty)),
        tast::Ty::TUint32 => Some(unary_tostring_call("uint32_to_string", value, ty)),
        tast::Ty::TUint64 => Some(unary_tostring_call("uint64_to_string", value, ty)),
        tast::Ty::TFloat32 => Some(unary_tostring_call("float32_to_string", value, ty)),
        tast::Ty::TFloat64 => Some(unary_tostring_call("float64_to_string", value, ty)),
        tast::Ty::TDyn { trait_name } if trait_name == "ToString" => Some(dyn_tostring_call(value)),
        tast::Ty::TRef { elem } => {
            let ref_ty = tast::Ty::TRef { elem: elem.clone() };
            let ref_go_ty = tast_ty_to_go_type(&ref_ty);
            let elem_go_ty = tast_ty_to_go_type(elem);
            let ref_get = goast::Expr::Call {
                func: Box::new(goast::Expr::Var {
                    name: runtime::ref_helper_fn_name("ref_get", &ref_ty),
                    ty: goty::GoType::TFunc {
                        params: vec![ref_go_ty],
                        ret_ty: Box::new(elem_go_ty.clone()),
                    },
                }),
                args: vec![value],
                ty: elem_go_ty,
            };
            let inner = tostring_expr(goenv, ref_get, elem);
            let with_prefix = goast::Expr::BinaryOp {
                op: goast::GoBinaryOp::Add,
                lhs: Box::new(goast::Expr::String {
                    value: "ref(".to_string(),
                    ty: goty::GoType::TString,
                }),
                rhs: Box::new(inner),
                ty: goty::GoType::TString,
            };
            Some(goast::Expr::BinaryOp {
                op: goast::GoBinaryOp::Add,
                lhs: Box::new(with_prefix),
                rhs: Box::new(goast::Expr::String {
                    value: ")".to_string(),
                    ty: goty::GoType::TString,
                }),
                ty: goty::GoType::TString,
            })
        }
        _ => None,
    }
}

fn tostring_expr(goenv: &GlobalGoEnv, value: goast::Expr, ty: &tast::Ty) -> goast::Expr {
    if has_builtin_tostring_expr(goenv, ty) {
        return builtin_tostring_expr(goenv, value, ty)
            .expect("builtin ToString expression must exist");
    }

    let impl_name = trait_impl_fn_name(&TastIdent("ToString".to_string()), ty, "to_string");
    let value_ty = tast_ty_to_go_type(ty);
    goast::Expr::Call {
        func: Box::new(goast::Expr::Var {
            name: go_toplevel_func_name(goenv, &impl_name),
            ty: goty::GoType::TFunc {
                params: vec![value_ty],
                ret_ty: Box::new(goty::GoType::TString),
            },
        }),
        args: vec![value],
        ty: goty::GoType::TString,
    }
}

fn has_builtin_tostring_expr(_goenv: &GlobalGoEnv, ty: &tast::Ty) -> bool {
    match ty {
        tast::Ty::TString
        | tast::Ty::TUnit
        | tast::Ty::TBool
        | tast::Ty::TChar
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
        | tast::Ty::TRef { .. } => true,
        tast::Ty::TDyn { trait_name } if trait_name == "ToString" => true,
        _ => false,
    }
}

fn dyn_tostring_call(value: goast::Expr) -> goast::Expr {
    let dyn_go_ty = goty::GoType::TName {
        name: dyn_struct_go_name("ToString"),
    };
    let param_name = "_goml_dyn_value".to_string();
    let param_expr_for_vtable = goast::Expr::Var {
        name: param_name.clone(),
        ty: dyn_go_ty.clone(),
    };
    let param_expr_for_data = goast::Expr::Var {
        name: param_name.clone(),
        ty: dyn_go_ty.clone(),
    };
    let vtable_ptr_expr = goast::Expr::FieldAccess {
        obj: Box::new(param_expr_for_vtable),
        field: "vtable".to_string(),
        ty: goty::GoType::TPointer {
            elem: Box::new(goty::GoType::TName {
                name: dyn_vtable_struct_go_name("ToString"),
            }),
        },
    };
    let method_expr = goast::Expr::FieldAccess {
        obj: Box::new(vtable_ptr_expr),
        field: "to_string".to_string(),
        ty: goty::GoType::TFunc {
            params: vec![any_go_type()],
            ret_ty: Box::new(goty::GoType::TString),
        },
    };
    let data_expr = goast::Expr::FieldAccess {
        obj: Box::new(param_expr_for_data),
        field: "data".to_string(),
        ty: any_go_type(),
    };
    let method_call = goast::Expr::Call {
        func: Box::new(method_expr),
        args: vec![data_expr],
        ty: goty::GoType::TString,
    };
    let func_ty = goty::GoType::TFunc {
        params: vec![dyn_go_ty.clone()],
        ret_ty: Box::new(goty::GoType::TString),
    };
    goast::Expr::Call {
        func: Box::new(goast::Expr::FuncLit {
            params: vec![(param_name, dyn_go_ty)],
            body: vec![goast::Stmt::Return {
                expr: Some(method_call),
            }],
            ty: func_ty,
        }),
        args: vec![value],
        ty: goty::GoType::TString,
    }
}

fn unary_tostring_call(func_name: &str, value: goast::Expr, ty: &tast::Ty) -> goast::Expr {
    let value_ty = tast_ty_to_go_type(ty);
    goast::Expr::Call {
        func: Box::new(goast::Expr::Var {
            name: func_name.to_string(),
            ty: goty::GoType::TFunc {
                params: vec![value_ty],
                ret_ty: Box::new(goty::GoType::TString),
            },
        }),
        args: vec![value],
        ty: goty::GoType::TString,
    }
}

fn gen_dyn_vtable_ctor_fn(
    goenv: &GlobalGoEnv,
    trait_name: &str,
    for_ty: &tast::Ty,
    methods: &[(String, Vec<tast::Ty>, tast::Ty)],
) -> goast::Fn {
    let vtable_struct_name = dyn_vtable_struct_go_name(trait_name);
    let vtable_ptr_ty = goty::GoType::TPointer {
        elem: Box::new(goty::GoType::TName {
            name: vtable_struct_name.clone(),
        }),
    };

    let mut fields = Vec::new();
    for (method_name, params, ret_ty) in methods {
        let mut go_params = Vec::with_capacity(params.len() + 1);
        go_params.push(any_go_type());
        go_params.extend(params.iter().map(tast_ty_to_go_type));
        let field_ty = goty::GoType::TFunc {
            params: go_params,
            ret_ty: Box::new(tast_ty_to_go_type(ret_ty)),
        };
        fields.push((
            go_ident(method_name),
            goast::Expr::Var {
                name: dyn_wrap_go_name(goenv, trait_name, for_ty, method_name),
                ty: field_ty,
            },
        ));
    }

    let lit = goast::Expr::StructLiteral {
        fields,
        ty: goty::GoType::TName {
            name: vtable_struct_name,
        },
    };

    let addr = goast::Expr::UnaryOp {
        op: goast::GoUnaryOp::AddrOf,
        expr: Box::new(lit),
        ty: vtable_ptr_ty.clone(),
    };

    goast::Fn {
        name: dyn_vtable_ctor_go_name(goenv, trait_name, for_ty),
        params: vec![],
        ret_ty: Some(vtable_ptr_ty),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return { expr: Some(addr) }],
        },
    }
}

fn tuple_to_go_struct_type(ty: &tast::Ty) -> goty::GoType {
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

#[cfg(any())]
mod legacy_anf_codegen {
    use super::*;

    fn compile_cexpr(goenv: &GlobalGoEnv, e: &anf::CExpr) -> goast::Expr {
        match e {
            anf::CExpr::CImm { imm } => compile_imm(goenv, imm),
            anf::CExpr::EConstr {
                constructor,
                args,
                ty,
            } => match constructor {
                Constructor::Enum(enum_constructor) => {
                    let variant_ty = variant_ty_by_index(goenv, ty, enum_constructor.index);
                    let variant_field_types = goenv
                        .get_enum(&enum_constructor.type_name)
                        .and_then(|def| def.variants.get(enum_constructor.index))
                        .map(|(_, fields)| fields.as_slice());
                    let fields = args
                        .iter()
                        .enumerate()
                        .map(|(i, a)| {
                            let field_ty = variant_field_types.and_then(|f| f.get(i));
                            let arg_ty = imm_ty(a);
                            let val = if let Some(ft) = field_ty
                                && needs_closure_to_func_wrap(&arg_ty, ft)
                            {
                                closure_to_func_lit(goenv, a, ft)
                            } else {
                                compile_imm(goenv, a)
                            };
                            (format!("_{}", i), val)
                        })
                        .collect();
                    goast::Expr::StructLiteral {
                        ty: variant_ty,
                        fields,
                    }
                }
                Constructor::Struct(struct_constructor) => {
                    let go_ty = tast_ty_to_go_type(ty);
                    let struct_def = goenv
                        .get_struct(&struct_constructor.type_name)
                        .unwrap_or_else(|| {
                            panic!("unknown struct {}", struct_constructor.type_name.0)
                        });
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
                        .map(|((fname, field_ty), arg)| {
                            let arg_ty = imm_ty(arg);
                            let val = if needs_closure_to_func_wrap(&arg_ty, field_ty) {
                                closure_to_func_lit(goenv, arg, field_ty)
                            } else {
                                compile_imm(goenv, arg)
                            };
                            (go_ident(&fname.0), val)
                        })
                        .collect();
                    goast::Expr::StructLiteral { ty: go_ty, fields }
                }
            },
            anf::CExpr::ETuple { items, ty } => {
                let fields = items
                    .iter()
                    .enumerate()
                    .map(|(i, a)| (format!("_{}", i), compile_imm(goenv, a)))
                    .collect();
                goast::Expr::StructLiteral {
                    ty: tuple_to_go_struct_type(ty),
                    fields,
                }
            }
            anf::CExpr::EArray { items, ty } => {
                let elem_ty = match ty {
                    tast::Ty::TArray { elem, .. } => elem.as_ref(),
                    _ => ty,
                };
                let elems = items
                    .iter()
                    .map(|item| {
                        let arg_ty = imm_ty(item);
                        if needs_closure_to_func_wrap(&arg_ty, elem_ty) {
                            closure_to_func_lit(goenv, item, elem_ty)
                        } else {
                            compile_imm(goenv, item)
                        }
                    })
                    .collect();
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
            anf::CExpr::EGo { .. } => {
                panic!("EGo should be handled as a statement, not as an expression")
            }
            anf::CExpr::EConstrGet {
                expr,
                constructor,
                field_index,
                ty: _,
            } => {
                let obj = compile_imm(goenv, expr);
                match constructor {
                    Constructor::Enum(enum_constructor) => {
                        let def = goenv
                            .get_enum(&enum_constructor.type_name)
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
                            tast::Ty::TStruct { name } => (name, Vec::new()),
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
                        let fields = instantiate_struct_fields(
                            goenv,
                            &struct_constructor.type_name,
                            &type_args,
                        );
                        let (field_name, field_ty) = &fields[*field_index];
                        goast::Expr::FieldAccess {
                            obj: Box::new(obj),
                            field: go_ident(field_name),
                            ty: tast_ty_to_go_type(field_ty),
                        }
                    }
                }
            }
            anf::CExpr::EUnary { op, expr, ty } => {
                let go_op = match op {
                    common_defs::UnaryOp::Neg => goast::GoUnaryOp::Neg,
                    common_defs::UnaryOp::Not => goast::GoUnaryOp::Not,
                };
                goast::Expr::UnaryOp {
                    op: go_op,
                    expr: Box::new(compile_imm(goenv, expr)),
                    ty: tast_ty_to_go_type(ty),
                }
            }
            anf::CExpr::EBinary { op, lhs, rhs, ty } => {
                let go_op = match op {
                    common_defs::BinaryOp::Add => goast::GoBinaryOp::Add,
                    common_defs::BinaryOp::Sub => goast::GoBinaryOp::Sub,
                    common_defs::BinaryOp::Mul => goast::GoBinaryOp::Mul,
                    common_defs::BinaryOp::Div => goast::GoBinaryOp::Div,
                    common_defs::BinaryOp::And => goast::GoBinaryOp::And,
                    common_defs::BinaryOp::Or => goast::GoBinaryOp::Or,
                    common_defs::BinaryOp::Less => goast::GoBinaryOp::Less,
                    common_defs::BinaryOp::Greater => goast::GoBinaryOp::Greater,
                    common_defs::BinaryOp::LessEq => goast::GoBinaryOp::LessEq,
                    common_defs::BinaryOp::GreaterEq => goast::GoBinaryOp::GreaterEq,
                    common_defs::BinaryOp::Eq => goast::GoBinaryOp::Eq,
                    common_defs::BinaryOp::NotEq => goast::GoBinaryOp::NotEq,
                };
                goast::Expr::BinaryOp {
                    op: go_op,
                    lhs: Box::new(compile_imm(goenv, lhs)),
                    rhs: Box::new(compile_imm(goenv, rhs)),
                    ty: tast_ty_to_go_type(ty),
                }
            }
            anf::CExpr::EToDyn {
                trait_name,
                for_ty,
                expr,
                ty,
            } => {
                let dyn_struct_ty = tast_ty_to_go_type(ty);
                let vtable_struct_name = dyn_vtable_struct_go_name(&trait_name.0);
                let vtable_ptr_ty = goty::GoType::TPointer {
                    elem: Box::new(goty::GoType::TName {
                        name: vtable_struct_name,
                    }),
                };

                let ctor_name = dyn_vtable_ctor_go_name(goenv, &trait_name.0, for_ty);
                let ctor_ty = goty::GoType::TFunc {
                    params: vec![],
                    ret_ty: Box::new(vtable_ptr_ty.clone()),
                };
                let vtable_expr = goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: ctor_name,
                        ty: ctor_ty,
                    }),
                    args: vec![],
                    ty: vtable_ptr_ty,
                };

                goast::Expr::StructLiteral {
                    fields: vec![
                        ("data".to_string(), compile_imm(goenv, expr)),
                        ("vtable".to_string(), vtable_expr),
                    ],
                    ty: dyn_struct_ty,
                }
            }
            anf::CExpr::EDynCall {
                trait_name,
                method_name,
                receiver,
                args,
                ty,
            } => {
                let receiver_expr_for_vtable = compile_imm(goenv, receiver);
                let receiver_expr_for_data = compile_imm(goenv, receiver);

                let (method_params, method_ret) = trait_method_sigs(goenv, &trait_name.0)
                    .into_iter()
                    .find(|(name, _, _)| name == &method_name.0)
                    .map(|(_name, params, ret)| (params, ret))
                    .unwrap_or_else(|| {
                        panic!("missing trait method {}::{}", trait_name.0, method_name.0)
                    });

                let mut fn_params = Vec::with_capacity(method_params.len() + 1);
                fn_params.push(any_go_type());
                fn_params.extend(method_params.iter().map(tast_ty_to_go_type));
                let fn_ret = tast_ty_to_go_type(&method_ret);

                let vtable_ptr_expr = goast::Expr::FieldAccess {
                    obj: Box::new(receiver_expr_for_vtable),
                    field: "vtable".to_string(),
                    ty: goty::GoType::TPointer {
                        elem: Box::new(goty::GoType::TName {
                            name: dyn_vtable_struct_go_name(&trait_name.0),
                        }),
                    },
                };
                let method_expr = goast::Expr::FieldAccess {
                    obj: Box::new(vtable_ptr_expr),
                    field: go_ident(&method_name.0),
                    ty: goty::GoType::TFunc {
                        params: fn_params,
                        ret_ty: Box::new(fn_ret.clone()),
                    },
                };

                let mut call_args = Vec::with_capacity(args.len() + 1);
                call_args.push(goast::Expr::FieldAccess {
                    obj: Box::new(receiver_expr_for_data),
                    field: "data".to_string(),
                    ty: any_go_type(),
                });
                call_args.extend(compile_call_args(goenv, &method_params, args));

                goast::Expr::Call {
                    func: Box::new(method_expr),
                    args: call_args,
                    ty: tast_ty_to_go_type(ty),
                }
            }
            anf::CExpr::ECall { func, args, ty } => {
                let func_tast_ty = imm_ty(func);
                let param_types: Vec<tast::Ty> = match &func_tast_ty {
                    tast::Ty::TFunc { params, .. } => params.clone(),
                    _ => Vec::new(),
                };
                let compiled_args = compile_call_args(goenv, &param_types, args);
                let func_ty = tast_ty_to_go_type(&func_tast_ty);

                if let anf::ImmExpr::ImmVar { name, .. } = &func
                    && runtime_builtin_available(goenv, name)
                    && *name == "missing"
                {
                    let helper_name = runtime::missing_helper_fn_name(ty);
                    goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: helper_name,
                            ty: goty::GoType::TFunc {
                                params: vec![goty::GoType::TString],
                                ret_ty: Box::new(tast_ty_to_go_type(ty)),
                            },
                        }),
                        args: compiled_args,
                        ty: tast_ty_to_go_type(ty),
                    }
                } else if let anf::ImmExpr::ImmVar { name, .. } = &func
                    && runtime_builtin_available(goenv, name)
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
                    && runtime_builtin_available(goenv, name)
                    && (*name == "ref"
                        || *name == "ref_get"
                        || *name == "ref_set"
                        || *name == "ptr_eq")
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
                    } else if name == "ptr_eq" {
                        let ref_ty = imm_ty(&args[0]);
                        let tast::Ty::TRef { .. } = &ref_ty else {
                            panic!("ptr_eq expects reference arguments, got {:?}", ref_ty);
                        };
                        let ref_go_ty = tast_ty_to_go_type(&ref_ty);
                        (
                            runtime::ref_helper_fn_name("ptr_eq", &ref_ty),
                            goty::GoType::TFunc {
                                params: vec![ref_go_ty.clone(), ref_go_ty.clone()],
                                ret_ty: Box::new(goty::GoType::TBool),
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
                    && runtime_builtin_available(goenv, name)
                    && (*name == "hashmap_new"
                        || *name == "hashmap_get"
                        || *name == "hashmap_set"
                        || *name == "hashmap_remove"
                        || *name == "hashmap_len"
                        || *name == "hashmap_contains")
                {
                    let map_ty = if name == "hashmap_new" {
                        ty.clone()
                    } else {
                        imm_ty(&args[0])
                    };
                    let tast::Ty::THashMap { key, value } = &map_ty else {
                        panic!("{} expects HashMap type, got {:?}", name, map_ty);
                    };

                    let map_go_ty = tast_ty_to_go_type(&map_ty);
                    let key_go_ty = tast_ty_to_go_type(key);
                    let value_go_ty = tast_ty_to_go_type(value);

                    let helper = runtime::hashmap_helper_fn_name(name, &map_ty);
                    let helper_ty = match name.as_str() {
                        "hashmap_new" => goty::GoType::TFunc {
                            params: vec![],
                            ret_ty: Box::new(map_go_ty.clone()),
                        },
                        "hashmap_get" => goty::GoType::TFunc {
                            params: vec![map_go_ty.clone(), key_go_ty.clone()],
                            ret_ty: Box::new(tast_ty_to_go_type(ty)),
                        },
                        "hashmap_set" => goty::GoType::TFunc {
                            params: vec![map_go_ty.clone(), key_go_ty.clone(), value_go_ty.clone()],
                            ret_ty: Box::new(goty::GoType::TUnit),
                        },
                        "hashmap_remove" => goty::GoType::TFunc {
                            params: vec![map_go_ty.clone(), key_go_ty.clone()],
                            ret_ty: Box::new(goty::GoType::TUnit),
                        },
                        "hashmap_len" => goty::GoType::TFunc {
                            params: vec![map_go_ty.clone()],
                            ret_ty: Box::new(goty::GoType::TInt32),
                        },
                        "hashmap_contains" => goty::GoType::TFunc {
                            params: vec![map_go_ty.clone(), key_go_ty.clone()],
                            ret_ty: Box::new(goty::GoType::TBool),
                        },
                        _ => unreachable!(),
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
                    && runtime_builtin_available(goenv, name)
                    && (*name == "slice"
                        || *name == "slice_get"
                        || *name == "slice_len"
                        || *name == "slice_sub")
                {
                    match name.as_str() {
                        "slice" | "slice_sub" => {
                            let mut args_iter = compiled_args.into_iter();
                            let array_arg = args_iter.next().unwrap();
                            let start_arg = args_iter.next().unwrap();
                            let end_arg = args_iter.next().unwrap();
                            goast::Expr::Slice {
                                array: Box::new(array_arg),
                                start: Box::new(start_arg),
                                end: Box::new(end_arg),
                                ty: tast_ty_to_go_type(ty),
                            }
                        }
                        "slice_get" => {
                            let mut args_iter = compiled_args.into_iter();
                            let slice_arg = args_iter.next().unwrap();
                            let index_arg = args_iter.next().unwrap();
                            goast::Expr::Index {
                                array: Box::new(slice_arg),
                                index: Box::new(index_arg),
                                ty: tast_ty_to_go_type(ty),
                            }
                        }
                        "slice_len" => {
                            let mut args_iter = compiled_args.into_iter();
                            let slice_arg = args_iter.next().unwrap();
                            goast::Expr::Call {
                                func: Box::new(goast::Expr::Var {
                                    name: "int32".to_string(),
                                    ty: goty::GoType::TFunc {
                                        params: vec![goty::GoType::TInt32],
                                        ret_ty: Box::new(goty::GoType::TInt32),
                                    },
                                }),
                                args: vec![goast::Expr::Call {
                                    func: Box::new(goast::Expr::Var {
                                        name: "len".to_string(),
                                        ty: goty::GoType::TFunc {
                                            params: vec![tast_ty_to_go_type(&imm_ty(&args[0]))],
                                            ret_ty: Box::new(goty::GoType::TInt32),
                                        },
                                    }),
                                    args: vec![slice_arg],
                                    ty: goty::GoType::TInt32,
                                }],
                                ty: tast_ty_to_go_type(ty),
                            }
                        }
                        _ => unreachable!(),
                    }
                } else if let anf::ImmExpr::ImmVar { name, .. } = &func
                    && runtime_builtin_available(goenv, name)
                    && (*name == "vec_new"
                        || *name == "vec_push"
                        || *name == "vec_get"
                        || *name == "vec_set"
                        || *name == "vec_len")
                {
                    match name.as_str() {
                        "vec_new" => goast::Expr::Nil {
                            ty: tast_ty_to_go_type(ty),
                        },
                        "vec_push" => goast::Expr::Call {
                            func: Box::new(goast::Expr::Var {
                                name: "append".to_string(),
                                ty: func_ty,
                            }),
                            args: compiled_args,
                            ty: tast_ty_to_go_type(ty),
                        },
                        "vec_get" => {
                            let mut args_iter = compiled_args.into_iter();
                            let v_arg = args_iter.next().unwrap();
                            let index_arg = args_iter.next().unwrap();
                            goast::Expr::Index {
                                array: Box::new(v_arg),
                                index: Box::new(index_arg),
                                ty: tast_ty_to_go_type(ty),
                            }
                        }
                        "vec_set" => {
                            let vec_ty = imm_ty(&args[0]);
                            goast::Expr::Call {
                                func: Box::new(goast::Expr::Var {
                                    name: runtime::vec_helper_fn_name("vec_set", &vec_ty),
                                    ty: func_ty,
                                }),
                                args: compiled_args,
                                ty: tast_ty_to_go_type(ty),
                            }
                        }
                        "vec_len" => {
                            let mut args_iter = compiled_args.into_iter();
                            let v_arg = args_iter.next().unwrap();
                            goast::Expr::Call {
                                func: Box::new(goast::Expr::Var {
                                    name: "int32".to_string(),
                                    ty: goty::GoType::TFunc {
                                        params: vec![goty::GoType::TInt32],
                                        ret_ty: Box::new(goty::GoType::TInt32),
                                    },
                                }),
                                args: vec![goast::Expr::Call {
                                    func: Box::new(goast::Expr::Var {
                                        name: "len".to_string(),
                                        ty: goty::GoType::TFunc {
                                            params: vec![tast_ty_to_go_type(&imm_ty(&args[0]))],
                                            ret_ty: Box::new(goty::GoType::TInt32),
                                        },
                                    }),
                                    args: vec![v_arg],
                                    ty: goty::GoType::TInt32,
                                }],
                                ty: tast_ty_to_go_type(ty),
                            }
                        }
                        _ => unreachable!(),
                    }
                } else {
                    goast::Expr::Call {
                        func: Box::new(compile_imm(goenv, func)),
                        args: compiled_args,
                        ty: tast_ty_to_go_type(ty),
                    }
                }
            }
            anf::CExpr::EProj { tuple, index, ty } => {
                let obj = compile_imm(goenv, tuple);
                goast::Expr::FieldAccess {
                    obj: Box::new(obj),
                    field: format!("_{}", index),
                    ty: tast_ty_to_go_type(ty),
                }
            }
        }
    }

    fn compile_int_match_branch<F, E>(
        goenv: &GlobalGoEnv,
        scrutinee: &anf::ImmExpr,
        arms: &[anf::Arm],
        default: &Option<Box<anf::Block>>,
        build_branch: &mut F,
        extract: E,
    ) -> Vec<goast::Stmt>
    where
        F: FnMut(anf::AExpr) -> Vec<goast::Stmt>,
        E: Fn(&Prim) -> Option<String>,
    {
        let mut cases = Vec::new();
        for arm in arms {
            if let anf::ImmExpr::ImmPrim { value, .. } = &arm.lhs {
                if let Some(v) = extract(value) {
                    cases.push((
                        goast::Expr::Int {
                            value: v,
                            ty: tast_ty_to_go_type(&imm_ty(&arm.lhs)),
                        },
                        goast::Block {
                            stmts: build_branch(block_to_aexpr(arm.body.clone())),
                        },
                    ));
                } else {
                    panic!("expected integer primitive in match arm");
                }
            } else {
                panic!("expected primitive literal in integer match arm");
            }
        }
        let default_block = default.as_ref().map(|d| goast::Block {
            stmts: build_branch(block_to_aexpr((**d).clone())),
        });
        vec![goast::Stmt::SwitchExpr {
            expr: compile_imm(goenv, scrutinee),
            cases,
            default: default_block,
        }]
    }

    fn compile_float_match_branch<F, E>(
        goenv: &GlobalGoEnv,
        scrutinee: &anf::ImmExpr,
        arms: &[anf::Arm],
        default: &Option<Box<anf::Block>>,
        build_branch: &mut F,
        extract: E,
    ) -> Vec<goast::Stmt>
    where
        F: FnMut(anf::AExpr) -> Vec<goast::Stmt>,
        E: Fn(&Prim) -> Option<f64>,
    {
        let mut cases = Vec::new();
        for arm in arms {
            if let anf::ImmExpr::ImmPrim { value, .. } = &arm.lhs {
                if let Some(v) = extract(value) {
                    cases.push((
                        goast::Expr::Float {
                            value: v,
                            ty: tast_ty_to_go_type(&imm_ty(&arm.lhs)),
                        },
                        goast::Block {
                            stmts: build_branch(block_to_aexpr(arm.body.clone())),
                        },
                    ));
                } else {
                    panic!("expected float primitive in match arm");
                }
            } else {
                panic!("expected primitive literal in float match arm");
            }
        }
        let default_block = default.as_ref().map(|d| goast::Block {
            stmts: build_branch(block_to_aexpr((**d).clone())),
        });
        vec![goast::Stmt::SwitchExpr {
            expr: compile_imm(goenv, scrutinee),
            cases,
            default: default_block,
        }]
    }

    fn compile_match_branches<F>(
        goenv: &GlobalGoEnv,
        scrutinee: &anf::ImmExpr,
        arms: &[anf::Arm],
        default: &Option<Box<anf::Block>>,
        mut build_branch: F,
    ) -> Vec<goast::Stmt>
    where
        F: FnMut(anf::AExpr) -> Vec<goast::Stmt>,
    {
        match imm_ty(scrutinee) {
            tast::Ty::TUnit => {
                if let Some(first) = arms.first() {
                    return build_branch(block_to_aexpr(first.body.clone()));
                }
                if let Some(default_arm) = default.as_ref() {
                    return build_branch(block_to_aexpr((**default_arm).clone()));
                }
                Vec::new()
            }
            tast::Ty::TBool => {
                let mut cases = Vec::new();
                for arm in arms {
                    if let anf::ImmExpr::ImmPrim { value, .. } = &arm.lhs {
                        if let Some(bool_value) = value.as_bool() {
                            cases.push((
                                goast::Expr::Bool {
                                    value: bool_value,
                                    ty: tast_ty_to_go_type(&imm_ty(&arm.lhs)),
                                },
                                goast::Block {
                                    stmts: build_branch(block_to_aexpr(arm.body.clone())),
                                },
                            ));
                        } else {
                            panic!("expected boolean primitive in boolean match arm");
                        }
                    } else {
                        panic!("expected primitive literal in boolean match arm");
                    }
                }
                let default_block = default.as_ref().map(|d| goast::Block {
                    stmts: build_branch(block_to_aexpr((**d).clone())),
                });
                vec![goast::Stmt::SwitchExpr {
                    expr: compile_imm(goenv, scrutinee),
                    cases,
                    default: default_block,
                }]
            }
            tast::Ty::TInt8 => {
                compile_int_match_branch(goenv, scrutinee, arms, default, &mut build_branch, |v| {
                    v.as_int8().map(|x| x.to_string())
                })
            }
            tast::Ty::TInt16 => {
                compile_int_match_branch(goenv, scrutinee, arms, default, &mut build_branch, |v| {
                    v.as_int16().map(|x| x.to_string())
                })
            }
            tast::Ty::TInt32 => {
                compile_int_match_branch(goenv, scrutinee, arms, default, &mut build_branch, |v| {
                    v.as_int32().map(|x| x.to_string())
                })
            }
            tast::Ty::TInt64 => {
                compile_int_match_branch(goenv, scrutinee, arms, default, &mut build_branch, |v| {
                    v.as_int64().map(|x| x.to_string())
                })
            }
            tast::Ty::TUint8 => {
                compile_int_match_branch(goenv, scrutinee, arms, default, &mut build_branch, |v| {
                    v.as_uint8().map(|x| x.to_string())
                })
            }
            tast::Ty::TUint16 => {
                compile_int_match_branch(goenv, scrutinee, arms, default, &mut build_branch, |v| {
                    v.as_uint16().map(|x| x.to_string())
                })
            }
            tast::Ty::TUint32 => {
                compile_int_match_branch(goenv, scrutinee, arms, default, &mut build_branch, |v| {
                    v.as_uint32().map(|x| x.to_string())
                })
            }
            tast::Ty::TUint64 => {
                compile_int_match_branch(goenv, scrutinee, arms, default, &mut build_branch, |v| {
                    v.as_uint64().map(|x| x.to_string())
                })
            }
            tast::Ty::TChar => {
                compile_int_match_branch(goenv, scrutinee, arms, default, &mut build_branch, |v| {
                    v.as_char().map(|ch| (ch as u32).to_string())
                })
            }
            tast::Ty::TFloat32 => compile_float_match_branch(
                goenv,
                scrutinee,
                arms,
                default,
                &mut build_branch,
                |v| v.as_float32().map(|x| x as f64),
            ),
            tast::Ty::TFloat64 => compile_float_match_branch(
                goenv,
                scrutinee,
                arms,
                default,
                &mut build_branch,
                |v| v.as_float64(),
            ),
            tast::Ty::TString => {
                let mut cases = Vec::new();
                for arm in arms {
                    if let anf::ImmExpr::ImmPrim { value, .. } = &arm.lhs {
                        if let Some(str_value) = value.as_str() {
                            cases.push((
                                goast::Expr::String {
                                    value: str_value.to_string(),
                                    ty: tast_ty_to_go_type(&imm_ty(&arm.lhs)),
                                },
                                goast::Block {
                                    stmts: build_branch(block_to_aexpr(arm.body.clone())),
                                },
                            ));
                        } else {
                            panic!("expected string primitive in string match arm");
                        }
                    } else {
                        panic!("expected primitive literal in string match arm");
                    }
                }
                let default_block = default.as_ref().map(|d| goast::Block {
                    stmts: build_branch(block_to_aexpr((**d).clone())),
                });
                vec![goast::Stmt::SwitchExpr {
                    expr: compile_imm(goenv, scrutinee),
                    cases,
                    default: default_block,
                }]
            }
            tast::Ty::TEnum { .. } => {
                let scrutinee_name = match scrutinee {
                    anf::ImmExpr::ImmVar { name, .. } => name.clone(),
                    _ => {
                        unreachable!("expected scrutinee to be a variable after ANF lowering")
                    }
                };
                let mut cases = Vec::new();
                for arm in arms {
                    if let anf::ImmExpr::ImmTag { index, ty } = &arm.lhs {
                        let vty = variant_ty_by_index(goenv, ty, *index);
                        cases.push((
                            vty,
                            goast::Block {
                                stmts: build_branch(block_to_aexpr(arm.body.clone())),
                            },
                        ));
                    } else {
                        panic!("expected ImmTag in enum match arm");
                    }
                }
                let default_block = default.as_ref().map(|d| goast::Block {
                    stmts: build_branch(block_to_aexpr((**d).clone())),
                });
                vec![goast::Stmt::SwitchType {
                    bind: Some(scrutinee_name),
                    expr: compile_imm(goenv, scrutinee),
                    cases,
                    default: default_block,
                }]
            }
            tast::Ty::TStruct { .. } => {
                panic!("struct matches are not supported in Go backend")
            }
            tast::Ty::TApp { ty: base, .. } => match base.as_ref() {
                tast::Ty::TEnum { .. } => {
                    let scrutinee_name = match scrutinee {
                        anf::ImmExpr::ImmVar { name, .. } => name.clone(),
                        _ => {
                            unreachable!("expected scrutinee to be a variable after ANF lowering")
                        }
                    };
                    let mut cases = Vec::new();
                    for arm in arms {
                        if let anf::ImmExpr::ImmTag { index, ty } = &arm.lhs {
                            let vty = variant_ty_by_index(goenv, ty, *index);
                            cases.push((
                                vty,
                                goast::Block {
                                    stmts: build_branch(block_to_aexpr(arm.body.clone())),
                                },
                            ));
                        } else {
                            panic!("expected ImmTag in enum match arm");
                        }
                    }
                    let default_block = default.as_ref().map(|d| goast::Block {
                        stmts: build_branch(block_to_aexpr((**d).clone())),
                    });
                    vec![goast::Stmt::SwitchType {
                        bind: Some(scrutinee_name),
                        expr: compile_imm(goenv, scrutinee),
                        cases,
                        default: default_block,
                    }]
                }
                _ => panic!(
                    "unsupported scrutinee type TApp({:?}, ..) for match in Go backend",
                    base
                ),
            },
            _ => panic!("unsupported scrutinee type for match in Go backend"),
        }
    }

    fn compile_cexpr_effect(goenv: &GlobalGoEnv, expr: &anf::CExpr) -> Vec<goast::Stmt> {
        match expr {
            anf::CExpr::CImm { .. }
            | anf::CExpr::EConstr { .. }
            | anf::CExpr::ETuple { .. }
            | anf::CExpr::EArray { .. }
            | anf::CExpr::EConstrGet { .. }
            | anf::CExpr::EUnary { .. }
            | anf::CExpr::EBinary { .. }
            | anf::CExpr::EToDyn { .. }
            | anf::CExpr::EProj { .. } => Vec::new(),
            anf::CExpr::ECall { .. } | anf::CExpr::EDynCall { .. } => {
                vec![goast::Stmt::Expr(compile_cexpr(goenv, expr))]
            }
            anf::CExpr::EGo { closure, .. } => {
                vec![compile_go(goenv, closure)]
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

    fn compile_go(goenv: &GlobalGoEnv, closure: &anf::ImmExpr) -> goast::Stmt {
        let closure_ty = imm_ty(closure);
        let call_expr = if let Some(apply) = find_closure_apply_fn(goenv, &closure_ty) {
            let apply_call = anf::CExpr::ECall {
                func: anf::ImmExpr::ImmVar {
                    name: apply.name.clone(),
                    ty: apply.ty.clone(),
                },
                args: vec![closure.clone()],
                ty: apply.ret_ty.clone(),
            };
            compile_cexpr(goenv, &apply_call)
        } else {
            let tast::Ty::TFunc { params, ret_ty } = &closure_ty else {
                panic!(
                    "go statement expects a zero-arg function value or closure, got {:?}",
                    closure_ty
                );
            };
            if !params.is_empty() {
                panic!(
                    "go statement expects a zero-arg function value, got {:?}",
                    closure_ty
                );
            }
            goast::Expr::Call {
                func: Box::new(compile_imm(goenv, closure)),
                args: vec![],
                ty: tast_ty_to_go_type(ret_ty),
            }
        };
        goast::Stmt::Go { call: call_expr }
    }

    fn find_closure_apply_fn(goenv: &GlobalGoEnv, closure_ty: &tast::Ty) -> Option<ClosureApplyFn> {
        // Look up the apply method via inherent_impls
        let fn_ty = goenv.closure_apply_method(closure_ty)?;

        let tast::Ty::TFunc { params, ret_ty } = &fn_ty else {
            return None;
        };

        if params.first()? != closure_ty {
            return None;
        }

        let apply_name = inherent_method_fn_name(closure_ty, "apply");

        Some(ClosureApplyFn {
            name: apply_name,
            ty: fn_ty.clone(),
            ret_ty: (**ret_ty).clone(),
        })
    }

    fn block_to_aexpr(block: anf::Block) -> anf::AExpr {
        let mut expr = anf::AExpr::ACExpr { expr: block.tail };
        for stmt in block.stmts.into_iter().rev() {
            let body_ty = expr.get_ty();
            expr = anf::AExpr::ALet {
                name: stmt.name,
                value: Box::new(stmt.value),
                body: Box::new(expr),
                ty: body_ty,
            };
        }
        expr
    }

    fn compile_aexpr_effect(
        goenv: &GlobalGoEnv,
        gensym: &Gensym,
        e: anf::AExpr,
    ) -> Vec<goast::Stmt> {
        match e {
            AExpr::ACExpr { expr } => match expr {
                anf::CExpr::EIf {
                    cond, then, else_, ..
                } => {
                    let cond_e = compile_imm(goenv, &cond);
                    let then_block = goast::Block {
                        stmts: compile_aexpr_effect(goenv, gensym, block_to_aexpr(*then)),
                    };
                    let else_block = goast::Block {
                        stmts: compile_aexpr_effect(goenv, gensym, block_to_aexpr(*else_)),
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
                } => compile_match_branches(goenv, scrutinee.as_ref(), &arms, &default, |branch| {
                    compile_aexpr_effect(goenv, gensym, branch)
                }),
                anf::CExpr::EWhile { cond, body, .. } => compile_while(goenv, gensym, *cond, *body),
                other => compile_cexpr_effect(goenv, &other),
            },
            AExpr::ALet {
                name,
                value,
                body,
                ty: _,
            } => {
                let mut out = Vec::new();
                let value_expr = *value;

                match value_expr {
                    complex @ (anf::CExpr::EIf { .. }
                    | anf::CExpr::EMatch { .. }
                    | anf::CExpr::EWhile { .. }) => {
                        out.push(goast::Stmt::VarDecl {
                            name: go_ident(&name),
                            ty: cexpr_ty(goenv, &complex),
                            value: None,
                        });
                        out.extend(compile_aexpr_assign(
                            goenv,
                            gensym,
                            &name,
                            AExpr::ACExpr { expr: complex },
                        ));
                    }
                    anf::CExpr::EGo { ref closure, .. } => {
                        out.push(compile_go(goenv, closure));
                        out.push(goast::Stmt::VarDecl {
                            name: go_ident(&name),
                            ty: goty::GoType::TUnit,
                            value: Some(goast::Expr::Unit {
                                ty: goty::GoType::TUnit,
                            }),
                        });
                        out.extend(compile_aexpr_effect(goenv, gensym, *body));
                        return out;
                    }
                    simple @ anf::CExpr::ECall { .. } => {
                        out.push(goast::Stmt::VarDecl {
                            name: go_ident(&name),
                            ty: cexpr_ty(goenv, &simple),
                            value: Some(compile_cexpr(goenv, &simple)),
                        });
                    }
                    simple => {
                        out.push(goast::Stmt::VarDecl {
                            name: go_ident(&name),
                            ty: cexpr_ty(goenv, &simple),
                            value: Some(compile_cexpr(goenv, &simple)),
                        });
                    }
                }
                out.extend(compile_aexpr_effect(goenv, gensym, *body));
                out
            }
        }
    }

    fn compile_while(
        goenv: &GlobalGoEnv,
        gensym: &Gensym,
        cond: anf::Block,
        body: anf::Block,
    ) -> Vec<goast::Stmt> {
        let cond_ty = cond.get_ty();
        if cond_ty != tast::Ty::TBool {
            panic!("while condition must have type bool, got {:?}", cond_ty);
        }

        let cond_var = gensym.gensym("cond");
        let mut stmts = Vec::new();
        stmts.push(goast::Stmt::VarDecl {
            name: go_ident(&cond_var),
            ty: goty::GoType::TBool,
            value: None,
        });

        let mut loop_body = compile_aexpr_assign(goenv, gensym, &cond_var, block_to_aexpr(cond));
        let not_cond = goast::Expr::UnaryOp {
            op: goast::GoUnaryOp::Not,
            expr: Box::new(goast::Expr::Var {
                name: go_ident(&cond_var),
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
        loop_body.extend(compile_aexpr_effect(goenv, gensym, block_to_aexpr(body)));

        stmts.push(goast::Stmt::Loop {
            body: goast::Block { stmts: loop_body },
            label: None,
        });
        stmts
    }

    fn compile_aexpr_assign(
        goenv: &GlobalGoEnv,
        gensym: &Gensym,
        target: &str,
        e: anf::AExpr,
    ) -> Vec<goast::Stmt> {
        match e {
            AExpr::ACExpr { expr } => match expr {
                anf::CExpr::EIf {
                    cond, then, else_, ..
                } => {
                    let cond_e = compile_imm(goenv, &cond);
                    let then_stmts =
                        compile_aexpr_assign(goenv, gensym, target, block_to_aexpr(*then));
                    let else_stmts =
                        compile_aexpr_assign(goenv, gensym, target, block_to_aexpr(*else_));
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
                } => compile_match_branches(goenv, scrutinee.as_ref(), &arms, &default, |branch| {
                    compile_aexpr_assign(goenv, gensym, target, branch)
                }),
                anf::CExpr::EWhile { cond, body, .. } => {
                    let mut stmts = compile_while(goenv, gensym, *cond, *body);
                    stmts.push(goast::Stmt::Assignment {
                        name: go_ident(target),
                        value: goast::Expr::Unit {
                            ty: goty::GoType::TUnit,
                        },
                    });
                    stmts
                }
                other @ (anf::CExpr::CImm { .. }
                | anf::CExpr::EConstr { .. }
                | anf::CExpr::EConstrGet { .. }
                | anf::CExpr::EUnary { .. }
                | anf::CExpr::EBinary { .. }
                | anf::CExpr::EToDyn { .. }
                | anf::CExpr::EProj { .. }
                | anf::CExpr::ETuple { .. }
                | anf::CExpr::EArray { .. }) => vec![goast::Stmt::Assignment {
                    name: go_ident(target),
                    value: compile_cexpr(goenv, &other),
                }],
                anf::CExpr::ECall { func, args, ty } => {
                    vec![goast::Stmt::Assignment {
                        name: go_ident(target),
                        value: compile_cexpr(goenv, &anf::CExpr::ECall { func, args, ty }),
                    }]
                }
                anf::CExpr::EDynCall {
                    trait_name,
                    method_name,
                    receiver,
                    args,
                    ty,
                } => {
                    vec![goast::Stmt::Assignment {
                        name: go_ident(target),
                        value: compile_cexpr(
                            goenv,
                            &anf::CExpr::EDynCall {
                                trait_name,
                                method_name,
                                receiver,
                                args,
                                ty,
                            },
                        ),
                    }]
                }
                anf::CExpr::EGo { closure, .. } => {
                    vec![
                        compile_go(goenv, &closure),
                        goast::Stmt::Assignment {
                            name: go_ident(target),
                            value: goast::Expr::Unit {
                                ty: goty::GoType::TUnit,
                            },
                        },
                    ]
                }
            },
            AExpr::ALet {
                name,
                value,
                body,
                ty: _,
            } => {
                let mut out = Vec::new();
                let value_expr = *value;

                match value_expr {
                    complex @ (anf::CExpr::EIf { .. }
                    | anf::CExpr::EMatch { .. }
                    | anf::CExpr::EWhile { .. }) => {
                        out.push(goast::Stmt::VarDecl {
                            name: go_ident(&name),
                            ty: cexpr_ty(goenv, &complex),
                            value: None,
                        });
                        out.extend(compile_aexpr_assign(
                            goenv,
                            gensym,
                            &name,
                            AExpr::ACExpr { expr: complex },
                        ));
                    }
                    anf::CExpr::EGo { ref closure, .. } => {
                        out.push(compile_go(goenv, closure));
                        out.push(goast::Stmt::VarDecl {
                            name: go_ident(&name),
                            ty: goty::GoType::TUnit,
                            value: Some(goast::Expr::Unit {
                                ty: goty::GoType::TUnit,
                            }),
                        });
                        out.extend(compile_aexpr_assign(goenv, gensym, target, *body));
                        return out;
                    }
                    simple @ anf::CExpr::ECall { .. } => {
                        out.push(goast::Stmt::VarDecl {
                            name: go_ident(&name),
                            ty: cexpr_ty(goenv, &simple),
                            value: Some(compile_cexpr(goenv, &simple)),
                        });
                    }
                    simple => {
                        out.push(goast::Stmt::VarDecl {
                            name: go_ident(&name),
                            ty: cexpr_ty(goenv, &simple),
                            value: Some(compile_cexpr(goenv, &simple)),
                        });
                    }
                }

                out.extend(compile_aexpr_assign(goenv, gensym, target, *body));
                out
            }
        }
    }

    fn compile_aexpr(goenv: &GlobalGoEnv, gensym: &Gensym, e: anf::AExpr) -> Vec<goast::Stmt> {
        let mut stmts = Vec::new();
        match e {
            AExpr::ACExpr { expr } => match expr {
                // Lower conditional expressions to if-statements with returns in branches
                anf::CExpr::EIf {
                    cond, then, else_, ..
                } => {
                    let cond_e = compile_imm(goenv, &cond);
                    let then_block = goast::Block {
                        stmts: compile_aexpr(goenv, gensym, block_to_aexpr(*then)),
                    };
                    let else_block = goast::Block {
                        stmts: compile_aexpr(goenv, gensym, block_to_aexpr(*else_)),
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
                        goenv,
                        scrutinee.as_ref(),
                        &arms,
                        &default,
                        |branch| compile_aexpr(goenv, gensym, branch),
                    ));
                }
                anf::CExpr::EWhile { cond, body, .. } => {
                    stmts.extend(compile_while(goenv, gensym, *cond, *body));
                    stmts.push(goast::Stmt::Return {
                        expr: Some(goast::Expr::Unit {
                            ty: goty::GoType::TUnit,
                        }),
                    });
                }
                _ => {
                    let e = compile_cexpr(goenv, &expr);
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
                let value_expr = *value;

                match value_expr {
                    // If RHS needs statements, declare then fill via if-lowering
                    complex @ (anf::CExpr::EIf { .. }
                    | anf::CExpr::EMatch { .. }
                    | anf::CExpr::EWhile { .. }) => {
                        stmts.push(goast::Stmt::VarDecl {
                            name: go_ident(&name),
                            ty: cexpr_ty(goenv, &complex),
                            value: None,
                        });
                        stmts.extend(compile_aexpr_assign(
                            goenv,
                            gensym,
                            &name,
                            AExpr::ACExpr { expr: complex },
                        ));
                    }
                    anf::CExpr::EGo { ref closure, .. } => {
                        stmts.push(compile_go(goenv, closure));
                        stmts.push(goast::Stmt::VarDecl {
                            name: go_ident(&name),
                            ty: goty::GoType::TUnit,
                            value: Some(goast::Expr::Unit {
                                ty: goty::GoType::TUnit,
                            }),
                        });
                        stmts.extend(compile_aexpr(goenv, gensym, *body));
                        return stmts;
                    }
                    simple @ anf::CExpr::ECall { .. } => {
                        stmts.push(goast::Stmt::VarDecl {
                            name: go_ident(&name),
                            ty: cexpr_ty(goenv, &simple),
                            value: Some(compile_cexpr(goenv, &simple)),
                        });
                    }
                    simple => {
                        stmts.push(goast::Stmt::VarDecl {
                            name: go_ident(&name),
                            ty: cexpr_ty(goenv, &simple),
                            value: Some(compile_cexpr(goenv, &simple)),
                        });
                    }
                }
                stmts.extend(compile_aexpr(goenv, gensym, *body));
            }
        }
        stmts
    }

    fn compile_fn(goenv: &GlobalGoEnv, gensym: &Gensym, f: anf::Fn) -> goast::Fn {
        let mut params = Vec::new();
        for (name, ty) in f.params {
            params.push((go_ident(&name), tast_ty_to_go_type(&ty)));
        }

        let go_ret_ty = tast_ty_to_go_type(&f.ret_ty);

        let is_entry = f.name == ENTRY_FUNCTION;
        let patched_name = if is_entry {
            ENTRY_WRAPPER_FUNCTION.to_string()
        } else {
            go_toplevel_func_name(goenv, &f.name)
        };

        let body = block_to_aexpr(f.body);

        let (ret_ty, body_stmts) = match go_ret_ty {
            goty::GoType::TVoid => (None, compile_aexpr(goenv, gensym, body)),
            _ => {
                let ret_name = gensym.gensym("ret");
                let mut stmts = Vec::new();

                stmts.push(goast::Stmt::VarDecl {
                    name: go_ident(&ret_name),
                    ty: go_ret_ty.clone(),
                    value: None,
                });

                stmts.extend(compile_aexpr_assign(goenv, gensym, &ret_name, body));

                stmts.push(goast::Stmt::Return {
                    expr: Some(goast::Expr::Var {
                        name: go_ident(&ret_name),
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
}

#[derive(Debug, Clone)]
struct ClosureApplyFn {
    name: String,
    ty: tast::Ty,
    ret_ty: tast::Ty,
}

fn find_closure_apply_fn(goenv: &GlobalGoEnv, closure_ty: &tast::Ty) -> Option<ClosureApplyFn> {
    let fn_ty = goenv.closure_apply_method(closure_ty)?;
    let tast::Ty::TFunc { params, ret_ty } = &fn_ty else {
        return None;
    };
    if params.first()? != closure_ty {
        return None;
    }

    let apply_name = inherent_method_fn_name(closure_ty, "apply");
    Some(ClosureApplyFn {
        name: apply_name,
        ty: fn_ty.clone(),
        ret_ty: (**ret_ty).clone(),
    })
}

fn compile_go(goenv: &GlobalGoEnv, closure: &anf::ImmExpr) -> goast::Stmt {
    let closure_ty = imm_ty(closure);
    let call_expr = if let Some(apply) = find_closure_apply_fn(goenv, &closure_ty) {
        let apply_expr = goast::Expr::Var {
            name: go_ident(&apply.name),
            ty: tast_ty_to_go_type(&apply.ty),
        };
        goast::Expr::Call {
            func: Box::new(apply_expr),
            args: vec![compile_imm(goenv, closure)],
            ty: tast_ty_to_go_type(&apply.ret_ty),
        }
    } else {
        let tast::Ty::TFunc { params, ret_ty } = &closure_ty else {
            panic!(
                "go statement expects a zero-arg function value or closure, got {:?}",
                closure_ty
            );
        };
        if !params.is_empty() {
            panic!(
                "go statement expects a zero-arg function value, got {:?}",
                closure_ty
            );
        }
        goast::Expr::Call {
            func: Box::new(compile_imm(goenv, closure)),
            args: vec![],
            ty: tast_ty_to_go_type(ret_ty),
        }
    };

    goast::Stmt::Go { call: call_expr }
}

#[derive(Debug)]
struct CompiledValue {
    stmts: Vec<goast::Stmt>,
    expr: goast::Expr,
}

fn compile_call(
    goenv: &GlobalGoEnv,
    func: &anf::ImmExpr,
    args: &[anf::ImmExpr],
    ty: &tast::Ty,
) -> goast::Expr {
    let func_tast_ty = imm_ty(func);
    let param_types: Vec<tast::Ty> = match &func_tast_ty {
        tast::Ty::TFunc { params, .. } => params.clone(),
        _ => Vec::new(),
    };
    let compiled_args = compile_call_args(goenv, &param_types, args);
    let func_ty = tast_ty_to_go_type(&func_tast_ty);

    if let anf::ImmExpr::Var { id, .. } = func
        && runtime_builtin_available(goenv, &id.0)
        && id.0 == "missing"
    {
        let helper_name = runtime::missing_helper_fn_name(ty);
        return goast::Expr::Call {
            func: Box::new(goast::Expr::Var {
                name: helper_name,
                ty: goty::GoType::TFunc {
                    params: vec![goty::GoType::TString],
                    ret_ty: Box::new(tast_ty_to_go_type(ty)),
                },
            }),
            args: compiled_args,
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let anf::ImmExpr::Var { id, .. } = func
        && runtime_builtin_available(goenv, &id.0)
        && (id.0 == "array_get" || id.0 == "array_set")
    {
        let array_ty = imm_ty(&args[0]);
        let helper = runtime::array_helper_fn_name(&id.0, &array_ty);
        return goast::Expr::Call {
            func: Box::new(goast::Expr::Var {
                name: helper,
                ty: func_ty,
            }),
            args: compiled_args,
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let anf::ImmExpr::Var { id, .. } = func
        && runtime_builtin_available(goenv, &id.0)
        && (id.0 == "ref" || id.0 == "ref_get" || id.0 == "ref_set" || id.0 == "ptr_eq")
    {
        let (helper, helper_ty) = if id.0 == "ref" {
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
        } else if id.0 == "ptr_eq" {
            let ref_ty = imm_ty(&args[0]);
            let tast::Ty::TRef { .. } = &ref_ty else {
                panic!("ptr_eq expects reference arguments, got {:?}", ref_ty);
            };
            let ref_go_ty = tast_ty_to_go_type(&ref_ty);
            (
                runtime::ref_helper_fn_name("ptr_eq", &ref_ty),
                goty::GoType::TFunc {
                    params: vec![ref_go_ty.clone(), ref_go_ty.clone()],
                    ret_ty: Box::new(goty::GoType::TBool),
                },
            )
        } else {
            let ref_ty = imm_ty(&args[0]);
            let tast::Ty::TRef { elem } = &ref_ty else {
                panic!("{} expects reference argument, got {:?}", id.0, ref_ty);
            };
            let ref_go_ty = tast_ty_to_go_type(&ref_ty);
            let elem_go_ty = tast_ty_to_go_type(elem);
            let ret_ty = if id.0 == "ref_get" {
                elem_go_ty.clone()
            } else {
                goty::GoType::TUnit
            };
            (
                runtime::ref_helper_fn_name(&id.0, &ref_ty),
                goty::GoType::TFunc {
                    params: if id.0 == "ref_get" {
                        vec![ref_go_ty.clone()]
                    } else {
                        vec![ref_go_ty.clone(), elem_go_ty.clone()]
                    },
                    ret_ty: Box::new(ret_ty),
                },
            )
        };

        return goast::Expr::Call {
            func: Box::new(goast::Expr::Var {
                name: helper,
                ty: helper_ty,
            }),
            args: compiled_args,
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let anf::ImmExpr::Var { id, .. } = func
        && runtime_builtin_available(goenv, &id.0)
        && (id.0 == "hashmap_new"
            || id.0 == "hashmap_get"
            || id.0 == "hashmap_set"
            || id.0 == "hashmap_remove"
            || id.0 == "hashmap_len"
            || id.0 == "hashmap_contains")
    {
        let map_ty = if id.0 == "hashmap_new" {
            ty.clone()
        } else {
            imm_ty(&args[0])
        };
        let tast::Ty::THashMap { key, value } = &map_ty else {
            panic!("{} expects HashMap type, got {:?}", id.0, map_ty);
        };

        let map_go_ty = tast_ty_to_go_type(&map_ty);
        let key_go_ty = tast_ty_to_go_type(key);
        let value_go_ty = tast_ty_to_go_type(value);

        let helper = runtime::hashmap_helper_fn_name(&id.0, &map_ty);
        let helper_ty = match id.0.as_str() {
            "hashmap_new" => goty::GoType::TFunc {
                params: vec![],
                ret_ty: Box::new(map_go_ty.clone()),
            },
            "hashmap_get" => goty::GoType::TFunc {
                params: vec![map_go_ty.clone(), key_go_ty.clone()],
                ret_ty: Box::new(tast_ty_to_go_type(ty)),
            },
            "hashmap_set" => goty::GoType::TFunc {
                params: vec![map_go_ty.clone(), key_go_ty.clone(), value_go_ty.clone()],
                ret_ty: Box::new(goty::GoType::TUnit),
            },
            "hashmap_remove" => goty::GoType::TFunc {
                params: vec![map_go_ty.clone(), key_go_ty.clone()],
                ret_ty: Box::new(goty::GoType::TUnit),
            },
            "hashmap_len" => goty::GoType::TFunc {
                params: vec![map_go_ty.clone()],
                ret_ty: Box::new(goty::GoType::TInt32),
            },
            "hashmap_contains" => goty::GoType::TFunc {
                params: vec![map_go_ty.clone(), key_go_ty.clone()],
                ret_ty: Box::new(goty::GoType::TBool),
            },
            _ => unreachable!(),
        };

        return goast::Expr::Call {
            func: Box::new(goast::Expr::Var {
                name: helper,
                ty: helper_ty,
            }),
            args: compiled_args,
            ty: tast_ty_to_go_type(ty),
        };
    }

    if let anf::ImmExpr::Var { id, .. } = func
        && runtime_builtin_available(goenv, &id.0)
        && (id.0 == "slice" || id.0 == "slice_get" || id.0 == "slice_len" || id.0 == "slice_sub")
    {
        let arg0_ty = imm_ty(&args[0]);
        return match id.0.as_str() {
            "slice" | "slice_sub" => {
                let mut args_iter = compiled_args.into_iter();
                let array_arg = args_iter.next().unwrap();
                let start_arg = args_iter.next().unwrap();
                let end_arg = args_iter.next().unwrap();
                goast::Expr::Slice {
                    array: Box::new(array_arg),
                    start: Box::new(start_arg),
                    end: Box::new(end_arg),
                    ty: tast_ty_to_go_type(ty),
                }
            }
            "slice_get" => {
                let mut args_iter = compiled_args.into_iter();
                let slice_arg = args_iter.next().unwrap();
                let index_arg = args_iter.next().unwrap();
                goast::Expr::Index {
                    array: Box::new(slice_arg),
                    index: Box::new(index_arg),
                    ty: tast_ty_to_go_type(ty),
                }
            }
            "slice_len" => {
                let mut args_iter = compiled_args.into_iter();
                let slice_arg = args_iter.next().unwrap();
                goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "int32".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TInt32],
                            ret_ty: Box::new(goty::GoType::TInt32),
                        },
                    }),
                    args: vec![goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: "len".to_string(),
                            ty: goty::GoType::TFunc {
                                params: vec![tast_ty_to_go_type(&arg0_ty)],
                                ret_ty: Box::new(goty::GoType::TInt32),
                            },
                        }),
                        args: vec![slice_arg],
                        ty: goty::GoType::TInt32,
                    }],
                    ty: tast_ty_to_go_type(ty),
                }
            }
            _ => unreachable!(),
        };
    }

    if let anf::ImmExpr::Var { id, .. } = func
        && runtime_builtin_available(goenv, &id.0)
        && (id.0 == "vec_new"
            || id.0 == "vec_push"
            || id.0 == "vec_get"
            || id.0 == "vec_set"
            || id.0 == "vec_len")
    {
        return match id.0.as_str() {
            "vec_new" => goast::Expr::Nil {
                ty: tast_ty_to_go_type(ty),
            },
            "vec_push" => goast::Expr::Call {
                func: Box::new(goast::Expr::Var {
                    name: "append".to_string(),
                    ty: func_ty,
                }),
                args: compiled_args,
                ty: tast_ty_to_go_type(ty),
            },
            "vec_get" => {
                let mut args_iter = compiled_args.into_iter();
                let v_arg = args_iter.next().unwrap();
                let index_arg = args_iter.next().unwrap();
                goast::Expr::Index {
                    array: Box::new(v_arg),
                    index: Box::new(index_arg),
                    ty: tast_ty_to_go_type(ty),
                }
            }
            "vec_set" => {
                let vec_ty = imm_ty(&args[0]);
                goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: runtime::vec_helper_fn_name("vec_set", &vec_ty),
                        ty: func_ty,
                    }),
                    args: compiled_args,
                    ty: tast_ty_to_go_type(ty),
                }
            }
            "vec_len" => {
                let arg0_ty = imm_ty(args.first().unwrap());
                let mut args_iter = compiled_args.into_iter();
                let v_arg = args_iter.next().unwrap();
                goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "int32".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TInt32],
                            ret_ty: Box::new(goty::GoType::TInt32),
                        },
                    }),
                    args: vec![goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: "len".to_string(),
                            ty: goty::GoType::TFunc {
                                params: vec![tast_ty_to_go_type(&arg0_ty)],
                                ret_ty: Box::new(goty::GoType::TInt32),
                            },
                        }),
                        args: vec![v_arg],
                        ty: goty::GoType::TInt32,
                    }],
                    ty: tast_ty_to_go_type(ty),
                }
            }
            _ => unreachable!(),
        };
    }

    if let tast::Ty::TStruct { name } = &func_tast_ty
        && is_closure_env_struct(name)
        && let Some(apply) = find_closure_apply_fn(goenv, &func_tast_ty)
    {
        let apply_expr = goast::Expr::Var {
            name: go_ident(&apply.name),
            ty: tast_ty_to_go_type(&apply.ty),
        };
        let mut call_args = vec![compile_imm(goenv, func)];
        call_args.extend(compiled_args);
        return goast::Expr::Call {
            func: Box::new(apply_expr),
            args: call_args,
            ty: tast_ty_to_go_type(&apply.ret_ty),
        };
    }

    goast::Expr::Call {
        func: Box::new(compile_imm(goenv, func)),
        args: compiled_args,
        ty: tast_ty_to_go_type(ty),
    }
}

fn compile_call_args(
    goenv: &GlobalGoEnv,
    param_types: &[tast::Ty],
    args: &[anf::ImmExpr],
) -> Vec<goast::Expr> {
    args.iter()
        .enumerate()
        .map(|(i, arg)| {
            let arg_ty = imm_ty(arg);
            if let Some(param_ty) = param_types.get(i)
                && needs_closure_to_func_wrap(&arg_ty, param_ty)
            {
                return closure_to_func_lit(goenv, arg, param_ty);
            }
            compile_imm(goenv, arg)
        })
        .collect()
}

fn compile_value_expr(goenv: &GlobalGoEnv, expr: &anf::ValueExpr) -> CompiledValue {
    match expr {
        anf::ValueExpr::Imm(imm) => CompiledValue {
            stmts: Vec::new(),
            expr: compile_imm(goenv, imm),
        },
        anf::ValueExpr::Constr {
            constructor,
            args,
            ty,
        } => match constructor {
            Constructor::Enum(enum_constructor) => {
                let variant_ty = variant_ty_by_index(goenv, ty, enum_constructor.index);
                let variant_field_types = goenv
                    .get_enum(&enum_constructor.type_name)
                    .and_then(|def| def.variants.get(enum_constructor.index))
                    .map(|(_, fields)| fields.as_slice());
                let fields = args
                    .iter()
                    .enumerate()
                    .map(|(i, a)| {
                        let field_ty = variant_field_types.and_then(|f| f.get(i));
                        let arg_ty = imm_ty(a);
                        let val = if let Some(ft) = field_ty
                            && needs_closure_to_func_wrap(&arg_ty, ft)
                        {
                            closure_to_func_lit(goenv, a, ft)
                        } else {
                            compile_imm(goenv, a)
                        };
                        (format!("_{}", i), val)
                    })
                    .collect();
                CompiledValue {
                    stmts: Vec::new(),
                    expr: goast::Expr::StructLiteral {
                        ty: variant_ty,
                        fields,
                    },
                }
            }
            Constructor::Struct(struct_constructor) => {
                let go_ty = tast_ty_to_go_type(ty);
                let struct_def = goenv
                    .get_struct(&struct_constructor.type_name)
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
                    .map(|((fname, field_ty), arg)| {
                        let arg_ty = imm_ty(arg);
                        let val = if needs_closure_to_func_wrap(&arg_ty, field_ty) {
                            closure_to_func_lit(goenv, arg, field_ty)
                        } else {
                            compile_imm(goenv, arg)
                        };
                        (go_ident(&fname.0), val)
                    })
                    .collect();
                CompiledValue {
                    stmts: Vec::new(),
                    expr: goast::Expr::StructLiteral { ty: go_ty, fields },
                }
            }
        },
        anf::ValueExpr::Tuple { items, ty } => {
            let orig_typs = match ty {
                tast::Ty::TTuple { typs } => Some(typs.as_slice()),
                _ => None,
            };
            let fields = items
                .iter()
                .enumerate()
                .map(|(i, a)| {
                    let val = if let Some(field_ty) = orig_typs.and_then(|ts| ts.get(i)) {
                        let arg_ty = imm_ty(a);
                        if needs_closure_to_func_wrap(&arg_ty, field_ty) {
                            closure_to_func_lit(goenv, a, field_ty)
                        } else {
                            compile_imm(goenv, a)
                        }
                    } else {
                        compile_imm(goenv, a)
                    };
                    (format!("_{}", i), val)
                })
                .collect();
            CompiledValue {
                stmts: Vec::new(),
                expr: goast::Expr::StructLiteral {
                    ty: tuple_to_go_struct_type(ty),
                    fields,
                },
            }
        }
        anf::ValueExpr::Array { items, ty } => {
            let elem_ty = match ty {
                tast::Ty::TArray { elem, .. } => elem.as_ref(),
                _ => ty,
            };
            let elems = items
                .iter()
                .map(|item| {
                    let arg_ty = imm_ty(item);
                    if needs_closure_to_func_wrap(&arg_ty, elem_ty) {
                        closure_to_func_lit(goenv, item, elem_ty)
                    } else {
                        compile_imm(goenv, item)
                    }
                })
                .collect();
            CompiledValue {
                stmts: Vec::new(),
                expr: goast::Expr::ArrayLiteral {
                    elems,
                    ty: tast_ty_to_go_type(ty),
                },
            }
        }
        anf::ValueExpr::ConstrGet {
            expr,
            constructor,
            field_index,
            ty: _,
        } => {
            let obj = compile_imm(goenv, expr);
            match constructor {
                Constructor::Enum(enum_constructor) => {
                    let def = goenv
                        .get_enum(&enum_constructor.type_name)
                        .expect("unknown enum in ConstrGet");
                    let field_ty = def.variants[enum_constructor.index].1[*field_index].clone();
                    let scrut_ty = imm_ty(expr);
                    let variant_ty = variant_ty_by_index(goenv, &scrut_ty, enum_constructor.index);
                    let cast = goast::Expr::Cast {
                        expr: Box::new(obj),
                        ty: variant_ty,
                    };
                    CompiledValue {
                        stmts: Vec::new(),
                        expr: goast::Expr::FieldAccess {
                            obj: Box::new(cast),
                            field: format!("_{}", field_index),
                            ty: tast_ty_to_go_type(&field_ty),
                        },
                    }
                }
                Constructor::Struct(struct_constructor) => {
                    let scrut_ty = imm_ty(expr);
                    let (ty_name, type_args) = match scrut_ty {
                        tast::Ty::TStruct { name } => (name, Vec::new()),
                        tast::Ty::TApp { ty, args } => {
                            let base_name = ty.get_constr_name_unsafe();
                            (base_name, args)
                        }
                        other => panic!(
                            "ConstrGet on non-struct type {:?} for constructor {}",
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
                        instantiate_struct_fields(goenv, &struct_constructor.type_name, &type_args);
                    let (field_name, field_ty) = &fields[*field_index];
                    CompiledValue {
                        stmts: Vec::new(),
                        expr: goast::Expr::FieldAccess {
                            obj: Box::new(obj),
                            field: go_ident(field_name),
                            ty: tast_ty_to_go_type(field_ty),
                        },
                    }
                }
            }
        }
        anf::ValueExpr::Unary { op, expr, ty } => {
            let go_op = match op {
                common_defs::UnaryOp::Neg => goast::GoUnaryOp::Neg,
                common_defs::UnaryOp::Not => goast::GoUnaryOp::Not,
            };
            CompiledValue {
                stmts: Vec::new(),
                expr: goast::Expr::UnaryOp {
                    op: go_op,
                    expr: Box::new(compile_imm(goenv, expr)),
                    ty: tast_ty_to_go_type(ty),
                },
            }
        }
        anf::ValueExpr::Binary { op, lhs, rhs, ty } => {
            let go_op = match op {
                common_defs::BinaryOp::Add => goast::GoBinaryOp::Add,
                common_defs::BinaryOp::Sub => goast::GoBinaryOp::Sub,
                common_defs::BinaryOp::Mul => goast::GoBinaryOp::Mul,
                common_defs::BinaryOp::Div => goast::GoBinaryOp::Div,
                common_defs::BinaryOp::And => goast::GoBinaryOp::And,
                common_defs::BinaryOp::Or => goast::GoBinaryOp::Or,
                common_defs::BinaryOp::Less => goast::GoBinaryOp::Less,
                common_defs::BinaryOp::Greater => goast::GoBinaryOp::Greater,
                common_defs::BinaryOp::LessEq => goast::GoBinaryOp::LessEq,
                common_defs::BinaryOp::GreaterEq => goast::GoBinaryOp::GreaterEq,
                common_defs::BinaryOp::Eq => goast::GoBinaryOp::Eq,
                common_defs::BinaryOp::NotEq => goast::GoBinaryOp::NotEq,
            };
            CompiledValue {
                stmts: Vec::new(),
                expr: goast::Expr::BinaryOp {
                    op: go_op,
                    lhs: Box::new(compile_imm(goenv, lhs)),
                    rhs: Box::new(compile_imm(goenv, rhs)),
                    ty: tast_ty_to_go_type(ty),
                },
            }
        }
        anf::ValueExpr::Assign {
            name,
            value,
            target_ty,
            ty,
        } => {
            let mut stmts = Vec::new();
            let rhs = if needs_closure_to_func_wrap(&imm_ty(value), target_ty) {
                closure_to_func_lit(goenv, value, target_ty)
            } else {
                compile_imm(goenv, value)
            };
            stmts.push(goast::Stmt::Assignment {
                name: go_ident(&name.0),
                value: rhs,
            });
            CompiledValue {
                stmts,
                expr: goast::Expr::Unit {
                    ty: tast_ty_to_go_type(ty),
                },
            }
        }
        anf::ValueExpr::Call { func, args, ty } => {
            if let anf::ImmExpr::Var { id, .. } = func
                && runtime_builtin_available(goenv, &id.0)
                && (id.0 == "print" || id.0 == "println")
            {
                let call_expr = goast::Expr::Call {
                    func: Box::new(compile_imm(goenv, func)),
                    args: args.iter().map(|arg| compile_imm(goenv, arg)).collect(),
                    ty: goty::GoType::TVoid,
                };
                CompiledValue {
                    stmts: vec![goast::Stmt::Expr(call_expr)],
                    expr: goast::Expr::Unit {
                        ty: tast_ty_to_go_type(ty),
                    },
                }
            } else {
                CompiledValue {
                    stmts: Vec::new(),
                    expr: compile_call(goenv, func, args, ty),
                }
            }
        }
        anf::ValueExpr::ToDyn {
            trait_name,
            for_ty,
            expr,
            ty,
        } => {
            let dyn_struct_ty = tast_ty_to_go_type(ty);
            let vtable_struct_name = dyn_vtable_struct_go_name(&trait_name.0);
            let vtable_ptr_ty = goty::GoType::TPointer {
                elem: Box::new(goty::GoType::TName {
                    name: vtable_struct_name,
                }),
            };

            let ctor_name = dyn_vtable_ctor_go_name(goenv, &trait_name.0, for_ty);
            let ctor_ty = goty::GoType::TFunc {
                params: vec![],
                ret_ty: Box::new(vtable_ptr_ty.clone()),
            };
            let vtable_expr = goast::Expr::Call {
                func: Box::new(goast::Expr::Var {
                    name: ctor_name,
                    ty: ctor_ty,
                }),
                args: vec![],
                ty: vtable_ptr_ty,
            };

            CompiledValue {
                stmts: Vec::new(),
                expr: goast::Expr::StructLiteral {
                    fields: vec![
                        ("data".to_string(), {
                            let data_expr_ty = imm_ty(expr);
                            let needs_closure_wrap = matches!(
                                &data_expr_ty,
                                tast::Ty::TStruct { name } if is_closure_env_struct(name)
                            ) && matches!(for_ty, tast::Ty::TFunc { .. });
                            if needs_closure_wrap {
                                closure_to_func_lit(goenv, expr, for_ty)
                            } else {
                                let compiled = compile_imm(goenv, expr);
                                ensure_typed_for_any(compiled, for_ty)
                            }
                        }),
                        ("vtable".to_string(), vtable_expr),
                    ],
                    ty: dyn_struct_ty,
                },
            }
        }
        anf::ValueExpr::DynCall {
            trait_name,
            method_name,
            receiver,
            args,
            ty,
        } => {
            let receiver_expr_for_vtable = compile_imm(goenv, receiver);
            let receiver_expr_for_data = compile_imm(goenv, receiver);

            let (method_params, method_ret) = trait_method_sigs(goenv, &trait_name.0)
                .into_iter()
                .find(|(name, _, _)| name == &method_name.0)
                .map(|(_name, params, ret)| (params, ret))
                .unwrap_or_else(|| {
                    panic!("missing trait method {}::{}", trait_name.0, method_name.0)
                });

            let mut fn_params = Vec::with_capacity(method_params.len() + 1);
            fn_params.push(any_go_type());
            fn_params.extend(method_params.iter().map(tast_ty_to_go_type));
            let fn_ret = tast_ty_to_go_type(&method_ret);

            let vtable_ptr_expr = goast::Expr::FieldAccess {
                obj: Box::new(receiver_expr_for_vtable),
                field: "vtable".to_string(),
                ty: goty::GoType::TPointer {
                    elem: Box::new(goty::GoType::TName {
                        name: dyn_vtable_struct_go_name(&trait_name.0),
                    }),
                },
            };
            let method_expr = goast::Expr::FieldAccess {
                obj: Box::new(vtable_ptr_expr),
                field: go_ident(&method_name.0),
                ty: goty::GoType::TFunc {
                    params: fn_params,
                    ret_ty: Box::new(fn_ret.clone()),
                },
            };

            let mut call_args = Vec::with_capacity(args.len() + 1);
            call_args.push(goast::Expr::FieldAccess {
                obj: Box::new(receiver_expr_for_data),
                field: "data".to_string(),
                ty: any_go_type(),
            });
            call_args.extend(compile_call_args(goenv, &method_params, args));

            CompiledValue {
                stmts: Vec::new(),
                expr: goast::Expr::Call {
                    func: Box::new(method_expr),
                    args: call_args,
                    ty: tast_ty_to_go_type(ty),
                },
            }
        }
        anf::ValueExpr::Go { closure, ty } => CompiledValue {
            stmts: vec![compile_go(goenv, closure)],
            expr: goast::Expr::Unit {
                ty: tast_ty_to_go_type(ty),
            },
        },
        anf::ValueExpr::Proj { tuple, index, ty } => {
            let obj = compile_imm(goenv, tuple);
            CompiledValue {
                stmts: Vec::new(),
                expr: goast::Expr::FieldAccess {
                    obj: Box::new(obj),
                    field: format!("_{}", index),
                    ty: tast_ty_to_go_type(ty),
                },
            }
        }
    }
}

fn panic_stmt(msg: &str) -> goast::Stmt {
    goast::Stmt::Expr(goast::Expr::Call {
        func: Box::new(goast::Expr::Var {
            name: "panic".to_string(),
            ty: goty::GoType::TFunc {
                params: vec![any_go_type()],
                ret_ty: Box::new(goty::GoType::TVoid),
            },
        }),
        args: vec![goast::Expr::String {
            value: msg.to_string(),
            ty: goty::GoType::TString,
        }],
        ty: goty::GoType::TVoid,
    })
}

fn is_while_loop(join: &anf::JoinBind) -> Option<WhileLoop> {
    if !join.params.is_empty() {
        return None;
    }
    let exit_id = find_while_exit_target(&join.body, &join.id)?;
    Some(WhileLoop {
        body: join.body.clone(),
        after: anf::Block {
            binds: Vec::new(),
            term: anf::Term::Jump {
                target: exit_id.clone(),
                args: Vec::new(),
                ret_ty: join.ret_ty.clone(),
            },
        },
        loop_id: join.id.clone(),
        exit_id,
    })
}

fn find_while_exit_target(block: &anf::Block, loop_id: &anf::JoinId) -> Option<anf::JoinId> {
    loop_exit_target(block, loop_id).or_else(|| {
        block.binds.iter().find_map(|bind| match bind {
            anf::Bind::Join(dispatch) if all_paths_reach_or_terminate(block, &dispatch.id) => {
                find_while_exit_target(&dispatch.body, loop_id)
            }
            _ => None,
        })
    })
}

struct WhileLoop {
    body: anf::Block,
    after: anf::Block,
    loop_id: anf::JoinId,
    exit_id: anf::JoinId,
}

fn loop_exit_target(block: &anf::Block, loop_id: &anf::JoinId) -> Option<anf::JoinId> {
    let joins = local_joins_in_block(block);
    match &block.term {
        anf::Term::If { then_, else_, .. } => {
            let then_reaches = any_path_reaches(then_, loop_id);
            let else_reaches = any_path_reaches(else_, loop_id);
            let break_block = if then_reaches && !else_reaches {
                else_
            } else if else_reaches && !then_reaches {
                then_
            } else {
                return None;
            };
            block_tail_target_through_joins(break_block, &joins, &mut HashSet::new())
        }
        anf::Term::Match { arms, default, .. } => {
            let mut saw_continue = false;
            let mut exit_target: Option<anf::JoinId> = None;
            for branch in arms
                .iter()
                .map(|arm| &arm.body)
                .chain(default.iter().map(|block| block.as_ref()))
            {
                if any_path_reaches(branch, loop_id) {
                    saw_continue = true;
                    continue;
                }
                let branch_target =
                    block_tail_target_through_joins(branch, &joins, &mut HashSet::new())?;
                if let Some(existing) = &exit_target {
                    if existing != &branch_target {
                        return None;
                    }
                } else {
                    exit_target = Some(branch_target);
                }
            }
            if saw_continue { exit_target } else { None }
        }
        _ => None,
    }
}

fn local_joins_in_block(block: &anf::Block) -> HashMap<&anf::JoinId, &anf::JoinBind> {
    let mut joins = HashMap::new();
    for bind in &block.binds {
        match bind {
            anf::Bind::Join(join_bind) => {
                joins.insert(&join_bind.id, join_bind);
            }
            anf::Bind::JoinRec(group) => {
                for join_bind in group {
                    joins.insert(&join_bind.id, join_bind);
                }
            }
            anf::Bind::Let(_) => {}
        }
    }
    joins
}

fn block_tail_target_through_joins(
    block: &anf::Block,
    parent_joins: &HashMap<&anf::JoinId, &anf::JoinBind>,
    visited: &mut HashSet<anf::JoinId>,
) -> Option<anf::JoinId> {
    let mut local_joins = parent_joins.clone();
    for bind in &block.binds {
        match bind {
            anf::Bind::Join(join_bind) => {
                local_joins.insert(&join_bind.id, join_bind);
            }
            anf::Bind::JoinRec(group) => {
                for join_bind in group {
                    local_joins.insert(&join_bind.id, join_bind);
                }
            }
            anf::Bind::Let(_) => {}
        }
    }
    term_tail_target_through_joins(&block.term, &local_joins, visited)
}

fn term_tail_target_through_joins(
    term: &anf::Term,
    joins: &HashMap<&anf::JoinId, &anf::JoinBind>,
    visited: &mut HashSet<anf::JoinId>,
) -> Option<anf::JoinId> {
    match term {
        anf::Term::Jump { target, args, .. } => {
            if args.is_empty() {
                return Some(target.clone());
            }
            if !visited.insert(target.clone()) {
                return None;
            }
            let result = joins.get(target).and_then(|join_bind| {
                block_tail_target_through_joins(&join_bind.body, joins, visited)
            });
            visited.remove(target);
            result
        }
        anf::Term::If { then_, else_, .. } => {
            let then_target = block_tail_target_through_joins(then_, joins, visited)?;
            let else_target = block_tail_target_through_joins(else_, joins, visited)?;
            if then_target == else_target {
                Some(then_target)
            } else {
                None
            }
        }
        anf::Term::Match { arms, default, .. } => {
            let mut target = None;
            for arm in arms {
                let branch_target = block_tail_target_through_joins(&arm.body, joins, visited)?;
                if let Some(existing) = &target {
                    if existing != &branch_target {
                        return None;
                    }
                } else {
                    target = Some(branch_target);
                }
            }
            if let Some(default) = default {
                let branch_target = block_tail_target_through_joins(default, joins, visited)?;
                if let Some(existing) = &target {
                    if existing != &branch_target {
                        return None;
                    }
                } else {
                    target = Some(branch_target);
                }
            }
            target
        }
        _ => None,
    }
}

fn any_path_reaches(block: &anf::Block, target: &anf::JoinId) -> bool {
    any_path_reaches_with_joins(block, target, &HashMap::new(), &mut HashSet::new())
}

fn any_path_reaches_with_joins(
    block: &anf::Block,
    target: &anf::JoinId,
    parent_joins: &HashMap<&anf::JoinId, &anf::JoinBind>,
    visited: &mut HashSet<anf::JoinId>,
) -> bool {
    let mut local_joins = parent_joins.clone();
    for bind in &block.binds {
        match bind {
            anf::Bind::Join(jb) => {
                local_joins.insert(&jb.id, jb);
            }
            anf::Bind::JoinRec(group) => {
                for jb in group {
                    local_joins.insert(&jb.id, jb);
                }
            }
            anf::Bind::Let(_) => {}
        }
    }
    any_path_reaches_term(&block.term, target, &local_joins, visited)
}

fn any_path_reaches_term(
    term: &anf::Term,
    target: &anf::JoinId,
    joins: &HashMap<&anf::JoinId, &anf::JoinBind>,
    visited: &mut HashSet<anf::JoinId>,
) -> bool {
    match term {
        anf::Term::Jump {
            target: t, args, ..
        } => {
            if t == target && args.is_empty() {
                return true;
            }
            if visited.contains(t) {
                return false;
            }
            if let Some(jb) = joins.get(t) {
                visited.insert(t.clone());
                any_path_reaches_with_joins(&jb.body, target, joins, visited)
            } else {
                false
            }
        }
        anf::Term::If { then_, else_, .. } => {
            any_path_reaches_with_joins(then_, target, joins, visited)
                || any_path_reaches_with_joins(else_, target, joins, visited)
        }
        anf::Term::Match { arms, default, .. } => {
            arms.iter()
                .any(|arm| any_path_reaches_with_joins(&arm.body, target, joins, visited))
                || default
                    .as_deref()
                    .is_some_and(|d| any_path_reaches_with_joins(d, target, joins, visited))
        }
        _ => false,
    }
}

fn all_paths_reach_or_terminate(block: &anf::Block, target: &anf::JoinId) -> bool {
    all_paths_reach_or_terminate_with_joins(block, target, &HashMap::new(), &mut HashSet::new())
}

fn all_paths_reach_or_terminate_with_joins(
    block: &anf::Block,
    target: &anf::JoinId,
    parent_joins: &HashMap<&anf::JoinId, &anf::JoinBind>,
    visited: &mut HashSet<anf::JoinId>,
) -> bool {
    let mut local_joins = parent_joins.clone();
    for bind in &block.binds {
        match bind {
            anf::Bind::Join(jb) => {
                local_joins.insert(&jb.id, jb);
            }
            anf::Bind::JoinRec(group) => {
                for jb in group {
                    local_joins.insert(&jb.id, jb);
                }
            }
            anf::Bind::Let(_) => {}
        }
    }
    all_paths_reach_or_terminate_term(&block.term, target, &local_joins, visited)
}

fn all_paths_reach_or_terminate_term(
    term: &anf::Term,
    target: &anf::JoinId,
    joins: &HashMap<&anf::JoinId, &anf::JoinBind>,
    visited: &mut HashSet<anf::JoinId>,
) -> bool {
    match term {
        anf::Term::Jump { target: t, .. } => {
            if t == target {
                return true;
            }
            if visited.contains(t) {
                return false;
            }
            if let Some(jb) = joins.get(t) {
                visited.insert(t.clone());
                let result =
                    all_paths_reach_or_terminate_with_joins(&jb.body, target, joins, visited);
                visited.remove(t);
                result
            } else {
                true
            }
        }
        anf::Term::If { then_, else_, .. } => {
            all_paths_reach_or_terminate_with_joins(then_, target, joins, visited)
                && all_paths_reach_or_terminate_with_joins(else_, target, joins, visited)
        }
        anf::Term::Match { arms, default, .. } => {
            arms.iter().all(|arm| {
                all_paths_reach_or_terminate_with_joins(&arm.body, target, joins, visited)
            }) && default
                .as_deref()
                .is_none_or(|d| all_paths_reach_or_terminate_with_joins(d, target, joins, visited))
        }
        anf::Term::Return(_) | anf::Term::Unreachable { .. } => true,
    }
}

fn compile_let_bind(goenv: &GlobalGoEnv, bind: &anf::LetBind) -> Vec<goast::Stmt> {
    let discard = bind.id.0 == "_" || bind.id.0.starts_with("_wild");

    if discard
        && let anf::ValueExpr::Call { func, args, .. } = &bind.value
        && let anf::ImmExpr::Var { id: func_id, .. } = func
        && runtime_builtin_available(goenv, &func_id.0)
        && func_id.0 == "vec_push"
        && let Some(anf::ImmExpr::Var { id: vec_id, .. }) = args.first()
    {
        let compiled = compile_value_expr(goenv, &bind.value);
        let mut stmts = compiled.stmts;
        stmts.push(goast::Stmt::Assignment {
            name: go_ident(&vec_id.0),
            value: compiled.expr,
        });
        return stmts;
    }

    let compiled = compile_value_expr(goenv, &bind.value);
    let mut stmts = compiled.stmts;
    if discard {
        if go_expr_can_be_statement(&compiled.expr) {
            stmts.push(goast::Stmt::Expr(compiled.expr));
        }
    } else {
        let expr_go_ty = compiled.expr.get_ty();
        let var_ty = if let goty::GoType::TName { name } = expr_go_ty
            && is_closure_env_struct(name)
        {
            expr_go_ty.clone()
        } else {
            tast_ty_to_go_type(&bind.ty)
        };
        stmts.push(goast::Stmt::VarDecl {
            name: go_ident(&bind.id.0),
            ty: var_ty,
            value: Some(compiled.expr),
        });
    }
    stmts
}

fn go_expr_can_be_statement(expr: &goast::Expr) -> bool {
    match expr {
        goast::Expr::Call { func, .. } => !go_call_result_must_be_used(func),
        _ => false,
    }
}

fn go_call_result_must_be_used(func: &goast::Expr) -> bool {
    let goast::Expr::Var { name, .. } = func else {
        return false;
    };
    matches!(
        name.as_str(),
        "append"
            | "cap"
            | "complex"
            | "imag"
            | "len"
            | "make"
            | "max"
            | "min"
            | "new"
            | "real"
            | "bool"
            | "byte"
            | "rune"
            | "string"
            | "int"
            | "int8"
            | "int16"
            | "int32"
            | "int64"
            | "uint"
            | "uint8"
            | "uint16"
            | "uint32"
            | "uint64"
            | "uintptr"
            | "float32"
            | "float64"
            | "complex64"
            | "complex128"
    )
}

fn needs_closure_to_func_wrap(arg_ty: &tast::Ty, param_ty: &tast::Ty) -> bool {
    if let tast::Ty::TStruct { name } = arg_ty
        && is_closure_env_struct(name)
        && matches!(param_ty, tast::Ty::TFunc { .. })
    {
        return true;
    }
    false
}

fn ensure_typed_for_any(expr: goast::Expr, for_ty: &tast::Ty) -> goast::Expr {
    let type_name = match for_ty {
        tast::Ty::TInt8 => Some("int8"),
        tast::Ty::TInt16 => Some("int16"),
        tast::Ty::TInt32 => Some("int32"),
        tast::Ty::TInt64 => Some("int64"),
        tast::Ty::TUint8 => Some("uint8"),
        tast::Ty::TUint16 => Some("uint16"),
        tast::Ty::TUint32 => Some("uint32"),
        tast::Ty::TUint64 => Some("uint64"),
        tast::Ty::TFloat32 => Some("float32"),
        tast::Ty::TFloat64 => Some("float64"),
        tast::Ty::TChar => Some("rune"),
        tast::Ty::TBool => Some("bool"),
        _ => None,
    };
    if let Some(name) = type_name {
        let go_ty = tast_ty_to_go_type(for_ty);
        goast::Expr::Call {
            func: Box::new(goast::Expr::Var {
                name: name.to_string(),
                ty: go_ty.clone(),
            }),
            args: vec![expr],
            ty: go_ty,
        }
    } else {
        expr
    }
}

fn closure_to_func_lit(
    goenv: &GlobalGoEnv,
    closure_imm: &anf::ImmExpr,
    func_ty: &tast::Ty,
) -> goast::Expr {
    let closure_ty = imm_ty(closure_imm);
    let apply = find_closure_apply_fn(goenv, &closure_ty)
        .expect("closure struct must have an apply method");

    let tast::Ty::TFunc {
        params: func_params,
        ret_ty: func_ret,
    } = func_ty
    else {
        unreachable!("closure_to_func_lit called with non-function target type");
    };

    let param_names: Vec<String> = func_params
        .iter()
        .enumerate()
        .map(|(i, _)| format!("p{}", i))
        .collect();

    let go_params: Vec<(String, goty::GoType)> = param_names
        .iter()
        .zip(func_params.iter())
        .map(|(name, ty)| (name.clone(), tast_ty_to_go_type(ty)))
        .collect();

    let mut call_args: Vec<goast::Expr> = vec![compile_imm(goenv, closure_imm)];
    for (name, ty) in &go_params {
        call_args.push(goast::Expr::Var {
            name: name.clone(),
            ty: ty.clone(),
        });
    }

    let apply_expr = goast::Expr::Var {
        name: go_ident(&apply.name),
        ty: tast_ty_to_go_type(&apply.ty),
    };

    let go_ret_ty = tast_ty_to_go_type(func_ret);
    let call_expr = goast::Expr::Call {
        func: Box::new(apply_expr),
        args: call_args,
        ty: go_ret_ty.clone(),
    };

    let body = vec![goast::Stmt::Return {
        expr: Some(call_expr),
    }];

    let func_go_ty = tast_ty_to_go_type(func_ty);

    goast::Expr::FuncLit {
        params: go_params,
        body,
        ty: func_go_ty,
    }
}

fn compile_jump_args(
    goenv: &GlobalGoEnv,
    params: &[(anf::LocalId, tast::Ty)],
    args: &[anf::ImmExpr],
) -> Vec<goast::Stmt> {
    let mut stmts = Vec::new();
    for ((param_id, param_ty), arg) in params.iter().zip(args.iter()) {
        if param_id.0 == "_" {
            continue;
        }
        let arg_ty = imm_ty(arg);
        let value = if needs_closure_to_func_wrap(&arg_ty, param_ty) {
            closure_to_func_lit(goenv, arg, param_ty)
        } else {
            compile_imm(goenv, arg)
        };
        stmts.push(goast::Stmt::Assignment {
            name: go_ident(&param_id.0),
            value,
        });
    }
    stmts
}

fn all_branches_jump_to_resolved(term: &anf::Term) -> Option<anf::JoinId> {
    match term {
        anf::Term::Jump { target, .. } => Some(target.clone()),
        anf::Term::If { then_, else_, .. } => {
            let then_target = block_tail_target_resolved(then_)?;
            let else_target = block_tail_target_resolved(else_)?;
            if then_target == else_target {
                Some(then_target)
            } else {
                None
            }
        }
        anf::Term::Match { arms, default, .. } => {
            let mut target = None;
            for arm in arms {
                let branch_target = block_tail_target_resolved(&arm.body)?;
                if let Some(existing) = &target {
                    if existing != &branch_target {
                        return None;
                    }
                } else {
                    target = Some(branch_target);
                }
            }
            if let Some(default) = default {
                let branch_target = block_tail_target_resolved(default)?;
                if let Some(existing) = &target {
                    if existing != &branch_target {
                        return None;
                    }
                } else {
                    target = Some(branch_target);
                }
            }
            target
        }
        _ => None,
    }
}

fn block_tail_target_resolved(block: &anf::Block) -> Option<anf::JoinId> {
    let mut joins = HashMap::new();
    for bind in &block.binds {
        if let anf::Bind::Join(join_bind) = bind {
            joins.insert(&join_bind.id, join_bind);
        }
    }
    term_tail_target_resolved(&block.term, &joins, &mut HashSet::new())
}

fn term_tail_target_resolved(
    term: &anf::Term,
    joins: &HashMap<&anf::JoinId, &anf::JoinBind>,
    visited: &mut HashSet<anf::JoinId>,
) -> Option<anf::JoinId> {
    match term {
        anf::Term::Jump { target, .. } => {
            if !visited.insert(target.clone()) {
                return None;
            }
            let result = if let Some(join_bind) = joins.get(target) {
                block_tail_target_resolved_with_joins(&join_bind.body, joins, visited)
            } else {
                Some(target.clone())
            };
            visited.remove(target);
            result
        }
        anf::Term::If { then_, else_, .. } => {
            let then_target = block_tail_target_resolved_with_joins(then_, joins, visited)?;
            let else_target = block_tail_target_resolved_with_joins(else_, joins, visited)?;
            if then_target == else_target {
                Some(then_target)
            } else {
                None
            }
        }
        anf::Term::Match { arms, default, .. } => {
            let mut target = None;
            for arm in arms {
                let branch_target =
                    block_tail_target_resolved_with_joins(&arm.body, joins, visited)?;
                if let Some(existing) = &target {
                    if existing != &branch_target {
                        return None;
                    }
                } else {
                    target = Some(branch_target);
                }
            }
            if let Some(default) = default {
                let branch_target = block_tail_target_resolved_with_joins(default, joins, visited)?;
                if let Some(existing) = &target {
                    if existing != &branch_target {
                        return None;
                    }
                } else {
                    target = Some(branch_target);
                }
            }
            target
        }
        _ => None,
    }
}

fn block_tail_target_resolved_with_joins(
    block: &anf::Block,
    parent_joins: &HashMap<&anf::JoinId, &anf::JoinBind>,
    visited: &mut HashSet<anf::JoinId>,
) -> Option<anf::JoinId> {
    let mut joins = parent_joins.clone();
    for bind in &block.binds {
        if let anf::Bind::Join(join_bind) = bind {
            joins.insert(&join_bind.id, join_bind);
        }
    }
    term_tail_target_resolved(&block.term, &joins, visited)
}

fn compile_block_structured(
    goenv: &GlobalGoEnv,
    block: &anf::Block,
    join_env: &JoinEnv,
) -> Vec<goast::Stmt> {
    let mut pending_joins: Vec<&anf::JoinBind> = Vec::new();
    let mut while_loop_ids: Vec<anf::JoinId> = Vec::new();
    let mut stmts = Vec::new();
    for bind in &block.binds {
        match bind {
            anf::Bind::Let(let_bind) => {
                stmts.extend(compile_let_bind(goenv, let_bind));
            }
            anf::Bind::Join(join_bind) => {
                for (id, ty) in &join_bind.params {
                    if id.0 != "_" {
                        stmts.push(goast::Stmt::VarDecl {
                            name: go_ident(&id.0),
                            ty: tast_ty_to_go_type(ty),
                            value: None,
                        });
                    }
                }
                pending_joins.push(join_bind);
            }
            anf::Bind::JoinRec(group) => {
                for j in group {
                    if is_while_loop(j).is_some() {
                        while_loop_ids.push(j.id.clone());
                    }
                }
                stmts.extend(compile_joinrec(goenv, group, join_env));
            }
        }
    }

    if let anf::Term::Jump { target, args, .. } = &block.term
        && args.is_empty()
        && while_loop_ids.contains(target)
    {
        return stmts;
    }

    stmts.extend(compile_term_with_continuations_ctx(
        goenv,
        &block.term,
        join_env,
        &pending_joins,
    ));
    stmts
}

fn compile_term_with_continuations_ctx(
    goenv: &GlobalGoEnv,
    term: &anf::Term,
    join_env: &JoinEnv,
    pending_joins: &[&anf::JoinBind],
) -> Vec<goast::Stmt> {
    if let Some(target) = all_branches_jump_to_resolved(term)
        && let Some(join_bind) = join_env.get(&target)
        && pending_joins.iter().any(|j| j.id == target)
    {
        let mut stmts = compile_term_jump_to_ctx(goenv, term, &target, join_bind, join_env);
        stmts.extend(compile_block_structured(goenv, &join_bind.body, join_env));
        return stmts;
    }
    compile_term_leaf_ctx(goenv, term, join_env)
}

fn compile_term_jump_to_ctx(
    goenv: &GlobalGoEnv,
    term: &anf::Term,
    target: &anf::JoinId,
    join_bind: &anf::JoinBind,
    join_env: &JoinEnv,
) -> Vec<goast::Stmt> {
    match term {
        anf::Term::Jump { args, .. } => compile_jump_args(goenv, &join_bind.params, args),
        anf::Term::If {
            cond, then_, else_, ..
        } => {
            let then_stmts = compile_branch_to_join_ctx(goenv, then_, target, join_bind, join_env);
            let else_stmts = compile_branch_to_join_ctx(goenv, else_, target, join_bind, join_env);
            vec![goast::Stmt::If {
                cond: compile_imm(goenv, cond),
                then: goast::Block { stmts: then_stmts },
                else_: Some(goast::Block { stmts: else_stmts }),
            }]
        }
        anf::Term::Match {
            scrut,
            arms,
            default,
            ..
        } => compile_match_to_join_ctx(
            goenv,
            scrut,
            arms,
            default.as_deref(),
            target,
            join_bind,
            join_env,
        ),
        _ => compile_term_leaf_ctx(goenv, term, join_env),
    }
}

fn compile_branch_to_join_ctx(
    goenv: &GlobalGoEnv,
    block: &anf::Block,
    target: &anf::JoinId,
    join_bind: &anf::JoinBind,
    join_env: &JoinEnv,
) -> Vec<goast::Stmt> {
    let mut inner_pending: Vec<&anf::JoinBind> = Vec::new();
    let mut stmts = Vec::new();
    for bind in &block.binds {
        match bind {
            anf::Bind::Let(let_bind) => {
                stmts.extend(compile_let_bind(goenv, let_bind));
            }
            anf::Bind::Join(jb) => {
                for (id, ty) in &jb.params {
                    if id.0 != "_" {
                        stmts.push(goast::Stmt::VarDecl {
                            name: go_ident(&id.0),
                            ty: tast_ty_to_go_type(ty),
                            value: None,
                        });
                    }
                }
                inner_pending.push(jb);
            }
            anf::Bind::JoinRec(group) => {
                stmts.extend(compile_joinrec(goenv, group, join_env));
            }
        }
    }

    if let Some(inner_target) = all_branches_jump_to_resolved(&block.term) {
        if inner_target == *target {
            stmts.extend(compile_term_jump_to_ctx(
                goenv,
                &block.term,
                target,
                join_bind,
                join_env,
            ));
            return stmts;
        }
        if let Some(inner_join) = join_env.get(&inner_target)
            && inner_pending.iter().any(|j| j.id == inner_target)
        {
            stmts.extend(compile_term_jump_to_ctx(
                goenv,
                &block.term,
                &inner_target,
                inner_join,
                join_env,
            ));
            stmts.extend(compile_branch_to_join_ctx(
                goenv,
                &inner_join.body,
                target,
                join_bind,
                join_env,
            ));
            return stmts;
        }
    }
    stmts.extend(compile_term_with_continuations_ctx(
        goenv,
        &block.term,
        join_env,
        &inner_pending,
    ));
    stmts
}

#[allow(clippy::too_many_arguments)]
fn compile_match_to_join_ctx(
    goenv: &GlobalGoEnv,
    scrut: &anf::ImmExpr,
    arms: &[anf::Arm],
    default: Option<&anf::Block>,
    target: &anf::JoinId,
    join_bind: &anf::JoinBind,
    join_env: &JoinEnv,
) -> Vec<goast::Stmt> {
    let scrut_ty = imm_ty(scrut);

    let mut var_arm: Option<(&anf::LocalId, &anf::Block)> = None;
    let mut literal_arms = Vec::new();
    for arm in arms {
        match &arm.lhs {
            anf::ImmExpr::Var { id, .. } => {
                var_arm = Some((id, &arm.body));
            }
            _ => literal_arms.push((&arm.lhs, &arm.body)),
        }
    }

    if scrut_ty == tast::Ty::TUnit {
        let block = literal_arms
            .first()
            .map(|(_, b)| *b)
            .or(var_arm.map(|(_, b)| b))
            .or(default);
        if let Some(block) = block {
            return compile_branch_to_join_ctx(goenv, block, target, join_bind, join_env);
        }
        return vec![panic_stmt("non-exhaustive match")];
    }

    match &scrut_ty {
        tast::Ty::TEnum { .. } => compile_enum_match_to_join_ctx(
            goenv,
            scrut,
            &literal_arms,
            var_arm,
            default,
            target,
            join_bind,
            join_env,
        ),
        tast::Ty::TApp { ty, .. } if matches!(ty.as_ref(), tast::Ty::TEnum { .. }) => {
            compile_enum_match_to_join_ctx(
                goenv,
                scrut,
                &literal_arms,
                var_arm,
                default,
                target,
                join_bind,
                join_env,
            )
        }
        _ => compile_switch_match_to_join_ctx(
            goenv,
            scrut,
            &literal_arms,
            var_arm,
            default,
            target,
            join_bind,
            join_env,
        ),
    }
}

#[allow(clippy::too_many_arguments)]
fn compile_enum_match_to_join_ctx(
    goenv: &GlobalGoEnv,
    scrut: &anf::ImmExpr,
    arms: &[(&anf::ImmExpr, &anf::Block)],
    var_default: Option<(&anf::LocalId, &anf::Block)>,
    default: Option<&anf::Block>,
    target: &anf::JoinId,
    join_bind: &anf::JoinBind,
    join_env: &JoinEnv,
) -> Vec<goast::Stmt> {
    if enum_is_tag_only(goenv, &imm_ty(scrut)) {
        return compile_switch_match_to_join_ctx(
            goenv,
            scrut,
            arms,
            var_default,
            default,
            target,
            join_bind,
            join_env,
        );
    }
    let mut cases = Vec::new();
    for (pat, body) in arms {
        let anf::ImmExpr::Tag { index, ty } = pat else {
            panic!("expected tag pattern in enum match, got {:?}", pat);
        };
        let vty = variant_ty_by_index(goenv, ty, *index);
        let stmts = compile_branch_to_join_ctx(goenv, body, target, join_bind, join_env);
        cases.push((vty, goast::Block { stmts }));
    }

    let default_block = if let Some((id, body)) = var_default {
        let mut stmts = Vec::new();
        if id.0 != "_" {
            stmts.push(goast::Stmt::VarDecl {
                name: go_ident(&id.0),
                ty: tast_ty_to_go_type(&imm_ty(scrut)),
                value: Some(compile_imm(goenv, scrut)),
            });
        }
        stmts.extend(compile_branch_to_join_ctx(
            goenv, body, target, join_bind, join_env,
        ));
        Some(goast::Block { stmts })
    } else if let Some(def_block) = default {
        Some(goast::Block {
            stmts: compile_branch_to_join_ctx(goenv, def_block, target, join_bind, join_env),
        })
    } else {
        Some(goast::Block {
            stmts: vec![panic_stmt("non-exhaustive match")],
        })
    };

    vec![goast::Stmt::SwitchType {
        bind: None,
        expr: compile_imm(goenv, scrut),
        cases,
        default: default_block,
    }]
}

#[allow(clippy::too_many_arguments)]
fn compile_switch_match_to_join_ctx(
    goenv: &GlobalGoEnv,
    scrut: &anf::ImmExpr,
    arms: &[(&anf::ImmExpr, &anf::Block)],
    var_default: Option<(&anf::LocalId, &anf::Block)>,
    default: Option<&anf::Block>,
    target: &anf::JoinId,
    join_bind: &anf::JoinBind,
    join_env: &JoinEnv,
) -> Vec<goast::Stmt> {
    let mut cases = Vec::new();
    for (pat, body) in arms {
        let stmts = compile_branch_to_join_ctx(goenv, body, target, join_bind, join_env);
        cases.push((compile_imm(goenv, pat), goast::Block { stmts }));
    }

    let default_block = if let Some((id, body)) = var_default {
        let mut stmts = Vec::new();
        if id.0 != "_" {
            stmts.push(goast::Stmt::VarDecl {
                name: go_ident(&id.0),
                ty: tast_ty_to_go_type(&imm_ty(scrut)),
                value: Some(compile_imm(goenv, scrut)),
            });
        }
        stmts.extend(compile_branch_to_join_ctx(
            goenv, body, target, join_bind, join_env,
        ));
        Some(goast::Block { stmts })
    } else if let Some(def_block) = default {
        Some(goast::Block {
            stmts: compile_branch_to_join_ctx(goenv, def_block, target, join_bind, join_env),
        })
    } else {
        Some(goast::Block {
            stmts: vec![panic_stmt("non-exhaustive match")],
        })
    };

    vec![goast::Stmt::SwitchExpr {
        expr: compile_imm(goenv, scrut),
        cases,
        default: default_block,
    }]
}

fn compile_term_leaf_ctx(
    goenv: &GlobalGoEnv,
    term: &anf::Term,
    join_env: &JoinEnv,
) -> Vec<goast::Stmt> {
    match term {
        anf::Term::Return(imm) => vec![goast::Stmt::Return {
            expr: Some(compile_imm(goenv, imm)),
        }],
        anf::Term::Jump { target, args, .. } => {
            if join_env.continue_targets.contains(target) {
                return vec![goast::Stmt::Continue];
            }
            if join_env.break_targets.contains(target) {
                if let Some(label) = join_env.break_labels.get(target) {
                    return vec![goast::Stmt::BreakLabel(label.clone())];
                }
                return vec![goast::Stmt::Break];
            }
            if let Some(join_bind) = join_env.get(target) {
                let mut stmts = compile_jump_args(goenv, &join_bind.params, args);
                stmts.extend(compile_block_structured(goenv, &join_bind.body, join_env));
                stmts
            } else {
                vec![panic_stmt(&format!("unknown join {:?}", target))]
            }
        }
        anf::Term::If {
            cond, then_, else_, ..
        } => {
            let then_stmts = compile_block_structured(goenv, then_, join_env);
            let else_stmts = compile_block_structured(goenv, else_, join_env);
            vec![goast::Stmt::If {
                cond: compile_imm(goenv, cond),
                then: goast::Block { stmts: then_stmts },
                else_: Some(goast::Block { stmts: else_stmts }),
            }]
        }
        anf::Term::Match {
            scrut,
            arms,
            default,
            ..
        } => compile_match_leaf_ctx(goenv, scrut, arms, default.as_deref(), join_env),
        anf::Term::Unreachable { .. } => vec![panic_stmt("unreachable")],
    }
}

fn compile_match_leaf_ctx(
    goenv: &GlobalGoEnv,
    scrut: &anf::ImmExpr,
    arms: &[anf::Arm],
    default: Option<&anf::Block>,
    join_env: &JoinEnv,
) -> Vec<goast::Stmt> {
    let scrut_ty = imm_ty(scrut);

    let mut var_arm: Option<(&anf::LocalId, &anf::Block)> = None;
    let mut literal_arms = Vec::new();
    for arm in arms {
        match &arm.lhs {
            anf::ImmExpr::Var { id, .. } => {
                var_arm = Some((id, &arm.body));
            }
            _ => literal_arms.push((&arm.lhs, &arm.body)),
        }
    }

    if scrut_ty == tast::Ty::TUnit {
        let block = literal_arms
            .first()
            .map(|(_, b)| *b)
            .or(var_arm.map(|(_, b)| b))
            .or(default);
        if let Some(block) = block {
            return compile_block_structured(goenv, block, join_env);
        }
        return vec![panic_stmt("non-exhaustive match")];
    }

    match &scrut_ty {
        tast::Ty::TEnum { .. } => {
            compile_enum_match_leaf_ctx(goenv, scrut, &literal_arms, var_arm, default, join_env)
        }
        tast::Ty::TApp { ty, .. } if matches!(ty.as_ref(), tast::Ty::TEnum { .. }) => {
            compile_enum_match_leaf_ctx(goenv, scrut, &literal_arms, var_arm, default, join_env)
        }
        _ => compile_switch_match_leaf_ctx(goenv, scrut, &literal_arms, var_arm, default, join_env),
    }
}

#[allow(clippy::too_many_arguments)]
fn compile_enum_match_leaf_ctx(
    goenv: &GlobalGoEnv,
    scrut: &anf::ImmExpr,
    arms: &[(&anf::ImmExpr, &anf::Block)],
    var_default: Option<(&anf::LocalId, &anf::Block)>,
    default: Option<&anf::Block>,
    join_env: &JoinEnv,
) -> Vec<goast::Stmt> {
    if enum_is_tag_only(goenv, &imm_ty(scrut)) {
        return compile_switch_match_leaf_ctx(goenv, scrut, arms, var_default, default, join_env);
    }
    let mut cases = Vec::new();
    for (pat, body) in arms {
        let anf::ImmExpr::Tag { index, ty } = pat else {
            panic!("expected tag pattern in enum match, got {:?}", pat);
        };
        let vty = variant_ty_by_index(goenv, ty, *index);
        let stmts = compile_block_structured(goenv, body, join_env);
        cases.push((vty, goast::Block { stmts }));
    }

    let default_block = if let Some((id, body)) = var_default {
        let mut stmts = Vec::new();
        if id.0 != "_" {
            stmts.push(goast::Stmt::VarDecl {
                name: go_ident(&id.0),
                ty: tast_ty_to_go_type(&imm_ty(scrut)),
                value: Some(compile_imm(goenv, scrut)),
            });
        }
        stmts.extend(compile_block_structured(goenv, body, join_env));
        Some(goast::Block { stmts })
    } else if let Some(def_block) = default {
        Some(goast::Block {
            stmts: compile_block_structured(goenv, def_block, join_env),
        })
    } else {
        Some(goast::Block {
            stmts: vec![panic_stmt("non-exhaustive match")],
        })
    };

    vec![goast::Stmt::SwitchType {
        bind: None,
        expr: compile_imm(goenv, scrut),
        cases,
        default: default_block,
    }]
}

#[allow(clippy::too_many_arguments)]
fn compile_switch_match_leaf_ctx(
    goenv: &GlobalGoEnv,
    scrut: &anf::ImmExpr,
    arms: &[(&anf::ImmExpr, &anf::Block)],
    var_default: Option<(&anf::LocalId, &anf::Block)>,
    default: Option<&anf::Block>,
    join_env: &JoinEnv,
) -> Vec<goast::Stmt> {
    let mut cases = Vec::new();
    for (pat, body) in arms {
        let stmts = compile_block_structured(goenv, body, join_env);
        cases.push((compile_imm(goenv, pat), goast::Block { stmts }));
    }

    let default_block = if let Some((id, body)) = var_default {
        let mut stmts = Vec::new();
        if id.0 != "_" {
            stmts.push(goast::Stmt::VarDecl {
                name: go_ident(&id.0),
                ty: tast_ty_to_go_type(&imm_ty(scrut)),
                value: Some(compile_imm(goenv, scrut)),
            });
        }
        stmts.extend(compile_block_structured(goenv, body, join_env));
        Some(goast::Block { stmts })
    } else if let Some(def_block) = default {
        Some(goast::Block {
            stmts: compile_block_structured(goenv, def_block, join_env),
        })
    } else {
        Some(goast::Block {
            stmts: vec![panic_stmt("non-exhaustive match")],
        })
    };

    vec![goast::Stmt::SwitchExpr {
        expr: compile_imm(goenv, scrut),
        cases,
        default: default_block,
    }]
}

fn compile_joinrec(
    goenv: &GlobalGoEnv,
    group: &[anf::JoinBind],
    join_env: &JoinEnv,
) -> Vec<goast::Stmt> {
    if group.len() == 1
        && let Some(wl) = is_while_loop(&group[0])
    {
        return compile_while_loop(goenv, &wl, join_env);
    }
    Vec::new()
}

fn compile_while_loop(goenv: &GlobalGoEnv, wl: &WhileLoop, join_env: &JoinEnv) -> Vec<goast::Stmt> {
    let loop_label = format!("Loop_{}", go_ident(&wl.loop_id.0));

    let mut inner_env = join_env.clone();
    inner_env.continue_targets.insert(wl.loop_id.clone());
    inner_env.break_targets.insert(wl.exit_id.clone());
    inner_env
        .break_labels
        .insert(wl.exit_id.clone(), loop_label.clone());
    let loop_body_stmts = compile_block_structured(goenv, &wl.body, &inner_env);

    let has_break_label = stmts_contain_break_label(&loop_body_stmts, &loop_label);
    let mut stmts = vec![goast::Stmt::Loop {
        body: goast::Block {
            stmts: loop_body_stmts,
        },
        label: if has_break_label {
            Some(loop_label)
        } else {
            None
        },
    }];

    stmts.extend(compile_block_structured(goenv, &wl.after, join_env));
    stmts
}

fn stmts_contain_break_label(stmts: &[goast::Stmt], label: &str) -> bool {
    stmts.iter().any(|s| stmt_contains_break_label(s, label))
}

fn stmt_contains_break_label(stmt: &goast::Stmt, label: &str) -> bool {
    match stmt {
        goast::Stmt::BreakLabel(lbl) => lbl == label,
        goast::Stmt::If { then, else_, .. } => {
            stmts_contain_break_label(&then.stmts, label)
                || else_
                    .as_ref()
                    .is_some_and(|b| stmts_contain_break_label(&b.stmts, label))
        }
        goast::Stmt::SwitchExpr { cases, default, .. } => {
            cases
                .iter()
                .any(|(_, b)| stmts_contain_break_label(&b.stmts, label))
                || default
                    .as_ref()
                    .is_some_and(|b| stmts_contain_break_label(&b.stmts, label))
        }
        goast::Stmt::SwitchType { cases, default, .. } => {
            cases
                .iter()
                .any(|(_, b)| stmts_contain_break_label(&b.stmts, label))
                || default
                    .as_ref()
                    .is_some_and(|b| stmts_contain_break_label(&b.stmts, label))
        }
        goast::Stmt::Loop { body, .. } => stmts_contain_break_label(&body.stmts, label),
        _ => false,
    }
}

fn build_join_env(block: &anf::Block) -> JoinEnv {
    let mut env = JoinEnv::default();
    collect_joins_from_block(block, &mut env);
    env
}

#[derive(Default, Clone)]
struct JoinEnv {
    joins: HashMap<anf::JoinId, anf::JoinBind>,
    continue_targets: HashSet<anf::JoinId>,
    break_targets: HashSet<anf::JoinId>,
    break_labels: HashMap<anf::JoinId, String>,
}

impl JoinEnv {
    fn get(&self, id: &anf::JoinId) -> Option<&anf::JoinBind> {
        self.joins.get(id)
    }
}

fn collect_joins_from_block(block: &anf::Block, env: &mut JoinEnv) {
    for bind in &block.binds {
        match bind {
            anf::Bind::Join(join_bind) => {
                env.joins.insert(join_bind.id.clone(), join_bind.clone());
                collect_joins_from_block(&join_bind.body, env);
            }
            anf::Bind::JoinRec(group) => {
                for join_bind in group {
                    if is_while_loop(join_bind).is_none() {
                        env.joins.insert(join_bind.id.clone(), join_bind.clone());
                    }
                    collect_joins_from_block(&join_bind.body, env);
                }
            }
            anf::Bind::Let(_) => {}
        }
    }
    match &block.term {
        anf::Term::If { then_, else_, .. } => {
            collect_joins_from_block(then_, env);
            collect_joins_from_block(else_, env);
        }
        anf::Term::Match { arms, default, .. } => {
            for arm in arms {
                collect_joins_from_block(&arm.body, env);
            }
            if let Some(def) = default {
                collect_joins_from_block(def, env);
            }
        }
        _ => {}
    }
}

fn entry_wrapper_function_name(goenv: &GlobalGoEnv, file: &anf::File) -> String {
    let mut used_names: HashSet<String> = file
        .toplevels
        .iter()
        .filter(|f| f.name != ENTRY_FUNCTION)
        .map(|f| go_ident(&f.name))
        .collect();
    used_names.extend(
        goenv
            .structs()
            .map(|(struct_name, _)| go_user_type_name(&struct_name.0)),
    );
    used_names.extend(
        goenv
            .enums()
            .map(|(enum_name, _)| go_user_type_name(&enum_name.0)),
    );
    for (enum_name, enum_def) in goenv.enums() {
        for (variant_name, _) in enum_def.variants.iter() {
            used_names.insert(variant_symbol_name(goenv, &enum_name.0, &variant_name.0));
        }
    }

    if !used_names.contains(ENTRY_WRAPPER_FUNCTION) {
        return ENTRY_WRAPPER_FUNCTION.to_string();
    }

    let mut index = 1usize;
    loop {
        let candidate = format!("{}__{}", ENTRY_WRAPPER_FUNCTION, index);
        if !used_names.contains(&candidate) {
            return candidate;
        }
        index += 1;
    }
}

fn patched_fn_name(goenv: &GlobalGoEnv, name: &str, entry_wrapper_name: &str) -> String {
    if name == ENTRY_FUNCTION {
        entry_wrapper_name.to_string()
    } else {
        go_toplevel_func_name(goenv, name)
    }
}

fn boxed_fn_params(params: &[(anf::LocalId, tast::Ty)]) -> Vec<(String, goty::GoType)> {
    params
        .iter()
        .map(|(id, ty)| (go_ident(&id.0), tast_ty_to_go_type(ty)))
        .collect()
}

fn compile_fn(
    goenv: &GlobalGoEnv,
    _gensym: &Gensym,
    f: &anf::Fn,
    entry_wrapper_name: &str,
) -> goast::Fn {
    let params = boxed_fn_params(&f.params);
    let go_ret_ty = tast_ty_to_go_type(&f.ret_ty);
    let ret_ty = match go_ret_ty {
        goty::GoType::TVoid => None,
        _ => Some(go_ret_ty.clone()),
    };
    let join_env = build_join_env(&f.body);
    let stmts = compile_block_structured(goenv, &f.body, &join_env);

    goast::Fn {
        name: patched_fn_name(goenv, &f.name, entry_wrapper_name),
        params,
        ret_ty,
        body: goast::Block { stmts },
    }
}

pub fn go_file(
    anfenv: GlobalAnfEnv,
    gensym: &Gensym,
    file: anf::File,
) -> (goast::File, GlobalGoEnv) {
    let mut goenv = GlobalGoEnv::from_anf_env(anfenv);
    goenv.toplevel_funcs = file.toplevels.iter().map(|f| f.name.clone()).collect();
    goenv.colliding_callable_idents = collect_colliding_callable_idents(&goenv);
    goenv.callable_go_names = collect_callable_go_names(&goenv);
    goenv.callable_go_name_set = goenv.callable_go_names.values().cloned().collect();
    let mut all = Vec::new();

    let (tuple_types, array_types, vec_types, ref_types, hashmap_types, missing_types) =
        collect_runtime_types(&goenv, &file);

    all.extend(runtime::make_runtime());
    all.extend(runtime::make_array_runtime(&array_types));
    all.extend(runtime::make_vec_runtime(&vec_types));
    all.extend(runtime::make_ref_runtime(&ref_types));
    all.extend(runtime::make_hashmap_runtime(&goenv, &hashmap_types));
    all.extend(runtime::make_missing_runtime(&missing_types));

    for ty in tuple_types.iter() {
        match tuple_to_go_struct_type(ty) {
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
    let entry_wrapper_name = entry_wrapper_function_name(&goenv, &file);
    let dyn_req = collect_dyn_requirements(&goenv, &file);
    let (dyn_vtable_ctor_go_names, dyn_wrap_go_names) =
        collect_dyn_helper_go_names(&goenv, &dyn_req);
    goenv.dyn_vtable_ctor_go_names = dyn_vtable_ctor_go_names;
    goenv.dyn_wrap_go_names = dyn_wrap_go_names;

    let mut toplevels = gen_type_definition(&goenv);
    toplevels.extend(gen_dyn_type_definitions(&goenv, &dyn_req));
    let impl_name_map = build_trait_impl_name_map(&goenv, &file.toplevels);
    toplevels.extend(gen_dyn_helper_fns(&goenv, &dyn_req, &impl_name_map));
    for item in file.toplevels {
        toplevels.push(goast::Item::Fn(compile_fn(
            &goenv,
            gensym,
            &item,
            &entry_wrapper_name,
        )));
    }
    all.extend(toplevels);
    all.push(goast::Item::Fn(goast::Fn {
        name: ENTRY_FUNCTION.to_string(),
        params: vec![],
        ret_ty: None,
        body: goast::Block {
            stmts: vec![goast::Stmt::Expr(goast::Expr::Call {
                func: Box::new(goast::Expr::Var {
                    name: entry_wrapper_name,
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
    (crate::go::dce::eliminate_dead_vars(file), goenv)
}

fn gen_type_definition(goenv: &GlobalGoEnv) -> Vec<goast::Item> {
    let mut defs = Vec::new();
    for (name, def) in goenv.structs() {
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
                name: go_ident(&fname.0),
                ty: tast_ty_to_go_type(fty),
            })
            .collect();
        defs.push(goast::Item::Struct(goast::Struct {
            name: go_user_type_name(&name.0),
            fields,
            methods: vec![],
        }));
    }

    for (name, def) in goenv.enums() {
        // Skip generating Go types for generic-specialized enums whose fields still contain type parameters
        let has_type_param = name.0.contains("TParam")
            || def
                .variants
                .iter()
                .any(|(_, fields)| fields.iter().any(|f| matches!(f, tast::Ty::TParam { .. })));
        if has_type_param {
            continue;
        }

        if is_tag_only_enum_def(def) {
            let enum_go_name = go_user_type_name(&name.0);
            defs.push(goast::Item::TypeDef(goast::TypeDef {
                name: enum_go_name.clone(),
                ty: goty::GoType::TInt32,
            }));
            defs.push(goast::Item::ConstGroup(goast::ConstGroup {
                specs: def
                    .variants
                    .iter()
                    .enumerate()
                    .map(|(index, (variant_name, _))| goast::ConstSpec {
                        name: variant_symbol_name(goenv, &name.0, &variant_name.0),
                        ty: goty::GoType::TName {
                            name: enum_go_name.clone(),
                        },
                        value: goast::Expr::Int {
                            value: index.to_string(),
                            ty: goty::GoType::TName {
                                name: enum_go_name.clone(),
                            },
                        },
                    })
                    .collect(),
            }));
            continue;
        }

        let type_identifier_method = format!("is{}", go_user_type_name(&name.0));

        defs.push(goast::Item::Interface(goast::Interface {
            name: go_user_type_name(&name.0),
            methods: vec![goast::MethodElem {
                name: type_identifier_method.clone(),
                params: vec![],
                ret: None,
            }],
        }));
        for (variant_name, variant_fields) in def.variants.iter() {
            let variant_name = variant_symbol_name(goenv, &name.0, &variant_name.0);
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
