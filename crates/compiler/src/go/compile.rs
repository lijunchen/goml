use crate::{
    anf::{self, GlobalAnfEnv},
    common::{Constructor, Prim},
    env::{
        EnumDef, ExternBindingMode, ExternFunc, ExternReturnMode, FnOrigin, Gensym, GlobalTypeEnv,
        InherentImplKey, StructDef,
    },
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
    if goenv.genv.value_env.extern_funcs.contains_key(name) || goenv.toplevel_funcs.contains(name) {
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
            | "bool_to_json"
            | "json_escape_string"
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
            | "go_error_to_string"
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
    let is_callable =
        goenv.toplevel_funcs.contains(name) || goenv.genv.value_env.extern_funcs.contains_key(name);
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
        .chain(goenv.genv.value_env.extern_funcs.keys())
        .map(|name| (name.clone(), resolve_toplevel_func_name(goenv, name)))
        .collect()
}

fn go_callable_name_collides(goenv: &GlobalGoEnv, ident: &str) -> bool {
    goenv.colliding_callable_idents.contains(ident)
}

fn collect_colliding_callable_idents(goenv: &GlobalGoEnv) -> HashSet<String> {
    let mut counts = HashMap::new();
    let mut colliding = HashSet::new();
    for name in goenv
        .toplevel_funcs
        .iter()
        .chain(goenv.genv.value_env.extern_funcs.keys())
    {
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
        || goenv
            .genv
            .type_env
            .extern_types
            .keys()
            .any(|extern_name| go_user_type_name(extern_name) == name)
}

fn go_value_name(goenv: &GlobalGoEnv, name: &str) -> String {
    if goenv.toplevel_funcs.contains(name) || goenv.genv.value_env.extern_funcs.contains_key(name) {
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

fn compile_imm_for_target_ty(
    goenv: &GlobalGoEnv,
    imm: &anf::ImmExpr,
    target_ty: &tast::Ty,
) -> goast::Expr {
    if needs_closure_to_func_wrap(&imm_ty(imm), target_ty) {
        closure_to_func_lit(goenv, imm, target_ty)
    } else {
        compile_imm(goenv, imm)
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
        || goenv
            .genv
            .type_env
            .extern_types
            .keys()
            .any(|extern_name| go_user_type_name(extern_name) == name)
        || goenv.callable_go_name_set.contains(name)
}

fn go_variant_symbol_name_is_reserved(goenv: &GlobalGoEnv, name: &str) -> bool {
    goenv.enums().any(|(enum_name, enum_def)| {
        enum_def.variants.iter().any(|(variant_name, _)| {
            variant_symbol_name(goenv, &enum_name.0, &variant_name.0) == name
        })
    })
}

fn go_package_alias_name_is_reserved(goenv: &GlobalGoEnv, name: &str) -> bool {
    go_toplevel_name_is_reserved(goenv, name)
        || go_variant_symbol_name_is_reserved(goenv, name)
        || goenv
            .genv
            .value_env
            .extern_funcs
            .keys()
            .any(|extern_name| go_ident(extern_name) == name)
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

fn go_package_default_alias(package_path: &str) -> String {
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

fn go_package_generated_alias(package_path: &str) -> String {
    let mut alias = String::from("_goml_pkg_");
    for byte in package_path.bytes() {
        if byte.is_ascii_alphanumeric() {
            alias.push(byte as char);
        } else {
            use std::fmt::Write;
            write!(&mut alias, "_x{:02x}_", byte).unwrap();
        }
    }
    alias
}

fn is_go_ident_char(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphanumeric()
}

fn raw_go_symbol_mentions_package(go_symbol: &str, package_alias: &str) -> bool {
    go_symbol.match_indices(package_alias).any(|(start, _)| {
        let before_ident = go_symbol[..start]
            .chars()
            .next_back()
            .is_some_and(is_go_ident_char);
        let end = start + package_alias.len();
        let after_dot = go_symbol[end..].starts_with('.');
        !before_ident && after_dot
    })
}

fn go_package_alias(goenv: &GlobalGoEnv, package_path: &str) -> String {
    match package_path {
        "fmt" => return "_goml_fmt".to_string(),
        "math" => return "_goml_math".to_string(),
        "unicode/utf8" => return "_goml_utf8".to_string(),
        _ => {}
    }
    let alias = go_package_default_alias(package_path);
    if go_package_default_alias_is_ambiguous(goenv, package_path)
        || go_package_alias_name_is_reserved(goenv, &alias)
        || runtime_generated_function_name(&alias)
    {
        return go_package_generated_alias(package_path);
    }
    alias
}

fn rewrite_go_package_alias(go_symbol: &str, from: &str, to: &str) -> String {
    if from == to || !raw_go_symbol_mentions_package(go_symbol, from) {
        return go_symbol.to_string();
    }

    enum State {
        Normal,
        LineComment,
        BlockComment,
        DoubleString { escaped: bool },
        RawString,
        Rune { escaped: bool },
    }

    let mut rewritten = String::new();
    let mut state = State::Normal;
    let mut index = 0usize;
    while index < go_symbol.len() {
        let rest = &go_symbol[index..];
        match state {
            State::Normal => {
                if rest.starts_with("//") {
                    rewritten.push_str("//");
                    index += 2;
                    state = State::LineComment;
                    continue;
                }
                if rest.starts_with("/*") {
                    rewritten.push_str("/*");
                    index += 2;
                    state = State::BlockComment;
                    continue;
                }
                let ch = rest.chars().next().unwrap();
                if ch == '"' {
                    rewritten.push(ch);
                    index += ch.len_utf8();
                    state = State::DoubleString { escaped: false };
                    continue;
                }
                if ch == '`' {
                    rewritten.push(ch);
                    index += ch.len_utf8();
                    state = State::RawString;
                    continue;
                }
                if ch == '\'' {
                    rewritten.push(ch);
                    index += ch.len_utf8();
                    state = State::Rune { escaped: false };
                    continue;
                }
                if rest.starts_with(from) {
                    let before_ident = go_symbol[..index]
                        .chars()
                        .next_back()
                        .is_some_and(is_go_ident_char);
                    let end = index + from.len();
                    let after_dot = go_symbol[end..].starts_with('.');
                    if !before_ident && after_dot {
                        rewritten.push_str(to);
                        index = end;
                        continue;
                    }
                }
                rewritten.push(ch);
                index += ch.len_utf8();
            }
            State::LineComment => {
                let ch = rest.chars().next().unwrap();
                rewritten.push(ch);
                index += ch.len_utf8();
                if ch == '\n' {
                    state = State::Normal;
                }
            }
            State::BlockComment => {
                if rest.starts_with("*/") {
                    rewritten.push_str("*/");
                    index += 2;
                    state = State::Normal;
                } else {
                    let ch = rest.chars().next().unwrap();
                    rewritten.push(ch);
                    index += ch.len_utf8();
                }
            }
            State::DoubleString { escaped } => {
                let ch = rest.chars().next().unwrap();
                rewritten.push(ch);
                index += ch.len_utf8();
                state = if escaped {
                    State::DoubleString { escaped: false }
                } else if ch == '\\' {
                    State::DoubleString { escaped: true }
                } else if ch == '"' {
                    State::Normal
                } else {
                    State::DoubleString { escaped: false }
                };
            }
            State::RawString => {
                let ch = rest.chars().next().unwrap();
                rewritten.push(ch);
                index += ch.len_utf8();
                if ch == '`' {
                    state = State::Normal;
                }
            }
            State::Rune { escaped } => {
                let ch = rest.chars().next().unwrap();
                rewritten.push(ch);
                index += ch.len_utf8();
                state = if escaped {
                    State::Rune { escaped: false }
                } else if ch == '\\' {
                    State::Rune { escaped: true }
                } else if ch == '\'' {
                    State::Normal
                } else {
                    State::Rune { escaped: false }
                };
            }
        }
    }
    rewritten
}

fn go_import_package_paths(goenv: &GlobalGoEnv) -> IndexSet<String> {
    let mut package_paths = IndexSet::new();
    for extern_fn in goenv.genv.value_env.extern_funcs.values() {
        if !extern_fn.package_path.is_empty() {
            package_paths.insert(extern_fn.package_path.clone());
        }
    }
    for extern_ty in goenv.genv.type_env.extern_types.values() {
        if let Some(package_path) = &extern_ty.package_path
            && !package_path.is_empty()
        {
            package_paths.insert(package_path.clone());
        }
    }
    package_paths
}

fn go_package_default_alias_is_ambiguous(goenv: &GlobalGoEnv, package_path: &str) -> bool {
    let alias = go_package_default_alias(package_path);
    go_import_package_paths(goenv)
        .iter()
        .filter(|other_path| go_package_default_alias(other_path) == alias)
        .take(2)
        .count()
        > 1
}

fn rewrite_raw_go_symbol_import_aliases(goenv: &GlobalGoEnv, go_symbol: &str) -> String {
    let mut rewritten = go_symbol.to_string();
    for package_path in go_import_package_paths(goenv) {
        let from = go_package_default_alias(&package_path);
        if raw_go_symbol_mentions_package(&rewritten, &from) {
            let to = go_package_alias(goenv, &package_path);
            rewritten = rewrite_go_package_alias(&rewritten, &from, &to);
        }
    }
    rewritten
}

fn go_import_alias(goenv: &GlobalGoEnv, package_path: &str) -> Option<String> {
    let alias = go_package_alias(goenv, package_path);
    if alias == go_package_default_alias(package_path) {
        None
    } else {
        Some(alias)
    }
}

fn go_type_alias_name_is_reserved(goenv: &GlobalGoEnv, name: &str) -> bool {
    goenv
        .structs()
        .any(|(struct_name, _)| go_user_type_name(&struct_name.0) == name)
        || goenv
            .enums()
            .any(|(enum_name, _)| go_user_type_name(&enum_name.0) == name)
        || goenv.enums().any(|(enum_name, enum_def)| {
            enum_def.variants.iter().any(|(variant_name, _)| {
                variant_symbol_name_for_go_enum(
                    goenv,
                    &go_user_type_name(&enum_name.0),
                    &variant_name.0,
                ) == name
            })
        })
}

fn extern_wrapper_fn_name(goenv: &GlobalGoEnv, goml_name: &str) -> String {
    let base = go_ident(&format!("{}_ffi_wrap", goml_name));
    if !go_package_alias_name_is_reserved(goenv, &base) {
        return base;
    }
    let base = if is_generated_tuple_type_name(&base) {
        format!("_goml_user_{}", base)
    } else {
        base
    };
    if !go_package_alias_name_is_reserved(goenv, &base) {
        return base;
    }
    let mut index = 0usize;
    loop {
        let candidate = if index == 0 {
            format!("{}__extern", base)
        } else {
            format!("{}__extern{}", base, index)
        };
        if !go_package_alias_name_is_reserved(goenv, &candidate) {
            return candidate;
        }
        index += 1;
    }
}

fn extern_target_name(goenv: &GlobalGoEnv, extern_fn: &ExternFunc) -> String {
    if extern_fn.package_path.is_empty() {
        rewrite_raw_go_symbol_import_aliases(goenv, &extern_fn.go_name)
    } else {
        let alias = go_package_alias(goenv, &extern_fn.package_path);
        let target = qualify_go_symbol(&alias, &extern_fn.go_name);
        rewrite_go_package_alias(
            &target,
            &go_package_default_alias(&extern_fn.package_path),
            &alias,
        )
    }
}

fn qualify_go_type_expr(package_alias: &str, go_name: &str) -> String {
    let mut parser = GoTypeExprQualifier::new(package_alias, go_name);
    parser.parse().unwrap_or_else(|| go_name.trim().to_string())
}

#[derive(Clone)]
struct GoTypeExprQualifier<'a> {
    package_alias: &'a str,
    input: &'a str,
    pos: usize,
}

impl<'a> GoTypeExprQualifier<'a> {
    fn new(package_alias: &'a str, input: &'a str) -> Self {
        Self {
            package_alias,
            input,
            pos: 0,
        }
    }

    fn parse(&mut self) -> Option<String> {
        let ty = self.parse_type()?;
        self.skip_ws();
        (self.pos == self.input.len()).then_some(ty)
    }

    fn parse_type(&mut self) -> Option<String> {
        self.skip_ws();

        if self.consume_str("<-chan") {
            let elem = self.parse_type()?;
            return Some(format!("<-chan {}", elem));
        }

        if self.consume_keyword("chan") {
            self.skip_ws();
            if self.consume_str("<-") {
                let elem = self.parse_type()?;
                return Some(format!("chan<- {}", elem));
            }
            let elem = self.parse_type()?;
            return Some(format!("chan {}", elem));
        }

        if self.consume_char('*') {
            let elem = self.parse_type()?;
            return Some(format!("*{}", elem));
        }

        if self.consume_str("[]") {
            let elem = self.parse_type()?;
            return Some(format!("[]{}", elem));
        }

        if self.peek_char() == Some('[') {
            self.pos += 1;
            let start = self.pos;
            while let Some(ch) = self.peek_char() {
                if ch == ']' {
                    let len = self.input[start..self.pos].trim();
                    self.pos += 1;
                    let elem = self.parse_type()?;
                    return Some(format!("[{}]{}", len, elem));
                }
                self.pos += ch.len_utf8();
            }
            return None;
        }

        if self.consume_keyword("map") {
            self.skip_ws();
            if !self.consume_char('[') {
                return None;
            }
            let key = self.parse_type()?;
            self.skip_ws();
            if !self.consume_char(']') {
                return None;
            }
            let value = self.parse_type()?;
            return Some(format!("map[{}]{}", key, value));
        }

        if self.consume_keyword("func") {
            return self
                .parse_func_signature()
                .map(|sig| format!("func{}", sig));
        }

        if self.consume_char('(') {
            let inner = self.parse_type()?;
            self.skip_ws();
            if !self.consume_char(')') {
                return None;
            }
            return Some(format!("({})", inner));
        }

        if self.consume_keyword("struct") || self.consume_keyword("interface") {
            return None;
        }

        self.parse_named_type()
    }

    fn parse_func_signature(&mut self) -> Option<String> {
        self.skip_ws();
        if !self.consume_char('(') {
            return None;
        }
        let params = self.parse_type_list(')')?;
        let mut sig = format!("({})", params.join(", "));
        self.skip_ws();
        if self.peek_char() == Some('(') {
            self.pos += 1;
            let results = self.parse_type_list(')')?;
            sig.push(' ');
            sig.push('(');
            sig.push_str(&results.join(", "));
            sig.push(')');
            return Some(sig);
        }
        if self.can_start_type() {
            let result = self.parse_type()?;
            sig.push(' ');
            sig.push_str(&result);
        }
        Some(sig)
    }

    fn parse_type_list(&mut self, end: char) -> Option<Vec<String>> {
        self.skip_ws();
        if self.consume_char(end) {
            return Some(Vec::new());
        }

        let mut tys = Vec::new();
        loop {
            tys.push(self.parse_type()?);
            self.skip_ws();
            if self.consume_char(',') {
                self.skip_ws();
                if self.consume_char(end) {
                    break;
                }
                continue;
            }
            if !self.consume_char(end) {
                return None;
            }
            break;
        }
        Some(tys)
    }

    fn parse_named_type(&mut self) -> Option<String> {
        let name = self.parse_ident()?;
        self.skip_ws();
        if self.consume_char('.') {
            let selector = self.parse_ident()?;
            return Some(format!("{}.{}", name, selector));
        }
        if is_go_predeclared_type(&name) {
            return Some(name);
        }
        Some(format!("{}.{}", self.package_alias, name))
    }

    fn can_start_type(&self) -> bool {
        let mut cursor = self.clone();
        cursor.skip_ws();
        match cursor.peek_char() {
            Some('*' | '[' | '(') => true,
            Some('<') => cursor.input[cursor.pos..].starts_with("<-chan"),
            Some(ch) if is_go_ident_start(ch) => true,
            _ => false,
        }
    }

    fn parse_ident(&mut self) -> Option<String> {
        self.skip_ws();
        let start = self.pos;
        let first = self.peek_char()?;
        if !is_go_ident_start(first) {
            return None;
        }
        self.pos += first.len_utf8();
        while let Some(ch) = self.peek_char() {
            if !is_go_ident_continue(ch) {
                break;
            }
            self.pos += ch.len_utf8();
        }
        Some(self.input[start..self.pos].to_string())
    }

    fn skip_ws(&mut self) {
        while let Some(ch) = self.peek_char() {
            if !ch.is_whitespace() {
                break;
            }
            self.pos += ch.len_utf8();
        }
    }

    fn consume_keyword(&mut self, kw: &str) -> bool {
        let mut cursor = self.clone();
        cursor.skip_ws();
        if !cursor.input[cursor.pos..].starts_with(kw) {
            return false;
        }
        let end = cursor.pos + kw.len();
        if cursor
            .input
            .get(end..)
            .and_then(|rest| rest.chars().next())
            .is_some_and(is_go_ident_continue)
        {
            return false;
        }
        self.pos = end;
        true
    }

    fn consume_str(&mut self, text: &str) -> bool {
        let mut cursor = self.clone();
        cursor.skip_ws();
        if !cursor.input[cursor.pos..].starts_with(text) {
            return false;
        }
        self.pos = cursor.pos + text.len();
        true
    }

    fn consume_char(&mut self, ch: char) -> bool {
        let mut cursor = self.clone();
        cursor.skip_ws();
        if cursor.peek_char() != Some(ch) {
            return false;
        }
        self.pos = cursor.pos + ch.len_utf8();
        true
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }
}

fn is_go_ident_start(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphabetic()
}

fn is_go_ident_continue(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphanumeric()
}

fn is_go_predeclared_type(name: &str) -> bool {
    matches!(
        name,
        "any"
            | "bool"
            | "byte"
            | "comparable"
            | "complex64"
            | "complex128"
            | "error"
            | "float32"
            | "float64"
            | "int"
            | "int8"
            | "int16"
            | "int32"
            | "int64"
            | "rune"
            | "string"
            | "uint"
            | "uint8"
            | "uint16"
            | "uint32"
            | "uint64"
            | "uintptr"
    )
}

fn qualify_go_symbol(package_alias: &str, go_name: &str) -> String {
    if let Some(rest) = go_name.strip_prefix("(*")
        && let Some((receiver_ty, suffix)) = rest.split_once(')')
    {
        return format!(
            "(*{}){}",
            qualify_go_type_expr(package_alias, receiver_ty),
            suffix
        );
    }

    if let Some(rest) = go_name.strip_prefix('(')
        && let Some((receiver_ty, suffix)) = rest.split_once(')')
    {
        return format!(
            "({}){}",
            qualify_go_type_expr(package_alias, receiver_ty),
            suffix
        );
    }

    if let Some((first_segment, rest)) = go_name.split_once('.') {
        if first_segment
            .chars()
            .next()
            .is_some_and(|ch| ch.is_ascii_lowercase())
        {
            if let Some((receiver_tail, method_suffix)) = rest.split_once('.') {
                return format!("({}.{}).{}", first_segment, receiver_tail, method_suffix);
            }
            return go_name.to_string();
        }
        return format!(
            "({}).{}",
            qualify_go_type_expr(package_alias, first_segment),
            rest
        );
    }

    format!("{}.{}", package_alias, go_name)
}

fn parse_go_method_symbol(go_name: &str) -> Option<&str> {
    if let Some(rest) = go_name.strip_prefix("(*") {
        return rest.split_once(").").map(|(_, method_name)| method_name);
    }

    if let Some(rest) = go_name.strip_prefix('(') {
        return rest.split_once(").").map(|(_, method_name)| method_name);
    }

    let parts = go_name.split('.').collect::<Vec<_>>();
    match parts.as_slice() {
        [receiver_ty, method_name]
            if receiver_ty
                .chars()
                .next()
                .is_some_and(|ch| ch.is_ascii_uppercase()) =>
        {
            Some(*method_name)
        }
        [package_name, receiver_ty, method_name]
            if package_name
                .chars()
                .next()
                .is_some_and(|ch| ch.is_ascii_lowercase())
                && receiver_ty
                    .chars()
                    .next()
                    .is_some_and(|ch| ch.is_ascii_uppercase()) =>
        {
            Some(*method_name)
        }
        _ => None,
    }
}

fn compile_extern_call(
    goenv: &GlobalGoEnv,
    extern_fn: &ExternFunc,
    param_tys: &[tast::Ty],
    mut call_args: Vec<goast::Expr>,
    call_ret_ty: goty::GoType,
) -> goast::Expr {
    match extern_fn.binding_mode {
        ExternBindingMode::Call => {}
        ExternBindingMode::Value => {
            panic!(
                "extern value binding {} cannot be called directly without a wrapper",
                extern_fn.go_name
            );
        }
        ExternBindingMode::FieldGetter | ExternBindingMode::FieldSetter => {
            panic!(
                "extern field binding {} cannot be called directly without a wrapper",
                extern_fn.go_name
            );
        }
    }

    call_args = param_tys
        .iter()
        .zip(call_args)
        .map(|(param_ty, arg)| adapt_extern_func_arg(goenv, param_ty, arg))
        .collect();

    if extern_fn.variadic_last {
        let last = call_args
            .pop()
            .expect("variadic extern binding requires a last argument");
        call_args.push(goast::Expr::Spread {
            ty: last.get_ty().clone(),
            expr: Box::new(last),
        });
    }

    if let Some(method_name) = parse_go_method_symbol(&extern_fn.go_name) {
        let mut args_iter = call_args.into_iter();
        let receiver = args_iter.next().unwrap_or_else(|| {
            panic!(
                "extern method binding {} requires a receiver argument",
                extern_fn.go_name
            )
        });
        let method_args = args_iter.collect::<Vec<_>>();
        return goast::Expr::Call {
            func: Box::new(goast::Expr::FieldAccess {
                obj: Box::new(receiver),
                field: method_name.to_string(),
                ty: goty::GoType::TFunc {
                    params: param_tys
                        .iter()
                        .skip(1)
                        .map(|ty| extern_func_ty_to_go_type(goenv, ty))
                        .collect(),
                    ret_ty: Box::new(call_ret_ty.clone()),
                },
            }),
            args: method_args,
            ty: call_ret_ty,
        };
    }

    goast::Expr::Call {
        func: Box::new(goast::Expr::Var {
            name: extern_target_name(goenv, extern_fn),
            ty: goty::GoType::TFunc {
                params: param_tys
                    .iter()
                    .map(|ty| extern_func_ty_to_go_type(goenv, ty))
                    .collect(),
                ret_ty: Box::new(call_ret_ty.clone()),
            },
        }),
        args: call_args,
        ty: call_ret_ty,
    }
}

fn compile_extern_value(
    goenv: &GlobalGoEnv,
    extern_fn: &ExternFunc,
    value_ty: goty::GoType,
) -> goast::Expr {
    goast::Expr::Var {
        name: extern_target_name(goenv, extern_fn),
        ty: value_ty,
    }
}

fn tuple_elem_tys(ty: &tast::Ty) -> Option<&[tast::Ty]> {
    let tast::Ty::TTuple { typs } = ty else {
        return None;
    };
    Some(typs.as_slice())
}

fn unwrap_tuple_elems(ty: &tast::Ty) -> Vec<&tast::Ty> {
    tuple_elem_tys(ty)
        .map(|tys| tys.iter().collect())
        .unwrap_or_else(|| vec![ty])
}

fn unwrap_tuple_go_tys(ty: &tast::Ty) -> Vec<goty::GoType> {
    unwrap_tuple_elems(ty)
        .into_iter()
        .map(tast_ty_to_go_type)
        .collect()
}

fn tuple_pack_expr(ty: &tast::Ty, names: &[String]) -> goast::Expr {
    if let Some(tuple_tys) = tuple_elem_tys(ty) {
        let fields = names
            .iter()
            .zip(tuple_tys.iter())
            .enumerate()
            .map(|(index, (name, field_ty))| {
                (
                    format!("_{}", index),
                    goast::Expr::Var {
                        name: name.clone(),
                        ty: tast_ty_to_go_type(field_ty),
                    },
                )
            })
            .collect();
        return goast::Expr::StructLiteral {
            fields,
            ty: tast_ty_to_go_type(ty),
        };
    }
    goast::Expr::Var {
        name: names[0].clone(),
        ty: tast_ty_to_go_type(ty),
    }
}

fn tuple_unpack_from_name(ty: &tast::Ty, name: &str) -> Vec<goast::Expr> {
    if let Some(tuple_tys) = tuple_elem_tys(ty) {
        let packed_ty = tast_ty_to_go_type(ty);
        return tuple_tys
            .iter()
            .enumerate()
            .map(|(index, field_ty)| goast::Expr::FieldAccess {
                obj: Box::new(goast::Expr::Var {
                    name: name.to_string(),
                    ty: packed_ty.clone(),
                }),
                field: format!("_{}", index),
                ty: tast_ty_to_go_type(field_ty),
            })
            .collect();
    }
    vec![goast::Expr::Var {
        name: name.to_string(),
        ty: tast_ty_to_go_type(ty),
    }]
}

fn declare_multi_assign(
    names: &[String],
    tys: &[goty::GoType],
    value: goast::Expr,
) -> Vec<goast::Stmt> {
    let mut stmts = names
        .iter()
        .zip(tys.iter())
        .map(|(name, ty)| goast::Stmt::VarDecl {
            name: name.clone(),
            ty: ty.clone(),
            value: None,
        })
        .collect::<Vec<_>>();
    stmts.push(goast::Stmt::MultiAssignment {
        names: names.to_vec(),
        value,
    });
    stmts
}

fn build_payload_success_return_stmts(
    payload_ty: &tast::Ty,
    payload_expr: goast::Expr,
    trailing: Vec<goast::Expr>,
) -> Vec<goast::Stmt> {
    if tuple_elem_tys(payload_ty).is_some() {
        let payload_name = "ret_payload".to_string();
        let mut exprs = tuple_unpack_from_name(payload_ty, &payload_name);
        exprs.extend(trailing);
        return vec![
            goast::Stmt::VarDecl {
                name: payload_name,
                ty: tast_ty_to_go_type(payload_ty),
                value: Some(payload_expr),
            },
            goast::Stmt::ReturnMulti { exprs },
        ];
    }

    let mut exprs = vec![payload_expr];
    exprs.extend(trailing);
    vec![goast::Stmt::ReturnMulti { exprs }]
}

fn extern_requires_wrapper(extern_fn: &ExternFunc) -> bool {
    matches!(
        extern_fn.binding_mode,
        ExternBindingMode::Value | ExternBindingMode::FieldGetter | ExternBindingMode::FieldSetter
    ) || extern_fn.return_mode != ExternReturnMode::Plain
        || matches!(
            &extern_fn.ty,
            tast::Ty::TFunc { ret_ty, .. } if matches!(ret_ty.as_ref(), tast::Ty::TTuple { .. })
        )
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

#[derive(Debug, Clone)]
struct ResultVariantLayout {
    ok_index: usize,
    err_index: usize,
    ok_ty: tast::Ty,
    err_ty: tast::Ty,
}

fn result_variant_layout(goenv: &GlobalGoEnv, ty: &tast::Ty) -> Option<ResultVariantLayout> {
    let enum_def = enum_def_for_ty(goenv, ty)?;
    if enum_def.variants.len() != 2 {
        return None;
    }

    let concrete_tys =
        extract_result_tys(ty).map(|(ok_ty, err_ty)| (ok_ty.clone(), err_ty.clone()));

    let mut ok_layout = None;
    let mut err_layout = None;
    for (index, (name, fields)) in enum_def.variants.iter().enumerate() {
        match name.0.as_str() {
            "Ok" if fields.len() == 1 => {
                ok_layout = Some((index, fields[0].clone()));
            }
            "Err" if fields.len() == 1 => {
                err_layout = Some((index, fields[0].clone()));
            }
            _ => return None,
        }
    }

    let (ok_index, def_ok_ty) = ok_layout?;
    let (err_index, def_err_ty) = err_layout?;
    let (ok_ty, err_ty) = concrete_tys.unwrap_or((def_ok_ty, def_err_ty));
    Some(ResultVariantLayout {
        ok_index,
        err_index,
        ok_ty,
        err_ty,
    })
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

#[derive(Debug, Clone)]
struct OptionVariantLayout {
    none_index: usize,
    some_index: usize,
    some_ty: tast::Ty,
}

fn option_variant_layout(goenv: &GlobalGoEnv, ty: &tast::Ty) -> Option<OptionVariantLayout> {
    let enum_def = enum_def_for_ty(goenv, ty)?;
    if enum_def.variants.len() != 2 {
        return None;
    }

    let concrete_some_ty = extract_option_ty(ty).cloned();

    let mut none_index = None;
    let mut some_layout = None;
    for (index, (name, fields)) in enum_def.variants.iter().enumerate() {
        match name.0.as_str() {
            "None" if fields.is_empty() => {
                none_index = Some(index);
            }
            "Some" if fields.len() == 1 => {
                some_layout = Some((index, fields[0].clone()));
            }
            _ => return None,
        }
    }

    let some_index = some_layout.as_ref()?.0;
    let some_ty = concrete_some_ty.unwrap_or(some_layout?.1);
    Some(OptionVariantLayout {
        none_index: none_index?,
        some_index,
        some_ty,
    })
}

fn is_builtin_go_error_ty(goenv: &GlobalGoEnv, ty: &tast::Ty) -> bool {
    let tast::Ty::TStruct { name } = ty else {
        return false;
    };
    if name != "GoError" {
        return false;
    }
    goenv
        .genv
        .type_env
        .extern_types
        .get(name)
        .is_some_and(|ext| {
            matches!(ext.package_path.as_deref(), None | Some("")) && ext.go_name == "error"
        })
}

enum ExternFuncReturnMode {
    Plain,
    Tuple(Vec<tast::Ty>),
    ErrorOnly { err_ty: tast::Ty },
    ErrorLast { ok_ty: tast::Ty, err_ty: tast::Ty },
    OptionOnly,
    OptionLast { some_ty: tast::Ty },
}

fn extern_func_return_mode(goenv: &GlobalGoEnv, ty: &tast::Ty) -> ExternFuncReturnMode {
    if let Some(mode) = native_return_mode(goenv, ty) {
        match mode {
            NativeReturnMode::Result { ok_ty, err_ty, .. } => {
                if matches!(ok_ty, tast::Ty::TUnit) {
                    return ExternFuncReturnMode::ErrorOnly { err_ty };
                }
                return ExternFuncReturnMode::ErrorLast { ok_ty, err_ty };
            }
            NativeReturnMode::Option { some_ty, .. } => {
                if matches!(some_ty, tast::Ty::TUnit) {
                    return ExternFuncReturnMode::OptionOnly;
                }
                return ExternFuncReturnMode::OptionLast { some_ty };
            }
        }
    }

    if let Some(tys) = tuple_elem_tys(ty) {
        return ExternFuncReturnMode::Tuple(tys.to_vec());
    }

    ExternFuncReturnMode::Plain
}

fn extern_func_ty_to_go_type(goenv: &GlobalGoEnv, ty: &tast::Ty) -> goty::GoType {
    match ty {
        tast::Ty::TFunc { params, ret_ty } => goty::GoType::TFunc {
            params: params
                .iter()
                .map(|param| extern_func_ty_to_go_type(goenv, param))
                .collect(),
            ret_ty: Box::new(extern_func_ret_ty_to_go_type(goenv, ret_ty)),
        },
        _ => tast_ty_to_go_type(ty),
    }
}

fn extern_func_ret_ty_to_go_type(goenv: &GlobalGoEnv, ty: &tast::Ty) -> goty::GoType {
    match extern_func_return_mode(goenv, ty) {
        ExternFuncReturnMode::Plain => extern_func_ty_to_go_type(goenv, ty),
        ExternFuncReturnMode::Tuple(tys) => goty::GoType::TMulti {
            elems: tys
                .iter()
                .map(|ty| extern_func_ty_to_go_type(goenv, ty))
                .collect(),
        },
        ExternFuncReturnMode::ErrorOnly { err_ty } => extern_func_ty_to_go_type(goenv, &err_ty),
        ExternFuncReturnMode::ErrorLast { ok_ty, err_ty } => {
            let mut elems = unwrap_tuple_elems(&ok_ty)
                .into_iter()
                .map(|ty| extern_func_ty_to_go_type(goenv, ty))
                .collect::<Vec<_>>();
            elems.push(extern_func_ty_to_go_type(goenv, &err_ty));
            goty::GoType::TMulti { elems }
        }
        ExternFuncReturnMode::OptionOnly => goty::GoType::TBool,
        ExternFuncReturnMode::OptionLast { some_ty } => {
            let mut elems = unwrap_tuple_elems(&some_ty)
                .into_iter()
                .map(|ty| extern_func_ty_to_go_type(goenv, ty))
                .collect::<Vec<_>>();
            elems.push(goty::GoType::TBool);
            goty::GoType::TMulti { elems }
        }
    }
}

fn adapt_extern_func_arg(
    goenv: &GlobalGoEnv,
    param_ty: &tast::Ty,
    arg: goast::Expr,
) -> goast::Expr {
    let tast::Ty::TFunc { params, ret_ty } = param_ty else {
        return arg;
    };

    let param_names = params
        .iter()
        .enumerate()
        .map(|(index, _)| format!("callback_arg_{}", index))
        .collect::<Vec<_>>();
    let go_params = param_names
        .iter()
        .zip(params.iter())
        .map(|(name, ty)| (name.clone(), extern_func_ty_to_go_type(goenv, ty)))
        .collect::<Vec<_>>();
    let wrapper_ty = extern_func_ty_to_go_type(goenv, param_ty);
    let call_expr = goast::Expr::Call {
        func: Box::new(arg),
        args: param_names
            .iter()
            .zip(params.iter())
            .map(|(name, ty)| goast::Expr::Var {
                name: name.clone(),
                ty: tast_ty_to_go_type(ty),
            })
            .collect(),
        ty: tast_ty_to_go_type(ret_ty),
    };

    let body = build_extern_func_arg_body(goenv, ret_ty, call_expr);

    goast::Expr::FuncLit {
        params: go_params,
        body,
        ty: wrapper_ty,
    }
}

fn build_extern_func_arg_body(
    goenv: &GlobalGoEnv,
    ret_ty: &tast::Ty,
    call_expr: goast::Expr,
) -> Vec<goast::Stmt> {
    match extern_func_return_mode(goenv, ret_ty) {
        ExternFuncReturnMode::Plain => vec![goast::Stmt::Return {
            expr: Some(call_expr),
        }],
        ExternFuncReturnMode::Tuple(_) => {
            let value_name = "ret_value".to_string();
            vec![
                goast::Stmt::VarDecl {
                    name: value_name.clone(),
                    ty: tast_ty_to_go_type(ret_ty),
                    value: Some(call_expr),
                },
                goast::Stmt::ReturnMulti {
                    exprs: tuple_unpack_from_name(ret_ty, &value_name),
                },
            ]
        }
        ExternFuncReturnMode::ErrorOnly { err_ty } => {
            let value_name = "ret_value".to_string();
            let value_ty = tast_ty_to_go_type(ret_ty);
            let ok_variant_ty = variant_ty_by_index(goenv, ret_ty, 0);
            let err_variant_ty = variant_ty_by_index(goenv, ret_ty, 1);
            vec![
                goast::Stmt::VarDecl {
                    name: value_name.clone(),
                    ty: value_ty.clone(),
                    value: Some(call_expr),
                },
                goast::Stmt::SwitchType {
                    bind: Some("ret_variant".to_string()),
                    expr: goast::Expr::Var {
                        name: value_name,
                        ty: value_ty,
                    },
                    cases: vec![
                        (
                            ok_variant_ty,
                            goast::Block {
                                stmts: vec![goast::Stmt::Return {
                                    expr: Some(goast::Expr::Nil {
                                        ty: tast_ty_to_go_type(&err_ty),
                                    }),
                                }],
                            },
                        ),
                        (
                            err_variant_ty.clone(),
                            goast::Block {
                                stmts: vec![goast::Stmt::Return {
                                    expr: Some(goast::Expr::FieldAccess {
                                        obj: Box::new(goast::Expr::Var {
                                            name: "ret_variant".to_string(),
                                            ty: err_variant_ty,
                                        }),
                                        field: "_0".to_string(),
                                        ty: tast_ty_to_go_type(&err_ty),
                                    }),
                                }],
                            },
                        ),
                    ],
                    default: Some(goast::Block {
                        stmts: vec![panic_stmt("non-exhaustive match")],
                    }),
                },
            ]
        }
        ExternFuncReturnMode::ErrorLast { ok_ty, err_ty } => {
            let value_name = "ret_value".to_string();
            let value_ty = tast_ty_to_go_type(ret_ty);
            let err_go_ty = tast_ty_to_go_type(&err_ty);
            let ok_variant_ty = variant_ty_by_index(goenv, ret_ty, 0);
            let err_variant_ty = variant_ty_by_index(goenv, ret_ty, 1);
            vec![
                goast::Stmt::VarDecl {
                    name: value_name.clone(),
                    ty: value_ty.clone(),
                    value: Some(call_expr),
                },
                goast::Stmt::SwitchType {
                    bind: Some("ret_variant".to_string()),
                    expr: goast::Expr::Var {
                        name: value_name,
                        ty: value_ty,
                    },
                    cases: vec![
                        (
                            ok_variant_ty.clone(),
                            goast::Block {
                                stmts: build_payload_success_return_stmts(
                                    &ok_ty,
                                    goast::Expr::FieldAccess {
                                        obj: Box::new(goast::Expr::Var {
                                            name: "ret_variant".to_string(),
                                            ty: ok_variant_ty,
                                        }),
                                        field: "_0".to_string(),
                                        ty: tast_ty_to_go_type(&ok_ty),
                                    },
                                    vec![goast::Expr::Nil { ty: err_go_ty }],
                                ),
                            },
                        ),
                        (
                            err_variant_ty.clone(),
                            goast::Block {
                                stmts: build_result_failure_stmts(
                                    &ok_ty,
                                    goast::Expr::FieldAccess {
                                        obj: Box::new(goast::Expr::Var {
                                            name: "ret_variant".to_string(),
                                            ty: err_variant_ty,
                                        }),
                                        field: "_0".to_string(),
                                        ty: tast_ty_to_go_type(&err_ty),
                                    },
                                ),
                            },
                        ),
                    ],
                    default: Some(goast::Block {
                        stmts: vec![panic_stmt("non-exhaustive match")],
                    }),
                },
            ]
        }
        ExternFuncReturnMode::OptionOnly => {
            let value_name = "ret_value".to_string();
            let value_ty = tast_ty_to_go_type(ret_ty);
            let none_variant_ty = variant_ty_by_index(goenv, ret_ty, 0);
            let some_variant_ty = variant_ty_by_index(goenv, ret_ty, 1);
            vec![
                goast::Stmt::VarDecl {
                    name: value_name.clone(),
                    ty: value_ty.clone(),
                    value: Some(call_expr),
                },
                goast::Stmt::SwitchType {
                    bind: Some("ret_variant".to_string()),
                    expr: goast::Expr::Var {
                        name: value_name,
                        ty: value_ty,
                    },
                    cases: vec![
                        (
                            none_variant_ty,
                            goast::Block {
                                stmts: vec![goast::Stmt::Return {
                                    expr: Some(goast::Expr::Bool {
                                        value: false,
                                        ty: goty::GoType::TBool,
                                    }),
                                }],
                            },
                        ),
                        (
                            some_variant_ty,
                            goast::Block {
                                stmts: vec![goast::Stmt::Return {
                                    expr: Some(goast::Expr::Bool {
                                        value: true,
                                        ty: goty::GoType::TBool,
                                    }),
                                }],
                            },
                        ),
                    ],
                    default: Some(goast::Block {
                        stmts: vec![panic_stmt("non-exhaustive match")],
                    }),
                },
            ]
        }
        ExternFuncReturnMode::OptionLast { some_ty } => {
            let value_name = "ret_value".to_string();
            let value_ty = tast_ty_to_go_type(ret_ty);
            let none_variant_ty = variant_ty_by_index(goenv, ret_ty, 0);
            let some_variant_ty = variant_ty_by_index(goenv, ret_ty, 1);
            vec![
                goast::Stmt::VarDecl {
                    name: value_name.clone(),
                    ty: value_ty.clone(),
                    value: Some(call_expr),
                },
                goast::Stmt::SwitchType {
                    bind: Some("ret_variant".to_string()),
                    expr: goast::Expr::Var {
                        name: value_name,
                        ty: value_ty,
                    },
                    cases: vec![
                        (
                            none_variant_ty,
                            goast::Block {
                                stmts: build_option_failure_stmts(&some_ty),
                            },
                        ),
                        (
                            some_variant_ty.clone(),
                            goast::Block {
                                stmts: build_payload_success_return_stmts(
                                    &some_ty,
                                    goast::Expr::FieldAccess {
                                        obj: Box::new(goast::Expr::Var {
                                            name: "ret_variant".to_string(),
                                            ty: some_variant_ty,
                                        }),
                                        field: "_0".to_string(),
                                        ty: tast_ty_to_go_type(&some_ty),
                                    },
                                    vec![goast::Expr::Bool {
                                        value: true,
                                        ty: goty::GoType::TBool,
                                    }],
                                ),
                            },
                        ),
                    ],
                    default: Some(goast::Block {
                        stmts: vec![panic_stmt("non-exhaustive match")],
                    }),
                },
            ]
        }
    }
}

#[derive(Debug, Clone)]
enum NativeReturnMode {
    Result {
        ok_index: usize,
        err_index: usize,
        ok_ty: tast::Ty,
        err_ty: tast::Ty,
    },
    Option {
        none_index: usize,
        some_index: usize,
        some_ty: tast::Ty,
    },
}

#[derive(Debug, Clone)]
struct NativeFnCtx {
    return_join: anf::JoinId,
    mode: NativeReturnMode,
}

type LetEnv = HashMap<anf::LocalId, anf::ValueExpr>;

fn native_return_mode(goenv: &GlobalGoEnv, ret_ty: &tast::Ty) -> Option<NativeReturnMode> {
    if let Some(layout) = result_variant_layout(goenv, ret_ty)
        && is_builtin_go_error_ty(goenv, &layout.err_ty)
    {
        return Some(NativeReturnMode::Result {
            ok_index: layout.ok_index,
            err_index: layout.err_index,
            ok_ty: layout.ok_ty,
            err_ty: layout.err_ty,
        });
    }

    if let Some(layout) = option_variant_layout(goenv, ret_ty) {
        return Some(NativeReturnMode::Option {
            none_index: layout.none_index,
            some_index: layout.some_index,
            some_ty: layout.some_ty,
        });
    }

    None
}

fn native_helper_fn_name(goml_name: &str) -> String {
    go_generated_ident(&format!("{}__native", goml_name))
}

fn native_helper_ret_ty(mode: &NativeReturnMode) -> goty::GoType {
    match mode {
        NativeReturnMode::Result { ok_ty, err_ty, .. } => {
            let mut elems = unwrap_tuple_go_tys(ok_ty);
            elems.push(tast_ty_to_go_type(err_ty));
            goty::GoType::TMulti { elems }
        }
        NativeReturnMode::Option { some_ty, .. } => {
            let mut elems = unwrap_tuple_go_tys(some_ty);
            elems.push(goty::GoType::TBool);
            goty::GoType::TMulti { elems }
        }
    }
}

fn native_fn_ctx(goenv: &GlobalGoEnv, f: &anf::Fn) -> Option<NativeFnCtx> {
    let anf::Bind::Join(return_join) = f.body.binds.first()? else {
        return None;
    };

    native_return_mode(goenv, &f.ret_ty).map(|mode| NativeFnCtx {
        return_join: return_join.id.clone(),
        mode,
    })
}

fn enum_variant_expr(
    goenv: &GlobalGoEnv,
    enum_ty: &tast::Ty,
    variant_index: usize,
    payload: Option<goast::Expr>,
) -> goast::Expr {
    if enum_is_tag_only(goenv, enum_ty) {
        assert!(
            payload.is_none(),
            "tag-only enum variant cannot carry payload: {:?}",
            enum_ty
        );
        return variant_const_expr_by_index(goenv, enum_ty, variant_index);
    }
    goast::Expr::StructLiteral {
        fields: payload
            .into_iter()
            .map(|payload| ("_0".to_string(), payload))
            .collect(),
        ty: variant_ty_by_index(goenv, enum_ty, variant_index),
    }
}

fn gen_extern_wrapper_fn(
    goenv: &GlobalGoEnv,
    goml_name: &str,
    extern_fn: &ExternFunc,
) -> Option<goast::Fn> {
    if !extern_requires_wrapper(extern_fn) {
        return None;
    }

    let tast::Ty::TFunc { params, ret_ty } = &extern_fn.ty else {
        panic!(
            "extern function {} does not have a function type",
            goml_name
        );
    };
    let wrapper_ret_ty = ret_ty.as_ref().clone();

    let go_params = params
        .iter()
        .enumerate()
        .map(|(index, ty)| (format!("p{}", index), tast_ty_to_go_type(ty)))
        .collect::<Vec<_>>();
    let call_args = go_params
        .iter()
        .map(|(name, ty)| goast::Expr::Var {
            name: name.clone(),
            ty: ty.clone(),
        })
        .collect::<Vec<_>>();
    match extern_fn.binding_mode {
        ExternBindingMode::Value => {
            return Some(goast::Fn {
                name: extern_wrapper_fn_name(goenv, goml_name),
                params: go_params,
                ret_ty: Some(tast_ty_to_go_type(&wrapper_ret_ty)),
                body: goast::Block {
                    stmts: vec![goast::Stmt::Return {
                        expr: Some(compile_extern_value(
                            goenv,
                            extern_fn,
                            tast_ty_to_go_type(&wrapper_ret_ty),
                        )),
                    }],
                },
            });
        }
        ExternBindingMode::FieldGetter => {
            let field_name = extern_fn
                .field_name
                .as_ref()
                .expect("field getter binding missing field name")
                .clone();
            return Some(goast::Fn {
                name: extern_wrapper_fn_name(goenv, goml_name),
                params: go_params.clone(),
                ret_ty: Some(tast_ty_to_go_type(&wrapper_ret_ty)),
                body: goast::Block {
                    stmts: vec![goast::Stmt::Return {
                        expr: Some(goast::Expr::FieldAccess {
                            obj: Box::new(goast::Expr::Var {
                                name: "p0".to_string(),
                                ty: go_params[0].1.clone(),
                            }),
                            field: field_name,
                            ty: tast_ty_to_go_type(&wrapper_ret_ty),
                        }),
                    }],
                },
            });
        }
        ExternBindingMode::FieldSetter => {
            let field_name = extern_fn
                .field_name
                .as_ref()
                .expect("field setter binding missing field name")
                .clone();
            let field_ty = go_params[1].1.clone();
            return Some(goast::Fn {
                name: extern_wrapper_fn_name(goenv, goml_name),
                params: go_params.clone(),
                ret_ty: Some(goty::GoType::TUnit),
                body: goast::Block {
                    stmts: vec![
                        goast::Stmt::FieldAssign {
                            target: goast::Expr::FieldAccess {
                                obj: Box::new(goast::Expr::Var {
                                    name: "p0".to_string(),
                                    ty: go_params[0].1.clone(),
                                }),
                                field: field_name,
                                ty: field_ty.clone(),
                            },
                            value: goast::Expr::Var {
                                name: "p1".to_string(),
                                ty: field_ty,
                            },
                        },
                        goast::Stmt::Return {
                            expr: Some(goast::Expr::Unit {
                                ty: goty::GoType::TUnit,
                            }),
                        },
                    ],
                },
            });
        }
        ExternBindingMode::Call => {}
    }

    let wrapper = match extern_fn.return_mode {
        ExternReturnMode::Plain => {
            let tuple_go_tys = unwrap_tuple_go_tys(&wrapper_ret_ty);
            let value_names = tuple_go_tys
                .iter()
                .enumerate()
                .map(|(index, _)| format!("ffi_value_{}", index))
                .collect::<Vec<_>>();
            let direct_call_expr = compile_extern_call(
                goenv,
                extern_fn,
                params,
                call_args,
                goty::GoType::TMulti {
                    elems: tuple_go_tys.clone(),
                },
            );
            let mut stmts = declare_multi_assign(&value_names, &tuple_go_tys, direct_call_expr);
            stmts.push(goast::Stmt::Return {
                expr: Some(tuple_pack_expr(&wrapper_ret_ty, &value_names)),
            });
            goast::Fn {
                name: extern_wrapper_fn_name(goenv, goml_name),
                params: go_params,
                ret_ty: Some(tast_ty_to_go_type(&wrapper_ret_ty)),
                body: goast::Block { stmts },
            }
        }
        ExternReturnMode::ErrorOnly | ExternReturnMode::ErrorLast => {
            let Some(layout) = result_variant_layout(goenv, &wrapper_ret_ty) else {
                panic!(
                    "extern function {} with return mode {:?} must return Result",
                    goml_name, extern_fn.return_mode
                );
            };
            let ok_ty = layout.ok_ty.clone();
            let err_ty = layout.err_ty.clone();

            let ok_go_tys = unwrap_tuple_go_tys(&ok_ty);
            let ok_names = ok_go_tys
                .iter()
                .enumerate()
                .map(|(index, _)| format!("ffi_value_{}", index))
                .collect::<Vec<_>>();
            let direct_call_ret_ty = match extern_fn.return_mode {
                ExternReturnMode::ErrorOnly => tast_ty_to_go_type(&err_ty),
                ExternReturnMode::ErrorLast => {
                    let mut elems = ok_go_tys.clone();
                    elems.push(tast_ty_to_go_type(&err_ty));
                    goty::GoType::TMulti { elems }
                }
                ExternReturnMode::Plain | ExternReturnMode::OptionLast => unreachable!(),
            };
            let direct_call_expr =
                compile_extern_call(goenv, extern_fn, params, call_args, direct_call_ret_ty);

            let err_name = "ffi_err".to_string();
            let err_go_ty = tast_ty_to_go_type(&err_ty);
            let err_result_expr = enum_variant_expr(
                goenv,
                &wrapper_ret_ty,
                layout.err_index,
                Some(goast::Expr::Var {
                    name: err_name.clone(),
                    ty: err_go_ty.clone(),
                }),
            );
            let ok_payload = if extern_fn.return_mode == ExternReturnMode::ErrorOnly {
                goast::Expr::Unit {
                    ty: goty::GoType::TUnit,
                }
            } else {
                tuple_pack_expr(&ok_ty, &ok_names)
            };
            let ok_result_expr =
                enum_variant_expr(goenv, &wrapper_ret_ty, layout.ok_index, Some(ok_payload));

            let mut stmts = Vec::new();
            match extern_fn.return_mode {
                ExternReturnMode::ErrorOnly => {
                    stmts.push(goast::Stmt::VarDecl {
                        name: err_name.clone(),
                        ty: err_go_ty.clone(),
                        value: Some(direct_call_expr),
                    });
                }
                ExternReturnMode::ErrorLast => {
                    let mut names = ok_names.clone();
                    names.push(err_name.clone());
                    let mut tys = ok_go_tys.clone();
                    tys.push(err_go_ty.clone());
                    stmts.extend(declare_multi_assign(&names, &tys, direct_call_expr));
                }
                ExternReturnMode::Plain | ExternReturnMode::OptionLast => unreachable!(),
            }

            stmts.push(goast::Stmt::If {
                cond: goast::Expr::BinaryOp {
                    op: goast::GoBinaryOp::NotEq,
                    lhs: Box::new(goast::Expr::Var {
                        name: err_name.clone(),
                        ty: err_go_ty.clone(),
                    }),
                    rhs: Box::new(goast::Expr::Nil { ty: err_go_ty }),
                    ty: goty::GoType::TBool,
                },
                then: goast::Block {
                    stmts: vec![goast::Stmt::Return {
                        expr: Some(err_result_expr),
                    }],
                },
                else_: None,
            });
            stmts.push(goast::Stmt::Return {
                expr: Some(ok_result_expr),
            });

            goast::Fn {
                name: extern_wrapper_fn_name(goenv, goml_name),
                params: go_params,
                ret_ty: Some(tast_ty_to_go_type(&wrapper_ret_ty)),
                body: goast::Block { stmts },
            }
        }
        ExternReturnMode::OptionLast => {
            let Some(layout) = option_variant_layout(goenv, &wrapper_ret_ty) else {
                panic!(
                    "extern function {} with return mode {:?} must return Option",
                    goml_name, extern_fn.return_mode
                );
            };
            let inner_ty = layout.some_ty.clone();

            let some_go_tys = unwrap_tuple_go_tys(&inner_ty);
            let value_names = some_go_tys
                .iter()
                .enumerate()
                .map(|(index, _)| format!("ffi_value_{}", index))
                .collect::<Vec<_>>();
            let ok_name = "ffi_ok".to_string();
            let mut direct_call_tys = some_go_tys.clone();
            direct_call_tys.push(goty::GoType::TBool);
            let direct_call_expr = compile_extern_call(
                goenv,
                extern_fn,
                params,
                call_args,
                goty::GoType::TMulti {
                    elems: direct_call_tys.clone(),
                },
            );
            let mut names = value_names.clone();
            names.push(ok_name.clone());
            let mut stmts = declare_multi_assign(&names, &direct_call_tys, direct_call_expr);
            stmts.push(goast::Stmt::If {
                cond: goast::Expr::Var {
                    name: ok_name,
                    ty: goty::GoType::TBool,
                },
                then: goast::Block {
                    stmts: vec![goast::Stmt::Return {
                        expr: Some(enum_variant_expr(
                            goenv,
                            &wrapper_ret_ty,
                            layout.some_index,
                            Some(tuple_pack_expr(&inner_ty, &value_names)),
                        )),
                    }],
                },
                else_: None,
            });
            stmts.push(goast::Stmt::Return {
                expr: Some(enum_variant_expr(
                    goenv,
                    &wrapper_ret_ty,
                    layout.none_index,
                    None,
                )),
            });

            goast::Fn {
                name: extern_wrapper_fn_name(goenv, goml_name),
                params: go_params,
                ret_ty: Some(tast_ty_to_go_type(&wrapper_ret_ty)),
                body: goast::Block { stmts },
            }
        }
    };

    Some(wrapper)
}

fn gen_extern_wrapper_fns(goenv: &GlobalGoEnv) -> Vec<goast::Item> {
    goenv
        .genv
        .value_env
        .extern_funcs
        .iter()
        .filter_map(|(goml_name, extern_fn)| {
            gen_extern_wrapper_fn(goenv, goml_name, extern_fn).map(goast::Item::Fn)
        })
        .collect()
}

fn gen_extern_bridge_fn(goenv: &GlobalGoEnv, goml_name: &str, extern_fn: &ExternFunc) -> goast::Fn {
    let tast::Ty::TFunc { params, ret_ty } = &extern_fn.ty else {
        panic!(
            "extern function {} does not have a function type",
            goml_name
        );
    };
    let bridge_ret_ty = ret_ty.as_ref().clone();
    let go_params = params
        .iter()
        .enumerate()
        .map(|(index, ty)| (format!("p{}", index), tast_ty_to_go_type(ty)))
        .collect::<Vec<_>>();
    let call_args = go_params
        .iter()
        .map(|(name, ty)| goast::Expr::Var {
            name: name.clone(),
            ty: ty.clone(),
        })
        .collect::<Vec<_>>();
    let call = if extern_requires_wrapper(extern_fn) {
        goast::Expr::Call {
            func: Box::new(goast::Expr::Var {
                name: extern_wrapper_fn_name(goenv, goml_name),
                ty: goty::GoType::TFunc {
                    params: params.iter().map(tast_ty_to_go_type).collect(),
                    ret_ty: Box::new(tast_ty_to_go_type(&bridge_ret_ty)),
                },
            }),
            args: call_args,
            ty: tast_ty_to_go_type(&bridge_ret_ty),
        }
    } else {
        compile_extern_call(
            goenv,
            extern_fn,
            params,
            call_args,
            tast_ty_to_go_type(&bridge_ret_ty),
        )
    };

    goast::Fn {
        name: go_toplevel_func_name(goenv, goml_name),
        params: go_params,
        ret_ty: Some(tast_ty_to_go_type(&bridge_ret_ty)),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return { expr: Some(call) }],
        },
    }
}

fn gen_extern_bridge_fns(goenv: &GlobalGoEnv) -> Vec<goast::Item> {
    goenv
        .genv
        .value_env
        .extern_funcs
        .iter()
        .map(|(goml_name, extern_fn)| {
            goast::Item::Fn(gen_extern_bridge_fn(goenv, goml_name, extern_fn))
        })
        .collect()
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
    for extern_fn in goenv.genv.value_env.extern_funcs.values() {
        collect_ty(&mut req, &extern_fn.ty);
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
                } else if let anf::ImmExpr::ImmVar { name, .. } = &func
                    && let Some(extern_fn) = goenv.genv.value_env.extern_funcs.get(name)
                {
                    if extern_requires_wrapper(extern_fn) {
                        goast::Expr::Call {
                            func: Box::new(goast::Expr::Var {
                                name: extern_wrapper_fn_name(goenv, name),
                                ty: goty::GoType::TFunc {
                                    params: param_types.iter().map(tast_ty_to_go_type).collect(),
                                    ret_ty: Box::new(tast_ty_to_go_type(ty)),
                                },
                            }),
                            args: compiled_args,
                            ty: tast_ty_to_go_type(ty),
                        }
                    } else {
                        compile_extern_call(
                            goenv,
                            extern_fn,
                            &param_types,
                            compiled_args,
                            tast_ty_to_go_type(ty),
                        )
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

    if let anf::ImmExpr::Var { id, .. } = func
        && let Some(extern_fn) = goenv.genv.value_env.extern_funcs.get(&id.0)
    {
        if !extern_requires_wrapper(extern_fn) {
            return compile_extern_call(
                goenv,
                extern_fn,
                &param_types,
                compiled_args,
                tast_ty_to_go_type(ty),
            );
        }

        return goast::Expr::Call {
            func: Box::new(goast::Expr::Var {
                name: extern_wrapper_fn_name(goenv, &id.0),
                ty: func_ty,
            }),
            args: compiled_args,
            ty: tast_ty_to_go_type(ty),
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

fn resolve_bound_value<'a>(
    imm: &'a anf::ImmExpr,
    let_env: &'a LetEnv,
) -> Option<&'a anf::ValueExpr> {
    let mut current = imm;
    loop {
        match current {
            anf::ImmExpr::Var { id, .. } => match let_env.get(id)? {
                anf::ValueExpr::Imm(next) => current = next,
                value => return Some(value),
            },
            _ => return None,
        }
    }
}

fn resolve_native_success_join<'a>(
    block: &anf::Block,
    pending_joins: &[&'a anf::JoinBind],
    join_env: &'a JoinEnv,
) -> Option<&'a anf::JoinBind> {
    let anf::Term::Jump { target, .. } = &block.term else {
        return None;
    };
    let mut current_target = target;
    loop {
        if let Some(join_bind) = pending_joins
            .iter()
            .copied()
            .find(|join| join.id == *current_target)
        {
            return Some(join_bind);
        }

        let join_bind = join_env.get(current_target)?;
        if !join_bind.body.binds.is_empty() {
            return None;
        }
        let anf::Term::Jump {
            target: next_target,
            args,
            ..
        } = &join_bind.body.term
        else {
            return None;
        };
        if args.len() != join_bind.params.len() {
            return None;
        }
        for ((param, _), arg) in join_bind.params.iter().zip(args.iter()) {
            let anf::ImmExpr::Var { id, .. } = arg else {
                return None;
            };
            if id != param {
                return None;
            }
        }
        current_target = next_target;
    }
}

fn imm_live_ids(imm: &anf::ImmExpr) -> HashSet<anf::LocalId> {
    match imm {
        anf::ImmExpr::Var { id, .. } => HashSet::from([id.clone()]),
        anf::ImmExpr::Prim { .. } | anf::ImmExpr::Tag { .. } => HashSet::new(),
    }
}

fn value_expr_has_side_effects(value: &anf::ValueExpr) -> bool {
    matches!(
        value,
        anf::ValueExpr::Assign { .. }
            | anf::ValueExpr::Call { .. }
            | anf::ValueExpr::DynCall { .. }
            | anf::ValueExpr::Go { .. }
    )
}

fn value_expr_live_ids(value: &anf::ValueExpr) -> HashSet<anf::LocalId> {
    match value {
        anf::ValueExpr::Imm(imm) => imm_live_ids(imm),
        anf::ValueExpr::Constr { args, .. }
        | anf::ValueExpr::Tuple { items: args, .. }
        | anf::ValueExpr::Array { items: args, .. } => {
            args.iter().flat_map(imm_live_ids).collect::<HashSet<_>>()
        }
        anf::ValueExpr::ConstrGet { expr, .. }
        | anf::ValueExpr::Unary { expr, .. }
        | anf::ValueExpr::ToDyn { expr, .. }
        | anf::ValueExpr::Go { closure: expr, .. }
        | anf::ValueExpr::Proj { tuple: expr, .. } => imm_live_ids(expr),
        anf::ValueExpr::Binary { lhs, rhs, .. } => {
            let mut live = imm_live_ids(lhs);
            live.extend(imm_live_ids(rhs));
            live
        }
        anf::ValueExpr::Assign { name, value, .. } => {
            let mut live = imm_live_ids(value);
            live.insert(name.clone());
            live
        }
        anf::ValueExpr::Call { func, args, .. } => {
            let mut live = imm_live_ids(func);
            live.extend(args.iter().flat_map(imm_live_ids));
            live
        }
        anf::ValueExpr::DynCall { receiver, args, .. } => {
            let mut live = imm_live_ids(receiver);
            live.extend(args.iter().flat_map(imm_live_ids));
            live
        }
    }
}

fn term_live_ids(term: &anf::Term) -> HashSet<anf::LocalId> {
    match term {
        anf::Term::Return(imm) => imm_live_ids(imm),
        anf::Term::Jump { args, .. } => args.iter().flat_map(imm_live_ids).collect::<HashSet<_>>(),
        anf::Term::If {
            cond, then_, else_, ..
        } => {
            let mut live = imm_live_ids(cond);
            live.extend(block_live_in_ids(then_));
            live.extend(block_live_in_ids(else_));
            live
        }
        anf::Term::Match {
            scrut,
            arms,
            default,
            ..
        } => {
            let mut live = imm_live_ids(scrut);
            for arm in arms {
                live.extend(imm_live_ids(&arm.lhs));
                live.extend(block_live_in_ids(&arm.body));
            }
            if let Some(default) = default {
                live.extend(block_live_in_ids(default));
            }
            live
        }
        anf::Term::Unreachable { .. } => HashSet::new(),
    }
}

fn join_live_in_ids(join_bind: &anf::JoinBind) -> HashSet<anf::LocalId> {
    let mut live = block_live_in_ids(&join_bind.body);
    for (param, _) in &join_bind.params {
        live.remove(param);
    }
    live
}

fn block_live_in_ids(block: &anf::Block) -> HashSet<anf::LocalId> {
    let mut live = term_live_ids(&block.term);
    for bind in block.binds.iter().rev() {
        match bind {
            anf::Bind::Let(let_bind) => {
                let value_live = value_expr_live_ids(&let_bind.value);
                let result_live = live.remove(&let_bind.id);
                if result_live || value_expr_has_side_effects(&let_bind.value) {
                    live.extend(value_live);
                }
            }
            anf::Bind::Join(join_bind) => {
                live.extend(join_live_in_ids(join_bind));
            }
            anf::Bind::JoinRec(group) => {
                for join_bind in group {
                    live.extend(join_live_in_ids(join_bind));
                }
            }
        }
    }
    live
}

fn join_params_need_success_value(join_bind: &anf::JoinBind) -> bool {
    let live = block_live_in_ids(&join_bind.body);
    join_bind.params.iter().any(|(id, _)| live.contains(id))
}

fn join_forwards_to_native_return(
    join_bind: &anf::JoinBind,
    join_env: &JoinEnv,
    native_ctx: &NativeFnCtx,
) -> bool {
    let mut current = join_bind;
    loop {
        if !current.body.binds.is_empty() || current.params.len() != 1 {
            return false;
        }
        let anf::Term::Jump { target, args, .. } = &current.body.term else {
            return false;
        };
        if args.len() != 1 {
            return false;
        }
        let anf::ImmExpr::Var { id, .. } = &args[0] else {
            return false;
        };
        if id != &current.params[0].0 {
            return false;
        }
        if *target == native_ctx.return_join {
            return true;
        }
        let Some(next) = join_env.get(target) else {
            return false;
        };
        current = next;
    }
}

fn build_result_failure_stmts(ok_ty: &tast::Ty, err_expr: goast::Expr) -> Vec<goast::Stmt> {
    let ok_go_ty = tast_ty_to_go_type(ok_ty);
    let mut exprs = tuple_unpack_from_name(ok_ty, "ret_zero");
    exprs.push(err_expr);
    vec![
        goast::Stmt::VarDecl {
            name: "ret_zero".to_string(),
            ty: ok_go_ty.clone(),
            value: None,
        },
        goast::Stmt::ReturnMulti { exprs },
    ]
}

fn build_option_failure_stmts(some_ty: &tast::Ty) -> Vec<goast::Stmt> {
    let some_go_ty = tast_ty_to_go_type(some_ty);
    let mut exprs = tuple_unpack_from_name(some_ty, "ret_zero");
    exprs.push(goast::Expr::Bool {
        value: false,
        ty: goty::GoType::TBool,
    });
    vec![
        goast::Stmt::VarDecl {
            name: "ret_zero".to_string(),
            ty: some_go_ty.clone(),
            value: None,
        },
        goast::Stmt::ReturnMulti { exprs },
    ]
}

fn native_mode_needs_success_value(mode: &NativeReturnMode) -> bool {
    match mode {
        NativeReturnMode::Result { ok_ty, .. } => !matches!(ok_ty, tast::Ty::TUnit),
        NativeReturnMode::Option { some_ty, .. } => !matches!(some_ty, tast::Ty::TUnit),
    }
}

fn compile_native_return_from_imm(
    goenv: &GlobalGoEnv,
    imm: &anf::ImmExpr,
    native_ctx: &NativeFnCtx,
    let_env: &LetEnv,
) -> Vec<goast::Stmt> {
    if let anf::ImmExpr::Tag { index, .. } = imm {
        match (&native_ctx.mode, index) {
            (
                NativeReturnMode::Option {
                    none_index,
                    some_ty,
                    ..
                },
                idx,
            ) if *idx == *none_index => {
                return build_option_failure_stmts(some_ty);
            }
            (
                NativeReturnMode::Result {
                    ok_index,
                    ok_ty,
                    err_ty,
                    ..
                },
                idx,
            ) if *idx == *ok_index && matches!(ok_ty, tast::Ty::TUnit) => {
                return vec![goast::Stmt::ReturnMulti {
                    exprs: vec![
                        goast::Expr::Unit {
                            ty: tast_ty_to_go_type(ok_ty),
                        },
                        goast::Expr::Nil {
                            ty: tast_ty_to_go_type(err_ty),
                        },
                    ],
                }];
            }
            _ => {}
        }
    }

    if let Some(value) = resolve_bound_value(imm, let_env) {
        let prefix = match imm {
            anf::ImmExpr::Var { id, .. } => id.0.replace('/', "_"),
            _ => "native_return".to_string(),
        };
        let need_success_value = native_mode_needs_success_value(&native_ctx.mode);
        if let Some(plan) =
            build_native_call_plan(goenv, value, native_ctx, need_success_value, &prefix)
            && (plan.success_expr.is_some() || !need_success_value)
        {
            let success_expr = plan.success_expr.unwrap_or_else(|| match &native_ctx.mode {
                NativeReturnMode::Result { ok_ty, .. } => goast::Expr::Unit {
                    ty: tast_ty_to_go_type(ok_ty),
                },
                NativeReturnMode::Option { some_ty, .. } => goast::Expr::Unit {
                    ty: tast_ty_to_go_type(some_ty),
                },
            });
            let mut stmts = plan.stmts;
            stmts.push(goast::Stmt::If {
                cond: plan.failure_cond,
                then: goast::Block {
                    stmts: match (&native_ctx.mode, plan.failure) {
                        (
                            NativeReturnMode::Result { ok_ty, .. },
                            NativeCallFailure::Result { err_expr },
                        ) => build_result_failure_stmts(ok_ty, err_expr),
                        (NativeReturnMode::Option { some_ty, .. }, NativeCallFailure::Option) => {
                            build_option_failure_stmts(some_ty)
                        }
                        _ => return vec![panic_stmt("mismatched native return failure mode")],
                    },
                },
                else_: None,
            });
            match &native_ctx.mode {
                NativeReturnMode::Result { ok_ty, err_ty, .. } => {
                    stmts.extend(build_payload_success_return_stmts(
                        ok_ty,
                        success_expr,
                        vec![goast::Expr::Nil {
                            ty: tast_ty_to_go_type(err_ty),
                        }],
                    ));
                }
                NativeReturnMode::Option { some_ty, .. } => {
                    stmts.extend(build_payload_success_return_stmts(
                        some_ty,
                        success_expr,
                        vec![goast::Expr::Bool {
                            value: true,
                            ty: goty::GoType::TBool,
                        }],
                    ));
                }
            }
            return stmts;
        }
        if let anf::ValueExpr::Constr {
            constructor: Constructor::Enum(enum_constructor),
            args,
            ..
        } = value
        {
            match &native_ctx.mode {
                NativeReturnMode::Result {
                    ok_index,
                    err_index,
                    ok_ty,
                    err_ty,
                } => {
                    if enum_constructor.index == *ok_index && args.len() == 1 {
                        return build_payload_success_return_stmts(
                            ok_ty,
                            compile_imm_for_target_ty(goenv, &args[0], ok_ty),
                            vec![goast::Expr::Nil {
                                ty: tast_ty_to_go_type(err_ty),
                            }],
                        );
                    }
                    if enum_constructor.index == *err_index && args.len() == 1 {
                        return build_result_failure_stmts(
                            ok_ty,
                            compile_imm_for_target_ty(goenv, &args[0], err_ty),
                        );
                    }
                }
                NativeReturnMode::Option {
                    none_index,
                    some_index,
                    some_ty,
                } => {
                    if enum_constructor.index == *some_index && args.len() == 1 {
                        return build_payload_success_return_stmts(
                            some_ty,
                            compile_imm_for_target_ty(goenv, &args[0], some_ty),
                            vec![goast::Expr::Bool {
                                value: true,
                                ty: goty::GoType::TBool,
                            }],
                        );
                    }
                    if enum_constructor.index == *none_index && args.is_empty() {
                        return build_option_failure_stmts(some_ty);
                    }
                }
            }
        }
    }

    let boxed_ty = imm_ty(imm);
    let boxed_expr = compile_imm(goenv, imm);
    match &native_ctx.mode {
        NativeReturnMode::Result {
            ok_index,
            err_index,
            ok_ty,
            err_ty,
        } => {
            let ok_variant_ty = variant_ty_by_index(goenv, &boxed_ty, *ok_index);
            let err_variant_ty = variant_ty_by_index(goenv, &boxed_ty, *err_index);
            let err_go_ty = tast_ty_to_go_type(err_ty);
            vec![goast::Stmt::SwitchType {
                bind: Some("ret_variant".to_string()),
                expr: boxed_expr,
                cases: vec![
                    (
                        ok_variant_ty.clone(),
                        goast::Block {
                            stmts: build_payload_success_return_stmts(
                                ok_ty,
                                goast::Expr::FieldAccess {
                                    obj: Box::new(goast::Expr::Var {
                                        name: "ret_variant".to_string(),
                                        ty: ok_variant_ty,
                                    }),
                                    field: "_0".to_string(),
                                    ty: tast_ty_to_go_type(ok_ty),
                                },
                                vec![goast::Expr::Nil {
                                    ty: err_go_ty.clone(),
                                }],
                            ),
                        },
                    ),
                    (
                        err_variant_ty.clone(),
                        goast::Block {
                            stmts: build_result_failure_stmts(
                                ok_ty,
                                goast::Expr::FieldAccess {
                                    obj: Box::new(goast::Expr::Var {
                                        name: "ret_variant".to_string(),
                                        ty: err_variant_ty,
                                    }),
                                    field: "_0".to_string(),
                                    ty: tast_ty_to_go_type(err_ty),
                                },
                            ),
                        },
                    ),
                ],
                default: Some(goast::Block {
                    stmts: vec![panic_stmt("non-exhaustive match")],
                }),
            }]
        }
        NativeReturnMode::Option {
            none_index,
            some_index,
            some_ty,
        } => {
            let none_variant_ty = variant_ty_by_index(goenv, &boxed_ty, *none_index);
            let some_variant_ty = variant_ty_by_index(goenv, &boxed_ty, *some_index);
            vec![goast::Stmt::SwitchType {
                bind: Some("ret_variant".to_string()),
                expr: boxed_expr,
                cases: vec![
                    (
                        none_variant_ty,
                        goast::Block {
                            stmts: build_option_failure_stmts(some_ty),
                        },
                    ),
                    (
                        some_variant_ty.clone(),
                        goast::Block {
                            stmts: build_payload_success_return_stmts(
                                some_ty,
                                goast::Expr::FieldAccess {
                                    obj: Box::new(goast::Expr::Var {
                                        name: "ret_variant".to_string(),
                                        ty: some_variant_ty,
                                    }),
                                    field: "_0".to_string(),
                                    ty: tast_ty_to_go_type(some_ty),
                                },
                                vec![goast::Expr::Bool {
                                    value: true,
                                    ty: goty::GoType::TBool,
                                }],
                            ),
                        },
                    ),
                ],
                default: Some(goast::Block {
                    stmts: vec![panic_stmt("non-exhaustive match")],
                }),
            }]
        }
    }
}

enum NativeCallFailure {
    Result { err_expr: goast::Expr },
    Option,
}

struct NativeCallPlan {
    stmts: Vec<goast::Stmt>,
    success_expr: Option<goast::Expr>,
    failure_cond: goast::Expr,
    failure: NativeCallFailure,
}

fn native_modes_compatible(lhs: &NativeReturnMode, rhs: &NativeReturnMode) -> bool {
    matches!(
        (lhs, rhs),
        (
            NativeReturnMode::Result { .. },
            NativeReturnMode::Result { .. }
        ) | (
            NativeReturnMode::Option { .. },
            NativeReturnMode::Option { .. }
        )
    )
}

fn build_native_call_plan(
    goenv: &GlobalGoEnv,
    value: &anf::ValueExpr,
    native_ctx: &NativeFnCtx,
    need_success_value: bool,
    prefix: &str,
) -> Option<NativeCallPlan> {
    let anf::ValueExpr::Call { func, args, ty } = value else {
        return None;
    };
    let callee_mode = native_return_mode(goenv, ty)?;
    let func_ty = imm_ty(func);
    let param_types = match &func_ty {
        tast::Ty::TFunc { params, .. } => params.clone(),
        _ => Vec::new(),
    };
    let compiled_args = compile_call_args(goenv, &param_types, args);

    if let anf::ImmExpr::Var { id, .. } = func
        && let Some(extern_fn) = goenv.genv.value_env.extern_funcs.get(&id.0)
    {
        return match (&native_ctx.mode, extern_fn.return_mode) {
            (NativeReturnMode::Result { .. }, ExternReturnMode::ErrorOnly) => {
                let NativeReturnMode::Result { err_ty, .. } = &callee_mode else {
                    return None;
                };
                let err_name = format!("{}_err", prefix);
                let err_go_ty = tast_ty_to_go_type(err_ty);
                Some(NativeCallPlan {
                    stmts: vec![goast::Stmt::VarDecl {
                        name: err_name.clone(),
                        ty: err_go_ty.clone(),
                        value: Some(compile_extern_call(
                            goenv,
                            extern_fn,
                            &param_types,
                            compiled_args,
                            err_go_ty.clone(),
                        )),
                    }],
                    success_expr: None,
                    failure_cond: goast::Expr::BinaryOp {
                        op: goast::GoBinaryOp::NotEq,
                        lhs: Box::new(goast::Expr::Var {
                            name: err_name.clone(),
                            ty: err_go_ty.clone(),
                        }),
                        rhs: Box::new(goast::Expr::Nil {
                            ty: err_go_ty.clone(),
                        }),
                        ty: goty::GoType::TBool,
                    },
                    failure: NativeCallFailure::Result {
                        err_expr: goast::Expr::Var {
                            name: err_name,
                            ty: err_go_ty,
                        },
                    },
                })
            }
            (NativeReturnMode::Result { .. }, ExternReturnMode::ErrorLast) => {
                let NativeReturnMode::Result { ok_ty, err_ty, .. } = &callee_mode else {
                    return None;
                };
                let ok_go_tys = unwrap_tuple_go_tys(ok_ty);
                let err_go_ty = tast_ty_to_go_type(err_ty);
                let value_names = ok_go_tys
                    .iter()
                    .enumerate()
                    .map(|(index, _)| format!("{}_value_{}", prefix, index))
                    .collect::<Vec<_>>();
                let err_name = format!("{}_err", prefix);
                let mut stmts = Vec::new();
                if need_success_value {
                    for (value_name, ok_go_ty) in value_names.iter().zip(ok_go_tys.iter()) {
                        stmts.push(goast::Stmt::VarDecl {
                            name: value_name.clone(),
                            ty: ok_go_ty.clone(),
                            value: None,
                        });
                    }
                }
                stmts.push(goast::Stmt::VarDecl {
                    name: err_name.clone(),
                    ty: err_go_ty.clone(),
                    value: None,
                });
                let mut names = if need_success_value {
                    value_names.clone()
                } else {
                    ok_go_tys
                        .iter()
                        .map(|_| "_".to_string())
                        .collect::<Vec<_>>()
                };
                names.push(err_name.clone());
                let mut elems = ok_go_tys.clone();
                elems.push(err_go_ty.clone());
                stmts.push(goast::Stmt::MultiAssignment {
                    names,
                    value: compile_extern_call(
                        goenv,
                        extern_fn,
                        &param_types,
                        compiled_args,
                        goty::GoType::TMulti { elems },
                    ),
                });
                Some(NativeCallPlan {
                    stmts,
                    success_expr: need_success_value.then(|| tuple_pack_expr(ok_ty, &value_names)),
                    failure_cond: goast::Expr::BinaryOp {
                        op: goast::GoBinaryOp::NotEq,
                        lhs: Box::new(goast::Expr::Var {
                            name: err_name.clone(),
                            ty: err_go_ty.clone(),
                        }),
                        rhs: Box::new(goast::Expr::Nil {
                            ty: err_go_ty.clone(),
                        }),
                        ty: goty::GoType::TBool,
                    },
                    failure: NativeCallFailure::Result {
                        err_expr: goast::Expr::Var {
                            name: err_name,
                            ty: err_go_ty,
                        },
                    },
                })
            }
            (NativeReturnMode::Option { .. }, ExternReturnMode::OptionLast) => {
                let NativeReturnMode::Option { some_ty, .. } = &callee_mode else {
                    return None;
                };
                let some_go_tys = unwrap_tuple_go_tys(some_ty);
                let value_names = some_go_tys
                    .iter()
                    .enumerate()
                    .map(|(index, _)| format!("{}_value_{}", prefix, index))
                    .collect::<Vec<_>>();
                let ok_name = format!("{}_ok", prefix);
                let mut stmts = Vec::new();
                if need_success_value {
                    for (value_name, some_go_ty) in value_names.iter().zip(some_go_tys.iter()) {
                        stmts.push(goast::Stmt::VarDecl {
                            name: value_name.clone(),
                            ty: some_go_ty.clone(),
                            value: None,
                        });
                    }
                }
                stmts.push(goast::Stmt::VarDecl {
                    name: ok_name.clone(),
                    ty: goty::GoType::TBool,
                    value: None,
                });
                let mut names = if need_success_value {
                    value_names.clone()
                } else {
                    some_go_tys
                        .iter()
                        .map(|_| "_".to_string())
                        .collect::<Vec<_>>()
                };
                names.push(ok_name.clone());
                let mut elems = some_go_tys.clone();
                elems.push(goty::GoType::TBool);
                stmts.push(goast::Stmt::MultiAssignment {
                    names,
                    value: compile_extern_call(
                        goenv,
                        extern_fn,
                        &param_types,
                        compiled_args,
                        goty::GoType::TMulti { elems },
                    ),
                });
                Some(NativeCallPlan {
                    stmts,
                    success_expr: need_success_value
                        .then(|| tuple_pack_expr(some_ty, &value_names)),
                    failure_cond: goast::Expr::UnaryOp {
                        op: goast::GoUnaryOp::Not,
                        expr: Box::new(goast::Expr::Var {
                            name: ok_name.clone(),
                            ty: goty::GoType::TBool,
                        }),
                        ty: goty::GoType::TBool,
                    },
                    failure: NativeCallFailure::Option,
                })
            }
            _ => None,
        };
    }

    if let anf::ImmExpr::Var { id, .. } = func
        && id.0 == "hashmap_get"
    {
        let map_ty = imm_ty(&args[0]);
        let tast::Ty::THashMap { key, value } = &map_ty else {
            return None;
        };
        let NativeReturnMode::Option { some_ty, .. } = &callee_mode else {
            return None;
        };
        let key_go_ty = tast_ty_to_go_type(key);
        let value_go_ty = tast_ty_to_go_type(value);
        let value_name = format!("{}_value", prefix);
        let ok_name = format!("{}_ok", prefix);
        let mut stmts = Vec::new();
        if need_success_value {
            stmts.push(goast::Stmt::VarDecl {
                name: value_name.clone(),
                ty: value_go_ty.clone(),
                value: None,
            });
        }
        stmts.push(goast::Stmt::VarDecl {
            name: ok_name.clone(),
            ty: goty::GoType::TBool,
            value: None,
        });
        stmts.push(goast::Stmt::MultiAssignment {
            names: vec![
                if need_success_value {
                    value_name.clone()
                } else {
                    "_".to_string()
                },
                ok_name.clone(),
            ],
            value: goast::Expr::Call {
                func: Box::new(goast::Expr::Var {
                    name: runtime::hashmap_get_native_helper_fn_name(&map_ty),
                    ty: goty::GoType::TFunc {
                        params: vec![tast_ty_to_go_type(&map_ty), key_go_ty],
                        ret_ty: Box::new(goty::GoType::TMulti {
                            elems: vec![value_go_ty.clone(), goty::GoType::TBool],
                        }),
                    },
                }),
                args: compiled_args,
                ty: goty::GoType::TMulti {
                    elems: vec![value_go_ty.clone(), goty::GoType::TBool],
                },
            },
        });
        return Some(NativeCallPlan {
            stmts,
            success_expr: need_success_value.then(|| goast::Expr::Var {
                name: value_name,
                ty: tast_ty_to_go_type(some_ty),
            }),
            failure_cond: goast::Expr::UnaryOp {
                op: goast::GoUnaryOp::Not,
                expr: Box::new(goast::Expr::Var {
                    name: ok_name.clone(),
                    ty: goty::GoType::TBool,
                }),
                ty: goty::GoType::TBool,
            },
            failure: NativeCallFailure::Option,
        });
    }

    if !native_modes_compatible(&native_ctx.mode, &callee_mode) {
        return None;
    }
    let anf::ImmExpr::Var { id, .. } = func else {
        return None;
    };
    if !goenv.toplevel_funcs.contains(&id.0) {
        return None;
    }

    let helper_ret_ty = native_helper_ret_ty(&callee_mode);
    let call_expr = goast::Expr::Call {
        func: Box::new(goast::Expr::Var {
            name: native_helper_fn_name(&id.0),
            ty: goty::GoType::TFunc {
                params: param_types.iter().map(tast_ty_to_go_type).collect(),
                ret_ty: Box::new(helper_ret_ty.clone()),
            },
        }),
        args: compiled_args,
        ty: helper_ret_ty,
    };

    match (&native_ctx.mode, callee_mode) {
        (
            NativeReturnMode::Result { err_ty, .. },
            NativeReturnMode::Result {
                ok_ty: callee_ok_ty,
                ..
            },
        ) => {
            let ok_go_tys = unwrap_tuple_go_tys(&callee_ok_ty);
            let err_go_ty = tast_ty_to_go_type(err_ty);
            let value_names = ok_go_tys
                .iter()
                .enumerate()
                .map(|(index, _)| format!("{}_value_{}", prefix, index))
                .collect::<Vec<_>>();
            let err_name = format!("{}_err", prefix);
            let mut stmts = Vec::new();
            for (value_name, ok_go_ty) in value_names.iter().zip(ok_go_tys.iter()) {
                if need_success_value {
                    stmts.push(goast::Stmt::VarDecl {
                        name: value_name.clone(),
                        ty: ok_go_ty.clone(),
                        value: None,
                    });
                }
            }
            stmts.push(goast::Stmt::VarDecl {
                name: err_name.clone(),
                ty: err_go_ty.clone(),
                value: None,
            });
            let mut names = if need_success_value {
                value_names.clone()
            } else {
                ok_go_tys
                    .iter()
                    .map(|_| "_".to_string())
                    .collect::<Vec<_>>()
            };
            names.push(err_name.clone());
            stmts.push(goast::Stmt::MultiAssignment {
                names,
                value: call_expr,
            });
            Some(NativeCallPlan {
                stmts,
                success_expr: need_success_value
                    .then(|| tuple_pack_expr(&callee_ok_ty, &value_names)),
                failure_cond: goast::Expr::BinaryOp {
                    op: goast::GoBinaryOp::NotEq,
                    lhs: Box::new(goast::Expr::Var {
                        name: err_name.clone(),
                        ty: err_go_ty.clone(),
                    }),
                    rhs: Box::new(goast::Expr::Nil {
                        ty: err_go_ty.clone(),
                    }),
                    ty: goty::GoType::TBool,
                },
                failure: NativeCallFailure::Result {
                    err_expr: goast::Expr::Var {
                        name: err_name,
                        ty: err_go_ty,
                    },
                },
            })
        }
        (
            NativeReturnMode::Option { .. },
            NativeReturnMode::Option {
                some_ty: callee_some_ty,
                ..
            },
        ) => {
            let some_go_tys = unwrap_tuple_go_tys(&callee_some_ty);
            let value_names = some_go_tys
                .iter()
                .enumerate()
                .map(|(index, _)| format!("{}_value_{}", prefix, index))
                .collect::<Vec<_>>();
            let ok_name = format!("{}_ok", prefix);
            let mut stmts = Vec::new();
            for (value_name, some_go_ty) in value_names.iter().zip(some_go_tys.iter()) {
                if need_success_value {
                    stmts.push(goast::Stmt::VarDecl {
                        name: value_name.clone(),
                        ty: some_go_ty.clone(),
                        value: None,
                    });
                }
            }
            stmts.push(goast::Stmt::VarDecl {
                name: ok_name.clone(),
                ty: goty::GoType::TBool,
                value: None,
            });
            let mut names = if need_success_value {
                value_names.clone()
            } else {
                some_go_tys
                    .iter()
                    .map(|_| "_".to_string())
                    .collect::<Vec<_>>()
            };
            names.push(ok_name.clone());
            stmts.push(goast::Stmt::MultiAssignment {
                names,
                value: call_expr,
            });
            Some(NativeCallPlan {
                stmts,
                success_expr: need_success_value
                    .then(|| tuple_pack_expr(&callee_some_ty, &value_names)),
                failure_cond: goast::Expr::UnaryOp {
                    op: goast::GoUnaryOp::Not,
                    expr: Box::new(goast::Expr::Var {
                        name: ok_name.clone(),
                        ty: goty::GoType::TBool,
                    }),
                    ty: goty::GoType::TBool,
                },
                failure: NativeCallFailure::Option,
            })
        }
        _ => None,
    }
}

fn native_try_scrutinee_id(
    goenv: &GlobalGoEnv,
    term: &anf::Term,
    join_env: &JoinEnv,
    pending_joins: &[&anf::JoinBind],
    native_ctx: Option<&NativeFnCtx>,
    let_env: &LetEnv,
) -> Option<anf::LocalId> {
    let native_ctx = native_ctx?;
    let anf::Term::Match {
        scrut,
        arms,
        default,
        ..
    } = term
    else {
        return None;
    };
    let anf::ImmExpr::Var { id, .. } = scrut else {
        return None;
    };
    if default.is_some()
        || compile_native_try_match(
            goenv,
            scrut,
            arms,
            native_ctx,
            join_env,
            pending_joins,
            let_env,
        )
        .is_none()
    {
        return None;
    }
    Some(id.clone())
}

fn can_inline_native_return_bound_value(
    goenv: &GlobalGoEnv,
    value: &anf::ValueExpr,
    native_ctx: &NativeFnCtx,
    prefix: &str,
) -> bool {
    let need_success_value = native_mode_needs_success_value(&native_ctx.mode);
    if let Some(plan) = build_native_call_plan(goenv, value, native_ctx, need_success_value, prefix)
        && (plan.success_expr.is_some() || !need_success_value)
    {
        return true;
    }

    match value {
        anf::ValueExpr::Constr {
            constructor: Constructor::Enum(enum_constructor),
            args,
            ..
        } => match &native_ctx.mode {
            NativeReturnMode::Result {
                ok_index,
                err_index,
                ..
            } => {
                (enum_constructor.index == *ok_index || enum_constructor.index == *err_index)
                    && args.len() == 1
            }
            NativeReturnMode::Option {
                none_index,
                some_index,
                ..
            } => {
                (enum_constructor.index == *none_index && args.is_empty())
                    || (enum_constructor.index == *some_index && args.len() == 1)
            }
        },
        _ => false,
    }
}

fn native_direct_return_bind_id(
    goenv: &GlobalGoEnv,
    term: &anf::Term,
    join_env: &JoinEnv,
    pending_joins: &[&anf::JoinBind],
    native_ctx: Option<&NativeFnCtx>,
    let_env: &LetEnv,
) -> Option<anf::LocalId> {
    let native_ctx = native_ctx?;
    let target = all_branches_jump_to(term)?;
    let forwards_to_native_return = if *target == native_ctx.return_join {
        true
    } else if let Some(join_bind) = join_env.get(target) {
        pending_joins.iter().any(|j| j.id == *target)
            && join_forwards_to_native_return(join_bind, join_env, native_ctx)
    } else {
        false
    };
    if !forwards_to_native_return {
        return None;
    }

    let anf::Term::Jump { args, .. } = term else {
        return None;
    };
    let [anf::ImmExpr::Var { id, .. }] = args.as_slice() else {
        return None;
    };
    let value = let_env.get(id)?;
    can_inline_native_return_bound_value(goenv, value, native_ctx, &id.0.replace('/', "_"))
        .then(|| id.clone())
}

fn matches_native_residual_arm(
    block: &anf::Block,
    native_ctx: &NativeFnCtx,
    let_env: &LetEnv,
) -> bool {
    let mut env = let_env.clone();
    for bind in &block.binds {
        if let anf::Bind::Let(let_bind) = bind {
            env.insert(let_bind.id.clone(), let_bind.value.clone());
        }
    }

    let anf::Term::Jump { target, args, .. } = &block.term else {
        return false;
    };
    if *target != native_ctx.return_join || args.len() != 1 {
        return false;
    }
    if matches!(
        (&native_ctx.mode, &args[0]),
        (
            NativeReturnMode::Option { none_index, .. },
            anf::ImmExpr::Tag { index, .. }
        ) if *index == *none_index
    ) {
        return true;
    }
    let Some(value) = resolve_bound_value(&args[0], &env) else {
        return false;
    };
    let anf::ValueExpr::Constr {
        constructor: Constructor::Enum(enum_constructor),
        args,
        ..
    } = value
    else {
        return false;
    };

    match &native_ctx.mode {
        NativeReturnMode::Result { err_index, .. } => {
            enum_constructor.index == *err_index && args.len() == 1
        }
        NativeReturnMode::Option { none_index, .. } => {
            enum_constructor.index == *none_index && args.is_empty()
        }
    }
}

fn compile_native_try_match(
    goenv: &GlobalGoEnv,
    scrut: &anf::ImmExpr,
    arms: &[anf::Arm],
    native_ctx: &NativeFnCtx,
    join_env: &JoinEnv,
    pending_joins: &[&anf::JoinBind],
    let_env: &LetEnv,
) -> Option<Vec<goast::Stmt>> {
    let anf::ImmExpr::Var { id, .. } = scrut else {
        return None;
    };
    let value = let_env.get(id)?;

    let (success_index, residual_index) = match &native_ctx.mode {
        NativeReturnMode::Result {
            ok_index,
            err_index,
            ..
        } => (*ok_index, *err_index),
        NativeReturnMode::Option {
            none_index,
            some_index,
            ..
        } => (*some_index, *none_index),
    };

    let success_arm = arms.iter().find(|arm| {
        matches!(
            arm.lhs,
            anf::ImmExpr::Tag {
                index,
                ..
            } if index == success_index
        )
    })?;
    let residual_arm = arms.iter().find(|arm| {
        matches!(
            arm.lhs,
            anf::ImmExpr::Tag {
                index,
                ..
            } if index == residual_index
        )
    })?;

    if !matches_native_residual_arm(&residual_arm.body, native_ctx, let_env) {
        return None;
    }

    let success_join = resolve_native_success_join(&success_arm.body, pending_joins, join_env)?;
    let need_success_value = join_params_need_success_value(success_join);
    let prefix = id.0.replace('/', "_");
    let plan = build_native_call_plan(goenv, value, native_ctx, need_success_value, &prefix)?;

    let mut stmts = plan.stmts;
    stmts.push(goast::Stmt::If {
        cond: plan.failure_cond,
        then: goast::Block {
            stmts: match (&native_ctx.mode, plan.failure) {
                (
                    NativeReturnMode::Result { ok_ty, .. },
                    NativeCallFailure::Result { err_expr },
                ) => build_result_failure_stmts(ok_ty, err_expr),
                (NativeReturnMode::Option { some_ty, .. }, NativeCallFailure::Option) => {
                    build_option_failure_stmts(some_ty)
                }
                _ => return None,
            },
        },
        else_: None,
    });

    if let Some((param, _)) = success_join.params.first()
        && let Some(success_expr) = plan.success_expr
    {
        stmts.push(goast::Stmt::Assignment {
            name: go_ident(&param.0),
            value: success_expr,
        });
    }
    stmts.extend(compile_block_structured_ctx(
        goenv,
        &success_join.body,
        join_env,
        Some(native_ctx),
        let_env,
    ));
    Some(stmts)
}

fn all_branches_jump_to(term: &anf::Term) -> Option<&anf::JoinId> {
    match term {
        anf::Term::Jump { target, .. } => Some(target),
        anf::Term::If { then_, else_, .. } => {
            let t = block_tail_target(then_)?;
            let e = block_tail_target(else_)?;
            if t == e { Some(t) } else { None }
        }
        anf::Term::Match { arms, default, .. } => {
            let mut result: Option<&anf::JoinId> = None;
            for arm in arms {
                let t = block_tail_target(&arm.body)?;
                if let Some(prev) = result {
                    if prev != t {
                        return None;
                    }
                } else {
                    result = Some(t);
                }
            }
            if let Some(def) = default {
                let t = block_tail_target(def)?;
                if let Some(prev) = result {
                    if prev != t {
                        return None;
                    }
                } else {
                    result = Some(t);
                }
            }
            result
        }
        _ => None,
    }
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

fn block_tail_target(block: &anf::Block) -> Option<&anf::JoinId> {
    all_branches_jump_to(&block.term)
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
    compile_block_structured_ctx(goenv, block, join_env, None, &HashMap::new())
}

fn compile_block_structured_ctx(
    goenv: &GlobalGoEnv,
    block: &anf::Block,
    join_env: &JoinEnv,
    native_ctx: Option<&NativeFnCtx>,
    let_env: &LetEnv,
) -> Vec<goast::Stmt> {
    let mut pending_joins: Vec<&anf::JoinBind> = Vec::new();
    let mut while_loop_ids: Vec<anf::JoinId> = Vec::new();
    let mut current_env = let_env.clone();
    let mut compiled_binds: Vec<(Option<anf::LocalId>, Vec<goast::Stmt>)> = Vec::new();
    for bind in &block.binds {
        match bind {
            anf::Bind::Let(let_bind) => {
                current_env.insert(let_bind.id.clone(), let_bind.value.clone());
                compiled_binds.push((Some(let_bind.id.clone()), compile_let_bind(goenv, let_bind)));
            }
            anf::Bind::Join(join_bind) => {
                if native_ctx.is_some_and(|ctx| ctx.return_join == join_bind.id) {
                    continue;
                }
                let mut bind_stmts = Vec::new();
                for (id, ty) in &join_bind.params {
                    if id.0 != "_" {
                        bind_stmts.push(goast::Stmt::VarDecl {
                            name: go_ident(&id.0),
                            ty: tast_ty_to_go_type(ty),
                            value: None,
                        });
                    }
                }
                pending_joins.push(join_bind);
                compiled_binds.push((None, bind_stmts));
            }
            anf::Bind::JoinRec(group) => {
                for j in group {
                    if is_while_loop(j).is_some() {
                        while_loop_ids.push(j.id.clone());
                    }
                }
                compiled_binds.push((
                    None,
                    compile_joinrec_ctx(goenv, group, join_env, native_ctx, &current_env),
                ));
            }
        }
    }
    let skip_bind = native_try_scrutinee_id(
        goenv,
        &block.term,
        join_env,
        &pending_joins,
        native_ctx,
        &current_env,
    )
    .or_else(|| {
        native_direct_return_bind_id(
            goenv,
            &block.term,
            join_env,
            &pending_joins,
            native_ctx,
            &current_env,
        )
    });
    let mut stmts = Vec::new();
    for (bind_id, bind_stmts) in compiled_binds {
        if let (Some(bind_id), Some(skip_bind)) = (bind_id.as_ref(), skip_bind.as_ref())
            && bind_id == skip_bind
        {
            continue;
        }
        stmts.extend(bind_stmts);
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
        native_ctx,
        &current_env,
    ));
    stmts
}

fn compile_term_with_continuations_ctx(
    goenv: &GlobalGoEnv,
    term: &anf::Term,
    join_env: &JoinEnv,
    pending_joins: &[&anf::JoinBind],
    native_ctx: Option<&NativeFnCtx>,
    let_env: &LetEnv,
) -> Vec<goast::Stmt> {
    if let Some(native_ctx) = native_ctx
        && let anf::Term::Match {
            scrut,
            arms,
            default,
            ..
        } = term
        && default.is_none()
        && let Some(stmts) = compile_native_try_match(
            goenv,
            scrut,
            arms,
            native_ctx,
            join_env,
            pending_joins,
            let_env,
        )
    {
        return stmts;
    }
    if let Some(native_ctx) = native_ctx
        && let Some(target) = all_branches_jump_to_resolved(term)
        && let Some(join_bind) = join_env.get(&target)
        && pending_joins.iter().any(|j| j.id == target)
        && join_forwards_to_native_return(join_bind, join_env, native_ctx)
    {
        return compile_term_direct_to_native_return_ctx(
            goenv, term, &target, join_env, native_ctx, let_env,
        );
    }
    if let Some(target) = all_branches_jump_to_resolved(term)
        && let Some(join_bind) = join_env.get(&target)
        && pending_joins.iter().any(|j| j.id == target)
    {
        let mut stmts = compile_term_jump_to_ctx(
            goenv, term, &target, join_bind, join_env, native_ctx, let_env,
        );
        stmts.extend(compile_block_structured_ctx(
            goenv,
            &join_bind.body,
            join_env,
            native_ctx,
            let_env,
        ));
        return stmts;
    }
    compile_term_leaf_ctx(goenv, term, join_env, native_ctx, let_env)
}

fn compile_term_jump_to_ctx(
    goenv: &GlobalGoEnv,
    term: &anf::Term,
    target: &anf::JoinId,
    join_bind: &anf::JoinBind,
    join_env: &JoinEnv,
    native_ctx: Option<&NativeFnCtx>,
    let_env: &LetEnv,
) -> Vec<goast::Stmt> {
    match term {
        anf::Term::Jump { args, .. } => compile_jump_args(goenv, &join_bind.params, args),
        anf::Term::If {
            cond, then_, else_, ..
        } => {
            let then_stmts = compile_branch_to_join_ctx(
                goenv, then_, target, join_bind, join_env, native_ctx, let_env,
            );
            let else_stmts = compile_branch_to_join_ctx(
                goenv, else_, target, join_bind, join_env, native_ctx, let_env,
            );
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
            native_ctx,
            let_env,
        ),
        _ => compile_term_leaf_ctx(goenv, term, join_env, native_ctx, let_env),
    }
}

fn compile_term_direct_to_native_return_ctx(
    goenv: &GlobalGoEnv,
    term: &anf::Term,
    target: &anf::JoinId,
    join_env: &JoinEnv,
    native_ctx: &NativeFnCtx,
    let_env: &LetEnv,
) -> Vec<goast::Stmt> {
    match term {
        anf::Term::Jump { args, .. } => args
            .first()
            .map(|imm| compile_native_return_from_imm(goenv, imm, native_ctx, let_env))
            .unwrap_or_else(|| vec![panic_stmt("missing native return value")]),
        anf::Term::If {
            cond, then_, else_, ..
        } => {
            let then_stmts = compile_branch_to_native_return_ctx(
                goenv, then_, target, join_env, native_ctx, let_env,
            );
            let else_stmts = compile_branch_to_native_return_ctx(
                goenv, else_, target, join_env, native_ctx, let_env,
            );
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
        } => compile_match_to_native_return_ctx(
            goenv,
            scrut,
            arms,
            default.as_deref(),
            target,
            join_env,
            native_ctx,
            let_env,
        ),
        _ => compile_term_leaf_ctx(goenv, term, join_env, Some(native_ctx), let_env),
    }
}

fn compile_branch_to_native_return_ctx(
    goenv: &GlobalGoEnv,
    block: &anf::Block,
    target: &anf::JoinId,
    join_env: &JoinEnv,
    native_ctx: &NativeFnCtx,
    let_env: &LetEnv,
) -> Vec<goast::Stmt> {
    let mut inner_pending: Vec<&anf::JoinBind> = Vec::new();
    let mut current_env = let_env.clone();
    let mut compiled_binds: Vec<(Option<anf::LocalId>, Vec<goast::Stmt>)> = Vec::new();
    for bind in &block.binds {
        match bind {
            anf::Bind::Let(let_bind) => {
                current_env.insert(let_bind.id.clone(), let_bind.value.clone());
                compiled_binds.push((Some(let_bind.id.clone()), compile_let_bind(goenv, let_bind)));
            }
            anf::Bind::Join(jb) => {
                if native_ctx.return_join == jb.id {
                    continue;
                }
                let mut bind_stmts = Vec::new();
                for (id, ty) in &jb.params {
                    if id.0 != "_" {
                        bind_stmts.push(goast::Stmt::VarDecl {
                            name: go_ident(&id.0),
                            ty: tast_ty_to_go_type(ty),
                            value: None,
                        });
                    }
                }
                inner_pending.push(jb);
                compiled_binds.push((None, bind_stmts));
            }
            anf::Bind::JoinRec(group) => {
                compiled_binds.push((
                    None,
                    compile_joinrec_ctx(goenv, group, join_env, Some(native_ctx), &current_env),
                ));
            }
        }
    }
    let skip_bind = native_try_scrutinee_id(
        goenv,
        &block.term,
        join_env,
        &inner_pending,
        Some(native_ctx),
        &current_env,
    )
    .or_else(|| {
        native_direct_return_bind_id(
            goenv,
            &block.term,
            join_env,
            &inner_pending,
            Some(native_ctx),
            &current_env,
        )
    });
    let mut stmts = Vec::new();
    for (bind_id, bind_stmts) in compiled_binds {
        if let (Some(bind_id), Some(skip_bind)) = (bind_id.as_ref(), skip_bind.as_ref())
            && bind_id == skip_bind
        {
            continue;
        }
        stmts.extend(bind_stmts);
    }

    if let Some(inner_target) = all_branches_jump_to_resolved(&block.term) {
        if inner_target == *target {
            stmts.extend(compile_term_direct_to_native_return_ctx(
                goenv,
                &block.term,
                target,
                join_env,
                native_ctx,
                &current_env,
            ));
            return stmts;
        }
        if let Some(inner_join) = join_env.get(&inner_target)
            && inner_pending.iter().any(|j| j.id == inner_target)
            && join_forwards_to_native_return(inner_join, join_env, native_ctx)
        {
            stmts.extend(compile_term_direct_to_native_return_ctx(
                goenv,
                &block.term,
                &inner_target,
                join_env,
                native_ctx,
                &current_env,
            ));
            return stmts;
        }
    }
    stmts.extend(compile_term_with_continuations_ctx(
        goenv,
        &block.term,
        join_env,
        &inner_pending,
        Some(native_ctx),
        &current_env,
    ));
    stmts
}

#[allow(clippy::too_many_arguments)]
fn compile_match_to_native_return_ctx(
    goenv: &GlobalGoEnv,
    scrut: &anf::ImmExpr,
    arms: &[anf::Arm],
    default: Option<&anf::Block>,
    target: &anf::JoinId,
    join_env: &JoinEnv,
    native_ctx: &NativeFnCtx,
    let_env: &LetEnv,
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
            return compile_branch_to_native_return_ctx(
                goenv, block, target, join_env, native_ctx, let_env,
            );
        }
        return vec![panic_stmt("non-exhaustive match")];
    }

    match &scrut_ty {
        tast::Ty::TEnum { .. } => compile_enum_match_to_native_return_ctx(
            goenv,
            scrut,
            &literal_arms,
            var_arm,
            default,
            target,
            join_env,
            native_ctx,
            let_env,
        ),
        tast::Ty::TApp { ty, .. } if matches!(ty.as_ref(), tast::Ty::TEnum { .. }) => {
            compile_enum_match_to_native_return_ctx(
                goenv,
                scrut,
                &literal_arms,
                var_arm,
                default,
                target,
                join_env,
                native_ctx,
                let_env,
            )
        }
        _ => compile_switch_match_to_native_return_ctx(
            goenv,
            scrut,
            &literal_arms,
            var_arm,
            default,
            target,
            join_env,
            native_ctx,
            let_env,
        ),
    }
}

#[allow(clippy::too_many_arguments)]
fn compile_enum_match_to_native_return_ctx(
    goenv: &GlobalGoEnv,
    scrut: &anf::ImmExpr,
    arms: &[(&anf::ImmExpr, &anf::Block)],
    var_default: Option<(&anf::LocalId, &anf::Block)>,
    default: Option<&anf::Block>,
    target: &anf::JoinId,
    join_env: &JoinEnv,
    native_ctx: &NativeFnCtx,
    let_env: &LetEnv,
) -> Vec<goast::Stmt> {
    if enum_is_tag_only(goenv, &imm_ty(scrut)) {
        return compile_switch_match_to_native_return_ctx(
            goenv,
            scrut,
            arms,
            var_default,
            default,
            target,
            join_env,
            native_ctx,
            let_env,
        );
    }
    let mut cases = Vec::new();
    for (pat, body) in arms {
        let anf::ImmExpr::Tag { index, ty } = pat else {
            panic!("expected tag pattern in enum match, got {:?}", pat);
        };
        let vty = variant_ty_by_index(goenv, ty, *index);
        let stmts =
            compile_branch_to_native_return_ctx(goenv, body, target, join_env, native_ctx, let_env);
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
        stmts.extend(compile_branch_to_native_return_ctx(
            goenv, body, target, join_env, native_ctx, let_env,
        ));
        Some(goast::Block { stmts })
    } else if let Some(def_block) = default {
        Some(goast::Block {
            stmts: compile_branch_to_native_return_ctx(
                goenv, def_block, target, join_env, native_ctx, let_env,
            ),
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
fn compile_switch_match_to_native_return_ctx(
    goenv: &GlobalGoEnv,
    scrut: &anf::ImmExpr,
    arms: &[(&anf::ImmExpr, &anf::Block)],
    var_default: Option<(&anf::LocalId, &anf::Block)>,
    default: Option<&anf::Block>,
    target: &anf::JoinId,
    join_env: &JoinEnv,
    native_ctx: &NativeFnCtx,
    let_env: &LetEnv,
) -> Vec<goast::Stmt> {
    let mut cases = Vec::new();
    for (pat, body) in arms {
        let stmts =
            compile_branch_to_native_return_ctx(goenv, body, target, join_env, native_ctx, let_env);
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
        stmts.extend(compile_branch_to_native_return_ctx(
            goenv, body, target, join_env, native_ctx, let_env,
        ));
        Some(goast::Block { stmts })
    } else if let Some(def_block) = default {
        Some(goast::Block {
            stmts: compile_branch_to_native_return_ctx(
                goenv, def_block, target, join_env, native_ctx, let_env,
            ),
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

fn compile_branch_to_join_ctx(
    goenv: &GlobalGoEnv,
    block: &anf::Block,
    target: &anf::JoinId,
    join_bind: &anf::JoinBind,
    join_env: &JoinEnv,
    native_ctx: Option<&NativeFnCtx>,
    let_env: &LetEnv,
) -> Vec<goast::Stmt> {
    let mut inner_pending: Vec<&anf::JoinBind> = Vec::new();
    let mut current_env = let_env.clone();
    let mut compiled_binds: Vec<(Option<anf::LocalId>, Vec<goast::Stmt>)> = Vec::new();
    for bind in &block.binds {
        match bind {
            anf::Bind::Let(let_bind) => {
                current_env.insert(let_bind.id.clone(), let_bind.value.clone());
                compiled_binds.push((Some(let_bind.id.clone()), compile_let_bind(goenv, let_bind)));
            }
            anf::Bind::Join(jb) => {
                if native_ctx.is_some_and(|ctx| ctx.return_join == jb.id) {
                    continue;
                }
                let mut bind_stmts = Vec::new();
                for (id, ty) in &jb.params {
                    if id.0 != "_" {
                        bind_stmts.push(goast::Stmt::VarDecl {
                            name: go_ident(&id.0),
                            ty: tast_ty_to_go_type(ty),
                            value: None,
                        });
                    }
                }
                inner_pending.push(jb);
                compiled_binds.push((None, bind_stmts));
            }
            anf::Bind::JoinRec(group) => {
                compiled_binds.push((
                    None,
                    compile_joinrec_ctx(goenv, group, join_env, native_ctx, &current_env),
                ));
            }
        }
    }
    let skip_bind = native_try_scrutinee_id(
        goenv,
        &block.term,
        join_env,
        &inner_pending,
        native_ctx,
        &current_env,
    );
    let mut stmts = Vec::new();
    for (bind_id, bind_stmts) in compiled_binds {
        if let (Some(bind_id), Some(skip_bind)) = (bind_id.as_ref(), skip_bind.as_ref())
            && bind_id == skip_bind
        {
            continue;
        }
        stmts.extend(bind_stmts);
    }

    if let Some(inner_target) = all_branches_jump_to_resolved(&block.term) {
        if inner_target == *target {
            stmts.extend(compile_term_jump_to_ctx(
                goenv,
                &block.term,
                target,
                join_bind,
                join_env,
                native_ctx,
                &current_env,
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
                native_ctx,
                &current_env,
            ));
            stmts.extend(compile_branch_to_join_ctx(
                goenv,
                &inner_join.body,
                target,
                join_bind,
                join_env,
                native_ctx,
                &current_env,
            ));
            return stmts;
        }
    }
    stmts.extend(compile_term_with_continuations_ctx(
        goenv,
        &block.term,
        join_env,
        &inner_pending,
        native_ctx,
        &current_env,
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
    native_ctx: Option<&NativeFnCtx>,
    let_env: &LetEnv,
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
            return compile_branch_to_join_ctx(
                goenv, block, target, join_bind, join_env, native_ctx, let_env,
            );
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
            native_ctx,
            let_env,
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
                native_ctx,
                let_env,
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
            native_ctx,
            let_env,
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
    native_ctx: Option<&NativeFnCtx>,
    let_env: &LetEnv,
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
            native_ctx,
            let_env,
        );
    }
    let mut cases = Vec::new();
    for (pat, body) in arms {
        let anf::ImmExpr::Tag { index, ty } = pat else {
            panic!("expected tag pattern in enum match, got {:?}", pat);
        };
        let vty = variant_ty_by_index(goenv, ty, *index);
        let stmts = compile_branch_to_join_ctx(
            goenv, body, target, join_bind, join_env, native_ctx, let_env,
        );
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
            goenv, body, target, join_bind, join_env, native_ctx, let_env,
        ));
        Some(goast::Block { stmts })
    } else if let Some(def_block) = default {
        Some(goast::Block {
            stmts: compile_branch_to_join_ctx(
                goenv, def_block, target, join_bind, join_env, native_ctx, let_env,
            ),
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
    native_ctx: Option<&NativeFnCtx>,
    let_env: &LetEnv,
) -> Vec<goast::Stmt> {
    let mut cases = Vec::new();
    for (pat, body) in arms {
        let stmts = compile_branch_to_join_ctx(
            goenv, body, target, join_bind, join_env, native_ctx, let_env,
        );
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
            goenv, body, target, join_bind, join_env, native_ctx, let_env,
        ));
        Some(goast::Block { stmts })
    } else if let Some(def_block) = default {
        Some(goast::Block {
            stmts: compile_branch_to_join_ctx(
                goenv, def_block, target, join_bind, join_env, native_ctx, let_env,
            ),
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
    native_ctx: Option<&NativeFnCtx>,
    let_env: &LetEnv,
) -> Vec<goast::Stmt> {
    match term {
        anf::Term::Return(imm) => {
            if let Some(native_ctx) = native_ctx {
                return compile_native_return_from_imm(goenv, imm, native_ctx, let_env);
            }
            vec![goast::Stmt::Return {
                expr: Some(compile_imm(goenv, imm)),
            }]
        }
        anf::Term::Jump { target, args, .. } => {
            if let Some(native_ctx) = native_ctx
                && *target == native_ctx.return_join
                && let Some(imm) = args.first()
            {
                return compile_native_return_from_imm(goenv, imm, native_ctx, let_env);
            }
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
                stmts.extend(compile_block_structured_ctx(
                    goenv,
                    &join_bind.body,
                    join_env,
                    native_ctx,
                    let_env,
                ));
                stmts
            } else {
                vec![panic_stmt(&format!("unknown join {:?}", target))]
            }
        }
        anf::Term::If {
            cond, then_, else_, ..
        } => {
            let then_stmts =
                compile_block_structured_ctx(goenv, then_, join_env, native_ctx, let_env);
            let else_stmts =
                compile_block_structured_ctx(goenv, else_, join_env, native_ctx, let_env);
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
        } => compile_match_leaf_ctx(
            goenv,
            scrut,
            arms,
            default.as_deref(),
            join_env,
            native_ctx,
            let_env,
        ),
        anf::Term::Unreachable { .. } => vec![panic_stmt("unreachable")],
    }
}

fn compile_match_leaf_ctx(
    goenv: &GlobalGoEnv,
    scrut: &anf::ImmExpr,
    arms: &[anf::Arm],
    default: Option<&anf::Block>,
    join_env: &JoinEnv,
    native_ctx: Option<&NativeFnCtx>,
    let_env: &LetEnv,
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
            return compile_block_structured_ctx(goenv, block, join_env, native_ctx, let_env);
        }
        return vec![panic_stmt("non-exhaustive match")];
    }

    match &scrut_ty {
        tast::Ty::TEnum { .. } => compile_enum_match_leaf_ctx(
            goenv,
            scrut,
            &literal_arms,
            var_arm,
            default,
            join_env,
            native_ctx,
            let_env,
        ),
        tast::Ty::TApp { ty, .. } if matches!(ty.as_ref(), tast::Ty::TEnum { .. }) => {
            compile_enum_match_leaf_ctx(
                goenv,
                scrut,
                &literal_arms,
                var_arm,
                default,
                join_env,
                native_ctx,
                let_env,
            )
        }
        _ => compile_switch_match_leaf_ctx(
            goenv,
            scrut,
            &literal_arms,
            var_arm,
            default,
            join_env,
            native_ctx,
            let_env,
        ),
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
    native_ctx: Option<&NativeFnCtx>,
    let_env: &LetEnv,
) -> Vec<goast::Stmt> {
    if enum_is_tag_only(goenv, &imm_ty(scrut)) {
        return compile_switch_match_leaf_ctx(
            goenv,
            scrut,
            arms,
            var_default,
            default,
            join_env,
            native_ctx,
            let_env,
        );
    }
    let mut cases = Vec::new();
    for (pat, body) in arms {
        let anf::ImmExpr::Tag { index, ty } = pat else {
            panic!("expected tag pattern in enum match, got {:?}", pat);
        };
        let vty = variant_ty_by_index(goenv, ty, *index);
        let stmts = compile_block_structured_ctx(goenv, body, join_env, native_ctx, let_env);
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
        stmts.extend(compile_block_structured_ctx(
            goenv, body, join_env, native_ctx, let_env,
        ));
        Some(goast::Block { stmts })
    } else if let Some(def_block) = default {
        Some(goast::Block {
            stmts: compile_block_structured_ctx(goenv, def_block, join_env, native_ctx, let_env),
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
    native_ctx: Option<&NativeFnCtx>,
    let_env: &LetEnv,
) -> Vec<goast::Stmt> {
    let mut cases = Vec::new();
    for (pat, body) in arms {
        let stmts = compile_block_structured_ctx(goenv, body, join_env, native_ctx, let_env);
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
        stmts.extend(compile_block_structured_ctx(
            goenv, body, join_env, native_ctx, let_env,
        ));
        Some(goast::Block { stmts })
    } else if let Some(def_block) = default {
        Some(goast::Block {
            stmts: compile_block_structured_ctx(goenv, def_block, join_env, native_ctx, let_env),
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

fn compile_joinrec_ctx(
    goenv: &GlobalGoEnv,
    group: &[anf::JoinBind],
    join_env: &JoinEnv,
    native_ctx: Option<&NativeFnCtx>,
    let_env: &LetEnv,
) -> Vec<goast::Stmt> {
    if group.len() == 1
        && let Some(wl) = is_while_loop(&group[0])
    {
        return compile_while_loop_ctx(goenv, &wl, join_env, native_ctx, let_env);
    }
    Vec::new()
}

fn compile_while_loop_ctx(
    goenv: &GlobalGoEnv,
    wl: &WhileLoop,
    join_env: &JoinEnv,
    native_ctx: Option<&NativeFnCtx>,
    let_env: &LetEnv,
) -> Vec<goast::Stmt> {
    let loop_label = format!("Loop_{}", go_ident(&wl.loop_id.0));

    let mut inner_env = join_env.clone();
    inner_env.continue_targets.insert(wl.loop_id.clone());
    inner_env.break_targets.insert(wl.exit_id.clone());
    inner_env
        .break_labels
        .insert(wl.exit_id.clone(), loop_label.clone());
    let loop_body_stmts =
        compile_block_structured_ctx(goenv, &wl.body, &inner_env, native_ctx, let_env);

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

    stmts.extend(compile_block_structured_ctx(
        goenv, &wl.after, join_env, native_ctx, let_env,
    ));
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
    used_names.extend(
        goenv
            .genv
            .type_env
            .extern_types
            .keys()
            .map(|extern_name| go_user_type_name(extern_name)),
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

fn boxed_fn_call_args(params: &[(anf::LocalId, tast::Ty)]) -> Vec<goast::Expr> {
    params
        .iter()
        .map(|(id, ty)| goast::Expr::Var {
            name: go_ident(&id.0),
            ty: tast_ty_to_go_type(ty),
        })
        .collect()
}

fn compile_boxed_fn_from_native(
    goenv: &GlobalGoEnv,
    f: &anf::Fn,
    native_ctx: &NativeFnCtx,
    entry_wrapper_name: &str,
) -> goast::Fn {
    let params = boxed_fn_params(&f.params);
    let helper_ret_ty = native_helper_ret_ty(&native_ctx.mode);
    let helper_name = native_helper_fn_name(&f.name);
    let call_expr = goast::Expr::Call {
        func: Box::new(goast::Expr::Var {
            name: helper_name,
            ty: goty::GoType::TFunc {
                params: params.iter().map(|(_, ty)| ty.clone()).collect(),
                ret_ty: Box::new(helper_ret_ty.clone()),
            },
        }),
        args: boxed_fn_call_args(&f.params),
        ty: helper_ret_ty.clone(),
    };

    let mut stmts = Vec::new();
    match &native_ctx.mode {
        NativeReturnMode::Result {
            ok_index,
            err_index,
            ok_ty,
            err_ty,
        } => {
            let ok_go_tys = unwrap_tuple_go_tys(ok_ty);
            let value_names = ok_go_tys
                .iter()
                .enumerate()
                .map(|(index, _)| format!("native_value_{}", index))
                .collect::<Vec<_>>();
            let err_go_ty = tast_ty_to_go_type(err_ty);
            for (value_name, ok_go_ty) in value_names.iter().zip(ok_go_tys.iter()) {
                stmts.push(goast::Stmt::VarDecl {
                    name: value_name.clone(),
                    ty: ok_go_ty.clone(),
                    value: None,
                });
            }
            stmts.push(goast::Stmt::VarDecl {
                name: "native_err".to_string(),
                ty: err_go_ty.clone(),
                value: None,
            });
            let mut assign_names = value_names.clone();
            assign_names.push("native_err".to_string());
            stmts.push(goast::Stmt::MultiAssignment {
                names: assign_names,
                value: call_expr,
            });
            stmts.push(goast::Stmt::If {
                cond: goast::Expr::BinaryOp {
                    op: goast::GoBinaryOp::NotEq,
                    lhs: Box::new(goast::Expr::Var {
                        name: "native_err".to_string(),
                        ty: err_go_ty.clone(),
                    }),
                    rhs: Box::new(goast::Expr::Nil {
                        ty: err_go_ty.clone(),
                    }),
                    ty: goty::GoType::TBool,
                },
                then: goast::Block {
                    stmts: vec![goast::Stmt::Return {
                        expr: Some(enum_variant_expr(
                            goenv,
                            &f.ret_ty,
                            *err_index,
                            Some(goast::Expr::Var {
                                name: "native_err".to_string(),
                                ty: err_go_ty,
                            }),
                        )),
                    }],
                },
                else_: None,
            });
            stmts.push(goast::Stmt::Return {
                expr: Some(enum_variant_expr(
                    goenv,
                    &f.ret_ty,
                    *ok_index,
                    Some(tuple_pack_expr(ok_ty, &value_names)),
                )),
            });
        }
        NativeReturnMode::Option {
            none_index,
            some_index,
            some_ty,
        } => {
            let some_go_tys = unwrap_tuple_go_tys(some_ty);
            let value_names = some_go_tys
                .iter()
                .enumerate()
                .map(|(index, _)| format!("native_value_{}", index))
                .collect::<Vec<_>>();
            for (value_name, some_go_ty) in value_names.iter().zip(some_go_tys.iter()) {
                stmts.push(goast::Stmt::VarDecl {
                    name: value_name.clone(),
                    ty: some_go_ty.clone(),
                    value: None,
                });
            }
            stmts.push(goast::Stmt::VarDecl {
                name: "native_ok".to_string(),
                ty: goty::GoType::TBool,
                value: None,
            });
            let mut assign_names = value_names.clone();
            assign_names.push("native_ok".to_string());
            stmts.push(goast::Stmt::MultiAssignment {
                names: assign_names,
                value: call_expr,
            });
            stmts.push(goast::Stmt::If {
                cond: goast::Expr::Var {
                    name: "native_ok".to_string(),
                    ty: goty::GoType::TBool,
                },
                then: goast::Block {
                    stmts: vec![goast::Stmt::Return {
                        expr: Some(enum_variant_expr(
                            goenv,
                            &f.ret_ty,
                            *some_index,
                            Some(tuple_pack_expr(some_ty, &value_names)),
                        )),
                    }],
                },
                else_: None,
            });
            stmts.push(goast::Stmt::Return {
                expr: Some(enum_variant_expr(goenv, &f.ret_ty, *none_index, None)),
            });
        }
    }

    goast::Fn {
        name: patched_fn_name(goenv, &f.name, entry_wrapper_name),
        params,
        ret_ty: Some(tast_ty_to_go_type(&f.ret_ty)),
        body: goast::Block { stmts },
    }
}

fn compile_native_fn(goenv: &GlobalGoEnv, f: &anf::Fn, native_ctx: &NativeFnCtx) -> goast::Fn {
    let params = boxed_fn_params(&f.params);
    let join_env = build_join_env(&f.body);
    let stmts =
        compile_block_structured_ctx(goenv, &f.body, &join_env, Some(native_ctx), &HashMap::new());

    goast::Fn {
        name: native_helper_fn_name(&f.name),
        params,
        ret_ty: Some(native_helper_ret_ty(&native_ctx.mode)),
        body: goast::Block { stmts },
    }
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

    if !goenv.genv.value_env.extern_funcs.is_empty() || !goenv.genv.type_env.extern_types.is_empty()
    {
        let mut existing_imports: IndexSet<String> = IndexSet::new();
        for item in &all {
            if let goast::Item::Import(import_decl) = item {
                for spec in &import_decl.specs {
                    existing_imports.insert(spec.path.clone());
                }
            }
        }

        let mut extra_specs = Vec::new();
        for extern_fn in goenv.genv.value_env.extern_funcs.values() {
            if !extern_fn.package_path.is_empty()
                && existing_imports.insert(extern_fn.package_path.clone())
            {
                extra_specs.push(goast::ImportSpec {
                    alias: go_import_alias(&goenv, &extern_fn.package_path),
                    path: extern_fn.package_path.clone(),
                });
            }
        }
        for extern_ty in goenv.genv.type_env.extern_types.values() {
            if let Some(package_path) = &extern_ty.package_path
                && !package_path.is_empty()
                && existing_imports.insert(package_path.clone())
            {
                extra_specs.push(goast::ImportSpec {
                    alias: go_import_alias(&goenv, package_path),
                    path: package_path.clone(),
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
    toplevels.extend(gen_extern_wrapper_fns(&goenv));
    toplevels.extend(gen_extern_bridge_fns(&goenv));
    for item in file.toplevels {
        if let Some(native_ctx) = native_fn_ctx(&goenv, &item) {
            toplevels.push(goast::Item::Fn(compile_native_fn(
                &goenv,
                &item,
                &native_ctx,
            )));
            toplevels.push(goast::Item::Fn(compile_boxed_fn_from_native(
                &goenv,
                &item,
                &native_ctx,
                &entry_wrapper_name,
            )));
        } else {
            toplevels.push(goast::Item::Fn(compile_fn(
                &goenv,
                gensym,
                &item,
                &entry_wrapper_name,
            )));
        }
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

    for (name, ext) in goenv.genv.type_env.extern_types.iter() {
        let alias_name = go_user_type_name(name);
        if go_type_alias_name_is_reserved(goenv, &alias_name) {
            continue;
        }
        if let Some(package_path) = &ext.package_path {
            let go_ty = if package_path.is_empty() {
                goty::GoType::TName {
                    name: rewrite_raw_go_symbol_import_aliases(goenv, &ext.go_name),
                }
            } else {
                let alias = go_package_alias(goenv, package_path);
                goty::GoType::TName {
                    name: rewrite_go_package_alias(
                        &qualify_go_type_expr(&alias, &ext.go_name),
                        &go_package_default_alias(package_path),
                        &alias,
                    ),
                }
            };
            defs.push(goast::Item::TypeAlias(goast::TypeAlias {
                name: alias_name,
                ty: go_ty,
            }));
        }
    }
    defs
}
