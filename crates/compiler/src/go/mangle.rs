use crate::tast;
use sha2::{Digest, Sha256};

pub const MAX_GO_IDENT_LEN: usize = 80;
const HASH_BYTES: usize = 16;

pub fn encode_ty(ty: &tast::Ty) -> String {
    match ty {
        tast::Ty::TUnit => "unit".to_string(),
        tast::Ty::TNever => "never".to_string(),
        tast::Ty::TBool => "bool".to_string(),
        tast::Ty::TInt => "int".to_string(),
        tast::Ty::TInt8 => "int8".to_string(),
        tast::Ty::TInt16 => "int16".to_string(),
        tast::Ty::TInt32 => "int32".to_string(),
        tast::Ty::TInt64 => "int64".to_string(),
        tast::Ty::TUint => "uint".to_string(),
        tast::Ty::TUint8 => "uint8".to_string(),
        tast::Ty::TUint16 => "uint16".to_string(),
        tast::Ty::TUint32 => "uint32".to_string(),
        tast::Ty::TUint64 => "uint64".to_string(),
        tast::Ty::TFloat32 => "float32".to_string(),
        tast::Ty::TFloat64 => "float64".to_string(),
        tast::Ty::TString => "string".to_string(),
        tast::Ty::TChar => "char".to_string(),
        tast::Ty::TVar(_v) => "Var".to_string(),
        tast::Ty::TParam { name } => format!("TParam_{}", name),
        tast::Ty::TProjection { for_ty, name, .. } => {
            format!("Projection_{}_{}", encode_ty_part(for_ty), name.0)
        }
        tast::Ty::TTuple { typs } => {
            format!("Tuple{}_{}", typs.len(), encode_ty_parts(typs))
        }
        tast::Ty::TEnum { name } | tast::Ty::TStruct { name } => name.clone(),
        tast::Ty::TDyn { trait_name } => format!("Dyn_{}", trait_name),
        tast::Ty::TApp { ty, args } => {
            let base = ty.get_constr_name_unsafe();
            if args.is_empty() {
                base
            } else {
                format!("App_{}__{}", encode_name_part(&base), encode_ty_parts(args))
            }
        }
        tast::Ty::TArray { len, elem } => format!("Array_{}_{}", len, encode_ty_part(elem)),
        tast::Ty::TSlice { elem } => format!("Slice_{}", encode_ty_part(elem)),
        tast::Ty::TVec { elem } => format!("Vec_{}", encode_ty_part(elem)),
        tast::Ty::TRef { elem } => format!("Ref_{}", encode_ty_part(elem)),
        tast::Ty::TChannel { elem } => format!("Channel_{}", encode_ty_part(elem)),
        tast::Ty::THashMap { key, value } => {
            format!("HashMap_{}_{}", encode_ty_part(key), encode_ty_part(value))
        }
        tast::Ty::TFunc { params, ret_ty } => {
            format!(
                "Fn{}_{}_to_{}",
                params.len(),
                encode_ty_parts(params),
                encode_ty_part(ret_ty)
            )
        }
    }
}

fn encode_ty_part(ty: &tast::Ty) -> String {
    let encoded = encode_ty(ty);
    encode_name_part(&encoded)
}

fn encode_ty_parts(tys: &[tast::Ty]) -> String {
    tys.iter().map(encode_ty_part).collect::<Vec<_>>().join("_")
}

fn encode_name_part(name: &str) -> String {
    format!("{}{}", name.len(), name)
}

pub fn go_ident(name: &str) -> String {
    go_ident_impl(name, true)
}

pub fn go_generated_ident(name: &str) -> String {
    go_ident_impl(name, false)
}

pub fn go_dyn_struct_name(trait_name: &str) -> String {
    let name = if trait_name.ends_with("_vtable") {
        format!("_goml_dyn_object_{}", trait_name)
    } else {
        format!("dyn__{}", trait_name)
    };
    go_generated_ident(&name)
}

pub fn go_hashed_ident(kind: &str, name: &str) -> String {
    let prefix = format!("_goml_{}_", kind);
    let digest = stable_hash(&format!("{}\0{}", kind, name));
    let suffix = format!("_h{}", digest);
    let budget = MAX_GO_IDENT_LEN - prefix.len() - suffix.len();
    let hint = bounded_hint(&encode_name(name), budget);
    format!("{}{}{}", prefix, hint, suffix)
}

fn go_ident_impl(name: &str, protect_generated: bool) -> String {
    if is_valid_go_ident(name) && !is_go_keyword(name) && !is_go_predeclared_identifier(name) {
        if protect_generated && is_generated_go_ident(name) {
            return compact_ident(&format!("_goml_user_{}", name));
        }
        return compact_ident(name);
    }
    compact_ident(&format!("_goml_m_{}", encode_name(name)))
}

fn encode_name(name: &str) -> String {
    let mut out = String::new();
    let mut chars = name.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch.is_ascii_alphanumeric() {
            out.push(ch);
            continue;
        }
        if ch == ':' && chars.peek() == Some(&':') {
            chars.next();
            out.push_str("_p_");
            continue;
        }
        let token = match ch {
            '_' => Some("__"),
            '#' => Some("_i_"),
            '[' => Some("_l_"),
            ']' => Some("_r_"),
            '(' => Some("_o_"),
            ')' => Some("_q_"),
            '{' => Some("_b_"),
            '}' => Some("_e_"),
            ',' => Some("_c_"),
            ':' => Some("_k_"),
            '$' => Some("_d_"),
            '-' => Some("_m_"),
            '.' => Some("_t_"),
            '/' => Some("_f_"),
            '*' => Some("_a_"),
            '&' => Some("_n_"),
            '+' => Some("_u_"),
            '<' => Some("_v_"),
            '>' => Some("_z_"),
            _ => None,
        };
        if let Some(token) = token {
            out.push_str(token);
            continue;
        }
        out.push_str("_x");
        let mut buf = [0u8; 4];
        for byte in ch.encode_utf8(&mut buf).as_bytes() {
            use std::fmt::Write;
            write!(&mut out, "{:02x}", byte).unwrap();
        }
        out.push('_');
    }
    out
}

fn compact_ident(candidate: &str) -> String {
    if candidate.len() <= MAX_GO_IDENT_LEN {
        return candidate.to_string();
    }
    let digest = stable_hash(candidate);
    let marker = format!("_h{}_", digest);
    let budget = MAX_GO_IDENT_LEN - marker.len();
    let head_len = budget * 2 / 3;
    let tail_len = budget - head_len;
    format!(
        "{}{}{}",
        &candidate[..head_len],
        marker,
        &candidate[candidate.len() - tail_len..]
    )
}

fn bounded_hint(hint: &str, budget: usize) -> String {
    if hint.len() <= budget {
        return hint.to_string();
    }
    let separator = "_";
    let content_budget = budget - separator.len();
    let head_len = content_budget * 2 / 3;
    let tail_len = content_budget - head_len;
    format!(
        "{}{}{}",
        &hint[..head_len],
        separator,
        &hint[hint.len() - tail_len..]
    )
}

fn stable_hash(value: &str) -> String {
    let digest = Sha256::digest(value.as_bytes());
    hex::encode(&digest[..HASH_BYTES])
}

fn is_generated_go_ident(name: &str) -> bool {
    name.starts_with("_goml_")
        || name.starts_with("dyn__")
        || has_generated_helper_prefix(name)
        || (name.starts_with("ref_") && name.ends_with("_x"))
        || (name.starts_with("hashmap_") && (name.ends_with("_x") || name.ends_with("_x_entry")))
}

fn has_generated_helper_prefix(name: &str) -> bool {
    [
        "array_get__",
        "array_set__",
        "vec_new__",
        "vec_push__",
        "vec_get__",
        "vec_set__",
        "vec_len__",
        "ref__",
        "ref_get__",
        "ref_set__",
        "ptr_eq__",
        "ptr_hash__",
        "hashmap_new__",
        "hashmap_len__",
        "hashmap_contains__",
        "hashmap_lookup__",
        "hashmap_get__",
        "hashmap_set__",
        "hashmap_remove__",
        "missing__",
    ]
    .iter()
    .any(|prefix| name.starts_with(prefix))
}

pub fn go_user_type_name(name: &str) -> String {
    let ident = go_ident(name);
    if is_generated_go_type_name(&ident) || is_generated_go_value_name(&ident) {
        go_generated_ident(&format!("_goml_user_{}", ident))
    } else {
        ident
    }
}

fn is_generated_go_type_name(name: &str) -> bool {
    has_len_prefixed_type_name(name, "Tuple")
        || has_len_prefixed_type_name(name, "Array")
        || name.starts_with("Slice_")
        || name.starts_with("Vec_")
        || name.starts_with("Ptr_")
        || name.starts_with("HashMap_")
        || name.starts_with("TFunc")
}

fn has_len_prefixed_type_name(name: &str, prefix: &str) -> bool {
    let Some(rest) = name.strip_prefix(prefix) else {
        return false;
    };
    let digit_count = rest.chars().take_while(|ch| ch.is_ascii_digit()).count();
    if digit_count == 0 {
        return false;
    }
    rest[digit_count..].is_empty() || rest[digit_count..].starts_with('_')
}

fn is_generated_go_value_name(name: &str) -> bool {
    matches!(name, "init" | "main")
}

fn is_valid_go_ident(s: &str) -> bool {
    let bytes = s.as_bytes();
    let Some((&first, rest)) = bytes.split_first() else {
        return false;
    };
    if !(first.is_ascii_alphabetic() || first == b'_') {
        return false;
    }
    rest.iter().all(|b| b.is_ascii_alphanumeric() || *b == b'_')
}

fn is_go_keyword(s: &str) -> bool {
    matches!(
        s,
        "break"
            | "default"
            | "func"
            | "interface"
            | "select"
            | "case"
            | "defer"
            | "go"
            | "map"
            | "struct"
            | "chan"
            | "else"
            | "goto"
            | "package"
            | "switch"
            | "const"
            | "fallthrough"
            | "if"
            | "range"
            | "type"
            | "continue"
            | "for"
            | "import"
            | "return"
            | "var"
    )
}

fn is_go_predeclared_identifier(s: &str) -> bool {
    matches!(
        s,
        "any"
            | "append"
            | "cap"
            | "clear"
            | "close"
            | "comparable"
            | "complex"
            | "copy"
            | "delete"
            | "error"
            | "false"
            | "imag"
            | "iota"
            | "len"
            | "make"
            | "max"
            | "min"
            | "new"
            | "nil"
            | "panic"
            | "print"
            | "println"
            | "real"
            | "recover"
            | "true"
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn valid_name_stays_readable() {
        assert_eq!(go_ident("read_file"), "read_file");
    }

    #[test]
    fn qualified_name_uses_compact_escapes() {
        assert_eq!(
            go_ident("alice::project::utils::message"),
            "_goml_m_alice_p_project_p_utils_p_message"
        );
    }

    #[test]
    fn compact_escapes_do_not_collide_with_user_text() {
        assert_ne!(go_ident("alice::utils"), go_ident("alice_p_utils"));
        assert_ne!(go_ident("trait#method"), go_ident("trait_i_method"));
        assert_ne!(go_ident("left_right?"), go_ident("left__right?"));
    }

    #[test]
    fn long_names_are_bounded_and_deterministic() {
        let prefix = "deeply_nested_generic_closure_".repeat(8);
        let first = go_ident(&format!("{prefix}::first"));
        let second = go_ident(&format!("{prefix}::second"));

        assert_eq!(first.len(), MAX_GO_IDENT_LEN);
        assert_eq!(first, go_ident(&format!("{prefix}::first")));
        assert_ne!(first, second);
        assert!(is_valid_go_ident(&first));
    }

    #[test]
    fn generated_namespace_is_protected_from_user_names() {
        assert_eq!(go_ident("missing__int32"), "_goml_user_missing__int32");
        assert_eq!(go_generated_ident("missing__int32"), "missing__int32");
        assert_eq!(
            go_ident("vec_new__Vec_5int32"),
            "_goml_user_vec_new__Vec_5int32"
        );
        assert_eq!(
            go_ident("vec_push__Vec_5int32"),
            "_goml_user_vec_push__Vec_5int32"
        );
        assert_eq!(
            go_ident("vec_get__Vec_5int32"),
            "_goml_user_vec_get__Vec_5int32"
        );
        assert_eq!(
            go_ident("vec_len__Vec_5int32"),
            "_goml_user_vec_len__Vec_5int32"
        );
    }

    #[test]
    fn hashed_names_keep_a_readable_hint() {
        let raw = "inherent#closure_env_make_pairer#apply".repeat(5);
        let first = go_hashed_ident("fn", &raw);
        let second = go_hashed_ident("fn", &format!("{raw}x"));

        assert_eq!(first.len(), MAX_GO_IDENT_LEN);
        assert!(first.starts_with("_goml_fn_inherent_i_closure"));
        assert_ne!(first, second);
    }
}
