use crate::tast;

pub fn encode_ty(ty: &tast::Ty) -> String {
    match ty {
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
        tast::Ty::TVar(_v) => "Var".to_string(),
        tast::Ty::TParam { name } => format!("TParam_{}", name),
        tast::Ty::TTuple { typs } => {
            let inner = typs.iter().map(encode_ty).collect::<Vec<_>>().join("_");
            format!("Tuple_{}", inner)
        }
        tast::Ty::TEnum { name } | tast::Ty::TStruct { name } => name.clone(),
        tast::Ty::TDyn { trait_name } => format!("Dyn_{}", trait_name),
        tast::Ty::TApp { ty, args } => {
            let base = ty.get_constr_name_unsafe();
            if args.is_empty() {
                base
            } else {
                let inner = args.iter().map(encode_ty).collect::<Vec<_>>().join("_");
                format!("{}_{}", base, inner)
            }
        }
        tast::Ty::TArray { len, elem } => format!("Array_{}_{}", len, encode_ty(elem)),
        tast::Ty::TSlice { elem } => format!("Slice_{}", encode_ty(elem)),
        tast::Ty::TVec { elem } => format!("Vec_{}", encode_ty(elem)),
        tast::Ty::TRef { elem } => format!("Ref_{}", encode_ty(elem)),
        tast::Ty::THashMap { key, value } => {
            format!("HashMap_{}_{}", encode_ty(key), encode_ty(value))
        }
        tast::Ty::TFunc { params, ret_ty } => {
            let p = params.iter().map(encode_ty).collect::<Vec<_>>().join("_");
            let r = encode_ty(ret_ty);
            format!("Fn_{}_to_{}", p, r)
        }
    }
}

pub fn go_ident(name: &str) -> String {
    if is_valid_go_ident(name) && !is_go_keyword(name) && !is_go_predeclared_identifier(name) {
        if name.starts_with("_goml_") {
            return format!("_goml_user_{}", name);
        }
        return name.to_string();
    }
    let mut out = String::from("_goml_");
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() {
            out.push(ch);
            continue;
        }
        if ch == '#' {
            out.push('_');
            continue;
        }
        if ch == '_' {
            out.push('_');
            continue;
        }
        out.push_str("_x");
        let mut buf = [0u8; 4];
        for b in ch.encode_utf8(&mut buf).as_bytes() {
            use std::fmt::Write;
            write!(&mut out, "{:02x}", b).unwrap();
        }
        out.push('_');
    }
    out
}

pub fn go_user_type_name(name: &str) -> String {
    let ident = go_ident(name);
    if is_generated_go_type_name(&ident) {
        format!("_goml_user_{}", ident)
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
