use crate::tast;
use ::ast::ast;

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
        tast::Ty::TVar(_v) => "Var".to_string(),
        tast::Ty::TParam { name } => format!("TParam_{}", name),
        tast::Ty::TTuple { typs } => {
            let inner = typs.iter().map(encode_ty).collect::<Vec<_>>().join("_");
            format!("Tuple_{}", inner)
        }
        tast::Ty::TEnum { name } | tast::Ty::TStruct { name } => name.clone(),
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
        tast::Ty::TRef { elem } => format!("Ref_{}", encode_ty(elem)),
        tast::Ty::TFunc { params, ret_ty } => {
            let p = params.iter().map(encode_ty).collect::<Vec<_>>().join("_");
            let r = encode_ty(ret_ty);
            format!("Fn_{}_to_{}", p, r)
        }
    }
}

pub fn decode_ty(encoded: &str) -> Result<tast::Ty, String> {
    if encoded.is_empty() {
        return Err("empty type encoding".to_string());
    }
    let tokens: Vec<&str> = encoded.split('_').collect();
    decode_range(&tokens, 0, tokens.len())
}

fn decode_range(tokens: &[&str], start: usize, end: usize) -> Result<tast::Ty, String> {
    if start >= end {
        return Err("invalid type encoding range".to_string());
    }
    let head = tokens[start];
    match head {
        "unit" => {
            if start + 1 == end {
                Ok(tast::Ty::TUnit)
            } else {
                Err("unexpected trailing tokens after unit".to_string())
            }
        }
        "bool" => {
            if start + 1 == end {
                Ok(tast::Ty::TBool)
            } else {
                Err("unexpected trailing tokens after bool".to_string())
            }
        }
        "int8" => {
            if start + 1 == end {
                Ok(tast::Ty::TInt8)
            } else {
                Err("unexpected trailing tokens after int8".to_string())
            }
        }
        "int16" => {
            if start + 1 == end {
                Ok(tast::Ty::TInt16)
            } else {
                Err("unexpected trailing tokens after int16".to_string())
            }
        }
        "int32" => {
            if start + 1 == end {
                Ok(tast::Ty::TInt32)
            } else {
                Err("unexpected trailing tokens after int32".to_string())
            }
        }
        "int64" => {
            if start + 1 == end {
                Ok(tast::Ty::TInt64)
            } else {
                Err("unexpected trailing tokens after int64".to_string())
            }
        }
        "uint8" => {
            if start + 1 == end {
                Ok(tast::Ty::TUint8)
            } else {
                Err("unexpected trailing tokens after uint8".to_string())
            }
        }
        "uint16" => {
            if start + 1 == end {
                Ok(tast::Ty::TUint16)
            } else {
                Err("unexpected trailing tokens after uint16".to_string())
            }
        }
        "uint32" => {
            if start + 1 == end {
                Ok(tast::Ty::TUint32)
            } else {
                Err("unexpected trailing tokens after uint32".to_string())
            }
        }
        "uint64" => {
            if start + 1 == end {
                Ok(tast::Ty::TUint64)
            } else {
                Err("unexpected trailing tokens after uint64".to_string())
            }
        }
        "float32" => {
            if start + 1 == end {
                Ok(tast::Ty::TFloat32)
            } else {
                Err("unexpected trailing tokens after float32".to_string())
            }
        }
        "float64" => {
            if start + 1 == end {
                Ok(tast::Ty::TFloat64)
            } else {
                Err("unexpected trailing tokens after float64".to_string())
            }
        }
        "string" => {
            if start + 1 == end {
                Ok(tast::Ty::TString)
            } else {
                Err("unexpected trailing tokens after string".to_string())
            }
        }
        "Var" => Err("type variables cannot be decoded".to_string()),
        "TParam" => {
            if start + 2 == end {
                Ok(tast::Ty::TParam {
                    name: tokens[start + 1].to_string(),
                })
            } else {
                Err("malformed type parameter encoding".to_string())
            }
        }
        "Tuple" => {
            let elems = decode_list(tokens, start + 1, end)?;
            if elems.is_empty() {
                Err("tuple encoding must have at least one element".to_string())
            } else {
                Ok(tast::Ty::TTuple { typs: elems })
            }
        }
        "Array" => {
            if start + 2 > end {
                return Err("array encoding missing length".to_string());
            }
            let len_token = tokens
                .get(start + 1)
                .ok_or_else(|| "array encoding missing length".to_string())?;
            let len = len_token
                .parse::<usize>()
                .map_err(|_| format!("invalid array length: {}", len_token))?;
            if start + 2 >= end {
                return Err("array encoding missing element".to_string());
            }
            let elem = decode_range(tokens, start + 2, end)?;
            Ok(tast::Ty::TArray {
                len,
                elem: Box::new(elem),
            })
        }
        "Ref" => {
            if start + 1 >= end {
                return Err("reference encoding missing element".to_string());
            }
            let elem = decode_range(tokens, start + 1, end)?;
            Ok(tast::Ty::TRef {
                elem: Box::new(elem),
            })
        }
        "Fn" => {
            let to_pos = tokens[start + 1..end]
                .iter()
                .position(|token| *token == "to")
                .map(|offset| start + 1 + offset)
                .ok_or_else(|| "function encoding missing to delimiter".to_string())?;
            let params = decode_params(tokens, start + 1, to_pos)?;
            let ret = decode_range(tokens, to_pos + 1, end)?;
            Ok(tast::Ty::TFunc {
                params,
                ret_ty: Box::new(ret),
            })
        }
        _ => {
            if start + 1 == end {
                // Unknown type constructor - default to TStruct since it's more common
                // The actual distinction will be made based on context
                Ok(tast::Ty::TStruct {
                    name: head.to_string(),
                })
            } else {
                let args = decode_list(tokens, start + 1, end)?;
                Ok(tast::Ty::TApp {
                    ty: Box::new(tast::Ty::TStruct {
                        name: head.to_string(),
                    }),
                    args,
                })
            }
        }
    }
}

fn decode_params(tokens: &[&str], start: usize, end: usize) -> Result<Vec<tast::Ty>, String> {
    if start >= end {
        return Ok(vec![]);
    }
    if start + 1 == end && tokens[start].is_empty() {
        return Ok(vec![]);
    }
    decode_list(tokens, start, end)
}

fn decode_list(tokens: &[&str], start: usize, end: usize) -> Result<Vec<tast::Ty>, String> {
    if start >= end {
        return Ok(vec![]);
    }
    let mut items = Vec::new();
    let mut cur = start;
    while cur < end {
        if tokens[cur].is_empty() {
            return Err("unexpected empty token in type encoding".to_string());
        }
        let (ty, next) = decode_next(tokens, cur, end)?;
        items.push(ty);
        cur = next;
    }
    Ok(items)
}

fn decode_next(tokens: &[&str], start: usize, end: usize) -> Result<(tast::Ty, usize), String> {
    for mid in (start + 1..=end).rev() {
        if let Ok(ty) = decode_range(tokens, start, mid) {
            return Ok((ty, mid));
        }
    }
    Err("failed to decode type component".to_string())
}

pub fn mangle_impl_name(trait_name: &ast::Ident, for_ty: &tast::Ty, method_name: &str) -> String {
    let for_ty_str = encode_ty(for_ty);
    format!("impl_{}_{}_{}", trait_name.0, for_ty_str, method_name)
}

pub fn mangle_inherent_name(for_ty: &tast::Ty, method_name: &str) -> String {
    let for_ty_str = encode_ty(for_ty);
    format!("impl_inherent_{}_{}", for_ty_str, method_name)
}
