use ::ast::ast::{
    self, Arm, Attribute, EnumDef, Expr, Ident, ImplBlock, Item, Pat, Path, StructDef,
};
use diagnostics::{Diagnostic, Diagnostics, Severity, Stage};
use parser::syntax::MySyntaxNodePtr;

const DERIVE_STAGE: &str = "derive";
const TO_STRING_TRAIT: &str = "ToString";
const TO_STRING_FN: &str = "to_string";
const TO_JSON_TRAIT: &str = "ToJson";
const TO_JSON_FN: &str = "to_json";
const STRING_ADD_FN: &str = "string_add";
const SELF_PARAM_NAME: &str = "self";

pub fn expand(ast: ast::File) -> Result<ast::File, Diagnostics> {
    let mut diagnostics = Diagnostics::new();
    let mut toplevels = Vec::with_capacity(ast.toplevels.len());

    for item in ast.toplevels.into_iter() {
        let mut derived_impls = Vec::new();
        match &item {
            Item::StructDef(struct_def) => {
                if let Some(attr_ptr) = find_derive_attr(&struct_def.attrs, TO_STRING_TRAIT) {
                    match derive_struct_tostring(struct_def, &attr_ptr) {
                        Ok(impl_block) => derived_impls.push(impl_block),
                        Err(diag) => diagnostics.push(diag),
                    }
                }
                if let Some(attr_ptr) = find_derive_attr(&struct_def.attrs, TO_JSON_TRAIT) {
                    match derive_struct_tojson(struct_def, &attr_ptr) {
                        Ok(impl_block) => derived_impls.push(impl_block),
                        Err(diag) => diagnostics.push(diag),
                    }
                }
            }
            Item::EnumDef(enum_def) => {
                if let Some(attr_ptr) = find_derive_attr(&enum_def.attrs, TO_STRING_TRAIT) {
                    match derive_enum_tostring(enum_def, &attr_ptr) {
                        Ok(impl_block) => derived_impls.push(impl_block),
                        Err(diag) => diagnostics.push(diag),
                    }
                }
                if let Some(attr_ptr) = find_derive_attr(&enum_def.attrs, TO_JSON_TRAIT) {
                    match derive_enum_tojson(enum_def, &attr_ptr) {
                        Ok(impl_block) => derived_impls.push(impl_block),
                        Err(diag) => diagnostics.push(diag),
                    }
                }
            }
            _ => {}
        }

        toplevels.push(item);
        for impl_block in derived_impls {
            toplevels.push(Item::ImplBlock(impl_block));
        }
    }

    if diagnostics.has_errors() {
        Err(diagnostics)
    } else {
        Ok(ast::File { toplevels })
    }
}

fn find_derive_attr(attrs: &[Attribute], trait_name: &str) -> Option<MySyntaxNodePtr> {
    attrs.iter().find_map(|attr| {
        parse_derive_targets(attr).and_then(|targets| {
            if targets.iter().any(|target| target == trait_name) {
                Some(attr.ast)
            } else {
                None
            }
        })
    })
}

fn parse_derive_targets(attr: &Attribute) -> Option<Vec<String>> {
    let trimmed = attr.text.trim();
    let without_wrapper = trimmed.strip_prefix("#[")?.strip_suffix(']')?;
    let inner = without_wrapper.trim();
    let after_derive = inner.strip_prefix("derive")?.trim_start();
    let without_paren = after_derive.strip_prefix('(')?.strip_suffix(')')?;
    let mut targets = Vec::new();
    for entry in without_paren.split(',') {
        let name = entry.trim();
        if !name.is_empty() {
            targets.push(name.to_string());
        }
    }
    if targets.is_empty() {
        None
    } else {
        Some(targets)
    }
}

fn derive_struct_tostring(
    struct_def: &StructDef,
    attr_ptr: &MySyntaxNodePtr,
) -> Result<ImplBlock, Diagnostic> {
    if !struct_def.generics.is_empty() {
        return Err(generic_not_supported("struct", &struct_def.name, attr_ptr));
    }

    let method = ast::Fn {
        attrs: Vec::new(),
        name: Ident::new(TO_STRING_FN),
        generics: Vec::new(),
        params: vec![(Ident::new(SELF_PARAM_NAME), ty_for_ident(&struct_def.name))],
        ret_ty: Some(ast::TypeExpr::TString),
        body: build_struct_body(struct_def, attr_ptr),
    };

    Ok(ImplBlock {
        attrs: Vec::new(),
        trait_name: None,
        for_type: ty_for_ident(&struct_def.name),
        methods: vec![method],
    })
}

fn derive_enum_tostring(
    enum_def: &EnumDef,
    attr_ptr: &MySyntaxNodePtr,
) -> Result<ImplBlock, Diagnostic> {
    if !enum_def.generics.is_empty() {
        return Err(generic_not_supported("enum", &enum_def.name, attr_ptr));
    }

    let method = ast::Fn {
        attrs: Vec::new(),
        name: Ident::new(TO_STRING_FN),
        generics: Vec::new(),
        params: vec![(Ident::new(SELF_PARAM_NAME), ty_for_ident(&enum_def.name))],
        ret_ty: Some(ast::TypeExpr::TString),
        body: build_enum_body(enum_def, attr_ptr),
    };

    Ok(ImplBlock {
        attrs: Vec::new(),
        trait_name: None,
        for_type: ty_for_ident(&enum_def.name),
        methods: vec![method],
    })
}

fn derive_struct_tojson(
    struct_def: &StructDef,
    attr_ptr: &MySyntaxNodePtr,
) -> Result<ImplBlock, Diagnostic> {
    if !struct_def.generics.is_empty() {
        return Err(generic_not_supported_json(
            "struct",
            &struct_def.name,
            attr_ptr,
        ));
    }

    let method = ast::Fn {
        attrs: Vec::new(),
        name: Ident::new(TO_JSON_FN),
        generics: Vec::new(),
        params: vec![(Ident::new(SELF_PARAM_NAME), ty_for_ident(&struct_def.name))],
        ret_ty: Some(ast::TypeExpr::TString),
        body: build_struct_json_body(struct_def, attr_ptr),
    };

    Ok(ImplBlock {
        attrs: Vec::new(),
        trait_name: None,
        for_type: ty_for_ident(&struct_def.name),
        methods: vec![method],
    })
}

fn derive_enum_tojson(
    enum_def: &EnumDef,
    attr_ptr: &MySyntaxNodePtr,
) -> Result<ImplBlock, Diagnostic> {
    if !enum_def.generics.is_empty() {
        return Err(generic_not_supported_json("enum", &enum_def.name, attr_ptr));
    }

    let method = ast::Fn {
        attrs: Vec::new(),
        name: Ident::new(TO_JSON_FN),
        generics: Vec::new(),
        params: vec![(Ident::new(SELF_PARAM_NAME), ty_for_ident(&enum_def.name))],
        ret_ty: Some(ast::TypeExpr::TString),
        body: build_enum_json_body(enum_def, attr_ptr),
    };

    Ok(ImplBlock {
        attrs: Vec::new(),
        trait_name: None,
        for_type: ty_for_ident(&enum_def.name),
        methods: vec![method],
    })
}

fn build_struct_json_body(struct_def: &StructDef, attr_ptr: &MySyntaxNodePtr) -> Expr {
    if struct_def.fields.is_empty() {
        return Expr::EString {
            value: "{}".to_string(),
        };
    }

    let mut parts = Vec::new();
    parts.push(Expr::EString {
        value: "{".to_string(),
    });

    for (idx, (field_name, field_ty)) in struct_def.fields.iter().enumerate() {
        // Add comma before each field except the first
        if idx > 0 {
            parts.push(Expr::EString {
                value: ",".to_string(),
            });
        }
        // "fieldName":
        parts.push(Expr::EString {
            value: format!("\"{}\":", field_name.0),
        });
        // field value as JSON
        parts.push(call_to_json(
            var_expr(field_name, attr_ptr),
            Some(field_ty),
            attr_ptr,
        ));
    }

    parts.push(Expr::EString {
        value: "}".to_string(),
    });

    let body = concat_parts(parts, attr_ptr);
    Expr::ELet {
        pat: Pat::PStruct {
            name: struct_def.name.clone(),
            fields: struct_def
                .fields
                .iter()
                .map(|(field_name, _)| {
                    (
                        field_name.clone(),
                        Pat::PVar {
                            name: field_name.clone(),
                            astptr: *attr_ptr,
                        },
                    )
                })
                .collect(),
        },
        annotation: None,
        value: Box::new(var_expr(&Ident::new(SELF_PARAM_NAME), attr_ptr)),
        body: Box::new(body),
    }
}

fn build_enum_json_body(enum_def: &EnumDef, attr_ptr: &MySyntaxNodePtr) -> Expr {
    let expr = Box::new(var_expr(&Ident::new(SELF_PARAM_NAME), attr_ptr));
    let arms: Vec<Arm> = enum_def
        .variants
        .iter()
        .map(|(variant_name, fields)| {
            let constructor = Path::from_idents(vec![enum_def.name.clone(), variant_name.clone()]);
            if fields.is_empty() {
                // {"tag":"VariantName"}
                Arm {
                    pat: Pat::PConstr {
                        constructor,
                        args: Vec::new(),
                    },
                    body: Expr::EString {
                        value: format!("{{\"tag\":\"{}\"}}", variant_name.0),
                    },
                }
            } else {
                let bindings: Vec<Ident> = (0..fields.len())
                    .map(|idx| Ident::new(&format!("__field{}", idx)))
                    .collect();
                let args = bindings
                    .iter()
                    .map(|binding| Pat::PVar {
                        name: binding.clone(),
                        astptr: *attr_ptr,
                    })
                    .collect();

                // {"tag":"VariantName","fields":[field0,field1,...]}
                let mut parts = Vec::new();
                parts.push(Expr::EString {
                    value: format!("{{\"tag\":\"{}\",\"fields\":[", variant_name.0),
                });
                for (idx, (binding, field_ty)) in bindings.iter().zip(fields.iter()).enumerate() {
                    if idx > 0 {
                        parts.push(Expr::EString {
                            value: ",".to_string(),
                        });
                    }
                    parts.push(call_to_json(
                        var_expr(binding, attr_ptr),
                        Some(field_ty),
                        attr_ptr,
                    ));
                }
                parts.push(Expr::EString {
                    value: "]}".to_string(),
                });
                Arm {
                    pat: Pat::PConstr { constructor, args },
                    body: concat_parts(parts, attr_ptr),
                }
            }
        })
        .collect();

    Expr::EMatch {
        expr,
        arms,
        astptr: *attr_ptr,
    }
}

fn build_struct_body(struct_def: &StructDef, attr_ptr: &MySyntaxNodePtr) -> Expr {
    if struct_def.fields.is_empty() {
        return Expr::EString {
            value: format!("{} {{}}", struct_def.name.0),
        };
    }

    let mut parts = Vec::new();
    parts.push(Expr::EString {
        value: format!("{} {{ ", struct_def.name.0),
    });

    for (idx, (field_name, field_ty)) in struct_def.fields.iter().enumerate() {
        parts.push(Expr::EString {
            value: format!("{}: ", field_name.0),
        });
        parts.push(call_to_string(
            var_expr(field_name, attr_ptr),
            Some(field_ty),
            attr_ptr,
        ));
        if idx + 1 != struct_def.fields.len() {
            parts.push(Expr::EString {
                value: ", ".to_string(),
            });
        }
    }

    parts.push(Expr::EString {
        value: " }".to_string(),
    });

    let body = concat_parts(parts, attr_ptr);
    Expr::ELet {
        pat: Pat::PStruct {
            name: struct_def.name.clone(),
            fields: struct_def
                .fields
                .iter()
                .map(|(field_name, _)| {
                    (
                        field_name.clone(),
                        Pat::PVar {
                            name: field_name.clone(),
                            astptr: *attr_ptr,
                        },
                    )
                })
                .collect(),
        },
        annotation: None,
        value: Box::new(var_expr(&Ident::new(SELF_PARAM_NAME), attr_ptr)),
        body: Box::new(body),
    }
}

fn build_enum_body(enum_def: &EnumDef, attr_ptr: &MySyntaxNodePtr) -> Expr {
    let expr = Box::new(var_expr(&Ident::new(SELF_PARAM_NAME), attr_ptr));
    let arms: Vec<Arm> = enum_def
        .variants
        .iter()
        .map(|(variant_name, fields)| {
            let constructor = Path::from_idents(vec![enum_def.name.clone(), variant_name.clone()]);
            if fields.is_empty() {
                Arm {
                    pat: Pat::PConstr {
                        constructor,
                        args: Vec::new(),
                    },
                    body: Expr::EString {
                        value: format!("{}::{}", enum_def.name.0, variant_name.0),
                    },
                }
            } else {
                let bindings: Vec<Ident> = (0..fields.len())
                    .map(|idx| Ident::new(&format!("__field{}", idx)))
                    .collect();
                let args = bindings
                    .iter()
                    .map(|binding| Pat::PVar {
                        name: binding.clone(),
                        astptr: *attr_ptr,
                    })
                    .collect();
                let mut parts = Vec::new();
                parts.push(Expr::EString {
                    value: format!("{}::{}(", enum_def.name.0, variant_name.0),
                });
                for (idx, (binding, field_ty)) in bindings.iter().zip(fields.iter()).enumerate() {
                    if idx > 0 {
                        parts.push(Expr::EString {
                            value: ", ".to_string(),
                        });
                    }
                    parts.push(call_to_string(
                        var_expr(binding, attr_ptr),
                        Some(field_ty),
                        attr_ptr,
                    ));
                }
                parts.push(Expr::EString {
                    value: ")".to_string(),
                });
                Arm {
                    pat: Pat::PConstr { constructor, args },
                    body: concat_parts(parts, attr_ptr),
                }
            }
        })
        .collect();

    Expr::EMatch {
        expr,
        arms,
        astptr: *attr_ptr,
    }
}

fn concat_parts(parts: Vec<Expr>, attr_ptr: &MySyntaxNodePtr) -> Expr {
    let mut iter = parts.into_iter();
    let mut acc = iter.next().unwrap_or(Expr::EString {
        value: String::new(),
    });
    for part in iter {
        acc = call_string_add(acc, part, attr_ptr);
    }
    acc
}

fn call_to_string(value: Expr, ty: Option<&ast::TypeExpr>, attr_ptr: &MySyntaxNodePtr) -> Expr {
    if matches!(ty, Some(ast::TypeExpr::TString)) {
        value
    } else {
        Expr::ECall {
            func: Box::new(Expr::EField {
                expr: Box::new(value),
                field: Ident::new(TO_STRING_FN),
                astptr: *attr_ptr,
            }),
            args: Vec::new(),
        }
    }
}

fn call_to_json(value: Expr, ty: Option<&ast::TypeExpr>, attr_ptr: &MySyntaxNodePtr) -> Expr {
    match ty {
        // String needs to be quoted and escaped in JSON
        Some(ast::TypeExpr::TString) => {
            // Call json_escape_string(value) which wraps with quotes and escapes
            call_function("json_escape_string", vec![value], attr_ptr)
        }
        // Booleans are serialized as true/false (lowercase)
        Some(ast::TypeExpr::TBool) => call_function("bool_to_json", vec![value], attr_ptr),
        // Numbers can be serialized directly via to_string
        Some(ast::TypeExpr::TInt8)
        | Some(ast::TypeExpr::TInt16)
        | Some(ast::TypeExpr::TInt32)
        | Some(ast::TypeExpr::TInt64)
        | Some(ast::TypeExpr::TUint8)
        | Some(ast::TypeExpr::TUint16)
        | Some(ast::TypeExpr::TUint32)
        | Some(ast::TypeExpr::TUint64)
        | Some(ast::TypeExpr::TFloat32)
        | Some(ast::TypeExpr::TFloat64) => Expr::ECall {
            func: Box::new(Expr::EField {
                expr: Box::new(value),
                field: Ident::new(TO_STRING_FN),
                astptr: *attr_ptr,
            }),
            args: Vec::new(),
        },
        // Unit serializes as null
        Some(ast::TypeExpr::TUnit) => Expr::EString {
            value: "null".to_string(),
        },
        // For other types (user-defined structs/enums), call .to_json()
        _ => Expr::ECall {
            func: Box::new(Expr::EField {
                expr: Box::new(value),
                field: Ident::new(TO_JSON_FN),
                astptr: *attr_ptr,
            }),
            args: Vec::new(),
        },
    }
}

fn call_string_add(lhs: Expr, rhs: Expr, attr_ptr: &MySyntaxNodePtr) -> Expr {
    call_function(STRING_ADD_FN, vec![lhs, rhs], attr_ptr)
}

fn call_function(name: &str, args: Vec<Expr>, attr_ptr: &MySyntaxNodePtr) -> Expr {
    Expr::ECall {
        func: Box::new(var_expr(&Ident::new(name), attr_ptr)),
        args,
    }
}

fn var_expr(name: &Ident, attr_ptr: &MySyntaxNodePtr) -> Expr {
    Expr::EVar {
        name: name.clone(),
        astptr: *attr_ptr,
    }
}

fn ty_for_ident(name: &Ident) -> ast::TypeExpr {
    ast::TypeExpr::TCon {
        name: name.0.clone(),
    }
}

fn generic_not_supported(kind: &str, name: &Ident, attr_ptr: &MySyntaxNodePtr) -> Diagnostic {
    Diagnostic::new(
        Stage::other(DERIVE_STAGE),
        Severity::Error,
        format!(
            "`#[derive(ToString)]` is not supported for generic {} `{}`",
            kind, name.0
        ),
    )
    .with_range(attr_ptr.text_range())
}

fn generic_not_supported_json(kind: &str, name: &Ident, attr_ptr: &MySyntaxNodePtr) -> Diagnostic {
    Diagnostic::new(
        Stage::other(DERIVE_STAGE),
        Severity::Error,
        format!(
            "`#[derive(ToJson)]` is not supported for generic {} `{}`",
            kind, name.0
        ),
    )
    .with_range(attr_ptr.text_range())
}
