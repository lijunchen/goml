use ::ast::ast::{
    self, Arm, AstIdent, Attribute, Block, EnumDef, Expr, ImplBlock, Item, LetStmt, Pat, Path,
    Stmt, StructDef,
};
use diagnostics::{Diagnostic, Diagnostics, Severity, Stage};
use parser::syntax::MySyntaxNodePtr;

const DERIVE_STAGE: &str = "derive";
const TO_STRING_TRAIT: &str = "ToString";
const TO_STRING_FN: &str = "to_string";
const TO_JSON_TRAIT: &str = "ToJson";
const TO_JSON_FN: &str = "to_json";
const HASH_TRAIT: &str = "Hash";
const HASH_FN: &str = "hash";
const EQ_TRAIT: &str = "Eq";
const EQ_FN: &str = "eq";
const SELF_PARAM_NAME: &str = "self";
const OTHER_PARAM_NAME: &str = "other";

pub fn expand(ast: ast::File) -> Result<ast::File, Diagnostics> {
    let mut diagnostics = Diagnostics::new();
    let mut toplevels = Vec::with_capacity(ast.toplevels.len());
    let package = ast.package.clone();
    let imports = ast.imports.clone();
    let use_traits = ast.use_traits.clone();

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
                if let Some(attr_ptr) = find_derive_attr(&struct_def.attrs, EQ_TRAIT) {
                    match derive_struct_eq(struct_def, &attr_ptr) {
                        Ok(impl_block) => derived_impls.push(impl_block),
                        Err(diag) => diagnostics.push(diag),
                    }
                }
                if let Some(attr_ptr) = find_derive_attr(&struct_def.attrs, HASH_TRAIT) {
                    match derive_struct_hash(struct_def, &attr_ptr) {
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
                if let Some(attr_ptr) = find_derive_attr(&enum_def.attrs, EQ_TRAIT) {
                    match derive_enum_eq(enum_def, &attr_ptr) {
                        Ok(impl_block) => derived_impls.push(impl_block),
                        Err(diag) => diagnostics.push(diag),
                    }
                }
                if let Some(attr_ptr) = find_derive_attr(&enum_def.attrs, HASH_TRAIT) {
                    match derive_enum_hash(enum_def, &attr_ptr) {
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
        Ok(ast::File {
            package,
            imports,
            use_traits,
            toplevels,
        })
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

fn expr_as_fn_body(expr: Expr, attr_ptr: &MySyntaxNodePtr) -> Block {
    match expr {
        Expr::EBlock { block, .. } => *block,
        tail => Block {
            stmts: Vec::new(),
            tail: Some(Box::new(tail)),
            astptr: *attr_ptr,
        },
    }
}

fn let_stmt(
    pat: Pat,
    annotation: Option<ast::TypeExpr>,
    value: Expr,
    attr_ptr: &MySyntaxNodePtr,
) -> Stmt {
    Stmt::Let(LetStmt {
        pat,
        annotation,
        value: Box::new(value),
        astptr: *attr_ptr,
    })
}

fn block_expr(stmts: Vec<Stmt>, tail: Expr, attr_ptr: &MySyntaxNodePtr) -> Expr {
    Expr::EBlock {
        block: Box::new(Block {
            stmts,
            tail: Some(Box::new(tail)),
            astptr: *attr_ptr,
        }),
        astptr: *attr_ptr,
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
        name: AstIdent::new(TO_STRING_FN),
        generics: Vec::new(),
        generic_bounds: Vec::new(),
        params: vec![(
            AstIdent::new(SELF_PARAM_NAME),
            ty_for_ident(&struct_def.name),
        )],
        ret_ty: Some(ast::TypeExpr::TString),
        body: expr_as_fn_body(build_struct_body(struct_def, attr_ptr), attr_ptr),
    };

    Ok(ImplBlock {
        attrs: Vec::new(),
        generics: Vec::new(),
        generic_bounds: Vec::new(),
        trait_name: Some(Path::from_ident(AstIdent::new(TO_STRING_TRAIT))),
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
        name: AstIdent::new(TO_STRING_FN),
        generics: Vec::new(),
        generic_bounds: Vec::new(),
        params: vec![(AstIdent::new(SELF_PARAM_NAME), ty_for_ident(&enum_def.name))],
        ret_ty: Some(ast::TypeExpr::TString),
        body: expr_as_fn_body(build_enum_body(enum_def, attr_ptr), attr_ptr),
    };

    Ok(ImplBlock {
        attrs: Vec::new(),
        generics: Vec::new(),
        generic_bounds: Vec::new(),
        trait_name: Some(Path::from_ident(AstIdent::new(TO_STRING_TRAIT))),
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
        name: AstIdent::new(TO_JSON_FN),
        generics: Vec::new(),
        generic_bounds: Vec::new(),
        params: vec![(
            AstIdent::new(SELF_PARAM_NAME),
            ty_for_ident(&struct_def.name),
        )],
        ret_ty: Some(ast::TypeExpr::TString),
        body: expr_as_fn_body(build_struct_json_body(struct_def, attr_ptr), attr_ptr),
    };

    Ok(ImplBlock {
        attrs: Vec::new(),
        generics: Vec::new(),
        generic_bounds: Vec::new(),
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
        name: AstIdent::new(TO_JSON_FN),
        generics: Vec::new(),
        generic_bounds: Vec::new(),
        params: vec![(AstIdent::new(SELF_PARAM_NAME), ty_for_ident(&enum_def.name))],
        ret_ty: Some(ast::TypeExpr::TString),
        body: expr_as_fn_body(build_enum_json_body(enum_def, attr_ptr), attr_ptr),
    };

    Ok(ImplBlock {
        attrs: Vec::new(),
        generics: Vec::new(),
        generic_bounds: Vec::new(),
        trait_name: None,
        for_type: ty_for_ident(&enum_def.name),
        methods: vec![method],
    })
}

fn derive_struct_eq(
    struct_def: &StructDef,
    attr_ptr: &MySyntaxNodePtr,
) -> Result<ImplBlock, Diagnostic> {
    if !struct_def.generics.is_empty() {
        return Err(generic_not_supported_other(
            EQ_TRAIT,
            "struct",
            &struct_def.name,
            attr_ptr,
        ));
    }

    let method = ast::Fn {
        attrs: Vec::new(),
        name: AstIdent::new(EQ_FN),
        generics: Vec::new(),
        generic_bounds: Vec::new(),
        params: vec![
            (
                AstIdent::new(SELF_PARAM_NAME),
                ty_for_ident(&struct_def.name),
            ),
            (
                AstIdent::new(OTHER_PARAM_NAME),
                ty_for_ident(&struct_def.name),
            ),
        ],
        ret_ty: Some(ast::TypeExpr::TBool),
        body: expr_as_fn_body(build_struct_eq_body(struct_def, attr_ptr), attr_ptr),
    };

    Ok(ImplBlock {
        attrs: Vec::new(),
        generics: Vec::new(),
        generic_bounds: Vec::new(),
        trait_name: Some(Path::from_ident(AstIdent::new(EQ_TRAIT))),
        for_type: ty_for_ident(&struct_def.name),
        methods: vec![method],
    })
}

fn derive_enum_eq(enum_def: &EnumDef, attr_ptr: &MySyntaxNodePtr) -> Result<ImplBlock, Diagnostic> {
    if !enum_def.generics.is_empty() {
        return Err(generic_not_supported_other(
            EQ_TRAIT,
            "enum",
            &enum_def.name,
            attr_ptr,
        ));
    }

    let method = ast::Fn {
        attrs: Vec::new(),
        name: AstIdent::new(EQ_FN),
        generics: Vec::new(),
        generic_bounds: Vec::new(),
        params: vec![
            (AstIdent::new(SELF_PARAM_NAME), ty_for_ident(&enum_def.name)),
            (
                AstIdent::new(OTHER_PARAM_NAME),
                ty_for_ident(&enum_def.name),
            ),
        ],
        ret_ty: Some(ast::TypeExpr::TBool),
        body: expr_as_fn_body(build_enum_eq_body(enum_def, attr_ptr), attr_ptr),
    };

    Ok(ImplBlock {
        attrs: Vec::new(),
        generics: Vec::new(),
        generic_bounds: Vec::new(),
        trait_name: Some(Path::from_ident(AstIdent::new(EQ_TRAIT))),
        for_type: ty_for_ident(&enum_def.name),
        methods: vec![method],
    })
}

fn derive_struct_hash(
    struct_def: &StructDef,
    attr_ptr: &MySyntaxNodePtr,
) -> Result<ImplBlock, Diagnostic> {
    if !struct_def.generics.is_empty() {
        return Err(generic_not_supported_other(
            HASH_TRAIT,
            "struct",
            &struct_def.name,
            attr_ptr,
        ));
    }

    let method = ast::Fn {
        attrs: Vec::new(),
        name: AstIdent::new(HASH_FN),
        generics: Vec::new(),
        generic_bounds: Vec::new(),
        params: vec![(
            AstIdent::new(SELF_PARAM_NAME),
            ty_for_ident(&struct_def.name),
        )],
        ret_ty: Some(ast::TypeExpr::TUint64),
        body: expr_as_fn_body(build_struct_hash_body(struct_def, attr_ptr), attr_ptr),
    };

    Ok(ImplBlock {
        attrs: Vec::new(),
        generics: Vec::new(),
        generic_bounds: Vec::new(),
        trait_name: Some(Path::from_ident(AstIdent::new(HASH_TRAIT))),
        for_type: ty_for_ident(&struct_def.name),
        methods: vec![method],
    })
}

fn derive_enum_hash(
    enum_def: &EnumDef,
    attr_ptr: &MySyntaxNodePtr,
) -> Result<ImplBlock, Diagnostic> {
    if !enum_def.generics.is_empty() {
        return Err(generic_not_supported_other(
            HASH_TRAIT,
            "enum",
            &enum_def.name,
            attr_ptr,
        ));
    }

    let method = ast::Fn {
        attrs: Vec::new(),
        name: AstIdent::new(HASH_FN),
        generics: Vec::new(),
        generic_bounds: Vec::new(),
        params: vec![(AstIdent::new(SELF_PARAM_NAME), ty_for_ident(&enum_def.name))],
        ret_ty: Some(ast::TypeExpr::TUint64),
        body: expr_as_fn_body(build_enum_hash_body(enum_def, attr_ptr), attr_ptr),
    };

    Ok(ImplBlock {
        attrs: Vec::new(),
        generics: Vec::new(),
        generic_bounds: Vec::new(),
        trait_name: Some(Path::from_ident(AstIdent::new(HASH_TRAIT))),
        for_type: ty_for_ident(&enum_def.name),
        methods: vec![method],
    })
}

fn build_struct_json_body(struct_def: &StructDef, attr_ptr: &MySyntaxNodePtr) -> Expr {
    if struct_def.fields.is_empty() {
        return Expr::EString {
            value: "{}".to_string(),
            astptr: *attr_ptr,
        };
    }

    let mut parts = Vec::new();
    parts.push(Expr::EString {
        value: "{".to_string(),
        astptr: *attr_ptr,
    });

    for (idx, (field_name, field_ty)) in struct_def.fields.iter().enumerate() {
        // Add comma before each field except the first
        if idx > 0 {
            parts.push(Expr::EString {
                value: ",".to_string(),
                astptr: *attr_ptr,
            });
        }
        // "fieldName":
        parts.push(Expr::EString {
            value: format!("\"{}\":", field_name.0),
            astptr: *attr_ptr,
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
        astptr: *attr_ptr,
    });

    let body = concat_parts(parts, attr_ptr);
    block_expr(
        vec![let_stmt(
            Pat::PStruct {
                name: Path::from_ident(struct_def.name.clone()),
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
                astptr: *attr_ptr,
            },
            None,
            var_expr(&AstIdent::new(SELF_PARAM_NAME), attr_ptr),
            attr_ptr,
        )],
        body,
        attr_ptr,
    )
}

fn build_enum_json_body(enum_def: &EnumDef, attr_ptr: &MySyntaxNodePtr) -> Expr {
    let expr = Box::new(var_expr(&AstIdent::new(SELF_PARAM_NAME), attr_ptr));
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
                        astptr: *attr_ptr,
                    },
                    body: Expr::EString {
                        value: format!("{{\"tag\":\"{}\"}}", variant_name.0),
                        astptr: *attr_ptr,
                    },
                }
            } else {
                let bindings: Vec<AstIdent> = (0..fields.len())
                    .map(|idx| AstIdent::new(&format!("__field{}", idx)))
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
                    astptr: *attr_ptr,
                });
                for (idx, (binding, field_ty)) in bindings.iter().zip(fields.iter()).enumerate() {
                    if idx > 0 {
                        parts.push(Expr::EString {
                            value: ",".to_string(),
                            astptr: *attr_ptr,
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
                    astptr: *attr_ptr,
                });
                Arm {
                    pat: Pat::PConstr {
                        constructor,
                        args,
                        astptr: *attr_ptr,
                    },
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
            astptr: *attr_ptr,
        };
    }

    let mut parts = Vec::new();
    parts.push(Expr::EString {
        value: format!("{} {{ ", struct_def.name.0),
        astptr: *attr_ptr,
    });

    for (idx, (field_name, field_ty)) in struct_def.fields.iter().enumerate() {
        parts.push(Expr::EString {
            value: format!("{}: ", field_name.0),
            astptr: *attr_ptr,
        });
        parts.push(call_to_string(
            var_expr(field_name, attr_ptr),
            Some(field_ty),
            attr_ptr,
        ));
        if idx + 1 != struct_def.fields.len() {
            parts.push(Expr::EString {
                value: ", ".to_string(),
                astptr: *attr_ptr,
            });
        }
    }

    parts.push(Expr::EString {
        value: " }".to_string(),
        astptr: *attr_ptr,
    });

    let body = concat_parts(parts, attr_ptr);
    block_expr(
        vec![let_stmt(
            Pat::PStruct {
                name: Path::from_ident(struct_def.name.clone()),
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
                astptr: *attr_ptr,
            },
            None,
            var_expr(&AstIdent::new(SELF_PARAM_NAME), attr_ptr),
            attr_ptr,
        )],
        body,
        attr_ptr,
    )
}

fn build_enum_body(enum_def: &EnumDef, attr_ptr: &MySyntaxNodePtr) -> Expr {
    let expr = Box::new(var_expr(&AstIdent::new(SELF_PARAM_NAME), attr_ptr));
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
                        astptr: *attr_ptr,
                    },
                    body: Expr::EString {
                        value: format!("{}::{}", enum_def.name.0, variant_name.0),
                        astptr: *attr_ptr,
                    },
                }
            } else {
                let bindings: Vec<AstIdent> = (0..fields.len())
                    .map(|idx| AstIdent::new(&format!("__field{}", idx)))
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
                    astptr: *attr_ptr,
                });
                for (idx, (binding, field_ty)) in bindings.iter().zip(fields.iter()).enumerate() {
                    if idx > 0 {
                        parts.push(Expr::EString {
                            value: ", ".to_string(),
                            astptr: *attr_ptr,
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
                    astptr: *attr_ptr,
                });
                Arm {
                    pat: Pat::PConstr {
                        constructor,
                        args,
                        astptr: *attr_ptr,
                    },
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

fn build_struct_eq_body(struct_def: &StructDef, attr_ptr: &MySyntaxNodePtr) -> Expr {
    let self_var = var_expr(&AstIdent::new(SELF_PARAM_NAME), attr_ptr);
    let other_var = var_expr(&AstIdent::new(OTHER_PARAM_NAME), attr_ptr);

    let mut out = Expr::EBool {
        value: true,
        astptr: *attr_ptr,
    };

    for (field_name, _field_ty) in struct_def.fields.iter() {
        let lhs = Expr::EField {
            expr: Box::new(self_var.clone()),
            field: field_name.clone(),
            astptr: *attr_ptr,
        };
        let rhs = Expr::EField {
            expr: Box::new(other_var.clone()),
            field: field_name.clone(),
            astptr: *attr_ptr,
        };
        let eq = trait_call(EQ_TRAIT, EQ_FN, vec![lhs, rhs], attr_ptr);
        out = Expr::EBinary {
            op: common_defs::BinaryOp::And,
            lhs: Box::new(out),
            rhs: Box::new(eq),
            astptr: *attr_ptr,
        };
    }
    out
}

fn build_enum_eq_body(enum_def: &EnumDef, attr_ptr: &MySyntaxNodePtr) -> Expr {
    let self_var = var_expr(&AstIdent::new(SELF_PARAM_NAME), attr_ptr);
    let other_var = var_expr(&AstIdent::new(OTHER_PARAM_NAME), attr_ptr);

    let scrutinee = Expr::ETuple {
        items: vec![self_var, other_var],
        astptr: *attr_ptr,
    };

    let mut arms = Vec::new();

    for (variant_idx, (variant_name, fields)) in enum_def.variants.iter().enumerate() {
        let left_bindings: Vec<AstIdent> = (0..fields.len())
            .map(|idx| AstIdent::new(&format!("__l{}_{}", variant_idx, idx)))
            .collect();
        let right_bindings: Vec<AstIdent> = (0..fields.len())
            .map(|idx| AstIdent::new(&format!("__r{}_{}", variant_idx, idx)))
            .collect();

        let constr = Path::from_idents(vec![enum_def.name.clone(), variant_name.clone()]);
        let left_pat = Pat::PConstr {
            constructor: constr.clone(),
            args: left_bindings
                .iter()
                .map(|b| Pat::PVar {
                    name: b.clone(),
                    astptr: *attr_ptr,
                })
                .collect(),
            astptr: *attr_ptr,
        };
        let right_pat = Pat::PConstr {
            constructor: constr,
            args: right_bindings
                .iter()
                .map(|b| Pat::PVar {
                    name: b.clone(),
                    astptr: *attr_ptr,
                })
                .collect(),
            astptr: *attr_ptr,
        };

        let pat = Pat::PTuple {
            pats: vec![left_pat, right_pat],
            astptr: *attr_ptr,
        };

        let mut body = Expr::EBool {
            value: true,
            astptr: *attr_ptr,
        };
        for (l, r) in left_bindings.iter().zip(right_bindings.iter()) {
            let eq = trait_call(
                EQ_TRAIT,
                EQ_FN,
                vec![var_expr(l, attr_ptr), var_expr(r, attr_ptr)],
                attr_ptr,
            );
            body = Expr::EBinary {
                op: common_defs::BinaryOp::And,
                lhs: Box::new(body),
                rhs: Box::new(eq),
                astptr: *attr_ptr,
            };
        }

        arms.push(Arm { pat, body });
    }

    arms.push(Arm {
        pat: Pat::PWild { astptr: *attr_ptr },
        body: Expr::EBool {
            value: false,
            astptr: *attr_ptr,
        },
    });

    Expr::EMatch {
        expr: Box::new(scrutinee),
        arms,
        astptr: *attr_ptr,
    }
}

fn build_struct_hash_body(struct_def: &StructDef, attr_ptr: &MySyntaxNodePtr) -> Expr {
    let mut stmts = Vec::new();
    stmts.push(let_stmt(
        Pat::PVar {
            name: AstIdent::new("h"),
            astptr: *attr_ptr,
        },
        Some(ast::TypeExpr::TUint64),
        Expr::EUInt64 {
            value: "14695981039346656037".to_string(),
            astptr: *attr_ptr,
        },
        attr_ptr,
    ));

    let self_var = var_expr(&AstIdent::new(SELF_PARAM_NAME), attr_ptr);
    for (field_name, _field_ty) in struct_def.fields.iter() {
        let field = Expr::EField {
            expr: Box::new(self_var.clone()),
            field: field_name.clone(),
            astptr: *attr_ptr,
        };
        let field_hash = trait_call(HASH_TRAIT, HASH_FN, vec![field], attr_ptr);
        let h_next = Expr::EBinary {
            op: common_defs::BinaryOp::Add,
            lhs: Box::new(Expr::EBinary {
                op: common_defs::BinaryOp::Mul,
                lhs: Box::new(var_expr(&AstIdent::new("h"), attr_ptr)),
                rhs: Box::new(Expr::EUInt64 {
                    value: "1099511628211".to_string(),
                    astptr: *attr_ptr,
                }),
                astptr: *attr_ptr,
            }),
            rhs: Box::new(field_hash),
            astptr: *attr_ptr,
        };
        stmts.push(let_stmt(
            Pat::PVar {
                name: AstIdent::new("h"),
                astptr: *attr_ptr,
            },
            None,
            h_next,
            attr_ptr,
        ));
    }

    block_expr(stmts, var_expr(&AstIdent::new("h"), attr_ptr), attr_ptr)
}

fn build_enum_hash_body(enum_def: &EnumDef, attr_ptr: &MySyntaxNodePtr) -> Expr {
    let self_var = Box::new(var_expr(&AstIdent::new(SELF_PARAM_NAME), attr_ptr));
    let mut arms = Vec::new();

    for (idx, (variant_name, fields)) in enum_def.variants.iter().enumerate() {
        let constructor = Path::from_idents(vec![enum_def.name.clone(), variant_name.clone()]);
        let bindings: Vec<AstIdent> = (0..fields.len())
            .map(|i| AstIdent::new(&format!("__field{}_{}", idx, i)))
            .collect();
        let args = bindings
            .iter()
            .map(|binding| Pat::PVar {
                name: binding.clone(),
                astptr: *attr_ptr,
            })
            .collect();

        let tag = Expr::EUInt64 {
            value: (idx as u64 + 1).to_string(),
            astptr: *attr_ptr,
        };

        let mut stmts = Vec::new();
        stmts.push(let_stmt(
            Pat::PVar {
                name: AstIdent::new("h"),
                astptr: *attr_ptr,
            },
            Some(ast::TypeExpr::TUint64),
            Expr::EBinary {
                op: common_defs::BinaryOp::Add,
                lhs: Box::new(Expr::EUInt64 {
                    value: "14695981039346656037".to_string(),
                    astptr: *attr_ptr,
                }),
                rhs: Box::new(tag),
                astptr: *attr_ptr,
            },
            attr_ptr,
        ));

        for binding in bindings.iter() {
            let field_hash = trait_call(
                HASH_TRAIT,
                HASH_FN,
                vec![var_expr(binding, attr_ptr)],
                attr_ptr,
            );
            let h_next = Expr::EBinary {
                op: common_defs::BinaryOp::Add,
                lhs: Box::new(Expr::EBinary {
                    op: common_defs::BinaryOp::Mul,
                    lhs: Box::new(var_expr(&AstIdent::new("h"), attr_ptr)),
                    rhs: Box::new(Expr::EUInt64 {
                        value: "1099511628211".to_string(),
                        astptr: *attr_ptr,
                    }),
                    astptr: *attr_ptr,
                }),
                rhs: Box::new(field_hash),
                astptr: *attr_ptr,
            };
            stmts.push(let_stmt(
                Pat::PVar {
                    name: AstIdent::new("h"),
                    astptr: *attr_ptr,
                },
                None,
                h_next,
                attr_ptr,
            ));
        }

        let body = block_expr(stmts, var_expr(&AstIdent::new("h"), attr_ptr), attr_ptr);

        arms.push(Arm {
            pat: Pat::PConstr {
                constructor,
                args,
                astptr: *attr_ptr,
            },
            body,
        });
    }

    Expr::EMatch {
        expr: self_var,
        arms,
        astptr: *attr_ptr,
    }
}

fn concat_parts(parts: Vec<Expr>, attr_ptr: &MySyntaxNodePtr) -> Expr {
    let mut iter = parts.into_iter();
    let mut acc = iter.next().unwrap_or(Expr::EString {
        value: String::new(),
        astptr: *attr_ptr,
    });
    for part in iter {
        acc = Expr::EBinary {
            op: common_defs::BinaryOp::Add,
            lhs: Box::new(acc),
            rhs: Box::new(part),
            astptr: *attr_ptr,
        };
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
                field: AstIdent::new(TO_STRING_FN),
                astptr: *attr_ptr,
            }),
            args: Vec::new(),
            astptr: *attr_ptr,
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
                field: AstIdent::new(TO_STRING_FN),
                astptr: *attr_ptr,
            }),
            args: Vec::new(),
            astptr: *attr_ptr,
        },
        // Unit serializes as null
        Some(ast::TypeExpr::TUnit) => Expr::EString {
            value: "null".to_string(),
            astptr: *attr_ptr,
        },
        // For other types (user-defined structs/enums), call .to_json()
        _ => Expr::ECall {
            func: Box::new(Expr::EField {
                expr: Box::new(value),
                field: AstIdent::new(TO_JSON_FN),
                astptr: *attr_ptr,
            }),
            args: Vec::new(),
            astptr: *attr_ptr,
        },
    }
}

fn call_function(name: &str, args: Vec<Expr>, attr_ptr: &MySyntaxNodePtr) -> Expr {
    Expr::ECall {
        func: Box::new(var_expr(&AstIdent::new(name), attr_ptr)),
        args,
        astptr: *attr_ptr,
    }
}

fn trait_call(
    trait_name: &str,
    method_name: &str,
    args: Vec<Expr>,
    attr_ptr: &MySyntaxNodePtr,
) -> Expr {
    Expr::ECall {
        func: Box::new(Expr::EPath {
            path: ast::Path::from_idents(vec![
                AstIdent::new(trait_name),
                AstIdent::new(method_name),
            ]),
            astptr: *attr_ptr,
        }),
        args,
        astptr: *attr_ptr,
    }
}

fn var_expr(name: &AstIdent, attr_ptr: &MySyntaxNodePtr) -> Expr {
    Expr::EPath {
        path: ast::Path::from_ident(name.clone()),
        astptr: *attr_ptr,
    }
}

fn ty_for_ident(name: &AstIdent) -> ast::TypeExpr {
    ast::TypeExpr::TCon {
        path: ast::Path::from_ident(name.clone()),
    }
}

fn generic_not_supported(kind: &str, name: &AstIdent, attr_ptr: &MySyntaxNodePtr) -> Diagnostic {
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

fn generic_not_supported_json(
    kind: &str,
    name: &AstIdent,
    attr_ptr: &MySyntaxNodePtr,
) -> Diagnostic {
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

fn generic_not_supported_other(
    trait_name: &str,
    kind: &str,
    name: &AstIdent,
    attr_ptr: &MySyntaxNodePtr,
) -> Diagnostic {
    Diagnostic::new(
        Stage::other(DERIVE_STAGE),
        Severity::Error,
        format!(
            "`#[derive({})]` is not supported for generic {} `{}`",
            trait_name, kind, name.0
        ),
    )
    .with_range(attr_ptr.text_range())
}
