use crate::{
    go::{
        compile::{GlobalGoEnv, variant_symbol_name},
        goast::{self, GoBinaryOp, ImportDecl, ImportSpec, Item, Package},
        goty,
        mangle::{encode_ty, go_generated_ident, go_ident},
    },
    names::{trait_impl_fn_name, ty_compact},
    package_names::ENTRY_FUNCTION,
    tast,
};

use indexmap::IndexSet;

fn ty_contains_type_param(ty: &tast::Ty) -> bool {
    match ty {
        tast::Ty::TParam { .. } => true,
        tast::Ty::TArray { elem, .. } | tast::Ty::TRef { elem } => ty_contains_type_param(elem),
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

// unit_to_string(x : struct{}) string
// bool_to_string(x : bool) string
// print(s : string) struct{}
// println(s : string) struct{}
// missing(s : string) struct{}
pub fn make_runtime() -> Vec<goast::Item> {
    let mut items = vec![
        Item::Package(Package {
            name: ENTRY_FUNCTION.to_string(),
        }),
        Item::Import(ImportDecl {
            specs: vec![
                ImportSpec {
                    alias: Some("_goml_fmt".to_string()),
                    path: "fmt".to_string(),
                },
                ImportSpec {
                    alias: Some("_goml_math".to_string()),
                    path: "math".to_string(),
                },
                ImportSpec {
                    alias: Some("_goml_utf8".to_string()),
                    path: "unicode/utf8".to_string(),
                },
            ],
        }),
        Item::Fn(unit_to_string()),
        Item::Fn(bool_to_string()),
        Item::Fn(bool_to_json()),
        Item::Fn(json_escape_string()),
        Item::Fn(string_len()),
        Item::Fn(string_get()),
        Item::Fn(char_to_string()),
        Item::Fn(int8_to_string()),
        Item::Fn(int16_to_string()),
        Item::Fn(int32_to_string()),
        Item::Fn(int64_to_string()),
        Item::Fn(uint8_to_string()),
        Item::Fn(uint16_to_string()),
        Item::Fn(uint32_to_string()),
        Item::Fn(uint64_to_string()),
        Item::Fn(float32_to_string()),
        Item::Fn(float64_to_string()),
        Item::Fn(int8_hash()),
        Item::Fn(int16_hash()),
        Item::Fn(int32_hash()),
        Item::Fn(int64_hash()),
        Item::Fn(char_hash()),
        Item::Fn(uint8_hash()),
        Item::Fn(uint16_hash()),
        Item::Fn(uint32_hash()),
        Item::Fn(float32_hash()),
        Item::Fn(float64_hash()),
        Item::Fn(string_hash()),
        Item::Fn(string_print()),
        Item::Fn(string_println()),
        Item::Fn(go_error_to_string()),
        Item::Fn(missing()),
    ];
    items.extend(make_builtin_eq_hash_trait_impls());
    items
}

fn make_builtin_eq_hash_trait_impls() -> Vec<goast::Item> {
    fn eq_impl(ty: tast::Ty) -> goast::Item {
        let go_ty = goast::tast_ty_to_go_type(&ty);
        let name = go_ident(&trait_impl_fn_name(
            &tast::TastIdent("Eq".to_string()),
            &ty,
            "eq",
        ));
        goast::Item::Fn(goast::Fn {
            name,
            params: vec![
                ("self".to_string(), go_ty.clone()),
                ("other".to_string(), go_ty.clone()),
            ],
            ret_ty: Some(goty::GoType::TBool),
            body: goast::Block {
                stmts: vec![goast::Stmt::Return {
                    expr: Some(goast::Expr::BinaryOp {
                        op: GoBinaryOp::Eq,
                        lhs: Box::new(goast::Expr::Var {
                            name: "self".to_string(),
                            ty: go_ty.clone(),
                        }),
                        rhs: Box::new(goast::Expr::Var {
                            name: "other".to_string(),
                            ty: go_ty.clone(),
                        }),
                        ty: goty::GoType::TBool,
                    }),
                }],
            },
        })
    }

    fn hash_impl(ty: tast::Ty, body_expr: goast::Expr) -> goast::Item {
        let go_ty = goast::tast_ty_to_go_type(&ty);
        let name = go_ident(&trait_impl_fn_name(
            &tast::TastIdent("Hash".to_string()),
            &ty,
            "hash",
        ));
        goast::Item::Fn(goast::Fn {
            name,
            params: vec![("self".to_string(), go_ty)],
            ret_ty: Some(goty::GoType::TUint64),
            body: goast::Block {
                stmts: vec![goast::Stmt::Return {
                    expr: Some(body_expr),
                }],
            },
        })
    }

    fn hash_call(hash_fn: &str, param_ty: goty::GoType) -> goast::Expr {
        goast::Expr::Call {
            func: Box::new(goast::Expr::Var {
                name: hash_fn.to_string(),
                ty: goty::GoType::TFunc {
                    params: vec![param_ty.clone()],
                    ret_ty: Box::new(goty::GoType::TUint64),
                },
            }),
            args: vec![goast::Expr::Var {
                name: "self".to_string(),
                ty: param_ty,
            }],
            ty: goty::GoType::TUint64,
        }
    }

    let eq_tys = vec![
        tast::Ty::TUnit,
        tast::Ty::TBool,
        tast::Ty::TString,
        tast::Ty::TChar,
        tast::Ty::TInt8,
        tast::Ty::TInt16,
        tast::Ty::TInt32,
        tast::Ty::TInt64,
        tast::Ty::TUint8,
        tast::Ty::TUint16,
        tast::Ty::TUint32,
        tast::Ty::TUint64,
        tast::Ty::TFloat32,
        tast::Ty::TFloat64,
    ];

    let mut items = Vec::new();
    for ty in eq_tys {
        items.push(eq_impl(ty));
    }

    items.push(hash_impl(
        tast::Ty::TUnit,
        goast::Expr::Int {
            value: "0".to_string(),
            ty: goty::GoType::TUint64,
        },
    ));
    items.push(goast::Item::Fn({
        let name = go_ident(&trait_impl_fn_name(
            &tast::TastIdent("Hash".to_string()),
            &tast::Ty::TBool,
            "hash",
        ));
        goast::Fn {
            name,
            params: vec![("self".to_string(), goty::GoType::TBool)],
            ret_ty: Some(goty::GoType::TUint64),
            body: goast::Block {
                stmts: vec![goast::Stmt::If {
                    cond: goast::Expr::Var {
                        name: "self".to_string(),
                        ty: goty::GoType::TBool,
                    },
                    then: goast::Block {
                        stmts: vec![goast::Stmt::Return {
                            expr: Some(goast::Expr::Int {
                                value: "1".to_string(),
                                ty: goty::GoType::TUint64,
                            }),
                        }],
                    },
                    else_: Some(goast::Block {
                        stmts: vec![goast::Stmt::Return {
                            expr: Some(goast::Expr::Int {
                                value: "0".to_string(),
                                ty: goty::GoType::TUint64,
                            }),
                        }],
                    }),
                }],
            },
        }
    }));
    items.push(hash_impl(
        tast::Ty::TString,
        hash_call("string_hash", goty::GoType::TString),
    ));
    items.push(hash_impl(
        tast::Ty::TChar,
        hash_call("char_hash", goty::GoType::TChar),
    ));
    items.push(hash_impl(
        tast::Ty::TInt8,
        hash_call("int8_hash", goty::GoType::TInt8),
    ));
    items.push(hash_impl(
        tast::Ty::TInt16,
        hash_call("int16_hash", goty::GoType::TInt16),
    ));
    items.push(hash_impl(
        tast::Ty::TInt32,
        hash_call("int32_hash", goty::GoType::TInt32),
    ));
    items.push(hash_impl(
        tast::Ty::TInt64,
        hash_call("int64_hash", goty::GoType::TInt64),
    ));
    items.push(hash_impl(
        tast::Ty::TUint8,
        hash_call("uint8_hash", goty::GoType::TUint8),
    ));
    items.push(hash_impl(
        tast::Ty::TUint16,
        hash_call("uint16_hash", goty::GoType::TUint16),
    ));
    items.push(hash_impl(
        tast::Ty::TUint32,
        hash_call("uint32_hash", goty::GoType::TUint32),
    ));
    items.push(hash_impl(
        tast::Ty::TUint64,
        goast::Expr::Var {
            name: "self".to_string(),
            ty: goty::GoType::TUint64,
        },
    ));
    items.push(hash_impl(
        tast::Ty::TFloat32,
        hash_call("float32_hash", goty::GoType::TFloat32),
    ));
    items.push(hash_impl(
        tast::Ty::TFloat64,
        hash_call("float64_hash", goty::GoType::TFloat64),
    ));

    items
}

fn go_error_to_string() -> goast::Fn {
    let go_error_ty = goty::GoType::TName {
        name: "GoError".to_string(),
    };
    goast::Fn {
        name: "go_error_to_string".to_string(),
        params: vec![("value".to_string(), go_error_ty.clone())],
        ret_ty: Some(goty::GoType::TString),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::Call {
                    func: Box::new(goast::Expr::FieldAccess {
                        obj: Box::new(goast::Expr::Var {
                            name: "value".to_string(),
                            ty: go_error_ty,
                        }),
                        field: "Error".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![],
                            ret_ty: Box::new(goty::GoType::TString),
                        },
                    }),
                    args: vec![],
                    ty: goty::GoType::TString,
                }),
            }],
        },
    }
}

pub fn array_helper_fn_name(prefix: &str, ty: &tast::Ty) -> String {
    format!("{}__{}", prefix, go_ident(&encode_ty(ty)))
}

pub fn vec_helper_fn_name(prefix: &str, ty: &tast::Ty) -> String {
    format!("{}__{}", prefix, go_ident(&encode_ty(ty)))
}

pub fn ref_helper_fn_name(prefix: &str, ty: &tast::Ty) -> String {
    format!("{}__{}", prefix, go_ident(&encode_ty(ty)))
}

pub fn hashmap_helper_fn_name(prefix: &str, ty: &tast::Ty) -> String {
    format!("{}__{}", prefix, go_ident(&encode_ty(ty)))
}

pub fn hashmap_get_native_helper_fn_name(ty: &tast::Ty) -> String {
    hashmap_helper_fn_name("hashmap_get_native", ty)
}

pub fn missing_helper_fn_name(ty: &tast::Ty) -> String {
    format!("missing__{}", go_ident(&encode_ty(ty)))
}

pub fn make_missing_runtime(missing_types: &IndexSet<tast::Ty>) -> Vec<goast::Item> {
    let mut items = Vec::new();
    for ty in missing_types {
        let ret_go_ty = goast::tast_ty_to_go_type(ty);
        let helper_fn = goast::Fn {
            name: missing_helper_fn_name(ty),
            params: vec![("s".to_string(), goty::GoType::TString)],
            ret_ty: Some(ret_go_ty.clone()),
            body: goast::Block {
                stmts: vec![
                    goast::Stmt::Expr(goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: "missing".to_string(),
                            ty: goty::GoType::TFunc {
                                params: vec![goty::GoType::TString],
                                ret_ty: Box::new(goty::GoType::TUnit),
                            },
                        }),
                        args: vec![goast::Expr::Var {
                            name: "s".to_string(),
                            ty: goty::GoType::TString,
                        }],
                        ty: goty::GoType::TUnit,
                    }),
                    goast::Stmt::VarDecl {
                        name: "ret".to_string(),
                        ty: ret_go_ty.clone(),
                        value: None,
                    },
                    goast::Stmt::Return {
                        expr: Some(goast::Expr::Var {
                            name: "ret".to_string(),
                            ty: ret_go_ty,
                        }),
                    },
                ],
            },
        };
        items.push(goast::Item::Fn(helper_fn));
    }
    items
}

pub fn make_array_runtime(array_types: &IndexSet<tast::Ty>) -> Vec<goast::Item> {
    let mut items = Vec::new();
    for ty in array_types {
        let tast::Ty::TArray { len, elem } = ty else {
            continue;
        };
        if *len == tast::ARRAY_WILDCARD_LEN {
            continue;
        }
        let arr_go_ty = goty::GoType::TArray {
            len: *len,
            elem: Box::new(goast::tast_ty_to_go_type(elem)),
        };
        let elem_go_ty = goast::tast_ty_to_go_type(elem);

        let get_fn = goast::Fn {
            name: array_helper_fn_name("array_get", ty),
            params: vec![
                ("arr".to_string(), arr_go_ty.clone()),
                ("index".to_string(), goty::GoType::TInt32),
            ],
            ret_ty: Some(elem_go_ty.clone()),
            body: goast::Block {
                stmts: vec![goast::Stmt::Return {
                    expr: Some(goast::Expr::Index {
                        array: Box::new(goast::Expr::Var {
                            name: "arr".to_string(),
                            ty: arr_go_ty.clone(),
                        }),
                        index: Box::new(goast::Expr::Var {
                            name: "index".to_string(),
                            ty: goty::GoType::TInt32,
                        }),
                        ty: elem_go_ty.clone(),
                    }),
                }],
            },
        };

        let set_fn = goast::Fn {
            name: array_helper_fn_name("array_set", ty),
            params: vec![
                ("arr".to_string(), arr_go_ty.clone()),
                ("index".to_string(), goty::GoType::TInt32),
                ("value".to_string(), elem_go_ty.clone()),
            ],
            ret_ty: Some(arr_go_ty.clone()),
            body: goast::Block {
                stmts: vec![
                    goast::Stmt::IndexAssign {
                        array: goast::Expr::Var {
                            name: "arr".to_string(),
                            ty: arr_go_ty.clone(),
                        },
                        index: goast::Expr::Var {
                            name: "index".to_string(),
                            ty: goty::GoType::TInt32,
                        },
                        value: goast::Expr::Var {
                            name: "value".to_string(),
                            ty: elem_go_ty.clone(),
                        },
                    },
                    goast::Stmt::Return {
                        expr: Some(goast::Expr::Var {
                            name: "arr".to_string(),
                            ty: arr_go_ty.clone(),
                        }),
                    },
                ],
            },
        };

        items.push(goast::Item::Fn(get_fn));
        items.push(goast::Item::Fn(set_fn));
    }

    items
}

pub fn make_vec_runtime(vec_types: &IndexSet<tast::Ty>) -> Vec<goast::Item> {
    let mut items = Vec::new();
    for ty in vec_types {
        let tast::Ty::TVec { elem } = ty else {
            continue;
        };
        if ty_contains_type_param(elem) {
            continue;
        }

        let vec_go_ty = goast::tast_ty_to_go_type(ty);
        let elem_go_ty = goast::tast_ty_to_go_type(elem);

        let set_fn = goast::Fn {
            name: vec_helper_fn_name("vec_set", ty),
            params: vec![
                ("vec".to_string(), vec_go_ty.clone()),
                ("index".to_string(), goty::GoType::TInt32),
                ("value".to_string(), elem_go_ty.clone()),
            ],
            ret_ty: Some(goty::GoType::TUnit),
            body: goast::Block {
                stmts: vec![
                    goast::Stmt::IndexAssign {
                        array: goast::Expr::Var {
                            name: "vec".to_string(),
                            ty: vec_go_ty.clone(),
                        },
                        index: goast::Expr::Var {
                            name: "index".to_string(),
                            ty: goty::GoType::TInt32,
                        },
                        value: goast::Expr::Var {
                            name: "value".to_string(),
                            ty: elem_go_ty,
                        },
                    },
                    goast::Stmt::Return {
                        expr: Some(goast::Expr::Unit {
                            ty: goty::GoType::TUnit,
                        }),
                    },
                ],
            },
        };

        items.push(goast::Item::Fn(set_fn));
    }
    items
}

pub fn make_ref_runtime(ref_types: &IndexSet<tast::Ty>) -> Vec<goast::Item> {
    let mut items = Vec::new();
    for ty in ref_types {
        let tast::Ty::TRef { elem } = ty else {
            continue;
        };

        if ty_contains_type_param(elem) {
            continue;
        }

        let struct_name = goast::ref_struct_name(elem);
        let struct_go_ty = goty::GoType::TName {
            name: struct_name.clone(),
        };
        let ref_go_ty = goty::GoType::TPointer {
            elem: Box::new(struct_go_ty.clone()),
        };
        let elem_go_ty = goast::tast_ty_to_go_type(elem);

        items.push(goast::Item::Struct(goast::Struct {
            name: struct_name,
            fields: vec![goast::Field {
                name: "value".to_string(),
                ty: elem_go_ty.clone(),
            }],
            methods: vec![],
        }));

        let new_fn = goast::Fn {
            name: ref_helper_fn_name("ref", ty),
            params: vec![("value".to_string(), elem_go_ty.clone())],
            ret_ty: Some(ref_go_ty.clone()),
            body: goast::Block {
                stmts: vec![goast::Stmt::Return {
                    expr: Some(goast::Expr::UnaryOp {
                        op: goast::GoUnaryOp::AddrOf,
                        expr: Box::new(goast::Expr::StructLiteral {
                            fields: vec![(
                                "value".to_string(),
                                goast::Expr::Var {
                                    name: "value".to_string(),
                                    ty: elem_go_ty.clone(),
                                },
                            )],
                            ty: struct_go_ty.clone(),
                        }),
                        ty: ref_go_ty.clone(),
                    }),
                }],
            },
        };

        let get_fn = goast::Fn {
            name: ref_helper_fn_name("ref_get", ty),
            params: vec![("reference".to_string(), ref_go_ty.clone())],
            ret_ty: Some(elem_go_ty.clone()),
            body: goast::Block {
                stmts: vec![goast::Stmt::Return {
                    expr: Some(goast::Expr::FieldAccess {
                        obj: Box::new(goast::Expr::Var {
                            name: "reference".to_string(),
                            ty: ref_go_ty.clone(),
                        }),
                        field: "value".to_string(),
                        ty: elem_go_ty.clone(),
                    }),
                }],
            },
        };

        let set_fn = goast::Fn {
            name: ref_helper_fn_name("ref_set", ty),
            params: vec![
                ("reference".to_string(), ref_go_ty.clone()),
                ("value".to_string(), elem_go_ty.clone()),
            ],
            ret_ty: Some(goty::GoType::TUnit),
            body: goast::Block {
                stmts: vec![
                    goast::Stmt::FieldAssign {
                        target: goast::Expr::FieldAccess {
                            obj: Box::new(goast::Expr::Var {
                                name: "reference".to_string(),
                                ty: ref_go_ty.clone(),
                            }),
                            field: "value".to_string(),
                            ty: elem_go_ty.clone(),
                        },
                        value: goast::Expr::Var {
                            name: "value".to_string(),
                            ty: elem_go_ty.clone(),
                        },
                    },
                    goast::Stmt::Return {
                        expr: Some(goast::Expr::Unit {
                            ty: goty::GoType::TUnit,
                        }),
                    },
                ],
            },
        };

        let ptr_eq_fn = goast::Fn {
            name: ref_helper_fn_name("ptr_eq", ty),
            params: vec![
                ("a".to_string(), ref_go_ty.clone()),
                ("b".to_string(), ref_go_ty.clone()),
            ],
            ret_ty: Some(goty::GoType::TBool),
            body: goast::Block {
                stmts: vec![goast::Stmt::Return {
                    expr: Some(goast::Expr::BinaryOp {
                        op: GoBinaryOp::Eq,
                        lhs: Box::new(goast::Expr::Var {
                            name: "a".to_string(),
                            ty: ref_go_ty.clone(),
                        }),
                        rhs: Box::new(goast::Expr::Var {
                            name: "b".to_string(),
                            ty: ref_go_ty.clone(),
                        }),
                        ty: goty::GoType::TBool,
                    }),
                }],
            },
        };

        items.push(goast::Item::Fn(new_fn));
        items.push(goast::Item::Fn(get_fn));
        items.push(goast::Item::Fn(set_fn));
        items.push(goast::Item::Fn(ptr_eq_fn));

        let eq_name = go_ident(&trait_impl_fn_name(
            &tast::TastIdent("Eq".to_string()),
            ty,
            "eq",
        ));
        let hash_name = go_ident(&trait_impl_fn_name(
            &tast::TastIdent("Hash".to_string()),
            ty,
            "hash",
        ));
        let inner_eq_name = go_ident(&trait_impl_fn_name(
            &tast::TastIdent("Eq".to_string()),
            elem,
            "eq",
        ));
        let inner_hash_name = go_ident(&trait_impl_fn_name(
            &tast::TastIdent("Hash".to_string()),
            elem,
            "hash",
        ));
        let ref_get_name = ref_helper_fn_name("ref_get", ty);
        let inner_eq_fn_ty = goty::GoType::TFunc {
            params: vec![elem_go_ty.clone(), elem_go_ty.clone()],
            ret_ty: Box::new(goty::GoType::TBool),
        };
        let inner_hash_fn_ty = goty::GoType::TFunc {
            params: vec![elem_go_ty.clone()],
            ret_ty: Box::new(goty::GoType::TUint64),
        };
        let ref_get_fn_ty = goty::GoType::TFunc {
            params: vec![ref_go_ty.clone()],
            ret_ty: Box::new(elem_go_ty.clone()),
        };

        items.push(goast::Item::Fn(goast::Fn {
            name: eq_name,
            params: vec![
                ("self".to_string(), ref_go_ty.clone()),
                ("other".to_string(), ref_go_ty.clone()),
            ],
            ret_ty: Some(goty::GoType::TBool),
            body: goast::Block {
                stmts: vec![goast::Stmt::Return {
                    expr: Some(goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: inner_eq_name,
                            ty: inner_eq_fn_ty.clone(),
                        }),
                        args: vec![
                            goast::Expr::Call {
                                func: Box::new(goast::Expr::Var {
                                    name: ref_get_name.clone(),
                                    ty: ref_get_fn_ty.clone(),
                                }),
                                args: vec![goast::Expr::Var {
                                    name: "self".to_string(),
                                    ty: ref_go_ty.clone(),
                                }],
                                ty: elem_go_ty.clone(),
                            },
                            goast::Expr::Call {
                                func: Box::new(goast::Expr::Var {
                                    name: ref_get_name.clone(),
                                    ty: ref_get_fn_ty.clone(),
                                }),
                                args: vec![goast::Expr::Var {
                                    name: "other".to_string(),
                                    ty: ref_go_ty.clone(),
                                }],
                                ty: elem_go_ty.clone(),
                            },
                        ],
                        ty: goty::GoType::TBool,
                    }),
                }],
            },
        }));

        items.push(goast::Item::Fn(goast::Fn {
            name: hash_name,
            params: vec![("self".to_string(), ref_go_ty.clone())],
            ret_ty: Some(goty::GoType::TUint64),
            body: goast::Block {
                stmts: vec![goast::Stmt::Return {
                    expr: Some(goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: inner_hash_name,
                            ty: inner_hash_fn_ty.clone(),
                        }),
                        args: vec![goast::Expr::Call {
                            func: Box::new(goast::Expr::Var {
                                name: ref_get_name,
                                ty: ref_get_fn_ty,
                            }),
                            args: vec![goast::Expr::Var {
                                name: "self".to_string(),
                                ty: ref_go_ty,
                            }],
                            ty: elem_go_ty,
                        }],
                        ty: goty::GoType::TUint64,
                    }),
                }],
            },
        }));
    }

    items
}

fn variant_struct_name(goenv: &GlobalGoEnv, enum_name: &str, variant_name: &str) -> String {
    variant_symbol_name(goenv, enum_name, variant_name)
}

fn synthetic_option_variant_name(option_name: &str, variant_name: &str) -> String {
    go_generated_ident(&format!("_goml_synthetic_{}_{}", option_name, variant_name))
}

fn synthetic_option_type_name(option_name: &str) -> String {
    go_generated_ident(&format!("_goml_synthetic_{}", option_name))
}

fn option_variant_go_names(goenv: &GlobalGoEnv, option_name: &str) -> Option<(String, String)> {
    let def = goenv.get_enum(&tast::TastIdent::new(option_name))?;
    let named_some = def
        .variants
        .iter()
        .find(|(name, fields)| name.0 == "Some" && fields.len() == 1)
        .map(|(name, _)| name.0.clone());
    let named_none = def
        .variants
        .iter()
        .find(|(name, fields)| name.0 == "None" && fields.is_empty())
        .map(|(name, _)| name.0.clone());
    if let (Some(some_variant), Some(none_variant)) = (named_some, named_none) {
        return Some((
            variant_struct_name(goenv, option_name, &some_variant),
            variant_struct_name(goenv, option_name, &none_variant),
        ));
    }

    let mut some_variants = Vec::new();
    let mut none_variants = Vec::new();

    for (name, fields) in &def.variants {
        if fields.len() == 1 {
            some_variants.push(name.0.clone());
        } else if fields.is_empty() {
            none_variants.push(name.0.clone());
        }
    }

    if some_variants.len() != 1 || none_variants.len() != 1 {
        return None;
    }

    let some_variant = &some_variants[0];
    let none_variant = &none_variants[0];
    Some((
        variant_struct_name(goenv, option_name, some_variant),
        variant_struct_name(goenv, option_name, none_variant),
    ))
}

fn make_synthetic_option_runtime(option_name: &str, value_go_ty: goty::GoType) -> Vec<goast::Item> {
    let option_go_name = synthetic_option_type_name(option_name);
    let type_identifier_method = format!("is{}", option_go_name);
    let some_go_name = synthetic_option_variant_name(option_name, "Some");
    let none_go_name = synthetic_option_variant_name(option_name, "None");

    vec![
        goast::Item::Interface(goast::Interface {
            name: option_go_name,
            methods: vec![goast::MethodElem {
                name: type_identifier_method.clone(),
                params: vec![],
                ret: None,
            }],
        }),
        goast::Item::Struct(goast::Struct {
            name: some_go_name.clone(),
            fields: vec![goast::Field {
                name: "_0".to_string(),
                ty: value_go_ty,
            }],
            methods: vec![goast::Method {
                receiver: goast::Receiver {
                    name: "_".to_string(),
                    ty: goty::GoType::TName { name: some_go_name },
                },
                name: type_identifier_method.clone(),
                params: vec![],
                body: goast::Block { stmts: vec![] },
            }],
        }),
        goast::Item::Struct(goast::Struct {
            name: none_go_name.clone(),
            fields: vec![],
            methods: vec![goast::Method {
                receiver: goast::Receiver {
                    name: "_".to_string(),
                    ty: goty::GoType::TName { name: none_go_name },
                },
                name: type_identifier_method,
                params: vec![],
                body: goast::Block { stmts: vec![] },
            }],
        }),
    ]
}

pub fn make_hashmap_runtime(
    goenv: &GlobalGoEnv,
    hashmap_types: &IndexSet<tast::Ty>,
) -> Vec<goast::Item> {
    let mut items = Vec::new();
    let mut synthetic_option_types = IndexSet::new();
    for ty in hashmap_types {
        let tast::Ty::THashMap { key, value } = ty else {
            continue;
        };

        if ty_contains_type_param(key) || ty_contains_type_param(value) {
            continue;
        }

        let map_struct_name = goast::hashmap_struct_name(key, value);
        let entry_struct_name = format!("{}_entry", map_struct_name);

        let key_go_ty = goast::tast_ty_to_go_type(key);
        let value_go_ty = goast::tast_ty_to_go_type(value);
        let entry_go_ty = goty::GoType::TName {
            name: entry_struct_name.clone(),
        };
        let bucket_slice_go_ty = goty::GoType::TSlice {
            elem: Box::new(entry_go_ty.clone()),
        };
        let buckets_go_ty = goty::GoType::TMap {
            key: Box::new(goty::GoType::TUint64),
            value: Box::new(bucket_slice_go_ty.clone()),
        };

        let map_struct_go_ty = goty::GoType::TName {
            name: map_struct_name.clone(),
        };
        let map_ptr_go_ty = goty::GoType::TPointer {
            elem: Box::new(map_struct_go_ty.clone()),
        };

        items.push(goast::Item::Struct(goast::Struct {
            name: entry_struct_name.clone(),
            fields: vec![
                goast::Field {
                    name: "active".to_string(),
                    ty: goty::GoType::TBool,
                },
                goast::Field {
                    name: "key".to_string(),
                    ty: key_go_ty.clone(),
                },
                goast::Field {
                    name: "value".to_string(),
                    ty: value_go_ty.clone(),
                },
            ],
            methods: vec![],
        }));

        items.push(goast::Item::Struct(goast::Struct {
            name: map_struct_name.clone(),
            fields: vec![
                goast::Field {
                    name: "buckets".to_string(),
                    ty: buckets_go_ty.clone(),
                },
                goast::Field {
                    name: "len".to_string(),
                    ty: goty::GoType::TInt32,
                },
            ],
            methods: vec![],
        }));

        let hash_impl = go_ident(&trait_impl_fn_name(
            &tast::TastIdent("Hash".to_string()),
            key,
            "hash",
        ));
        let eq_impl = go_ident(&trait_impl_fn_name(
            &tast::TastIdent("Eq".to_string()),
            key,
            "eq",
        ));

        let hash_fn_ty = goty::GoType::TFunc {
            params: vec![key_go_ty.clone()],
            ret_ty: Box::new(goty::GoType::TUint64),
        };
        let eq_fn_ty = goty::GoType::TFunc {
            params: vec![key_go_ty.clone(), key_go_ty.clone()],
            ret_ty: Box::new(goty::GoType::TBool),
        };

        let new_fn = goast::Fn {
            name: hashmap_helper_fn_name("hashmap_new", ty),
            params: vec![],
            ret_ty: Some(map_ptr_go_ty.clone()),
            body: goast::Block {
                stmts: vec![goast::Stmt::Return {
                    expr: Some(goast::Expr::UnaryOp {
                        op: goast::GoUnaryOp::AddrOf,
                        expr: Box::new(goast::Expr::StructLiteral {
                            fields: vec![
                                (
                                    "buckets".to_string(),
                                    goast::Expr::Make {
                                        ty: buckets_go_ty.clone(),
                                    },
                                ),
                                (
                                    "len".to_string(),
                                    goast::Expr::Int {
                                        value: "0".to_string(),
                                        ty: goty::GoType::TInt32,
                                    },
                                ),
                            ],
                            ty: map_struct_go_ty.clone(),
                        }),
                        ty: map_ptr_go_ty.clone(),
                    }),
                }],
            },
        };

        let len_fn = goast::Fn {
            name: hashmap_helper_fn_name("hashmap_len", ty),
            params: vec![("m".to_string(), map_ptr_go_ty.clone())],
            ret_ty: Some(goty::GoType::TInt32),
            body: goast::Block {
                stmts: vec![
                    goast::Stmt::If {
                        cond: goast::Expr::BinaryOp {
                            op: GoBinaryOp::Eq,
                            lhs: Box::new(goast::Expr::Var {
                                name: "m".to_string(),
                                ty: map_ptr_go_ty.clone(),
                            }),
                            rhs: Box::new(goast::Expr::Nil {
                                ty: map_ptr_go_ty.clone(),
                            }),
                            ty: goty::GoType::TBool,
                        },
                        then: goast::Block {
                            stmts: vec![goast::Stmt::Return {
                                expr: Some(goast::Expr::Int {
                                    value: "0".to_string(),
                                    ty: goty::GoType::TInt32,
                                }),
                            }],
                        },
                        else_: None,
                    },
                    goast::Stmt::Return {
                        expr: Some(goast::Expr::FieldAccess {
                            obj: Box::new(goast::Expr::Var {
                                name: "m".to_string(),
                                ty: map_ptr_go_ty.clone(),
                            }),
                            field: "len".to_string(),
                            ty: goty::GoType::TInt32,
                        }),
                    },
                ],
            },
        };

        let contains_fn = goast::Fn {
            name: hashmap_helper_fn_name("hashmap_contains", ty),
            params: vec![
                ("m".to_string(), map_ptr_go_ty.clone()),
                ("key".to_string(), key_go_ty.clone()),
            ],
            ret_ty: Some(goty::GoType::TBool),
            body: goast::Block {
                stmts: {
                    let mut stmts = Vec::new();
                    stmts.push(goast::Stmt::If {
                        cond: goast::Expr::BinaryOp {
                            op: GoBinaryOp::Eq,
                            lhs: Box::new(goast::Expr::Var {
                                name: "m".to_string(),
                                ty: map_ptr_go_ty.clone(),
                            }),
                            rhs: Box::new(goast::Expr::Nil {
                                ty: map_ptr_go_ty.clone(),
                            }),
                            ty: goty::GoType::TBool,
                        },
                        then: goast::Block {
                            stmts: vec![goast::Stmt::Return {
                                expr: Some(goast::Expr::Bool {
                                    value: false,
                                    ty: goty::GoType::TBool,
                                }),
                            }],
                        },
                        else_: None,
                    });

                    stmts.push(goast::Stmt::VarDecl {
                        name: "h".to_string(),
                        ty: goty::GoType::TUint64,
                        value: Some(goast::Expr::Call {
                            func: Box::new(goast::Expr::Var {
                                name: hash_impl.clone(),
                                ty: hash_fn_ty.clone(),
                            }),
                            args: vec![goast::Expr::Var {
                                name: "key".to_string(),
                                ty: key_go_ty.clone(),
                            }],
                            ty: goty::GoType::TUint64,
                        }),
                    });

                    let buckets_expr = goast::Expr::FieldAccess {
                        obj: Box::new(goast::Expr::Var {
                            name: "m".to_string(),
                            ty: map_ptr_go_ty.clone(),
                        }),
                        field: "buckets".to_string(),
                        ty: buckets_go_ty.clone(),
                    };

                    stmts.push(goast::Stmt::VarDecl {
                        name: "bucket".to_string(),
                        ty: bucket_slice_go_ty.clone(),
                        value: Some(goast::Expr::Index {
                            array: Box::new(buckets_expr),
                            index: Box::new(goast::Expr::Var {
                                name: "h".to_string(),
                                ty: goty::GoType::TUint64,
                            }),
                            ty: bucket_slice_go_ty.clone(),
                        }),
                    });

                    stmts.push(goast::Stmt::VarDecl {
                        name: "i".to_string(),
                        ty: goty::GoType::TInt32,
                        value: Some(goast::Expr::Int {
                            value: "0".to_string(),
                            ty: goty::GoType::TInt32,
                        }),
                    });

                    let len_bucket = goast::Expr::Call {
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
                                    params: vec![bucket_slice_go_ty.clone()],
                                    ret_ty: Box::new(goty::GoType::TInt32),
                                },
                            }),
                            args: vec![goast::Expr::Var {
                                name: "bucket".to_string(),
                                ty: bucket_slice_go_ty.clone(),
                            }],
                            ty: goty::GoType::TInt32,
                        }],
                        ty: goty::GoType::TInt32,
                    };

                    let entry_at_i = goast::Expr::Index {
                        array: Box::new(goast::Expr::Var {
                            name: "bucket".to_string(),
                            ty: bucket_slice_go_ty.clone(),
                        }),
                        index: Box::new(goast::Expr::Var {
                            name: "i".to_string(),
                            ty: goty::GoType::TInt32,
                        }),
                        ty: entry_go_ty.clone(),
                    };

                    let entry_active = goast::Expr::FieldAccess {
                        obj: Box::new(goast::Expr::Var {
                            name: "entry".to_string(),
                            ty: entry_go_ty.clone(),
                        }),
                        field: "active".to_string(),
                        ty: goty::GoType::TBool,
                    };
                    let entry_key = goast::Expr::FieldAccess {
                        obj: Box::new(goast::Expr::Var {
                            name: "entry".to_string(),
                            ty: entry_go_ty.clone(),
                        }),
                        field: "key".to_string(),
                        ty: key_go_ty.clone(),
                    };

                    let eq_call = goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: eq_impl.clone(),
                            ty: eq_fn_ty.clone(),
                        }),
                        args: vec![
                            entry_key,
                            goast::Expr::Var {
                                name: "key".to_string(),
                                ty: key_go_ty.clone(),
                            },
                        ],
                        ty: goty::GoType::TBool,
                    };

                    let cond = goast::Expr::BinaryOp {
                        op: GoBinaryOp::And,
                        lhs: Box::new(entry_active),
                        rhs: Box::new(eq_call),
                        ty: goty::GoType::TBool,
                    };

                    let body = goast::Block {
                        stmts: vec![
                            goast::Stmt::If {
                                cond: goast::Expr::BinaryOp {
                                    op: GoBinaryOp::GreaterEq,
                                    lhs: Box::new(goast::Expr::Var {
                                        name: "i".to_string(),
                                        ty: goty::GoType::TInt32,
                                    }),
                                    rhs: Box::new(len_bucket),
                                    ty: goty::GoType::TBool,
                                },
                                then: goast::Block {
                                    stmts: vec![goast::Stmt::Break],
                                },
                                else_: None,
                            },
                            goast::Stmt::VarDecl {
                                name: "entry".to_string(),
                                ty: entry_go_ty.clone(),
                                value: Some(entry_at_i),
                            },
                            goast::Stmt::If {
                                cond,
                                then: goast::Block {
                                    stmts: vec![goast::Stmt::Return {
                                        expr: Some(goast::Expr::Bool {
                                            value: true,
                                            ty: goty::GoType::TBool,
                                        }),
                                    }],
                                },
                                else_: None,
                            },
                            goast::Stmt::Assignment {
                                name: "i".to_string(),
                                value: goast::Expr::BinaryOp {
                                    op: GoBinaryOp::Add,
                                    lhs: Box::new(goast::Expr::Var {
                                        name: "i".to_string(),
                                        ty: goty::GoType::TInt32,
                                    }),
                                    rhs: Box::new(goast::Expr::Int {
                                        value: "1".to_string(),
                                        ty: goty::GoType::TInt32,
                                    }),
                                    ty: goty::GoType::TInt32,
                                },
                            },
                        ],
                    };
                    stmts.push(goast::Stmt::Loop { body, label: None });

                    stmts.push(goast::Stmt::Return {
                        expr: Some(goast::Expr::Bool {
                            value: false,
                            ty: goty::GoType::TBool,
                        }),
                    });

                    stmts
                },
            },
        };

        let option_name = format!("Option__{}", ty_compact(value));
        let option_tast_ty = tast::Ty::TEnum {
            name: option_name.clone(),
        };
        let has_option_def = goenv
            .get_enum(&tast::TastIdent::new(&option_name))
            .is_some();
        let option_go_ty = if has_option_def {
            goast::tast_ty_to_go_type(&option_tast_ty)
        } else {
            goty::GoType::TName {
                name: synthetic_option_type_name(&option_name),
            }
        };
        let option_variant_names = option_variant_go_names(goenv, &option_name);
        if !has_option_def && synthetic_option_types.insert(option_name.clone()) {
            items.extend(make_synthetic_option_runtime(
                &option_name,
                value_go_ty.clone(),
            ));
        }
        let (option_some_go_name, option_none_go_name) =
            option_variant_names.unwrap_or_else(|| {
                (
                    synthetic_option_variant_name(&option_name, "Some"),
                    synthetic_option_variant_name(&option_name, "None"),
                )
            });
        let option_some_go_ty = goty::GoType::TName {
            name: option_some_go_name.clone(),
        };
        let option_none_go_ty = goty::GoType::TName {
            name: option_none_go_name.clone(),
        };

        let get_native_fn = goast::Fn {
            name: hashmap_get_native_helper_fn_name(ty),
            params: vec![
                ("m".to_string(), map_ptr_go_ty.clone()),
                ("key".to_string(), key_go_ty.clone()),
            ],
            ret_ty: Some(goty::GoType::TMulti {
                elems: vec![value_go_ty.clone(), goty::GoType::TBool],
            }),
            body: goast::Block {
                stmts: {
                    let mut stmts = Vec::new();
                    stmts.push(goast::Stmt::If {
                        cond: goast::Expr::BinaryOp {
                            op: GoBinaryOp::Eq,
                            lhs: Box::new(goast::Expr::Var {
                                name: "m".to_string(),
                                ty: map_ptr_go_ty.clone(),
                            }),
                            rhs: Box::new(goast::Expr::Nil {
                                ty: map_ptr_go_ty.clone(),
                            }),
                            ty: goty::GoType::TBool,
                        },
                        then: goast::Block {
                            stmts: vec![
                                goast::Stmt::VarDecl {
                                    name: "zero".to_string(),
                                    ty: value_go_ty.clone(),
                                    value: None,
                                },
                                goast::Stmt::ReturnMulti {
                                    exprs: vec![
                                        goast::Expr::Var {
                                            name: "zero".to_string(),
                                            ty: value_go_ty.clone(),
                                        },
                                        goast::Expr::Bool {
                                            value: false,
                                            ty: goty::GoType::TBool,
                                        },
                                    ],
                                },
                            ],
                        },
                        else_: None,
                    });

                    stmts.push(goast::Stmt::VarDecl {
                        name: "h".to_string(),
                        ty: goty::GoType::TUint64,
                        value: Some(goast::Expr::Call {
                            func: Box::new(goast::Expr::Var {
                                name: hash_impl.clone(),
                                ty: hash_fn_ty.clone(),
                            }),
                            args: vec![goast::Expr::Var {
                                name: "key".to_string(),
                                ty: key_go_ty.clone(),
                            }],
                            ty: goty::GoType::TUint64,
                        }),
                    });

                    let buckets_expr = goast::Expr::FieldAccess {
                        obj: Box::new(goast::Expr::Var {
                            name: "m".to_string(),
                            ty: map_ptr_go_ty.clone(),
                        }),
                        field: "buckets".to_string(),
                        ty: buckets_go_ty.clone(),
                    };

                    stmts.push(goast::Stmt::VarDecl {
                        name: "bucket".to_string(),
                        ty: bucket_slice_go_ty.clone(),
                        value: Some(goast::Expr::Index {
                            array: Box::new(buckets_expr),
                            index: Box::new(goast::Expr::Var {
                                name: "h".to_string(),
                                ty: goty::GoType::TUint64,
                            }),
                            ty: bucket_slice_go_ty.clone(),
                        }),
                    });

                    stmts.push(goast::Stmt::VarDecl {
                        name: "i".to_string(),
                        ty: goty::GoType::TInt32,
                        value: Some(goast::Expr::Int {
                            value: "0".to_string(),
                            ty: goty::GoType::TInt32,
                        }),
                    });

                    let len_bucket = goast::Expr::Call {
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
                                    params: vec![bucket_slice_go_ty.clone()],
                                    ret_ty: Box::new(goty::GoType::TInt32),
                                },
                            }),
                            args: vec![goast::Expr::Var {
                                name: "bucket".to_string(),
                                ty: bucket_slice_go_ty.clone(),
                            }],
                            ty: goty::GoType::TInt32,
                        }],
                        ty: goty::GoType::TInt32,
                    };

                    let entry_at_i = goast::Expr::Index {
                        array: Box::new(goast::Expr::Var {
                            name: "bucket".to_string(),
                            ty: bucket_slice_go_ty.clone(),
                        }),
                        index: Box::new(goast::Expr::Var {
                            name: "i".to_string(),
                            ty: goty::GoType::TInt32,
                        }),
                        ty: entry_go_ty.clone(),
                    };

                    let entry_active = goast::Expr::FieldAccess {
                        obj: Box::new(goast::Expr::Var {
                            name: "entry".to_string(),
                            ty: entry_go_ty.clone(),
                        }),
                        field: "active".to_string(),
                        ty: goty::GoType::TBool,
                    };
                    let entry_key = goast::Expr::FieldAccess {
                        obj: Box::new(goast::Expr::Var {
                            name: "entry".to_string(),
                            ty: entry_go_ty.clone(),
                        }),
                        field: "key".to_string(),
                        ty: key_go_ty.clone(),
                    };
                    let entry_value = goast::Expr::FieldAccess {
                        obj: Box::new(goast::Expr::Var {
                            name: "entry".to_string(),
                            ty: entry_go_ty.clone(),
                        }),
                        field: "value".to_string(),
                        ty: value_go_ty.clone(),
                    };

                    let eq_call = goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: eq_impl.clone(),
                            ty: eq_fn_ty.clone(),
                        }),
                        args: vec![
                            entry_key,
                            goast::Expr::Var {
                                name: "key".to_string(),
                                ty: key_go_ty.clone(),
                            },
                        ],
                        ty: goty::GoType::TBool,
                    };

                    let cond = goast::Expr::BinaryOp {
                        op: GoBinaryOp::And,
                        lhs: Box::new(entry_active),
                        rhs: Box::new(eq_call),
                        ty: goty::GoType::TBool,
                    };

                    let body = goast::Block {
                        stmts: vec![
                            goast::Stmt::If {
                                cond: goast::Expr::BinaryOp {
                                    op: GoBinaryOp::GreaterEq,
                                    lhs: Box::new(goast::Expr::Var {
                                        name: "i".to_string(),
                                        ty: goty::GoType::TInt32,
                                    }),
                                    rhs: Box::new(len_bucket),
                                    ty: goty::GoType::TBool,
                                },
                                then: goast::Block {
                                    stmts: vec![goast::Stmt::Break],
                                },
                                else_: None,
                            },
                            goast::Stmt::VarDecl {
                                name: "entry".to_string(),
                                ty: entry_go_ty.clone(),
                                value: Some(entry_at_i),
                            },
                            goast::Stmt::If {
                                cond,
                                then: goast::Block {
                                    stmts: vec![goast::Stmt::ReturnMulti {
                                        exprs: vec![
                                            entry_value,
                                            goast::Expr::Bool {
                                                value: true,
                                                ty: goty::GoType::TBool,
                                            },
                                        ],
                                    }],
                                },
                                else_: None,
                            },
                            goast::Stmt::Assignment {
                                name: "i".to_string(),
                                value: goast::Expr::BinaryOp {
                                    op: GoBinaryOp::Add,
                                    lhs: Box::new(goast::Expr::Var {
                                        name: "i".to_string(),
                                        ty: goty::GoType::TInt32,
                                    }),
                                    rhs: Box::new(goast::Expr::Int {
                                        value: "1".to_string(),
                                        ty: goty::GoType::TInt32,
                                    }),
                                    ty: goty::GoType::TInt32,
                                },
                            },
                        ],
                    };
                    stmts.push(goast::Stmt::Loop { body, label: None });

                    stmts.push(goast::Stmt::VarDecl {
                        name: "zero".to_string(),
                        ty: value_go_ty.clone(),
                        value: None,
                    });
                    stmts.push(goast::Stmt::ReturnMulti {
                        exprs: vec![
                            goast::Expr::Var {
                                name: "zero".to_string(),
                                ty: value_go_ty.clone(),
                            },
                            goast::Expr::Bool {
                                value: false,
                                ty: goty::GoType::TBool,
                            },
                        ],
                    });
                    stmts
                },
            },
        };

        let get_fn = goast::Fn {
            name: hashmap_helper_fn_name("hashmap_get", ty),
            params: vec![
                ("m".to_string(), map_ptr_go_ty.clone()),
                ("key".to_string(), key_go_ty.clone()),
            ],
            ret_ty: Some(option_go_ty.clone()),
            body: goast::Block {
                stmts: vec![
                    goast::Stmt::VarDecl {
                        name: "value".to_string(),
                        ty: value_go_ty.clone(),
                        value: None,
                    },
                    goast::Stmt::VarDecl {
                        name: "ok".to_string(),
                        ty: goty::GoType::TBool,
                        value: None,
                    },
                    goast::Stmt::MultiAssignment {
                        names: vec!["value".to_string(), "ok".to_string()],
                        value: goast::Expr::Call {
                            func: Box::new(goast::Expr::Var {
                                name: hashmap_get_native_helper_fn_name(ty),
                                ty: goty::GoType::TFunc {
                                    params: vec![map_ptr_go_ty.clone(), key_go_ty.clone()],
                                    ret_ty: Box::new(goty::GoType::TMulti {
                                        elems: vec![value_go_ty.clone(), goty::GoType::TBool],
                                    }),
                                },
                            }),
                            args: vec![
                                goast::Expr::Var {
                                    name: "m".to_string(),
                                    ty: map_ptr_go_ty.clone(),
                                },
                                goast::Expr::Var {
                                    name: "key".to_string(),
                                    ty: key_go_ty.clone(),
                                },
                            ],
                            ty: goty::GoType::TMulti {
                                elems: vec![value_go_ty.clone(), goty::GoType::TBool],
                            },
                        },
                    },
                    goast::Stmt::If {
                        cond: goast::Expr::Var {
                            name: "ok".to_string(),
                            ty: goty::GoType::TBool,
                        },
                        then: goast::Block {
                            stmts: vec![goast::Stmt::Return {
                                expr: Some(goast::Expr::StructLiteral {
                                    fields: vec![(
                                        "_0".to_string(),
                                        goast::Expr::Var {
                                            name: "value".to_string(),
                                            ty: value_go_ty.clone(),
                                        },
                                    )],
                                    ty: option_some_go_ty.clone(),
                                }),
                            }],
                        },
                        else_: None,
                    },
                    goast::Stmt::Return {
                        expr: Some(goast::Expr::StructLiteral {
                            fields: vec![],
                            ty: option_none_go_ty.clone(),
                        }),
                    },
                ],
            },
        };

        let set_fn = goast::Fn {
            name: hashmap_helper_fn_name("hashmap_set", ty),
            params: vec![
                ("m".to_string(), map_ptr_go_ty.clone()),
                ("key".to_string(), key_go_ty.clone()),
                ("value".to_string(), value_go_ty.clone()),
            ],
            ret_ty: Some(goty::GoType::TUnit),
            body: goast::Block {
                stmts: {
                    let mut stmts = Vec::new();
                    stmts.push(goast::Stmt::If {
                        cond: goast::Expr::BinaryOp {
                            op: GoBinaryOp::Eq,
                            lhs: Box::new(goast::Expr::Var {
                                name: "m".to_string(),
                                ty: map_ptr_go_ty.clone(),
                            }),
                            rhs: Box::new(goast::Expr::Nil {
                                ty: map_ptr_go_ty.clone(),
                            }),
                            ty: goty::GoType::TBool,
                        },
                        then: goast::Block {
                            stmts: vec![goast::Stmt::Return {
                                expr: Some(goast::Expr::Unit {
                                    ty: goty::GoType::TUnit,
                                }),
                            }],
                        },
                        else_: None,
                    });

                    stmts.push(goast::Stmt::VarDecl {
                        name: "h".to_string(),
                        ty: goty::GoType::TUint64,
                        value: Some(goast::Expr::Call {
                            func: Box::new(goast::Expr::Var {
                                name: hash_impl.clone(),
                                ty: hash_fn_ty.clone(),
                            }),
                            args: vec![goast::Expr::Var {
                                name: "key".to_string(),
                                ty: key_go_ty.clone(),
                            }],
                            ty: goty::GoType::TUint64,
                        }),
                    });

                    let buckets_expr = goast::Expr::FieldAccess {
                        obj: Box::new(goast::Expr::Var {
                            name: "m".to_string(),
                            ty: map_ptr_go_ty.clone(),
                        }),
                        field: "buckets".to_string(),
                        ty: buckets_go_ty.clone(),
                    };

                    stmts.push(goast::Stmt::VarDecl {
                        name: "bucket".to_string(),
                        ty: bucket_slice_go_ty.clone(),
                        value: Some(goast::Expr::Index {
                            array: Box::new(buckets_expr),
                            index: Box::new(goast::Expr::Var {
                                name: "h".to_string(),
                                ty: goty::GoType::TUint64,
                            }),
                            ty: bucket_slice_go_ty.clone(),
                        }),
                    });

                    stmts.push(goast::Stmt::VarDecl {
                        name: "i".to_string(),
                        ty: goty::GoType::TInt32,
                        value: Some(goast::Expr::Int {
                            value: "0".to_string(),
                            ty: goty::GoType::TInt32,
                        }),
                    });

                    let len_bucket = goast::Expr::Call {
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
                                    params: vec![bucket_slice_go_ty.clone()],
                                    ret_ty: Box::new(goty::GoType::TInt32),
                                },
                            }),
                            args: vec![goast::Expr::Var {
                                name: "bucket".to_string(),
                                ty: bucket_slice_go_ty.clone(),
                            }],
                            ty: goty::GoType::TInt32,
                        }],
                        ty: goty::GoType::TInt32,
                    };

                    let entry_at_i = goast::Expr::Index {
                        array: Box::new(goast::Expr::Var {
                            name: "bucket".to_string(),
                            ty: bucket_slice_go_ty.clone(),
                        }),
                        index: Box::new(goast::Expr::Var {
                            name: "i".to_string(),
                            ty: goty::GoType::TInt32,
                        }),
                        ty: entry_go_ty.clone(),
                    };

                    let entry_active = goast::Expr::FieldAccess {
                        obj: Box::new(goast::Expr::Var {
                            name: "entry".to_string(),
                            ty: entry_go_ty.clone(),
                        }),
                        field: "active".to_string(),
                        ty: goty::GoType::TBool,
                    };
                    let entry_key = goast::Expr::FieldAccess {
                        obj: Box::new(goast::Expr::Var {
                            name: "entry".to_string(),
                            ty: entry_go_ty.clone(),
                        }),
                        field: "key".to_string(),
                        ty: key_go_ty.clone(),
                    };

                    let eq_call = goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: eq_impl.clone(),
                            ty: eq_fn_ty.clone(),
                        }),
                        args: vec![
                            entry_key,
                            goast::Expr::Var {
                                name: "key".to_string(),
                                ty: key_go_ty.clone(),
                            },
                        ],
                        ty: goty::GoType::TBool,
                    };

                    let cond = goast::Expr::BinaryOp {
                        op: GoBinaryOp::And,
                        lhs: Box::new(entry_active),
                        rhs: Box::new(eq_call),
                        ty: goty::GoType::TBool,
                    };

                    let body = goast::Block {
                        stmts: vec![
                            goast::Stmt::If {
                                cond: goast::Expr::BinaryOp {
                                    op: GoBinaryOp::GreaterEq,
                                    lhs: Box::new(goast::Expr::Var {
                                        name: "i".to_string(),
                                        ty: goty::GoType::TInt32,
                                    }),
                                    rhs: Box::new(len_bucket),
                                    ty: goty::GoType::TBool,
                                },
                                then: goast::Block {
                                    stmts: vec![goast::Stmt::Break],
                                },
                                else_: None,
                            },
                            goast::Stmt::VarDecl {
                                name: "entry".to_string(),
                                ty: entry_go_ty.clone(),
                                value: Some(entry_at_i),
                            },
                            goast::Stmt::If {
                                cond,
                                then: goast::Block {
                                    stmts: vec![
                                        goast::Stmt::FieldAssign {
                                            target: goast::Expr::FieldAccess {
                                                obj: Box::new(goast::Expr::Index {
                                                    array: Box::new(goast::Expr::Var {
                                                        name: "bucket".to_string(),
                                                        ty: bucket_slice_go_ty.clone(),
                                                    }),
                                                    index: Box::new(goast::Expr::Var {
                                                        name: "i".to_string(),
                                                        ty: goty::GoType::TInt32,
                                                    }),
                                                    ty: entry_go_ty.clone(),
                                                }),
                                                field: "value".to_string(),
                                                ty: value_go_ty.clone(),
                                            },
                                            value: goast::Expr::Var {
                                                name: "value".to_string(),
                                                ty: value_go_ty.clone(),
                                            },
                                        },
                                        goast::Stmt::Return {
                                            expr: Some(goast::Expr::Unit {
                                                ty: goty::GoType::TUnit,
                                            }),
                                        },
                                    ],
                                },
                                else_: None,
                            },
                            goast::Stmt::Assignment {
                                name: "i".to_string(),
                                value: goast::Expr::BinaryOp {
                                    op: GoBinaryOp::Add,
                                    lhs: Box::new(goast::Expr::Var {
                                        name: "i".to_string(),
                                        ty: goty::GoType::TInt32,
                                    }),
                                    rhs: Box::new(goast::Expr::Int {
                                        value: "1".to_string(),
                                        ty: goty::GoType::TInt32,
                                    }),
                                    ty: goty::GoType::TInt32,
                                },
                            },
                        ],
                    };
                    stmts.push(goast::Stmt::Loop { body, label: None });

                    let entry_lit = goast::Expr::StructLiteral {
                        fields: vec![
                            (
                                "active".to_string(),
                                goast::Expr::Bool {
                                    value: true,
                                    ty: goty::GoType::TBool,
                                },
                            ),
                            (
                                "key".to_string(),
                                goast::Expr::Var {
                                    name: "key".to_string(),
                                    ty: key_go_ty.clone(),
                                },
                            ),
                            (
                                "value".to_string(),
                                goast::Expr::Var {
                                    name: "value".to_string(),
                                    ty: value_go_ty.clone(),
                                },
                            ),
                        ],
                        ty: entry_go_ty.clone(),
                    };

                    let append_fn_ty = goty::GoType::TFunc {
                        params: vec![bucket_slice_go_ty.clone(), entry_go_ty.clone()],
                        ret_ty: Box::new(bucket_slice_go_ty.clone()),
                    };

                    stmts.push(goast::Stmt::Assignment {
                        name: "bucket".to_string(),
                        value: goast::Expr::Call {
                            func: Box::new(goast::Expr::Var {
                                name: "append".to_string(),
                                ty: append_fn_ty,
                            }),
                            args: vec![
                                goast::Expr::Var {
                                    name: "bucket".to_string(),
                                    ty: bucket_slice_go_ty.clone(),
                                },
                                entry_lit,
                            ],
                            ty: bucket_slice_go_ty.clone(),
                        },
                    });

                    let buckets_expr_2 = goast::Expr::FieldAccess {
                        obj: Box::new(goast::Expr::Var {
                            name: "m".to_string(),
                            ty: map_ptr_go_ty.clone(),
                        }),
                        field: "buckets".to_string(),
                        ty: buckets_go_ty.clone(),
                    };
                    stmts.push(goast::Stmt::IndexAssign {
                        array: buckets_expr_2,
                        index: goast::Expr::Var {
                            name: "h".to_string(),
                            ty: goty::GoType::TUint64,
                        },
                        value: goast::Expr::Var {
                            name: "bucket".to_string(),
                            ty: bucket_slice_go_ty.clone(),
                        },
                    });

                    stmts.push(goast::Stmt::FieldAssign {
                        target: goast::Expr::FieldAccess {
                            obj: Box::new(goast::Expr::Var {
                                name: "m".to_string(),
                                ty: map_ptr_go_ty.clone(),
                            }),
                            field: "len".to_string(),
                            ty: goty::GoType::TInt32,
                        },
                        value: goast::Expr::BinaryOp {
                            op: GoBinaryOp::Add,
                            lhs: Box::new(goast::Expr::FieldAccess {
                                obj: Box::new(goast::Expr::Var {
                                    name: "m".to_string(),
                                    ty: map_ptr_go_ty.clone(),
                                }),
                                field: "len".to_string(),
                                ty: goty::GoType::TInt32,
                            }),
                            rhs: Box::new(goast::Expr::Int {
                                value: "1".to_string(),
                                ty: goty::GoType::TInt32,
                            }),
                            ty: goty::GoType::TInt32,
                        },
                    });

                    stmts.push(goast::Stmt::Return {
                        expr: Some(goast::Expr::Unit {
                            ty: goty::GoType::TUnit,
                        }),
                    });
                    stmts
                },
            },
        };

        let remove_fn = goast::Fn {
            name: hashmap_helper_fn_name("hashmap_remove", ty),
            params: vec![
                ("m".to_string(), map_ptr_go_ty.clone()),
                ("key".to_string(), key_go_ty.clone()),
            ],
            ret_ty: Some(goty::GoType::TUnit),
            body: goast::Block {
                stmts: {
                    let mut stmts = Vec::new();
                    stmts.push(goast::Stmt::If {
                        cond: goast::Expr::BinaryOp {
                            op: GoBinaryOp::Eq,
                            lhs: Box::new(goast::Expr::Var {
                                name: "m".to_string(),
                                ty: map_ptr_go_ty.clone(),
                            }),
                            rhs: Box::new(goast::Expr::Nil {
                                ty: map_ptr_go_ty.clone(),
                            }),
                            ty: goty::GoType::TBool,
                        },
                        then: goast::Block {
                            stmts: vec![goast::Stmt::Return {
                                expr: Some(goast::Expr::Unit {
                                    ty: goty::GoType::TUnit,
                                }),
                            }],
                        },
                        else_: None,
                    });

                    stmts.push(goast::Stmt::VarDecl {
                        name: "h".to_string(),
                        ty: goty::GoType::TUint64,
                        value: Some(goast::Expr::Call {
                            func: Box::new(goast::Expr::Var {
                                name: hash_impl.clone(),
                                ty: hash_fn_ty.clone(),
                            }),
                            args: vec![goast::Expr::Var {
                                name: "key".to_string(),
                                ty: key_go_ty.clone(),
                            }],
                            ty: goty::GoType::TUint64,
                        }),
                    });

                    let buckets_expr = goast::Expr::FieldAccess {
                        obj: Box::new(goast::Expr::Var {
                            name: "m".to_string(),
                            ty: map_ptr_go_ty.clone(),
                        }),
                        field: "buckets".to_string(),
                        ty: buckets_go_ty.clone(),
                    };

                    stmts.push(goast::Stmt::VarDecl {
                        name: "bucket".to_string(),
                        ty: bucket_slice_go_ty.clone(),
                        value: Some(goast::Expr::Index {
                            array: Box::new(buckets_expr),
                            index: Box::new(goast::Expr::Var {
                                name: "h".to_string(),
                                ty: goty::GoType::TUint64,
                            }),
                            ty: bucket_slice_go_ty.clone(),
                        }),
                    });

                    stmts.push(goast::Stmt::VarDecl {
                        name: "i".to_string(),
                        ty: goty::GoType::TInt32,
                        value: Some(goast::Expr::Int {
                            value: "0".to_string(),
                            ty: goty::GoType::TInt32,
                        }),
                    });

                    let len_bucket = goast::Expr::Call {
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
                                    params: vec![bucket_slice_go_ty.clone()],
                                    ret_ty: Box::new(goty::GoType::TInt32),
                                },
                            }),
                            args: vec![goast::Expr::Var {
                                name: "bucket".to_string(),
                                ty: bucket_slice_go_ty.clone(),
                            }],
                            ty: goty::GoType::TInt32,
                        }],
                        ty: goty::GoType::TInt32,
                    };

                    let entry_at_i = goast::Expr::Index {
                        array: Box::new(goast::Expr::Var {
                            name: "bucket".to_string(),
                            ty: bucket_slice_go_ty.clone(),
                        }),
                        index: Box::new(goast::Expr::Var {
                            name: "i".to_string(),
                            ty: goty::GoType::TInt32,
                        }),
                        ty: entry_go_ty.clone(),
                    };

                    let entry_active = goast::Expr::FieldAccess {
                        obj: Box::new(goast::Expr::Var {
                            name: "entry".to_string(),
                            ty: entry_go_ty.clone(),
                        }),
                        field: "active".to_string(),
                        ty: goty::GoType::TBool,
                    };
                    let entry_key = goast::Expr::FieldAccess {
                        obj: Box::new(goast::Expr::Var {
                            name: "entry".to_string(),
                            ty: entry_go_ty.clone(),
                        }),
                        field: "key".to_string(),
                        ty: key_go_ty.clone(),
                    };

                    let eq_call = goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: eq_impl.clone(),
                            ty: eq_fn_ty.clone(),
                        }),
                        args: vec![
                            entry_key,
                            goast::Expr::Var {
                                name: "key".to_string(),
                                ty: key_go_ty.clone(),
                            },
                        ],
                        ty: goty::GoType::TBool,
                    };

                    let cond = goast::Expr::BinaryOp {
                        op: GoBinaryOp::And,
                        lhs: Box::new(entry_active),
                        rhs: Box::new(eq_call),
                        ty: goty::GoType::TBool,
                    };

                    let body = goast::Block {
                        stmts: vec![
                            goast::Stmt::If {
                                cond: goast::Expr::BinaryOp {
                                    op: GoBinaryOp::GreaterEq,
                                    lhs: Box::new(goast::Expr::Var {
                                        name: "i".to_string(),
                                        ty: goty::GoType::TInt32,
                                    }),
                                    rhs: Box::new(len_bucket),
                                    ty: goty::GoType::TBool,
                                },
                                then: goast::Block {
                                    stmts: vec![goast::Stmt::Break],
                                },
                                else_: None,
                            },
                            goast::Stmt::VarDecl {
                                name: "entry".to_string(),
                                ty: entry_go_ty.clone(),
                                value: Some(entry_at_i),
                            },
                            goast::Stmt::If {
                                cond,
                                then: goast::Block {
                                    stmts: vec![
                                        goast::Stmt::FieldAssign {
                                            target: goast::Expr::FieldAccess {
                                                obj: Box::new(goast::Expr::Index {
                                                    array: Box::new(goast::Expr::Var {
                                                        name: "bucket".to_string(),
                                                        ty: bucket_slice_go_ty.clone(),
                                                    }),
                                                    index: Box::new(goast::Expr::Var {
                                                        name: "i".to_string(),
                                                        ty: goty::GoType::TInt32,
                                                    }),
                                                    ty: entry_go_ty.clone(),
                                                }),
                                                field: "active".to_string(),
                                                ty: goty::GoType::TBool,
                                            },
                                            value: goast::Expr::Bool {
                                                value: false,
                                                ty: goty::GoType::TBool,
                                            },
                                        },
                                        goast::Stmt::FieldAssign {
                                            target: goast::Expr::FieldAccess {
                                                obj: Box::new(goast::Expr::Var {
                                                    name: "m".to_string(),
                                                    ty: map_ptr_go_ty.clone(),
                                                }),
                                                field: "len".to_string(),
                                                ty: goty::GoType::TInt32,
                                            },
                                            value: goast::Expr::BinaryOp {
                                                op: GoBinaryOp::Sub,
                                                lhs: Box::new(goast::Expr::FieldAccess {
                                                    obj: Box::new(goast::Expr::Var {
                                                        name: "m".to_string(),
                                                        ty: map_ptr_go_ty.clone(),
                                                    }),
                                                    field: "len".to_string(),
                                                    ty: goty::GoType::TInt32,
                                                }),
                                                rhs: Box::new(goast::Expr::Int {
                                                    value: "1".to_string(),
                                                    ty: goty::GoType::TInt32,
                                                }),
                                                ty: goty::GoType::TInt32,
                                            },
                                        },
                                        goast::Stmt::Return {
                                            expr: Some(goast::Expr::Unit {
                                                ty: goty::GoType::TUnit,
                                            }),
                                        },
                                    ],
                                },
                                else_: None,
                            },
                            goast::Stmt::Assignment {
                                name: "i".to_string(),
                                value: goast::Expr::BinaryOp {
                                    op: GoBinaryOp::Add,
                                    lhs: Box::new(goast::Expr::Var {
                                        name: "i".to_string(),
                                        ty: goty::GoType::TInt32,
                                    }),
                                    rhs: Box::new(goast::Expr::Int {
                                        value: "1".to_string(),
                                        ty: goty::GoType::TInt32,
                                    }),
                                    ty: goty::GoType::TInt32,
                                },
                            },
                        ],
                    };
                    stmts.push(goast::Stmt::Loop { body, label: None });

                    stmts.push(goast::Stmt::Return {
                        expr: Some(goast::Expr::Unit {
                            ty: goty::GoType::TUnit,
                        }),
                    });
                    stmts
                },
            },
        };

        items.push(goast::Item::Fn(new_fn));
        items.push(goast::Item::Fn(len_fn));
        items.push(goast::Item::Fn(get_native_fn));
        items.push(goast::Item::Fn(get_fn));
        items.push(goast::Item::Fn(set_fn));
        items.push(goast::Item::Fn(remove_fn));
        items.push(goast::Item::Fn(contains_fn));
    }
    items
}

fn unit_to_string() -> goast::Fn {
    goast::Fn {
        name: "unit_to_string".to_string(),
        params: vec![("x".to_string(), goty::GoType::TUnit)],
        ret_ty: Some(goty::GoType::TString),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::String {
                    value: "()".to_string(),
                    ty: goty::GoType::TString,
                }),
            }],
        },
    }
}

fn bool_to_string() -> goast::Fn {
    goast::Fn {
        name: "bool_to_string".to_string(),
        params: vec![("x".to_string(), goty::GoType::TBool)],
        ret_ty: Some(goty::GoType::TString),
        body: goast::Block {
            stmts: vec![goast::Stmt::If {
                cond: goast::Expr::Var {
                    name: "x".to_string(),
                    ty: goty::GoType::TBool,
                },
                then: goast::Block {
                    stmts: vec![goast::Stmt::Return {
                        expr: Some(goast::Expr::String {
                            value: "true".to_string(),
                            ty: goty::GoType::TString,
                        }),
                    }],
                },
                else_: Some(goast::Block {
                    stmts: vec![goast::Stmt::Return {
                        expr: Some(goast::Expr::String {
                            value: "false".to_string(),
                            ty: goty::GoType::TString,
                        }),
                    }],
                }),
            }],
        },
    }
}

fn bool_to_json() -> goast::Fn {
    // bool_to_json is the same as bool_to_string for JSON (true/false are valid JSON)
    goast::Fn {
        name: "bool_to_json".to_string(),
        params: vec![("x".to_string(), goty::GoType::TBool)],
        ret_ty: Some(goty::GoType::TString),
        body: goast::Block {
            stmts: vec![goast::Stmt::If {
                cond: goast::Expr::Var {
                    name: "x".to_string(),
                    ty: goty::GoType::TBool,
                },
                then: goast::Block {
                    stmts: vec![goast::Stmt::Return {
                        expr: Some(goast::Expr::String {
                            value: "true".to_string(),
                            ty: goty::GoType::TString,
                        }),
                    }],
                },
                else_: Some(goast::Block {
                    stmts: vec![goast::Stmt::Return {
                        expr: Some(goast::Expr::String {
                            value: "false".to_string(),
                            ty: goty::GoType::TString,
                        }),
                    }],
                }),
            }],
        },
    }
}

fn json_escape_string() -> goast::Fn {
    // Returns a JSON-escaped string with surrounding quotes
    // Uses fmt.Sprintf("%q", s) which produces a Go string literal that is JSON-compatible
    let fmt_ty = goty::GoType::TFunc {
        params: vec![goty::GoType::TString, goty::GoType::TString],
        ret_ty: Box::new(goty::GoType::TString),
    };
    goast::Fn {
        name: "json_escape_string".to_string(),
        params: vec![("s".to_string(), goty::GoType::TString)],
        ret_ty: Some(goty::GoType::TString),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "_goml_fmt.Sprintf".to_string(),
                        ty: fmt_ty,
                    }),
                    args: vec![
                        goast::Expr::String {
                            value: "%q".to_string(),
                            ty: goty::GoType::TString,
                        },
                        goast::Expr::Var {
                            name: "s".to_string(),
                            ty: goty::GoType::TString,
                        },
                    ],
                    ty: goty::GoType::TString,
                }),
            }],
        },
    }
}

fn to_string_fn(name: &str, ty: goty::GoType) -> goast::Fn {
    let fmt_spec = match &ty {
        goty::GoType::TFloat32 | goty::GoType::TFloat64 => "%g",
        _ => "%d",
    };
    let fmt_ty = goty::GoType::TFunc {
        params: vec![goty::GoType::TString, ty.clone()],
        ret_ty: Box::new(goty::GoType::TString),
    };
    goast::Fn {
        name: name.to_string(),
        params: vec![("x".to_string(), ty.clone())],
        ret_ty: Some(goty::GoType::TString),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "_goml_fmt.Sprintf".to_string(),
                        ty: fmt_ty,
                    }),
                    args: vec![
                        goast::Expr::String {
                            value: fmt_spec.to_string(),
                            ty: goty::GoType::TString,
                        },
                        goast::Expr::Var {
                            name: "x".to_string(),
                            ty,
                        },
                    ],
                    ty: goty::GoType::TString,
                }),
            }],
        },
    }
}

fn int8_to_string() -> goast::Fn {
    to_string_fn("int8_to_string", goty::GoType::TInt8)
}

fn int16_to_string() -> goast::Fn {
    to_string_fn("int16_to_string", goty::GoType::TInt16)
}

fn int32_to_string() -> goast::Fn {
    to_string_fn("int32_to_string", goty::GoType::TInt32)
}

fn int64_to_string() -> goast::Fn {
    to_string_fn("int64_to_string", goty::GoType::TInt64)
}

fn uint8_to_string() -> goast::Fn {
    to_string_fn("uint8_to_string", goty::GoType::TUint8)
}

fn uint16_to_string() -> goast::Fn {
    to_string_fn("uint16_to_string", goty::GoType::TUint16)
}

fn uint32_to_string() -> goast::Fn {
    to_string_fn("uint32_to_string", goty::GoType::TUint32)
}

fn uint64_to_string() -> goast::Fn {
    to_string_fn("uint64_to_string", goty::GoType::TUint64)
}

fn float32_to_string() -> goast::Fn {
    to_string_fn("float32_to_string", goty::GoType::TFloat32)
}

fn float64_to_string() -> goast::Fn {
    to_string_fn("float64_to_string", goty::GoType::TFloat64)
}

fn char_to_string() -> goast::Fn {
    goast::Fn {
        name: "char_to_string".to_string(),
        params: vec![("x".to_string(), goty::GoType::TChar)],
        ret_ty: Some(goty::GoType::TString),
        body: goast::Block {
            stmts: vec![
                goast::Stmt::If {
                    cond: goast::Expr::UnaryOp {
                        op: goast::GoUnaryOp::Not,
                        expr: Box::new(goast::Expr::Call {
                            func: Box::new(goast::Expr::Var {
                                name: "_goml_utf8.ValidRune".to_string(),
                                ty: goty::GoType::TFunc {
                                    params: vec![goty::GoType::TChar],
                                    ret_ty: Box::new(goty::GoType::TBool),
                                },
                            }),
                            args: vec![goast::Expr::Var {
                                name: "x".to_string(),
                                ty: goty::GoType::TChar,
                            }],
                            ty: goty::GoType::TBool,
                        }),
                        ty: goty::GoType::TBool,
                    },
                    then: goast::Block {
                        stmts: vec![goast::Stmt::Expr(goast::Expr::Call {
                            func: Box::new(goast::Expr::Var {
                                name: "panic".to_string(),
                                ty: goty::GoType::TFunc {
                                    params: vec![goty::GoType::TString],
                                    ret_ty: Box::new(goty::GoType::TVoid),
                                },
                            }),
                            args: vec![goast::Expr::String {
                                value: "invalid char".to_string(),
                                ty: goty::GoType::TString,
                            }],
                            ty: goty::GoType::TVoid,
                        })],
                    },
                    else_: None,
                },
                goast::Stmt::Return {
                    expr: Some(goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: "string".to_string(),
                            ty: goty::GoType::TFunc {
                                params: vec![goty::GoType::TChar],
                                ret_ty: Box::new(goty::GoType::TString),
                            },
                        }),
                        args: vec![goast::Expr::Var {
                            name: "x".to_string(),
                            ty: goty::GoType::TChar,
                        }],
                        ty: goty::GoType::TString,
                    }),
                },
            ],
        },
    }
}

fn int8_hash() -> goast::Fn {
    goast::Fn {
        name: "int8_hash".to_string(),
        params: vec![("x".to_string(), goty::GoType::TInt8)],
        ret_ty: Some(goty::GoType::TUint64),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "uint64".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TInt8],
                            ret_ty: Box::new(goty::GoType::TUint64),
                        },
                    }),
                    args: vec![goast::Expr::Var {
                        name: "x".to_string(),
                        ty: goty::GoType::TInt8,
                    }],
                    ty: goty::GoType::TUint64,
                }),
            }],
        },
    }
}

fn int16_hash() -> goast::Fn {
    goast::Fn {
        name: "int16_hash".to_string(),
        params: vec![("x".to_string(), goty::GoType::TInt16)],
        ret_ty: Some(goty::GoType::TUint64),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "uint64".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TInt16],
                            ret_ty: Box::new(goty::GoType::TUint64),
                        },
                    }),
                    args: vec![goast::Expr::Var {
                        name: "x".to_string(),
                        ty: goty::GoType::TInt16,
                    }],
                    ty: goty::GoType::TUint64,
                }),
            }],
        },
    }
}

fn int32_hash() -> goast::Fn {
    goast::Fn {
        name: "int32_hash".to_string(),
        params: vec![("x".to_string(), goty::GoType::TInt32)],
        ret_ty: Some(goty::GoType::TUint64),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "uint64".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TInt32],
                            ret_ty: Box::new(goty::GoType::TUint64),
                        },
                    }),
                    args: vec![goast::Expr::Var {
                        name: "x".to_string(),
                        ty: goty::GoType::TInt32,
                    }],
                    ty: goty::GoType::TUint64,
                }),
            }],
        },
    }
}

fn char_hash() -> goast::Fn {
    goast::Fn {
        name: "char_hash".to_string(),
        params: vec![("x".to_string(), goty::GoType::TChar)],
        ret_ty: Some(goty::GoType::TUint64),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "uint64".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TChar],
                            ret_ty: Box::new(goty::GoType::TUint64),
                        },
                    }),
                    args: vec![goast::Expr::Var {
                        name: "x".to_string(),
                        ty: goty::GoType::TChar,
                    }],
                    ty: goty::GoType::TUint64,
                }),
            }],
        },
    }
}

fn int64_hash() -> goast::Fn {
    goast::Fn {
        name: "int64_hash".to_string(),
        params: vec![("x".to_string(), goty::GoType::TInt64)],
        ret_ty: Some(goty::GoType::TUint64),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "uint64".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TInt64],
                            ret_ty: Box::new(goty::GoType::TUint64),
                        },
                    }),
                    args: vec![goast::Expr::Var {
                        name: "x".to_string(),
                        ty: goty::GoType::TInt64,
                    }],
                    ty: goty::GoType::TUint64,
                }),
            }],
        },
    }
}

fn uint8_hash() -> goast::Fn {
    goast::Fn {
        name: "uint8_hash".to_string(),
        params: vec![("x".to_string(), goty::GoType::TUint8)],
        ret_ty: Some(goty::GoType::TUint64),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "uint64".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TUint8],
                            ret_ty: Box::new(goty::GoType::TUint64),
                        },
                    }),
                    args: vec![goast::Expr::Var {
                        name: "x".to_string(),
                        ty: goty::GoType::TUint8,
                    }],
                    ty: goty::GoType::TUint64,
                }),
            }],
        },
    }
}

fn uint16_hash() -> goast::Fn {
    goast::Fn {
        name: "uint16_hash".to_string(),
        params: vec![("x".to_string(), goty::GoType::TUint16)],
        ret_ty: Some(goty::GoType::TUint64),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "uint64".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TUint16],
                            ret_ty: Box::new(goty::GoType::TUint64),
                        },
                    }),
                    args: vec![goast::Expr::Var {
                        name: "x".to_string(),
                        ty: goty::GoType::TUint16,
                    }],
                    ty: goty::GoType::TUint64,
                }),
            }],
        },
    }
}

fn uint32_hash() -> goast::Fn {
    goast::Fn {
        name: "uint32_hash".to_string(),
        params: vec![("x".to_string(), goty::GoType::TUint32)],
        ret_ty: Some(goty::GoType::TUint64),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "uint64".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TUint32],
                            ret_ty: Box::new(goty::GoType::TUint64),
                        },
                    }),
                    args: vec![goast::Expr::Var {
                        name: "x".to_string(),
                        ty: goty::GoType::TUint32,
                    }],
                    ty: goty::GoType::TUint64,
                }),
            }],
        },
    }
}

fn float32_hash() -> goast::Fn {
    goast::Fn {
        name: "float32_hash".to_string(),
        params: vec![("x".to_string(), goty::GoType::TFloat32)],
        ret_ty: Some(goty::GoType::TUint64),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "uint64".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TUint32],
                            ret_ty: Box::new(goty::GoType::TUint64),
                        },
                    }),
                    args: vec![goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: "_goml_math.Float32bits".to_string(),
                            ty: goty::GoType::TFunc {
                                params: vec![goty::GoType::TFloat32],
                                ret_ty: Box::new(goty::GoType::TUint32),
                            },
                        }),
                        args: vec![goast::Expr::Var {
                            name: "x".to_string(),
                            ty: goty::GoType::TFloat32,
                        }],
                        ty: goty::GoType::TUint32,
                    }],
                    ty: goty::GoType::TUint64,
                }),
            }],
        },
    }
}

fn float64_hash() -> goast::Fn {
    goast::Fn {
        name: "float64_hash".to_string(),
        params: vec![("x".to_string(), goty::GoType::TFloat64)],
        ret_ty: Some(goty::GoType::TUint64),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "_goml_math.Float64bits".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TFloat64],
                            ret_ty: Box::new(goty::GoType::TUint64),
                        },
                    }),
                    args: vec![goast::Expr::Var {
                        name: "x".to_string(),
                        ty: goty::GoType::TFloat64,
                    }],
                    ty: goty::GoType::TUint64,
                }),
            }],
        },
    }
}

fn string_hash() -> goast::Fn {
    let len_s = goast::Expr::Call {
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
                    params: vec![goty::GoType::TString],
                    ret_ty: Box::new(goty::GoType::TInt32),
                },
            }),
            args: vec![goast::Expr::Var {
                name: "s".to_string(),
                ty: goty::GoType::TString,
            }],
            ty: goty::GoType::TInt32,
        }],
        ty: goty::GoType::TInt32,
    };

    let byte_at_i = goast::Expr::Index {
        array: Box::new(goast::Expr::Var {
            name: "s".to_string(),
            ty: goty::GoType::TString,
        }),
        index: Box::new(goast::Expr::Var {
            name: "i".to_string(),
            ty: goty::GoType::TInt32,
        }),
        ty: goty::GoType::TUint8,
    };

    let h_next = goast::Expr::BinaryOp {
        op: GoBinaryOp::Add,
        lhs: Box::new(goast::Expr::BinaryOp {
            op: GoBinaryOp::Mul,
            lhs: Box::new(goast::Expr::Var {
                name: "h".to_string(),
                ty: goty::GoType::TUint64,
            }),
            rhs: Box::new(goast::Expr::Int {
                value: "1099511628211".to_string(),
                ty: goty::GoType::TUint64,
            }),
            ty: goty::GoType::TUint64,
        }),
        rhs: Box::new(goast::Expr::Call {
            func: Box::new(goast::Expr::Var {
                name: "uint64".to_string(),
                ty: goty::GoType::TFunc {
                    params: vec![goty::GoType::TUint8],
                    ret_ty: Box::new(goty::GoType::TUint64),
                },
            }),
            args: vec![byte_at_i],
            ty: goty::GoType::TUint64,
        }),
        ty: goty::GoType::TUint64,
    };

    goast::Fn {
        name: "string_hash".to_string(),
        params: vec![("s".to_string(), goty::GoType::TString)],
        ret_ty: Some(goty::GoType::TUint64),
        body: goast::Block {
            stmts: vec![
                goast::Stmt::VarDecl {
                    name: "h".to_string(),
                    ty: goty::GoType::TUint64,
                    value: Some(goast::Expr::Int {
                        value: "14695981039346656037".to_string(),
                        ty: goty::GoType::TUint64,
                    }),
                },
                goast::Stmt::VarDecl {
                    name: "i".to_string(),
                    ty: goty::GoType::TInt32,
                    value: Some(goast::Expr::Int {
                        value: "0".to_string(),
                        ty: goty::GoType::TInt32,
                    }),
                },
                goast::Stmt::Loop {
                    body: goast::Block {
                        stmts: vec![
                            goast::Stmt::If {
                                cond: goast::Expr::BinaryOp {
                                    op: GoBinaryOp::GreaterEq,
                                    lhs: Box::new(goast::Expr::Var {
                                        name: "i".to_string(),
                                        ty: goty::GoType::TInt32,
                                    }),
                                    rhs: Box::new(len_s),
                                    ty: goty::GoType::TBool,
                                },
                                then: goast::Block {
                                    stmts: vec![goast::Stmt::Break],
                                },
                                else_: None,
                            },
                            goast::Stmt::Assignment {
                                name: "h".to_string(),
                                value: h_next,
                            },
                            goast::Stmt::Assignment {
                                name: "i".to_string(),
                                value: goast::Expr::BinaryOp {
                                    op: GoBinaryOp::Add,
                                    lhs: Box::new(goast::Expr::Var {
                                        name: "i".to_string(),
                                        ty: goty::GoType::TInt32,
                                    }),
                                    rhs: Box::new(goast::Expr::Int {
                                        value: "1".to_string(),
                                        ty: goty::GoType::TInt32,
                                    }),
                                    ty: goty::GoType::TInt32,
                                },
                            },
                        ],
                    },
                    label: None,
                },
                goast::Stmt::Return {
                    expr: Some(goast::Expr::Var {
                        name: "h".to_string(),
                        ty: goty::GoType::TUint64,
                    }),
                },
            ],
        },
    }
}

fn string_len() -> goast::Fn {
    goast::Fn {
        name: "string_len".to_string(),
        params: vec![("s".to_string(), goty::GoType::TString)],
        ret_ty: Some(goty::GoType::TInt32),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::Call {
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
                                params: vec![goty::GoType::TString],
                                ret_ty: Box::new(goty::GoType::TInt32),
                            },
                        }),
                        args: vec![goast::Expr::Var {
                            name: "s".to_string(),
                            ty: goty::GoType::TString,
                        }],
                        ty: goty::GoType::TInt32,
                    }],
                    ty: goty::GoType::TInt32,
                }),
            }],
        },
    }
}

fn string_get() -> goast::Fn {
    goast::Fn {
        name: "string_get".to_string(),
        params: vec![
            ("s".to_string(), goty::GoType::TString),
            ("i".to_string(), goty::GoType::TInt32),
        ],
        ret_ty: Some(goty::GoType::TChar),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "rune".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TUint8],
                            ret_ty: Box::new(goty::GoType::TChar),
                        },
                    }),
                    args: vec![goast::Expr::Index {
                        array: Box::new(goast::Expr::Var {
                            name: "s".to_string(),
                            ty: goty::GoType::TString,
                        }),
                        index: Box::new(goast::Expr::Var {
                            name: "i".to_string(),
                            ty: goty::GoType::TInt32,
                        }),
                        ty: goty::GoType::TUint8,
                    }],
                    ty: goty::GoType::TChar,
                }),
            }],
        },
    }
}

fn string_print() -> goast::Fn {
    goast::Fn {
        name: "string_print".to_string(),
        params: vec![("s".to_string(), goty::GoType::TString)],
        ret_ty: Some(goty::GoType::TUnit),
        body: goast::Block {
            stmts: vec![
                goast::Stmt::Expr(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "_goml_fmt.Print".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TString],
                            ret_ty: Box::new(goty::GoType::TVoid),
                        },
                    }),
                    args: vec![goast::Expr::Var {
                        name: "s".to_string(),
                        ty: goty::GoType::TString,
                    }],
                    ty: goty::GoType::TVoid,
                }),
                goast::Stmt::Return {
                    expr: Some(goast::Expr::Unit {
                        ty: goty::GoType::TUnit,
                    }),
                },
            ],
        },
    }
}

fn string_println() -> goast::Fn {
    goast::Fn {
        name: "string_println".to_string(),
        params: vec![("s".to_string(), goty::GoType::TString)],
        ret_ty: Some(goty::GoType::TUnit),
        body: goast::Block {
            stmts: vec![
                goast::Stmt::Expr(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "_goml_fmt.Println".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TString],
                            ret_ty: Box::new(goty::GoType::TVoid),
                        },
                    }),
                    args: vec![goast::Expr::Var {
                        name: "s".to_string(),
                        ty: goty::GoType::TString,
                    }],
                    ty: goty::GoType::TVoid,
                }),
                goast::Stmt::Return {
                    expr: Some(goast::Expr::Unit {
                        ty: goty::GoType::TUnit,
                    }),
                },
            ],
        },
    }
}

fn missing() -> goast::Fn {
    goast::Fn {
        name: "missing".to_string(),
        params: vec![("s".to_string(), goty::GoType::TString)],
        ret_ty: Some(goty::GoType::TUnit),
        body: goast::Block {
            stmts: vec![
                goast::Stmt::Expr(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "println".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TString],
                            ret_ty: Box::new(goty::GoType::TVoid),
                        },
                    }),
                    args: vec![goast::Expr::BinaryOp {
                        op: GoBinaryOp::Add,
                        lhs: Box::new(goast::Expr::String {
                            value: "missing: ".to_string(),
                            ty: goty::GoType::TString,
                        }),
                        rhs: Box::new(goast::Expr::Var {
                            name: "s".to_string(),
                            ty: goty::GoType::TString,
                        }),
                        ty: goty::GoType::TString,
                    }],
                    ty: goty::GoType::TVoid,
                }),
                goast::Stmt::Expr(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "panic".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TString],
                            ret_ty: Box::new(goty::GoType::TVoid),
                        },
                    }),
                    args: vec![goast::Expr::String {
                        value: "".to_string(),
                        ty: goty::GoType::TString,
                    }],
                    ty: goty::GoType::TVoid,
                }),
                goast::Stmt::Return {
                    expr: Some(goast::Expr::Unit {
                        ty: goty::GoType::TUnit,
                    }),
                },
            ],
        },
    }
}
