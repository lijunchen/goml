use crate::{
    go::{
        goast::{self, GoBinaryOp, ImportDecl, ImportSpec, Item, Package},
        goty,
        mangle::{encode_ty, go_ident},
    },
    tast,
};

use indexmap::IndexSet;

fn ty_contains_type_param(ty: &tast::Ty) -> bool {
    match ty {
        tast::Ty::TParam { .. } => true,
        tast::Ty::TArray { elem, .. } | tast::Ty::TRef { elem } => ty_contains_type_param(elem),
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
    vec![
        Item::Package(Package {
            name: "main".to_string(),
        }),
        Item::Import(ImportDecl {
            specs: vec![ImportSpec {
                alias: None,
                path: "fmt".to_string(),
            }],
        }),
        Item::Fn(unit_to_string()),
        Item::Fn(bool_to_string()),
        Item::Fn(bool_to_json()),
        Item::Fn(json_escape_string()),
        Item::Fn(string_len()),
        Item::Fn(string_get()),
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
        Item::Fn(string_print()),
        Item::Fn(string_println()),
        Item::Fn(missing()),
    ]
}

pub fn array_helper_fn_name(prefix: &str, ty: &tast::Ty) -> String {
    format!("{}__{}", prefix, go_ident(&encode_ty(ty)))
}

pub fn ref_helper_fn_name(prefix: &str, ty: &tast::Ty) -> String {
    format!("{}__{}", prefix, go_ident(&encode_ty(ty)))
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

        items.push(goast::Item::Fn(new_fn));
        items.push(goast::Item::Fn(get_fn));
        items.push(goast::Item::Fn(set_fn));
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
                        name: "fmt.Sprintf".to_string(),
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
                        name: "fmt.Sprintf".to_string(),
                        ty: fmt_ty,
                    }),
                    args: vec![
                        goast::Expr::String {
                            value: "%d".to_string(),
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
        ret_ty: Some(goty::GoType::TString),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "string".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TUint8],
                            ret_ty: Box::new(goty::GoType::TString),
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
                    ty: goty::GoType::TString,
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
                        name: "fmt.Print".to_string(),
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
                        name: "fmt.Println".to_string(),
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
