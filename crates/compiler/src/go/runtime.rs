use crate::{
    go::{
        goast::{self, BinaryOp, ImportDecl, ImportSpec, Item, Package, UnaryOp},
        goty,
    },
    tast,
    type_encoding::encode_ty,
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
// int_to_string(x : int) string
// int_neg(x : int) int
// bool_not(x : bool) bool
// bool_and(x : bool, y : bool) bool
// bool_or(x : bool, y : bool) bool
// int_add(x : int, y : int) int
// int_sub(x : int, y : int) int
// int_mul(x : int, y : int) int
// int_div(x : int, y : int) int
// int_less(x : int, y : int) bool
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
        Item::Fn(int_to_string()),
        Item::Fn(int8_to_string()),
        Item::Fn(int_neg()),
        Item::Fn(int8_neg()),
        Item::Fn(bool_not()),
        Item::Fn(bool_and()),
        Item::Fn(bool_or()),
        Item::Fn(int_add()),
        Item::Fn(int_sub()),
        Item::Fn(int_mul()),
        Item::Fn(int_div()),
        Item::Fn(int_less()),
        Item::Fn(int8_add()),
        Item::Fn(int8_sub()),
        Item::Fn(int8_mul()),
        Item::Fn(int8_div()),
        Item::Fn(int8_less()),
        Item::Fn(string_add()),
        Item::Fn(string_print()),
        Item::Fn(string_println()),
        Item::Fn(missing()),
    ]
}

pub fn array_helper_fn_name(prefix: &str, ty: &tast::Ty) -> String {
    format!("{}__{}", prefix, encode_ty(ty))
}

pub fn ref_helper_fn_name(prefix: &str, ty: &tast::Ty) -> String {
    format!("{}__{}", prefix, encode_ty(ty))
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
                ("index".to_string(), goty::GoType::TInt),
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
                            ty: goty::GoType::TInt,
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
                ("index".to_string(), goty::GoType::TInt),
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
                            ty: goty::GoType::TInt,
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
                        op: goast::UnaryOp::AddrOf,
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

fn int_to_string() -> goast::Fn {
    goast::Fn {
        name: "int_to_string".to_string(),
        params: vec![("x".to_string(), goty::GoType::TInt)],
        ret_ty: Some(goty::GoType::TString),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "fmt.Sprintf".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TString, goty::GoType::TInt],
                            ret_ty: Box::new(goty::GoType::TString),
                        },
                    }),
                    args: vec![
                        goast::Expr::String {
                            value: "%d".to_string(),
                            ty: goty::GoType::TString,
                        },
                        goast::Expr::Var {
                            name: "x".to_string(),
                            ty: goty::GoType::TInt,
                        },
                    ],
                    ty: goty::GoType::TString,
                }),
            }],
        },
    }
}

fn int_neg() -> goast::Fn {
    goast::Fn {
        name: "int_neg".to_string(),
        params: vec![("x".to_string(), goty::GoType::TInt)],
        ret_ty: Some(goty::GoType::TInt),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::UnaryOp {
                    op: UnaryOp::Neg,
                    expr: Box::new(goast::Expr::Var {
                        name: "x".to_string(),
                        ty: goty::GoType::TInt,
                    }),
                    ty: goty::GoType::TInt,
                }),
            }],
        },
    }
}

fn int8_to_string() -> goast::Fn {
    goast::Fn {
        name: "int8_to_string".to_string(),
        params: vec![("x".to_string(), goty::GoType::TInt8)],
        ret_ty: Some(goty::GoType::TString),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "fmt.Sprintf".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TString, goty::GoType::TInt8],
                            ret_ty: Box::new(goty::GoType::TString),
                        },
                    }),
                    args: vec![
                        goast::Expr::String {
                            value: "%d".to_string(),
                            ty: goty::GoType::TString,
                        },
                        goast::Expr::Var {
                            name: "x".to_string(),
                            ty: goty::GoType::TInt8,
                        },
                    ],
                    ty: goty::GoType::TString,
                }),
            }],
        },
    }
}

fn int8_neg() -> goast::Fn {
    goast::Fn {
        name: "int8_neg".to_string(),
        params: vec![("x".to_string(), goty::GoType::TInt8)],
        ret_ty: Some(goty::GoType::TInt8),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::UnaryOp {
                    op: UnaryOp::Neg,
                    expr: Box::new(goast::Expr::Var {
                        name: "x".to_string(),
                        ty: goty::GoType::TInt8,
                    }),
                    ty: goty::GoType::TInt8,
                }),
            }],
        },
    }
}

fn bool_not() -> goast::Fn {
    goast::Fn {
        name: "bool_not".to_string(),
        params: vec![("x".to_string(), goty::GoType::TBool)],
        ret_ty: Some(goty::GoType::TBool),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::UnaryOp {
                    op: UnaryOp::Not,
                    expr: Box::new(goast::Expr::Var {
                        name: "x".to_string(),
                        ty: goty::GoType::TBool,
                    }),
                    ty: goty::GoType::TBool,
                }),
            }],
        },
    }
}

fn bool_and() -> goast::Fn {
    goast::Fn {
        name: "bool_and".to_string(),
        params: vec![
            ("x".to_string(), goty::GoType::TBool),
            ("y".to_string(), goty::GoType::TBool),
        ],
        ret_ty: Some(goty::GoType::TBool),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::BinaryOp {
                    op: BinaryOp::And,
                    lhs: Box::new(goast::Expr::Var {
                        name: "x".to_string(),
                        ty: goty::GoType::TBool,
                    }),
                    rhs: Box::new(goast::Expr::Var {
                        name: "y".to_string(),
                        ty: goty::GoType::TBool,
                    }),
                    ty: goty::GoType::TBool,
                }),
            }],
        },
    }
}

fn bool_or() -> goast::Fn {
    goast::Fn {
        name: "bool_or".to_string(),
        params: vec![
            ("x".to_string(), goty::GoType::TBool),
            ("y".to_string(), goty::GoType::TBool),
        ],
        ret_ty: Some(goty::GoType::TBool),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::BinaryOp {
                    op: BinaryOp::Or,
                    lhs: Box::new(goast::Expr::Var {
                        name: "x".to_string(),
                        ty: goty::GoType::TBool,
                    }),
                    rhs: Box::new(goast::Expr::Var {
                        name: "y".to_string(),
                        ty: goty::GoType::TBool,
                    }),
                    ty: goty::GoType::TBool,
                }),
            }],
        },
    }
}

fn int_add() -> goast::Fn {
    arithmetic_binary_fn("int_add", BinaryOp::Add)
}

fn int_sub() -> goast::Fn {
    arithmetic_binary_fn("int_sub", BinaryOp::Sub)
}

fn int_mul() -> goast::Fn {
    arithmetic_binary_fn("int_mul", BinaryOp::Mul)
}

fn int_div() -> goast::Fn {
    arithmetic_binary_fn("int_div", BinaryOp::Div)
}

fn int8_add() -> goast::Fn {
    arithmetic_binary_fn_int8("int8_add", BinaryOp::Add)
}

fn int8_sub() -> goast::Fn {
    arithmetic_binary_fn_int8("int8_sub", BinaryOp::Sub)
}

fn int8_mul() -> goast::Fn {
    arithmetic_binary_fn_int8("int8_mul", BinaryOp::Mul)
}

fn int8_div() -> goast::Fn {
    arithmetic_binary_fn_int8("int8_div", BinaryOp::Div)
}

fn arithmetic_binary_fn(name: &str, op: BinaryOp) -> goast::Fn {
    goast::Fn {
        name: name.to_string(),
        params: vec![
            ("x".to_string(), goty::GoType::TInt),
            ("y".to_string(), goty::GoType::TInt),
        ],
        ret_ty: Some(goty::GoType::TInt),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::BinaryOp {
                    op,
                    lhs: Box::new(goast::Expr::Var {
                        name: "x".to_string(),
                        ty: goty::GoType::TInt,
                    }),
                    rhs: Box::new(goast::Expr::Var {
                        name: "y".to_string(),
                        ty: goty::GoType::TInt,
                    }),
                    ty: goty::GoType::TInt,
                }),
            }],
        },
    }
}

fn arithmetic_binary_fn_int8(name: &str, op: BinaryOp) -> goast::Fn {
    goast::Fn {
        name: name.to_string(),
        params: vec![
            ("x".to_string(), goty::GoType::TInt8),
            ("y".to_string(), goty::GoType::TInt8),
        ],
        ret_ty: Some(goty::GoType::TInt8),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::BinaryOp {
                    op,
                    lhs: Box::new(goast::Expr::Var {
                        name: "x".to_string(),
                        ty: goty::GoType::TInt8,
                    }),
                    rhs: Box::new(goast::Expr::Var {
                        name: "y".to_string(),
                        ty: goty::GoType::TInt8,
                    }),
                    ty: goty::GoType::TInt8,
                }),
            }],
        },
    }
}

fn int_less() -> goast::Fn {
    goast::Fn {
        name: "int_less".to_string(),
        params: vec![
            ("x".to_string(), goty::GoType::TInt),
            ("y".to_string(), goty::GoType::TInt),
        ],
        ret_ty: Some(goty::GoType::TBool),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::BinaryOp {
                    op: BinaryOp::Less,
                    lhs: Box::new(goast::Expr::Var {
                        name: "x".to_string(),
                        ty: goty::GoType::TInt,
                    }),
                    rhs: Box::new(goast::Expr::Var {
                        name: "y".to_string(),
                        ty: goty::GoType::TInt,
                    }),
                    ty: goty::GoType::TBool,
                }),
            }],
        },
    }
}

fn int8_less() -> goast::Fn {
    goast::Fn {
        name: "int8_less".to_string(),
        params: vec![
            ("x".to_string(), goty::GoType::TInt8),
            ("y".to_string(), goty::GoType::TInt8),
        ],
        ret_ty: Some(goty::GoType::TBool),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::BinaryOp {
                    op: BinaryOp::Less,
                    lhs: Box::new(goast::Expr::Var {
                        name: "x".to_string(),
                        ty: goty::GoType::TInt8,
                    }),
                    rhs: Box::new(goast::Expr::Var {
                        name: "y".to_string(),
                        ty: goty::GoType::TInt8,
                    }),
                    ty: goty::GoType::TBool,
                }),
            }],
        },
    }
}

fn string_add() -> goast::Fn {
    goast::Fn {
        name: "string_add".to_string(),
        params: vec![
            ("a".to_string(), goty::GoType::TString),
            ("b".to_string(), goty::GoType::TString),
        ],
        ret_ty: Some(goty::GoType::TString),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::BinaryOp {
                    op: BinaryOp::Add,
                    lhs: Box::new(goast::Expr::Var {
                        name: "a".to_string(),
                        ty: goty::GoType::TString,
                    }),
                    rhs: Box::new(goast::Expr::Var {
                        name: "b".to_string(),
                        ty: goty::GoType::TString,
                    }),
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
                        op: BinaryOp::Add,
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
