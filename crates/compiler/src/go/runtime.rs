use crate::go::{
    goast::{self, BinaryOp, ImportDecl, ImportSpec, Item, Package, UnaryOp},
    goty,
};

// unit_to_string(x : struct{}) string
// bool_to_string(x : bool) string
// int_to_string(x : int) string
// int_neg(x : int) int
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
        Item::Fn(int_neg()),
        Item::Fn(int_add()),
        Item::Fn(int_sub()),
        Item::Fn(int_mul()),
        Item::Fn(int_div()),
        Item::Fn(int_less()),
        Item::Fn(string_add()),
        Item::Fn(string_print()),
        Item::Fn(string_println()),
        Item::Fn(missing()),
    ]
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
                    func: "fmt.Sprintf".to_string(),
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
                    func: "fmt.Print".to_string(),
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
                    func: "fmt.Println".to_string(),
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
                    func: "println".to_string(),
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
                    func: "panic".to_string(),
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
