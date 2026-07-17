use crate::{
    go::{
        compile::{GlobalGoEnv, variant_symbol_name},
        goast::{self, GoBinaryOp, ImportDecl, ImportSpec, Item, Package},
        goty,
        mangle::{encode_ty, go_dyn_struct_name, go_generated_ident, go_ident},
    },
    intrinsics::{IntrinsicId, LangItemId, RuntimeHookId},
    names::{trait_impl_fn_name, ty_compact},
    package_names::ENTRY_FUNCTION,
    tast,
};

use indexmap::IndexSet;

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

// unit_to_string(x : struct{}) string
// bool_to_string(x : bool) string
// print(s : string) struct{}
// println(s : string) struct{}
// missing(s : string) struct{}
pub fn make_runtime() -> Vec<goast::Item> {
    let items = vec![
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
                    alias: Some("_goml_os".to_string()),
                    path: "os".to_string(),
                },
                ImportSpec {
                    alias: Some("_goml_reflect".to_string()),
                    path: "reflect".to_string(),
                },
                ImportSpec {
                    alias: Some("_goml_utf8".to_string()),
                    path: "unicode/utf8".to_string(),
                },
            ],
        }),
        Item::Fn(unit_to_string()),
        Item::Fn(bool_to_string()),
        Item::Fn(string_len()),
        Item::Fn(string_get()),
        Item::Fn(string_byte_slice()),
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
        Item::Fn(std_env_args_raw()),
        Item::Fn(std_fs_read_file_raw()),
        Item::Fn(std_fs_write_file_raw()),
        Item::Fn(std_fs_create_dir_all_raw()),
        Item::Fn(std_fs_file_exists_raw()),
        Item::Fn(std_fs_read_dir_raw()),
        Item::Fn(std_io_print_raw()),
        Item::Fn(std_io_println_raw()),
        Item::Fn(std_io_eprint_raw()),
        Item::Fn(std_process_exit_raw()),
        Item::Fn(std_testing_fail_raw()),
        Item::Fn(missing()),
    ];
    items
}

pub fn runtime_hook_fn_name(id: RuntimeHookId) -> String {
    go_generated_ident(&format!("_goml_runtime_{}", id.key().replace('.', "_")))
}

fn go_error_ty() -> goty::GoType {
    goty::GoType::TName {
        name: "error".to_string(),
    }
}

fn os_dir_entry_ty() -> goty::GoType {
    goty::GoType::TName {
        name: "_goml_os.DirEntry".to_string(),
    }
}

fn os_file_info_ty() -> goty::GoType {
    goty::GoType::TName {
        name: "_goml_os.FileInfo".to_string(),
    }
}

fn tuple_ty(typs: Vec<tast::Ty>) -> tast::Ty {
    tast::Ty::TTuple { typs }
}

fn tuple_literal(ty: &tast::Ty, fields: Vec<goast::Expr>) -> goast::Expr {
    goast::Expr::StructLiteral {
        fields: fields
            .into_iter()
            .enumerate()
            .map(|(idx, expr)| (format!("_{}", idx), expr))
            .collect(),
        ty: goast::tast_ty_to_go_type(ty),
    }
}

fn vec_from_slice_expr(elem: &tast::Ty, value: goast::Expr) -> goast::Expr {
    let struct_ty = goty::GoType::TName {
        name: goast::vec_struct_name(elem),
    };
    let vec_ty = goty::GoType::TPointer {
        elem: Box::new(struct_ty.clone()),
    };
    goast::Expr::UnaryOp {
        op: goast::GoUnaryOp::AddrOf,
        expr: Box::new(goast::Expr::StructLiteral {
            fields: vec![("items".to_string(), value)],
            ty: struct_ty,
        }),
        ty: vec_ty,
    }
}

fn error_string_expr(err_name: &str) -> goast::Expr {
    goast::Expr::Call {
        func: Box::new(goast::Expr::FieldAccess {
            obj: Box::new(goast::Expr::Var {
                name: err_name.to_string(),
                ty: go_error_ty(),
            }),
            field: "Error".to_string(),
            ty: goty::GoType::TFunc {
                params: vec![],
                ret_ty: Box::new(goty::GoType::TString),
            },
        }),
        args: vec![],
        ty: goty::GoType::TString,
    }
}

fn std_env_args_raw() -> goast::Fn {
    let elem = tast::Ty::TString;
    let vec_ty = tast::Ty::TVec {
        elem: Box::new(elem.clone()),
    };
    let slice_ty = goty::GoType::TSlice {
        elem: Box::new(goty::GoType::TString),
    };
    goast::Fn {
        name: runtime_hook_fn_name(RuntimeHookId::StdEnvArgs),
        params: vec![],
        ret_ty: Some(goast::tast_ty_to_go_type(&vec_ty)),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(vec_from_slice_expr(
                    &elem,
                    goast::Expr::Var {
                        name: "_goml_os.Args".to_string(),
                        ty: slice_ty,
                    },
                )),
            }],
        },
    }
}

fn std_fs_read_file_raw() -> goast::Fn {
    let data_ty = goty::GoType::TSlice {
        elem: Box::new(goty::GoType::TUint8),
    };
    let err_ty = go_error_ty();
    let ret_ty = tuple_ty(vec![tast::Ty::TBool, tast::Ty::TString, tast::Ty::TString]);
    goast::Fn {
        name: runtime_hook_fn_name(RuntimeHookId::StdFsReadFile),
        params: vec![("path".to_string(), goty::GoType::TString)],
        ret_ty: Some(goast::tast_ty_to_go_type(&ret_ty)),
        body: goast::Block {
            stmts: vec![
                goast::Stmt::VarDecl {
                    name: "data".to_string(),
                    ty: data_ty.clone(),
                    value: None,
                },
                goast::Stmt::VarDecl {
                    name: "err".to_string(),
                    ty: err_ty.clone(),
                    value: None,
                },
                goast::Stmt::MultiAssignment {
                    names: vec!["data".to_string(), "err".to_string()],
                    value: goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: "_goml_os.ReadFile".to_string(),
                            ty: goty::GoType::TFunc {
                                params: vec![goty::GoType::TString],
                                ret_ty: Box::new(goty::GoType::TMulti {
                                    elems: vec![data_ty.clone(), err_ty.clone()],
                                }),
                            },
                        }),
                        args: vec![goast::Expr::Var {
                            name: "path".to_string(),
                            ty: goty::GoType::TString,
                        }],
                        ty: goty::GoType::TMulti {
                            elems: vec![data_ty.clone(), err_ty.clone()],
                        },
                    },
                },
                goast::Stmt::If {
                    cond: goast::Expr::BinaryOp {
                        op: GoBinaryOp::NotEq,
                        lhs: Box::new(goast::Expr::Var {
                            name: "err".to_string(),
                            ty: err_ty.clone(),
                        }),
                        rhs: Box::new(goast::Expr::Nil { ty: err_ty }),
                        ty: goty::GoType::TBool,
                    },
                    then: goast::Block {
                        stmts: vec![goast::Stmt::Return {
                            expr: Some(tuple_literal(
                                &ret_ty,
                                vec![
                                    goast::Expr::Bool {
                                        value: false,
                                        ty: goty::GoType::TBool,
                                    },
                                    goast::Expr::String {
                                        value: String::new(),
                                        ty: goty::GoType::TString,
                                    },
                                    error_string_expr("err"),
                                ],
                            )),
                        }],
                    },
                    else_: None,
                },
                goast::Stmt::Return {
                    expr: Some(tuple_literal(
                        &ret_ty,
                        vec![
                            goast::Expr::Bool {
                                value: true,
                                ty: goty::GoType::TBool,
                            },
                            goast::Expr::Call {
                                func: Box::new(goast::Expr::Var {
                                    name: "string".to_string(),
                                    ty: goty::GoType::TFunc {
                                        params: vec![data_ty.clone()],
                                        ret_ty: Box::new(goty::GoType::TString),
                                    },
                                }),
                                args: vec![goast::Expr::Var {
                                    name: "data".to_string(),
                                    ty: data_ty,
                                }],
                                ty: goty::GoType::TString,
                            },
                            goast::Expr::String {
                                value: String::new(),
                                ty: goty::GoType::TString,
                            },
                        ],
                    )),
                },
            ],
        },
    }
}

fn std_fs_write_file_raw() -> goast::Fn {
    let data_ty = goty::GoType::TSlice {
        elem: Box::new(goty::GoType::TUint8),
    };
    let err_ty = go_error_ty();
    let ret_ty = tuple_ty(vec![tast::Ty::TBool, tast::Ty::TString]);
    goast::Fn {
        name: runtime_hook_fn_name(RuntimeHookId::StdFsWriteFile),
        params: vec![
            ("path".to_string(), goty::GoType::TString),
            ("content".to_string(), goty::GoType::TString),
        ],
        ret_ty: Some(goast::tast_ty_to_go_type(&ret_ty)),
        body: goast::Block {
            stmts: vec![
                goast::Stmt::VarDecl {
                    name: "err".to_string(),
                    ty: err_ty.clone(),
                    value: Some(goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: "_goml_os.WriteFile".to_string(),
                            ty: goty::GoType::TFunc {
                                params: vec![
                                    goty::GoType::TString,
                                    data_ty.clone(),
                                    goty::GoType::TInt32,
                                ],
                                ret_ty: Box::new(err_ty.clone()),
                            },
                        }),
                        args: vec![
                            goast::Expr::Var {
                                name: "path".to_string(),
                                ty: goty::GoType::TString,
                            },
                            goast::Expr::Call {
                                func: Box::new(goast::Expr::Var {
                                    name: "[]byte".to_string(),
                                    ty: goty::GoType::TFunc {
                                        params: vec![goty::GoType::TString],
                                        ret_ty: Box::new(data_ty),
                                    },
                                }),
                                args: vec![goast::Expr::Var {
                                    name: "content".to_string(),
                                    ty: goty::GoType::TString,
                                }],
                                ty: goty::GoType::TSlice {
                                    elem: Box::new(goty::GoType::TUint8),
                                },
                            },
                            goast::Expr::Int {
                                value: "0644".to_string(),
                                ty: goty::GoType::TInt32,
                            },
                        ],
                        ty: err_ty.clone(),
                    }),
                },
                goast::Stmt::If {
                    cond: goast::Expr::BinaryOp {
                        op: GoBinaryOp::NotEq,
                        lhs: Box::new(goast::Expr::Var {
                            name: "err".to_string(),
                            ty: err_ty.clone(),
                        }),
                        rhs: Box::new(goast::Expr::Nil { ty: err_ty }),
                        ty: goty::GoType::TBool,
                    },
                    then: goast::Block {
                        stmts: vec![goast::Stmt::Return {
                            expr: Some(tuple_literal(
                                &ret_ty,
                                vec![
                                    goast::Expr::Bool {
                                        value: false,
                                        ty: goty::GoType::TBool,
                                    },
                                    error_string_expr("err"),
                                ],
                            )),
                        }],
                    },
                    else_: None,
                },
                goast::Stmt::Return {
                    expr: Some(tuple_literal(
                        &ret_ty,
                        vec![
                            goast::Expr::Bool {
                                value: true,
                                ty: goty::GoType::TBool,
                            },
                            goast::Expr::String {
                                value: String::new(),
                                ty: goty::GoType::TString,
                            },
                        ],
                    )),
                },
            ],
        },
    }
}

fn std_fs_create_dir_all_raw() -> goast::Fn {
    let err_ty = go_error_ty();
    let ret_ty = tuple_ty(vec![tast::Ty::TBool, tast::Ty::TString]);
    goast::Fn {
        name: runtime_hook_fn_name(RuntimeHookId::StdFsCreateDirAll),
        params: vec![("path".to_string(), goty::GoType::TString)],
        ret_ty: Some(goast::tast_ty_to_go_type(&ret_ty)),
        body: goast::Block {
            stmts: vec![
                goast::Stmt::VarDecl {
                    name: "err".to_string(),
                    ty: err_ty.clone(),
                    value: Some(goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: "_goml_os.MkdirAll".to_string(),
                            ty: goty::GoType::TFunc {
                                params: vec![goty::GoType::TString, goty::GoType::TUint32],
                                ret_ty: Box::new(err_ty.clone()),
                            },
                        }),
                        args: vec![
                            goast::Expr::Var {
                                name: "path".to_string(),
                                ty: goty::GoType::TString,
                            },
                            goast::Expr::Int {
                                value: "0755".to_string(),
                                ty: goty::GoType::TUint32,
                            },
                        ],
                        ty: err_ty.clone(),
                    }),
                },
                goast::Stmt::If {
                    cond: goast::Expr::BinaryOp {
                        op: GoBinaryOp::NotEq,
                        lhs: Box::new(goast::Expr::Var {
                            name: "err".to_string(),
                            ty: err_ty.clone(),
                        }),
                        rhs: Box::new(goast::Expr::Nil { ty: err_ty }),
                        ty: goty::GoType::TBool,
                    },
                    then: goast::Block {
                        stmts: vec![goast::Stmt::Return {
                            expr: Some(tuple_literal(
                                &ret_ty,
                                vec![
                                    goast::Expr::Bool {
                                        value: false,
                                        ty: goty::GoType::TBool,
                                    },
                                    error_string_expr("err"),
                                ],
                            )),
                        }],
                    },
                    else_: None,
                },
                goast::Stmt::Return {
                    expr: Some(tuple_literal(
                        &ret_ty,
                        vec![
                            goast::Expr::Bool {
                                value: true,
                                ty: goty::GoType::TBool,
                            },
                            goast::Expr::String {
                                value: String::new(),
                                ty: goty::GoType::TString,
                            },
                        ],
                    )),
                },
            ],
        },
    }
}

fn std_fs_file_exists_raw() -> goast::Fn {
    let err_ty = go_error_ty();
    let info_ty = os_file_info_ty();
    goast::Fn {
        name: runtime_hook_fn_name(RuntimeHookId::StdFsFileExists),
        params: vec![("path".to_string(), goty::GoType::TString)],
        ret_ty: Some(goty::GoType::TBool),
        body: goast::Block {
            stmts: vec![
                goast::Stmt::VarDecl {
                    name: "err".to_string(),
                    ty: err_ty.clone(),
                    value: None,
                },
                goast::Stmt::MultiAssignment {
                    names: vec!["_".to_string(), "err".to_string()],
                    value: goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: "_goml_os.Stat".to_string(),
                            ty: goty::GoType::TFunc {
                                params: vec![goty::GoType::TString],
                                ret_ty: Box::new(goty::GoType::TMulti {
                                    elems: vec![info_ty, err_ty.clone()],
                                }),
                            },
                        }),
                        args: vec![goast::Expr::Var {
                            name: "path".to_string(),
                            ty: goty::GoType::TString,
                        }],
                        ty: goty::GoType::TMulti {
                            elems: vec![os_file_info_ty(), err_ty.clone()],
                        },
                    },
                },
                goast::Stmt::Return {
                    expr: Some(goast::Expr::BinaryOp {
                        op: GoBinaryOp::Eq,
                        lhs: Box::new(goast::Expr::Var {
                            name: "err".to_string(),
                            ty: err_ty.clone(),
                        }),
                        rhs: Box::new(goast::Expr::Nil { ty: err_ty }),
                        ty: goty::GoType::TBool,
                    }),
                },
            ],
        },
    }
}

fn std_fs_read_dir_raw() -> goast::Fn {
    let entry_ty = os_dir_entry_ty();
    let entries_ty = goty::GoType::TSlice {
        elem: Box::new(entry_ty.clone()),
    };
    let names_ty = goty::GoType::TSlice {
        elem: Box::new(goty::GoType::TString),
    };
    let names_elem = tast::Ty::TString;
    let err_ty = go_error_ty();
    let ret_ty = tuple_ty(vec![
        tast::Ty::TBool,
        tast::Ty::TVec {
            elem: Box::new(tast::Ty::TString),
        },
        tast::Ty::TString,
    ]);
    goast::Fn {
        name: runtime_hook_fn_name(RuntimeHookId::StdFsReadDir),
        params: vec![("path".to_string(), goty::GoType::TString)],
        ret_ty: Some(goast::tast_ty_to_go_type(&ret_ty)),
        body: goast::Block {
            stmts: vec![
                goast::Stmt::VarDecl {
                    name: "entries".to_string(),
                    ty: entries_ty.clone(),
                    value: None,
                },
                goast::Stmt::VarDecl {
                    name: "err".to_string(),
                    ty: err_ty.clone(),
                    value: None,
                },
                goast::Stmt::MultiAssignment {
                    names: vec!["entries".to_string(), "err".to_string()],
                    value: goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: "_goml_os.ReadDir".to_string(),
                            ty: goty::GoType::TFunc {
                                params: vec![goty::GoType::TString],
                                ret_ty: Box::new(goty::GoType::TMulti {
                                    elems: vec![entries_ty.clone(), err_ty.clone()],
                                }),
                            },
                        }),
                        args: vec![goast::Expr::Var {
                            name: "path".to_string(),
                            ty: goty::GoType::TString,
                        }],
                        ty: goty::GoType::TMulti {
                            elems: vec![entries_ty.clone(), err_ty.clone()],
                        },
                    },
                },
                goast::Stmt::If {
                    cond: goast::Expr::BinaryOp {
                        op: GoBinaryOp::NotEq,
                        lhs: Box::new(goast::Expr::Var {
                            name: "err".to_string(),
                            ty: err_ty.clone(),
                        }),
                        rhs: Box::new(goast::Expr::Nil { ty: err_ty }),
                        ty: goty::GoType::TBool,
                    },
                    then: goast::Block {
                        stmts: vec![goast::Stmt::Return {
                            expr: Some(tuple_literal(
                                &ret_ty,
                                vec![
                                    goast::Expr::Bool {
                                        value: false,
                                        ty: goty::GoType::TBool,
                                    },
                                    vec_from_slice_expr(
                                        &names_elem,
                                        goast::Expr::Nil {
                                            ty: names_ty.clone(),
                                        },
                                    ),
                                    error_string_expr("err"),
                                ],
                            )),
                        }],
                    },
                    else_: None,
                },
                goast::Stmt::VarDecl {
                    name: "names".to_string(),
                    ty: names_ty.clone(),
                    value: None,
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
                    label: None,
                    body: goast::Block {
                        stmts: vec![
                            goast::Stmt::If {
                                cond: goast::Expr::BinaryOp {
                                    op: GoBinaryOp::GreaterEq,
                                    lhs: Box::new(goast::Expr::Var {
                                        name: "i".to_string(),
                                        ty: goty::GoType::TInt32,
                                    }),
                                    rhs: Box::new(goast::Expr::Call {
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
                                                    params: vec![entries_ty.clone()],
                                                    ret_ty: Box::new(goty::GoType::TInt32),
                                                },
                                            }),
                                            args: vec![goast::Expr::Var {
                                                name: "entries".to_string(),
                                                ty: entries_ty.clone(),
                                            }],
                                            ty: goty::GoType::TInt32,
                                        }],
                                        ty: goty::GoType::TInt32,
                                    }),
                                    ty: goty::GoType::TBool,
                                },
                                then: goast::Block {
                                    stmts: vec![goast::Stmt::Break],
                                },
                                else_: None,
                            },
                            goast::Stmt::VarDecl {
                                name: "entry".to_string(),
                                ty: entry_ty.clone(),
                                value: Some(goast::Expr::Index {
                                    array: Box::new(goast::Expr::Var {
                                        name: "entries".to_string(),
                                        ty: entries_ty.clone(),
                                    }),
                                    index: Box::new(goast::Expr::Var {
                                        name: "i".to_string(),
                                        ty: goty::GoType::TInt32,
                                    }),
                                    ty: entry_ty.clone(),
                                }),
                            },
                            goast::Stmt::Assignment {
                                name: "names".to_string(),
                                value: goast::Expr::Call {
                                    func: Box::new(goast::Expr::Var {
                                        name: "append".to_string(),
                                        ty: goty::GoType::TFunc {
                                            params: vec![names_ty.clone(), goty::GoType::TString],
                                            ret_ty: Box::new(names_ty.clone()),
                                        },
                                    }),
                                    args: vec![
                                        goast::Expr::Var {
                                            name: "names".to_string(),
                                            ty: names_ty.clone(),
                                        },
                                        goast::Expr::Call {
                                            func: Box::new(goast::Expr::FieldAccess {
                                                obj: Box::new(goast::Expr::Var {
                                                    name: "entry".to_string(),
                                                    ty: entry_ty.clone(),
                                                }),
                                                field: "Name".to_string(),
                                                ty: goty::GoType::TFunc {
                                                    params: vec![],
                                                    ret_ty: Box::new(goty::GoType::TString),
                                                },
                                            }),
                                            args: vec![],
                                            ty: goty::GoType::TString,
                                        },
                                    ],
                                    ty: names_ty.clone(),
                                },
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
                },
                goast::Stmt::Return {
                    expr: Some(tuple_literal(
                        &ret_ty,
                        vec![
                            goast::Expr::Bool {
                                value: true,
                                ty: goty::GoType::TBool,
                            },
                            vec_from_slice_expr(
                                &names_elem,
                                goast::Expr::Var {
                                    name: "names".to_string(),
                                    ty: names_ty,
                                },
                            ),
                            goast::Expr::String {
                                value: String::new(),
                                ty: goty::GoType::TString,
                            },
                        ],
                    )),
                },
            ],
        },
    }
}

fn std_process_exit_raw() -> goast::Fn {
    goast::Fn {
        name: runtime_hook_fn_name(RuntimeHookId::StdProcessExit),
        params: vec![("code".to_string(), goty::GoType::TInt32)],
        ret_ty: Some(goty::GoType::TUnit),
        body: goast::Block {
            stmts: vec![
                goast::Stmt::Expr(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "_goml_os.Exit".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TInt32],
                            ret_ty: Box::new(goty::GoType::TVoid),
                        },
                    }),
                    args: vec![goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: "int".to_string(),
                            ty: goty::GoType::TFunc {
                                params: vec![goty::GoType::TInt32],
                                ret_ty: Box::new(goty::GoType::TInt32),
                            },
                        }),
                        args: vec![goast::Expr::Var {
                            name: "code".to_string(),
                            ty: goty::GoType::TInt32,
                        }],
                        ty: goty::GoType::TInt32,
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

fn std_testing_fail_raw() -> goast::Fn {
    goast::Fn {
        name: runtime_hook_fn_name(RuntimeHookId::StdTestingFail),
        params: vec![("message".to_string(), goty::GoType::TString)],
        ret_ty: Some(goty::GoType::TUnit),
        body: goast::Block {
            stmts: vec![
                goast::Stmt::Expr(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "_goml_fmt.Fprintln".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TString, goty::GoType::TString],
                            ret_ty: Box::new(goty::GoType::TVoid),
                        },
                    }),
                    args: vec![
                        goast::Expr::Var {
                            name: "_goml_os.Stderr".to_string(),
                            ty: goty::GoType::TName {
                                name: "_goml_os.File".to_string(),
                            },
                        },
                        goast::Expr::Var {
                            name: "message".to_string(),
                            ty: goty::GoType::TString,
                        },
                    ],
                    ty: goty::GoType::TVoid,
                }),
                goast::Stmt::Expr(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "_goml_os.Exit".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![goty::GoType::TInt32],
                            ret_ty: Box::new(goty::GoType::TVoid),
                        },
                    }),
                    args: vec![goast::Expr::Int {
                        value: "101".to_string(),
                        ty: goty::GoType::TInt32,
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

fn std_io_print_raw() -> goast::Fn {
    goast::Fn {
        name: runtime_hook_fn_name(RuntimeHookId::StdIoPrint),
        params: vec![("value".to_string(), goty::GoType::TString)],
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
                        name: "value".to_string(),
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

fn std_io_println_raw() -> goast::Fn {
    goast::Fn {
        name: runtime_hook_fn_name(RuntimeHookId::StdIoPrintln),
        params: vec![("value".to_string(), goty::GoType::TString)],
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
                        name: "value".to_string(),
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

fn std_io_eprint_raw() -> goast::Fn {
    goast::Fn {
        name: runtime_hook_fn_name(RuntimeHookId::StdIoEprint),
        params: vec![("value".to_string(), goty::GoType::TString)],
        ret_ty: Some(goty::GoType::TUnit),
        body: goast::Block {
            stmts: vec![
                goast::Stmt::Expr(goast::Expr::Call {
                    func: Box::new(goast::Expr::Var {
                        name: "_goml_fmt.Fprint".to_string(),
                        ty: goty::GoType::TFunc {
                            params: vec![
                                goty::GoType::TName {
                                    name: "any".to_string(),
                                },
                                goty::GoType::TString,
                            ],
                            ret_ty: Box::new(goty::GoType::TVoid),
                        },
                    }),
                    args: vec![
                        goast::Expr::Var {
                            name: "_goml_os.Stderr".to_string(),
                            ty: goty::GoType::TName {
                                name: "any".to_string(),
                            },
                        },
                        goast::Expr::Var {
                            name: "value".to_string(),
                            ty: goty::GoType::TString,
                        },
                    ],
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

pub fn array_helper_fn_name(prefix: &str, ty: &tast::Ty) -> String {
    go_generated_ident(&format!("{}__{}", prefix, go_ident(&encode_ty(ty))))
}

pub fn vec_helper_fn_name(prefix: &str, ty: &tast::Ty) -> String {
    go_generated_ident(&format!("{}__{}", prefix, go_ident(&encode_ty(ty))))
}

pub fn ref_helper_fn_name(prefix: &str, ty: &tast::Ty) -> String {
    go_generated_ident(&format!("{}__{}", prefix, go_ident(&encode_ty(ty))))
}

pub fn hashmap_helper_fn_name(prefix: &str, ty: &tast::Ty) -> String {
    go_generated_ident(&format!("{}__{}", prefix, go_ident(&encode_ty(ty))))
}

pub fn hashmap_lookup_helper_fn_name(ty: &tast::Ty) -> String {
    hashmap_helper_fn_name("hashmap_lookup", ty)
}

pub fn missing_helper_fn_name(ty: &tast::Ty) -> String {
    go_generated_ident(&format!("missing__{}", go_ident(&encode_ty(ty))))
}

fn missing_base_fn_name() -> String {
    go_generated_ident("_goml_intrinsic_missing")
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
                            name: missing_base_fn_name(),
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
            name: array_helper_fn_name(IntrinsicId::ArrayGet.source_name(), ty),
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
            name: array_helper_fn_name(IntrinsicId::ArraySet.source_name(), ty),
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

fn vec_items_expr(vec_ty: &goty::GoType, items_ty: &goty::GoType) -> goast::Expr {
    goast::Expr::FieldAccess {
        obj: Box::new(goast::Expr::Var {
            name: "vec".to_string(),
            ty: vec_ty.clone(),
        }),
        field: "items".to_string(),
        ty: items_ty.clone(),
    }
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

        let elem_go_ty = goast::tast_ty_to_go_type(elem);
        let items_go_ty = goty::GoType::TSlice {
            elem: Box::new(elem_go_ty.clone()),
        };
        let struct_name = goast::vec_struct_name(elem);
        let struct_go_ty = goty::GoType::TName {
            name: struct_name.clone(),
        };
        let vec_go_ty = goty::GoType::TPointer {
            elem: Box::new(struct_go_ty.clone()),
        };

        items.push(goast::Item::Struct(goast::Struct {
            name: struct_name,
            fields: vec![goast::Field {
                name: "items".to_string(),
                ty: items_go_ty.clone(),
            }],
            methods: vec![],
        }));

        items.push(goast::Item::Fn(goast::Fn {
            name: vec_helper_fn_name(IntrinsicId::VecNew.source_name(), ty),
            params: vec![],
            ret_ty: Some(vec_go_ty.clone()),
            body: goast::Block {
                stmts: vec![goast::Stmt::Return {
                    expr: Some(goast::Expr::UnaryOp {
                        op: goast::GoUnaryOp::AddrOf,
                        expr: Box::new(goast::Expr::StructLiteral {
                            fields: vec![(
                                "items".to_string(),
                                goast::Expr::Nil {
                                    ty: items_go_ty.clone(),
                                },
                            )],
                            ty: struct_go_ty,
                        }),
                        ty: vec_go_ty.clone(),
                    }),
                }],
            },
        }));

        items.push(goast::Item::Fn(goast::Fn {
            name: vec_helper_fn_name(IntrinsicId::VecPush.source_name(), ty),
            params: vec![
                ("vec".to_string(), vec_go_ty.clone()),
                ("elem".to_string(), elem_go_ty.clone()),
            ],
            ret_ty: Some(goty::GoType::TUnit),
            body: goast::Block {
                stmts: vec![
                    goast::Stmt::FieldAssign {
                        target: vec_items_expr(&vec_go_ty, &items_go_ty),
                        value: goast::Expr::Call {
                            func: Box::new(goast::Expr::Var {
                                name: "append".to_string(),
                                ty: goty::GoType::TFunc {
                                    params: vec![items_go_ty.clone(), elem_go_ty.clone()],
                                    ret_ty: Box::new(items_go_ty.clone()),
                                },
                            }),
                            args: vec![
                                vec_items_expr(&vec_go_ty, &items_go_ty),
                                goast::Expr::Var {
                                    name: "elem".to_string(),
                                    ty: elem_go_ty.clone(),
                                },
                            ],
                            ty: items_go_ty.clone(),
                        },
                    },
                    goast::Stmt::Return {
                        expr: Some(goast::Expr::Unit {
                            ty: goty::GoType::TUnit,
                        }),
                    },
                ],
            },
        }));

        items.push(goast::Item::Fn(goast::Fn {
            name: vec_helper_fn_name(IntrinsicId::VecGet.source_name(), ty),
            params: vec![
                ("vec".to_string(), vec_go_ty.clone()),
                ("index".to_string(), goty::GoType::TInt32),
            ],
            ret_ty: Some(elem_go_ty.clone()),
            body: goast::Block {
                stmts: vec![goast::Stmt::Return {
                    expr: Some(goast::Expr::Index {
                        array: Box::new(vec_items_expr(&vec_go_ty, &items_go_ty)),
                        index: Box::new(goast::Expr::Var {
                            name: "index".to_string(),
                            ty: goty::GoType::TInt32,
                        }),
                        ty: elem_go_ty.clone(),
                    }),
                }],
            },
        }));

        items.push(goast::Item::Fn(goast::Fn {
            name: vec_helper_fn_name(IntrinsicId::VecSet.source_name(), ty),
            params: vec![
                ("vec".to_string(), vec_go_ty.clone()),
                ("index".to_string(), goty::GoType::TInt32),
                ("value".to_string(), elem_go_ty.clone()),
            ],
            ret_ty: Some(goty::GoType::TUnit),
            body: goast::Block {
                stmts: vec![
                    goast::Stmt::IndexAssign {
                        array: vec_items_expr(&vec_go_ty, &items_go_ty),
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
                        expr: Some(goast::Expr::Unit {
                            ty: goty::GoType::TUnit,
                        }),
                    },
                ],
            },
        }));

        items.push(goast::Item::Fn(goast::Fn {
            name: vec_helper_fn_name(IntrinsicId::VecLen.source_name(), ty),
            params: vec![("vec".to_string(), vec_go_ty.clone())],
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
                                    params: vec![items_go_ty.clone()],
                                    ret_ty: Box::new(goty::GoType::TInt32),
                                },
                            }),
                            args: vec![vec_items_expr(&vec_go_ty, &items_go_ty)],
                            ty: goty::GoType::TInt32,
                        }],
                        ty: goty::GoType::TInt32,
                    }),
                }],
            },
        }));
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
            name: ref_helper_fn_name(IntrinsicId::RefNew.source_name(), ty),
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
            name: ref_helper_fn_name(IntrinsicId::RefGet.source_name(), ty),
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
            name: ref_helper_fn_name(IntrinsicId::RefSet.source_name(), ty),
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
            name: ref_helper_fn_name(IntrinsicId::RefPtrEq.source_name(), ty),
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

        let reflect_value_ty = goty::GoType::TName {
            name: "_goml_reflect.Value".to_string(),
        };
        let ptr_hash_fn = goast::Fn {
            name: ref_helper_fn_name(IntrinsicId::RefPtrHash.source_name(), ty),
            params: vec![("reference".to_string(), ref_go_ty.clone())],
            ret_ty: Some(goty::GoType::TUint64),
            body: goast::Block {
                stmts: vec![goast::Stmt::Return {
                    expr: Some(goast::Expr::Call {
                        func: Box::new(goast::Expr::Var {
                            name: "uint64".to_string(),
                            ty: goty::GoType::TFunc {
                                params: vec![goty::GoType::TName {
                                    name: "uintptr".to_string(),
                                }],
                                ret_ty: Box::new(goty::GoType::TUint64),
                            },
                        }),
                        args: vec![goast::Expr::Call {
                            func: Box::new(goast::Expr::FieldAccess {
                                obj: Box::new(goast::Expr::Call {
                                    func: Box::new(goast::Expr::Var {
                                        name: "_goml_reflect.ValueOf".to_string(),
                                        ty: goty::GoType::TFunc {
                                            params: vec![ref_go_ty.clone()],
                                            ret_ty: Box::new(reflect_value_ty.clone()),
                                        },
                                    }),
                                    args: vec![goast::Expr::Var {
                                        name: "reference".to_string(),
                                        ty: ref_go_ty.clone(),
                                    }],
                                    ty: reflect_value_ty,
                                }),
                                field: "Pointer".to_string(),
                                ty: goty::GoType::TFunc {
                                    params: vec![],
                                    ret_ty: Box::new(goty::GoType::TName {
                                        name: "uintptr".to_string(),
                                    }),
                                },
                            }),
                            args: vec![],
                            ty: goty::GoType::TName {
                                name: "uintptr".to_string(),
                            },
                        }],
                        ty: goty::GoType::TUint64,
                    }),
                }],
            },
        };

        items.push(goast::Item::Fn(new_fn));
        items.push(goast::Item::Fn(get_fn));
        items.push(goast::Item::Fn(set_fn));
        items.push(goast::Item::Fn(ptr_eq_fn));
        items.push(goast::Item::Fn(ptr_hash_fn));
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
    let type_identifier_method = go_generated_ident(&format!("is{}", option_go_name));
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

fn hashmap_entry_literal(
    entry_go_ty: &goty::GoType,
    key_go_ty: &goty::GoType,
    value_go_ty: &goty::GoType,
) -> goast::Expr {
    goast::Expr::StructLiteral {
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
    }
}

fn increment_hashmap_len(map_ptr_go_ty: &goty::GoType) -> goast::Stmt {
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
    }
}

pub fn make_hashmap_runtime(
    goenv: &GlobalGoEnv,
    hashmap_types: &IndexSet<tast::Ty>,
) -> Vec<goast::Item> {
    let mut items = Vec::new();
    let mut synthetic_option_types = IndexSet::new();
    let Some(hash_trait) = goenv.genv.lang_item(LangItemId::Hash) else {
        return items;
    };
    let Some(eq_trait) = goenv.genv.lang_item(LangItemId::Eq) else {
        return items;
    };
    for ty in hashmap_types {
        let tast::Ty::THashMap { key, value } = ty else {
            continue;
        };

        if ty_contains_type_param(key) || ty_contains_type_param(value) {
            continue;
        }

        let map_struct_name = goast::hashmap_struct_name(key, value);
        let entry_struct_name = go_generated_ident(&format!("{}_entry", map_struct_name));

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

        let hash_impl = go_ident(&trait_impl_fn_name(hash_trait, key, "hash"));
        let eq_impl = go_ident(&trait_impl_fn_name(eq_trait, key, "eq"));

        let hash_fn_ty = goty::GoType::TFunc {
            params: vec![key_go_ty.clone()],
            ret_ty: Box::new(goty::GoType::TUint64),
        };
        let eq_fn_ty = goty::GoType::TFunc {
            params: vec![key_go_ty.clone(), key_go_ty.clone()],
            ret_ty: Box::new(goty::GoType::TBool),
        };

        let new_fn = goast::Fn {
            name: hashmap_helper_fn_name(IntrinsicId::HashMapNew.source_name(), ty),
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
            name: hashmap_helper_fn_name(IntrinsicId::HashMapLen.source_name(), ty),
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
            name: hashmap_helper_fn_name(IntrinsicId::HashMapContains.source_name(), ty),
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
                        value: Some(hashmap_key_hash_expr(
                            key,
                            &key_go_ty,
                            &hash_impl,
                            &hash_fn_ty,
                            hash_trait,
                        )),
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

        let option_name = goenv
            .liftenv
            .monoenv
            .lang_item_instance(LangItemId::Option, std::slice::from_ref(value))
            .map(|name| name.0.clone())
            .unwrap_or_else(|| {
                format!("_goml_{}__{}", LangItemId::Option.key(), ty_compact(value))
            });
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
            name: hashmap_lookup_helper_fn_name(ty),
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
                        value: Some(hashmap_key_hash_expr(
                            key,
                            &key_go_ty,
                            &hash_impl,
                            &hash_fn_ty,
                            hash_trait,
                        )),
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
            name: hashmap_helper_fn_name(IntrinsicId::HashMapGet.source_name(), ty),
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
                                name: hashmap_lookup_helper_fn_name(ty),
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
            name: hashmap_helper_fn_name(IntrinsicId::HashMapSet.source_name(), ty),
            params: vec![
                ("m".to_string(), map_ptr_go_ty.clone()),
                ("key".to_string(), key_go_ty.clone()),
                ("value".to_string(), value_go_ty.clone()),
            ],
            ret_ty: Some(goty::GoType::TUnit),
            body: goast::Block {
                stmts: {
                    let mut stmts = vec![goast::Stmt::VarDecl {
                        name: "reuse_index".to_string(),
                        ty: goty::GoType::TInt32,
                        value: Some(goast::Expr::Int {
                            value: "-1".to_string(),
                            ty: goty::GoType::TInt32,
                        }),
                    }];
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
                        value: Some(hashmap_key_hash_expr(
                            key,
                            &key_go_ty,
                            &hash_impl,
                            &hash_fn_ty,
                            hash_trait,
                        )),
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
                            goast::Stmt::If {
                                cond: goast::Expr::BinaryOp {
                                    op: GoBinaryOp::And,
                                    lhs: Box::new(goast::Expr::UnaryOp {
                                        op: goast::GoUnaryOp::Not,
                                        expr: Box::new(goast::Expr::FieldAccess {
                                            obj: Box::new(goast::Expr::Var {
                                                name: "entry".to_string(),
                                                ty: entry_go_ty.clone(),
                                            }),
                                            field: "active".to_string(),
                                            ty: goty::GoType::TBool,
                                        }),
                                        ty: goty::GoType::TBool,
                                    }),
                                    rhs: Box::new(goast::Expr::BinaryOp {
                                        op: GoBinaryOp::Less,
                                        lhs: Box::new(goast::Expr::Var {
                                            name: "reuse_index".to_string(),
                                            ty: goty::GoType::TInt32,
                                        }),
                                        rhs: Box::new(goast::Expr::Int {
                                            value: "0".to_string(),
                                            ty: goty::GoType::TInt32,
                                        }),
                                        ty: goty::GoType::TBool,
                                    }),
                                    ty: goty::GoType::TBool,
                                },
                                then: goast::Block {
                                    stmts: vec![goast::Stmt::Assignment {
                                        name: "reuse_index".to_string(),
                                        value: goast::Expr::Var {
                                            name: "i".to_string(),
                                            ty: goty::GoType::TInt32,
                                        },
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

                    stmts.push(goast::Stmt::If {
                        cond: goast::Expr::BinaryOp {
                            op: GoBinaryOp::GreaterEq,
                            lhs: Box::new(goast::Expr::Var {
                                name: "reuse_index".to_string(),
                                ty: goty::GoType::TInt32,
                            }),
                            rhs: Box::new(goast::Expr::Int {
                                value: "0".to_string(),
                                ty: goty::GoType::TInt32,
                            }),
                            ty: goty::GoType::TBool,
                        },
                        then: goast::Block {
                            stmts: vec![
                                goast::Stmt::IndexAssign {
                                    array: goast::Expr::Var {
                                        name: "bucket".to_string(),
                                        ty: bucket_slice_go_ty.clone(),
                                    },
                                    index: goast::Expr::Var {
                                        name: "reuse_index".to_string(),
                                        ty: goty::GoType::TInt32,
                                    },
                                    value: hashmap_entry_literal(
                                        &entry_go_ty,
                                        &key_go_ty,
                                        &value_go_ty,
                                    ),
                                },
                                increment_hashmap_len(&map_ptr_go_ty),
                                goast::Stmt::Return {
                                    expr: Some(goast::Expr::Unit {
                                        ty: goty::GoType::TUnit,
                                    }),
                                },
                            ],
                        },
                        else_: None,
                    });

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
                                hashmap_entry_literal(&entry_go_ty, &key_go_ty, &value_go_ty),
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

                    stmts.push(increment_hashmap_len(&map_ptr_go_ty));

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
            name: hashmap_helper_fn_name(IntrinsicId::HashMapRemove.source_name(), ty),
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
                        value: Some(hashmap_key_hash_expr(
                            key,
                            &key_go_ty,
                            &hash_impl,
                            &hash_fn_ty,
                            hash_trait,
                        )),
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
                                        goast::Stmt::VarDecl {
                                            name: "zero".to_string(),
                                            ty: entry_go_ty.clone(),
                                            value: None,
                                        },
                                        goast::Stmt::IndexAssign {
                                            array: goast::Expr::Var {
                                                name: "bucket".to_string(),
                                                ty: bucket_slice_go_ty.clone(),
                                            },
                                            index: goast::Expr::Var {
                                                name: "i".to_string(),
                                                ty: goty::GoType::TInt32,
                                            },
                                            value: goast::Expr::Var {
                                                name: "zero".to_string(),
                                                ty: entry_go_ty.clone(),
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

fn hashmap_key_hash_expr(
    key: &tast::Ty,
    key_go_ty: &goty::GoType,
    hash_impl: &str,
    hash_fn_ty: &goty::GoType,
    hash_trait: &tast::TastIdent,
) -> goast::Expr {
    if matches!(key, tast::Ty::TDyn { trait_name } if trait_name == &hash_trait.0) {
        let dyn_go_ty = goty::GoType::TName {
            name: go_dyn_struct_name(&hash_trait.0),
        };
        return goast::Expr::Call {
            func: Box::new(goast::Expr::FieldAccess {
                obj: Box::new(goast::Expr::FieldAccess {
                    obj: Box::new(goast::Expr::Var {
                        name: "key".to_string(),
                        ty: dyn_go_ty.clone(),
                    }),
                    field: "vtable".to_string(),
                    ty: goty::GoType::TPointer {
                        elem: Box::new(goty::GoType::TName {
                            name: go_generated_ident("dyn__Hash_vtable"),
                        }),
                    },
                }),
                field: "hash".to_string(),
                ty: goty::GoType::TFunc {
                    params: vec![goty::GoType::TName {
                        name: "any".to_string(),
                    }],
                    ret_ty: Box::new(goty::GoType::TUint64),
                },
            }),
            args: vec![goast::Expr::FieldAccess {
                obj: Box::new(goast::Expr::Var {
                    name: "key".to_string(),
                    ty: dyn_go_ty,
                }),
                field: "data".to_string(),
                ty: goty::GoType::TName {
                    name: "any".to_string(),
                },
            }],
            ty: goty::GoType::TUint64,
        };
    }

    goast::Expr::Call {
        func: Box::new(goast::Expr::Var {
            name: hash_impl.to_string(),
            ty: hash_fn_ty.clone(),
        }),
        args: vec![goast::Expr::Var {
            name: "key".to_string(),
            ty: key_go_ty.clone(),
        }],
        ty: goty::GoType::TUint64,
    }
}

fn unit_to_string() -> goast::Fn {
    goast::Fn {
        name: runtime_hook_fn_name(RuntimeHookId::UnitToString),
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
        name: runtime_hook_fn_name(RuntimeHookId::BoolToString),
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

fn to_string_fn(id: RuntimeHookId, ty: goty::GoType) -> goast::Fn {
    let fmt_spec = match &ty {
        goty::GoType::TFloat32 | goty::GoType::TFloat64 => "%g",
        _ => "%d",
    };
    let fmt_ty = goty::GoType::TFunc {
        params: vec![goty::GoType::TString, ty.clone()],
        ret_ty: Box::new(goty::GoType::TString),
    };
    goast::Fn {
        name: runtime_hook_fn_name(id),
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
    to_string_fn(RuntimeHookId::Int8ToString, goty::GoType::TInt8)
}

fn int16_to_string() -> goast::Fn {
    to_string_fn(RuntimeHookId::Int16ToString, goty::GoType::TInt16)
}

fn int32_to_string() -> goast::Fn {
    to_string_fn(RuntimeHookId::Int32ToString, goty::GoType::TInt32)
}

fn int64_to_string() -> goast::Fn {
    to_string_fn(RuntimeHookId::Int64ToString, goty::GoType::TInt64)
}

fn uint8_to_string() -> goast::Fn {
    to_string_fn(RuntimeHookId::Uint8ToString, goty::GoType::TUint8)
}

fn uint16_to_string() -> goast::Fn {
    to_string_fn(RuntimeHookId::Uint16ToString, goty::GoType::TUint16)
}

fn uint32_to_string() -> goast::Fn {
    to_string_fn(RuntimeHookId::Uint32ToString, goty::GoType::TUint32)
}

fn uint64_to_string() -> goast::Fn {
    to_string_fn(RuntimeHookId::Uint64ToString, goty::GoType::TUint64)
}

fn float32_to_string() -> goast::Fn {
    to_string_fn(RuntimeHookId::Float32ToString, goty::GoType::TFloat32)
}

fn float64_to_string() -> goast::Fn {
    to_string_fn(RuntimeHookId::Float64ToString, goty::GoType::TFloat64)
}

fn char_to_string() -> goast::Fn {
    goast::Fn {
        name: runtime_hook_fn_name(RuntimeHookId::CharToString),
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
        name: runtime_hook_fn_name(RuntimeHookId::Int8Hash),
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
        name: runtime_hook_fn_name(RuntimeHookId::Int16Hash),
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
        name: runtime_hook_fn_name(RuntimeHookId::Int32Hash),
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
        name: runtime_hook_fn_name(RuntimeHookId::CharHash),
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
        name: runtime_hook_fn_name(RuntimeHookId::Int64Hash),
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
        name: runtime_hook_fn_name(RuntimeHookId::Uint8Hash),
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
        name: runtime_hook_fn_name(RuntimeHookId::Uint16Hash),
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
        name: runtime_hook_fn_name(RuntimeHookId::Uint32Hash),
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
        name: runtime_hook_fn_name(RuntimeHookId::Float32Hash),
        params: vec![("x".to_string(), goty::GoType::TFloat32)],
        ret_ty: Some(goty::GoType::TUint64),
        body: goast::Block {
            stmts: vec![
                goast::Stmt::If {
                    cond: goast::Expr::BinaryOp {
                        op: GoBinaryOp::Eq,
                        lhs: Box::new(goast::Expr::Var {
                            name: "x".to_string(),
                            ty: goty::GoType::TFloat32,
                        }),
                        rhs: Box::new(goast::Expr::Float {
                            value: 0.0,
                            ty: goty::GoType::TFloat32,
                        }),
                        ty: goty::GoType::TBool,
                    },
                    then: goast::Block {
                        stmts: vec![goast::Stmt::Return {
                            expr: Some(goast::Expr::Int {
                                value: "0".to_string(),
                                ty: goty::GoType::TUint64,
                            }),
                        }],
                    },
                    else_: None,
                },
                goast::Stmt::Return {
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
                },
            ],
        },
    }
}

fn float64_hash() -> goast::Fn {
    goast::Fn {
        name: runtime_hook_fn_name(RuntimeHookId::Float64Hash),
        params: vec![("x".to_string(), goty::GoType::TFloat64)],
        ret_ty: Some(goty::GoType::TUint64),
        body: goast::Block {
            stmts: vec![
                goast::Stmt::If {
                    cond: goast::Expr::BinaryOp {
                        op: GoBinaryOp::Eq,
                        lhs: Box::new(goast::Expr::Var {
                            name: "x".to_string(),
                            ty: goty::GoType::TFloat64,
                        }),
                        rhs: Box::new(goast::Expr::Float {
                            value: 0.0,
                            ty: goty::GoType::TFloat64,
                        }),
                        ty: goty::GoType::TBool,
                    },
                    then: goast::Block {
                        stmts: vec![goast::Stmt::Return {
                            expr: Some(goast::Expr::Int {
                                value: "0".to_string(),
                                ty: goty::GoType::TUint64,
                            }),
                        }],
                    },
                    else_: None,
                },
                goast::Stmt::Return {
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
                },
            ],
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
        name: runtime_hook_fn_name(RuntimeHookId::StringHash),
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
        name: runtime_hook_fn_name(RuntimeHookId::StringLen),
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
        name: runtime_hook_fn_name(RuntimeHookId::StringGet),
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

fn string_byte_slice() -> goast::Fn {
    goast::Fn {
        name: runtime_hook_fn_name(RuntimeHookId::StringByteSlice),
        params: vec![
            ("s".to_string(), goty::GoType::TString),
            ("start".to_string(), goty::GoType::TInt32),
            ("end".to_string(), goty::GoType::TInt32),
        ],
        ret_ty: Some(goty::GoType::TString),
        body: goast::Block {
            stmts: vec![goast::Stmt::Return {
                expr: Some(goast::Expr::Slice {
                    array: Box::new(goast::Expr::Var {
                        name: "s".to_string(),
                        ty: goty::GoType::TString,
                    }),
                    start: Box::new(goast::Expr::Var {
                        name: "start".to_string(),
                        ty: goty::GoType::TInt32,
                    }),
                    end: Box::new(goast::Expr::Var {
                        name: "end".to_string(),
                        ty: goty::GoType::TInt32,
                    }),
                    ty: goty::GoType::TString,
                }),
            }],
        },
    }
}

fn string_print() -> goast::Fn {
    goast::Fn {
        name: runtime_hook_fn_name(RuntimeHookId::StringPrint),
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
        name: runtime_hook_fn_name(RuntimeHookId::StringPrintln),
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
        name: missing_base_fn_name(),
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
