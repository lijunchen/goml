use ast::ast::Uident;
use std::collections::HashMap;

use crate::{
    anf::{self, AExpr},
    env::Env,
    go::goty::GoType,
    tast,
};

use super::goty;
use super::runtime;

#[derive(Debug)]
pub struct File {
    pub toplevels: Vec<Item>,
}

#[derive(Debug)]
pub enum Item {
    Interface(Interface),
    Struct(Struct),
    Fn(Fn),
    EmbededRawString(EmbededRawString),
}

#[derive(Debug)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
}

#[derive(Debug)]
pub struct Interface {
    pub name: String,
    pub methods: Vec<MethodElem>,
}

#[derive(Debug)]
pub struct MethodElem {
    pub name: String,
    pub params: Vec<(String, goty::GoType)>,
    pub ret: Option<goty::GoType>,
}

#[derive(Debug)]
pub struct Field {
    pub name: String,
    pub ty: goty::GoType,
}

#[derive(Debug)]
pub struct Fn {
    pub name: String,
    pub params: Vec<(String, goty::GoType)>,
    pub ret_ty: Option<goty::GoType>,
    pub body: Block,
}

#[derive(Debug)]
pub struct EmbededRawString {
    pub value: String,
}

#[derive(Debug)]
pub struct Receiver {
    pub name: String,
    pub ty: goty::GoType,
}

#[derive(Debug)]
pub struct Method {
    pub receiver: Receiver,
    pub name: String,
    pub params: Vec<(String, goty::GoType)>,
    pub body: Block,
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub enum Expr {
    Nil {
        ty: goty::GoType,
    },
    Void {
        ty: goty::GoType,
    },
    Unit {
        ty: goty::GoType,
    },
    Var {
        name: String,
        ty: goty::GoType,
    },
    Bool {
        value: bool,
        ty: goty::GoType,
    },
    Int {
        value: i32,
        ty: goty::GoType,
    },
    String {
        value: String,
        ty: goty::GoType,
    },
    Call {
        func: String,
        args: Vec<Expr>,
        ty: goty::GoType,
    },
    FieldAccess {
        obj: Box<Expr>,
        field: String,
        ty: goty::GoType,
    },
    Cast {
        expr: Box<Expr>,
        ty: goty::GoType,
    },
    StructLiteral {
        fields: Vec<(String, Expr)>,
        ty: goty::GoType,
    },
    Block {
        stmts: Vec<Stmt>,
        expr: Option<Box<Expr>>,
        ty: goty::GoType,
    },
}

impl Expr {
    pub fn get_ty(&self) -> &goty::GoType {
        match self {
            Expr::Nil { ty }
            | Expr::Void { ty }
            | Expr::Unit { ty }
            | Expr::Var { ty, .. }
            | Expr::Bool { ty, .. }
            | Expr::Int { ty, .. }
            | Expr::String { ty, .. }
            | Expr::Call { ty, .. }
            | Expr::FieldAccess { ty, .. }
            | Expr::Cast { ty, .. }
            | Expr::StructLiteral { ty, .. }
            | Expr::Block { ty, .. } => ty,
        }
    }
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    VarDecl {
        name: String,
        ty: goty::GoType,
        value: Option<Expr>,
    },
    Assignment {
        name: String,
        value: Expr,
    },
    Return {
        expr: Option<Expr>,
    },
    If {
        cond: Expr,
        then: Block,
        else_: Option<Block>,
    },
    // switch <expr> { case <value>: ...; default: ... }
    SwitchExpr {
        expr: Expr,
        cases: Vec<(Expr, Block)>,
        default: Option<Block>,
    },
    // switch _ := <expr>.(type) { case <Type>: ...; default: ... }
    SwitchType {
        bind: String,
        expr: Expr,
        cases: Vec<(goty::GoType, Block)>,
        default: Option<Block>,
    },
}

fn tast_ty_to_go_type(ty: &tast::Ty) -> goty::GoType {
    match ty {
        tast::Ty::TVar { .. } => {
            panic!("unresolved type variable ")
        }
        tast::Ty::TUnit => goty::GoType::TUnit,
        tast::Ty::TBool => goty::GoType::TBool,
        tast::Ty::TInt => goty::GoType::TInt,
        tast::Ty::TString => goty::GoType::TString,
        tast::Ty::TTuple { typs } => {
            // compile to struct with field _0, _1, ...
            let name = go_type_name_for(ty);
            goty::GoType::TStruct {
                name,
                fields: typs
                    .iter()
                    .enumerate()
                    .map(|(i, t)| (format!("_{}", i), tast_ty_to_go_type(t)))
                    .collect(),
            }
        }
        tast::Ty::TApp { name, args } => {
            if !args.is_empty() {
                unreachable!("generic types not supported in Go backend");
            }
            goty::GoType::TName {
                name: name.0.clone(),
            }
        }
        tast::Ty::TParam { name } => {
            panic!("unresolved type parameter {}", name)
        }
        tast::Ty::TFunc { params, ret_ty } => {
            let param_tys = params.iter().map(tast_ty_to_go_type).collect();
            let ret_ty = Box::new(tast_ty_to_go_type(ret_ty));
            goty::GoType::TFunc {
                params: param_tys,
                ret_ty,
            }
        }
    }
}

fn compile_imm(env: &Env, imm: &anf::ImmExpr) -> Expr {
    match imm {
        anf::ImmExpr::ImmVar { name, ty: _ } => Expr::Var {
            name: name.clone(),
            ty: tast_ty_to_go_type(&imm_ty(imm)),
        },
        anf::ImmExpr::ImmUnit { ty: _ } => Expr::Unit {
            ty: goty::GoType::TUnit,
        },
        anf::ImmExpr::ImmBool { value, ty: _ } => Expr::Bool {
            value: *value,
            ty: goty::GoType::TBool,
        },
        anf::ImmExpr::ImmInt { value, ty: _ } => Expr::Int {
            value: *value,
            ty: goty::GoType::TInt,
        },
        anf::ImmExpr::ImmString { value, ty: _ } => Expr::String {
            value: value.clone(),
            ty: goty::GoType::TString,
        },
        anf::ImmExpr::ImmTag { index, ty } => Expr::StructLiteral {
            fields: vec![],
            ty: variant_ty_by_index(env, ty, *index),
        },
    }
}

fn imm_ty(imm: &anf::ImmExpr) -> tast::Ty {
    match imm {
        anf::ImmExpr::ImmVar { ty, .. }
        | anf::ImmExpr::ImmUnit { ty }
        | anf::ImmExpr::ImmBool { ty, .. }
        | anf::ImmExpr::ImmInt { ty, .. }
        | anf::ImmExpr::ImmString { ty, .. }
        | anf::ImmExpr::ImmTag { ty, .. } => ty.clone(),
    }
}

fn cexpr_ty(e: &anf::CExpr) -> goty::GoType {
    let t = match e {
        anf::CExpr::CImm { imm } => imm_ty(imm),
        anf::CExpr::EConstr { ty, .. }
        | anf::CExpr::ETuple { ty, .. }
        | anf::CExpr::EMatch { ty, .. }
        | anf::CExpr::EIf { ty, .. }
        | anf::CExpr::EConstrGet { ty, .. }
        | anf::CExpr::ECall { ty, .. }
        | anf::CExpr::EProj { ty, .. } => ty.clone(),
    };
    tast_ty_to_go_type(&t)
}

fn lookup_variant_name(env: &Env, ty: &tast::Ty, index: usize) -> String {
    if let tast::Ty::TApp { name, .. } = ty
        && let Some(def) = env.enums.get(name)
    {
        let (vname, _fields) = &def.variants[index];
        return vname.0.clone();
    }
    panic!(
        "Cannot resolve variant name for ty {:?} index {}",
        ty, index
    );
}

fn variant_ty_by_index(env: &Env, ty: &tast::Ty, index: usize) -> goty::GoType {
    let vname = lookup_variant_name(env, ty, index);
    let ty = tast::Ty::TApp {
        name: Uident::new(&vname),
        args: vec![],
    };
    tast_ty_to_go_type(&ty)
}

fn tuple_to_go_struct_type(_env: &Env, ty: &tast::Ty) -> goty::GoType {
    if let tast::Ty::TTuple { typs } = ty {
        let name = go_type_name_for(ty);
        goty::GoType::TStruct {
            name,
            fields: typs
                .iter()
                .enumerate()
                .map(|(i, t)| (format!("_{}", i), tast_ty_to_go_type(t)))
                .collect(),
        }
    } else {
        panic!("expected tuple type, got {:?}", ty);
    }
}

fn compile_cexpr(env: &Env, e: &anf::CExpr) -> Expr {
    match e {
        anf::CExpr::CImm { imm } => compile_imm(env, imm),
        anf::CExpr::EConstr { index, args, ty } => {
            let variant_ty = variant_ty_by_index(env, ty, *index);
            let fields = args
                .iter()
                .enumerate()
                .map(|(i, a)| (format!("_{}", i), compile_imm(env, a)))
                .collect();
            Expr::StructLiteral {
                ty: variant_ty,
                fields,
            }
        }
        anf::CExpr::ETuple { items, ty } => {
            let fields = items
                .iter()
                .enumerate()
                .map(|(i, a)| (format!("_{}", i), compile_imm(env, a)))
                .collect();
            Expr::StructLiteral {
                ty: tuple_to_go_struct_type(env, ty),
                fields,
            }
        }
        anf::CExpr::EMatch { expr, .. } => match imm_ty(expr) {
            // Boolean matches are handled as statements (not expressions) in Go.
            tast::Ty::TBool => {
                panic!("boolean match should be lowered to Stmt::If in compile_aexpr")
            }
            _ => {
                panic!("EMatch should be lowered in compile_aexpr/compile_aexpr_assign")
            }
        },
        anf::CExpr::EIf {
            cond: _,
            then: _,
            else_: _,
            ty: _,
        } => panic!("EIf should be lowered to Stmt::If in compile_aexpr"),
        anf::CExpr::EConstrGet {
            expr,
            variant_index,
            field_index,
            ty: _,
        } => {
            let obj = compile_imm(env, expr);
            // Determine variant cast type from the scrutinee's enum type, not the field type
            let enum_ty = imm_ty(expr);
            let vty = variant_ty_by_index(env, &enum_ty, *variant_index);
            Expr::FieldAccess {
                obj: Box::new(Expr::Cast {
                    expr: Box::new(obj),
                    ty: vty,
                }),
                field: format!("_{}", field_index),
                ty: tast_ty_to_go_type(&enum_ty),
            }
        }
        anf::CExpr::ECall { func, args, ty } => {
            let args = args.iter().map(|arg| compile_imm(env, arg)).collect();
            Expr::Call {
                func: func.clone(),
                args,
                ty: tast_ty_to_go_type(ty),
            }
        }
        anf::CExpr::EProj { tuple, index, ty } => {
            let obj = compile_imm(env, tuple);
            Expr::FieldAccess {
                obj: Box::new(obj),
                field: format!("_{}", index),
                ty: tast_ty_to_go_type(ty),
            }
        }
    }
}

fn compile_aexpr_assign(env: &Env, target: &str, e: anf::AExpr) -> Vec<Stmt> {
    match e {
        AExpr::ACExpr { expr } => match expr {
            anf::CExpr::CImm { .. }
            | anf::CExpr::EConstr { .. }
            | anf::CExpr::EConstrGet { .. }
            | anf::CExpr::ECall { .. }
            | anf::CExpr::EProj { .. }
            | anf::CExpr::ETuple { .. } => vec![Stmt::Assignment {
                name: target.to_string(),
                value: compile_cexpr(env, &expr),
            }],
            anf::CExpr::EIf {
                cond, then, else_, ..
            } => {
                let cond_e = compile_imm(env, &cond);
                let then_stmts = compile_aexpr_assign(env, target, *then);
                let else_stmts = compile_aexpr_assign(env, target, *else_);
                vec![Stmt::If {
                    cond: cond_e,
                    then: Block { stmts: then_stmts },
                    else_: Some(Block { stmts: else_stmts }),
                }]
            }
            anf::CExpr::EMatch {
                expr,
                arms,
                default,
                ty: _,
            } => match imm_ty(&expr) {
                tast::Ty::TUnit => {
                    // Only one possible value of unit: run the first arm or default
                    if let Some(first) = arms.first() {
                        compile_aexpr_assign(env, target, first.body.clone())
                    } else if let Some(def) = default {
                        compile_aexpr_assign(env, target, *def)
                    } else {
                        vec![]
                    }
                }
                tast::Ty::TBool => {
                    // Expression switch on boolean scrutinee
                    let mut cases = Vec::new();
                    for arm in arms {
                        if let anf::ImmExpr::ImmBool { value, .. } = arm.lhs {
                            cases.push((
                                Expr::Bool {
                                    value,
                                    ty: goty::GoType::TBool,
                                },
                                Block {
                                    stmts: compile_aexpr_assign(env, target, arm.body),
                                },
                            ));
                        } else {
                            panic!("expected ImmBool in boolean match arm");
                        }
                    }
                    let def_block = default.map(|d| Block {
                        stmts: compile_aexpr_assign(env, target, *d),
                    });
                    vec![Stmt::SwitchExpr {
                        expr: compile_imm(env, &expr),
                        cases,
                        default: def_block,
                    }]
                }
                tast::Ty::TApp { .. } => {
                    // Type switch on enum ADT
                    let mut cases = Vec::new();
                    for arm in arms {
                        if let anf::ImmExpr::ImmTag { index, ty } = arm.lhs.clone() {
                            let vty = variant_ty_by_index(env, &ty, index);
                            cases.push((
                                vty,
                                Block {
                                    stmts: compile_aexpr_assign(env, target, arm.body),
                                },
                            ));
                        } else {
                            panic!("expected ImmTag in enum match arm");
                        }
                    }
                    let def_block = default.map(|d| Block {
                        stmts: compile_aexpr_assign(env, target, *d),
                    });
                    vec![Stmt::SwitchType {
                        bind: "_".to_string(),
                        expr: compile_imm(env, &expr),
                        cases,
                        default: def_block,
                    }]
                }
                _ => panic!("unsupported scrutinee type for match in Go backend"),
            },
        },
        AExpr::ALet {
            name,
            value,
            body,
            ty: _,
        } => {
            let mut out = Vec::new();
            // Declare the inner variable first
            out.push(Stmt::VarDecl {
                name: name.clone(),
                ty: cexpr_ty(&value),
                value: None,
            });

            // Initialize it (may itself require if-lowering)
            match &*value {
                anf::CExpr::EIf { .. } | anf::CExpr::EMatch { .. } => {
                    out.extend(compile_aexpr_assign(
                        env,
                        &name,
                        AExpr::ACExpr {
                            expr: *value.clone(),
                        },
                    ));
                }
                _ => {
                    out.push(Stmt::Assignment {
                        name: name.clone(),
                        value: compile_cexpr(env, &value),
                    });
                }
            }

            // Continue with body to finally assign into the target
            out.extend(compile_aexpr_assign(env, target, *body));
            out
        }
    }
}

fn compile_aexpr(env: &Env, e: anf::AExpr) -> Vec<Stmt> {
    let mut stmts = Vec::new();
    match e {
        AExpr::ACExpr { expr } => match expr {
            // Lower conditional expressions to if-statements with returns in branches
            anf::CExpr::EIf {
                cond, then, else_, ..
            } => {
                let cond_e = compile_imm(env, &cond);
                let then_block = Block {
                    stmts: compile_aexpr(env, *then),
                };
                let else_block = Block {
                    stmts: compile_aexpr(env, *else_),
                };
                stmts.push(Stmt::If {
                    cond: cond_e,
                    then: then_block,
                    else_: Some(else_block),
                });
            }
            anf::CExpr::EMatch {
                expr,
                arms,
                default,
                ty: _,
            } => match imm_ty(&expr) {
                tast::Ty::TUnit => {
                    if let Some(first) = arms.first() {
                        stmts.extend(compile_aexpr(env, first.body.clone()));
                    } else if let Some(def) = default {
                        stmts.extend(compile_aexpr(env, *def));
                    }
                }
                tast::Ty::TBool => {
                    let mut cases = Vec::new();
                    for arm in arms {
                        if let anf::ImmExpr::ImmBool { value, .. } = arm.lhs {
                            cases.push((
                                Expr::Bool {
                                    value,
                                    ty: goty::GoType::TBool,
                                },
                                Block {
                                    stmts: compile_aexpr(env, arm.body),
                                },
                            ));
                        } else {
                            panic!("expected ImmBool in boolean match arm");
                        }
                    }
                    let def_block = default.map(|d| Block {
                        stmts: compile_aexpr(env, *d),
                    });
                    stmts.push(Stmt::SwitchExpr {
                        expr: compile_imm(env, &expr),
                        cases,
                        default: def_block,
                    });
                }
                tast::Ty::TApp { .. } => {
                    let mut cases = Vec::new();
                    for arm in arms {
                        if let anf::ImmExpr::ImmTag { index, ty } = arm.lhs.clone() {
                            let vty = variant_ty_by_index(env, &ty, index);
                            cases.push((
                                vty,
                                Block {
                                    stmts: compile_aexpr(env, arm.body),
                                },
                            ));
                        } else {
                            panic!("expected ImmTag in enum match arm");
                        }
                    }
                    let def_block = default.map(|d| Block {
                        stmts: compile_aexpr(env, *d),
                    });
                    stmts.push(Stmt::SwitchType {
                        bind: "_".to_string(),
                        expr: compile_imm(env, &expr),
                        cases,
                        default: def_block,
                    });
                }
                _ => panic!("unsupported scrutinee type for match in Go backend"),
            },
            _ => {
                let e = compile_cexpr(env, &expr);
                match e.get_ty() {
                    goty::GoType::TVoid => {}
                    _ => {
                        stmts.push(Stmt::Return { expr: Some(e) });
                    }
                }
            }
        },
        AExpr::ALet {
            name,
            value,
            body,
            ty,
        } => {
            match &*value {
                // If RHS needs statements, declare then fill via if-lowering
                anf::CExpr::EIf { .. } | anf::CExpr::EMatch { .. } => {
                    stmts.push(Stmt::VarDecl {
                        name: name.clone(),
                        ty: cexpr_ty(&value),
                        value: None,
                    });
                    stmts.extend(compile_aexpr_assign(
                        env,
                        &name,
                        AExpr::ACExpr {
                            expr: *value.clone(),
                        },
                    ));
                }
                anf::CExpr::ETuple { .. } => {
                    stmts.push(Stmt::VarDecl {
                        name: name.clone(),
                        ty: cexpr_ty(&value),
                        value: Some(compile_cexpr(env, &value)),
                    });
                }
                _ => {
                    stmts.push(Stmt::VarDecl {
                        name: name.clone(),
                        ty: cexpr_ty(&value),
                        value: Some(compile_cexpr(env, &value)),
                    });
                }
            }
            stmts.extend(compile_aexpr(env, *body));
        }
    }
    stmts
}

fn compile_fn(env: &Env, f: anf::Fn) -> Fn {
    let mut params = Vec::new();
    for (name, ty) in f.params {
        params.push((name, tast_ty_to_go_type(&ty)));
    }
    let stmts = compile_aexpr(env, f.body);
    let patched_name = if f.name == "main" {
        "main0".to_string()
    } else {
        f.name.clone()
    };
    Fn {
        name: patched_name,
        params,
        ret_ty: Some(tast_ty_to_go_type(&f.ret_ty)),
        body: Block { stmts },
    }
}

mod anf_renamer {
    use crate::anf;

    pub fn rename(file: anf::File) -> anf::File {
        anf::File {
            toplevels: file.toplevels.into_iter().map(rename_fn).collect(),
        }
    }

    fn rename_fn(f: anf::Fn) -> anf::Fn {
        anf::Fn {
            name: f.name,
            params: f
                .params
                .into_iter()
                .map(|(n, t)| (n.replace("/", "__"), t))
                .collect(),
            ret_ty: f.ret_ty,
            body: rename_aexpr(f.body),
        }
    }

    fn rename_imm(imm: anf::ImmExpr) -> anf::ImmExpr {
        match imm {
            anf::ImmExpr::ImmVar { name, ty } => anf::ImmExpr::ImmVar {
                name: name.replace("/", "__"),
                ty,
            },
            anf::ImmExpr::ImmUnit { ty } => anf::ImmExpr::ImmUnit { ty },
            anf::ImmExpr::ImmBool { value, ty } => anf::ImmExpr::ImmBool { value, ty },
            anf::ImmExpr::ImmInt { value, ty } => anf::ImmExpr::ImmInt { value, ty },
            anf::ImmExpr::ImmString { value, ty } => anf::ImmExpr::ImmString { value, ty },
            anf::ImmExpr::ImmTag { index, ty } => anf::ImmExpr::ImmTag { index, ty },
        }
    }

    fn rename_cexpr(e: anf::CExpr) -> anf::CExpr {
        match e {
            anf::CExpr::CImm { imm } => anf::CExpr::CImm {
                imm: rename_imm(imm),
            },
            anf::CExpr::EConstr { index, args, ty } => anf::CExpr::EConstr {
                index,
                args: args.into_iter().map(rename_imm).collect(),
                ty,
            },
            anf::CExpr::ETuple { items, ty } => anf::CExpr::ETuple {
                items: items.into_iter().map(rename_imm).collect(),
                ty,
            },
            anf::CExpr::EMatch {
                expr,
                arms,
                default,
                ty,
            } => anf::CExpr::EMatch {
                expr: Box::new(rename_imm(*expr)),
                arms: arms
                    .into_iter()
                    .map(|arm| anf::Arm {
                        lhs: rename_imm(arm.lhs),
                        body: rename_aexpr(arm.body),
                    })
                    .collect(),
                default: default.map(|d| Box::new(rename_aexpr(*d))),
                ty,
            },
            anf::CExpr::EIf {
                cond,
                then,
                else_,
                ty,
            } => anf::CExpr::EIf {
                cond: Box::new(rename_imm(*cond)),
                then: Box::new(rename_aexpr(*then)),
                else_: Box::new(rename_aexpr(*else_)),
                ty,
            },
            anf::CExpr::EConstrGet {
                expr,
                variant_index,
                field_index,
                ty,
            } => anf::CExpr::EConstrGet {
                expr: Box::new(rename_imm(*expr)),
                variant_index,
                field_index,
                ty,
            },
            anf::CExpr::ECall { func, args, ty } => anf::CExpr::ECall {
                func: func.replace("/", "__"),
                args: args.into_iter().map(rename_imm).collect(),
                ty,
            },
            anf::CExpr::EProj { tuple, index, ty } => anf::CExpr::EProj {
                tuple: Box::new(rename_imm(*tuple)),
                index,
                ty,
            },
        }
    }

    fn rename_aexpr(e: anf::AExpr) -> anf::AExpr {
        match e {
            anf::AExpr::ACExpr { expr } => anf::AExpr::ACExpr {
                expr: rename_cexpr(expr),
            },
            anf::AExpr::ALet {
                name,
                value,
                body,
                ty,
            } => anf::AExpr::ALet {
                name: name.replace("/", "__"),
                value: Box::new(rename_cexpr(*value)),
                body: Box::new(rename_aexpr(*body)),
                ty,
            },
        }
    }
}

fn collect_tuple_types(file: &anf::File) -> HashMap<String, goty::GoType> {
    let mut env = HashMap::new();
    for item in file.toplevels.iter() {
        go_item(&mut env, item);
    }
    return env;

    ///////////////////////////////////////////////////////////////////////////

    fn go_item(env: &mut HashMap<String, GoType>, item: &anf::Fn) {
        go_aexpr(env, &item.body);
    }

    fn go_aexpr(env: &mut HashMap<String, GoType>, aexpr: &anf::AExpr) {
        match aexpr {
            AExpr::ACExpr { expr } => {
                go_cexpr(env, expr);
            }
            AExpr::ALet {
                name: _,
                value,
                body,
                ty,
            } => {
                go_cexpr(env, value);
                go_aexpr(env, body);
                go_type(env, ty)
            }
        }
    }

    fn go_cexpr(env: &mut HashMap<String, GoType>, cexpr: &anf::CExpr) {
        match cexpr {
            anf::CExpr::CImm { imm } => go_immexpr(env, imm),
            anf::CExpr::EConstr { index: _, args, ty } => {
                for a in args {
                    go_immexpr(env, a);
                }
                go_type(env, ty);
            }
            anf::CExpr::ETuple { items, ty } => {
                for i in items {
                    go_immexpr(env, i);
                }
                go_type(env, ty);
            }
            anf::CExpr::EMatch {
                expr,
                arms,
                default,
                ty,
            } => {
                go_immexpr(env, expr);
                for arm in arms {
                    go_immexpr(env, &arm.lhs);
                    go_aexpr(env, &arm.body);
                }
                if let Some(d) = default {
                    go_aexpr(env, d);
                }
                go_type(env, ty);
            }
            anf::CExpr::EIf {
                cond,
                then,
                else_,
                ty,
            } => {
                go_immexpr(env, cond);
                go_aexpr(env, then);
                go_aexpr(env, else_);
                go_type(env, ty);
            }
            anf::CExpr::EConstrGet {
                expr,
                variant_index: _,
                field_index: _,
                ty,
            } => {
                go_immexpr(env, expr);
                go_type(env, ty)
            }
            anf::CExpr::ECall { func: _, args, ty } => {
                for a in args {
                    go_immexpr(env, a);
                }
                go_type(env, ty)
            }
            anf::CExpr::EProj {
                tuple,
                index: _,
                ty,
            } => {
                go_immexpr(env, tuple);
                go_type(env, ty)
            }
        }
    }

    fn go_immexpr(env: &mut HashMap<String, GoType>, imm: &anf::ImmExpr) {
        match imm {
            anf::ImmExpr::ImmVar { name: _, ty } => go_type(env, ty),
            anf::ImmExpr::ImmUnit { ty } => go_type(env, ty),
            anf::ImmExpr::ImmBool { value: _, ty } => go_type(env, ty),
            anf::ImmExpr::ImmInt { value: _, ty } => go_type(env, ty),
            anf::ImmExpr::ImmString { value: _, ty } => go_type(env, ty),
            anf::ImmExpr::ImmTag { index: _, ty } => go_type(env, ty),
        }
    }

    fn go_type(env: &mut HashMap<String, GoType>, ty: &tast::Ty) {
        match ty {
            tast::Ty::TVar(_) => {}
            tast::Ty::TUnit => {}
            tast::Ty::TBool => {}
            tast::Ty::TInt => {}
            tast::Ty::TString => {}
            tast::Ty::TTuple { typs } => {
                // gen go struct type
                let name = go_type_name_for(ty);
                for t in typs.iter() {
                    go_type(env, t)
                }
                if !env.contains_key(&name) {
                    let mut fields = Vec::new();
                    for t in typs.iter().enumerate() {
                        fields.push((format!("_{}", t.0), tast_ty_to_go_type(t.1)))
                    }
                    env.insert(name.clone(), GoType::TStruct { name, fields });
                    for t in typs {
                        go_type(env, t);
                    }
                }
            }
            tast::Ty::TApp { name: _, args } => {
                for t in args {
                    go_type(env, t);
                }
            }
            tast::Ty::TParam { name: _ } => {}
            tast::Ty::TFunc { params, ret_ty } => {
                for t in params {
                    go_type(env, t);
                }
                go_type(env, ret_ty);
            }
        }
    }
}

pub fn go_file(env: &Env, file: anf::File) -> File {
    let mut all = Vec::new();

    all.extend(runtime::make_runtime());

    let tuple_types = collect_tuple_types(&file);

    for tt in tuple_types {
        all.push(Item::Struct(Struct {
            name: tt.0,
            fields: match tt.1 {
                goty::GoType::TStruct { name: _, fields } => {
                    let fields = fields.iter().map(|(n, t)| Field {
                        name: n.clone(),
                        ty: t.clone(),
                    });
                    fields.collect()
                }
                _ => panic!("expected struct type"),
            },
            methods: vec![],
        }));
    }

    let file = anf_renamer::rename(file);

    let mut toplevels = gen_type_definition(env);
    for item in file.toplevels {
        let gof = compile_fn(env, item);
        toplevels.push(Item::Fn(gof));
    }
    all.extend(toplevels);
    all.push(Item::Fn(Fn {
        name: "main".to_string(),
        params: vec![],
        ret_ty: None,
        body: Block {
            stmts: vec![Stmt::Expr(Expr::Call {
                func: "main0".to_string(),
                args: vec![],
                ty: goty::GoType::TVoid,
            })],
        },
    }));
    File { toplevels: all }
}

#[allow(unused)]
fn gen_type_definition(env: &Env) -> Vec<Item> {
    let mut defs = Vec::new();
    for (name, def) in env.enums.iter() {
        let type_identifier_method = format!("is{}", name.0);

        defs.push(Item::Interface(Interface {
            name: name.0.clone(),
            methods: vec![MethodElem {
                name: type_identifier_method.clone(),
                params: vec![],
                ret: None,
            }],
        }));
        for (variant_name, variant_fields) in def.variants.iter() {
            let variant_name = variant_name.0.clone();
            let mut fields = Vec::new();
            for (i, field) in variant_fields.iter().enumerate() {
                fields.push(Field {
                    name: format!("_{}", i),
                    ty: tast_ty_to_go_type(field),
                });
            }

            let methods = vec![Method {
                receiver: Receiver {
                    name: "_".to_string(),
                    ty: goty::GoType::TName {
                        name: variant_name.clone(),
                    },
                },
                name: type_identifier_method.clone(),
                params: vec![],
                body: Block { stmts: vec![] },
            }];

            defs.push(Item::Struct(Struct {
                name: variant_name,
                fields,
                methods,
            }));
        }
    }
    defs
}

fn go_type_name_for(ty: &tast::Ty) -> String {
    match ty {
        tast::Ty::TUnit => "Unit".to_string(),
        tast::Ty::TBool => "bool".to_string(),
        tast::Ty::TInt => "int".to_string(),
        tast::Ty::TString => "string".to_string(),
        tast::Ty::TApp { name, .. } => name.0.clone(),
        tast::Ty::TTuple { typs } => {
            let mut s = format!("Tuple{}", typs.len());
            for t in typs {
                s.push('_');
                s.push_str(&go_type_name_for(t).replace(['{', '}', ' ', '[', ']', ','], "_"));
            }
            s
        }
        // Fallback textual
        tast::Ty::TVar(_) | tast::Ty::TParam { .. } | tast::Ty::TFunc { .. } => format!("{:?}", ty),
    }
}

#[test]
fn test_type_gen() {
    use crate::env::EnumDef;
    use expect_test::expect;

    let mut env = Env::new();
    env.enums.insert(
        Uident::new("Tree"),
        EnumDef {
            name: Uident::new("Tree"),
            generics: vec![],
            variants: vec![
                (Uident::new("Empty"), vec![]),
                (Uident::new("Leaf"), vec![tast::Ty::TInt]),
                (
                    Uident::new("Node"),
                    vec![
                        tast::Ty::TApp {
                            name: Uident::new("Tree"),
                            args: vec![],
                        },
                        tast::Ty::TApp {
                            name: Uident::new("Tree"),
                            args: vec![],
                        },
                    ],
                ),
            ],
        },
    );

    let item = gen_type_definition(&env);
    let dummy_file = File { toplevels: item };
    expect![[r#"
        type Tree interface {
            isTree()
        }

        type Empty struct {}

        func (_ Empty) isTree() {}

        type Leaf struct {
            _0 int
        }

        func (_ Leaf) isTree() {}

        type Node struct {
            _0 Tree
            _1 Tree
        }

        func (_ Node) isTree() {}
    "#]]
    .assert_eq(&dummy_file.to_pretty(&env, 120));
}
