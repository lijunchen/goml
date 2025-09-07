use ast::ast::Uident;

use crate::{
    anf::{self, AExpr},
    env::Env,
    tast,
};

#[derive(Debug)]
pub struct File {
    pub toplevels: Vec<Item>,
}

#[derive(Debug)]
pub enum Item {
    Interface(Interface),
    Struct(Struct),
    Fn(Fn),
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
    pub params: Vec<(String, tast::Ty)>,
    pub ret: Option<tast::Ty>,
}

#[derive(Debug)]
pub struct Field {
    pub name: String,
    pub ty: tast::Ty,
}

#[derive(Debug)]
pub struct Fn {
    pub name: String,
    pub params: Vec<(String, tast::Ty)>,
    pub body: Block,
}

#[derive(Debug)]
pub struct Receiver {
    pub name: String,
    pub ty: tast::Ty,
}

#[derive(Debug)]
pub struct Method {
    pub receiver: Receiver,
    pub name: String,
    pub params: Vec<(String, tast::Ty)>,
    pub body: Block,
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub enum Expr {
    Nil,
    Var {
        name: String,
    },
    Bool {
        value: bool,
    },
    Int {
        value: i32,
    },
    String {
        value: String,
    },
    Call {
        func: String,
        args: Vec<Expr>,
    },
    FieldAccess {
        obj: Box<Expr>,
        field: String,
    },
    Cast {
        expr: Box<Expr>,
        ty: tast::Ty,
    },
    StructLiteral {
        ty: tast::Ty,
        fields: Vec<(String, Expr)>,
    },
    Block {
        stmts: Vec<Stmt>,
        expr: Option<Box<Expr>>,
    },
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    VarDecl {
        name: String,
        ty: Option<tast::Ty>,
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
}

fn compile_imm(env: &Env, imm: &anf::ImmExpr) -> Expr {
    match imm {
        anf::ImmExpr::ImmVar { name, ty: _ } => Expr::Var { name: name.clone() },
        anf::ImmExpr::ImmUnit { ty: _ } => Expr::Nil,
        anf::ImmExpr::ImmBool { value, ty: _ } => Expr::Bool { value: *value },
        anf::ImmExpr::ImmInt { value, ty: _ } => Expr::Int { value: *value },
        anf::ImmExpr::ImmString { value, ty: _ } => Expr::String {
            value: value.clone(),
        },
        anf::ImmExpr::ImmTag { index, ty } => Expr::StructLiteral {
            ty: variant_ty_by_index(env, ty, *index),
            fields: vec![],
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

fn cexpr_ty(e: &anf::CExpr) -> tast::Ty {
    match e {
        anf::CExpr::CImm { imm } => imm_ty(imm),
        anf::CExpr::EConstr { ty, .. }
        | anf::CExpr::ETuple { ty, .. }
        | anf::CExpr::EMatch { ty, .. }
        | anf::CExpr::EIf { ty, .. }
        | anf::CExpr::EConstrGet { ty, .. }
        | anf::CExpr::ECall { ty, .. }
        | anf::CExpr::EProj { ty, .. } => ty.clone(),
    }
}

fn lookup_variant_name(env: &Env, ty: &tast::Ty, index: usize) -> String {
    if let tast::Ty::TApp { name, .. } = ty {
        if let Some(def) = env.enums.get(name) {
            let (vname, _fields) = &def.variants[index];
            return vname.0.clone();
        }
    }
    panic!(
        "Cannot resolve variant name for ty {:?} index {}",
        ty, index
    );
}

fn variant_ty_by_index(env: &Env, ty: &tast::Ty, index: usize) -> tast::Ty {
    let vname = lookup_variant_name(env, ty, index);
    tast::Ty::TApp {
        name: Uident::new(&vname),
        args: vec![],
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
                ty: ty.clone(),
                fields,
            }
        }
        anf::CExpr::EMatch { ty, .. } => match ty {
            // Boolean matches are handled as statements (not expressions) in Go.
            tast::Ty::TBool => {
                panic!("boolean match should be lowered to Stmt::If in compile_aexpr")
            }
            _ => {
                panic!("not imp yet")
            }
        },
        anf::CExpr::EIf {
            cond,
            then,
            else_,
            ty: _,
        } => panic!("EIf should be lowered to Stmt::If in compile_aexpr"),
        anf::CExpr::EConstrGet {
            expr,
            variant_index,
            field_index,
            ty,
        } => {
            let obj = compile_imm(env, expr);
            let vty = variant_ty_by_index(env, ty, *variant_index);
            Expr::FieldAccess {
                obj: Box::new(Expr::Cast {
                    expr: Box::new(obj),
                    ty: vty,
                }),
                field: format!("_{}", field_index),
            }
        }
        anf::CExpr::ECall { func, args, ty: _ } => {
            let args = args.iter().map(|arg| compile_imm(env, arg)).collect();
            Expr::Call {
                func: func.clone(),
                args,
            }
        }
        anf::CExpr::EProj {
            tuple,
            index,
            ty: _,
        } => {
            let obj = compile_imm(env, tuple);
            Expr::FieldAccess {
                obj: Box::new(obj),
                field: format!("_{}", index),
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
                ty,
            } => match ty {
                tast::Ty::TBool => {
                    let cond_e = compile_imm(env, &expr);
                    if arms.len() == 2 {
                        let then_stmts = compile_aexpr_assign(env, target, arms[0].body.clone());
                        let else_stmts = compile_aexpr_assign(env, target, arms[1].body.clone());
                        vec![Stmt::If {
                            cond: cond_e,
                            then: Block { stmts: then_stmts },
                            else_: Some(Block { stmts: else_stmts }),
                        }]
                    } else if arms.len() == 1 && default.is_some() {
                        let then_stmts = compile_aexpr_assign(env, target, arms[0].body.clone());
                        let else_stmts = compile_aexpr_assign(env, target, *default.unwrap());
                        vec![Stmt::If {
                            cond: cond_e,
                            then: Block { stmts: then_stmts },
                            else_: Some(Block { stmts: else_stmts }),
                        }]
                    } else {
                        panic!("match to bool must have 2 arms or 1 arm + default");
                    }
                }
                _ => panic!("not imp yet"),
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
                ty: None,
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
                        value: compile_cexpr(env, &*value),
                    });
                }
            }

            // Continue with body to finally assign into the target
            out.extend(compile_aexpr_assign(env, target, *body));
            out
        }
    }
}

fn compile_aexpr_as_expr(env: &Env, e: Box<anf::AExpr>) -> Expr {
    match *e {
        AExpr::ACExpr { expr } => compile_cexpr(env, &expr),
        AExpr::ALet {
            name,
            value,
            body,
            ty: _,
        } => {
            let init_expr = compile_cexpr(env, &value);
            let var_ty = cexpr_ty(&value);
            let mut stmts = vec![Stmt::VarDecl {
                name,
                ty: Some(var_ty),
                value: Some(init_expr),
            }];

            let inner = compile_aexpr_as_expr(env, body);
            Expr::Block {
                stmts: {
                    if let Expr::Block {
                        stmts: inner_stmts,
                        expr,
                    } = inner
                    {
                        stmts.extend(inner_stmts);
                        if let Some(expr) = expr.map(|b| *b) {
                            return Expr::Block {
                                stmts,
                                expr: Some(Box::new(expr)),
                            };
                        } else {
                            return Expr::Block { stmts, expr: None };
                        }
                    }
                    stmts
                },
                expr: Some(Box::new(inner)),
            }
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
                ty,
            } => match ty {
                tast::Ty::TBool => {
                    let cond_e = compile_imm(env, &expr);
                    if arms.len() == 2 {
                        let then_block = Block {
                            stmts: compile_aexpr(env, arms[0].body.clone()),
                        };
                        let else_block = Block {
                            stmts: compile_aexpr(env, arms[1].body.clone()),
                        };
                        stmts.push(Stmt::If {
                            cond: cond_e,
                            then: then_block,
                            else_: Some(else_block),
                        });
                    } else if arms.len() == 1 && default.is_some() {
                        let then_block = Block {
                            stmts: compile_aexpr(env, arms[0].body.clone()),
                        };
                        let else_block = Block {
                            stmts: compile_aexpr(env, *default.unwrap()),
                        };
                        stmts.push(Stmt::If {
                            cond: cond_e,
                            then: then_block,
                            else_: Some(else_block),
                        });
                    } else {
                        panic!("match to bool must have 2 arms or 1 arm + default");
                    }
                }
                _ => panic!("not imp yet"),
            },
            _ => {
                stmts.push(Stmt::Return {
                    expr: Some(compile_cexpr(env, &expr)),
                });
            }
        },
        AExpr::ALet {
            name,
            value,
            body,
            ty: _,
        } => {
            match &*value {
                // If RHS needs statements, declare then fill via if-lowering
                anf::CExpr::EIf { .. } | anf::CExpr::EMatch { .. } => {
                    stmts.push(Stmt::VarDecl {
                        name: name.clone(),
                        ty: None,
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
                        ty: None,
                        value: Some(compile_cexpr(env, &*value)),
                    });
                }
                _ => {
                    stmts.push(Stmt::VarDecl {
                        name: name.clone(),
                        ty: Some(cexpr_ty(&*value)),
                        value: Some(compile_cexpr(env, &*value)),
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
        params.push((name, ty));
    }
    let stmts = compile_aexpr(env, f.body);
    Fn {
        name: f.name,
        params,
        body: Block { stmts: stmts },
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
            params: f.params,
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

pub fn go_file(env: &Env, file: anf::File) -> File {
    let file = anf_renamer::rename(file);

    let mut toplevels = gen_type_definition(env);
    for item in file.toplevels {
        let gof = compile_fn(env, item);
        toplevels.push(Item::Fn(gof));
    }
    // Collect tuple types used in the Go AST and emit lifted struct defs first
    let tuple_defs = collect_tuple_types(&toplevels);
    let mut all = Vec::new();
    for typs in tuple_defs {
        let name = go_type_name_for(&tast::Ty::TTuple { typs: typs.clone() });
        let fields: Vec<Field> = typs
            .iter()
            .enumerate()
            .map(|(i, t)| Field {
                name: format!("_{}", i),
                ty: t.clone(),
            })
            .collect();
        all.push(Item::Struct(Struct {
            name,
            fields,
            methods: vec![],
        }));
    }
    all.extend(toplevels);
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
                    ty: field.clone(),
                });
            }

            let methods = vec![Method {
                receiver: Receiver {
                    name: "_".to_string(),
                    ty: tast::Ty::TApp {
                        name: Uident::new(&variant_name),
                        args: vec![],
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

fn collect_tuple_types(items: &Vec<Item>) -> Vec<Vec<tast::Ty>> {
    use std::collections::HashSet;

    fn collect_from_ty(ty: &tast::Ty, out: &mut Vec<tast::Ty>) {
        if let tast::Ty::TTuple { typs } = ty {
            out.push(ty.clone());
            for t in typs {
                collect_from_ty(t, out);
            }
        }
    }

    fn collect_from_expr(e: &Expr, out: &mut Vec<tast::Ty>) {
        match e {
            Expr::Nil
            | Expr::Var { .. }
            | Expr::Bool { .. }
            | Expr::Int { .. }
            | Expr::String { .. } => {}
            Expr::Call { args, .. } => {
                for a in args {
                    collect_from_expr(a, out);
                }
            }
            Expr::FieldAccess { obj, .. } => collect_from_expr(obj, out),
            Expr::Cast { expr, ty } => {
                collect_from_expr(expr, out);
                collect_from_ty(ty, out);
            }
            Expr::StructLiteral { ty, fields } => {
                collect_from_ty(ty, out);
                for (_, fe) in fields {
                    collect_from_expr(fe, out);
                }
            }
            Expr::Block { stmts, expr } => {
                for s in stmts {
                    collect_from_stmt(s, out);
                }
                if let Some(e) = expr {
                    collect_from_expr(e, out);
                }
            }
        }
    }

    fn collect_from_stmt(s: &Stmt, out: &mut Vec<tast::Ty>) {
        match s {
            Stmt::Expr(e) => collect_from_expr(e, out),
            Stmt::VarDecl { ty, value, .. } => {
                if let Some(t) = ty {
                    collect_from_ty(t, out);
                }
                if let Some(v) = value {
                    collect_from_expr(v, out);
                }
            }
            Stmt::Assignment { value, .. } => collect_from_expr(value, out),
            Stmt::Return { expr } => {
                if let Some(e) = expr {
                    collect_from_expr(e, out);
                }
            }
            Stmt::If { cond, then, else_ } => {
                collect_from_expr(cond, out);
                for s in &then.stmts {
                    collect_from_stmt(s, out);
                }
                if let Some(b) = else_ {
                    for s in &b.stmts {
                        collect_from_stmt(s, out);
                    }
                }
            }
        }
    }

    let mut acc: Vec<tast::Ty> = Vec::new();
    for item in items {
        match item {
            Item::Interface(_) => {}
            Item::Struct(s) => {
                for f in &s.fields {
                    collect_from_ty(&f.ty, &mut acc);
                }
            }
            Item::Fn(f) => {
                for (_, t) in &f.params {
                    collect_from_ty(t, &mut acc);
                }
                for s in &f.body.stmts {
                    collect_from_stmt(s, &mut acc);
                }
            }
        }
    }

    // Dedup using HashSet, but return deterministically ordered list by Debug string
    let mut set: HashSet<tast::Ty> = HashSet::new();
    for t in acc {
        if let tast::Ty::TTuple { .. } = t {
            set.insert(t);
        }
    }
    let mut vec: Vec<tast::Ty> = set.into_iter().collect();
    vec.sort_by(|a, b| format!("{:?}", a).cmp(&format!("{:?}", b)));
    vec.into_iter()
        .map(|t| match t {
            tast::Ty::TTuple { typs } => typs,
            _ => unreachable!(),
        })
        .collect()
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

        func (_ Node) isTree() {}"#]]
    .assert_eq(&dummy_file.to_pretty(&env, 120));
}
