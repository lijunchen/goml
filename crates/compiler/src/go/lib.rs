use ast::ast::Uident;
use indexmap::IndexMap;

use crate::{
    anf::{self, AExpr},
    env::Env,
    go::{
        goast::{self, go_type_name_for, tast_ty_to_go_type},
        goty::GoType,
    },
    tast,
};

use super::goty;
use super::runtime;

fn compile_imm(env: &Env, imm: &anf::ImmExpr) -> goast::Expr {
    match imm {
        anf::ImmExpr::ImmVar { name, ty: _ } => goast::Expr::Var {
            name: name.clone(),
            ty: tast_ty_to_go_type(&imm_ty(imm)),
        },
        anf::ImmExpr::ImmUnit { ty: _ } => goast::Expr::Unit {
            ty: goty::GoType::TUnit,
        },
        anf::ImmExpr::ImmBool { value, ty: _ } => goast::Expr::Bool {
            value: *value,
            ty: goty::GoType::TBool,
        },
        anf::ImmExpr::ImmInt { value, ty: _ } => goast::Expr::Int {
            value: *value,
            ty: goty::GoType::TInt,
        },
        anf::ImmExpr::ImmString { value, ty: _ } => goast::Expr::String {
            value: value.clone(),
            ty: goty::GoType::TString,
        },
        anf::ImmExpr::ImmTag { index, ty } => goast::Expr::StructLiteral {
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

fn compile_cexpr(env: &Env, e: &anf::CExpr) -> goast::Expr {
    match e {
        anf::CExpr::CImm { imm } => compile_imm(env, imm),
        anf::CExpr::EConstr { index, args, ty } => {
            let variant_ty = variant_ty_by_index(env, ty, *index);
            let fields = args
                .iter()
                .enumerate()
                .map(|(i, a)| (format!("_{}", i), compile_imm(env, a)))
                .collect();
            goast::Expr::StructLiteral {
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
            goast::Expr::StructLiteral {
                ty: tuple_to_go_struct_type(env, ty),
                fields,
            }
        }
        anf::CExpr::EMatch { expr, .. } => match imm_ty(expr) {
            // Boolean matches are handled as statements (not expressions) in Go.
            tast::Ty::TBool => {
                panic!("boolean match should be lowered to  goast::Stmt::If in compile_aexpr")
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
        } => panic!("EIf should be lowered to  goast::Stmt::If in compile_aexpr"),
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
            goast::Expr::FieldAccess {
                obj: Box::new(goast::Expr::Cast {
                    expr: Box::new(obj),
                    ty: vty,
                }),
                field: format!("_{}", field_index),
                ty: tast_ty_to_go_type(&enum_ty),
            }
        }
        anf::CExpr::ECall { func, args, ty } => {
            let args = args.iter().map(|arg| compile_imm(env, arg)).collect();
            goast::Expr::Call {
                func: func.clone(),
                args,
                ty: tast_ty_to_go_type(ty),
            }
        }
        anf::CExpr::EProj { tuple, index, ty } => {
            let obj = compile_imm(env, tuple);
            goast::Expr::FieldAccess {
                obj: Box::new(obj),
                field: format!("_{}", index),
                ty: tast_ty_to_go_type(ty),
            }
        }
    }
}

fn compile_aexpr_assign(env: &Env, target: &str, e: anf::AExpr) -> Vec<goast::Stmt> {
    match e {
        AExpr::ACExpr { expr } => match expr {
            anf::CExpr::CImm { .. }
            | anf::CExpr::EConstr { .. }
            | anf::CExpr::EConstrGet { .. }
            | anf::CExpr::ECall { .. }
            | anf::CExpr::EProj { .. }
            | anf::CExpr::ETuple { .. } => vec![goast::Stmt::Assignment {
                name: target.to_string(),
                value: compile_cexpr(env, &expr),
            }],
            anf::CExpr::EIf {
                cond, then, else_, ..
            } => {
                let cond_e = compile_imm(env, &cond);
                let then_stmts = compile_aexpr_assign(env, target, *then);
                let else_stmts = compile_aexpr_assign(env, target, *else_);
                vec![goast::Stmt::If {
                    cond: cond_e,
                    then: goast::Block { stmts: then_stmts },
                    else_: Some(goast::Block { stmts: else_stmts }),
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
                                goast::Expr::Bool {
                                    value,
                                    ty: goty::GoType::TBool,
                                },
                                goast::Block {
                                    stmts: compile_aexpr_assign(env, target, arm.body),
                                },
                            ));
                        } else {
                            panic!("expected ImmBool in boolean match arm");
                        }
                    }
                    let def_block = default.map(|d| goast::Block {
                        stmts: compile_aexpr_assign(env, target, *d),
                    });
                    vec![goast::Stmt::SwitchExpr {
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
                                goast::Block {
                                    stmts: compile_aexpr_assign(env, target, arm.body),
                                },
                            ));
                        } else {
                            panic!("expected ImmTag in enum match arm");
                        }
                    }
                    let def_block = default.map(|d| goast::Block {
                        stmts: compile_aexpr_assign(env, target, *d),
                    });
                    vec![goast::Stmt::SwitchType {
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
            out.push(goast::Stmt::VarDecl {
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
                    out.push(goast::Stmt::Assignment {
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

fn compile_aexpr(env: &Env, e: anf::AExpr) -> Vec<goast::Stmt> {
    let mut stmts = Vec::new();
    match e {
        AExpr::ACExpr { expr } => match expr {
            // Lower conditional expressions to if-statements with returns in branches
            anf::CExpr::EIf {
                cond, then, else_, ..
            } => {
                let cond_e = compile_imm(env, &cond);
                let then_block = goast::Block {
                    stmts: compile_aexpr(env, *then),
                };
                let else_block = goast::Block {
                    stmts: compile_aexpr(env, *else_),
                };
                stmts.push(goast::Stmt::If {
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
                                goast::Expr::Bool {
                                    value,
                                    ty: goty::GoType::TBool,
                                },
                                goast::Block {
                                    stmts: compile_aexpr(env, arm.body),
                                },
                            ));
                        } else {
                            panic!("expected ImmBool in boolean match arm");
                        }
                    }
                    let def_block = default.map(|d| goast::Block {
                        stmts: compile_aexpr(env, *d),
                    });
                    stmts.push(goast::Stmt::SwitchExpr {
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
                                goast::Block {
                                    stmts: compile_aexpr(env, arm.body),
                                },
                            ));
                        } else {
                            panic!("expected ImmTag in enum match arm");
                        }
                    }
                    let def_block = default.map(|d| goast::Block {
                        stmts: compile_aexpr(env, *d),
                    });
                    stmts.push(goast::Stmt::SwitchType {
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
                        stmts.push(goast::Stmt::Return { expr: Some(e) });
                    }
                }
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
                    stmts.push(goast::Stmt::VarDecl {
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
                    stmts.push(goast::Stmt::VarDecl {
                        name: name.clone(),
                        ty: cexpr_ty(&value),
                        value: Some(compile_cexpr(env, &value)),
                    });
                }
                _ => {
                    stmts.push(goast::Stmt::VarDecl {
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

fn compile_fn(env: &Env, f: anf::Fn) -> goast::Fn {
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
    goast::Fn {
        name: patched_name,
        params,
        ret_ty: Some(tast_ty_to_go_type(&f.ret_ty)),
        body: goast::Block { stmts },
    }
}

fn collect_tuple_types(file: &anf::File) -> IndexMap<String, goty::GoType> {
    let mut env = IndexMap::new();
    for item in file.toplevels.iter() {
        go_item(&mut env, item);
    }
    return env;

    ///////////////////////////////////////////////////////////////////////////

    fn go_item(env: &mut IndexMap<String, GoType>, item: &anf::Fn) {
        go_aexpr(env, &item.body);
    }

    fn go_aexpr(env: &mut IndexMap<String, GoType>, aexpr: &anf::AExpr) {
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

    fn go_cexpr(env: &mut IndexMap<String, GoType>, cexpr: &anf::CExpr) {
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

    fn go_immexpr(env: &mut IndexMap<String, GoType>, imm: &anf::ImmExpr) {
        match imm {
            anf::ImmExpr::ImmVar { name: _, ty } => go_type(env, ty),
            anf::ImmExpr::ImmUnit { ty } => go_type(env, ty),
            anf::ImmExpr::ImmBool { value: _, ty } => go_type(env, ty),
            anf::ImmExpr::ImmInt { value: _, ty } => go_type(env, ty),
            anf::ImmExpr::ImmString { value: _, ty } => go_type(env, ty),
            anf::ImmExpr::ImmTag { index: _, ty } => go_type(env, ty),
        }
    }

    fn go_type(env: &mut IndexMap<String, GoType>, ty: &tast::Ty) {
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

pub fn go_file(env: &Env, file: anf::File) -> goast::File {
    let mut all = Vec::new();

    all.extend(runtime::make_runtime());

    let tuple_types = collect_tuple_types(&file);

    for tt in tuple_types {
        all.push(goast::Item::Struct(goast::Struct {
            name: tt.0,
            fields: match tt.1 {
                goty::GoType::TStruct { name: _, fields } => {
                    let fields = fields.iter().map(|(n, t)| goast::Field {
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

    let file = anf::anf_renamer::rename(file);

    let mut toplevels = gen_type_definition(env);
    for item in file.toplevels {
        let gof = compile_fn(env, item);
        toplevels.push(goast::Item::Fn(gof));
    }
    all.extend(toplevels);
    all.push(goast::Item::Fn(goast::Fn {
        name: "main".to_string(),
        params: vec![],
        ret_ty: None,
        body: goast::Block {
            stmts: vec![goast::Stmt::Expr(goast::Expr::Call {
                func: "main0".to_string(),
                args: vec![],
                ty: goty::GoType::TVoid,
            })],
        },
    }));
    goast::File { toplevels: all }
}

#[allow(unused)]
fn gen_type_definition(env: &Env) -> Vec<goast::Item> {
    let mut defs = Vec::new();
    for (name, def) in env.enums.iter() {
        let type_identifier_method = format!("is{}", name.0);

        defs.push(goast::Item::Interface(goast::Interface {
            name: name.0.clone(),
            methods: vec![goast::MethodElem {
                name: type_identifier_method.clone(),
                params: vec![],
                ret: None,
            }],
        }));
        for (variant_name, variant_fields) in def.variants.iter() {
            let variant_name = variant_name.0.clone();
            let mut fields = Vec::new();
            for (i, field) in variant_fields.iter().enumerate() {
                fields.push(goast::Field {
                    name: format!("_{}", i),
                    ty: tast_ty_to_go_type(field),
                });
            }

            let methods = vec![goast::Method {
                receiver: goast::Receiver {
                    name: "_".to_string(),
                    ty: goty::GoType::TName {
                        name: variant_name.clone(),
                    },
                },
                name: type_identifier_method.clone(),
                params: vec![],
                body: goast::Block { stmts: vec![] },
            }];

            defs.push(goast::Item::Struct(goast::Struct {
                name: variant_name,
                fields,
                methods,
            }));
        }
    }
    defs
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
    let dummy_file = goast::File { toplevels: item };
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
