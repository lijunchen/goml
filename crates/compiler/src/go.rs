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
        ty: String,
        fields: Vec<(String, Expr)>,
    },
    If {
        cond: Box<Expr>,
        then: Box<Expr>,
        else_: Box<Expr>,
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
        anf::ImmExpr::ImmTag { index, ty } => {
            let variant_name = lookup_variant_name(env, ty, *index);
            Expr::StructLiteral {
                ty: variant_name,
                fields: vec![],
            }
        }
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
            let variant_name = lookup_variant_name(env, ty, *index);
            let fields = args
                .iter()
                .enumerate()
                .map(|(i, a)| (format!("_{}", i), compile_imm(env, a)))
                .collect();
            Expr::StructLiteral {
                ty: variant_name,
                fields,
            }
        }
        anf::CExpr::ETuple { items: _, ty: _ } => todo!(),
        anf::CExpr::EMatch {
            expr,
            arms,
            default,
            ty,
        } => match ty {
            tast::Ty::TBool => {
                // compile to if else statement
                let cond = compile_imm(env, expr);
                if arms.len() == 2 {
                    let then = compile_aexpr_as_expr(env, Box::new(arms[0].body.clone()));
                    let else_ = compile_aexpr_as_expr(env, Box::new(arms[1].body.clone()));
                    Expr::If {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        else_: Box::new(else_),
                    }
                } else if arms.len() == 1 && default.is_some() {
                    let then = compile_aexpr_as_expr(env, Box::new(arms[0].body.clone()));
                    let else_ = compile_aexpr_as_expr(env, default.as_ref().unwrap().clone());
                    Expr::If {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        else_: Box::new(else_),
                    }
                } else {
                    panic!("match to bool must have 2 arms or 1 arm + default");
                }
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
        } => {
            let cond_e = compile_imm(env, cond);
            let then_e = compile_aexpr_as_expr(env, then.clone());
            let else_e = compile_aexpr_as_expr(env, else_.clone());
            Expr::If {
                cond: Box::new(cond_e),
                then: Box::new(then_e),
                else_: Box::new(else_e),
            }
        }
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
            tuple: _,
            index: _,
            ty: _,
        } => todo!(),
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
        AExpr::ACExpr { expr } => {
            stmts.push(Stmt::Expr(compile_cexpr(env, &expr)));
        }
        AExpr::ALet {
            name,
            value,
            body,
            ty,
        } => {
            stmts.push(Stmt::VarDecl {
                name: name.clone(),
                ty: Some(ty.clone()),
                value: Some(compile_cexpr(env, &value)),
            });
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

pub fn go_file(env: &Env, file: anf::File) -> File {
    let mut toplevels = gen_type_definition(env);
    for item in file.toplevels {
        let gof = compile_fn(env, item);
        toplevels.push(Item::Fn(gof));
    }
    File {
        toplevels: toplevels,
    }
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
            _0 Int
        }

        func (_ Leaf) isTree() {}

        type Node struct {
            _0 Tree
            _1 Tree
        }

        func (_ Node) isTree() {}"#]]
    .assert_eq(&dummy_file.to_pretty(&env, 120));
}
