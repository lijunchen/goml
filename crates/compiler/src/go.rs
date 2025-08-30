use ast::ast::Uident;

use crate::{anf, env::Env, tast};

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
pub struct Method {
    pub receiver: String,
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
    Var {
        name: String,
    },
    Literal {
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

pub fn go_file(env: &Env, file: anf::File) -> File {
    todo!()
}

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
                receiver: variant_name.clone(),
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
    use expect_test::{Expect, expect};

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

        type Empty struct { }

        func (Empty) isTree() {}

        type Leaf struct {
        _0 Int
        }

        func (Leaf) isTree() {}

        type Node struct {
        _0 Tree
            _1 Tree
        }

        func (Node) isTree() {}"#]]
    .assert_eq(&dummy_file.to_pretty(&env, 120));
}
