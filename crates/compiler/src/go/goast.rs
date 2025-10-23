use crate::{go::goty, tast};

#[derive(Debug)]
pub struct File {
    pub toplevels: Vec<Item>,
}

#[derive(Debug)]
pub enum Item {
    Package(Package),
    Import(ImportDecl),
    Interface(Interface),
    Struct(Struct),
    Fn(Fn),
}

#[derive(Debug)]
pub struct Package {
    pub name: String,
}

#[derive(Debug)]
pub struct ImportDecl {
    pub specs: Vec<ImportSpec>,
}

#[derive(Debug)]
pub struct ImportSpec {
    pub alias: Option<String>,
    pub path: String,
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
        func: Box<Expr>,
        args: Vec<Expr>,
        ty: goty::GoType,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr>,
        ty: goty::GoType,
    },
    BinaryOp {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        ty: goty::GoType,
    },
    FieldAccess {
        obj: Box<Expr>,
        field: String,
        ty: goty::GoType,
    },
    Index {
        array: Box<Expr>,
        index: Box<Expr>,
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
    ArrayLiteral {
        elems: Vec<Expr>,
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
            | Expr::UnaryOp { ty, .. }
            | Expr::BinaryOp { ty, .. }
            | Expr::FieldAccess { ty, .. }
            | Expr::Index { ty, .. }
            | Expr::Cast { ty, .. }
            | Expr::StructLiteral { ty, .. }
            | Expr::ArrayLiteral { ty, .. }
            | Expr::Block { ty, .. } => ty,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Less,
    And,
    Or,
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
    IndexAssign {
        array: Expr,
        index: Expr,
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
        bind: Option<String>,
        expr: Expr,
        cases: Vec<(goty::GoType, Block)>,
        default: Option<Block>,
    },
}

pub fn tast_ty_to_go_type(ty: &tast::Ty) -> goty::GoType {
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
        tast::Ty::TCon { name } => goty::GoType::TName { name: name.clone() },
        tast::Ty::TApp { ty, args } => {
            if !args.is_empty() {
                unreachable!("generic types not supported in Go backend");
            }
            tast_ty_to_go_type(ty.as_ref())
        }
        tast::Ty::TArray { len, elem } => goty::GoType::TArray {
            len: *len,
            elem: Box::new(tast_ty_to_go_type(elem)),
        },
        tast::Ty::TRef { elem } => goty::GoType::TSlice {
            elem: Box::new(tast_ty_to_go_type(elem)),
        },
        tast::Ty::TParam { name } => goty::GoType::TName { name: name.clone() },
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

pub fn go_type_name_for(ty: &tast::Ty) -> String {
    match ty {
        tast::Ty::TUnit => "unit".to_string(),
        tast::Ty::TBool => "bool".to_string(),
        tast::Ty::TInt => "int".to_string(),
        tast::Ty::TString => "string".to_string(),
        tast::Ty::TCon { name } => name.clone(),
        tast::Ty::TApp { ty, .. } => go_type_name_for(ty.as_ref()),
        tast::Ty::TTuple { typs } => {
            let mut s = format!("Tuple{}", typs.len());
            for t in typs {
                s.push('_');
                s.push_str(&go_type_name_for(t).replace(['{', '}', ' ', '[', ']', ','], "_"));
            }
            s
        }
        tast::Ty::TArray { len, elem } => format!(
            "Array{}_{}",
            len,
            go_type_name_for(elem).replace(['{', '}', ' ', '[', ']', ','], "_")
        ),
        tast::Ty::TRef { elem } => format!(
            "Ref_{}",
            go_type_name_for(elem).replace(['{', '}', ' ', '[', ']', ','], "_")
        ),
        // Fallback textual
        tast::Ty::TVar(_) | tast::Ty::TParam { .. } | tast::Ty::TFunc { .. } => format!("{:?}", ty),
    }
}
