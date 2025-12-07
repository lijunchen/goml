pub type Ty = crate::tast::Ty;
use crate::common::{Constructor, Prim};
use crate::tast;

pub use ast::ast::{BinaryOp, UnaryOp};

#[derive(Debug, Clone)]
pub struct File {
    pub toplevels: Vec<Fn>,
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub name: String,
    pub params: Vec<(String, Ty)>,
    pub ret_ty: Ty,
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub enum Expr {
    EVar {
        name: String,
        ty: Ty,
    },
    EPrim {
        value: Prim,
        ty: Ty,
    },
    EConstr {
        constructor: Constructor,
        args: Vec<Expr>,
        ty: Ty,
    },
    ETuple {
        items: Vec<Expr>,
        ty: Ty,
    },
    EArray {
        items: Vec<Expr>,
        ty: Ty,
    },
    EClosure {
        params: Vec<tast::ClosureParam>,
        body: Box<Expr>,
        ty: Ty,
    },
    ELet {
        name: String,
        value: Box<Expr>,
        body: Box<Expr>,
        ty: Ty,
    },
    EMatch {
        expr: Box<Expr>,
        arms: Vec<Arm>,
        default: Option<Box<Expr>>,
        ty: Ty,
    },
    EIf {
        cond: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
        ty: Ty,
    },
    EWhile {
        cond: Box<Expr>,
        body: Box<Expr>,
        ty: Ty,
    },
    EConstrGet {
        expr: Box<Expr>,
        constructor: Constructor,
        field_index: usize,
        ty: Ty,
    },
    EUnary {
        op: UnaryOp,
        expr: Box<Expr>,
        ty: Ty,
    },
    EBinary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        ty: Ty,
    },
    ECall {
        func: Box<Expr>,
        args: Vec<Expr>,
        ty: Ty,
    },
    EProj {
        tuple: Box<Expr>,
        index: usize,
        ty: Ty,
    },
}

impl Expr {
    pub fn get_ty(&self) -> Ty {
        match self {
            Expr::EVar { ty, .. } => ty.clone(),
            Expr::EPrim { ty, .. } => ty.clone(),
            Expr::EConstr { ty, .. } => ty.clone(),
            Expr::ETuple { ty, .. } => ty.clone(),
            Expr::EArray { ty, .. } => ty.clone(),
            Expr::EClosure { ty, .. } => ty.clone(),
            Expr::ELet { ty, .. } => ty.clone(),
            Expr::EMatch { ty, .. } => ty.clone(),
            Expr::EIf { ty, .. } => ty.clone(),
            Expr::EWhile { ty, .. } => ty.clone(),
            Expr::EConstrGet { ty, .. } => ty.clone(),
            Expr::EUnary { ty, .. } => ty.clone(),
            Expr::EBinary { ty, .. } => ty.clone(),
            Expr::ECall { ty, .. } => ty.clone(),
            Expr::EProj { ty, .. } => ty.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Arm {
    pub lhs: Expr,
    pub body: Expr,
}

pub fn eunit() -> Expr {
    Expr::EPrim {
        value: Prim::unit(),
        ty: Ty::TUnit,
    }
}

pub fn ebool(value: bool) -> Expr {
    Expr::EPrim {
        value: Prim::boolean(value),
        ty: Ty::TBool,
    }
}
