pub type Ty = crate::tast::Ty;
use crate::common::{Constructor, Prim};
use crate::intrinsics::CallableBody;
use crate::tast;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct File {
    pub toplevels: Vec<Fn>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Fn {
    pub name: String,
    #[serde(default = "root_default")]
    pub root: bool,
    pub generics: Vec<String>,
    #[serde(default)]
    pub trait_impl: Option<TraitImpl>,
    pub params: Vec<(String, Ty)>,
    pub ret_ty: Ty,
    pub body: Block,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TraitImpl {
    pub trait_ref: tast::TraitRef,
    pub for_ty: Ty,
}

fn root_default() -> bool {
    true
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct LetStmt {
    pub name: String,
    pub value: Expr,
    pub ty: Ty,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Block {
    pub stmts: Vec<LetStmt>,
    pub tail: Option<Box<Expr>>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum Expr {
    EVar {
        name: String,
        ty: Ty,
    },
    ECallable {
        name: String,
        body: CallableBody,
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
    EBlock {
        block: Box<Block>,
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
    EBreak {
        ty: Ty,
    },
    EContinue {
        ty: Ty,
    },
    EReturn {
        expr: Option<Box<Expr>>,
        ty: Ty,
    },
    EGo {
        expr: Box<Expr>,
        ty: Ty,
    },
    EConstrGet {
        expr: Box<Expr>,
        constructor: Constructor,
        field_index: usize,
        ty: Ty,
    },
    EUnary {
        op: common_defs::UnaryOp,
        expr: Box<Expr>,
        ty: Ty,
    },
    ECast {
        expr: Box<Expr>,
        ty: Ty,
    },
    EBinary {
        op: common_defs::BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        ty: Ty,
    },
    EAssign {
        name: String,
        value: Box<Expr>,
        target_ty: Ty,
        ty: Ty,
    },
    ECall {
        func: Box<Expr>,
        args: Vec<Expr>,
        ty: Ty,
    },
    EToDyn {
        trait_name: tast::TastIdent,
        for_ty: Ty,
        expr: Box<Expr>,
        ty: Ty,
    },
    EDynCall {
        trait_name: tast::TastIdent,
        method_name: tast::TastIdent,
        receiver: Box<Expr>,
        args: Vec<Expr>,
        ty: Ty,
    },
    ETraitCall {
        trait_ref: tast::TraitRef,
        method_name: tast::TastIdent,
        receiver: Box<Expr>,
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
            Expr::ECallable { ty, .. } => ty.clone(),
            Expr::EPrim { ty, .. } => ty.clone(),
            Expr::EConstr { ty, .. } => ty.clone(),
            Expr::ETuple { ty, .. } => ty.clone(),
            Expr::EArray { ty, .. } => ty.clone(),
            Expr::EClosure { ty, .. } => ty.clone(),
            Expr::EBlock { ty, .. } => ty.clone(),
            Expr::EMatch { ty, .. } => ty.clone(),
            Expr::EIf { ty, .. } => ty.clone(),
            Expr::EWhile { ty, .. } => ty.clone(),
            Expr::EBreak { ty, .. } => ty.clone(),
            Expr::EContinue { ty, .. } => ty.clone(),
            Expr::EReturn { ty, .. } => ty.clone(),
            Expr::EGo { ty, .. } => ty.clone(),
            Expr::EConstrGet { ty, .. } => ty.clone(),
            Expr::EUnary { ty, .. } => ty.clone(),
            Expr::ECast { ty, .. } => ty.clone(),
            Expr::EBinary { ty, .. } => ty.clone(),
            Expr::EAssign { ty, .. } => ty.clone(),
            Expr::ECall { ty, .. } => ty.clone(),
            Expr::EToDyn { ty, .. } => ty.clone(),
            Expr::EDynCall { ty, .. } => ty.clone(),
            Expr::ETraitCall { ty, .. } => ty.clone(),
            Expr::EProj { ty, .. } => ty.clone(),
        }
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
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

pub fn verify_let_types(file: &File) -> Vec<String> {
    let mut errors = Vec::new();
    for function in &file.toplevels {
        verify_block_let_types(&function.body, &mut errors);
    }
    errors
}

fn verify_block_let_types(block: &Block, errors: &mut Vec<String>) {
    for stmt in &block.stmts {
        let value_ty = stmt.value.get_ty();
        if stmt.ty != value_ty {
            errors.push(format!(
                "let {} has declared type {:?}, but its value has type {:?}",
                stmt.name, stmt.ty, value_ty
            ));
        }
        verify_expr_let_types(&stmt.value, errors);
    }
    if let Some(tail) = &block.tail {
        verify_expr_let_types(tail, errors);
    }
}

fn verify_expr_list_let_types(exprs: &[Expr], errors: &mut Vec<String>) {
    for expr in exprs {
        verify_expr_let_types(expr, errors);
    }
}

fn verify_expr_let_types(expr: &Expr, errors: &mut Vec<String>) {
    match expr {
        Expr::EVar { .. }
        | Expr::ECallable { .. }
        | Expr::EPrim { .. }
        | Expr::EBreak { .. }
        | Expr::EContinue { .. } => {}
        Expr::EConstr { args, .. }
        | Expr::ETuple { items: args, .. }
        | Expr::EArray { items: args, .. } => verify_expr_list_let_types(args, errors),
        Expr::EClosure { body, .. }
        | Expr::EGo { expr: body, .. }
        | Expr::EConstrGet { expr: body, .. }
        | Expr::EUnary { expr: body, .. }
        | Expr::ECast { expr: body, .. }
        | Expr::EToDyn { expr: body, .. }
        | Expr::EProj { tuple: body, .. } => verify_expr_let_types(body, errors),
        Expr::EBlock { block, .. } => verify_block_let_types(block, errors),
        Expr::EMatch {
            expr,
            arms,
            default,
            ..
        } => {
            verify_expr_let_types(expr, errors);
            for arm in arms {
                verify_expr_let_types(&arm.lhs, errors);
                verify_expr_let_types(&arm.body, errors);
            }
            if let Some(default) = default {
                verify_expr_let_types(default, errors);
            }
        }
        Expr::EIf {
            cond,
            then_branch,
            else_branch,
            ..
        } => {
            verify_expr_let_types(cond, errors);
            verify_expr_let_types(then_branch, errors);
            verify_expr_let_types(else_branch, errors);
        }
        Expr::EWhile { cond, body, .. }
        | Expr::EBinary {
            lhs: cond,
            rhs: body,
            ..
        } => {
            verify_expr_let_types(cond, errors);
            verify_expr_let_types(body, errors);
        }
        Expr::EReturn { expr, .. } => {
            if let Some(expr) = expr {
                verify_expr_let_types(expr, errors);
            }
        }
        Expr::EAssign { value, .. } => verify_expr_let_types(value, errors),
        Expr::ECall { func, args, .. } => {
            verify_expr_let_types(func, errors);
            verify_expr_list_let_types(args, errors);
        }
        Expr::EDynCall { receiver, args, .. } | Expr::ETraitCall { receiver, args, .. } => {
            verify_expr_let_types(receiver, errors);
            verify_expr_list_let_types(args, errors);
        }
    }
}
