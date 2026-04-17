pub type Ty = crate::tast::Ty;

use std::cell::RefCell;

use crate::common::{Constructor, Prim};
use crate::env::Gensym;
use crate::env::{EnumDef, StructDef};
use crate::lift::{GlobalLiftEnv, LiftArm, LiftExpr, LiftFile};
use crate::tast::TastIdent;
use common_defs::{BinaryOp, UnaryOp};

#[derive(Debug, Clone)]
struct LoopCtx {
    loop_id: JoinId,
    exit_id: JoinId,
    exit_ret_ty: Ty,
}

#[derive(Debug, Clone)]
struct ReturnCtx {
    join_id: JoinId,
    ret_ty: Ty,
    takes_arg: bool,
}

#[derive(Debug, Clone)]
pub struct GlobalAnfEnv {
    pub liftenv: GlobalLiftEnv,
    loop_ctx: RefCell<Option<LoopCtx>>,
    return_ctx: RefCell<Option<ReturnCtx>>,
}

impl GlobalAnfEnv {
    pub fn from_lift_env(liftenv: GlobalLiftEnv) -> Self {
        GlobalAnfEnv {
            liftenv,
            loop_ctx: RefCell::new(None),
            return_ctx: RefCell::new(None),
        }
    }

    pub fn enums(&self) -> impl Iterator<Item = (&TastIdent, &EnumDef)> {
        self.liftenv.enums()
    }

    pub fn structs(&self) -> impl Iterator<Item = (&TastIdent, &StructDef)> {
        self.liftenv.structs()
    }

    pub fn get_struct(&self, name: &TastIdent) -> Option<&StructDef> {
        self.liftenv.get_struct(name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalId(pub String);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct JoinId(pub String);

#[derive(Debug, Clone)]
pub struct File {
    pub toplevels: Vec<Fn>,
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub name: String,
    pub params: Vec<(LocalId, Ty)>,
    pub ret_ty: Ty,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub binds: Vec<Bind>,
    pub term: Term,
}

#[derive(Debug, Clone)]
pub enum ImmExpr {
    Var { id: LocalId, ty: Ty },
    Prim { value: Prim, ty: Ty },
    Tag { index: usize, ty: Ty },
}

#[derive(Debug, Clone)]
pub struct LetBind {
    pub id: LocalId,
    pub value: ValueExpr,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct JoinBind {
    pub id: JoinId,
    pub params: Vec<(LocalId, Ty)>,
    pub ret_ty: Ty,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub enum Bind {
    Let(LetBind),
    Join(JoinBind),
    JoinRec(Vec<JoinBind>),
}

#[derive(Debug, Clone)]
pub enum ValueExpr {
    Imm(ImmExpr),

    Constr {
        constructor: Constructor,
        args: Vec<ImmExpr>,
        ty: Ty,
    },
    Tuple {
        items: Vec<ImmExpr>,
        ty: Ty,
    },
    Array {
        items: Vec<ImmExpr>,
        ty: Ty,
    },
    ConstrGet {
        expr: ImmExpr,
        constructor: Constructor,
        field_index: usize,
        ty: Ty,
    },
    Unary {
        op: UnaryOp,
        expr: ImmExpr,
        ty: Ty,
    },
    Binary {
        op: BinaryOp,
        lhs: ImmExpr,
        rhs: ImmExpr,
        ty: Ty,
    },
    Assign {
        name: LocalId,
        value: ImmExpr,
        target_ty: Ty,
        ty: Ty,
    },
    Call {
        func: ImmExpr,
        args: Vec<ImmExpr>,
        ty: Ty,
    },
    ToDyn {
        trait_name: TastIdent,
        for_ty: Ty,
        expr: ImmExpr,
        ty: Ty,
    },
    DynCall {
        trait_name: TastIdent,
        method_name: TastIdent,
        receiver: ImmExpr,
        args: Vec<ImmExpr>,
        ty: Ty,
    },
    Go {
        closure: ImmExpr,
        ty: Ty,
    },
    Proj {
        tuple: ImmExpr,
        index: usize,
        ty: Ty,
    },
}

impl Block {
    pub fn get_ty(&self) -> Ty {
        term_tast_ty(&self.term)
    }
}

#[derive(Debug, Clone)]
pub struct Arm {
    pub lhs: ImmExpr,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub enum Term {
    Return(ImmExpr),
    Jump {
        target: JoinId,
        args: Vec<ImmExpr>,
        ret_ty: Ty,
    },
    If {
        cond: ImmExpr,
        then_: Box<Block>,
        else_: Box<Block>,
        ret_ty: Ty,
    },
    Match {
        scrut: ImmExpr,
        arms: Vec<Arm>,
        default: Option<Box<Block>>,
        ret_ty: Ty,
    },
    Unreachable {
        ty: Ty,
    },
}

fn imm_ty(imm: &ImmExpr) -> Ty {
    match imm {
        ImmExpr::Var { ty, .. } | ImmExpr::Prim { ty, .. } | ImmExpr::Tag { ty, .. } => ty.clone(),
    }
}

fn value_expr_tast_ty(e: &ValueExpr) -> Ty {
    match e {
        ValueExpr::Imm(imm) => imm_ty(imm),
        ValueExpr::Constr { ty, .. }
        | ValueExpr::Tuple { ty, .. }
        | ValueExpr::Array { ty, .. }
        | ValueExpr::ConstrGet { ty, .. }
        | ValueExpr::Unary { ty, .. }
        | ValueExpr::Binary { ty, .. }
        | ValueExpr::Assign { ty, .. }
        | ValueExpr::Call { ty, .. }
        | ValueExpr::ToDyn { ty, .. }
        | ValueExpr::DynCall { ty, .. }
        | ValueExpr::Go { ty, .. }
        | ValueExpr::Proj { ty, .. } => ty.clone(),
    }
}

fn term_tast_ty(t: &Term) -> Ty {
    match t {
        Term::Return(imm) => imm_ty(imm),
        Term::Jump { ret_ty, .. } | Term::If { ret_ty, .. } | Term::Match { ret_ty, .. } => {
            ret_ty.clone()
        }
        Term::Unreachable { ty } => ty.clone(),
    }
}

fn unit_imm() -> ImmExpr {
    ImmExpr::Prim {
        value: Prim::Unit { value: () },
        ty: Ty::TUnit,
    }
}

fn local(name: String) -> LocalId {
    LocalId(name)
}

fn join(name: String) -> JoinId {
    JoinId(name)
}

fn lift_imm_to_imm(lift_imm: LiftExpr) -> ImmExpr {
    match lift_imm {
        LiftExpr::EVar { name, ty } => ImmExpr::Var {
            id: local(name),
            ty,
        },
        LiftExpr::EPrim { value, ty } => ImmExpr::Prim { value, ty },
        LiftExpr::EConstr {
            constructor: Constructor::Enum(enum_constructor),
            args,
            ty,
        } if args.is_empty() => ImmExpr::Tag {
            index: enum_constructor.enum_index(),
            ty,
        },
        other => panic!("expected immediate expression, found {:?}", other),
    }
}

fn reify_k<'a>(
    gensym: &'a Gensym,
    value_ty: Ty,
    k: Box<dyn FnOnce(ImmExpr) -> Block + 'a>,
) -> (JoinBind, Ty) {
    let id = join(gensym.gensym("k"));
    if value_ty == Ty::TUnit {
        let body = k(unit_imm());
        let ret_ty = body.get_ty();
        return (
            JoinBind {
                id,
                params: Vec::new(),
                ret_ty: ret_ty.clone(),
                body,
            },
            ret_ty,
        );
    }
    let param = local(gensym.gensym("jp"));
    let body = k(ImmExpr::Var {
        id: param.clone(),
        ty: value_ty.clone(),
    });
    let ret_ty = body.get_ty();
    (
        JoinBind {
            id,
            params: vec![(param, value_ty)],
            ret_ty: ret_ty.clone(),
            body,
        },
        ret_ty,
    )
}

fn lift_expr_always_exits_control_flow(expr: &LiftExpr) -> bool {
    match expr {
        LiftExpr::EBreak { .. } | LiftExpr::EContinue { .. } | LiftExpr::EReturn { .. } => true,
        LiftExpr::EConstr { args, .. } => args.iter().any(lift_expr_always_exits_control_flow),
        LiftExpr::ETuple { items, .. } | LiftExpr::EArray { items, .. } => {
            items.iter().any(lift_expr_always_exits_control_flow)
        }
        LiftExpr::ELet { value, body, .. } => {
            lift_expr_always_exits_control_flow(value) || lift_expr_always_exits_control_flow(body)
        }
        LiftExpr::EIf {
            cond: _,
            then_branch,
            else_branch,
            ..
        } => {
            lift_expr_always_exits_control_flow(then_branch)
                && lift_expr_always_exits_control_flow(else_branch)
        }
        LiftExpr::EMatch { arms, default, .. } => {
            let has_catch_all = default.is_some()
                || arms
                    .iter()
                    .any(|arm| matches!(arm.lhs, LiftExpr::EVar { .. }));
            has_catch_all
                && arms
                    .iter()
                    .all(|arm| lift_expr_always_exits_control_flow(&arm.body))
                && default
                    .as_deref()
                    .is_none_or(lift_expr_always_exits_control_flow)
        }
        LiftExpr::EWhile { cond, .. } => lift_expr_always_exits_control_flow(cond),
        LiftExpr::EGo { expr, .. }
        | LiftExpr::EConstrGet { expr, .. }
        | LiftExpr::EUnary { expr, .. }
        | LiftExpr::EToDyn { expr, .. } => lift_expr_always_exits_control_flow(expr),
        LiftExpr::EBinary { op, lhs, rhs, .. } => {
            lift_expr_always_exits_control_flow(lhs)
                || match op {
                    common_defs::BinaryOp::And | common_defs::BinaryOp::Or => false,
                    _ => lift_expr_always_exits_control_flow(rhs),
                }
        }
        LiftExpr::EAssign { value, .. } => lift_expr_always_exits_control_flow(value),
        LiftExpr::ECall { func, args, .. } => {
            lift_expr_always_exits_control_flow(func)
                || args.iter().any(lift_expr_always_exits_control_flow)
        }
        LiftExpr::EDynCall { receiver, args, .. } => {
            lift_expr_always_exits_control_flow(receiver)
                || args.iter().any(lift_expr_always_exits_control_flow)
        }
        LiftExpr::EProj { tuple, .. } => lift_expr_always_exits_control_flow(tuple),
        LiftExpr::EVar { .. } | LiftExpr::EPrim { .. } => false,
    }
}

fn reify_k_for_expr<'a>(
    gensym: &'a Gensym,
    expr: &LiftExpr,
    k: Box<dyn FnOnce(ImmExpr) -> Block + 'a>,
) -> (JoinBind, Ty) {
    if expr.get_ty() == Ty::TUnit && lift_expr_always_exits_control_flow(expr) {
        let id = join(gensym.gensym("k"));
        return (
            JoinBind {
                id,
                params: Vec::new(),
                ret_ty: Ty::TUnit,
                body: Block {
                    binds: Vec::new(),
                    term: Term::Unreachable { ty: Ty::TUnit },
                },
            },
            Ty::TUnit,
        );
    }
    reify_k(gensym, expr.get_ty(), k)
}

fn lower_arms<'a>(
    anfenv: &'a GlobalAnfEnv,
    gensym: &'a Gensym,
    join_id: JoinId,
    join_ret_ty: Ty,
    join_takes_arg: bool,
    arms: Vec<LiftArm>,
) -> Vec<Arm> {
    arms.into_iter()
        .map(|arm| {
            let lhs = match arm.lhs {
                LiftExpr::EVar { name, ty } => ImmExpr::Var {
                    id: local(name),
                    ty,
                },
                LiftExpr::EPrim { value, ty } => ImmExpr::Prim { value, ty },
                LiftExpr::EConstr {
                    constructor: Constructor::Enum(enum_constructor),
                    args: _,
                    ty,
                } => ImmExpr::Tag {
                    index: enum_constructor.enum_index(),
                    ty,
                },
                other => panic!("unexpected match arm lhs {:?}", other),
            };

            let body = lower(
                anfenv,
                gensym,
                arm.body,
                Box::new({
                    let join_id = join_id.clone();
                    let join_ret_ty = join_ret_ty.clone();
                    move |imm| Block {
                        binds: Vec::new(),
                        term: Term::Jump {
                            target: join_id,
                            args: if join_takes_arg {
                                vec![imm]
                            } else {
                                Vec::new()
                            },
                            ret_ty: join_ret_ty,
                        },
                    }
                }),
            );

            Arm { lhs, body }
        })
        .collect()
}

fn lower<'a>(
    anfenv: &'a GlobalAnfEnv,
    gensym: &'a Gensym,
    e: LiftExpr,
    k: Box<dyn FnOnce(ImmExpr) -> Block + 'a>,
) -> Block {
    let e_ty = e.get_ty();
    match e {
        LiftExpr::EVar { name, ty } => k(ImmExpr::Var {
            id: local(name),
            ty,
        }),
        LiftExpr::EPrim { value, ty } => k(ImmExpr::Prim { value, ty }),
        LiftExpr::EConstr {
            constructor: Constructor::Enum(enum_constructor),
            args,
            ty: _,
        } if args.is_empty() => k(ImmExpr::Tag {
            index: enum_constructor.enum_index(),
            ty: e_ty,
        }),
        LiftExpr::ELet {
            name,
            value,
            body,
            ty: _,
        } => {
            let body = *body;
            let value = *value;
            let id = local(name);

            match value {
                LiftExpr::EIf { .. }
                | LiftExpr::EMatch { .. }
                | LiftExpr::EWhile { .. }
                | LiftExpr::ELet { .. }
                | LiftExpr::EBreak { .. }
                | LiftExpr::EContinue { .. }
                | LiftExpr::EReturn { .. } => {
                    let value_ty = value.get_ty();
                    lower(
                        anfenv,
                        gensym,
                        value,
                        Box::new(move |imm| {
                            let block = lower(anfenv, gensym, body, k);
                            let mut binds = vec![Bind::Let(LetBind {
                                id,
                                value: ValueExpr::Imm(imm),
                                ty: value_ty,
                            })];
                            binds.extend(block.binds);
                            Block {
                                binds,
                                term: block.term,
                            }
                        }),
                    )
                }
                other => lower_value(
                    anfenv,
                    gensym,
                    other,
                    Box::new(move |value_expr| {
                        let value_ty = value_expr_tast_ty(&value_expr);
                        let block = lower(anfenv, gensym, body, k);
                        let mut binds = vec![Bind::Let(LetBind {
                            id,
                            value: value_expr,
                            ty: value_ty,
                        })];
                        binds.extend(block.binds);
                        Block {
                            binds,
                            term: block.term,
                        }
                    }),
                ),
            }
        }
        LiftExpr::EIf {
            cond,
            then_branch,
            else_branch,
            ty: _,
        } => {
            let then_branch = *then_branch;
            let else_branch = *else_branch;
            let expr = LiftExpr::EIf {
                cond: cond.clone(),
                then_branch: Box::new(then_branch.clone()),
                else_branch: Box::new(else_branch.clone()),
                ty: e_ty.clone(),
            };
            let (join_bind, join_ret_ty) = reify_k_for_expr(gensym, &expr, k);
            let join_id = join_bind.id.clone();
            let join_takes_arg = !join_bind.params.is_empty();

            lower(
                anfenv,
                gensym,
                *cond,
                Box::new(move |cond_imm| {
                    let then_block = lower(
                        anfenv,
                        gensym,
                        then_branch,
                        Box::new({
                            let join_id = join_id.clone();
                            let join_ret_ty = join_ret_ty.clone();
                            move |imm| Block {
                                binds: Vec::new(),
                                term: Term::Jump {
                                    target: join_id,
                                    args: if join_takes_arg {
                                        vec![imm]
                                    } else {
                                        Vec::new()
                                    },
                                    ret_ty: join_ret_ty,
                                },
                            }
                        }),
                    );

                    let else_block = lower(
                        anfenv,
                        gensym,
                        else_branch,
                        Box::new({
                            let join_id = join_id.clone();
                            let join_ret_ty = join_ret_ty.clone();
                            move |imm| Block {
                                binds: Vec::new(),
                                term: Term::Jump {
                                    target: join_id,
                                    args: if join_takes_arg {
                                        vec![imm]
                                    } else {
                                        Vec::new()
                                    },
                                    ret_ty: join_ret_ty,
                                },
                            }
                        }),
                    );

                    Block {
                        binds: vec![Bind::Join(join_bind)],
                        term: Term::If {
                            cond: cond_imm,
                            then_: Box::new(then_block),
                            else_: Box::new(else_block),
                            ret_ty: join_ret_ty,
                        },
                    }
                }),
            )
        }
        LiftExpr::EMatch {
            expr,
            arms,
            default,
            ty: _,
        } => {
            let match_expr = LiftExpr::EMatch {
                expr: expr.clone(),
                arms: arms.clone(),
                default: default.clone(),
                ty: e_ty.clone(),
            };
            let (join_bind, join_ret_ty) = reify_k_for_expr(gensym, &match_expr, k);
            let join_id = join_bind.id.clone();
            let join_takes_arg = !join_bind.params.is_empty();

            lower(
                anfenv,
                gensym,
                *expr,
                Box::new(move |scrut_imm| {
                    let anf_arms = lower_arms(
                        anfenv,
                        gensym,
                        join_id.clone(),
                        join_ret_ty.clone(),
                        join_takes_arg,
                        arms,
                    );

                    let anf_default = default.map(|default| {
                        Box::new(lower(
                            anfenv,
                            gensym,
                            *default,
                            Box::new({
                                let join_id = join_id.clone();
                                let join_ret_ty = join_ret_ty.clone();
                                move |imm| Block {
                                    binds: Vec::new(),
                                    term: Term::Jump {
                                        target: join_id,
                                        args: if join_takes_arg {
                                            vec![imm]
                                        } else {
                                            Vec::new()
                                        },
                                        ret_ty: join_ret_ty,
                                    },
                                }
                            }),
                        ))
                    });

                    Block {
                        binds: vec![Bind::Join(join_bind)],
                        term: Term::Match {
                            scrut: scrut_imm,
                            arms: anf_arms,
                            default: anf_default,
                            ret_ty: join_ret_ty,
                        },
                    }
                }),
            )
        }
        LiftExpr::EWhile { cond, body, ty: _ } => {
            let (exit, exit_ret_ty) = {
                let id = join(gensym.gensym("exit"));
                let body = k(unit_imm());
                let ret_ty = body.get_ty();
                (
                    JoinBind {
                        id,
                        params: Vec::new(),
                        ret_ty: ret_ty.clone(),
                        body,
                    },
                    ret_ty,
                )
            };

            let cond_expr = *cond;
            let body_expr = *body;
            let loop_id = join(gensym.gensym("loop"));
            let exit_id = exit.id.clone();
            let loop_id_in_body = loop_id.clone();
            let exit_ret_ty_in_body = exit_ret_ty.clone();

            let prev_ctx = anfenv.loop_ctx.borrow().clone();
            *anfenv.loop_ctx.borrow_mut() = Some(LoopCtx {
                loop_id: loop_id.clone(),
                exit_id: exit_id.clone(),
                exit_ret_ty: exit_ret_ty.clone(),
            });

            let loop_body = lower(
                anfenv,
                gensym,
                cond_expr,
                Box::new(move |cond_imm| {
                    let then_block = lower(
                        anfenv,
                        gensym,
                        body_expr,
                        Box::new({
                            let loop_id = loop_id_in_body.clone();
                            let exit_ret_ty = exit_ret_ty_in_body.clone();
                            move |_imm| Block {
                                binds: Vec::new(),
                                term: Term::Jump {
                                    target: loop_id,
                                    args: Vec::new(),
                                    ret_ty: exit_ret_ty,
                                },
                            }
                        }),
                    );

                    let else_block = Block {
                        binds: Vec::new(),
                        term: Term::Jump {
                            target: exit_id,
                            args: Vec::new(),
                            ret_ty: exit_ret_ty_in_body.clone(),
                        },
                    };

                    Block {
                        binds: Vec::new(),
                        term: Term::If {
                            cond: cond_imm,
                            then_: Box::new(then_block),
                            else_: Box::new(else_block),
                            ret_ty: exit_ret_ty_in_body,
                        },
                    }
                }),
            );

            *anfenv.loop_ctx.borrow_mut() = prev_ctx;

            Block {
                binds: vec![
                    Bind::Join(exit),
                    Bind::JoinRec(vec![JoinBind {
                        id: loop_id.clone(),
                        params: Vec::new(),
                        ret_ty: exit_ret_ty.clone(),
                        body: loop_body,
                    }]),
                ],
                term: Term::Jump {
                    target: loop_id,
                    args: Vec::new(),
                    ret_ty: exit_ret_ty,
                },
            }
        }
        LiftExpr::EBreak { ty: _ } => {
            let ctx = anfenv
                .loop_ctx
                .borrow()
                .clone()
                .expect("break outside while loop");
            Block {
                binds: Vec::new(),
                term: Term::Jump {
                    target: ctx.exit_id,
                    args: Vec::new(),
                    ret_ty: ctx.exit_ret_ty,
                },
            }
        }
        LiftExpr::EContinue { ty: _ } => {
            let ctx = anfenv
                .loop_ctx
                .borrow()
                .clone()
                .expect("continue outside while loop");
            Block {
                binds: Vec::new(),
                term: Term::Jump {
                    target: ctx.loop_id,
                    args: Vec::new(),
                    ret_ty: ctx.exit_ret_ty,
                },
            }
        }
        LiftExpr::EReturn { expr, ty: _ } => {
            let ctx = anfenv
                .return_ctx
                .borrow()
                .clone()
                .expect("return outside function");
            if let Some(expr) = expr {
                lower(
                    anfenv,
                    gensym,
                    *expr,
                    Box::new(move |imm| Block {
                        binds: Vec::new(),
                        term: Term::Jump {
                            target: ctx.join_id,
                            args: if ctx.takes_arg { vec![imm] } else { Vec::new() },
                            ret_ty: ctx.ret_ty,
                        },
                    }),
                )
            } else {
                Block {
                    binds: Vec::new(),
                    term: Term::Jump {
                        target: ctx.join_id,
                        args: Vec::new(),
                        ret_ty: ctx.ret_ty,
                    },
                }
            }
        }
        other => lower_imm(anfenv, gensym, other, k),
    }
}

fn lower_value<'a>(
    anfenv: &'a GlobalAnfEnv,
    gensym: &'a Gensym,
    e: LiftExpr,
    k: Box<dyn FnOnce(ValueExpr) -> Block + 'a>,
) -> Block {
    let e_ty = e.get_ty();
    match e {
        LiftExpr::EVar { .. } | LiftExpr::EPrim { .. } => k(ValueExpr::Imm(lift_imm_to_imm(e))),
        LiftExpr::EConstr {
            constructor: Constructor::Enum(enum_constructor),
            args,
            ty,
        } if args.is_empty() => k(ValueExpr::Imm(ImmExpr::Tag {
            index: enum_constructor.enum_index(),
            ty,
        })),
        LiftExpr::EConstrGet {
            expr,
            constructor,
            field_index,
            ty: _,
        } => lower(
            anfenv,
            gensym,
            *expr,
            Box::new(move |imm| {
                k(ValueExpr::ConstrGet {
                    expr: imm,
                    constructor,
                    field_index,
                    ty: e_ty,
                })
            }),
        ),
        LiftExpr::EUnary { op, expr, ty: _ } => lower(
            anfenv,
            gensym,
            *expr,
            Box::new(move |imm| {
                k(ValueExpr::Unary {
                    op,
                    expr: imm,
                    ty: e_ty,
                })
            }),
        ),
        LiftExpr::EBinary { op, lhs, rhs, .. } => lower(
            anfenv,
            gensym,
            *lhs,
            Box::new(move |lhs_imm| {
                lower(
                    anfenv,
                    gensym,
                    *rhs,
                    Box::new(move |rhs_imm| {
                        k(ValueExpr::Binary {
                            op,
                            lhs: lhs_imm,
                            rhs: rhs_imm,
                            ty: e_ty,
                        })
                    }),
                )
            }),
        ),
        LiftExpr::EAssign {
            name,
            value,
            target_ty,
            ty: _,
        } => lower(
            anfenv,
            gensym,
            *value,
            Box::new(move |value_imm| {
                k(ValueExpr::Assign {
                    name: local(name),
                    value: value_imm,
                    target_ty,
                    ty: e_ty,
                })
            }),
        ),
        LiftExpr::EConstr {
            constructor,
            args,
            ty: _,
        } => lower_list(
            anfenv,
            gensym,
            args,
            Box::new(move |args| {
                k(ValueExpr::Constr {
                    constructor,
                    args,
                    ty: e_ty,
                })
            }),
        ),
        LiftExpr::ETuple { items, ty: _ } => lower_list(
            anfenv,
            gensym,
            items,
            Box::new(move |items| k(ValueExpr::Tuple { items, ty: e_ty })),
        ),
        LiftExpr::EArray { items, ty: _ } => lower_list(
            anfenv,
            gensym,
            items,
            Box::new(move |items| k(ValueExpr::Array { items, ty: e_ty })),
        ),
        LiftExpr::ECall { func, args, ty: _ } => lower(
            anfenv,
            gensym,
            *func,
            Box::new(move |func_imm| {
                lower_list(
                    anfenv,
                    gensym,
                    args,
                    Box::new(move |args_imm| {
                        let call_ty = if let ImmExpr::Var { id, .. } = &func_imm {
                            if id.0 == "array_set" {
                                args_imm.first().map(imm_ty).unwrap_or_else(|| e_ty.clone())
                            } else if id.0 == "array_get" {
                                args_imm
                                    .first()
                                    .and_then(|arg0| match imm_ty(arg0) {
                                        Ty::TArray { elem, .. } => Some(*elem),
                                        _ => None,
                                    })
                                    .unwrap_or_else(|| e_ty.clone())
                            } else {
                                e_ty.clone()
                            }
                        } else {
                            e_ty.clone()
                        };
                        k(ValueExpr::Call {
                            func: func_imm,
                            args: args_imm,
                            ty: call_ty,
                        })
                    }),
                )
            }),
        ),
        LiftExpr::EToDyn {
            trait_name,
            for_ty,
            expr,
            ty: _,
        } => lower(
            anfenv,
            gensym,
            *expr,
            Box::new(move |imm| {
                k(ValueExpr::ToDyn {
                    trait_name,
                    for_ty,
                    expr: imm,
                    ty: e_ty,
                })
            }),
        ),
        LiftExpr::EDynCall {
            trait_name,
            method_name,
            receiver,
            args,
            ty: _,
        } => lower(
            anfenv,
            gensym,
            *receiver,
            Box::new(move |receiver_imm| {
                lower_list(
                    anfenv,
                    gensym,
                    args,
                    Box::new(move |args_imm| {
                        k(ValueExpr::DynCall {
                            trait_name,
                            method_name,
                            receiver: receiver_imm,
                            args: args_imm,
                            ty: e_ty,
                        })
                    }),
                )
            }),
        ),
        LiftExpr::EGo { expr, ty: _ } => lower(
            anfenv,
            gensym,
            *expr,
            Box::new(move |closure| k(ValueExpr::Go { closure, ty: e_ty })),
        ),
        LiftExpr::EProj { tuple, index, .. } => lower(
            anfenv,
            gensym,
            *tuple,
            Box::new(move |tuple_imm| {
                k(ValueExpr::Proj {
                    tuple: tuple_imm,
                    index,
                    ty: e_ty,
                })
            }),
        ),
        other => panic!("expected value expression, found {:?}", other),
    }
}

fn lower_imm<'a>(
    anfenv: &'a GlobalAnfEnv,
    gensym: &'a Gensym,
    e: LiftExpr,
    k: Box<dyn FnOnce(ImmExpr) -> Block + 'a>,
) -> Block {
    match e {
        LiftExpr::EVar { name, ty } => k(ImmExpr::Var {
            id: local(name),
            ty,
        }),
        LiftExpr::EPrim { value, ty } => k(ImmExpr::Prim { value, ty }),
        LiftExpr::EConstr {
            constructor: Constructor::Enum(enum_constructor),
            args,
            ty,
        } if args.is_empty() => k(ImmExpr::Tag {
            index: enum_constructor.enum_index(),
            ty,
        }),
        other => lower_value(
            anfenv,
            gensym,
            other,
            Box::new(move |value| {
                let ty = value_expr_tast_ty(&value);
                let id = local(gensym.gensym("t"));
                let block = k(ImmExpr::Var {
                    id: id.clone(),
                    ty: ty.clone(),
                });
                let mut binds = vec![Bind::Let(LetBind { id, value, ty })];
                binds.extend(block.binds);
                Block {
                    binds,
                    term: block.term,
                }
            }),
        ),
    }
}

fn lower_list<'a>(
    anfenv: &'a GlobalAnfEnv,
    gensym: &'a Gensym,
    es: Vec<LiftExpr>,
    k: Box<dyn FnOnce(Vec<ImmExpr>) -> Block + 'a>,
) -> Block {
    let current = es.into_iter();
    fn go<'a>(
        anfenv: &'a GlobalAnfEnv,
        gensym: &'a Gensym,
        mut it: impl Iterator<Item = LiftExpr> + 'a,
        mut out: Vec<ImmExpr>,
        k: Box<dyn FnOnce(Vec<ImmExpr>) -> Block + 'a>,
    ) -> Block {
        match it.next() {
            None => k(out),
            Some(head) => lower(
                anfenv,
                gensym,
                head,
                Box::new(move |imm_head| {
                    out.push(imm_head);
                    go(anfenv, gensym, it, out, k)
                }),
            ),
        }
    }
    go(anfenv, gensym, current, Vec::new(), k)
}

pub fn anf_file(liftenv: GlobalLiftEnv, gensym: &Gensym, file: LiftFile) -> (File, GlobalAnfEnv) {
    let anfenv = GlobalAnfEnv::from_lift_env(liftenv);
    let mut toplevels = Vec::new();
    for lift_fn in file.toplevels {
        let name = lift_fn.name;
        let params = lift_fn
            .params
            .into_iter()
            .map(|(p, ty)| (local(p), ty))
            .collect();
        let ret_ty = lift_fn.ret_ty;
        let ret_join_id = join(gensym.gensym("ret"));
        let takes_arg = ret_ty != Ty::TUnit;
        let ret_join = if takes_arg {
            let ret_param = local(gensym.gensym("retv"));
            JoinBind {
                id: ret_join_id.clone(),
                params: vec![(ret_param.clone(), ret_ty.clone())],
                ret_ty: ret_ty.clone(),
                body: Block {
                    binds: Vec::new(),
                    term: Term::Return(ImmExpr::Var {
                        id: ret_param,
                        ty: ret_ty.clone(),
                    }),
                },
            }
        } else {
            JoinBind {
                id: ret_join_id.clone(),
                params: Vec::new(),
                ret_ty: ret_ty.clone(),
                body: Block {
                    binds: Vec::new(),
                    term: Term::Return(unit_imm()),
                },
            }
        };

        let prev_return_ctx = anfenv.return_ctx.borrow().clone();
        *anfenv.return_ctx.borrow_mut() = Some(ReturnCtx {
            join_id: ret_join_id.clone(),
            ret_ty: ret_ty.clone(),
            takes_arg,
        });

        let lowered_body = lower(
            &anfenv,
            gensym,
            lift_fn.body,
            Box::new({
                let ret_join_id = ret_join_id.clone();
                let ret_ty = ret_ty.clone();
                move |imm| Block {
                    binds: Vec::new(),
                    term: Term::Jump {
                        target: ret_join_id,
                        args: if takes_arg { vec![imm] } else { Vec::new() },
                        ret_ty,
                    },
                }
            }),
        );

        *anfenv.return_ctx.borrow_mut() = prev_return_ctx;

        let mut binds = vec![Bind::Join(ret_join)];
        binds.extend(lowered_body.binds);
        let body = Block {
            binds,
            term: lowered_body.term,
        };
        toplevels.push(Fn {
            name,
            params,
            ret_ty,
            body,
        });
    }
    let file = File { toplevels };
    anf_verify::verify(&file);
    (file, anfenv)
}

pub mod anf_renamer {
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
                .map(|(n, t)| (rename_local_id(n), t))
                .collect(),
            ret_ty: f.ret_ty,
            body: rename_block(f.body),
        }
    }

    fn rename_local_id(id: anf::LocalId) -> anf::LocalId {
        anf::LocalId(id.0.replace("/", "__"))
    }

    fn rename_join_id(id: anf::JoinId) -> anf::JoinId {
        anf::JoinId(id.0.replace("/", "__"))
    }

    fn rename_imm(imm: anf::ImmExpr) -> anf::ImmExpr {
        match imm {
            anf::ImmExpr::Var { id, ty } => anf::ImmExpr::Var {
                id: rename_local_id(id),
                ty,
            },
            anf::ImmExpr::Prim { value, ty } => anf::ImmExpr::Prim { value, ty },
            anf::ImmExpr::Tag { index, ty } => anf::ImmExpr::Tag { index, ty },
        }
    }

    fn rename_value_expr(e: anf::ValueExpr) -> anf::ValueExpr {
        match e {
            anf::ValueExpr::Imm(imm) => anf::ValueExpr::Imm(rename_imm(imm)),
            anf::ValueExpr::Constr {
                constructor,
                args,
                ty,
            } => anf::ValueExpr::Constr {
                constructor,
                args: args.into_iter().map(rename_imm).collect(),
                ty,
            },
            anf::ValueExpr::Tuple { items, ty } => anf::ValueExpr::Tuple {
                items: items.into_iter().map(rename_imm).collect(),
                ty,
            },
            anf::ValueExpr::Array { items, ty } => anf::ValueExpr::Array {
                items: items.into_iter().map(rename_imm).collect(),
                ty,
            },
            anf::ValueExpr::ConstrGet {
                expr,
                constructor,
                field_index,
                ty,
            } => anf::ValueExpr::ConstrGet {
                expr: rename_imm(expr),
                constructor,
                field_index,
                ty,
            },
            anf::ValueExpr::Unary { op, expr, ty } => anf::ValueExpr::Unary {
                op,
                expr: rename_imm(expr),
                ty,
            },
            anf::ValueExpr::Binary { op, lhs, rhs, ty } => anf::ValueExpr::Binary {
                op,
                lhs: rename_imm(lhs),
                rhs: rename_imm(rhs),
                ty,
            },
            anf::ValueExpr::Assign {
                name,
                value,
                target_ty,
                ty,
            } => anf::ValueExpr::Assign {
                name: rename_local_id(name),
                value: rename_imm(value),
                target_ty,
                ty,
            },
            anf::ValueExpr::Call { func, args, ty } => anf::ValueExpr::Call {
                func: rename_imm(func),
                args: args.into_iter().map(rename_imm).collect(),
                ty,
            },
            anf::ValueExpr::ToDyn {
                trait_name,
                for_ty,
                expr,
                ty,
            } => anf::ValueExpr::ToDyn {
                trait_name,
                for_ty,
                expr: rename_imm(expr),
                ty,
            },
            anf::ValueExpr::DynCall {
                trait_name,
                method_name,
                receiver,
                args,
                ty,
            } => anf::ValueExpr::DynCall {
                trait_name,
                method_name,
                receiver: rename_imm(receiver),
                args: args.into_iter().map(rename_imm).collect(),
                ty,
            },
            anf::ValueExpr::Go { closure, ty } => anf::ValueExpr::Go {
                closure: rename_imm(closure),
                ty,
            },
            anf::ValueExpr::Proj { tuple, index, ty } => anf::ValueExpr::Proj {
                tuple: rename_imm(tuple),
                index,
                ty,
            },
        }
    }

    fn rename_block(block: anf::Block) -> anf::Block {
        anf::Block {
            binds: block.binds.into_iter().map(rename_bind).collect(),
            term: rename_term(block.term),
        }
    }

    fn rename_bind(bind: anf::Bind) -> anf::Bind {
        match bind {
            anf::Bind::Let(let_bind) => anf::Bind::Let(anf::LetBind {
                id: rename_local_id(let_bind.id),
                value: rename_value_expr(let_bind.value),
                ty: let_bind.ty,
            }),
            anf::Bind::Join(join_bind) => anf::Bind::Join(rename_join_bind(join_bind)),
            anf::Bind::JoinRec(binds) => {
                anf::Bind::JoinRec(binds.into_iter().map(rename_join_bind).collect())
            }
        }
    }

    fn rename_join_bind(bind: anf::JoinBind) -> anf::JoinBind {
        anf::JoinBind {
            id: rename_join_id(bind.id),
            params: bind
                .params
                .into_iter()
                .map(|(id, ty)| (rename_local_id(id), ty))
                .collect(),
            ret_ty: bind.ret_ty,
            body: rename_block(bind.body),
        }
    }

    fn rename_term(term: anf::Term) -> anf::Term {
        match term {
            anf::Term::Return(imm) => anf::Term::Return(rename_imm(imm)),
            anf::Term::Jump {
                target,
                args,
                ret_ty,
            } => anf::Term::Jump {
                target: rename_join_id(target),
                args: args.into_iter().map(rename_imm).collect(),
                ret_ty,
            },
            anf::Term::If {
                cond,
                then_,
                else_,
                ret_ty,
            } => anf::Term::If {
                cond: rename_imm(cond),
                then_: Box::new(rename_block(*then_)),
                else_: Box::new(rename_block(*else_)),
                ret_ty,
            },
            anf::Term::Match {
                scrut,
                arms,
                default,
                ret_ty,
            } => anf::Term::Match {
                scrut: rename_imm(scrut),
                arms: arms
                    .into_iter()
                    .map(|arm| anf::Arm {
                        lhs: rename_imm(arm.lhs),
                        body: rename_block(arm.body),
                    })
                    .collect(),
                default: default.map(|b| Box::new(rename_block(*b))),
                ret_ty,
            },
            anf::Term::Unreachable { ty } => anf::Term::Unreachable { ty },
        }
    }
}

pub mod anf_verify {
    use crate::anf;
    use crate::lift::is_closure_env_struct;
    use std::collections::{HashMap, HashSet};

    fn is_closure_struct(ty: &anf::Ty) -> bool {
        matches!(ty, anf::Ty::TStruct { name } if is_closure_env_struct(name))
    }

    #[derive(Debug, Clone)]
    struct JoinSig {
        params: Vec<anf::Ty>,
        ret_ty: anf::Ty,
    }

    pub fn verify(file: &anf::File) {
        let mut errors = Vec::new();
        for f in &file.toplevels {
            verify_fn(&mut errors, f);
        }
        if !errors.is_empty() {
            panic!("join-anf verification failed:\n{}", errors.join("\n"));
        }
    }

    fn verify_fn(errors: &mut Vec<String>, f: &anf::Fn) {
        let mut locals = HashMap::<anf::LocalId, anf::Ty>::new();
        for (id, ty) in &f.params {
            insert_local(errors, &mut locals, id.clone(), ty.clone());
        }

        let joins = HashMap::<anf::JoinId, JoinSig>::new();
        verify_block(errors, &f.ret_ty, &locals, &joins, &f.body);
        let body_ty = f.body.get_ty();
        if !tys_compatible(&body_ty, &f.ret_ty) {
            errors.push(format!(
                "fn {} has return type {:?} but body has type {:?}",
                f.name, f.ret_ty, body_ty
            ));
        }
    }

    fn verify_block(
        errors: &mut Vec<String>,
        fn_ret_ty: &anf::Ty,
        locals: &HashMap<anf::LocalId, anf::Ty>,
        joins: &HashMap<anf::JoinId, JoinSig>,
        block: &anf::Block,
    ) {
        let mut locals = locals.clone();
        let mut joins = joins.clone();

        for bind in &block.binds {
            match bind {
                anf::Bind::Let(let_bind) => {
                    verify_value_expr(errors, &locals, &let_bind.value);
                    let inferred = super::value_expr_tast_ty(&let_bind.value);
                    if !tys_compatible(&inferred, &let_bind.ty) {
                        errors.push(format!(
                            "let {:?} has declared type {:?} but value has type {:?}",
                            let_bind.id, let_bind.ty, inferred
                        ));
                    }
                    insert_local(
                        errors,
                        &mut locals,
                        let_bind.id.clone(),
                        let_bind.ty.clone(),
                    );
                }
                anf::Bind::Join(join_bind) => {
                    verify_join_bind(errors, fn_ret_ty, &locals, &joins, join_bind, false);
                    insert_join(errors, &mut joins, join_bind);
                }
                anf::Bind::JoinRec(group) => {
                    for join_bind in group {
                        insert_join(errors, &mut joins, join_bind);
                    }
                    for join_bind in group {
                        verify_join_bind(errors, fn_ret_ty, &locals, &joins, join_bind, true);
                    }
                }
            }
        }

        verify_term(errors, fn_ret_ty, &locals, &joins, &block.term);
    }

    fn verify_join_bind(
        errors: &mut Vec<String>,
        fn_ret_ty: &anf::Ty,
        outer_locals: &HashMap<anf::LocalId, anf::Ty>,
        outer_joins: &HashMap<anf::JoinId, JoinSig>,
        join_bind: &anf::JoinBind,
        is_rec: bool,
    ) {
        let mut locals = outer_locals.clone();
        let mut param_ids = HashSet::<anf::LocalId>::new();
        for (id, ty) in &join_bind.params {
            if !param_ids.insert(id.clone()) {
                errors.push(format!(
                    "join {:?} has duplicate param {:?}",
                    join_bind.id, id
                ));
            }
            insert_local(errors, &mut locals, id.clone(), ty.clone());
        }

        let joins = if is_rec {
            outer_joins.clone()
        } else {
            let mut without_self = outer_joins.clone();
            without_self.remove(&join_bind.id);
            without_self
        };

        verify_block(errors, fn_ret_ty, &locals, &joins, &join_bind.body);
        let body_ty = join_bind.body.get_ty();
        if !tys_compatible(&body_ty, &join_bind.ret_ty) {
            errors.push(format!(
                "join {:?} has ret_ty {:?} but body has type {:?}",
                join_bind.id, join_bind.ret_ty, body_ty
            ));
        }
    }

    fn insert_local(
        errors: &mut Vec<String>,
        locals: &mut HashMap<anf::LocalId, anf::Ty>,
        id: anf::LocalId,
        ty: anf::Ty,
    ) {
        if let Some(prev) = locals.insert(id.clone(), ty.clone())
            && prev != ty
        {
            errors.push(format!(
                "local {:?} has conflicting types {:?} and {:?}",
                id, prev, ty
            ));
        }
    }

    fn insert_join(
        errors: &mut Vec<String>,
        joins: &mut HashMap<anf::JoinId, JoinSig>,
        join_bind: &anf::JoinBind,
    ) {
        let sig = JoinSig {
            params: join_bind.params.iter().map(|(_, ty)| ty.clone()).collect(),
            ret_ty: join_bind.ret_ty.clone(),
        };
        if let Some(prev) = joins.insert(join_bind.id.clone(), sig.clone())
            && (prev.params != sig.params || prev.ret_ty != sig.ret_ty)
        {
            errors.push(format!(
                "join {:?} has conflicting signatures {:?} and {:?}",
                join_bind.id, prev, sig
            ));
        }
    }

    fn normalize_ty(ty: &anf::Ty) -> &anf::Ty {
        match ty {
            anf::Ty::TApp { ty, args } if args.is_empty() => normalize_ty(ty),
            _ => ty,
        }
    }

    fn tys_compatible(a: &anf::Ty, b: &anf::Ty) -> bool {
        let a = normalize_ty(a);
        let b = normalize_ty(b);
        match (a, b) {
            (anf::Ty::TArray { len: la, elem: ea }, anf::Ty::TArray { len: lb, elem: eb }) => {
                let len_ok = la == lb
                    || *la == crate::tast::ARRAY_WILDCARD_LEN
                    || *lb == crate::tast::ARRAY_WILDCARD_LEN;
                len_ok && tys_compatible(ea, eb)
            }
            (anf::Ty::TTuple { typs: as_ }, anf::Ty::TTuple { typs: bs }) => {
                as_.len() == bs.len()
                    && as_.iter().zip(bs.iter()).all(|(a, b)| tys_compatible(a, b))
            }
            (anf::Ty::TApp { ty: at, args: aa }, anf::Ty::TApp { ty: bt, args: ba }) => {
                aa.len() == ba.len()
                    && tys_compatible(at, bt)
                    && aa.iter().zip(ba.iter()).all(|(a, b)| tys_compatible(a, b))
            }
            (anf::Ty::TSlice { elem: ea }, anf::Ty::TSlice { elem: eb })
            | (anf::Ty::TVec { elem: ea }, anf::Ty::TVec { elem: eb })
            | (anf::Ty::TRef { elem: ea }, anf::Ty::TRef { elem: eb }) => tys_compatible(ea, eb),
            (
                anf::Ty::THashMap { key: ka, value: va },
                anf::Ty::THashMap { key: kb, value: vb },
            ) => tys_compatible(ka, kb) && tys_compatible(va, vb),
            (
                anf::Ty::TFunc {
                    params: pa,
                    ret_ty: ra,
                },
                anf::Ty::TFunc {
                    params: pb,
                    ret_ty: rb,
                },
            ) => {
                pa.len() == pb.len()
                    && pa.iter().zip(pb.iter()).all(|(a, b)| tys_compatible(a, b))
                    && tys_compatible(ra, rb)
            }
            _ => {
                if is_closure_struct(a) && matches!(b, anf::Ty::TFunc { .. })
                    || is_closure_struct(b) && matches!(a, anf::Ty::TFunc { .. })
                {
                    return true;
                }
                a == b
            }
        }
    }

    fn verify_value_expr(
        errors: &mut Vec<String>,
        locals: &HashMap<anf::LocalId, anf::Ty>,
        expr: &anf::ValueExpr,
    ) {
        match expr {
            anf::ValueExpr::Imm(imm) => verify_imm(errors, locals, imm),
            anf::ValueExpr::Constr { args, ty, .. } => {
                for a in args {
                    verify_imm(errors, locals, a);
                }
                verify_ty_is_value(errors, ty);
            }
            anf::ValueExpr::Tuple { items, ty } | anf::ValueExpr::Array { items, ty } => {
                for a in items {
                    verify_imm(errors, locals, a);
                }
                verify_ty_is_value(errors, ty);
            }
            anf::ValueExpr::ConstrGet { expr, ty, .. } => {
                verify_imm(errors, locals, expr);
                verify_ty_is_value(errors, ty);
            }
            anf::ValueExpr::Unary { expr, ty, .. } => {
                verify_imm(errors, locals, expr);
                verify_ty_is_value(errors, ty);
            }
            anf::ValueExpr::Binary { lhs, rhs, ty, .. } => {
                verify_imm(errors, locals, lhs);
                verify_imm(errors, locals, rhs);
                verify_ty_is_value(errors, ty);
            }
            anf::ValueExpr::Assign {
                name,
                value,
                target_ty,
                ty,
            } => {
                verify_imm(errors, locals, value);
                if let Some(local_ty) = locals.get(name) {
                    if !tys_compatible(local_ty, target_ty) {
                        errors.push(format!(
                            "assign target type mismatch for {}: local {:?} vs target {:?}",
                            name.0, local_ty, target_ty
                        ));
                    }
                } else {
                    errors.push(format!("assign target {} is not in scope", name.0));
                }
                verify_ty_is_value(errors, ty);
            }
            anf::ValueExpr::Call { func, args, ty } => {
                verify_imm(errors, locals, func);
                for a in args {
                    verify_imm(errors, locals, a);
                }
                verify_ty_is_value(errors, ty);
            }
            anf::ValueExpr::ToDyn {
                trait_name: _,
                for_ty,
                expr,
                ty,
            } => {
                verify_imm(errors, locals, expr);
                verify_ty_is_value(errors, for_ty);
                verify_ty_is_value(errors, ty);
            }
            anf::ValueExpr::DynCall {
                trait_name: _,
                method_name: _,
                receiver,
                args,
                ty,
            } => {
                verify_imm(errors, locals, receiver);
                for a in args {
                    verify_imm(errors, locals, a);
                }
                verify_ty_is_value(errors, ty);
            }
            anf::ValueExpr::Go { closure, ty } => {
                verify_imm(errors, locals, closure);
                verify_ty_is_value(errors, ty);
            }
            anf::ValueExpr::Proj { tuple, ty, .. } => {
                verify_imm(errors, locals, tuple);
                verify_ty_is_value(errors, ty);
            }
        }
    }

    fn verify_term(
        errors: &mut Vec<String>,
        fn_ret_ty: &anf::Ty,
        locals: &HashMap<anf::LocalId, anf::Ty>,
        joins: &HashMap<anf::JoinId, JoinSig>,
        term: &anf::Term,
    ) {
        match term {
            anf::Term::Return(imm) => {
                verify_imm(errors, locals, imm);
                let ret = super::imm_ty(imm);
                if !tys_compatible(&ret, fn_ret_ty) {
                    errors.push(format!(
                        "return has type {:?} but function expects {:?}",
                        ret, fn_ret_ty
                    ));
                }
            }
            anf::Term::Jump {
                target,
                args,
                ret_ty,
            } => {
                let Some(sig) = joins.get(target) else {
                    errors.push(format!("jump to unknown join {:?}", target));
                    return;
                };
                if sig.params.len() != args.len() {
                    errors.push(format!(
                        "jump to {:?} expects {} args but got {}",
                        target,
                        sig.params.len(),
                        args.len()
                    ));
                }
                for (i, arg) in args.iter().enumerate() {
                    verify_imm(errors, locals, arg);
                    if let Some(param_ty) = sig.params.get(i) {
                        let arg_ty = super::imm_ty(arg);
                        if !tys_compatible(&arg_ty, param_ty) {
                            errors.push(format!(
                                "jump arg {} to {:?} has type {:?} but param expects {:?}",
                                i, target, arg_ty, param_ty
                            ));
                        }
                    }
                }
                if !tys_compatible(&sig.ret_ty, ret_ty) {
                    errors.push(format!(
                        "jump to {:?} has ret_ty {:?} but join ret_ty is {:?}",
                        target, ret_ty, sig.ret_ty
                    ));
                }
            }
            anf::Term::If {
                cond,
                then_,
                else_,
                ret_ty,
            } => {
                verify_imm(errors, locals, cond);
                let cond_ty = super::imm_ty(cond);
                if cond_ty != anf::Ty::TBool {
                    errors.push(format!("if cond has type {:?}, expected bool", cond_ty));
                }
                verify_block(errors, fn_ret_ty, locals, joins, then_);
                verify_block(errors, fn_ret_ty, locals, joins, else_);
                let then_ty = then_.get_ty();
                let else_ty = else_.get_ty();
                if !tys_compatible(&then_ty, ret_ty) || !tys_compatible(&else_ty, ret_ty) {
                    errors.push(format!(
                        "if has ret_ty {:?} but branches have {:?} and {:?}",
                        ret_ty, then_ty, else_ty
                    ));
                }
            }
            anf::Term::Match {
                scrut,
                arms,
                default,
                ret_ty,
            } => {
                verify_imm(errors, locals, scrut);
                let scrut_ty = super::imm_ty(scrut);
                for arm in arms {
                    verify_imm(errors, locals, &arm.lhs);
                    let lhs_ty = super::imm_ty(&arm.lhs);
                    if !tys_compatible(&lhs_ty, &scrut_ty) {
                        errors.push(format!(
                            "match arm pattern has type {:?} but scrutinee has type {:?}",
                            lhs_ty, scrut_ty
                        ));
                    }
                    verify_block(errors, fn_ret_ty, locals, joins, &arm.body);
                    let body_ty = arm.body.get_ty();
                    if !tys_compatible(&body_ty, ret_ty) {
                        errors.push(format!(
                            "match arm body has type {:?} but match ret_ty is {:?}",
                            body_ty, ret_ty
                        ));
                    }
                }
                if let Some(default) = default {
                    verify_block(errors, fn_ret_ty, locals, joins, default);
                    let body_ty = default.get_ty();
                    if !tys_compatible(&body_ty, ret_ty) {
                        errors.push(format!(
                            "match default body has type {:?} but match ret_ty is {:?}",
                            body_ty, ret_ty
                        ));
                    }
                }
            }
            anf::Term::Unreachable { ty } => {
                verify_ty_is_value(errors, ty);
            }
        }
    }

    fn verify_imm(
        errors: &mut Vec<String>,
        locals: &HashMap<anf::LocalId, anf::Ty>,
        imm: &anf::ImmExpr,
    ) {
        match imm {
            anf::ImmExpr::Var { id, ty } => {
                if let Some(expected) = locals.get(id) {
                    if !tys_compatible(expected, ty) {
                        errors.push(format!(
                            "local {:?} used at type {:?} but defined at type {:?}",
                            id, ty, expected
                        ));
                    }
                } else {
                    verify_ty_is_value(errors, ty);
                }
            }
            anf::ImmExpr::Prim { ty, .. } | anf::ImmExpr::Tag { ty, .. } => {
                verify_ty_is_value(errors, ty);
            }
        }
    }

    fn verify_ty_is_value(errors: &mut Vec<String>, ty: &anf::Ty) {
        match ty {
            anf::Ty::TVar(id) => {
                errors.push(format!("unresolved type variable {:?}", id));
            }
            anf::Ty::TTuple { typs } => {
                for t in typs {
                    verify_ty_is_value(errors, t);
                }
            }
            anf::Ty::TApp { ty, args } => {
                verify_ty_is_value(errors, ty);
                for a in args {
                    verify_ty_is_value(errors, a);
                }
            }
            anf::Ty::TArray { elem, .. } => verify_ty_is_value(errors, elem),
            anf::Ty::TSlice { elem } => verify_ty_is_value(errors, elem),
            anf::Ty::TVec { elem } => verify_ty_is_value(errors, elem),
            anf::Ty::TRef { elem } => verify_ty_is_value(errors, elem),
            anf::Ty::THashMap { key, value } => {
                verify_ty_is_value(errors, key);
                verify_ty_is_value(errors, value);
            }
            anf::Ty::TFunc { params, ret_ty } => {
                for p in params {
                    verify_ty_is_value(errors, p);
                }
                verify_ty_is_value(errors, ret_ty);
            }
            anf::Ty::TUnit
            | anf::Ty::TBool
            | anf::Ty::TInt8
            | anf::Ty::TInt16
            | anf::Ty::TInt32
            | anf::Ty::TInt64
            | anf::Ty::TUint8
            | anf::Ty::TUint16
            | anf::Ty::TUint32
            | anf::Ty::TUint64
            | anf::Ty::TFloat32
            | anf::Ty::TFloat64
            | anf::Ty::TString
            | anf::Ty::TChar
            | anf::Ty::TEnum { .. }
            | anf::Ty::TStruct { .. }
            | anf::Ty::TDyn { .. }
            | anf::Ty::TParam { .. } => {}
        }
    }
}
