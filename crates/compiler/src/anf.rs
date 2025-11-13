pub type Ty = crate::tast::Ty;
use crate::tast::{Constructor, Prim};
use crate::{
    core,
    env::{Gensym, GlobalTypeEnv},
};

#[derive(Debug, Clone)]
pub struct File {
    pub toplevels: Vec<Fn>,
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub name: String,
    pub params: Vec<(String, Ty)>,
    pub ret_ty: Ty,
    pub body: AExpr,
}

#[derive(Debug, Clone)]
pub enum ImmExpr {
    ImmVar { name: String, ty: Ty },
    ImmPrim { value: Prim, ty: Ty },
    ImmTag { index: usize, ty: Ty },
}

#[derive(Debug, Clone)]
pub enum CExpr {
    CImm {
        imm: ImmExpr,
    },

    EConstr {
        constructor: Constructor,
        args: Vec<ImmExpr>,
        ty: Ty,
    },
    ETuple {
        items: Vec<ImmExpr>,
        ty: Ty,
    },
    EArray {
        items: Vec<ImmExpr>,
        ty: Ty,
    },
    EMatch {
        expr: Box<ImmExpr>,
        arms: Vec<Arm>,
        default: Option<Box<AExpr>>,
        ty: Ty,
    },
    EIf {
        cond: Box<ImmExpr>,
        then: Box<AExpr>,
        else_: Box<AExpr>,
        ty: Ty,
    },
    EWhile {
        cond: Box<AExpr>,
        body: Box<AExpr>,
        ty: Ty,
    },
    EConstrGet {
        expr: Box<ImmExpr>,
        constructor: Constructor,
        field_index: usize,
        ty: Ty,
    },
    ECall {
        func: ImmExpr,
        args: Vec<ImmExpr>,
        ty: Ty,
    },
    EProj {
        tuple: Box<ImmExpr>,
        index: usize,
        ty: Ty,
    },
}

#[derive(Debug, Clone)]
pub enum AExpr {
    ACExpr {
        expr: CExpr,
    },
    ALet {
        name: String,
        value: Box<CExpr>,
        body: Box<AExpr>,
        ty: Ty,
    },
}

impl AExpr {
    pub fn get_ty(&self) -> Ty {
        match self {
            AExpr::ACExpr { expr } => cexpr_tast_ty(expr),
            AExpr::ALet { ty, .. } => ty.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Arm {
    pub lhs: ImmExpr,
    pub body: AExpr,
}

// Helper function to convert core immediate expressions to ANF immediate expressions
// Assumes the input core::Expr is guaranteed to be an immediate variant.
fn core_imm_to_anf_imm(core_imm: core::Expr) -> ImmExpr {
    match core_imm {
        core::Expr::EVar { name, ty } => ImmExpr::ImmVar { name, ty },
        core::Expr::EPrim { value, ty } => ImmExpr::ImmPrim { value, ty },
        core::Expr::EConstr {
            constructor: Constructor::Enum(enum_constructor),
            args,
            ty,
        } if args.is_empty() => ImmExpr::ImmTag {
            index: enum_constructor.enum_index(),
            ty,
        },
        // Other core::Expr variants are not immediate and should not appear as match arm LHS patterns.
        _ => panic!(
            "Expected an immediate expression for match arm LHS, found {:?}",
            core_imm
        ),
    }
}

fn cexpr_tast_ty(e: &CExpr) -> Ty {
    match e {
        CExpr::CImm { imm } => imm_ty(imm),
        CExpr::EConstr { ty, .. }
        | CExpr::ETuple { ty, .. }
        | CExpr::EArray { ty, .. }
        | CExpr::EMatch { ty, .. }
        | CExpr::EIf { ty, .. }
        | CExpr::EWhile { ty, .. }
        | CExpr::ECall { ty, .. }
        | CExpr::EProj { ty, .. }
        | CExpr::EConstrGet { ty, .. } => ty.clone(),
    }
}

fn imm_ty(imm: &ImmExpr) -> Ty {
    match imm {
        ImmExpr::ImmVar { ty, .. } | ImmExpr::ImmPrim { ty, .. } | ImmExpr::ImmTag { ty, .. } => {
            ty.clone()
        }
    }
}

// Helper function to compile match arms with complex patterns to immediate patterns
fn compile_match_arms_to_anf<'a>(
    genv: &'a GlobalTypeEnv,
    gensym: &'a Gensym,
    scrutinee: ImmExpr,
    arms: Vec<core::Arm>,
    default: Option<Box<core::Expr>>,
    body_ty: Ty,
    k: Box<dyn FnOnce(CExpr) -> AExpr + 'a>,
) -> AExpr {
    let mut anf_arms = Vec::new();

    // Process arms in original order to maintain sequence
    for arm in arms {
        match &arm.lhs {
            core::Expr::EVar { .. } | core::Expr::EPrim { .. } => {
                // Immediate patterns can be converted directly
                let anf_lhs = core_imm_to_anf_imm(arm.lhs);
                let anf_body = anf(
                    genv,
                    gensym,
                    arm.body,
                    Box::new(|c| AExpr::ACExpr { expr: c }),
                );
                anf_arms.push(Arm {
                    lhs: anf_lhs,
                    body: anf_body,
                });
            }
            core::Expr::EConstr {
                constructor: Constructor::Enum(enum_constructor),
                args,
                ty,
            } if args.is_empty() => {
                // Nullary constructors are immediate
                let anf_lhs = ImmExpr::ImmTag {
                    index: enum_constructor.enum_index(),
                    ty: ty.clone(),
                };
                let anf_body = anf(
                    genv,
                    gensym,
                    arm.body,
                    Box::new(|c| AExpr::ACExpr { expr: c }),
                );
                anf_arms.push(Arm {
                    lhs: anf_lhs,
                    body: anf_body,
                });
            }
            core::Expr::EConstr {
                constructor: Constructor::Enum(enum_constructor),
                args: _args,
                ty,
            } => {
                // Patterns were already simplified in compile_match.rs. Do not duplicate field extraction here.
                let anf_lhs = ImmExpr::ImmTag {
                    index: enum_constructor.enum_index(),
                    ty: ty.clone(),
                };
                let anf_body = anf(
                    genv,
                    gensym,
                    arm.body,
                    Box::new(|c| AExpr::ACExpr { expr: c }),
                );
                anf_arms.push(Arm {
                    lhs: anf_lhs,
                    body: anf_body,
                });
            }
            _ => {
                panic!("Unexpected pattern in match arm: {:?}", arm.lhs);
            }
        }
    }

    // Convert default case
    let anf_default = default.map(|def_body| {
        Box::new(anf(
            genv,
            gensym,
            *def_body,
            Box::new(|c| AExpr::ACExpr { expr: c }),
        ))
    });

    k(CExpr::EMatch {
        expr: Box::new(scrutinee),
        arms: anf_arms,
        default: anf_default,
        ty: body_ty,
    })
}

fn anf<'a>(
    genv: &'a GlobalTypeEnv,
    gensym: &'a Gensym,
    e: core::Expr,
    k: Box<dyn FnOnce(CExpr) -> AExpr + 'a>,
) -> AExpr {
    let e_ty = e.get_ty();
    match e {
        core::Expr::EVar { name, ty } => k(CExpr::CImm {
            imm: ImmExpr::ImmVar { name, ty },
        }),
        core::Expr::EPrim { value, ty } => k(CExpr::CImm {
            imm: ImmExpr::ImmPrim { value, ty },
        }),

        core::Expr::EConstr {
            constructor: Constructor::Enum(enum_constructor),
            args,
            ty: _,
        } if args.is_empty() => {
            // Nullary enum constructors are immediate tags
            k(CExpr::CImm {
                imm: ImmExpr::ImmTag {
                    index: enum_constructor.enum_index(),
                    ty: e_ty,
                },
            })
        }
        core::Expr::EConstr {
            constructor,
            args,
            ty: _,
        } => {
            let constructor = constructor.clone();
            let ty_clone = e_ty.clone();
            anf_list(
                genv,
                gensym,
                &args,
                Box::new(move |args| {
                    k(CExpr::EConstr {
                        constructor: constructor.clone(),
                        args,
                        ty: ty_clone.clone(),
                    })
                }),
            )
        }
        core::Expr::ETuple { items, ty: _ } => anf_list(
            genv,
            gensym,
            &items,
            Box::new(move |items| {
                k(CExpr::ETuple {
                    items,
                    ty: e_ty.clone(),
                })
            }),
        ),
        core::Expr::EArray { items, ty: _ } => anf_list(
            genv,
            gensym,
            &items,
            Box::new(move |items| {
                k(CExpr::EArray {
                    items,
                    ty: e_ty.clone(),
                })
            }),
        ),
        core::Expr::EClosure { .. } => {
            panic!("lambda lift should have removed closures before ANF conversion");
        }
        core::Expr::ELet {
            name,
            value,
            body,
            ty: _,
        } => anf(
            genv,
            gensym,
            *value,
            Box::new(move |ve| AExpr::ALet {
                name,
                value: Box::new(ve),
                body: Box::new(anf(genv, gensym, *body, k)),
                ty: e_ty.clone(),
            }),
        ),
        core::Expr::EIf {
            cond,
            then_branch,
            else_branch,
            ty: _,
        } => {
            let then_core = *then_branch;
            let else_core = *else_branch;
            let ty_clone = e_ty.clone();
            anf_imm(
                genv,
                gensym,
                *cond,
                Box::new(move |cond_imm| {
                    let then_a = anf(
                        genv,
                        gensym,
                        then_core,
                        Box::new(|c| AExpr::ACExpr { expr: c }),
                    );
                    let else_a = anf(
                        genv,
                        gensym,
                        else_core,
                        Box::new(|c| AExpr::ACExpr { expr: c }),
                    );
                    k(CExpr::EIf {
                        cond: Box::new(cond_imm),
                        then: Box::new(then_a),
                        else_: Box::new(else_a),
                        ty: ty_clone.clone(),
                    })
                }),
            )
        }
        core::Expr::EWhile { cond, body, ty: _ } => {
            let cond_core = *cond;
            let body_core = *body;
            let ty_clone = e_ty.clone();
            let cond_a = anf(
                genv,
                gensym,
                cond_core,
                Box::new(|c| AExpr::ACExpr { expr: c }),
            );
            let body_a = anf(
                genv,
                gensym,
                body_core,
                Box::new(|c| AExpr::ACExpr { expr: c }),
            );
            k(CExpr::EWhile {
                cond: Box::new(cond_a),
                body: Box::new(body_a),
                ty: ty_clone,
            })
        }
        core::Expr::EMatch {
            expr,
            arms,
            default,
            ty: _,
        } => anf_imm(
            genv,
            gensym,
            *expr,
            Box::new(move |imm_expr| {
                compile_match_arms_to_anf(genv, gensym, imm_expr, arms, default, e_ty, k)
            }),
        ),
        core::Expr::EConstrGet {
            expr,
            constructor,
            field_index,
            ty: _,
        } => anf_imm(
            genv,
            gensym,
            *expr,
            Box::new(move |e| {
                k(CExpr::EConstrGet {
                    expr: Box::new(e),
                    constructor: constructor.clone(),
                    field_index,
                    ty: e_ty,
                })
            }),
        ),
        core::Expr::ECall { func, args, ty: _ } => {
            let call_ty = e_ty.clone();
            anf_imm(
                genv,
                gensym,
                *func,
                Box::new(move |func_imm| {
                    let call_ty = call_ty.clone();
                    anf_list(
                        genv,
                        gensym,
                        &args,
                        Box::new(move |args| {
                            k(CExpr::ECall {
                                func: func_imm,
                                args,
                                ty: call_ty.clone(),
                            })
                        }),
                    )
                }),
            )
        }
        core::Expr::EProj {
            tuple,
            index,
            ty: _,
        } => anf_imm(
            genv,
            gensym,
            *tuple,
            Box::new(move |e| {
                k(CExpr::EProj {
                    tuple: Box::new(e),
                    index,
                    ty: e_ty,
                })
            }),
        ),
    }
}

fn anf_imm<'a>(
    genv: &'a GlobalTypeEnv,
    gensym: &'a Gensym,
    e: core::Expr,
    k: Box<dyn FnOnce(ImmExpr) -> AExpr + 'a>,
) -> AExpr {
    match e {
        core::Expr::EVar { name, ty } => k(ImmExpr::ImmVar { name, ty }),
        core::Expr::EPrim { value, ty } => k(ImmExpr::ImmPrim { value, ty }),
        _ => {
            let name = gensym.gensym("t");
            let ty = e.get_ty();
            anf(
                genv,
                gensym,
                e,
                Box::new(move |value_expr| {
                    let body_expr = k(ImmExpr::ImmVar {
                        name: name.clone(),
                        ty: ty.clone(),
                    });
                    let body_ty = body_expr.get_ty();
                    AExpr::ALet {
                        name: name.clone(),
                        value: Box::new(value_expr),
                        body: Box::new(body_expr),
                        ty: body_ty,
                    }
                }),
            )
        }
    }
}

fn anf_list<'a>(
    genv: &'a GlobalTypeEnv,
    gensym: &'a Gensym,
    es: &'a [core::Expr],
    k: Box<dyn FnOnce(Vec<ImmExpr>) -> AExpr + 'a>,
) -> AExpr {
    if es.is_empty() {
        k(Vec::new())
    } else {
        let head = &es[0];
        let tail = &es[1..];
        anf_imm(
            genv,
            gensym,
            head.clone(),
            Box::new(move |imm_head| {
                anf_list(
                    genv,
                    gensym,
                    tail,
                    Box::new(move |mut imm_tail| {
                        imm_tail.insert(0, imm_head);
                        k(imm_tail)
                    }),
                )
            }),
        )
    }
}

pub fn anf_file(genv: &GlobalTypeEnv, gensym: &Gensym, file: core::File) -> File {
    let mut toplevels = Vec::new();
    for core_fn in file.toplevels {
        let name = core_fn.name;
        let params = core_fn.params;
        let ret_ty = core_fn.ret_ty;
        let body = anf(
            genv,
            gensym,
            core_fn.body,
            Box::new(|c| AExpr::ACExpr { expr: c }),
        );
        toplevels.push(Fn {
            name,
            params,
            ret_ty,
            body,
        });
    }
    File { toplevels }
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
            anf::ImmExpr::ImmPrim { value, ty } => anf::ImmExpr::ImmPrim { value, ty },
            anf::ImmExpr::ImmTag { index, ty } => anf::ImmExpr::ImmTag { index, ty },
        }
    }

    fn rename_cexpr(e: anf::CExpr) -> anf::CExpr {
        match e {
            anf::CExpr::CImm { imm } => anf::CExpr::CImm {
                imm: rename_imm(imm),
            },
            anf::CExpr::EConstr {
                constructor,
                args,
                ty,
            } => anf::CExpr::EConstr {
                constructor,
                args: args.into_iter().map(rename_imm).collect(),
                ty,
            },
            anf::CExpr::ETuple { items, ty } => anf::CExpr::ETuple {
                items: items.into_iter().map(rename_imm).collect(),
                ty,
            },
            anf::CExpr::EArray { items, ty } => anf::CExpr::EArray {
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
            anf::CExpr::EWhile { cond, body, ty } => anf::CExpr::EWhile {
                cond: Box::new(rename_aexpr(*cond)),
                body: Box::new(rename_aexpr(*body)),
                ty,
            },
            anf::CExpr::EConstrGet {
                expr,
                constructor,
                field_index,
                ty,
            } => anf::CExpr::EConstrGet {
                expr: Box::new(rename_imm(*expr)),
                constructor,
                field_index,
                ty,
            },
            anf::CExpr::ECall { func, args, ty } => anf::CExpr::ECall {
                func: rename_imm(func),
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
