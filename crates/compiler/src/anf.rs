pub type Ty = crate::tast::Ty;
use ast::ast::Ident;

use crate::common::{Constructor, Prim};
use crate::core::BinaryOp;
use crate::env::Gensym;
use crate::env::{EnumDef, StructDef};
use crate::lift::{GlobalLiftEnv, LiftArm, LiftExpr, LiftFile};

#[derive(Debug, Clone)]
pub struct GlobalAnfEnv {
    pub liftenv: GlobalLiftEnv,
}

impl GlobalAnfEnv {
    pub fn from_lift_env(liftenv: GlobalLiftEnv) -> Self {
        GlobalAnfEnv { liftenv }
    }

    pub fn enums(&self) -> impl Iterator<Item = (&Ident, &EnumDef)> {
        self.liftenv.enums()
    }

    pub fn structs(&self) -> impl Iterator<Item = (&Ident, &StructDef)> {
        self.liftenv.structs()
    }

    pub fn get_struct(&self, name: &Ident) -> Option<&StructDef> {
        self.liftenv.get_struct(name)
    }
}

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
    EBinary {
        op: BinaryOp,
        lhs: Box<ImmExpr>,
        rhs: Box<ImmExpr>,
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

// Helper function to convert lift immediate expressions to ANF immediate expressions
// Assumes the input LiftExpr is guaranteed to be an immediate variant.
fn lift_imm_to_anf_imm(lift_imm: LiftExpr) -> ImmExpr {
    match lift_imm {
        LiftExpr::EVar { name, ty } => ImmExpr::ImmVar { name, ty },
        LiftExpr::EPrim { value, ty } => ImmExpr::ImmPrim { value, ty },
        LiftExpr::EConstr {
            constructor: Constructor::Enum(enum_constructor),
            args,
            ty,
        } if args.is_empty() => ImmExpr::ImmTag {
            index: enum_constructor.enum_index(),
            ty,
        },
        // Other LiftExpr variants are not immediate and should not appear as match arm LHS patterns.
        _ => panic!(
            "Expected an immediate expression for match arm LHS, found {:?}",
            lift_imm
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
        | CExpr::EBinary { ty, .. }
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
    anfenv: &'a GlobalAnfEnv,
    gensym: &'a Gensym,
    scrutinee: ImmExpr,
    arms: Vec<LiftArm>,
    default: Option<Box<LiftExpr>>,
    body_ty: Ty,
    k: Box<dyn FnOnce(CExpr) -> AExpr + 'a>,
) -> AExpr {
    let mut anf_arms = Vec::new();

    // Process arms in original order to maintain sequence
    for arm in arms {
        match &arm.lhs {
            LiftExpr::EVar { .. } | LiftExpr::EPrim { .. } => {
                // Immediate patterns can be converted directly
                let anf_lhs = lift_imm_to_anf_imm(arm.lhs);
                let anf_body = anf(
                    anfenv,
                    gensym,
                    arm.body,
                    Box::new(|c| AExpr::ACExpr { expr: c }),
                );
                anf_arms.push(Arm {
                    lhs: anf_lhs,
                    body: anf_body,
                });
            }
            LiftExpr::EConstr {
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
                    anfenv,
                    gensym,
                    arm.body,
                    Box::new(|c| AExpr::ACExpr { expr: c }),
                );
                anf_arms.push(Arm {
                    lhs: anf_lhs,
                    body: anf_body,
                });
            }
            LiftExpr::EConstr {
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
                    anfenv,
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
            anfenv,
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
    anfenv: &'a GlobalAnfEnv,
    gensym: &'a Gensym,
    e: LiftExpr,
    k: Box<dyn FnOnce(CExpr) -> AExpr + 'a>,
) -> AExpr {
    let e_ty = e.get_ty();
    match e {
        LiftExpr::EVar { name, ty } => k(CExpr::CImm {
            imm: ImmExpr::ImmVar { name, ty },
        }),
        LiftExpr::EPrim { value, ty } => k(CExpr::CImm {
            imm: ImmExpr::ImmPrim { value, ty },
        }),

        LiftExpr::EConstr {
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
        LiftExpr::EConstr {
            constructor,
            args,
            ty: _,
        } => {
            let constructor = constructor.clone();
            let ty_clone = e_ty.clone();
            anf_list(
                anfenv,
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
        LiftExpr::ETuple { items, ty: _ } => anf_list(
            anfenv,
            gensym,
            &items,
            Box::new(move |items| {
                k(CExpr::ETuple {
                    items,
                    ty: e_ty.clone(),
                })
            }),
        ),
        LiftExpr::EArray { items, ty: _ } => anf_list(
            anfenv,
            gensym,
            &items,
            Box::new(move |items| {
                k(CExpr::EArray {
                    items,
                    ty: e_ty.clone(),
                })
            }),
        ),
        LiftExpr::ELet {
            name,
            value,
            body,
            ty: _,
        } => anf(
            anfenv,
            gensym,
            *value,
            Box::new(move |ve| AExpr::ALet {
                name,
                value: Box::new(ve),
                body: Box::new(anf(anfenv, gensym, *body, k)),
                ty: e_ty.clone(),
            }),
        ),
        LiftExpr::EIf {
            cond,
            then_branch,
            else_branch,
            ty: _,
        } => {
            let then_mono = *then_branch;
            let else_mono = *else_branch;
            let ty_clone = e_ty.clone();
            anf_imm(
                anfenv,
                gensym,
                *cond,
                Box::new(move |cond_imm| {
                    let then_a = anf(
                        anfenv,
                        gensym,
                        then_mono,
                        Box::new(|c| AExpr::ACExpr { expr: c }),
                    );
                    let else_a = anf(
                        anfenv,
                        gensym,
                        else_mono,
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
        LiftExpr::EWhile { cond, body, ty: _ } => {
            let cond_mono = *cond;
            let body_mono = *body;
            let ty_clone = e_ty.clone();
            let cond_a = anf(
                anfenv,
                gensym,
                cond_mono,
                Box::new(|c| AExpr::ACExpr { expr: c }),
            );
            let body_a = anf(
                anfenv,
                gensym,
                body_mono,
                Box::new(|c| AExpr::ACExpr { expr: c }),
            );
            k(CExpr::EWhile {
                cond: Box::new(cond_a),
                body: Box::new(body_a),
                ty: ty_clone,
            })
        }
        LiftExpr::EMatch {
            expr,
            arms,
            default,
            ty: _,
        } => anf_imm(
            anfenv,
            gensym,
            *expr,
            Box::new(move |imm_expr| {
                compile_match_arms_to_anf(anfenv, gensym, imm_expr, arms, default, e_ty, k)
            }),
        ),
        LiftExpr::EConstrGet {
            expr,
            constructor,
            field_index,
            ty: _,
        } => anf_imm(
            anfenv,
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
        LiftExpr::EBinary {
            op,
            lhs,
            rhs,
            ty: _,
        } => {
            let op_copy = op;
            anf_imm(
                anfenv,
                gensym,
                *lhs,
                Box::new(move |lhs_imm| {
                    let op_copy = op_copy;
                    let e_ty = e_ty.clone();
                    anf_imm(
                        anfenv,
                        gensym,
                        *rhs,
                        Box::new(move |rhs_imm| {
                            k(CExpr::EBinary {
                                op: op_copy,
                                lhs: Box::new(lhs_imm),
                                rhs: Box::new(rhs_imm),
                                ty: e_ty.clone(),
                            })
                        }),
                    )
                }),
            )
        }
        LiftExpr::ECall { func, args, ty: _ } => {
            let call_ty = e_ty.clone();
            anf_imm(
                anfenv,
                gensym,
                *func,
                Box::new(move |func_imm| {
                    let call_ty = call_ty.clone();
                    anf_list(
                        anfenv,
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
        LiftExpr::EProj {
            tuple,
            index,
            ty: _,
        } => anf_imm(
            anfenv,
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
    anfenv: &'a GlobalAnfEnv,
    gensym: &'a Gensym,
    e: LiftExpr,
    k: Box<dyn FnOnce(ImmExpr) -> AExpr + 'a>,
) -> AExpr {
    match e {
        LiftExpr::EVar { name, ty } => k(ImmExpr::ImmVar { name, ty }),
        LiftExpr::EPrim { value, ty } => k(ImmExpr::ImmPrim { value, ty }),
        _ => {
            let name = gensym.gensym("t");
            let ty = e.get_ty();
            anf(
                anfenv,
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
    anfenv: &'a GlobalAnfEnv,
    gensym: &'a Gensym,
    es: &'a [LiftExpr],
    k: Box<dyn FnOnce(Vec<ImmExpr>) -> AExpr + 'a>,
) -> AExpr {
    if es.is_empty() {
        k(Vec::new())
    } else {
        let head = &es[0];
        let tail = &es[1..];
        anf_imm(
            anfenv,
            gensym,
            head.clone(),
            Box::new(move |imm_head| {
                anf_list(
                    anfenv,
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

pub fn anf_file(liftenv: GlobalLiftEnv, gensym: &Gensym, file: LiftFile) -> (File, GlobalAnfEnv) {
    let anfenv = GlobalAnfEnv::from_lift_env(liftenv);
    let mut toplevels = Vec::new();
    for lift_fn in file.toplevels {
        let name = lift_fn.name;
        let params = lift_fn.params;
        let ret_ty = lift_fn.ret_ty;
        let body = anf(
            &anfenv,
            gensym,
            lift_fn.body,
            Box::new(|c| AExpr::ACExpr { expr: c }),
        );
        toplevels.push(Fn {
            name,
            params,
            ret_ty,
            body,
        });
    }
    (File { toplevels }, anfenv)
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
            anf::CExpr::EBinary { op, lhs, rhs, ty } => anf::CExpr::EBinary {
                op,
                lhs: Box::new(rename_imm(*lhs)),
                rhs: Box::new(rename_imm(*rhs)),
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
