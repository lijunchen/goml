use crate::common::{Constructor, Prim, StructConstructor};
use crate::env::PackageTypeEnv;
use crate::hir;
use crate::intrinsics::LangItemId;
use crate::tast;
use crate::typer::results::{
    CalleeElab, Coercion, NameRefElab, StructLitArgElab, StructPatArgElab, TryKind, TypeckResults,
};
use crate::typer::type_ops::instantiate_self_ty;

pub fn build_file(
    genv: &PackageTypeEnv,
    hir: &hir::PackageHir,
    hir_table: &hir::HirTable,
    results: &TypeckResults,
) -> tast::File {
    let mut toplevels = Vec::new();
    for def_id in hir.toplevels.iter().copied() {
        match hir_table.def(def_id).clone() {
            hir::Def::ImplBlock(impl_block) => toplevels.push(tast::Item::ImplBlock(
                build_impl_block(genv, hir_table, results, &impl_block),
            )),
            hir::Def::Fn(func) => toplevels.push(tast::Item::Fn(build_fn(
                genv,
                hir_table,
                results,
                &func,
                &tparams_for(&func.generics),
                None,
                genv.current()
                    .value_env
                    .get_function_scheme(&func.name)
                    .as_ref(),
            ))),
            hir::Def::EnumDef(..)
            | hir::Def::StructDef(..)
            | hir::Def::TraitDef(..)
            | hir::Def::ExternFn(..) => {}
        }
    }
    tast::File { toplevels }
}

fn build_impl_block(
    genv: &PackageTypeEnv,
    hir_table: &hir::HirTable,
    results: &TypeckResults,
    impl_block: &hir::ImplBlock,
) -> tast::ImplBlock {
    let impl_tparams = tparams_for(&impl_block.generics);
    let for_ty = tast::Ty::from_hir(genv, &impl_block.for_type, &impl_tparams);
    let raw_trait_ref = impl_block
        .trait_ref
        .as_ref()
        .map(|trait_ref| tast::TraitRef::from_hir(genv, trait_ref, &impl_tparams));
    let trait_ref = raw_trait_ref.as_ref().map(|raw_trait_ref| {
        genv.current()
            .trait_env
            .trait_impls
            .keys()
            .find(|key| {
                erase_projection_qualifiers(&key.for_ty) == erase_projection_qualifiers(&for_ty)
                    && erase_trait_ref_projection_qualifiers(&key.trait_ref)
                        == erase_trait_ref_projection_qualifiers(raw_trait_ref)
            })
            .map(|key| key.trait_ref.clone())
            .unwrap_or_else(|| raw_trait_ref.clone())
    });

    let mut methods = Vec::new();
    for method_id in impl_block.methods.iter().copied() {
        let hir::Def::Fn(func) = hir_table.def(method_id).clone() else {
            continue;
        };
        let mut all_generics = impl_block.generics.clone();
        all_generics.extend(func.generics.clone());
        let scheme = match &trait_ref {
            Some(trait_ref) => genv
                .current()
                .trait_env
                .trait_impls
                .get(&crate::env::TraitImplKey {
                    trait_ref: trait_ref.clone(),
                    for_ty: for_ty.clone(),
                })
                .and_then(|definition| definition.methods.get(&func.name))
                .cloned(),
            None => genv
                .current()
                .lookup_inherent_method_scheme(&for_ty, &tast::TastIdent::new(&func.name)),
        };
        methods.push(build_fn(
            genv,
            hir_table,
            results,
            &func,
            &tparams_for(&all_generics),
            Some(&for_ty),
            scheme.as_ref(),
        ));
    }

    tast::ImplBlock {
        generics: impl_block
            .generics
            .iter()
            .map(|g| g.to_ident_name())
            .collect(),
        trait_ref,
        for_type: for_ty,
        methods,
    }
}

fn erase_trait_ref_projection_qualifiers(trait_ref: &tast::TraitRef) -> tast::TraitRef {
    tast::TraitRef {
        name: trait_ref.name.clone(),
        args: trait_ref
            .args
            .iter()
            .map(erase_projection_qualifiers)
            .collect(),
    }
}

fn erase_projection_qualifiers(ty: &tast::Ty) -> tast::Ty {
    crate::typer::type_ops::rewrite_ty(ty, &mut |ty| {
        let tast::Ty::TProjection { for_ty, name, .. } = ty else {
            return None;
        };
        Some(tast::Ty::TProjection {
            trait_ref: None,
            for_ty: for_ty.clone(),
            name: name.clone(),
        })
    })
}

fn build_fn(
    genv: &PackageTypeEnv,
    hir_table: &hir::HirTable,
    results: &TypeckResults,
    func: &hir::Fn,
    tparams: &[tast::TastIdent],
    self_ty: Option<&tast::Ty>,
    scheme: Option<&crate::env::FnScheme>,
) -> tast::Fn {
    let scheme_signature = scheme.and_then(|scheme| match &scheme.ty {
        tast::Ty::TFunc { params, ret_ty } => Some((params, ret_ty.as_ref())),
        _ => None,
    });
    let params = func
        .params
        .iter()
        .enumerate()
        .map(|(index, (name, ty))| {
            let name_str = hir_table.local_ident_name(*name);
            let mut ty = scheme_signature
                .and_then(|(params, _)| params.get(index))
                .cloned()
                .unwrap_or_else(|| tast::Ty::from_hir(genv, ty, tparams));
            if let Some(self_ty) = self_ty {
                ty = instantiate_self_ty(&ty, self_ty);
            }
            (name_str, ty)
        })
        .collect::<Vec<_>>();

    let mut ret_ty = scheme_signature
        .map(|(_, ret_ty)| ret_ty.clone())
        .unwrap_or_else(|| {
            func.ret_ty
                .as_ref()
                .map(|ty| tast::Ty::from_hir(genv, ty, tparams))
                .unwrap_or(tast::Ty::TUnit)
        });
    if let Some(self_ty) = self_ty {
        ret_ty = instantiate_self_ty(&ret_ty, self_ty);
    }

    tast::Fn {
        visibility: func.visibility,
        name: func.name.clone(),
        params,
        ret_ty,
        body: build_block(hir_table, results, &func.body),
    }
}

fn tparams_for(generics: &[hir::HirIdent]) -> Vec<tast::TastIdent> {
    generics
        .iter()
        .map(|g| tast::TastIdent(g.to_ident_name()))
        .collect()
}

fn build_expr(
    hir_table: &hir::HirTable,
    results: &TypeckResults,
    expr_id: hir::ExprId,
) -> tast::Expr {
    let built = match hir_table.expr(expr_id).clone() {
        hir::Expr::ENameRef { .. } | hir::Expr::EStaticMember { .. } => {
            build_name_ref_expr(hir_table, results, expr_id)
        }
        hir::Expr::EUnit => tast::Expr::EPrim {
            value: Prim::unit(),
            ty: tast::Ty::TUnit,
        },
        hir::Expr::EBool { value } => tast::Expr::EPrim {
            value: Prim::boolean(value),
            ty: tast::Ty::TBool,
        },
        hir::Expr::EInt { value } => {
            let resolved_ty = results.expr_ty(expr_id).cloned();
            match resolved_ty {
                Some(tast::Ty::TInt) => tast::Expr::EPrim {
                    value: Prim::Int {
                        value: parse_signed(&value).unwrap_or(0),
                    },
                    ty: tast::Ty::TInt,
                },
                Some(tast::Ty::TInt8) => tast::Expr::EPrim {
                    value: Prim::Int8 {
                        value: parse_signed(&value).unwrap_or(0),
                    },
                    ty: tast::Ty::TInt8,
                },
                Some(tast::Ty::TInt16) => tast::Expr::EPrim {
                    value: Prim::Int16 {
                        value: parse_signed(&value).unwrap_or(0),
                    },
                    ty: tast::Ty::TInt16,
                },
                Some(tast::Ty::TInt32) => tast::Expr::EPrim {
                    value: Prim::Int32 {
                        value: parse_signed(&value).unwrap_or(0),
                    },
                    ty: tast::Ty::TInt32,
                },
                Some(tast::Ty::TInt64) => tast::Expr::EPrim {
                    value: Prim::Int64 {
                        value: parse_signed(&value).unwrap_or(0),
                    },
                    ty: tast::Ty::TInt64,
                },
                Some(tast::Ty::TUint) => tast::Expr::EPrim {
                    value: Prim::UInt {
                        value: parse_unsigned(&value).unwrap_or(0),
                    },
                    ty: tast::Ty::TUint,
                },
                Some(tast::Ty::TUint8) => tast::Expr::EPrim {
                    value: Prim::UInt8 {
                        value: parse_unsigned(&value).unwrap_or(0),
                    },
                    ty: tast::Ty::TUint8,
                },
                Some(tast::Ty::TUint16) => tast::Expr::EPrim {
                    value: Prim::UInt16 {
                        value: parse_unsigned(&value).unwrap_or(0),
                    },
                    ty: tast::Ty::TUint16,
                },
                Some(tast::Ty::TUint32) => tast::Expr::EPrim {
                    value: Prim::UInt32 {
                        value: parse_unsigned(&value).unwrap_or(0),
                    },
                    ty: tast::Ty::TUint32,
                },
                Some(tast::Ty::TUint64) => tast::Expr::EPrim {
                    value: Prim::UInt64 {
                        value: parse_unsigned(&value).unwrap_or(0),
                    },
                    ty: tast::Ty::TUint64,
                },
                Some(tast::Ty::TFloat32) => tast::Expr::EPrim {
                    value: Prim::Float32 {
                        value: value.parse::<f64>().unwrap_or(0.0) as f32,
                    },
                    ty: tast::Ty::TFloat32,
                },
                Some(tast::Ty::TFloat64) => tast::Expr::EPrim {
                    value: Prim::Float64 {
                        value: value.parse::<f64>().unwrap_or(0.0),
                    },
                    ty: tast::Ty::TFloat64,
                },
                _ => tast::Expr::EPrim {
                    value: Prim::Int {
                        value: parse_signed(&value).unwrap_or(0),
                    },
                    ty: tast::Ty::TInt,
                },
            }
        }
        hir::Expr::EFloat { value } => {
            let resolved_ty = results.expr_ty(expr_id).cloned();
            match resolved_ty {
                Some(tast::Ty::TInt) => tast::Expr::EPrim {
                    value: Prim::Int {
                        value: value as i64,
                    },
                    ty: tast::Ty::TInt,
                },
                Some(tast::Ty::TInt8) => tast::Expr::EPrim {
                    value: Prim::Int8 { value: value as i8 },
                    ty: tast::Ty::TInt8,
                },
                Some(tast::Ty::TInt16) => tast::Expr::EPrim {
                    value: Prim::Int16 {
                        value: value as i16,
                    },
                    ty: tast::Ty::TInt16,
                },
                Some(tast::Ty::TInt32) => tast::Expr::EPrim {
                    value: Prim::Int32 {
                        value: value as i32,
                    },
                    ty: tast::Ty::TInt32,
                },
                Some(tast::Ty::TInt64) => tast::Expr::EPrim {
                    value: Prim::Int64 {
                        value: value as i64,
                    },
                    ty: tast::Ty::TInt64,
                },
                Some(tast::Ty::TUint) => tast::Expr::EPrim {
                    value: Prim::UInt {
                        value: value as u64,
                    },
                    ty: tast::Ty::TUint,
                },
                Some(tast::Ty::TUint8) => tast::Expr::EPrim {
                    value: Prim::UInt8 { value: value as u8 },
                    ty: tast::Ty::TUint8,
                },
                Some(tast::Ty::TUint16) => tast::Expr::EPrim {
                    value: Prim::UInt16 {
                        value: value as u16,
                    },
                    ty: tast::Ty::TUint16,
                },
                Some(tast::Ty::TUint32) => tast::Expr::EPrim {
                    value: Prim::UInt32 {
                        value: value as u32,
                    },
                    ty: tast::Ty::TUint32,
                },
                Some(tast::Ty::TUint64) => tast::Expr::EPrim {
                    value: Prim::UInt64 {
                        value: value as u64,
                    },
                    ty: tast::Ty::TUint64,
                },
                Some(tast::Ty::TFloat32) => tast::Expr::EPrim {
                    value: Prim::Float32 {
                        value: value as f32,
                    },
                    ty: tast::Ty::TFloat32,
                },
                _ => tast::Expr::EPrim {
                    value: Prim::Float64 { value },
                    ty: tast::Ty::TFloat64,
                },
            }
        }
        hir::Expr::EString { value } => tast::Expr::EPrim {
            value: Prim::string(value),
            ty: tast::Ty::TString,
        },
        hir::Expr::EChar { value } => tast::Expr::EPrim {
            value: Prim::Char {
                value: parse_char_literal(&value).unwrap_or('\0'),
            },
            ty: tast::Ty::TChar,
        },
        hir::Expr::EConstr { args, .. } => {
            let constructor = results
                .constructor_for_expr(expr_id)
                .cloned()
                .unwrap_or_else(error_constructor);
            let args = args
                .iter()
                .copied()
                .map(|arg| build_expr(hir_table, results, arg))
                .collect::<Vec<_>>();
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            if args.is_empty()
                && let tast::Ty::TFunc { params, ret_ty } = &ty
            {
                let params = params
                    .iter()
                    .enumerate()
                    .map(|(idx, param_ty)| tast::ClosureParam {
                        name: format!("ctor_arg_{idx}"),
                        ty: param_ty.clone(),
                        astptr: None,
                    })
                    .collect::<Vec<_>>();
                let args = params
                    .iter()
                    .map(|param| tast::Expr::EVar {
                        name: param.name.clone(),
                        ty: param.ty.clone(),
                        astptr: None,
                    })
                    .collect::<Vec<_>>();
                return tast::Expr::EClosure {
                    params,
                    body: Box::new(tast::Expr::EConstr {
                        constructor,
                        args,
                        ty: (**ret_ty).clone(),
                    }),
                    ty,
                    captures: Vec::new(),
                };
            }
            tast::Expr::EConstr {
                constructor,
                args,
                ty,
            }
        }
        hir::Expr::EStructLiteral { .. } => {
            let Some(elab) = results.struct_lit_elab(expr_id) else {
                return tast::Expr::EVar {
                    name: "<error>".to_string(),
                    ty: results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit),
                    astptr: None,
                };
            };
            let args = elab
                .args
                .iter()
                .map(|arg| match arg {
                    StructLitArgElab::Expr(e) => build_expr(hir_table, results, *e),
                    StructLitArgElab::Missing { expected_ty } => tast::Expr::EVar {
                        name: "<error>".to_string(),
                        ty: expected_ty.clone(),
                        astptr: None,
                    },
                })
                .collect::<Vec<_>>();
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Expr::EConstr {
                constructor: elab.constructor.clone(),
                args,
                ty,
            }
        }
        hir::Expr::ETuple { items } => {
            let items = items
                .iter()
                .copied()
                .map(|e| build_expr(hir_table, results, e))
                .collect::<Vec<_>>();
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Expr::ETuple { items, ty }
        }
        hir::Expr::EArray { items } => {
            let items = items
                .iter()
                .copied()
                .map(|e| build_expr(hir_table, results, e))
                .collect::<Vec<_>>();
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Expr::EArray { items, ty }
        }
        hir::Expr::EClosure { params, body } => {
            let params = params
                .iter()
                .map(|p| {
                    let name = hir_table.local_ident_name(p.name);
                    let ty = results.local_ty(p.name).cloned().unwrap_or(tast::Ty::TUnit);
                    tast::ClosureParam {
                        name,
                        ty,
                        astptr: Some(p.astptr),
                    }
                })
                .collect::<Vec<_>>();
            let body = Box::new(build_expr(hir_table, results, body));
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            let captures = results
                .closure_captures(expr_id)
                .map(|v| v.to_vec())
                .unwrap_or_default();
            tast::Expr::EClosure {
                params,
                body,
                ty,
                captures,
            }
        }
        hir::Expr::EMatch { expr, arms } => {
            let scrutinee = Box::new(build_expr(hir_table, results, expr));
            let arms = arms
                .iter()
                .map(|arm| tast::Arm {
                    pat: build_pat(hir_table, results, arm.pat),
                    guard: arm.guard.map(|guard| build_expr(hir_table, results, guard)),
                    body: build_expr(hir_table, results, arm.body),
                })
                .collect::<Vec<_>>();
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Expr::EMatch {
                expr: scrutinee,
                arms,
                ty,
                astptr: hir_table.expr_ptr(expr_id),
            }
        }
        hir::Expr::EIf {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond = Box::new(build_expr(hir_table, results, cond));
            let then_branch = Box::new(build_expr(hir_table, results, then_branch));
            let else_branch = Box::new(build_expr(hir_table, results, else_branch));
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Expr::EIf {
                cond,
                then_branch,
                else_branch,
                ty,
            }
        }
        hir::Expr::EWhile { cond, body } => {
            let cond = Box::new(build_expr(hir_table, results, cond));
            let body = Box::new(build_expr(hir_table, results, body));
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Expr::EWhile { cond, body, ty }
        }
        hir::Expr::EFor {
            pat,
            iterator,
            body,
        } => {
            let pat = build_pat(hir_table, results, pat);
            let iterator = Box::new(build_expr(hir_table, results, iterator));
            let (into_iter_trait_ref, iterator_trait_ref, iterator_ty) = results
                .for_elab(expr_id)
                .map(|elab| {
                    (
                        elab.into_iter_trait_ref.clone(),
                        elab.iterator_trait_ref.clone(),
                        elab.iterator_ty.clone(),
                    )
                })
                .unwrap_or_else(|| {
                    let into_iter_trait_ref = tast::TraitRef {
                        name: tast::TastIdent::new(LangItemId::IntoIterator.source_name()),
                        args: Vec::new(),
                    };
                    let iterator_trait_ref = tast::TraitRef {
                        name: tast::TastIdent::new(LangItemId::Iterator.source_name()),
                        args: Vec::new(),
                    };
                    let iterator_ty = tast::Ty::TProjection {
                        trait_ref: Some(into_iter_trait_ref.clone()),
                        for_ty: Box::new(iterator.get_ty()),
                        name: tast::TastIdent::new("IntoIter"),
                    };
                    (into_iter_trait_ref, iterator_trait_ref, iterator_ty)
                });
            let body = Box::new(build_expr(hir_table, results, body));
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Expr::EFor {
                pat: Box::new(pat),
                iterator,
                into_iter_trait_ref,
                iterator_trait_ref,
                iterator_ty,
                body,
                ty,
            }
        }
        hir::Expr::EBreak => {
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Expr::EBreak { ty }
        }
        hir::Expr::EContinue => {
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Expr::EContinue { ty }
        }
        hir::Expr::EReturn { expr } => {
            let expr = expr.map(|expr| Box::new(build_expr(hir_table, results, expr)));
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Expr::EReturn { expr, ty }
        }
        hir::Expr::EGo { expr } => {
            let expr = Box::new(build_expr(hir_table, results, expr));
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Expr::EGo { expr, ty }
        }
        hir::Expr::ECall { .. } => {
            let Some(call) = results.call_elab(expr_id) else {
                return tast::Expr::EVar {
                    name: "<error>".to_string(),
                    ty: results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit),
                    astptr: None,
                };
            };
            let func = Box::new(build_callee(hir_table, results, &call.callee));
            let args = call
                .args
                .iter()
                .copied()
                .map(|e| build_expr(hir_table, results, e))
                .collect::<Vec<_>>();
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Expr::ECall { func, args, ty }
        }
        hir::Expr::EUnary { op, expr } => {
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            if op == common_defs::UnaryOp::Neg
                && let hir::Expr::EInt { value } = hir_table.expr(expr)
                && matches!(
                    ty,
                    tast::Ty::TInt
                        | tast::Ty::TInt8
                        | tast::Ty::TInt16
                        | tast::Ty::TInt32
                        | tast::Ty::TInt64
                )
            {
                let negated = format!("-{}", value);
                let prim = match &ty {
                    tast::Ty::TInt => Prim::Int {
                        value: negated.parse::<i64>().unwrap_or(0),
                    },
                    tast::Ty::TInt8 => Prim::Int8 {
                        value: negated.parse::<i8>().unwrap_or(0),
                    },
                    tast::Ty::TInt16 => Prim::Int16 {
                        value: negated.parse::<i16>().unwrap_or(0),
                    },
                    tast::Ty::TInt32 => Prim::Int32 {
                        value: negated.parse::<i32>().unwrap_or(0),
                    },
                    tast::Ty::TInt64 => Prim::Int64 {
                        value: negated.parse::<i64>().unwrap_or(0),
                    },
                    _ => unreachable!(),
                };
                tast::Expr::EPrim { value: prim, ty }
            } else {
                let expr = Box::new(build_expr(hir_table, results, expr));
                let resolution = results
                    .unary_resolution(expr_id)
                    .cloned()
                    .unwrap_or(tast::UnaryResolution::Builtin);
                tast::Expr::EUnary {
                    op,
                    expr,
                    ty,
                    resolution,
                }
            }
        }
        hir::Expr::ECast { expr, .. } => {
            let expr = Box::new(build_expr(hir_table, results, expr));
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Expr::ECast { expr, ty }
        }
        hir::Expr::ETry { expr } => build_try_expr(hir_table, results, expr_id, expr),
        hir::Expr::ERange { start, end } => {
            let start = build_expr(hir_table, results, start);
            let end = build_expr(hir_table, results, end);
            let ret_ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            let func_ty = tast::Ty::TFunc {
                params: vec![tast::Ty::TInt, tast::Ty::TInt],
                ret_ty: Box::new(ret_ty.clone()),
            };
            tast::Expr::ECall {
                func: Box::new(tast::Expr::EVar {
                    name: LangItemId::Range.source_name().to_string(),
                    ty: func_ty,
                    astptr: None,
                }),
                args: vec![start, end],
                ty: ret_ty,
            }
        }
        hir::Expr::EBinary { op, lhs, rhs } => {
            let lhs = Box::new(build_expr(hir_table, results, lhs));
            let rhs = Box::new(build_expr(hir_table, results, rhs));
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            let resolution = results
                .binary_resolution(expr_id)
                .cloned()
                .unwrap_or(tast::BinaryResolution::Builtin);
            tast::Expr::EBinary {
                op,
                lhs,
                rhs,
                ty,
                resolution,
            }
        }
        hir::Expr::EProj { tuple, index } => {
            let tuple = Box::new(build_expr(hir_table, results, tuple));
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Expr::EProj { tuple, index, ty }
        }
        hir::Expr::EField { expr, field } => {
            let expr = Box::new(build_expr(hir_table, results, expr));
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Expr::EField {
                expr,
                field_name: field.to_ident_name(),
                ty,
                astptr: None,
            }
        }
        hir::Expr::EIndex { base, index } => {
            let base = Box::new(build_expr(hir_table, results, base));
            let index = Box::new(build_expr(hir_table, results, index));
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Expr::EIndex {
                base,
                index,
                ty,
                astptr: hir_table.expr_ptr(expr_id),
            }
        }
        hir::Expr::EBlock { block } => {
            let block = Box::new(build_block(hir_table, results, &block));
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Expr::EBlock { block, ty }
        }
    };
    apply_coercions(results, expr_id, built)
}

fn build_let_stmt(
    hir_table: &hir::HirTable,
    results: &TypeckResults,
    stmt: &hir::LetStmt,
) -> tast::LetStmt {
    let pat = build_pat(hir_table, results, stmt.pat);
    let value = Box::new(build_expr(hir_table, results, stmt.value));
    tast::LetStmt {
        is_mut: stmt.is_mut,
        pat,
        value,
    }
}

fn build_try_expr(
    hir_table: &hir::HirTable,
    results: &TypeckResults,
    expr_id: hir::ExprId,
    inner_expr_id: hir::ExprId,
) -> tast::Expr {
    let Some(try_elab) = results.try_elab(expr_id) else {
        return tast::Expr::EVar {
            name: "<error>".to_string(),
            ty: results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit),
            astptr: None,
        };
    };

    let inner_expr = Box::new(build_expr(hir_table, results, inner_expr_id));
    let inner_ty = results
        .expr_ty(inner_expr_id)
        .cloned()
        .unwrap_or(tast::Ty::TUnit);
    let ok_ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
    let outer_ret_ty = try_elab.outer_ret_ty.clone();
    let value_name = format!("try_value/{}", expr_id.idx);
    let residual_name = format!("try_residual/{}", expr_id.idx);

    let arms = match try_elab.kind {
        TryKind::Result => {
            let Some((inner_name, err_ty)) =
                build_result_parts(&inner_ty, &try_elab.container_name)
            else {
                return tast::Expr::EVar {
                    name: "<error>".to_string(),
                    ty: ok_ty,
                    astptr: None,
                };
            };
            let Some(outer_name) = build_result_name(&outer_ret_ty, &try_elab.container_name)
            else {
                return tast::Expr::EVar {
                    name: "<error>".to_string(),
                    ty: results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit),
                    astptr: None,
                };
            };
            vec![
                tast::Arm {
                    pat: tast::Pat::PConstr {
                        constructor: enum_constructor(&inner_name, "Ok", try_elab.success_index),
                        args: vec![tast::Pat::PVar {
                            name: value_name.clone(),
                            ty: ok_ty.clone(),
                            astptr: None,
                        }],
                        ty: inner_ty.clone(),
                        astptr: None,
                    },
                    guard: None,
                    body: tast::Expr::EVar {
                        name: value_name,
                        ty: ok_ty.clone(),
                        astptr: None,
                    },
                },
                tast::Arm {
                    pat: tast::Pat::PConstr {
                        constructor: enum_constructor(&inner_name, "Err", try_elab.residual_index),
                        args: vec![tast::Pat::PVar {
                            name: residual_name.clone(),
                            ty: err_ty.clone(),
                            astptr: None,
                        }],
                        ty: inner_ty,
                        astptr: None,
                    },
                    guard: None,
                    body: tast::Expr::EReturn {
                        expr: Some(Box::new(tast::Expr::EConstr {
                            constructor: enum_constructor(
                                &outer_name,
                                "Err",
                                try_elab.residual_index,
                            ),
                            args: vec![tast::Expr::EVar {
                                name: residual_name,
                                ty: err_ty,
                                astptr: None,
                            }],
                            ty: outer_ret_ty,
                        })),
                        ty: ok_ty.clone(),
                    },
                },
            ]
        }
        TryKind::Option => {
            let Some(inner_name) = build_option_name(&inner_ty, &try_elab.container_name) else {
                return tast::Expr::EVar {
                    name: "<error>".to_string(),
                    ty: ok_ty,
                    astptr: None,
                };
            };
            let Some(outer_name) = build_option_name(&outer_ret_ty, &try_elab.container_name)
            else {
                return tast::Expr::EVar {
                    name: "<error>".to_string(),
                    ty: results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit),
                    astptr: None,
                };
            };
            vec![
                tast::Arm {
                    pat: tast::Pat::PConstr {
                        constructor: enum_constructor(&inner_name, "Some", try_elab.success_index),
                        args: vec![tast::Pat::PVar {
                            name: value_name.clone(),
                            ty: ok_ty.clone(),
                            astptr: None,
                        }],
                        ty: inner_ty.clone(),
                        astptr: None,
                    },
                    guard: None,
                    body: tast::Expr::EVar {
                        name: value_name,
                        ty: ok_ty.clone(),
                        astptr: None,
                    },
                },
                tast::Arm {
                    pat: tast::Pat::PConstr {
                        constructor: enum_constructor(&inner_name, "None", try_elab.residual_index),
                        args: vec![],
                        ty: inner_ty,
                        astptr: None,
                    },
                    guard: None,
                    body: tast::Expr::EReturn {
                        expr: Some(Box::new(tast::Expr::EConstr {
                            constructor: enum_constructor(
                                &outer_name,
                                "None",
                                try_elab.residual_index,
                            ),
                            args: vec![],
                            ty: outer_ret_ty,
                        })),
                        ty: ok_ty.clone(),
                    },
                },
            ]
        }
    };

    tast::Expr::EMatch {
        expr: inner_expr,
        arms,
        ty: ok_ty,
        astptr: hir_table.expr_ptr(expr_id),
    }
}

fn build_result_parts(ty: &tast::Ty, expected_name: &str) -> Option<(String, tast::Ty)> {
    let tast::Ty::TApp { ty, args } = ty else {
        return None;
    };
    let tast::Ty::TEnum { name } = ty.as_ref() else {
        return None;
    };
    if name == expected_name && args.len() == 2 {
        Some((name.clone(), args[1].clone()))
    } else {
        None
    }
}

fn build_result_name(ty: &tast::Ty, expected_name: &str) -> Option<String> {
    let tast::Ty::TApp { ty, args } = ty else {
        return None;
    };
    let tast::Ty::TEnum { name } = ty.as_ref() else {
        return None;
    };
    if name == expected_name && args.len() == 2 {
        Some(name.clone())
    } else {
        None
    }
}

fn build_option_name(ty: &tast::Ty, expected_name: &str) -> Option<String> {
    let tast::Ty::TApp { ty, args } = ty else {
        return None;
    };
    let tast::Ty::TEnum { name } = ty.as_ref() else {
        return None;
    };
    if name == expected_name && args.len() == 1 {
        Some(name.clone())
    } else {
        None
    }
}

fn enum_constructor(type_name: &str, variant: &str, index: usize) -> Constructor {
    Constructor::Enum(crate::common::EnumConstructor {
        type_name: tast::TastIdent::new(type_name),
        variant: tast::TastIdent::new(variant),
        index,
    })
}

fn build_assign_stmt(
    hir_table: &hir::HirTable,
    results: &TypeckResults,
    stmt: &hir::AssignStmt,
) -> tast::AssignStmt {
    let target = Box::new(build_expr(hir_table, results, stmt.target));
    let value = Box::new(build_expr(hir_table, results, stmt.value));
    tast::AssignStmt { target, value }
}

fn build_stmt(hir_table: &hir::HirTable, results: &TypeckResults, stmt: &hir::Stmt) -> tast::Stmt {
    match stmt {
        hir::Stmt::Let(stmt) => tast::Stmt::Let(build_let_stmt(hir_table, results, stmt)),
        hir::Stmt::Assign(stmt) => tast::Stmt::Assign(build_assign_stmt(hir_table, results, stmt)),
        hir::Stmt::Expr(stmt) => tast::Stmt::Expr(tast::ExprStmt {
            expr: build_expr(hir_table, results, stmt.expr),
        }),
    }
}

fn build_block(
    hir_table: &hir::HirTable,
    results: &TypeckResults,
    block: &hir::Block,
) -> tast::Block {
    let stmts = block
        .stmts
        .iter()
        .map(|stmt| build_stmt(hir_table, results, stmt))
        .collect();
    let tail = block
        .tail
        .map(|tail| Box::new(build_expr(hir_table, results, tail)));
    tast::Block { stmts, tail }
}

fn error_constructor() -> Constructor {
    Constructor::Struct(StructConstructor {
        type_name: tast::TastIdent::new("<error>"),
    })
}

fn build_name_ref_expr(
    _hir_table: &hir::HirTable,
    results: &TypeckResults,
    expr_id: hir::ExprId,
) -> tast::Expr {
    match results.name_ref_elab(expr_id) {
        Some(NameRefElab::Var { name, ty, astptr }) => tast::Expr::EVar {
            name: name.clone(),
            ty: ty.clone(),
            astptr: *astptr,
        },
        Some(NameRefElab::Callable {
            name,
            body,
            ty,
            astptr,
        }) => tast::Expr::ECallable {
            name: name.clone(),
            body: *body,
            ty: ty.clone(),
            astptr: *astptr,
        },
        Some(NameRefElab::TraitMethod {
            trait_ref,
            method_name,
            ty,
            astptr,
        }) => tast::Expr::ETraitMethod {
            trait_ref: trait_ref.clone(),
            method_name: method_name.clone(),
            ty: ty.clone(),
            astptr: *astptr,
        },
        Some(NameRefElab::DynTraitMethod {
            trait_name,
            method_name,
            ty,
            astptr,
        }) => tast::Expr::EDynTraitMethod {
            trait_name: trait_name.clone(),
            method_name: method_name.clone(),
            ty: ty.clone(),
            astptr: *astptr,
        },
        Some(NameRefElab::InherentMethod {
            receiver_ty,
            method_name,
            ty,
            astptr,
        }) => tast::Expr::EInherentMethod {
            receiver_ty: receiver_ty.clone(),
            method_name: method_name.clone(),
            ty: ty.clone(),
            astptr: *astptr,
        },
        None => tast::Expr::EVar {
            name: "<error>".to_string(),
            ty: results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit),
            astptr: None,
        },
    }
}

fn build_callee(
    hir_table: &hir::HirTable,
    results: &TypeckResults,
    callee: &CalleeElab,
) -> tast::Expr {
    match callee {
        CalleeElab::Expr(expr_id) => build_expr(hir_table, results, *expr_id),
        CalleeElab::Var { name, ty, astptr } => tast::Expr::EVar {
            name: name.clone(),
            ty: ty.clone(),
            astptr: *astptr,
        },
        CalleeElab::Callable {
            name,
            body,
            ty,
            astptr,
        } => tast::Expr::ECallable {
            name: name.clone(),
            body: *body,
            ty: ty.clone(),
            astptr: *astptr,
        },
        CalleeElab::TraitMethod {
            trait_ref,
            method_name,
            ty,
            astptr,
        } => tast::Expr::ETraitMethod {
            trait_ref: trait_ref.clone(),
            method_name: method_name.clone(),
            ty: ty.clone(),
            astptr: *astptr,
        },
        CalleeElab::DynTraitMethod {
            trait_name,
            method_name,
            ty,
            astptr,
        } => tast::Expr::EDynTraitMethod {
            trait_name: trait_name.clone(),
            method_name: method_name.clone(),
            ty: ty.clone(),
            astptr: *astptr,
        },
        CalleeElab::InherentMethod {
            receiver_ty,
            method_name,
            ty,
            astptr,
        } => tast::Expr::EInherentMethod {
            receiver_ty: receiver_ty.clone(),
            method_name: method_name.clone(),
            ty: ty.clone(),
            astptr: *astptr,
        },
        CalleeElab::Error { ty, astptr } => tast::Expr::EVar {
            name: "<error>".to_string(),
            ty: ty.clone(),
            astptr: *astptr,
        },
    }
}

fn apply_coercions(
    results: &TypeckResults,
    expr_id: hir::ExprId,
    mut expr: tast::Expr,
) -> tast::Expr {
    for coercion in results.coercions(expr_id) {
        match coercion {
            Coercion::ToDyn {
                trait_name,
                for_ty,
                ty,
                astptr,
            } => {
                expr = tast::Expr::EToDyn {
                    trait_name: trait_name.clone(),
                    for_ty: for_ty.clone(),
                    expr: Box::new(expr),
                    ty: ty.clone(),
                    astptr: *astptr,
                };
            }
        }
    }
    expr
}

fn build_pat(hir_table: &hir::HirTable, results: &TypeckResults, pat_id: hir::PatId) -> tast::Pat {
    let astptr = hir_table.pat_ptr(pat_id);
    match hir_table.pat(pat_id).clone() {
        hir::Pat::PVar { name, astptr } => {
            let ty = results
                .local_ty(name)
                .cloned()
                .or_else(|| results.pat_ty(pat_id).cloned())
                .unwrap_or(tast::Ty::TUnit);
            tast::Pat::PVar {
                name: hir_table.local_ident_name(name),
                ty,
                astptr: Some(astptr),
            }
        }
        hir::Pat::PUnit => tast::Pat::PPrim {
            value: Prim::unit(),
            ty: tast::Ty::TUnit,
            astptr,
        },
        hir::Pat::PBool { value } => tast::Pat::PPrim {
            value: Prim::boolean(value),
            ty: tast::Ty::TBool,
            astptr,
        },
        hir::Pat::PInt { value } => {
            let resolved_ty = results.pat_ty(pat_id).cloned();
            match resolved_ty {
                Some(tast::Ty::TInt) => tast::Pat::PPrim {
                    value: Prim::Int {
                        value: parse_signed(&value).unwrap_or(0),
                    },
                    ty: tast::Ty::TInt,
                    astptr,
                },
                Some(tast::Ty::TInt8) => tast::Pat::PPrim {
                    value: Prim::Int8 {
                        value: parse_signed(&value).unwrap_or(0),
                    },
                    ty: tast::Ty::TInt8,
                    astptr,
                },
                Some(tast::Ty::TInt16) => tast::Pat::PPrim {
                    value: Prim::Int16 {
                        value: parse_signed(&value).unwrap_or(0),
                    },
                    ty: tast::Ty::TInt16,
                    astptr,
                },
                Some(tast::Ty::TInt32) => tast::Pat::PPrim {
                    value: Prim::Int32 {
                        value: parse_signed(&value).unwrap_or(0),
                    },
                    ty: tast::Ty::TInt32,
                    astptr,
                },
                Some(tast::Ty::TInt64) => tast::Pat::PPrim {
                    value: Prim::Int64 {
                        value: parse_signed(&value).unwrap_or(0),
                    },
                    ty: tast::Ty::TInt64,
                    astptr,
                },
                Some(tast::Ty::TUint) => tast::Pat::PPrim {
                    value: Prim::UInt {
                        value: parse_unsigned(&value).unwrap_or(0),
                    },
                    ty: tast::Ty::TUint,
                    astptr,
                },
                Some(tast::Ty::TUint8) => tast::Pat::PPrim {
                    value: Prim::UInt8 {
                        value: parse_unsigned(&value).unwrap_or(0),
                    },
                    ty: tast::Ty::TUint8,
                    astptr,
                },
                Some(tast::Ty::TUint16) => tast::Pat::PPrim {
                    value: Prim::UInt16 {
                        value: parse_unsigned(&value).unwrap_or(0),
                    },
                    ty: tast::Ty::TUint16,
                    astptr,
                },
                Some(tast::Ty::TUint32) => tast::Pat::PPrim {
                    value: Prim::UInt32 {
                        value: parse_unsigned(&value).unwrap_or(0),
                    },
                    ty: tast::Ty::TUint32,
                    astptr,
                },
                Some(tast::Ty::TUint64) => tast::Pat::PPrim {
                    value: Prim::UInt64 {
                        value: parse_unsigned(&value).unwrap_or(0),
                    },
                    ty: tast::Ty::TUint64,
                    astptr,
                },
                Some(tast::Ty::TFloat32) => tast::Pat::PPrim {
                    value: Prim::Float32 {
                        value: value.parse::<f64>().unwrap_or(0.0) as f32,
                    },
                    ty: tast::Ty::TFloat32,
                    astptr,
                },
                Some(tast::Ty::TFloat64) => tast::Pat::PPrim {
                    value: Prim::Float64 {
                        value: value.parse::<f64>().unwrap_or(0.0),
                    },
                    ty: tast::Ty::TFloat64,
                    astptr,
                },
                _ => tast::Pat::PPrim {
                    value: Prim::Int {
                        value: parse_signed(&value).unwrap_or(0),
                    },
                    ty: results.pat_ty(pat_id).cloned().unwrap_or(tast::Ty::TInt),
                    astptr,
                },
            }
        }
        hir::Pat::PFloat { value } => {
            let resolved_ty = results.pat_ty(pat_id).cloned();
            match resolved_ty {
                Some(tast::Ty::TInt) => tast::Pat::PPrim {
                    value: Prim::Int {
                        value: value.parse::<f64>().unwrap_or(0.0) as i64,
                    },
                    ty: tast::Ty::TInt,
                    astptr,
                },
                Some(tast::Ty::TInt8) => tast::Pat::PPrim {
                    value: Prim::Int8 {
                        value: value.parse::<f64>().unwrap_or(0.0) as i8,
                    },
                    ty: tast::Ty::TInt8,
                    astptr,
                },
                Some(tast::Ty::TInt16) => tast::Pat::PPrim {
                    value: Prim::Int16 {
                        value: value.parse::<f64>().unwrap_or(0.0) as i16,
                    },
                    ty: tast::Ty::TInt16,
                    astptr,
                },
                Some(tast::Ty::TInt32) => tast::Pat::PPrim {
                    value: Prim::Int32 {
                        value: value.parse::<f64>().unwrap_or(0.0) as i32,
                    },
                    ty: tast::Ty::TInt32,
                    astptr,
                },
                Some(tast::Ty::TInt64) => tast::Pat::PPrim {
                    value: Prim::Int64 {
                        value: value.parse::<f64>().unwrap_or(0.0) as i64,
                    },
                    ty: tast::Ty::TInt64,
                    astptr,
                },
                Some(tast::Ty::TUint) => tast::Pat::PPrim {
                    value: Prim::UInt {
                        value: value.parse::<f64>().unwrap_or(0.0) as u64,
                    },
                    ty: tast::Ty::TUint,
                    astptr,
                },
                Some(tast::Ty::TUint8) => tast::Pat::PPrim {
                    value: Prim::UInt8 {
                        value: value.parse::<f64>().unwrap_or(0.0) as u8,
                    },
                    ty: tast::Ty::TUint8,
                    astptr,
                },
                Some(tast::Ty::TUint16) => tast::Pat::PPrim {
                    value: Prim::UInt16 {
                        value: value.parse::<f64>().unwrap_or(0.0) as u16,
                    },
                    ty: tast::Ty::TUint16,
                    astptr,
                },
                Some(tast::Ty::TUint32) => tast::Pat::PPrim {
                    value: Prim::UInt32 {
                        value: value.parse::<f64>().unwrap_or(0.0) as u32,
                    },
                    ty: tast::Ty::TUint32,
                    astptr,
                },
                Some(tast::Ty::TUint64) => tast::Pat::PPrim {
                    value: Prim::UInt64 {
                        value: value.parse::<f64>().unwrap_or(0.0) as u64,
                    },
                    ty: tast::Ty::TUint64,
                    astptr,
                },
                Some(tast::Ty::TFloat32) => tast::Pat::PPrim {
                    value: Prim::Float32 {
                        value: value.parse::<f64>().unwrap_or(0.0) as f32,
                    },
                    ty: tast::Ty::TFloat32,
                    astptr,
                },
                _ => tast::Pat::PPrim {
                    value: Prim::Float64 {
                        value: value.parse::<f64>().unwrap_or(0.0),
                    },
                    ty: results
                        .pat_ty(pat_id)
                        .cloned()
                        .unwrap_or(tast::Ty::TFloat64),
                    astptr,
                },
            }
        }
        hir::Pat::PString { value } => tast::Pat::PPrim {
            value: Prim::string(value),
            ty: tast::Ty::TString,
            astptr,
        },
        hir::Pat::PChar { value } => tast::Pat::PPrim {
            value: Prim::Char {
                value: parse_char_literal(&value).unwrap_or('\0'),
            },
            ty: tast::Ty::TChar,
            astptr,
        },
        hir::Pat::PConstr { args, .. } => {
            let constructor = results
                .constructor_for_pat(pat_id)
                .cloned()
                .unwrap_or_else(error_constructor);
            let args = args
                .iter()
                .copied()
                .map(|p| build_pat(hir_table, results, p))
                .collect::<Vec<_>>();
            let ty = results.pat_ty(pat_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Pat::PConstr {
                constructor,
                args,
                ty,
                astptr,
            }
        }
        hir::Pat::PStruct { .. } => {
            let Some(elab) = results.struct_pat_elab(pat_id) else {
                return tast::Pat::PWild {
                    ty: results.pat_ty(pat_id).cloned().unwrap_or(tast::Ty::TUnit),
                    astptr,
                };
            };
            let args = elab
                .args
                .iter()
                .map(|arg| match arg {
                    StructPatArgElab::Pat(p) => build_pat(hir_table, results, *p),
                    StructPatArgElab::MissingWild { expected_ty } => tast::Pat::PWild {
                        ty: expected_ty.clone(),
                        astptr: None,
                    },
                })
                .collect::<Vec<_>>();
            let ty = results.pat_ty(pat_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Pat::PConstr {
                constructor: elab.constructor.clone(),
                args,
                ty,
                astptr,
            }
        }
        hir::Pat::PTuple { pats } => {
            let items = pats
                .iter()
                .copied()
                .map(|p| build_pat(hir_table, results, p))
                .collect::<Vec<_>>();
            let ty = results.pat_ty(pat_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Pat::PTuple { items, ty, astptr }
        }
        hir::Pat::PArray {
            prefix,
            rest,
            suffix,
        } => {
            let prefix = prefix
                .iter()
                .map(|pat| build_pat(hir_table, results, *pat))
                .collect::<Vec<_>>();
            let suffix = suffix
                .iter()
                .map(|pat| build_pat(hir_table, results, *pat))
                .collect::<Vec<_>>();
            let ty = results.pat_ty(pat_id).cloned().unwrap_or(tast::Ty::TUnit);
            let rest = rest.map(|rest| {
                let rest_ty = rest
                    .binding
                    .and_then(|name| results.local_ty(name).cloned())
                    .unwrap_or_else(|| match &ty {
                        tast::Ty::TArray { len, elem } => tast::Ty::TArray {
                            len: len.saturating_sub(prefix.len() + suffix.len()),
                            elem: elem.clone(),
                        },
                        tast::Ty::TVec { elem } | tast::Ty::TSlice { elem } => {
                            tast::Ty::TSlice { elem: elem.clone() }
                        }
                        _ => tast::Ty::TUnit,
                    });
                tast::ArrayPatRest {
                    binding: rest.binding.map(|name| hir_table.local_ident_name(name)),
                    ty: rest_ty,
                    astptr: Some(rest.astptr),
                }
            });
            tast::Pat::PArray {
                prefix,
                rest,
                suffix,
                ty,
                astptr,
            }
        }
        hir::Pat::PAlias { name, pat, .. } => {
            let ty = results.pat_ty(pat_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Pat::PAlias {
                name: hir_table.local_ident_name(name),
                pat: Box::new(build_pat(hir_table, results, pat)),
                ty,
                astptr,
            }
        }
        hir::Pat::POr { pats } => {
            let ty = results.pat_ty(pat_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Pat::POr {
                pats: pats
                    .iter()
                    .map(|pat| build_pat(hir_table, results, *pat))
                    .collect(),
                ty,
                astptr,
            }
        }
        hir::Pat::PRange {
            start,
            end,
            inclusive,
        } => {
            let start = match build_pat(hir_table, results, start) {
                tast::Pat::PPrim { value, .. } => value,
                _ => Prim::unit(),
            };
            let end = match build_pat(hir_table, results, end) {
                tast::Pat::PPrim { value, .. } => value,
                _ => Prim::unit(),
            };
            tast::Pat::PRange {
                start,
                end,
                inclusive,
                ty: results.pat_ty(pat_id).cloned().unwrap_or(tast::Ty::TUnit),
                astptr,
            }
        }
        hir::Pat::PWild => tast::Pat::PWild {
            ty: results.pat_ty(pat_id).cloned().unwrap_or(tast::Ty::TUnit),
            astptr,
        },
    }
}

fn parse_signed<T>(s: &str) -> Option<T>
where
    T: std::str::FromStr<Err = std::num::ParseIntError>,
{
    s.parse().ok()
}

fn parse_unsigned<T>(s: &str) -> Option<T>
where
    T: std::str::FromStr<Err = std::num::ParseIntError>,
{
    if s.starts_with('-') {
        return None;
    }
    s.parse().ok()
}

fn parse_char_literal(s: &str) -> Option<char> {
    if s.is_empty() {
        return None;
    }
    if let Some(rest) = s.strip_prefix('\\') {
        let mut chars = rest.chars();
        let tag = chars.next()?;
        let out = match tag {
            '\'' => Some('\''),
            '"' => Some('"'),
            '\\' => Some('\\'),
            '/' => Some('/'),
            'b' => Some('\u{0008}'),
            'f' => Some('\u{000C}'),
            'n' => Some('\n'),
            'r' => Some('\r'),
            't' => Some('\t'),
            'u' => {
                let hex: String = chars.by_ref().take(4).collect();
                if hex.chars().count() != 4 || chars.next().is_some() {
                    None
                } else if let Ok(code) = u32::from_str_radix(&hex, 16) {
                    char::from_u32(code)
                } else {
                    None
                }
            }
            _ => None,
        }?;
        if chars.next().is_some() {
            return None;
        }
        return Some(out);
    }

    let mut iter = s.chars();
    let ch = iter.next()?;
    if iter.next().is_some() {
        return None;
    }
    Some(ch)
}
