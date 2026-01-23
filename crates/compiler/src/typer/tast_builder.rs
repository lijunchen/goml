use crate::common::{Constructor, Prim, StructConstructor};
use crate::env::PackageTypeEnv;
use crate::hir;
use crate::tast;
use crate::typer::results::{
    CalleeElab, Coercion, NameRefElab, StructLitArgElab, StructPatArgElab, TypeckResults,
};
use crate::typer::toplevel::go_symbol_name;

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
            ))),
            hir::Def::ExternGo(ext) => {
                toplevels.push(tast::Item::ExternGo(build_extern_go(genv, &ext)))
            }
            hir::Def::ExternType(ext) => toplevels.push(tast::Item::ExternType(tast::ExternType {
                goml_name: ext.goml_name.to_ident_name(),
            })),
            hir::Def::EnumDef(..)
            | hir::Def::StructDef(..)
            | hir::Def::TraitDef(..)
            | hir::Def::ExternBuiltin(..) => {}
        }
    }
    tast::File { toplevels }
}

fn build_extern_go(genv: &PackageTypeEnv, ext: &hir::ExternGo) -> tast::ExternGo {
    let params = ext
        .params
        .iter()
        .map(|(name, ty)| (name.to_ident_name(), tast::Ty::from_hir(genv, ty, &[])))
        .collect::<Vec<_>>();
    let ret_ty = ext
        .ret_ty
        .as_ref()
        .map(|ty| tast::Ty::from_hir(genv, ty, &[]))
        .unwrap_or(tast::Ty::TUnit);
    tast::ExternGo {
        goml_name: ext.goml_name.to_ident_name(),
        go_name: go_symbol_name(&ext.go_symbol),
        package_path: ext.package_path.clone(),
        params,
        ret_ty,
    }
}

fn build_impl_block(
    genv: &PackageTypeEnv,
    hir_table: &hir::HirTable,
    results: &TypeckResults,
    impl_block: &hir::ImplBlock,
) -> tast::ImplBlock {
    let impl_tparams = tparams_for(&impl_block.generics);
    let for_ty = tast::Ty::from_hir(genv, &impl_block.for_type, &impl_tparams);
    let trait_name = impl_block.trait_name.as_ref().map(|t| {
        let name = t.to_ident_name();
        super::util::resolve_trait_name(genv, &name)
            .map(|(resolved, _)| tast::TastIdent(resolved))
            .unwrap_or_else(|| tast::TastIdent(name))
    });

    let mut methods = Vec::new();
    for method_id in impl_block.methods.iter().copied() {
        let hir::Def::Fn(func) = hir_table.def(method_id).clone() else {
            continue;
        };
        let mut all_generics = impl_block.generics.clone();
        all_generics.extend(func.generics.clone());
        methods.push(build_fn(
            genv,
            hir_table,
            results,
            &func,
            &tparams_for(&all_generics),
            Some(&for_ty),
        ));
    }

    tast::ImplBlock {
        generics: impl_block
            .generics
            .iter()
            .map(|g| g.to_ident_name())
            .collect(),
        trait_name,
        for_type: for_ty,
        methods,
    }
}

fn build_fn(
    genv: &PackageTypeEnv,
    hir_table: &hir::HirTable,
    results: &TypeckResults,
    func: &hir::Fn,
    tparams: &[tast::TastIdent],
    self_ty: Option<&tast::Ty>,
) -> tast::Fn {
    let params = func
        .params
        .iter()
        .map(|(name, ty)| {
            let name_str = hir_table.local_ident_name(*name);
            let mut ty = tast::Ty::from_hir(genv, ty, tparams);
            if let Some(self_ty) = self_ty {
                ty = instantiate_self_ty(&ty, self_ty);
            }
            (name_str, ty)
        })
        .collect::<Vec<_>>();

    let mut ret_ty = func
        .ret_ty
        .as_ref()
        .map(|ty| tast::Ty::from_hir(genv, ty, tparams))
        .unwrap_or(tast::Ty::TUnit);
    if let Some(self_ty) = self_ty {
        ret_ty = instantiate_self_ty(&ret_ty, self_ty);
    }

    tast::Fn {
        name: func.name.clone(),
        params,
        ret_ty,
        body: build_expr(hir_table, results, func.body),
    }
}

fn tparams_for(generics: &[hir::HirIdent]) -> Vec<tast::TastIdent> {
    generics
        .iter()
        .map(|g| tast::TastIdent(g.to_ident_name()))
        .collect()
}

fn instantiate_self_ty(ty: &tast::Ty, self_ty: &tast::Ty) -> tast::Ty {
    match ty {
        tast::Ty::TVar(var) => tast::Ty::TVar(*var),
        tast::Ty::TUnit => tast::Ty::TUnit,
        tast::Ty::TBool => tast::Ty::TBool,
        tast::Ty::TInt8 => tast::Ty::TInt8,
        tast::Ty::TInt16 => tast::Ty::TInt16,
        tast::Ty::TInt32 => tast::Ty::TInt32,
        tast::Ty::TInt64 => tast::Ty::TInt64,
        tast::Ty::TUint8 => tast::Ty::TUint8,
        tast::Ty::TUint16 => tast::Ty::TUint16,
        tast::Ty::TUint32 => tast::Ty::TUint32,
        tast::Ty::TUint64 => tast::Ty::TUint64,
        tast::Ty::TFloat32 => tast::Ty::TFloat32,
        tast::Ty::TFloat64 => tast::Ty::TFloat64,
        tast::Ty::TString => tast::Ty::TString,
        tast::Ty::TTuple { typs } => tast::Ty::TTuple {
            typs: typs
                .iter()
                .map(|ty| instantiate_self_ty(ty, self_ty))
                .collect(),
        },
        tast::Ty::TEnum { name } => tast::Ty::TEnum { name: name.clone() },
        tast::Ty::TStruct { name } => {
            if name == "Self" {
                self_ty.clone()
            } else {
                tast::Ty::TStruct { name: name.clone() }
            }
        }
        tast::Ty::TDyn { trait_name } => tast::Ty::TDyn {
            trait_name: trait_name.clone(),
        },
        tast::Ty::TApp { ty, args } => tast::Ty::TApp {
            ty: Box::new(instantiate_self_ty(ty, self_ty)),
            args: args
                .iter()
                .map(|ty| instantiate_self_ty(ty, self_ty))
                .collect(),
        },
        tast::Ty::TArray { len, elem } => tast::Ty::TArray {
            len: *len,
            elem: Box::new(instantiate_self_ty(elem, self_ty)),
        },
        tast::Ty::TVec { elem } => tast::Ty::TVec {
            elem: Box::new(instantiate_self_ty(elem, self_ty)),
        },
        tast::Ty::TRef { elem } => tast::Ty::TRef {
            elem: Box::new(instantiate_self_ty(elem, self_ty)),
        },
        tast::Ty::TParam { name } => tast::Ty::TParam { name: name.clone() },
        tast::Ty::TFunc { params, ret_ty } => tast::Ty::TFunc {
            params: params
                .iter()
                .map(|param| instantiate_self_ty(param, self_ty))
                .collect(),
            ret_ty: Box::new(instantiate_self_ty(ret_ty, self_ty)),
        },
    }
}

fn build_expr(
    hir_table: &hir::HirTable,
    results: &TypeckResults,
    expr_id: hir::ExprId,
) -> tast::Expr {
    let built = match hir_table.expr(expr_id).clone() {
        hir::Expr::ENameRef { .. } => build_name_ref_expr(hir_table, results, expr_id),
        hir::Expr::EUnit => tast::Expr::EPrim {
            value: Prim::unit(),
            ty: tast::Ty::TUnit,
        },
        hir::Expr::EBool { value } => tast::Expr::EPrim {
            value: Prim::boolean(value),
            ty: tast::Ty::TBool,
        },
        hir::Expr::EInt { value } => tast::Expr::EPrim {
            value: Prim::Int32 {
                value: parse_signed(&value).unwrap_or(0),
            },
            ty: tast::Ty::TInt32,
        },
        hir::Expr::EInt8 { value } => tast::Expr::EPrim {
            value: Prim::Int8 {
                value: parse_signed(&value).unwrap_or(0),
            },
            ty: tast::Ty::TInt8,
        },
        hir::Expr::EInt16 { value } => tast::Expr::EPrim {
            value: Prim::Int16 {
                value: parse_signed(&value).unwrap_or(0),
            },
            ty: tast::Ty::TInt16,
        },
        hir::Expr::EInt32 { value } => tast::Expr::EPrim {
            value: Prim::Int32 {
                value: parse_signed(&value).unwrap_or(0),
            },
            ty: tast::Ty::TInt32,
        },
        hir::Expr::EInt64 { value } => tast::Expr::EPrim {
            value: Prim::Int64 {
                value: parse_signed(&value).unwrap_or(0),
            },
            ty: tast::Ty::TInt64,
        },
        hir::Expr::EUInt8 { value } => tast::Expr::EPrim {
            value: Prim::UInt8 {
                value: parse_unsigned(&value).unwrap_or(0),
            },
            ty: tast::Ty::TUint8,
        },
        hir::Expr::EUInt16 { value } => tast::Expr::EPrim {
            value: Prim::UInt16 {
                value: parse_unsigned(&value).unwrap_or(0),
            },
            ty: tast::Ty::TUint16,
        },
        hir::Expr::EUInt32 { value } => tast::Expr::EPrim {
            value: Prim::UInt32 {
                value: parse_unsigned(&value).unwrap_or(0),
            },
            ty: tast::Ty::TUint32,
        },
        hir::Expr::EUInt64 { value } => tast::Expr::EPrim {
            value: Prim::UInt64 {
                value: parse_unsigned(&value).unwrap_or(0),
            },
            ty: tast::Ty::TUint64,
        },
        hir::Expr::EFloat { value } => tast::Expr::EPrim {
            value: Prim::Float64 { value },
            ty: tast::Ty::TFloat64,
        },
        hir::Expr::EFloat32 { value } => tast::Expr::EPrim {
            value: Prim::Float32 {
                value: value.parse::<f32>().unwrap_or(0.0),
            },
            ty: tast::Ty::TFloat32,
        },
        hir::Expr::EFloat64 { value } => tast::Expr::EPrim {
            value: Prim::Float64 {
                value: value.parse::<f64>().unwrap_or(0.0),
            },
            ty: tast::Ty::TFloat64,
        },
        hir::Expr::EString { value } => tast::Expr::EPrim {
            value: Prim::string(value),
            ty: tast::Ty::TString,
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
        hir::Expr::ELet { pat, value, .. } => {
            let pat = build_pat(hir_table, results, pat);
            let value = Box::new(build_expr(hir_table, results, value));
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Expr::ELet { pat, value, ty }
        }
        hir::Expr::EMatch { expr, arms } => {
            let scrutinee = Box::new(build_expr(hir_table, results, expr));
            let arms = arms
                .iter()
                .map(|arm| tast::Arm {
                    pat: build_pat(hir_table, results, arm.pat),
                    body: build_expr(hir_table, results, arm.body),
                })
                .collect::<Vec<_>>();
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Expr::EMatch {
                expr: scrutinee,
                arms,
                ty,
                astptr: None,
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
            let expr = Box::new(build_expr(hir_table, results, expr));
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
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
        hir::Expr::EBlock { exprs } => {
            let exprs = exprs
                .iter()
                .copied()
                .map(|e| build_expr(hir_table, results, e))
                .collect::<Vec<_>>();
            let ty = results.expr_ty(expr_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Expr::EBlock { exprs, ty }
        }
    };
    apply_coercions(results, expr_id, built)
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
        Some(NameRefElab::TraitMethod {
            trait_name,
            method_name,
            ty,
            astptr,
        }) => tast::Expr::ETraitMethod {
            trait_name: trait_name.clone(),
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
        CalleeElab::TraitMethod {
            trait_name,
            method_name,
            ty,
            astptr,
        } => tast::Expr::ETraitMethod {
            trait_name: trait_name.clone(),
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
        },
        hir::Pat::PBool { value } => tast::Pat::PPrim {
            value: Prim::boolean(value),
            ty: tast::Ty::TBool,
        },
        hir::Pat::PInt { value } => tast::Pat::PPrim {
            value: Prim::Int32 {
                value: parse_signed(&value).unwrap_or(0),
            },
            ty: results.pat_ty(pat_id).cloned().unwrap_or(tast::Ty::TInt32),
        },
        hir::Pat::PInt8 { value } => tast::Pat::PPrim {
            value: Prim::Int8 {
                value: parse_signed(&value).unwrap_or(0),
            },
            ty: results.pat_ty(pat_id).cloned().unwrap_or(tast::Ty::TInt8),
        },
        hir::Pat::PInt16 { value } => tast::Pat::PPrim {
            value: Prim::Int16 {
                value: parse_signed(&value).unwrap_or(0),
            },
            ty: results.pat_ty(pat_id).cloned().unwrap_or(tast::Ty::TInt16),
        },
        hir::Pat::PInt32 { value } => tast::Pat::PPrim {
            value: Prim::Int32 {
                value: parse_signed(&value).unwrap_or(0),
            },
            ty: results.pat_ty(pat_id).cloned().unwrap_or(tast::Ty::TInt32),
        },
        hir::Pat::PInt64 { value } => tast::Pat::PPrim {
            value: Prim::Int64 {
                value: parse_signed(&value).unwrap_or(0),
            },
            ty: results.pat_ty(pat_id).cloned().unwrap_or(tast::Ty::TInt64),
        },
        hir::Pat::PUInt8 { value } => tast::Pat::PPrim {
            value: Prim::UInt8 {
                value: parse_unsigned(&value).unwrap_or(0),
            },
            ty: results.pat_ty(pat_id).cloned().unwrap_or(tast::Ty::TUint8),
        },
        hir::Pat::PUInt16 { value } => tast::Pat::PPrim {
            value: Prim::UInt16 {
                value: parse_unsigned(&value).unwrap_or(0),
            },
            ty: results.pat_ty(pat_id).cloned().unwrap_or(tast::Ty::TUint16),
        },
        hir::Pat::PUInt32 { value } => tast::Pat::PPrim {
            value: Prim::UInt32 {
                value: parse_unsigned(&value).unwrap_or(0),
            },
            ty: results.pat_ty(pat_id).cloned().unwrap_or(tast::Ty::TUint32),
        },
        hir::Pat::PUInt64 { value } => tast::Pat::PPrim {
            value: Prim::UInt64 {
                value: parse_unsigned(&value).unwrap_or(0),
            },
            ty: results.pat_ty(pat_id).cloned().unwrap_or(tast::Ty::TUint64),
        },
        hir::Pat::PString { value } => tast::Pat::PPrim {
            value: Prim::string(value),
            ty: tast::Ty::TString,
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
            }
        }
        hir::Pat::PStruct { .. } => {
            let Some(elab) = results.struct_pat_elab(pat_id) else {
                return tast::Pat::PWild {
                    ty: results.pat_ty(pat_id).cloned().unwrap_or(tast::Ty::TUnit),
                };
            };
            let args = elab
                .args
                .iter()
                .map(|arg| match arg {
                    StructPatArgElab::Pat(p) => build_pat(hir_table, results, *p),
                    StructPatArgElab::MissingWild { expected_ty } => tast::Pat::PWild {
                        ty: expected_ty.clone(),
                    },
                })
                .collect::<Vec<_>>();
            let ty = results.pat_ty(pat_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Pat::PConstr {
                constructor: elab.constructor.clone(),
                args,
                ty,
            }
        }
        hir::Pat::PTuple { pats } => {
            let items = pats
                .iter()
                .copied()
                .map(|p| build_pat(hir_table, results, p))
                .collect::<Vec<_>>();
            let ty = results.pat_ty(pat_id).cloned().unwrap_or(tast::Ty::TUnit);
            tast::Pat::PTuple { items, ty }
        }
        hir::Pat::PWild => tast::Pat::PWild {
            ty: results.pat_ty(pat_id).cloned().unwrap_or(tast::Ty::TUnit),
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
