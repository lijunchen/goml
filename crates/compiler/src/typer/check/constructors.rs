use std::collections::HashMap;

use super::*;
use crate::typer::util;

pub(super) struct ConstructorRequest<'a> {
    pub(super) expr_id: hir::ExprId,
    pub(super) constructor_ref: &'a hir::ConstructorRef,
    pub(super) args: &'a [hir::ExprId],
    pub(super) hint_ret_ty: Option<&'a tast::Ty>,
}

pub(super) struct StructLiteralRequest<'a> {
    pub(super) expr_id: hir::ExprId,
    pub(super) name: &'a hir::QualifiedPath,
    pub(super) fields: &'a [(hir::HirIdent, hir::ExprId)],
    pub(super) hint_ret_ty: Option<&'a tast::Ty>,
}

impl Typer {
    pub(super) fn constructor_path_from_id(
        &self,
        diagnostics: &mut Diagnostics,
        ctor_id: &hir::ConstructorId,
    ) -> hir::Path {
        match ctor_id {
            hir::ConstructorId::EnumVariant {
                enum_def,
                variant_idx,
            } => {
                if let hir::Def::EnumDef(enum_def_data) = self.hir_table.def(*enum_def) {
                    let Some(variant_idx) = usize::try_from(*variant_idx).ok() else {
                        util::push_ice(
                            diagnostics,
                            format!("invalid enum variant index {}", variant_idx),
                        );
                        return hir::Path::from_ident("<error>".to_string());
                    };
                    let Some((variant_ident, _)) = enum_def_data.variants.get(variant_idx) else {
                        util::push_ice(
                            diagnostics,
                            format!(
                                "Enum {} variant index {} out of bounds",
                                self.hir_table.def_path(*enum_def).display(),
                                variant_idx
                            ),
                        );
                        return hir::Path::from_ident("<error>".to_string());
                    };
                    let variant_name = variant_ident.to_ident_name();
                    let mut segments = self.hir_table.def_path(*enum_def).segments.clone();
                    segments.push(hir::PathSegment::new(variant_name));
                    hir::Path::new(segments)
                } else {
                    util::push_ice(diagnostics, "Constructor points to non-enum DefId");
                    hir::Path::from_ident("<error>".to_string())
                }
            }
        }
    }

    pub(super) fn infer_constructor_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        request: ConstructorRequest<'_>,
    ) -> tast::Expr {
        let ConstructorRequest {
            expr_id,
            constructor_ref,
            args,
            hint_ret_ty,
        } = request;
        let constructor_path = match constructor_ref {
            hir::ConstructorRef::Resolved(ctor_id) => {
                self.constructor_path_from_id(diagnostics, ctor_id)
            }
            hir::ConstructorRef::Unresolved(path) => path.clone(),
            hir::ConstructorRef::Ambiguous { path, .. } => {
                util::push_error_with_range(
                    diagnostics,
                    format!("Ambiguous constructor {}", path.display()),
                    self.expr_range(expr_id),
                );
                return self.error_expr(None);
            }
        };

        let Some(variant_ident) = constructor_path.last_ident() else {
            util::push_ice(diagnostics, "Constructor path missing final segment");
            return self.error_expr(None);
        };
        let namespace = constructor_path.namespace_segments();
        let (enum_ident, enum_env) = if namespace.is_empty() {
            (None, genv.current())
        } else {
            let name = namespace
                .iter()
                .map(|seg| seg.seg().clone())
                .collect::<Vec<_>>()
                .join("::");
            let (resolved, env) = util::resolve_type_name(genv, &name);
            (Some(tast::TastIdent(resolved)), env)
        };
        let variant_name = tast::TastIdent(variant_ident.clone());
        let ctor = enum_env.lookup_constructor_with_namespace(enum_ident.as_ref(), &variant_name);
        let Some((constructor, constr_ty)) = ctor else {
            util::push_error_with_range(
                diagnostics,
                format!(
                    "Constructor {} not found in environment",
                    constructor_path.display()
                ),
                self.expr_range(expr_id),
            );
            return self.error_expr(None);
        };

        let expected_arity = match &constructor {
            common::Constructor::Enum(enum_constructor) => {
                let def = enum_env.enums().get(&enum_constructor.type_name);
                let Some(def) = def else {
                    util::push_ice(
                        diagnostics,
                        format!(
                            "Enum {} not found when checking constructor {}",
                            enum_constructor.type_name.0,
                            constructor.name().0
                        ),
                    );
                    return self.error_expr(None);
                };
                let variant = def.variants.get(enum_constructor.index);
                let Some((_, tys)) = variant else {
                    util::push_ice(
                        diagnostics,
                        format!(
                            "Enum {} variant index {} out of bounds",
                            enum_constructor.type_name.0, enum_constructor.index
                        ),
                    );
                    return self.error_expr(None);
                };
                tys.len()
            }
            common::Constructor::Struct(struct_constructor) => {
                let def = enum_env.structs().get(&struct_constructor.type_name);
                let Some(def) = def else {
                    util::push_ice(
                        diagnostics,
                        format!(
                            "Struct {} not found when checking constructor {}",
                            struct_constructor.type_name.0,
                            constructor.name().0
                        ),
                    );
                    return self.error_expr(None);
                };
                def.fields.len()
            }
        };

        let inst_constr_ty = self.inst_ty(&constr_ty);
        let param_tys = match &inst_constr_ty {
            tast::Ty::TFunc { params, .. } => params.clone(),
            _ => Vec::new(),
        };

        let ret_ty = match &inst_constr_ty {
            tast::Ty::TFunc { ret_ty, .. } => *ret_ty.clone(),
            _ => inst_constr_ty.clone(),
        };

        if expected_arity != args.len() {
            if args.is_empty() && expected_arity > 0 {
                if let Some(hint) = hint_ret_ty {
                    self.unify(diagnostics, &inst_constr_ty, hint, self.expr_range(expr_id));
                }

                self.results
                    .record_constructor_expr(expr_id, constructor.clone());

                let params = param_tys
                    .iter()
                    .enumerate()
                    .map(|(idx, ty)| tast::ClosureParam {
                        name: format!("ctor_arg_{idx}"),
                        ty: ty.clone(),
                        astptr: None,
                    })
                    .collect::<Vec<_>>();
                let args_tast = params
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
                        args: args_tast,
                        ty: ret_ty,
                    }),
                    ty: inst_constr_ty,
                    captures: Vec::new(),
                };
            }

            util::push_error_with_range(
                diagnostics,
                format!(
                    "Constructor {} expects {} arguments, but got {}",
                    constructor.name().0,
                    expected_arity,
                    args.len()
                ),
                self.expr_range(expr_id),
            );
            return self.error_expr(None);
        }

        if let Some(hint) = hint_ret_ty {
            self.unify(diagnostics, &ret_ty, hint, self.expr_range(expr_id));
        }

        let mut args_tast = Vec::new();
        if param_tys.is_empty() {
            for arg in args.iter() {
                args_tast.push(self.infer_expr(genv, local_env, diagnostics, *arg));
            }
        } else {
            for (arg, param_ty) in args.iter().zip(param_tys.iter()) {
                let expected_ty = self.norm(param_ty);
                let arg_tast = self.check_expr(genv, local_env, diagnostics, *arg, &expected_ty);
                args_tast.push(arg_tast);
            }
        }

        if !args_tast.is_empty() {
            let actual_params: Vec<tast::Ty> = if param_tys.is_empty() {
                args_tast.iter().map(|arg| arg.get_ty()).collect()
            } else {
                param_tys
            };
            let actual_ty = tast::Ty::TFunc {
                params: actual_params,
                ret_ty: Box::new(ret_ty.clone()),
            };
            self.equate(
                diagnostics,
                &inst_constr_ty,
                &actual_ty,
                self.expr_range(expr_id),
            );
        } else {
            self.equate(
                diagnostics,
                &inst_constr_ty,
                &ret_ty,
                self.expr_range(expr_id),
            );
        }

        self.results
            .record_constructor_expr(expr_id, constructor.clone());

        tast::Expr::EConstr {
            constructor,
            args: args_tast,
            ty: ret_ty,
        }
    }

    pub(super) fn infer_struct_literal_expr(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        request: StructLiteralRequest<'_>,
    ) -> tast::Expr {
        let StructLiteralRequest {
            expr_id,
            name,
            fields,
            hint_ret_ty,
        } = request;
        let name_display = name.display();
        let (resolved_name, type_env) = util::resolve_type_name(genv, &name_display);
        let ctor = type_env.lookup_constructor(&tast::TastIdent(resolved_name.clone()));
        let Some((constructor, constr_ty)) = ctor else {
            util::push_error_with_range(
                diagnostics,
                format!("Constructor {} not found in environment", resolved_name),
                self.expr_range(expr_id),
            );
            return self.error_expr(None);
        };

        let struct_fields = match &constructor {
            common::Constructor::Struct(struct_constructor) => {
                let type_name = &struct_constructor.type_name;
                let struct_def = type_env.structs().get(type_name);
                let Some(struct_def) = struct_def else {
                    util::push_ice(
                        diagnostics,
                        format!(
                            "Struct {} not found when checking literal {}",
                            type_name.0, resolved_name
                        ),
                    );
                    return self.error_expr(None);
                };
                struct_def.fields.clone()
            }
            common::Constructor::Enum { .. } => {
                util::push_error_with_range(
                    diagnostics,
                    format!(
                        "Constructor {} refers to an enum, but a struct literal was used",
                        name_display
                    ),
                    self.expr_range(expr_id),
                );
                return self.error_expr(None);
            }
        };

        let inst_constr_ty = self.inst_ty(&constr_ty);
        if let Some(hint) = hint_ret_ty {
            let ret_ty = match &inst_constr_ty {
                tast::Ty::TFunc { ret_ty, .. } => *ret_ty.clone(),
                _ => inst_constr_ty.clone(),
            };
            self.unify(diagnostics, &ret_ty, hint, self.expr_range(expr_id));
        }
        let param_tys: Vec<tast::Ty> = match &inst_constr_ty {
            tast::Ty::TFunc { params, .. } => params.iter().map(|p| self.norm(p)).collect(),
            _ => Vec::new(),
        };

        if !param_tys.is_empty() && param_tys.len() != struct_fields.len() {
            util::push_ice(
                diagnostics,
                format!(
                    "Constructor {} expects {} fields, but got {}",
                    resolved_name,
                    param_tys.len(),
                    struct_fields.len()
                ),
            );
        }

        let mut field_positions: HashMap<tast::TastIdent, usize> = HashMap::new();
        for (idx, (fname, _)) in struct_fields.iter().enumerate() {
            field_positions.insert(fname.clone(), idx);
        }

        let mut ordered_args: Vec<Option<tast::Expr>> = vec![None; struct_fields.len()];
        let mut ordered_hir_args: Vec<Option<hir::ExprId>> = vec![None; struct_fields.len()];
        for (field_name, expr) in fields.iter() {
            let idx = field_positions.get(&tast::TastIdent(field_name.to_ident_name()));
            let Some(&idx) = idx else {
                util::push_error_with_range(
                    diagnostics,
                    format!(
                        "Unknown field {} on struct literal {}",
                        field_name.to_ident_name(),
                        resolved_name
                    ),
                    self.expr_range(*expr),
                );
                self.infer_expr(genv, local_env, diagnostics, *expr);
                continue;
            };
            let slot = ordered_args.get_mut(idx);
            let Some(slot) = slot else {
                util::push_ice(
                    diagnostics,
                    format!("struct literal field index {} out of bounds", idx),
                );
                continue;
            };
            if slot.is_some() {
                util::push_error_with_range(
                    diagnostics,
                    format!(
                        "Duplicate field {} in struct literal {}",
                        field_name.to_ident_name(),
                        name_display
                    ),
                    self.expr_range(*expr),
                );
                continue;
            }
            let hir_slot = ordered_hir_args.get_mut(idx);
            if let Some(hir_slot) = hir_slot {
                *hir_slot = Some(*expr);
            }
            let field_expr = if let Some(expected_ty) = param_tys.get(idx) {
                self.check_expr(genv, local_env, diagnostics, *expr, expected_ty)
            } else {
                self.infer_expr(genv, local_env, diagnostics, *expr)
            };
            *slot = Some(field_expr);
        }

        let mut elab_args = Vec::with_capacity(struct_fields.len());
        for (idx, slot) in ordered_args.iter_mut().enumerate() {
            if slot.is_none() {
                let missing = struct_fields.get(idx).map(|(name, _)| name.0.as_str());
                let Some(missing) = missing else {
                    util::push_ice(
                        diagnostics,
                        format!("struct literal field index {} out of bounds", idx),
                    );
                    let placeholder = self.error_expr(None);
                    elab_args.push(StructLitArgElab::Missing {
                        expected_ty: placeholder.get_ty(),
                    });
                    *slot = Some(placeholder);
                    continue;
                };
                util::push_error_with_range(
                    diagnostics,
                    format!(
                        "Missing field {} in struct literal {}",
                        missing, name_display
                    ),
                    self.expr_range(expr_id),
                );
                let placeholder = param_tys
                    .get(idx)
                    .cloned()
                    .map(|ty| self.error_expr_with_ty(None, ty))
                    .unwrap_or_else(|| self.error_expr(None));
                elab_args.push(StructLitArgElab::Missing {
                    expected_ty: placeholder.get_ty(),
                });
                *slot = Some(placeholder);
            } else if let Some(hir_arg) = ordered_hir_args.get(idx).and_then(|x| *x) {
                elab_args.push(StructLitArgElab::Expr(hir_arg));
            } else {
                let placeholder_ty = slot
                    .as_ref()
                    .map(tast::Expr::get_ty)
                    .unwrap_or(tast::Ty::TUnit);
                elab_args.push(StructLitArgElab::Missing {
                    expected_ty: placeholder_ty,
                });
            }
        }

        let args_tast: Vec<tast::Expr> = ordered_args
            .into_iter()
            .map(|arg| arg.unwrap_or_else(|| self.error_expr(None)))
            .collect();
        let ret_ty = match &inst_constr_ty {
            tast::Ty::TFunc { ret_ty, .. } => *ret_ty.clone(),
            _ => inst_constr_ty.clone(),
        };

        if !args_tast.is_empty() {
            let actual_params: Vec<_> = args_tast
                .iter()
                .enumerate()
                .map(|(i, arg)| param_tys.get(i).cloned().unwrap_or_else(|| arg.get_ty()))
                .collect();
            let actual_ty = tast::Ty::TFunc {
                params: actual_params,
                ret_ty: Box::new(ret_ty.clone()),
            };
            self.equate(
                diagnostics,
                &inst_constr_ty,
                &actual_ty,
                self.expr_range(expr_id),
            );
        } else {
            self.equate(
                diagnostics,
                &inst_constr_ty,
                &ret_ty,
                self.expr_range(expr_id),
            );
        }

        self.results.record_struct_lit_elab(
            expr_id,
            StructLitElab {
                constructor: constructor.clone(),
                args: elab_args,
            },
        );

        tast::Expr::EConstr {
            constructor,
            args: args_tast,
            ty: ret_ty,
        }
    }
}
