use std::collections::{HashMap, hash_map::Entry};

use super::*;
use crate::typer::util;

impl Typer {
    pub(super) fn check_pat(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        pat: hir::PatId,
        ty: &tast::Ty,
    ) -> tast::Pat {
        let pat_node = self.hir_table.pat(pat).clone();
        let range = self.pat_range(pat);
        let astptr = self.pat_astptr(pat);
        if matches!(self.norm(ty), tast::Ty::TDyn { .. })
            && !matches!(pat_node, hir::Pat::PVar { .. } | hir::Pat::PWild)
        {
            util::push_error_with_range(
                diagnostics,
                format!(
                    "Pattern matching on {} is not supported",
                    util::format_ty_for_diag(ty)
                ),
                range,
            );
            let out = self.check_pat_wild(ty, astptr);
            self.results.record_pat_ty(pat, out.get_ty());
            return out;
        }
        let out = match pat_node {
            hir::Pat::PVar { name, astptr } => {
                self.check_pat_var(local_env, diagnostics, name, Some(astptr), ty)
            }
            hir::Pat::PUnit => self.check_pat_unit(diagnostics, ty, astptr),
            hir::Pat::PBool { value } => self.check_pat_bool(diagnostics, value, ty, range, astptr),
            hir::Pat::PInt { value } => self.check_pat_int(diagnostics, &value, ty, range, astptr),
            hir::Pat::PInt8 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TInt8, ty, range, astptr)
            }
            hir::Pat::PInt16 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TInt16, ty, range, astptr)
            }
            hir::Pat::PInt32 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TInt32, ty, range, astptr)
            }
            hir::Pat::PInt64 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TInt64, ty, range, astptr)
            }
            hir::Pat::PUInt8 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TUint8, ty, range, astptr)
            }
            hir::Pat::PUInt16 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TUint16, ty, range, astptr)
            }
            hir::Pat::PUInt32 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TUint32, ty, range, astptr)
            }
            hir::Pat::PUInt64 { value } => {
                self.check_pat_typed_int(diagnostics, &value, &tast::Ty::TUint64, ty, range, astptr)
            }
            hir::Pat::PFloat { value } => {
                self.check_pat_float(diagnostics, &value, ty, range, astptr)
            }
            hir::Pat::PFloat32 { value } => self.check_pat_typed_float(
                diagnostics,
                &value,
                &tast::Ty::TFloat32,
                ty,
                range,
                astptr,
            ),
            hir::Pat::PFloat64 { value } => self.check_pat_typed_float(
                diagnostics,
                &value,
                &tast::Ty::TFloat64,
                ty,
                range,
                astptr,
            ),
            hir::Pat::PString { value } => {
                self.check_pat_string(diagnostics, &value, ty, range, astptr)
            }
            hir::Pat::PChar { value } => {
                self.check_pat_char(diagnostics, value.as_str(), ty, range, astptr)
            }
            hir::Pat::PConstr { .. } => {
                self.check_pat_constructor(genv, local_env, diagnostics, pat, ty)
            }
            hir::Pat::PStruct { .. } => {
                self.check_pat_constructor(genv, local_env, diagnostics, pat, ty)
            }
            hir::Pat::PTuple { pats } => {
                self.check_pat_tuple(genv, local_env, diagnostics, pat, &pats, ty)
            }
            hir::Pat::PArray {
                prefix,
                rest,
                suffix,
            } => self.check_pat_array(
                genv,
                local_env,
                diagnostics,
                pat,
                &prefix,
                rest.as_ref(),
                &suffix,
                ty,
            ),
            hir::Pat::PAlias {
                name,
                pat: inner,
                astptr,
            } => self.check_pat_alias(genv, local_env, diagnostics, name, inner, astptr, ty),
            hir::Pat::POr { pats } => {
                self.check_pat_or(genv, local_env, diagnostics, pat, &pats, ty)
            }
            hir::Pat::PRange {
                start,
                end,
                inclusive,
            } => self.check_pat_range(genv, local_env, diagnostics, pat, start, end, inclusive, ty),
            hir::Pat::PWild => self.check_pat_wild(ty, astptr),
        };
        self.results.record_pat_ty(pat, out.get_ty());
        out
    }

    fn check_pat_var(
        &mut self,
        local_env: &mut LocalTypeEnv,
        _diagnostics: &mut Diagnostics,
        name: hir::LocalId,
        astptr: Option<MySyntaxNodePtr>,
        ty: &tast::Ty,
    ) -> tast::Pat {
        local_env.insert_var(name, ty.clone());
        self.results.record_local_ty(name, ty.clone());
        let name_str = self.hir_table.local_ident_name(name);
        tast::Pat::PVar {
            name: name_str,
            ty: ty.clone(),
            astptr,
        }
    }

    fn check_pat_unit(
        &mut self,
        diagnostics: &mut Diagnostics,
        ty: &tast::Ty,
        astptr: Option<MySyntaxNodePtr>,
    ) -> tast::Pat {
        self.equate(diagnostics, &tast::Ty::TUnit, ty, None);
        tast::Pat::PPrim {
            value: Prim::Unit { value: () },
            ty: tast::Ty::TUnit,
            astptr,
        }
    }

    fn check_pat_bool(
        &mut self,
        diagnostics: &mut Diagnostics,
        value: bool,
        ty: &tast::Ty,
        range: Option<TextRange>,
        astptr: Option<MySyntaxNodePtr>,
    ) -> tast::Pat {
        self.equate(diagnostics, &tast::Ty::TBool, ty, range);
        tast::Pat::PPrim {
            value: Prim::boolean(value),
            ty: tast::Ty::TBool,
            astptr,
        }
    }

    fn check_pat_int(
        &mut self,
        diagnostics: &mut Diagnostics,
        value: &str,
        ty: &tast::Ty,
        range: Option<TextRange>,
        astptr: Option<MySyntaxNodePtr>,
    ) -> tast::Pat {
        let target_ty = integer_literal_target(ty).unwrap_or(tast::Ty::TInt32);
        let prim = parse_integer_literal_with_ty(diagnostics, value, &target_ty, range)
            .unwrap_or_else(|| Prim::zero_for_int_ty(&target_ty));
        self.equate(diagnostics, &target_ty, ty, range);
        tast::Pat::PPrim {
            value: prim,
            ty: ty.clone(),
            astptr,
        }
    }

    fn check_pat_typed_int(
        &mut self,
        diagnostics: &mut Diagnostics,
        value: &str,
        literal_ty: &tast::Ty,
        expected_ty: &tast::Ty,
        range: Option<TextRange>,
        astptr: Option<MySyntaxNodePtr>,
    ) -> tast::Pat {
        let prim = parse_integer_literal_with_ty(diagnostics, value, literal_ty, range)
            .unwrap_or_else(|| Prim::zero_for_int_ty(literal_ty));
        self.equate(diagnostics, literal_ty, expected_ty, range);
        tast::Pat::PPrim {
            value: prim,
            ty: literal_ty.clone(),
            astptr,
        }
    }

    fn check_pat_float(
        &mut self,
        diagnostics: &mut Diagnostics,
        value: &str,
        ty: &tast::Ty,
        range: Option<TextRange>,
        astptr: Option<MySyntaxNodePtr>,
    ) -> tast::Pat {
        let target_ty = if is_float_ty(ty) {
            ty.clone()
        } else {
            tast::Ty::TFloat64
        };
        let prim = parse_float_literal_with_ty(diagnostics, value, &target_ty, range)
            .unwrap_or(Prim::Float64 { value: 0.0 });
        self.equate(diagnostics, &target_ty, ty, range);
        tast::Pat::PPrim {
            value: prim,
            ty: ty.clone(),
            astptr,
        }
    }

    fn check_pat_typed_float(
        &mut self,
        diagnostics: &mut Diagnostics,
        value: &str,
        literal_ty: &tast::Ty,
        expected_ty: &tast::Ty,
        range: Option<TextRange>,
        astptr: Option<MySyntaxNodePtr>,
    ) -> tast::Pat {
        let prim = parse_float_literal_with_ty(diagnostics, value, literal_ty, range)
            .unwrap_or(Prim::Float64 { value: 0.0 });
        self.equate(diagnostics, literal_ty, expected_ty, range);
        tast::Pat::PPrim {
            value: prim,
            ty: literal_ty.clone(),
            astptr,
        }
    }

    fn check_pat_string(
        &mut self,
        diagnostics: &mut Diagnostics,
        value: &String,
        ty: &tast::Ty,
        range: Option<TextRange>,
        astptr: Option<MySyntaxNodePtr>,
    ) -> tast::Pat {
        self.equate(diagnostics, &tast::Ty::TString, ty, range);
        tast::Pat::PPrim {
            value: Prim::string(value.to_owned()),
            ty: tast::Ty::TString,
            astptr,
        }
    }

    fn check_pat_char(
        &mut self,
        diagnostics: &mut Diagnostics,
        value: &str,
        ty: &tast::Ty,
        range: Option<TextRange>,
        astptr: Option<MySyntaxNodePtr>,
    ) -> tast::Pat {
        self.equate(diagnostics, &tast::Ty::TChar, ty, range);
        let ch = parse_char_literal(diagnostics, value, range).unwrap_or('\0');
        tast::Pat::PPrim {
            value: Prim::character(ch),
            ty: tast::Ty::TChar,
            astptr,
        }
    }

    fn check_pat_constructor(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        pat: hir::PatId,
        ty: &tast::Ty,
    ) -> tast::Pat {
        let pat_node = self.hir_table.pat(pat);
        let kind = pat_node.clone();
        match kind {
            hir::Pat::PConstr {
                constructor: constructor_ref,
                args,
            } => {
                let constructor_path = match &constructor_ref {
                    hir::ConstructorRef::Resolved(ctor_id) => {
                        self.constructor_path_from_id(diagnostics, ctor_id)
                    }
                    hir::ConstructorRef::Unresolved(path) => path.clone(),
                    hir::ConstructorRef::Ambiguous { path, .. } => {
                        util::push_error_with_range(
                            diagnostics,
                            format!("Ambiguous constructor {}", path.display()),
                            self.pat_range(pat),
                        );
                        return self.check_pat_wild(ty, self.pat_astptr(pat));
                    }
                };

                let Some(variant_ident) = constructor_path.last_ident() else {
                    util::push_ice(diagnostics, "Constructor path missing final segment");
                    return self.check_pat_wild(ty, self.pat_astptr(pat));
                };
                let variant_name = tast::TastIdent(variant_ident.clone());
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
                let ctor =
                    enum_env.lookup_constructor_with_namespace(enum_ident.as_ref(), &variant_name);
                let Some((constructor, constr_ty)) = ctor else {
                    util::push_error_with_range(
                        diagnostics,
                        format!(
                            "Constructor {} not found in environment",
                            constructor_path.display()
                        ),
                        self.pat_range(pat),
                    );
                    return self.check_pat_wild(ty, self.pat_astptr(pat));
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
                            return self.check_pat_wild(ty, self.pat_astptr(pat));
                        };
                        let variant = def.variants.get(enum_constructor.index);
                        let Some(variant) = variant else {
                            util::push_ice(
                                diagnostics,
                                format!(
                                    "Enum {} variant index {} out of bounds",
                                    enum_constructor.type_name.0, enum_constructor.index
                                ),
                            );
                            return self.check_pat_wild(ty, self.pat_astptr(pat));
                        };
                        if matches!(variant.fields, crate::env::EnumVariantFields::Struct(_)) {
                            util::push_error_with_range(
                                diagnostics,
                                format!(
                                    "Constructor {} is a struct-like enum variant and requires named fields",
                                    constructor_path.display()
                                ),
                                self.pat_range(pat),
                            );
                            return self.check_pat_wild(ty, self.pat_astptr(pat));
                        }
                        variant.fields.len()
                    }
                    common::Constructor::Struct(_) => {
                        util::push_error_with_range(
                            diagnostics,
                            format!(
                                "Struct {} patterns must use field syntax",
                                constructor.name().0
                            ),
                            self.pat_range(pat),
                        );
                        return self.check_pat_wild(ty, self.pat_astptr(pat));
                    }
                };

                if expected_arity != args.len() {
                    util::push_error_with_range(
                        diagnostics,
                        format!(
                            "Constructor {} expects {} arguments, but got {}",
                            constructor.name().0,
                            expected_arity,
                            args.len()
                        ),
                        self.pat_range(pat),
                    );
                    return self.check_pat_wild(ty, self.pat_astptr(pat));
                }

                let inst_constr_ty = self.inst_ty(&constr_ty);
                let (param_tys, ret_ty) = match &inst_constr_ty {
                    tast::Ty::TFunc { params, ret_ty } => (params.clone(), ret_ty.as_ref().clone()),
                    ty => (Vec::new(), ty.clone()),
                };

                let mut args_tast = Vec::with_capacity(args.len());
                for (idx, arg_ast) in args.iter().enumerate() {
                    let expected_ty = param_tys
                        .get(idx)
                        .cloned()
                        .unwrap_or_else(|| self.fresh_ty_var());
                    let arg_tast =
                        self.check_pat(genv, local_env, diagnostics, *arg_ast, &expected_ty);
                    args_tast.push(arg_tast);
                }

                self.equate(diagnostics, &ret_ty, ty, self.pat_range(pat));

                self.results
                    .record_constructor_pat(pat, constructor.clone());
                tast::Pat::PConstr {
                    constructor,
                    args: args_tast,
                    ty: ret_ty,
                    astptr: self.pat_astptr(pat),
                }
            }
            hir::Pat::PStruct {
                name,
                enum_constructor,
                fields,
                has_rest,
            } => {
                let name_display = name.display();
                let (type_name, type_env, ctor) = if let Some(constructor_ref) = enum_constructor {
                    let constructor_path = match constructor_ref {
                        hir::ConstructorRef::Resolved(constructor_id) => {
                            self.constructor_path_from_id(diagnostics, &constructor_id)
                        }
                        hir::ConstructorRef::Unresolved(path) => path.clone(),
                        hir::ConstructorRef::Ambiguous { path, .. } => {
                            util::push_error_with_range(
                                diagnostics,
                                format!("Ambiguous constructor {}", path.display()),
                                self.pat_range(pat),
                            );
                            return self.check_pat_wild(ty, self.pat_astptr(pat));
                        }
                    };
                    let Some(variant_name) = constructor_path.last_ident() else {
                        util::push_ice(diagnostics, "Constructor path missing final segment");
                        return self.check_pat_wild(ty, self.pat_astptr(pat));
                    };
                    let namespace = constructor_path.namespace_segments();
                    let (enum_name, type_env) = if namespace.is_empty() {
                        (None, genv.current())
                    } else {
                        let name = namespace
                            .iter()
                            .map(|segment| segment.seg().clone())
                            .collect::<Vec<_>>()
                            .join("::");
                        let (resolved, env) = util::resolve_type_name(genv, &name);
                        (Some(tast::TastIdent(resolved)), env)
                    };
                    let variant_name = tast::TastIdent(variant_name.clone());
                    (
                        constructor_path.display(),
                        type_env,
                        type_env
                            .lookup_constructor_with_namespace(enum_name.as_ref(), &variant_name),
                    )
                } else {
                    let (type_name, type_env) = util::resolve_type_name(genv, &name_display);
                    let ctor = type_env.lookup_constructor(&tast::TastIdent(type_name.clone()));
                    (type_name, type_env, ctor)
                };
                let Some((constructor, constr_ty)) = ctor else {
                    util::push_error_with_range(
                        diagnostics,
                        format!("Constructor {} not found when checking pattern", type_name),
                        self.pat_range(pat),
                    );
                    return self.check_pat_wild(ty, self.pat_astptr(pat));
                };
                let named_fields = match &constructor {
                    common::Constructor::Struct(struct_constructor) => {
                        let Some(struct_def) =
                            type_env.structs().get(&struct_constructor.type_name)
                        else {
                            util::push_ice(
                                diagnostics,
                                format!(
                                    "Struct {} not found when checking pattern",
                                    struct_constructor.type_name.0
                                ),
                            );
                            return self.check_pat_wild(ty, self.pat_astptr(pat));
                        };
                        if struct_def.has_hidden_fields && !has_rest {
                            util::push_error_with_range(
                                diagnostics,
                                format!(
                                    "Struct pattern {} must use `..` because it has private fields",
                                    name_display
                                ),
                                self.pat_range(pat),
                            );
                        }
                        struct_def.fields.clone()
                    }
                    common::Constructor::Enum(enum_constructor) => {
                        let Some(enum_def) = type_env.enums().get(&enum_constructor.type_name)
                        else {
                            util::push_ice(
                                diagnostics,
                                format!(
                                    "Enum {} not found when checking pattern",
                                    enum_constructor.type_name.0
                                ),
                            );
                            return self.check_pat_wild(ty, self.pat_astptr(pat));
                        };
                        let Some(variant) = enum_def.variants.get(enum_constructor.index) else {
                            util::push_ice(
                                diagnostics,
                                format!(
                                    "Enum {} variant index {} out of bounds",
                                    enum_constructor.type_name.0, enum_constructor.index
                                ),
                            );
                            return self.check_pat_wild(ty, self.pat_astptr(pat));
                        };
                        let Some(fields) = variant.fields.struct_fields() else {
                            util::push_error_with_range(
                                diagnostics,
                                format!(
                                    "Constructor {} is not a struct-like enum variant",
                                    name_display
                                ),
                                self.pat_range(pat),
                            );
                            return self.check_pat_wild(ty, self.pat_astptr(pat));
                        };
                        fields.to_vec()
                    }
                };
                let mut field_map: HashMap<String, hir::PatId> = HashMap::new();
                for (fname, pat_id) in fields.iter() {
                    let field_name = fname.to_ident_name();
                    match field_map.entry(field_name) {
                        Entry::Occupied(field) => {
                            util::push_error_with_range(
                                diagnostics,
                                format!(
                                    "Struct pattern {} has duplicate field {}",
                                    type_name,
                                    field.key()
                                ),
                                self.pat_range(pat),
                            );
                        }
                        Entry::Vacant(field) => {
                            field.insert(*pat_id);
                        }
                    }
                }

                let inst_constr_ty = self.inst_ty(&constr_ty);
                let (param_tys, ret_ty) = match &inst_constr_ty {
                    tast::Ty::TFunc { params, ret_ty } => (params.clone(), ret_ty.as_ref().clone()),
                    ty => (Vec::new(), ty.clone()),
                };

                if !param_tys.is_empty() && param_tys.len() != named_fields.len() {
                    util::push_ice(
                        diagnostics,
                        format!(
                            "Constructor {} expects {} fields, but got {}",
                            type_name,
                            param_tys.len(),
                            named_fields.len()
                        ),
                    );
                }

                let mut args_tast = Vec::with_capacity(named_fields.len());
                let mut elab_args = Vec::with_capacity(named_fields.len());
                for (idx, (field_name, _)) in named_fields.iter().enumerate() {
                    let expected_ty = param_tys
                        .get(idx)
                        .cloned()
                        .unwrap_or_else(|| self.fresh_ty_var());
                    let pat_id = field_map.remove(&field_name.0);
                    let pat_tast = match pat_id {
                        Some(pat_id) => {
                            elab_args.push(StructPatArgElab::Pat(pat_id));
                            self.check_pat(genv, local_env, diagnostics, pat_id, &expected_ty)
                        }
                        None => {
                            if !has_rest {
                                util::push_error_with_range(
                                    diagnostics,
                                    format!(
                                        "Struct pattern {} missing field {}",
                                        name_display, field_name.0
                                    ),
                                    self.pat_range(pat),
                                );
                            }
                            let wild = self.check_pat_wild(&expected_ty, None);
                            elab_args.push(StructPatArgElab::MissingWild {
                                expected_ty: wild.get_ty(),
                            });
                            wild
                        }
                    };
                    args_tast.push(pat_tast);
                }

                if !field_map.is_empty() {
                    let mut extra = field_map.keys().cloned().collect::<Vec<_>>();
                    extra.sort();
                    util::push_error_with_range(
                        diagnostics,
                        format!(
                            "Struct pattern {} has unknown fields: {}",
                            name_display,
                            extra.join(", ")
                        ),
                        self.pat_range(pat),
                    );
                }

                self.equate(diagnostics, &ret_ty, ty, self.pat_range(pat));

                self.results.record_struct_pat_elab(
                    pat,
                    StructPatElab {
                        constructor: constructor.clone(),
                        args: elab_args,
                    },
                );
                tast::Pat::PConstr {
                    constructor,
                    args: args_tast,
                    ty: ret_ty,
                    astptr: self.pat_astptr(pat),
                }
            }
            _ => {
                util::push_ice(diagnostics, "Expected constructor pattern");
                self.check_pat_wild(ty, self.pat_astptr(pat))
            }
        }
    }

    pub(super) fn check_irrefutable_let_pattern(
        &mut self,
        genv: &PackageTypeEnv,
        diagnostics: &mut Diagnostics,
        pat_id: hir::PatId,
        pat: &tast::Pat,
    ) {
        self.check_irrefutable_pattern(genv, diagnostics, pat_id, pat, "let binding");
    }

    pub(super) fn check_irrefutable_for_pattern(
        &mut self,
        genv: &PackageTypeEnv,
        diagnostics: &mut Diagnostics,
        pat_id: hir::PatId,
        pat: &tast::Pat,
    ) {
        self.check_irrefutable_pattern(genv, diagnostics, pat_id, pat, "for loop");
    }

    fn check_irrefutable_pattern(
        &mut self,
        genv: &PackageTypeEnv,
        diagnostics: &mut Diagnostics,
        pat_id: hir::PatId,
        pat: &tast::Pat,
        context: &str,
    ) {
        let pat = self.normalize_match_pat(pat);
        if crate::typer::match_analysis::is_irrefutable(genv, &pat) {
            return;
        }
        diagnostics.push(
            Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!("Refutable pattern is not allowed in {context}; use match instead"),
            )
            .with_range(self.pat_range(pat_id)),
        );
    }

    fn check_pat_tuple(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        pat_id: hir::PatId,
        pats: &[hir::PatId],
        ty: &tast::Ty,
    ) -> tast::Pat {
        let expected_elem_tys: Vec<tast::Ty> = match ty {
            tast::Ty::TTuple { typs } if typs.len() == pats.len() => typs.clone(),
            _ => (0..pats.len()).map(|_| self.fresh_ty_var()).collect(),
        };

        let mut pats_tast = Vec::new();
        let mut pat_typs = Vec::new();
        for (pat, expected_ty) in pats.iter().zip(expected_elem_tys.iter()) {
            let pat_tast = self.check_pat(genv, local_env, diagnostics, *pat, expected_ty);
            pat_typs.push(pat_tast.get_ty());
            pats_tast.push(pat_tast);
        }
        let pat_ty = tast::Ty::TTuple { typs: pat_typs };
        self.equate(diagnostics, &pat_ty, ty, None);
        tast::Pat::PTuple {
            items: pats_tast,
            ty: pat_ty,
            astptr: self.pat_astptr(pat_id),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn check_pat_array(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        pat_id: hir::PatId,
        prefix: &[hir::PatId],
        rest: Option<&hir::ArrayPatRest>,
        suffix: &[hir::PatId],
        ty: &tast::Ty,
    ) -> tast::Pat {
        let container_ty = self.norm(ty);
        let item_count = prefix.len() + suffix.len();
        let (elem_ty, rest_ty) = match &container_ty {
            tast::Ty::TArray { len, elem } => {
                let valid = if rest.is_some() {
                    item_count <= *len
                } else {
                    item_count == *len
                };
                if !valid {
                    util::push_error_with_range(
                        diagnostics,
                        if rest.is_some() {
                            format!(
                                "Array pattern requires at least {} elements, but the array has length {}",
                                item_count, len
                            )
                        } else {
                            format!(
                                "Array pattern has {} elements, but the array has length {}",
                                item_count, len
                            )
                        },
                        self.pat_range(pat_id),
                    );
                }
                (
                    elem.as_ref().clone(),
                    tast::Ty::TArray {
                        len: len.saturating_sub(item_count),
                        elem: elem.clone(),
                    },
                )
            }
            tast::Ty::TVec { elem } | tast::Ty::TSlice { elem } => (
                elem.as_ref().clone(),
                tast::Ty::TSlice { elem: elem.clone() },
            ),
            _ => {
                util::push_error_with_range(
                    diagnostics,
                    format!(
                        "Array pattern requires an array, Vec, or Slice, but found {}",
                        util::format_ty_for_diag(&container_ty)
                    ),
                    self.pat_range(pat_id),
                );
                return self.check_pat_wild(ty, self.pat_astptr(pat_id));
            }
        };

        let prefix = prefix
            .iter()
            .map(|pat| self.check_pat(genv, local_env, diagnostics, *pat, &elem_ty))
            .collect();
        let suffix = suffix
            .iter()
            .map(|pat| self.check_pat(genv, local_env, diagnostics, *pat, &elem_ty))
            .collect();
        let rest = rest.map(|rest| {
            let binding = rest.binding.map(|name| {
                local_env.insert_var(name, rest_ty.clone());
                self.results.record_local_ty(name, rest_ty.clone());
                self.hir_table.local_ident_name(name)
            });
            tast::ArrayPatRest {
                binding,
                ty: rest_ty,
                astptr: Some(rest.astptr),
            }
        });

        self.equate(diagnostics, &container_ty, ty, self.pat_range(pat_id));
        tast::Pat::PArray {
            prefix,
            rest,
            suffix,
            ty: container_ty,
            astptr: self.pat_astptr(pat_id),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn check_pat_alias(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        name: hir::LocalId,
        pat: hir::PatId,
        astptr: MySyntaxNodePtr,
        ty: &tast::Ty,
    ) -> tast::Pat {
        local_env.insert_var(name, ty.clone());
        self.results.record_local_ty(name, ty.clone());
        let inner = self.check_pat(genv, local_env, diagnostics, pat, ty);
        tast::Pat::PAlias {
            name: self.hir_table.local_ident_name(name),
            pat: Box::new(inner),
            ty: ty.clone(),
            astptr: Some(astptr),
        }
    }

    fn check_pat_or(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        pat_id: hir::PatId,
        pats: &[hir::PatId],
        ty: &tast::Ty,
    ) -> tast::Pat {
        if pats.is_empty() {
            util::push_error_with_range(
                diagnostics,
                "Or-pattern must contain at least one alternative",
                self.pat_range(pat_id),
            );
            return self.check_pat_wild(ty, self.pat_astptr(pat_id));
        }
        let pats = pats
            .iter()
            .map(|pat| self.check_pat(genv, local_env, diagnostics, *pat, ty))
            .collect();
        tast::Pat::POr {
            pats,
            ty: ty.clone(),
            astptr: self.pat_astptr(pat_id),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn check_pat_range(
        &mut self,
        genv: &PackageTypeEnv,
        local_env: &mut LocalTypeEnv,
        diagnostics: &mut Diagnostics,
        pat_id: hir::PatId,
        start: hir::PatId,
        end: hir::PatId,
        inclusive: bool,
        ty: &tast::Ty,
    ) -> tast::Pat {
        let start = self.check_pat(genv, local_env, diagnostics, start, ty);
        let end = self.check_pat(genv, local_env, diagnostics, end, ty);
        let (tast::Pat::PPrim { value: start, .. }, tast::Pat::PPrim { value: end, .. }) =
            (start, end)
        else {
            util::push_error_with_range(
                diagnostics,
                "Range pattern endpoints must be integer or char literals",
                self.pat_range(pat_id),
            );
            return self.check_pat_wild(ty, self.pat_astptr(pat_id));
        };
        let Some(ordering) = compare_range_prims(&start, &end) else {
            util::push_error_with_range(
                diagnostics,
                "Range patterns only support integer and char literals of the same type",
                self.pat_range(pat_id),
            );
            return self.check_pat_wild(ty, self.pat_astptr(pat_id));
        };
        let valid = if inclusive {
            ordering != std::cmp::Ordering::Greater
        } else {
            ordering == std::cmp::Ordering::Less
        };
        if !valid {
            util::push_error_with_range(
                diagnostics,
                "Range pattern lower bound must be less than its upper bound",
                self.pat_range(pat_id),
            );
        }
        tast::Pat::PRange {
            start,
            end,
            inclusive,
            ty: ty.clone(),
            astptr: self.pat_astptr(pat_id),
        }
    }

    fn check_pat_wild(&mut self, ty: &tast::Ty, astptr: Option<MySyntaxNodePtr>) -> tast::Pat {
        tast::Pat::PWild {
            ty: ty.clone(),
            astptr,
        }
    }
}

fn compare_range_prims(start: &Prim, end: &Prim) -> Option<std::cmp::Ordering> {
    match (start, end) {
        (Prim::Int8 { value: left }, Prim::Int8 { value: right }) => left.partial_cmp(right),
        (Prim::Int16 { value: left }, Prim::Int16 { value: right }) => left.partial_cmp(right),
        (Prim::Int32 { value: left }, Prim::Int32 { value: right }) => left.partial_cmp(right),
        (Prim::Int64 { value: left }, Prim::Int64 { value: right }) => left.partial_cmp(right),
        (Prim::UInt8 { value: left }, Prim::UInt8 { value: right }) => left.partial_cmp(right),
        (Prim::UInt16 { value: left }, Prim::UInt16 { value: right }) => left.partial_cmp(right),
        (Prim::UInt32 { value: left }, Prim::UInt32 { value: right }) => left.partial_cmp(right),
        (Prim::UInt64 { value: left }, Prim::UInt64 { value: right }) => left.partial_cmp(right),
        (Prim::Char { value: left }, Prim::Char { value: right }) => left.partial_cmp(right),
        _ => None,
    }
}
