use std::collections::{HashMap, HashSet};

use diagnostics::{Severity, Stage};
use indexmap::IndexMap;
use parser::{Diagnostic, Diagnostics};
use text_size::TextRange;

use crate::{
    builtins,
    env::{self, FnScheme, GlobalTypeEnv, PackageTypeEnv},
    hir::{self},
    intrinsics::{
        CallableBody, ExternCapability, LangItemId, callable_body_from_attributes,
        lang_item_from_attributes, validate_callable_signature,
    },
    package_names::{BUILTIN_PACKAGE, ROOT_PACKAGE, is_special_unqualified_package},
    tast::{self},
    typer::{
        Typer,
        localenv::LocalTypeEnv,
        name_resolution,
        type_ops::{decompose_struct_type, instantiate_self_ty, substitute_ty_params},
        util::{type_expr_range, type_param_name_set, validate_ty},
    },
};

fn predeclare_types(genv: &mut GlobalTypeEnv, hir: &hir::PackageHir, hir_table: &hir::HirTable) {
    for item in hir.toplevels.iter() {
        match hir_table.def(*item) {
            hir::Def::EnumDef(enum_def) => {
                genv.ensure_enum_placeholder(
                    tast::TastIdent(enum_def.name.to_ident_name()),
                    enum_def
                        .generics
                        .iter()
                        .map(|i| tast::TastIdent(i.to_ident_name()))
                        .collect(),
                );
            }
            hir::Def::StructDef(hir::StructDef { name, generics, .. }) => {
                genv.ensure_struct_placeholder(
                    tast::TastIdent(name.to_ident_name()),
                    generics
                        .iter()
                        .map(|i| tast::TastIdent(i.to_ident_name()))
                        .collect(),
                );
            }
            hir::Def::TraitDef(trait_def) => {
                genv.trait_env.trait_defs.insert(
                    trait_def.name.to_ident_name(),
                    env::TraitDef {
                        params: trait_def
                            .generics
                            .iter()
                            .map(|param| tast::TastIdent(param.to_ident_name()))
                            .collect(),
                        predicates: Vec::new(),
                        supertraits: Vec::new(),
                        associated_types: trait_def
                            .associated_types
                            .iter()
                            .map(|associated| {
                                (
                                    associated.name.to_ident_name(),
                                    env::AssociatedTypeDef::default(),
                                )
                            })
                            .collect(),
                        methods: IndexMap::new(),
                    },
                );
            }
            _ => {}
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum NominalKind {
    Enum,
    Struct,
}

impl NominalKind {
    fn label(self) -> &'static str {
        match self {
            NominalKind::Enum => "enum",
            NominalKind::Struct => "struct",
        }
    }
}

fn validate_nominal_type_names(
    diagnostics: &mut Diagnostics,
    hir: &hir::PackageHir,
    hir_table: &hir::HirTable,
) {
    let mut seen = HashMap::new();
    for item in hir.toplevels.iter() {
        let (name, kind) = match hir_table.def(*item) {
            hir::Def::EnumDef(enum_def) => (enum_def.name.to_ident_name(), NominalKind::Enum),
            hir::Def::StructDef(struct_def) => {
                (struct_def.name.to_ident_name(), NominalKind::Struct)
            }
            _ => continue,
        };

        match seen.insert(name.clone(), kind) {
            Some(prev) if prev == kind => diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!("{} {} is defined multiple times", kind.label(), name),
            )),
            Some(_) => diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!("type {} is defined as both a struct and an enum", name),
            )),
            None => {}
        }
    }
}

fn validate_top_level_function_names(
    diagnostics: &mut Diagnostics,
    hir: &hir::PackageHir,
    hir_table: &hir::HirTable,
) {
    let mut seen = HashSet::new();
    for item in hir.toplevels.iter() {
        let name = match hir_table.def(*item) {
            hir::Def::Fn(func) => func.name.clone(),
            hir::Def::ExternFn(ext) => ext.name.to_ident_name(),
            _ => continue,
        };

        if !seen.insert(name.clone()) {
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!("function {} is defined multiple times", name),
            ));
        }
    }
}

fn validate_type_parameter_names<'a>(
    diagnostics: &mut Diagnostics,
    generics: impl IntoIterator<Item = &'a hir::HirIdent>,
) {
    let mut seen = HashSet::new();
    for param in generics {
        let name = param.to_ident_name();
        if !seen.insert(name.clone()) {
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!("type parameter {} is defined multiple times", name),
            ));
        }
    }
}

fn validate_top_level_type_parameter_names(
    diagnostics: &mut Diagnostics,
    hir: &hir::PackageHir,
    hir_table: &hir::HirTable,
) {
    for item in hir.toplevels.iter() {
        match hir_table.def(*item) {
            hir::Def::EnumDef(enum_def) => {
                validate_type_parameter_names(diagnostics, enum_def.generics.iter());
            }
            hir::Def::StructDef(struct_def) => {
                validate_type_parameter_names(diagnostics, struct_def.generics.iter());
            }
            hir::Def::ImplBlock(impl_block) => {
                validate_type_parameter_names(diagnostics, impl_block.generics.iter());
                for method in impl_block.methods.iter() {
                    let hir::Def::Fn(func) = hir_table.def(*method) else {
                        continue;
                    };
                    validate_type_parameter_names(
                        diagnostics,
                        impl_block.generics.iter().chain(func.generics.iter()),
                    );
                }
            }
            hir::Def::Fn(func) => {
                validate_type_parameter_names(diagnostics, func.generics.iter());
            }
            hir::Def::ExternFn(ext) => {
                validate_type_parameter_names(diagnostics, ext.generics.iter());
            }
            hir::Def::TraitDef(trait_def) => {
                validate_type_parameter_names(diagnostics, trait_def.generics.iter());
            }
        }
    }
}

fn validate_decl_ty(
    env: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    ty: &tast::Ty,
    range: Option<text_size::TextRange>,
    tparams: &HashSet<String>,
) {
    validate_ty(env, diagnostics, ty, range, tparams);
    validate_no_self_ty(diagnostics, ty, range);
}

fn register_lang_item(
    env: &mut PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    attrs: &[hir::Attribute],
    name: &str,
) {
    let item = match lang_item_from_attributes(attrs.iter().map(|attr| attr.text.as_str())) {
        Ok(item) => item,
        Err(message) => {
            diagnostics.push(Diagnostic::new(Stage::Typer, Severity::Error, message));
            return;
        }
    };
    let Some(item) = item else {
        return;
    };
    if env.extern_capability != ExternCapability::Core {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!("lang item {} is not permitted in this source", item.key()),
        ));
        return;
    }
    if name != item.source_name() {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!(
                "lang item {} must be declared as {}",
                item.key(),
                item.source_name()
            ),
        ));
        return;
    }
    if let Err(existing) = env
        .current_mut()
        .lang_items
        .insert(item, tast::TastIdent::new(name))
    {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!(
                "lang item {} is already declared as {}",
                item.key(),
                existing.0
            ),
        ));
    }
}

fn validate_no_self_ty(
    diagnostics: &mut Diagnostics,
    ty: &tast::Ty,
    range: Option<text_size::TextRange>,
) {
    match ty {
        tast::Ty::TStruct { name } if name == "Self" => {
            diagnostics.push(
                Diagnostic::new(
                    Stage::Typer,
                    Severity::Error,
                    "Self type is only valid in impl methods".to_string(),
                )
                .with_range(range),
            );
        }
        tast::Ty::TTuple { typs } => {
            for ty in typs {
                validate_no_self_ty(diagnostics, ty, range);
            }
        }
        tast::Ty::TApp { ty, args } => {
            validate_no_self_ty(diagnostics, ty, range);
            for arg in args {
                validate_no_self_ty(diagnostics, arg, range);
            }
        }
        tast::Ty::TArray { elem, .. }
        | tast::Ty::TSlice { elem }
        | tast::Ty::TVec { elem }
        | tast::Ty::TRef { elem } => validate_no_self_ty(diagnostics, elem, range),
        tast::Ty::THashMap { key, value } => {
            validate_no_self_ty(diagnostics, key, range);
            validate_no_self_ty(diagnostics, value, range);
        }
        tast::Ty::TFunc { params, ret_ty } => {
            for param in params {
                validate_no_self_ty(diagnostics, param, range);
            }
            validate_no_self_ty(diagnostics, ret_ty, range);
        }
        _ => {}
    }
}

fn validate_enum_variant_names(diagnostics: &mut Diagnostics, enum_def: &hir::EnumDef) {
    let mut seen = HashSet::new();
    let enum_name = enum_def.name.to_ident_name();
    for variant in enum_def.variants.iter() {
        let name = variant.name.to_ident_name();
        if !seen.insert(name.clone()) {
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "variant {} is defined multiple times in enum {}",
                    name, enum_name
                ),
            ));
        }
        if let hir::EnumVariantFields::Struct(fields) = &variant.fields {
            let mut seen_fields = HashSet::new();
            for (field, _) in fields {
                let field_name = field.to_ident_name();
                if !seen_fields.insert(field_name.clone()) {
                    diagnostics.push(Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "field {} is defined multiple times in variant {}::{}",
                            field_name, enum_name, name
                        ),
                    ));
                }
            }
        }
    }
}

fn validate_struct_field_names(diagnostics: &mut Diagnostics, struct_def: &hir::StructDef) {
    let mut seen = HashSet::new();
    let struct_name = struct_def.name.to_ident_name();
    for (field, _) in struct_def.fields.iter() {
        let name = field.to_ident_name();
        if !seen.insert(name.clone()) {
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "field {} is defined multiple times in struct {}",
                    name, struct_name
                ),
            ));
        }
    }
}

fn validate_trait_method_names(diagnostics: &mut Diagnostics, trait_def: &hir::TraitDef) {
    let mut seen = HashSet::new();
    let trait_name = trait_def.name.to_ident_name();
    for method in trait_def.method_sigs.iter() {
        let name = method.name.to_ident_name();
        if !seen.insert(name.clone()) {
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "method {} is defined multiple times in trait {}",
                    name, trait_name
                ),
            ));
        }
    }
}

#[derive(Clone)]
struct ProjectionCandidate {
    for_ty: tast::Ty,
    trait_ref: tast::TraitRef,
    associated_types: HashSet<String>,
}

fn projection_candidates_from_predicates(
    env: &PackageTypeEnv,
    predicates: &[env::TypePredicate],
) -> Vec<ProjectionCandidate> {
    predicates
        .iter()
        .filter_map(|predicate| {
            let env::TypePredicate::Trait { for_ty, trait_ref } = predicate else {
                return None;
            };
            let (resolved, trait_env) = super::util::resolve_trait_name(env, &trait_ref.name.0)?;
            let trait_def = trait_env.trait_env.trait_defs.get(&resolved)?;
            Some(ProjectionCandidate {
                for_ty: for_ty.clone(),
                trait_ref: trait_ref.clone(),
                associated_types: trait_def.associated_types.keys().cloned().collect(),
            })
        })
        .collect()
}

pub(crate) fn resolve_ty_projections_from_predicates(
    env: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    ty: &tast::Ty,
    predicates: &[env::TypePredicate],
    range: Option<TextRange>,
) -> tast::Ty {
    let candidates = projection_candidates_from_predicates(env, predicates);
    resolve_ty_projections(env, diagnostics, ty, &candidates, range)
}

fn resolve_trait_ref_projections(
    env: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    trait_ref: &tast::TraitRef,
    candidates: &[ProjectionCandidate],
    range: Option<TextRange>,
) -> tast::TraitRef {
    tast::TraitRef {
        name: trait_ref.name.clone(),
        args: trait_ref
            .args
            .iter()
            .map(|arg| resolve_ty_projections(env, diagnostics, arg, candidates, range))
            .collect(),
    }
}

fn resolve_ty_projections(
    env: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    ty: &tast::Ty,
    candidates: &[ProjectionCandidate],
    range: Option<TextRange>,
) -> tast::Ty {
    match ty {
        tast::Ty::TProjection {
            trait_ref,
            for_ty,
            name,
        } => {
            let for_ty = resolve_ty_projections(env, diagnostics, for_ty, candidates, range);
            let trait_ref = match trait_ref {
                Some(trait_ref) => Some(resolve_trait_ref_projections(
                    env,
                    diagnostics,
                    trait_ref,
                    candidates,
                    range,
                )),
                None => {
                    let matching = candidates
                        .iter()
                        .filter(|candidate| {
                            candidate.for_ty == for_ty
                                && candidate.associated_types.contains(&name.0)
                        })
                        .collect::<Vec<_>>();
                    match matching.as_slice() {
                        [candidate] => Some(resolve_trait_ref_projections(
                            env,
                            diagnostics,
                            &candidate.trait_ref,
                            candidates,
                            range,
                        )),
                        [] => {
                            diagnostics.push(
                                Diagnostic::new(
                                    Stage::Typer,
                                    Severity::Error,
                                    format!(
                                        "Associated type {}::{} is not provided by a trait bound",
                                        super::util::format_ty_for_diag(&for_ty),
                                        name.0
                                    ),
                                )
                                .with_range(range),
                            );
                            None
                        }
                        _ => {
                            let names = matching
                                .iter()
                                .map(|candidate| {
                                    super::util::format_trait_ref_for_diag(&candidate.trait_ref)
                                })
                                .collect::<Vec<_>>()
                                .join(", ");
                            diagnostics.push(
                                Diagnostic::new(
                                    Stage::Typer,
                                    Severity::Error,
                                    format!(
                                        "Associated type {}::{} is ambiguous between {}",
                                        super::util::format_ty_for_diag(&for_ty),
                                        name.0,
                                        names
                                    ),
                                )
                                .with_range(range),
                            );
                            None
                        }
                    }
                }
            };
            tast::Ty::TProjection {
                trait_ref,
                for_ty: Box::new(for_ty),
                name: name.clone(),
            }
        }
        tast::Ty::TTuple { typs } => tast::Ty::TTuple {
            typs: typs
                .iter()
                .map(|ty| resolve_ty_projections(env, diagnostics, ty, candidates, range))
                .collect(),
        },
        tast::Ty::TApp { ty, args } => tast::Ty::TApp {
            ty: Box::new(resolve_ty_projections(
                env,
                diagnostics,
                ty,
                candidates,
                range,
            )),
            args: args
                .iter()
                .map(|ty| resolve_ty_projections(env, diagnostics, ty, candidates, range))
                .collect(),
        },
        tast::Ty::TArray { len, elem } => tast::Ty::TArray {
            len: *len,
            elem: Box::new(resolve_ty_projections(
                env,
                diagnostics,
                elem,
                candidates,
                range,
            )),
        },
        tast::Ty::TSlice { elem } => tast::Ty::TSlice {
            elem: Box::new(resolve_ty_projections(
                env,
                diagnostics,
                elem,
                candidates,
                range,
            )),
        },
        tast::Ty::TVec { elem } => tast::Ty::TVec {
            elem: Box::new(resolve_ty_projections(
                env,
                diagnostics,
                elem,
                candidates,
                range,
            )),
        },
        tast::Ty::TRef { elem } => tast::Ty::TRef {
            elem: Box::new(resolve_ty_projections(
                env,
                diagnostics,
                elem,
                candidates,
                range,
            )),
        },
        tast::Ty::THashMap { key, value } => tast::Ty::THashMap {
            key: Box::new(resolve_ty_projections(
                env,
                diagnostics,
                key,
                candidates,
                range,
            )),
            value: Box::new(resolve_ty_projections(
                env,
                diagnostics,
                value,
                candidates,
                range,
            )),
        },
        tast::Ty::TFunc { params, ret_ty } => tast::Ty::TFunc {
            params: params
                .iter()
                .map(|ty| resolve_ty_projections(env, diagnostics, ty, candidates, range))
                .collect(),
            ret_ty: Box::new(resolve_ty_projections(
                env,
                diagnostics,
                ret_ty,
                candidates,
                range,
            )),
        },
        _ => ty.clone(),
    }
}

fn resolve_type_predicates(
    env: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    predicates: Vec<env::TypePredicate>,
) -> Vec<env::TypePredicate> {
    resolve_type_predicates_with_candidates(env, diagnostics, predicates, &[])
}

fn resolve_type_predicates_with_candidates(
    env: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    predicates: Vec<env::TypePredicate>,
    extra_candidates: &[ProjectionCandidate],
) -> Vec<env::TypePredicate> {
    let mut candidates = projection_candidates_from_predicates(env, &predicates);
    candidates.extend_from_slice(extra_candidates);
    predicates
        .into_iter()
        .map(|predicate| match predicate {
            env::TypePredicate::Trait { for_ty, trait_ref } => env::TypePredicate::Trait {
                for_ty: resolve_ty_projections(env, diagnostics, &for_ty, &candidates, None),
                trait_ref: resolve_trait_ref_projections(
                    env,
                    diagnostics,
                    &trait_ref,
                    &candidates,
                    None,
                ),
            },
            env::TypePredicate::Equality { lhs, rhs } => env::TypePredicate::Equality {
                lhs: resolve_ty_projections(env, diagnostics, &lhs, &candidates, None),
                rhs: resolve_ty_projections(env, diagnostics, &rhs, &candidates, None),
            },
        })
        .collect()
}

fn instantiate_predicate_self(
    predicate: &env::TypePredicate,
    self_ty: &tast::Ty,
) -> env::TypePredicate {
    match predicate {
        env::TypePredicate::Trait { for_ty, trait_ref } => env::TypePredicate::Trait {
            for_ty: instantiate_self_ty(for_ty, self_ty),
            trait_ref: tast::TraitRef {
                name: trait_ref.name.clone(),
                args: trait_ref
                    .args
                    .iter()
                    .map(|arg| instantiate_self_ty(arg, self_ty))
                    .collect(),
            },
        },
        env::TypePredicate::Equality { lhs, rhs } => env::TypePredicate::Equality {
            lhs: instantiate_self_ty(lhs, self_ty),
            rhs: instantiate_self_ty(rhs, self_ty),
        },
    }
}

fn expand_implied_predicates(
    env: &PackageTypeEnv,
    predicates: Vec<env::TypePredicate>,
) -> Vec<env::TypePredicate> {
    let mut result = Vec::new();
    for predicate in predicates {
        if !result.contains(&predicate) {
            result.push(predicate);
        }
    }
    let mut index = 0;
    while index < result.len() {
        let predicate = result[index].clone();
        index += 1;
        let env::TypePredicate::Trait { for_ty, trait_ref } = predicate else {
            continue;
        };
        for application in super::util::trait_ref_closure(env, &trait_ref) {
            let implied = env::TypePredicate::Trait {
                for_ty: for_ty.clone(),
                trait_ref: application.clone(),
            };
            if !result.contains(&implied) {
                result.push(implied);
            }
            let Some((resolved, trait_env)) =
                super::util::resolve_trait_name(env, &application.name.0)
            else {
                continue;
            };
            let Some(definition) = trait_env.trait_env.trait_defs.get(&resolved) else {
                continue;
            };
            if definition.params.len() != application.args.len() {
                continue;
            }
            let substitution = definition
                .params
                .iter()
                .zip(application.args.iter())
                .map(|(param, arg)| (param.0.clone(), arg.clone()))
                .collect::<HashMap<_, _>>();
            for declared in &definition.predicates {
                let declared = super::type_ops::substitute_predicate(declared, &substitution);
                let declared = instantiate_predicate_self(&declared, &for_ty);
                if !result.contains(&declared) {
                    result.push(declared);
                }
            }
            for (name, associated) in &definition.associated_types {
                let projection = tast::Ty::TProjection {
                    trait_ref: Some(application.clone()),
                    for_ty: Box::new(for_ty.clone()),
                    name: tast::TastIdent::new(name),
                };
                for bound in &associated.bounds {
                    let bound = super::type_ops::substitute_trait_ref(bound, &substitution);
                    let implied = instantiate_predicate_self(
                        &env::TypePredicate::Trait {
                            for_ty: projection.clone(),
                            trait_ref: bound,
                        },
                        &for_ty,
                    );
                    if !result.contains(&implied) {
                        result.push(implied);
                    }
                }
            }
        }
    }
    result
}

fn normalize_impl_associated_types(
    ty: &tast::Ty,
    trait_ref: &tast::TraitRef,
    for_ty: &tast::Ty,
    associated_types: &IndexMap<String, tast::Ty>,
) -> tast::Ty {
    let mut normalized = ty.clone();
    for _ in 0..=associated_types.len() {
        let next = super::type_ops::rewrite_ty(&normalized, &mut |ty| {
            let tast::Ty::TProjection {
                trait_ref: Some(projection_trait),
                for_ty: projection_self,
                name,
            } = ty
            else {
                return None;
            };
            (projection_trait == trait_ref && projection_self.as_ref() == for_ty)
                .then(|| associated_types.get(&name.0).cloned())
                .flatten()
        });
        if next == normalized {
            break;
        }
        normalized = next;
    }
    normalized
}

fn normalize_selected_associated_types(
    trait_solver: &mut super::traits::solver::TraitSolver<'_>,
    typer: &mut Typer,
    ty: &tast::Ty,
) -> tast::Ty {
    let mut normalized = typer.norm(ty);
    for _ in 0..64 {
        let next = super::type_ops::rewrite_ty(&normalized, &mut |ty| {
            let tast::Ty::TProjection {
                trait_ref: Some(trait_ref),
                for_ty,
                name,
            } = ty
            else {
                return None;
            };
            let trait_ref = tast::TraitRef {
                name: trait_ref.name.clone(),
                args: trait_ref.args.iter().map(|arg| typer.norm(arg)).collect(),
            };
            let for_ty = typer.norm(for_ty);
            match trait_solver.select(
                typer,
                super::obligations::TraitGoal {
                    trait_ref: trait_ref.clone(),
                    for_ty: for_ty.clone(),
                },
            ) {
                super::traits::solver::SelectionResult::Unique(selection) => {
                    match selection.source {
                        super::traits::solver::SelectionSource::Impl {
                            definition,
                            substitution,
                            ..
                        } => definition
                            .associated_types
                            .get(&name.0)
                            .map(|binding| substitute_ty_params(binding, &substitution)),
                        super::traits::solver::SelectionSource::ParamEnv => {
                            let projection = tast::Ty::TProjection {
                                trait_ref: Some(trait_ref),
                                for_ty: Box::new(for_ty),
                                name: name.clone(),
                            };
                            let projection = typer.norm(&projection);
                            (projection != *ty).then_some(projection)
                        }
                        super::traits::solver::SelectionSource::Dyn => None,
                    }
                }
                super::traits::solver::SelectionResult::NoSolution
                | super::traits::solver::SelectionResult::Ambiguous(_)
                | super::traits::solver::SelectionResult::Overflow => None,
            }
        });
        let next = typer.norm(&next);
        if next == normalized {
            return normalized;
        }
        normalized = next;
    }
    normalized
}

fn contains_impl_associated_projection(
    ty: &tast::Ty,
    trait_ref: &tast::TraitRef,
    for_ty: &tast::Ty,
) -> bool {
    let mut found = false;
    let _ = super::type_ops::rewrite_ty(ty, &mut |ty| {
        if matches!(
            ty,
            tast::Ty::TProjection {
                trait_ref: Some(projection_trait),
                for_ty: projection_self,
                ..
            } if projection_trait == trait_ref && projection_self.as_ref() == for_ty
        ) {
            found = true;
        }
        None
    });
    found
}

fn define_enum(env: &mut PackageTypeEnv, diagnostics: &mut Diagnostics, enum_def: &hir::EnumDef) {
    register_lang_item(
        env,
        diagnostics,
        &enum_def.attrs,
        &enum_def.name.to_ident_name(),
    );
    validate_enum_variant_names(diagnostics, enum_def);
    let params_env: Vec<tast::TastIdent> = enum_def
        .generics
        .iter()
        .map(|i| tast::TastIdent(i.to_ident_name()))
        .collect();
    let tparam_names = type_param_name_set(&enum_def.generics);

    let variants = enum_def
        .variants
        .iter()
        .map(|variant| {
            let mut lower_ty = |ast_ty: &hir::TypeExpr| {
                let ty = tast::Ty::from_hir(env, ast_ty, &params_env);
                validate_decl_ty(
                    env,
                    diagnostics,
                    &ty,
                    type_expr_range(ast_ty),
                    &tparam_names,
                );
                ty
            };
            let fields = match &variant.fields {
                hir::EnumVariantFields::Unit => env::EnumVariantFields::Unit,
                hir::EnumVariantFields::Tuple(types) => {
                    env::EnumVariantFields::Tuple(types.iter().map(lower_ty).collect())
                }
                hir::EnumVariantFields::Struct(fields) => env::EnumVariantFields::Struct(
                    fields
                        .iter()
                        .map(|(name, ty)| (tast::TastIdent(name.to_ident_name()), lower_ty(ty)))
                        .collect(),
                ),
            };
            env::EnumVariantDef {
                name: tast::TastIdent(variant.name.to_ident_name()),
                fields,
            }
        })
        .collect();
    env.current_mut().insert_enum(env::EnumDef {
        name: tast::TastIdent(enum_def.name.to_ident_name()),
        generics: enum_def
            .generics
            .iter()
            .map(|i| tast::TastIdent(i.to_ident_name()))
            .collect(),
        variants,
    });
}

fn define_struct(
    env: &mut PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    struct_def: &hir::StructDef,
) {
    register_lang_item(
        env,
        diagnostics,
        &struct_def.attrs,
        &struct_def.name.to_ident_name(),
    );
    validate_struct_field_names(diagnostics, struct_def);
    let params_env: Vec<tast::TastIdent> = struct_def
        .generics
        .iter()
        .map(|i| tast::TastIdent(i.to_ident_name()))
        .collect();
    let tparam_names = type_param_name_set(&struct_def.generics);
    let fields = struct_def
        .fields
        .iter()
        .map(|(fname, ast_ty)| {
            let ty = tast::Ty::from_hir(env, ast_ty, &params_env);
            validate_decl_ty(
                env,
                diagnostics,
                &ty,
                type_expr_range(ast_ty),
                &tparam_names,
            );
            (tast::TastIdent(fname.to_ident_name()), ty)
        })
        .collect();

    let name = tast::TastIdent(struct_def.name.to_ident_name());
    env.current_mut().type_env.public_struct_fields.insert(
        name.clone(),
        struct_def
            .public_fields
            .iter()
            .map(hir::HirIdent::to_ident_name)
            .collect(),
    );
    env.current_mut().insert_struct(env::StructDef {
        name,
        generics: struct_def
            .generics
            .iter()
            .map(|i| tast::TastIdent(i.to_ident_name()))
            .collect(),
        fields,
        has_hidden_fields: false,
    });
}

fn define_trait(
    env: &mut PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    trait_def: &hir::TraitDef,
) {
    register_lang_item(
        env,
        diagnostics,
        &trait_def.attrs,
        &trait_def.name.to_ident_name(),
    );
    validate_trait_method_names(diagnostics, trait_def);
    let trait_params = trait_def
        .generics
        .iter()
        .map(|param| tast::TastIdent(param.to_ident_name()))
        .collect::<Vec<_>>();
    let trait_param_names = type_param_name_set(&trait_def.generics);
    let trait_ref = tast::TraitRef {
        name: tast::TastIdent(trait_def.name.to_ident_name()),
        args: trait_params
            .iter()
            .map(|param| tast::Ty::TParam {
                name: param.0.clone(),
            })
            .collect(),
    };
    let associated_names = trait_def
        .associated_types
        .iter()
        .map(|associated| associated.name.to_ident_name())
        .collect::<HashSet<_>>();
    let mut projection_candidates = vec![ProjectionCandidate {
        for_ty: tast::Ty::TStruct {
            name: "Self".to_string(),
        },
        trait_ref: trait_ref.clone(),
        associated_types: associated_names,
    }];
    let mut supertraits = Vec::new();
    for supertrait in &trait_def.supertraits {
        let Some(supertrait) = resolve_hir_trait_ref(env, diagnostics, supertrait, &trait_params)
        else {
            continue;
        };
        let supertrait = resolve_trait_ref_projections(
            env,
            diagnostics,
            &supertrait,
            &projection_candidates,
            None,
        );
        if let Some((resolved, supertrait_env)) =
            super::util::resolve_trait_name(env, &supertrait.name.0)
            && let Some(supertrait_def) = supertrait_env.trait_env.trait_defs.get(&resolved)
        {
            projection_candidates.push(ProjectionCandidate {
                for_ty: tast::Ty::TStruct {
                    name: "Self".to_string(),
                },
                trait_ref: supertrait.clone(),
                associated_types: supertrait_def.associated_types.keys().cloned().collect(),
            });
        }
        supertraits.push(supertrait);
    }
    let mut predicates = build_trait_constraints(
        env,
        diagnostics,
        &trait_def.generics,
        &trait_def.generic_bounds,
        &trait_def.predicates,
        &projection_candidates,
    );
    if env.lang_item(LangItemId::IntoIterator) == Some(&trait_ref.name)
        && let Some(iterator) = env.lang_item(LangItemId::Iterator).cloned()
    {
        let self_ty = tast::Ty::TStruct {
            name: "Self".to_string(),
        };
        let item = tast::Ty::TProjection {
            trait_ref: Some(trait_ref.clone()),
            for_ty: Box::new(self_ty.clone()),
            name: tast::TastIdent::new("Item"),
        };
        let into_iter = tast::Ty::TProjection {
            trait_ref: Some(trait_ref.clone()),
            for_ty: Box::new(self_ty),
            name: tast::TastIdent::new("IntoIter"),
        };
        let iterator_item = tast::Ty::TProjection {
            trait_ref: Some(tast::TraitRef {
                name: iterator,
                args: Vec::new(),
            }),
            for_ty: Box::new(into_iter),
            name: tast::TastIdent::new("Item"),
        };
        predicates.push(env::TypePredicate::Equality {
            lhs: item,
            rhs: iterator_item,
        });
    }
    let mut associated_types = IndexMap::new();
    for associated in &trait_def.associated_types {
        let name = associated.name.to_ident_name();
        if associated_types.contains_key(&name) {
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "associated type {} is defined multiple times in trait {}",
                    name,
                    trait_def.name.to_ident_name()
                ),
            ));
            continue;
        }
        let mut bounds = Vec::new();
        for bound in &associated.bounds {
            let Some(bound) = resolve_hir_trait_ref(env, diagnostics, bound, &trait_params) else {
                continue;
            };
            bounds.push(resolve_trait_ref_projections(
                env,
                diagnostics,
                &bound,
                &projection_candidates,
                None,
            ));
        }
        associated_types.insert(name, env::AssociatedTypeDef { bounds });
    }
    let mut methods = IndexMap::new();

    for hir::TraitMethodSignature {
        name: method_name,
        params,
        ret_ty,
    } in trait_def.method_sigs.iter()
    {
        let param_tys = params
            .iter()
            .map(|ast_ty| {
                let ty = tast::Ty::from_hir(env, ast_ty, &trait_params);
                let ty = resolve_ty_projections(
                    env,
                    diagnostics,
                    &ty,
                    &projection_candidates,
                    type_expr_range(ast_ty),
                );
                validate_ty(
                    env,
                    diagnostics,
                    &ty,
                    type_expr_range(ast_ty),
                    &trait_param_names,
                );
                ty
            })
            .collect::<Vec<_>>();
        let hir_ret_ty = ret_ty;
        let ret_ty = tast::Ty::from_hir(env, hir_ret_ty, &trait_params);
        let ret_ty = resolve_ty_projections(
            env,
            diagnostics,
            &ret_ty,
            &projection_candidates,
            type_expr_range(hir_ret_ty),
        );
        validate_ty(
            env,
            diagnostics,
            &ret_ty,
            type_expr_range(hir_ret_ty),
            &trait_param_names,
        );
        let fn_ty = tast::Ty::TFunc {
            params: param_tys,
            ret_ty: Box::new(ret_ty),
        };

        methods.insert(
            method_name.to_ident_name(),
            FnScheme {
                type_params: vec![],
                constraints: vec![],
                ty: fn_ty,
                body: CallableBody::Goml,
            },
        );
    }

    env.current_mut().trait_env.trait_defs.insert(
        trait_def.name.to_ident_name(),
        env::TraitDef {
            params: trait_params,
            predicates,
            supertraits,
            associated_types,
            methods,
        },
    );
}

fn resolve_hir_trait_ref(
    env: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    trait_ref: &hir::TraitRef,
    type_params: &[tast::TastIdent],
) -> Option<tast::TraitRef> {
    let raw_name = trait_ref.name.to_ident_name();
    let Some((name, trait_env)) = super::util::resolve_trait_name(env, &raw_name) else {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!("Unknown trait {}", raw_name),
        ));
        return None;
    };
    let Some(definition) = trait_env.trait_env.trait_defs.get(&name) else {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!("Unknown trait {}", raw_name),
        ));
        return None;
    };
    let type_param_names = type_params
        .iter()
        .map(|param| param.0.clone())
        .collect::<HashSet<_>>();
    let diagnostic_count = diagnostics.len();
    let args = trait_ref
        .args
        .iter()
        .map(|arg| {
            let ty = tast::Ty::from_hir(env, arg, type_params);
            validate_ty(
                env,
                diagnostics,
                &ty,
                type_expr_range(arg),
                &type_param_names,
            );
            ty
        })
        .collect::<Vec<_>>();
    if diagnostics.len() != diagnostic_count {
        return None;
    }
    if definition.params.len() != args.len() {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!(
                "Trait {} expects {} type arguments, but got {}",
                name,
                definition.params.len(),
                args.len()
            ),
        ));
        return None;
    }
    Some(tast::TraitRef {
        name: tast::TastIdent(name),
        args,
    })
}

fn resolve_hir_trait_ref_silent(
    env: &PackageTypeEnv,
    trait_ref: &hir::TraitRef,
    type_params: &[tast::TastIdent],
) -> Option<tast::TraitRef> {
    let raw_name = trait_ref.name.to_ident_name();
    let (name, trait_env) = super::util::resolve_trait_name(env, &raw_name)?;
    let definition = trait_env.trait_env.trait_defs.get(&name)?;
    let args = trait_ref
        .args
        .iter()
        .map(|arg| tast::Ty::from_hir(env, arg, type_params))
        .collect::<Vec<_>>();
    (definition.params.len() == args.len()).then_some(tast::TraitRef {
        name: tast::TastIdent(name),
        args,
    })
}

fn add_fn_constraints_from_bounds(
    env: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    known_type_params: &HashSet<String>,
    type_params: &[tast::TastIdent],
    bounds: &[(hir::HirIdent, Vec<hir::TraitRef>)],
    constraints: &mut Vec<env::TypePredicate>,
) {
    for (param, traits) in bounds.iter() {
        let param_name = param.to_ident_name();
        if !known_type_params.contains(&param_name) {
            continue;
        }
        for trait_ref in traits.iter() {
            let Some(trait_ref) = resolve_hir_trait_ref(env, diagnostics, trait_ref, type_params)
            else {
                continue;
            };
            let constraint = env::TypePredicate::Trait {
                for_ty: tast::Ty::TParam {
                    name: param_name.clone(),
                },
                trait_ref,
            };
            if !constraints.contains(&constraint) {
                constraints.push(constraint);
            }
        }
    }
}

fn add_type_predicates(
    env: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    known_type_params: &HashSet<String>,
    type_params: &[tast::TastIdent],
    predicates: &[hir::Predicate],
    constraints: &mut Vec<env::TypePredicate>,
    allow_self: bool,
) {
    for predicate in predicates {
        let predicate = match predicate {
            hir::Predicate::Trait { ty, trait_ref } => {
                let for_ty = tast::Ty::from_hir(env, ty, type_params);
                if allow_self {
                    validate_ty(
                        env,
                        diagnostics,
                        &for_ty,
                        type_expr_range(ty),
                        known_type_params,
                    );
                } else {
                    validate_decl_ty(
                        env,
                        diagnostics,
                        &for_ty,
                        type_expr_range(ty),
                        known_type_params,
                    );
                }
                let Some(trait_ref) =
                    resolve_hir_trait_ref(env, diagnostics, trait_ref, type_params)
                else {
                    continue;
                };
                env::TypePredicate::Trait { for_ty, trait_ref }
            }
            hir::Predicate::Equality { lhs, rhs } => {
                let lhs_ty = tast::Ty::from_hir(env, lhs, type_params);
                let rhs_ty = tast::Ty::from_hir(env, rhs, type_params);
                if allow_self {
                    validate_ty(
                        env,
                        diagnostics,
                        &lhs_ty,
                        type_expr_range(lhs),
                        known_type_params,
                    );
                    validate_ty(
                        env,
                        diagnostics,
                        &rhs_ty,
                        type_expr_range(rhs),
                        known_type_params,
                    );
                } else {
                    validate_decl_ty(
                        env,
                        diagnostics,
                        &lhs_ty,
                        type_expr_range(lhs),
                        known_type_params,
                    );
                    validate_decl_ty(
                        env,
                        diagnostics,
                        &rhs_ty,
                        type_expr_range(rhs),
                        known_type_params,
                    );
                }
                env::TypePredicate::Equality {
                    lhs: lhs_ty,
                    rhs: rhs_ty,
                }
            }
        };
        if !constraints.contains(&predicate) {
            constraints.push(predicate);
        }
    }
}

fn build_fn_constraints(
    env: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    generics: &[hir::HirIdent],
    bounds: &[(hir::HirIdent, Vec<hir::TraitRef>)],
    predicates: &[hir::Predicate],
) -> Vec<env::TypePredicate> {
    let known_type_params = generics
        .iter()
        .map(|param| param.to_ident_name())
        .collect::<HashSet<_>>();
    let mut constraints = Vec::new();
    let type_params = generics
        .iter()
        .map(|param| tast::TastIdent(param.to_ident_name()))
        .collect::<Vec<_>>();
    add_fn_constraints_from_bounds(
        env,
        diagnostics,
        &known_type_params,
        &type_params,
        bounds,
        &mut constraints,
    );
    add_type_predicates(
        env,
        diagnostics,
        &known_type_params,
        &type_params,
        predicates,
        &mut constraints,
        false,
    );
    let constraints = resolve_type_predicates(env, diagnostics, constraints);
    expand_implied_predicates(env, constraints)
}

fn build_trait_constraints(
    env: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    generics: &[hir::HirIdent],
    bounds: &[(hir::HirIdent, Vec<hir::TraitRef>)],
    predicates: &[hir::Predicate],
    projection_candidates: &[ProjectionCandidate],
) -> Vec<env::TypePredicate> {
    let known_type_params = generics
        .iter()
        .map(|param| param.to_ident_name())
        .collect::<HashSet<_>>();
    let type_params = generics
        .iter()
        .map(|param| tast::TastIdent(param.to_ident_name()))
        .collect::<Vec<_>>();
    let mut constraints = Vec::new();
    add_fn_constraints_from_bounds(
        env,
        diagnostics,
        &known_type_params,
        &type_params,
        bounds,
        &mut constraints,
    );
    add_type_predicates(
        env,
        diagnostics,
        &known_type_params,
        &type_params,
        predicates,
        &mut constraints,
        true,
    );
    let constraints = resolve_type_predicates_with_candidates(
        env,
        diagnostics,
        constraints,
        projection_candidates,
    );
    expand_implied_predicates(env, constraints)
}

fn build_method_constraints(
    env: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    all_generics: &[hir::HirIdent],
    impl_bounds: &[(hir::HirIdent, Vec<hir::TraitRef>)],
    impl_predicates: &[hir::Predicate],
    method_bounds: &[(hir::HirIdent, Vec<hir::TraitRef>)],
    method_predicates: &[hir::Predicate],
) -> Vec<env::TypePredicate> {
    let known_type_params = all_generics
        .iter()
        .map(|param| param.to_ident_name())
        .collect::<HashSet<_>>();
    let mut constraints = Vec::new();
    let type_params = all_generics
        .iter()
        .map(|param| tast::TastIdent(param.to_ident_name()))
        .collect::<Vec<_>>();
    add_fn_constraints_from_bounds(
        env,
        diagnostics,
        &known_type_params,
        &type_params,
        impl_bounds,
        &mut constraints,
    );
    add_type_predicates(
        env,
        diagnostics,
        &known_type_params,
        &type_params,
        impl_predicates,
        &mut constraints,
        false,
    );
    add_fn_constraints_from_bounds(
        env,
        diagnostics,
        &known_type_params,
        &type_params,
        method_bounds,
        &mut constraints,
    );
    add_type_predicates(
        env,
        diagnostics,
        &known_type_params,
        &type_params,
        method_predicates,
        &mut constraints,
        false,
    );
    let constraints = resolve_type_predicates(env, diagnostics, constraints);
    expand_implied_predicates(env, constraints)
}

fn is_local_name(current_package: &str, name: &str) -> bool {
    if is_special_unqualified_package(current_package) {
        return !name.contains("::");
    }
    let Some(rest) = name.strip_prefix(&format!("{current_package}::")) else {
        return false;
    };
    !rest.contains("::")
}

fn is_local_nominal_type(current_package: &str, ty: &tast::Ty) -> bool {
    match ty {
        tast::Ty::TStruct { name } | tast::Ty::TEnum { name } => {
            is_local_name(current_package, name)
        }
        tast::Ty::TApp { ty, .. } => is_local_nominal_type(current_package, ty),
        tast::Ty::TSlice { .. }
        | tast::Ty::TVec { .. }
        | tast::Ty::TRef { .. }
        | tast::Ty::THashMap { .. } => current_package == BUILTIN_PACKAGE,
        _ => false,
    }
}

fn define_trait_impl(
    env: &mut PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    impl_block: &hir::ImplBlock,
    hir_trait_ref: &hir::TraitRef,
    hir_table: &hir::HirTable,
) {
    let impl_tparams = type_param_name_set(&impl_block.generics);
    let impl_generics_tast: Vec<tast::TastIdent> = impl_block
        .generics
        .iter()
        .map(|g| tast::TastIdent(g.to_ident_name()))
        .collect();
    let for_ty = tast::Ty::from_hir(env, &impl_block.for_type, &impl_generics_tast);
    let raw_trait_name = hir_trait_ref.name.to_ident_name();
    let trait_is_defined = super::util::resolve_trait_name(env, &raw_trait_name)
        .and_then(|(name, trait_env)| trait_env.trait_env.trait_defs.get(&name))
        .is_some();
    if !trait_is_defined {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!(
                "Trait {} is not defined, cannot implement it for {}",
                raw_trait_name,
                super::util::format_ty_for_diag(&for_ty)
            ),
        ));
        return;
    }
    let Some(trait_ref) =
        resolve_hir_trait_ref(env, diagnostics, hir_trait_ref, &impl_generics_tast)
    else {
        return;
    };
    let mut trait_ref = tast::TraitRef {
        name: trait_ref.name,
        args: trait_ref
            .args
            .iter()
            .map(|arg| instantiate_self_ty(arg, &for_ty))
            .collect(),
    };
    let impl_constraints = build_fn_constraints(
        env,
        diagnostics,
        &impl_block.generics,
        &impl_block.generic_bounds,
        &impl_block.predicates,
    );
    let mut projection_candidates = projection_candidates_from_predicates(env, &impl_constraints);
    trait_ref = resolve_trait_ref_projections(
        env,
        diagnostics,
        &trait_ref,
        &projection_candidates,
        type_expr_range(&impl_block.for_type),
    );
    validate_decl_ty(
        env,
        diagnostics,
        &for_ty,
        type_expr_range(&impl_block.for_type),
        &impl_tparams,
    );
    let mut constrained_params = super::type_ops::injective_type_params(&for_ty);
    for arg in &trait_ref.args {
        constrained_params.extend(super::type_ops::injective_type_params(arg));
    }
    let mut unconstrained_impl_params = impl_tparams
        .difference(&constrained_params)
        .cloned()
        .collect::<Vec<_>>();
    unconstrained_impl_params.sort();
    for param in &unconstrained_impl_params {
        diagnostics.push(
            Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Implementation type parameter {} is not constrained by type {}",
                    param,
                    super::util::format_ty_for_diag(&for_ty)
                ),
            )
            .with_range(type_expr_range(&impl_block.for_type)),
        );
    }
    let trait_name_str = trait_ref.name.0.clone();
    let Some((_, trait_env)) = super::util::resolve_trait_name(env, &trait_name_str) else {
        return;
    };
    let trait_def = trait_env.trait_env.trait_defs.get(&trait_name_str).cloned();
    let Some(trait_def) = trait_def else {
        super::util::push_ice(
            diagnostics,
            format!("trait def missing after resolution: {}", trait_name_str),
        );
        return;
    };
    let trait_local = is_local_name(&env.package, &trait_name_str);
    let type_local = is_local_nominal_type(&env.package, &for_ty);
    if !trait_local && !type_local {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!(
                "Impl violates orphan rule: trait {} and type {} are not local to package {}",
                trait_name_str,
                super::util::format_ty_for_diag(&for_ty),
                env.package
            ),
        ));
        return;
    }

    let key = env::TraitImplKey {
        trait_ref: trait_ref.clone(),
        for_ty: for_ty.clone(),
    };
    if env.current().trait_env.trait_impls.contains_key(&key) {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!(
                "Trait {} implementation for {} is already defined",
                trait_name_str,
                super::util::format_ty_for_diag(&for_ty)
            ),
        ));
        return;
    }

    let trait_method_names: HashSet<String> = trait_def.methods.keys().cloned().collect();

    let mut implemented_methods: HashSet<String> = HashSet::new();
    let mut impl_methods: IndexMap<String, env::FnScheme> = IndexMap::new();
    let mut impl_valid = unconstrained_impl_params.is_empty();
    projection_candidates.push(ProjectionCandidate {
        for_ty: tast::Ty::TStruct {
            name: "Self".to_string(),
        },
        trait_ref: trait_ref.clone(),
        associated_types: trait_def.associated_types.keys().cloned().collect(),
    });
    let mut implemented_associated_types = HashSet::new();
    let mut associated_types = IndexMap::new();
    for (name, hir_ty) in &impl_block.associated_types {
        let name = name.to_ident_name();
        if !trait_def.associated_types.contains_key(&name) {
            impl_valid = false;
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Associated type {} is not declared in trait {}",
                    name, trait_name_str
                ),
            ));
            continue;
        }
        if !implemented_associated_types.insert(name.clone()) {
            impl_valid = false;
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Associated type {} is bound multiple times in impl of trait {}",
                    name, trait_name_str
                ),
            ));
            continue;
        }
        let ty = tast::Ty::from_hir(env, hir_ty, &impl_generics_tast);
        let ty = resolve_ty_projections(
            env,
            diagnostics,
            &ty,
            &projection_candidates,
            type_expr_range(hir_ty),
        );
        let ty = instantiate_self_ty(&ty, &for_ty);
        validate_ty(
            env,
            diagnostics,
            &ty,
            type_expr_range(hir_ty),
            &impl_tparams,
        );
        associated_types.insert(name, ty);
    }
    for name in trait_def.associated_types.keys() {
        if !implemented_associated_types.contains(name) {
            impl_valid = false;
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Trait {} implementation for {} is missing associated type {}",
                    trait_name_str,
                    super::util::format_ty_for_diag(&for_ty),
                    name
                ),
            ));
        }
    }
    let associated_types = associated_types
        .iter()
        .map(|(name, ty)| {
            (
                name.clone(),
                normalize_impl_associated_types(ty, &trait_ref, &for_ty, &associated_types),
            )
        })
        .collect::<IndexMap<_, _>>();
    for (name, ty) in &associated_types {
        if contains_impl_associated_projection(ty, &trait_ref, &for_ty) {
            impl_valid = false;
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Associated type {} has a cyclic definition in impl of trait {}",
                    name, trait_name_str
                ),
            ));
        }
    }

    for m in impl_block.methods.iter() {
        let m = match hir_table.def(*m) {
            hir::Def::Fn(func) => func,
            _ => continue,
        };
        let method_name_str = m.name.clone();

        if !trait_method_names.contains(&method_name_str) {
            impl_valid = false;
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Method {} is not declared in trait {}",
                    method_name_str, trait_name_str
                ),
            ));
            continue;
        }

        if !implemented_methods.insert(method_name_str.clone()) {
            impl_valid = false;
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Method {} implemented multiple times in impl of trait {}",
                    method_name_str, trait_name_str
                ),
            ));
            continue;
        }

        if !m.generics.is_empty() {
            impl_valid = false;
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Trait method implementation {}::{} cannot declare type parameters",
                    trait_name_str, method_name_str
                ),
            ));
            continue;
        }

        let trait_sig = trait_env
            .lookup_trait_method_scheme(&trait_ref, &tast::TastIdent::new(&method_name_str))
            .map(|scheme| scheme.ty);
        let Some(trait_sig) = trait_sig else {
            impl_valid = false;
            super::util::push_ice(
                diagnostics,
                format!(
                    "trait method signature missing: {}::{}",
                    trait_name_str, method_name_str
                ),
            );
            continue;
        };

        let all_generics = impl_block.generics.clone();
        let tparam_names = type_param_name_set(&all_generics);
        let all_generics_tast: Vec<tast::TastIdent> = all_generics
            .iter()
            .map(|g| tast::TastIdent(g.to_ident_name()))
            .collect();
        let params = m
            .params
            .iter()
            .map(|(_, hir_ty)| {
                let ty = tast::Ty::from_hir(env, hir_ty, &all_generics_tast);
                let ty = resolve_ty_projections(
                    env,
                    diagnostics,
                    &ty,
                    &projection_candidates,
                    type_expr_range(hir_ty),
                );
                validate_ty(
                    env,
                    diagnostics,
                    &ty,
                    type_expr_range(hir_ty),
                    &tparam_names,
                );
                let ty = instantiate_self_ty(&ty, &for_ty);
                normalize_impl_associated_types(&ty, &trait_ref, &for_ty, &associated_types)
            })
            .collect::<Vec<_>>();
        let ret = match &m.ret_ty {
            Some(hir_ty) => {
                let ret = tast::Ty::from_hir(env, hir_ty, &all_generics_tast);
                let ret = resolve_ty_projections(
                    env,
                    diagnostics,
                    &ret,
                    &projection_candidates,
                    type_expr_range(hir_ty),
                );
                validate_ty(
                    env,
                    diagnostics,
                    &ret,
                    type_expr_range(hir_ty),
                    &tparam_names,
                );
                let ret = instantiate_self_ty(&ret, &for_ty);
                normalize_impl_associated_types(&ret, &trait_ref, &for_ty, &associated_types)
            }
            None => tast::Ty::TUnit,
        };

        let impl_method_ty = tast::Ty::TFunc {
            params: params.clone(),
            ret_ty: Box::new(ret.clone()),
        };

        let expected_method_ty = instantiate_trait_method_ty(&trait_sig, &for_ty);
        let expected_method_ty = normalize_impl_associated_types(
            &expected_method_ty,
            &trait_ref,
            &for_ty,
            &associated_types,
        );

        let mut method_ok = true;
        match (&expected_method_ty, &impl_method_ty) {
            (
                tast::Ty::TFunc {
                    params: expected_params,
                    ret_ty: expected_ret,
                },
                tast::Ty::TFunc {
                    params: impl_params,
                    ret_ty: impl_ret,
                },
            ) => {
                if expected_params.len() != impl_params.len() {
                    diagnostics.push(Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Trait {}::{} expects {} parameters but impl has {}",
                            trait_name_str,
                            method_name_str,
                            expected_params.len(),
                            impl_params.len()
                        ),
                    ));
                    method_ok = false;
                }

                for (idx, (expected, actual)) in
                    expected_params.iter().zip(impl_params.iter()).enumerate()
                {
                    if expected != actual {
                        diagnostics.push(Diagnostic::new(
                            Stage::Typer,
                            Severity::Error,
                            format!(
                                "Trait {}::{} parameter {} expected type {} but found {}",
                                trait_name_str,
                                method_name_str,
                                idx,
                                super::util::format_ty_for_diag(expected),
                                super::util::format_ty_for_diag(actual)
                            ),
                        ));
                        method_ok = false;
                    }
                }

                if **expected_ret != **impl_ret {
                    diagnostics.push(Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Trait {}::{} expected return type {} but found {}",
                            trait_name_str,
                            method_name_str,
                            super::util::format_ty_for_diag(expected_ret),
                            super::util::format_ty_for_diag(impl_ret)
                        ),
                    ));
                    method_ok = false;
                }
            }
            _ => {
                diagnostics.push(Diagnostic::new(
                    Stage::Typer,
                    Severity::Error,
                    format!(
                        "Trait {}::{} does not have a function type signature",
                        trait_name_str, method_name_str
                    ),
                ));
                method_ok = false;
            }
        }

        if method_ok {
            let type_params: Vec<String> = all_generics.iter().map(|g| g.to_ident_name()).collect();
            let constraints = build_fn_constraints(
                env,
                diagnostics,
                &all_generics,
                &m.generic_bounds,
                &m.predicates,
            );
            impl_methods.insert(
                method_name_str.clone(),
                env::FnScheme {
                    type_params,
                    constraints,
                    ty: impl_method_ty,
                    body: CallableBody::Goml,
                },
            );
        } else {
            impl_valid = false;
        }
    }

    for method_name in trait_method_names.iter() {
        if !implemented_methods.contains(method_name) {
            impl_valid = false;
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Trait {} implementation for {} is missing method {}",
                    trait_name_str,
                    super::util::format_ty_for_diag(&for_ty),
                    method_name
                ),
            ));
        }
    }

    let mut candidate = env::ImplDef {
        params: impl_generics_tast,
        constraints: impl_constraints,
        associated_types,
        methods: impl_methods,
        valid: impl_valid,
        origin: type_expr_range(&impl_block.for_type),
    };
    let overlap = candidate
        .valid
        .then(|| {
            env.visible_trait_impls(&trait_name_str)
                .into_iter()
                .filter(|(_, _, _, _, existing)| existing.valid)
                .find(|(_, _, existing_trait_ref, existing_ty, existing)| {
                    super::traits::coherence::impls_overlap(
                        env,
                        existing_trait_ref,
                        existing_ty,
                        existing,
                        &trait_ref,
                        &for_ty,
                        &candidate,
                    )
                })
        })
        .flatten();
    if let Some((package, index, _, _, existing)) = overlap {
        let previous = existing.origin.map_or_else(
            || format!("{}#{}", package, index),
            |origin| {
                format!(
                    "{}#{} at source range {}..{}",
                    package,
                    index,
                    u32::from(origin.start()),
                    u32::from(origin.end())
                )
            },
        );
        diagnostics.push(
            Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Trait {} implementation for {} overlaps with implementation {}",
                    trait_name_str,
                    super::util::format_ty_for_diag(&for_ty),
                    previous
                ),
            )
            .with_range(type_expr_range(&impl_block.for_type)),
        );
        candidate.valid = false;
    }

    env.current_mut()
        .trait_env
        .trait_impls
        .insert(key, candidate);
}

fn define_inherent_impl(
    env: &mut PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    impl_block: &hir::ImplBlock,
    hir_table: &hir::HirTable,
) {
    if !impl_block.associated_types.is_empty() {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            "Associated types can only be bound in trait implementations".to_string(),
        ));
    }
    // Combine impl generics with method generics for type parameter validation
    let impl_tparams = type_param_name_set(&impl_block.generics);
    let impl_generics_tast: Vec<tast::TastIdent> = impl_block
        .generics
        .iter()
        .map(|g| tast::TastIdent(g.to_ident_name()))
        .collect();
    let for_ty = tast::Ty::from_hir(env, &impl_block.for_type, &impl_generics_tast);
    validate_decl_ty(
        env,
        diagnostics,
        &for_ty,
        type_expr_range(&impl_block.for_type),
        &impl_tparams,
    );
    if env.extern_capability != ExternCapability::Core
        && !is_local_nominal_type(&env.package, &for_ty)
    {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!(
                "Inherent impl for non-local type {} is not allowed",
                super::util::format_ty_for_diag(&for_ty)
            ),
        ));
        return;
    }

    let key = if !impl_block.generics.is_empty() {
        let Some(constr_name) = super::util::try_constr_name(&for_ty) else {
            super::util::push_ice(
                diagnostics,
                format!(
                    "Expected constructor type in inherent impl, got {}",
                    super::util::format_ty_for_diag(&for_ty)
                ),
            );
            return;
        };
        env::InherentImplKey::Constr(constr_name)
    } else {
        env::InherentImplKey::Exact(for_ty.clone())
    };
    let mut methods_to_add: IndexMap<String, env::FnScheme> = IndexMap::new();

    let mut implemented_methods: HashSet<String> = HashSet::new();
    for m in impl_block.methods.iter() {
        let m = match hir_table.def(*m) {
            hir::Def::Fn(func) => func,
            _ => continue,
        };
        let method_name_str = m.name.clone();

        if !implemented_methods.insert(method_name_str.clone()) {
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Method {} implemented multiple times in impl for {}",
                    method_name_str,
                    super::util::format_ty_for_diag(&for_ty)
                ),
            ));
            continue;
        }

        // Combine impl generics and method generics
        let mut all_generics = impl_block.generics.clone();
        all_generics.extend(m.generics.clone());
        let tparam_names = type_param_name_set(&all_generics);
        let all_generics_tast: Vec<tast::TastIdent> = all_generics
            .iter()
            .map(|g| tast::TastIdent(g.to_ident_name()))
            .collect();

        let params = m
            .params
            .iter()
            .map(|(_, hir_ty)| {
                let ty = tast::Ty::from_hir(env, hir_ty, &all_generics_tast);
                validate_ty(
                    env,
                    diagnostics,
                    &ty,
                    type_expr_range(hir_ty),
                    &tparam_names,
                );
                instantiate_self_ty(&ty, &for_ty)
            })
            .collect::<Vec<_>>();
        let ret = match &m.ret_ty {
            Some(hir_ty) => {
                let ret = tast::Ty::from_hir(env, hir_ty, &all_generics_tast);
                validate_ty(
                    env,
                    diagnostics,
                    &ret,
                    type_expr_range(hir_ty),
                    &tparam_names,
                );
                instantiate_self_ty(&ret, &for_ty)
            }
            None => tast::Ty::TUnit,
        };

        let impl_method_ty = tast::Ty::TFunc {
            params: params.clone(),
            ret_ty: Box::new(ret.clone()),
        };

        let type_params: Vec<String> = all_generics.iter().map(|g| g.to_ident_name()).collect();
        let constraints = build_method_constraints(
            env,
            diagnostics,
            &all_generics,
            &impl_block.generic_bounds,
            &impl_block.predicates,
            &m.generic_bounds,
            &m.predicates,
        );

        methods_to_add.insert(
            method_name_str,
            env::FnScheme {
                type_params,
                constraints,
                ty: impl_method_ty,
                body: CallableBody::Goml,
            },
        );
        if m.visibility == ::ast::ast::Visibility::Public {
            env.current_mut()
                .trait_env
                .public_inherent_methods
                .entry(key.clone())
                .or_default()
                .push(m.name.clone());
        }
    }

    // Insert or extend the impl def
    let impl_def = env
        .current_mut()
        .trait_env
        .inherent_impls
        .entry(key)
        .or_default();
    impl_def.methods.extend(methods_to_add);
}

fn define_function(env: &mut PackageTypeEnv, diagnostics: &mut Diagnostics, func: &hir::Fn) {
    let name = func.name.clone();
    if name == "main" {
        if !func.params.is_empty() {
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                "main function must not have parameters".to_string(),
            ));
        }
        if !func.generics.is_empty() {
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                "main function must not have type parameters".to_string(),
            ));
        }
    }
    let tparam_names = type_param_name_set(&func.generics);
    let generics_tast: Vec<tast::TastIdent> = func
        .generics
        .iter()
        .map(|g| tast::TastIdent(g.to_ident_name()))
        .collect();
    let params = func
        .params
        .iter()
        .map(|(_, hir_ty)| {
            let ty = tast::Ty::from_hir(env, hir_ty, &generics_tast);
            validate_decl_ty(
                env,
                diagnostics,
                &ty,
                type_expr_range(hir_ty),
                &tparam_names,
            );
            ty
        })
        .collect::<Vec<_>>();
    let ret = match &func.ret_ty {
        Some(hir_ty) => {
            let ret = tast::Ty::from_hir(env, hir_ty, &generics_tast);
            validate_decl_ty(
                env,
                diagnostics,
                &ret,
                type_expr_range(hir_ty),
                &tparam_names,
            );
            ret
        }
        None => tast::Ty::TUnit,
    };
    let fn_constraints = build_fn_constraints(
        env,
        diagnostics,
        &func.generics,
        &func.generic_bounds,
        &func.predicates,
    );
    let projection_candidates = projection_candidates_from_predicates(env, &fn_constraints);
    let params = params
        .iter()
        .map(|ty| resolve_ty_projections(env, diagnostics, ty, &projection_candidates, None))
        .collect();
    let ret = resolve_ty_projections(env, diagnostics, &ret, &projection_candidates, None);
    env.current_mut().value_env.funcs.insert(
        name,
        FnScheme {
            type_params: func.generics.iter().map(|g| g.to_ident_name()).collect(),
            constraints: fn_constraints,
            ty: tast::Ty::TFunc {
                params,
                ret_ty: Box::new(ret),
            },
            body: CallableBody::Goml,
        },
    );
}

fn define_extern_fn(env: &mut PackageTypeEnv, diagnostics: &mut Diagnostics, ext: &hir::ExternFn) {
    let name = ext.name.to_ident_name();
    let local_name = name.rsplit("::").next().unwrap_or(&name);
    let body = match callable_body_from_attributes(ext.attrs.iter().map(|attr| attr.text.as_str()))
    {
        Ok(body) => body,
        Err(message) => {
            diagnostics.push(Diagnostic::new(Stage::Typer, Severity::Error, message));
            return;
        }
    };
    if !env.extern_capability.permits(body) {
        diagnostics.push(Diagnostic::new(
            Stage::Typer,
            Severity::Error,
            format!("extern {local_name} is not permitted in this source"),
        ));
        return;
    }
    let tparams: Vec<tast::TastIdent> = ext
        .generics
        .iter()
        .map(|g| tast::TastIdent(g.to_ident_name()))
        .collect();
    let tparam_names = type_param_name_set(&ext.generics);
    let params = ext
        .params
        .iter()
        .map(|(_, hir_ty)| {
            let ty = tast::Ty::from_hir(env, hir_ty, &tparams);
            validate_decl_ty(
                env,
                diagnostics,
                &ty,
                type_expr_range(hir_ty),
                &tparam_names,
            );
            ty
        })
        .collect::<Vec<_>>();
    let ret_ty = match &ext.ret_ty {
        Some(hir_ty) => {
            let ty = tast::Ty::from_hir(env, hir_ty, &tparams);
            validate_decl_ty(
                env,
                diagnostics,
                &ty,
                type_expr_range(hir_ty),
                &tparam_names,
            );
            ty
        }
        None => tast::Ty::TUnit,
    };
    let fn_constraints = build_fn_constraints(
        env,
        diagnostics,
        &ext.generics,
        &ext.generic_bounds,
        &ext.predicates,
    );
    let projection_candidates = projection_candidates_from_predicates(env, &fn_constraints);
    let params = params
        .iter()
        .map(|ty| resolve_ty_projections(env, diagnostics, ty, &projection_candidates, None))
        .collect::<Vec<_>>();
    let ret_ty = resolve_ty_projections(env, diagnostics, &ret_ty, &projection_candidates, None);
    let type_params = ext
        .generics
        .iter()
        .map(|generic| generic.to_ident_name())
        .collect::<Vec<_>>();
    let ty = tast::Ty::TFunc {
        params,
        ret_ty: Box::new(ret_ty),
    };
    let contract_constraints = fn_constraints
        .iter()
        .filter_map(|predicate| {
            let env::TypePredicate::Trait { for_ty, trait_ref } = predicate else {
                return None;
            };
            let tast::Ty::TParam { name } = for_ty else {
                return None;
            };
            Some((name.clone(), trait_ref.name.0.clone()))
        })
        .collect::<Vec<_>>();
    if let Err(message) =
        validate_callable_signature(body, &type_params, &contract_constraints, &ty)
    {
        diagnostics.push(Diagnostic::new(Stage::Typer, Severity::Error, message));
        return;
    }

    env.current_mut().value_env.funcs.insert(
        ext.name.to_ident_name(),
        FnScheme {
            type_params,
            constraints: fn_constraints,
            ty,
            body,
        },
    );
}

fn instantiate_trait_method_ty(ty: &tast::Ty, self_ty: &tast::Ty) -> tast::Ty {
    instantiate_self_ty(ty, self_ty)
}

pub(crate) fn collect_typedefs(
    env: &mut PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    hir: &hir::PackageHir,
    hir_table: &hir::HirTable,
) {
    validate_nominal_type_names(diagnostics, hir, hir_table);
    validate_top_level_function_names(diagnostics, hir, hir_table);
    validate_top_level_type_parameter_names(diagnostics, hir, hir_table);
    predeclare_types(env.current_mut(), hir, hir_table);

    for item in hir.toplevels.iter() {
        match hir_table.def(*item) {
            hir::Def::EnumDef(enum_def) => define_enum(env, diagnostics, enum_def),
            hir::Def::StructDef(struct_def) => define_struct(env, diagnostics, struct_def),
            hir::Def::TraitDef(trait_def) => define_trait(env, diagnostics, trait_def),
            _ => {}
        }
    }
    validate_supertrait_cycles(env, diagnostics);

    for item in hir.toplevels.iter() {
        match hir_table.def(*item) {
            hir::Def::ImplBlock(impl_block) => {
                if let Some(trait_ref) = &impl_block.trait_ref {
                    define_trait_impl(env, diagnostics, impl_block, trait_ref, hir_table);
                } else {
                    define_inherent_impl(env, diagnostics, impl_block, hir_table);
                }
            }
            hir::Def::Fn(func) => define_function(env, diagnostics, func),
            hir::Def::ExternFn(ext) => define_extern_fn(env, diagnostics, ext),
            _ => {}
        }
    }
    validate_trait_impl_requirements(env, diagnostics, hir_table);
    validate_trait_impl_coherence(env, diagnostics);
    validate_no_infinite_size_structs(env, diagnostics);
}

fn validate_supertrait_cycles(env: &PackageTypeEnv, diagnostics: &mut Diagnostics) {
    fn visit(
        name: &str,
        graph: &HashMap<String, Vec<String>>,
        visiting: &mut Vec<String>,
        visited: &mut HashSet<String>,
        reported: &mut HashSet<Vec<String>>,
        diagnostics: &mut Diagnostics,
    ) {
        if visited.contains(name) {
            return;
        }
        if let Some(start) = visiting.iter().position(|active| active == name) {
            let mut cycle = visiting[start..].to_vec();
            cycle.push(name.to_string());
            if reported.insert(cycle.clone()) {
                diagnostics.push(Diagnostic::new(
                    Stage::Typer,
                    Severity::Error,
                    format!("Supertrait cycle detected: {}", cycle.join(" -> ")),
                ));
            }
            return;
        }
        visiting.push(name.to_string());
        if let Some(supertraits) = graph.get(name) {
            for supertrait in supertraits {
                if graph.contains_key(supertrait) {
                    visit(supertrait, graph, visiting, visited, reported, diagnostics);
                }
            }
        }
        let _ = visiting.pop();
        visited.insert(name.to_string());
    }

    let graph = env
        .current()
        .trait_env
        .trait_defs
        .iter()
        .map(|(name, definition)| {
            (
                name.clone(),
                definition
                    .supertraits
                    .iter()
                    .map(|supertrait| supertrait.name.0.clone())
                    .collect(),
            )
        })
        .collect::<HashMap<_, _>>();
    let mut visited = HashSet::new();
    let mut reported = HashSet::new();
    for name in graph.keys() {
        visit(
            name,
            &graph,
            &mut Vec::new(),
            &mut visited,
            &mut reported,
            diagnostics,
        );
    }
}

fn validate_trait_impl_requirements(
    env: &mut PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    hir_table: &hir::HirTable,
) {
    let impls = env
        .current()
        .trait_env
        .trait_impls
        .iter()
        .enumerate()
        .map(|(index, (key, definition))| (index, key.clone(), definition.clone()))
        .collect::<Vec<_>>();
    let mut invalid = HashSet::new();
    for (index, key, definition) in impls {
        if !definition.valid {
            continue;
        }
        let Some((_, trait_env)) = super::util::resolve_trait_name(env, &key.trait_ref.name.0)
        else {
            continue;
        };
        let Some(trait_def) = trait_env
            .trait_env
            .trait_defs
            .get(&key.trait_ref.name.0)
            .cloned()
        else {
            continue;
        };
        let substitution = trait_def
            .params
            .iter()
            .zip(key.trait_ref.args.iter())
            .map(|(param, arg)| (param.0.clone(), arg.clone()))
            .collect::<HashMap<_, _>>();
        let param_env = super::obligations::ParamEnv::from_predicates(&definition.constraints);
        let mut trait_solver = super::traits::solver::TraitSolver::new(env, &param_env);
        let mut typer = Typer::new(hir_table.clone());
        typer.param_type_aliases = predicate_type_aliases(&definition.constraints);
        typer.param_projection_aliases = predicate_projection_aliases(&definition.constraints);
        for supertrait in &trait_def.supertraits {
            let supertrait = super::type_ops::substitute_trait_ref(supertrait, &substitution);
            let supertrait = tast::TraitRef {
                name: supertrait.name,
                args: supertrait
                    .args
                    .iter()
                    .map(|arg| instantiate_self_ty(arg, &key.for_ty))
                    .map(|arg| {
                        normalize_impl_associated_types(
                            &arg,
                            &key.trait_ref,
                            &key.for_ty,
                            &definition.associated_types,
                        )
                    })
                    .collect(),
            };
            let goal = super::obligations::TraitGoal {
                trait_ref: supertrait.clone(),
                for_ty: key.for_ty.clone(),
            };
            if matches!(
                trait_solver.select(&mut typer, goal),
                super::traits::solver::SelectionResult::Unique(_)
            ) {
                continue;
            }
            invalid.insert(index);
            diagnostics.push(
                Diagnostic::new(
                    Stage::Typer,
                    Severity::Error,
                    format!(
                        "Trait {} implementation for {} requires supertrait {}",
                        key.trait_ref.name.0,
                        super::util::format_ty_for_diag(&key.for_ty),
                        super::util::format_trait_ref_for_diag(&supertrait)
                    ),
                )
                .with_range(definition.origin),
            );
        }
        for declared in &trait_def.predicates {
            let declared = super::type_ops::substitute_predicate(declared, &substitution);
            let declared = instantiate_predicate_self(&declared, &key.for_ty);
            let declared = match declared {
                env::TypePredicate::Trait { for_ty, trait_ref } => env::TypePredicate::Trait {
                    for_ty: normalize_impl_associated_types(
                        &for_ty,
                        &key.trait_ref,
                        &key.for_ty,
                        &definition.associated_types,
                    ),
                    trait_ref: tast::TraitRef {
                        name: trait_ref.name,
                        args: trait_ref
                            .args
                            .iter()
                            .map(|arg| {
                                normalize_impl_associated_types(
                                    arg,
                                    &key.trait_ref,
                                    &key.for_ty,
                                    &definition.associated_types,
                                )
                            })
                            .collect(),
                    },
                },
                env::TypePredicate::Equality { lhs, rhs } => env::TypePredicate::Equality {
                    lhs: normalize_impl_associated_types(
                        &lhs,
                        &key.trait_ref,
                        &key.for_ty,
                        &definition.associated_types,
                    ),
                    rhs: normalize_impl_associated_types(
                        &rhs,
                        &key.trait_ref,
                        &key.for_ty,
                        &definition.associated_types,
                    ),
                },
            };
            let satisfied = match &declared {
                env::TypePredicate::Trait { for_ty, trait_ref } => matches!(
                    trait_solver.select(
                        &mut typer,
                        super::obligations::TraitGoal {
                            trait_ref: trait_ref.clone(),
                            for_ty: for_ty.clone(),
                        },
                    ),
                    super::traits::solver::SelectionResult::Unique(_)
                ),
                env::TypePredicate::Equality { lhs, rhs } => {
                    normalize_selected_associated_types(&mut trait_solver, &mut typer, lhs)
                        == normalize_selected_associated_types(&mut trait_solver, &mut typer, rhs)
                }
            };
            if satisfied {
                continue;
            }
            invalid.insert(index);
            let requirement = match &declared {
                env::TypePredicate::Trait { for_ty, trait_ref } => format!(
                    "{}: {}",
                    super::util::format_ty_for_diag(for_ty),
                    super::util::format_trait_ref_for_diag(trait_ref)
                ),
                env::TypePredicate::Equality { lhs, rhs } => format!(
                    "{} == {}",
                    super::util::format_ty_for_diag(lhs),
                    super::util::format_ty_for_diag(rhs)
                ),
            };
            diagnostics.push(
                Diagnostic::new(
                    Stage::Typer,
                    Severity::Error,
                    format!(
                        "Trait {} implementation for {} does not satisfy declared requirement {}",
                        key.trait_ref.name.0,
                        super::util::format_ty_for_diag(&key.for_ty),
                        requirement
                    ),
                )
                .with_range(definition.origin),
            );
        }
        for (name, associated) in &trait_def.associated_types {
            let Some(binding) = definition.associated_types.get(name) else {
                continue;
            };
            for bound in &associated.bounds {
                let bound = super::type_ops::substitute_trait_ref(bound, &substitution);
                let bound = tast::TraitRef {
                    name: bound.name,
                    args: bound
                        .args
                        .iter()
                        .map(|arg| instantiate_self_ty(arg, &key.for_ty))
                        .map(|arg| {
                            normalize_impl_associated_types(
                                &arg,
                                &key.trait_ref,
                                &key.for_ty,
                                &definition.associated_types,
                            )
                        })
                        .collect(),
                };
                let binding = normalize_impl_associated_types(
                    binding,
                    &key.trait_ref,
                    &key.for_ty,
                    &definition.associated_types,
                );
                let goal = super::obligations::TraitGoal {
                    trait_ref: bound.clone(),
                    for_ty: binding.clone(),
                };
                if matches!(
                    trait_solver.select(&mut typer, goal),
                    super::traits::solver::SelectionResult::Unique(_)
                ) {
                    continue;
                }
                invalid.insert(index);
                diagnostics.push(
                    Diagnostic::new(
                        Stage::Typer,
                        Severity::Error,
                        format!(
                            "Associated type {} = {} does not satisfy bound {} in impl of {}",
                            name,
                            super::util::format_ty_for_diag(&binding),
                            super::util::format_trait_ref_for_diag(&bound),
                            key.trait_ref.name.0
                        ),
                    )
                    .with_range(definition.origin),
                );
            }
        }
    }
    for index in invalid {
        if let Some((_, definition)) = env.current_mut().trait_env.trait_impls.get_index_mut(index)
        {
            definition.valid = false;
        }
    }
}

fn validate_trait_impl_coherence(env: &mut PackageTypeEnv, diagnostics: &mut Diagnostics) {
    let impls = env
        .current()
        .trait_env
        .trait_impls
        .iter()
        .enumerate()
        .map(|(index, (key, definition))| (index, key.clone(), definition.clone()))
        .collect::<Vec<_>>();
    let mut invalid = HashSet::new();
    for (index, key, definition) in &impls {
        if !definition.valid || invalid.contains(index) {
            continue;
        }
        let overlap = env
            .visible_trait_impls(&key.trait_ref.name.0)
            .into_iter()
            .filter(|(package, other_index, _, _, other)| {
                if !other.valid {
                    return false;
                }
                package != &env.package || (other_index < index && !invalid.contains(other_index))
            })
            .find(|(_, _, other_trait_ref, other_ty, other)| {
                super::traits::coherence::impls_overlap(
                    env,
                    other_trait_ref,
                    other_ty,
                    other,
                    &key.trait_ref,
                    &key.for_ty,
                    definition,
                )
            });
        let Some((package, other_index, _, _, other)) = overlap else {
            continue;
        };
        let previous = other.origin.map_or_else(
            || format!("{}#{}", package, other_index),
            |origin| {
                format!(
                    "{}#{} at source range {}..{}",
                    package,
                    other_index,
                    u32::from(origin.start()),
                    u32::from(origin.end())
                )
            },
        );
        diagnostics.push(
            Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!(
                    "Trait {} implementation for {} overlaps with implementation {}",
                    key.trait_ref.name.0,
                    super::util::format_ty_for_diag(&key.for_ty),
                    previous
                ),
            )
            .with_range(definition.origin),
        );
        invalid.insert(*index);
    }
    for index in invalid {
        if let Some((_, definition)) = env.current_mut().trait_env.trait_impls.get_index_mut(index)
        {
            definition.valid = false;
        }
    }
}

fn validate_no_infinite_size_structs(env: &PackageTypeEnv, diagnostics: &mut Diagnostics) {
    let structs = env.current().structs();
    for (name, def) in structs.iter() {
        let args = def
            .generics
            .iter()
            .map(|param| tast::Ty::TParam {
                name: param.0.clone(),
            })
            .collect::<Vec<_>>();
        let mut active = Vec::new();
        if has_infinite_size(structs, &name.0, &args, &mut active) {
            diagnostics.push(
                Diagnostic::new(
                    Stage::Typer,
                    Severity::Error,
                    format!(
                        "Struct {} has infinite size due to recursive field; use Ref[{}] for indirection",
                        name.0, name.0
                    ),
                )
            );
        }
    }
}

fn has_infinite_size(
    structs: &IndexMap<tast::TastIdent, env::StructDef>,
    target: &str,
    args: &[tast::Ty],
    active: &mut Vec<(String, Vec<tast::Ty>)>,
) -> bool {
    if recursive_struct_specialization(target, args, active) {
        return true;
    }
    let Some(def) = structs.get(&tast::TastIdent(target.to_string())) else {
        return false;
    };
    if def.generics.len() != args.len() {
        return false;
    }
    let mut subst = HashMap::new();
    for (param, arg) in def.generics.iter().zip(args.iter()) {
        subst.insert(param.0.clone(), arg.clone());
    }
    active.push((target.to_string(), args.to_vec()));
    for (_, field_ty) in &def.fields {
        let field_ty = substitute_ty_params(field_ty, &subst);
        if ty_contains_inline_struct(structs, &field_ty, active) {
            let _ = active.pop();
            return true;
        }
    }
    let _ = active.pop();
    false
}

fn recursive_struct_specialization(
    target: &str,
    args: &[tast::Ty],
    active: &[(String, Vec<tast::Ty>)],
) -> bool {
    active.iter().rev().any(|(active_name, active_args)| {
        active_name == target
            && (active_args == args
                || active_args
                    .iter()
                    .zip(args.iter())
                    .any(|(old_ty, new_ty)| ty_contains_proper_subterm(new_ty, old_ty)))
    })
}

fn ty_contains_proper_subterm(ty: &tast::Ty, needle: &tast::Ty) -> bool {
    match ty {
        tast::Ty::TTuple { typs } => typs
            .iter()
            .any(|item| item == needle || ty_contains_proper_subterm(item, needle)),
        tast::Ty::TApp { ty, args } => {
            ty.as_ref() == needle
                || ty_contains_proper_subterm(ty, needle)
                || args
                    .iter()
                    .any(|arg| arg == needle || ty_contains_proper_subterm(arg, needle))
        }
        tast::Ty::TArray { elem, .. }
        | tast::Ty::TSlice { elem }
        | tast::Ty::TVec { elem }
        | tast::Ty::TRef { elem } => {
            elem.as_ref() == needle || ty_contains_proper_subterm(elem, needle)
        }
        tast::Ty::THashMap { key, value } => {
            key.as_ref() == needle
                || ty_contains_proper_subterm(key, needle)
                || value.as_ref() == needle
                || ty_contains_proper_subterm(value, needle)
        }
        tast::Ty::TFunc { params, ret_ty } => {
            params
                .iter()
                .any(|param| param == needle || ty_contains_proper_subterm(param, needle))
                || ret_ty.as_ref() == needle
                || ty_contains_proper_subterm(ret_ty, needle)
        }
        _ => false,
    }
}

fn ty_contains_inline_struct(
    structs: &IndexMap<tast::TastIdent, env::StructDef>,
    ty: &tast::Ty,
    active: &mut Vec<(String, Vec<tast::Ty>)>,
) -> bool {
    match ty {
        tast::Ty::TStruct { name, .. } => has_infinite_size(structs, name, &[], active),
        tast::Ty::TApp { .. } => decompose_struct_type(ty)
            .is_some_and(|(name, args)| has_infinite_size(structs, &name, &args, active)),
        tast::Ty::TTuple { typs } => typs
            .iter()
            .any(|t| ty_contains_inline_struct(structs, t, active)),
        tast::Ty::TArray { elem, .. } => ty_contains_inline_struct(structs, elem, active),
        _ => false,
    }
}

pub fn check_file(
    hir: hir::PackageHir,
    hir_table: name_resolution::HirTable,
) -> (tast::File, env::GlobalTypeEnv, Diagnostics) {
    check_file_with_env(
        hir,
        hir_table,
        env::GlobalTypeEnv::new(),
        builtins::builtin_env(),
        ROOT_PACKAGE,
        HashMap::new(),
    )
}

pub fn check_file_with_env(
    hir: hir::PackageHir,
    hir_table: name_resolution::HirTable,
    genv: env::GlobalTypeEnv,
    builtins: env::GlobalTypeEnv,
    package: &str,
    deps: HashMap<String, env::GlobalTypeEnv>,
) -> (tast::File, env::GlobalTypeEnv, Diagnostics) {
    check_file_with_env_inner(
        hir,
        hir_table,
        genv,
        builtins,
        package,
        deps,
        ExternCapability::None,
    )
}

pub fn check_file_with_env_capability(
    hir: hir::PackageHir,
    hir_table: name_resolution::HirTable,
    genv: env::GlobalTypeEnv,
    builtins: env::GlobalTypeEnv,
    package: &str,
    deps: HashMap<String, env::GlobalTypeEnv>,
    capability: ExternCapability,
) -> (tast::File, env::GlobalTypeEnv, Diagnostics) {
    check_file_with_env_inner(hir, hir_table, genv, builtins, package, deps, capability)
}

fn check_file_with_env_inner(
    hir: hir::PackageHir,
    hir_table: name_resolution::HirTable,
    genv: env::GlobalTypeEnv,
    builtins: env::GlobalTypeEnv,
    package: &str,
    deps: HashMap<String, env::GlobalTypeEnv>,
    capability: ExternCapability,
) -> (tast::File, env::GlobalTypeEnv, Diagnostics) {
    let TypecheckedPackage {
        hir,
        mut typer,
        genv,
        mut diagnostics,
    } = typecheck_package(hir, hir_table, genv, builtins, package, deps, capability);
    let file = crate::typer::tast_builder::build_file(
        &genv,
        &hir,
        &typer.hir_table,
        typer.results.results(),
    );
    let file = subst_file(&mut typer, &mut diagnostics, file);

    (file, genv.current, diagnostics)
}

struct TypecheckedPackage {
    hir: hir::PackageHir,
    typer: Typer,
    genv: PackageTypeEnv,
    diagnostics: Diagnostics,
}

fn typecheck_package(
    hir: hir::PackageHir,
    hir_table: name_resolution::HirTable,
    genv: env::GlobalTypeEnv,
    builtins: env::GlobalTypeEnv,
    package: &str,
    deps: HashMap<String, env::GlobalTypeEnv>,
    capability: ExternCapability,
) -> TypecheckedPackage {
    let mut genv = env::PackageTypeEnv::new(package.to_string(), builtins, genv, deps)
        .with_extern_capability(capability);
    let mut typer = Typer::new(hir_table);
    let mut diagnostics = Diagnostics::new();
    let package_source = hir
        .toplevels
        .iter()
        .filter_map(|item| typer.hir_table.def_source(*item))
        .next()
        .filter(|source| {
            hir.toplevels
                .iter()
                .filter_map(|item| typer.hir_table.def_source(*item))
                .all(|candidate| candidate == *source)
        })
        .map(std::path::Path::to_path_buf);
    if let Some(source) = package_source {
        diagnostics.set_source(source);
    }
    collect_typedefs(&mut genv, &mut diagnostics, &hir, &typer.hir_table);
    let in_scope_traits = build_in_scope_traits(&genv, &hir, &mut diagnostics);
    for item in hir.toplevels.iter() {
        if let Some(source) = typer
            .hir_table
            .def_source(*item)
            .map(std::path::Path::to_path_buf)
        {
            diagnostics.set_source(source);
        } else {
            diagnostics.clear_source();
        }
        match typer.hir_table.def(*item).clone() {
            hir::Def::ImplBlock(impl_block) => typecheck_impl_block(
                &genv,
                &mut typer,
                &mut diagnostics,
                &impl_block,
                &in_scope_traits,
            ),
            hir::Def::Fn(func) => {
                typecheck_fn(&genv, &mut typer, &mut diagnostics, &func, &in_scope_traits)
            }
            hir::Def::EnumDef(..)
            | hir::Def::StructDef(..)
            | hir::Def::TraitDef(..)
            | hir::Def::ExternFn(..) => {}
        }
    }
    diagnostics.clear_source();
    let mut results = std::mem::replace(
        &mut typer.results,
        crate::typer::results::TypeckResultsBuilder::new(&typer.hir_table),
    );
    results.finalize_types(&mut typer);
    typer.results = results;
    TypecheckedPackage {
        hir,
        typer,
        genv,
        diagnostics,
    }
}

pub fn check_file_with_env_and_results(
    hir: hir::PackageHir,
    hir_table: name_resolution::HirTable,
    genv: env::GlobalTypeEnv,
    builtins: env::GlobalTypeEnv,
    package: &str,
    deps: HashMap<String, env::GlobalTypeEnv>,
) -> (
    name_resolution::HirTable,
    crate::typer::results::TypeckResults,
    env::GlobalTypeEnv,
    Diagnostics,
) {
    let TypecheckedPackage {
        typer,
        genv,
        diagnostics,
        ..
    } = typecheck_package(
        hir,
        hir_table,
        genv,
        builtins,
        package,
        deps,
        ExternCapability::None,
    );
    let Typer {
        hir_table, results, ..
    } = typer;

    (hir_table, results.finish(), genv.current, diagnostics)
}

pub fn check_file_with_env_tast_and_results(
    hir: hir::PackageHir,
    hir_table: name_resolution::HirTable,
    genv: env::GlobalTypeEnv,
    builtins: env::GlobalTypeEnv,
    package: &str,
    deps: HashMap<String, env::GlobalTypeEnv>,
) -> (
    tast::File,
    name_resolution::HirTable,
    crate::typer::results::TypeckResults,
    env::GlobalTypeEnv,
    Diagnostics,
) {
    let TypecheckedPackage {
        hir,
        mut typer,
        genv,
        mut diagnostics,
    } = typecheck_package(
        hir,
        hir_table,
        genv,
        builtins,
        package,
        deps,
        ExternCapability::None,
    );
    let file = crate::typer::tast_builder::build_file(
        &genv,
        &hir,
        &typer.hir_table,
        typer.results.results(),
    );
    let file = subst_file(&mut typer, &mut diagnostics, file);
    let Typer {
        hir_table, results, ..
    } = typer;

    (file, hir_table, results.finish(), genv.current, diagnostics)
}

fn subst_file(typer: &mut Typer, diagnostics: &mut Diagnostics, file: tast::File) -> tast::File {
    let toplevels = file
        .toplevels
        .into_iter()
        .map(|item| match item {
            tast::Item::Fn(func) => tast::Item::Fn(tast::Fn {
                body: typer.subst_block(diagnostics, func.body),
                ..func
            }),
            tast::Item::ImplBlock(impl_block) => tast::Item::ImplBlock(tast::ImplBlock {
                methods: impl_block
                    .methods
                    .into_iter()
                    .map(|method| tast::Fn {
                        body: typer.subst_block(diagnostics, method.body),
                        ..method
                    })
                    .collect(),
                ..impl_block
            }),
        })
        .collect();
    tast::File { toplevels }
}

fn build_in_scope_traits(
    genv: &PackageTypeEnv,
    hir: &hir::PackageHir,
    diagnostics: &mut Diagnostics,
) -> Vec<tast::TastIdent> {
    let mut traits = genv
        .builtins()
        .trait_env
        .trait_defs
        .keys()
        .cloned()
        .map(tast::TastIdent)
        .collect::<Vec<_>>();
    for trait_name in genv.current().trait_env.trait_defs.keys() {
        traits.push(tast::TastIdent(trait_name.clone()));
    }
    for use_trait in hir.use_traits.iter() {
        let name = use_trait.display();
        if let Some(resolved) = resolve_trait_ident_or_report(genv, diagnostics, &name) {
            traits.push(resolved);
        }
    }
    traits.sort_by(|a, b| a.0.cmp(&b.0));
    traits.dedup_by(|a, b| a.0 == b.0);
    traits
}

fn init_trait_bounds(
    tparams: &[tast::TastIdent],
) -> indexmap::IndexMap<String, Vec<tast::TraitRef>> {
    let mut bounds = indexmap::IndexMap::new();
    for param in tparams.iter() {
        bounds.insert(param.0.clone(), Vec::new());
    }
    bounds
}

fn extend_trait_bounds(
    genv: &PackageTypeEnv,
    bounds: &mut indexmap::IndexMap<String, Vec<tast::TraitRef>>,
    generic_bounds: &[(hir::HirIdent, Vec<hir::TraitRef>)],
) {
    let type_params = bounds
        .keys()
        .cloned()
        .map(tast::TastIdent)
        .collect::<Vec<_>>();
    for (param, traits) in generic_bounds.iter() {
        let param_name = param.to_ident_name();
        let Some(out) = bounds.get_mut(&param_name) else {
            continue;
        };
        for trait_ref in traits.iter() {
            if let Some(resolved) = resolve_hir_trait_ref_silent(genv, trait_ref, &type_params) {
                out.push(resolved);
            }
        }
    }
}

fn resolve_trait_ident_or_report(
    genv: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    name: &str,
) -> Option<tast::TastIdent> {
    if let Some((resolved, _env)) = super::util::resolve_trait_name(genv, name) {
        return Some(tast::TastIdent(resolved));
    }
    diagnostics.push(Diagnostic::new(
        Stage::Typer,
        Severity::Error,
        format!("Unknown trait {}", name),
    ));
    None
}

fn normalize_trait_bounds(bounds: &mut indexmap::IndexMap<String, Vec<tast::TraitRef>>) {
    for traits in bounds.values_mut() {
        let mut unique = Vec::new();
        for trait_ref in std::mem::take(traits) {
            if !unique.contains(&trait_ref) {
                unique.push(trait_ref);
            }
        }
        *traits = unique;
    }
}

fn build_param_env_predicates(
    genv: &PackageTypeEnv,
    diagnostics: &mut Diagnostics,
    tparams: &[tast::TastIdent],
    bounds: &indexmap::IndexMap<String, Vec<tast::TraitRef>>,
    predicates: &[hir::Predicate],
    self_ty: Option<&tast::Ty>,
) -> Vec<env::TypePredicate> {
    let mut result = bounds
        .iter()
        .flat_map(|(name, traits)| {
            traits
                .iter()
                .cloned()
                .map(|trait_ref| env::TypePredicate::Trait {
                    for_ty: tast::Ty::TParam { name: name.clone() },
                    trait_ref,
                })
        })
        .collect::<Vec<_>>();
    for predicate in predicates {
        let predicate = match predicate {
            hir::Predicate::Trait { ty, trait_ref } => {
                let Some(trait_ref) = resolve_hir_trait_ref_silent(genv, trait_ref, tparams) else {
                    continue;
                };
                env::TypePredicate::Trait {
                    for_ty: tast::Ty::from_hir(genv, ty, tparams),
                    trait_ref,
                }
            }
            hir::Predicate::Equality { lhs, rhs } => env::TypePredicate::Equality {
                lhs: tast::Ty::from_hir(genv, lhs, tparams),
                rhs: tast::Ty::from_hir(genv, rhs, tparams),
            },
        };
        let predicate = match (predicate, self_ty) {
            (env::TypePredicate::Trait { for_ty, trait_ref }, Some(self_ty)) => {
                env::TypePredicate::Trait {
                    for_ty: instantiate_self_ty(&for_ty, self_ty),
                    trait_ref: tast::TraitRef {
                        name: trait_ref.name,
                        args: trait_ref
                            .args
                            .iter()
                            .map(|arg| instantiate_self_ty(arg, self_ty))
                            .collect(),
                    },
                }
            }
            (env::TypePredicate::Equality { lhs, rhs }, Some(self_ty)) => {
                env::TypePredicate::Equality {
                    lhs: instantiate_self_ty(&lhs, self_ty),
                    rhs: instantiate_self_ty(&rhs, self_ty),
                }
            }
            (predicate, None) => predicate,
        };
        if !result.contains(&predicate) {
            result.push(predicate);
        }
    }
    let result = resolve_type_predicates(genv, diagnostics, result);
    expand_implied_predicates(genv, result)
}

fn predicate_type_aliases(predicates: &[env::TypePredicate]) -> HashMap<String, tast::Ty> {
    let mut aliases = HashMap::new();
    for predicate in predicates {
        let env::TypePredicate::Equality { lhs, rhs } = predicate else {
            continue;
        };
        if matches!(lhs, tast::Ty::TProjection { .. })
            || matches!(rhs, tast::Ty::TProjection { .. })
        {
            continue;
        }
        let _ = super::traits::coherence::unify(lhs, rhs, &mut aliases);
    }
    aliases
}

fn predicate_projection_aliases(predicates: &[env::TypePredicate]) -> HashMap<tast::Ty, tast::Ty> {
    let mut aliases = HashMap::new();
    let mut projection_equalities = Vec::new();
    for predicate in predicates {
        let env::TypePredicate::Equality { lhs, rhs } = predicate else {
            continue;
        };
        match (lhs, rhs) {
            (tast::Ty::TProjection { .. }, tast::Ty::TProjection { .. }) => {
                projection_equalities.push((lhs.clone(), rhs.clone()));
            }
            (tast::Ty::TProjection { .. }, other) => {
                aliases.entry(lhs.clone()).or_insert_with(|| other.clone());
            }
            (other, tast::Ty::TProjection { .. }) => {
                aliases.entry(rhs.clone()).or_insert_with(|| other.clone());
            }
            _ => {}
        }
    }
    loop {
        let mut changed = false;
        for (lhs, rhs) in &projection_equalities {
            let lhs_alias = resolve_projection_alias(lhs, &aliases);
            let rhs_alias = resolve_projection_alias(rhs, &aliases);
            if lhs_alias != *lhs && rhs_alias == *rhs {
                aliases.insert(rhs.clone(), lhs_alias);
                changed = true;
            } else if rhs_alias != *rhs && lhs_alias == *lhs {
                aliases.insert(lhs.clone(), rhs_alias);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    for (lhs, rhs) in projection_equalities {
        if !aliases.contains_key(&lhs) && !aliases.contains_key(&rhs) {
            aliases.insert(lhs, rhs);
        }
    }
    aliases
}

fn resolve_projection_alias(ty: &tast::Ty, aliases: &HashMap<tast::Ty, tast::Ty>) -> tast::Ty {
    let mut current = ty.clone();
    let mut visited = HashSet::new();
    while visited.insert(current.clone()) {
        let Some(next) = aliases.get(&current) else {
            break;
        };
        current = next.clone();
    }
    current
}

fn normalize_type_predicate(
    typer: &mut Typer,
    predicate: &env::TypePredicate,
) -> env::TypePredicate {
    match predicate {
        env::TypePredicate::Trait { for_ty, trait_ref } => env::TypePredicate::Trait {
            for_ty: typer.norm(for_ty),
            trait_ref: tast::TraitRef {
                name: trait_ref.name.clone(),
                args: trait_ref.args.iter().map(|arg| typer.norm(arg)).collect(),
            },
        },
        env::TypePredicate::Equality { lhs, rhs } => env::TypePredicate::Equality {
            lhs: typer.norm(lhs),
            rhs: typer.norm(rhs),
        },
    }
}

fn validate_function_parameter_names(
    diagnostics: &mut Diagnostics,
    hir_table: &hir::HirTable,
    f: &hir::Fn,
) {
    let mut seen = HashSet::new();
    for (id, _) in f.params.iter() {
        let name = hir_table.local_hint(*id).to_string();
        if !seen.insert(name.clone()) {
            diagnostics.push(Diagnostic::new(
                Stage::Typer,
                Severity::Error,
                format!("parameter {} is defined multiple times", name),
            ));
        }
    }
}

fn typecheck_fn(
    genv: &PackageTypeEnv,
    typer: &mut Typer,
    diagnostics: &mut Diagnostics,
    f: &hir::Fn,
    in_scope_traits: &[tast::TastIdent],
) {
    let tparams: Vec<tast::TastIdent> = f
        .generics
        .iter()
        .map(|g| tast::TastIdent(g.to_ident_name()))
        .collect();
    let mut bounds = init_trait_bounds(&tparams);
    extend_trait_bounds(genv, &mut bounds, &f.generic_bounds);
    normalize_trait_bounds(&mut bounds);
    typecheck_function_body(
        genv,
        typer,
        diagnostics,
        FunctionCheck {
            function: f,
            in_scope_traits,
            tparams: &tparams,
            bounds,
            predicates: f.predicates.clone(),
            self_ty: None,
        },
    );
}

fn typecheck_impl_block(
    genv: &PackageTypeEnv,
    typer: &mut Typer,
    diagnostics: &mut Diagnostics,
    impl_block: &hir::ImplBlock,
    in_scope_traits: &[tast::TastIdent],
) {
    let impl_generics_tast: Vec<tast::TastIdent> = impl_block
        .generics
        .iter()
        .map(|g| tast::TastIdent(g.to_ident_name()))
        .collect();
    let for_ty = tast::Ty::from_hir(genv, &impl_block.for_type, &impl_generics_tast);
    for f in impl_block.methods.iter() {
        let f = match typer.hir_table.def(*f).clone() {
            hir::Def::Fn(func) => func,
            _ => continue,
        };
        let mut all_generics = impl_block.generics.clone();
        all_generics.extend(f.generics.clone());
        let tparams: Vec<tast::TastIdent> = all_generics
            .iter()
            .map(|g| tast::TastIdent(g.to_ident_name()))
            .collect();

        let mut bounds = init_trait_bounds(&tparams);
        extend_trait_bounds(genv, &mut bounds, &impl_block.generic_bounds);
        extend_trait_bounds(genv, &mut bounds, &f.generic_bounds);
        normalize_trait_bounds(&mut bounds);
        let mut predicates = impl_block.predicates.clone();
        predicates.extend(f.predicates.clone());
        typecheck_function_body(
            genv,
            typer,
            diagnostics,
            FunctionCheck {
                function: &f,
                in_scope_traits,
                tparams: &tparams,
                bounds,
                predicates,
                self_ty: Some(&for_ty),
            },
        );
    }
}

struct FunctionCheck<'a> {
    function: &'a hir::Fn,
    in_scope_traits: &'a [tast::TastIdent],
    tparams: &'a [tast::TastIdent],
    bounds: IndexMap<String, Vec<tast::TraitRef>>,
    predicates: Vec<hir::Predicate>,
    self_ty: Option<&'a tast::Ty>,
}

fn typecheck_function_body(
    genv: &PackageTypeEnv,
    typer: &mut Typer,
    diagnostics: &mut Diagnostics,
    check: FunctionCheck<'_>,
) {
    let FunctionCheck {
        function,
        in_scope_traits,
        tparams,
        mut bounds,
        predicates,
        self_ty,
    } = check;
    validate_function_parameter_names(diagnostics, &typer.hir_table, function);
    let mut local_env = LocalTypeEnv::new();
    local_env.set_in_scope_traits(in_scope_traits.to_vec());
    let predicates =
        build_param_env_predicates(genv, diagnostics, tparams, &bounds, &predicates, self_ty);
    typer.param_type_aliases = predicate_type_aliases(&predicates);
    typer.param_projection_aliases = predicate_projection_aliases(&predicates)
        .into_iter()
        .map(|(projection, alias)| (typer.norm(&projection), typer.norm(&alias)))
        .collect();
    let projection_candidates = projection_candidates_from_predicates(genv, &predicates);
    for predicate in &predicates {
        let env::TypePredicate::Trait { for_ty, trait_ref } = predicate else {
            continue;
        };
        let tast::Ty::TParam { name } = for_ty else {
            continue;
        };
        if let Some(traits) = bounds.get_mut(name)
            && !traits.contains(trait_ref)
        {
            traits.push(trait_ref.clone());
        }
    }
    normalize_trait_bounds(&mut bounds);
    local_env.set_tparam_trait_bounds(bounds);
    local_env.set_predicates(predicates);

    let param_types = function
        .params
        .iter()
        .map(|(name, hir_ty)| {
            let ty = tast::Ty::from_hir(genv, hir_ty, tparams);
            let ty = resolve_ty_projections(
                genv,
                diagnostics,
                &ty,
                &projection_candidates,
                type_expr_range(hir_ty),
            );
            let ty = match self_ty {
                Some(self_ty) => instantiate_self_ty(&ty, self_ty),
                None => ty,
            };
            (*name, ty)
        })
        .collect::<Vec<_>>();
    let ret_ty = function
        .ret_ty
        .as_ref()
        .map(|ty| {
            let typed = tast::Ty::from_hir(genv, ty, tparams);
            let typed = resolve_ty_projections(
                genv,
                diagnostics,
                &typed,
                &projection_candidates,
                type_expr_range(ty),
            );
            match self_ty {
                Some(self_ty) => instantiate_self_ty(&typed, self_ty),
                None => typed,
            }
        })
        .unwrap_or(tast::Ty::TUnit);

    local_env.set_tparams_env(tparams);
    local_env.push_scope();
    for (id, ty) in param_types {
        local_env.insert_var(id, ty.clone());
        typer.results.record_local_ty(id, ty);
    }
    typer.return_ty_stack.push(ret_ty.clone());
    let _ = typer.check_block(genv, &mut local_env, diagnostics, &function.body, &ret_ty);
    let _ = typer.return_ty_stack.pop();
    local_env.pop_scope(diagnostics);
    local_env.clear_tparams_env();
    typer.tparam_trait_bounds = local_env
        .tparam_trait_bounds_map()
        .iter()
        .map(|(name, traits)| (name.clone(), traits.clone()))
        .collect();
    typer.param_env_predicates = local_env
        .predicates()
        .iter()
        .map(|predicate| normalize_type_predicate(typer, predicate))
        .collect();
    local_env.clear_tparam_trait_bounds();
    local_env.clear_predicates();
    typer.solve(genv, diagnostics);
    typer.tparam_trait_bounds.clear();
    typer.param_env_predicates.clear();
    typer.param_type_aliases.clear();
    typer.param_projection_aliases.clear();
}
