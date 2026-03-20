use crate::common::{self, Constructor, Prim};
use crate::core;
use crate::env::{Gensym, GlobalTypeEnv, StructDef};
use crate::names::{inherent_method_fn_name, trait_impl_fn_name};
use crate::tast::Arm;
use crate::tast::Expr::{self, *};
use crate::tast::Pat::{self, *};
use crate::tast::TastIdent;
use crate::tast::Ty;
use crate::tast::{self, File};
use diagnostics::{Diagnostic, Diagnostics, Severity, Stage};
use text_size::TextRange;

use indexmap::IndexMap;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

#[derive(Debug, Clone)]
struct Column {
    var: String,
    pat: tast::Pat,
}

#[derive(Debug, Clone)]
struct Row {
    columns: Vec<Column>,
    body: tast::Expr,
}

impl Row {
    fn remove_column(&mut self, var: &str) -> Option<Column> {
        let mut index = None;
        for (i, col) in self.columns.iter().enumerate() {
            if col.var == var {
                index = Some(i);
                break;
            }
        }
        if let Some(i) = index {
            let col = self.columns.remove(i);
            return Some(col);
        }
        None
    }

    fn get_ty(&self) -> Ty {
        self.body.get_ty()
    }
}

fn make_rows(name: &str, arms: &[Arm]) -> Vec<Row> {
    let mut result = Vec::new();
    for Arm { pat, body } in arms.iter() {
        result.push(Row {
            columns: vec![Column {
                var: name.to_string(),
                pat: pat.clone(),
            }],
            body: body.clone(),
        })
    }
    result
}

fn move_variable_patterns(row: &mut Row) {
    row.columns.retain(|col| match &col.pat {
        Pat::PVar {
            name,
            ty,
            astptr: _,
        } => {
            let old_body = row.body.clone();
            let old_body_ty = old_body.get_ty();
            row.body = EBlock {
                block: Box::new(tast::Block {
                    stmts: vec![tast::Stmt::Let(tast::LetStmt {
                        is_mut: false,
                        pat: Pat::PVar {
                            name: name.clone(),
                            ty: ty.clone(),
                            astptr: None,
                        },
                        value: Box::new(EVar {
                            name: col.var.clone(),
                            ty: col.pat.get_ty(),
                            astptr: None,
                        }),
                    })],
                    tail: Some(Box::new(old_body)),
                }),
                ty: old_body_ty,
            };
            false
        }
        Pat::PWild { ty: _, astptr: _ } => false,
        _ => true,
    });
}

#[derive(Debug, Clone)]
struct Variable {
    pub name: String,
    pub ty: Ty,
}

impl Variable {
    pub fn to_core(&self) -> core::Expr {
        core::Expr::EVar {
            name: self.name.clone(),
            ty: self.ty.clone(),
        }
    }
}

fn emissing(ty: &Ty) -> core::Expr {
    core::Expr::ECall {
        func: Box::new(core::Expr::EVar {
            name: "missing".to_string(),
            ty: Ty::TFunc {
                params: vec![Ty::TString],
                ret_ty: Box::new(ty.clone()),
            },
        }),
        args: vec![core::Expr::EPrim {
            value: Prim::string("".to_string()),
            ty: Ty::TString,
        }],
        ty: ty.clone(),
    }
}

fn push_compile_error(
    diagnostics: &mut Diagnostics,
    message: impl Into<String>,
    range: Option<TextRange>,
) {
    diagnostics
        .push(Diagnostic::new(Stage::other("compile"), Severity::Error, message).with_range(range));
}

fn push_compile_ice(
    diagnostics: &mut Diagnostics,
    message: impl Into<String>,
    range: Option<TextRange>,
) {
    push_compile_error(
        diagnostics,
        format!("Internal compiler error: {}", message.into()),
        range,
    );
}

fn pat_range(pat: &Pat) -> Option<TextRange> {
    match pat {
        Pat::PVar { astptr, .. }
        | Pat::PPrim { astptr, .. }
        | Pat::PConstr { astptr, .. }
        | Pat::PTuple { astptr, .. }
        | Pat::PWild { astptr, .. } => astptr.as_ref().map(|ptr| ptr.text_range()),
    }
}

fn expr_range(expr: &Expr) -> Option<TextRange> {
    match expr {
        Expr::EVar { astptr, .. }
        | Expr::EMatch { astptr, .. }
        | Expr::EField { astptr, .. }
        | Expr::EIndex { astptr, .. }
        | Expr::ETraitMethod { astptr, .. }
        | Expr::EDynTraitMethod { astptr, .. }
        | Expr::EInherentMethod { astptr, .. }
        | Expr::EToDyn { astptr, .. } => astptr.as_ref().map(|ptr| ptr.text_range()),
        _ => None,
    }
}

fn has_tparam(ty: &Ty) -> bool {
    match ty {
        Ty::TParam { .. } => true,
        Ty::TVar(_) => false,
        Ty::TUnit
        | Ty::TBool
        | Ty::TInt8
        | Ty::TInt16
        | Ty::TInt32
        | Ty::TInt64
        | Ty::TUint8
        | Ty::TUint16
        | Ty::TUint32
        | Ty::TUint64
        | Ty::TFloat32
        | Ty::TFloat64
        | Ty::TString
        | Ty::TChar
        | Ty::TEnum { .. }
        | Ty::TStruct { .. }
        | Ty::TDyn { .. } => false,
        Ty::TTuple { typs } => typs.iter().any(has_tparam),
        Ty::TApp { ty, args } => has_tparam(ty) || args.iter().any(has_tparam),
        Ty::TArray { elem, .. } => has_tparam(elem),
        Ty::TSlice { elem } => has_tparam(elem),
        Ty::TVec { elem } => has_tparam(elem),
        Ty::TRef { elem } => has_tparam(elem),
        Ty::THashMap { key, value } => has_tparam(key) || has_tparam(value),
        Ty::TFunc { params, ret_ty } => params.iter().any(has_tparam) || has_tparam(ret_ty),
    }
}

fn branch_variable(rows: &[Row]) -> Variable {
    let mut counts = HashMap::new();
    let mut var_ty: HashMap<String, Ty> = HashMap::new();
    for row in rows {
        for col in &row.columns {
            *counts.entry(&col.var).or_insert(0_usize) += 1;
            var_ty.insert(col.var.clone(), col.pat.get_ty());
        }
    }
    let var = rows[0]
        .columns
        .iter()
        .map(|col| col.var.clone())
        .max_by_key(|var| counts[var])
        .unwrap();
    Variable {
        name: var.clone(),
        ty: var_ty[&var].clone(),
    }
}

fn substitute_ty_params(ty: &Ty, subst: &HashMap<String, Ty>) -> Ty {
    match ty {
        Ty::TVar(_)
        | Ty::TUnit
        | Ty::TBool
        | Ty::TInt8
        | Ty::TInt16
        | Ty::TInt32
        | Ty::TInt64
        | Ty::TUint8
        | Ty::TUint16
        | Ty::TUint32
        | Ty::TUint64
        | Ty::TFloat32
        | Ty::TFloat64
        | Ty::TString
        | Ty::TChar => ty.clone(),
        Ty::TTuple { typs } => Ty::TTuple {
            typs: typs
                .iter()
                .map(|t| substitute_ty_params(t, subst))
                .collect(),
        },
        Ty::TEnum { name } => Ty::TEnum { name: name.clone() },
        Ty::TStruct { name } => Ty::TStruct { name: name.clone() },
        Ty::TDyn { trait_name } => Ty::TDyn {
            trait_name: trait_name.clone(),
        },
        Ty::TApp { ty, args } => Ty::TApp {
            ty: Box::new(substitute_ty_params(ty, subst)),
            args: args
                .iter()
                .map(|t| substitute_ty_params(t, subst))
                .collect(),
        },
        Ty::TParam { name } => subst
            .get(name)
            .cloned()
            .unwrap_or_else(|| Ty::TParam { name: name.clone() }),
        Ty::TArray { len, elem } => Ty::TArray {
            len: *len,
            elem: Box::new(substitute_ty_params(elem, subst)),
        },
        Ty::TSlice { elem } => Ty::TSlice {
            elem: Box::new(substitute_ty_params(elem, subst)),
        },
        Ty::TVec { elem } => Ty::TVec {
            elem: Box::new(substitute_ty_params(elem, subst)),
        },
        Ty::TRef { elem } => Ty::TRef {
            elem: Box::new(substitute_ty_params(elem, subst)),
        },
        Ty::THashMap { key, value } => Ty::THashMap {
            key: Box::new(substitute_ty_params(key, subst)),
            value: Box::new(substitute_ty_params(value, subst)),
        },
        Ty::TFunc { params, ret_ty } => Ty::TFunc {
            params: params
                .iter()
                .map(|t| substitute_ty_params(t, subst))
                .collect(),
            ret_ty: Box::new(substitute_ty_params(ret_ty, subst)),
        },
    }
}

fn instantiate_struct_fields(
    diagnostics: &mut Diagnostics,
    struct_def: &StructDef,
    type_args: &[Ty],
    range: Option<TextRange>,
) -> Option<Vec<(String, Ty)>> {
    if struct_def.generics.len() != type_args.len() {
        push_compile_error(
            diagnostics,
            format!(
                "Struct {} expects {} type arguments, but got {}",
                struct_def.name.0,
                struct_def.generics.len(),
                type_args.len()
            ),
            range,
        );
        return None;
    }

    let mut subst = HashMap::new();
    for (param, arg) in struct_def.generics.iter().zip(type_args.iter()) {
        subst.insert(param.0.clone(), arg.clone());
    }

    Some(
        struct_def
            .fields
            .iter()
            .map(|(fname, fty)| (fname.0.clone(), substitute_ty_params(fty, &subst)))
            .collect(),
    )
}

fn decompose_struct_type(ty: &Ty) -> Option<(TastIdent, Vec<Ty>)> {
    match ty {
        Ty::TStruct { name } => Some((TastIdent(name.clone()), Vec::new())),
        Ty::TApp { ty: base, args } => {
            let (type_name, mut collected) = decompose_struct_type(base)?;
            collected.extend(args.iter().cloned());
            Some((type_name, collected))
        }
        _ => None,
    }
}

fn decompose_enum_type(ty: &Ty) -> Option<(TastIdent, Vec<Ty>)> {
    match ty {
        Ty::TEnum { name } => Some((TastIdent(name.clone()), Vec::new())),
        Ty::TApp { ty: base, args } => {
            let (type_name, mut collected) = decompose_enum_type(base)?;
            collected.extend(args.iter().cloned());
            Some((type_name, collected))
        }
        _ => None,
    }
}

#[derive(Debug, Clone)]
enum PlaceRoot {
    Local { name: String, ty: Ty },
    Ref { expr: Expr, ty: Ty },
    Value { expr: Expr, ty: Ty },
}

#[derive(Debug, Clone)]
enum PlaceStep {
    Proj {
        parent_ty: Ty,
        index: usize,
        item_ty: Ty,
    },
    Field {
        parent_ty: Ty,
        field_name: String,
        item_ty: Ty,
        range: Option<TextRange>,
    },
    Index {
        container_ty: Ty,
        index: Expr,
        item_ty: Ty,
        range: Option<TextRange>,
    },
}

#[derive(Debug, Clone)]
enum EvaluatedPlaceStep {
    Proj {
        parent_var: String,
        parent_ty: Ty,
        index: usize,
    },
    Field {
        parent_var: String,
        parent_ty: Ty,
        field_name: String,
        range: Option<TextRange>,
    },
    Index {
        parent_var: String,
        parent_ty: Ty,
        index_var: String,
        index_ty: Ty,
        range: Option<TextRange>,
    },
}

#[derive(Debug, Clone)]
enum CompiledPlaceRoot {
    Local { name: String, ty: Ty },
    Ref { ref_var: String, ref_ty: Ty },
    Value,
}

fn is_ref_get_call(expr: &Expr) -> Option<&Expr> {
    match expr {
        Expr::ECall { func, args, .. } if args.len() == 1 => match func.as_ref() {
            Expr::EVar { name, .. } if name == "ref_get" => Some(&args[0]),
            _ => None,
        },
        _ => None,
    }
}

fn decompose_place(expr: &Expr) -> Option<(PlaceRoot, Vec<PlaceStep>)> {
    fn go(expr: &Expr, steps: &mut Vec<PlaceStep>) -> Option<PlaceRoot> {
        match expr {
            Expr::EVar { name, ty, .. } => Some(PlaceRoot::Local {
                name: name.clone(),
                ty: ty.clone(),
            }),
            Expr::EProj { tuple, index, ty } => {
                let root = go(tuple, steps)?;
                steps.push(PlaceStep::Proj {
                    parent_ty: tuple.get_ty(),
                    index: *index,
                    item_ty: ty.clone(),
                });
                Some(root)
            }
            Expr::EField {
                expr,
                field_name,
                ty,
                astptr,
            } => {
                let root = go(expr, steps)?;
                steps.push(PlaceStep::Field {
                    parent_ty: expr.get_ty(),
                    field_name: field_name.clone(),
                    item_ty: ty.clone(),
                    range: astptr.as_ref().map(|ptr| ptr.text_range()),
                });
                Some(root)
            }
            Expr::EIndex {
                base,
                index,
                ty,
                astptr,
            } => {
                let root = go(base, steps)?;
                steps.push(PlaceStep::Index {
                    container_ty: base.get_ty(),
                    index: index.as_ref().clone(),
                    item_ty: ty.clone(),
                    range: astptr.as_ref().map(|ptr| ptr.text_range()),
                });
                Some(root)
            }
            _ => {
                if let Some(ref_expr) = is_ref_get_call(expr) {
                    Some(PlaceRoot::Ref {
                        expr: ref_expr.clone(),
                        ty: expr.get_ty(),
                    })
                } else {
                    Some(PlaceRoot::Value {
                        expr: expr.clone(),
                        ty: expr.get_ty(),
                    })
                }
            }
        }
    }

    let mut steps = Vec::new();
    let root = go(expr, &mut steps)?;
    Some((root, steps))
}

fn core_var(name: impl Into<String>, ty: &Ty) -> core::Expr {
    core::Expr::EVar {
        name: name.into(),
        ty: ty.clone(),
    }
}

fn builtin_func(name: &str, params: Vec<Ty>, ret_ty: Ty) -> core::Expr {
    core::Expr::EVar {
        name: name.to_string(),
        ty: Ty::TFunc {
            params,
            ret_ty: Box::new(ret_ty),
        },
    }
}

fn builtin_call(name: &str, params: Vec<Ty>, ret_ty: Ty, args: Vec<core::Expr>) -> core::Expr {
    core::Expr::ECall {
        func: Box::new(builtin_func(name, params, ret_ty.clone())),
        args,
        ty: ret_ty,
    }
}

fn compile_index_read_core(
    diagnostics: &mut Diagnostics,
    container: core::Expr,
    container_ty: &Ty,
    index: core::Expr,
    item_ty: &Ty,
    range: Option<TextRange>,
) -> core::Expr {
    match container_ty {
        Ty::TArray { .. } => builtin_call(
            "array_get",
            vec![container_ty.clone(), Ty::TInt32],
            item_ty.clone(),
            vec![container, index],
        ),
        Ty::TVec { .. } => builtin_call(
            "vec_get",
            vec![container_ty.clone(), Ty::TInt32],
            item_ty.clone(),
            vec![container, index],
        ),
        Ty::TSlice { .. } => builtin_call(
            "slice_get",
            vec![container_ty.clone(), Ty::TInt32],
            item_ty.clone(),
            vec![container, index],
        ),
        Ty::THashMap { key, .. } => builtin_call(
            "hashmap_get",
            vec![container_ty.clone(), key.as_ref().clone()],
            item_ty.clone(),
            vec![container, index],
        ),
        _ => {
            push_compile_error(
                diagnostics,
                format!("type {:?} does not support indexing", container_ty),
                range,
            );
            emissing(item_ty)
        }
    }
}

fn compile_ref_get_core(reference: core::Expr, ref_ty: &Ty, value_ty: &Ty) -> core::Expr {
    builtin_call(
        "ref_get",
        vec![ref_ty.clone()],
        value_ty.clone(),
        vec![reference],
    )
}

fn compile_ref_set_core(reference: core::Expr, value: core::Expr, ref_ty: &Ty) -> core::Expr {
    builtin_call(
        "ref_set",
        vec![ref_ty.clone(), value.get_ty()],
        Ty::TUnit,
        vec![reference, value],
    )
}

fn compile_array_set_core(
    container: core::Expr,
    index: core::Expr,
    value: core::Expr,
) -> core::Expr {
    let container_ty = container.get_ty();
    builtin_call(
        "array_set",
        vec![container_ty.clone(), Ty::TInt32, value.get_ty()],
        container_ty,
        vec![container, index, value],
    )
}

fn compile_vec_set_core(container: core::Expr, index: core::Expr, value: core::Expr) -> core::Expr {
    let container_ty = container.get_ty();
    builtin_call(
        "vec_set",
        vec![container_ty, Ty::TInt32, value.get_ty()],
        Ty::TUnit,
        vec![container, index, value],
    )
}

fn compile_hashmap_set_core(
    container: core::Expr,
    index: core::Expr,
    value: core::Expr,
) -> core::Expr {
    let container_ty = container.get_ty();
    let key_ty = match &container_ty {
        Ty::THashMap { key, .. } => key.as_ref().clone(),
        _ => value.get_ty(),
    };
    builtin_call(
        "hashmap_set",
        vec![container_ty, key_ty, value.get_ty()],
        Ty::TUnit,
        vec![container, index, value],
    )
}

type StructFieldInfo = (TastIdent, Vec<(String, Ty)>, usize);

fn struct_field_info(
    genv: &GlobalTypeEnv,
    diagnostics: &mut Diagnostics,
    parent_ty: &Ty,
    field_name: &str,
    range: Option<TextRange>,
) -> Option<StructFieldInfo> {
    let Some((type_name, type_args)) = decompose_struct_type(parent_ty) else {
        push_compile_error(
            diagnostics,
            format!(
                "field access is only valid on struct types, got {:?}",
                parent_ty
            ),
            range,
        );
        return None;
    };
    let Some(struct_def) = genv.structs().get(&type_name) else {
        push_compile_ice(
            diagnostics,
            format!(
                "struct {} not found while compiling field access",
                type_name.0
            ),
            range,
        );
        return None;
    };
    let inst_fields = instantiate_struct_fields(diagnostics, struct_def, &type_args, range)?;
    let Some((field_index, _)) = inst_fields
        .iter()
        .enumerate()
        .find(|(_, (name, _))| name == field_name)
    else {
        push_compile_error(
            diagnostics,
            format!("struct {} has no field {}", type_name.0, field_name),
            range,
        );
        return None;
    };
    Some((type_name, inst_fields, field_index))
}

fn compile_field_get_core(
    genv: &GlobalTypeEnv,
    diagnostics: &mut Diagnostics,
    expr: core::Expr,
    parent_ty: &Ty,
    field_name: &str,
    field_ty: &Ty,
    range: Option<TextRange>,
) -> core::Expr {
    let Some((type_name, _, field_index)) =
        struct_field_info(genv, diagnostics, parent_ty, field_name, range)
    else {
        return emissing(field_ty);
    };
    core::Expr::EConstrGet {
        expr: Box::new(expr),
        constructor: common::Constructor::Struct(common::StructConstructor { type_name }),
        field_index,
        ty: field_ty.clone(),
    }
}

fn rebuild_tuple_with_proj(
    parent: core::Expr,
    parent_ty: &Ty,
    index: usize,
    value: core::Expr,
) -> core::Expr {
    let Ty::TTuple { typs } = parent_ty else {
        return emissing(parent_ty);
    };
    let mut replacement = Some(value);
    let items = typs
        .iter()
        .enumerate()
        .map(|(item_index, item_ty)| {
            if item_index == index {
                replacement.take().unwrap_or_else(|| emissing(item_ty))
            } else {
                core::Expr::EProj {
                    tuple: Box::new(parent.clone()),
                    index: item_index,
                    ty: item_ty.clone(),
                }
            }
        })
        .collect();
    core::Expr::ETuple {
        items,
        ty: parent_ty.clone(),
    }
}

fn rebuild_struct_with_field(
    genv: &GlobalTypeEnv,
    diagnostics: &mut Diagnostics,
    parent: core::Expr,
    parent_ty: &Ty,
    field_name: &str,
    value: core::Expr,
    range: Option<TextRange>,
) -> core::Expr {
    let Some((type_name, fields, field_index)) =
        struct_field_info(genv, diagnostics, parent_ty, field_name, range)
    else {
        return emissing(parent_ty);
    };
    let constructor = common::Constructor::Struct(common::StructConstructor { type_name });
    let mut replacement = Some(value);
    let args = fields
        .iter()
        .enumerate()
        .map(|(index, (_, field_ty))| {
            if index == field_index {
                replacement.take().unwrap_or_else(|| emissing(field_ty))
            } else {
                core::Expr::EConstrGet {
                    expr: Box::new(parent.clone()),
                    constructor: constructor.clone(),
                    field_index: index,
                    ty: field_ty.clone(),
                }
            }
        })
        .collect();
    core::Expr::EConstr {
        constructor,
        args,
        ty: parent_ty.clone(),
    }
}

struct ConstructorCase {
    constructor: Constructor,
    vars: Vec<Variable>,
    rows: Vec<Row>,
}

#[allow(clippy::too_many_arguments)]
fn compile_constructor_cases(
    genv: &GlobalTypeEnv,
    gensym: &Gensym,
    diagnostics: &mut Diagnostics,
    rows: Vec<Row>,
    bvar: &Variable,
    mut cases: Vec<ConstructorCase>,
    ty: &Ty,
    match_range: Option<TextRange>,
    variant_names: &[String],
) -> Vec<core::Arm> {
    let mut has_wildcard = false;
    for mut row in rows {
        if let Some(col) = row.remove_column(&bvar.name) {
            let col_range = pat_range(&col.pat).or(match_range);
            if let Pat::PConstr {
                constructor,
                args,
                ty: _,
                ..
            } = col.pat
            {
                let Some(enum_ctor) = constructor.as_enum() else {
                    push_compile_ice(
                        diagnostics,
                        "expected enum constructor while compiling match arms",
                        col_range,
                    );
                    has_wildcard = true;
                    for ConstructorCase { rows, .. } in &mut cases {
                        rows.push(row.clone());
                    }
                    continue;
                };
                let idx = enum_ctor.enum_index();
                let Some(case) = cases.get_mut(idx) else {
                    push_compile_ice(
                        diagnostics,
                        format!(
                            "enum constructor index {} is out of range for type {}",
                            idx, enum_ctor.type_name.0
                        ),
                        col_range,
                    );
                    has_wildcard = true;
                    for ConstructorCase { rows, .. } in &mut cases {
                        rows.push(row.clone());
                    }
                    continue;
                };
                if case.vars.len() != args.len() {
                    push_compile_ice(
                        diagnostics,
                        format!(
                            "constructor {}::{} expects {} fields, but pattern provided {}",
                            enum_ctor.type_name.0,
                            enum_ctor.variant.0,
                            case.vars.len(),
                            args.len()
                        ),
                        col_range,
                    );
                }
                let mut cols = row.columns;
                for (var, pat) in case.vars.iter().zip(args.into_iter()) {
                    cols.push(Column {
                        var: var.name.clone(),
                        pat,
                    });
                }
                cases[idx].rows.push(Row {
                    columns: cols,
                    body: row.body,
                });
            } else {
                push_compile_ice(
                    diagnostics,
                    "expected constructor pattern while compiling enum match",
                    col_range,
                );
                has_wildcard = true;
                for ConstructorCase { rows, .. } in &mut cases {
                    rows.push(row.clone());
                }
            }
        } else {
            has_wildcard = true;
            for ConstructorCase { rows, .. } in &mut cases {
                rows.push(row.clone());
            }
        }
    }

    if !has_wildcard {
        let missing: Vec<&str> = cases
            .iter()
            .enumerate()
            .filter(|(_, case)| case.rows.is_empty())
            .map(|(i, _)| variant_names[i].as_str())
            .collect();

        if !missing.is_empty() {
            let message = format!(
                "non-exhaustive match: missing pattern{} {}",
                if missing.len() > 1 { "s" } else { "" },
                missing.join(", ")
            );
            push_compile_error(diagnostics, message, match_range);
        }
    }

    let mut arms = vec![];
    for case in cases.into_iter() {
        let args = case.vars.into_iter().map(|var| var.to_core()).collect();
        let arm = core::Arm {
            lhs: core::Expr::EConstr {
                constructor: case.constructor,
                args,
                ty: bvar.ty.clone(),
            },
            body: compile_rows(genv, gensym, diagnostics, case.rows, ty, match_range),
        };
        arms.push(arm);
    }
    arms
}

#[allow(clippy::too_many_arguments)]
fn compile_enum_case(
    genv: &GlobalTypeEnv,
    gensym: &Gensym,
    diagnostics: &mut Diagnostics,
    rows: Vec<Row>,
    bvar: &Variable,
    ty: &Ty,
    name: &TastIdent,
    match_range: Option<TextRange>,
) -> core::Expr {
    let Some(tydef) = genv.enums().get(name).cloned() else {
        push_compile_ice(
            diagnostics,
            format!("enum {} not found during match compilation", name.0),
            match_range,
        );
        return emissing(ty);
    };
    let body_ty = rows.first().map(|r| r.get_ty()).unwrap_or(Ty::TUnit);

    let type_args = decompose_enum_type(&bvar.ty)
        .map(|(_, args)| args)
        .unwrap_or_default();
    let mut subst = HashMap::new();
    for (param, arg) in tydef.generics.iter().zip(type_args.iter()) {
        subst.insert(param.0.clone(), arg.clone());
    }

    let variant_names: Vec<String> = tydef
        .variants
        .iter()
        .map(|(variant, _)| variant.0.clone())
        .collect();

    let cases: Vec<ConstructorCase> = tydef
        .variants
        .iter()
        .enumerate()
        .map(|(index, (variant, args))| ConstructorCase {
            constructor: Constructor::Enum(common::EnumConstructor {
                type_name: name.clone(),
                variant: variant.clone(),
                index,
            }),
            vars: args
                .iter()
                .map(|arg_ty| Variable {
                    name: gensym.gensym("x"),
                    ty: substitute_ty_params(arg_ty, &subst),
                })
                .collect::<Vec<_>>(),
            rows: vec![],
        })
        .collect();

    let case_let_stmts = cases
        .iter()
        .map(|case| {
            case.vars
                .iter()
                .enumerate()
                .map(|(field_index, var)| core::LetStmt {
                    name: var.name.clone(),
                    value: core::Expr::EConstrGet {
                        expr: Box::new(bvar.to_core()),
                        constructor: case.constructor.clone(),
                        field_index,
                        ty: var.ty.clone(),
                    },
                    ty: ty.clone(),
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let arms = compile_constructor_cases(
        genv,
        gensym,
        diagnostics,
        rows,
        bvar,
        cases,
        ty,
        match_range,
        &variant_names,
    );

    let mut new_arms = vec![];
    for (let_stmts, mut arm) in case_let_stmts.into_iter().zip(arms.into_iter()) {
        arm.body = prepend_lets_to_expr(let_stmts, arm.body, ty);
        new_arms.push(arm);
    }

    core::Expr::EMatch {
        expr: Box::new(bvar.to_core()),
        arms: new_arms,
        default: None,
        ty: body_ty,
    }
}

#[allow(clippy::too_many_arguments)]
fn compile_struct_case(
    genv: &GlobalTypeEnv,
    gensym: &Gensym,
    diagnostics: &mut Diagnostics,
    rows: Vec<Row>,
    bvar: &Variable,
    ty: &Ty,
    name: &TastIdent,
    type_args: &[Ty],
    match_range: Option<TextRange>,
) -> core::Expr {
    let Some(struct_def) = genv.structs().get(name).cloned() else {
        push_compile_ice(
            diagnostics,
            format!("struct {} not found during match compilation", name.0),
            match_range,
        );
        return emissing(ty);
    };

    let inst_fields = if struct_def.generics.is_empty() {
        struct_def
            .fields
            .iter()
            .map(|(fname, fty)| (fname.0.clone(), fty.clone()))
            .collect::<Vec<_>>()
    } else {
        let Some(fields) =
            instantiate_struct_fields(diagnostics, &struct_def, type_args, match_range)
        else {
            return emissing(ty);
        };
        fields
    };

    let field_vars: Vec<Variable> = inst_fields
        .iter()
        .map(|(_, fty)| Variable {
            name: gensym.gensym("x"),
            ty: fty.clone(),
        })
        .collect();

    let constructor = Constructor::Struct(common::StructConstructor {
        type_name: name.clone(),
    });

    let mut new_rows = vec![];
    for row in rows {
        let mut cols = vec![];
        for Column { var, pat } in row.columns {
            if var == bvar.name {
                match pat {
                    Pat::PConstr {
                        constructor,
                        args,
                        ty: _,
                        ..
                    } if constructor.is_struct() => {
                        if field_vars.len() != args.len() {
                            push_compile_ice(
                                diagnostics,
                                format!(
                                    "struct pattern for {} expects {} fields, but got {}",
                                    name.0,
                                    field_vars.len(),
                                    args.len()
                                ),
                                match_range,
                            );
                            return emissing(ty);
                        }
                        for (var, arg_pat) in field_vars.iter().zip(args.into_iter()) {
                            cols.push(Column {
                                var: var.name.clone(),
                                pat: arg_pat,
                            });
                        }
                    }
                    _ => {
                        push_compile_ice(
                            diagnostics,
                            "expected struct pattern while compiling struct match",
                            pat_range(&pat).or(match_range),
                        );
                        return emissing(ty);
                    }
                }
            } else {
                cols.push(Column { var, pat });
            }
        }
        new_rows.push(Row {
            columns: cols,
            body: row.body,
        });
    }

    let inner = compile_rows(genv, gensym, diagnostics, new_rows, ty, match_range);
    let let_stmts = field_vars
        .iter()
        .enumerate()
        .map(|(field_index, var)| core::LetStmt {
            name: var.name.clone(),
            value: core::Expr::EConstrGet {
                expr: Box::new(bvar.to_core()),
                constructor: constructor.clone(),
                field_index,
                ty: var.ty.clone(),
            },
            ty: ty.clone(),
        })
        .collect();
    prepend_lets_to_expr(let_stmts, inner, ty)
}

#[allow(clippy::too_many_arguments)]
fn compile_tuple_case(
    genv: &GlobalTypeEnv,
    gensym: &Gensym,
    diagnostics: &mut Diagnostics,
    rows: Vec<Row>,
    bvar: &Variable,
    typs: &[Ty],
    ty: &Ty,
    match_range: Option<TextRange>,
) -> core::Expr {
    let names = typs.iter().map(|_| gensym.gensym("x")).collect::<Vec<_>>();
    let mut new_rows = vec![];

    for row in rows {
        let mut cols = vec![];
        for Column { var, pat } in row.columns {
            if var == bvar.name {
                if let PTuple { items, ty: _, .. } = pat {
                    if items.len() != names.len() {
                        push_compile_ice(
                            diagnostics,
                            format!(
                                "tuple pattern expects {} elements, but got {}",
                                names.len(),
                                items.len()
                            ),
                            match_range,
                        );
                        return emissing(ty);
                    }
                    for (i, item) in items.into_iter().enumerate() {
                        cols.push(Column {
                            var: names[i].clone(),
                            pat: item,
                        });
                    }
                } else {
                    push_compile_ice(
                        diagnostics,
                        "expected tuple pattern while compiling tuple match",
                        pat_range(&pat).or(match_range),
                    );
                    return emissing(ty);
                }
            } else {
                cols.push(Column { var, pat });
            }
        }
        new_rows.push(Row {
            columns: cols,
            body: row.body,
        });
    }

    let inner = compile_rows(genv, gensym, diagnostics, new_rows, ty, match_range);
    let let_stmts = names
        .iter()
        .enumerate()
        .map(|(i, name)| core::LetStmt {
            name: name.clone(),
            value: core::Expr::EProj {
                tuple: Box::new(bvar.to_core()),
                index: i,
                ty: typs[i].clone(),
            },
            ty: ty.clone(),
        })
        .collect();
    prepend_lets_to_expr(let_stmts, inner, ty)
}

fn compile_unit_case(
    genv: &GlobalTypeEnv,
    gensym: &Gensym,
    diagnostics: &mut Diagnostics,
    rows: Vec<Row>,
    bvar: &Variable,
    match_range: Option<TextRange>,
) -> core::Expr {
    let body_ty = rows.first().map(|r| r.get_ty()).unwrap_or(Ty::TUnit);
    let mut new_rows = vec![];
    for mut r in rows {
        #[allow(clippy::redundant_pattern_matching)]
        if let Some(_) = r.remove_column(&bvar.name) {
            new_rows.push(r);
        } else {
            new_rows.push(r);
        }
    }

    core::Expr::EMatch {
        expr: Box::new(bvar.to_core()),
        arms: vec![core::Arm {
            lhs: core::eunit(),
            body: compile_rows(genv, gensym, diagnostics, new_rows, &bvar.ty, match_range),
        }],
        default: None,
        ty: body_ty,
    }
}

fn compile_bool_case(
    genv: &GlobalTypeEnv,
    gensym: &Gensym,
    diagnostics: &mut Diagnostics,
    rows: Vec<Row>,
    bvar: &Variable,
    match_range: Option<TextRange>,
) -> core::Expr {
    let body_ty = rows.first().map(|r| r.get_ty()).unwrap_or(Ty::TUnit);

    let mut true_rows = vec![];
    let mut false_rows = vec![];
    let mut has_wildcard = false;
    for mut r in rows {
        if let Some(col) = r.remove_column(&bvar.name) {
            let col_range = pat_range(&col.pat).or(match_range);
            match col.pat {
                Pat::PPrim { value, ty: _, .. } => {
                    if let Some(value) = value.as_bool() {
                        if value {
                            true_rows.push(r);
                        } else {
                            false_rows.push(r);
                        }
                    } else {
                        push_compile_ice(
                            diagnostics,
                            "expected boolean literal pattern while compiling bool match",
                            col_range,
                        );
                        has_wildcard = true;
                        true_rows.push(r.clone());
                        false_rows.push(r);
                    }
                }
                Pat::PWild { .. } => {
                    has_wildcard = true;
                    true_rows.push(r.clone());
                    false_rows.push(r);
                }
                _ => {
                    push_compile_ice(
                        diagnostics,
                        "expected bool pattern while compiling bool match",
                        col_range,
                    );
                    has_wildcard = true;
                    true_rows.push(r.clone());
                    false_rows.push(r);
                }
            }
        } else {
            has_wildcard = true;
            true_rows.push(r.clone());
            false_rows.push(r);
        }
    }

    let missing_true = true_rows.is_empty();
    let missing_false = false_rows.is_empty();

    if !has_wildcard && (missing_true || missing_false) {
        let missing: Vec<&str> = [
            if missing_true { Some("true") } else { None },
            if missing_false { Some("false") } else { None },
        ]
        .into_iter()
        .flatten()
        .collect();

        let message = format!(
            "non-exhaustive match: missing pattern{}{}",
            if missing.len() > 1 { "s " } else { " " },
            missing.join(", ")
        );
        push_compile_error(diagnostics, message, match_range);
    }

    core::Expr::EMatch {
        expr: Box::new(bvar.to_core()),
        arms: vec![
            core::Arm {
                lhs: core::ebool(true),
                body: compile_rows(genv, gensym, diagnostics, true_rows, &bvar.ty, match_range),
            },
            core::Arm {
                lhs: core::ebool(false),
                body: compile_rows(genv, gensym, diagnostics, false_rows, &bvar.ty, match_range),
            },
        ],
        default: None,
        ty: body_ty,
    }
}

#[allow(clippy::too_many_arguments)]
fn compile_int_case(
    genv: &GlobalTypeEnv,
    gensym: &Gensym,
    diagnostics: &mut Diagnostics,
    rows: Vec<Row>,
    bvar: &Variable,
    ty: &Ty,
    literal_ty: Ty,
    match_range: Option<TextRange>,
) -> core::Expr {
    match literal_ty {
        Ty::TInt8 => {
            return compile_int_case_impl::<i8, _, _>(
                genv,
                gensym,
                diagnostics,
                rows,
                bvar,
                ty,
                Ty::TInt8,
                match_range,
                |prim| prim.as_int8(),
                |value| Prim::Int8 { value },
            );
        }
        Ty::TInt16 => {
            return compile_int_case_impl::<i16, _, _>(
                genv,
                gensym,
                diagnostics,
                rows,
                bvar,
                ty,
                Ty::TInt16,
                match_range,
                |prim| prim.as_int16(),
                |value| Prim::Int16 { value },
            );
        }
        Ty::TInt32 => {
            return compile_int_case_impl::<i32, _, _>(
                genv,
                gensym,
                diagnostics,
                rows,
                bvar,
                ty,
                Ty::TInt32,
                match_range,
                |prim| prim.as_int32(),
                |value| Prim::Int32 { value },
            );
        }
        Ty::TInt64 => {
            return compile_int_case_impl::<i64, _, _>(
                genv,
                gensym,
                diagnostics,
                rows,
                bvar,
                ty,
                Ty::TInt64,
                match_range,
                |prim| prim.as_int64(),
                |value| Prim::Int64 { value },
            );
        }
        Ty::TUint8 => {
            return compile_int_case_impl::<u8, _, _>(
                genv,
                gensym,
                diagnostics,
                rows,
                bvar,
                ty,
                Ty::TUint8,
                match_range,
                |prim| prim.as_uint8(),
                |value| Prim::UInt8 { value },
            );
        }
        Ty::TUint16 => {
            return compile_int_case_impl::<u16, _, _>(
                genv,
                gensym,
                diagnostics,
                rows,
                bvar,
                ty,
                Ty::TUint16,
                match_range,
                |prim| prim.as_uint16(),
                |value| Prim::UInt16 { value },
            );
        }
        Ty::TUint32 => {
            return compile_int_case_impl::<u32, _, _>(
                genv,
                gensym,
                diagnostics,
                rows,
                bvar,
                ty,
                Ty::TUint32,
                match_range,
                |prim| prim.as_uint32(),
                |value| Prim::UInt32 { value },
            );
        }
        Ty::TUint64 => {
            return compile_int_case_impl::<u64, _, _>(
                genv,
                gensym,
                diagnostics,
                rows,
                bvar,
                ty,
                Ty::TUint64,
                match_range,
                |prim| prim.as_uint64(),
                |value| Prim::UInt64 { value },
            );
        }
        Ty::TChar => {
            return compile_int_case_impl::<u32, _, _>(
                genv,
                gensym,
                diagnostics,
                rows,
                bvar,
                ty,
                Ty::TChar,
                match_range,
                |prim| prim.as_char().map(|ch| ch as u32),
                |value| Prim::Char {
                    value: char::from_u32(value).unwrap_or('\u{FFFD}'),
                },
            );
        }
        _ => {}
    }

    push_compile_ice(
        diagnostics,
        format!(
            "expected integer or char literal type while compiling integer match, got {:?}",
            literal_ty
        ),
        match_range,
    );
    emissing(ty)
}

#[allow(clippy::too_many_arguments)]
fn compile_int_case_impl<T, Extract, ToPrim>(
    genv: &GlobalTypeEnv,
    gensym: &Gensym,
    diagnostics: &mut Diagnostics,
    rows: Vec<Row>,
    bvar: &Variable,
    ty: &Ty,
    literal_ty: Ty,
    match_range: Option<TextRange>,
    extract: Extract,
    to_prim: ToPrim,
) -> core::Expr
where
    T: Eq + Hash + Copy,
    Extract: Fn(&Prim) -> Option<T>,
    ToPrim: Fn(T) -> Prim,
{
    let body_ty = rows.first().map(|r| r.get_ty()).unwrap_or(Ty::TUnit);

    let mut value_rows: IndexMap<T, Vec<Row>> = IndexMap::new();
    let mut fallback_rows: Vec<Row> = Vec::new();
    let mut default_rows: Vec<Row> = Vec::new();

    for mut row in rows {
        if let Some(col) = row.remove_column(&bvar.name) {
            let col_range = pat_range(&col.pat).or(match_range);
            match col.pat {
                Pat::PPrim { value, ty: _, .. } => {
                    if let Some(key) = extract(&value) {
                        let entry = value_rows
                            .entry(key)
                            .or_insert_with(|| fallback_rows.clone());
                        entry.push(row);
                    } else {
                        push_compile_ice(
                            diagnostics,
                            "expected integer literal pattern while compiling integer match",
                            col_range,
                        );
                        let row_clone = row.clone();
                        for rows in value_rows.values_mut() {
                            rows.push(row_clone.clone());
                        }
                        fallback_rows.push(row_clone.clone());
                        default_rows.push(row);
                    }
                }
                Pat::PWild { .. } => {
                    let row_clone = row.clone();
                    for rows in value_rows.values_mut() {
                        rows.push(row_clone.clone());
                    }
                    fallback_rows.push(row_clone.clone());
                    default_rows.push(row);
                }
                _ => {
                    push_compile_ice(
                        diagnostics,
                        "expected integer pattern while compiling integer match",
                        col_range,
                    );
                    let row_clone = row.clone();
                    for rows in value_rows.values_mut() {
                        rows.push(row_clone.clone());
                    }
                    fallback_rows.push(row_clone.clone());
                    default_rows.push(row);
                }
            }
        } else {
            let row_clone = row.clone();
            for rows in value_rows.values_mut() {
                rows.push(row_clone.clone());
            }
            fallback_rows.push(row_clone.clone());
            default_rows.push(row);
        }
    }

    if default_rows.is_empty() {
        let type_name = match &literal_ty {
            Ty::TInt8 => "int8",
            Ty::TInt16 => "int16",
            Ty::TInt32 => "int32",
            Ty::TInt64 => "int64",
            Ty::TUint8 => "uint8",
            Ty::TUint16 => "uint16",
            Ty::TUint32 => "uint32",
            Ty::TUint64 => "uint64",
            Ty::TChar => "char",
            _ => "integer",
        };
        let message = format!(
            "non-exhaustive match on {} literal; add a wildcard arm `_`",
            type_name
        );
        push_compile_error(diagnostics, message, match_range);
        return emissing(ty);
    }

    let arms = value_rows
        .into_iter()
        .map(|(value, rows)| core::Arm {
            lhs: core::Expr::EPrim {
                value: to_prim(value),
                ty: literal_ty.clone(),
            },
            body: compile_rows(genv, gensym, diagnostics, rows, ty, match_range),
        })
        .collect();

    let default = if default_rows.is_empty() {
        None
    } else {
        Some(Box::new(compile_rows(
            genv,
            gensym,
            diagnostics,
            default_rows,
            ty,
            match_range,
        )))
    };

    core::Expr::EMatch {
        expr: Box::new(bvar.to_core()),
        arms,
        default,
        ty: body_ty,
    }
}

#[allow(clippy::too_many_arguments)]
fn compile_float_case(
    genv: &GlobalTypeEnv,
    gensym: &Gensym,
    diagnostics: &mut Diagnostics,
    rows: Vec<Row>,
    bvar: &Variable,
    ty: &Ty,
    literal_ty: Ty,
    match_range: Option<TextRange>,
) -> core::Expr {
    let body_ty = rows.first().map(|r| r.get_ty()).unwrap_or(Ty::TUnit);

    let mut value_rows: IndexMap<u64, (Prim, Vec<Row>)> = IndexMap::new();
    let mut fallback_rows: Vec<Row> = Vec::new();
    let mut default_rows: Vec<Row> = Vec::new();

    for mut row in rows {
        if let Some(col) = row.remove_column(&bvar.name) {
            let col_range = pat_range(&col.pat).or(match_range);
            match col.pat {
                Pat::PPrim {
                    ref value, ty: _, ..
                } => {
                    let bits = match &literal_ty {
                        Ty::TFloat32 => value.as_float32().map(|v| v.to_bits() as u64),
                        Ty::TFloat64 => value.as_float64().map(|v| v.to_bits()),
                        _ => None,
                    };
                    if let Some(key) = bits {
                        let entry = value_rows
                            .entry(key)
                            .or_insert_with(|| (value.clone(), fallback_rows.clone()));
                        entry.1.push(row);
                    } else {
                        push_compile_ice(
                            diagnostics,
                            "expected float literal pattern while compiling float match",
                            col_range,
                        );
                        let row_clone = row.clone();
                        for (_, rows) in value_rows.values_mut() {
                            rows.push(row_clone.clone());
                        }
                        fallback_rows.push(row_clone.clone());
                        default_rows.push(row);
                    }
                }
                Pat::PWild { .. } => {
                    let row_clone = row.clone();
                    for (_, rows) in value_rows.values_mut() {
                        rows.push(row_clone.clone());
                    }
                    fallback_rows.push(row_clone.clone());
                    default_rows.push(row);
                }
                _ => {
                    push_compile_ice(
                        diagnostics,
                        "expected float pattern while compiling float match",
                        col_range,
                    );
                    let row_clone = row.clone();
                    for (_, rows) in value_rows.values_mut() {
                        rows.push(row_clone.clone());
                    }
                    fallback_rows.push(row_clone.clone());
                    default_rows.push(row);
                }
            }
        } else {
            let row_clone = row.clone();
            for (_, rows) in value_rows.values_mut() {
                rows.push(row_clone.clone());
            }
            fallback_rows.push(row_clone.clone());
            default_rows.push(row);
        }
    }

    if default_rows.is_empty() {
        let type_name = match &literal_ty {
            Ty::TFloat32 => "float32",
            Ty::TFloat64 => "float64",
            _ => "float",
        };
        let message = format!(
            "non-exhaustive match on {} literal; add a wildcard arm `_`",
            type_name
        );
        push_compile_error(diagnostics, message, match_range);
        return emissing(ty);
    }

    let arms = value_rows
        .into_iter()
        .map(|(_, (prim, rows))| core::Arm {
            lhs: core::Expr::EPrim {
                value: prim,
                ty: literal_ty.clone(),
            },
            body: compile_rows(genv, gensym, diagnostics, rows, ty, match_range),
        })
        .collect();

    let default = if default_rows.is_empty() {
        None
    } else {
        Some(Box::new(compile_rows(
            genv,
            gensym,
            diagnostics,
            default_rows,
            ty,
            match_range,
        )))
    };

    core::Expr::EMatch {
        expr: Box::new(bvar.to_core()),
        arms,
        default,
        ty: body_ty,
    }
}

fn compile_string_case(
    genv: &GlobalTypeEnv,
    gensym: &Gensym,
    diagnostics: &mut Diagnostics,
    rows: Vec<Row>,
    bvar: &Variable,
    ty: &Ty,
    match_range: Option<TextRange>,
) -> core::Expr {
    let body_ty = rows.first().map(|r| r.get_ty()).unwrap_or(Ty::TUnit);

    let mut value_rows: IndexMap<String, Vec<Row>> = IndexMap::new();
    let mut fallback_rows: Vec<Row> = Vec::new();
    let mut default_rows: Vec<Row> = Vec::new();

    for mut row in rows {
        if let Some(col) = row.remove_column(&bvar.name) {
            let col_range = pat_range(&col.pat).or(match_range);
            match col.pat {
                Pat::PPrim { value, ty: _, .. } => {
                    if let Some(key) = value.as_str() {
                        let entry = value_rows
                            .entry(key.to_string())
                            .or_insert_with(|| fallback_rows.clone());
                        entry.push(row);
                    } else {
                        push_compile_ice(
                            diagnostics,
                            "expected string literal pattern while compiling string match",
                            col_range,
                        );
                        let row_clone = row.clone();
                        for rows in value_rows.values_mut() {
                            rows.push(row_clone.clone());
                        }
                        fallback_rows.push(row_clone.clone());
                        default_rows.push(row);
                    }
                }
                Pat::PWild { .. } => {
                    let row_clone = row.clone();
                    for rows in value_rows.values_mut() {
                        rows.push(row_clone.clone());
                    }
                    fallback_rows.push(row_clone.clone());
                    default_rows.push(row);
                }
                _ => {
                    push_compile_ice(
                        diagnostics,
                        "expected string pattern while compiling string match",
                        col_range,
                    );
                    let row_clone = row.clone();
                    for rows in value_rows.values_mut() {
                        rows.push(row_clone.clone());
                    }
                    fallback_rows.push(row_clone.clone());
                    default_rows.push(row);
                }
            }
        } else {
            let row_clone = row.clone();
            for rows in value_rows.values_mut() {
                rows.push(row_clone.clone());
            }
            fallback_rows.push(row_clone.clone());
            default_rows.push(row);
        }
    }

    if default_rows.is_empty() {
        let message = "non-exhaustive match on string literal; add a wildcard arm `_`".to_string();
        push_compile_error(diagnostics, message, match_range);
        return emissing(ty);
    }

    let arms = value_rows
        .into_iter()
        .map(|(value, rows)| core::Arm {
            lhs: core::Expr::EPrim {
                value: Prim::string(value),
                ty: Ty::TString,
            },
            body: compile_rows(genv, gensym, diagnostics, rows, ty, match_range),
        })
        .collect();

    let default = Some(Box::new(compile_rows(
        genv,
        gensym,
        diagnostics,
        default_rows,
        ty,
        match_range,
    )));

    core::Expr::EMatch {
        expr: Box::new(bvar.to_core()),
        arms,
        default,
        ty: body_ty,
    }
}

fn compile_rows(
    genv: &GlobalTypeEnv,
    gensym: &Gensym,
    diagnostics: &mut Diagnostics,
    mut rows: Vec<Row>,
    ty: &Ty,
    match_range: Option<TextRange>,
) -> core::Expr {
    if rows.is_empty() {
        return emissing(ty);
    }
    for row in &mut rows {
        move_variable_patterns(row);
    }

    if rows.first().is_some_and(|c| c.columns.is_empty()) {
        let row = rows.remove(0);
        return compile_expr(&row.body, genv, gensym, diagnostics);
    }

    let bvar = branch_variable(&rows);
    match &bvar.ty {
        Ty::TVar(..) => {
            push_compile_ice(
                diagnostics,
                "unresolved type variable reached match compilation",
                match_range,
            );
            emissing(ty)
        }
        Ty::TUnit => compile_unit_case(genv, gensym, diagnostics, rows, &bvar, match_range),
        Ty::TBool => compile_bool_case(genv, gensym, diagnostics, rows, &bvar, match_range),
        Ty::TInt32 => compile_int_case(
            genv,
            gensym,
            diagnostics,
            rows,
            &bvar,
            ty,
            Ty::TInt32,
            match_range,
        ),
        Ty::TInt8 => compile_int_case(
            genv,
            gensym,
            diagnostics,
            rows,
            &bvar,
            ty,
            Ty::TInt8,
            match_range,
        ),
        Ty::TInt16 => compile_int_case(
            genv,
            gensym,
            diagnostics,
            rows,
            &bvar,
            ty,
            Ty::TInt16,
            match_range,
        ),
        Ty::TInt64 => compile_int_case(
            genv,
            gensym,
            diagnostics,
            rows,
            &bvar,
            ty,
            Ty::TInt64,
            match_range,
        ),
        Ty::TUint8 => compile_int_case(
            genv,
            gensym,
            diagnostics,
            rows,
            &bvar,
            ty,
            Ty::TUint8,
            match_range,
        ),
        Ty::TUint16 => compile_int_case(
            genv,
            gensym,
            diagnostics,
            rows,
            &bvar,
            ty,
            Ty::TUint16,
            match_range,
        ),
        Ty::TUint32 => compile_int_case(
            genv,
            gensym,
            diagnostics,
            rows,
            &bvar,
            ty,
            Ty::TUint32,
            match_range,
        ),
        Ty::TUint64 => compile_int_case(
            genv,
            gensym,
            diagnostics,
            rows,
            &bvar,
            ty,
            Ty::TUint64,
            match_range,
        ),
        Ty::TFloat32 | Ty::TFloat64 => compile_float_case(
            genv,
            gensym,
            diagnostics,
            rows,
            &bvar,
            ty,
            bvar.ty.clone(),
            match_range,
        ),
        Ty::TString => compile_string_case(genv, gensym, diagnostics, rows, &bvar, ty, match_range),
        Ty::TChar => compile_int_case(
            genv,
            gensym,
            diagnostics,
            rows,
            &bvar,
            ty,
            Ty::TChar,
            match_range,
        ),
        Ty::TEnum { name } => {
            let ident = TastIdent::new(name);
            compile_enum_case(
                genv,
                gensym,
                diagnostics,
                rows,
                &bvar,
                ty,
                &ident,
                match_range,
            )
        }
        Ty::TStruct { name } => {
            let ident = TastIdent::new(name);
            compile_struct_case(
                genv,
                gensym,
                diagnostics,
                rows,
                &bvar,
                ty,
                &ident,
                &[],
                match_range,
            )
        }
        Ty::TApp { ty: base, args } => match base.as_ref() {
            Ty::TEnum { name } => {
                let ident = TastIdent::new(name);
                compile_enum_case(
                    genv,
                    gensym,
                    diagnostics,
                    rows,
                    &bvar,
                    ty,
                    &ident,
                    match_range,
                )
            }
            Ty::TStruct { name } => {
                let ident = TastIdent::new(name);
                compile_struct_case(
                    genv,
                    gensym,
                    diagnostics,
                    rows,
                    &bvar,
                    ty,
                    &ident,
                    args,
                    match_range,
                )
            }
            _ => {
                push_compile_ice(
                    diagnostics,
                    format!(
                        "expected enum or struct type constructor inside applied type in match, got {:?}",
                        base
                    ),
                    match_range,
                );
                emissing(ty)
            }
        },
        Ty::TTuple { typs } => compile_tuple_case(
            genv,
            gensym,
            diagnostics,
            rows,
            &bvar,
            typs,
            ty,
            match_range,
        ),
        Ty::TArray { .. } => {
            push_compile_error(
                diagnostics,
                "matching on array types is not supported",
                match_range,
            );
            emissing(ty)
        }
        Ty::TVec { .. } => {
            push_compile_error(
                diagnostics,
                "matching on Vec types is not supported",
                match_range,
            );
            emissing(ty)
        }
        Ty::TSlice { .. } => {
            push_compile_error(
                diagnostics,
                "matching on Slice types is not supported",
                match_range,
            );
            emissing(ty)
        }
        Ty::THashMap { .. } => {
            push_compile_error(
                diagnostics,
                "matching on HashMap types is not supported",
                match_range,
            );
            emissing(ty)
        }
        Ty::TFunc { .. } => {
            push_compile_ice(
                diagnostics,
                "function type reached match compilation unexpectedly",
                match_range,
            );
            emissing(ty)
        }
        Ty::TParam { .. } => {
            push_compile_ice(
                diagnostics,
                "type parameter reached match compilation unexpectedly",
                match_range,
            );
            emissing(ty)
        }
        Ty::TRef { .. } => {
            push_compile_error(
                diagnostics,
                "matching on reference types is not supported",
                match_range,
            );
            emissing(ty)
        }
        Ty::TDyn { .. } => {
            push_compile_error(
                diagnostics,
                "matching on dyn trait objects is not supported",
                match_range,
            );
            emissing(ty)
        }
    }
}

fn prepend_lets_to_expr(let_stmts: Vec<core::LetStmt>, body: core::Expr, ty: &Ty) -> core::Expr {
    if let_stmts.is_empty() {
        body
    } else {
        core::Expr::EBlock {
            block: Box::new(core::Block {
                stmts: let_stmts,
                tail: Some(Box::new(body)),
            }),
            ty: ty.clone(),
        }
    }
}

type HiddenMutCells = HashMap<String, Ty>;

fn collect_hidden_mut_cells(block: &tast::Block) -> HiddenMutCells {
    let mut mutable_bindings = HashMap::new();
    collect_mutable_bindings_block(block, &mut mutable_bindings);

    let mut captured = HashSet::new();
    collect_captured_names_block(block, &mut captured);

    mutable_bindings.retain(|name, _| captured.contains(name));
    mutable_bindings
}

fn collect_mutable_bindings_block(block: &tast::Block, mutable_bindings: &mut HiddenMutCells) {
    for stmt in &block.stmts {
        match stmt {
            tast::Stmt::Let(tast::LetStmt { is_mut, pat, value }) => {
                if *is_mut && let Pat::PVar { name, ty, .. } = pat {
                    mutable_bindings.insert(name.clone(), ty.clone());
                }
                collect_mutable_bindings_expr(value, mutable_bindings);
            }
            tast::Stmt::Assign(tast::AssignStmt { target, value }) => {
                collect_mutable_bindings_expr(target, mutable_bindings);
                collect_mutable_bindings_expr(value, mutable_bindings);
            }
            tast::Stmt::Expr(tast::ExprStmt { expr }) => {
                collect_mutable_bindings_expr(expr, mutable_bindings);
            }
        }
    }
    if let Some(tail) = &block.tail {
        collect_mutable_bindings_expr(tail, mutable_bindings);
    }
}

fn collect_mutable_bindings_expr(expr: &Expr, mutable_bindings: &mut HiddenMutCells) {
    match expr {
        EVar { .. }
        | EPrim { .. }
        | EBreak { .. }
        | EContinue { .. }
        | ETraitMethod { .. }
        | EDynTraitMethod { .. }
        | EInherentMethod { .. } => {}
        EReturn { expr, .. } => {
            if let Some(expr) = expr {
                collect_mutable_bindings_expr(expr, mutable_bindings);
            }
        }
        EConstr { args, .. } => {
            for arg in args {
                collect_mutable_bindings_expr(arg, mutable_bindings);
            }
        }
        ETuple { items, .. } | EArray { items, .. } => {
            for item in items {
                collect_mutable_bindings_expr(item, mutable_bindings);
            }
        }
        EClosure { body, .. } => {
            collect_mutable_bindings_expr(body, mutable_bindings);
        }
        EBlock { block, .. } => {
            collect_mutable_bindings_block(block, mutable_bindings);
        }
        EMatch { expr, arms, .. } => {
            collect_mutable_bindings_expr(expr, mutable_bindings);
            for arm in arms {
                collect_mutable_bindings_expr(&arm.body, mutable_bindings);
            }
        }
        EIf {
            cond,
            then_branch,
            else_branch,
            ..
        } => {
            collect_mutable_bindings_expr(cond, mutable_bindings);
            collect_mutable_bindings_expr(then_branch, mutable_bindings);
            collect_mutable_bindings_expr(else_branch, mutable_bindings);
        }
        EWhile { cond, body, .. } => {
            collect_mutable_bindings_expr(cond, mutable_bindings);
            collect_mutable_bindings_expr(body, mutable_bindings);
        }
        EGo { expr, .. } => {
            collect_mutable_bindings_expr(expr, mutable_bindings);
        }
        ECall { func, args, .. } => {
            collect_mutable_bindings_expr(func, mutable_bindings);
            for arg in args {
                collect_mutable_bindings_expr(arg, mutable_bindings);
            }
        }
        EUnary { expr, .. }
        | EProj { tuple: expr, .. }
        | EField { expr, .. }
        | EToDyn { expr, .. } => {
            collect_mutable_bindings_expr(expr, mutable_bindings);
        }
        EIndex { base, index, .. } => {
            collect_mutable_bindings_expr(base, mutable_bindings);
            collect_mutable_bindings_expr(index, mutable_bindings);
        }
        EBinary { lhs, rhs, .. } => {
            collect_mutable_bindings_expr(lhs, mutable_bindings);
            collect_mutable_bindings_expr(rhs, mutable_bindings);
        }
    }
}

fn collect_captured_names_block(block: &tast::Block, captured: &mut HashSet<String>) {
    for stmt in &block.stmts {
        match stmt {
            tast::Stmt::Let(tast::LetStmt { value, .. }) => {
                collect_captured_names_expr(value, captured)
            }
            tast::Stmt::Assign(tast::AssignStmt { target, value }) => {
                collect_captured_names_expr(target, captured);
                collect_captured_names_expr(value, captured);
            }
            tast::Stmt::Expr(tast::ExprStmt { expr }) => {
                collect_captured_names_expr(expr, captured)
            }
        }
    }
    if let Some(tail) = &block.tail {
        collect_captured_names_expr(tail, captured);
    }
}

fn collect_captured_names_expr(expr: &Expr, captured: &mut HashSet<String>) {
    match expr {
        EVar { .. }
        | EPrim { .. }
        | EBreak { .. }
        | EContinue { .. }
        | ETraitMethod { .. }
        | EDynTraitMethod { .. }
        | EInherentMethod { .. } => {}
        EReturn { expr, .. } => {
            if let Some(expr) = expr {
                collect_captured_names_expr(expr, captured);
            }
        }
        EConstr { args, .. } => {
            for arg in args {
                collect_captured_names_expr(arg, captured);
            }
        }
        ETuple { items, .. } | EArray { items, .. } => {
            for item in items {
                collect_captured_names_expr(item, captured);
            }
        }
        EClosure { body, captures, .. } => {
            for (name, _) in captures {
                captured.insert(name.clone());
            }
            collect_captured_names_expr(body, captured);
        }
        EBlock { block, .. } => {
            collect_captured_names_block(block, captured);
        }
        EMatch { expr, arms, .. } => {
            collect_captured_names_expr(expr, captured);
            for arm in arms {
                collect_captured_names_expr(&arm.body, captured);
            }
        }
        EIf {
            cond,
            then_branch,
            else_branch,
            ..
        } => {
            collect_captured_names_expr(cond, captured);
            collect_captured_names_expr(then_branch, captured);
            collect_captured_names_expr(else_branch, captured);
        }
        EWhile { cond, body, .. } => {
            collect_captured_names_expr(cond, captured);
            collect_captured_names_expr(body, captured);
        }
        EGo { expr, .. } => {
            collect_captured_names_expr(expr, captured);
        }
        ECall { func, args, .. } => {
            collect_captured_names_expr(func, captured);
            for arg in args {
                collect_captured_names_expr(arg, captured);
            }
        }
        EUnary { expr, .. }
        | EProj { tuple: expr, .. }
        | EField { expr, .. }
        | EToDyn { expr, .. } => {
            collect_captured_names_expr(expr, captured);
        }
        EIndex { base, index, .. } => {
            collect_captured_names_expr(base, captured);
            collect_captured_names_expr(index, captured);
        }
        EBinary { lhs, rhs, .. } => {
            collect_captured_names_expr(lhs, captured);
            collect_captured_names_expr(rhs, captured);
        }
    }
}

fn hidden_cell_ty(inner_ty: &Ty) -> Ty {
    Ty::TRef {
        elem: Box::new(inner_ty.clone()),
    }
}

fn hidden_cell_var(name: &str, inner_ty: &Ty) -> core::Expr {
    core::Expr::EVar {
        name: name.to_string(),
        ty: hidden_cell_ty(inner_ty),
    }
}

fn hidden_ref_alloc(value: core::Expr, inner_ty: &Ty) -> core::Expr {
    let ref_ty = hidden_cell_ty(inner_ty);
    core::Expr::ECall {
        func: Box::new(core::Expr::EVar {
            name: "ref".to_string(),
            ty: Ty::TFunc {
                params: vec![inner_ty.clone()],
                ret_ty: Box::new(ref_ty.clone()),
            },
        }),
        args: vec![value],
        ty: ref_ty,
    }
}

fn hidden_ref_get(name: &str, inner_ty: &Ty) -> core::Expr {
    let ref_ty = hidden_cell_ty(inner_ty);
    core::Expr::ECall {
        func: Box::new(core::Expr::EVar {
            name: "ref_get".to_string(),
            ty: Ty::TFunc {
                params: vec![ref_ty.clone()],
                ret_ty: Box::new(inner_ty.clone()),
            },
        }),
        args: vec![hidden_cell_var(name, inner_ty)],
        ty: inner_ty.clone(),
    }
}

fn hidden_ref_set(name: &str, inner_ty: &Ty, value: core::Expr) -> core::Expr {
    let ref_ty = hidden_cell_ty(inner_ty);
    core::Expr::ECall {
        func: Box::new(core::Expr::EVar {
            name: "ref_set".to_string(),
            ty: Ty::TFunc {
                params: vec![ref_ty.clone(), inner_ty.clone()],
                ret_ty: Box::new(Ty::TUnit),
            },
        }),
        args: vec![hidden_cell_var(name, inner_ty), value],
        ty: Ty::TUnit,
    }
}

fn lower_hidden_mut_block(block: core::Block, hidden_mut_cells: &HiddenMutCells) -> core::Block {
    let stmts = block
        .stmts
        .into_iter()
        .map(|stmt| lower_hidden_mut_let(stmt, hidden_mut_cells))
        .collect();
    let tail = block
        .tail
        .map(|tail| Box::new(lower_hidden_mut_expr(*tail, hidden_mut_cells)));
    core::Block { stmts, tail }
}

fn lower_hidden_mut_let(stmt: core::LetStmt, hidden_mut_cells: &HiddenMutCells) -> core::LetStmt {
    let core::LetStmt { name, value, ty } = stmt;
    let value = lower_hidden_mut_expr(value, hidden_mut_cells);
    if let Some(inner_ty) = hidden_mut_cells.get(&name) {
        core::LetStmt {
            name,
            value: hidden_ref_alloc(value, inner_ty),
            ty: hidden_cell_ty(inner_ty),
        }
    } else {
        core::LetStmt { name, value, ty }
    }
}

fn lower_hidden_mut_expr(expr: core::Expr, hidden_mut_cells: &HiddenMutCells) -> core::Expr {
    match expr {
        core::Expr::EVar { name, ty } => {
            if let Some(inner_ty) = hidden_mut_cells.get(&name) {
                hidden_ref_get(&name, inner_ty)
            } else {
                core::Expr::EVar { name, ty }
            }
        }
        core::Expr::EPrim { value, ty } => core::Expr::EPrim { value, ty },
        core::Expr::EConstr {
            constructor,
            args,
            ty,
        } => core::Expr::EConstr {
            constructor,
            args: args
                .into_iter()
                .map(|arg| lower_hidden_mut_expr(arg, hidden_mut_cells))
                .collect(),
            ty,
        },
        core::Expr::ETuple { items, ty } => core::Expr::ETuple {
            items: items
                .into_iter()
                .map(|item| lower_hidden_mut_expr(item, hidden_mut_cells))
                .collect(),
            ty,
        },
        core::Expr::EArray { items, ty } => core::Expr::EArray {
            items: items
                .into_iter()
                .map(|item| lower_hidden_mut_expr(item, hidden_mut_cells))
                .collect(),
            ty,
        },
        core::Expr::EClosure { params, body, ty } => core::Expr::EClosure {
            params,
            body: Box::new(lower_hidden_mut_expr(*body, hidden_mut_cells)),
            ty,
        },
        core::Expr::EBlock { block, ty } => core::Expr::EBlock {
            block: Box::new(lower_hidden_mut_block(*block, hidden_mut_cells)),
            ty,
        },
        core::Expr::EMatch {
            expr,
            arms,
            default,
            ty,
        } => core::Expr::EMatch {
            expr: Box::new(lower_hidden_mut_expr(*expr, hidden_mut_cells)),
            arms: arms
                .into_iter()
                .map(|arm| core::Arm {
                    lhs: lower_hidden_mut_expr(arm.lhs, hidden_mut_cells),
                    body: lower_hidden_mut_expr(arm.body, hidden_mut_cells),
                })
                .collect(),
            default: default
                .map(|default| Box::new(lower_hidden_mut_expr(*default, hidden_mut_cells))),
            ty,
        },
        core::Expr::EIf {
            cond,
            then_branch,
            else_branch,
            ty,
        } => core::Expr::EIf {
            cond: Box::new(lower_hidden_mut_expr(*cond, hidden_mut_cells)),
            then_branch: Box::new(lower_hidden_mut_expr(*then_branch, hidden_mut_cells)),
            else_branch: Box::new(lower_hidden_mut_expr(*else_branch, hidden_mut_cells)),
            ty,
        },
        core::Expr::EWhile { cond, body, ty } => core::Expr::EWhile {
            cond: Box::new(lower_hidden_mut_expr(*cond, hidden_mut_cells)),
            body: Box::new(lower_hidden_mut_expr(*body, hidden_mut_cells)),
            ty,
        },
        core::Expr::EBreak { ty } => core::Expr::EBreak { ty },
        core::Expr::EContinue { ty } => core::Expr::EContinue { ty },
        core::Expr::EReturn { expr, ty } => core::Expr::EReturn {
            expr: expr.map(|expr| Box::new(lower_hidden_mut_expr(*expr, hidden_mut_cells))),
            ty,
        },
        core::Expr::EGo { expr, ty } => core::Expr::EGo {
            expr: Box::new(lower_hidden_mut_expr(*expr, hidden_mut_cells)),
            ty,
        },
        core::Expr::EConstrGet {
            expr,
            constructor,
            field_index,
            ty,
        } => core::Expr::EConstrGet {
            expr: Box::new(lower_hidden_mut_expr(*expr, hidden_mut_cells)),
            constructor,
            field_index,
            ty,
        },
        core::Expr::EUnary { op, expr, ty } => core::Expr::EUnary {
            op,
            expr: Box::new(lower_hidden_mut_expr(*expr, hidden_mut_cells)),
            ty,
        },
        core::Expr::EBinary { op, lhs, rhs, ty } => core::Expr::EBinary {
            op,
            lhs: Box::new(lower_hidden_mut_expr(*lhs, hidden_mut_cells)),
            rhs: Box::new(lower_hidden_mut_expr(*rhs, hidden_mut_cells)),
            ty,
        },
        core::Expr::EAssign {
            name,
            value,
            target_ty,
            ty,
        } => {
            let value = lower_hidden_mut_expr(*value, hidden_mut_cells);
            if let Some(inner_ty) = hidden_mut_cells.get(&name) {
                hidden_ref_set(&name, inner_ty, value)
            } else {
                core::Expr::EAssign {
                    name,
                    value: Box::new(value),
                    target_ty,
                    ty,
                }
            }
        }
        core::Expr::ECall { func, args, ty } => core::Expr::ECall {
            func: Box::new(lower_hidden_mut_expr(*func, hidden_mut_cells)),
            args: args
                .into_iter()
                .map(|arg| lower_hidden_mut_expr(arg, hidden_mut_cells))
                .collect(),
            ty,
        },
        core::Expr::EToDyn {
            trait_name,
            for_ty,
            expr,
            ty,
        } => core::Expr::EToDyn {
            trait_name,
            for_ty,
            expr: Box::new(lower_hidden_mut_expr(*expr, hidden_mut_cells)),
            ty,
        },
        core::Expr::EDynCall {
            trait_name,
            method_name,
            receiver,
            args,
            ty,
        } => core::Expr::EDynCall {
            trait_name,
            method_name,
            receiver: Box::new(lower_hidden_mut_expr(*receiver, hidden_mut_cells)),
            args: args
                .into_iter()
                .map(|arg| lower_hidden_mut_expr(arg, hidden_mut_cells))
                .collect(),
            ty,
        },
        core::Expr::ETraitCall {
            trait_name,
            method_name,
            receiver,
            args,
            ty,
        } => core::Expr::ETraitCall {
            trait_name,
            method_name,
            receiver: Box::new(lower_hidden_mut_expr(*receiver, hidden_mut_cells)),
            args: args
                .into_iter()
                .map(|arg| lower_hidden_mut_expr(arg, hidden_mut_cells))
                .collect(),
            ty,
        },
        core::Expr::EProj { tuple, index, ty } => core::Expr::EProj {
            tuple: Box::new(lower_hidden_mut_expr(*tuple, hidden_mut_cells)),
            index,
            ty,
        },
    }
}

pub fn compile_file(
    genv: &GlobalTypeEnv,
    gensym: &Gensym,
    diagnostics: &mut Diagnostics,
    file: &File,
) -> core::File {
    let mut toplevels = vec![];
    for item in file.toplevels.iter() {
        match item {
            tast::Item::ImplBlock(impl_block) => {
                let for_ty = &impl_block.for_type;
                for m in impl_block.methods.iter() {
                    let method_name = &m.name;
                    let func_name = if let Some(trait_name) = &impl_block.trait_name {
                        trait_impl_fn_name(trait_name, for_ty, method_name)
                    } else {
                        inherent_method_fn_name(for_ty, method_name)
                    };

                    let f = core::Fn {
                        name: func_name,
                        generics: impl_block.generics.clone(),
                        params: m
                            .params
                            .iter()
                            .map(|(name, ty)| (name.clone(), ty.clone()))
                            .collect(),
                        ret_ty: m.ret_ty.clone(),
                        body: {
                            let hidden_mut_cells = collect_hidden_mut_cells(&m.body);
                            let body = compile_block(&m.body, &m.ret_ty, genv, gensym, diagnostics);
                            if hidden_mut_cells.is_empty() {
                                body
                            } else {
                                lower_hidden_mut_block(body, &hidden_mut_cells)
                            }
                        },
                    };

                    toplevels.push(f);
                }
            }
            tast::Item::Fn(f) => {
                toplevels.push(core::Fn {
                    name: f.name.clone(),
                    generics: vec![],
                    params: f
                        .params
                        .iter()
                        .map(|(name, ty)| (name.clone(), ty.clone()))
                        .collect(),
                    ret_ty: f.ret_ty.clone(),
                    body: {
                        let hidden_mut_cells = collect_hidden_mut_cells(&f.body);
                        let body = compile_block(&f.body, &f.ret_ty, genv, gensym, diagnostics);
                        if hidden_mut_cells.is_empty() {
                            body
                        } else {
                            lower_hidden_mut_block(body, &hidden_mut_cells)
                        }
                    },
                });
            }
            tast::Item::ExternGo(_) => {}
            tast::Item::ExternType(_) => {}
        }
    }
    core::File { toplevels }
}

fn block_expr_from_parts(stmts: &[tast::Stmt], tail: Option<&Expr>, ty: &Ty) -> Expr {
    Expr::EBlock {
        block: Box::new(tast::Block {
            stmts: stmts.to_vec(),
            tail: tail.cloned().map(Box::new),
        }),
        ty: ty.clone(),
    }
}

fn compile_assign_action(
    target: &Expr,
    value: &Expr,
    genv: &GlobalTypeEnv,
    gensym: &Gensym,
    diagnostics: &mut Diagnostics,
) -> core::Expr {
    match target {
        Expr::EVar { name, ty, .. } => core::Expr::EAssign {
            name: name.clone(),
            value: Box::new(compile_expr(value, genv, gensym, diagnostics)),
            target_ty: ty.clone(),
            ty: Ty::TUnit,
        },
        Expr::EIndex { .. } => {
            compile_index_assign_action(target, value, genv, gensym, diagnostics)
        }
        _ => {
            push_compile_error(
                diagnostics,
                "unsupported assignment target",
                expr_range(target),
            );
            emissing(&Ty::TUnit)
        }
    }
}

fn compile_index_assign_action(
    target: &Expr,
    value: &Expr,
    genv: &GlobalTypeEnv,
    gensym: &Gensym,
    diagnostics: &mut Diagnostics,
) -> core::Expr {
    let Some((root, steps)) = decompose_place(target) else {
        push_compile_error(
            diagnostics,
            "unsupported indexed assignment target",
            expr_range(target),
        );
        return emissing(&Ty::TUnit);
    };

    if steps.is_empty() {
        push_compile_error(
            diagnostics,
            "unsupported indexed assignment target",
            expr_range(target),
        );
        return emissing(&Ty::TUnit);
    }

    let mut stmts = Vec::new();
    let (compiled_root, mut current_var, mut current_ty) = match root {
        PlaceRoot::Local { name, ty } => {
            let root_var = gensym.gensym("place_root");
            stmts.push(core::LetStmt {
                name: root_var.clone(),
                value: core_var(name.clone(), &ty),
                ty: ty.clone(),
            });
            (
                CompiledPlaceRoot::Local {
                    name,
                    ty: ty.clone(),
                },
                root_var,
                ty,
            )
        }
        PlaceRoot::Ref { expr, ty } => {
            let ref_ty = expr.get_ty();
            let ref_var = gensym.gensym("place_ref");
            stmts.push(core::LetStmt {
                name: ref_var.clone(),
                value: compile_expr(&expr, genv, gensym, diagnostics),
                ty: ref_ty.clone(),
            });
            let root_var = gensym.gensym("place_root");
            stmts.push(core::LetStmt {
                name: root_var.clone(),
                value: compile_ref_get_core(core_var(ref_var.clone(), &ref_ty), &ref_ty, &ty),
                ty: ty.clone(),
            });
            (
                CompiledPlaceRoot::Ref {
                    ref_var,
                    ref_ty: ref_ty.clone(),
                },
                root_var,
                ty,
            )
        }
        PlaceRoot::Value { expr, ty } => {
            let root_var = gensym.gensym("place_root");
            stmts.push(core::LetStmt {
                name: root_var.clone(),
                value: compile_expr(&expr, genv, gensym, diagnostics),
                ty: ty.clone(),
            });
            (CompiledPlaceRoot::Value, root_var, ty)
        }
    };

    let mut evaluated_steps = Vec::new();
    for step in steps {
        match step {
            PlaceStep::Proj {
                parent_ty,
                index,
                item_ty,
            } => {
                let parent_var = current_var.clone();
                let next_var = gensym.gensym("place");
                stmts.push(core::LetStmt {
                    name: next_var.clone(),
                    value: core::Expr::EProj {
                        tuple: Box::new(core_var(parent_var.clone(), &current_ty)),
                        index,
                        ty: item_ty.clone(),
                    },
                    ty: item_ty.clone(),
                });
                evaluated_steps.push(EvaluatedPlaceStep::Proj {
                    parent_var,
                    parent_ty,
                    index,
                });
                current_var = next_var;
                current_ty = item_ty;
            }
            PlaceStep::Field {
                parent_ty,
                field_name,
                item_ty,
                range,
            } => {
                let parent_var = current_var.clone();
                let next_var = gensym.gensym("place");
                stmts.push(core::LetStmt {
                    name: next_var.clone(),
                    value: compile_field_get_core(
                        genv,
                        diagnostics,
                        core_var(parent_var.clone(), &current_ty),
                        &parent_ty,
                        &field_name,
                        &item_ty,
                        range,
                    ),
                    ty: item_ty.clone(),
                });
                evaluated_steps.push(EvaluatedPlaceStep::Field {
                    parent_var,
                    parent_ty,
                    field_name,
                    range,
                });
                current_var = next_var;
                current_ty = item_ty;
            }
            PlaceStep::Index {
                container_ty,
                index,
                item_ty,
                range,
            } => {
                let parent_var = current_var.clone();
                let index_ty = index.get_ty();
                let index_var = gensym.gensym("index");
                stmts.push(core::LetStmt {
                    name: index_var.clone(),
                    value: compile_expr(&index, genv, gensym, diagnostics),
                    ty: index_ty.clone(),
                });
                let next_var = gensym.gensym("place");
                stmts.push(core::LetStmt {
                    name: next_var.clone(),
                    value: compile_index_read_core(
                        diagnostics,
                        core_var(parent_var.clone(), &current_ty),
                        &container_ty,
                        core_var(index_var.clone(), &index_ty),
                        &item_ty,
                        range,
                    ),
                    ty: item_ty.clone(),
                });
                evaluated_steps.push(EvaluatedPlaceStep::Index {
                    parent_var,
                    parent_ty: container_ty,
                    index_var,
                    index_ty,
                    range,
                });
                current_var = next_var;
                current_ty = item_ty;
            }
        }
    }

    let value_ty = value.get_ty();
    let value_var = gensym.gensym("value");
    stmts.push(core::LetStmt {
        name: value_var.clone(),
        value: compile_expr(value, genv, gensym, diagnostics),
        ty: value_ty.clone(),
    });

    let mut updated = core_var(value_var, &value_ty);
    let mut final_action = None;
    for step in evaluated_steps.into_iter().rev() {
        match step {
            EvaluatedPlaceStep::Proj {
                parent_var,
                parent_ty,
                index,
            } => {
                updated = rebuild_tuple_with_proj(
                    core_var(parent_var, &parent_ty),
                    &parent_ty,
                    index,
                    updated,
                );
            }
            EvaluatedPlaceStep::Field {
                parent_var,
                parent_ty,
                field_name,
                range,
            } => {
                updated = rebuild_struct_with_field(
                    genv,
                    diagnostics,
                    core_var(parent_var, &parent_ty),
                    &parent_ty,
                    &field_name,
                    updated,
                    range,
                );
            }
            EvaluatedPlaceStep::Index {
                parent_var,
                parent_ty,
                index_var,
                index_ty,
                range,
            } => match &parent_ty {
                Ty::TArray { .. } => {
                    updated = compile_array_set_core(
                        core_var(parent_var, &parent_ty),
                        core_var(index_var, &index_ty),
                        updated,
                    );
                }
                Ty::TVec { .. } => {
                    final_action = Some(compile_vec_set_core(
                        core_var(parent_var, &parent_ty),
                        core_var(index_var, &index_ty),
                        updated.clone(),
                    ));
                    break;
                }
                Ty::THashMap { .. } => {
                    final_action = Some(compile_hashmap_set_core(
                        core_var(parent_var, &parent_ty),
                        core_var(index_var, &index_ty),
                        updated.clone(),
                    ));
                    break;
                }
                Ty::TSlice { .. } => {
                    push_compile_error(
                        diagnostics,
                        "cannot assign through Slice indexing; Slice is read-only",
                        range,
                    );
                    final_action = Some(emissing(&Ty::TUnit));
                    break;
                }
                _ => {
                    push_compile_ice(
                        diagnostics,
                        format!(
                            "unexpected indexed assignment container type {:?}",
                            parent_ty
                        ),
                        range,
                    );
                    final_action = Some(emissing(&Ty::TUnit));
                    break;
                }
            },
        }
    }

    let final_action = final_action.unwrap_or_else(|| match compiled_root {
        CompiledPlaceRoot::Local { name, ty } => core::Expr::EAssign {
            name,
            value: Box::new(updated),
            target_ty: ty,
            ty: Ty::TUnit,
        },
        CompiledPlaceRoot::Ref { ref_var, ref_ty } => {
            compile_ref_set_core(core_var(ref_var, &ref_ty), updated, &ref_ty)
        }
        CompiledPlaceRoot::Value => {
            push_compile_error(
                diagnostics,
                "array indexed assignment requires a writable root such as a mutable local or `ref_get(...)`",
                expr_range(target),
            );
            emissing(&Ty::TUnit)
        }
    });

    core::Expr::EBlock {
        block: Box::new(core::Block {
            stmts,
            tail: Some(Box::new(final_action)),
        }),
        ty: Ty::TUnit,
    }
}

fn compile_block(
    block: &tast::Block,
    ty: &Ty,
    genv: &GlobalTypeEnv,
    gensym: &Gensym,
    diagnostics: &mut Diagnostics,
) -> core::Block {
    compile_block_parts(
        genv,
        gensym,
        diagnostics,
        &block.stmts,
        block.tail.as_deref(),
        ty,
    )
}

fn compile_block_parts(
    genv: &GlobalTypeEnv,
    gensym: &Gensym,
    diagnostics: &mut Diagnostics,
    stmts: &[tast::Stmt],
    tail: Option<&Expr>,
    ty: &Ty,
) -> core::Block {
    if stmts.is_empty() {
        return core::Block {
            stmts: vec![],
            tail: tail.map(|tail| Box::new(compile_expr(tail, genv, gensym, diagnostics))),
        };
    }

    let first = &stmts[0];
    let rest = &stmts[1..];

    match first {
        tast::Stmt::Let(tast::LetStmt {
            pat:
                Pat::PVar {
                    name,
                    ty: pat_ty,
                    astptr: _,
                },
            value,
            ..
        }) => {
            let core_value = compile_expr(value, genv, gensym, diagnostics);
            let mut rest_block = compile_block_parts(genv, gensym, diagnostics, rest, tail, ty);
            let mut new_stmts = Vec::with_capacity(rest_block.stmts.len() + 1);
            new_stmts.push(core::LetStmt {
                name: name.clone(),
                value: core_value,
                ty: pat_ty.clone(),
            });
            new_stmts.append(&mut rest_block.stmts);
            core::Block {
                stmts: new_stmts,
                tail: rest_block.tail,
            }
        }
        tast::Stmt::Let(tast::LetStmt { pat, value, .. }) => {
            let core_value = compile_expr(value, genv, gensym, diagnostics);
            let x = gensym.gensym("mtmp");
            let body_expr = block_expr_from_parts(rest, tail, ty);
            let rows = vec![
                Row {
                    columns: vec![Column {
                        var: x.clone(),
                        pat: pat.clone(),
                    }],
                    body: body_expr,
                },
                Row {
                    columns: vec![Column {
                        var: x.clone(),
                        pat: Pat::PWild {
                            ty: pat.get_ty(),
                            astptr: None,
                        },
                    }],
                    body: ECall {
                        func: Box::new(Expr::EVar {
                            name: "missing".to_string(),
                            ty: Ty::TFunc {
                                params: vec![Ty::TString],
                                ret_ty: Box::new(ty.clone()),
                            },
                            astptr: None,
                        }),
                        args: vec![Expr::EPrim {
                            value: Prim::string("".to_string()),
                            ty: Ty::TString,
                        }],
                        ty: ty.clone(),
                    },
                },
            ];
            core::Block {
                stmts: vec![core::LetStmt {
                    name: x,
                    value: core_value,
                    ty: ty.clone(),
                }],
                tail: Some(Box::new(compile_rows(
                    genv,
                    gensym,
                    diagnostics,
                    rows,
                    ty,
                    None,
                ))),
            }
        }
        tast::Stmt::Assign(tast::AssignStmt { target, value }) => {
            let assign = compile_assign_action(target, value, genv, gensym, diagnostics);
            let x = gensym.gensym("_wild");
            let mut rest_block = compile_block_parts(genv, gensym, diagnostics, rest, tail, ty);
            let mut new_stmts = Vec::with_capacity(rest_block.stmts.len() + 1);
            new_stmts.push(core::LetStmt {
                name: x,
                value: assign,
                ty: Ty::TUnit,
            });
            new_stmts.append(&mut rest_block.stmts);
            core::Block {
                stmts: new_stmts,
                tail: rest_block.tail,
            }
        }
        tast::Stmt::Expr(tast::ExprStmt { expr }) => {
            let core_value = compile_expr(expr, genv, gensym, diagnostics);
            let x = gensym.gensym("_wild");
            let mut rest_block = compile_block_parts(genv, gensym, diagnostics, rest, tail, ty);
            let mut new_stmts = Vec::with_capacity(rest_block.stmts.len() + 1);
            new_stmts.push(core::LetStmt {
                name: x,
                value: core_value,
                ty: expr.get_ty(),
            });
            new_stmts.append(&mut rest_block.stmts);
            core::Block {
                stmts: new_stmts,
                tail: rest_block.tail,
            }
        }
    }
}

fn compile_expr(
    e: &Expr,
    genv: &GlobalTypeEnv,
    gensym: &Gensym,
    diagnostics: &mut Diagnostics,
) -> core::Expr {
    match e {
        EVar {
            name,
            ty,
            astptr: _,
        } => core::Expr::EVar {
            name: name.to_string(),
            ty: ty.clone(),
        },
        EPrim { value, ty } => core::Expr::EPrim {
            value: value.clone(),
            ty: ty.clone(),
        },
        ETuple { items, ty } => {
            let items = items
                .iter()
                .map(|item| compile_expr(item, genv, gensym, diagnostics))
                .collect();
            core::Expr::ETuple {
                items,
                ty: ty.clone(),
            }
        }
        EArray { items, ty } => {
            let items = items
                .iter()
                .map(|item| compile_expr(item, genv, gensym, diagnostics))
                .collect();
            core::Expr::EArray {
                items,
                ty: ty.clone(),
            }
        }
        EConstr {
            constructor,
            args,
            ty,
        } => {
            let args = args
                .iter()
                .map(|arg| compile_expr(arg, genv, gensym, diagnostics))
                .collect();
            core::Expr::EConstr {
                constructor: constructor.clone(),
                args,
                ty: ty.clone(),
            }
        }
        EClosure {
            params,
            body,
            ty,
            captures: _,
        } => {
            let params = params.clone();
            let body = Box::new(compile_expr(body, genv, gensym, diagnostics));
            core::Expr::EClosure {
                params,
                body,
                ty: ty.clone(),
            }
        }
        EBlock { block, ty } => core::Expr::EBlock {
            block: Box::new(compile_block(block, ty, genv, gensym, diagnostics)),
            ty: ty.clone(),
        },
        EMatch {
            expr,
            arms,
            ty,
            astptr,
        } => match expr.as_ref() {
            EVar {
                name,
                ty: _ty,
                astptr: _,
            } => {
                let rows = make_rows(name, arms);
                let match_range = astptr.as_ref().map(|ptr| ptr.text_range());
                compile_rows(genv, gensym, diagnostics, rows, ty, match_range)
            }
            _ => {
                // create a new variable
                // match (a, b, c) { .. }
                // =>
                // let tmp = (a, b, c) in match tmp { ... }
                let mtmp = gensym.gensym("mtmp");
                let rows = make_rows(mtmp.as_str(), arms);
                let core_expr = compile_expr(expr, genv, gensym, diagnostics);
                let match_range = astptr.as_ref().map(|ptr| ptr.text_range());
                let core_rows = compile_rows(genv, gensym, diagnostics, rows, ty, match_range);
                core::Expr::EBlock {
                    block: Box::new(core::Block {
                        stmts: vec![core::LetStmt {
                            name: mtmp,
                            value: core_expr,
                            ty: ty.clone(),
                        }],
                        tail: Some(Box::new(core_rows)),
                    }),
                    ty: ty.clone(),
                }
            }
        },
        EIf {
            cond,
            then_branch,
            else_branch,
            ty,
        } => core::Expr::EIf {
            cond: Box::new(compile_expr(cond, genv, gensym, diagnostics)),
            then_branch: Box::new(compile_expr(then_branch, genv, gensym, diagnostics)),
            else_branch: Box::new(compile_expr(else_branch, genv, gensym, diagnostics)),
            ty: ty.clone(),
        },
        EWhile { cond, body, ty } => core::Expr::EWhile {
            cond: Box::new(compile_expr(cond, genv, gensym, diagnostics)),
            body: Box::new(compile_expr(body, genv, gensym, diagnostics)),
            ty: ty.clone(),
        },
        EBreak { ty } => core::Expr::EBreak { ty: ty.clone() },
        EContinue { ty } => core::Expr::EContinue { ty: ty.clone() },
        EReturn { expr, ty } => core::Expr::EReturn {
            expr: expr
                .as_ref()
                .map(|expr| Box::new(compile_expr(expr, genv, gensym, diagnostics))),
            ty: ty.clone(),
        },
        EGo { expr, ty } => core::Expr::EGo {
            expr: Box::new(compile_expr(expr, genv, gensym, diagnostics)),
            ty: ty.clone(),
        },
        EUnary {
            op,
            expr,
            ty,
            resolution,
        } => {
            let arg = compile_expr(expr, genv, gensym, diagnostics);

            match resolution {
                tast::UnaryResolution::Builtin => core::Expr::EUnary {
                    op: *op,
                    expr: Box::new(arg),
                    ty: ty.clone(),
                },
                tast::UnaryResolution::Overloaded { trait_name } => {
                    let method = op.method_name();
                    let self_ty = arg.get_ty();
                    let func_name = trait_impl_fn_name(trait_name, &self_ty, method);
                    core::Expr::ECall {
                        func: Box::new(core::Expr::EVar {
                            name: func_name,
                            ty: Ty::TFunc {
                                params: vec![self_ty.clone()],
                                ret_ty: Box::new(ty.clone()),
                            },
                        }),
                        args: vec![arg],
                        ty: ty.clone(),
                    }
                }
            }
        }
        EBinary {
            op,
            lhs,
            rhs,
            ty,
            resolution,
        } => {
            let lhs_expr = compile_expr(lhs, genv, gensym, diagnostics);
            let rhs_expr = compile_expr(rhs, genv, gensym, diagnostics);

            match resolution {
                tast::BinaryResolution::Builtin => core::Expr::EBinary {
                    op: *op,
                    lhs: Box::new(lhs_expr),
                    rhs: Box::new(rhs_expr),
                    ty: ty.clone(),
                },
                tast::BinaryResolution::Overloaded { trait_name } => {
                    let method = op.method_name();
                    let self_ty = lhs_expr.get_ty();
                    let func_name = trait_impl_fn_name(trait_name, &self_ty, method);
                    let param_tys = vec![lhs_expr.get_ty(), rhs_expr.get_ty()];
                    core::Expr::ECall {
                        func: Box::new(core::Expr::EVar {
                            name: func_name,
                            ty: Ty::TFunc {
                                params: param_tys,
                                ret_ty: Box::new(ty.clone()),
                            },
                        }),
                        args: vec![lhs_expr, rhs_expr],
                        ty: ty.clone(),
                    }
                }
            }
        }
        ECall { func, args, ty } => {
            let args = args
                .iter()
                .map(|arg| compile_expr(arg, genv, gensym, diagnostics))
                .collect::<Vec<_>>();

            if let tast::Expr::EDynTraitMethod {
                trait_name,
                method_name,
                ..
            } = func.as_ref()
            {
                let Some((receiver, other_args)) = args.split_first() else {
                    push_compile_ice(
                        diagnostics,
                        "dyn trait call expects a receiver argument",
                        expr_range(func),
                    );
                    return emissing(ty);
                };
                return core::Expr::EDynCall {
                    trait_name: trait_name.clone(),
                    method_name: method_name.clone(),
                    receiver: Box::new(receiver.clone()),
                    args: other_args.to_vec(),
                    ty: ty.clone(),
                };
            }

            if let tast::Expr::ETraitMethod {
                trait_name,
                method_name,
                ..
            } = func.as_ref()
            {
                let Some((receiver, other_args)) = args.split_first() else {
                    push_compile_ice(
                        diagnostics,
                        "trait call expects a receiver argument",
                        expr_range(func),
                    );
                    return emissing(ty);
                };
                let for_ty = receiver.get_ty();
                if (trait_name.0 == "ToString" && method_name.0 == "to_string")
                    || has_tparam(&for_ty)
                {
                    return core::Expr::ETraitCall {
                        trait_name: trait_name.clone(),
                        method_name: method_name.clone(),
                        receiver: Box::new(receiver.clone()),
                        args: other_args.to_vec(),
                        ty: ty.clone(),
                    };
                }
            }

            let func_expr = if let tast::Expr::ETraitMethod {
                trait_name,
                method_name,
                ty: method_ty,
                ..
            } = func.as_ref()
            {
                let Some(receiver) = args.first() else {
                    push_compile_ice(
                        diagnostics,
                        "trait call expects at least one argument",
                        expr_range(func),
                    );
                    return emissing(ty);
                };
                let for_ty = receiver.get_ty();
                core::Expr::EVar {
                    name: trait_impl_fn_name(trait_name, &for_ty, &method_name.0),
                    ty: method_ty.clone(),
                }
            } else if let tast::Expr::EInherentMethod {
                receiver_ty,
                method_name,
                ty: method_ty,
                ..
            } = func.as_ref()
            {
                let has_inherent = genv
                    .lookup_inherent_method_scheme(receiver_ty, method_name)
                    .is_some();
                if has_inherent {
                    core::Expr::EVar {
                        name: inherent_method_fn_name(receiver_ty, &method_name.0),
                        ty: method_ty.clone(),
                    }
                } else if let Some((type_name, type_args)) = decompose_struct_type(receiver_ty)
                    && let Some(struct_def) = genv.structs().get(&type_name)
                    && let Some(inst_fields) =
                        instantiate_struct_fields(diagnostics, struct_def, &type_args, None)
                    && let Some((field_index, (_, field_ty))) = inst_fields
                        .iter()
                        .enumerate()
                        .find(|(_, (fname, _))| fname == &method_name.0)
                    && matches!(field_ty, tast::Ty::TFunc { .. })
                {
                    let receiver_core = args[0].clone();
                    let field_core = core::Expr::EConstrGet {
                        expr: Box::new(receiver_core),
                        constructor: common::Constructor::Struct(common::StructConstructor {
                            type_name,
                        }),
                        field_index,
                        ty: field_ty.clone(),
                    };
                    let other_args = args[1..].to_vec();
                    return core::Expr::ECall {
                        func: Box::new(field_core),
                        args: other_args,
                        ty: ty.clone(),
                    };
                } else {
                    core::Expr::EVar {
                        name: inherent_method_fn_name(receiver_ty, &method_name.0),
                        ty: method_ty.clone(),
                    }
                }
            } else {
                compile_expr(func, genv, gensym, diagnostics)
            };

            core::Expr::ECall {
                func: Box::new(func_expr),
                args,
                ty: ty.clone(),
            }
        }
        ETraitMethod { ty, astptr, .. } => {
            push_compile_error(
                diagnostics,
                "Trait method values are not supported; call the method directly",
                astptr.as_ref().map(|ptr| ptr.text_range()),
            );
            emissing(ty)
        }
        EDynTraitMethod { ty, astptr, .. } => {
            push_compile_error(
                diagnostics,
                "Dynamic trait method values are not supported; call the method directly",
                astptr.as_ref().map(|ptr| ptr.text_range()),
            );
            emissing(ty)
        }
        EInherentMethod { ty, astptr, .. } => {
            push_compile_error(
                diagnostics,
                "Inherent method values are not supported; call the method directly",
                astptr.as_ref().map(|ptr| ptr.text_range()),
            );
            emissing(ty)
        }
        EToDyn {
            trait_name,
            for_ty,
            expr,
            ty,
            ..
        } => {
            let expr = compile_expr(expr, genv, gensym, diagnostics);
            core::Expr::EToDyn {
                trait_name: trait_name.clone(),
                for_ty: for_ty.clone(),
                expr: Box::new(expr),
                ty: ty.clone(),
            }
        }
        EProj { tuple, index, ty } => {
            let tuple = compile_expr(tuple, genv, gensym, diagnostics);
            core::Expr::EProj {
                tuple: Box::new(tuple),
                index: *index,
                ty: ty.clone(),
            }
        }
        EIndex {
            base,
            index,
            ty,
            astptr,
        } => {
            let base_core = compile_expr(base, genv, gensym, diagnostics);
            let index_core = compile_expr(index, genv, gensym, diagnostics);
            compile_index_read_core(
                diagnostics,
                base_core,
                &base.get_ty(),
                index_core,
                ty,
                astptr.as_ref().map(|ptr| ptr.text_range()),
            )
        }
        EField {
            expr,
            field_name,
            ty,
            astptr,
        } => {
            let expr_core = compile_expr(expr, genv, gensym, diagnostics);
            compile_field_get_core(
                genv,
                diagnostics,
                expr_core,
                &expr.get_ty(),
                field_name,
                ty,
                astptr.as_ref().map(|ptr| ptr.text_range()),
            )
        }
    }
}
