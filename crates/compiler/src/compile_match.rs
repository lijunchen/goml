use ast::ast::{BinaryOp, Uident, UnaryOp};

use crate::core;
use crate::env::{Gensym, GlobalTypeEnv, StructDef};
use crate::mangle::mangle_impl_name;
use crate::tast::Arm;
use crate::tast::Constructor;
use crate::tast::Expr::{self, *};
use crate::tast::Pat::{self, *};
use crate::tast::Ty;
use crate::tast::{self, File, Prim};

use indexmap::IndexMap;
use std::collections::HashMap;

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
            row.body = ELet {
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
                ty: row.body.get_ty(),
                body: Box::new(row.body.clone()),
            };
            false
        }
        Pat::PWild { ty: _ } => false,
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
        | Ty::TString => ty.clone(),
        Ty::TTuple { typs } => Ty::TTuple {
            typs: typs
                .iter()
                .map(|t| substitute_ty_params(t, subst))
                .collect(),
        },
        Ty::TCon { name } => Ty::TCon { name: name.clone() },
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
        Ty::TRef { elem } => Ty::TRef {
            elem: Box::new(substitute_ty_params(elem, subst)),
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

fn instantiate_struct_fields(struct_def: &StructDef, type_args: &[Ty]) -> Vec<(String, Ty)> {
    if struct_def.generics.len() != type_args.len() {
        panic!(
            "Struct {} expects {} type arguments, but got {}",
            struct_def.name.0,
            struct_def.generics.len(),
            type_args.len()
        );
    }

    let mut subst = HashMap::new();
    for (param, arg) in struct_def.generics.iter().zip(type_args.iter()) {
        subst.insert(param.0.clone(), arg.clone());
    }

    struct_def
        .fields
        .iter()
        .map(|(fname, fty)| (fname.0.clone(), substitute_ty_params(fty, &subst)))
        .collect()
}

fn decompose_struct_type(ty: &Ty) -> Option<(Uident, Vec<Ty>)> {
    match ty {
        Ty::TCon { name } => Some((Uident::new(name), Vec::new())),
        Ty::TApp { ty: base, args } => {
            let (type_name, mut collected) = decompose_struct_type(base)?;
            collected.extend(args.iter().cloned());
            Some((type_name, collected))
        }
        _ => None,
    }
}

struct ConstructorCase {
    constructor: Constructor,
    vars: Vec<Variable>,
    rows: Vec<Row>,
}

fn compile_constructor_cases(
    genv: &GlobalTypeEnv,
    gensym: &Gensym,
    rows: Vec<Row>,
    bvar: &Variable,
    mut cases: Vec<ConstructorCase>,
    ty: &Ty,
) -> Vec<core::Arm> {
    for mut row in rows {
        if let Some(col) = row.remove_column(&bvar.name) {
            if let Pat::PConstr {
                constructor,
                args,
                ty: _,
            } = col.pat
            {
                let idx = constructor
                    .as_enum()
                    .expect("expected enum constructor in compile_constructor_cases")
                    .enum_index();
                let mut cols = row.columns;
                for (var, pat) in cases[idx].vars.iter().zip(args.into_iter()) {
                    cols.push(Column {
                        var: var.name.clone(),
                        pat,
                    })
                }
                cases[idx].rows.push(Row {
                    columns: cols,
                    body: row.body,
                })
            } else {
                unreachable!()
            }
        } else {
            for ConstructorCase { rows, .. } in &mut cases {
                rows.push(row.clone())
            }
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
            body: compile_rows(genv, gensym, case.rows, ty),
        };
        arms.push(arm);
    }
    arms
}

fn compile_enum_case(
    genv: &GlobalTypeEnv,
    gensym: &Gensym,
    rows: Vec<Row>,
    bvar: &Variable,
    ty: &Ty,
    name: &Uident,
) -> core::Expr {
    let tydef = &genv.enums[name];
    let body_ty = rows.first().map(|r| r.get_ty()).unwrap_or(Ty::TUnit);

    let cases: Vec<ConstructorCase> = tydef
        .variants
        .iter()
        .enumerate()
        .map(|(index, (variant, args))| ConstructorCase {
            constructor: Constructor::Enum(tast::EnumConstructor {
                type_name: name.clone(),
                variant: variant.clone(),
                index,
            }),
            vars: args
                .iter()
                .map(|arg_ty| Variable {
                    name: gensym.gensym("x"),
                    ty: arg_ty.clone(),
                })
                .collect::<Vec<_>>(),
            rows: vec![],
        })
        .collect();

    let mut results = Vec::new();

    for case in cases.iter().take(tydef.variants.len()) {
        let hole = core::eunit();
        let mut result = hole;
        for (field, var) in case.vars.iter().enumerate().rev() {
            result = core::Expr::ELet {
                name: var.name.clone(),
                value: Box::new(core::Expr::EConstrGet {
                    expr: Box::new(bvar.to_core()),
                    constructor: case.constructor.clone(),
                    field_index: field,
                    ty: var.ty.clone(),
                }),
                body: Box::new(result),
                ty: ty.clone(),
            }
        }
        results.push(result);
    }

    let arms = compile_constructor_cases(genv, gensym, rows, bvar, cases, ty);

    let mut new_arms = vec![];
    for (mut res, mut arm) in results.into_iter().zip(arms.into_iter()) {
        replace_default_expr(&mut res, arm.body);
        arm.body = res;
        new_arms.push(arm);
    }

    core::Expr::EMatch {
        expr: Box::new(bvar.to_core()),
        arms: new_arms,
        default: None,
        ty: body_ty,
    }
}

fn compile_struct_case(
    genv: &GlobalTypeEnv,
    gensym: &Gensym,
    rows: Vec<Row>,
    bvar: &Variable,
    ty: &Ty,
    name: &Uident,
    type_args: &[Ty],
) -> core::Expr {
    let struct_def = genv
        .structs
        .get(name)
        .unwrap_or_else(|| panic!("Unknown struct {}", name.0));

    let inst_fields = if struct_def.generics.is_empty() {
        struct_def
            .fields
            .iter()
            .map(|(fname, fty)| (fname.0.clone(), fty.clone()))
            .collect::<Vec<_>>()
    } else {
        instantiate_struct_fields(struct_def, type_args)
    };

    let field_vars: Vec<Variable> = inst_fields
        .iter()
        .map(|(_, fty)| Variable {
            name: gensym.gensym("x"),
            ty: fty.clone(),
        })
        .collect();

    let constructor = Constructor::Struct(tast::StructConstructor {
        type_name: name.clone(),
    });

    let hole = core::eunit();
    let mut result = hole;
    for (field_index, var) in field_vars.iter().enumerate().rev() {
        result = core::Expr::ELet {
            name: var.name.clone(),
            value: Box::new(core::Expr::EConstrGet {
                expr: Box::new(bvar.to_core()),
                constructor: constructor.clone(),
                field_index,
                ty: var.ty.clone(),
            }),
            body: Box::new(result),
            ty: ty.clone(),
        };
    }

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
                    } if constructor.is_struct() => {
                        for (var, arg_pat) in field_vars.iter().zip(args.into_iter()) {
                            cols.push(Column {
                                var: var.name.clone(),
                                pat: arg_pat,
                            });
                        }
                    }
                    _ => unreachable!("expected struct pattern"),
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

    let inner = compile_rows(genv, gensym, new_rows, ty);
    replace_default_expr(&mut result, inner);
    result
}

fn compile_tuple_case(
    genv: &GlobalTypeEnv,
    gensym: &Gensym,
    rows: Vec<Row>,
    bvar: &Variable,
    typs: &[Ty],
    ty: &Ty,
) -> core::Expr {
    let names = typs.iter().map(|_| gensym.gensym("x")).collect::<Vec<_>>();
    let mut new_rows = vec![];

    let hole = core::eunit();
    // Create a let expression that extracts tuple elements
    let mut result = hole;

    // For each element in the tuple, create a binding
    for (i, name) in names.iter().enumerate().rev() {
        result = core::Expr::ELet {
            name: name.clone(),
            value: Box::new(core::Expr::EProj {
                tuple: Box::new(bvar.to_core()),
                index: i,
                ty: typs[i].clone(),
            }),
            body: Box::new(result),
            ty: ty.clone(),
        };
    }

    for row in rows {
        let mut cols = vec![];
        for Column { var, pat } in row.columns {
            if var == bvar.name {
                if let PTuple { items, ty: _ } = pat {
                    for (i, item) in items.into_iter().enumerate() {
                        cols.push(Column {
                            var: names[i].clone(),
                            pat: item,
                        });
                    }
                } else {
                    // since the type of bvar.ty is Tuple,
                    // so we should not reach here
                    unreachable!()
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

    let inner = compile_rows(genv, gensym, new_rows, ty);

    // Replace the empty default result with the actual compiled rows
    replace_default_expr(&mut result, inner);
    result
}

fn compile_unit_case(
    genv: &GlobalTypeEnv,
    gensym: &Gensym,
    rows: Vec<Row>,
    bvar: &Variable,
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
            body: compile_rows(genv, gensym, new_rows, &bvar.ty),
        }],
        default: None,
        ty: body_ty,
    }
}

fn compile_bool_case(
    genv: &GlobalTypeEnv,
    gensym: &Gensym,
    rows: Vec<Row>,
    bvar: &Variable,
) -> core::Expr {
    let body_ty = rows.first().map(|r| r.get_ty()).unwrap_or(Ty::TUnit);

    let mut true_rows = vec![];
    let mut false_rows = vec![];
    for mut r in rows {
        if let Some(col) = r.remove_column(&bvar.name) {
            match col.pat {
                Pat::PPrim { value, ty: _ } => {
                    if value.as_bool().expect("expected boolean primitive pattern") {
                        true_rows.push(r);
                    } else {
                        false_rows.push(r);
                    }
                }
                _ => unreachable!("expected bool pattern"),
            }
        } else {
            true_rows.push(r.clone());
            false_rows.push(r);
        }
    }
    core::Expr::EMatch {
        expr: Box::new(bvar.to_core()),
        arms: vec![
            core::Arm {
                lhs: core::ebool(true),
                body: compile_rows(genv, gensym, true_rows, &bvar.ty),
            },
            core::Arm {
                lhs: core::ebool(false),
                body: compile_rows(genv, gensym, false_rows, &bvar.ty),
            },
        ],
        default: None,
        ty: body_ty,
    }
}

fn compile_int_case(
    genv: &GlobalTypeEnv,
    gensym: &Gensym,
    rows: Vec<Row>,
    bvar: &Variable,
    ty: &Ty,
    literal_ty: Ty,
) -> core::Expr {
    let body_ty = rows.first().map(|r| r.get_ty()).unwrap_or(Ty::TUnit);

    let mut value_rows: IndexMap<i128, Vec<Row>> = IndexMap::new();
    let mut fallback_rows: Vec<Row> = Vec::new();
    let mut default_rows: Vec<Row> = Vec::new();

    for mut row in rows {
        if let Some(col) = row.remove_column(&bvar.name) {
            match col.pat {
                Pat::PPrim { value, ty: _ } => {
                    let key = value
                        .as_signed()
                        .or_else(|| value.as_unsigned().map(|v| v as i128))
                        .expect("expected integer primitive pattern");
                    let entry = value_rows
                        .entry(key)
                        .or_insert_with(|| fallback_rows.clone());
                    entry.push(row);
                }
                Pat::PWild { .. } => {
                    let row_clone = row.clone();
                    for rows in value_rows.values_mut() {
                        rows.push(row_clone.clone());
                    }
                    fallback_rows.push(row_clone.clone());
                    default_rows.push(row);
                }
                _ => unreachable!("expected int pattern"),
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

    let arms = value_rows
        .into_iter()
        .map(|(value, rows)| core::Arm {
            lhs: core::Expr::EPrim {
                value: Prim::from_int_literal(value, &literal_ty),
                ty: literal_ty.clone(),
            },
            body: compile_rows(genv, gensym, rows, ty),
        })
        .collect();

    let default = if default_rows.is_empty() {
        None
    } else {
        Some(Box::new(compile_rows(genv, gensym, default_rows, ty)))
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
    rows: Vec<Row>,
    bvar: &Variable,
    ty: &Ty,
) -> core::Expr {
    let body_ty = rows.first().map(|r| r.get_ty()).unwrap_or(Ty::TUnit);

    let mut value_rows: IndexMap<String, Vec<Row>> = IndexMap::new();
    let mut fallback_rows: Vec<Row> = Vec::new();
    let mut default_rows: Vec<Row> = Vec::new();

    for mut row in rows {
        if let Some(col) = row.remove_column(&bvar.name) {
            match col.pat {
                Pat::PPrim { value, ty: _ } => {
                    let key = value
                        .as_str()
                        .expect("expected string primitive pattern")
                        .to_string();
                    let entry = value_rows
                        .entry(key)
                        .or_insert_with(|| fallback_rows.clone());
                    entry.push(row);
                }
                Pat::PWild { .. } => {
                    let row_clone = row.clone();
                    for rows in value_rows.values_mut() {
                        rows.push(row_clone.clone());
                    }
                    fallback_rows.push(row_clone.clone());
                    default_rows.push(row);
                }
                _ => unreachable!("expected string pattern"),
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

    let arms = value_rows
        .into_iter()
        .map(|(value, rows)| core::Arm {
            lhs: core::Expr::EPrim {
                value: Prim::string(value),
                ty: Ty::TString,
            },
            body: compile_rows(genv, gensym, rows, ty),
        })
        .collect();

    let default = if default_rows.is_empty() {
        None
    } else {
        Some(Box::new(compile_rows(genv, gensym, default_rows, ty)))
    };

    core::Expr::EMatch {
        expr: Box::new(bvar.to_core()),
        arms,
        default,
        ty: body_ty,
    }
}

fn compile_rows(genv: &GlobalTypeEnv, gensym: &Gensym, mut rows: Vec<Row>, ty: &Ty) -> core::Expr {
    if rows.is_empty() {
        return emissing(ty);
    }
    for row in &mut rows {
        move_variable_patterns(row);
    }

    if rows.first().is_some_and(|c| c.columns.is_empty()) {
        let row = rows.remove(0);
        return compile_expr(&row.body, genv, gensym);
    }

    let bvar = branch_variable(&rows);
    match &bvar.ty {
        Ty::TVar(..) => unreachable!(),
        Ty::TUnit => compile_unit_case(genv, gensym, rows, &bvar),
        Ty::TBool => compile_bool_case(genv, gensym, rows, &bvar),
        Ty::TInt32 => compile_int_case(genv, gensym, rows, &bvar, ty, Ty::TInt32),
        Ty::TInt8 => compile_int_case(genv, gensym, rows, &bvar, ty, Ty::TInt8),
        Ty::TInt16 => compile_int_case(genv, gensym, rows, &bvar, ty, Ty::TInt16),
        Ty::TInt64 => compile_int_case(genv, gensym, rows, &bvar, ty, Ty::TInt64),
        Ty::TUint8 => compile_int_case(genv, gensym, rows, &bvar, ty, Ty::TUint8),
        Ty::TUint16 => compile_int_case(genv, gensym, rows, &bvar, ty, Ty::TUint16),
        Ty::TUint32 => compile_int_case(genv, gensym, rows, &bvar, ty, Ty::TUint32),
        Ty::TUint64 => compile_int_case(genv, gensym, rows, &bvar, ty, Ty::TUint64),
        Ty::TFloat32 | Ty::TFloat64 => {
            panic!("Matching on floating point types is not supported")
        }
        Ty::TString => compile_string_case(genv, gensym, rows, &bvar, ty),
        Ty::TCon { name } => {
            let ident = Uident::new(name);
            if genv.enums.contains_key(&ident) {
                compile_enum_case(genv, gensym, rows, &bvar, ty, &ident)
            } else if genv.structs.contains_key(&ident) {
                compile_struct_case(genv, gensym, rows, &bvar, ty, &ident, &[])
            } else {
                panic!("Unknown type constructor {} in match", name)
            }
        }
        Ty::TApp { ty: base, args } => {
            let name = base.get_constr_name_unsafe();
            let ident = Uident::new(&name);
            if genv.enums.contains_key(&ident) {
                compile_enum_case(genv, gensym, rows, &bvar, ty, &ident)
            } else if genv.structs.contains_key(&ident) {
                compile_struct_case(genv, gensym, rows, &bvar, ty, &ident, args)
            } else {
                panic!("Unknown type constructor {} in match", name)
            }
        }
        Ty::TTuple { typs } => compile_tuple_case(genv, gensym, rows, &bvar, typs, ty),
        Ty::TArray { .. } => unreachable!("Array pattern matching is not supported"),
        Ty::TFunc { .. } => unreachable!(),
        Ty::TParam { .. } => unreachable!(),
        Ty::TRef { .. } => panic!("Matching on reference types is not supported"),
    }
}

// Helper function to replace the default empty expression with the actual compiled inner expression
fn replace_default_expr(expr: &mut core::Expr, replacement: core::Expr) {
    match expr {
        core::Expr::ELet { body, .. } => replace_default_expr(body, replacement),
        _ => *expr = replacement,
    }
}

pub fn compile_file(genv: &GlobalTypeEnv, gensym: &Gensym, file: &File) -> core::File {
    let mut toplevels = vec![];
    for item in file.toplevels.iter() {
        match item {
            tast::Item::ImplBlock(impl_block) => {
                let for_ty = &impl_block.for_type;
                for m in impl_block.methods.iter() {
                    let trait_name = &impl_block.trait_name;
                    let method_name = &m.name;

                    let f = core::Fn {
                        name: mangle_impl_name(trait_name, for_ty, method_name),
                        params: m
                            .params
                            .iter()
                            .map(|(name, ty)| (name.clone(), ty.clone()))
                            .collect(),
                        ret_ty: m.ret_ty.clone(),
                        body: compile_expr(&m.body, genv, gensym),
                    };

                    toplevels.push(f);
                }
            }
            tast::Item::Fn(f) => {
                toplevels.push(core::Fn {
                    name: f.name.clone(),
                    params: f
                        .params
                        .iter()
                        .map(|(name, ty)| (name.clone(), ty.clone()))
                        .collect(),
                    ret_ty: f.ret_ty.clone(),
                    body: compile_expr(&f.body, genv, gensym),
                });
            }
            tast::Item::ExternGo(_) => {}
            tast::Item::ExternType(_) => {}
        }
    }
    core::File { toplevels }
}

fn builtin_function_for(
    op: BinaryOp,
    lhs_ty: &Ty,
    rhs_ty: &Ty,
    _result_ty: &Ty,
) -> Option<&'static str> {
    match op {
        BinaryOp::Add => match (lhs_ty, rhs_ty) {
            (Ty::TInt32, Ty::TInt32) => Some("int_add"),
            (Ty::TInt8, Ty::TInt8) => Some("int8_add"),
            (Ty::TInt16, Ty::TInt16) => Some("int16_add"),
            (Ty::TInt64, Ty::TInt64) => Some("int64_add"),
            (Ty::TUint8, Ty::TUint8) => Some("uint8_add"),
            (Ty::TUint16, Ty::TUint16) => Some("uint16_add"),
            (Ty::TUint32, Ty::TUint32) => Some("uint32_add"),
            (Ty::TUint64, Ty::TUint64) => Some("uint64_add"),
            (Ty::TFloat32, Ty::TFloat32) => Some("float32_add"),
            (Ty::TFloat64, Ty::TFloat64) => Some("float64_add"),
            (Ty::TString, Ty::TString) => Some("string_add"),
            _ => None,
        },
        BinaryOp::Sub => match (lhs_ty, rhs_ty) {
            (Ty::TInt32, Ty::TInt32) => Some("int_sub"),
            (Ty::TInt8, Ty::TInt8) => Some("int8_sub"),
            (Ty::TInt16, Ty::TInt16) => Some("int16_sub"),
            (Ty::TInt64, Ty::TInt64) => Some("int64_sub"),
            (Ty::TUint8, Ty::TUint8) => Some("uint8_sub"),
            (Ty::TUint16, Ty::TUint16) => Some("uint16_sub"),
            (Ty::TUint32, Ty::TUint32) => Some("uint32_sub"),
            (Ty::TUint64, Ty::TUint64) => Some("uint64_sub"),
            (Ty::TFloat32, Ty::TFloat32) => Some("float32_sub"),
            (Ty::TFloat64, Ty::TFloat64) => Some("float64_sub"),
            _ => None,
        },
        BinaryOp::Mul => match (lhs_ty, rhs_ty) {
            (Ty::TInt32, Ty::TInt32) => Some("int_mul"),
            (Ty::TInt8, Ty::TInt8) => Some("int8_mul"),
            (Ty::TInt16, Ty::TInt16) => Some("int16_mul"),
            (Ty::TInt64, Ty::TInt64) => Some("int64_mul"),
            (Ty::TUint8, Ty::TUint8) => Some("uint8_mul"),
            (Ty::TUint16, Ty::TUint16) => Some("uint16_mul"),
            (Ty::TUint32, Ty::TUint32) => Some("uint32_mul"),
            (Ty::TUint64, Ty::TUint64) => Some("uint64_mul"),
            (Ty::TFloat32, Ty::TFloat32) => Some("float32_mul"),
            (Ty::TFloat64, Ty::TFloat64) => Some("float64_mul"),
            _ => None,
        },
        BinaryOp::Div => match (lhs_ty, rhs_ty) {
            (Ty::TInt32, Ty::TInt32) => Some("int_div"),
            (Ty::TInt8, Ty::TInt8) => Some("int8_div"),
            (Ty::TInt16, Ty::TInt16) => Some("int16_div"),
            (Ty::TInt64, Ty::TInt64) => Some("int64_div"),
            (Ty::TUint8, Ty::TUint8) => Some("uint8_div"),
            (Ty::TUint16, Ty::TUint16) => Some("uint16_div"),
            (Ty::TUint32, Ty::TUint32) => Some("uint32_div"),
            (Ty::TUint64, Ty::TUint64) => Some("uint64_div"),
            (Ty::TFloat32, Ty::TFloat32) => Some("float32_div"),
            (Ty::TFloat64, Ty::TFloat64) => Some("float64_div"),
            _ => None,
        },
        BinaryOp::And => match (lhs_ty, rhs_ty) {
            (Ty::TBool, Ty::TBool) => Some("bool_and"),
            _ => None,
        },
        BinaryOp::Or => match (lhs_ty, rhs_ty) {
            (Ty::TBool, Ty::TBool) => Some("bool_or"),
            _ => None,
        },
    }
}

fn builtin_unary_function_for(op: UnaryOp, arg_ty: &Ty, _result_ty: &Ty) -> Option<&'static str> {
    match op {
        UnaryOp::Neg => match arg_ty {
            Ty::TInt8 => Some("int8_neg"),
            Ty::TInt16 => Some("int16_neg"),
            Ty::TInt32 => Some("int_neg"),
            Ty::TInt64 => Some("int64_neg"),
            Ty::TUint8 => Some("uint8_neg"),
            Ty::TUint16 => Some("uint16_neg"),
            Ty::TUint32 => Some("uint32_neg"),
            Ty::TUint64 => Some("uint64_neg"),
            Ty::TFloat32 => Some("float32_neg"),
            Ty::TFloat64 => Some("float64_neg"),
            _ => None,
        },
        UnaryOp::Not => match arg_ty {
            Ty::TBool => Some("bool_not"),
            _ => None,
        },
    }
}

fn compile_expr(e: &Expr, genv: &GlobalTypeEnv, gensym: &Gensym) -> core::Expr {
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
                .map(|item| compile_expr(item, genv, gensym))
                .collect();
            core::Expr::ETuple {
                items,
                ty: ty.clone(),
            }
        }
        EArray { items, ty } => {
            let items = items
                .iter()
                .map(|item| compile_expr(item, genv, gensym))
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
                .map(|arg| compile_expr(arg, genv, gensym))
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
            let body = Box::new(compile_expr(body, genv, gensym));
            core::Expr::EClosure {
                params,
                body,
                ty: ty.clone(),
            }
        }
        ELet {
            pat:
                Pat::PVar {
                    name,
                    ty: _pat_ty,
                    astptr: _,
                },
            value,
            body,
            ty,
        } => core::Expr::ELet {
            name: name.clone(),
            value: Box::new(compile_expr(value, genv, gensym)),
            body: Box::new(compile_expr(body, genv, gensym)),
            ty: ty.clone(),
        },
        ELet {
            pat,
            value,
            body,
            ty,
        } => {
            let core_value = compile_expr(value, genv, gensym);
            let x = gensym.gensym("mtmp");
            let rows = vec![
                Row {
                    columns: vec![Column {
                        var: x.clone(),
                        pat: pat.clone(),
                    }],
                    body: *body.clone(),
                },
                Row {
                    columns: vec![Column {
                        var: x.clone(),
                        pat: Pat::PWild { ty: pat.get_ty() },
                    }],
                    body: ECall {
                        func: Box::new(Expr::EVar {
                            name: "missing".to_string(),
                            ty: Ty::TFunc {
                                params: vec![Ty::TString],
                                ret_ty: Box::new(body.get_ty()),
                            },
                            astptr: None,
                        }),
                        args: vec![Expr::EPrim {
                            value: Prim::string("".to_string()),
                            ty: Ty::TString,
                        }],
                        ty: body.get_ty(),
                    },
                },
            ];
            core::Expr::ELet {
                name: x,
                value: Box::new(core_value),
                body: Box::new(compile_rows(genv, gensym, rows, ty)),
                ty: ty.clone(),
            }
        }
        EMatch { expr, arms, ty } => match expr.as_ref() {
            EVar {
                name,
                ty: _ty,
                astptr: _,
            } => {
                let rows = make_rows(name, arms);
                compile_rows(genv, gensym, rows, ty)
            }
            _ => {
                // create a new variable
                // match (a, b, c) { .. }
                // =>
                // let tmp = (a, b, c) in match tmp { ... }
                let mtmp = gensym.gensym("mtmp");
                let rows = make_rows(mtmp.as_str(), arms);
                let core_expr = compile_expr(expr, genv, gensym);
                let core_rows = compile_rows(genv, gensym, rows, ty);
                core::Expr::ELet {
                    name: mtmp,
                    value: Box::new(core_expr),
                    body: Box::new(core_rows),
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
            cond: Box::new(compile_expr(cond, genv, gensym)),
            then_branch: Box::new(compile_expr(then_branch, genv, gensym)),
            else_branch: Box::new(compile_expr(else_branch, genv, gensym)),
            ty: ty.clone(),
        },
        EWhile { cond, body, ty } => core::Expr::EWhile {
            cond: Box::new(compile_expr(cond, genv, gensym)),
            body: Box::new(compile_expr(body, genv, gensym)),
            ty: ty.clone(),
        },
        EUnary {
            op,
            expr,
            ty,
            resolution,
        } => {
            let arg = compile_expr(expr, genv, gensym);
            let arg_ty = arg.get_ty();

            match resolution {
                tast::UnaryResolution::Builtin => {
                    let func = builtin_unary_function_for(*op, &arg_ty, ty).unwrap_or_else(|| {
                        panic!(
                            "Unsupported builtin unary operator {:?} for type {:?}",
                            op, arg_ty
                        )
                    });
                    core::Expr::ECall {
                        func: Box::new(core::Expr::EVar {
                            name: func.to_string(),
                            ty: Ty::TFunc {
                                params: vec![arg_ty.clone()],
                                ret_ty: Box::new(ty.clone()),
                            },
                        }),
                        args: vec![arg],
                        ty: ty.clone(),
                    }
                }
                tast::UnaryResolution::Overloaded { trait_name } => {
                    let method = op.method_name();
                    let self_ty = arg.get_ty();
                    let func_name = mangle_impl_name(trait_name, &self_ty, method);
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
            let args = vec![
                compile_expr(lhs, genv, gensym),
                compile_expr(rhs, genv, gensym),
            ];
            let param_tys: Vec<_> = args.iter().map(|arg| arg.get_ty()).collect();

            match resolution {
                tast::BinaryResolution::Builtin => {
                    let func = builtin_function_for(*op, &param_tys[0], &param_tys[1], ty)
                        .unwrap_or_else(|| {
                            panic!("Unsupported builtin operator {:?} for type {:?}", op, ty)
                        });
                    core::Expr::ECall {
                        func: Box::new(core::Expr::EVar {
                            name: func.to_string(),
                            ty: Ty::TFunc {
                                params: param_tys,
                                ret_ty: Box::new(ty.clone()),
                            },
                        }),
                        args,
                        ty: ty.clone(),
                    }
                }
                tast::BinaryResolution::Overloaded { trait_name } => {
                    let method = op.method_name();
                    let self_ty = args[0].get_ty();
                    let func_name = mangle_impl_name(trait_name, &self_ty, method);
                    let param_tys = args.iter().map(|arg| arg.get_ty()).collect();
                    core::Expr::ECall {
                        func: Box::new(core::Expr::EVar {
                            name: func_name,
                            ty: Ty::TFunc {
                                params: param_tys,
                                ret_ty: Box::new(ty.clone()),
                            },
                        }),
                        args,
                        ty: ty.clone(),
                    }
                }
            }
        }
        ECall { func, args, ty } => {
            let core_func = compile_expr(func, genv, gensym);
            let args = args
                .iter()
                .map(|arg| compile_expr(arg, genv, gensym))
                .collect::<Vec<_>>();

            let func_expr = if let tast::Expr::EVar { name, .. } = func.as_ref()
                && genv.overloaded_funcs_to_trait_name.contains_key(name)
            {
                let trait_name = genv.overloaded_funcs_to_trait_name[name].clone();
                let for_ty = args[0].get_ty();
                core::Expr::EVar {
                    name: mangle_impl_name(&trait_name, &for_ty, name),
                    ty: core_func.get_ty(),
                }
            } else {
                core_func
            };

            core::Expr::ECall {
                func: Box::new(func_expr),
                args,
                ty: ty.clone(),
            }
        }
        EProj { tuple, index, ty } => {
            let tuple = compile_expr(tuple, genv, gensym);
            core::Expr::EProj {
                tuple: Box::new(tuple),
                index: *index,
                ty: ty.clone(),
            }
        }
        EField {
            expr,
            field_name,
            ty,
        } => {
            let base_ty = expr.get_ty();
            let expr_core = compile_expr(expr, genv, gensym);
            let (type_name, type_args) = decompose_struct_type(&base_ty)
                .unwrap_or_else(|| panic!("Field access on non-struct type {:?}", base_ty));
            let struct_def = genv
                .structs
                .get(&type_name)
                .unwrap_or_else(|| panic!("Struct {} not found", type_name.0));
            let inst_fields = instantiate_struct_fields(struct_def, &type_args);
            let (field_index, _) = inst_fields
                .iter()
                .enumerate()
                .find(|(_, (name, _))| name == field_name)
                .unwrap_or_else(|| panic!("Struct {} has no field {}", type_name.0, field_name));
            core::Expr::EConstrGet {
                expr: Box::new(expr_core),
                constructor: tast::Constructor::Struct(tast::StructConstructor { type_name }),
                field_index,
                ty: ty.clone(),
            }
        }
    }
}
