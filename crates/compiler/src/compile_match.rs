use ast::ast::{BinaryOp, Uident, UnaryOp};

use crate::core;
use crate::env::{Env, StructDef};
use crate::mangle::mangle_impl_name;
use crate::tast::Arm;
use crate::tast::Constructor;
use crate::tast::Expr::{self, *};
use crate::tast::Pat::{self, *};
use crate::tast::Ty;
use crate::tast::{self, File};

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
        func: "missing".to_string(),
        args: vec![core::Expr::EString {
            value: "".to_string(),
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
        Ty::TVar(_) | Ty::TUnit | Ty::TBool | Ty::TInt | Ty::TString => ty.clone(),
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

struct ConstructorCase {
    constructor: Constructor,
    vars: Vec<Variable>,
    rows: Vec<Row>,
}

fn compile_constructor_cases(
    env: &Env,
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
            body: compile_rows(env, case.rows, ty),
        };
        arms.push(arm);
    }
    arms
}

fn compile_enum_case(
    env: &Env,
    rows: Vec<Row>,
    bvar: &Variable,
    ty: &Ty,
    name: &Uident,
) -> core::Expr {
    let tydef = &env.enums[name];
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
                    name: env.gensym("x"),
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

    let arms = compile_constructor_cases(env, rows, bvar, cases, ty);

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
    env: &Env,
    rows: Vec<Row>,
    bvar: &Variable,
    ty: &Ty,
    name: &Uident,
    type_args: &[Ty],
) -> core::Expr {
    let struct_def = env
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
            name: env.gensym("x"),
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

    let inner = compile_rows(env, new_rows, ty);
    replace_default_expr(&mut result, inner);
    result
}

fn compile_tuple_case(
    env: &Env,
    rows: Vec<Row>,
    bvar: &Variable,
    typs: &[Ty],
    ty: &Ty,
) -> core::Expr {
    let names = typs.iter().map(|_| env.gensym("x")).collect::<Vec<_>>();
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

    let inner = compile_rows(env, new_rows, ty);

    // Replace the empty default result with the actual compiled rows
    replace_default_expr(&mut result, inner);
    result
}

fn compile_unit_case(env: &Env, rows: Vec<Row>, bvar: &Variable) -> core::Expr {
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
            body: compile_rows(env, new_rows, &bvar.ty),
        }],
        default: None,
        ty: body_ty,
    }
}

fn compile_bool_case(env: &Env, rows: Vec<Row>, bvar: &Variable) -> core::Expr {
    let body_ty = rows.first().map(|r| r.get_ty()).unwrap_or(Ty::TUnit);

    let mut true_rows = vec![];
    let mut false_rows = vec![];
    for mut r in rows {
        if let Some(col) = r.remove_column(&bvar.name) {
            if let Pat::PBool { value, ty: _ } = col.pat {
                if value {
                    true_rows.push(r);
                } else {
                    false_rows.push(r);
                }
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
                body: compile_rows(env, true_rows, &bvar.ty),
            },
            core::Arm {
                lhs: core::ebool(false),
                body: compile_rows(env, false_rows, &bvar.ty),
            },
        ],
        default: None,
        ty: body_ty,
    }
}

fn compile_int_case(env: &Env, rows: Vec<Row>, bvar: &Variable, ty: &Ty) -> core::Expr {
    let body_ty = rows.first().map(|r| r.get_ty()).unwrap_or(Ty::TUnit);

    let mut value_rows: IndexMap<i32, Vec<Row>> = IndexMap::new();
    let mut fallback_rows: Vec<Row> = Vec::new();
    let mut default_rows: Vec<Row> = Vec::new();

    for mut row in rows {
        if let Some(col) = row.remove_column(&bvar.name) {
            match col.pat {
                Pat::PInt { value, ty: _ } => {
                    let entry = value_rows
                        .entry(value)
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
            lhs: core::Expr::EInt {
                value,
                ty: Ty::TInt,
            },
            body: compile_rows(env, rows, ty),
        })
        .collect();

    let default = if default_rows.is_empty() {
        None
    } else {
        Some(Box::new(compile_rows(env, default_rows, ty)))
    };

    core::Expr::EMatch {
        expr: Box::new(bvar.to_core()),
        arms,
        default,
        ty: body_ty,
    }
}

fn compile_string_case(env: &Env, rows: Vec<Row>, bvar: &Variable, ty: &Ty) -> core::Expr {
    let body_ty = rows.first().map(|r| r.get_ty()).unwrap_or(Ty::TUnit);

    let mut value_rows: IndexMap<String, Vec<Row>> = IndexMap::new();
    let mut fallback_rows: Vec<Row> = Vec::new();
    let mut default_rows: Vec<Row> = Vec::new();

    for mut row in rows {
        if let Some(col) = row.remove_column(&bvar.name) {
            match col.pat {
                Pat::PString { value, ty: _ } => {
                    let entry = value_rows
                        .entry(value)
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
            lhs: core::Expr::EString {
                value,
                ty: Ty::TString,
            },
            body: compile_rows(env, rows, ty),
        })
        .collect();

    let default = if default_rows.is_empty() {
        None
    } else {
        Some(Box::new(compile_rows(env, default_rows, ty)))
    };

    core::Expr::EMatch {
        expr: Box::new(bvar.to_core()),
        arms,
        default,
        ty: body_ty,
    }
}

fn compile_rows(env: &Env, mut rows: Vec<Row>, ty: &Ty) -> core::Expr {
    if rows.is_empty() {
        return emissing(ty);
    }
    for row in &mut rows {
        move_variable_patterns(row);
    }

    if rows.first().is_some_and(|c| c.columns.is_empty()) {
        let row = rows.remove(0);
        return compile_expr(&row.body, env);
    }

    let bvar = branch_variable(&rows);
    match &bvar.ty {
        Ty::TVar(..) => unreachable!(),
        Ty::TUnit => compile_unit_case(env, rows, &bvar),
        Ty::TBool => compile_bool_case(env, rows, &bvar),
        Ty::TInt => compile_int_case(env, rows, &bvar, ty),
        Ty::TString => compile_string_case(env, rows, &bvar, ty),
        Ty::TCon { name } => {
            let ident = Uident::new(name);
            if env.enums.contains_key(&ident) {
                compile_enum_case(env, rows, &bvar, ty, &ident)
            } else if env.structs.contains_key(&ident) {
                compile_struct_case(env, rows, &bvar, ty, &ident, &[])
            } else {
                panic!("Unknown type constructor {} in match", name)
            }
        }
        Ty::TApp { ty: base, args } => {
            let name = base.get_constr_name_unsafe();
            let ident = Uident::new(&name);
            if env.enums.contains_key(&ident) {
                compile_enum_case(env, rows, &bvar, ty, &ident)
            } else if env.structs.contains_key(&ident) {
                compile_struct_case(env, rows, &bvar, ty, &ident, args)
            } else {
                panic!("Unknown type constructor {} in match", name)
            }
        }
        Ty::TTuple { typs } => compile_tuple_case(env, rows, &bvar, typs, ty),
        Ty::TArray { .. } => unreachable!("Array pattern matching is not supported"),
        Ty::TFunc { .. } => unreachable!(),
        Ty::TParam { .. } => unreachable!(),
    }
}

// Helper function to replace the default empty expression with the actual compiled inner expression
fn replace_default_expr(expr: &mut core::Expr, replacement: core::Expr) {
    match expr {
        core::Expr::ELet { body, .. } => replace_default_expr(body, replacement),
        _ => *expr = replacement,
    }
}

pub fn compile_file(env: &Env, file: &File) -> core::File {
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
                        body: compile_expr(&m.body, env),
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
                    body: compile_expr(&f.body, env),
                });
            }
            tast::Item::ExternGo(_) => {}
        }
    }
    core::File { toplevels }
}

fn builtin_function_for(op: BinaryOp, ty: &Ty) -> Option<&'static str> {
    match op {
        BinaryOp::Add => match ty {
            Ty::TInt => Some("int_add"),
            Ty::TString => Some("string_add"),
            _ => None,
        },
        BinaryOp::Sub => match ty {
            Ty::TInt => Some("int_sub"),
            _ => None,
        },
        BinaryOp::Mul => match ty {
            Ty::TInt => Some("int_mul"),
            _ => None,
        },
        BinaryOp::Div => match ty {
            Ty::TInt => Some("int_div"),
            _ => None,
        },
        BinaryOp::And => match ty {
            Ty::TBool => Some("bool_and"),
            _ => None,
        },
        BinaryOp::Or => match ty {
            Ty::TBool => Some("bool_or"),
            _ => None,
        },
    }
}

fn builtin_unary_function_for(op: UnaryOp, ty: &Ty) -> Option<&'static str> {
    match op {
        UnaryOp::Neg => match ty {
            Ty::TInt => Some("int_neg"),
            _ => None,
        },
        UnaryOp::Not => match ty {
            Ty::TBool => Some("bool_not"),
            _ => None,
        },
    }
}

fn compile_expr(e: &Expr, env: &Env) -> core::Expr {
    match e {
        EVar {
            name,
            ty,
            astptr: _,
        } => core::Expr::EVar {
            name: name.to_string(),
            ty: ty.clone(),
        },
        EUnit { .. } => core::Expr::EUnit {
            ty: core::Ty::TUnit,
        },
        EBool { value, ty: _ } => core::Expr::EBool {
            value: *value,
            ty: Ty::TBool,
        },
        EInt { value, ty: _ } => core::Expr::EInt {
            value: *value,
            ty: Ty::TInt,
        },
        EString { value, ty: _ } => core::Expr::EString {
            value: value.clone(),
            ty: Ty::TString,
        },
        ETuple { items, ty } => {
            let items = items.iter().map(|item| compile_expr(item, env)).collect();
            core::Expr::ETuple {
                items,
                ty: ty.clone(),
            }
        }
        EArray { items, ty } => {
            let items = items.iter().map(|item| compile_expr(item, env)).collect();
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
            let args = args.iter().map(|arg| compile_expr(arg, env)).collect();
            core::Expr::EConstr {
                constructor: constructor.clone(),
                args,
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
            value: Box::new(compile_expr(value, env)),
            body: Box::new(compile_expr(body, env)),
            ty: ty.clone(),
        },
        ELet {
            pat,
            value,
            body,
            ty,
        } => {
            let core_value = compile_expr(value, env);
            let x = env.gensym("mtmp");
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
                        func: "missing".to_string(),
                        args: vec![Expr::EString {
                            value: "".to_string(),
                            ty: Ty::TString,
                        }],
                        ty: body.get_ty(),
                    },
                },
            ];
            core::Expr::ELet {
                name: x,
                value: Box::new(core_value),
                body: Box::new(compile_rows(env, rows, ty)),
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
                compile_rows(env, rows, ty)
            }
            _ => {
                // create a new variable
                // match (a, b, c) { .. }
                // =>
                // let tmp = (a, b, c) in match tmp { ... }
                let mtmp = env.gensym("mtmp");
                let rows = make_rows(mtmp.as_str(), arms);
                let core_expr = compile_expr(expr, env);
                let core_rows = compile_rows(env, rows, ty);
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
            cond: Box::new(compile_expr(cond, env)),
            then_branch: Box::new(compile_expr(then_branch, env)),
            else_branch: Box::new(compile_expr(else_branch, env)),
            ty: ty.clone(),
        },
        EUnary {
            op,
            expr,
            ty,
            resolution,
        } => {
            let arg = compile_expr(expr, env);

            match resolution {
                tast::UnaryResolution::Builtin => {
                    let func = builtin_unary_function_for(*op, ty).unwrap_or_else(|| {
                        panic!(
                            "Unsupported builtin unary operator {:?} for type {:?}",
                            op, ty
                        )
                    });
                    core::Expr::ECall {
                        func: func.to_string(),
                        args: vec![arg],
                        ty: ty.clone(),
                    }
                }
                tast::UnaryResolution::Overloaded { trait_name } => {
                    let method = op.method_name();
                    let self_ty = arg.get_ty();
                    let func_name = mangle_impl_name(&trait_name, &self_ty, method);
                    core::Expr::ECall {
                        func: func_name,
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
            let mut args = Vec::with_capacity(2);
            args.push(compile_expr(lhs, env));
            args.push(compile_expr(rhs, env));

            match resolution {
                tast::BinaryResolution::Builtin => {
                    let func = builtin_function_for(*op, ty).unwrap_or_else(|| {
                        panic!("Unsupported builtin operator {:?} for type {:?}", op, ty)
                    });
                    core::Expr::ECall {
                        func: func.to_string(),
                        args,
                        ty: ty.clone(),
                    }
                }
                tast::BinaryResolution::Overloaded { trait_name } => {
                    let method = op.method_name();
                    let self_ty = args[0].get_ty();
                    let func_name = mangle_impl_name(&trait_name, &self_ty, method);
                    core::Expr::ECall {
                        func: func_name,
                        args,
                        ty: ty.clone(),
                    }
                }
            }
        }
        ECall { func, args, ty } => {
            let is_overloaded = env.overloaded_funcs_to_trait_name.contains_key(func);

            let args = args.iter().map(|arg| compile_expr(arg, env)).collect();

            if !is_overloaded {
                core::Expr::ECall {
                    func: func.clone(),
                    args,
                    ty: ty.clone(),
                }
            } else {
                let trait_name = env.overloaded_funcs_to_trait_name[func].clone();
                let for_ty = args[0].get_ty();
                core::Expr::ECall {
                    func: mangle_impl_name(&trait_name, &for_ty, func),
                    args,
                    ty: ty.clone(),
                }
            }
        }
        EProj { tuple, index, ty } => {
            let tuple = compile_expr(tuple, env);
            core::Expr::EProj {
                tuple: Box::new(tuple),
                index: *index,
                ty: ty.clone(),
            }
        }
    }
}
