use std::collections::{HashMap, HashSet};

use crate::go::goast as ast;

// Public entry: eliminate unused local variables from Go AST while
// preserving side-effecting expressions (primarily calls).
pub fn eliminate_dead_vars(file: ast::File) -> ast::File {
    let toplevels = file.toplevels.into_iter().map(dce_item).collect();
    let file = ast::File { toplevels };
    let file = prune_dead_functions(file);
    prune_unused_imports(file)
}

fn dce_item(item: ast::Item) -> ast::Item {
    match item {
        ast::Item::Package(pkg) => ast::Item::Package(pkg),
        ast::Item::Import(imports) => ast::Item::Import(imports),
        ast::Item::Fn(mut f) => {
            let live_out: HashSet<String> = HashSet::new();
            f.body = dce_block_with_live(f.body, &live_out).0;
            ast::Item::Fn(f)
        }
        ast::Item::Struct(s) => ast::Item::Struct(s),
        ast::Item::Interface(i) => ast::Item::Interface(i),
    }
}

fn dce_block_with_live(
    block: ast::Block,
    live_out: &HashSet<String>,
) -> (ast::Block, HashSet<String>) {
    let mut live: HashSet<String> = live_out.clone();
    // Track variables that appear on the LHS of an assignment we keep,
    // so that we don't drop their prior declaration even if its value isn't used.
    let mut needs_decl: HashSet<String> = HashSet::new();
    let mut out: Vec<ast::Stmt> = Vec::with_capacity(block.stmts.len());

    // Process nested blocks first where needed, while scanning backwards
    for stmt in block.stmts.into_iter().rev() {
        match stmt {
            ast::Stmt::Expr(e) => {
                let e = dce_expr(e);
                // keep expression statements; they may have side-effects
                add_uses_expr(&mut live, &e);
                out.push(ast::Stmt::Expr(e));
            }
            ast::Stmt::VarDecl { name, ty, value } => {
                let value = value.map(dce_expr);
                let used_rhs = value.as_ref().map(vars_used_in_expr).unwrap_or_default();

                if live.contains(&name) {
                    // Needed later as an rvalue; keep the declaration with initializer
                    for u in &used_rhs {
                        live.insert(u.clone());
                    }
                    live.remove(&name);
                    out.push(ast::Stmt::VarDecl { name, ty, value });
                } else if needs_decl.contains(&name) {
                    // The variable is assigned later with '=' so we must keep its declaration.
                    // If initializer exists but is dead, preserve its side effects and drop the init.
                    if let Some(v) = value {
                        if expr_has_side_effects(&v) {
                            for u in &used_rhs {
                                live.insert(u.clone());
                            }
                            // Keep side effects before the declaration in final order
                            out.push(ast::Stmt::Expr(v));
                        }
                        // Keep declaration without initializer
                        out.push(ast::Stmt::VarDecl {
                            name: name.clone(),
                            ty,
                            value: None,
                        });
                    } else {
                        out.push(ast::Stmt::VarDecl {
                            name: name.clone(),
                            ty,
                            value: None,
                        });
                    }
                    // Declaration satisfied; further outer scopes don't need it
                    needs_decl.remove(&name);
                } else {
                    // Not needed. Preserve side-effects of RHS if any, then drop
                    if let Some(v) = value
                        && expr_has_side_effects(&v)
                    {
                        for u in &used_rhs {
                            live.insert(u.clone());
                        }
                        out.push(ast::Stmt::Expr(v));
                    }
                }
            }
            ast::Stmt::Assignment { name, value } => {
                let value = dce_expr(value);
                let used_rhs = vars_used_in_expr(&value);
                if live.contains(&name) {
                    // This assignment feeds a later rvalue use; keep it and require a prior decl
                    for u in &used_rhs {
                        live.insert(u.clone());
                    }
                    live.remove(&name);
                    needs_decl.insert(name.clone());
                    out.push(ast::Stmt::Assignment { name, value });
                } else {
                    // Dead store: keep only side effects, do not require declaration
                    if expr_has_side_effects(&value) {
                        for u in &used_rhs {
                            live.insert(u.clone());
                        }
                        out.push(ast::Stmt::Expr(value));
                    }
                }
            }
            ast::Stmt::IndexAssign {
                array,
                index,
                value,
            } => {
                let array = dce_expr(array);
                let index = dce_expr(index);
                let value = dce_expr(value);
                add_uses_expr(&mut live, &array);
                add_uses_expr(&mut live, &index);
                add_uses_expr(&mut live, &value);
                out.push(ast::Stmt::IndexAssign {
                    array,
                    index,
                    value,
                });
            }
            ast::Stmt::PointerAssign { pointer, value } => {
                let pointer = dce_expr(pointer);
                let value = dce_expr(value);
                add_uses_expr(&mut live, &pointer);
                add_uses_expr(&mut live, &value);
                out.push(ast::Stmt::PointerAssign { pointer, value });
            }
            ast::Stmt::FieldAssign { target, value } => {
                let target = dce_expr(target);
                let value = dce_expr(value);
                add_uses_expr(&mut live, &target);
                add_uses_expr(&mut live, &value);
                out.push(ast::Stmt::FieldAssign { target, value });
            }
            ast::Stmt::Return { expr } => {
                let expr = expr.map(dce_expr);
                if let Some(e) = &expr {
                    add_uses_expr(&mut live, e);
                }
                out.push(ast::Stmt::Return { expr });
            }
            ast::Stmt::Loop { body } => {
                let (body_block, body_live_in) = dce_block_with_live(body, &live);
                live.extend(body_live_in);
                needs_decl.extend(assigned_vars_in_block(&body_block));
                out.push(ast::Stmt::Loop { body: body_block });
            }
            ast::Stmt::Break => {
                out.push(ast::Stmt::Break);
            }
            ast::Stmt::If { cond, then, else_ } => {
                let cond = dce_expr(cond);
                let (then_b, then_live_in) = dce_block_with_live(then, &live);
                let (else_b_opt, else_live_in) = if let Some(b) = else_ {
                    let (b2, live_in) = dce_block_with_live(b, &live);
                    (Some(b2), live_in)
                } else {
                    (None, HashSet::new())
                };
                add_uses_expr(&mut live, &cond);
                live.extend(then_live_in);
                live.extend(else_live_in);
                // Propagate assignment requirements up so declarations are preserved
                needs_decl.extend(assigned_vars_in_block(&then_b));
                if let Some(ref eb) = else_b_opt {
                    needs_decl.extend(assigned_vars_in_block(eb));
                }
                out.push(ast::Stmt::If {
                    cond,
                    then: then_b,
                    else_: else_b_opt,
                });
            }
            ast::Stmt::SwitchExpr {
                expr,
                cases,
                default,
            } => {
                let expr = dce_expr(expr);
                let mut new_cases: Vec<(ast::Expr, ast::Block)> = Vec::with_capacity(cases.len());
                let mut cases_live_in: HashSet<String> = HashSet::new();
                for (val, blk) in cases {
                    let val = dce_expr(val);
                    let (b2, live_in) = dce_block_with_live(blk, &live);
                    add_uses_expr(&mut live, &val);
                    cases_live_in.extend(live_in);
                    needs_decl.extend(assigned_vars_in_block(&b2));
                    new_cases.push((val, b2));
                }
                let (default_b, default_live_in) = if let Some(b) = default {
                    let (b2, live_in) = dce_block_with_live(b, &live);
                    needs_decl.extend(assigned_vars_in_block(&b2));
                    (Some(b2), live_in)
                } else {
                    (None, HashSet::new())
                };
                add_uses_expr(&mut live, &expr);
                live.extend(cases_live_in);
                live.extend(default_live_in);
                out.push(ast::Stmt::SwitchExpr {
                    expr,
                    cases: new_cases,
                    default: default_b,
                });
            }
            ast::Stmt::SwitchType {
                bind,
                expr,
                cases,
                default,
            } => {
                let expr = dce_expr(expr);
                let mut new_cases: Vec<(crate::go::goty::GoType, ast::Block)> =
                    Vec::with_capacity(cases.len());
                let mut cases_live_in: HashSet<String> = HashSet::new();
                for (t, blk) in cases {
                    let (b2, live_in) = dce_block_with_live(blk, &live);
                    cases_live_in.extend(live_in);
                    needs_decl.extend(assigned_vars_in_block(&b2));
                    new_cases.push((t, b2));
                }
                let (default_b, default_live_in) = if let Some(b) = default {
                    let (b2, live_in) = dce_block_with_live(b, &live);
                    needs_decl.extend(assigned_vars_in_block(&b2));
                    (Some(b2), live_in)
                } else {
                    (None, HashSet::new())
                };
                // If the type-switch binding variable is not used in any case/default
                // blocks, drop the binding (switch x := e.(type) -> switch e.(type)).
                let bind = bind.filter(|bname| {
                    !(!cases_live_in.contains(bname) && !default_live_in.contains(bname))
                });
                add_uses_expr(&mut live, &expr);
                live.extend(cases_live_in);
                live.extend(default_live_in);
                out.push(ast::Stmt::SwitchType {
                    bind,
                    expr,
                    cases: new_cases,
                    default: default_b,
                });
            }
        }
    }

    out.reverse();
    (ast::Block { stmts: out }, live)
}

fn dce_expr(expr: ast::Expr) -> ast::Expr {
    match expr {
        ast::Expr::FieldAccess { obj, field, ty } => ast::Expr::FieldAccess {
            obj: Box::new(dce_expr(*obj)),
            field,
            ty,
        },
        ast::Expr::Index { array, index, ty } => ast::Expr::Index {
            array: Box::new(dce_expr(*array)),
            index: Box::new(dce_expr(*index)),
            ty,
        },
        ast::Expr::Cast { expr, ty } => ast::Expr::Cast {
            expr: Box::new(dce_expr(*expr)),
            ty,
        },
        ast::Expr::StructLiteral { fields, ty } => ast::Expr::StructLiteral {
            fields: fields.into_iter().map(|(n, e)| (n, dce_expr(e))).collect(),
            ty,
        },
        ast::Expr::ArrayLiteral { elems, ty } => ast::Expr::ArrayLiteral {
            elems: elems.into_iter().map(dce_expr).collect(),
            ty,
        },
        ast::Expr::UnaryOp { op, expr, ty } => ast::Expr::UnaryOp {
            op,
            expr: Box::new(dce_expr(*expr)),
            ty,
        },
        ast::Expr::BinaryOp { op, lhs, rhs, ty } => ast::Expr::BinaryOp {
            op,
            lhs: Box::new(dce_expr(*lhs)),
            rhs: Box::new(dce_expr(*rhs)),
            ty,
        },
        ast::Expr::Block { stmts, expr, ty } => {
            let live_out: HashSet<String> = HashSet::new();
            let (blk, _live_in) = dce_block_with_live(ast::Block { stmts }, &live_out);
            let expr = expr.map(|e| Box::new(dce_expr(*e)));
            ast::Expr::Block {
                stmts: blk.stmts,
                expr,
                ty,
            }
        }
        ast::Expr::Call { func, args, ty } => ast::Expr::Call {
            func: Box::new(dce_expr(*func)),
            args: args.into_iter().map(dce_expr).collect(),
            ty,
        },
        // Leaves
        e @ ast::Expr::Nil { .. }
        | e @ ast::Expr::Void { .. }
        | e @ ast::Expr::Unit { .. }
        | e @ ast::Expr::Var { .. }
        | e @ ast::Expr::Bool { .. }
        | e @ ast::Expr::Int { .. }
        | e @ ast::Expr::String { .. } => e,
    }
}

// === Utility: variable usage and side-effects ===

fn add_uses_expr(live: &mut HashSet<String>, e: &ast::Expr) {
    for u in vars_used_in_expr(e) {
        live.insert(u);
    }
}

fn assigned_vars_in_block(b: &ast::Block) -> HashSet<String> {
    let mut s = HashSet::new();
    for stmt in &b.stmts {
        match stmt {
            ast::Stmt::Assignment { name, .. } => {
                s.insert(name.clone());
            }
            ast::Stmt::If { then, else_, .. } => {
                s.extend(assigned_vars_in_block(then));
                if let Some(b) = else_ {
                    s.extend(assigned_vars_in_block(b));
                }
            }
            ast::Stmt::SwitchExpr { cases, default, .. } => {
                for (_e, blk) in cases {
                    s.extend(assigned_vars_in_block(blk));
                }
                if let Some(b) = default {
                    s.extend(assigned_vars_in_block(b));
                }
            }
            ast::Stmt::SwitchType { cases, default, .. } => {
                for (_t, blk) in cases {
                    s.extend(assigned_vars_in_block(blk));
                }
                if let Some(b) = default {
                    s.extend(assigned_vars_in_block(b));
                }
            }
            ast::Stmt::Loop { body } => {
                s.extend(assigned_vars_in_block(body));
            }
            ast::Stmt::Break => {}
            _ => {}
        }
    }
    s
}

fn vars_used_in_expr(e: &ast::Expr) -> HashSet<String> {
    let mut s = HashSet::new();
    match e {
        ast::Expr::Var { name, .. } => {
            s.insert(name.clone());
        }
        ast::Expr::FieldAccess { obj, .. } => {
            s.extend(vars_used_in_expr(obj));
        }
        ast::Expr::Index { array, index, .. } => {
            s.extend(vars_used_in_expr(array));
            s.extend(vars_used_in_expr(index));
        }
        ast::Expr::UnaryOp { expr, .. } => {
            s.extend(vars_used_in_expr(expr));
        }
        ast::Expr::BinaryOp { lhs, rhs, .. } => {
            s.extend(vars_used_in_expr(lhs));
            s.extend(vars_used_in_expr(rhs));
        }
        ast::Expr::Cast { expr, .. } => {
            s.extend(vars_used_in_expr(expr));
        }
        ast::Expr::StructLiteral { fields, .. } => {
            for (_, e) in fields {
                s.extend(vars_used_in_expr(e));
            }
        }
        ast::Expr::ArrayLiteral { elems, .. } => {
            for e in elems {
                s.extend(vars_used_in_expr(e));
            }
        }
        ast::Expr::Call { func, args, .. } => {
            s.extend(vars_used_in_expr(func));
            for arg in args {
                s.extend(vars_used_in_expr(arg));
            }
        }
        ast::Expr::Block { stmts, expr, .. } => {
            // Compute free vars from statements/expr inside block without cloning
            let mut used: HashSet<String> = HashSet::new();
            let mut declared: HashSet<String> = HashSet::new();
            for st in stmts {
                match st {
                    ast::Stmt::VarDecl { name, value, .. } => {
                        declared.insert(name.clone());
                        if let Some(v) = value {
                            used.extend(vars_used_in_expr(v));
                        }
                    }
                    ast::Stmt::Assignment { value, .. } => {
                        used.extend(vars_used_in_expr(value));
                    }
                    ast::Stmt::IndexAssign {
                        array,
                        index,
                        value,
                    } => {
                        used.extend(vars_used_in_expr(array));
                        used.extend(vars_used_in_expr(index));
                        used.extend(vars_used_in_expr(value));
                    }
                    ast::Stmt::PointerAssign { pointer, value } => {
                        used.extend(vars_used_in_expr(pointer));
                        used.extend(vars_used_in_expr(value));
                    }
                    ast::Stmt::FieldAssign { target, value } => {
                        used.extend(vars_used_in_expr(target));
                        used.extend(vars_used_in_expr(value));
                    }
                    ast::Stmt::Return { expr } => {
                        if let Some(e) = expr {
                            used.extend(vars_used_in_expr(e));
                        }
                    }
                    ast::Stmt::Expr(e) => used.extend(vars_used_in_expr(e)),
                    ast::Stmt::If { cond, then, else_ } => {
                        used.extend(vars_used_in_expr(cond));
                        used.extend(free_vars_in_block(then));
                        if let Some(b) = else_ {
                            used.extend(free_vars_in_block(b));
                        }
                    }
                    ast::Stmt::SwitchExpr {
                        expr,
                        cases,
                        default,
                    } => {
                        used.extend(vars_used_in_expr(expr));
                        for (e, b) in cases {
                            used.extend(vars_used_in_expr(e));
                            used.extend(free_vars_in_block(b));
                        }
                        if let Some(b) = default {
                            used.extend(free_vars_in_block(b));
                        }
                    }
                    ast::Stmt::SwitchType {
                        expr,
                        cases,
                        default,
                        ..
                    } => {
                        used.extend(vars_used_in_expr(expr));
                        for (_t, b) in cases {
                            used.extend(free_vars_in_block(b));
                        }
                        if let Some(b) = default {
                            used.extend(free_vars_in_block(b));
                        }
                    }
                    ast::Stmt::Loop { body } => {
                        used.extend(free_vars_in_block(body));
                    }
                    ast::Stmt::Break => {}
                }
            }
            s.extend(&used - &declared);
            if let Some(e) = expr {
                s.extend(vars_used_in_expr(e));
            }
        }
        ast::Expr::Nil { .. }
        | ast::Expr::Void { .. }
        | ast::Expr::Unit { .. }
        | ast::Expr::Bool { .. }
        | ast::Expr::Int { .. }
        | ast::Expr::String { .. } => {}
    }
    s
}

fn free_vars_in_block(b: &ast::Block) -> HashSet<String> {
    let mut used: HashSet<String> = HashSet::new();
    let mut declared: HashSet<String> = HashSet::new();
    for stmt in &b.stmts {
        match stmt {
            ast::Stmt::VarDecl { name, value, .. } => {
                declared.insert(name.clone());
                if let Some(v) = value {
                    used.extend(vars_used_in_expr(v));
                }
            }
            ast::Stmt::Assignment { value, .. } => {
                used.extend(vars_used_in_expr(value));
            }
            ast::Stmt::IndexAssign {
                array,
                index,
                value,
            } => {
                used.extend(vars_used_in_expr(array));
                used.extend(vars_used_in_expr(index));
                used.extend(vars_used_in_expr(value));
            }
            ast::Stmt::PointerAssign { pointer, value } => {
                used.extend(vars_used_in_expr(pointer));
                used.extend(vars_used_in_expr(value));
            }
            ast::Stmt::FieldAssign { target, value } => {
                used.extend(vars_used_in_expr(target));
                used.extend(vars_used_in_expr(value));
            }
            ast::Stmt::Return { expr } => {
                if let Some(e) = expr {
                    used.extend(vars_used_in_expr(e));
                }
            }
            ast::Stmt::Expr(e) => used.extend(vars_used_in_expr(e)),
            ast::Stmt::If { cond, then, else_ } => {
                used.extend(vars_used_in_expr(cond));
                used.extend(free_vars_in_block(then));
                if let Some(b) = else_ {
                    used.extend(free_vars_in_block(b));
                }
            }
            ast::Stmt::SwitchExpr {
                expr,
                cases,
                default,
            } => {
                used.extend(vars_used_in_expr(expr));
                for (e, b) in cases {
                    used.extend(vars_used_in_expr(e));
                    used.extend(free_vars_in_block(b));
                }
                if let Some(b) = default {
                    used.extend(free_vars_in_block(b));
                }
            }
            ast::Stmt::SwitchType {
                expr,
                cases,
                default,
                ..
            } => {
                used.extend(vars_used_in_expr(expr));
                for (_t, b) in cases {
                    used.extend(free_vars_in_block(b));
                }
                if let Some(b) = default {
                    used.extend(free_vars_in_block(b));
                }
            }
            ast::Stmt::Loop { body } => {
                used.extend(free_vars_in_block(body));
            }
            ast::Stmt::Break => {}
        }
    }
    &used - &declared
}

fn expr_has_side_effects(e: &ast::Expr) -> bool {
    match e {
        ast::Expr::Call { .. } => true,
        ast::Expr::Block { stmts, expr, .. } => {
            // any side-effect in nested statements or nested expr
            stmts.iter().any(stmt_has_side_effects)
                || expr
                    .as_ref()
                    .map(|e| expr_has_side_effects(e))
                    .unwrap_or(false)
        }
        ast::Expr::FieldAccess { obj, .. } => expr_has_side_effects(obj),
        ast::Expr::Index { array, index, .. } => {
            expr_has_side_effects(array) || expr_has_side_effects(index)
        }
        ast::Expr::UnaryOp { expr, .. } => expr_has_side_effects(expr),
        ast::Expr::BinaryOp { lhs, rhs, .. } => {
            expr_has_side_effects(lhs) || expr_has_side_effects(rhs)
        }
        ast::Expr::Cast { expr, .. } => expr_has_side_effects(expr),
        ast::Expr::StructLiteral { fields, .. } => {
            fields.iter().any(|(_, e)| expr_has_side_effects(e))
        }
        ast::Expr::ArrayLiteral { elems, .. } => elems.iter().any(expr_has_side_effects),
        ast::Expr::Var { .. }
        | ast::Expr::Nil { .. }
        | ast::Expr::Void { .. }
        | ast::Expr::Unit { .. }
        | ast::Expr::Bool { .. }
        | ast::Expr::Int { .. }
        | ast::Expr::String { .. } => false,
    }
}

fn stmt_has_side_effects(s: &ast::Stmt) -> bool {
    match s {
        ast::Stmt::Expr(e) => expr_has_side_effects(e),
        ast::Stmt::VarDecl { value, .. } => {
            value.as_ref().map(expr_has_side_effects).unwrap_or(false)
        }
        ast::Stmt::Assignment { value, .. } => expr_has_side_effects(value),
        ast::Stmt::IndexAssign { .. } => true,
        ast::Stmt::PointerAssign { .. } => true,
        ast::Stmt::FieldAssign { .. } => true,
        ast::Stmt::Return { expr } => expr.as_ref().map(expr_has_side_effects).unwrap_or(false),
        ast::Stmt::Loop { body } => body.stmts.iter().any(stmt_has_side_effects),
        ast::Stmt::Break => false,
        ast::Stmt::If { cond, then, else_ } => {
            expr_has_side_effects(cond)
                || then.stmts.iter().any(stmt_has_side_effects)
                || else_
                    .as_ref()
                    .map(|b| b.stmts.iter().any(stmt_has_side_effects))
                    .unwrap_or(false)
        }
        ast::Stmt::SwitchExpr {
            expr,
            cases,
            default,
        } => {
            expr_has_side_effects(expr)
                || cases.iter().any(|(e, b)| {
                    expr_has_side_effects(e) || b.stmts.iter().any(stmt_has_side_effects)
                })
                || default
                    .as_ref()
                    .map(|b| b.stmts.iter().any(stmt_has_side_effects))
                    .unwrap_or(false)
        }
        ast::Stmt::SwitchType {
            expr,
            cases,
            default,
            ..
        } => {
            expr_has_side_effects(expr)
                || cases
                    .iter()
                    .any(|(_t, b)| b.stmts.iter().any(stmt_has_side_effects))
                || default
                    .as_ref()
                    .map(|b| b.stmts.iter().any(stmt_has_side_effects))
                    .unwrap_or(false)
        }
    }
}

fn prune_dead_functions(file: ast::File) -> ast::File {
    let mut fn_map: HashMap<String, &ast::Fn> = HashMap::new();
    for item in &file.toplevels {
        if let ast::Item::Fn(f) = item {
            fn_map.insert(f.name.clone(), f);
        }
    }

    if fn_map.is_empty() {
        return file;
    }

    let fn_names: HashSet<String> = fn_map.keys().cloned().collect();

    let mut reachable: HashSet<String> = HashSet::new();
    let mut stack: Vec<String> = Vec::new();
    for root in ["main", "main0"] {
        if fn_map.contains_key(root) {
            stack.push(root.to_string());
        }
    }

    while let Some(name) = stack.pop() {
        if !reachable.insert(name.clone()) {
            continue;
        }

        if let Some(f) = fn_map.get(&name) {
            for callee in called_functions_in_fn(f, &fn_names) {
                if fn_map.contains_key(&callee) {
                    stack.push(callee);
                }
            }
        }
    }

    let toplevels = file
        .toplevels
        .into_iter()
        .filter(|item| match item {
            ast::Item::Fn(f) => reachable.contains(&f.name),
            _ => true,
        })
        .collect();

    ast::File { toplevels }
}

fn called_functions_in_fn(f: &ast::Fn, fn_names: &HashSet<String>) -> HashSet<String> {
    let mut calls = HashSet::new();
    collect_called_in_block(&f.body, &mut calls, fn_names);
    calls
}

fn collect_called_in_block(
    block: &ast::Block,
    calls: &mut HashSet<String>,
    fn_names: &HashSet<String>,
) {
    for stmt in &block.stmts {
        collect_called_in_stmt(stmt, calls, fn_names);
    }
}

fn collect_called_in_stmt(
    stmt: &ast::Stmt,
    calls: &mut HashSet<String>,
    fn_names: &HashSet<String>,
) {
    match stmt {
        ast::Stmt::Expr(e) => collect_called_in_expr(e, calls, fn_names),
        ast::Stmt::VarDecl { value, .. } => {
            if let Some(v) = value {
                collect_called_in_expr(v, calls, fn_names);
            }
        }
        ast::Stmt::Assignment { value, .. } => collect_called_in_expr(value, calls, fn_names),
        ast::Stmt::IndexAssign {
            array,
            index,
            value,
        } => {
            collect_called_in_expr(array, calls, fn_names);
            collect_called_in_expr(index, calls, fn_names);
            collect_called_in_expr(value, calls, fn_names);
        }
        ast::Stmt::PointerAssign { pointer, value } => {
            collect_called_in_expr(pointer, calls, fn_names);
            collect_called_in_expr(value, calls, fn_names);
        }
        ast::Stmt::FieldAssign { target, value } => {
            collect_called_in_expr(target, calls, fn_names);
            collect_called_in_expr(value, calls, fn_names);
        }
        ast::Stmt::Return { expr } => {
            if let Some(e) = expr {
                collect_called_in_expr(e, calls, fn_names);
            }
        }
        ast::Stmt::If { cond, then, else_ } => {
            collect_called_in_expr(cond, calls, fn_names);
            collect_called_in_block(then, calls, fn_names);
            if let Some(b) = else_ {
                collect_called_in_block(b, calls, fn_names);
            }
        }
        ast::Stmt::SwitchExpr {
            expr,
            cases,
            default,
        } => {
            collect_called_in_expr(expr, calls, fn_names);
            for (e, b) in cases {
                collect_called_in_expr(e, calls, fn_names);
                collect_called_in_block(b, calls, fn_names);
            }
            if let Some(b) = default {
                collect_called_in_block(b, calls, fn_names);
            }
        }
        ast::Stmt::SwitchType {
            expr,
            cases,
            default,
            ..
        } => {
            collect_called_in_expr(expr, calls, fn_names);
            for (_t, b) in cases {
                collect_called_in_block(b, calls, fn_names);
            }
            if let Some(b) = default {
                collect_called_in_block(b, calls, fn_names);
            }
        }
        ast::Stmt::Loop { body } => {
            collect_called_in_block(body, calls, fn_names);
        }
        ast::Stmt::Break => {}
    }
}

fn collect_called_in_expr(
    expr: &ast::Expr,
    calls: &mut HashSet<String>,
    fn_names: &HashSet<String>,
) {
    match expr {
        ast::Expr::Call { func, args, .. } => {
            collect_called_in_expr(func, calls, fn_names);
            for arg in args {
                collect_called_in_expr(arg, calls, fn_names);
            }
        }
        ast::Expr::FieldAccess { obj, .. } => collect_called_in_expr(obj, calls, fn_names),
        ast::Expr::Index { array, index, .. } => {
            collect_called_in_expr(array, calls, fn_names);
            collect_called_in_expr(index, calls, fn_names);
        }
        ast::Expr::Cast { expr, .. } => collect_called_in_expr(expr, calls, fn_names),
        ast::Expr::StructLiteral { fields, .. } => {
            for (_, e) in fields {
                collect_called_in_expr(e, calls, fn_names);
            }
        }
        ast::Expr::ArrayLiteral { elems, .. } => {
            for e in elems {
                collect_called_in_expr(e, calls, fn_names);
            }
        }
        ast::Expr::Block { stmts, expr, .. } => {
            for stmt in stmts {
                collect_called_in_stmt(stmt, calls, fn_names);
            }
            if let Some(e) = expr {
                collect_called_in_expr(e, calls, fn_names);
            }
        }
        ast::Expr::UnaryOp { expr, .. } => collect_called_in_expr(expr, calls, fn_names),
        ast::Expr::BinaryOp { lhs, rhs, .. } => {
            collect_called_in_expr(lhs, calls, fn_names);
            collect_called_in_expr(rhs, calls, fn_names);
        }
        ast::Expr::Var { name, .. } => {
            if fn_names.contains(name) {
                calls.insert(name.clone());
            }
        }
        ast::Expr::Nil { .. }
        | ast::Expr::Void { .. }
        | ast::Expr::Unit { .. }
        | ast::Expr::Bool { .. }
        | ast::Expr::Int { .. }
        | ast::Expr::String { .. } => {}
    }
}

fn prune_unused_imports(file: ast::File) -> ast::File {
    let import_names = gather_import_names(&file);
    if import_names.is_empty() {
        return file;
    }

    let used_packages = collect_used_packages(&file, &import_names);

    let mut toplevels = Vec::with_capacity(file.toplevels.len());
    for item in file.toplevels {
        match item {
            ast::Item::Import(mut decl) => {
                decl.specs.retain(|spec| {
                    let binding = import_spec_binding(spec);
                    used_packages.contains(&binding)
                });
                if !decl.specs.is_empty() {
                    toplevels.push(ast::Item::Import(decl));
                }
            }
            other => toplevels.push(other),
        }
    }

    ast::File { toplevels }
}

fn gather_import_names(file: &ast::File) -> HashSet<String> {
    let mut names = HashSet::new();
    for item in &file.toplevels {
        if let ast::Item::Import(decl) = item {
            for spec in &decl.specs {
                names.insert(import_spec_binding(spec));
            }
        }
    }
    names
}

fn collect_used_packages(file: &ast::File, imports: &HashSet<String>) -> HashSet<String> {
    let mut used = HashSet::new();
    for item in &file.toplevels {
        collect_packages_in_item(item, imports, &mut used);
    }
    used
}

fn collect_packages_in_item(
    item: &ast::Item,
    imports: &HashSet<String>,
    used: &mut HashSet<String>,
) {
    match item {
        ast::Item::Fn(f) => collect_packages_in_block(&f.body, imports, used),
        ast::Item::Struct(s) => {
            for method in &s.methods {
                collect_packages_in_block(&method.body, imports, used);
            }
        }
        ast::Item::Package(_) | ast::Item::Import(_) | ast::Item::Interface(_) => {}
    }
}

fn collect_packages_in_block(
    block: &ast::Block,
    imports: &HashSet<String>,
    used: &mut HashSet<String>,
) {
    for stmt in &block.stmts {
        collect_packages_in_stmt(stmt, imports, used);
    }
}

fn collect_packages_in_stmt(
    stmt: &ast::Stmt,
    imports: &HashSet<String>,
    used: &mut HashSet<String>,
) {
    match stmt {
        ast::Stmt::Expr(e) => collect_packages_in_expr(e, imports, used),
        ast::Stmt::VarDecl { value, .. } => {
            if let Some(v) = value {
                collect_packages_in_expr(v, imports, used);
            }
        }
        ast::Stmt::Assignment { value, .. } => collect_packages_in_expr(value, imports, used),
        ast::Stmt::IndexAssign {
            array,
            index,
            value,
        } => {
            collect_packages_in_expr(array, imports, used);
            collect_packages_in_expr(index, imports, used);
            collect_packages_in_expr(value, imports, used);
        }
        ast::Stmt::PointerAssign { pointer, value } => {
            collect_packages_in_expr(pointer, imports, used);
            collect_packages_in_expr(value, imports, used);
        }
        ast::Stmt::FieldAssign { target, value } => {
            collect_packages_in_expr(target, imports, used);
            collect_packages_in_expr(value, imports, used);
        }
        ast::Stmt::Return { expr } => {
            if let Some(e) = expr {
                collect_packages_in_expr(e, imports, used);
            }
        }
        ast::Stmt::If { cond, then, else_ } => {
            collect_packages_in_expr(cond, imports, used);
            collect_packages_in_block(then, imports, used);
            if let Some(b) = else_ {
                collect_packages_in_block(b, imports, used);
            }
        }
        ast::Stmt::SwitchExpr {
            expr,
            cases,
            default,
        } => {
            collect_packages_in_expr(expr, imports, used);
            for (e, b) in cases {
                collect_packages_in_expr(e, imports, used);
                collect_packages_in_block(b, imports, used);
            }
            if let Some(b) = default {
                collect_packages_in_block(b, imports, used);
            }
        }
        ast::Stmt::SwitchType {
            expr,
            cases,
            default,
            ..
        } => {
            collect_packages_in_expr(expr, imports, used);
            for (_t, b) in cases {
                collect_packages_in_block(b, imports, used);
            }
            if let Some(b) = default {
                collect_packages_in_block(b, imports, used);
            }
        }
        ast::Stmt::Loop { body } => {
            collect_packages_in_block(body, imports, used);
        }
        ast::Stmt::Break => {}
    }
}

fn collect_packages_in_expr(
    expr: &ast::Expr,
    imports: &HashSet<String>,
    used: &mut HashSet<String>,
) {
    match expr {
        ast::Expr::Call { func, args, .. } => {
            if let ast::Expr::Var { name, .. } = func.as_ref()
                && let Some((pkg, _)) = name.split_once('.')
                && imports.contains(pkg)
            {
                used.insert(pkg.to_string());
            }
            collect_packages_in_expr(func, imports, used);
            for arg in args {
                collect_packages_in_expr(arg, imports, used);
            }
        }
        ast::Expr::FieldAccess { obj, .. } => {
            collect_packages_in_expr(obj, imports, used);
        }
        ast::Expr::Index { array, index, .. } => {
            collect_packages_in_expr(array, imports, used);
            collect_packages_in_expr(index, imports, used);
        }
        ast::Expr::Cast { expr, .. } => collect_packages_in_expr(expr, imports, used),
        ast::Expr::StructLiteral { fields, .. } => {
            for (_, e) in fields {
                collect_packages_in_expr(e, imports, used);
            }
        }
        ast::Expr::ArrayLiteral { elems, .. } => {
            for e in elems {
                collect_packages_in_expr(e, imports, used);
            }
        }
        ast::Expr::Block { stmts, expr, .. } => {
            for stmt in stmts {
                collect_packages_in_stmt(stmt, imports, used);
            }
            if let Some(e) = expr {
                collect_packages_in_expr(e, imports, used);
            }
        }
        ast::Expr::UnaryOp { expr, .. } => collect_packages_in_expr(expr, imports, used),
        ast::Expr::BinaryOp { lhs, rhs, .. } => {
            collect_packages_in_expr(lhs, imports, used);
            collect_packages_in_expr(rhs, imports, used);
        }
        ast::Expr::Nil { .. }
        | ast::Expr::Void { .. }
        | ast::Expr::Unit { .. }
        | ast::Expr::Var { .. }
        | ast::Expr::Bool { .. }
        | ast::Expr::Int { .. }
        | ast::Expr::String { .. } => {}
    }
}

fn import_spec_binding(spec: &ast::ImportSpec) -> String {
    if let Some(alias) = &spec.alias {
        alias.clone()
    } else {
        spec.path
            .rsplit('/')
            .next()
            .map(|s| s.to_string())
            .unwrap_or_else(|| spec.path.clone())
    }
}
