use std::collections::HashSet;

use crate::go::goast as ast;

// Public entry: eliminate unused local variables from Go AST while
// preserving side-effecting expressions (primarily calls).
pub fn eliminate_dead_vars(file: ast::File) -> ast::File {
    let toplevels = file.toplevels.into_iter().map(dce_item).collect();
    ast::File { toplevels }
}

fn dce_item(item: ast::Item) -> ast::Item {
    match item {
        ast::Item::Fn(mut f) => {
            let live_out: HashSet<String> = HashSet::new();
            f.body = dce_block_with_live(f.body, &live_out).0;
            ast::Item::Fn(f)
        }
        ast::Item::Struct(s) => ast::Item::Struct(s),
        ast::Item::Interface(i) => ast::Item::Interface(i),
        ast::Item::EmbededRawString(s) => ast::Item::EmbededRawString(s),
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
            ast::Stmt::Return { expr } => {
                let expr = expr.map(dce_expr);
                if let Some(e) = &expr {
                    add_uses_expr(&mut live, e);
                }
                out.push(ast::Stmt::Return { expr });
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
        ast::Expr::Cast { expr, ty } => ast::Expr::Cast {
            expr: Box::new(dce_expr(*expr)),
            ty,
        },
        ast::Expr::StructLiteral { fields, ty } => ast::Expr::StructLiteral {
            fields: fields.into_iter().map(|(n, e)| (n, dce_expr(e))).collect(),
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
            func,
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
        ast::Expr::Cast { expr, .. } => {
            s.extend(vars_used_in_expr(expr));
        }
        ast::Expr::StructLiteral { fields, .. } => {
            for (_, e) in fields {
                s.extend(vars_used_in_expr(e));
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
                }
            }
            s.extend(&used - &declared);
            if let Some(e) = expr {
                s.extend(vars_used_in_expr(e));
            }
        }
        ast::Expr::Call { args, .. } => {
            for a in args {
                s.extend(vars_used_in_expr(a));
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
        ast::Expr::Cast { expr, .. } => expr_has_side_effects(expr),
        ast::Expr::StructLiteral { fields, .. } => {
            fields.iter().any(|(_, e)| expr_has_side_effects(e))
        }
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
        ast::Stmt::Return { expr } => expr.as_ref().map(expr_has_side_effects).unwrap_or(false),
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
