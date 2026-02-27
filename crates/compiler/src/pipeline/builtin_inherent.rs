use std::collections::HashSet;

use diagnostics::Diagnostics;

use crate::{
    builtins, compile_match, env::Gensym, names::parse_inherent_method_fn_name,
    pipeline::pipeline::CompilationError,
};

pub(crate) fn collect_required_builtin_collection_methods(
    files: &[crate::core::File],
) -> HashSet<(String, String)> {
    let builtin_keys = builtins::builtin_collection_method_keys();
    let mut required = HashSet::new();
    for file in files {
        for func in &file.toplevels {
            collect_required_builtin_collection_methods_from_block(
                &func.body,
                builtin_keys,
                &mut required,
            );
        }
    }
    required
}

fn collect_required_builtin_collection_methods_from_block(
    block: &crate::core::Block,
    builtin_keys: &HashSet<(String, String)>,
    required: &mut HashSet<(String, String)>,
) {
    for stmt in &block.stmts {
        collect_required_builtin_collection_methods_from_expr(&stmt.value, builtin_keys, required);
    }
    if let Some(tail) = &block.tail {
        collect_required_builtin_collection_methods_from_expr(tail, builtin_keys, required);
    }
}

fn collect_required_builtin_collection_methods_from_expr(
    expr: &crate::core::Expr,
    builtin_keys: &HashSet<(String, String)>,
    required: &mut HashSet<(String, String)>,
) {
    match expr {
        crate::core::Expr::EVar { .. } | crate::core::Expr::EPrim { .. } => {}
        crate::core::Expr::EConstr { args, .. }
        | crate::core::Expr::ETuple { items: args, .. }
        | crate::core::Expr::EArray { items: args, .. } => {
            for arg in args {
                collect_required_builtin_collection_methods_from_expr(arg, builtin_keys, required);
            }
        }
        crate::core::Expr::EClosure { body, .. } => {
            collect_required_builtin_collection_methods_from_expr(body, builtin_keys, required);
        }
        crate::core::Expr::EBlock { block, .. } => {
            collect_required_builtin_collection_methods_from_block(block, builtin_keys, required);
        }
        crate::core::Expr::EMatch {
            expr,
            arms,
            default,
            ..
        } => {
            collect_required_builtin_collection_methods_from_expr(expr, builtin_keys, required);
            for arm in arms {
                collect_required_builtin_collection_methods_from_expr(
                    &arm.lhs,
                    builtin_keys,
                    required,
                );
                collect_required_builtin_collection_methods_from_expr(
                    &arm.body,
                    builtin_keys,
                    required,
                );
            }
            if let Some(default) = default {
                collect_required_builtin_collection_methods_from_expr(
                    default,
                    builtin_keys,
                    required,
                );
            }
        }
        crate::core::Expr::EIf {
            cond,
            then_branch,
            else_branch,
            ..
        } => {
            collect_required_builtin_collection_methods_from_expr(cond, builtin_keys, required);
            collect_required_builtin_collection_methods_from_expr(
                then_branch,
                builtin_keys,
                required,
            );
            collect_required_builtin_collection_methods_from_expr(
                else_branch,
                builtin_keys,
                required,
            );
        }
        crate::core::Expr::EWhile { cond, body, .. } => {
            collect_required_builtin_collection_methods_from_expr(cond, builtin_keys, required);
            collect_required_builtin_collection_methods_from_expr(body, builtin_keys, required);
        }
        crate::core::Expr::EGo { expr, .. }
        | crate::core::Expr::EConstrGet { expr, .. }
        | crate::core::Expr::EUnary { expr, .. }
        | crate::core::Expr::EToDyn { expr, .. }
        | crate::core::Expr::EProj { tuple: expr, .. } => {
            collect_required_builtin_collection_methods_from_expr(expr, builtin_keys, required);
        }
        crate::core::Expr::EBinary { lhs, rhs, .. } => {
            collect_required_builtin_collection_methods_from_expr(lhs, builtin_keys, required);
            collect_required_builtin_collection_methods_from_expr(rhs, builtin_keys, required);
        }
        crate::core::Expr::ECall { func, args, .. } => {
            if let crate::core::Expr::EVar { name, .. } = func.as_ref()
                && let Some((base, method)) = parse_inherent_method_fn_name(name)
            {
                let key = (base.to_string(), method.to_string());
                if builtin_keys.contains(&key) {
                    required.insert(key);
                }
            }
            collect_required_builtin_collection_methods_from_expr(func, builtin_keys, required);
            for arg in args {
                collect_required_builtin_collection_methods_from_expr(arg, builtin_keys, required);
            }
        }
        crate::core::Expr::EDynCall { receiver, args, .. }
        | crate::core::Expr::ETraitCall { receiver, args, .. } => {
            collect_required_builtin_collection_methods_from_expr(receiver, builtin_keys, required);
            for arg in args {
                collect_required_builtin_collection_methods_from_expr(arg, builtin_keys, required);
            }
        }
    }
}

pub(crate) fn compile_builtin_collection_methods(
    required: &HashSet<(String, String)>,
    gensym: &Gensym,
    diagnostics: &mut Diagnostics,
) -> crate::core::File {
    if required.is_empty() {
        return crate::core::File {
            toplevels: Vec::new(),
        };
    }
    let core = compile_match::compile_file(
        &builtins::builtin_env(),
        gensym,
        diagnostics,
        &builtins::builtin_collection_impl_tast(),
    );
    let toplevels = core
        .toplevels
        .into_iter()
        .filter(|f| {
            parse_inherent_method_fn_name(&f.name)
                .map(|(base, method)| required.contains(&(base.to_string(), method.to_string())))
                .unwrap_or(false)
        })
        .collect();
    crate::core::File { toplevels }
}

pub(crate) fn compile_builtin_collection_methods_checked(
    required: &HashSet<(String, String)>,
    gensym: &Gensym,
) -> Result<crate::core::File, CompilationError> {
    let mut diagnostics = Diagnostics::new();
    let core = compile_builtin_collection_methods(required, gensym, &mut diagnostics);
    if diagnostics.has_errors() {
        return Err(CompilationError::Compile { diagnostics });
    }
    Ok(core)
}
