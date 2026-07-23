use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
};

use cst::cst::CstNode;
use cst::nodes::{BinaryExpr, Block, ClosureExpr, Fn, MatchArm, Pattern};
use parser::syntax::{MySyntaxKind, MySyntaxNode, MySyntaxNodePtr};
use text_size::TextSize;

use crate::{env::GlobalTypeEnv, registry::ModuleCoord, tast};

use super::{
    ColonColonCompletionItem, ColonColonCompletionKind, DotCompletionItem, DotCompletionKind,
    ValueCompletionItem, ValueCompletionKind,
    hir_index::{ClosureParamIndex, HirResultsIndex},
    signature::{CallSignatureContext, call_signature_context_from_parts},
    symbol_index::{build_symbol_lookup, build_symbol_lookup_with_overrides},
    syntax::{
        ancestor_path_from_token, call_expr_and_active_parameter, ident_prefix_at_offset,
        token_at_offset_for_query, use_decl_from_token,
    },
    typecheck::typecheck_for_query_with_overrides,
};

const COMPLETION_PLACEHOLDER: &str = "completion_placeholder";
const VALUE_COMPLETION_KEYWORDS: &[&str] = &[
    "array", "bool", "char", "dyn", "else", "enum", "extern", "false", "float32", "float64", "fn",
    "for", "go", "if", "impl", "in", "int8", "int16", "int32", "int64", "let", "match", "package",
    "pub", "return", "string", "struct", "trait", "true", "type", "uint8", "uint16", "uint32",
    "uint64", "unit", "use", "while", "_",
];

#[derive(Debug, Clone)]
struct RankedValueItem {
    item: ValueCompletionItem,
    ty_text: Option<String>,
    kind_rank: u8,
    scope_rank: usize,
}

pub fn dot_completions(
    path: &Path,
    src: &str,
    line: u32,
    col: u32,
) -> Option<Vec<DotCompletionItem>> {
    dot_completions_with_overrides(path, src, line, col, &HashMap::new())
}

pub fn dot_completions_with_overrides(
    path: &Path,
    src: &str,
    line: u32,
    col: u32,
    source_overrides: &HashMap<PathBuf, String>,
) -> Option<Vec<DotCompletionItem>> {
    crate::pipeline::with_compiler_stack(|| {
        let line_index = line_index::LineIndex::new(src);
        let offset = line_index.offset(line_index::LineCol { line, col })?;
        let (prefix_start, prefix) = ident_prefix_at_offset(src, offset)?;
        let dot_offset = prefix_start.checked_sub(TextSize::from(1))?;
        if src.as_bytes().get(u32::from(dot_offset) as usize) != Some(&b'.') {
            return None;
        }

        let parse_src = if prefix.is_empty() {
            let mut fixed_src = src.to_string();
            let insert_index = u32::from(offset) as usize;
            fixed_src.insert_str(insert_index, COMPLETION_PLACEHOLDER);
            fixed_src
        } else {
            src.to_string()
        };

        let result = parser::parse(path, &parse_src);
        let root = MySyntaxNode::new_root(result.green_node);
        let file = cst::cst::File::cast(root.clone())?;
        let dot_token = match file.syntax().token_at_offset(dot_offset) {
            rowan::TokenAtOffset::None => return None,
            rowan::TokenAtOffset::Single(token) => token,
            rowan::TokenAtOffset::Between(left, right) => {
                if right.kind() == MySyntaxKind::Dot {
                    right
                } else if left.kind() == MySyntaxKind::Dot {
                    left
                } else {
                    return None;
                }
            }
        };

        if dot_token.kind() != MySyntaxKind::Dot {
            return None;
        }

        let mut current = dot_token.parent();
        let mut binary_node = None;
        while let Some(node) = current {
            if node.kind() == MySyntaxKind::EXPR_BINARY {
                binary_node = Some(node);
                break;
            }
            current = node.parent();
        }
        let binary_node = binary_node?;
        let binary_expr = BinaryExpr::cast(binary_node)?;
        binary_expr
            .op()
            .map(|token| token.kind())
            .filter(|kind| *kind == MySyntaxKind::Dot)?;

        let mut exprs = binary_expr.exprs();
        let lhs_expr = exprs.next()?;
        let lhs_ptr = MySyntaxNodePtr::new(lhs_expr.syntax());

        let (hir_table, results, genv, _diagnostics) =
            typecheck_for_query_with_overrides(path, &parse_src, source_overrides).ok()?;
        let index = HirResultsIndex::new(&hir_table, path);
        let expr_id = index.expr_id(&lhs_ptr)?;
        let ty = normalize_completion_ty(results.expr_ty(expr_id)?.clone());
        let items = completions_for_type(&genv, &ty);
        Some(filter_dot_items(items, &prefix))
    })
}

pub fn value_completions(
    path: &Path,
    src: &str,
    line: u32,
    col: u32,
) -> Option<Vec<ValueCompletionItem>> {
    value_completions_with_overrides(path, src, line, col, &HashMap::new())
}

pub fn value_completions_with_overrides(
    path: &Path,
    src: &str,
    line: u32,
    col: u32,
    source_overrides: &HashMap<PathBuf, String>,
) -> Option<Vec<ValueCompletionItem>> {
    crate::pipeline::with_compiler_stack(|| {
        let line_index = line_index::LineIndex::new(src);
        let offset = line_index.offset(line_index::LineCol { line, col })?;
        let (prefix_start, prefix) = ident_prefix_at_offset(src, offset)?;
        if let Some(items) = import_root_completions(path, src, offset, prefix_start, &prefix) {
            return Some(items);
        }

        if prefix_start > TextSize::from(0)
            && src
                .as_bytes()
                .get(u32::from(prefix_start.checked_sub(TextSize::from(1))?) as usize)
                == Some(&b'.')
        {
            return None;
        }

        if prefix_start >= TextSize::from(2)
            && src.as_bytes().get(
                u32::from(prefix_start.checked_sub(TextSize::from(2))?) as usize
                    ..u32::from(prefix_start) as usize,
            ) == Some(b"::")
        {
            return None;
        }

        if prefix.is_empty() {
            return Some(Vec::new());
        }

        let parse = parser::parse(path, src);
        let root = MySyntaxNode::new_root(parse.green_node);

        let mut ranked_items = Vec::new();
        let mut call_context = None;
        if let Ok((hir_table, results, genv, _diagnostics)) =
            typecheck_for_query_with_overrides(path, src, source_overrides)
        {
            let current_package = build_symbol_lookup_with_overrides(path, src, source_overrides)
                .graph
                .map(|graph| graph.entry_package);
            let index = HirResultsIndex::new(&hir_table, path);
            let closure_params = ClosureParamIndex::new(&hir_table, path);
            call_context = call_expr_and_active_parameter(&root, offset).and_then(
                |(call_expr, active_parameter)| {
                    call_signature_context_from_parts(
                        path,
                        src,
                        &hir_table,
                        &results,
                        &call_expr,
                        active_parameter,
                    )
                },
            );

            ranked_items.extend(visible_local_items(
                &root,
                offset,
                &index,
                &closure_params,
                &results,
            ));
            ranked_items.extend(genv.value_env.funcs.iter().filter_map(|(name, scheme)| {
                let name = local_completion_function_name(name, current_package.as_deref())?;
                Some(RankedValueItem {
                    item: ValueCompletionItem {
                        name,
                        kind: ValueCompletionKind::Function,
                        detail: Some(scheme.ty.to_pretty(80)),
                    },
                    ty_text: Some(scheme.ty.to_pretty(80)),
                    kind_rank: 1,
                    scope_rank: usize::MAX - 1,
                })
            }));
        }

        ranked_items.extend(visible_import_namespace_items(path, src));

        let mut seen = HashSet::new();
        let mut items = Vec::new();
        for ranked in ranked_items
            .into_iter()
            .filter(|item| item.item.name.starts_with(&prefix))
        {
            if seen.insert(ranked.item.name.clone()) {
                items.push(ranked);
            }
        }

        for keyword in VALUE_COMPLETION_KEYWORDS {
            if !keyword.starts_with(&prefix) {
                continue;
            }
            if !seen.insert((*keyword).to_string()) {
                continue;
            }
            items.push(RankedValueItem {
                item: ValueCompletionItem {
                    name: (*keyword).to_string(),
                    kind: ValueCompletionKind::Keyword,
                    detail: None,
                },
                ty_text: None,
                kind_rank: 3,
                scope_rank: usize::MAX,
            });
        }

        let expected_ty = call_context
            .as_ref()
            .and_then(CallSignatureContext::expected_param)
            .map(|param| param.ty.to_pretty(80));

        sort_value_items(&mut items, expected_ty.as_deref());
        Some(items.into_iter().map(|item| item.item).take(50).collect())
    })
}

pub fn colon_colon_completions(
    path: &Path,
    src: &str,
    line: u32,
    col: u32,
) -> Option<Vec<ColonColonCompletionItem>> {
    colon_colon_completions_with_overrides(path, src, line, col, &HashMap::new())
}

pub fn colon_colon_completions_with_overrides(
    path: &Path,
    src: &str,
    line: u32,
    col: u32,
    source_overrides: &HashMap<PathBuf, String>,
) -> Option<Vec<ColonColonCompletionItem>> {
    crate::pipeline::with_compiler_stack(|| {
        let line_index = line_index::LineIndex::new(src);
        let offset = line_index.offset(line_index::LineCol { line, col })?;
        let (prefix_start, prefix) = ident_prefix_at_offset(src, offset)?;
        let colon_start = prefix_start.checked_sub(TextSize::from(2))?;
        if src
            .as_bytes()
            .get(u32::from(colon_start) as usize..u32::from(prefix_start) as usize)
            != Some(b"::")
        {
            return None;
        }

        let parse_src = if prefix.is_empty() {
            let mut fixed_src = src.to_string();
            let insert_index = u32::from(offset) as usize;
            fixed_src.insert_str(insert_index, COMPLETION_PLACEHOLDER);
            fixed_src
        } else {
            src.to_string()
        };

        let result = parser::parse(path, &parse_src);
        let root = MySyntaxNode::new_root(result.green_node);
        let file = cst::cst::File::cast(root.clone())?;

        let focus_offset = if prefix.is_empty() {
            offset
        } else {
            offset.checked_sub(TextSize::from(1))?
        };

        let token = match file.syntax().token_at_offset(focus_offset) {
            rowan::TokenAtOffset::None => None,
            rowan::TokenAtOffset::Single(token) => Some(token),
            rowan::TokenAtOffset::Between(left, right) => {
                if right.kind() == MySyntaxKind::Ident {
                    Some(right)
                } else {
                    Some(left)
                }
            }
        }?;

        let path_node = ancestor_path_from_token(&token)?;
        let segments = path_node
            .ident_tokens()
            .map(|tok| tok.to_string())
            .collect::<Vec<_>>();
        if segments.is_empty() {
            return None;
        }
        let namespace = segments[..segments.len().saturating_sub(1)].join("::");
        if namespace.is_empty() {
            return None;
        }

        let genv = typecheck_for_query_with_overrides(path, &parse_src, source_overrides)
            .ok()
            .map(|(_hir_table, _results, genv, _diagnostics)| genv);
        let package_items = if use_decl_from_token(&token).is_some() {
            import_colon_colon_items_for_namespace(path, &namespace)
        } else {
            Vec::new()
        };
        let mut items = if package_items.is_empty() {
            let namespace = resolve_completion_namespace(path, src, &namespace);
            colon_colon_items_for_namespace(genv.as_ref(), &namespace)
        } else {
            package_items
        };
        items.sort_by(|a, b| a.name.cmp(&b.name));
        items.dedup_by(|a, b| a.name == b.name && a.kind == b.kind && a.detail == b.detail);
        items.retain(|item| item.name.starts_with(&prefix));
        Some(items)
    })
}

fn local_completion_function_name(name: &str, current_package: Option<&str>) -> Option<String> {
    if !name.contains("::") {
        return Some(name.to_string());
    }
    let local = name.strip_prefix(&format!("{}::", current_package?))?;
    (!local.contains("::")).then(|| local.to_string())
}

fn normalize_completion_ty(ty: tast::Ty) -> tast::Ty {
    match ty {
        tast::Ty::TRef { elem } => normalize_completion_ty(*elem),
        other => other,
    }
}

fn completions_for_type(genv: &GlobalTypeEnv, ty: &tast::Ty) -> Vec<DotCompletionItem> {
    let mut items = Vec::new();

    if let Some(name) = type_constructor_name(ty) {
        let uident = tast::TastIdent(name.to_string());
        if let Some(struct_def) = genv.structs().get(&uident) {
            for (field_name, field_ty) in &struct_def.fields {
                items.push(DotCompletionItem {
                    name: field_name.0.clone(),
                    kind: DotCompletionKind::Field,
                    detail: Some(field_ty.to_pretty(80)),
                });
            }
        }
    }

    let mut methods = inherent_methods_for_receiver(genv, ty)
        .into_iter()
        .collect::<BTreeMap<_, _>>();
    for (method_name, method_ty) in trait_methods_for_receiver(genv, ty) {
        methods.entry(method_name).or_insert(method_ty);
    }
    items.extend(
        methods
            .into_iter()
            .map(|(method_name, method_ty)| DotCompletionItem {
                name: method_name,
                kind: DotCompletionKind::Method,
                detail: Some(method_ty),
            }),
    );

    items
}

fn type_constructor_name(ty: &tast::Ty) -> Option<&str> {
    match ty {
        tast::Ty::TEnum { name } | tast::Ty::TStruct { name } => Some(name.as_str()),
        tast::Ty::TApp { ty, .. } => type_constructor_name(ty),
        tast::Ty::TRef { elem } => type_constructor_name(elem),
        _ => None,
    }
}

fn filter_dot_items(items: Vec<DotCompletionItem>, prefix: &str) -> Vec<DotCompletionItem> {
    if prefix.is_empty() {
        return items;
    }

    items
        .into_iter()
        .filter(|item| item.name.starts_with(prefix))
        .collect()
}

fn import_root_completions(
    path: &Path,
    src: &str,
    offset: TextSize,
    prefix_start: TextSize,
    prefix: &str,
) -> Option<Vec<ValueCompletionItem>> {
    if !is_import_root_completion_context(path, src, offset, prefix_start) {
        return None;
    }

    let mut names = BTreeSet::new();
    names.insert(crate::package_names::STD_PACKAGE.to_string());

    if let Ok((root_dir, dependencies)) =
        crate::pipeline::packages::discover_dependency_versions_from_file(path)
    {
        collect_local_package_names(&root_dir, path, &mut names);
        for dep in dependencies.keys() {
            if let Ok(coord) = ModuleCoord::parse(dep) {
                names.insert(format!("{}::{}", coord.owner, coord.module));
            }
        }
    } else {
        let root_dir = path
            .parent()
            .filter(|parent| !parent.as_os_str().is_empty())
            .unwrap_or_else(|| Path::new("."));
        collect_local_package_names(root_dir, path, &mut names);
    }

    Some(
        names
            .into_iter()
            .filter(|name| name.starts_with(prefix))
            .map(|name| ValueCompletionItem {
                name,
                kind: ValueCompletionKind::Package,
                detail: Some("package".to_string()),
            })
            .collect(),
    )
}

fn visible_import_namespace_items(path: &Path, src: &str) -> Vec<RankedValueItem> {
    visible_import_namespace_names(path, src)
        .into_iter()
        .map(|name| RankedValueItem {
            item: ValueCompletionItem {
                name,
                kind: ValueCompletionKind::Package,
                detail: Some("package".to_string()),
            },
            ty_text: None,
            kind_rank: 2,
            scope_rank: usize::MAX - 2,
        })
        .collect()
}

fn visible_import_namespace_names(path: &Path, src: &str) -> BTreeSet<String> {
    let result = parser::parse(path, src);
    let root = MySyntaxNode::new_root(result.green_node);
    let Some(file) = cst::cst::File::cast(root) else {
        return BTreeSet::new();
    };

    let symbols = build_symbol_lookup(path, src);
    file.use_decls()
        .filter_map(|use_decl| {
            let path = use_decl.path()?;
            let segments = path
                .ident_tokens()
                .map(|token| token.to_string())
                .collect::<Vec<_>>();
            if segments.is_empty() {
                return None;
            }
            let target = segments.join("::");
            if !symbols
                .graph
                .as_ref()
                .is_some_and(|graph| graph.package_dirs.contains_key(&target))
            {
                return None;
            }
            use_decl
                .alias_token()
                .map(|token| token.to_string())
                .or_else(|| Some(symbols.default_alias_for_package(&target)))
        })
        .collect()
}

fn import_namespace_aliases(path: &Path, src: &str) -> BTreeMap<String, String> {
    let result = parser::parse(path, src);
    let root = MySyntaxNode::new_root(result.green_node);
    let Some(file) = cst::cst::File::cast(root) else {
        return BTreeMap::new();
    };

    let symbols = build_symbol_lookup(path, src);
    let mut aliases = BTreeMap::new();
    for use_decl in file.use_decls() {
        let Some(path) = use_decl.path() else {
            continue;
        };
        let segments = path
            .ident_tokens()
            .map(|token| token.to_string())
            .collect::<Vec<_>>();
        let target = segments.join("::");
        if !symbols
            .graph
            .as_ref()
            .is_some_and(|graph| graph.package_dirs.contains_key(&target))
        {
            continue;
        }
        let Some(alias) = use_decl
            .alias_token()
            .map(|token| token.to_string())
            .or_else(|| Some(symbols.default_alias_for_package(&target)))
        else {
            continue;
        };
        if !target.is_empty() {
            aliases.insert(alias, target);
        }
    }
    aliases
}

fn resolve_completion_namespace(path: &Path, src: &str, namespace: &str) -> String {
    let segments = namespace
        .split("::")
        .filter(|segment| !segment.is_empty())
        .map(|segment| segment.to_string())
        .collect::<Vec<_>>();
    let Some(first) = segments.first() else {
        return namespace.to_string();
    };

    let aliases = import_namespace_aliases(path, src);
    let Some(target) = aliases.get(first) else {
        return namespace.to_string();
    };
    if segments.len() == 1 {
        return target.clone();
    }
    let mut resolved = target.clone();
    for segment in &segments[1..] {
        resolved.push_str("::");
        resolved.push_str(segment);
    }
    resolved
}

fn visible_local_items(
    root: &MySyntaxNode,
    offset: TextSize,
    index: &HirResultsIndex,
    closure_params: &ClosureParamIndex,
    results: &crate::typer::results::TypeckResults,
) -> Vec<RankedValueItem> {
    let token = token_at_offset_for_query(root, offset).or_else(|| {
        offset
            .checked_sub(TextSize::from(1))
            .and_then(|prev| token_at_offset_for_query(root, prev))
    });
    let Some(token) = token else {
        return Vec::new();
    };
    let Some(parent) = token.parent() else {
        return Vec::new();
    };

    let path = parent.ancestors().collect::<Vec<_>>();
    let mut items = Vec::new();
    let mut seen = HashSet::new();

    for (depth, node) in path.iter().enumerate() {
        if let Some(function) = Fn::cast(node.clone()) {
            add_fn_params(&function, depth, index, results, &mut seen, &mut items);
        }
        if let Some(closure) = ClosureExpr::cast(node.clone()) {
            add_closure_params(
                &closure,
                depth,
                closure_params,
                results,
                &mut seen,
                &mut items,
            );
        }
        if depth == 0 {
            continue;
        }
        let child = &path[depth - 1];
        if let Some(block) = Block::cast(node.clone()) {
            add_block_locals(&block, child, depth, index, results, &mut seen, &mut items);
        }
        if let Some(match_arm) = MatchArm::cast(node.clone()) {
            add_match_arm_bindings(
                &match_arm, child, depth, index, results, &mut seen, &mut items,
            );
        }
    }

    items
}

fn add_fn_params(
    function: &Fn,
    depth: usize,
    index: &HirResultsIndex,
    results: &crate::typer::results::TypeckResults,
    seen: &mut HashSet<String>,
    items: &mut Vec<RankedValueItem>,
) {
    let Some(params) = function.param_list() else {
        return;
    };
    for param in params.params() {
        let Some(name) = param.lident().map(|ident| ident.to_string()) else {
            continue;
        };
        if !seen.insert(name.clone()) {
            continue;
        }
        let ptr = MySyntaxNodePtr::new(param.syntax());
        let ty_text = index
            .local_id(&ptr)
            .and_then(|local_id| results.local_ty(local_id))
            .map(|ty| ty.to_pretty(80))
            .or_else(|| param.ty().map(|ty| ty.to_string()));
        items.push(RankedValueItem {
            item: ValueCompletionItem {
                name,
                kind: ValueCompletionKind::Variable,
                detail: ty_text.clone(),
            },
            ty_text,
            kind_rank: 0,
            scope_rank: depth,
        });
    }
}

fn add_closure_params(
    closure: &ClosureExpr,
    depth: usize,
    closure_params: &ClosureParamIndex,
    results: &crate::typer::results::TypeckResults,
    seen: &mut HashSet<String>,
    items: &mut Vec<RankedValueItem>,
) {
    let Some(params) = closure.params() else {
        return;
    };
    for param in params.params() {
        let Some(name) = param.lident().map(|ident| ident.to_string()) else {
            continue;
        };
        if !seen.insert(name.clone()) {
            continue;
        }
        let ptr = MySyntaxNodePtr::new(param.syntax());
        let ty_text = param.ty().map(|ty| ty.to_string()).or_else(|| {
            closure_params
                .local_id(&ptr)
                .and_then(|local_id| results.local_ty(local_id))
                .map(|ty| ty.to_pretty(80))
        });
        items.push(RankedValueItem {
            item: ValueCompletionItem {
                name,
                kind: ValueCompletionKind::Variable,
                detail: ty_text.clone(),
            },
            ty_text,
            kind_rank: 0,
            scope_rank: depth,
        });
    }
}

fn add_block_locals(
    block: &Block,
    child: &parser::syntax::MySyntaxNode,
    depth: usize,
    index: &HirResultsIndex,
    results: &crate::typer::results::TypeckResults,
    seen: &mut HashSet<String>,
    items: &mut Vec<RankedValueItem>,
) {
    let child_start = child.text_range().start();
    for stmt in block.stmts() {
        if stmt.syntax().text_range().end() > child_start {
            break;
        }
        if let cst::nodes::Stmt::LetStmt(let_stmt) = stmt
            && let Some(pattern) = let_stmt.pattern()
        {
            add_pattern_bindings(&pattern, depth, index, results, seen, items);
        }
    }
}

fn add_match_arm_bindings(
    match_arm: &MatchArm,
    child: &parser::syntax::MySyntaxNode,
    depth: usize,
    index: &HirResultsIndex,
    results: &crate::typer::results::TypeckResults,
    seen: &mut HashSet<String>,
    items: &mut Vec<RankedValueItem>,
) {
    if match_arm
        .expr()
        .is_some_and(|expr| expr.syntax().text_range() == child.text_range())
        && let Some(pattern) = match_arm.pattern()
    {
        add_pattern_bindings(&pattern, depth, index, results, seen, items);
    }
}

fn add_pattern_bindings(
    pattern: &Pattern,
    depth: usize,
    index: &HirResultsIndex,
    results: &crate::typer::results::TypeckResults,
    seen: &mut HashSet<String>,
    items: &mut Vec<RankedValueItem>,
) {
    let mut names = HashMap::new();
    for node in pattern.syntax().descendants() {
        let Some(var_pat) = cst::nodes::VarPat::cast(node) else {
            continue;
        };
        let Some(name) = var_pat.lident().map(|ident| ident.to_string()) else {
            continue;
        };
        names.entry(name).or_insert(var_pat);
    }

    for (name, var_pat) in names {
        if !seen.insert(name.clone()) {
            continue;
        }
        let ptr = MySyntaxNodePtr::new(var_pat.syntax());
        let ty_text = index
            .local_id(&ptr)
            .and_then(|local_id| results.local_ty(local_id))
            .map(|ty| ty.to_pretty(80))
            .or_else(|| {
                index
                    .pat_id(&ptr)
                    .and_then(|pat_id| results.pat_ty(pat_id))
                    .map(|ty| ty.to_pretty(80))
            });
        items.push(RankedValueItem {
            item: ValueCompletionItem {
                name,
                kind: ValueCompletionKind::Variable,
                detail: ty_text.clone(),
            },
            ty_text,
            kind_rank: 0,
            scope_rank: depth,
        });
    }
}

fn sort_value_items(items: &mut [RankedValueItem], expected_ty: Option<&str>) {
    items.sort_by(|a, b| {
        value_item_match_rank(a, expected_ty)
            .cmp(&value_item_match_rank(b, expected_ty))
            .then(a.kind_rank.cmp(&b.kind_rank))
            .then(a.scope_rank.cmp(&b.scope_rank))
            .then(a.item.name.cmp(&b.item.name))
    });
}

fn value_item_match_rank(item: &RankedValueItem, expected_ty: Option<&str>) -> u8 {
    match expected_ty {
        Some(expected_ty) if item.ty_text.as_deref() == Some(expected_ty) => 0,
        Some(_) => 1,
        None => 0,
    }
}

fn is_import_root_completion_context(
    path: &Path,
    src: &str,
    offset: TextSize,
    prefix_start: TextSize,
) -> bool {
    let result = parser::parse(path, src);
    let root = MySyntaxNode::new_root(result.green_node);

    let token = token_at_offset_for_query(&root, offset).or_else(|| {
        offset
            .checked_sub(TextSize::from(1))
            .and_then(|prev| token_at_offset_for_query(&root, prev))
    });
    if let Some(token) = token
        && let Some(use_decl) = use_decl_from_token(&token)
    {
        let start = u32::from(use_decl.syntax().text_range().start()) as usize;
        let end = u32::from(prefix_start) as usize;
        if start <= end && end <= src.len() {
            let leading = &src[start..end];
            return starts_with_use_keyword(leading.trim_start()) && !leading.contains("::");
        }
    }

    let offset = u32::from(offset) as usize;
    let line_prefix = src[..offset.min(src.len())]
        .rsplit('\n')
        .next()
        .unwrap_or_default();
    let trimmed = line_prefix.trim_start();
    starts_with_use_keyword(trimmed) && !trimmed.contains("::")
}

fn collect_local_package_names(root_dir: &Path, source_path: &Path, names: &mut BTreeSet<String>) {
    let Ok(manifest) = crate::config::load_module_manifest(&root_dir.join("goml.toml")) else {
        return;
    };
    let artifact_dir = root_dir.join(&manifest.build.target_dir);
    let current_dir = source_path.parent().unwrap_or(root_dir);
    collect_local_package_names_inner(
        root_dir,
        root_dir,
        current_dir,
        &artifact_dir,
        &manifest.module.path,
        names,
    );
}

fn collect_local_package_names_inner(
    root_dir: &Path,
    dir: &Path,
    current_dir: &Path,
    artifact_dir: &Path,
    module_path: &str,
    names: &mut BTreeSet<String>,
) {
    if dir != root_dir && dir.join("goml.toml").is_file() {
        return;
    }
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    let mut children = Vec::new();
    let mut has_source = false;
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_file() && path.extension().is_some_and(|ext| ext == "gom") {
            has_source = true;
        } else if path.is_dir()
            && !entry.file_name().to_string_lossy().starts_with('.')
            && path != artifact_dir
        {
            children.push(path);
        }
    }
    if has_source && dir != current_dir {
        let relative = dir.strip_prefix(root_dir).unwrap_or(dir);
        let suffix = relative
            .components()
            .filter_map(|component| component.as_os_str().to_str())
            .collect::<Vec<_>>()
            .join("::");
        if suffix.is_empty() {
            names.insert(module_path.to_string());
        } else {
            names.insert(format!("{module_path}::{suffix}"));
        }
    }
    children.sort();
    for child in children {
        collect_local_package_names_inner(
            root_dir,
            &child,
            current_dir,
            artifact_dir,
            module_path,
            names,
        );
    }
}

fn starts_with_use_keyword(text: &str) -> bool {
    text == "use" || text.starts_with("use ")
}

fn import_colon_colon_items_for_namespace(
    path: &Path,
    namespace: &str,
) -> Vec<ColonColonCompletionItem> {
    let mut packages = BTreeSet::from([
        "std::env".to_string(),
        "std::fs".to_string(),
        "std::io".to_string(),
        "std::process".to_string(),
    ]);
    if let Ok((root_dir, dependencies)) =
        crate::pipeline::packages::discover_dependency_versions_from_file(path)
    {
        collect_local_package_names(&root_dir, path, &mut packages);
        packages.extend(dependencies.keys().cloned());
        if let Ok(external_deps) = crate::external::resolve_dependency_versions(&dependencies) {
            packages.extend(external_deps.package_names());
        }
    }
    let prefix = format!("{namespace}::");
    packages
        .into_iter()
        .filter_map(|package| {
            let rest = package.strip_prefix(&prefix)?;
            rest.split("::").next().map(str::to_string)
        })
        .collect::<BTreeSet<_>>()
        .into_iter()
        .map(|name| ColonColonCompletionItem {
            name,
            kind: ColonColonCompletionKind::Package,
            detail: Some("package".to_string()),
        })
        .collect()
}

fn colon_colon_items_for_namespace(
    genv: Option<&GlobalTypeEnv>,
    namespace: &str,
) -> Vec<ColonColonCompletionItem> {
    let mut items = Vec::new();

    let Some(genv) = genv else {
        return items;
    };

    if let Some(enum_def) = genv.enums().get(&tast::TastIdent(namespace.to_string())) {
        for variant in &enum_def.variants {
            let payload = variant.fields.types();
            let detail = match &variant.fields {
                crate::env::EnumVariantFields::Unit => Some(namespace.to_string()),
                crate::env::EnumVariantFields::Tuple(_) => {
                    let payload_str = payload
                        .iter()
                        .map(|ty| ty.to_pretty(80))
                        .collect::<Vec<_>>()
                        .join(", ");
                    Some(format!("({}) -> {}", payload_str, namespace))
                }
                crate::env::EnumVariantFields::Struct(fields) => {
                    let payload_str = fields
                        .iter()
                        .map(|(name, ty)| format!("{}: {}", name.0, ty.to_pretty(80)))
                        .collect::<Vec<_>>()
                        .join(", ");
                    Some(format!("{{ {} }} -> {}", payload_str, namespace))
                }
            };
            items.push(ColonColonCompletionItem {
                name: variant.name.0.clone(),
                kind: ColonColonCompletionKind::Variant,
                detail,
            });
        }
        items.extend(colon_colon_inherent_methods(
            genv,
            tast::Ty::TEnum {
                name: namespace.to_string(),
            },
        ));
        return items;
    }

    if genv.trait_env.trait_defs.contains_key(namespace) {
        if let Some(trait_def) = genv.trait_env.trait_defs.get(namespace) {
            for (method_name, scheme) in trait_def.methods.iter() {
                items.push(ColonColonCompletionItem {
                    name: method_name.clone(),
                    kind: ColonColonCompletionKind::Method,
                    detail: Some(scheme.ty.to_pretty(80)),
                });
            }
        }
        return items;
    }

    if genv
        .structs()
        .contains_key(&tast::TastIdent(namespace.to_string()))
    {
        items.extend(colon_colon_inherent_methods(
            genv,
            tast::Ty::TStruct {
                name: namespace.to_string(),
            },
        ));
        return items;
    }

    if genv
        .trait_env
        .inherent_impls
        .contains_key(&crate::env::InherentImplKey::Constr(namespace.to_string()))
    {
        items.extend(colon_colon_inherent_methods(
            genv,
            tast::Ty::TStruct {
                name: namespace.to_string(),
            },
        ));
        return items;
    }

    let ns_prefix = format!("{}::", namespace);

    for name in genv.type_env.enums.keys() {
        if let Some(member) = strip_namespace_member(&name.0, &ns_prefix) {
            items.push(ColonColonCompletionItem {
                name: member.to_string(),
                kind: ColonColonCompletionKind::Type,
                detail: Some("enum".to_string()),
            });
        }
    }

    for name in genv.type_env.structs.keys() {
        if let Some(member) = strip_namespace_member(&name.0, &ns_prefix) {
            items.push(ColonColonCompletionItem {
                name: member.to_string(),
                kind: ColonColonCompletionKind::Type,
                detail: Some("struct".to_string()),
            });
        }
    }

    for name in genv.trait_env.trait_defs.keys() {
        if let Some(member) = strip_namespace_member(name, &ns_prefix) {
            items.push(ColonColonCompletionItem {
                name: member.to_string(),
                kind: ColonColonCompletionKind::Trait,
                detail: None,
            });
        }
    }

    for name in genv.value_env.funcs.keys() {
        if let Some(member) = strip_namespace_member(name, &ns_prefix) {
            items.push(ColonColonCompletionItem {
                name: member.to_string(),
                kind: ColonColonCompletionKind::Value,
                detail: Some("fn".to_string()),
            });
        }
    }

    items
}

fn strip_namespace_member<'a>(full: &'a str, ns_prefix: &str) -> Option<&'a str> {
    if !full.starts_with(ns_prefix) {
        return None;
    }
    let rest = &full[ns_prefix.len()..];
    if rest.is_empty() || rest.contains("::") {
        None
    } else {
        Some(rest)
    }
}

fn colon_colon_inherent_methods(
    genv: &GlobalTypeEnv,
    receiver_ty: tast::Ty,
) -> Vec<ColonColonCompletionItem> {
    inherent_methods_for_receiver(genv, &receiver_ty)
        .into_iter()
        .map(|(method_name, method_ty)| ColonColonCompletionItem {
            name: method_name,
            kind: ColonColonCompletionKind::Method,
            detail: Some(method_ty),
        })
        .collect()
}

fn inherent_methods_for_receiver(
    genv: &GlobalTypeEnv,
    receiver_ty: &tast::Ty,
) -> Vec<(String, String)> {
    let mut methods = BTreeMap::new();

    if let Some(impl_def) = genv
        .trait_env
        .inherent_impls
        .get(&crate::env::InherentImplKey::Exact(receiver_ty.clone()))
    {
        for (method_name, method_scheme) in impl_def.methods.iter() {
            methods.insert(method_name.clone(), method_scheme.ty.to_pretty(80));
        }
    }

    if let Some(constr_name) = completion_constructor_name(receiver_ty)
        && let Some(impl_def) = genv
            .trait_env
            .inherent_impls
            .get(&crate::env::InherentImplKey::Constr(constr_name))
    {
        for (method_name, method_scheme) in impl_def.methods.iter() {
            methods
                .entry(method_name.clone())
                .or_insert_with(|| method_scheme.ty.to_pretty(80));
        }
    }

    methods.into_iter().collect()
}

fn trait_methods_for_receiver(
    genv: &GlobalTypeEnv,
    receiver_ty: &tast::Ty,
) -> Vec<(String, String)> {
    let mut methods = BTreeMap::new();
    let package_env = crate::env::PackageTypeEnv::new(
        "completion".to_string(),
        GlobalTypeEnv::default(),
        genv.clone(),
        Default::default(),
    );
    let param_env = crate::typer::ParamEnv::default();
    let mut trait_solver = crate::typer::traits::solver::TraitSolver::new(&package_env, &param_env);
    for (key, impl_def) in &genv.trait_env.trait_impls {
        if !impl_def.valid {
            continue;
        }
        let Some(substitution) = crate::typer::impl_self_subst(&key.for_ty, receiver_ty) else {
            continue;
        };
        let trait_ref = crate::typer::type_ops::substitute_trait_ref(&key.trait_ref, &substitution);
        if !crate::typer::type_ops::trait_ref_contains_tparam(&trait_ref)
            && !matches!(
                trait_solver.select_ground(crate::typer::TraitGoal {
                    trait_ref: trait_ref.clone(),
                    for_ty: receiver_ty.clone(),
                }),
                crate::typer::traits::solver::SelectionResult::Unique(_)
            )
        {
            continue;
        }
        let Some(trait_def) = genv.trait_env.trait_defs.get(&trait_ref.name.0) else {
            continue;
        };
        for method_name in trait_def.methods.keys() {
            let method_name_ident = tast::TastIdent(method_name.clone());
            let Some(scheme) = genv.lookup_trait_method_scheme(&trait_ref, &method_name_ident)
            else {
                continue;
            };
            let method_ty = crate::typer::type_ops::instantiate_self_ty(&scheme.ty, receiver_ty);
            methods
                .entry(method_name.clone())
                .or_insert_with(|| method_ty.to_pretty(80));
        }
    }
    methods.into_iter().collect()
}

fn completion_constructor_name(ty: &tast::Ty) -> Option<String> {
    match ty {
        tast::Ty::TEnum { name } | tast::Ty::TStruct { name } => Some(name.clone()),
        tast::Ty::TApp { .. }
        | tast::Ty::TSlice { .. }
        | tast::Ty::TVec { .. }
        | tast::Ty::TRef { .. }
        | tast::Ty::THashMap { .. } => Some(ty.get_constr_name_unsafe()),
        _ => None,
    }
}
