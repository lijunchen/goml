use std::{
    collections::{BTreeMap, BTreeSet, HashSet},
    fs,
    path::Path,
};

use cst::cst::CstNode;
use cst::nodes::BinaryExpr;
use parser::syntax::{MySyntaxKind, MySyntaxNode, MySyntaxNodePtr};
use text_size::TextSize;

use crate::{config::GomlConfig, env::GlobalTypeEnv, registry::ModuleCoord, tast};

use super::{
    ColonColonCompletionItem, ColonColonCompletionKind, DotCompletionItem, DotCompletionKind,
    ValueCompletionItem, ValueCompletionKind,
    hir_index::HirResultsIndex,
    symbol_index::{ProjectSymbolIndex, build_symbol_index},
    syntax::{
        ancestor_path_from_token, ident_prefix_at_offset, token_at_offset_for_query,
        use_decl_from_token,
    },
    typecheck::typecheck_for_query,
};

const COMPLETION_PLACEHOLDER: &str = "completion_placeholder";
const VALUE_COMPLETION_KEYWORDS: &[&str] = &[
    "array", "bool", "char", "dyn", "else", "enum", "extern", "false", "float32", "float64", "fn",
    "for", "go", "if", "impl", "import", "in", "int8", "int16", "int32", "int64", "let", "match",
    "package", "return", "string", "struct", "trait", "true", "type", "uint8", "uint16", "uint32",
    "uint64", "unit", "use", "while", "_",
];

pub fn dot_completions(
    path: &Path,
    src: &str,
    line: u32,
    col: u32,
) -> Option<Vec<DotCompletionItem>> {
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

    let (hir_table, results, genv, _diagnostics) = typecheck_for_query(path, &parse_src).ok()?;
    let index = HirResultsIndex::new(&hir_table);
    let expr_id = index.expr_id(&lhs_ptr)?;
    let ty = normalize_completion_ty(results.expr_ty(expr_id)?.clone());
    let items = completions_for_type(&genv, &ty);
    Some(filter_dot_items(items, &prefix))
}

pub fn value_completions(
    path: &Path,
    src: &str,
    line: u32,
    col: u32,
) -> Option<Vec<ValueCompletionItem>> {
    let line_index = line_index::LineIndex::new(src);
    let offset = line_index.offset(line_index::LineCol { line, col })?;
    let (prefix_start, prefix) = ident_prefix_at_offset(src, offset)?;
    if let Some(items) = use_root_completions(path, src, offset, prefix_start, &prefix) {
        return Some(items);
    }
    if prefix.is_empty() {
        return Some(Vec::new());
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

    let mut items = Vec::new();
    if let Ok((_hir_table, _results, genv, _diagnostics)) = typecheck_for_query(path, src) {
        items.extend(
            genv.value_env
                .funcs
                .iter()
                .filter(|(name, _)| !name.contains("::") && name.starts_with(&prefix))
                .map(|(name, scheme)| ValueCompletionItem {
                    name: name.clone(),
                    kind: ValueCompletionKind::Function,
                    detail: Some(scheme.ty.to_pretty(80)),
                }),
        );
    }
    items.extend(visible_use_namespace_items(path, src, &prefix));

    let mut seen = items
        .iter()
        .map(|item| item.name.clone())
        .collect::<HashSet<_>>();
    for keyword in VALUE_COMPLETION_KEYWORDS {
        if !keyword.starts_with(&prefix) {
            continue;
        }
        if !seen.insert((*keyword).to_string()) {
            continue;
        }
        items.push(ValueCompletionItem {
            name: (*keyword).to_string(),
            kind: ValueCompletionKind::Keyword,
            detail: None,
        });
    }

    items.sort_by(|a, b| a.name.cmp(&b.name));
    items.truncate(50);
    Some(items)
}

pub fn colon_colon_completions(
    path: &Path,
    src: &str,
    line: u32,
    col: u32,
) -> Option<Vec<ColonColonCompletionItem>> {
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

    let genv = typecheck_for_query(path, &parse_src)
        .ok()
        .map(|(_hir_table, _results, genv, _diagnostics)| genv);
    let symbol_index = build_symbol_index(path, &parse_src)
        .ok()
        .map(|(_graph, index)| index);

    let mut items = if use_decl_from_token(&token).is_some() {
        use_colon_colon_items_for_namespace(path, genv.as_ref(), symbol_index.as_ref(), &namespace)
    } else {
        colon_colon_items_for_namespace(genv.as_ref(), symbol_index.as_ref(), &namespace)
    };
    items.sort_by(|a, b| a.name.cmp(&b.name));
    items.dedup_by(|a, b| a.name == b.name && a.kind == b.kind && a.detail == b.detail);
    items.retain(|item| item.name.starts_with(&prefix));
    Some(items)
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

    items.extend(inherent_methods_for_receiver(genv, ty).into_iter().map(
        |(method_name, method_ty)| DotCompletionItem {
            name: method_name,
            kind: DotCompletionKind::Method,
            detail: Some(method_ty),
        },
    ));

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

fn use_root_completions(
    path: &Path,
    src: &str,
    offset: TextSize,
    prefix_start: TextSize,
    prefix: &str,
) -> Option<Vec<ValueCompletionItem>> {
    if !is_use_root_completion_context(path, src, offset, prefix_start) {
        return None;
    }

    let mut names = BTreeSet::new();
    let current_package = current_package_name(path, src);

    if let Ok((module_dir, config)) = crate::pipeline::packages::discover_project_from_file(path) {
        collect_local_package_names(&module_dir, current_package.as_deref(), &mut names);
        for dep in config.dependencies.keys() {
            if let Ok(coord) = ModuleCoord::parse(dep) {
                names.insert(format!("{}::{}", coord.owner, coord.module));
            }
        }
    } else {
        let root_dir = path
            .parent()
            .filter(|parent| !parent.as_os_str().is_empty())
            .unwrap_or_else(|| Path::new("."));
        collect_local_package_names(root_dir, current_package.as_deref(), &mut names);
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

fn visible_use_namespace_items(path: &Path, src: &str, prefix: &str) -> Vec<ValueCompletionItem> {
    visible_use_namespace_names(path, src)
        .into_iter()
        .filter(|name| name.starts_with(prefix))
        .map(|name| ValueCompletionItem {
            name,
            kind: ValueCompletionKind::Package,
            detail: Some("package".to_string()),
        })
        .collect()
}

fn visible_use_namespace_names(path: &Path, src: &str) -> BTreeSet<String> {
    let result = parser::parse(path, src);
    let root = MySyntaxNode::new_root(result.green_node);
    let Some(file) = cst::cst::File::cast(root) else {
        return BTreeSet::new();
    };

    let external_import_paths = crate::pipeline::packages::discover_project_from_file(path)
        .ok()
        .and_then(|(module_dir, config)| {
            crate::external::resolve_project_dependencies(&module_dir, &config).ok()
        })
        .map(|external_deps| external_deps.import_paths())
        .unwrap_or_default();

    file.use_decls()
        .filter_map(|use_decl| use_decl.path())
        .filter_map(|path| {
            let segments = path
                .ident_tokens()
                .map(|token| token.to_string())
                .collect::<Vec<_>>();
            if segments.is_empty() {
                return None;
            }

            let full_path = segments.join("::");
            if let Some(logical_name) = external_import_paths.get(&full_path) {
                return Some(logical_name.clone());
            }

            if segments.len() == 1 {
                return segments.into_iter().next();
            }

            None
        })
        .collect()
}

fn is_use_root_completion_context(
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

fn current_package_name(path: &Path, src: &str) -> Option<String> {
    let result = parser::parse(path, src);
    let root = MySyntaxNode::new_root(result.green_node);
    let file = cst::cst::File::cast(root)?;
    file.package_decl()
        .and_then(|decl| decl.name_token())
        .map(|token| token.to_string())
}

fn collect_local_package_names(
    root_dir: &Path,
    current_package: Option<&str>,
    names: &mut BTreeSet<String>,
) {
    let Ok(entries) = fs::read_dir(root_dir) else {
        return;
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }
        let Some(name) = local_package_name(&path) else {
            continue;
        };
        if current_package.is_some_and(|current| current == name) {
            continue;
        }
        names.insert(name);
    }
}

fn local_package_name(dir: &Path) -> Option<String> {
    if let Some(config) = GomlConfig::find_package_config(dir) {
        return Some(config.package.name);
    }

    let entries = fs::read_dir(dir).ok()?;
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().is_some_and(|ext| ext == "gom") {
            return dir
                .file_name()
                .and_then(|name| name.to_str())
                .map(|name| name.to_string());
        }
    }

    None
}

fn starts_with_use_keyword(text: &str) -> bool {
    text == "use" || text.starts_with("use ")
}

fn use_colon_colon_items_for_namespace(
    path: &Path,
    genv: Option<&GlobalTypeEnv>,
    symbol_index: Option<&ProjectSymbolIndex>,
    namespace: &str,
) -> Vec<ColonColonCompletionItem> {
    let mut items = Vec::new();

    if let Ok((module_dir, config)) = crate::pipeline::packages::discover_project_from_file(path) {
        let mut module_children = BTreeSet::new();
        for dep in config.dependencies.keys() {
            if let Ok(coord) = ModuleCoord::parse(dep)
                && namespace == coord.owner
            {
                module_children.insert(coord.module);
            }
        }
        items.extend(
            module_children
                .into_iter()
                .map(|name| ColonColonCompletionItem {
                    name,
                    kind: ColonColonCompletionKind::Package,
                    detail: Some("package".to_string()),
                }),
        );

        if let Ok(external_deps) =
            crate::external::resolve_project_dependencies(&module_dir, &config)
        {
            let import_paths = external_deps.import_paths();
            let prefix = format!("{namespace}::");
            let mut child_packages = BTreeSet::new();
            for import_path in import_paths.keys() {
                let Some(rest) = import_path.strip_prefix(&prefix) else {
                    continue;
                };
                let Some(child) = rest.split("::").next() else {
                    continue;
                };
                if !child.is_empty() {
                    child_packages.insert(child.to_string());
                }
            }
            items.extend(
                child_packages
                    .into_iter()
                    .map(|name| ColonColonCompletionItem {
                        name,
                        kind: ColonColonCompletionKind::Package,
                        detail: Some("package".to_string()),
                    }),
            );

            if let Some(alias) = import_paths.get(namespace) {
                items.extend(colon_colon_items_for_namespace(genv, symbol_index, alias));
            }
        }
    }

    if items.is_empty() {
        return colon_colon_items_for_namespace(genv, symbol_index, namespace);
    }

    items
}

fn colon_colon_items_for_namespace(
    genv: Option<&GlobalTypeEnv>,
    symbol_index: Option<&ProjectSymbolIndex>,
    namespace: &str,
) -> Vec<ColonColonCompletionItem> {
    let mut items = Vec::new();

    if let Some(symbol_index) = symbol_index {
        items.extend(
            symbol_index
                .package_children(namespace)
                .into_iter()
                .map(|name| ColonColonCompletionItem {
                    name,
                    kind: ColonColonCompletionKind::Package,
                    detail: Some("package".to_string()),
                }),
        );
    }

    let Some(genv) = genv else {
        return items;
    };

    if let Some(enum_def) = genv.enums().get(&tast::TastIdent(namespace.to_string())) {
        for (variant_name, payload) in &enum_def.variants {
            let detail = if payload.is_empty() {
                Some(namespace.to_string())
            } else {
                let payload_str = payload
                    .iter()
                    .map(|ty| ty.to_pretty(80))
                    .collect::<Vec<_>>()
                    .join(", ");
                Some(format!("({}) -> {}", payload_str, namespace))
            };
            items.push(ColonColonCompletionItem {
                name: variant_name.0.clone(),
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
