use std::{
    collections::{BTreeMap, HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
};

use cst::cst::CstNode;
use cst::nodes::{ArgList, BinaryExpr, CallExpr};
use parser::syntax::{MySyntaxKind, MySyntaxNode, MySyntaxNodePtr, MySyntaxToken};
use text_size::{TextRange, TextSize};

use crate::{artifact::PackageExports, builtins, env::GlobalTypeEnv, hir, pipeline, tast};

const COMPLETION_PLACEHOLDER: &str = "completion_placeholder";

#[derive(Debug, Clone)]
struct HirResultsIndex {
    expr_by_ptr: HashMap<MySyntaxNodePtr, hir::ExprId>,
    pat_by_ptr: HashMap<MySyntaxNodePtr, hir::PatId>,
    local_by_ptr: HashMap<MySyntaxNodePtr, hir::LocalId>,
}

impl HirResultsIndex {
    fn new(hir_table: &hir::HirTable) -> Self {
        let mut expr_by_ptr = HashMap::new();
        let mut pat_by_ptr = HashMap::new();
        let mut local_by_ptr = HashMap::new();

        for idx in 0..hir_table.expr_count() {
            let expr_id = hir::ExprId {
                pkg: hir_table.package(),
                idx: idx as u32,
            };
            if let Some(ptr) = hir_table.expr_ptr(expr_id) {
                expr_by_ptr.insert(ptr, expr_id);
            }
        }

        for idx in 0..hir_table.pat_count() {
            let pat_id = hir::PatId {
                pkg: hir_table.package(),
                idx: idx as u32,
            };
            if let Some(ptr) = hir_table.pat_ptr(pat_id) {
                pat_by_ptr.insert(ptr, pat_id);
            }
        }

        for (local_id, _info) in hir_table.iter_locals() {
            if let Some(ptr) = hir_table.local_origin_ptr(local_id) {
                local_by_ptr.insert(ptr, local_id);
            }
        }

        Self {
            expr_by_ptr,
            pat_by_ptr,
            local_by_ptr,
        }
    }

    fn expr_id(&self, ptr: &MySyntaxNodePtr) -> Option<hir::ExprId> {
        self.expr_by_ptr.get(ptr).copied()
    }

    fn pat_id(&self, ptr: &MySyntaxNodePtr) -> Option<hir::PatId> {
        self.pat_by_ptr.get(ptr).copied()
    }

    fn local_id(&self, ptr: &MySyntaxNodePtr) -> Option<hir::LocalId> {
        self.local_by_ptr.get(ptr).copied()
    }
}

fn typecheck_single_file_for_query(
    path: &Path,
    src: &str,
) -> Result<
    (
        hir::HirTable,
        crate::typer::results::TypeckResults,
        GlobalTypeEnv,
        diagnostics::Diagnostics,
    ),
    String,
> {
    let result = parser::parse(path, src);
    let (green_node, mut parse_diagnostics) = result.into_parts();
    let root = MySyntaxNode::new_root(green_node);
    let cst = cst::cst::File::cast(root).ok_or_else(|| "failed to cast CST".to_string())?;

    let lower = ::ast::lower::lower(cst);
    let (ast, mut lower_diagnostics) = lower.into_parts();
    parse_diagnostics.append(&mut lower_diagnostics);
    let ast = ast.ok_or_else(|| "AST lowering error".to_string())?;

    if !ast.imports.is_empty() || !ast.use_traits.is_empty() {
        return Err("package uses are not supported in this context".to_string());
    }

    let original_ast = ast.clone();
    let ast = match crate::derive::expand(ast) {
        Ok(ast) => ast,
        Err(mut derive_diagnostics) => {
            parse_diagnostics.append(&mut derive_diagnostics);
            original_ast
        }
    };

    let (hir, hir_table, mut hir_diagnostics) = crate::hir::lower_to_hir(ast);
    let package = hir.name.0.clone();
    let (hir_table, results, genv, mut type_diagnostics) =
        crate::typer::check_file_with_env_and_results(
            hir,
            hir_table,
            GlobalTypeEnv::new(),
            builtins::builtin_env(),
            &package,
            HashMap::new(),
        );
    type_diagnostics.append(&mut hir_diagnostics);
    parse_diagnostics.append(&mut type_diagnostics);

    let exports = PackageExports {
        type_env: genv.type_env.clone(),
        trait_env: genv.trait_env.clone(),
        value_env: genv.value_env.clone(),
    };
    let mut full_env = builtins::builtin_env();
    exports.apply_to(&mut full_env);

    Ok((hir_table, results, full_env, parse_diagnostics))
}

pub fn hover_type(path: &Path, src: &str, line: u32, col: u32) -> Result<String, String> {
    let result = parser::parse(path, src);
    let root = MySyntaxNode::new_root(result.green_node);
    let cst = cst::cst::File::cast(root).ok_or_else(|| "failed to cast syntax tree".to_string())?;

    let line_index = line_index::LineIndex::new(src);
    let offset = line_index
        .offset(line_index::LineCol { line, col })
        .ok_or_else(|| "failed to get offset from line and column".to_string())?;
    let node = cst.syntax().token_at_offset(offset);
    let token = match node {
        rowan::TokenAtOffset::None => None,
        rowan::TokenAtOffset::Single(x) => Some(x),
        rowan::TokenAtOffset::Between(x, y) => {
            if x.kind() == MySyntaxKind::Ident {
                Some(x)
            } else {
                Some(y)
            }
        }
    };
    let range = token.as_ref().map(|tok| tok.text_range());

    let (hir_table, results, genv, _diagnostics) = typecheck_single_file_for_query(path, src)
        .or_else(|_| {
            pipeline::pipeline::typecheck_with_packages_and_results(path, src)
                .map_err(|e| format!("{:?}", e))
        })?;
    let index = HirResultsIndex::new(&hir_table);
    let closure_params = ClosureParamIndex::new(&hir_table);

    if let Some(token) = token.as_ref() {
        if let Some(ty) = param_type_from_token(token, &closure_params, &results) {
            return Ok(ty);
        }

        if let Some(pat_id) = find_mapped_pat_id_from_token(token, &index)
            && let Some(ty) = results.pat_ty(pat_id)
        {
            return Ok(ty.to_pretty(80));
        }

        if let Some(expr_id) = find_mapped_expr_id_from_token(token, &index)
            && let Some(ty) = results.expr_ty(expr_id)
        {
            return Ok(ty.to_pretty(80));
        }

        if let Some(local_id) = find_mapped_local_id_from_token(token, &index)
            && let Some(ty) = results.local_ty(local_id)
        {
            return Ok(ty.to_pretty(80));
        }
    }

    if let Some(token) = token.as_ref()
        && let Some(segments) = path_segments_from_token(token)
        && let Some(ty) = lookup_type_from_segments(&genv, &segments)
    {
        return Ok(ty);
    }

    if let Some(segments) = path_segments_at_offset(src, offset)
        && let Some(ty) = lookup_type_from_segments(&genv, &segments)
    {
        return Ok(ty);
    }

    if let Some(range) = range
        && let Some(ty) = lookup_type_from_path_range(&genv, cst.syntax(), &range)
    {
        return Ok(ty);
    }

    if let Some(ty) = lookup_type_from_path_offset(&genv, src, offset) {
        return Ok(ty);
    }

    Err("no type information found".to_string())
}

#[derive(Debug, Clone)]
struct ClosureParamIndex {
    local_by_ptr: HashMap<MySyntaxNodePtr, hir::LocalId>,
}

impl ClosureParamIndex {
    fn new(hir_table: &hir::HirTable) -> Self {
        let mut local_by_ptr = HashMap::new();
        for idx in 0..hir_table.expr_count() {
            let expr_id = hir::ExprId {
                pkg: hir_table.package(),
                idx: idx as u32,
            };
            if let hir::Expr::EClosure { params, .. } = hir_table.expr(expr_id) {
                for param in params {
                    local_by_ptr.insert(param.astptr, param.name);
                }
            }
        }
        Self { local_by_ptr }
    }

    fn local_id(&self, ptr: &MySyntaxNodePtr) -> Option<hir::LocalId> {
        self.local_by_ptr.get(ptr).copied()
    }
}

fn param_type_from_token(
    token: &MySyntaxToken,
    closure_params: &ClosureParamIndex,
    results: &crate::typer::results::TypeckResults,
) -> Option<String> {
    let mut current = token.parent();
    while let Some(node) = current {
        if let Some(param) = cst::nodes::Param::cast(node.clone()) {
            return param.ty().map(|t| t.to_string());
        }
        if let Some(param) = cst::nodes::ClosureParam::cast(node.clone()) {
            if let Some(ty) = param.ty() {
                return Some(ty.to_string());
            }
            let ptr = MySyntaxNodePtr::new(param.syntax());
            let local_id = closure_params.local_id(&ptr)?;
            return results.local_ty(local_id).map(|t| t.to_pretty(80));
        }
        current = node.parent();
    }
    None
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DotCompletionKind {
    Field,
    Method,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DotCompletionItem {
    pub name: String,
    pub kind: DotCompletionKind,
    pub detail: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ColonColonCompletionKind {
    Type,
    Value,
    Trait,
    Variant,
    Method,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ColonColonCompletionItem {
    pub name: String,
    pub kind: ColonColonCompletionKind,
    pub detail: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueCompletionKind {
    Function,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueCompletionItem {
    pub name: String,
    pub kind: ValueCompletionKind,
    pub detail: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignatureHelpItem {
    pub label: String,
    pub parameters: Vec<String>,
    pub active_parameter: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum InlayHintKind {
    Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InlayHintItem {
    pub offset: TextSize,
    pub label: String,
    pub kind: InlayHintKind,
}

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
        .map(|tok| tok.kind())
        .filter(|kind| *kind == MySyntaxKind::Dot)?;

    let mut exprs = binary_expr.exprs();
    let lhs_expr = exprs.next()?;
    let lhs_ptr = MySyntaxNodePtr::new(lhs_expr.syntax());

    let (hir_table, results, genv, _diagnostics) =
        typecheck_single_file_for_query(path, &parse_src)
            .or_else(|_| {
                pipeline::pipeline::typecheck_with_packages_and_results(path, &parse_src)
                    .map_err(|e| format!("{:?}", e))
            })
            .ok()?;
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

    let (_hir_table, _results, genv, _diagnostics) = typecheck_single_file_for_query(path, src)
        .or_else(|_| {
            pipeline::pipeline::typecheck_with_packages_and_results(path, src)
                .map_err(|e| format!("{:?}", e))
        })
        .ok()?;
    let mut items = genv
        .value_env
        .funcs
        .iter()
        .filter(|(name, _scheme)| !name.contains("::") && name.starts_with(&prefix))
        .map(|(name, scheme)| ValueCompletionItem {
            name: name.clone(),
            kind: ValueCompletionKind::Function,
            detail: Some(scheme.ty.to_pretty(80)),
        })
        .collect::<Vec<_>>();

    items.sort_by(|a, b| a.name.cmp(&b.name));
    items.truncate(50);
    Some(items)
}

pub fn signature_help(path: &Path, src: &str, line: u32, col: u32) -> Option<SignatureHelpItem> {
    let line_index = line_index::LineIndex::new(src);
    let offset = line_index.offset(line_index::LineCol { line, col })?;
    let result = parser::parse(path, src);
    let root = MySyntaxNode::new_root(result.green_node);
    let file = cst::cst::File::cast(root.clone())?;
    let (call_expr, active_parameter) = call_expr_and_active_parameter(file.syntax(), offset)?;

    let (hir_table, results, _genv, _diagnostics) = typecheck_single_file_for_query(path, src)
        .or_else(|_| {
            pipeline::pipeline::typecheck_with_packages_and_results(path, src)
                .map_err(|e| format!("{:?}", e))
        })
        .ok()?;
    let index = HirResultsIndex::new(&hir_table);
    let call_expr_id = index.expr_id(&MySyntaxNodePtr::new(call_expr.syntax()))?;
    let (parameter_types, return_type) =
        signature_for_call_expr(&hir_table, &results, call_expr_id)?;

    let parameters = parameter_types
        .iter()
        .map(|ty| ty.to_pretty(80))
        .collect::<Vec<_>>();
    let label = format!(
        "({}) -> {}",
        parameters.join(", "),
        return_type.to_pretty(80)
    );
    let active_parameter = if parameters.is_empty() {
        0
    } else {
        active_parameter.min((parameters.len() - 1) as u32)
    };

    Some(SignatureHelpItem {
        label,
        parameters,
        active_parameter,
    })
}

pub fn inlay_hints(path: &Path, src: &str) -> Option<Vec<InlayHintItem>> {
    let (hir_table, results, _genv, _diagnostics) = typecheck_single_file_for_query(path, src)
        .or_else(|_| {
            pipeline::pipeline::typecheck_with_packages_and_results(path, src)
                .map_err(|e| format!("{:?}", e))
        })
        .ok()?;

    let mut hints = Vec::new();
    for idx in 0..hir_table.expr_count() {
        let expr_id = hir::ExprId {
            pkg: hir_table.package(),
            idx: idx as u32,
        };
        match hir_table.expr(expr_id) {
            hir::Expr::ELet {
                pat, annotation, ..
            } => {
                if annotation.is_some() {
                    continue;
                }

                let mut local_defs = Vec::new();
                collect_pattern_locals(&hir_table, *pat, &mut local_defs);
                for (local_id, astptr) in local_defs {
                    if !should_emit_type_inlay_hint(&hir_table, local_id) {
                        continue;
                    }
                    let Some(ty) = results.local_ty(local_id).cloned() else {
                        continue;
                    };
                    if contains_type_var(&ty) {
                        continue;
                    }
                    hints.push(InlayHintItem {
                        offset: astptr.text_range().end(),
                        label: format!(": {}", ty.to_pretty(80)),
                        kind: InlayHintKind::Type,
                    });
                }
            }
            hir::Expr::EClosure { params, .. } => {
                for param in params {
                    if param.ty.is_some() {
                        continue;
                    }
                    if !should_emit_type_inlay_hint(&hir_table, param.name) {
                        continue;
                    }
                    let Some(ty) = results.local_ty(param.name).cloned() else {
                        continue;
                    };
                    if contains_type_var(&ty) {
                        continue;
                    }
                    hints.push(InlayHintItem {
                        offset: param.astptr.text_range().end(),
                        label: format!(": {}", ty.to_pretty(80)),
                        kind: InlayHintKind::Type,
                    });
                }
            }
            _ => {}
        }
    }

    hints.sort_by(|a, b| {
        a.offset
            .cmp(&b.offset)
            .then(a.label.cmp(&b.label))
            .then(a.kind.cmp(&b.kind))
    });
    hints.dedup_by(|a, b| a.offset == b.offset && a.label == b.label && a.kind == b.kind);

    Some(hints)
}

fn normalize_completion_ty(ty: tast::Ty) -> tast::Ty {
    match ty {
        tast::Ty::TRef { elem } => normalize_completion_ty(*elem),
        other => other,
    }
}

fn collect_pattern_locals(
    hir_table: &hir::HirTable,
    pat_id: hir::PatId,
    out: &mut Vec<(hir::LocalId, MySyntaxNodePtr)>,
) {
    match hir_table.pat(pat_id) {
        hir::Pat::PVar { name, astptr } => out.push((*name, *astptr)),
        hir::Pat::PConstr { args, .. } => {
            for arg in args {
                collect_pattern_locals(hir_table, *arg, out);
            }
        }
        hir::Pat::PStruct { fields, .. } => {
            for (_, pat) in fields {
                collect_pattern_locals(hir_table, *pat, out);
            }
        }
        hir::Pat::PTuple { pats } => {
            for pat in pats {
                collect_pattern_locals(hir_table, *pat, out);
            }
        }
        hir::Pat::PUnit
        | hir::Pat::PBool { .. }
        | hir::Pat::PInt { .. }
        | hir::Pat::PInt8 { .. }
        | hir::Pat::PInt16 { .. }
        | hir::Pat::PInt32 { .. }
        | hir::Pat::PInt64 { .. }
        | hir::Pat::PUInt8 { .. }
        | hir::Pat::PUInt16 { .. }
        | hir::Pat::PUInt32 { .. }
        | hir::Pat::PUInt64 { .. }
        | hir::Pat::PString { .. }
        | hir::Pat::PChar { .. }
        | hir::Pat::PWild => {}
    }
}

fn should_emit_type_inlay_hint(hir_table: &hir::HirTable, local_id: hir::LocalId) -> bool {
    hir_table.local_hint(local_id) != "_"
}

fn contains_type_var(ty: &tast::Ty) -> bool {
    match ty {
        tast::Ty::TVar(_) => true,
        tast::Ty::TTuple { typs } => typs.iter().any(contains_type_var),
        tast::Ty::TFunc { params, ret_ty } => {
            params.iter().any(contains_type_var) || contains_type_var(ret_ty)
        }
        tast::Ty::TApp { ty, args } => contains_type_var(ty) || args.iter().any(contains_type_var),
        tast::Ty::TArray { elem, .. } => contains_type_var(elem),
        tast::Ty::TVec { elem } => contains_type_var(elem),
        tast::Ty::TRef { elem } => contains_type_var(elem),
        tast::Ty::THashMap { key, value } => contains_type_var(key) || contains_type_var(value),
        tast::Ty::TUnit
        | tast::Ty::TBool
        | tast::Ty::TInt8
        | tast::Ty::TInt16
        | tast::Ty::TInt32
        | tast::Ty::TInt64
        | tast::Ty::TUint8
        | tast::Ty::TUint16
        | tast::Ty::TUint32
        | tast::Ty::TUint64
        | tast::Ty::TFloat32
        | tast::Ty::TFloat64
        | tast::Ty::TString
        | tast::Ty::TChar
        | tast::Ty::TEnum { .. }
        | tast::Ty::TStruct { .. }
        | tast::Ty::TDyn { .. }
        | tast::Ty::TParam { .. } => false,
    }
}

fn call_expr_and_active_parameter(
    root: &MySyntaxNode,
    offset: TextSize,
) -> Option<(CallExpr, u32)> {
    let token = token_at_offset_for_query(root, offset)
        .or_else(|| token_at_offset_for_query(root, offset.checked_sub(TextSize::from(1))?))?;

    let mut current = token.parent();
    while let Some(node) = current {
        if let Some(call_expr) = CallExpr::cast(node.clone())
            && let Some(arg_list) = call_expr.arg_list()
        {
            let range = arg_list.syntax().text_range();
            if range.start() <= offset && offset <= range.end() {
                let active_parameter = active_parameter_in_arg_list(&arg_list, offset);
                return Some((call_expr, active_parameter));
            }
        }
        current = node.parent();
    }

    None
}

fn token_at_offset_for_query(root: &MySyntaxNode, offset: TextSize) -> Option<MySyntaxToken> {
    match root.token_at_offset(offset) {
        rowan::TokenAtOffset::None => None,
        rowan::TokenAtOffset::Single(token) => Some(token),
        rowan::TokenAtOffset::Between(left, right) => {
            if right.kind() == MySyntaxKind::Ident
                || right.kind() == MySyntaxKind::LParen
                || right.kind() == MySyntaxKind::Comma
            {
                Some(right)
            } else {
                Some(left)
            }
        }
    }
}

fn active_parameter_in_arg_list(arg_list: &ArgList, offset: TextSize) -> u32 {
    let capped_offset = offset.min(arg_list.syntax().text_range().end());
    let mut depth: i32 = 0;
    let mut commas: u32 = 0;

    for token in arg_list
        .syntax()
        .descendants_with_tokens()
        .filter_map(|it| it.into_token())
    {
        if token.text_range().start() >= capped_offset {
            break;
        }

        match token.kind() {
            MySyntaxKind::LParen | MySyntaxKind::LBrace | MySyntaxKind::LBracket => depth += 1,
            MySyntaxKind::RParen | MySyntaxKind::RBrace | MySyntaxKind::RBracket => depth -= 1,
            MySyntaxKind::Comma if depth == 1 => commas += 1,
            _ => {}
        }
    }

    commas
}

fn signature_for_call_expr(
    hir_table: &hir::HirTable,
    results: &crate::typer::results::TypeckResults,
    call_expr_id: hir::ExprId,
) -> Option<(Vec<tast::Ty>, tast::Ty)> {
    let hir::Expr::ECall { func, .. } = hir_table.expr(call_expr_id) else {
        return None;
    };

    let call_ty = call_callee_type(results, call_expr_id)?;
    let tast::Ty::TFunc { params, ret_ty } = call_ty else {
        return None;
    };

    let mut params = params;
    if matches!(hir_table.expr(*func), hir::Expr::EField { .. }) && !params.is_empty() {
        params.remove(0);
    }

    Some((params, *ret_ty))
}

fn call_callee_type(
    results: &crate::typer::results::TypeckResults,
    call_expr_id: hir::ExprId,
) -> Option<tast::Ty> {
    let call_elab = results.call_elab(call_expr_id)?;
    match &call_elab.callee {
        crate::typer::results::CalleeElab::Expr(expr_id) => results.expr_ty(*expr_id).cloned(),
        crate::typer::results::CalleeElab::Var { ty, .. }
        | crate::typer::results::CalleeElab::TraitMethod { ty, .. }
        | crate::typer::results::CalleeElab::DynTraitMethod { ty, .. }
        | crate::typer::results::CalleeElab::InherentMethod { ty, .. }
        | crate::typer::results::CalleeElab::Error { ty, .. } => Some(ty.clone()),
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
        rowan::TokenAtOffset::Single(x) => Some(x),
        rowan::TokenAtOffset::Between(x, y) => {
            if y.kind() == MySyntaxKind::Ident {
                Some(y)
            } else {
                Some(x)
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

    let (_hir_table, _results, genv, _diagnostics) =
        typecheck_single_file_for_query(path, &parse_src)
            .or_else(|_| {
                pipeline::pipeline::typecheck_with_packages_and_results(path, &parse_src)
                    .map_err(|e| format!("{:?}", e))
            })
            .ok()?;

    let mut items = colon_colon_items_for_namespace(&genv, &namespace);
    items.sort_by(|a, b| a.name.cmp(&b.name));
    items.retain(|item| item.name.starts_with(&prefix));
    Some(items)
}

fn type_constructor_name(ty: &tast::Ty) -> Option<&str> {
    match ty {
        tast::Ty::TEnum { name } | tast::Ty::TStruct { name } => Some(name.as_str()),
        tast::Ty::TApp { ty, .. } => type_constructor_name(ty),
        tast::Ty::TRef { elem } => type_constructor_name(elem),
        _ => None,
    }
}

fn find_mapped_expr_id_from_token(
    token: &MySyntaxToken,
    index: &HirResultsIndex,
) -> Option<hir::ExprId> {
    let mut current = token.parent();
    while let Some(node) = current {
        if cst::nodes::Expr::can_cast(node.kind()) {
            let ptr = MySyntaxNodePtr::new(&node);
            if let Some(id) = index.expr_id(&ptr) {
                return Some(id);
            }
        }
        current = node.parent();
    }
    None
}

fn find_mapped_pat_id_from_token(
    token: &MySyntaxToken,
    index: &HirResultsIndex,
) -> Option<hir::PatId> {
    let mut current = token.parent();
    while let Some(node) = current {
        if cst::nodes::Pattern::can_cast(node.kind()) {
            let ptr = MySyntaxNodePtr::new(&node);
            if let Some(id) = index.pat_id(&ptr) {
                return Some(id);
            }
        }
        current = node.parent();
    }
    None
}

fn find_mapped_local_id_from_token(
    token: &MySyntaxToken,
    index: &HirResultsIndex,
) -> Option<hir::LocalId> {
    let mut current = token.parent();
    while let Some(node) = current {
        let ptr = MySyntaxNodePtr::new(&node);
        if let Some(id) = index.local_id(&ptr) {
            return Some(id);
        }
        current = node.parent();
    }
    None
}

fn lookup_type_from_path_range(
    genv: &GlobalTypeEnv,
    root: &MySyntaxNode,
    range: &rowan::TextRange,
) -> Option<String> {
    let segments = path_segments_at_range(root, range)?;
    if segments.is_empty() {
        return None;
    }
    lookup_type_from_segments(genv, &segments)
}

fn path_segments_at_range(root: &MySyntaxNode, range: &rowan::TextRange) -> Option<Vec<String>> {
    root.descendants().find_map(|node| {
        if !node.text_range().contains_range(*range) {
            return None;
        }
        let path = cst::nodes::Path::cast(node)?;
        let segments = path
            .ident_tokens()
            .map(|tok| tok.to_string())
            .collect::<Vec<_>>();
        if segments.is_empty() {
            None
        } else {
            Some(segments)
        }
    })
}

fn lookup_type_from_path_offset(
    genv: &GlobalTypeEnv,
    src: &str,
    offset: TextSize,
) -> Option<String> {
    let segments = path_segments_at_offset(src, offset)?;
    if segments.is_empty() {
        return None;
    }
    lookup_type_from_segments(genv, &segments)
}

fn path_segments_at_offset(src: &str, offset: TextSize) -> Option<Vec<String>> {
    let mut idx = u32::from(offset) as usize;
    if idx >= src.len() {
        return None;
    }
    let bytes = src.as_bytes();
    if !is_path_char(bytes[idx]) && idx > 0 && is_path_char(bytes[idx - 1]) {
        idx -= 1;
    }
    if !is_path_char(bytes[idx]) {
        return None;
    }
    let mut start = idx;
    while start > 0 && is_path_char(bytes[start - 1]) {
        start -= 1;
    }
    let mut end = idx + 1;
    while end < bytes.len() && is_path_char(bytes[end]) {
        end += 1;
    }
    let slice = &src[start..end];
    let trimmed = slice.trim_matches(':');
    if trimmed.is_empty() {
        return None;
    }
    let segments = trimmed
        .split("::")
        .filter(|s| !s.is_empty())
        .map(|s| s.to_string())
        .collect::<Vec<_>>();
    if segments.is_empty() {
        None
    } else {
        Some(segments)
    }
}

fn is_path_char(b: u8) -> bool {
    matches!(b, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' | b':')
}

fn is_ident_char(b: u8) -> bool {
    matches!(b, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_')
}

fn ident_prefix_at_offset(src: &str, offset: TextSize) -> Option<(TextSize, String)> {
    let mut idx = u32::from(offset) as usize;
    let bytes = src.as_bytes();
    if idx > bytes.len() {
        return None;
    }
    while idx > 0 && is_ident_char(bytes[idx - 1]) {
        idx -= 1;
    }
    let start = idx;
    let end = u32::from(offset) as usize;
    let prefix = src.get(start..end)?.to_string();
    Some((TextSize::from(start as u32), prefix))
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

fn ancestor_path_from_token(token: &MySyntaxToken) -> Option<cst::nodes::Path> {
    let mut current = token.parent();
    while let Some(node) = current {
        if let Some(path) = cst::nodes::Path::cast(node.clone()) {
            return Some(path);
        }
        current = node.parent();
    }
    None
}

fn colon_colon_items_for_namespace(
    genv: &GlobalTypeEnv,
    namespace: &str,
) -> Vec<ColonColonCompletionItem> {
    let mut items = Vec::new();

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
        | tast::Ty::TVec { .. }
        | tast::Ty::TRef { .. }
        | tast::Ty::THashMap { .. } => Some(ty.get_constr_name_unsafe()),
        _ => None,
    }
}

fn path_segments_from_token(token: &MySyntaxToken) -> Option<Vec<String>> {
    let mut current = token.parent();
    while let Some(node) = current {
        if let Some(path) = cst::nodes::Path::cast(node.clone()) {
            let segments = path
                .ident_tokens()
                .map(|tok| tok.to_string())
                .collect::<Vec<_>>();
            if segments.is_empty() {
                return None;
            }
            return Some(segments);
        }
        current = node.parent();
    }
    None
}

fn lookup_type_from_segments(genv: &GlobalTypeEnv, segments: &[String]) -> Option<String> {
    if let Some(ty) = find_function_type(genv, segments) {
        return Some(ty.to_pretty(80));
    }
    if segments.len() >= 2 {
        let constr = tast::TastIdent(segments[segments.len() - 1].clone());
        let enum_name = tast::TastIdent(segments[..segments.len() - 1].join("::"));
        if let Some((_ctor, ty)) = genv.lookup_constructor_with_namespace(Some(&enum_name), &constr)
        {
            return Some(ty.to_pretty(80));
        }
        let short_enum = tast::TastIdent(segments[segments.len() - 2].clone());
        if let Some((_ctor, ty)) =
            genv.lookup_constructor_with_namespace(Some(&short_enum), &constr)
        {
            return Some(ty.to_pretty(80));
        }
    }
    None
}

fn find_function_type(genv: &GlobalTypeEnv, segments: &[String]) -> Option<tast::Ty> {
    let full_path = segments.join("::");
    if let Some(ty) = genv.get_type_of_function(&full_path) {
        return Some(ty);
    }
    if let Some(last) = segments.last()
        && let Some(ty) = genv.get_type_of_function(last)
    {
        return Some(ty);
    }
    let segment_strs = segments.iter().map(|s| s.as_str()).collect::<Vec<_>>();
    let mut best: Option<tast::Ty> = None;
    for (name, scheme) in genv.value_env.funcs.iter() {
        let parts = name.split("::").collect::<Vec<_>>();
        if parts.is_empty() {
            continue;
        }
        if parts.len() >= segment_strs.len()
            && parts[parts.len() - segment_strs.len()..] == segment_strs
        {
            return Some(scheme.ty.clone());
        }
        if let Some(last) = segments.last()
            && parts.last() == Some(&last.as_str())
        {
            best = Some(scheme.ty.clone());
        }
    }
    best
}

pub fn goto_definition(
    path: &Path,
    src: &str,
    line: u32,
    col: u32,
) -> Result<text_size::TextRange, String> {
    let locations = goto_definition_locations(path, src, line, col)?;
    let mut same_file = locations
        .iter()
        .filter(|loc| loc.path == path)
        .collect::<Vec<_>>();
    if same_file.len() == 1 {
        return Ok(same_file.swap_remove(0).range);
    }
    Err("no definition found".to_string())
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefinitionLocation {
    pub path: PathBuf,
    pub range: TextRange,
}

#[derive(Debug, Default)]
struct ProjectSymbolIndex {
    values: HashMap<String, Vec<DefinitionLocation>>,
    types: HashMap<String, Vec<DefinitionLocation>>,
    impl_methods: HashMap<(String, String), Vec<DefinitionLocation>>,
    trait_methods: HashMap<(String, String), Vec<DefinitionLocation>>,
    struct_fields: HashMap<(String, String), Vec<DefinitionLocation>>,
    enum_variants: HashMap<(String, String), Vec<DefinitionLocation>>,
    packages: HashMap<String, PathBuf>,
}

impl ProjectSymbolIndex {
    fn add_value(&mut self, name: String, loc: DefinitionLocation) {
        self.values.entry(name).or_default().push(loc);
    }

    fn add_type(&mut self, name: String, loc: DefinitionLocation) {
        self.types.entry(name).or_default().push(loc);
    }

    fn add_impl_method(&mut self, receiver: String, method: String, loc: DefinitionLocation) {
        self.impl_methods
            .entry((receiver, method))
            .or_default()
            .push(loc);
    }

    fn add_trait_method(&mut self, tr: String, method: String, loc: DefinitionLocation) {
        self.trait_methods
            .entry((tr, method))
            .or_default()
            .push(loc);
    }

    fn add_struct_field(&mut self, st: String, field: String, loc: DefinitionLocation) {
        self.struct_fields.entry((st, field)).or_default().push(loc);
    }

    fn add_enum_variant(&mut self, en: String, variant: String, loc: DefinitionLocation) {
        self.enum_variants
            .entry((en, variant))
            .or_default()
            .push(loc);
    }

    fn find_value(&self, name: &str) -> Vec<DefinitionLocation> {
        self.values.get(name).cloned().unwrap_or_default()
    }

    fn find_type(&self, name: &str) -> Vec<DefinitionLocation> {
        self.types.get(name).cloned().unwrap_or_default()
    }

    fn find_impl_methods(&self, receiver: &str, method: &str) -> Vec<DefinitionLocation> {
        self.impl_methods
            .get(&(receiver.to_string(), method.to_string()))
            .cloned()
            .unwrap_or_default()
    }

    fn find_trait_methods(&self, tr: &str, method: &str) -> Vec<DefinitionLocation> {
        self.trait_methods
            .get(&(tr.to_string(), method.to_string()))
            .cloned()
            .unwrap_or_default()
    }

    fn find_struct_field(&self, st: &str, field: &str) -> Vec<DefinitionLocation> {
        self.struct_fields
            .get(&(st.to_string(), field.to_string()))
            .cloned()
            .unwrap_or_default()
    }

    fn find_enum_variant(&self, en: &str, variant: &str) -> Vec<DefinitionLocation> {
        self.enum_variants
            .get(&(en.to_string(), variant.to_string()))
            .cloned()
            .unwrap_or_default()
    }

    fn find_package(&self, pkg: &str) -> Option<PathBuf> {
        self.packages.get(pkg).cloned()
    }
}

fn index_source_file_symbols(
    index: &mut ProjectSymbolIndex,
    file_path: &Path,
    src: &str,
) -> Result<(), String> {
    let result = parser::parse(file_path, src);
    let root = MySyntaxNode::new_root(result.green_node);
    let cst_file =
        cst::cst::File::cast(root).ok_or_else(|| "failed to cast syntax tree".to_string())?;
    let package_name = cst_file
        .package_decl()
        .and_then(|d| d.name_token())
        .map(|t| t.to_string())
        .unwrap_or_else(|| "Main".to_string());

    for item in cst_file.items() {
        match item {
            cst::nodes::Item::Fn(f) => {
                if let Some(name_tok) = f.lident() {
                    let mut names = Vec::new();
                    add_name_variants(&mut names, &package_name, &name_tok.to_string());
                    for name in names {
                        index.add_value(
                            name,
                            DefinitionLocation {
                                path: file_path.to_path_buf(),
                                range: name_tok.text_range(),
                            },
                        );
                    }
                }
            }
            cst::nodes::Item::Struct(s) => {
                let Some(type_tok) = s.uident() else {
                    continue;
                };
                let mut type_names = Vec::new();
                add_name_variants(&mut type_names, &package_name, &type_tok.to_string());
                for tn in type_names.iter() {
                    index.add_type(
                        tn.clone(),
                        DefinitionLocation {
                            path: file_path.to_path_buf(),
                            range: type_tok.text_range(),
                        },
                    );
                }
                if let Some(fields) = s.field_list() {
                    for field in fields.fields() {
                        let Some(field_tok) = field.lident() else {
                            continue;
                        };
                        for tn in type_names.iter() {
                            index.add_struct_field(
                                tn.clone(),
                                field_tok.to_string(),
                                DefinitionLocation {
                                    path: file_path.to_path_buf(),
                                    range: field_tok.text_range(),
                                },
                            );
                        }
                    }
                }
            }
            cst::nodes::Item::Enum(e) => {
                let Some(type_tok) = e.uident() else {
                    continue;
                };
                let mut type_names = Vec::new();
                add_name_variants(&mut type_names, &package_name, &type_tok.to_string());
                for tn in type_names.iter() {
                    index.add_type(
                        tn.clone(),
                        DefinitionLocation {
                            path: file_path.to_path_buf(),
                            range: type_tok.text_range(),
                        },
                    );
                }
                if let Some(variants) = e.variant_list() {
                    for variant in variants.variants() {
                        let Some(var_tok) = variant.uident() else {
                            continue;
                        };
                        for tn in type_names.iter() {
                            index.add_enum_variant(
                                tn.clone(),
                                var_tok.to_string(),
                                DefinitionLocation {
                                    path: file_path.to_path_buf(),
                                    range: var_tok.text_range(),
                                },
                            );
                        }
                    }
                }
            }
            cst::nodes::Item::Trait(t) => {
                let Some(type_tok) = t.uident() else {
                    continue;
                };
                let mut type_names = Vec::new();
                add_name_variants(&mut type_names, &package_name, &type_tok.to_string());
                for tn in type_names.iter() {
                    index.add_type(
                        tn.clone(),
                        DefinitionLocation {
                            path: file_path.to_path_buf(),
                            range: type_tok.text_range(),
                        },
                    );
                }
                if let Some(methods) = t.trait_method_list() {
                    for m in methods.methods() {
                        let Some(method_tok) = m.lident() else {
                            continue;
                        };
                        for tn in type_names.iter() {
                            index.add_trait_method(
                                tn.clone(),
                                method_tok.to_string(),
                                DefinitionLocation {
                                    path: file_path.to_path_buf(),
                                    range: method_tok.text_range(),
                                },
                            );
                        }
                    }
                }
            }
            cst::nodes::Item::Impl(i) => {
                let receiver = i
                    .for_type()
                    .and_then(|t| cst_type_path_name(&t))
                    .unwrap_or_default();
                if receiver.is_empty() {
                    continue;
                }
                let receiver_keys = collect_receiver_keys(&package_name, &receiver);
                for f in i.functions() {
                    let Some(method_tok) = f.lident() else {
                        continue;
                    };
                    for rk in receiver_keys.iter() {
                        index.add_impl_method(
                            rk.clone(),
                            method_tok.to_string(),
                            DefinitionLocation {
                                path: file_path.to_path_buf(),
                                range: method_tok.text_range(),
                            },
                        );
                    }
                }
            }
            cst::nodes::Item::Extern(ex) => {
                if ex.type_keyword().is_some() {
                    let Some(type_tok) = ex.uident() else {
                        continue;
                    };
                    let mut type_names = Vec::new();
                    add_name_variants(&mut type_names, &package_name, &type_tok.to_string());
                    for tn in type_names.iter() {
                        index.add_type(
                            tn.clone(),
                            DefinitionLocation {
                                path: file_path.to_path_buf(),
                                range: type_tok.text_range(),
                            },
                        );
                    }
                } else if let Some(name_tok) = ex.lident() {
                    let mut names = Vec::new();
                    add_name_variants(&mut names, &package_name, &name_tok.to_string());
                    for name in names {
                        index.add_value(
                            name,
                            DefinitionLocation {
                                path: file_path.to_path_buf(),
                                range: name_tok.text_range(),
                            },
                        );
                    }
                }
            }
        }
    }

    Ok(())
}

fn parse_ast_for_discovery(path: &Path, src: &str) -> Result<::ast::ast::File, String> {
    let result = parser::parse(path, src);
    let (green_node, mut diagnostics) = result.into_parts();
    let root = MySyntaxNode::new_root(green_node);
    let cst = cst::cst::File::cast(root).ok_or_else(|| "failed to cast CST".to_string())?;
    let lower = ::ast::lower::lower(cst);
    let (ast, mut lower_diagnostics) = lower.into_parts();
    diagnostics.append(&mut lower_diagnostics);
    let ast = ast.ok_or_else(|| "AST lowering error".to_string())?;
    Ok(ast)
}

fn discover_packages_for_query(
    path: &Path,
    src: &str,
) -> Result<crate::pipeline::packages::PackageGraph, String> {
    if let Ok((module_dir, config)) = crate::pipeline::packages::discover_project_from_file(path) {
        let entry_path = module_dir.join(&config.package.entry);
        let entry_ast = if entry_path == path {
            Some(parse_ast_for_discovery(path, src)?)
        } else {
            None
        };
        let graph = crate::pipeline::packages::discover_packages(
            &module_dir,
            Some(entry_path.as_path()),
            entry_ast,
        )
        .map_err(|e| format!("{:?}", e))?;
        return Ok(graph);
    }

    let root_dir = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    let entry_ast = parse_ast_for_discovery(path, src)?;
    crate::pipeline::packages::discover_packages(root_dir, Some(path), Some(entry_ast))
        .map_err(|e| format!("{:?}", e))
}

fn ident_tokens_to_segments(path: &cst::nodes::Path) -> Vec<String> {
    path.ident_tokens().map(|t| t.to_string()).collect()
}

fn qualify_name(package: &str, name: &str) -> String {
    if package == "Main" || package == "Builtin" {
        name.to_string()
    } else {
        format!("{}::{}", package, name)
    }
}

fn add_name_variants(map: &mut Vec<String>, package: &str, name: &str) {
    map.push(name.to_string());
    let qualified = qualify_name(package, name);
    if qualified != name {
        map.push(qualified);
    }
}

fn collect_receiver_keys(package: &str, receiver: &str) -> Vec<String> {
    let mut keys = Vec::new();
    if receiver.is_empty() {
        return keys;
    }
    keys.push(receiver.to_string());
    if receiver.contains("::") {
        if let Some(last) = receiver.rsplit("::").next()
            && last != receiver
        {
            keys.push(last.to_string());
        }
        return keys;
    }
    let qualified = qualify_name(package, receiver);
    if qualified != receiver {
        keys.push(qualified);
    }
    keys
}

fn cst_type_path_name(ty: &cst::nodes::Type) -> Option<String> {
    match ty {
        cst::nodes::Type::TAppTy(app) => {
            let path = app.path()?;
            let segments = ident_tokens_to_segments(&path);
            if segments.is_empty() {
                None
            } else {
                Some(segments.join("::"))
            }
        }
        cst::nodes::Type::DynTy(d) => {
            let path = d.path()?;
            let segments = ident_tokens_to_segments(&path);
            if segments.is_empty() {
                None
            } else {
                Some(segments.join("::"))
            }
        }
        _ => None,
    }
}

fn index_package_symbols(
    index: &mut ProjectSymbolIndex,
    package_dir: &Path,
    package_files: &[hir::SourceFileAst],
    src_overrides: &HashMap<PathBuf, String>,
) -> Result<(), String> {
    let package_goml = package_dir.join("goml.toml");
    if package_goml.exists() {
        if let Some(pkg_name) = package_dir.file_name().and_then(|s| s.to_str()) {
            index.packages.insert(pkg_name.to_string(), package_goml);
        }
    } else if let Some(pkg_name) = package_dir.file_name().and_then(|s| s.to_str()) {
        let mut gom_files = package_files
            .iter()
            .map(|f| f.path.clone())
            .collect::<Vec<_>>();
        gom_files.sort();
        if let Some(first) = gom_files.first() {
            index.packages.insert(pkg_name.to_string(), first.clone());
        }
    }

    for file in package_files {
        let file_path = &file.path;
        let src = if let Some(override_src) = src_overrides.get(file_path) {
            override_src.clone()
        } else {
            fs::read_to_string(file_path)
                .map_err(|e| format!("failed to read {}: {}", file_path.display(), e))?
        };
        index_source_file_symbols(index, file_path, &src)?;
    }

    Ok(())
}

fn package_nav_target_in_dir(dir: &Path) -> Option<PathBuf> {
    let toml = dir.join("goml.toml");
    if toml.exists() {
        return Some(toml);
    }
    let mut gom_files = Vec::new();
    let entries = fs::read_dir(dir).ok()?;
    for entry in entries {
        let path = entry.ok()?.path();
        if path.extension().is_some_and(|ext| ext == "gom") {
            gom_files.push(path);
        }
    }
    gom_files.sort();
    gom_files.into_iter().next()
}

fn index_builtin_symbols(index: &mut ProjectSymbolIndex) -> Result<(), String> {
    let builtin_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("src")
        .join("builtin.gom");
    let Ok(src) = fs::read_to_string(&builtin_path) else {
        return Ok(());
    };
    index_source_file_symbols(index, &builtin_path, &src)
}

fn build_symbol_index(
    path: &Path,
    src: &str,
) -> Result<(crate::pipeline::packages::PackageGraph, ProjectSymbolIndex), String> {
    let graph = discover_packages_for_query(path, src)?;
    let mut index = ProjectSymbolIndex::default();
    let mut overrides = HashMap::new();
    overrides.insert(path.to_path_buf(), src.to_string());

    for (pkg_name, unit) in graph.packages.iter() {
        let Some(pkg_dir) = graph.package_dirs.get(pkg_name) else {
            continue;
        };
        index_package_symbols(&mut index, pkg_dir, &unit.files, &overrides)?;
    }

    index_builtin_symbols(&mut index)?;

    Ok((graph, index))
}

fn token_segment_index(token: &MySyntaxToken, segments: &[MySyntaxToken]) -> Option<usize> {
    segments
        .iter()
        .position(|seg| seg.text_range() == token.text_range())
}

fn use_decl_from_token(token: &MySyntaxToken) -> Option<cst::nodes::UseDecl> {
    let mut current = token.parent();
    while let Some(node) = current {
        if let Some(use_decl) = cst::nodes::UseDecl::cast(node.clone()) {
            return Some(use_decl);
        }
        current = node.parent();
    }
    None
}

fn self_definition_location(path: &Path, token: &MySyntaxToken) -> Option<DefinitionLocation> {
    if token.kind() != MySyntaxKind::Ident {
        return None;
    }

    let mut current = token.parent();
    while let Some(node) = current {
        if let Some(f) = cst::nodes::Fn::cast(node.clone())
            && f.lident()
                .is_some_and(|t| t.text_range() == token.text_range())
        {
            return Some(DefinitionLocation {
                path: path.to_path_buf(),
                range: token.text_range(),
            });
        }
        if let Some(s) = cst::nodes::Struct::cast(node.clone())
            && s.uident()
                .is_some_and(|t| t.text_range() == token.text_range())
        {
            return Some(DefinitionLocation {
                path: path.to_path_buf(),
                range: token.text_range(),
            });
        }
        if let Some(e) = cst::nodes::Enum::cast(node.clone())
            && e.uident()
                .is_some_and(|t| t.text_range() == token.text_range())
        {
            return Some(DefinitionLocation {
                path: path.to_path_buf(),
                range: token.text_range(),
            });
        }
        if let Some(t) = cst::nodes::Trait::cast(node.clone())
            && t.uident()
                .is_some_and(|t| t.text_range() == token.text_range())
        {
            return Some(DefinitionLocation {
                path: path.to_path_buf(),
                range: token.text_range(),
            });
        }
        if let Some(v) = cst::nodes::Variant::cast(node.clone())
            && v.uident()
                .is_some_and(|t| t.text_range() == token.text_range())
        {
            return Some(DefinitionLocation {
                path: path.to_path_buf(),
                range: token.text_range(),
            });
        }
        if let Some(f) = cst::nodes::StructField::cast(node.clone())
            && f.lident()
                .is_some_and(|t| t.text_range() == token.text_range())
        {
            return Some(DefinitionLocation {
                path: path.to_path_buf(),
                range: token.text_range(),
            });
        }
        if let Some(m) = cst::nodes::TraitMethod::cast(node.clone())
            && m.lident()
                .is_some_and(|t| t.text_range() == token.text_range())
        {
            return Some(DefinitionLocation {
                path: path.to_path_buf(),
                range: token.text_range(),
            });
        }
        if let Some(ex) = cst::nodes::Extern::cast(node.clone()) {
            if ex.type_keyword().is_some()
                && ex
                    .uident()
                    .is_some_and(|t| t.text_range() == token.text_range())
            {
                return Some(DefinitionLocation {
                    path: path.to_path_buf(),
                    range: token.text_range(),
                });
            }
            if ex.type_keyword().is_none()
                && ex
                    .lident()
                    .is_some_and(|t| t.text_range() == token.text_range())
            {
                return Some(DefinitionLocation {
                    path: path.to_path_buf(),
                    range: token.text_range(),
                });
            }
        }

        current = node.parent();
    }

    None
}

fn local_def_range_from_pats(hir_table: &hir::HirTable, local: hir::LocalId) -> Option<TextRange> {
    for idx in 0..hir_table.pat_count() {
        let pat_id = hir::PatId {
            pkg: hir_table.package(),
            idx: idx as u32,
        };
        if let hir::Pat::PVar { name, astptr } = hir_table.pat(pat_id)
            && *name == local
        {
            return Some(astptr.text_range());
        }
    }
    None
}

fn local_def_range_from_fn_params(token: &MySyntaxToken, name: &str) -> Option<TextRange> {
    let mut current = token.parent();
    while let Some(node) = current {
        if let Some(f) = cst::nodes::Fn::cast(node.clone()) {
            let params = f.param_list()?.params();
            for p in params {
                let ident = p.lident()?;
                if ident.to_string() == name {
                    return Some(ident.text_range());
                }
            }
            return None;
        }
        current = node.parent();
    }
    None
}

fn expr_ids_from_token(token: &MySyntaxToken, index: &HirResultsIndex) -> Vec<hir::ExprId> {
    let mut ids = Vec::new();
    let mut seen = HashSet::new();
    let mut current = token.parent();
    while let Some(node) = current {
        let ptr = MySyntaxNodePtr::new(&node);
        if let Some(id) = index.expr_id(&ptr)
            && seen.insert(id.idx)
        {
            ids.push(id);
        }
        current = node.parent();
    }
    ids
}

fn pat_ids_from_token(token: &MySyntaxToken, index: &HirResultsIndex) -> Vec<hir::PatId> {
    let mut ids = Vec::new();
    let mut seen = HashSet::new();
    let mut current = token.parent();
    while let Some(node) = current {
        let ptr = MySyntaxNodePtr::new(&node);
        if let Some(id) = index.pat_id(&ptr)
            && seen.insert(id.idx)
        {
            ids.push(id);
        }
        current = node.parent();
    }
    ids
}

fn tast_ty_constr_candidates(ty: &tast::Ty) -> Vec<String> {
    fn inner(ty: &tast::Ty, out: &mut Vec<String>) {
        match ty {
            tast::Ty::TStruct { name } | tast::Ty::TEnum { name } => out.push(name.clone()),
            tast::Ty::TApp { ty, .. } => inner(ty, out),
            tast::Ty::TRef { .. } => out.push("Ref".to_string()),
            tast::Ty::TVec { .. } => out.push("Vec".to_string()),
            tast::Ty::THashMap { .. } => out.push("HashMap".to_string()),
            _ => {}
        }
    }

    let mut out = Vec::new();
    inner(ty, &mut out);
    let mut expanded = Vec::new();
    for name in out {
        expanded.push(name.clone());
        if name.contains("::")
            && let Some(last) = name.rsplit("::").next()
            && last != name
        {
            expanded.push(last.to_string());
        }
    }
    expanded
}

fn lookup_symbol_locations_for_path(
    graph: Option<&crate::pipeline::packages::PackageGraph>,
    index: &ProjectSymbolIndex,
    token: &MySyntaxToken,
    segments: &[String],
) -> Vec<DefinitionLocation> {
    if segments.is_empty() {
        return Vec::new();
    }

    let mut locations = Vec::new();
    let full_name = segments.join("::");

    if segments[0] != "Builtin"
        && token.to_string() == segments[0]
        && graph
            .and_then(|g| g.package_dirs.get(&segments[0]))
            .is_some()
        && let Some(loc) = index.find_package(&segments[0])
    {
        locations.push(DefinitionLocation {
            path: loc,
            range: TextRange::new(TextSize::from(0), TextSize::from(0)),
        });
        return locations;
    }

    if segments.len() >= 2 {
        let enum_name = segments[..segments.len() - 1].join("::");
        let variant_name = segments[segments.len() - 1].as_str();
        if token.to_string() == *variant_name {
            locations.extend(index.find_enum_variant(&enum_name, variant_name));
            if !locations.is_empty() {
                return locations;
            }
        }
        if token.to_string() == segments[segments.len() - 2] {
            locations.extend(index.find_type(&enum_name));
            if !locations.is_empty() {
                return locations;
            }
        }
    }

    locations.extend(index.find_value(&full_name));
    locations.extend(index.find_type(&full_name));
    locations
}

pub fn goto_definition_locations(
    path: &Path,
    src: &str,
    line: u32,
    col: u32,
) -> Result<Vec<DefinitionLocation>, String> {
    let result = parser::parse(path, src);
    let root = MySyntaxNode::new_root(result.green_node);
    let cst = cst::cst::File::cast(root).ok_or_else(|| "failed to cast syntax tree".to_string())?;

    let line_index = line_index::LineIndex::new(src);
    let offset = line_index
        .offset(line_index::LineCol { line, col })
        .ok_or_else(|| "failed to get offset from line and column".to_string())?;

    let token = match cst.syntax().token_at_offset(offset) {
        rowan::TokenAtOffset::None => None,
        rowan::TokenAtOffset::Single(x) => Some(x),
        rowan::TokenAtOffset::Between(x, y) => {
            if x.kind() == MySyntaxKind::Ident {
                Some(x)
            } else {
                Some(y)
            }
        }
    }
    .ok_or_else(|| "no token at position".to_string())?;

    if let Some(use_decl) = use_decl_from_token(&token)
        && let Some(path_node) = use_decl.path()
    {
        let ident_tokens = path_node.ident_tokens().collect::<Vec<_>>();
        let segments = ident_tokens
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>();
        if segments.is_empty() {
            return Err("no definition found".to_string());
        }
        if let Some(idx) = token_segment_index(&token, &ident_tokens) {
            if idx == 0 {
                let pkg = &segments[0];
                let pkg_dir = if let Ok((module_dir, _)) =
                    crate::pipeline::packages::discover_project_from_file(path)
                {
                    module_dir.join(pkg)
                } else {
                    path.parent()
                        .filter(|p| !p.as_os_str().is_empty())
                        .unwrap_or_else(|| Path::new("."))
                        .join(pkg)
                };
                if let Some(target) = package_nav_target_in_dir(&pkg_dir) {
                    return Ok(vec![DefinitionLocation {
                        path: target,
                        range: TextRange::new(TextSize::from(0), TextSize::from(0)),
                    }]);
                }
            } else {
                let lookup = segments[..=idx].join("::");
                if let Ok((_graph, index)) = build_symbol_index(path, src) {
                    let mut out = index.find_type(&lookup);
                    out.extend(index.find_value(&lookup));
                    if !out.is_empty() {
                        return Ok(out);
                    }
                }
            }
        }
    }

    if let Some(loc) = self_definition_location(path, &token) {
        return Ok(vec![loc]);
    }

    let (graph, sym_index) = match build_symbol_index(path, src) {
        Ok((g, i)) => (Some(g), i),
        Err(_) => {
            let mut i = ProjectSymbolIndex::default();
            let _ = index_source_file_symbols(&mut i, path, src);
            let _ = index_builtin_symbols(&mut i);
            (None, i)
        }
    };

    let typecheck = typecheck_single_file_for_query(path, src).or_else(|_| {
        pipeline::pipeline::typecheck_with_packages_and_results(path, src)
            .map_err(|e| format!("{:?}", e))
    });

    if let Ok((hir_table, results, _genv, _diagnostics)) = typecheck {
        let index = HirResultsIndex::new(&hir_table);
        let expr_ids = expr_ids_from_token(&token, &index);
        for expr_id in expr_ids.iter() {
            match hir_table.expr(*expr_id) {
                hir::Expr::ENameRef { res, hint, .. } => match res {
                    hir::NameRef::Local(local_id) => {
                        if let Some(range) = local_def_range_from_pats(&hir_table, *local_id) {
                            return Ok(vec![DefinitionLocation {
                                path: path.to_path_buf(),
                                range,
                            }]);
                        }
                        if let Some(range) = local_def_range_from_fn_params(&token, hint) {
                            return Ok(vec![DefinitionLocation {
                                path: path.to_path_buf(),
                                range,
                            }]);
                        }
                    }
                    hir::NameRef::Def(_) => {
                        let mut out = sym_index.find_value(hint);
                        out.extend(sym_index.find_type(hint));
                        if !out.is_empty() {
                            return Ok(out);
                        }
                    }
                    hir::NameRef::Builtin(_) => {
                        let mut out = sym_index.find_value(hint);
                        out.extend(sym_index.find_type(hint));
                        if !out.is_empty() {
                            return Ok(out);
                        }
                    }
                    hir::NameRef::Unresolved(p) => {
                        let segments = p
                            .segments()
                            .iter()
                            .map(|s| s.seg.clone())
                            .collect::<Vec<_>>();
                        let out = lookup_symbol_locations_for_path(
                            graph.as_ref(),
                            &sym_index,
                            &token,
                            &segments,
                        );
                        if !out.is_empty() {
                            return Ok(out);
                        }
                    }
                },
                hir::Expr::EStaticMember { path: p, .. } => {
                    if token.kind() == MySyntaxKind::Ident
                        && let Some(member) = p.last_ident()
                        && token.to_string() == member.as_str()
                        && let Some(elab) = results.name_ref_elab(*expr_id)
                    {
                        match elab {
                            crate::typer::results::NameRefElab::InherentMethod {
                                receiver_ty,
                                method_name,
                                ..
                            } => {
                                let receiver_keys = tast_ty_constr_candidates(receiver_ty);
                                let mut out = Vec::new();
                                for rk in receiver_keys.iter() {
                                    out.extend(sym_index.find_impl_methods(rk, &method_name.0));
                                }
                                if !out.is_empty() {
                                    return Ok(out);
                                }
                            }
                            crate::typer::results::NameRefElab::TraitMethod {
                                trait_name,
                                method_name,
                                ..
                            }
                            | crate::typer::results::NameRefElab::DynTraitMethod {
                                trait_name,
                                method_name,
                                ..
                            } => {
                                let out =
                                    sym_index.find_trait_methods(&trait_name.0, &method_name.0);
                                if !out.is_empty() {
                                    return Ok(out);
                                }
                            }
                            crate::typer::results::NameRefElab::Var { .. } => {}
                        }
                    }
                    let segments = p
                        .segments()
                        .iter()
                        .map(|s| s.seg.clone())
                        .collect::<Vec<_>>();
                    let out = lookup_symbol_locations_for_path(
                        graph.as_ref(),
                        &sym_index,
                        &token,
                        &segments,
                    );
                    if !out.is_empty() {
                        return Ok(out);
                    }
                }
                hir::Expr::EField { expr, field } => {
                    if token.kind() != MySyntaxKind::Ident {
                        continue;
                    }
                    let field_name = field.to_ident_name();
                    if token.to_string() != field_name {
                        continue;
                    }

                    if let Some(elab) = results.name_ref_elab(*expr_id) {
                        let receiver_ty =
                            results.expr_ty(*expr).cloned().unwrap_or(tast::Ty::TUnit);
                        let receiver_keys = tast_ty_constr_candidates(&receiver_ty);
                        let method_name = match elab {
                            crate::typer::results::NameRefElab::InherentMethod {
                                method_name,
                                ..
                            }
                            | crate::typer::results::NameRefElab::TraitMethod {
                                method_name, ..
                            }
                            | crate::typer::results::NameRefElab::DynTraitMethod {
                                method_name,
                                ..
                            } => method_name.0.clone(),
                            crate::typer::results::NameRefElab::Var { .. } => field_name.clone(),
                        };
                        let mut out = Vec::new();
                        for rk in receiver_keys.iter() {
                            out.extend(sym_index.find_impl_methods(rk, &method_name));
                        }
                        if out.is_empty()
                            && let crate::typer::results::NameRefElab::TraitMethod {
                                trait_name,
                                ..
                            }
                            | crate::typer::results::NameRefElab::DynTraitMethod {
                                trait_name,
                                ..
                            } = elab
                        {
                            out.extend(sym_index.find_trait_methods(&trait_name.0, &method_name));
                        }
                        if !out.is_empty() {
                            return Ok(out);
                        }
                    } else if let Some(receiver_ty) = results.expr_ty(*expr) {
                        let receiver_keys = tast_ty_constr_candidates(receiver_ty);
                        let mut out = Vec::new();
                        for rk in receiver_keys.iter() {
                            out.extend(sym_index.find_struct_field(rk, &field_name));
                        }
                        if !out.is_empty() {
                            return Ok(out);
                        }
                    }
                }
                hir::Expr::EStructLiteral { .. } => {
                    if let Some(elab) = results.struct_lit_elab(*expr_id)
                        && token.kind() == MySyntaxKind::Ident
                    {
                        let field_name = token.to_string();
                        let struct_name = elab.constructor.type_name().0.clone();
                        let mut out = sym_index.find_struct_field(&struct_name, &field_name);
                        if out.is_empty()
                            && struct_name.contains("::")
                            && let Some(last) = struct_name.rsplit("::").next()
                        {
                            out.extend(sym_index.find_struct_field(last, &field_name));
                        }
                        if !out.is_empty() {
                            return Ok(out);
                        }
                    }
                }
                hir::Expr::EConstr { .. } => {
                    if let Some(constructor) = results.constructor_for_expr(*expr_id) {
                        if let Some(enum_ctor) = constructor.as_enum() {
                            let variant = enum_ctor.variant.0.as_str();
                            let enum_name = enum_ctor.type_name.0.as_str();
                            if token.to_string() == variant {
                                let out = sym_index.find_enum_variant(enum_name, variant);
                                if !out.is_empty() {
                                    return Ok(out);
                                }
                            }
                            if token.to_string()
                                == enum_ctor
                                    .type_name
                                    .0
                                    .rsplit("::")
                                    .next()
                                    .unwrap_or(&enum_ctor.type_name.0)
                            {
                                let out = sym_index.find_type(enum_name);
                                if !out.is_empty() {
                                    return Ok(out);
                                }
                            }
                        } else if let Some(struct_ctor) = constructor.as_struct() {
                            let st = struct_ctor.type_name.0.as_str();
                            if token.to_string()
                                == struct_ctor
                                    .type_name
                                    .0
                                    .rsplit("::")
                                    .next()
                                    .unwrap_or(&struct_ctor.type_name.0)
                            {
                                let out = sym_index.find_type(st);
                                if !out.is_empty() {
                                    return Ok(out);
                                }
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        let pat_ids = pat_ids_from_token(&token, &index);
        for pat_id in pat_ids.iter() {
            if let Some(constructor) = results.constructor_for_pat(*pat_id)
                && let Some(enum_ctor) = constructor.as_enum()
            {
                let variant = enum_ctor.variant.0.as_str();
                let enum_name = enum_ctor.type_name.0.as_str();
                if token.to_string() == variant {
                    let out = sym_index.find_enum_variant(enum_name, variant);
                    if !out.is_empty() {
                        return Ok(out);
                    }
                }
            }
        }
    }

    if token.kind() == MySyntaxKind::Ident {
        let segments = path_segments_from_token(&token).unwrap_or_default();
        if !segments.is_empty() {
            let out =
                lookup_symbol_locations_for_path(graph.as_ref(), &sym_index, &token, &segments);
            if !out.is_empty() {
                return Ok(out);
            }
        }
    }

    Err("no definition found".to_string())
}
