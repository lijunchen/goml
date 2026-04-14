use cst::cst::CstNode;
use cst::nodes::{ArgList, CallExpr, PackageDecl};
use parser::syntax::{MySyntaxKind, MySyntaxNode, MySyntaxToken};
use text_size::TextSize;

pub(crate) fn call_expr_and_active_parameter(
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

pub(crate) fn token_at_offset_for_query(
    root: &MySyntaxNode,
    offset: TextSize,
) -> Option<MySyntaxToken> {
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

pub(crate) fn active_parameter_in_arg_list(arg_list: &ArgList, offset: TextSize) -> u32 {
    let capped_offset = offset.min(arg_list.syntax().text_range().end());
    let mut depth: i32 = 0;
    let mut commas: u32 = 0;

    for token in arg_list
        .syntax()
        .descendants_with_tokens()
        .filter_map(|item| item.into_token())
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

pub(crate) fn ident_prefix_at_offset(src: &str, offset: TextSize) -> Option<(TextSize, String)> {
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

fn is_ident_char(byte: u8) -> bool {
    matches!(byte, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_')
}

fn is_path_char(byte: u8) -> bool {
    matches!(byte, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' | b':')
}

pub(crate) fn path_segments_at_offset(src: &str, offset: TextSize) -> Option<Vec<String>> {
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
        .filter(|segment| !segment.is_empty())
        .map(|segment| segment.to_string())
        .collect::<Vec<_>>();
    if segments.is_empty() {
        None
    } else {
        Some(segments)
    }
}

pub(crate) fn path_segments_at_range(
    root: &MySyntaxNode,
    range: &rowan::TextRange,
) -> Option<Vec<String>> {
    root.descendants().find_map(|node| {
        if !node.text_range().contains_range(*range) {
            return None;
        }
        let path = cst::nodes::Path::cast(node)?;
        let segments = path
            .ident_tokens()
            .map(|token| token.to_string())
            .collect::<Vec<_>>();
        if segments.is_empty() {
            None
        } else {
            Some(segments)
        }
    })
}

pub(crate) fn ancestor_path_from_token(token: &MySyntaxToken) -> Option<cst::nodes::Path> {
    let mut current = token.parent();
    while let Some(node) = current {
        if let Some(path) = cst::nodes::Path::cast(node.clone()) {
            return Some(path);
        }
        current = node.parent();
    }
    None
}

pub(crate) fn path_segments_from_token(token: &MySyntaxToken) -> Option<Vec<String>> {
    let path = ancestor_path_from_token(token)?;
    let segments = path
        .ident_tokens()
        .map(|tok| tok.to_string())
        .collect::<Vec<_>>();
    if segments.is_empty() {
        None
    } else {
        Some(segments)
    }
}

pub(crate) fn token_segment_index(
    token: &MySyntaxToken,
    segments: &[MySyntaxToken],
) -> Option<usize> {
    segments
        .iter()
        .position(|segment| segment.text_range() == token.text_range())
}

pub(crate) fn use_decl_from_token(token: &MySyntaxToken) -> Option<cst::nodes::UseDecl> {
    let mut current = token.parent();
    while let Some(node) = current {
        if let Some(use_decl) = cst::nodes::UseDecl::cast(node.clone()) {
            return Some(use_decl);
        }
        current = node.parent();
    }
    None
}

pub(crate) fn package_decl_from_token(token: &MySyntaxToken) -> Option<PackageDecl> {
    let mut current = token.parent();
    while let Some(node) = current {
        if let Some(package_decl) = PackageDecl::cast(node.clone()) {
            return Some(package_decl);
        }
        current = node.parent();
    }
    None
}
