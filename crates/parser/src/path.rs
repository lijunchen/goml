use crate::parser::Parser;
use crate::syntax::MySyntaxKind;
use lexer::T;

/// Parses a path (e.g., `A::B::C`).
/// If `always_wrap` is true, always wraps the result in a PATH node.
/// If `always_wrap` is false, only wraps when there are namespace separators (::).
/// Returns true if a PATH node was created.
pub fn parse_path_inner(p: &mut Parser, always_wrap: bool) -> bool {
    let current = p.peek();
    debug_assert!(matches!(current, T![ident] | T![::]));

    let has_namespace = matches!(current, T![::]);
    let should_wrap =
        always_wrap || has_namespace || matches!(current, T![ident]) && matches!(p.nth(1), T![::]);
    let marker = if should_wrap { Some(p.open()) } else { None };

    if has_namespace {
        p.expect(T![::]);
    }

    if !p.at(T![ident]) {
        p.advance_with_error("expected an identifier in path");
        if let Some(m) = marker {
            p.close(m, MySyntaxKind::PATH);
        }
        return true;
    }

    p.expect(T![ident]);

    while p.at(T![::]) {
        p.expect(T![::]);
        if p.at(T![ident]) {
            p.expect(T![ident]);
        } else {
            p.advance_with_error("expected an identifier after '::'");
            break;
        }
    }

    if should_wrap {
        if let Some(m) = marker {
            p.close(m, MySyntaxKind::PATH);
        }
        true
    } else {
        false
    }
}

/// Parses a path, only wrapping in a PATH node when there are namespace separators.
/// Used for expressions where a simple identifier doesn't need wrapping.
#[allow(dead_code)]
pub fn parse_path(p: &mut Parser) -> bool {
    parse_path_inner(p, false)
}

/// Parses a path, always wrapping in a PATH node.
/// Used for type expressions and struct literals where we always expect a Path in the CST.
pub fn parse_path_always(p: &mut Parser) -> bool {
    parse_path_inner(p, true)
}
