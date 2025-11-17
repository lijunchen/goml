use crate::parser::Parser;
use crate::syntax::MySyntaxKind;
use lexer::T;

pub fn parse_path(p: &mut Parser) -> bool {
    let current = p.peek();
    debug_assert!(matches!(current, T![ident] | T![::]));

    let mut has_namespace = matches!(current, T![::]);
    let should_wrap = has_namespace || matches!(current, T![ident]) && matches!(p.nth(1), T![::]);
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
        has_namespace = true;
        p.expect(T![::]);
        if p.at(T![ident]) {
            p.expect(T![ident]);
        } else {
            p.advance_with_error("expected an identifier after '::'");
            break;
        }
    }

    if has_namespace {
        if let Some(m) = marker {
            p.close(m, MySyntaxKind::PATH);
        }
        true
    } else {
        false
    }
}
