use crate::parser::Parser;
use crate::syntax::MySyntaxKind;
use lexer::T;
use lexer::TokenKind;

fn is_path_segment(kind: TokenKind) -> bool {
    matches!(kind, T![ident])
}

pub fn parse_path_always(p: &mut Parser) {
    let marker = p.open();

    if !is_path_segment(p.peek()) {
        p.advance_with_error("expected a path segment");
        p.close(marker, MySyntaxKind::PATH);
        return;
    }

    p.advance();

    while p.at(T![::]) {
        p.expect(T![::]);
        if is_path_segment(p.peek()) {
            p.advance();
        } else {
            p.advance_with_error("expected a path segment after '::'");
            break;
        }
    }

    p.close(marker, MySyntaxKind::PATH);
}
