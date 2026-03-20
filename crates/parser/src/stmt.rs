use crate::{
    expr::expr,
    file::type_expr,
    parser::{MarkerClosed, Parser},
    pattern,
    syntax::MySyntaxKind,
};
use lexer::T;

pub fn let_stmt(p: &mut Parser) -> Option<MarkerClosed> {
    if !p.at(T![let]) {
        return None;
    }

    let m = p.open();
    p.expect(T![let]);
    p.eat(T![mut]);
    if pattern::pattern(p).is_none() {
        p.advance_with_error("expected a pattern in let statement");
    }
    if p.at(T![:]) {
        p.expect(T![:]);
        type_expr(p);
    }
    p.expect(T![=]);
    if expr(p).is_none() {
        p.advance_with_error("let statement expected an expression");
    }
    p.expect(T![;]);
    Some(p.close(m, MySyntaxKind::STMT_LET))
}

pub fn finish_assign_stmt(p: &mut Parser, target: MarkerClosed) -> MarkerClosed {
    let m = target.precede(p);
    p.expect(T![=]);
    if expr(p).is_none() {
        p.advance_with_error("assignment statement expected an expression");
    }
    p.expect(T![;]);
    p.close(m, MySyntaxKind::STMT_ASSIGN)
}

pub fn wrap_expr_stmt(p: &mut Parser, expr_marker: MarkerClosed) -> MarkerClosed {
    let m = expr_marker.precede(p);
    p.close(m, MySyntaxKind::STMT_EXPR)
}
