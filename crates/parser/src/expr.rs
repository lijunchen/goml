use crate::{
    file::{block, type_expr},
    parser::{MarkerClosed, Parser},
    syntax::MySyntaxKind,
};
use lexer::T;
use lexer::TokenKind;

pub const EXPR_FIRST: &[TokenKind] = &[
    T![int],
    T![float],
    T![str],
    T![ident],
    T![true],
    T![false],
    T![-],
    T![!],
    T!['('],
    T!['['],
    T![if],
    T![match],
    T![while],
    T![|],
    T![||],
    T![go],
];

fn expect_expr_with_message(p: &mut Parser, message: &str) -> bool {
    if expr(p).is_some() {
        true
    } else {
        if !p.eof() {
            p.advance_with_error(message);
        }
        false
    }
}

fn expect_expr_bp_with_message(p: &mut Parser, min_bp: u8, message: &str) -> bool {
    if expr_bp(p, min_bp).is_some() {
        true
    } else {
        if !p.eof() {
            p.advance_with_error(message);
        }
        false
    }
}

fn atom(p: &mut Parser) -> Option<MarkerClosed> {
    let result = match p.peek() {
        T![int] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::EXPR_INT)
        }
        T![float] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::EXPR_FLOAT)
        }
        T!['['] => {
            let m = p.open();
            p.expect(T!['[']);
            if !p.at(T![']'])
                && !p.eof()
                && expect_expr_with_message(p, "expected an expression in array literal")
            {
                while p.at(T![,]) {
                    p.expect(T![,]);
                    if p.at(T![']']) {
                        break;
                    }
                    if !expect_expr_with_message(p, "expected an expression in array literal") {
                        break;
                    }
                }
            }
            p.expect(T![']']);
            p.close(m, MySyntaxKind::EXPR_ARRAY_LITERAL)
        }
        T![str] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::EXPR_STR)
        }
        T![true] | T![false] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::EXPR_BOOL)
        }
        // ExprName = 'name'
        T![ident] => {
            let m = p.open();
            p.expect(T![ident]);
            if looks_like_struct_literal(p) {
                struct_literal_field_list(p);
                p.close(m, MySyntaxKind::EXPR_STRUCT_LITERAL)
            } else {
                p.close(m, MySyntaxKind::EXPR_IDENT)
            }
        }
        // ExprParen = '( Expr ')'
        T!['('] => {
            let m = p.open();
            p.expect(T!['(']);
            if p.at(T![')']) {
                p.expect(T![')']);
                p.close(m, MySyntaxKind::EXPR_UNIT)
            } else {
                expect_expr_with_message(p, "expected an expression in tuple literal");
                if p.at(T![,]) {
                    while p.at(T![,]) {
                        p.expect(T![,]);
                        if p.at_any(EXPR_FIRST) {
                            expect_expr_with_message(p, "expected an expression in tuple literal");
                        }
                    }
                    p.expect(T![')']);
                    p.close(m, MySyntaxKind::EXPR_TUPLE)
                } else {
                    p.expect(T![')']);
                    p.close(m, MySyntaxKind::EXPR_TUPLE)
                }
            }
        }
        T![if] => {
            let m = p.open();
            p.expect(T![if]);

            let cond_marker = p.open();
            if p.at_any(EXPR_FIRST) {
                expect_expr_with_message(p, "expected an expression after `if`");
            } else {
                p.advance_with_error("expected an expression after `if`");
            }
            p.close(cond_marker, MySyntaxKind::EXPR_IF_COND);

            let then_marker = p.open();
            if p.at(T!['{']) {
                block(p);
            } else if p.at_any(EXPR_FIRST) {
                expect_expr_with_message(p, "expected a then-branch expression for `if`");
            } else {
                p.advance_with_error("expected a then-branch expression for `if`");
            }
            p.close(then_marker, MySyntaxKind::EXPR_IF_THEN);

            if p.at(T![else]) {
                p.expect(T![else]);
                let else_marker = p.open();
                if p.at(T!['{']) {
                    block(p);
                } else if p.at_any(EXPR_FIRST) {
                    expect_expr_with_message(p, "expected an else-branch expression for `if`");
                } else {
                    p.advance_with_error("expected an else-branch expression for `if`");
                }
                p.close(else_marker, MySyntaxKind::EXPR_IF_ELSE);
            } else {
                p.advance_with_error("expected `else` in `if` expression");
            }

            p.close(m, MySyntaxKind::EXPR_IF)
        }
        T![match] => {
            let m = p.open();
            p.expect(T![match]);
            expect_expr_with_message(p, "expected a scrutinee expression for `match`");
            if p.at(T!['{']) {
                match_arm_list(p);
            }
            p.close(m, MySyntaxKind::EXPR_MATCH)
        }
        T![while] => {
            let m = p.open();
            p.expect(T![while]);

            let cond_marker = p.open();
            if p.at_any(EXPR_FIRST) {
                expect_expr_with_message(p, "expected an expression after `while`");
            } else {
                p.advance_with_error("expected an expression after `while`");
            }
            p.close(cond_marker, MySyntaxKind::EXPR_WHILE_COND);

            let body_marker = p.open();
            if p.at(T!['{']) {
                block(p);
            } else if p.at_any(EXPR_FIRST) {
                expect_expr_with_message(p, "expected a body expression for `while`");
            } else {
                p.advance_with_error("expected a body expression for `while`");
            }
            p.close(body_marker, MySyntaxKind::EXPR_WHILE_BODY);

            p.close(m, MySyntaxKind::EXPR_WHILE)
        }
        T![go] => {
            let m = p.open();
            p.expect(T![go]);
            if !expect_expr_with_message(p, "expected an expression after `go`") {
                while !p.eof() && !p.at(T![;]) && !p.at(T!['}']) {
                    p.advance();
                }
            }
            p.close(m, MySyntaxKind::EXPR_GO)
        }
        T![|] | T![||] => closure_expr(p),
        _ => {
            return None;
        }
    };
    Some(result)
}

fn closure_expr(p: &mut Parser) -> MarkerClosed {
    let m = p.open();
    closure_param_list(p);
    closure_body(p);
    p.close(m, MySyntaxKind::EXPR_CLOSURE)
}

fn closure_param_list(p: &mut Parser) {
    let m = p.open();
    if p.at(T![||]) {
        p.expect(T![||]);
        p.close(m, MySyntaxKind::CLOSURE_PARAM_LIST);
        return;
    }

    p.expect(T![|]);
    while !p.at(T![|]) && !p.eof() {
        closure_param(p);
        if p.at(T![,]) {
            p.expect(T![,]);
        } else if p.at(T![|]) {
            break;
        } else {
            p.advance_with_error("expected `,` or `|` after closure parameter");
        }
    }
    p.expect(T![|]);
    p.close(m, MySyntaxKind::CLOSURE_PARAM_LIST);
}

fn closure_param(p: &mut Parser) {
    let m = p.open();
    if p.at(T![ident]) {
        p.expect(T![ident]);
    } else {
        if !p.at(T![|]) && !p.eof() {
            p.advance_with_error("expected an identifier in closure parameter");
        }
        p.close(m, MySyntaxKind::CLOSURE_PARAM);
        return;
    }
    if p.at(T![:]) {
        p.expect(T![:]);
        type_expr(p);
    }
    p.close(m, MySyntaxKind::CLOSURE_PARAM);
}

fn closure_body(p: &mut Parser) {
    let m = p.open();
    if p.at(T!['{']) {
        block(p);
    } else if p.at_any(EXPR_FIRST) {
        expect_expr_with_message(p, "expected a closure body");
    } else {
        p.advance_with_error("expected a closure body");
    }
    p.close(m, MySyntaxKind::EXPR_CLOSURE_BODY);
}

pub fn match_arm_list(p: &mut Parser) {
    assert!(p.at(T!['{']));
    let m = p.open();
    p.expect(T!['{']);
    while !p.eof() && !p.at(T!['}']) {
        match_arm(p);
        p.eat(T![,]);
    }
    p.expect(T!['}']);
    p.close(m, MySyntaxKind::MATCH_ARM_LIST);
}

fn match_arm(p: &mut Parser) {
    let m = p.open();
    let _ = super::pattern::pattern(p);
    p.expect(T![=>]);
    if p.at(T!['{']) {
        block(p);
    } else {
        expect_expr_with_message(p, "expected an expression in match arm");
    }
    p.close(m, MySyntaxKind::MATCH_ARM);
}

fn struct_literal_field_list(p: &mut Parser) {
    assert!(p.at(T!['{']));
    let m = p.open();
    p.expect(T!['{']);
    while !p.eof() && !p.at(T!['}']) {
        if p.at(T![ident]) {
            struct_literal_field(p);
            p.eat(T![,]);
        } else {
            p.advance_with_error("expected a struct field");
        }
    }
    p.expect(T!['}']);
    p.close(m, MySyntaxKind::STRUCT_LITERAL_FIELD_LIST);
}

fn struct_literal_field(p: &mut Parser) {
    assert!(p.at(T![ident]));
    let m = p.open();
    p.expect(T![ident]);
    p.expect(T![:]);
    if p.at_any(EXPR_FIRST) {
        expect_expr_with_message(p, "expected an expression");
    } else {
        p.advance_with_error("expected an expression");
    }
    p.close(m, MySyntaxKind::STRUCT_LITERAL_FIELD);
}

fn looks_like_struct_literal(p: &mut Parser) -> bool {
    if !p.at(T!['{']) {
        return false;
    }

    match p.nth(1) {
        T!['}'] => true,
        T![ident] => matches!(p.nth(2), T![:]),
        _ => false,
    }
}

fn postfix_binding_power(op: TokenKind) -> Option<(u8, ())> {
    match op {
        T!['('] => Some((7, ())),
        _ => None,
    }
}

fn prefix_binding_power(op: TokenKind) -> Option<u8> {
    match op {
        T![-] | T![!] => Some(23),
        _ => None,
    }
}

fn infix_binding_power(op: TokenKind) -> Option<(u8, u8)> {
    match op {
        T![||] => Some((1, 2)),
        T![&&] => Some((3, 4)),
        T![+] | T![-] => Some((13, 14)),
        T![*] | T![/] => Some((15, 16)),
        T![.] => Some((23, 24)),
        _ => None,
    }
}

pub fn expr(p: &mut Parser) -> Option<MarkerClosed> {
    expr_bp(p, 0)
}

fn expr_bp(p: &mut Parser, min_bp: u8) -> Option<MarkerClosed> {
    let mut lhs = if let Some(r_bp) = prefix_binding_power(p.peek()) {
        let m = p.open();
        p.advance();
        expect_expr_bp_with_message(p, r_bp, "expected an operand for prefix operator");
        p.close(m, MySyntaxKind::EXPR_PREFIX)
    } else {
        atom(p)?
    };

    loop {
        if p.eof() {
            break;
        }

        let op = p.peek();

        if let Some((l_bp, ())) = postfix_binding_power(op) {
            if l_bp < min_bp {
                break;
            }
            if p.at(T!['(']) {
                let m = lhs.precede(p);
                arg_list(p);
                lhs = m.completed(p, MySyntaxKind::EXPR_CALL)
            } else {
                let op = p.peek();
                p.advance_with_error(&format!("unexpected postfix operator {:?}", op));
            }
            continue;
        }

        if let Some((l_bp, r_bp)) = infix_binding_power(op) {
            if l_bp < min_bp {
                break;
            }
            let m = lhs.precede(p);
            p.advance();
            expect_expr_bp_with_message(p, r_bp, "expected a right-hand side for binary operator");
            lhs = m.completed(p, MySyntaxKind::EXPR_BINARY);
            continue;
        }
        break;
    }

    Some(lhs)
}

// ArgList = '(' Arg* ')'
pub fn arg_list(p: &mut Parser) {
    assert!(p.at(T!['(']));
    let m = p.open();
    p.expect(T!['(']);
    while !p.at(T![')']) && !p.eof() {
        if p.at_any(EXPR_FIRST) {
            arg(p);
        } else {
            break;
        }
    }
    p.expect(T![')']);
    p.close(m, MySyntaxKind::ARG_LIST);
}

// Arg = Expr ','?
fn arg(p: &mut Parser) {
    let m = p.open();
    if expr(p).is_none() {
        p.advance_with_error("expected an expression");
    }
    if !p.at(T![')']) {
        p.expect(T![,]);
    }
    p.close(m, MySyntaxKind::ARG);
}
