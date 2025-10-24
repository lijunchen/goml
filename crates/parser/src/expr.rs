use crate::{
    file::{block, type_expr},
    parser::{MarkerClosed, Parser},
    pattern,
    syntax::MySyntaxKind,
};
use lexer::T;
use lexer::TokenKind;

pub const EXPR_FIRST: &[TokenKind] = &[
    T![int],
    T![str],
    T![lident],
    T![uident],
    T![true],
    T![false],
    T![-],
    T![!],
    T![ref],
    T!['('],
    T!['['],
    T![if],
    T![let],
    T![match],
    T![|],
    T![||],
];

fn atom(p: &mut Parser) -> Option<MarkerClosed> {
    let result = match p.peek() {
        T![int] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::EXPR_INT)
        }
        T!['['] => {
            let m = p.open();
            p.expect(T!['[']);
            if !p.at(T![']']) && !p.eof() {
                if p.at_any(EXPR_FIRST) {
                    expr(p);
                    while p.at(T![,]) {
                        p.expect(T![,]);
                        if p.at(T![']']) {
                            break;
                        }
                        if p.at_any(EXPR_FIRST) {
                            expr(p);
                        } else {
                            p.advance_with_error("expected an expression in array literal");
                            break;
                        }
                    }
                } else {
                    p.advance_with_error("expected an expression in array literal");
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
        T![lident] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::EXPR_LIDENT)
        }
        T![uident] => {
            let m = p.open();
            p.expect(T![uident]);
            if p.at(T!['{']) {
                struct_literal_field_list(p);
                p.close(m, MySyntaxKind::EXPR_STRUCT_LITERAL)
            } else {
                p.close(m, MySyntaxKind::EXPR_UIDENT)
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
                expr(p);
                if p.at(T![,]) {
                    while p.at(T![,]) {
                        p.expect(T![,]);
                        if p.at_any(EXPR_FIRST) {
                            expr(p);
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
                expr(p);
            } else {
                p.advance_with_error("expected an expression after `if`");
            }
            p.close(cond_marker, MySyntaxKind::EXPR_IF_COND);

            let then_marker = p.open();
            if p.at(T!['{']) {
                block(p);
            } else if p.at_any(EXPR_FIRST) {
                expr(p);
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
                    expr(p);
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
            expr(p);
            if p.at(T!['{']) {
                match_arm_list(p);
            }
            p.close(m, MySyntaxKind::EXPR_MATCH)
        }
        T![|] | T![||] => closure_expr(p),
        _ => {
            dbg!(&p.peek());
            dbg!(&EXPR_FIRST);
            dbg!(EXPR_FIRST.contains(&p.peek()));
            assert!(!p.at_any(EXPR_FIRST));
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
    if p.at(T![lident]) {
        p.expect(T![lident]);
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
        expr(p);
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
        expr(p);
    }
    p.close(m, MySyntaxKind::MATCH_ARM);
}

fn struct_literal_field_list(p: &mut Parser) {
    assert!(p.at(T!['{']));
    let m = p.open();
    p.expect(T!['{']);
    while !p.eof() && !p.at(T!['}']) {
        if p.at(T![lident]) {
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
    assert!(p.at(T![lident]));
    let m = p.open();
    p.expect(T![lident]);
    p.expect(T![:]);
    if p.at_any(EXPR_FIRST) {
        expr(p);
    } else {
        p.advance_with_error("expected an expression");
    }
    p.close(m, MySyntaxKind::STRUCT_LITERAL_FIELD);
}

fn postfix_binding_power(op: TokenKind) -> Option<(u8, ())> {
    match op {
        T!['('] => Some((7, ())),
        _ => None,
    }
}

fn prefix_binding_power(op: TokenKind) -> Option<u8> {
    match op {
        T![-] | T![!] | T![ref] => Some(23),
        _ => None,
    }
}

fn infix_binding_power(op: TokenKind) -> Option<(u8, u8)> {
    match op {
        T![:=] => Some((0, 1)),
        T![||] => Some((1, 2)),
        T![&&] => Some((3, 4)),
        T![+] | T![-] => Some((13, 14)),
        T![*] | T![/] => Some((15, 16)),
        T![.] => Some((23, 24)),
        _ => None,
    }
}

pub fn expr(p: &mut Parser) {
    let token = p.peek();
    // let m = p.open();
    if token == T![let] {
        let_expr(p);
        // p.close(m, MySyntaxKind::EXPR);
        return;
    }
    expr_bp(p, 0);
    // p.close(m, MySyntaxKind::EXPR);
}

fn let_expr(p: &mut Parser) {
    assert!(p.at(T![let]));
    let m = p.open();
    p.expect(T![let]);
    let _ = pattern::pattern(p);
    p.expect(T![=]);
    if !p.at_any(EXPR_FIRST) {
        p.advance_with_error("let [_] expected an expression");
        return;
    }
    {
        let n = p.open();
        expr(p);
        p.close(n, MySyntaxKind::EXPR_LET_VALUE);
    }
    p.expect(T![in]);
    if !p.at_any(EXPR_FIRST) {
        p.advance_with_error("let .. in [_] expected an expression");
        return;
    }
    {
        let n = p.open();
        expr(p);
        p.close(n, MySyntaxKind::EXPR_LET_BODY);
    }
    p.close(m, MySyntaxKind::EXPR_LET);
}

fn expr_bp(p: &mut Parser, min_bp: u8) {
    let mut lhs = if let Some(r_bp) = prefix_binding_power(p.peek()) {
        let op = p.peek();
        let m = p.open();
        p.advance();
        expr_bp(p, r_bp);
        let kind = match op {
            T![ref] => MySyntaxKind::EXPR_REF,
            _ => MySyntaxKind::EXPR_PREFIX,
        };
        p.close(m, kind)
    } else {
        match atom(p) {
            Some(lhs) => lhs,
            None => return,
        }
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
            expr_bp(p, r_bp);
            let kind = match op {
                T![:=] => MySyntaxKind::EXPR_ASSIGN,
                _ => MySyntaxKind::EXPR_BINARY,
            };
            lhs = m.completed(p, kind);
            continue;
        }
        break;
    }
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
    expr(p);
    if !p.at(T![')']) {
        p.expect(T![,]);
    }
    p.close(m, MySyntaxKind::ARG);
}
