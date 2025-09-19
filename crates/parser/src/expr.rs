use crate::{
    file::block,
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
    T!['('],
    T![if],
    T![let],
    T![match],
];

fn atom(p: &mut Parser) -> Option<MarkerClosed> {
    let result = match p.peek() {
        T![int] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::EXPR_INT)
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
        T![match] => {
            let m = p.open();
            p.expect(T![match]);
            expr(p);
            if p.at(T!['{']) {
                match_arm_list(p);
            }
            p.close(m, MySyntaxKind::EXPR_MATCH)
        }
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

fn prefix_binding_power(_op: TokenKind) -> Option<((), u8)> {
    None
}

fn postfix_binding_power(op: TokenKind) -> Option<(u8, ())> {
    match op {
        T!['('] => Some((7, ())),
        _ => None,
    }
}

fn infix_binding_power(op: TokenKind) -> Option<(u8, u8)> {
    match op {
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
    let op = p.peek();

    let lhs = if let Some(((), r_bp)) = prefix_binding_power(op) {
        let _m = p.open();
        p.advance();
        expr_bp(p, r_bp);
        todo!()
    } else {
        atom(p)
    };

    if lhs.is_none() {
        return;
    }
    let mut lhs = lhs.unwrap();

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
            if op == T![:] {
                todo!()
            } else {
                p.advance();
                expr_bp(p, r_bp);
                lhs = m.completed(p, MySyntaxKind::EXPR_BINARY);
            }
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
