use crate::{
    file::{block, type_expr, type_param_list},
    parser::{MarkerClosed, Parser},
    path::parse_path_always,
    syntax::MySyntaxKind,
};
use lexer::T;
use lexer::TokenKind;

pub const EXPR_FIRST: &[TokenKind] = &[
    T![int],
    T![int8_lit],
    T![int16_lit],
    T![int32_lit],
    T![int64_lit],
    T![uint8_lit],
    T![uint16_lit],
    T![uint32_lit],
    T![uint64_lit],
    T![float],
    T![float32_lit],
    T![float64_lit],
    T![str],
    T![multiline_str],
    T![char_lit],
    T![ident],
    T![true],
    T![false],
    T![-],
    T![!],
    T![~],
    T!['('],
    T!['['],
    T![if],
    T![match],
    T![while],
    T![for],
    T![break],
    T![continue],
    T![return],
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
        T![int8_lit] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::EXPR_INT8)
        }
        T![int16_lit] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::EXPR_INT16)
        }
        T![int32_lit] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::EXPR_INT32)
        }
        T![int64_lit] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::EXPR_INT64)
        }
        T![uint8_lit] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::EXPR_UINT8)
        }
        T![uint16_lit] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::EXPR_UINT16)
        }
        T![uint32_lit] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::EXPR_UINT32)
        }
        T![uint64_lit] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::EXPR_UINT64)
        }
        T![float] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::EXPR_FLOAT)
        }
        T![float32_lit] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::EXPR_FLOAT32)
        }
        T![float64_lit] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::EXPR_FLOAT64)
        }
        T!['['] => {
            let m = p.open();
            p.expect(T!['[']);
            let _struct_literals = p.with_struct_literals_allowed(true);
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
        T![multiline_str] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::EXPR_MULTILINE_STR)
        }
        T![char_lit] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::EXPR_CHAR)
        }
        T![true] | T![false] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::EXPR_BOOL)
        }
        // ExprName = 'name'
        T![ident] => {
            let m = p.open();
            if path_has_type_args(p) {
                let owner = p.open();
                parse_path_always(p);
                p.expect(T![::]);
                type_param_list(p);
                p.close(owner, MySyntaxKind::TYPE_TAPP);
                if p.eat(T![::]) {
                    parse_path_always(p);
                }
            } else if path_has_legacy_type_args(p) {
                p.error("generic expression arguments must use `::[...]`");
                let owner = p.open();
                parse_path_always(p);
                type_param_list(p);
                p.close(owner, MySyntaxKind::TYPE_TAPP);
                p.expect(T![::]);
                parse_path_always(p);
            } else {
                parse_path_always(p);
            }
            if p.struct_literals_allowed() && looks_like_struct_literal(p) {
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
            let _struct_literals = p.with_struct_literals_allowed(true);
            if p.at(T![')']) {
                p.expect(T![')']);
                p.close(m, MySyntaxKind::EXPR_UNIT)
            } else {
                expect_expr_with_message(p, "expected an expression in paren or tuple literal");
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
                    p.close(m, MySyntaxKind::EXPR_PAREN)
                }
            }
        }
        T![if] => {
            let m = p.open();
            p.expect(T![if]);

            let cond_marker = p.open();
            let struct_literals = p.with_struct_literals_allowed(false);
            if p.eat(T![let]) {
                let _ = super::pattern::pattern(p);
                p.expect(T![=]);
                expect_expr_with_message(p, "expected an expression after `=` in `if let`");
            } else if p.at_any(EXPR_FIRST) {
                expect_expr_with_message(p, "expected an expression after `if`");
            } else {
                p.advance_with_error("expected an expression after `if`");
            }
            drop(struct_literals);
            p.close(cond_marker, MySyntaxKind::EXPR_IF_COND);

            let then_marker = p.open();
            if p.at(T!['{']) {
                block(p);
            } else {
                p.error("`if` then branches require a block");
                if p.at_any(EXPR_FIRST) {
                    expect_expr_with_message(p, "expected a then-branch block for `if`");
                }
            }
            p.close(then_marker, MySyntaxKind::EXPR_IF_THEN);

            if p.at(T![else]) {
                p.expect(T![else]);
                let else_marker = p.open();
                if p.at(T!['{']) {
                    block(p);
                } else if p.at(T![if]) {
                    expect_expr_with_message(p, "expected an `if` expression after `else`");
                } else {
                    p.error("`else` branches require a block or another `if`");
                    if p.at_any(EXPR_FIRST) {
                        expect_expr_with_message(p, "expected an else-branch block for `if`");
                    }
                }
                p.close(else_marker, MySyntaxKind::EXPR_IF_ELSE);
            }

            p.close(m, MySyntaxKind::EXPR_IF)
        }
        T![match] => {
            let m = p.open();
            p.expect(T![match]);
            let struct_literals = p.with_struct_literals_allowed(false);
            expect_expr_with_message(p, "expected a scrutinee expression for `match`");
            drop(struct_literals);
            if p.at(T!['{']) {
                match_arm_list(p);
            } else {
                p.error("`match` requires a brace-delimited arm list");
            }
            p.close(m, MySyntaxKind::EXPR_MATCH)
        }
        T![while] => {
            let m = p.open();
            p.expect(T![while]);

            let cond_marker = p.open();
            let struct_literals = p.with_struct_literals_allowed(false);
            if p.eat(T![let]) {
                let _ = super::pattern::pattern(p);
                p.expect(T![=]);
                expect_expr_with_message(p, "expected an expression after `=` in `while let`");
            } else if p.at_any(EXPR_FIRST) {
                expect_expr_with_message(p, "expected an expression after `while`");
            } else {
                p.advance_with_error("expected an expression after `while`");
            }
            drop(struct_literals);
            p.close(cond_marker, MySyntaxKind::EXPR_WHILE_COND);

            let body_marker = p.open();
            if p.at(T!['{']) {
                block(p);
            } else {
                p.error("`while` bodies require a block");
                if p.at_any(EXPR_FIRST) {
                    expect_expr_with_message(p, "expected a body block for `while`");
                }
            }
            p.close(body_marker, MySyntaxKind::EXPR_WHILE_BODY);

            p.close(m, MySyntaxKind::EXPR_WHILE)
        }
        T![for] => {
            let m = p.open();
            p.expect(T![for]);

            let _ = super::pattern::pattern(p);
            if !p.eat(T![in]) {
                p.error("expected `in` after `for` pattern");
            }
            let struct_literals = p.with_struct_literals_allowed(false);
            if p.at_any(EXPR_FIRST) {
                expect_expr_with_message(p, "expected an iterator expression after `in`");
            } else {
                p.error("expected an iterator expression after `in`");
            }
            drop(struct_literals);
            if p.at(T!['{']) {
                block(p);
            } else {
                p.error("expected a block for `for` loop body");
                if p.at_any(EXPR_FIRST) {
                    let _ = expr(p);
                }
            }

            p.close(m, MySyntaxKind::EXPR_FOR)
        }
        T![break] => {
            let m = p.open();
            p.expect(T![break]);
            p.close(m, MySyntaxKind::EXPR_BREAK)
        }
        T![continue] => {
            let m = p.open();
            p.expect(T![continue]);
            p.close(m, MySyntaxKind::EXPR_CONTINUE)
        }
        T![return] => {
            let m = p.open();
            p.expect(T![return]);
            if !matches!(
                p.peek(),
                T![;] | T![,] | T![')'] | T![']'] | T!['}'] | T![eof]
            ) && !expect_expr_with_message(p, "expected an expression after `return`")
            {
                while !p.eof() && !matches!(p.peek(), T![;] | T![,] | T![')'] | T![']'] | T!['}']) {
                    p.advance();
                }
            }
            p.close(m, MySyntaxKind::EXPR_RETURN)
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

fn path_has_type_args(p: &mut Parser) -> bool {
    let mut index = 1;
    while p.nth(index) == T![::] && p.nth(index + 1) == T![ident] {
        index += 2;
    }
    p.nth(index) == T![::] && p.nth(index + 1) == T!['[']
}

fn path_has_legacy_type_args(p: &mut Parser) -> bool {
    let mut index = 1;
    while p.nth(index) == T![::] && p.nth(index + 1) == T![ident] {
        index += 2;
    }
    if p.nth(index) != T!['['] {
        return false;
    }
    let mut depth = 0;
    loop {
        match p.nth(index) {
            T!['['] => depth += 1,
            T![']'] => {
                depth -= 1;
                if depth == 0 {
                    return p.nth(index + 1) == T![::];
                }
            }
            T![eof] => return false,
            _ => {}
        }
        index += 1;
    }
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
        p.expect_lower_ident("closure parameter name");
    } else if p.at(T![_]) {
        p.expect(T![_]);
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
        if !p.eat(T![,]) && !p.at(T!['}']) {
            p.error("expected `,` between match arms");
        }
    }
    p.expect(T!['}']);
    p.close(m, MySyntaxKind::MATCH_ARM_LIST);
}

fn match_arm(p: &mut Parser) {
    let m = p.open();
    let _ = super::pattern::pattern(p);
    if p.at(T![if]) {
        let guard = p.open();
        p.expect(T![if]);
        expect_expr_with_message(p, "expected an expression after match guard `if`");
        p.close(guard, MySyntaxKind::MATCH_ARM_GUARD);
    }
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
    let _struct_literals = p.with_struct_literals_allowed(true);
    while !p.eof() && !p.at(T!['}']) {
        if p.at(T![ident]) {
            struct_literal_field(p);
            if !p.eat(T![,]) && !p.at(T!['}']) {
                p.error("expected `,` between struct literal fields");
            }
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
    p.expect_lower_ident("struct literal field name");
    if p.eat(T![:]) {
        if p.at_any(EXPR_FIRST) {
            expect_expr_with_message(p, "expected an expression");
        } else {
            p.advance_with_error("expected an expression");
        }
    }
    p.close(m, MySyntaxKind::STRUCT_LITERAL_FIELD);
}

fn looks_like_struct_literal(p: &mut Parser) -> bool {
    if !p.at(T!['{']) {
        return false;
    }

    match p.nth(1) {
        T!['}'] => true,
        T![ident] => match p.nth(2) {
            T![:] | T![,] => true,
            T!['}'] => !matches!(p.nth(3), T![else] | T!['{']),
            _ => false,
        },
        _ => false,
    }
}

fn postfix_binding_power(op: TokenKind) -> Option<(u8, ())> {
    match op {
        T!['('] => Some((21, ())),
        T!['['] => Some((21, ())),
        T![?] => Some((22, ())),
        _ => None,
    }
}

fn prefix_binding_power(op: TokenKind) -> Option<u8> {
    match op {
        T![-] | T![!] | T![~] => Some(23),
        _ => None,
    }
}

fn infix_binding_power(op: TokenKind) -> Option<(u8, u8)> {
    match op {
        T![||] => Some((1, 2)),
        T![&&] => Some((3, 4)),
        T![==] | T![!=] | T![<] | T![>] | T![<=] | T![>=] => Some((5, 6)),
        T![|] => Some((7, 8)),
        T![^] => Some((8, 9)),
        T![&] => Some((9, 10)),
        T![<<] | T![>>] => Some((12, 13)),
        T![+] | T![-] => Some((13, 14)),
        T![*] | T![/] | T![%] => Some((15, 16)),
        T![.] => Some((23, 24)),
        _ => None,
    }
}

pub fn expr(p: &mut Parser) -> Option<MarkerClosed> {
    expr_bp(p, 0)
}

fn expr_bp(p: &mut Parser, min_bp: u8) -> Option<MarkerClosed> {
    let _expr_depth = p.enter_expr()?;
    let mut lhs = if let Some(r_bp) = prefix_binding_power(p.peek()) {
        let m = p.open();
        p.advance();
        expect_expr_bp_with_message(p, r_bp, "expected an operand for prefix operator");
        p.close(m, MySyntaxKind::EXPR_PREFIX)
    } else {
        atom(p)?
    };
    let mut parsed_range = false;
    let mut parsed_comparison = false;

    loop {
        if p.eof() {
            break;
        }

        let op = p.peek();

        if op == T![..] {
            if min_bp > 0 {
                break;
            }
            if parsed_range {
                p.advance_with_error("range expressions cannot be chained");
                expect_expr_bp_with_message(p, 1, "expected a range endpoint after `..`");
                continue;
            }
            let m = lhs.precede(p);
            p.advance();
            expect_expr_bp_with_message(p, 1, "expected a range endpoint after `..`");
            lhs = m.completed(p, MySyntaxKind::EXPR_RANGE);
            parsed_range = true;
            continue;
        }

        if op == T![as] {
            let l_bp = 17;
            if l_bp < min_bp {
                break;
            }
            let m = lhs.precede(p);
            p.advance();
            type_expr(p);
            lhs = m.completed(p, MySyntaxKind::EXPR_CAST);
            continue;
        }

        if let Some((l_bp, ())) = postfix_binding_power(op) {
            if l_bp < min_bp {
                break;
            }
            if p.at(T!['(']) {
                let m = lhs.precede(p);
                arg_list(p);
                lhs = m.completed(p, MySyntaxKind::EXPR_CALL)
            } else if p.at(T!['[']) {
                let m = lhs.precede(p);
                p.expect(T!['[']);
                let _struct_literals = p.with_struct_literals_allowed(true);
                if p.at_any(EXPR_FIRST) {
                    expect_expr_with_message(p, "expected an index expression");
                } else if !p.at(T![']']) {
                    p.advance_with_error("expected an index expression");
                }
                p.expect(T![']']);
                lhs = m.completed(p, MySyntaxKind::EXPR_INDEX)
            } else if p.at(T![?]) {
                let m = lhs.precede(p);
                p.advance();
                lhs = m.completed(p, MySyntaxKind::EXPR_TRY)
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
            let comparison = matches!(op, T![==] | T![!=] | T![<] | T![>] | T![<=] | T![>=]);
            if comparison && parsed_comparison {
                p.advance_with_error("comparison operators cannot be chained");
                expect_expr_bp_with_message(
                    p,
                    r_bp,
                    "expected a right-hand side for comparison operator",
                );
                continue;
            }
            let m = lhs.precede(p);
            p.advance();
            expect_expr_bp_with_message(p, r_bp, "expected a right-hand side for binary operator");
            lhs = m.completed(p, MySyntaxKind::EXPR_BINARY);
            parsed_comparison |= comparison;
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
    let _struct_literals = p.with_struct_literals_allowed(true);
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
