use lexer::{T, TokenKind};

use crate::{
    parser::{MarkerClosed, Parser},
    path::parse_path_always,
    syntax::MySyntaxKind,
};

pub const PATTERN_FIRST: &[TokenKind] = &[
    T![true],
    T![false],
    T![ident],
    T!['('],
    T!['['],
    T![_],
    T![-],
    T![int_lit],
    T![float],
    T![str],
    T![char_lit],
];

pub fn pattern(p: &mut Parser) -> Option<MarkerClosed> {
    let _pattern_depth = p.enter_pattern()?;
    or_pattern(p)
}

fn or_pattern(p: &mut Parser) -> Option<MarkerClosed> {
    let mut pat = alias_pattern(p)?;
    if !p.at(T![|]) {
        return Some(pat);
    }

    let marker = pat.precede(p);
    while p.eat(T![|]) {
        if p.at_any(PATTERN_FIRST) {
            let _ = alias_pattern(p);
        } else {
            p.error("expected a pattern after `|`");
            break;
        }
    }
    pat = marker.completed(p, MySyntaxKind::PATTERN_OR);
    Some(pat)
}

fn alias_pattern(p: &mut Parser) -> Option<MarkerClosed> {
    if p.at_lower_ident() && p.nth(1) == T![@] {
        let marker = p.open();
        p.expect(T![ident]);
        p.expect(T![@]);
        if let Some(_alias_depth) = p.enter_pattern()
            && alias_pattern(p).is_none()
        {
            p.error("expected a pattern after `@`");
        }
        return Some(p.close(marker, MySyntaxKind::PATTERN_ALIAS));
    }
    range_pattern(p)
}

fn range_pattern(p: &mut Parser) -> Option<MarkerClosed> {
    let pat = simple_pattern(p)?;
    if !matches!(p.peek(), T![..] | T![..=]) {
        return Some(pat);
    }

    let marker = pat.precede(p);
    p.advance();
    if simple_pattern(p).is_none() {
        p.error("expected a range pattern endpoint");
    }
    Some(marker.completed(p, MySyntaxKind::PATTERN_RANGE))
}

fn parse_pattern_list(p: &mut Parser) {
    if !p.at_any(PATTERN_FIRST) {
        return;
    }

    let _ = pattern(p);
    loop {
        if p.at(T![,]) {
            p.expect(T![,]);
            if p.at(T![')']) {
                break;
            }
            if p.at_any(PATTERN_FIRST) {
                let _ = pattern(p);
                continue;
            }
            p.advance_with_error("expected a pattern");
            continue;
        }
        if p.at(T![')']) || p.eof() {
            break;
        }
        if p.at_any(PATTERN_FIRST) {
            p.advance_with_error("expected `,` between patterns");
            let _ = pattern(p);
            continue;
        }
        p.advance_with_error("expected `,` or `)` in pattern");
    }
}

fn simple_pattern(p: &mut Parser) -> Option<MarkerClosed> {
    if !p.at_any(PATTERN_FIRST) {
        let m = p.open();
        p.error("expected a pattern");
        p.close(m, MySyntaxKind::ErrorTree);
        return None;
    }
    Some(match p.peek() {
        T![true] | T![false] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::PATTERN_BOOL)
        }
        T![-] => {
            let m = p.open();
            p.advance();
            match p.peek() {
                T![int_lit] => {
                    p.advance();
                    p.close(m, MySyntaxKind::PATTERN_INT)
                }
                T![float] => {
                    p.advance();
                    p.close(m, MySyntaxKind::PATTERN_FLOAT)
                }
                _ => {
                    p.error("expected a numeric literal after '-' in pattern");
                    p.close(m, MySyntaxKind::ErrorTree)
                }
            }
        }
        T![int_lit] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::PATTERN_INT)
        }
        T![float] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::PATTERN_FLOAT)
        }
        T![str] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::PATTERN_STRING)
        }
        T![char_lit] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::PATTERN_CHAR)
        }
        T![_] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::PATTERN_WILDCARD)
        }
        T!['('] => {
            let m = p.open();
            p.advance();
            if p.at(T![')']) {
                p.expect(T![')']);
                p.close(m, MySyntaxKind::PATTERN_UNIT)
            } else {
                parse_pattern_list(p);
                p.expect(T![')']);
                p.close(m, MySyntaxKind::PATTERN_TUPLE)
            }
        }
        T!['['] => array_pattern(p),
        T![ident] => {
            let m = p.open();
            let is_simple_var =
                p.at_lower_ident() && !matches!(p.nth(1), T![::] | T!['('] | T!['{']);

            if is_simple_var {
                p.expect(T![ident]);
                p.close(m, MySyntaxKind::PATTERN_VARIABLE)
            } else {
                parse_path_always(p);
                if p.at(T!['(']) {
                    p.expect(T!['(']);
                    parse_pattern_list(p);
                    p.expect(T![')']);
                    p.close(m, MySyntaxKind::PATTERN_CONSTR)
                } else if p.at(T!['{']) {
                    struct_pattern_field_list(p);
                    p.close(m, MySyntaxKind::PATTERN_CONSTR)
                } else {
                    p.close(m, MySyntaxKind::PATTERN_CONSTR)
                }
            }
        }
        _ => unreachable!(),
    })
}

fn array_pattern(p: &mut Parser) -> MarkerClosed {
    let marker = p.open();
    p.expect(T!['[']);
    let mut has_rest = false;
    while !p.eof() && !p.at(T![']']) {
        let is_bound_rest = p.at_lower_ident() && p.nth(1) == T![@] && p.nth(2) == T![..];
        if p.at(T![..]) || is_bound_rest {
            if has_rest {
                p.error("array pattern can contain at most one `..`");
            }
            rest_pattern(p);
            has_rest = true;
        } else if p.at_any(PATTERN_FIRST) {
            let _ = pattern(p);
        } else {
            p.advance_with_error("expected an array pattern element");
        }

        if p.eat(T![,]) {
            continue;
        }
        if !p.at(T![']']) {
            p.error("expected `,` or `]` in array pattern");
            if !p.eof() {
                p.advance();
            }
        }
    }
    p.expect(T![']']);
    p.close(marker, MySyntaxKind::PATTERN_ARRAY)
}

fn rest_pattern(p: &mut Parser) -> MarkerClosed {
    let marker = p.open();
    if p.at_lower_ident() {
        p.expect(T![ident]);
        p.expect(T![@]);
    }
    p.expect(T![..]);
    p.close(marker, MySyntaxKind::PATTERN_REST)
}

fn struct_pattern_field_list(p: &mut Parser) {
    assert!(p.at(T!['{']));
    let m = p.open();
    p.expect(T!['{']);
    let mut has_rest = false;
    while !p.eof() && !p.at(T!['}']) {
        if p.at(T![..]) {
            if has_rest {
                p.error("struct pattern can contain at most one `..`");
            }
            rest_pattern(p);
            has_rest = true;
            p.eat(T![,]);
            if !p.at(T!['}']) {
                p.error("`..` must be the last item in a struct pattern");
            }
        } else if p.at(T![ident]) {
            struct_pattern_field(p);
            if !p.eat(T![,]) && !p.at(T!['}']) {
                p.error("expected `,` between struct pattern fields");
            }
        } else {
            p.advance_with_error("expected a struct pattern field");
        }
    }
    p.expect(T!['}']);
    p.close(m, MySyntaxKind::STRUCT_PATTERN_FIELD_LIST);
}

fn struct_pattern_field(p: &mut Parser) {
    assert!(p.at(T![ident]));
    let m = p.open();
    p.expect_lower_ident("struct pattern field name");
    if p.at(T![:]) {
        p.expect(T![:]);
        if p.at_any(PATTERN_FIRST) {
            let _ = pattern(p);
        } else {
            p.advance_with_error("expected a pattern");
        }
    }
    p.close(m, MySyntaxKind::STRUCT_PATTERN_FIELD);
}
