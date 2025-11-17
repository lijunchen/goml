use lexer::{T, TokenKind};

use crate::{
    parser::{MarkerClosed, Parser},
    path::parse_path,
    syntax::MySyntaxKind,
};

pub const PATTERN_FIRST: &[TokenKind] = &[
    T![true],
    T![false],
    T![ident],
    T![::],
    T!['('],
    T![_],
    T![int],
    T![str],
];

pub fn pattern(p: &mut Parser) -> Option<MarkerClosed> {
    simple_pattern(p)
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
        T![int] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::PATTERN_INT)
        }
        T![str] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::PATTERN_STRING)
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
                while p.at_any(PATTERN_FIRST) {
                    let _ = pattern(p);
                    p.eat(T![,]);
                }

                p.expect(T![')']);
                p.close(m, MySyntaxKind::PATTERN_TUPLE)
            }
        }
        T![ident] | T![::] => {
            let m = p.open();
            let has_namespace = parse_path(p);
            if p.at(T!['(']) {
                p.expect(T!['(']);
                while p.at_any(PATTERN_FIRST) {
                    let _ = pattern(p);
                    p.eat(T![,]);
                }
                p.expect(T![')']);
                p.close(m, MySyntaxKind::PATTERN_CONSTR)
            } else if p.at(T!['{']) {
                struct_pattern_field_list(p);
                p.close(m, MySyntaxKind::PATTERN_CONSTR)
            } else if has_namespace {
                p.close(m, MySyntaxKind::PATTERN_CONSTR)
            } else {
                p.close(m, MySyntaxKind::PATTERN_VARIABLE)
            }
        }
        _ => unreachable!(),
    })
}

fn struct_pattern_field_list(p: &mut Parser) {
    assert!(p.at(T!['{']));
    let m = p.open();
    p.expect(T!['{']);
    while !p.eof() && !p.at(T!['}']) {
        if p.at(T![ident]) {
            struct_pattern_field(p);
            p.eat(T![,]);
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
    p.expect(T![ident]);
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
