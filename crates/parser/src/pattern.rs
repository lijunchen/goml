use lexer::{T, TokenKind};

use crate::{
    parser::{MarkerClosed, Parser},
    syntax::MySyntaxKind,
};

pub const PATTERN_FIRST: &[TokenKind] = &[
    T![true],
    T![false],
    T![uident],
    T![lident],
    T!['('],
    T![_],
    T![int],
    T![str],
];

pub fn pattern(p: &mut Parser) {
    let _ = simple_pattern(p);
}

fn simple_pattern(p: &mut Parser) -> MarkerClosed {
    if !p.at_any(PATTERN_FIRST) {
        dbg!(&p.filename);
        dbg!(&p.peek());
    }
    assert!(p.at_any(PATTERN_FIRST));
    match p.peek() {
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
                return p.close(m, MySyntaxKind::PATTERN_UNIT);
            }

            while p.at_any(PATTERN_FIRST) {
                pattern(p);
                p.eat(T![,]);
            }

            p.expect(T![')']);
            p.close(m, MySyntaxKind::PATTERN_TUPLE)
        }
        T![lident] => {
            let m = p.open();
            p.advance();
            p.close(m, MySyntaxKind::PATTERN_VARIABLE)
        }
        T![uident] => {
            let m = p.open();
            p.advance();
            if p.at(T!['(']) {
                p.expect(T!['(']);
                while p.at_any(PATTERN_FIRST) {
                    pattern(p);
                    p.eat(T![,]);
                }
                p.expect(T![')']);
            } else if p.at(T!['{']) {
                struct_pattern_field_list(p);
            }
            p.close(m, MySyntaxKind::PATTERN_CONSTR)
        }
        _ => unreachable!(),
    }
}

fn struct_pattern_field_list(p: &mut Parser) {
    assert!(p.at(T!['{']));
    let m = p.open();
    p.expect(T!['{']);
    while !p.eof() && !p.at(T!['}']) {
        if p.at(T![lident]) {
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
    assert!(p.at(T![lident]));
    let m = p.open();
    p.expect(T![lident]);
    p.expect(T![:]);
    if p.at_any(PATTERN_FIRST) {
        pattern(p);
    } else {
        p.advance_with_error("expected a pattern");
    }
    p.close(m, MySyntaxKind::STRUCT_PATTERN_FIELD);
}
