use lexer::{T, TokenKind};

use crate::{
    expr::{EXPR_FIRST, expr},
    parser::{MarkerClosed, MarkerOpened, Parser},
    path::parse_path,
    stmt,
    syntax::MySyntaxKind,
};

pub fn file(p: &mut Parser) {
    let m = p.open();
    while !p.eof() {
        if p.at(T![#]) {
            let attrs = attribute_list(p);
            item_with_attrs(p, attrs);
        } else if p.at(T![extern]) {
            extern_decl(p)
        } else if p.at(T![fn]) {
            func(p)
        } else if p.at(T![enum]) {
            enum_def(p)
        } else if p.at(T![struct]) {
            struct_def(p)
        } else if p.at(T![trait]) {
            trait_def(p)
        } else if p.at(T![impl]) {
            impl_block(p)
        } else if p.at_any(EXPR_FIRST) {
            if expr(p).is_none() {
                p.advance_with_error("expected an expression");
            }
        } else {
            p.advance_with_error("expected a function")
        }
    }
    p.close(m, MySyntaxKind::FILE);
}

fn item_with_attrs(p: &mut Parser, attrs: MarkerClosed) {
    if p.at(T![extern]) {
        let m = attrs.precede(p);
        extern_decl_with_marker(p, m);
    } else if p.at(T![fn]) {
        let m = attrs.precede(p);
        func_with_marker(p, m);
    } else if p.at(T![enum]) {
        let m = attrs.precede(p);
        enum_def_with_marker(p, m);
    } else if p.at(T![struct]) {
        let m = attrs.precede(p);
        struct_def_with_marker(p, m);
    } else if p.at(T![trait]) {
        let m = attrs.precede(p);
        trait_def_with_marker(p, m);
    } else if p.at(T![impl]) {
        let m = attrs.precede(p);
        impl_block_with_marker(p, m);
    } else {
        let m = attrs.precede(p);
        p.error("expected a top-level item after attributes");
        if !p.eof() {
            p.advance();
        }
        p.close(m, MySyntaxKind::ErrorTree);
    }
}

fn attribute_list(p: &mut Parser) -> MarkerClosed {
    let m = p.open();
    while p.at(T![#]) {
        attribute(p);
    }
    p.close(m, MySyntaxKind::ATTRIBUTE_LIST)
}

fn attribute(p: &mut Parser) {
    assert!(p.at(T![#]));
    let m = p.open();
    p.expect(T![#]);
    p.eat(T![!]);
    if p.eat(T!['[']) {
        attribute_body(p);
    } else {
        p.error("expected '[' after '#'");
    }
    p.close(m, MySyntaxKind::ATTRIBUTE);
}

fn attribute_body(p: &mut Parser) {
    let mut depth = 1;
    while depth > 0 && !p.eof() {
        if p.at(T!['[']) {
            depth += 1;
            p.advance();
        } else if p.at(T![']']) {
            depth -= 1;
            p.advance();
        } else {
            p.advance();
        }
    }
    if depth > 0 {
        p.error("unterminated attribute");
    }
}

fn extern_decl(p: &mut Parser) {
    assert!(p.at(T![extern]));
    let m = p.open();
    extern_decl_with_marker(p, m);
}

fn extern_decl_with_marker(p: &mut Parser, m: MarkerOpened) {
    p.expect(T![extern]);
    if p.at(T![type]) {
        p.expect(T![type]);
        if p.at(T![ident]) {
            p.advance();
        } else {
            p.advance_with_error("expected a type name");
        }
        p.close(m, MySyntaxKind::EXTERN);
        return;
    }
    if p.at(T![str]) {
        p.advance();
    } else {
        p.advance_with_error("expected a language string");
    }
    if p.at(T![str]) {
        p.advance();
    } else {
        p.advance_with_error("expected a package string");
    }
    let has_symbol_override = if p.at(T![str]) {
        p.advance();
        true
    } else {
        false
    };
    if has_symbol_override && !p.at(T![type]) && !p.at(T![ident]) {
        p.advance_with_error("expected a function or type declaration after Go symbol");
    }

    if p.at(T![type]) {
        p.expect(T![type]);
        if p.at(T![ident]) {
            p.advance();
        } else {
            p.advance_with_error("expected a type name");
        }
    } else {
        if p.at(T![ident]) {
            p.advance();
        } else {
            p.advance_with_error("expected a function name");
        }
        if p.at(T!['(']) {
            param_list(p);
        } else {
            p.advance_with_error("expected parameter list");
        }
        if p.eat(T![->]) {
            type_expr(p);
        }
    }
    p.close(m, MySyntaxKind::EXTERN);
}

fn func(p: &mut Parser) {
    assert!(p.at(T![fn]));
    let m = p.open();
    func_with_marker(p, m);
}

fn func_with_marker(p: &mut Parser, m: MarkerOpened) {
    p.expect(T![fn]);
    p.expect(T![ident]);
    if p.at(T!['[']) {
        generic_list(p);
    }
    if p.at(T!['(']) {
        param_list(p);
    }
    if p.eat(T![->]) {
        type_expr(p);
    }
    if p.at(T!['{']) {
        block(p);
    }
    p.close(m, MySyntaxKind::FN);
}

fn impl_block(p: &mut Parser) {
    assert!(p.at(T![impl]));
    let m = p.open();
    impl_block_with_marker(p, m);
}

fn impl_block_with_marker(p: &mut Parser, m: MarkerOpened) {
    p.expect(T![impl]);
    let has_trait = p.at(T![ident]) && matches!(p.nth(1), T![for]);
    if has_trait {
        p.expect(T![ident]);
        p.expect(T![for]);
        type_expr(p);
    } else {
        type_expr(p);
    }
    if p.at(T!['{']) {
        p.advance();
        while !p.at(T!['}']) && !p.eof() {
            if p.at(T![fn]) {
                func(p);
            } else {
                p.advance_with_error("expected a function");
            }
        }
        p.expect(T!['}']);
    }
    p.close(m, MySyntaxKind::IMPL);
}

fn trait_def(p: &mut Parser) {
    assert!(p.at(T![trait]));
    let m = p.open();
    trait_def_with_marker(p, m);
}

fn trait_def_with_marker(p: &mut Parser, m: MarkerOpened) {
    p.expect(T![trait]);
    p.expect(T![ident]);
    if p.at(T!['[']) {
        generic_list(p);
    }
    if p.at(T!['{']) {
        trait_method_list(p);
    }
    p.close(m, MySyntaxKind::TRAIT);
}

// {
//   fn foo(x: int32) -> int32;
//   fn bar(x: int32) -> int32;
// }

fn trait_method_list(p: &mut Parser) {
    assert!(p.at(T!['{']));
    p.expect(T!['{']);
    let m = p.open();
    while !p.at(T!['}']) && !p.eof() {
        if p.at(T![fn]) {
            trait_method(p);
            p.eat(T![;]);
        } else {
            p.advance_with_error("expected a method");
        }
    }
    p.expect(T!['}']);
    p.close(m, MySyntaxKind::TRAIT_METHOD_SIG_LIST);
}

fn trait_method(p: &mut Parser) {
    assert!(p.at(T![fn]));
    let m = p.open();
    p.expect(T![fn]);
    p.expect(T![ident]);
    if p.at(T!['(']) {
        type_list(p);
    }
    if p.eat(T![->]) {
        type_expr(p);
    }
    p.close(m, MySyntaxKind::TRAIT_METHOD_SIG);
}

fn enum_def(p: &mut Parser) {
    assert!(p.at(T![enum]));
    let m = p.open();
    enum_def_with_marker(p, m);
}

fn enum_def_with_marker(p: &mut Parser, m: MarkerOpened) {
    p.expect(T![enum]);
    p.expect(T![ident]);
    if p.at(T!['[']) {
        generic_list(p);
    }
    if p.at(T!['{']) {
        variant_list(p);
    }
    p.close(m, MySyntaxKind::ENUM);
}

fn struct_def(p: &mut Parser) {
    assert!(p.at(T![struct]));
    let m = p.open();
    struct_def_with_marker(p, m);
}

fn struct_def_with_marker(p: &mut Parser, m: MarkerOpened) {
    p.expect(T![struct]);
    p.expect(T![ident]);
    if p.at(T!['[']) {
        generic_list(p);
    }
    if p.at(T!['{']) {
        struct_field_list(p);
    }
    p.close(m, MySyntaxKind::STRUCT);
}

fn variant_list(p: &mut Parser) {
    assert!(p.at(T!['{']));
    p.expect(T!['{']);
    let m = p.open();
    while !p.at(T!['}']) && !p.eof() {
        if p.at(T![ident]) {
            variant(p);
            p.eat(T![,]);
        } else {
            p.advance_with_error("expected a variant");
        }
    }
    p.expect(T!['}']);
    p.close(m, MySyntaxKind::VARIANT_LIST);
}

fn struct_field_list(p: &mut Parser) {
    assert!(p.at(T!['{']));
    p.expect(T!['{']);
    let m = p.open();
    while !p.at(T!['}']) && !p.eof() {
        if p.at(T![ident]) {
            struct_field(p);
            p.eat(T![,]);
        } else {
            p.advance_with_error("expected a field");
        }
    }
    p.expect(T!['}']);
    p.close(m, MySyntaxKind::STRUCT_FIELD_LIST);
}

fn struct_field(p: &mut Parser) {
    assert!(p.at(T![ident]));
    let m = p.open();
    p.expect(T![ident]);
    p.expect(T![:]);
    type_expr(p);
    p.close(m, MySyntaxKind::STRUCT_FIELD);
}

fn variant(p: &mut Parser) {
    assert!(p.at(T![ident]));
    let m = p.open();
    p.expect(T![ident]);
    if p.at(T!['(']) {
        type_list(p);
    }
    p.close(m, MySyntaxKind::VARIANT);
}

const TYPE_FIRST: &[TokenKind] = &[
    T![Unit],
    T![Bool],
    T![Int8],
    T![Int16],
    T![Int32],
    T![Int64],
    T![Uint8],
    T![Uint16],
    T![Uint32],
    T![Uint64],
    T![Float32],
    T![Float64],
    T![String],
    T!['['],
    T!['('],
    T![ident],
    T![::],
];

fn type_list(p: &mut Parser) {
    assert!(p.at(T!['(']));
    let m = p.open();
    p.expect(T!['(']);
    while !p.at(T![')']) && !p.eof() {
        if p.at_any(TYPE_FIRST) {
            type_expr(p);
            p.eat(T![,]);
        } else {
            p.advance_with_error("expected a type");
        }
    }
    p.expect(T![')']);
    p.close(m, MySyntaxKind::TYPE_LIST);
}

fn generic(p: &mut Parser) {
    assert!(p.at(T![ident]));
    let m = p.open();
    p.expect(T![ident]);
    if p.at(T!['[']) {
        generic_list(p);
    }
    p.close(m, MySyntaxKind::GENERIC);
}

fn generic_list(p: &mut Parser) {
    assert!(p.at(T!['[']));
    let m = p.open();
    p.expect(T!['[']);
    while !p.at(T![']']) && !p.eof() {
        if p.at(T![ident]) {
            generic(p);
            p.eat(T![,]);
        } else {
            p.advance_with_error("expected a generic");
        }
    }
    p.expect(T![']']);
    p.close(m, MySyntaxKind::GENERIC_LIST);
}

const PARAM_LIST_RECOVERY: &[TokenKind] = &[T![->], T!['{'], T![fn]];
fn param_list(p: &mut Parser) {
    assert!(p.at(T!['(']));
    let m = p.open();

    p.expect(T!['(']);
    while !p.at(T![')']) && !p.eof() {
        if p.at(T![ident]) {
            param(p);
        } else {
            if p.at_any(PARAM_LIST_RECOVERY) {
                break;
            }
            p.advance_with_error("expected a parameter");
        }
    }
    p.expect(T![')']);
    p.close(m, MySyntaxKind::PARAM_LIST);
}

fn param(p: &mut Parser) {
    assert!(p.at(T![ident]));
    let m = p.open();
    p.expect(T![ident]);
    p.expect(T![:]);
    type_expr(p);
    if !p.at(T![')']) {
        p.expect(T![,]);
    }
    p.close(m, MySyntaxKind::PARAM);
}

pub(crate) fn type_expr(p: &mut Parser) {
    if type_expr_bp(p, 0).is_none() && !p.eof() {
        p.advance_with_error("expected a type");
    }
}

fn type_expr_bp(p: &mut Parser, min_bp: u8) -> Option<MarkerClosed> {
    let mut lhs = type_atom(p)?;

    loop {
        if p.eof() {
            break;
        }

        let op = p.peek();
        if let Some((l_bp, r_bp)) = type_infix_binding_power(op) {
            if l_bp < min_bp {
                break;
            }
            let m = lhs.precede(p);
            p.expect(T![->]);
            if type_expr_bp(p, r_bp).is_none() {
                p.error("expected a return type");
            }
            lhs = m.completed(p, MySyntaxKind::TYPE_FUNC);
            continue;
        }
        break;
    }

    Some(lhs)
}

fn type_infix_binding_power(op: TokenKind) -> Option<(u8, u8)> {
    match op {
        T![->] => Some((5, 4)),
        _ => None,
    }
}

fn type_atom(p: &mut Parser) -> Option<MarkerClosed> {
    let m = p.open();
    let result = match p.peek() {
        T![Unit] => {
            p.advance();
            p.close(m, MySyntaxKind::TYPE_UNIT)
        }
        T![Bool] => {
            p.advance();
            p.close(m, MySyntaxKind::TYPE_BOOL)
        }
        T![Int8] => {
            p.advance();
            p.close(m, MySyntaxKind::TYPE_INT8)
        }
        T![Int16] => {
            p.advance();
            p.close(m, MySyntaxKind::TYPE_INT16)
        }
        T![Int32] => {
            p.advance();
            p.close(m, MySyntaxKind::TYPE_INT32)
        }
        T![Int64] => {
            p.advance();
            p.close(m, MySyntaxKind::TYPE_INT64)
        }
        T![Uint8] => {
            p.advance();
            p.close(m, MySyntaxKind::TYPE_UINT8)
        }
        T![Uint16] => {
            p.advance();
            p.close(m, MySyntaxKind::TYPE_UINT16)
        }
        T![Uint32] => {
            p.advance();
            p.close(m, MySyntaxKind::TYPE_UINT32)
        }
        T![Uint64] => {
            p.advance();
            p.close(m, MySyntaxKind::TYPE_UINT64)
        }
        T![Float32] => {
            p.advance();
            p.close(m, MySyntaxKind::TYPE_FLOAT32)
        }
        T![Float64] => {
            p.advance();
            p.close(m, MySyntaxKind::TYPE_FLOAT64)
        }
        T![String] => {
            p.advance();
            p.close(m, MySyntaxKind::TYPE_STRING)
        }
        T!['('] => {
            type_list(p);
            p.close(m, MySyntaxKind::TYPE_TUPLE)
        }
        T!['['] => {
            p.expect(T!['[']);
            if p.at_any(TYPE_FIRST) {
                if type_expr_bp(p, 0).is_none() {
                    p.error("expected array element type");
                }
            } else {
                p.advance_with_error("expected array element type");
            }
            if p.at(T![;]) {
                p.expect(T![;]);
            } else {
                p.advance_with_error("expected ';' after array element type");
            }
            if p.at(T![int]) {
                p.advance();
            } else {
                p.advance_with_error("expected array length");
            }
            if p.at(T![']']) {
                p.expect(T![']']);
            } else {
                p.advance_with_error("expected closing ']' for array type");
            }
            p.close(m, MySyntaxKind::TYPE_ARRAY)
        }
        T![ident] | T![::] => {
            parse_path(p);
            if p.at(T!['[']) {
                type_param_list(p);
            }
            p.close(m, MySyntaxKind::TYPE_TAPP)
        }
        _ => {
            p.events.pop();
            return None;
        }
    };
    Some(result)
}

fn type_param_list(p: &mut Parser) {
    assert!(p.at(T!['[']));
    let m = p.open();
    p.expect(T!['[']);
    while !p.at(T![']']) && !p.eof() {
        if p.at_any(TYPE_FIRST) {
            type_expr(p);
            p.eat(T![,]);
        } else {
            p.advance_with_error("expected a type");
        }
    }
    p.expect(T![']']);
    p.close(m, MySyntaxKind::TYPE_PARAM_LIST);
}

pub fn block(p: &mut Parser) {
    assert!(p.at(T!['{']));
    let m = p.open();
    p.expect(T!['{']);

    let mut trailing_expr_seen = false;

    while !p.eof() && !p.at(T!['}']) {
        if trailing_expr_seen {
            p.advance_with_error("expected `}` after block expression");
            break;
        }

        if p.at(T![let]) {
            let _ = stmt::let_stmt(p);
            continue;
        }

        if p.at_any(EXPR_FIRST) {
            if let Some(expr_marker) = expr(p) {
                if p.eat(T![;]) {
                    stmt::wrap_expr_stmt(p, expr_marker);
                } else {
                    trailing_expr_seen = true;
                }
                continue;
            } else {
                break;
            }
        }

        if p.eat(T![;]) {
            continue;
        }

        p.advance_with_error("expected a statement or expression");
    }

    p.expect(T!['}']);
    p.close(m, MySyntaxKind::BLOCK);
}
