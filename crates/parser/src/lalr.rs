use std::convert::Infallible;

use diagnostics::Diagnostics;
use lexer::{Token, TokenKind};
use rowan::GreenNodeBuilder;

use crate::parser::ParseResult;
use crate::syntax::{MySyntaxKind, ToSyntaxKind};

macro_rules! parser_tokens {
    ($($variant:ident),+ $(,)?) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum ParserToken {
            $($variant,)+
            ImplicitSemi,
        }

        impl From<TokenKind> for ParserToken {
            fn from(kind: TokenKind) -> Self {
                match kind {
                    $(TokenKind::$variant => Self::$variant,)+
                }
            }
        }
    };
}

parser_tokens!(
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Eq,
    Semi,
    Comma,
    ColonColon,
    Colon,
    Arrow,
    FatArrow,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Dot,
    DotDotEq,
    DotDot,
    At,
    AndAnd,
    Amp,
    OrOr,
    Pipe,
    Caret,
    Tilde,
    Bang,
    Less,
    LessLess,
    Greater,
    GreaterGreater,
    GreaterEq,
    LessEq,
    EqEq,
    NotEq,
    Question,
    Pound,
    ExternKeyword,
    PackageKeyword,
    UseKeyword,
    ImportKeyword,
    AsKeyword,
    PubKeyword,
    ModKeyword,
    CrateKeyword,
    SuperKeyword,
    FnKeyword,
    TraitKeyword,
    ImplKeyword,
    ForKeyword,
    EnumKeyword,
    StructKeyword,
    TypeKeyword,
    WhereKeyword,
    MatchKeyword,
    IfKeyword,
    ElseKeyword,
    LetKeyword,
    MutKeyword,
    InKeyword,
    ReturnKeyword,
    GoKeyword,
    WhileKeyword,
    BreakKeyword,
    ContinueKeyword,
    DynKeyword,
    TrueKeyword,
    FalseKeyword,
    WildcardKeyword,
    UnitKeyword,
    BoolKeyword,
    Int8Keyword,
    Int16Keyword,
    Int32Keyword,
    Int64Keyword,
    Uint8Keyword,
    Uint16Keyword,
    Uint32Keyword,
    Uint64Keyword,
    Float32Keyword,
    Float64Keyword,
    StringKeyword,
    CharKeyword,
    ArrayKeyword,
    Ident,
    Float32Lit,
    Float64Lit,
    Float,
    Int8Lit,
    Int16Lit,
    Int32Lit,
    Int64Lit,
    UInt8Lit,
    UInt16Lit,
    UInt32Lit,
    UInt64Lit,
    Int,
    Str,
    MultilineStr,
    CharLit,
    Whitespace,
    Comment,
    Error,
    Eof,
);

#[derive(Clone)]
pub enum CstElement {
    Node(CstNode),
    Token(usize),
}

#[derive(Clone)]
pub struct CstNode {
    kind: MySyntaxKind,
    children: Vec<CstElement>,
}

pub enum Postfix {
    Call(CstElement),
    Index(CstElement, CstElement, CstElement),
    Try(CstElement),
    Dot(CstElement, CstElement),
}

impl CstElement {
    pub fn node(kind: MySyntaxKind, children: Vec<Self>) -> Self {
        Self::Node(CstNode { kind, children })
    }
}

pub fn token(location: usize) -> CstElement {
    CstElement::Token(location)
}

pub fn node(kind: MySyntaxKind, children: Vec<CstElement>) -> CstElement {
    CstElement::node(kind, children)
}

pub fn root(children: Vec<CstElement>) -> CstNode {
    CstNode {
        kind: MySyntaxKind::FILE,
        children,
    }
}

pub fn prepend(mut prefix: Vec<CstElement>, element: CstElement) -> CstElement {
    match element {
        CstElement::Node(mut node) => {
            prefix.append(&mut node.children);
            node.children = prefix;
            CstElement::Node(node)
        }
        CstElement::Token(_) => unreachable!(),
    }
}

pub fn retag_append(
    element: CstElement,
    kind: MySyntaxKind,
    mut suffix: Vec<CstElement>,
) -> CstElement {
    match element {
        CstElement::Node(mut node) => {
            node.kind = kind;
            node.children.append(&mut suffix);
            CstElement::Node(node)
        }
        CstElement::Token(_) => unreachable!(),
    }
}

pub fn apply_postfix(mut element: CstElement, postfixes: Vec<Postfix>) -> CstElement {
    for postfix in postfixes {
        element = match postfix {
            Postfix::Call(args) => node(MySyntaxKind::EXPR_CALL, vec![element, args]),
            Postfix::Index(lbracket, index, rbracket) => node(
                MySyntaxKind::EXPR_INDEX,
                vec![element, lbracket, index, rbracket],
            ),
            Postfix::Try(question) => node(MySyntaxKind::EXPR_TRY, vec![element, question]),
            Postfix::Dot(dot, rhs) => node(MySyntaxKind::EXPR_BINARY, vec![element, dot, rhs]),
        };
    }
    element
}

pub fn flatten<T>(parts: T) -> Vec<CstElement>
where
    T: IntoIterator<Item = Vec<CstElement>>,
{
    parts.into_iter().flatten().collect()
}

pub fn comma_list(
    pairs: Vec<(CstElement, CstElement)>,
    last: Option<CstElement>,
) -> Vec<CstElement> {
    let mut children = Vec::with_capacity(pairs.len() * 2 + usize::from(last.is_some()));
    for (element, comma) in pairs {
        children.push(element);
        children.push(comma);
    }
    children.extend(last);
    children
}

pub fn path_segment_count(element: &CstElement) -> usize {
    match element {
        CstElement::Node(node) => node
            .children
            .iter()
            .filter(|child| matches!(child, CstElement::Token(_)))
            .count()
            .div_ceil(2),
        CstElement::Token(_) => 0,
    }
}

pub fn tokens<'tokens>(
    tokens: &'tokens [Token<'_>],
) -> impl Iterator<Item = Result<(usize, ParserToken, usize), Infallible>> + 'tokens {
    let significant = tokens
        .iter()
        .filter(|token| !token.kind.is_trivia())
        .collect::<Vec<_>>();
    let mut parser_tokens = Vec::with_capacity(significant.len());
    let mut control_braces = Vec::new();
    for (index, token) in significant.iter().enumerate() {
        let start = u32::from(token.range.start()) as usize;
        let end = u32::from(token.range.end()) as usize;
        parser_tokens.push(Ok((start, token.kind.into(), end)));
        if token.kind == TokenKind::LBrace {
            control_braces.push(starts_control_body(&significant, index));
        } else if token.kind == TokenKind::RBrace
            && control_braces.pop().unwrap_or(false)
            && significant
                .get(index + 1)
                .is_some_and(|next| starts_implicit_statement(next.kind))
        {
            parser_tokens.push(Ok((end, ParserToken::ImplicitSemi, end)));
        }
    }
    parser_tokens.into_iter()
}

fn starts_control_body(tokens: &[&Token<'_>], open: usize) -> bool {
    let mut parens = 0usize;
    let mut brackets = 0usize;
    for token in tokens[..open].iter().rev() {
        match token.kind {
            TokenKind::RParen => parens += 1,
            TokenKind::LParen if parens > 0 => parens -= 1,
            TokenKind::RBracket => brackets += 1,
            TokenKind::LBracket if brackets > 0 => brackets -= 1,
            _ if parens > 0 || brackets > 0 => {}
            TokenKind::IfKeyword
            | TokenKind::MatchKeyword
            | TokenKind::WhileKeyword
            | TokenKind::ForKeyword
            | TokenKind::ElseKeyword => return true,
            TokenKind::FatArrow
            | TokenKind::Semi
            | TokenKind::LBrace
            | TokenKind::RBrace
            | TokenKind::FnKeyword
            | TokenKind::StructKeyword
            | TokenKind::EnumKeyword
            | TokenKind::TraitKeyword
            | TokenKind::ImplKeyword => return false,
            _ => {}
        }
    }
    false
}

fn starts_implicit_statement(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::IfKeyword
            | TokenKind::MatchKeyword
            | TokenKind::WhileKeyword
            | TokenKind::ForKeyword
            | TokenKind::LetKeyword
            | TokenKind::ReturnKeyword
            | TokenKind::GoKeyword
            | TokenKind::BreakKeyword
            | TokenKind::ContinueKeyword
            | TokenKind::Ident
            | TokenKind::Float32Lit
            | TokenKind::Float64Lit
            | TokenKind::Float
            | TokenKind::Int8Lit
            | TokenKind::Int16Lit
            | TokenKind::Int32Lit
            | TokenKind::Int64Lit
            | TokenKind::UInt8Lit
            | TokenKind::UInt16Lit
            | TokenKind::UInt32Lit
            | TokenKind::UInt64Lit
            | TokenKind::Int
            | TokenKind::Str
            | TokenKind::MultilineStr
            | TokenKind::CharLit
            | TokenKind::TrueKeyword
            | TokenKind::FalseKeyword
    )
}

pub fn handles(tokens: &[Token<'_>]) -> bool {
    let mut depth = 0usize;
    let mut aliases = 0usize;
    for token in tokens {
        match token.kind {
            TokenKind::LParen | TokenKind::LBracket | TokenKind::LBrace => {
                depth += 1;
                if depth > 512 {
                    return false;
                }
            }
            TokenKind::RParen | TokenKind::RBracket | TokenKind::RBrace => {
                depth = depth.saturating_sub(1);
            }
            TokenKind::At => {
                aliases += 1;
                if aliases > 128 {
                    return false;
                }
            }
            _ => {}
        }
    }
    true
}

pub fn finish(root: CstNode, tokens: &[Token<'_>]) -> ParseResult {
    let mut builder = GreenNodeBuilder::new();
    let mut cursor = 0;
    build_node(&root, tokens, &mut cursor, &mut builder);
    ParseResult {
        green_node: builder.finish(),
        diagnostics: Diagnostics::new(),
    }
}

fn build_node(
    node: &CstNode,
    tokens: &[Token<'_>],
    cursor: &mut usize,
    builder: &mut GreenNodeBuilder<'_>,
) {
    let visible = node.kind != MySyntaxKind::TombStone;
    if visible {
        builder.start_node(node.kind.into());
    }
    for child in &node.children {
        match child {
            CstElement::Node(node) => {
                if let Some(location) = first_location(node) {
                    while let Some(token) = tokens.get(*cursor) {
                        let start = u32::from(token.range.start()) as usize;
                        if !token.kind.is_trivia() || start >= location {
                            break;
                        }
                        builder.token(token.kind.to_syntax_kind(), token.text);
                        *cursor += 1;
                    }
                }
                build_node(node, tokens, cursor, builder)
            }
            CstElement::Token(location) => {
                while let Some(token) = tokens.get(*cursor) {
                    let start = u32::from(token.range.start()) as usize;
                    if start >= *location {
                        break;
                    }
                    builder.token(token.kind.to_syntax_kind(), token.text);
                    *cursor += 1;
                }
                if let Some(token) = tokens.get(*cursor) {
                    builder.token(token.kind.to_syntax_kind(), token.text);
                    *cursor += 1;
                }
                while let Some(token) = tokens.get(*cursor) {
                    if !token.kind.is_trivia() {
                        break;
                    }
                    builder.token(token.kind.to_syntax_kind(), token.text);
                    *cursor += 1;
                }
            }
        }
    }
    if node.kind == MySyntaxKind::FILE {
        while let Some(token) = tokens.get(*cursor) {
            builder.token(token.kind.to_syntax_kind(), token.text);
            *cursor += 1;
        }
    }
    if visible {
        builder.finish_node();
    }
}

fn first_location(node: &CstNode) -> Option<usize> {
    node.children.iter().find_map(|child| match child {
        CstElement::Node(node) => first_location(node),
        CstElement::Token(location) => Some(*location),
    })
}
