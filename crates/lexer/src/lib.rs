use std::fmt::{Debug, Write};

use logos::{Logos, Span};
use text_size::{TextRange, TextSize};

#[cfg(test)]
mod tests;

#[derive(Clone)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub text: &'a str,
    pub range: TextRange,
}

impl Debug for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('{')?;
        f.write_str("kind: ")?;
        f.write_str(&format!("{:?}", self.kind))?;
        f.write_str(", ")?;
        f.write_str("text: ")?;
        f.write_str(&format!("{:?}", &self.text))?;
        f.write_char('}')
    }
}

pub fn lex(input: &str) -> Vec<Token<'_>> {
    let lexer = Lexer::new(input);
    let toks: Vec<Token> = lexer.collect();
    toks
}

pub struct Lexer<'a> {
    inner: logos::Lexer<'a, TokenKind>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            inner: TokenKind::lexer(input),
        }
    }
}

fn range_from_span(span: Span) -> TextRange {
    let std::ops::Range { start, end } = span;
    let start = TextSize::try_from(start).unwrap();
    let end = TextSize::try_from(end).unwrap();
    TextRange::new(start, end)
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let kind = self.inner.next()?;
        if let Ok(kind) = kind {
            let text = self.inner.slice();
            Some(Self::Item {
                kind,
                text,
                range: range_from_span(self.inner.span()),
            })
        } else {
            let text = self.inner.slice();
            Some(Self::Item {
                kind: TokenKind::Error,
                text,
                range: range_from_span(self.inner.span()),
            })
        }
    }
}

fn lex_multiline_str(lex: &mut logos::Lexer<TokenKind>) -> Option<()> {
    let bytes = lex.remainder().as_bytes();

    let mut consumed = 0usize;
    while consumed < bytes.len() && bytes[consumed] != b'\n' {
        consumed += 1;
    }

    if consumed >= bytes.len() {
        return None;
    }

    // Include the newline separating the first and second lines.
    consumed += 1;
    let mut lines = 1usize;

    loop {
        let line_start = consumed;
        if line_start >= bytes.len() {
            break;
        }

        let mut idx = line_start;
        while idx < bytes.len() && (bytes[idx] == b' ' || bytes[idx] == b'\t') {
            idx += 1;
        }

        if idx + 1 >= bytes.len() || bytes[idx] != b'\\' || bytes[idx + 1] != b'\\' {
            // The previous line was the last multiline string line; trim the
            // trailing newline that brought us here.
            if lines >= 2 {
                consumed = line_start.saturating_sub(1);
                break;
            }
            return None;
        }

        idx += 2;
        while idx < bytes.len() && bytes[idx] != b'\n' {
            idx += 1;
        }

        lines += 1;
        if idx >= bytes.len() {
            consumed = idx;
            break;
        }

        // Keep the newline between string lines.
        consumed = idx + 1;
    }

    if lines < 2 {
        return None;
    }

    lex.bump(consumed);
    Some(())
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Logos)]
pub enum TokenKind {
    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    #[token("=")]
    Eq,

    #[token(";")]
    Semi,

    #[token(",")]
    Comma,

    #[token("::")]
    ColonColon,

    #[token(":")]
    Colon,

    #[token("->")]
    Arrow,

    #[token("=>")]
    FatArrow,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[token(".")]
    Dot,

    #[token("&&")]
    AndAnd,

    #[token("||")]
    OrOr,

    #[token("|")]
    Pipe,

    #[token("!")]
    Bang,

    #[token("#")]
    Pound,

    #[token("extern")]
    ExternKeyword,

    #[token("fn")]
    FnKeyword,

    #[token("trait")]
    TraitKeyword,

    #[token("impl")]
    ImplKeyword,

    #[token("for")]
    ForKeyword,

    #[token("enum")]
    EnumKeyword,

    #[token("struct")]
    StructKeyword,

    #[token("type")]
    TypeKeyword,

    #[token("match")]
    MatchKeyword,

    #[token("if")]
    IfKeyword,

    #[token("else")]
    ElseKeyword,

    #[token("let")]
    LetKeyword,

    #[token("in")]
    InKeyword,

    #[token("return")]
    ReturnKeyword,

    #[token("go")]
    GoKeyword,

    #[token("while")]
    WhileKeyword,

    #[token("true")]
    TrueKeyword,

    #[token("false")]
    FalseKeyword,

    #[token("_")]
    WildcardKeyword,

    #[token("unit")]
    UnitKeyword,

    #[token("bool")]
    BoolKeyword,

    #[token("int8")]
    Int8Keyword,

    #[token("int16")]
    Int16Keyword,

    #[token("int32")]
    Int32Keyword,

    #[token("int64")]
    Int64Keyword,

    #[token("uint8")]
    Uint8Keyword,

    #[token("uint16")]
    Uint16Keyword,

    #[token("uint32")]
    Uint32Keyword,

    #[token("uint64")]
    Uint64Keyword,

    #[token("float32")]
    Float32Keyword,

    #[token("float64")]
    Float64Keyword,

    #[token("string")]
    StringKeyword,

    #[token("array")]
    ArrayKeyword,

    #[regex("[A-Za-z][A-Za-z_0-9]*")]
    Ident,

    #[regex(r"[0-9]+\.[0-9]+", priority = 2)]
    Float,

    #[regex("[0-9]+")]
    Int,

    #[regex(r#""([^"\\\x00-\x1F]|\\(["\\bnfrt/]|u[a-fA-F0-9]{4}))*""#)]
    Str,
    #[regex(r"\\{2}", lex_multiline_str, priority = 2)]
    MultilineStr,

    #[regex(r"[ \t\r\n]+")]
    Whitespace,

    #[regex("//.*")]
    Comment,

    Error,
    Eof,
}

impl TokenKind {
    pub fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace | Self::Comment)
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::LParen => "(",
            Self::RParen => ")",
            Self::LBrace => "{",
            Self::RBrace => "}",
            Self::LBracket => "[",
            Self::RBracket => "]",
            Self::Eq => "=",
            Self::Semi => ";",
            Self::Comma => ",",
            Self::Colon => ":",
            Self::ColonColon => "::",
            Self::Arrow => "->",
            Self::FatArrow => "=>",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Star => "*",
            Self::Slash => "/",
            Self::Dot => ".",
            Self::AndAnd => "&&",
            Self::OrOr => "||",
            Self::Pipe => "|",
            Self::Bang => "!",
            Self::Pound => "#",
            Self::ExternKeyword => "extern",
            Self::EnumKeyword => "enum",
            Self::StructKeyword => "struct",
            Self::TypeKeyword => "type",
            Self::FnKeyword => "fn",
            Self::TraitKeyword => "trait",
            Self::ImplKeyword => "impl",
            Self::ForKeyword => "for",
            Self::IfKeyword => "if",
            Self::MatchKeyword => "match",
            Self::ElseKeyword => "else",
            Self::LetKeyword => "let",
            Self::InKeyword => "in",
            Self::ReturnKeyword => "return",
            Self::GoKeyword => "go",
            Self::WhileKeyword => "while",
            Self::TrueKeyword => "true",
            Self::FalseKeyword => "false",
            Self::WildcardKeyword => "_",
            Self::UnitKeyword => "unit",
            Self::BoolKeyword => "bool",
            Self::Int8Keyword => "int8",
            Self::Int16Keyword => "int16",
            Self::Int32Keyword => "int32",
            Self::Int64Keyword => "int64",
            Self::Uint8Keyword => "uint8",
            Self::Uint16Keyword => "uint16",
            Self::Uint32Keyword => "uint32",
            Self::Uint64Keyword => "uint64",
            Self::Float32Keyword => "float32",
            Self::Float64Keyword => "float64",
            Self::StringKeyword => "string",
            Self::ArrayKeyword => "array",
            Self::Ident => "ident",
            Self::Float => "float",
            Self::Int => "int",
            Self::Str => "str",
            Self::MultilineStr => "multiline_str",
            Self::Whitespace => "whitespace",
            Self::Comment => "comment",
            Self::Error => "unknown token",
            Self::Eof => "end of file",
        })
    }
}

#[macro_export]
macro_rules! T {
    ['('] => { $crate::TokenKind::LParen };
    [')'] => { $crate::TokenKind::RParen };
    ['{'] => { $crate::TokenKind::LBrace };
    ['}'] => { $crate::TokenKind::RBrace };
    ['['] => { $crate::TokenKind::LBracket };
    [']'] => { $crate::TokenKind::RBracket };
    [=] => { $crate::TokenKind::Eq };
    [;] => { $crate::TokenKind::Semi };
    [,] => { $crate::TokenKind::Comma };
    [::] => { $crate::TokenKind::ColonColon };
    [:] => { $crate::TokenKind::Colon };
    [->] => { $crate::TokenKind::Arrow };
    [=>] => { $crate::TokenKind::FatArrow };
    [+] => { $crate::TokenKind::Plus };
    [-] => { $crate::TokenKind::Minus };
    [*] => { $crate::TokenKind::Star };
    [/] => { $crate::TokenKind::Slash };
    [.] => { $crate::TokenKind::Dot };
    [&&] => { $crate::TokenKind::AndAnd };
    [||] => { $crate::TokenKind::OrOr };
    [|] => { $crate::TokenKind::Pipe };
    [!] => { $crate::TokenKind::Bang };
    [#] => { $crate::TokenKind::Pound };
    [extern] => { $crate::TokenKind::ExternKeyword };
    [fn] => { $crate::TokenKind::FnKeyword };
    [trait] => { $crate::TokenKind::TraitKeyword };
    [impl] => { $crate::TokenKind::ImplKeyword };
    [for] => { $crate::TokenKind::ForKeyword };
    [enum] => { $crate::TokenKind::EnumKeyword };
    [struct] => { $crate::TokenKind::StructKeyword };
    [type] => { $crate::TokenKind::TypeKeyword };
    [match] => { $crate::TokenKind::MatchKeyword };
    [if] => { $crate::TokenKind::IfKeyword };
    [else] => { $crate::TokenKind::ElseKeyword };
    [let] => { $crate::TokenKind::LetKeyword };
    [in] => { $crate::TokenKind::InKeyword };
    [return] => { $crate::TokenKind::ReturnKeyword };
    [go] => { $crate::TokenKind::GoKeyword };
    [while] => { $crate::TokenKind::WhileKeyword };
    [true] => { $crate::TokenKind::TrueKeyword };
    [false] => { $crate::TokenKind::FalseKeyword };
    [_] => { $crate::TokenKind::WildcardKeyword };
    [Unit] => { $crate::TokenKind::UnitKeyword };
    [Bool] => { $crate::TokenKind::BoolKeyword };
    [Int8] => { $crate::TokenKind::Int8Keyword };
    [Int16] => { $crate::TokenKind::Int16Keyword };
    [Int32] => { $crate::TokenKind::Int32Keyword };
    [Int64] => { $crate::TokenKind::Int64Keyword };
    [Uint8] => { $crate::TokenKind::Uint8Keyword };
    [Uint16] => { $crate::TokenKind::Uint16Keyword };
    [Uint32] => { $crate::TokenKind::Uint32Keyword };
    [Uint64] => { $crate::TokenKind::Uint64Keyword };
    [Float32] => { $crate::TokenKind::Float32Keyword };
    [Float64] => { $crate::TokenKind::Float64Keyword };
    [String] => { $crate::TokenKind::StringKeyword };
    [array] => { $crate::TokenKind::ArrayKeyword };
    [ident] => { $crate::TokenKind::Ident };
    [float] => { $crate::TokenKind::Float };
    [int] => { $crate::TokenKind::Int };
    [str] => { $crate::TokenKind::Str };
    [multiline_str] => { $crate::TokenKind::MultilineStr };
    [whitespace] => { $crate::TokenKind::Whitespace };
    [comment] => { $crate::TokenKind::Comment };
    [error] => { $crate::TokenKind::Error };
    [eof] => { $crate::TokenKind::Eof };
}
