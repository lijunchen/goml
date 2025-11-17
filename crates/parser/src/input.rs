use lexer::{T, Token, TokenKind};
use text_size::TextRange;

pub struct Input<'t> {
    pub tokens: Vec<Token<'t>>,
    cursor: usize,
}

impl<'t> Input<'t> {
    pub fn new(tokens: Vec<Token<'t>>) -> Self {
        Self { tokens, cursor: 0 }
    }

    pub fn eof(&mut self) -> bool {
        self.eat_trivia();
        self.cursor == self.tokens.len()
    }

    pub fn skip(&mut self) {
        if !self.eof() {
            self.cursor += 1;
        }
    }

    pub fn peek(&mut self) -> TokenKind {
        self.eat_trivia();
        self.peek_raw_kind()
    }

    pub fn nth(&self, n: usize) -> TokenKind {
        let mut idx = self.cursor;
        let mut remaining = n;
        while idx < self.tokens.len() {
            let kind = self.tokens[idx].kind;
            if !kind.is_trivia() {
                if remaining == 0 {
                    return kind;
                }
                remaining -= 1;
            }
            idx += 1;
        }
        T![eof]
    }

    #[allow(unused)]
    fn at(&mut self, kind: TokenKind) -> bool {
        self.eat_trivia();
        self.peek() == kind
    }

    fn eat_trivia(&mut self) {
        while self.at_trivia() {
            self.cursor += 1;
        }
    }

    fn at_trivia(&self) -> bool {
        self.peek_raw_kind().is_trivia()
    }

    #[allow(unused)]
    fn peek_raw_token(&self) -> &Token<'_> {
        &self.tokens[self.cursor]
    }

    fn peek_raw_kind(&self) -> TokenKind {
        self.tokens.get(self.cursor).map_or(T![eof], |it| it.kind)
    }

    pub fn current_range(&self) -> Option<TextRange> {
        self.tokens.get(self.cursor).map(|token| token.range)
    }

    pub fn current_text(&self) -> Option<&'t str> {
        self.tokens.get(self.cursor).map(|token| token.text)
    }
}
