use std::cell::Cell;
use std::mem;
use std::path::{Path, PathBuf};

use crate::syntax::{MySyntaxKind, ToSyntaxKind};
use diagnostics::{Diagnostic, Diagnostics, Severity, Stage};
use lexer::{T, Token, TokenKind};
use rowan::{GreenNode, GreenNodeBuilder};

use super::event::Event;
use super::input::Input;

pub struct Parser<'t> {
    pub filename: PathBuf,
    pub input: Input<'t>,
    fuel: Cell<u32>,
    pub events: Vec<Event>,
    diagnostics: Diagnostics,
    stuck_reported: Cell<bool>,
}

pub struct ParseResult {
    pub green_node: GreenNode,
    pub diagnostics: Diagnostics,
}

impl ParseResult {
    pub fn has_errors(&self) -> bool {
        self.diagnostics.has_errors()
    }

    pub fn format_errors(&self, src: &str) -> Vec<String> {
        crate::error::format_parser_diagnostics(&self.diagnostics, src)
    }

    pub fn diagnostics(&self) -> &Diagnostics {
        &self.diagnostics
    }

    pub fn into_diagnostics(self) -> Diagnostics {
        self.diagnostics
    }

    pub fn into_parts(self) -> (GreenNode, Diagnostics) {
        (self.green_node, self.diagnostics)
    }
}

#[derive(Clone, Copy)]
pub struct MarkerOpened {
    index: usize,
}

#[derive(Clone, Copy)]
pub struct MarkerClosed {
    index: usize,
}

impl MarkerOpened {
    pub fn new(pos: usize) -> Self {
        Self { index: pos }
    }

    pub fn completed(self, p: &mut Parser, kind: MySyntaxKind) -> MarkerClosed {
        let event_at_pos = &mut p.events[self.index];
        assert!(matches!(
            event_at_pos,
            Event::Open {
                kind: _,
                forward_parent: _
            }
        ));
        *event_at_pos = Event::Open {
            kind,
            forward_parent: None,
        };
        p.events.push(Event::Close);

        MarkerClosed { index: self.index }
    }
}

impl MarkerClosed {
    pub fn precede(self, p: &mut Parser) -> MarkerOpened {
        let m = p.open();
        if let Event::Open {
            kind: _,
            ref mut forward_parent,
        } = p.events[self.index]
        {
            *forward_parent = Some(m.index - self.index)
        } else {
            unreachable!()
        }
        m
    }
}

impl<'t> Parser<'t> {
    pub fn new(filename: &Path, tokens: Vec<Token<'t>>) -> Self {
        Self {
            filename: filename.into(),
            input: Input::new(tokens),
            fuel: Cell::new(256),
            events: Vec::new(),
            diagnostics: Diagnostics::new(),
            stuck_reported: Cell::new(false),
        }
    }
}

impl Parser<'_> {
    pub fn peek(&mut self) -> TokenKind {
        if self.fuel.get() == 0 {
            if !self.stuck_reported.get() {
                let message = "parser did not consume input while parsing";
                let range = self.input.current_range();
                let diagnostic =
                    Diagnostic::new(Stage::Parser, Severity::Error, message).with_range(range);
                self.diagnostics.push(diagnostic);
                self.stuck_reported.set(true);
            }
            return T![eof];
        }
        self.fuel.set(self.fuel.get() - 1);
        self.input.peek()
    }

    pub fn nth(&mut self, n: usize) -> TokenKind {
        if self.fuel.get() == 0 {
            if !self.stuck_reported.get() {
                let message = "parser did not consume input while parsing";
                let range = self.input.current_range();
                let diagnostic =
                    Diagnostic::new(Stage::Parser, Severity::Error, message).with_range(range);
                self.diagnostics.push(diagnostic);
                self.stuck_reported.set(true);
            }
            return T![eof];
        }
        self.fuel.set(self.fuel.get() - 1);
        self.input.nth(n)
    }

    pub fn eof(&mut self) -> bool {
        self.input.eof()
    }

    pub fn at(&mut self, kind: TokenKind) -> bool {
        self.peek() == kind
    }

    pub fn at_any(&mut self, kinds: &[TokenKind]) -> bool {
        let k = self.peek();
        kinds.contains(&k)
    }

    pub fn eat(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub fn expect(&mut self, kind: TokenKind) {
        if self.eat(kind) {
            return;
        }

        let cur_kind = self.peek();
        let err_msg = format!(
            "expect {:?}, actual {:?}",
            kind.to_string(),
            cur_kind.to_string()
        );

        self.events.push(Event::Error(err_msg));
    }

    pub fn advance(&mut self) {
        self.fuel.set(256);
        self.input.skip();
        self.stuck_reported.set(false);
        self.events.push(Event::Advance);
    }

    pub fn error(&mut self, msg: &str) {
        self.events.push(Event::Error(msg.to_string()));
    }

    pub fn advance_with_error(&mut self, error: &str) {
        let m = self.open();
        self.events.push(Event::Error(error.to_string()));
        self.advance();
        self.close(m, MySyntaxKind::ErrorTree);
    }
}

impl Parser<'_> {
    pub fn parse(mut self) -> Vec<Event> {
        super::file::file(&mut self);
        self.events
    }

    pub fn open(&mut self) -> MarkerOpened {
        let pos = self.events.len();
        self.events.push(Event::Open {
            kind: MySyntaxKind::TombStone,
            forward_parent: None,
        });
        MarkerOpened::new(pos)
    }

    pub fn close(&mut self, m: MarkerOpened, kind: MySyntaxKind) -> MarkerClosed {
        self.events[m.index] = Event::Open {
            kind,
            forward_parent: None,
        };
        self.events.push(Event::Close);
        MarkerClosed { index: m.index }
    }

    pub fn build_tree(mut self) -> ParseResult {
        let mut cursor = 0;
        let tokens = &self.input.tokens;
        let mut builder = GreenNodeBuilder::new();
        let mut diagnostics = self.diagnostics;

        for i in 0..self.events.len() {
            match mem::replace(&mut self.events[i], Event::tombstone()) {
                Event::Open {
                    kind,
                    forward_parent,
                } => {
                    let mut idx = i;
                    let mut fp = forward_parent;
                    let mut kinds = vec![kind];
                    while let Some(fwd) = fp {
                        idx += fwd;
                        fp = match mem::replace(&mut self.events[idx], Event::tombstone()) {
                            Event::Open {
                                kind,
                                forward_parent,
                            } => {
                                kinds.push(kind);
                                forward_parent
                            }
                            _ => unreachable!(),
                        }
                    }
                    for kind in kinds.into_iter().rev() {
                        if kind != MySyntaxKind::TombStone {
                            builder.start_node((kind).into())
                        }
                    }
                }
                Event::Close => {
                    builder.finish_node();
                }
                Event::Advance => {
                    if let Some(token) = tokens.get(cursor) {
                        builder.token(token.kind.to_syntax_kind(), token.text);
                        cursor += 1;
                    }
                }
                Event::Error(msg) => {
                    let range = tokens
                        .get(cursor)
                        .map(|token| token.range)
                        .or_else(|| tokens.last().map(|token| token.range));
                    let diagnostic =
                        Diagnostic::new(Stage::Parser, Severity::Error, msg).with_range(range);
                    diagnostics.push(diagnostic);
                }
            }
            while let Some(token) = tokens.get(cursor) {
                if token.kind == T![eof] || !token.kind.is_trivia() {
                    break;
                }
                builder.token(token.kind.to_syntax_kind(), token.text);
                cursor += 1;
            }
        }

        let node = builder.finish();

        ParseResult {
            green_node: node,
            diagnostics,
        }
    }
}

pub const STMT_RECOVERY: &[TokenKind] = &[T![fn]];
