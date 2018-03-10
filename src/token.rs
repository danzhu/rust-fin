use common::*;

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum TokenKind {
    Def,
    Struct,
    As,
    If,
    Then,
    Elif,
    Else,
    Let,
    Comma,
    Period,
    Arrow,
    LParen,
    RParen,
    Quote,
    Not,
    Operator(Op),
    Id(String),
    Type(String),
    Int(i32),
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Token { kind, span }
    }
}
