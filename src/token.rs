use common::*;

#[derive(Clone, Debug)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

#[derive(Clone, Debug)]
pub enum TokenKind {
    Def,
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
    Int(i32),
}
