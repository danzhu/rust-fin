use std::fmt;

use common::*;

#[derive(Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Token { kind, span }
    }
}

#[derive(Clone)]
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
    Bind(String),
    Type(String),
    Int(i32),
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TokenKind::Def => write!(f, "def"),
            TokenKind::Struct => write!(f, "struct"),
            TokenKind::As => write!(f, "as"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Then => write!(f, "then"),
            TokenKind::Elif => write!(f, "elif"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::Let => write!(f, "let"),
            TokenKind::Comma => write!(f, "comma"),
            TokenKind::Period => write!(f, "period"),
            TokenKind::Arrow => write!(f, "arrow"),
            TokenKind::LParen => write!(f, "left parenthesis"),
            TokenKind::RParen => write!(f, "right parenthesis"),
            TokenKind::Quote => write!(f, "quote"),
            TokenKind::Not => write!(f, "not"),
            TokenKind::Operator(op) => write!(f, "operator {}", op),
            TokenKind::Bind(ref bind) => write!(f, "bind {}", bind),
            TokenKind::Type(ref tp) => write!(f, "type {}", tp),
            TokenKind::Int(val) => write!(f, "int {}", val),
        }
    }
}
