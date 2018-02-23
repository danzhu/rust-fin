#[derive(Copy, Clone, Debug)]
pub struct Pos {
    pub line: i32,
    pub column: i32,
}

#[derive(Copy, Clone, Debug)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

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
    Operator(String),
    Id(String),
    Int(i32),
}
