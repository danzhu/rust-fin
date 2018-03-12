use std::{io, iter, num, result};

use common::*;
use token::*;
use ctx::*;

struct Lexer<Iter: Iterator<Item = char>> {
    source: iter::Peekable<Iter>,
    pos: Pos,
}

pub struct Error {
    pub kind: ErrorKind,
    pub pos: Pos,
}

pub enum ErrorKind {
    ParseInt(num::ParseIntError),
    UnexpectedChar(char),
}

pub type Result<T> = result::Result<T, Error>;

pub fn lex(src: &str) -> Result<Vec<Token>> {
    Lexer::new(src.chars()).collect()
}

impl<Iter> Lexer<Iter>
where
    Iter: Iterator<Item = char>,
{
    pub fn new(source: Iter) -> Self {
        Self {
            source: source.peekable(),
            pos: Pos { line: 1, column: 1 },
        }
    }

    fn read(&mut self) -> Option<char> {
        let ch = self.source.next();
        match ch {
            Some('\n') => {
                self.pos.line += 1;
                self.pos.column = 1;
            }
            Some(_) => {
                self.pos.column += 1;
            }
            None => {}
        }
        ch
    }

    fn read_while<Pred>(&mut self, pred: Pred) -> String
    where
        Pred: Fn(char) -> bool,
    {
        let mut s = String::new();
        loop {
            match self.source.peek() {
                Some(&c) if pred(c) => {
                    self.read();
                    s.push(c);
                }
                _ => break s,
            }
        }
    }

    fn error<T>(&self, kind: ErrorKind, pos: Pos) -> Option<Result<T>> {
        Some(Err(Error { kind, pos }))
    }
}

impl<Iter> Iterator for Lexer<Iter>
where
    Iter: Iterator<Item = char>,
{
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        let ch = *self.source.peek()?;

        // skip whitespace
        if ch.is_whitespace() {
            self.read();
            return self.next();
        }

        // skip comments
        if ch == '#' {
            self.read();
            loop {
                match self.read() {
                    Some('\n') | None => break,
                    _ => {}
                }
            }
            return self.next();
        }

        let start = self.pos;

        let kind = if ch.is_alphabetic() {
            // id
            let id = self.read_while(|c| c.is_alphanumeric());

            match id.as_ref() {
                "def" => TokenKind::Def,
                "struct" => TokenKind::Struct,
                "as" => TokenKind::As,
                "if" => TokenKind::If,
                "then" => TokenKind::Then,
                "elif" => TokenKind::Elif,
                "else" => TokenKind::Else,
                "let" => TokenKind::Let,
                _ => if ch.is_lowercase() {
                    TokenKind::Id(id)
                } else {
                    TokenKind::Type(id)
                },
            }
        } else if ch.is_numeric() {
            // int
            let val = self.read_while(|c| c.is_numeric());
            match val.parse() {
                Ok(val) => TokenKind::Int(val),
                Err(err) => return self.error(ErrorKind::ParseInt(err), start),
            }
        } else {
            self.read();
            match ch {
                '\'' => TokenKind::Quote,
                '-' => if let Some(&'>') = self.source.peek() {
                    self.read();
                    TokenKind::Arrow
                } else {
                    TokenKind::Operator(Op::Arith(ArithOp::Sub))
                },
                '!' => if let Some(&'=') = self.source.peek() {
                    self.read();
                    TokenKind::Operator(Op::Comp(CompOp::Ne))
                } else {
                    TokenKind::Not
                },
                '+' => TokenKind::Operator(Op::Arith(ArithOp::Add)),
                '*' => TokenKind::Operator(Op::Arith(ArithOp::Mul)),
                '/' => TokenKind::Operator(Op::Arith(ArithOp::Div)),
                '%' => TokenKind::Operator(Op::Arith(ArithOp::Mod)),
                '=' => TokenKind::Operator(Op::Comp(CompOp::Eq)),
                '<' => TokenKind::Operator(Op::Comp(CompOp::Lt)),
                '>' => TokenKind::Operator(Op::Comp(CompOp::Gt)),
                ',' => TokenKind::Comma,
                '.' => TokenKind::Period,
                '(' => TokenKind::LParen,
                ')' => TokenKind::RParen,
                _ => return self.error(ErrorKind::UnexpectedChar(ch), start),
            }
        };

        let end = self.pos;
        let span = Span::new(start, end);

        Some(Ok(Token::new(kind, span)))
    }
}

impl Error {
    pub fn print<Out>(&self, f: &mut Out, ctx: &Context) -> io::Result<()>
    where
        Out: io::Write,
    {
        write!(f, "{}: error: ", self.pos.format(ctx))?;
        match self.kind {
            ErrorKind::ParseInt(ref err) => write!(f, "{}", err),
            ErrorKind::UnexpectedChar(ch) => write!(f, "unexpected character '{}'", ch),
        }
    }
}
