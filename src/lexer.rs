use std::fmt;
use std::io;
use std::iter;
use std::num;
use std::result;

use common::*;
use token::*;

struct Lexer<Iter: Iterator<Item = char>> {
    source: iter::Peekable<Iter>,
    pos: Pos,
}

pub enum Error {
    Io(io::Error),
    ParseInt(num::ParseIntError),
    UnexpectedChar(char),
}

pub type Result<T> = result::Result<T, Error>;

pub fn lex<In>(mut input: In) -> Result<Vec<Token>>
where
    In: io::Read,
{
    let mut src = String::new();
    input.read_to_string(&mut src)?;
    let lex = Lexer::new(src.chars());
    lex.collect()
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
                Some(&c) if pred(c) => s.push(c),
                _ => break s,
            }
            self.read();
        }
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
                _ => TokenKind::Id(id),
            }
        } else if ch.is_numeric() {
            // int
            match self.read_while(|c| c.is_numeric()).parse() {
                Ok(val) => TokenKind::Int(val),
                Err(err) => return Some(Err(Error::ParseInt(err))),
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
                _ => return Some(Err(Error::UnexpectedChar(ch))),
            }
        };

        let end = self.pos;

        Some(Ok(Token {
            span: Span { start, end },
            kind,
        }))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Io(ref err) => write!(f, "{}", err),
            Error::ParseInt(ref err) => write!(f, "{}", err),
            Error::UnexpectedChar(ch) => write!(f, "unexpected character '{}'", ch),
        }
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err)
    }
}
