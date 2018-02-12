use std;

use symbol::{Op, Paren};

const INDENT_STACK_EMPTY: &str = "indent stack empty";

#[derive(Debug)]
pub enum Token {
    Def,
    As,
    Let,
    Indent,
    Dedent,
    Newline,
    Equal,
    Comma,
    Id(String),
    Int(i32),
    Open(Paren),
    Close(Paren),
    Operator(Op),
}

#[derive(Debug)]
pub enum LexerError {
    Indent,
    ParseIntError(std::num::ParseIntError),
    UnknownCharacter,
}

pub struct Lexer<Iter: Iterator<Item=char>> {
    source: std::iter::Peekable<Iter>,
    indents: Vec<usize>,
    dedents: usize,
}

type LexerResult<T> = Result<T, LexerError>;

impl<Iter: Iterator<Item=char>> Lexer<Iter> {
    pub fn new(source: Iter) -> Lexer<Iter> {
        Lexer {
            source: source.peekable(),
            indents: vec![0],
            dedents: 0,
        }
    }

    fn take_while<P>(&mut self, pred: P) -> String
        where P: Fn(char) -> bool {
        let mut s = String::new();
        loop {
            match self.source.peek() {
                Some(&c) if pred(c) => s.push(c),
                _ => break s,
            }
            self.source.next();
        }
    }
}

impl<Iter: Iterator<Item=char>> Iterator for Lexer<Iter> {
    type Item = LexerResult<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.dedents > 0 {
            self.dedents -= 1;
            return Some(Ok(Token::Dedent));
        }

        let ch = match self.source.peek() {
            Some(&ch) => ch,
            None => return if self.indents.len() > 1 {
                // cleanup previous indents
                self.dedents = self.indents.len() - 1;
                self.indents = vec![0];
                self.next()
            } else {
                None
            },
        };

        if ch == '\n' {
            self.source.next();

            let mut indent = 0;
            while let Some(&' ') = self.source.peek() {
                self.source.next();
                indent += 1;
            }

            // ignore indent on empty line
            if let Some(&'\n') = self.source.peek() {
                return self.next();
            }

            // ignore indent on comment line
            if let Some(&'#') = self.source.peek() {
                return self.next();
            }

            let last_indent = *self.indents.last().expect(INDENT_STACK_EMPTY);

            if indent > last_indent {
                self.indents.push(indent);
                return Some(Ok(Token::Indent));
            }

            if indent < last_indent {
                assert_eq!(self.dedents, 0);
                while indent < *self.indents.last().expect(INDENT_STACK_EMPTY) {
                    self.indents.pop();
                    self.dedents += 1;
                }

                if indent > *self.indents.last().expect(INDENT_STACK_EMPTY) {
                    return Some(Err(LexerError::Indent));
                }
            }

            return Some(Ok(Token::Newline));
        }

        // skip whitespace
        if ch.is_whitespace() {
            self.source.next();
            return self.next();
        }

        // skip comments
        if ch == '#' {
            self.source.next();
            loop {
                match self.source.peek() {
                    Some(&'\n') | None => break,
                    _ => self.source.next(),
                };
            }
            return self.next();
        }

        let tok = if ch.is_alphabetic() {
            // id
            let id = self.take_while(|c| c.is_alphanumeric());

            if id == "def" {
                Token::Def
            } else if id == "as" {
                Token::As
            } else if id == "let" {
                Token::Let
            } else {
                Token::Id(id)
            }
        } else if ch.is_numeric() {
            // int
            match self.take_while(|c| c.is_numeric()).parse() {
                Ok(val) => Token::Int(val),
                Err(err) => return Some(Err(LexerError::ParseIntError(err))),
            }
        } else {
            // single character tokens
            self.source.next();
            match ch {
                '=' => Token::Equal,
                ',' => Token::Comma,
                '(' => Token::Open(Paren::Paren),
                ')' => Token::Close(Paren::Paren),
                '[' => Token::Open(Paren::Bracket),
                ']' => Token::Close(Paren::Bracket),
                '{' => Token::Open(Paren::Brace),
                '}' => Token::Close(Paren::Brace),
                '+' => Token::Operator(Op::Add),
                '-' => Token::Operator(Op::Sub),
                '*' => Token::Operator(Op::Mul),
                '/' => Token::Operator(Op::Div),
                '%' => Token::Operator(Op::Rem),
                _ => return Some(Err(LexerError::UnknownCharacter)),
            }
        };

        Some(Ok(tok))
    }
}
