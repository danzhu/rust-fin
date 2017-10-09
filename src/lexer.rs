use std::str;
use std::iter;

use ast::{Op, Paren};

#[derive(Debug)]
pub enum Token {
    Def,
    As,
    Indent,
    Dedent,
    Newline,
    Comma,
    Id(String),
    Int(i32),
    Open(Paren),
    Close(Paren),
    Operator(Op),
}

pub struct Lexer<Iter: Iterator<Item=char>> {
    source: iter::Peekable<Iter>,
    indents: Vec<usize>,
    dedents: usize,
}

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
    type Item = Token;

    // TODO: error handling
    fn next(&mut self) -> Option<Token> {
        if self.dedents > 0 {
            self.dedents -= 1;
            return Some(Token::Dedent);
        }

        let ch = match self.source.peek() {
            Some(&ch) => ch,
            None => if self.indents.len() > 1 {
                self.dedents = self.indents.len() - 1;
                self.indents = vec![0];
                return self.next();
            } else {
                return None;
            },
        };

        if ch == '\n' {
            self.source.next();

            let mut indent = 0;
            while let Some(&' ') = self.source.peek() {
                indent += 1;
                self.source.next();
            }

            // ignore indent on empty line
            if let Some(&'\n') = self.source.peek() {
                return self.next();
            }

            // ignore indent on comment line
            if let Some(&'#') = self.source.peek() {
                return self.next();
            }

            let last_indent = *self.indents.last().unwrap();

            if indent > last_indent {
                self.indents.push(indent);
                return Some(Token::Indent);
            }

            if indent < last_indent {
                assert_eq!(self.dedents, 0);
                while indent < *self.indents.last().unwrap() {
                    self.indents.pop();
                    self.dedents += 1;
                }

                if indent > *self.indents.last().unwrap() {
                    panic!("incorrect indent");
                }
            }

            return Some(Token::Newline);
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
            } else {
                Token::Id(id)
            }
        } else if ch.is_numeric() {
            // int
            Token::Int(self.take_while(|c| c.is_numeric())
                       .parse()
                       .expect("failed to parse integer"))
        } else {
            // single character tokens
            self.source.next();
            match ch {
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
                _ => panic!("unrecognized character"),
            }
        };

        Some(tok)
    }
}
