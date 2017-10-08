use std::str;
use std::iter;

use ast::{Op, Paren};

#[derive(Debug)]
pub enum Token {
    Def,
    Id(String),
    Int(i32),
    Open(Paren),
    Close(Paren),
    Operator(Op),
}

pub struct Lexer<Iter: Iterator<Item=char>> {
    source: iter::Peekable<Iter>,
}

impl<Iter: Iterator<Item=char>> Lexer<Iter> {
    pub fn new(source: Iter) -> Lexer<Iter> {
        Lexer {
            source: source.peekable(),
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
        let ch = match self.source.peek() {
            Some(&ch) => ch,
            None => return None,
        };

        if ch.is_whitespace() {
            self.source.next();
            return self.next();
        }

        let tok = if ch.is_alphabetic() {
            // id
            let id = self.take_while(|c| c.is_alphanumeric());

            if id == "def" {
                Token::Def
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
