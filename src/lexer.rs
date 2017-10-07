use std::str;
use std::iter;

use operator::Op;

#[derive(Debug)]
pub enum Token {
    Lparen,
    Rparen,
    Operator(Op),
    Int(i32),
    Id(String),
}

pub struct Lexer<'a> {
    source: iter::Peekable<str::Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: str::Chars<'a>) -> Lexer<'a> {
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

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let ch = match self.source.peek() {
            Some(&ch) => ch,
            None => return None,
        };

        if ch.is_whitespace() {
            self.source.next();
            self.next()
        } else if ch.is_alphabetic() {
            Some(Token::Id(self.take_while(|c| c.is_alphanumeric())))
        } else if ch.is_numeric() {
            Some(Token::Int(self.take_while(|c| c.is_numeric())
                            .parse()
                            .expect("failed to parse integer")))
        } else {
            self.source.next();
            match ch {
                '(' => Some(Token::Lparen),
                ')' => Some(Token::Rparen),
                '+' => Some(Token::Operator(Op::Add)),
                '-' => Some(Token::Operator(Op::Sub)),
                '*' => Some(Token::Operator(Op::Mul)),
                '/' => Some(Token::Operator(Op::Div)),
                '%' => Some(Token::Operator(Op::Rem)),
                _ => panic!("unrecognized character"),
            }
        }
    }
}
