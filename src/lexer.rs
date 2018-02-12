use std;

#[derive(Debug)]
pub enum Token {
    Def,
    As,
    Let,
    Comma,
    Period,
    Arrow,
    Function(String),
    Id(String),
    Int(i32),
    Open(Paren),
    Close(Paren),
}

#[derive(Debug, Clone, Copy)]
pub enum Paren {
    Paren,
    Bracket,
    Brace,
}

#[derive(Debug)]
pub enum LexerError {
    ParseIntError(std::num::ParseIntError),
    ExpectCharacter(char),
    UnexpectedCharacter(char),
}

pub struct Lexer<Iter: Iterator<Item=char>> {
    source: std::iter::Peekable<Iter>,
}

type LexerResult<T> = Result<T, LexerError>;

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
    type Item = LexerResult<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        let ch = match self.source.peek() {
            Some(&ch) => ch,
            None => return None,
        };

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
        } else if ch == '\'' {
            self.source.next();
            Token::Function(self.take_while(|c| !c.is_whitespace()))
        } else if ch == '-' {
            self.source.next();
            if let Some('>') = self.source.next() {
                Token::Arrow
            } else {
                return Some(Err(LexerError::ExpectCharacter('>')))
            }
        } else {
            // single character tokens
            self.source.next();
            match ch {
                ',' => Token::Comma,
                '.' => Token::Period,
                '(' => Token::Open(Paren::Paren),
                ')' => Token::Close(Paren::Paren),
                '[' => Token::Open(Paren::Bracket),
                ']' => Token::Close(Paren::Bracket),
                '{' => Token::Open(Paren::Brace),
                '}' => Token::Close(Paren::Brace),
                _ => return Some(Err(LexerError::UnexpectedCharacter(ch))),
            }
        };

        Some(Ok(tok))
    }
}
