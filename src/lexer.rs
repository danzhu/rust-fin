use std;

#[derive(Clone, Debug)]
pub enum Token {
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
    Function(String),
    Id(String),
    Int(i32),
}

pub enum LexerError {
    ParseIntError(std::num::ParseIntError),
    Unexpected(char),
}

pub struct Lexer<Iter: Iterator<Item=char>> {
    source: std::iter::Peekable<Iter>,
}

type LexerResult<T> = Result<T, LexerError>;

impl std::fmt::Debug for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            &LexerError::ParseIntError(ref err) => {
                write!(f, "{}", err)?;
            }
            &LexerError::Unexpected(ch) => {
                write!(f, "unexpected character '{}'", ch)?;
            }
        }
        Ok(())
    }
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
            } else if id == "if" {
                Token::If
            } else if id == "then" {
                Token::Then
            } else if id == "elif" {
                Token::Elif
            } else if id == "else" {
                Token::Else
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
            self.source.next();
            match ch {
                '\'' => Token::Function(self.take_while(|c| c.is_alphanumeric())),
                '+' | '*' | '/' | '%' | '=' => Token::Function(ch.to_string()),
                '-' => if let Some(&'>') = self.source.peek() {
                    self.source.next();
                    Token::Arrow
                } else {
                    Token::Function(ch.to_string())
                },
                ',' => Token::Comma,
                '.' => Token::Period,
                '(' => Token::LParen,
                ')' => Token::RParen,
                _ => return Some(Err(LexerError::Unexpected(ch))),
            }
        };

        Some(Ok(tok))
    }
}
