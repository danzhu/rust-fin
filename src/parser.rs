use std::iter;

use ast::{Module, Func, Expr, Op, Paren};
use lexer::Token;

type Error<T> = Result<T, &'static str>;

pub struct Parser<T: Iterator<Item=Token>> {
    source: iter::Peekable<T>,
}

impl<T: Iterator<Item=Token>> Parser<T> {
    pub fn new(source: T) -> Parser<T> {
        Parser {
            source: source.peekable(),
        }
    }

    pub fn parse(&mut self) -> Error<Module> {
        Ok(Module {
            functions: vec![self.func()?],
        })
    }

    fn func(&mut self) -> Error<Func> {
        Ok(Func {
            name: "main".to_string(),
            expr: self.expr()?,
        })
    }

    fn expr(&mut self) -> Error<Expr> {
        let mut res = self.term()?;
        while let Some(&Token::Operator(op)) = self.source.peek() {
            match op {
                Op::Add | Op::Sub => {
                    self.source.next();
                    let right = self.term()?;
                    res = Expr::Binary {
                        op,
                        left: Box::new(res),
                        right: Box::new(right),
                    }
                },
                _ => break,
            }
        }
        Ok(res)
    }

    fn term(&mut self) -> Error<Expr> {
        let mut res = self.factor()?;
        while let Some(&Token::Operator(op)) = self.source.peek() {
            match op {
                Op::Mul | Op::Div | Op::Rem => {
                    self.source.next();
                    let right = self.factor()?;
                    res = Expr::Binary {
                        op,
                        left: Box::new(res),
                        right: Box::new(right),
                    }
                },
                _ => break,
            }
        }
        Ok(res)
    }

    fn factor(&mut self) -> Error<Expr> {
        match self.source.next() {
            Some(Token::Int(val)) => Ok(Expr::Int(val)),
            Some(Token::Open(Paren::Paren)) => {
                let res = self.expr()?;
                match self.source.next() {
                    Some(Token::Close(Paren::Paren)) => Ok(res),
                    _ => Err("expect closing parenthesis"),
                }
            }
            _ => Err("expect integer literal")
        }
    }
}
