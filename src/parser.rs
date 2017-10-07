use std::iter;

use ast::{Module, Func, Expr};
use lexer::Token;
use operator::Op;

type Error<T> = Result<T, String>;

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
        let mut expr = self.term()?;
        while let Some(&Token::Operator(op)) = self.source.peek() {
            match op {
                Op::Add | Op::Sub => {
                    self.source.next();
                    let right = self.term()?;
                    expr = Expr::Binary {
                        op,
                        left: Box::new(expr),
                        right: Box::new(right),
                    }
                },
                _ => break,
            }
        }
        Ok(expr)
    }

    fn term(&mut self) -> Error<Expr> {
        let mut expr = self.factor()?;
        while let Some(&Token::Operator(op)) = self.source.peek() {
            match op {
                Op::Mul | Op::Div | Op::Rem => {
                    self.source.next();
                    let right = self.factor()?;
                    expr = Expr::Binary {
                        op,
                        left: Box::new(expr),
                        right: Box::new(right),
                    }
                },
                _ => break,
            }
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Error<Expr> {
        match self.source.next() {
            Some(Token::Int(val)) => Ok(Expr::Int(val)),
            _ => Err("expect integer literal".to_string())
        }
    }
}
