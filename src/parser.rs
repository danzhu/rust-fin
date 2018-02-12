use std::iter;

use ast::{Module, Func, Expr, ExprKind, Op, Paren, Decl};
use lexer::Token;
use util::*;

macro_rules! expect {
    ($src:expr, $pat:pat, $res:expr) => (
        match $src.next() {
            Some(Ok($pat)) => $res,
            Some(Err(err)) => return Err(err),
            _ => return Err(concat!("expect ", stringify!($pat))),
        }
    );
    ($src:expr, $pat:pat) => (
        match $src.next() {
            Some(Ok($pat)) => {},
            Some(Err(err)) => return Err(err),
            _ => return Err(concat!("expect ", stringify!($pat))),
        }
    );
}

type Error<T> = Result<T, &'static str>;

pub struct Parser<T: Iterator<Item=Result<Token, &'static str>>> {
    source: iter::Peekable<T>,
}

impl<T: Iterator<Item=Result<Token, &'static str>>> Parser<T> {
    pub fn new(source: T) -> Parser<T> {
        Parser {
            source: source.peekable(),
        }
    }

    pub fn parse(&mut self) -> Error<Module> {
        let mut functions = Vec::new();
        while self.source.peek().is_some() {
            functions.push(self.func()?);
        }
        Ok(Module { functions })
    }

    fn func(&mut self) -> Error<Func> {
        expect!(self.source, Token::Def);
        let name = expect!(self.source, Token::Id(name), name);

        expect!(self.source, Token::Open(Paren::Paren));
        let mut params = Vec::new();
        if let Some(&Token::Close(Paren::Paren)) = self.source.try_peek()? {
            // no params
            self.source.next();
        } else {
            loop {
                let name = expect!(self.source, Token::Id(name), name);
                params.push(Decl { name });

                match self.source.try_next()? {
                    Some(Token::Close(Paren::Paren)) => break,
                    Some(Token::Comma) => {},
                    _ => return Err("expect closing parenthesis or comma"),
                }
            }
        }

        expect!(self.source, Token::As);

        let body = self.block()?;

        Ok(Func { name, params, body })
    }

    fn block(&mut self) -> Error<Expr> {
        expect!(self.source, Token::Indent);

        let mut exprs = Vec::new();
        loop {
            match self.source.try_peek()? {
                Some(&Token::Dedent) => break,
                None => panic!("no matching dedent"),
                _ => exprs.push(self.statement()?),
            }
        }

        expect!(self.source, Token::Dedent);

        Ok(Expr::new(ExprKind::Block { exprs }))
    }

    fn statement(&mut self) -> Error<Expr> {
        let expr = match self.source.try_peek()? {
            Some(&Token::Let) => {
                self.source.next();
                let var = expect!(self.source, Token::Id(name), name);
                expect!(self.source, Token::Equal);
                let value = self.expr()?;
                Expr::new(ExprKind::Let { var, value: Box::new(value) })
            },
            _ => self.expr()?,
        };
        expect!(self.source, Token::Newline);
        Ok(expr)
    }

    fn expr(&mut self) -> Error<Expr> {
        let mut res = self.term()?;
        while let Some(&Token::Operator(op)) = self.source.try_peek()? {
            match op {
                Op::Add | Op::Sub => {
                    self.source.next();
                    let right = self.term()?;
                    res = Expr::new(ExprKind::Binary {
                        op,
                        left: Box::new(res),
                        right: Box::new(right),
                    })
                },
                _ => break,
            }
        }
        Ok(res)
    }

    fn term(&mut self) -> Error<Expr> {
        let mut res = self.factor()?;
        while let Some(&Token::Operator(op)) = self.source.try_peek()? {
            match op {
                Op::Mul | Op::Div | Op::Rem => {
                    self.source.next();
                    let right = self.factor()?;
                    res = Expr::new(ExprKind::Binary {
                        op,
                        left: Box::new(res),
                        right: Box::new(right),
                    })
                },
                _ => break,
            }
        }
        Ok(res)
    }

    fn factor(&mut self) -> Error<Expr> {
        match self.source.try_next()? {
            Some(Token::Id(name)) => {
                if let Some(&Token::Open(Paren::Paren)) = self.source.try_peek()? {
                    // function call
                    self.source.next();

                    let mut args = Vec::new();
                    if let Some(&Token::Close(Paren::Paren)) = self.source.try_peek()? {
                        // no params
                        self.source.next();
                    } else {
                        loop {
                            args.push(self.expr()?);
                            match self.source.try_next()? {
                                Some(Token::Close(Paren::Paren)) => break,
                                Some(Token::Comma) => {},
                                _ => return Err("expect closing parenthesis or comma"),
                            }
                        }
                    }

                    Ok(Expr::new(ExprKind::Call {
                        name,
                        args,
                    }))
                } else {
                    // id
                    Ok(Expr::new(ExprKind::Id(name)))
                }
            },
            Some(Token::Int(val)) => Ok(Expr::new(ExprKind::Int(val))),
            Some(Token::Open(Paren::Paren)) => {
                let res = self.expr()?;
                expect!(self.source, Token::Close(Paren::Paren));
                Ok(res)
            }
            _ => Err("expect integer literal or open parenthesis")
        }
    }
}
