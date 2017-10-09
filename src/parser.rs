use std::iter;

use ast::{Module, Func, Expr, ExprKind, Op, Paren};
use lexer::Token;

macro_rules! expect {
    ($src:expr, $pat:pat, $res:expr) => (
        match $src.next() {
            Some($pat) => $res,
            _ => return Err(concat!("expect ", stringify!($pat))),
        }
    )
}

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
        let mut functions = Vec::new();
        while self.source.peek().is_some() {
            functions.push(self.func()?);
        }
        Ok(Module { functions })
    }

    fn func(&mut self) -> Error<Func> {
        expect!(self.source, Token::Def, {});

        let name = expect!(self.source, Token::Id(name), name);

        expect!(self.source, Token::Open(Paren::Paren), {});
        expect!(self.source, Token::Close(Paren::Paren), {});
        expect!(self.source, Token::As, {});

        let expr = self.block()?;

        Ok(Func { name, expr })
    }

    fn block(&mut self) -> Error<Expr> {
        expect!(self.source, Token::Indent, {});

        let mut exprs = Vec::new();

        loop {
            match self.source.peek() {
                Some(&Token::Dedent) => break,
                None => panic!("no matching dedent"),
                _ => exprs.push(self.statement()?),
            }
        }

        expect!(self.source, Token::Dedent, {});

        Ok(Expr::new(ExprKind::Block { exprs }))
    }

    fn statement(&mut self) -> Error<Expr> {
        let expr = self.expr()?;
        expect!(self.source, Token::Newline, {});
        Ok(expr)
    }

    fn expr(&mut self) -> Error<Expr> {
        let mut res = self.term()?;
        while let Some(&Token::Operator(op)) = self.source.peek() {
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
        while let Some(&Token::Operator(op)) = self.source.peek() {
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
        match self.source.next() {
            Some(Token::Int(val)) => Ok(Expr::new(ExprKind::Int(val))),
            Some(Token::Open(Paren::Paren)) => {
                let res = self.expr()?;
                expect!(self.source, Token::Close(Paren::Paren), {});
                Ok(res)
            }
            _ => Err("expect integer literal or open parenthesis")
        }
    }
}
